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
  Encapsulation of a CADRG Layer.
  CADRG support was written by Tim Ranger for the Canadian Department of
  National Defense and is provided 'as-is' without warranty. DND assumes
  no liability for damages or financial loss incurred in the use of this
  software.
}

{$IFDEF DCC}
  unit GisLayerCADRG ;
  {$HPPEMIT '#pragma link "GisLayerCADRG"'}
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
    System.Security,
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Types,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisClasses,
    GisLayer,
    GisRtl,
    GisTypes,
    GisTypesUI,
    GisLayerPixel,
    GisParams,
    GisFileCADRG;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

const
  {#gendoc:hide}
  SCALE_GAP_PERCENTAGE = 10;

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerCADRG = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFNDEF OXYGENE}
    {#GENDOC:HIDE}
    TGIS_LayerCADRG_ChildLayers = array of TObject;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of CADRG layer.
  /// </summary>
  TGIS_LayerCADRG = {$IFDEF OXYGENE} public {$ENDIF}  class( TGIS_LayerPixel )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private variables
      FOnReadFrame : TGIS_FileCADRG_ReadFrameEvent ;
      CADRG : TGIS_FileCADRGDecoder ;
    // various protected procedures
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setUp        ; override;

      /// <inheritdoc/>
      procedure fset_Interpretation ( const _value : TGIS_LayerPixelInterpretation
                                    ) ; override ;

      /// <inheritdoc/>
      procedure fset_GridImage      ( const _value : Boolean
                                    ) ; override;
      /// <summary>
      ///   Property access routine for IsGridImage property.
      /// </summary>
      /// <param name="_isgrid">
      ///   True if data is grid
      /// </param>
      procedure setGridImage ( const _isgrid      : Boolean
                             ) ; //override;

      /// <summary>
      ///   Property access routine for DisableCADRG property.
      /// </summary>
      /// <returns>
      ///   map series structure
      /// </returns>
      function  getMapSeries : TGIS_FileCADRG_MapSeries ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function  getBitmapData( const _extent   : TGIS_Extent ;
                               const _bitmap   : TGIS_Pixels ;
                               const _width    : Integer ;
                               const _height    : Integer
                             ) : Boolean ; override;

      /// <inheritdoc/>
      function  getGridData       ( const _extent       : TGIS_Extent      ;
                                    const _grid         : TGIS_GridArray
                                  ) : Boolean ; override;


    protected

      procedure   doDestroy  ; override;

    public // various public routines

      /// <inheritdoc/>
      constructor Create     ; override;

      /// <inheritdoc/>
      procedure   ReadConfig ; override;
      {$IFDEF WINFORMS}
        /// <inheritdoc/>
        function  Locate    ( const _ptg          : TGIS_Point       ;
                              var   _rgbMapped    : TGIS_RGBTriple   ;
                              var   _nativesVals  : TGIS_DoubleArray ;
                              var   _transparency : Boolean
                            ) : Boolean ; override;
      {$ELSE}
        /// <inheritdoc/>
        function  Locate    ( const _ptg          : TGIS_Point       ;
                              var   _rgbMapped    : TGIS_Color       ;
                              var   _nativesVals  : TGIS_DoubleArray ;
                              var   _transparency : Boolean
                            ) : Boolean ; override;
      {$ENDIF}

      /// <inheritdoc/>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override ;


      /// <summary>
      ///   Rebuilds Layer working on data base.
      /// </summary>
      procedure   DoReload  ;

    public // properties

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Map series structure.
      /// </summary>
      property MapSeries   : TGIS_FileCADRG_MapSeries read  getMapSeries ;

    published // events
      {$IFDEF CLR}
        /// <event/>
        event    ReadFrameEvent : TGIS_FileCADRG_ReadFrameEvent
                                                      delegate FOnReadFrame ;
      {$ELSE}
        /// <event/>
        property ReadFrameEvent : TGIS_FileCADRG_ReadFrameEvent
                                                      read  FOnReadFrame
                                                      write FOnReadFrame ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
//    System.Generics.Defaults,

    GisRegistredLayers,
    GisFunctions,
    GisResource,
    GisInterfaces,
    GisInternals ;
{$ENDIF}

type
  // Encapsulation of CADRG sub layer layer.
  T_LayerCADRG_Child = class( TGIS_LayerPixel )

    // various private variables
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      CADRG      : TGIS_FileCADRGDecoder ;
      pParent    : TGIS_LayerCADRG ;
      iBoundary  : Integer ;
      MapCadrg   : TGIS_FileCADRG_Map ;
      dMinScale  : Double   ;
      dScale     : Integer  ;  // Numeric representation of the layers scale.

    // various protected routines
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // <summary>
      //   Makes a general layer setup.
      // </summary>
      procedure setUp       ; override;

      /// <inheritdoc/>
      procedure fset_MinHeight     ( const _value : Single
                                    ) ; override ;

      /// <inheritdoc/>
      procedure fset_MaxHeight     ( const _value : Single
                                    ) ;override ;

      /// <inheritdoc/>
      function  getLine   ( const _buffer : TBytes                ;
                            const _offset : Integer               ;
                            const _linenr : Integer               ;
                            const _start  : Integer               ;
                            const _bytes  : Integer
                          ) : Integer ; override;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;

      function  getScaledBlockPixels(
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bheight    : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                   ) : TPoint;

      // <summary>
      //   Internal use only.
      //   For reading an image line.
      // </summary>
      // <param name="_buffer">
      //   pointer
      // </param>
      // <param name="_linenr">
      //   line number
      // </param>
      // <param name="_start">
      //   left margin (bytes to skip)
      // </param>
      // <param name="_bytes">
      //   bytes count
      // </param>
      function  getDBLine ( const _buffer : TBytes                ;
                            const _offset : Integer               ;
                            const _linenr : Integer               ;
                            const _start  : Integer               ;
                            const _bytes  : Integer
                          ) : Integer ;

      /// <summary>
      ///   Internal use only. For reading an image line.
      /// </summary>
      /// <returns>
      ///   number of read pixels
      /// </returns>
      /// <param name="_buffer">
      ///   pixels array
      /// </param>
      /// <param name="_offset">
      ///   start in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_pixStart">
      ///   left margin (pixels to skip)
      /// </param>
      /// <param name="_pixCount">
      ///   number of image pixels to read
      /// </param>
      function  getDBLinePixels   ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer;

    public
      /// <inheritdoc/>
      function getBitmapData( const _extent   : TGIS_Extent ;
                              const _bitmap   : TGIS_Pixels ;
                              const _width    : Integer ;
                              const _height    : Integer
                             ) : Boolean ; override;
    public

      {$IFDEF WINFORMS}
        /// <inheritdoc/>
        function  Locate    ( const _ptg          : TGIS_Point       ;
                              var   _rgbMapped    : TGIS_RGBTriple   ;
                              var   _nativesVals  : TGIS_DoubleArray ;
                              var   _transparency : Boolean
                            ) : Boolean ; override;
      {$ELSE}
        /// <inheritdoc/>
        function  Locate    ( const _ptg          : TGIS_Point       ;
                              var   _rgbMapped    : TGIS_Color       ;
                              var   _nativesVals  : TGIS_DoubleArray ;
                              var   _transparency : Boolean
                            ) : Boolean ; override;
      {$ENDIF}
      /// <inheritdoc/>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override;

  end ;

  {$IFDEF JAVA}
  T_listSortComparLayers = class( java.util.Comparator<TGIS_LayerAbstract> )
    public
      function    compare ( _item1    : TGIS_LayerAbstract ;
                            _item2    : TGIS_LayerAbstract
                          ) : Integer ;
  end ;

  function T_listSortComparLayers.compare(
    _item1 : TGIS_LayerAbstract ;
    _item2 : TGIS_LayerAbstract
  ) : Integer ;
  begin
    Result := compare_layers( _item1, _item2 ) ;
  end ;
{$ENDIF}

//==============================================================================
// Utilities
//==============================================================================

  { Compare layers for ChildLayers sorting.
    This sort is set up to make Largest scale top precedence
    So is actually a descending sort
  }
  function compare_layers( const _item1, _item2 : TGIS_LayerAbstract ) : Integer ;
  var
    layer1, layer2 : T_LayerCADRG_Child;
  begin
      layer1 := T_LayerCADRG_Child( _item1 ) ;
      layer2 := T_LayerCADRG_Child( _item2 ) ;
    if      layer1.scaleX = layer2.scaleX then
            Result :=  0
    else if layer1.scaleX < layer2.scaleX then
            Result :=  1
    else
            Result := -1 ;
  end ;

//==============================================================================
// T_LayerCADRG_Child
//==============================================================================

  function  T_LayerCADRG_Child.getLine (
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    lcounter: LongWord;
    icounter: Integer;
    row: Word; // Row of frame within zone/extent
    col: Word; // Column of frame within
    relative_line: Integer;
    byte_count: Integer;
    starting_pixel: Integer;
    frame_found: Boolean;
    offset : Integer ;
  begin
    if assigned(CADRG.ReadFrameEvent) then begin
      Result := getDBLine ( _buffer, _offset, _linenr, _start, _bytes) ;
      exit ;
    end ;
    offset   := _offset ;
    icounter := 0;
    Result := _bytes ;

    while icounter < _bytes do begin
      _buffer[offset] := CADRG.TransparentBlue  ;
      inc(offset);
      _buffer[offset] := CADRG.TransparentGreen ;
      inc(offset);
      _buffer[offset] := CADRG.TransparentRed   ;
      inc(offset);
      icounter := icounter + 3 ;
    end ;

    if _linenr >= FCellHeight then
      exit ;

    // Determine which frame corresponds to the _linenr. Orientation of frame rows 0 is
    // Bottom of frame rows, the row 0 for _linenr is top of extent so do the
    // conversion accordingly
    row := ( CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesNS
             - 1
           ) - ( Cardinal(_linenr) div 1536 ) ;
    relative_line := _linenr mod 1536;

    // Calculate a pixel value from the _start byte offset
    starting_pixel := _start div 3;

    // Get the frame that the first X value appears in
    col := starting_pixel div 1536;

    // Find the frame in the TOC with this magnification,row,col
    // In some cases the entire frame may not be in the TOC This appears to be
    // how entire Frames are masked. So if it is not found adjust the buffer
    byte_count  := 0     ;
    frame_found := False ;
    for lcounter :=0 to high( CADRG.FrameFileIndexRecords ) do begin
      if ( ( CADRG.FrameFileIndexRecords[lcounter].FrameLocationRowNumber    = row    )
           and
           ( CADRG.FrameFileIndexRecords[lcounter].FrameLocationColumnNumber = col    )
           and
           ( CADRG.FrameFileIndexRecords[lcounter].BoundaryRectangleNumber = iBoundary )
        )
      then begin
        byte_count := CADRG.GetFrameRowRGB( lcounter,
                                            relative_line,
                                            starting_pixel mod 1536,
                                            _bytes,
                                            _buffer,
                                            _offset
                                          ) ;
        frame_found := True ;
        break;
      end ;
    end ;

    if not frame_found then
      byte_count := ( 1536 - ( starting_pixel mod 1536 ) ) * 3 ;

    while ( ( byte_count < _bytes ) and
            ( col < CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesEW )
          ) do
    begin
      // Move to the next frame column wise
      col := col + 1;

      // Find it in the TOC
      frame_found := False ;
      for lcounter := 0 to high( CADRG.FrameFileIndexRecords ) do begin
        if ( ( CADRG.FrameFileIndexRecords[lcounter].FrameLocationRowNumber    = row  )
             and
             ( CADRG.FrameFileIndexRecords[lcounter].FrameLocationColumnNumber = col )
             and
             ( CADRG.FrameFileIndexRecords[lcounter].BoundaryRectangleNumber = iBoundary )
           )
        then begin
          byte_count := byte_count +
                        CADRG.GetFrameRowRGB( lcounter,
                                              relative_line,
                                              0,
                                              _bytes - byte_count,
                                              _buffer,
                                              _offset + byte_count
                                            ) ;
          frame_found := True ;
          break;
        end ;
      end ;
      if not frame_found then
        byte_count := byte_count + (1536 * 3);
    end ;

    Result := _bytes;

  end ;

  function  T_LayerCADRG_Child.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer;
  var
    i : Integer;
    icounter: {$IFDEF JAVA} nullable {$ENDIF} Integer;
    row: Word; // Row of frame within zone/extent
    col: Word; // Column of frame within
    relative_line: Integer;
    pix_count: Integer;
    starting_pixel: Integer;
    offset : Integer ;
    transp : Integer ;
    r : TGIS_FileCADRG_FrameFileIndexRec ;
    colstart : Integer ;
    count : Integer ;
    colmax : Integer ;
    pixportion : Integer ;
    to_read : Integer ;
    rowcol : Cardinal ;
  begin
    if assigned(CADRG.ReadFrameEvent) then begin
      Result := getDBLinePixels ( _buffer, _offset, _linenr, _pixStart,
                                  _pixCount ) ;
      exit ;
    end ;

    if not assigned (CADRG.Dct) then begin
      CADRG.Dct := TDictionary<Cardinal, Integer>.Create ;


      for i := 0 to high(CADRG.FrameFileIndexRecords) do begin
        r := CADRG.FrameFileIndexRecords[i] ;
        if r.BoundaryRectangleNumber = iBoundary then begin
          rowcol := (Cardinal(r.FrameLocationRowNumber) shl 16) or
                    (Cardinal(r.FrameLocationColumnNumber))  ;

        if not CADRG.Dct.TryGetValue(rowcol, icounter ) then
          CADRG.Dct.Add( rowcol, i );
        end;
      end;

    end;
    offset   := _offset ;
    Result := _pixCount ;
    to_read := _pixCount ;


    transp :=  Integer(CADRG.TransparentBlue) or
              (Integer(CADRG.TransparentGreen) shl 08) or
              (Integer(CADRG.TransparentRed)   shl 16) ;
    icounter := 0;
    while icounter < _pixCount do begin
      _buffer[offset +icounter] := transp ;
      inc(icounter) ;
    end ;

    if _linenr >= FCellHeight then
      exit ;

    // Determine which frame corresponds to the _linenr. Orientation of frame rows 0 is
    // Bottom of frame rows, the row 0 for _linenr is top of extent so do the
    // conversion accordingly
    row := ( CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesNS
             - 1
           ) - ( Cardinal(_linenr) div 1536 ) ;
    relative_line := _linenr mod 1536;


    // Get the frame that the first X value appears in
    colstart := _pixStart div 1536;
    colmax   := ((FBitWidth +1535) div 1536) -1 ;
    // Calculate a pixel value from the _start byte offset
    starting_pixel := _pixStart mod 1536 ;

    // Find the frame in the TOC with this magnification,row,col
    // In some cases the entire frame may not be in the TOC This appears to be
    // how entire Frames are masked. So if it is not found adjust the buffer
    pix_count  := 0     ;

    for col := colstart to colmax do begin

      pixportion := 1536 - starting_pixel ;
      rowcol := (Cardinal(row) shl 16) or (Cardinal(col))  ;

      if CADRG.Dct.TryGetValue( rowcol, icounter ) then begin
        if pixportion > (_pixCount -pix_count) then
          pixportion := _pixCount -pix_count;


        count := CADRG.GetFrameRowARGB( icounter,
                                        relative_line,
                                        starting_pixel ,
                                        pixportion,
                                        _buffer,
                                        _offset +pix_count
                                      ) ;
        pix_count := pix_count +count ;
//        pix_count := pix_count +pixportion ;
      end
      else
        pix_count := pix_count +( 1536 - starting_pixel ) ;
//      pixportion := 1536 ;
      starting_pixel := 0 ;
    end;

    Result := _pixCount;
  end ;


  function  T_LayerCADRG_Child.getScaledBlockPixels(
                                     const _buffer     : TGIS_Pixels  ;
                                     const _bwidth     : Integer ;
                                     const _bheight    : Integer ;
                                     const _left       : Integer ;
                                     const _top        : Integer ;
                                     const _lineStart  : Integer ;
                                     const _lineCount  : Integer ;
                                     const _pixStart   : Integer ;
                                     const _pixCount   : Integer ;
                                     const _scale_y    : Single ;
                                     const _scale_x    : Single
                                  ) : TPoint ;
  var
    i : Integer ;
    icounter: {$IFDEF JAVA} nullable {$ENDIF} Integer;
    row: Word; // Row of frame within zone/extent
    col: Word; // Column of frame within
    relative_line: Integer;
    pix_count: Integer;
    starting_pixel: Integer;
    r : TGIS_FileCADRG_FrameFileIndexRec ;
    colstart : Integer ;
    count : Integer ;
    colmax : Integer ;
    totalpix : Integer ;

    curr_line : Integer ;
    rl,  rt : Integer ;
    pix_in, line_in : Integer ;
    bdxy : TPoint ;
    lines_no : Integer ;
    ldl : Integer ;
    snpixstart : Single ;
    npixstart : Integer ;
    ncurr_line  : Integer ;
    sncurr_line : Single ;
    sdx : Single ;
    sdy : Single ;
    rowcol : Cardinal ;
  begin
    pix_count := 0 ;
    rt := 0 ;
    if not assigned (CADRG.Dct) then begin
      CADRG.Dct := TDictionary<Cardinal, Integer>.Create ;
      for i := 0 to high(CADRG.FrameFileIndexRecords) do begin
        r := CADRG.FrameFileIndexRecords[i] ;
        if r.BoundaryRectangleNumber = iBoundary then begin
          rowcol := (Cardinal(r.FrameLocationRowNumber) shl 16) or
                    (Cardinal(r.FrameLocationColumnNumber))  ;
          if not CADRG.Dct.TryGetValue(rowcol, icounter ) then
            CADRG.Dct.Add(rowcol, i);
        end;
      end;
    end;
   {$IFDEF GIS_NORECORDS}
     Result := new TPoint(0, 0) ;
   {$ELSE}
     Result.X := 0;
     Result.Y := 0;
   {$ENDIF}
    curr_line := _lineStart ;
    if curr_line >= FCellHeight then
      exit ;

    ncurr_line := curr_line ;
    sncurr_line := curr_line ;
    sdx := 1/_scale_x ;
    sdy := 1/_scale_y ;
//Loop - vertical move
    lines_no := 0 ;
    ldl := 0 ;
    while lines_no < _bheight -_top do begin
      rl := _left ;
      if ldl > 0 then
        rt := rt +ldl
      else
        rt := _top +lines_no ;
      Result.X := 0 ;
      Result.Y := 0 ;
      npixstart := _pixStart ;
      snpixstart := npixstart ;

      // Determine which frame corresponds to the _linenr. Orientation of frame rows 0 is
      // Bottom of frame rows, the row 0 for _linenr is top of extent so do the
      // conversion accordingly
      row := ( CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesNS
               - 1
             ) - ( Cardinal(curr_line) div 1536 ) ;
      relative_line := curr_line mod 1536;
      line_in := 1536 -relative_line ;


      // Get the frame that the first X value appears in
      colstart := npixstart div 1536;
      colmax   := ((npixstart +_pixCount +1535) div 1536) -1 ;
      // Calculate a pixel value from the _start byte offset
      starting_pixel := npixstart mod 1536 ;


      // Find the frame in the TOC with this magnification,row,col
      // In some cases the entire frame may not be in the TOC This appears to be
      // how entire Frames are masked. So if it is not found adjust the buffer
      pix_count  := 0     ;
      totalpix := -starting_pixel ;
      ldl := 0 ;

      for col := colstart to colmax do begin

        pix_in := 1536 -starting_pixel ;
        while (npixstart div 1536) < col do begin
          npixstart := npixstart + pix_in ;
          starting_pixel := npixstart mod 1536 ;
          pix_in := 1536 -starting_pixel ;
        end ;
        if (npixstart div 1536) > col then begin
          starting_pixel := npixstart mod 1536 ;
          continue ;
        end;
        rowcol := (Cardinal(row) shl 16) or (Cardinal(col))  ;

        if CADRG.Dct.TryGetValue(rowcol, icounter ) then begin

          if (sdx  < 50) or (pParent.SubLayers.Count < 5) then
            bdxy := CADRG.GetScaledFrameBlockARGB( icounter,
                                          _buffer,
                                          _bwidth,
                                          _bheight,
                                          rl,
                                          rt,
                                          relative_line,
                                          line_in,
                                          starting_pixel ,
                                          pix_in,
                                          _scale_y,
                                          _scale_x
                                        )
          else
            bdxy := CADRG.FillFrameBlockARGB( icounter,
                                          _buffer,
                                          _bwidth,
                                          _bheight,
                                          rl,
                                          rt,
                                          relative_line,
                                          line_in,
                                          starting_pixel ,
                                          pix_in,
                                          _scale_y,
                                          _scale_x
                                        ) ;

          count := bdxy.X ;
          if ldl < bdxy.Y then
            ldl := bdxy.Y ;
          pix_count := pix_count +count ;
          if count > 0 then
            snpixstart := snpixstart +count*sdx
          else
            snpixstart := npixstart + pix_in ;
          npixstart := TruncS(snpixstart) ;
          totalpix := totalpix +pix_in +starting_pixel ;
          if count > 0 then
            rl := rl +count
          else
            rl := _left +TruncS((totalpix -_pixStart)/sdx)  ;
        end
        else begin
          totalpix := totalpix +pix_in +starting_pixel ;
          pix_count := TruncS(totalpix*_scale_x) ;
          snpixstart := snpixstart +pix_in ;
          npixstart := TruncS(snpixstart) ;
          rl := _left +pix_count  ;
        end;
        if npixstart >= (_pixStart +_pixCount)  then begin
          npixstart := 0 ;
          break ;
        end;
        starting_pixel := npixstart mod 1536 ;
        rl := _left+pix_count ;
        pix_in := 1536 ;
      end;

      if (ncurr_line + line_in +relative_line) >= FBitHeight then begin
        ncurr_line := ncurr_line + line_in ;
        Result.X := pix_count ;
        Result.Y := lines_no ;
        exit ;
      end;

      if ldl <> 0 then begin
        sncurr_line :=  sncurr_line +ldl*sdy ;
        ncurr_line := RoundS(sncurr_line) ;
        curr_line := ncurr_line ;
        lines_no := lines_no +ldl ;
      end
      else begin
        sncurr_line :=  sncurr_line +1536 ;
        ncurr_line := RoundS(sncurr_line) ;
        curr_line := ncurr_line ;
        lines_no := TruncS((ncurr_line -_lineStart) * _scale_y) ;
      end;
      if sncurr_line >= FBitHeight then
        break ;

    end;
    Result.X := pix_count ;
    Result.Y := lines_no ;

  end ;




  function  T_LayerCADRG_Child.getDBLine (
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    lcounter: LongWord;
    icounter: Integer;
    row: Word; // Row of frame within zone/extent
    col: Word; // Column of frame within
    relative_line: Integer;
    byte_count: Integer;
    starting_pixel: Integer;
    offset : Integer ;
    maxlc : Integer ;
    minlc : Integer ;
    frame : TGIS_FileCADRG_SimpleFrame ;
    sl : TGIS_FileCADRG_Sublayer ;
  begin
    try
      // Start by filling the entire buffer with transparent values. Some entire
      // frames may be masked
      // ** Not sure how to do this in tatuk yet so fill with Blue for now
      offset   := _offset ;
      icounter := 0;
      while icounter < _bytes do begin
        _buffer[offset] := CADRG.TransparentBlue  ;
        inc(offset);
        _buffer[offset] := CADRG.TransparentGreen ;
        inc(offset);
        _buffer[offset] := CADRG.TransparentRed   ;
        inc(offset);
        icounter := icounter + 3 ;
      end ;

      // Determine which frame corresponds to the _linenr. Orientation of frame rows 0 is
      // Bottom of frame rows, the row 0 for _linenr is top of extent so do the
      // conversion accordingly

      sl := TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]) ;
      CADRG.CurrentLayer := sl ;

      row := Cardinal( sl.NumberOfFramesNS - 1) - ( Cardinal(_linenr) div 1536 ) ;
      relative_line := _linenr mod 1536;

      // Calculate a pixel value from the _start byte offset
      starting_pixel := _start div 3;

      // Get the frame that the first X value appears in
      col := starting_pixel div 1536;

      byte_count  := 0 ;

      minlc := row*sl.NumberOfFramesEW +sl.FrameStartIndex ;
      maxlc := minlc +sl.NumberOfFramesEW -1 ;
      if col <> 0 then begin
        starting_pixel := starting_pixel mod 1536 ;
        inc(minlc, col) ;
      end ;

      for lcounter := minlc to maxlc do begin
        frame := TGIS_FileCADRG_SimpleFrame(
                    CADRG.BDSimpleFramesList.Items[lcounter]
                                        ) ;

        if frame.Available then begin
          byte_count := byte_count +
                        CADRG.GetFrameRowRGB( lcounter,
                                              relative_line,
                                              starting_pixel,
                                              _bytes - byte_count,
                                              _buffer,
                                              _offset + byte_count
                                            ) ;
        end
        else
          byte_count := byte_count + (1536 -starting_pixel)*3 ;
        starting_pixel := 0 ;
        if byte_count >= _bytes then begin
          break ;
        end ;
      end ;

      Result := _bytes;
    except
      raise EGIS_Exception.Create( GIS_RS_ERR_FILEREAD, Path, 0 ) ;
    end ;

  end ;

  function  T_LayerCADRG_Child.getDBLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer;
  var
    lcounter: LongWord;
    icounter: Integer;
    row: Word; // Row of frame within zone/extent
    col: Word; // Column of frame within
    relative_line: Integer;
    pix_count: Integer;
    starting_pixel: Integer;
    offset : Integer ;
    maxlc : Integer ;
    minlc : Integer ;
    frame : TGIS_FileCADRG_SimpleFrame ;
    sl : TGIS_FileCADRG_Sublayer ;
    transp : Integer ;
  begin

    try

      // Start by filling the entire buffer with transparent values. Some entire
      // frames may be masked
      // ** Not sure how to do this in tatuk yet so fill with Blue for now
      offset   := _offset ;
      transp :=  Integer(CADRG.TransparentBlue) or
                (Integer(CADRG.TransparentGreen) shl 08) or
                (Integer(CADRG.TransparentRed)   shl 16) ;
      icounter := 0;
      while icounter < _pixCount do begin
        _buffer[offset +icounter] := transp ;
        inc(icounter) ;
      end ;

      // Determine which frame corresponds to the _linenr. Orientation of frame rows 0 is
      // Bottom of frame rows, the row 0 for _linenr is top of extent so do the
      // conversion accordingly

      sl := TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]) ;
      CADRG.CurrentLayer := sl ;

      row := Cardinal( sl.NumberOfFramesNS - 1) - ( Cardinal(_linenr) div 1536 ) ;
      relative_line := _linenr mod 1536;

      // Calculate a pixel value from the _start byte offset
      starting_pixel := _pixStart;

      // Get the frame that the first X value appears in
      col := starting_pixel div 1536;

      pix_count  := 0 ;

      minlc := row*sl.NumberOfFramesEW +sl.FrameStartIndex ;
      maxlc := minlc +sl.NumberOfFramesEW -1 ;
      if col <> 0 then begin
        starting_pixel := starting_pixel mod 1536 ;
        inc(minlc, col) ;
      end ;

      for lcounter := minlc to maxlc do begin
        frame := TGIS_FileCADRG_SimpleFrame(
                    CADRG.BDSimpleFramesList.Items[lcounter]
                                        ) ;

        if frame.Available then begin
          pix_count := pix_count +
                        CADRG.GetFrameRowARGB( lcounter,
                                              relative_line,
                                              starting_pixel,
                                              _pixCount - pix_count,
                                              _buffer,
                                              _offset + pix_count
                                            ) ;
        end
        else
          pix_count := pix_count + (1536 -starting_pixel) ;
        starting_pixel := 0 ;
        if pix_count >= _pixCount then begin
          break ;
        end ;
      end ;

      Result := _pixCount;
    except
      raise EGIS_Exception.Create( GIS_RS_ERR_FILEREAD, Path, 0 ) ;
    end ;

  end ;

  procedure T_LayerCADRG_Child.setUp;
  var
    str_scale     : TStringBuilder ;
    int_str_scale : TStringBuilder ;
    ms            : TGIS_FileCADRG_Map ;
    msa           : TGIS_FileCADRG_Map ;
    i             : Integer ;
    ls            : Integer ;
    ims           : String ;
  begin
    CADRG := pParent.CADRG ;

    internalTransparentColor := TGIS_Color.FromARGB(0,  CADRG.TransparentRed,
                                                        CADRG.TransparentGreen,
                                                        CADRG.TransparentBlue ) ;

    if assigned(CADRG.BDSubLayersList) then begin
      // Set up some of the header information
      FBitHeight := 1536 *
         TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).NumberOfFramesNS ;
      FBitWidth  := 1536 *
          TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).NumberOfFramesEW ;
      realBitCount  := 24;
      realLineWidth := 3 * FBitWidth; //3 bytes rgb x width
//      intBitCount   := realBitCount  ;
      intLineWidth  := realLineWidth ;
      MapCadrg    := TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).MapCADRG ;

      // setup internal representation header

      // Set up the extent for this layer
      FExtent.XMin :=
        TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).SWLowerLeftLongitude  ;
      FExtent.XMax :=
        TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).NEUpperRightLongitude ;
      FExtent.YMax :=
        TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).NEUpperRightLatitude   ;
      FExtent.YMin :=
        TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).SWLowerLeftLatitude   ;

      scaleX :=
        TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).LongitudeHorizontalInterval ;
      scaleY :=
        -TGIS_FileCADRG_Sublayer(CADRG.BDSubLayersList.Items[iBoundary]).LatitudeVerticalInterval ;
    end
    else begin
      // Set up some of the header information
      FBitHeight := 1536 *
         pParent.CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesNS ;
      FBitWidth  := 1536 *
          pParent.CADRG.BoundaryRectangleRecords[iBoundary].NumberOfFramesEW ;
      realBitCount  := 24;
      realLineWidth := 3 * FBitWidth; //3 bytes rgb x width
//      intBitCount   := realBitCount  ;
      intLineWidth  := realLineWidth ;

      // setup internal representation header

      // Set up the extent for this layer
      FExtent.XMin :=
        pParent.CADRG.BoundaryRectangleRecords[iBoundary].SWLowerLeftLongitude  ;
      FExtent.XMax :=
        pParent.CADRG.BoundaryRectangleRecords[iBoundary].NEUpperRightLongitude ;
      FExtent.YMax :=
        pParent.CADRG.BoundaryRectangleRecords[iBoundary].NEUpperRightLatitude   ;
      FExtent.YMin :=
        pParent.CADRG.BoundaryRectangleRecords[iBoundary].SWLowerLeftLatitude   ;

      ims := Trim(ConvertAnsiString( pParent.CADRG.BoundaryRectangleRecords[iBoundary].ScaleOrResolution )) ;

      for i := StringFirst to StringLast(ims) do begin
        if (ims[i] = #0) or (ims[i] = ' ') then begin
          SetLengthStr(ims, i) ;
          break ;
        end ;
      end ;

      str_scale := TStringBuilder.Create( UpperCase(Trim(ims))) ;
      int_str_scale := TStringBuilder.Create ;
      try
        ls := str_scale.Length-1;

        ms := TGIS_FileCADRG_Map.Create ;

        if str_scale[StringFirst] = ':' then begin //big scale - CADRG
          for i := 2 to ls - 1 do
            int_str_scale.Append( str_scale[i] ) ;
          if str_scale[ls] = 'M' then
            int_str_scale.Append( '000000' )
          else
          if str_scale[ls] = 'K' then
            int_str_scale.Append( '000' ) ;

          ms.ProductNameId := 0 ;
          ms.ProductName := 'CADRG' ;
        end
        else begin //CIB
          for i := 0 to ls - 1 do begin
            if (str_scale[i] >= '0') and (str_scale[i] <= '9') then
              int_str_scale.Append( str_scale[i] )
            else begin
              FreeObject( int_str_scale ) ;
              int_str_scale := TStringBuilder.Create( '33333' );
              break ;
            end ;
          end ;

          ms.ProductNameId := 1 ;
          ms.ProductName := 'CIB' ;
        end ;

        try
          dScale := TruncS(DotStrToFloat(int_str_scale.ToString)) ;
        except
          dScale := 0 ;
        end ;
      finally
        FreeObject( int_str_scale ) ;
        FreeObject( str_scale ) ;
      end ;

      if dScale <> 0 then
        dMinScale := 0.5 / dScale ;

      ms.Scale := dScale ;
      ms.SeriesCode   := '  ' ;
      ms.SeriesCodeId := 0 ;
      msa := CADRG.MapSeriesCadrg.AddUniqueMap(ms) ;
      msa.Enabled := True ;
      MapCadrg    := msa ;

      if msa <> ms then begin
        FreeObject( ms ) ;
      end ;

      scaleX := pParent.CADRG.BoundaryRectangleRecords[iBoundary].LongitudeHorizontalInterval ;
      scaleY := -pParent.CADRG.BoundaryRectangleRecords[iBoundary].LatitudeVerticalInterval ;
    end ;

    if scaleX <> 0 then
      FBitWidth :=   RoundS(( Extent.XMax - Extent.XMin ) / scaleX)  ;
    if scaleY <> 0 then
      FBitHeight := - RoundS(( Extent.YMax - Extent.YMin ) / scaleY)  ;

    inherited ;
  end ;

procedure T_LayerCADRG_Child.fset_MinHeight( const _value : Single
                                        ) ;
begin
  if FMinZ <> _value  then begin
    FMinZ := _value ;
    modifiedMinHeight := True ;
  end;
end;

procedure T_LayerCADRG_Child.fset_MaxHeight( const _value : Single
                                        ) ;
begin
  if FMaxZ <> _value  then begin
    FMaxZ := _value ;
    modifiedMinHeight := True ;
  end;
end;

  function  T_LayerCADRG_Child.getBitmapData(
    const _extent   : TGIS_Extent ;
    const _bitmap   : TGIS_Pixels ;
    const _width    : Integer ;
    const _height   : Integer
  ) : Boolean ;
  var
    buf : TGIS_Pixels ;
    oheight, owidth : Integer ;
    iheight, iwidth : Integer ;
    iwidth3, wheight : Integer ;
    width, height : Integer ;
    osx, osy : Double ;
    sosx, sosy : Double ;
    isx, isy : Double ;
    il, it, ol, ot : Integer ;
    vext : TGIS_Extent ;
    parext : TGIS_Extent ;
    maxrow : Integer ;
    line, i : Integer ;
    xscale, oxscale   : Single ;
    yscale, oyscale  : Single ;
    buf_off : Integer ;
    resize : Boolean ;
    bxy : TPoint ;
    sl, st : Integer ;
    width_buf, height_buf : Integer ;
    procedure fast_scalebitmap( const _buf_in    : TGIS_Pixels ;
                                const _w_buf_in  : Integer ;
                                const _h_buf_in  : Integer ;
                                const _buf_out   : TGIS_Pixels ;
                                const _w_buf_out : Integer ;
                                const _h_buf_out : Integer ;
                                const _left      : Integer ;
                                const _top       : Integer
                               ) ;
    var
      o_ii, o_kk : Integer ;
      ir, ic : Integer ;
      scale_y, scale_x : Single ;
      i_lidx, o_lidx : Integer ;
    begin
      scale_y := _h_buf_out/_h_buf_in ;
      scale_x := _w_buf_out/_w_buf_in ;
      for o_ii := 0 to _h_buf_out -1 do begin
        ir := TruncS((o_ii +_top)/scale_y) ;
        if ir >= _h_buf_in then
          ir := _h_buf_in -1 ;
        i_lidx :=ir*_w_buf_in ;
        o_lidx :=o_ii*_w_buf_out ;
        for o_kk := 0 to _w_buf_out -1 do begin
           ic := TruncS((o_kk +_left)/scale_x) ;
           if ic >= _w_buf_in then
             ic := _w_buf_in -1 ;
           if  _buf_in[i_lidx +ic] and Integer($FF000000) <> 0 then
             _buf_out[o_lidx +o_kk] := _buf_in[i_lidx +ic] ;
        end ;
      end ;
    end ;
  begin
    Result := True ;

    if (_extent.XMin > FExtent.XMax) or
       (_extent.XMax < FExtent.XMin) or
       (_extent.YMin > FExtent.YMax) or
       (_extent.YMax < FExtent.YMin) then
      exit ;

    if ( FCellWidth  = 0 ) or
       ( FCellHeight = 0 ) then
      exit ;
    if( _width <= 0) or (_height <= 0) then
      exit ;

    parext :=  pParent.Extent ;
    height := _height ;
    width  := _width ;

    isx := (FExtent.XMax -FExtent.XMin)/baseCellWidth ;
    isy := (FExtent.YMax -FExtent.YMin)/baseCellHeight ;

    osx := (_extent.XMax -_extent.XMin)/width  ;
    osy := (_extent.YMax -_extent.YMin)/height ;

    {$IFDEF GIS_NORECORDS}
      vext := new TGIS_Extent ;
    {$ENDIF}
    if _extent.XMin >= FExtent.XMin then begin
      vext.XMin := _extent.XMin ;
      ol := 0 ;
      il := TruncS((_extent.XMin -FExtent.XMin)/isx) ;
      if osx < isx then
        sosx := isx -(_extent.XMin -FExtent.XMin -il*isx)
      else
        sosx := isx ;

      if sosx < 0 then
        sosx := 0 ;

    end
    else begin
      vext.XMin := FExtent.XMin ;
      il := 0 ;
      ol := TruncS((FExtent.XMin - _extent.XMin)/osx);
      if osx < isx then
        sosx := osx
      else
        sosx := isx ;
    end ;

    if _extent.YMin > FExtent.YMin then
      vext.YMin := _extent.YMin
    else
      vext.YMin := FExtent.YMin ;

    if _extent.XMax < FExtent.XMax then
      vext.XMax := _extent.XMax
    else
      vext.XMax  := FExtent.XMax ;

    if _extent.YMax <= FExtent.YMax then begin
      vext.YMax := _extent.YMax ;
      it := TruncS((FExtent.YMax -_extent.YMax)/isy) ;
      ot := 0 ;
      if osy < isy then
        sosy :=  isy -(FExtent.YMax -it*isy -_extent.YMax)
      else
        sosy := isy ;
    end
    else begin
      vext.YMax  := FExtent.YMax ;
      it := 0 ;
      ot := TruncS((_extent.YMax -FExtent.YMax)/osy);
      if osy < isy then
        sosy := osy
      else
        sosy := isy ;
    end ;

    iwidth  := RoundS((vext.XMax -vext.XMin)/isx);
    iheight := RoundS((vext.YMax -vext.YMin)/isy);

    while (vext.XMax  -vext.XMin +isx) > (iwidth*isx) do begin
      if (il +iwidth) < baseCellWidth then
        inc(iwidth)
      else
        break ;
    end ;

    while (vext.YMax - vext.YMin +isy) > (iheight*isy)  do begin
      if (it +iheight) < baseCellHeight then
        inc(iheight)
      else
        break ;
    end ;

    owidth  := RoundS((vext.XMax -vext.XMin)/osx);

    while (ol +owidth) <  TruncS((vext.XMax - _extent.XMin)/osx) do begin
      if (ol +owidth) < width then
        inc(owidth)
      else
        break ;
    end ;

   oheight := RoundS((vext.YMax -vext.YMin)/osy);

    while (ot +oheight) <  TruncS((_extent.YMax -FExtent.YMin)/osy) do begin
      if (ot +oheight) < height then
        inc(oheight)
      else
        break ;
    end ;

    maxrow := baseCellHeight -1;


    wheight := 0 ;
    line := it ;
    iwidth3 := iwidth * 3 ;

    xscale := isx/osx ;
    yscale := isy/osy ;
    oxscale := xscale ;
    oyscale := yscale ;
    resize := False ;



    if xscale >= yscale then begin
      if xscale > 1.0 then begin
        yscale := yscale/xscale ;
        xscale := 1.0 ;
        resize := True ;
      end;
    end
    else begin
      if yscale > 1.0 then begin
        xscale := xscale/yscale ;
        yscale := 1.0 ;
        resize := True ;
      end;
    end;

    if resize then begin
      width_buf := RoundS(_width *(xscale/oxscale)) ;
      if width_buf = 0 then
        inc(width_buf) ;
      height_buf := RoundS(_height*(yscale/oyscale)) ;
      if height_buf = 0 then
        inc(height_buf) ;
      ol := RoundS(ol*(xscale/oxscale)) ;
      ot := RoundS(ot*(yscale/oyscale)) ;
      SetLength( buf, width_buf*height_buf) ;
      buf_off := 0 ;
      for i := 0 to width_buf*height_buf -1 do
        buf[i] := $00FFFFFF ;

      bxy := getScaledBlockPixels(buf, width_buf, height_buf , ol, ot, line,
                                    iheight, il, iwidth, yscale, xscale) ;
      sl := TruncS((isx -sosx)/osx) ;
      st := TruncS((isy -sosy)/osy) ;
      fast_scalebitmap( buf, width_buf, height_buf,
                        _bitmap, _width, _height, sl, st ) ;
      SetLength(buf, 0) ;
    end
    else begin
      buf := _bitmap ;
      width_buf := _width ;
      height_buf := _height ;
      bxy := getScaledBlockPixels(buf, width_buf, height_buf , ol, ot, line,
                                    iheight, il, iwidth, yscale, xscale) ;
    end;

  end ;

  function T_LayerCADRG_Child.Locate(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean
   ) : Boolean ;
  begin
    Result := inherited Locate( _ptg, _rgbMapped, _nativesVals, _transparency );
  end ;

  function T_LayerCADRG_Child.LocateEx(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean          ;
    const _pixelsize    : Double
   ) : Boolean ;
  begin
    Result := inherited LocateEx( _ptg, _rgbMapped, _nativesVals,
                                  _transparency, _pixelsize
                                );
  end ;


//==============================================================================
// TGIS_LayerCADRG
//==============================================================================

  constructor TGIS_LayerCADRG.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
  end ;

  procedure TGIS_LayerCADRG.doDestroy ;
  begin
    FreeObject( CADRG ) ;

    inherited;
  end ;

  procedure TGIS_LayerCADRG.setUp ;
  var
    i, maxi    : Integer            ;
    childlayer : T_LayerCADRG_Child ;
    scstr      : String ;
  begin
    try

      FBandsCount := 3 ;
      Params.Pixel.RedBand   := 1 ;
      Params.Pixel.GreenBand := 2 ;
      Params.Pixel.BlueBand  := 3 ;

      if not assigned(FOnReadFrame) then
        CADRG := TGIS_FileCADRGDecoder.Create(Path)
      else begin
        CADRG := TGIS_FileCADRGDecoder.Create(FOnReadFrame) ;
      end ;

      // Create a list where we will stored an ordered list of layers
      SubLayers := TGIS_LayerAbstractList.Create( False );

      //Cycle through each of the Boundary records and set up new instances of
      // this class for each one. Only the root class parses the the TOC and
      // configures subordinate array of TGIS_LAYERCADRG objects. Boundary
      // records are a 0 based array

      if assigned(CADRG.BDSubLayersList) then
        maxi := CADRG.BDSubLayersList.Count -1
      else
        maxi := high( CADRG.BoundaryRectangleRecords ) ;

      FExtent.XMin := GIS_MAX_DOUBLE ;
      FExtent.YMin := GIS_MAX_DOUBLE ;
      FExtent.XMax := -GIS_MAX_DOUBLE ;
      FExtent.YMax := -GIS_MAX_DOUBLE ;

      scaleX := GIS_MAX_DOUBLE ;
      scaleY := -GIS_MAX_DOUBLE ;

      for i := 0 to maxi do begin
        // Make sure the extent value is valid. The Overview and legend images
        // are in here but have $ffffffffffffffff as the bounding rectangle
        // points so ignore them

        if not assigned(CADRG.BDSubLayersList) then begin
          scstr := Trim( ConvertAnsiString( CADRG.BoundaryRectangleRecords[i].ScaleOrResolution )) ;
          if ((scstr[StringFirst] < '0') or (scstr[StringFirst] > '9')) and
             (maxi > 0) then
            continue ;
        end ;
        // Set this instance to point to the first valid BoundaryRecordIndex
        childlayer := T_LayerCADRG_Child.Create;
        childlayer.Path      := ''   ;
        childlayer.pParent   := Self ;
        childlayer.iBoundary := i    ;
        childlayer.Name      := IntToStr(i) ;
        childlayer.Caption   := IntToStr(i) ;
        childlayer.setUp ;
        childlayer.SetCSByEPSG( GIS_EPSG_WGS84 ) ;
        Extent := GisMaxExtent(Extent, childlayer.Extent ) ;
        scaleX  := Min(scaleX, childlayer.scaleX ) ;
        scaleY  := Max(scaleY, childlayer.scaleY ) ;
        SubLayers.Add( childlayer ) ;
      end ;
      baseCellWidth := RoundS((Extent.XMax -Extent.XMin)/scaleX) ;
      FCellWidth := baseCellWidth ;
      FBitWidth  := baseCellWidth ;

      baseCellHeight  := RoundS((Extent.YMin -Extent.YMax)/scaleY) ;
      FCellHeight := baseCellHeight ;
      FBitHeight := baseCellHeight ;

      FPixelSize := GisPoint(
                              ( FExtent.XMax - FExtent.XMin ) / FBitWidth,
                              ( FExtent.YMax - FExtent.YMin ) / FBitHeight
                            ) ;

      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          java.util.Collections.sort( SubLayers, new T_listSortComparLayers ) ;
        {$ELSE}
          SubLayers.Sort( @compare_layers );
        {$ENDIF}
      {$ELSE}
        SubLayers.Sort( TComparer<TGIS_LayerAbstract>.Construct( compare_layers ) );
      {$ENDIF}

      // This layer class doesn't use layer config files so disable it before calling
      // inherited setup. If this is not done the transparency color mask ranges we setup
      // internal to this class get destroyed.
      UseConfig := false;
//      specialImport := True ;

      SetCSByEPSG( GIS_EPSG_WGS84 ) ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := 'CADRG - Compressed ARC Digitized Raster Graphics (TOC)'    +
                   #13#10                                                      +
                   'CADRG support was written by Tim Ranger for the Canadian ' +
                   'Department of National Defense and is provided ''as-is'' ' +
                   'without warranty. DND assumes no liability for damages or '+
                   'financial loss incurred in the use of this software. '
    except
      raise EGIS_Exception.Create( GIS_RS_ERR_FILEREAD, Path, 0) ;
    end ;

  end ;

  procedure TGIS_LayerCADRG.fset_Interpretation (
                  const _value : TGIS_LayerPixelInterpretation
                                                ) ;
  begin
    FInterpretation := _value ;
    case FInterpretation of
      TGIS_LayerPixelInterpretation.Grid :
        begin
          rgbAsGrid := True ;
          setGridImage(rgbAsGrid) ;
        end;
      TGIS_LayerPixelInterpretation.Pixel :
        begin
          rgbAsGrid := False ;
          setGridImage(rgbAsGrid) ;
        end
      else begin
        FInterpretation := TGIS_LayerPixelInterpretation.Default ;
        rgbAsGrid := False ;
        if GridBand = 0 then
          FIsGridImage := rgbAsGrid ;
        setGridImage(FIsGridImage) ;
     end;
    end;
  end ;

procedure TGIS_LayerCADRG.fset_GridImage(
  const _value : Boolean
) ;
var
  pixparams : TGIS_ParamsSectionPixel ;
begin
  pixparams := TGIS_ParamsSectionPixel(Params) ;
  if (FIsGridImage <> _value) or (pixparams.Pixel.GridBand <> FGridBand)
  then begin
    FIsGridImage := _value ;
    if FIsGridImage then begin
      if pixparams.Pixel.GridBand > FBandsCount then
        pixparams.Pixel.GridBand := 1 ;
      FGridBand :=  pixparams.Pixel.GridBand ;
    end else
      pixparams.Pixel.GridBand := 0 ;

    if FIsGridImage then begin
      if  (pixparams.Pixel.GridBand = 0) and (not rgbAsGrid) then begin
        pixparams.Pixel.GridBand := 1 ;
        FGridBand :=  pixparams.Pixel.GridBand ;
      end;
    end;
    setGridImage(FIsGridImage) ;
    setupParams ;
  end ;

  if IsGridImage then
    View3D.Mode := TGIS_3DLayerType.Dem ;
end ;


  procedure TGIS_LayerCADRG.setGridImage( const _isgrid : Boolean ) ;
  var
    i  : Integer            ;
  begin
    FIsGridImage := _isgrid ;
    if assigned(SubLayers) then
      for i:=0 to SubLayers.Count - 1 do
        T_LayerCADRG_Child( SubLayers[ i ]).IsGridImage := _isgrid ;
  end ;

  function TGIS_LayerCADRG.getMapSeries : TGIS_FileCADRG_MapSeries ;
  begin
    if assigned(CADRG) then
      Result := CADRG.MapSeriesCadrg
    else
      Result := nil ;
  end ;

  procedure TGIS_LayerCADRG.ReadConfig ;
  var
    i  : Integer  ;
  begin
    inherited ;

    if assigned( CADRG ) then begin
//      internalTransparentColor.rgbBlue     := CADRG.TransparentBlue  ;
//      internalTransparentColor.rgbGreen    := CADRG.TransparentGreen ;
//      internalTransparentColor.rgbRed      := CADRG.TransparentRed   ;
//      internalTransparentColor.rgbReserved := $FF ;

      internalTransparentColor     := TGIS_Color.FromARGB(0,
                                        CADRG.TransparentRed,
                                        CADRG.TransparentGreen,
                                        CADRG.TransparentBlue)  ;

      for i:=0 to SubLayers.Count - 1 do
        T_LayerCADRG_Child( SubLayers[ i ]).internalTransparentColor :=
                    internalTransparentColor ;
    end ;
  end ;

  function TGIS_LayerCADRG.Locate(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean
  ) : Boolean ;
  var
    i   : Integer            ;
    ll  : T_LayerCADRG_Child ;
    isresult : Boolean ;
  begin
    Result := False ;
    isresult := False ;

    if ( _ptg.X < Extent.XMin ) or ( _ptg.X > Extent.XMax ) or
       ( _ptg.Y < Extent.YMin ) or ( _ptg.Y > Extent.YMax )
    then
      exit ;

    for i:= SubLayers.Count - 1 downto 0 do begin
      ll := T_LayerCADRG_Child( SubLayers[ i ] ) ;

      ll.FViewer := Viewer  ;

      if ( _ptg.X < ll.Extent.XMin ) or ( _ptg.X > ll.Extent.XMax ) or
         ( _ptg.Y < ll.Extent.YMin ) or ( _ptg.Y > ll.Extent.YMax )
      then
        continue ;

      if ll.Active then begin
        ll.Params.Assign( Params );
        Result := ll.Locate( _ptg, _rgbMapped, _nativesVals, _transparency ) ;
      end;
      if Result then begin
        isresult := Result ;
        if not _transparency then
          break ;
      end;
    end ;
    Result := isresult ;
  end ;


  function TGIS_LayerCADRG.LocateEx(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean          ;
    const _pixelsize    : Double
  ) : Boolean ;
  var
    i   : Integer            ;
    ll  : T_LayerCADRG_Child ;
    isresult : Boolean ;
  begin
    Result := False ;
    isresult := False ;

    if ( _ptg.X < Extent.XMin ) or ( _ptg.X > Extent.XMax ) or
       ( _ptg.Y < Extent.YMin ) or ( _ptg.Y > Extent.YMax )
    then
      exit ;

    for i:= SubLayers.Count - 1 downto 0 do begin
      ll := T_LayerCADRG_Child( SubLayers[ i ] ) ;

      ll.FViewer := Viewer  ;

      if ( _ptg.X < ll.Extent.XMin ) or ( _ptg.X > ll.Extent.XMax ) or
         ( _ptg.Y < ll.Extent.YMin ) or ( _ptg.Y > ll.Extent.YMax )
      then
        continue ;

      if ll.Active then begin
        ll.Params.Assign( Params );
        Result := ll.LocateEx( _ptg, _rgbMapped, _nativesVals,
                               _transparency, _pixelsize
                             ) ;
      end;
      if Result then begin
        isresult := Result ;
        if not _transparency then
          break ;
      end;
    end ;
    Result := isresult ;
  end ;

  function  TGIS_LayerCADRG.getBitmapData(
    const _extent   : TGIS_Extent ;
    const _bitmap   : TGIS_Pixels ;
    const _width    : Integer ;
    const _height   : Integer
  ) : Boolean ;
  var
    fullext : TGIS_Extent ;
    r       : Boolean ;
    i       : Integer            ;
    ll      : T_LayerCADRG_Child ;
  begin
    Result := True ;
    fullext := Extent ;

    for i:= 0 to SubLayers.Count - 1 do begin

      ll := T_LayerCADRG_Child( SubLayers[ i ] ) ;
      FreeObject(ll.CADRG.Dct) ;

      ll.FViewer := Viewer  ;

      if not GisIsCommonExtent( _extent, ll.Extent ) then
        continue ;
      if ll.Active then begin
        ll.Params.Assign( Params );
        r := ll.getBitmapData(_extent, _bitmap, _width, _height ) ;
      end;
    end ;
    if (bandsMap[0] <> 0 ) or ( bandsMap[1] <> 1) or (bandsMap[2] <> 2) or
       (bandsMap[3] <> 3)
    then
      finalARGBMap(_bitmap, _width, _height) ;
  end ;

  function  TGIS_LayerCADRG.getGridData( const _extent       : TGIS_Extent      ;
                                         const _grid         : TGIS_GridArray
                                         ) : Boolean ;
  var
    bmp      : TGIS_Pixels ;
    tw, th   : Integer ;
    gg       : Boolean ;
    bandno  : Integer ;
    i, k    : Integer ;
    sval    : Single ;
  const
    MAX_LINES = 900 ;
    FACTOR_RED = 0.35 ;
    FACTOR_GREEN = 0.45 ;
    FACTOR_BLUE = 0.2  ;
  begin
    if FInterpretation = TGIS_LayerPixelInterpretation.Grid then begin
      gg := True ;
      bandno := 0 ;
    end
    else begin
      gg := False ;
      bandno := Params.Pixel.GridBand ;
    end;

    if FMinZ >= FMaxZ then begin
      if FBitHeight > MAX_LINES then
        th := MAX_LINES
      else
        th := FBitHeight ;
      FMinZ :=  GIS_MAX_SINGLE ;
      FMaxZ := -GIS_MAX_SINGLE ;

      tw := RoundS((th*FBitWidth)/FBitHeight) ;
      SetLength(bmp, tw * th ) ;
      getBitmapData(FExtent, bmp, tw, th) ;
      if gg then begin
        for i := low(bmp) to high(bmp) do begin
          if (bmp[i] and (Integer($FF000000))) = 0 then
            continue ;
          sval := (((bmp[i] shr 16) and $FF) * FACTOR_RED) +
                  (((bmp[i] shr 08) and $FF) * FACTOR_GREEN) +
                  (((bmp[i] shr 00) and $FF) * FACTOR_BLUE) ;
          if sval > FMaxZ then
            FMaxZ := sval ;
          if sval < FMinZ then
            FMinZ := sval ;
        end;
      end
      else begin
        case bandno of
          1 :
            begin
              for i := low(bmp) to high(bmp) do begin
                if (bmp[i] and (Integer($FF000000))) = 0 then
                  continue ;
                sval := (((bmp[i] shr 16) and $FF) * FACTOR_RED) ;
                if sval > FMaxZ then
                  FMaxZ := sval ;
                if sval < FMinZ then
                  FMinZ := sval ;
              end;

            end;
          2 :
            begin
              for i := low(bmp) to high(bmp) do begin
                if (bmp[i] and (Integer($FF000000))) = 0 then
                  continue ;
                sval := (((bmp[i] shr 08) and $FF) * FACTOR_GREEN) ;
                if sval > FMaxZ then
                  FMaxZ := sval ;
                if sval < FMinZ then
                  FMinZ := sval ;
              end;

            end;
          3 :
            begin
              for i := low(bmp) to high(bmp) do begin
                if (bmp[i] and (Integer($FF000000))) = 0 then
                  continue ;
                sval := (((bmp[i] shr 00) and $FF) * FACTOR_BLUE) ;
                if sval > FMaxZ then
                  FMaxZ := sval ;
                if sval < FMinZ then
                  FMinZ := sval ;
              end;
            end;
        end;
      end;
      SetLength(bmp, 0) ;
      if assigned(SubLayers) then
        for i:=0 to SubLayers.Count - 1 do begin
          T_LayerCADRG_Child( SubLayers[ i ]).fset_MinHeight(FMinZ) ;
          T_LayerCADRG_Child( SubLayers[ i ]).fset_MaxHeight(FMaxZ) ;
        end;

    end;
    tw := length(_grid[0]) ;
    th := length(_grid) ;
    SetLength(bmp, tw * th ) ;
    getBitmapData(_extent, bmp, tw, th) ;

      if gg then begin
        for i := low(_grid) to high(_grid) do begin
          for k := low(_grid[0]) to high(_grid[0]) do
            if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
              _grid[i, k] := GIS_GRID_NOVALUE
            else
              _grid[i, k] := (((bmp[i*tw +k] shr 16) and $FF) * FACTOR_RED) +
                             (((bmp[i*tw +k] shr 08) and $FF) * FACTOR_GREEN) +
                             (((bmp[i*tw +k] shr 00) and $FF) * FACTOR_BLUE) ;
        end;
      end
      else begin
        case bandno of
          1 :
            begin
              for i := low(_grid) to high(_grid) do begin
                for k := low(_grid[0]) to high(_grid[0]) do
                  if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                    _grid[i, k] := GIS_GRID_NOVALUE
                  else
                    _grid[i, k] := (((bmp[i*tw +k] shr 16) and $FF) * FACTOR_RED) ;
              end;
            end;
          2 :
            begin
              for i := low(_grid) to high(_grid) do begin
                for k := low(_grid[0]) to high(_grid[0]) do
                  if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                    _grid[i, k] := GIS_GRID_NOVALUE
                  else
                    _grid[i, k] := (((bmp[i*tw +k] shr 08) and $FF) * FACTOR_GREEN) ;
              end;
            end;
          3 :
            begin
              for i := low(_grid) to high(_grid) do begin
                for k := low(_grid[0]) to high(_grid[0]) do
                  if (bmp[i*tw +k] and (Integer($FF000000))) = 0 then
                    _grid[i, k] := GIS_GRID_NOVALUE
                  else
                    _grid[i, k] := (((bmp[i*tw +k] shr 00) and $FF) * FACTOR_BLUE) ;
              end;
            end;
        end;
      end;
      SetLength(bmp, 0) ;
      if True then

      Result := True ;
  end;



  procedure TGIS_LayerCADRG.DoReload ;
  var
    i, maxi    : Integer ;
    childlayer : T_LayerCADRG_Child ;
  begin
    if not assigned( FOnReadFrame ) then
      exit ;

    if assigned(CADRG) then begin
      FExtent.XMin := GIS_MAX_DOUBLE ;
      FExtent.YMin := GIS_MAX_DOUBLE ;
      FExtent.XMax := -GIS_MAX_DOUBLE ;
      FExtent.YMax := -GIS_MAX_DOUBLE ;

      CADRG.DoReload ;

      // Create a list where we will stored an ordered list of layers
      SubLayers.Clear ;

      //Cycle through each of the Boundary records and set up new instances of
      // this class for each one. Only the root class parses the the TOC and
      // configures subordinate array of TGIS_LAYERCADRG objects. Boundary
      // records are a 0 based array

      maxi := CADRG.BDSubLayersList.Count -1 ;
      for i := 0 to maxi do begin
        // Make sure the extent value is valid. The Overview and legend images
        // are in here but have $ffffffffffffffff as the bounding rectangle
        // points so ignore them
        // Set this instance to point to the first valid BoundaryRecordIndex
        childlayer := T_LayerCADRG_Child.Create;
        childlayer.Path      := ''   ;
        childlayer.pParent   := Self ;
        childlayer.iBoundary := i    ;
        childlayer.setUp ;
        Extent := GisMaxExtent(Extent, childlayer.Extent ) ;
        SubLayers.Add( childlayer ) ;
      end ;
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          java.util.Collections.sort( SubLayers, new T_listSortComparLayers ) ;
        {$ELSE}
          SubLayers.Sort( @compare_layers );
        {$ENDIF}
      {$ELSE}
        SubLayers.Sort( TComparer<TGIS_LayerAbstract>.Construct( compare_layers ) );
      {$ENDIF}
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerCADRG.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-CADRG', 'CADRG - Compressed ARC Digitized Raster Graphics (TOC)',
                   TGIS_LayerCADRG, '.toc',
                   TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerCADRG.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

