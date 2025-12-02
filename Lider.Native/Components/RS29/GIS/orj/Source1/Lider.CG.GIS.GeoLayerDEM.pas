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
  Encapsulation of an USGS DEM (Digital Elevation Models) ASCII Grid layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerDEM ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerDEM"'}
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

    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,

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
  GisLayerDEM = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of an USGS DEM Ascii Grid layer.
  /// </summary>
  TGIS_LayerDEM = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private
        /// <summary>
        ///   DEM level code.
        /// </summary>
        lvl : Integer ;

        /// <summary>
        ///   Accuracy code for elevation.
        /// </summary>
        cData : Integer ;

        /// <summary>
        ///   Vertical spatial resolution.
        /// </summary>
        resZ      : Double ;

        sizeOfBRecord : Integer ;

        /// <summary>
        ///   Grid array - for ASCII file
        /// </summary>
        arGrid : TGIS_GridArray ;

        /// <summary>
        ///   Line buffer (line of singles).
        /// </summary>
        lineBuffer : array of Single ;

        /// <summary>
        ///   Number of lines present in lineBuffer.
        /// </summary>
        lineInBuffer  : Integer ;

        /// <summary>
        ///   vertical units.
        /// </summary>
        untVertical : Integer ;

        /// <summary>
        ///   vertical units.
        /// </summary>
        untVerticalEPSG : Integer ;

        /// <summary>
        ///   horizontal units.
        /// </summary>
        untHorizontal : Integer ;

        /// <summary>
        ///   horizontal units.
        /// </summary>
        untHorizontalEPSG : Integer ;

        /// <summary>
        ///   datum.
        /// </summary>
        datumCode : Integer ;

        /// <summary>
        ///   coordinate system.
        /// </summary>
        coordSystem  :Integer ;

        utmZone : Integer ;

        /// <summary>
        ///   True if is new format with projection.
        /// </summary>
        newFormat : Boolean ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <summary>
      ///   Set needful parameters.
      /// </summary>
      procedure setHdrInfo        ;

      /// <summary>
      ///   Set projection.
      /// </summary>
      procedure setProjection     ;

      /// <inheritdoc/>
      procedure setUp             ; override;

      /// <summary>
      ///   Read the data form the file into the memory.
      /// </summary>
      procedure prepareImage      ;

      /// <inheritdoc/>
      function  getLine         ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer; override  ;

      /// <inheritdoc/>
      function  getNativeValue    ( const _pt  : TPoint  ;
                                    const _ar  : TGIS_DoubleArray
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; override;
    protected

      procedure doDestroy ; override;

    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      function  PreRecognize     ( const _path     : String ;
                                     var _new_path : String
                                  ) : Boolean ; override;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoCsProjections ;
{$ENDIF}

const
  USGS_DEM_NODATA     = -32767 ;
  BASE_RECORD_SIZE    =   1024 ;
  ARECORD_SIZE        =   894 ;
  BRECORD_HEADER_SIZE =    144 ;

  // Offsets
  OFFSET_VERTUNITS    = 528 ;
  OFFSET_HORZUNITS    = 534 ;
  OFFSET_COORDINATES  = 546 ;
  OFFSET_MINELEVATION = 738 ;
  OFFSET_MAXELEVATION = 762 ;
  OFFSET_ELEVATIONACC = 810 ;
  OFFSET_XRESULUTION  = 816 ;
  OFFSET_YRESOLUTION  = 828 ;
  OFFSET_VERTSPATRES  = 840 ;
  OFFSET_ROWSNUMBER   = 852 ;
  OFFSET_COLSNUMBER   = 858 ;

//==============================================================================
// TGIS_LayerDEM
//==============================================================================

  constructor TGIS_LayerDEM.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    FIsGridImage := True;
    FIsNativeGridImage := True ;
    FBandsCount := 1 ;
  end ;

  procedure TGIS_LayerDEM.doDestroy ;
  begin
    Dormant ;

    SetLength( arGrid, 0, 0 ) ;

    inherited ;
  end ;

  procedure TGIS_LayerDEM.setHdrInfo ;
  var
    ext : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    i, j, k : Integer ;
    inbuf : Array [0..1024] of Byte ;
    corner : Array [0..3] of Array[0..1] of Double ;
    workStr : String ;
    nRows, nCols : Integer ;

    procedure setWorkStr(_idx, _length : Integer) ;
    var
      ii : Integer ;
      sb : TStringBuilder ;
    begin
      sb := TStringBuilder.Create ;
      try

        sb.Length := _length ;

        for ii := 0 to _length - 1 do begin
          if inbuf[_idx] = Byte('D') then
            sb[ii] := 'E'
          else
            sb[ii] := Char(inbuf[_idx]) ;

          inc(_idx) ;
        end ;

      finally
        workStr := Trim(sb.ToString) ;
        FreeObject( sb ) ;
      end ;
    end ;

  begin
    {$IFDEF OXYGENE}
      fileStream.Read( inbuf, length(inbuf) ) ;
    {$ELSE}
      fileStream.Read( inbuf[0]  , sizeOf(inbuf) ) ;
    {$ENDIF}

    // DEM level code
    setWorkStr(144, 6) ;

    //set but not read
    if IsStringEmpty( workStr ) then
      lvl := 0
    else
      lvl := StrToInt(workStr) ;

    // Corners coordinates
    j := OFFSET_COORDINATES ;
    for i := 0 to 3 do begin
      {$IFDEF OXYGENE}
        corner[i] := new Double[2] ;
      {$ENDIF}
      for k := 0 to 1 do begin
        setWorkStr(j, 24) ;
        corner[i][k] := DotStrToFloat(workStr) ;
        inc(j, 24) ;
      end ;
    end ;

    ext.XMin := corner[0][0] ;
    ext.XMax := corner[0][0] ;

    ext.YMax := corner[0][1] ;
    ext.YMin := corner[0][1] ;

    for i := 1 to 3 do begin
      if corner[i][0] < ext.XMin then
        ext.XMin := corner[i][0]
      else
      if corner[i][0] > ext.XMax then
        ext.XMax := corner[i][0] ;

      if corner[i][1] < ext.YMin then
        ext.YMin := corner[i][1]
      else
      if corner[i][1] > ext.YMax then
        ext.YMax := corner[i][1] ;
    end ;

    // Units
    setWorkStr(OFFSET_VERTUNITS, 6) ;
    untHorizontal := StrToInt(workStr) ;

    setWorkStr(OFFSET_HORZUNITS, 6) ;

    //set but not read
    untVertical := StrToInt(workStr) ;

    if FMinZ >= FMaxZ then begin
      // Min elevation
      setWorkStr(OFFSET_MINELEVATION, 24) ;
      FMinZ := DotStrToFloat(workStr) ;

      // Max elevation
      setWorkStr(OFFSET_MAXELEVATION, 24) ;
      FMaxZ := DotStrToFloat(workStr) ;
    end;

    // Accuracy code for elevation
    setWorkStr(OFFSET_ELEVATIONACC, 6) ;

    //set but not read
    cData := StrToInt(workStr) ;

    // Resolution X - easting
    setWorkStr(OFFSET_XRESULUTION, 12) ;
    scaleX := DotStrToFloat(workStr) ;

    // Resolution Y - northing
    setWorkStr(OFFSET_YRESOLUTION, 12) ;
    scaleY := -DotStrToFloat(workStr) ;

    // Vertical spatial resolution
    setWorkStr(OFFSET_VERTSPATRES, 12) ;
    resZ := DotStrToFloat(workStr) ;

    // DEM rows
    setWorkStr(OFFSET_ROWSNUMBER, 6) ;
    nRows := StrToInt(workStr) ;

    FBitHeight := 1 + FloorS(ext.YMax /(-scaleY)) - CeilS(ext.YMin / (-scaleY));

    // DEM cols
    setWorkStr(OFFSET_COLSNUMBER, 6) ;
    nCols := StrToInt(workStr) ;

    FBitWidth := nCols ;

    //set but not read
    sizeOfBRecord := (fileStream.Size -ARECORD_SIZE) div FBitWidth ;

    Extent := ext ;

    setWorkStr( 156, 6);
    coordSystem := StrToInt(workStr) ;
    setWorkStr( 162, 6);
    utmZone := StrToInt(workStr) ;

    // new format
    newFormat := (nRows <> 1) or (nCols <>1) ;

    if newFormat then begin
      setWorkStr(890, 2) ;
      if not IsStringEmpty( workStr ) then
        datumCode := StrToInt(workStr);
    end;

    SetLengthStr(workStr, 0);
  end ;

  procedure TGIS_LayerDEM.setProjection ;
  var
    gcs_map     : TGIS_CSGeographicCoordinateSystem ;
    proj_map    : TGIS_CSAbstract ;
    proj_param  : TGIS_CSProjParameters ;
    unit_map    : TGIS_CSUnits ;
    sname       : String ;
    tmp_epsg    : Integer ;
  begin
    tmp_epsg := 4267 ; //NAD 27

    if newFormat then begin
      // Horizontal datum
      case datumCode of
        1 : tmp_epsg := 4267           ;//NAD 27
        2 : tmp_epsg := GIS_EPSG_WGS72 ;//WGS 72
        3 : tmp_epsg := GIS_EPSG_WGS84 ;//WGS 84
        4 : tmp_epsg := 4269           ;//NAD 83
        5 : tmp_epsg := 4135           ;//Old Hawaii
        6 : tmp_epsg := 4139           ;//Puerto Rico
      end;
    end ;
    gcs_map := CSGeographicCoordinateSystemList.ByEPSG( tmp_epsg ) ;

    if (coordSystem = 1) then begin// UTM
      // construct UTM as TM to help proper EPSG code matching
      proj_map   := CSProjList.ByEPSG( CSPROJ_Transverse_Mercator ) ;
      proj_param := CSProjectedCoordinateSystemList.DefaultParamsForUTM(
                      utmZone
                    ) ;

      unit_map := CSUnitsList.ByEPSG( untHorizontalEPSG ) ;
      assert( assigned( unit_map ) ) ;

      if utmZone >= 0 then
        sname := Format( 'UTM Zone %dN %s', [ utmZone, unit_map.WKT ] )
      else
        sname := Format( 'UTM Zone %dS %s', [ utmZone, unit_map.WKT ] ) ;

      CS := CSProjectedCoordinateSystemList.Prepare(
              -1, sname,
              gcs_map.EPSG,
              untHorizontalEPSG,
              proj_map.EPSG,
              proj_param
            ) ;
    end
    else begin
      unit_map := CSUnitsList.ByEPSG( untHorizontalEPSG ) ;
      assert( assigned( unit_map ) ) ;

      sname := Format( '%s %s', [ gcs_map.WKT, unit_map.WKT ] ) ;

      CS := CSGeographicCoordinateSystemList.Prepare(
              -1, sname,
              gcs_map.Datum.EPSG,
              gcs_map.PrimeMeridian.EPSG,
              untHorizontalEPSG
            ) ;
    end ;
  end;

  procedure TGIS_LayerDEM.setUp ;
  var
    ext  : TGIS_Extent ;
  begin

    ext := GisExtent( 0,0,0,0 ) ;
    try

      fileStream := openBufferedFileStream( Path ) ;

      setHdrInfo ;

      realBitCount := 24 ;
      colorsNo := 0 ;

      FAntialias := True ;
      Params.Pixel.GridNoValue := USGS_DEM_NODATA ;
      FNoDataValue := Params.Pixel.GridNoValue ;

      realLineWidth := ( FBitWidth * realBitCount +7 ) div 8 ;
      intLineWidth  := FBitWidth * 3 ;

      SetLength( lineBuffer, FBitHeight ) ;

      //set but not read
      lineInBuffer := -1 ;

      prepareImage ;

      ext := Extent ;

      case untHorizontal of
        0 :  untHorizontalEPSG := 9101 ; // radians
        1 :  untHorizontalEPSG := 9002 ; // feets
        2 :  untHorizontalEPSG := 9001 ; // meters
        3 :  untHorizontalEPSG := 9104 ; // arc sec
        else untHorizontalEPSG := 9001 ; // meters
      end ;

      //set but not read
      case untVertical of
        0 :  untVerticalEPSG := 9101 ; // radians
        1 :  untVerticalEPSG := 9002 ; // feets
        2 :  untVerticalEPSG := 9001 ; // meters
        3 :  untVerticalEPSG := 9104 ; // arc sec
        else untVerticalEPSG := 9001 ; // meters
      end ;

      setProjection ;

      Extent := ext ;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;
      // Transparency
      redTransp[0]    := BASE_TRANSPARENT_FLAG ;
      greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]   := BASE_TRANSPARENT_FLAG ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'USGS Digital Elevation Model ASCII Grid (DEM)' +#13#10 +
                           '%d x %d',
                           [ FBitWidth, FBitHeight ]
                         ) ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  procedure TGIS_LayerDEM.prepareImage  ;
  var
    i, k, n, ns : Integer ;
    strBuffer : Array of Byte ;
    row_start : Integer ;
    workStr : String ;
    ridx    : Integer ;
    buf_idx : Integer ;
    str_empty : Boolean ;
    row     : Integer ;
    col     : Integer ;
    lastcol : Integer ;
    rowno   : Integer ;
    colno   : Integer ;
    firstX  : Double  ;
    firstY  : Double  ;
    locZ    : Double  ;
    locMinZ : Double ;
    locMaxZ : Double ;
    sdat    : Single ;
    mustback : Boolean ;
    maxsize : Int64 ;
    inbuffer : Integer ;
    toread  : Integer ;
    first : Boolean ;

    procedure trimBRecord ;
    var
      n1, i1 : Integer ;
      ln : Integer ;
    begin
      n1 := 0 ;
      while True do begin
        inc( n1 ) ;
        if n1 >= inbuffer then begin
          inbuffer := 0 ;
          exit ;
        end ;
        if (strBuffer[n1] >= Byte('1')) and (strBuffer[n1] <= Byte('9')) then begin
          if not first then
            break ;
          if (strBuffer[n1+6] >= Byte('1')) and (strBuffer[n1+6] <= Byte('9')) then begin
            for i1 := 1 to 6 do begin
              if strBuffer[n1+i1] <> Byte(' ') then
                break ;
            end ;
            if i1 >= 6 then begin
              first := False ;
              break ;
            end ;
          end ;
        end ;
      end ;

      ln := 0 ;
      while strBuffer[n1] <> Byte(' ') do begin
        inc( n1 ) ;
        if (strBuffer[n1] = $D) or (strBuffer[n1] = $A) then
          ln := n1 ;
      end ;
      if ln = 0 then
        ln := n1-6
      else
        inc(ln) ;
      if ln > 0 then begin
        inbuffer := inbuffer -ln ;
        maxsize := maxsize -ln ;
      end;
    end ;

    procedure setWorkStr(_idx, _length : Integer) ;
    var
      ii : Integer ;
      sb : TStringBuilder ;
    begin
      str_empty := True ;

      sb := TStringBuilder.Create ;
      try
        if _idx > 0 then begin
          if  ( strBuffer[_idx] >= Byte('0')) and (strBuffer[_idx] <= Byte('9')) then
          begin
            if strBuffer[_idx -1] = Byte('-') then begin
              dec(_idx) ;
              dec(buf_idx) ;
            end;
          end;
        end;

        sb.Length := _length ;

        for ii := 0 to _length - 1 do begin
          if strBuffer[_idx] = Byte('D') then
            sb[ii] := 'E'
          else begin
            sb[ii] := Char( strBuffer[_idx] ) ;
            if strBuffer[_idx] <> Byte(' ') then
              str_empty := False ;
          end ;
          inc(_idx) ;
        end ;
        for ii := _length - 1 downto 0 do begin
          if sb[ii] <> #0 then begin
            if  ( Byte(sb[ii]) < Byte('0')) or (Byte(sb[ii]) > Byte('9')) then
              sb[ii] := #0
            else
              break ;
          end;
        end;

      finally
        workStr := Trim(sb.ToString) ;
        FreeObject( sb ) ;
      end ;
    end ;

    function set_brecord_header : Boolean ;
    const
      BRECOFF_ROW     = 0   ;
      BRECOFF_COL     = 6   ;
      BRECOFF_ROWNO   = 12  ;
      BRECOFF_COLNO   = 18  ;
      BRECOFF_FIRSTX  = 24  ;
      BRECOFF_FIRSTY  = 48  ;
      BRECOFF_LOCZ    = 72  ;
      BRECOFF_LOCMINZ = 96  ;
      BRECOFF_LOCMAXZ = 120 ;
    begin
      Result := True ;
      setWorkStr(BRECOFF_ROW, 6) ;
      row     := StrToInt(workStr) -1;
      setWorkStr(BRECOFF_COL, 6) ;
      col     := StrToInt(workStr) -1;

      if col < lastcol then begin
        Result := False ;
        exit ;
      end ;

      setWorkStr(BRECOFF_ROWNO, 6) ;
      rowno   := StrToInt(workStr) ;
      setWorkStr(BRECOFF_COLNO, 6) ;
      colno   := StrToInt(workStr) ;

      setWorkStr(BRECOFF_FIRSTX, 24) ;
      firstX  := DotStrToFloat(workStr) ;
      setWorkStr(BRECOFF_FIRSTY, 24) ;
      firstY  := DotStrToFloat(workStr) ;

      setWorkStr(BRECOFF_LOCZ, 24) ;
      locZ    := DotStrToFloat(workStr) ;

      setWorkStr(BRECOFF_LOCMINZ, 24) ;
      locMinZ := DotStrToFloat(workStr) ;
      setWorkStr(BRECOFF_LOCMAXZ, 24) ;
      locMaxZ := DotStrToFloat(workStr) ;
    end ;

  begin
    try
      SetLength(arGrid, FBitHeight, FBitWidth) ;
      SetLength(strBuffer, BASE_RECORD_SIZE) ;

      fileStream.Seek( ARECORD_SIZE , soBeginning ) ;

      lastcol := 0 ;

      maxsize := fileStream.Size -ARECORD_SIZE ;
      first := True ;

      for k := 0 to FBitWidth -1 do begin
        if maxsize >= BASE_RECORD_SIZE then
          inbuffer := BASE_RECORD_SIZE
        else
          inbuffer := maxsize ;

         toread := inbuffer ;
        {$IFDEF OXYGENE}
          fileStream.Read( strBuffer, inbuffer ) ;
        {$ELSE}
          fileStream.Read( strBuffer[0], inbuffer ) ;
        {$ENDIF}
        trimBRecord ;
        if inbuffer = 0 then
          break ;

        if inbuffer < toread then begin

          fileStream.Seek(-inbuffer , soCurrent ) ;

          if maxsize >= BASE_RECORD_SIZE then
            inbuffer := BASE_RECORD_SIZE
          else
            inbuffer := maxsize ;

          {$IFDEF OXYGENE}
            fileStream.Read( strBuffer, inbuffer ) ;
          {$ELSE}
            fileStream.Read( strBuffer[0], inbuffer ) ;
          {$ENDIF}
        end ;
        maxsize := maxsize -inbuffer ;
        mustback := False ;

        if not set_brecord_header then
          break ;

        row_start := RoundS(Abs((firstY - FExtent.YMin)/(-scaleY))) ;
        if row_start > FBitHeight then
          row_start := FBitHeight ;
        buf_idx := BRECORD_HEADER_SIZE ;
        ridx := FBitHeight -1 ;

        if row_start > 0 then begin
          for i := 0 to row_start -1 do begin
            arGrid[ridx][k] := FNoDataValue ;
            dec(ridx) ;
          end ;
        end ;

        for i := row_start to FBitHeight -1 do begin
          setWorkStr(buf_idx, 6) ;
          inc(buf_idx, 6) ;
          if str_empty then begin
            break ;
          end ;

          sdat := DotStrToFloat(workStr) ;
          if rowno > 0 then begin
            if sdat <> FNoDataValue then
              arGrid[ridx][k] := sdat * resZ +locZ
            else
              arGrid[ridx][k] := sdat ;
            dec(rowno) ;
          end ;

          dec(ridx) ;
          if (buf_idx +6) > inbuffer then begin
            if rowno > 0 then begin
              ns := 0 ;
              for n := 0 to (inbuffer -buf_idx -1) do
                if (strBuffer[buf_idx +n] <> Byte(#13)) and
                   (strBuffer[buf_idx +n] <> Byte(#10)) then begin
                  inc(ns) ;
                end
                else
                  mustback := True ;

              if mustback then
                maxsize := maxsize +ns ;

              if maxsize >= BASE_RECORD_SIZE then
                inbuffer := BASE_RECORD_SIZE
              else
                inbuffer := maxsize ;

              maxsize :=  maxsize -inbuffer ;

              if mustback then
                fileStream.Seek(-ns , soCurrent ) ;
              {$IFDEF OXYGENE}
                fileStream.Read( strBuffer, inbuffer ) ;
              {$ELSE}
                fileStream.Read( strBuffer[0], inbuffer ) ;
              {$ENDIF}
              buf_idx := 0 ;
            end
            else begin
              break ;
            end ;
          end

        end ;
        if mustback then begin
          fileStream.Seek(buf_idx-inbuffer , soCurrent ) ;
          maxsize :=  maxsize +inbuffer -buf_idx ;
        end ;
        lastcol := col ;
        while ridx >= 0 do begin
          arGrid[ridx][k] := FNoDataValue ;
          dec(ridx) ;
        end ;
      end ;
    finally
      strBuffer := nil ;
    end ;
  end ;

  function TGIS_LayerDEM.getLine( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer;
  var
    i      : Integer;
    start  : Integer ;
    line   : Integer ;
    pixels : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
  begin
    try

      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      start := (_start div 3) ;

      offset := 0 ;

      for i := start to start +pixels -2 do begin
        if arGrid[line, i] = FNoDataValue then begin
          ccolor := colorNoData ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;

          makeTransparent := True ;
        end
        else begin
          ccolor := GetColorRamp( arGrid[line, i] ) ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
        end ;
        offset := offset +3 ;
      end ;

      // last triple
      if arGrid[line, start +pixels -1] = FNoDataValue then begin
        makeTransparent := True ;
        ccolor := colorNoData ;
      end else
        ccolor := GetColorRamp(arGrid[line, start +pixels -1]) ;

      _buffer[_offset + offset    ] := ccolor.B ;
      _buffer[_offset + offset + 1] := ccolor.G ;
      _buffer[_offset + offset + 2] := ccolor.R ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerDEM.getNativeValue( const _pt : TPoint  ;
                                           const _ar : TGIS_DoubleArray
                                         ) : Boolean ;

  begin
    try
      Result := True ;

      _ar[0] := arGrid[ _pt.Y, _pt.X ] ;

      if _ar[0] = FNoDataValue then Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerDEM.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                        const _linenr   : Integer          ;
                                        const _startIdx : Integer          ;
                                        const _count    : Integer
                                      ) : Integer ;
  var
    i : Integer ;
  begin
    try
      Result := 0 ;

      if ( _linenr < 0 ) or ( _linenr > FBitHeight ) then
        exit ;

      for i := 0 to _count -1 do
        _buffer[i] := arGrid[_linenr, i +_startIdx]
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerDEM.PreRecognize( const _path     : String ;
                                         var _new_path : String
                                     ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    if SafeFileExists( GetPathNoExt( _path ) + '.hdr' ) then
      Result := False ;
  end;

  { Perform initialization section.
  }
  class procedure GisLayerDEM.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DEM', 'Digital Elevation Model (ASCII GRID or SPOT)',
                   TGIS_LayerDEM, '.dem',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    GisLayerDEM.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

