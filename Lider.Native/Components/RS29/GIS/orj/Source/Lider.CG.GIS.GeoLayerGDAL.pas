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
  Encapsulation of a raster Layer using GDAL library.
}

{$IFDEF DCC}
  unit GisLayerGDAL ;
  {$HPPEMIT '#pragma link "GisLayerGDAL"'}
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
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.SysUtils,
    System.Classes,

    GisTypes,
    GisTypesUI,
    GisCsSystems,
    GisFunctions,
    GisLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerGDAL = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of a raster Layer using GDAL library.
  /// </summary>
  TGIS_LayerGDAL = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private
      FGDAL         : TObject ;
      FXSize        : Integer ;
      FBufSize      : Integer ;
      FBufRed       : TBytes ;
      FBufGreen     : TBytes ;
      FBufBlue      : TBytes ;
      FBufAlpha     : TBytes ;
      FLineBuffer   : TGIS_SingleArray ;
      FLineInBuffer : Integer ;
      FIs48Bits     : Boolean ;
      FWordDivider  : array of Double ;
      FDLLPath      : String ;
      FSQLPath      : String ;
      FNumBands     : Integer ;
      FSubDataset   : String ;
      FActualZoom   : Double ;
      FActualWidth  : Integer ;
      FZoomWidth    : Integer ;
      FZoomHeight   : Integer ;
      FStripStart   : Integer ;
      FStripHeight  : Integer ;
      FStripLeft    : Integer ;
      FDatasize     : Integer ;
      FPixelBytes   : Integer ;
      FIsARGB       : Boolean ;
      FIsRGB        : Boolean ;
      FCalcMinMaxZ  : Boolean ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp    ; override;

      /// <inheritdoc/>
      function  getLine           ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getNativeValue   ( const _pt     : TPoint  ;
                                   const _ar     : TGIS_DoubleArray
                                 ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine    ( const _buffer   : TGIS_SingleArray ;
                                   const _linenr   : Integer          ;
                                   const _startIdx : Integer          ;
                                   const _count    : Integer
                                 ) : Integer ; override;

      /// <inheritdoc/>
      function  getAlphaLine    ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer; override;

      /// <inheritdoc/>
      function  setFileScale    ( const _dwidth : Double ;
                                  const _swidth : Double
                                ) : Double ; override;

      /// <inheritdoc/>
      procedure setFileView    ( const _viewRect    : TRect
                               ) ; override;

      /// <inheritdoc/>
      function  importPixelData( const _layer    : TGIS_LayerPixel
                               ) : Boolean ; override;

      /// <summary>
      ///   Parse configuration file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <param name="_params">
      ///   read parameters
      /// </param>
      procedure parseConfig   ( const _path      : String ;
                                const _params    : TStrings
                              ) ;

      /// <summary>
      ///   Process tokens in a SQLParameters property.
      /// </summary>
      /// <param name="_token">
      ///   token name
      /// </param>
      /// <returns>
      ///   token value
      /// </returns>
      function  passwordCallBack    ( const _token  : String
                                    ) : String ;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

      /// <summary>
      ///   Calculate statistics.
      /// </summary>
      /// <param name="_zoom">
      ///   zoom of testing
      /// </param>
      procedure prepareMinMaxZ  ( const _zoom : Double = -1
                                 ) ; override     ;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      function  PreRecognize     ( const _path     : String ;
                                     var _new_path : String
                                  ) : Boolean ; override;
     {$IFDEF OXYGENE}
       /// <summary>
       ///   Get metadata list from domain.
       /// </summary>
       /// <param name="_domain">
       ///   domain name
       /// </param>
       /// <returns>
       ///   list of domains
       /// </returns>
       function GetMetadata( const _domain : String ) : TGIS_Strings ;
     {$ELSE}

       /// <summary>
       ///   Get metadata list from domain.
       /// </summary>
       /// <param name="_domain">
       ///   domain name
       /// </param>
       /// <returns>
       ///   list of domains
       /// </returns>
       function GetMetadata( const _domain : String ) : TStrings     ;
     {$ENDIF}

      /// <summary>
      ///   Set global config option.
      /// </summary>
      /// <param name="_key">
      ///   key name
      /// </param>
      /// <param name="_val">
      ///   value name
      /// </param>
      procedure SetConfigOption( const _key : String ;
                                 const _val : String
                                ) ;

      /// <summary>
      ///   Set dataset creation option.
      /// </summary>
      /// <param name="_key">
      ///   key name
      /// </param>
      /// <param name="_val">
      ///   value name
      /// </param>
      procedure SetCreateOption( const _key : String ;
                                 const _val : String
                                ) ;

      /// <inheritdoc/>
      procedure Build          (  const _path      : String         ;
                                  const _grid      : Boolean        ;
                                  const _cs        : TGIS_CSCoordinateSystem  ;
                                  const _ext       : TGIS_Extent    ;
                                  const _width     : Integer        ;
                                  const _height    : Integer ;
                                  const _subformat : TGIS_LayerPixelSubFormat
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

    public

        /// <summary>
        ///   GDAL DLL path.
        /// </summary>
        property DLLPath        : String read FDLLPath write FDLLPath ;

        /// <summary>
        ///   GDAL SQL path.
        /// </summary>
        property SQLPath        : String read FSQLPath write FSQLPath ;

        /// <summary>
        ///   SubDataset path.
        /// </summary>
        property SubDatasetPath : String read FSubDataset write FSubDataset ;
  end ;

//##############################################################################
implementation

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisRtl,
    GisInternals,
    GisClasses,
    GisFileGDAL,
    GisLogger,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

//==============================================================================
// TGIS_LayerGDAL
//==============================================================================

  constructor TGIS_LayerGDAL.Create ;
  begin
    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent,
                            TGIS_LayerSubType.Exportable] ;

    FGDAL     := nil ;
    FBufRed   := nil ;
    FBufGreen := nil ;
    FBufBlue  := nil ;
    FBufAlpha := nil ;
    FSQLPath  := '';

    useAltitudeZones := False ;
    FActualZoom := 1 ;
  end ;

  procedure TGIS_LayerGDAL.doDestroy ;
  begin
    FBufRed   := nil ;
    FBufGreen := nil ;
    FBufBlue  := nil ;
    FBufAlpha := nil ;

    FreeObject( FGDAL ) ;

    inherited ;
  end ;

  function TGIS_LayerGDAL.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileGDAL ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileGDAL.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerGDAL.passwordCallBack(
    const _token : String
  ) : String ;
  begin
    Result := GisPasswordList.Get( Name, _token ) ;
    if IsStringEmpty( Result ) then begin
      if assigned( FOnPassword ) then
        {$IFDEF OXYGENE}
          Result := FOnPassword( Self,
                                 TGIS_TemplateProducerEventArgs.Create( _token )
                               )
        {$ELSE}
          Result := PasswordEvent( Self, _token )
        {$ENDIF}
      else
        Result := _token ;
      GisPasswordList.Add( Name, _token, Result ) ;
    end ;
  end ;

  procedure TGIS_LayerGDAL.parseConfig(
    const _path   : String ;
    const _params : TStrings
  ) ;
  begin
    ReadSQLParamsFromPath( _path, _params  ) ;

    // resolve any ID/Password tokens
    {$IFNDEF OXYGENE}
      _params.Text  := TemplateProducer( _params.Text, nil,
                                         passwordCallBack, False
                                        ) ;
    {$ELSE}
      _params.Text  := TemplateProducer( _params.Text, nil,
                                         @passwordCallBack, False
                                        ) ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerGDAL.setUp ;
  var
    cswkt     : String  ;
    rotateX   : Double ;
    rotateY   : Double ;
    channels  : TGIS_IntegerArray ;
    datatype  : String ;
    colorint  : String ;
    colors    : Integer ;
    i         : Integer ;
    cpath     : String ;
    config    : TStringList ;
    ext       : TGIS_Extent ;
    minz      : Single ;
    maxz      : Single ;
  begin
    minz := 0 ;
    maxz := 0 ;
    FCalcMinMaxZ := False ;

    if ( CompareText( GetFileExt( Path ), GIS_TTKPS_EXT    ) = 0 ) or
       ( CompareText( GetFileExt( Path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
      config := TStringList.Create ;
      try
        parseConfig( Path, config ) ;
        cpath       := config.Values[GIS_INI_PATH] ;
        FSubDataset := config.Values[GIS_INI_LAYERSQL_LAYER] ;

        cpath := GetPathAbsolute( GetFileDir( Path ), cpath ) ;

        if not assigned( FGDAL ) then begin
          FGDAL := TGIS_FileGDAL.Create( cpath ) ;
          TGIS_FileGDAL(FGDAL).DLLPath    := FDLLPath ;
        end ;
        TGIS_FileGDAL(FGDAL).SubDataset := FSubDataset ;

        TGIS_FileGDAL(FGDAL).OpenDataset( True ) ;
      finally
        FreeObject( config ) ;
      end ;
    end
    else begin
      if not assigned( FGDAL ) then begin
        FGDAL := TGIS_FileGDAL.Create( Path ) ;
        TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
        TGIS_FileGDAL(FGDAL).SubDataset := FSubDataset ;
      end ;

      TGIS_FileGDAL(FGDAL).OpenDataset( True ) ;
    end ;

    {$IFDEF GIS_NORECORDS}
      ext := new TGIS_Extent() ;
    {$ENDIF}
    TGIS_FileGDAL(FGDAL).GetDatasetProperties(
      FBitWidth, FBitHeight, cswkt, ext, scaleX, scaleY, rotateX, rotateY,
      FNumBands, channels
    ) ;

    if ( rotateX <> 0 ) or ( rotateY <> 0) then begin
      yRotDivSc := rotateY / scaleX ;
      xRotDivSc := rotateX / scaleY ;
      scxF_yRotDivScxRotDivSMcsyF := 1 - yRotDivSc * xRotDivSc ;
      scyF_yRotDivScxRotDivSMcsxF := scxF_yRotDivScxRotDivSMcsyF ;
      if (scyF_yRotDivScxRotDivSMcsxF <> 0) and (scyF_yRotDivScxRotDivSMcsxF <> 1) then
        baseRotation := True ;
    end
    else if scaleY > 0 then begin
      scaleY := -scaleY ;
      scaleYFactor := -1 ;
      scxF_yRotDivScxRotDivSMcsyF := scaleXFactor - yRotDivSc*xRotDivSc*scaleYFactor ;
      scyF_yRotDivScxRotDivSMcsxF := scaleYFactor - yRotDivSc*xRotDivSc*scaleXFactor ;
      if (scxF_yRotDivScxRotDivSMcsyF <> 0) and (scyF_yRotDivScxRotDivSMcsxF <> 0) then
        baseRotation := True ;
    end ;

    FExtent := ext ;

    if length( channels ) > 0 then
      Params.Pixel.RedBand := channels[0] ;

    if (Params.Pixel.GridBand > 0) and (Params.Pixel.GridBand <= FNumBands) then begin
      TGIS_FileGDAL(FGDAL).GetRasterBandProperties(
        Params.Pixel.GridBand, FXSize, datatype, FDatasize, minz, maxz, FNoDataValue,
        colorint, colors
      ) ;
    end
    else
      TGIS_FileGDAL(FGDAL).GetRasterBandProperties(
        Params.Pixel.RedBand, FXSize, datatype, FDatasize, minz, maxz, FNoDataValue,
        colorint, colors
      ) ;
    FBufSize  := FXSize * FDatasize ;
    FIs48Bits := FDatasize = 2 ;
    FBandsCount := FNumBands ;

    if FIs48Bits then begin
      SetLength( FWordDivider, FNumBands ) ;
      for i := 0 to FNumBands-1 do
        FWordDivider[i] := 1 ;

      FWordDivider[Params.Pixel.RedBand-1] := maxz / 255 ;
      if FWordDivider[Params.Pixel.RedBand-1] = 0 then
        FWordDivider[Params.Pixel.RedBand-1] := 1 ;
    end ;

    if FNumBands >=3 then begin
      Params.Pixel.GreenBand := channels[1] ;

      if FIs48Bits then begin
        TGIS_FileGDAL(FGDAL).GetRasterBandProperties(
          Params.Pixel.GreenBand, FXSize, datatype, FDatasize, minz, maxz, FNoDataValue,
          colorint, colors
        ) ;
        FWordDivider[Params.Pixel.GreenBand-1] := maxz / 255 ;
        if FWordDivider[Params.Pixel.GreenBand-1] = 0 then
          FWordDivider[Params.Pixel.GreenBand-1] := 1 ;
      end ;

      Params.Pixel.BlueBand  := channels[2] ;

      if FIs48Bits then begin
        TGIS_FileGDAL(FGDAL).GetRasterBandProperties(
          Params.Pixel.BlueBand, FXSize, datatype, FDatasize, minz, maxz, FNoDataValue,
          colorint, colors
        ) ;
        FWordDivider[Params.Pixel.BlueBand-1] := maxz / 255 ;
        if FWordDivider[Params.Pixel.BlueBand-1] = 0 then
          FWordDivider[Params.Pixel.BlueBand-1] := 1 ;
      end ;
    end ;

    if (FNumBands = 4) and (channels[3] <> -1) then begin
      Params.Pixel.AlphaBand := channels[3] ;

      isPartialTransparent := True ;
      forceCachedMode      := True ;
    end ;

    for i := 3 to FNumBands-1 do begin
      if FIs48Bits and (channels[i] <> -1) then begin
        TGIS_FileGDAL(FGDAL).GetRasterBandProperties(
          channels[i], FXSize, datatype, FDatasize, minz, maxz, FNoDataValue,
          colorint, colors
        ) ;
        FWordDivider[i] := maxz / 255 ;
        if FWordDivider[i] = 0 then
          FWordDivider[i] := 1 ;
      end ;

    end ;

    if (( FNumBands = 1 ) and (FDatasize >=2 )) or (Params.Pixel.GridBand > 0) or
       (( FNumBands > 0 ) and (FDatasize >=4 )) then begin
      // assume, this should be grid
      FIsGridImage       := True ;
      FIsNativeGridImage := True ;
      FAntialias := True ;

      if Params.Pixel.GridBand = 0 then
        Params.Pixel.GridBand    := Params.Pixel.RedBand ;
      Params.Pixel.GridNoValue := FNoDataValue ;
      redTransp[0]   := BASE_TRANSPARENT_FLAG ;
      greenTransp[0] := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]  := BASE_TRANSPARENT_FLAG ;
    end ;

    if (( colors = 0 ) or FIsGridImage) and (FNumBands >=3) then begin
      realBitCount  := 24;
      realLineWidth := ( Int64(FBitWidth)*realBitCount +7 ) div 8 ;
      intLineWidth  := realLineWidth ;
    end
    else begin
      realBitCount  := 8  ;
      realLineWidth := ( Int64(FBitWidth)*realBitCount +7 ) div 8 ;
      colorsNo      := 256 ;
      setBitmapPalette ;
    end ;

    if (realBitCount = 1) OR (realBitCount = 4) OR (realBitCount = 8) then begin
      intLineWidth := FBitWidth * 3 ;
      colorsNo := 0 ;
    end
    else begin
      intLineWidth := realLineWidth ;
    end ;

    for i := 0 to colors -1 do begin
      try
        bitPalette[i] := TGIS_Color.FromARGB(
          TGIS_FileGDAL(FGDAL).GetRasterBandGetColorEntry(
                                Params.Pixel.RedBand, i, 4
                              ) and $FF,
          TGIS_FileGDAL(FGDAL).GetRasterBandGetColorEntry(
                                Params.Pixel.RedBand, i, 1
                               ) and $FF,
          TGIS_FileGDAL(FGDAL).GetRasterBandGetColorEntry(
                                Params.Pixel.RedBand, i, 2
                               ) and $FF,
          TGIS_FileGDAL(FGDAL).GetRasterBandGetColorEntry(
                                Params.Pixel.RedBand, i, 3
                               ) and $FF
          ) ;
      except
        bitPalette[i] := TGIS_Color.FromARGB( 0, 0, 0, 0 );
      end;

      if bitPalette[i].A = 0 then begin
        if not isPartialTransparent then begin
      // setting transparent color
          internalTransparentColor.ARGB     := $FFFFFFFF ;
        end ;
      end ;
    end ;

    if FMinZ >= FMaxZ then begin
      if maxz > minz then begin
        FMinZ := minz ;
        FMaxZ := maxz ;
      end
      else begin
        FMaxZ   := -GIS_MAX_SINGLE  ;
        FMinZ   :=  GIS_MAX_SINGLE  ;
        FCalcMinMaxZ := True ;
      end;
    end ;

    FExtent3D.ZMin := FMinZ ;
    FExtent3D.ZMax := FMaxZ ;

    inherited ;

    SetCSByWKT( cswkt ) ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := TGIS_FileGDAL(FGDAL).GetInfo ;
  end ;

  function TGIS_LayerGDAL.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  begin
    if _swidth <> 0 then begin
      Result := _dwidth / _swidth ;
    end
    else
      Result := 1 ;

    if Result > 1 then
      Result := 1 ;

    if Abs(Abs(FActualZoom) -Abs(Result)) > 0.5/_dwidth then
      FActualZoom := Result
    else
      Result := FActualZoom ;
  end ;

  procedure TGIS_LayerGDAL.setFileView(
    const _viewRect : TRect
  ) ;
  var
    vb, vt, vl, vr : Integer ;
    zxl, zxr,
    zyt, zyb      : Integer  ;
    cor           : Integer  ;
    actualsize    : Integer ;
    ww, hh        : Integer ;
    gb            : Integer ;
  begin
    vb := _viewRect.Bottom ;
    vr := _viewRect.Right ;
    vt := _viewRect.Top ;
    vl := _viewRect.Left ;

    if vb >= RoundS(FBitHeight * FActualZoom) then
      vb := RoundS(FBitHeight * FActualZoom) ;

    if vr >= RoundS(FBitWidth * FActualZoom) then
      vr := RoundS(FBitWidth * FActualZoom) ;

    FStripStart := vt ;
    FStripLeft := vl ;
    if FActualZoom >= 1 then begin
      if FStripLeft > 0 then
        dec(FStripLeft) ;
    end ;

    FStripHeight := vb -vt ;
    FActualWidth := vr -FStripLeft ;
    if FActualZoom >= 1 then begin
      if FStripLeft + FActualWidth > FBitWidth then begin
        if FStripLeft > 0 then
          dec(FStripLeft) ;
        FActualWidth := FBitWidth -FStripLeft ;
      end ;
    end ;

    zyt := TruncS(FStripStart/FActualZoom) ;
    zyb := TruncS(vb/FActualZoom) ;

    if zyb >= FBitHeight then begin
      cor := zyb - FBitHeight ;
      zyb := zyb -cor ;
      zyt := zyt -cor ;
      if zyt < 0 then
        zyt := 0 ;
    end ;
    zxl := TruncS(FStripLeft/FActualZoom) ;
    zxr := TruncS(vr/FActualZoom) ;

    if zxr >= FBitWidth then
      zxr := FBitWidth ;

    if FActualZoom >= 1 then begin
      if FStripStart +FStripHeight > FBitHeight then
        FStripHeight := FBitHeight -FStripStart ;
    end ;

    if FActualWidth > (zxr -zxl ) then
      FActualWidth := (zxr -zxl ) ;
    if FStripHeight > (zyb -zyt ) then
      FStripHeight := (zyb -zyt ) ;

    FZoomWidth  := FActualWidth ;
    FZoomHeight := FStripHeight ;

    actualsize := (vb - vt +3)*(vr - vl +4)*FDatasize ;
    ww := zxr-zxl ;
    hh := zyb-zyt ;

    if FIsGridImage then begin
      SetLength( FLineBuffer, actualsize ) ;
      if Params.Pixel.GridBand >= 1 then
        gb := Params.Pixel.GridBand
      else
        gb := 1 ;

      TGIS_FileGDAL(FGDAL).ReadRasterBandGrid(
        gb, zxl, zyt, ww, hh, FLineBuffer, FZoomWidth, FZoomHeight
      ) ;
      FPixelBytes := 1 ;
    end
    else begin
      SetLength( FBufRed, actualsize ) ;
      if FNumBands >= 3 then begin
        SetLength( FBufGreen, actualsize ) ;
        SetLength( FBufBlue, actualsize ) ;
      end ;

      if isPartialTransparent then
        SetLength( FBufAlpha, actualsize ) ;

      if assigned( FBufRed ) then
        TGIS_FileGDAL(FGDAL).ReadRasterBandRGB(
          Params.Pixel.RedBand, zxl, zyt, ww, hh, FBufRed, FZoomWidth, FZoomHeight
       ) ;

      if assigned( FBufGreen ) then
        TGIS_FileGDAL(FGDAL).ReadRasterBandRGB(
          Params.Pixel.GreenBand, zxl, zyt, ww, hh, FBufGreen, FZoomWidth, FZoomHeight
       ) ;

      if assigned( FBufBlue ) then
        TGIS_FileGDAL(FGDAL).ReadRasterBandRGB(
          Params.Pixel.BlueBand, zxl, zyt, ww, hh, FBufBlue, FZoomWidth, FZoomHeight
       ) ;

      if assigned( FBufAlpha ) then
        TGIS_FileGDAL(FGDAL).ReadRasterBandRGB(
          4, zxl, zyt, ww, hh, FBufAlpha, FZoomWidth, FZoomHeight
       ) ;

      FIsARGB := False ;
      FIsRGB  := False ;

      if FNumBands >= 3 then begin
        FPixelBytes := 3 ;
        FIsRGB := assigned( FBufBlue ) and assigned( FBufGreen ) and
                  assigned( FBufRed  ) ;
        if FNumBands >= 4 then begin
          inc( FPixelBytes ) ;
          FIsARGB := assigned( FBufAlpha ) and assigned( FBufBlue ) and
                     assigned( FBufGreen ) and assigned( FBufRed  ) ;
        end ;
      end
      else
        FPixelBytes := 1 ;
    end ;
  end ;

  function TGIS_LayerGDAL.getLinePixels(
    const _buffer   : TGIS_Pixels ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    loc_start     : Integer ;
    dx            : Integer ;
    off           : Integer ;
    xl            : Integer ;
    line_nr       : Integer ;
    zoomed_height : Integer ;
    pixels        : Integer ;
    rr, rg, rb    : Integer ;
    rgba          : array [0..3] of Byte ;

   procedure copy_bytes_set_alpha ;
    var
      i   : Integer ;
    begin
      for i := 0 to pixels -1 do begin
        rgba[0] := FBufRed  [i+off] ;
        rgba[1] := FBufGreen[i+off] ;
        rgba[2] := FBufBlue [i+off] ;
        rgba[3] := $FF ;

        _buffer[_offset+i] := Integer(Byte(rgba[rr])       ) or
                              Integer(Byte(rgba[rg]) shl 08) or
                              Integer(Byte(rgba[rb]) shl 16) or
                              Integer(Byte(rgba[3 ]) shl 24)  ;
      end ;
    end ;

    procedure copy_bytes_alpha ;
    var
      i : Integer ;
    begin
      for i := 0 to pixels -1 do begin
        rgba[0] := FBufRed  [i+off] ;
        rgba[1] := FBufGreen[i+off] ;
        rgba[2] := FBufBlue [i+off] ;
        rgba[3] := FBufAlpha[i+off] ;

        _buffer[_offset+i] := Integer(Byte(rgba[rr])       ) or
                              Integer(Byte(rgba[rg]) shl 08) or
                              Integer(Byte(rgba[rb]) shl 16) or
                              Integer(Byte(rgba[3 ]) shl 24)  ;
      end ;
    end ;

    procedure copy_bytes_48bits ;
    var
      i     : Integer ;
      wval  : Word ;
    begin
      for i := 0 to pixels -1 do begin
        {$IFDEF OXYGENE}
          wval := BitConverter.ToInt16( FBufRed, (i*2)+off ) ;
        {$ELSE}
          wval := PWord( NativeInt(FBufRed) + (i*2)+off )^ ;
        {$ENDIF}
        if Params.Pixel.RedBand > 0 then
          wval := RoundS( wval/FWordDivider[Params.Pixel.RedBand-1] ) ;

        if wval >  255 then
          wval := 255 ;

        rgba[0] := Byte(wval) ;

        if FIsRGB then begin
          {$IFDEF OXYGENE}
            wval := BitConverter.ToInt16( FBufGreen, (i*2)+off ) ;
          {$ELSE}
            wval := PWord( NativeInt(FBufGreen) + (i*2)+off )^ ;
          {$ENDIF}
          if Params.Pixel.GreenBand > 0 then
            wval := RoundS( wval/FWordDivider[Params.Pixel.GreenBand-1] ) ;
          if wval >  255 then
            wval := 255 ;
        end
        else
          wval := rgba[0] ;

        rgba[1] := Byte(wval) ;

        if FIsRGB then begin
          {$IFDEF OXYGENE}
            wval := BitConverter.ToInt16( FBufBlue, (i*2)+off ) ;
          {$ELSE}
            wval := PWord( NativeInt(FBufBlue) + (i*2)+off )^ ;
          {$ENDIF}
          if Params.Pixel.BlueBand > 0 then
            wval := RoundS( wval/FWordDivider[Params.Pixel.BlueBand-1] ) ;
          if wval >  255 then
            wval := 255 ;
        end
        else
          wval := rgba[0] ;

        rgba[2] := Byte(wval) ;

        if FIsARGB then
          {$IFDEF OXYGENE}
            rgba[3] := FBufAlpha[i+off]
          {$ELSE}
            rgba[3] := PByte(NativeInt(FBufAlpha)+i+off)^
          {$ENDIF}
        else
          rgba[3] := $FF ;

        _buffer[_offset+i] := Integer(Byte(rgba[rr])       ) or
                              Integer(Byte(rgba[rg]) shl 08) or
                              Integer(Byte(rgba[rb]) shl 16) or
                              Integer(Byte(rgba[3 ]) shl 24)  ;
      end ;
    end ;

  begin
    xl := _pixStart ;
    dx :=  xl - FStripLeft ;
    if dx < 0 then
      dx := 0 ;
    pixels := _pixCount ;

    zoomed_height := RoundS( FBitHeight * FActualZoom ) ;

    if _linenr >= zoomed_height then
      line_nr := zoomed_height -1
    else
      line_nr := _linenr ;

    if line_nr < FStripStart then
      line_nr := FStripStart ;

    loc_start := line_nr - FStripStart ;
    off := loc_start * FZoomWidth * FDatasize + FDatasize * dx ;

    if BandsCount <= 4 then begin
      rr := 2 - bandsMap[0] ;
      rg := 2 - bandsMap[1] ;
      rb := 2 - bandsMap[2] ;
    end
    else begin
      rr := 2 ;
      rg := 1 ;
      rb := 0 ;
    end ;

    if FDatasize > 0 then begin
      if FIs48Bits then
        copy_bytes_48bits
      else begin
        if FPixelBytes = 1 then
          pixels := convertBitsToPixels( FBufRed, off, _buffer, _offset, _pixStart, _pixCount )
        else if (FPixelBytes = 3) and FIsRGB then
          copy_bytes_set_alpha
        else if (FPixelBytes = 4) then
          if FIsARGB then
            copy_bytes_alpha
          else
            copy_bytes_set_alpha
      end ;
    end ;

    Result := pixels ;
  end ;

  function TGIS_LayerGDAL.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, j    : Integer ;
    offset  : Integer ;
    pixels  : Integer ;
    start   : Integer ;
    ccolor  : TGIS_Color ;
    wval    : Word ;
    line_nr : Integer ;
    zheight : Integer ;
    dx      : Integer ;
 begin
    Result := _bytes ;
    if FDatasize = 0 then exit ;

    try
      if FIsGridImage then begin
        pixels := _bytes div 3 ;
        Result := _bytes ;

        start  := (_start div 3) ;
        offset := 0 ;
        pixels := pixels +start ;

        FLineInBuffer := _linenr ;

        TGIS_FileGDAL(FGDAL).ReadRasterBandGrid(
          Params.Pixel.GridBand, 0, _linenr, FXSize, 1, FLineBuffer, FXSize, 1
        ) ;
        if length( FLineBuffer ) = 0 then
          ccolor := colorNoData
        else begin
          for i := start to pixels -2 do begin
            if FLineBuffer[i] = FNoDataValue then begin
              ccolor := colorNoData ;
              _buffer[_offset + offset    ] := ccolor.B ;
              _buffer[_offset + offset + 1] := ccolor.G ;
              _buffer[_offset + offset + 2] := ccolor.R ;
              makeTransparent := True ;
            end
            else begin
              ccolor := GetColorRamp( FLineBuffer[i] ) ;
              _buffer[_offset + offset    ] := ccolor.B ;
              _buffer[_offset + offset + 1] := ccolor.G ;
              _buffer[_offset + offset + 2] := ccolor.R ;
            end ;
            offset := offset +3 ;
          end ;

          // last triple
          if FLineBuffer[pixels -1] = FNoDataValue then begin
            ccolor := colorNoData ;
            makeTransparent := True ;
          end
          else
            ccolor := GetColorRamp( FLineBuffer[pixels -1] ) ;
        end ;

        _buffer[_offset + offset    ] := ccolor.B ;
        _buffer[_offset + offset + 1] := ccolor.G ;
        _buffer[_offset + offset + 2] := ccolor.R ;
      end
      else begin
        i := 0 ;
        j := 0 ;

        if ( realBitCount = 24 ) then begin
          pixels := _bytes ;
          offset := _start div 3 ;
        end
        else begin
          offset := _start ;
          pixels := _bytes ;
        end ;

        dx := offset - FStripLeft ;
        if dx < 0 then
          dx := 0 ;

        zheight := RoundS(FBitHeight * FActualZoom) ;
        if _linenr >= zheight then
          line_nr := zheight -1
        else
          line_nr := _linenr ;

        if line_nr < FStripStart then
          line_nr := FStripStart ;

        line_nr := line_nr -FStripStart ;
        offset := line_nr * FZoomWidth*FDatasize + FDatasize*dx ;

        while i < ( pixels ) do begin

          if FIs48Bits then begin
            if assigned( FBufBlue ) then begin
              {$IFDEF OXYGENE}
                wval := BitConverter.ToInt16( FBufBlue, j + offset ) ;
              {$ELSE}
                wval := PWord( NativeInt(FBufBlue)+j+offset )^ ;
              {$ENDIF}
              if Params.Pixel.BlueBand > 0 then
                wval := RoundS( wval/FWordDivider[Params.Pixel.BlueBand-1] ) ;
              if wval >  255 then
                wval := 255 ;

              _buffer[ i ] := Byte(wval) ;
              inc( i ) ;
            end ;

            if assigned( FBufGreen ) then begin
              {$IFDEF OXYGENE}
                wval := BitConverter.ToInt16( FBufGreen, j + offset ) ;
              {$ELSE}
                wval := PWord( NativeInt(FBufGreen)+j+offset )^ ;
              {$ENDIF}
              if Params.Pixel.GreenBand > 0 then
                wval := RoundS( wval/FWordDivider[Params.Pixel.GreenBand-1] ) ;
              if wval >  255 then
                wval := 255 ;

              _buffer[ i ] := Byte(wval) ;
              inc( i ) ;
            end ;

            if assigned( FBufRed ) then begin
              {$IFDEF OXYGENE}
                wval := BitConverter.ToInt16( FBufRed, j + offset ) ;
              {$ELSE}
                wval := PWord( NativeInt(FBufRed)+j+offset )^ ;
              {$ENDIF}
              if Params.Pixel.RedBand > 0 then
                wval := RoundS( wval/FWordDivider[Params.Pixel.RedBand-1] ) ;
              if wval >  255 then
                wval := 255 ;

              _buffer[ i ] := Byte(wval) ;
              inc( i ) ;
            end ;

            inc( j, 2 ) ;
          end
          else begin
            if assigned( FBufBlue ) then begin
              {$IFDEF OXYGENE}
                _buffer[ i ] := FBufBlue[j + offset] ;
              {$ELSE}
                _buffer[ i ] := PByte( NativeInt(FBufBlue)+j+offset )^ ;
              {$ENDIF}
              inc( i ) ;
            end ;

            if assigned( FBufGreen ) then begin
              {$IFDEF OXYGENE}
                _buffer[ i ] := FBufGreen[j + offset] ;
              {$ELSE}
                _buffer[ i ] := PByte( NativeInt(FBufGreen)+j+offset )^ ;
              {$ENDIF}
              inc( i ) ;
            end ;

            if assigned( FBufRed ) then begin
              {$IFDEF OXYGENE}
                _buffer[ i ] := FBufRed[j + offset] ;
              {$ELSE}
                _buffer[ i ] := PByte( NativeInt( FBufRed )+j+offset )^ ;
              {$ENDIF}
              inc( i ) ;
            end ;

            inc( j ) ;
          end ;
        end ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  function TGIS_LayerGDAL.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    dx  : Integer ;
    zheight, line_nr : Integer ;
    offset : Integer ;
  begin
    Result := True ;

    dx := _pt.X - FStripLeft ;
    if dx < 0 then
      dx := 0 ;

    zheight := RoundS(FBitHeight * FActualZoom) ;
    if _pt.Y >= zheight then
      line_nr := zheight -1
    else
      line_nr := _pt.Y ;

    if line_nr < FStripStart then
      line_nr := FStripStart ;

    line_nr := line_nr -FStripStart ;
    offset := line_nr * FZoomWidth + dx ;

    _ar[0] := FLineBuffer[_pt.X] ;
  end ;

  function TGIS_LayerGDAL.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
   ) : Integer ;
  var
    i, dx   : Integer ;
    zheight,
    line_nr : Integer ;
    offset  : Integer ;
    oln,
    bln     : Integer ;
  begin
    Result := 0 ;
    if (_linenr < 0) or (_linenr > FBitHeight) then exit ;

    if FCalcMinMaxZ then
      prepareMinMaxZ ;

    dx := _startIdx - FStripLeft ;
    if dx < 0 then
      dx := 0 ;

    zheight := RoundS(FBitHeight * FActualZoom) ;
    if _linenr >= zheight then
      line_nr := zheight -1
    else
      line_nr := _linenr ;

    if line_nr < FStripStart then
      line_nr := FStripStart ;

    line_nr := line_nr -FStripStart ;

    offset := line_nr * FZoomWidth + dx ;

    bln := length(_buffer) ;
    if bln = FBandsCount*_count then begin //all pixels values
      oln := length(FLineBuffer) ;
      SetLength(FLineBuffer, _count) ;
      for i := 0 to FBandsCount -1 do begin
        TGIS_FileGDAL(FGDAL).ReadRasterBandGrid(
          i +1, _startIdx, _linenr, _count, 1, FLineBuffer, _count, 1
        ) ;
        for dx := 0 to _count -1 do begin
          _buffer[i +dx] := FLineBuffer[dx] ;
        end ;
      end ;
      SetLength(FLineBuffer, oln) ;
    end
    else begin
      if length(FLineBuffer) > 0 then begin
        for i := 0 to _count -1 do
          _buffer[i] := FLineBuffer[i +offset] ;
      end
      else begin
        if Params.Pixel.GridBand > 0 then begin
          if bln = (FBitWidth * FBitHeight) then
            TGIS_FileGDAL(FGDAL).ReadRasterBandGrid(
              Params.Pixel.GridBand, _startIdx, _linenr, bln, 1, _buffer, FBitWidth, FBitHeight
            )
          else
            TGIS_FileGDAL(FGDAL).ReadRasterBandGrid(
              Params.Pixel.GridBand, _startIdx, _linenr, bln, 1, _buffer, bln, 1
            )
        end ;
      end ;
    end ;

    Result := _count ;
  end ;

  procedure TGIS_LayerGDAL.prepareMinMaxZ(
    const _zoom : Double = -1
   );
  var
    i, k,
    lno    : Integer ;
    zoom   : Double ;
    lines  : Integer ;
    arr    : TGIS_SingleArray ;
  begin
    FCalcMinMaxZ := False ;
    Alive ;

    if FStripHeight <= 0 then
      FStripHeight := BitHeight ;
    if FActualWidth <= 0 then
      FActualWidth := BitWidth ;

    if 900 > FStripHeight then
      lines := FStripHeight
    else
      lines := 900 ;


    zoom := (1.0 * FStripHeight) / lines ;

    SetLength( arr, FActualWidth ) ;
    for i := 0 to lines -1 do begin
      lno := TruncS( i*zoom ) ;
      getNativeLine( arr, lno, 0, FActualWidth ) ;

      for k := 0 to FActualWidth -1 do begin
        if arr[k] <> FNoDataValue then begin
          if arr[k] < FMinZ then
            FMinZ := arr[k]
          else if arr[k] > FMaxZ then
            FMaxZ := arr[k] ;
        end ;
      end ;
    end ;
    SetLength( arr, 0 ) ;
    FExtent3D.ZMin := FMinZ ;
    FExtent3D.ZMax := FMaxZ ;
  end ;

  function TGIS_LayerGDAL.getAlphaLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, dx  : Integer ;
    zheight, line_nr : Integer ;
    offset : Integer ;
  begin
    Result := _bytes ;
    if FDatasize = 0 then exit ;

    if (FNumBands = 1) then begin
      for i := 0 to _bytes -1 do
        _buffer[ i +_offset] := bitPalette[FBufRed[i +_start]].A ;
    end
    else begin
      dx := _offset - FStripLeft ;
      if dx < 0 then
        dx := 0 ;

      zheight := RoundS(FBitHeight * FActualZoom) ;
      if _linenr >= zheight then
        line_nr := zheight -1
      else
        line_nr := _linenr ;

      if line_nr < FStripStart then
        line_nr := FStripStart ;

      line_nr := line_nr -FStripStart ;
      offset := line_nr * FZoomWidth + dx ;

      if assigned( FBufAlpha ) then
        for i := 0 to _bytes -1 do
          _buffer[ i+_offset] := FBufAlpha[i+offset]
      else
        for i := 0 to _bytes -1 do
          _buffer[ i +_offset] := $FF ;
    end ;
  end ;


  function TGIS_LayerGDAL.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    cpath   : String ;
    config  : TStringList ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    if ( CompareText( GetFileExt( _path ), GIS_TTKPS_EXT    ) = 0 ) or
       ( CompareText( GetFileExt( _path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
      config := TStringList.Create ;
      try
        parseConfig( _path, config ) ;
        cpath  := config.Values[GIS_INI_PATH] ;

        cpath := GetPathAbsolute( GetFileDir( _path ), cpath ) ;

        if not assigned( FGDAL ) then begin
          FGDAL := TGIS_FileGDAL.Create( cpath ) ;
          TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
        end ;

        if not TGIS_FileGDAL(FGDAL).PreRecognize then
          Result := False ;
      finally
        FreeObject( config ) ;
      end ;
    end
    else begin
      if SafeFileExists( _path ) then begin
        if not assigned( FGDAL ) then begin
          FGDAL := TGIS_FileGDAL.Create( _path ) ;
          TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
        end ;

        if not TGIS_FileGDAL(FGDAL).PreRecognize then
          Result := False ;
      end
      else if IsServerPath( _path ) then
        Result := False ;
    end ;
  end ;

  {$IFDEF OXYGENE}
   function TGIS_LayerGDAL.GetMetadata(
    const _domain : String
   ) : TGIS_Strings ;
  {$ELSE}
   function TGIS_LayerGDAL.GetMetadata(
    const _domain : String
   ) : TStrings     ;
  {$ENDIF}
  begin
    if not assigned( FGDAL ) then begin
      FGDAL := TGIS_FileGDAL.Create( Path ) ;
      TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
    end ;

    Result := TGIS_FileGDAL(FGDAL).GetMetadata( _domain ) ;
  end ;

  procedure TGIS_LayerGDAL.SetConfigOption(
    const _key : String ;
    const _val : String
  ) ;
  begin
    if not assigned( FGDAL ) then begin
      FGDAL := TGIS_FileGDAL.Create( Path ) ;
      TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
    end ;

    TGIS_FileGDAL(FGDAL).SetConfigOption( _key, _val ) ;
  end ;

  procedure TGIS_LayerGDAL.SetCreateOption(
    const _key : String ;
    const _val : String
  ) ;
  begin
    if not assigned( FGDAL ) then begin
      FGDAL := TGIS_FileGDAL.Create( Path ) ;
      TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
    end ;

    TGIS_FileGDAL(FGDAL).SetCreateOption( _key, _val ) ;
  end ;

  procedure TGIS_LayerGDAL.Build(
    const _path      : String         ;
    const _grid      : Boolean        ;
    const _cs        : TGIS_CSCoordinateSystem  ;
    const _ext       : TGIS_Extent    ;
    const _width     : Integer        ;
    const _height    : Integer ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  var
    pgdal : TGIS_FileGDAL ;
  begin
    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    inherited Build( _path, _grid, _cs, _ext, _width, _height, _subformat ) ;

    pgdal := TGIS_FileGDAL.Create( _path, _ext, _width, _height,
                                   _subformat, 300, CS
                                 ) ;
    try
      if pgdal.CanCreateDataset( _grid ) then
        pgdal.BuildDataset( _grid ) ;
    finally
      FreeObject( pgdal ) ;
    end ;
  end ;

  function TGIS_LayerGDAL.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    pgdal     : TGIS_FileGDAL ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    sx, sy    : Double ;
    grda      : TGIS_GridArray ;
    abrt      : Boolean ;
    cnt       : Integer ;
    pos       : Integer ;
    coptions  : TStrings;
    pixformat : TGIS_PixelFormat ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;

    pgdal := TGIS_FileGDAL.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
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
      coptions := TGIS_FileGDAL(FGDAL).GetCreateOptions ;
      for i := 0 to coptions.Count - 1 do
        pgdal.SetCreateOption( coptions.Names[i], coptions.ValueFromIndex[i] ) ;

      if pgdal.CanCreateDataset( FIsGridImage ) then
        pgdal.BuildDataset( FIsGridImage )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERUNKNOWN ), Path, 0  ) ;

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
      if lw > 0 then
        inc(maxc) ;

      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then
        inc(maxr) ;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
        lw := 0 ;
      end;
      try
        cnt := maxr * maxc ;
        pos := 0 ;
         if pgdal.PixelFormat = TGIS_PixelFormat.Custom then begin
          {$IFDEF OXYGENE}
            grda := new array of Single[ch] ;
            var ii : Integer ;
            for ii := 0 to ch-1 do
              grda[ii] := new Single[cw] ;
          {$ELSE}
            SetLength( grda, ch, cw ) ;
          {$ENDIF}

          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setNoDataTable( grda ) ;
              _layer.GetGrid(ext, grda) ;
              pgdal.WriteGrid(k*cw, i*ch, grda );
              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}
              inc( pos ) ;
            end;
          end ;
        end
        else begin
          pixformat := pgdal.PixelFormat ;
          SetLength(pix, cw*ch) ;
          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setBmpTransparent( pix ) ;
              _layer.GetBitmap(ext, pix, cw, ch) ;
              pgdal.Write(k*cw, i*ch, pix, pixformat, cw, ch );
              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}
              inc( pos ) ;
            end;
          end ;
        end;
      finally
         if pgdal.PixelFormat = TGIS_PixelFormat.Custom then
          grda := nil
        else
          pix := nil ;
      end ;
    finally
      FreeObject(pgdal) ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}
    end;
  end;

  procedure TGIS_LayerGDAL.SaveData ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    pgdal     : TGIS_FileGDAL ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    grda    : TGIS_GridArray ;

  const
    MAX_CELL_WH = 1024 ;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    pgdal := TGIS_FileGDAL.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    try

      if pgdal.CanCreateDataset( FIsGridImage ) then
        pgdal.BuildDataset( FIsGridImage )
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERUNKNOWN ), Path, 0  ) ;

      cw := FBitWidth ;
      ch := FBitHeight ;

      if not FIsGridImage then begin

        if assigned(oBitmap) then begin
          cw := FBitWidth ;
          ch := FBitHeight ;
        end
        else begin
          if FBitHeight <= MAX_CELL_WH then
            ch := FBitHeight
          else
            ch := MAX_CELL_WH ;

          if FBitWidth <= MAX_CELL_WH then
            cw := FBitWidth
          else
            cw := MAX_CELL_WH ;
        end ;

        maxc := FBitWidth div cw ;
        lw := FBitWidth mod cw ;
        if lw > 0 then begin
          lw := 0 ;
          inc(maxc) ;
        end ;
        maxr := FBitHeight div ch ;
        lh := FBitHeight mod ch ;
        if lh > 0 then begin
          lh := 0 ;
          inc(maxr) ;
        end ;

        sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
        sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end ;
        try
          if not assigned(oBitmap) then
            SetLength(pix, cw*ch)
          else
            pix := oBitmap ;

          pixformat := TGIS_PixelFormat.RGB ;

          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              if not assigned(oBitmap) then
                getBitmapData(ext, pix, cw, ch) ;
              pgdal.Write(k*cw, i*ch, pix, pixformat, cw, ch );
            end ;
          end ;
        finally
          if pix <> oBitmap then
            pix := nil ;
        end ;
      end
      else begin
        maxc := FBitWidth div cw ;
        lw   := FBitWidth mod cw ;
        maxr := FBitHeight div ch ;
        lh   := FBitHeight mod ch ;

        sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
        sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

        if (maxr > 0) then begin
          if maxc = 0 then begin
            maxc := 1 ;
            cw := lw ;
            lw := 0 ;
          end ;
          grda :=  InitializeGrid(ch, cw);
        end ;

        for i := 0 to maxr -1 do begin
          ext.YMax := FExtent.YMax -i*(ch*sy) ;
          ext.YMin := ext.YMax -ch*sy ;
          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            getGridData(ext, grda) ;
            pgdal.WriteGrid(k*cw, i*ch, grda);
          end ;
          if lw <> 0 then begin
            grda := nil ;
            grda :=  InitializeGrid(ch, lw);
            ext.XMin := FExtent.XMax -lw*sx;
            ext.XMax := FExtent.XMax ;
            getGridData(ext, grda) ;
            pgdal.WriteGrid(maxc*cw, i*ch, grda );
            grda := nil ;
            grda :=  InitializeGrid(ch, cw);
          end ;
        end ;

        if lh <> 0 then begin
          ext.YMin := FExtent.YMin ;
          ext.YMax := FExtent.YMin +(lh*sy) ;
          if maxc > 0 then begin
            grda := nil ;
            grda :=  InitializeGrid(lh, cw);

            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              getGridData(ext, grda) ;
              pgdal.WriteGrid(k*cw, maxr*ch, grda);
            end ;
          end ;

          if lw <> 0 then begin
            ext.XMin := FExtent.XMax -(lw*sx) ;
            ext.XMax := FExtent.XMax ;
            grda := nil ;
            grda :=  InitializeGrid(lh, lw);
            getGridData(ext, grda) ;
            pgdal.WriteGrid(maxc*cw, maxr*ch, grda);
          end ;
        end ;

        grda := nil ;
      end ;
    finally
      FreeObject( pgdal ) ;
    end ;
  end ;

  function TGIS_LayerGDAL.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    cpath     : String ;
    config    : TStringList ;
    lst       : TStrings ;
    i, k      : Integer ;

    function get_name( const _sdataset : String ) : String ;
    var
      tkn : TGIS_Tokenizer ;
    begin
      Result := _sdataset ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _sdataset, [':'], True ) ;
        if tkn.Result.Count = 3 then
          Result := tkn.Result[2] ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      if ( CompareText( GetFileExt( Path ), GIS_TTKPS_EXT    ) = 0 ) or
         ( CompareText( GetFileExt( Path ), GIS_TTKLAYER_EXT ) = 0 ) then begin
        config := TStringList.Create ;
        try
          parseConfig( Path, config ) ;
          cpath := config.Values[GIS_INI_PATH] ;

          cpath := GetPathAbsolute( GetFileDir( Path ), cpath ) ;

          if not assigned( FGDAL ) then begin
            FGDAL := TGIS_FileGDAL.Create( cpath ) ;
            TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
          end ;

          TGIS_FileGDAL(FGDAL).OpenDataset( False ) ;
        finally
          FreeObject( config ) ;
        end ;
      end
      else begin
        if not assigned( FGDAL ) then begin
          FGDAL := TGIS_FileGDAL.Create( Path ) ;
          TGIS_FileGDAL(FGDAL).DLLPath := FDLLPath ;
        end ;

        TGIS_FileGDAL(FGDAL).OpenDataset( False ) ;
      end ;

      lst := TGIS_FileGDAL(FGDAL).GetMetadata( 'SUBDATASETS' ) ;
      try
        if lst.Count > 0 then begin
          k := 1 ;
          for i := 0 to lst.Count-1 do
            if lst.Names[i] = Format('SUBDATASET_%d_NAME',[k]) then begin
              Result.Add(
                TGIS_LayerInfo.Create(
                  get_name( lst.ValueFromIndex[i] ),
                  TGIS_RegisteredLayerType.Pixel,
                  TGIS_ShapeType.Unknown
                )
              ) ;
              inc( k ) ;
            end ;
        end
        else
          Result.Add(
            TGIS_LayerInfo.Create(
              GetFileNameNoExt( Path ),
              TGIS_RegisteredLayerType.Pixel,
              TGIS_ShapeType.Unknown
            )
          ) ;
      finally
        FreeObject( lst ) ;
      end ;

    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;

  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerGDAL.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'GDAL', 'GDAL Wrapper for raster formats', TGIS_LayerGDAL,
                   '.*;' + GIS_TTKLAYER_PIXEL_FILTER,
                   TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write ],
                   GIS_OTHER_LAYER_PRIORITY, False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerGDAL.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

