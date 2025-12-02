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
  DEM generator to process and visualize grid operations using 3x3 window.
}

{$IFDEF DCC}
  unit GisDem ;
  {$HPPEMIT '#pragma link "GisDem"'}
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
    TatukGIS.RTL   ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisTypes,
    GisLayerPixel,
    GisRtl ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl    ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Basic operation definition to run on dem.
  /// </summary>
  TGIS_DemOperation = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Value for no data.
      /// </summary>
      NoDataValue : Single ;
      /// <summary>
      ///   Width resolution.
      /// </summary>
      XRes        : Double ;
      /// <summary>
      ///   Height resolution.
      /// </summary>
      YRes        : Double ;
      /// <summary>
      ///   The ratio of vertical units to horizontal.
      /// </summary>
      Scale       : Double ;
      /// <summary>
      ///   Data window to process.
      /// </summary>
      DWindow     : array [0..8] of Single ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ; virtual;

      /// <summary>
      ///   Run operation.
      /// </summary>
      /// <returns>
      ///   Calculated value.
      /// </returns>
      function  Run : Single ; virtual;

      /// <summary>
      ///   Prepare operation.
      /// </summary>
      procedure Prepare ; virtual;

      /// <summary>
      ///   Get operation description.
      /// </summary>
      /// <returns>
      ///   text.
      /// </returns>
      function  Description : String ;
  end ;

  /// <summary>
  ///   Degrees or Percent slope mode.
  /// </summary>
  TGIS_DemSlopeMode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   expressed as percentage.
    /// </summary>
    Percent,
    /// <summary>
    ///   expressed as degrees.
    /// </summary>
    Degrees
  ) ;

  /// <summary>
  ///   Generate a slope map from elevation raster.
  /// </summary>
  TGIS_DemOperationSlope = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_DemOperation )
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Slope mode.
      /// </summary>
      Mode : TGIS_DemSlopeMode ;
    public
      /// <inheritdoc/>
      constructor Create ; overload; override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_mode">
      ///   Slope mode
      /// </param>
      /// <param name="_scale">
      ///   Ratio of vertical units to horizontal.
      /// </param>
      constructor Create( const _mode   : TGIS_DemSlopeMode ;
                          const _scale  : Double
                         ) ; reintroduce ; overload;

      /// <inheritdoc/>
      function Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a hydrological slope map from elevation raster.
  /// </summary>
  TGIS_DemOperationSlopeHydro = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_DemOperation )
    {$IFDEF OXYGENE} unit or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Slope mode.
      /// </summary>
      Mode : TGIS_DemSlopeMode ;
    public
      /// <inheritdoc/>
      constructor Create ; overload; override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_mode">
      ///   Slope mode
      /// </param>
      /// <param name="_scale">
      ///   Ratio of vertical units to horizontal.
      /// </param>
      constructor Create( const _mode   : TGIS_DemSlopeMode ;
                          const _scale  : Double
                         ) ; reintroduce ; overload;

      /// <inheritdoc/>
      function Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a shaded relief map from elevation raster.
  /// </summary>
  TGIS_DemOperationHillShade = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_DemOperation )
    private
      sin_alt_rad       : Double ;
      cos_alt_rad_z_sf  : Double ;
      az_rad            : Double ;
      square_z_sf       : Double ;
    private
      ZFactor   : Double ;
      Azimuth   : Double ;
      Altitude  : Double ;
      Combined  : Boolean ;
    public
      /// <inheritdoc/>
      constructor Create ; overload; override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_zfactor">
      ///   vertical exaggeration used to pre-multiply the elevations
      /// </param>
      /// <param name="_azimuth">
      ///   azimuth of the light, in degrees
      /// </param>
      /// <param name="_altitude">
      ///   altitude of the light, in degrees
      /// </param>
      /// <param name="_combined">
      ///   a combination of slope and oblique shading
      /// </param>
      constructor Create( const _zfactor   : Double ;
                          const _azimuth   : Double ;
                          const _altitude  : Double ;
                          const _combined  : Boolean
                         ) ; reintroduce ; overload;

      /// <inheritdoc/>
      function  Run : Single ; override;

      /// <inheritdoc/>
      procedure Prepare ; override;
  end ;

  /// <summary>
  ///   Generate an aspect map from elevation raster.
  /// </summary>
  /// <remarks>
  ///   The values between 0 deg and 360 deg representing the azimuth that slopes are
  ///   facing. The definition of the azimuth is such that : 0 deg means that the
  ///   slope is facing the North, 90 deg it's facing the East, 180 deg it's facing
  ///   the South and 270 deg it's facing the West.
  /// </remarks>
  TGIS_DemOperationAspect = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_DemOperation )
    private
      AngleAsAzimuth : Boolean ;
    public
      /// <inheritdoc/>
      constructor Create ; overload; override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_angleAsAzimuth">
      ///   return trigonometric angle instead of azimuth
      /// </param>
      constructor Create( const _angleAsAzimuth   : Boolean
                         ) ; reintroduce ; overload;

      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a Terrain Ruggedness Index map from elevation raster.
  /// </summary>
  /// <remarks>
  ///   Index, which is defined as the mean difference between a central pixel
  ///   and its surrounding cells.
  /// </remarks>
  TGIS_DemOperationTRI = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_DemOperation )
    public
      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a Topographic Position Index map from elevation raster.
  /// </summary>
  /// <remarks>
  ///   Index, which is defined as the difference between a central pixel
  ///   and the mean of its surrounding cells.
  /// </remarks>
  TGIS_DemOperationTPI = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_DemOperation )
    public
      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a roughness map from elevation raster.
  /// </summary>
  /// <remarks>
  ///   Roughness is the largest inter-cell difference of a central pixel
  ///   and its surrounding cell.
  /// </remarks>
  TGIS_DemOperationRoughness = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_DemOperation )
    public
      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Total Curvature Modes.
  /// </summary>
  TGIS_DemTotalCurvatureMode = {$IFDEF OXYGENE} public {$ENDIF}(
    /// <summary>
    ///   parallel to the slope
    /// </summary>
    Profile,
    /// <summary>
    ///   perpendicular to the direction of the maximum slope
    /// </summary>
    Plan
  ) ;

  /// <summary>
  ///   Generate a total curvature map from elevation raster.
  /// </summary>
  TGIS_DemOperationTotalCurvature = {$IFDEF OXYGENE} public {$ENDIF}
                                      class( TGIS_DemOperation )
    private
      XRes2 : Double ;
      YRes2 : Double ;
      CSA2  : Double ;
    private
      Mode : TGIS_DemTotalCurvatureMode ;
    public
      /// <inheritdoc/>
      constructor Create ; overload;override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_mode">
      ///   curvature mode
      /// </param>
      constructor Create( const _mode   : TGIS_DemTotalCurvatureMode
                         ) ; reintroduce ; overload;
      /// <inheritdoc/>
      function  Run : Single ; override;

      /// <inheritdoc/>
      procedure Prepare ; override;
  end ;

  /// <summary>
  ///   Generate a matrix gained map from elevation raster.
  /// </summary>
  TGIS_DemOperationMatrixGain = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_DemOperation )
    private
      Matrix     : TGIS_Matrix3x3 ;
      GainFactor : Double ;
    public
      /// <inheritdoc/>
      constructor Create ; overload;override;

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_matrix">
      ///   gain matrix
      /// </param>
      /// <param name="_gainFactor">
      ///   gain factor
      /// </param>
      constructor Create( const _matrix     : TGIS_Matrix3x3 ;
                          const _gainFactor : Double
                         ) ; reintroduce ; overload;
      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Generate a flow directions map from elevation raster.
  /// </summary>
  /// <remarks>
  ///   All 8 adjacent directions at a given point can be described using
  ///   the eight-direction pour point model. Values are coded :
  ///   32  64  128 <br />
  ///   16   x    1 <br />
  ///    8   4    2 <br />
  ///  If a cell is lower than its eight neighbors, that cell is given
  ///  the value of its lowest neighbor.
  /// </remarks>
  TGIS_DemOperationFlowDir = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_DemOperation )
    protected
    public
      /// <inheritdoc/>
      constructor Create ; overload;override;

      /// <inheritdoc/>
      function  Run : Single ; override;
  end ;

  /// <summary>
  ///   Fast DEM operations on a 3x3 pixel window.
  /// </summary>
  /// <remarks>
  ///  <para>
  ///    The output DEM is suitable only for visual preview.
  ///  </para>
  ///  <para>
  ///    The algorithm doesn't take the coordinate system into account thus the
  ///    result values are just rough approximations.
  ///  </para>
  ///  <para>
  ///    Generator assumes that x, y and z (elevation) units are identical.
  ///    If x and y units are identical, but z units are different, the
  ///    scale parameter of the operation can be used to set the ratio of vertical
  ///    units to horizontal. For projections near the equator, where units of
  ///    latitude and longitude are similar, elevation (z) units can be converted
  ///    by using scale 370400 (feet) or 111120 (meters).
  ///  </para>
  /// </remarks>
  TGIS_DemGenerator = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_BaseObjectDisposable )
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Process dem operation.
      /// </summary>
      /// <param name="_input">
      ///   source layer
      /// </param>
      /// <param name="_extent">
      ///   extent to process
      /// </param>
      /// <param name="_output">
      ///   destination layer
      /// </param>
      /// <param name="_operation">
      ///   operation to process
      /// </param>
      /// <param name="_onbusy">
      ///   Busy event
      /// </param>
      procedure Process( const _input     : TGIS_LayerPixel ;
                         const _extent    : TGIS_Extent ;
                         const _output    : TGIS_LayerPixel ;
                         const _operation : TGIS_DemOperation ;
                         const _onbusy    : TGIS_BusyEvent
                        ) ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    GisInternals ;
{$ENDIF}

const
  RAD_TO_DEG    = 180.0 / Pi ;
  DEG_TO_RAD    = Pi / 180.0 ;
  SQUARE_M_PI_2 = (Pi*Pi)/4 ;

//=============================================================================
// TGIS_DemGenerator
//=============================================================================

  constructor TGIS_DemGenerator.Create ;
  begin
    inherited ;

  end ;

  procedure TGIS_DemGenerator.Process(
    const _input      : TGIS_LayerPixel ;
    const _extent     : TGIS_Extent ;
    const _output     : TGIS_LayerPixel ;
    const _operation  : TGIS_DemOperation ;
    const _onbusy     : TGIS_BusyEvent
  );
  var
    i,j,k,l : Integer ;
    xsize   : Integer ;
    ysize   : Integer ;
    xscale  : Double ;
    yscale  : Double ;
    buf3l   : TGIS_GridArray ;
    val     : Single ;
    usealg  : Boolean ;
    abrt    : Boolean ;
    l1,l2,l3: Integer ;
    inodata : Single ;
    pix     : TGIS_LayerPixelLock ;
  begin
    if not assigned( _input     ) then exit ;
    if not assigned( _output    ) then exit ;
    if not assigned( _operation ) then exit ;

    xsize   := _input.BitWidth ;
    ysize   := _input.BitHeight ;
    xscale  := ( _extent.XMax - _extent.XMin ) / xsize ;
    yscale  := ( _extent.YMax - _extent.YMin ) / ysize ;
    abrt    := False ;
    inodata := _input.NoDataValue ;

    _operation.NoDataValue := _output.NoDataValue ;
    _operation.XRes        := xscale ;
    _operation.YRes        := yscale ;
    _operation.Prepare ;

    buf3l := InitializeGrid( 3, xsize+1 ) ;

    if assigned( _onbusy ) then
      {$IFDEF OXYGENE}
        _onbusy( nil, new TGIS_BusyEventArgs( -1, -1, abrt ) ) ;
      {$ELSE}
        _onbusy( nil, -1, -1, abrt ) ;
      {$ENDIF}

    _input.SetCurrentFileScale( 1, 1 ) ;

    pix := _output.LockPixels( _extent, _output.CS, False ) ;
    try
      _input.ReadGridLine( buf3l[0], 0, 0, xsize ) ;
      _input.ReadGridLine( buf3l[1], 1, 0, xsize ) ;

      for i := 0 to xsize-1 do begin
        pix.Grid[ 0      , i] :=  _operation.NoDataValue ;
        pix.Grid[ ysize-1, i] := _operation.NoDataValue ;
      end ;

      l1 := 0 ;
      l2 := 1 ;
      l3 := 2 ;
      for i := 1 to ysize-2 do begin
        _input.ReadGridLine( buf3l[l3], i+1, 0, xsize ) ;

        pix.Grid[ i, 0 ]      := _operation.NoDataValue  ;
        pix.Grid[ i, xsize-1] := _operation.NoDataValue  ;

        for j := 1 to xsize - 2 do begin
          _operation.DWindow[0] := buf3l[l1,j-1] ;
          _operation.DWindow[1] := buf3l[l1,j  ] ;
          _operation.DWindow[2] := buf3l[l1,j+1] ;
          _operation.DWindow[3] := buf3l[l2,j-1] ;
          _operation.DWindow[4] := buf3l[l2,j  ] ;
          _operation.DWindow[5] := buf3l[l2,j+1] ;
          _operation.DWindow[6] := buf3l[l3,j-1] ;
          _operation.DWindow[7] := buf3l[l3,j  ] ;
          _operation.DWindow[8] := buf3l[l3,j+1] ;

          usealg := True ;
          val := _operation.NoDataValue ;
          if Abs(_operation.DWindow[4] - inodata) < 1e-10 then begin
            val := _operation.NoDataValue ;
            usealg := False ;
          end
          else begin
            for k := 0 to 8 do begin
              if Abs(_operation.DWindow[k] - inodata) < 1e-10 then begin
                val := _operation.NoDataValue ;
                usealg := False ;
                break ;
              end ;
            end ;
          end ;

          if usealg then
            val := _operation.Run ;

          pix.Grid[i, j] := val ;

          if ( val < _output.MinHeight      ) and
             ( val <> _operation.NoDataValue ) then
            _output.MinHeight := val ;

          if ( val > _output.MaxHeight      ) and
             ( val <> _operation.NoDataValue ) then
            _output.MaxHeight := val ;

          if assigned( _onbusy ) then begin
            if ((i+1) mod 200 ) = 0 then begin
              {$IFDEF OXYGENE}
                _onbusy( nil, new TGIS_BusyEventArgs( (i+1), ysize, abrt ) ) ;
              {$ELSE}
                _onbusy( nil, (i+1), ysize, abrt ) ;
              {$ENDIF}
              if abrt then exit ;
            end ;
          end ;
        end ;

        l  := l1 ;
        l1 := l2 ;
        l2 := l3 ;
        l3 := l ;
      end ;
    finally
      _output.UnlockPixels( pix ) ;
    end ;

    if assigned( _onbusy ) then
      {$IFDEF OXYGENE}
        _onbusy( nil, new TGIS_BusyEventArgs( -1, -1, abrt ) ) ;
      {$ELSE}
        _onbusy( nil, -1, -1, abrt ) ;
      {$ENDIF}
  end ;


//=============================================================================
// TGIS_DemOperation
//=============================================================================

  constructor TGIS_DemOperation.Create ;
  begin
    inherited ;

    XRes  := 1 ;
    YRes  := 1 ;
    Scale := 1 ;
  end ;

  function TGIS_DemOperation.Description: String;
  begin
    Result := Copy( Self.ToString, length('TGIS_DemOperation')+1, MaxInt ) ;
  end;

  procedure TGIS_DemOperation.Prepare ;
  begin

  end ;

  function TGIS_DemOperation.Run : Single ;
  begin
    Result := NoDataValue ;
  end ;


//=============================================================================
// TGIS_DemOperationSlope
//=============================================================================

  constructor TGIS_DemOperationSlope.Create ;
  begin
    inherited ;

    Mode := TGIS_DemSlopeMode.Degrees ;
  end ;

  constructor TGIS_DemOperationSlope.Create(
    const _mode   : TGIS_DemSlopeMode ;
    const _scale  : Double
  ) ;
  begin
    inherited Create ;

    Mode  := _mode ;
    Scale := _scale ;
  end ;

  function TGIS_DemOperationSlope.Run : Single ;
  var
    dx, dy, key : Double ;
  begin
    dx  := (DWindow[3] - DWindow[5]) / XRes ;
    dy  := (DWindow[7] - DWindow[1]) / YRes ;
    key := (dx * dx + dy * dy) ;

    if Mode = TGIS_DemSlopeMode.Degrees then
      Result := ArcTan( Sqrt(key)/(2*Scale) ) * RAD_TO_DEG
    else
      Result := 100*( Sqrt(key) / (2*Scale) ) ;
  end ;


//=============================================================================
// TGIS_DemOperationSlopeHydro
//=============================================================================

  constructor TGIS_DemOperationSlopeHydro.Create;
  begin
    inherited;

    Mode := TGIS_DemSlopeMode.Degrees ;
  end ;

  constructor TGIS_DemOperationSlopeHydro.Create(
    const _mode   : TGIS_DemSlopeMode ;
    const _scale  : Double
  ) ;
  begin
    inherited Create ;

    Mode  := _mode ;
    Scale := _scale ;
  end ;

  function TGIS_DemOperationSlopeHydro.Run : Single ;
  var
    key      : Double ;
    localMin : Single ;
    localMax : Single ;
    k        : Integer ;
  begin
    localMin := DWindow[0] ;
    localMax := DWindow[0] ;

    for k := 1 to 8 do begin
      if (DWindow[k] < localMin) then
        localMin := DWindow[k] ;

      if (DWindow[k] > localMax) then
        localMax := DWindow[k] ;
    end ;

    key := DWindow[4] - localMin;

    if Mode = TGIS_DemSlopeMode.Degrees then
      Result := ArcTan( Sqrt(key) / (2*Scale) ) * RAD_TO_DEG
    else
      Result := 100*( Sqrt(key) / (2*Scale) ) ;
  end ;

//=============================================================================
// TGIS_DemOperationHillShade
//=============================================================================

  constructor TGIS_DemOperationHillShade.Create ;
  begin
    inherited ;

    ZFactor   := 1 ;
    Azimuth   := 315 ;
    Altitude  := 45 ;
    Combined  := False ;
  end ;

  constructor TGIS_DemOperationHillShade.Create(
    const _zfactor  : Double ;
    const _azimuth  : Double ;
    const _altitude : Double ;
    const _combined : Boolean
  ) ;
  begin
    inherited Create ;

    ZFactor   := _zfactor ;
    Azimuth   := _azimuth ;
    Altitude  := _altitude ;
    Combined  := _combined ;
  end ;

  procedure TGIS_DemOperationHillShade.Prepare ;
  var
    z_scale_factor : Double ;
  begin
    inherited ;

    sin_alt_rad      := Sin(Altitude * DEG_TO_RAD) ;
    az_rad           := Azimuth * DEG_TO_RAD ;
    z_scale_factor   := ZFactor / (2*Scale) ;
    cos_alt_rad_z_sf := Cos(Altitude*DEG_TO_RAD) * z_scale_factor ;
    square_z_sf      := z_scale_factor * z_scale_factor ;
  end ;

  function TGIS_DemOperationHillShade.Run : Single ;
  var
    x, y, aspect, xx_plus_yy, cang, slope : Double ;
  begin
    if Combined then begin
      x := (DWindow[3] - DWindow[5]) / XRes ;
      y := (DWindow[7] - DWindow[1]) / YRes ;

      xx_plus_yy := x * x + y * y;

      aspect := ArcTan2(y,x) ;
      slope  := xx_plus_yy * square_z_sf ;
      cang   := ArcCos((sin_alt_rad -
                       cos_alt_rad_z_sf * Sqrt(xx_plus_yy) *
                       Sin(aspect - az_rad)) /
                       Sqrt(1 + slope)
                      ) ;
      cang := 1 - cang * ArcTan(Sqrt(slope)) / SQUARE_M_PI_2 ;
      if (cang <= 0.0) then
        cang := 1.0
      else
        cang := 1.0 + (254.0 * cang) ;

      Result := cang ;
    end
    else begin
      x := (DWindow[3] - DWindow[5]) / XRes ;
      y := (DWindow[7] - DWindow[1]) / YRes ;

      xx_plus_yy := x * x + y * y;
      aspect     := ArcTan2(y,x);
      cang       := (sin_alt_rad -
                     cos_alt_rad_z_sf * Sqrt(xx_plus_yy) *
                     Sin(aspect - az_rad)) /
                     Sqrt(1 + square_z_sf * xx_plus_yy);
      if (cang <= 0.0) then
          cang := 1.0
      else
          cang := 1.0 + (254.0 * cang);

      Result := cang ;
    end ;
  end ;


//=============================================================================
// TGIS_DemOperationAspect
//=============================================================================

  constructor TGIS_DemOperationAspect.Create ;
  begin
    inherited ;

    AngleAsAzimuth := True ;
  end ;

  constructor TGIS_DemOperationAspect.Create(
    const _angleAsAzimuth : Boolean
  ) ;
  begin
    inherited Create ;

    AngleAsAzimuth := _angleAsAzimuth ;
  end ;

  function TGIS_DemOperationAspect.Run : Single ;
  var
    dx, dy : Double ;
    aspect : Single ;
  begin
    dx := (DWindow[5] - DWindow[3]);
    dy := (DWindow[7] - DWindow[1]);

    aspect := ArcTan2(dy,-dx) / DEG_TO_RAD ;

    if (dx = 0) and (dy = 0) then
      aspect := NoDataValue
    else if ( AngleAsAzimuth ) then begin
      if (aspect > 90.0) then
        aspect := 450.0 - aspect
      else
        aspect := 90.0 - aspect ;
    end
    else begin
      if (aspect < 0) then
        aspect := aspect + 360.0 ;
    end ;

    if (aspect = 360.0) then
      aspect := 0.0 ;

    Result := aspect ;
  end ;

//=============================================================================
// TGIS_DemOperationTRI
//=============================================================================

  constructor TGIS_DemOperationTRI.Create ;
  begin
    inherited ;

  end ;

  function TGIS_DemOperationTRI.Run : Single ;
  begin
    Result :=(Abs(DWindow[0]-DWindow[4]) +
              Abs(DWindow[1]-DWindow[4]) +
              Abs(DWindow[2]-DWindow[4]) +
              Abs(DWindow[3]-DWindow[4]) +
              Abs(DWindow[5]-DWindow[4]) +
              Abs(DWindow[6]-DWindow[4]) +
              Abs(DWindow[7]-DWindow[4]) +
              Abs(DWindow[8]-DWindow[4]))/8;
  end ;

//=============================================================================
// TGIS_DemOperationTPI
//=============================================================================

  constructor TGIS_DemOperationTPI.Create ;
  begin
    inherited ;

  end ;

  function TGIS_DemOperationTPI.Run : Single ;
  begin
    Result := DWindow[4] - ( (DWindow[0]+DWindow[1]+DWindow[2]+DWindow[3]+
                              DWindow[5]+DWindow[6]+DWindow[7]+DWindow[8])/8) ;
  end ;

//=============================================================================
// TGIS_DemOperationRoughness
//=============================================================================

  constructor TGIS_DemOperationRoughness.Create ;
  begin
    inherited ;

  end ;

  function TGIS_DemOperationRoughness.Run : Single ;
  var
    roughnessMin : Single ;
    roughnessMax : Single ;
    k : Integer ;
  begin
    roughnessMin := DWindow[0] ;
    roughnessMax := DWindow[0] ;

    for k := 1 to 8 do begin
      if (DWindow[k] > roughnessMax) then
        roughnessMax := DWindow[k] ;
      if (DWindow[k] < roughnessMin) then
        roughnessMin := DWindow[k] ;
    end ;
    Result := roughnessMax - roughnessMin ;
  end ;


//=============================================================================
// TGIS_DemOperationTotalCurvature
//=============================================================================

  constructor TGIS_DemOperationTotalCurvature.Create ;
  begin
    inherited;
    Mode := TGIS_DemTotalCurvatureMode.Profile ;
  end ;

  constructor TGIS_DemOperationTotalCurvature.Create(
    const _mode   : TGIS_DemTotalCurvatureMode
  ) ;
  begin
    inherited Create ;

    Mode  := _mode ;
    Scale := 1 ;
  end ;

  procedure TGIS_DemOperationTotalCurvature.Prepare ;
  begin
    inherited ;

    XRes2 := XRes * XRes ;
    YRes2 := YRes * YRes ;
    CSA2  := ( 4 * XRes * YRes )
  end ;

  function TGIS_DemOperationTotalCurvature.Run : Single ;
  var
    a, b, c, d, e, dx, dy : Double ;
    z1,z2,z3,z4,z5,z6,z7,z8,z9 : Single ;
    prfdenom, plndenom : Double ;
    p1,p2 : Double ;
  begin
    z1 := DWindow[0] ;
    z2 := DWindow[1] ;
    z3 := DWindow[2] ;
    z4 := DWindow[3] ;
    z5 := DWindow[4] ;
    z6 := DWindow[5] ;
    z7 := DWindow[6] ;
    z8 := DWindow[7] ;
    z9 := DWindow[8] ;
    dx := XRes ;
    dy := YRes ;

    a := (z1 + z3 + z4 + z6 + z7 + z9)/(6 * XRes2) - (z2 + z5 + z8)/(3 * XRes2);
    b := (z1 + z2 + z3 + z7 + z8 + z9)/(6 * YRes2) - (z4 + z5 + z6)/(3 * YRes2);
    c := (z3 + z7 - z1 - z9)/CSA2;
    d := (z3 + z6 + z9 - z1 - z4 - z7)/(6 * dx);
    e := (z1 + z2 + z3 - z7 - z8 -z9)/(6 * dy);

    p1 := ( (Sqr(e)+Sqr(d) )* Power(1+Sqr(d)+Sqr(e),1.5) ) ;
    p2 := Power(10,7)* p1 ;
    prfdenom := ( p2 )* Power(10,-7) ;
    plndenom := ( Power(10,7)*Power((Sqr(e)+Sqr(d)),1.5) )*Power(10,-7);

    if (prfdenom = 0 ) or (plndenom = 0) then begin
      if ( (a > 0) and (b > 0)) or ((a < 0) and (b < 0) ) then
        Result := - (a + b) * Scale
      else
        Result := 0 ;
    end
    else begin
    if Mode = TGIS_DemTotalCurvatureMode.Profile then
        Result := Scale * -2 * ( a * Sqr(d) + b * Sqr(e) + c * d * e) / prfdenom
    else
        Result := Scale * -2 * (b * Sqr(d) + a * Sqr(e) - c * d * e) / plndenom;
    end ;

  end ;


//=============================================================================
// TGIS_DemOperationMatrix
//=============================================================================

  constructor TGIS_DemOperationMatrixGain.Create ;
  begin
    inherited ;

    Matrix[1,1] := 0  ;
    Matrix[1,2] := -1 ;
    Matrix[1,3] := 0  ;
    Matrix[2,1] := -1 ;
    Matrix[2,2] := 12 ;
    Matrix[2,3] := -1 ;
    Matrix[3,1] := 0  ;
    Matrix[3,2] := -1 ;
    Matrix[3,3] := 0  ;

    GainFactor := 1 ;
  end ;

  constructor TGIS_DemOperationMatrixGain.Create(
    const _matrix     : TGIS_Matrix3x3 ;
    const _gainFactor : Double
  ) ;
  begin
    inherited Create ;

    Matrix     := _matrix ;
    GainFactor := _gainFactor ;
  end ;

  function TGIS_DemOperationMatrixGain.Run : Single ;
  begin
    Result := ( DWindow[0] * Matrix[1,1] +
                DWindow[1] * Matrix[1,2] +
                DWindow[2] * Matrix[1,3] +
                DWindow[3] * Matrix[2,1] +
                DWindow[4] * Matrix[2,2] +
                DWindow[5] * Matrix[2,3] +
                DWindow[6] * Matrix[3,1] +
                DWindow[7] * Matrix[3,2] +
                DWindow[8] * Matrix[3,3] ) * GainFactor ;
  end ;

//=============================================================================
// TGIS_DemOperationFlowDir
//=============================================================================

  constructor TGIS_DemOperationFlowDir.Create ;
  begin
    inherited ;

  end ;

  function TGIS_DemOperationFlowDir.Run : Single ;
  var
    localMin : Single ;
    k, l, m  : Integer ;
    min_val  : Single ;
  begin
    localMin := DWindow[4] ;
    min_val  := DWindow[0] ;
    l := 4 ;
    m := 0 ;

    for k := 0 to 8 do begin
      if (DWindow[k] < localMin) then begin
        localMin := DWindow[k] ;
        l := k ;
      end
      else begin
        if (k <> 4) and ( DWindow[k] < min_val ) then begin
          min_val := DWindow[k] ;
          m := k ;
        end ;
      end;
    end ;

    if l = 4 then
      l := m ;

    case l of
      0 : Result := 32  ;
      1 : Result := 64  ;
      2 : Result := 128  ;
      3 : Result := 16  ;
      5 : Result := 1 ;
      6 : Result := 8 ;
      7 : Result := 4 ;
      8 : Result := 2
    else  Result := 0  ;
    end

  end ;

//==================================== END =====================================
end.
