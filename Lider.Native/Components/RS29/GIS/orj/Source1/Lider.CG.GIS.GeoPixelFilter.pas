//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  A collection of filters for pixel layers.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoPixelFilter ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoPixelFilter"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Generics.Collections,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoLayerPixel ;
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

  {#gendoc:hide}
  GisPixelFilter = class
    public
      class procedure SelfRegisterPipeline ;
  end ;

  /// <summary>
  ///   Defines the color space in which the filter is to operate.
  /// </summary>
  TGIS_PixelFilterColorSpace = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Each color band of the image is processed separately.
    /// </summary>
    RGB,
    /// <summary>
    ///   Lightness is computed from the RGB bands, processed and transformed
    ///   back to RGB.
    /// </summary>
    HSL
  ) ;


  /// <summary>
  ///   Abstract class for all pixel layer filters.
  /// </summary>
  TGIS_PixelFilterAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                             class( TGIS_Object )
    protected
      /// <summary>
      ///   Size of the filter block (row/column length).
      /// </summary>
      iBlockSize : Integer ;
      /// <summary>
      ///   NoData value of the source layer.
      /// </summary>
      srcNoData  : Single ;
      /// <summary>
      ///   Value (or lightness) of the current pixel of the source layer.
      /// </summary>
      srcValue   : Single ;
      /// <summary>
      ///   Minimum value of the source layer.
      /// </summary>
      srcMinVal  : Single ;
      /// <summary>
      ///   Maximum value of the source layer.
      /// </summary>
      srcMaxVal  : Single ;
      /// <summary>
      ///   NoData value of the destination layer.
      /// </summary>
      dstNoData  : Single ;
      /// <summary>
      ///   Pixel lock of the source layer.
      /// </summary>
      srcLock    : TGIS_LayerPixelLock ;
      /// <summary>
      ///   Bounds of the source layer.
      /// </summary>
      oBounds    : TRect ;
      /// <summary>
      ///   Currently processed block.
      /// </summary>
      aBlock     : TGIS_SingleArray ;
      /// <summary>
      ///   Number of valid values in the current block.
      /// </summary>
      iValueCnt  : Integer ;
      /// <summary>
      ///   If True, then the filter must be prepared before execution;
      ///   False by default.
      /// </summary>
      bPrepare   : Boolean ;
      /// <summary>
      ///   If True, then the filter processes pixel-by-bixel instead of blocks;
      ///   False by default.
      /// </summary>
      bProcPixel : Boolean ;
      /// <summary>
      ///   If True, then the filter is a compound filter.
      /// </summary>
      bCompound  : Boolean ;
    private
      FSourceLayer      : TGIS_LayerPixel ;
      FDestinationLayer : TGIS_LayerPixel ;
      FBand             : Integer ;
      FColorSpace       : TGIS_PixelFilterColorSpace ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FOnBusy : TGIS_BusyEvent ;
    private
      function  fget_Band : Integer ;
      procedure fset_Band ( const _val : Integer
                          ) ;
    protected
      /// <summary>
      ///   Prepares mask array from a string representation(comma-separated
      ///   integers).
      /// </summary>
      /// <param name="_str">
      ///   string representation of the mask
      /// </param>
      /// <param name="_mask">
      ///   destination array
      /// </param>
      procedure prepareMask ( const _str  : String ;
                                var _mask : TGIS_SingleArray
                            ) ;
      /// <summary>
      ///   Prepares a string representation of a mask array.
      /// </summary>
      /// <param name="_mask">
      ///   mask array
      /// </param>
      /// <returns>
      ///   String representation of the mask.
      /// </returns>
     function  maskToString (  var _mask : TGIS_SingleArray
                            ) : String ;
      /// <summary>
      ///   Gets the pixel value for a given position within the pixel lock
      ///   bands.
      /// </summary>
      /// <param name="_h">
      ///   height pixel coordinate
      /// </param>
      /// <param name="_w">
      ///   width pixel coordinate
      /// </param>
      /// <returns>
      ///   Pixel value.
      /// </returns>
      function  getValue   ( const _h : Integer ;
                             const _w : Integer
                           ) : Single ;
      /// <summary>
      ///   Processes image using other filters.
      /// </summary>
      procedure procCompound ; virtual ; abstract ;
      /// <summary>
      ///   Prepares the filter if necessary (if bPrepare=True).
      /// </summary>
      procedure prepFilter ; virtual ; abstract ;
      /// <summary>
      ///   Processes current pixel (if bSkipBlock=True).
      /// </summary>
      /// <returns>
      ///   New pixel value.
      /// </returns>
      function  procPixel  : Single ; virtual ; abstract ;
      /// <summary>
      ///   Processes current block (if bSkipBlock=False).
      /// </summary>
      /// <returns>
      ///   New pixel value.
      /// </returns>
      function  procBlock  : Single ; virtual ; abstract ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Prepares and raises the BusyEvent.
      /// </summary>
      /// <param name="_sender">
      ///   object which raises the event
      /// </param>
      /// <param name="_pos">
      ///   progress indicator
      /// </param>
      /// <param name="_end">
      ///   final value
      /// </param>
      /// <returns>
      ///   True if the process should be aborted
      /// </returns>
      function  raiseBusyEvent ( const _sender : TObject ;
                                 const _pos    : Int64 ;
                                 const _end    : Int64
                               ) : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Applies the filter on the source layer.
      /// </summary>
      procedure Execute ;
    public
      /// <summary>
      ///   Source layer, pixel (image or grid).
      /// </summary>
      property SourceLayer      : TGIS_LayerPixel
                                  read  FSourceLayer
                                  write FSourceLayer ;
      /// <summary>
      ///  Destination layer, pixel (image or grid); if nil then the source
      ///  layer will be altered.
      /// </summary>
      property DestinationLayer : TGIS_LayerPixel
                                  read  FDestinationLayer
                                  write FDestinationLayer ;
      /// <summary>
      ///   Defines the band to be processed; if set to zero then all bands will
      ///   be processed; default is 1.
      /// </summary>
      property Band             : Integer
                                  read  fget_Band
                                  write fset_Band ;
      /// <summary>
      ///   Defines the color space in which the image will be processed;
      ///   default is HSL.
      /// </summary>
      property ColorSpace       : TGIS_PixelFilterColorSpace
                                  read  FColorSpace
                                  write FColorSpace ;
    published
      /// <summary>
      ///   Event fired upon progress of the generation process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  FOnBusy
                             write FOnBusy ;
      {$ENDIF}
  end ;


  /// <summary>
  ///   Threshold filter for pixel layers - if the pixel value is smaller than
  ///   the threshold then it is set to the smallest value in the layer,
  ///   otherwise to the largest.
  /// </summary>
  TGIS_PixelFilterThreshold = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_PixelFilterAbstract )
    private
      FThreshold : Single ;
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      procedure prepFilter ; override ;
      /// <inheritdoc/>
      function  procPixel : Single ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Value of the threshold.
      /// </summary>
      property Threshold : Single
                           read  FThreshold
                           write FThreshold ;
  end ;


  /// <summary>
  ///   Abstract class for all noise pixel layer filters.
  /// </summary>
  TGIS_PixelFilterNoise = {$IFDEF OXYGENE} public abstract {$ENDIF}
                          class( TGIS_PixelFilterAbstract )
    private
      FAmount : Single ;
    private
      function  fget_Amount : Single ;
      procedure fset_Amount ( const _val : Single
                            ) ;
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Percentage amount of noise in the output image;
      ///   varies from 0 to 100.
      /// </summary>
      property Amount : Single
                        read  fget_Amount
                        write fset_Amount ;
  end ;


  /// <summary>
  ///   Salt-And-Pepper noise filter for pixel layers.
  /// </summary>
  TGIS_PixelFilterNoiseSaltPepper = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_PixelFilterNoise )
    protected
      /// <inheritdoc/>
      procedure prepFilter ; override ;
      /// <inheritdoc/>
      function  procPixel : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Gaussian noise filter for pixel layers.
  /// </summary>
  TGIS_PixelFilterNoiseGaussian = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_PixelFilterNoise )
    private
      sMean   : Single ;
      sStdDev : Single ;
    protected
      /// <inheritdoc/>
      procedure prepFilter ; override ;
      /// <inheritdoc/>
      function  procPixel : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Predefined mask filters.
  /// </summary>
  TGIS_PixelFilterMaskType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Low-pass blur filter (3x3 mask size).
    /// </summary>
    LowPass3x3,
    /// <summary>
    ///   Low-pass blur filter (5x5 mask size).
    /// </summary>
    LowPass5x5,
    /// <summary>
    ///   Low-pass blur filter (7x7 mask size).
    /// </summary>
    LowPass7x7,
    /// <summary>
    ///   High-pass sharpen filter (3x3 mask size).
    /// </summary>
    HighPass3x3,
    /// <summary>
    ///   High-pass sharpen filter (5x5 mask size).
    /// </summary>
    HighPass5x5,
    /// <summary>
    ///   High-pass sharpen filter (7x7 mask size).
    /// </summary>
    HighPass7x7,
    /// <summary>
    ///   Gaussian blur filter (3x3 mask size).
    /// </summary>
    Gaussian3x3,
    /// <summary>
    ///   Gaussian blur filter (5x5 mask size).
    /// </summary>
    Gaussian5x5,
    /// <summary>
    ///   Gaussian blur filter (7x7 mask size).
    /// </summary>
    Gaussian7x7,
    /// <summary>
    ///   Laplacian filter (3x3 mask size).
    /// </summary>
    Laplacian3x3,
    /// <summary>
    ///   Laplacian filter (5x5 mask size).
    /// </summary>
    Laplacian5x5,
    /// <summary>
    ///   North gradient filter (3x3 mask size).
    /// </summary>
    GradientNorth,
    /// <summary>
    ///   East gradient filter (3x3 mask size).
    /// </summary>
    GradientEast,
    /// <summary>
    ///   South gradient filter (3x3 mask size).
    /// </summary>
    GradientSouth,
    /// <summary>
    ///   West gradient filter (3x3 mask size).
    /// </summary>
    GradientWest,
    /// <summary>
    ///   Northwest gradient filter (3x3 mask size).
    /// </summary>
    GradientNorthwest,
    /// <summary>
    ///   Northeast gradient filter (3x3 mask size).
    /// </summary>
    GradientNortheast,
    /// <summary>
    ///   Southwest gradient filter (3x3 mask size).
    /// </summary>
    GradientSouthwest,
    /// <summary>
    ///   Southeast gradient filter (3x3 mask size).
    /// </summary>
    GradientSoutheast,
    /// <summary>
    ///   Point (discontinuity) detector (3x3 mask size).
    /// </summary>
    PointDetector,
    /// <summary>
    ///   Horizontal line detector (3x3 mask size).
    /// </summary>
    LineDetectorHorizontal,
    /// <summary>
    ///   Vertical line detector (3x3 mask size).
    /// </summary>
    LineDetectorVertical,
    /// <summary>
    ///   Left diagonal line detector (3x3 mask size).
    /// </summary>
    LineDetectorLeftDiagonal,
    /// <summary>
    ///   Right diagonal line detector (3x3 mask size).
    /// </summary>
    LineDetectorRightDiagonal,
    /// <summary>
    ///   Custom (user-defined) filter.
    /// </summary>
    Custom
  ) ;


  /// <summary>
  ///   Applies mask filters to pixel layers by convolution.
  /// </summary>
  TGIS_PixelFilterConvolution = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_PixelFilterAbstract )
    private
      aMask       : TGIS_SingleArray ;
      aCustomMask : TGIS_SingleArray ;
      bZeroSum    : Boolean ;
    private
      FMaskType : TGIS_PixelFilterMaskType ;
    private
      function  fget_MaskType   : TGIS_PixelFilterMaskType ;
      procedure fset_MaskType   ( const _val : TGIS_PixelFilterMaskType
                                ) ;
      function  fget_CustomMask : String ;
      procedure fset_CustomMask ( const _val : String
                                ) ;
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      procedure prepFilter ; override ;
      /// <inheritdoc/>
      function  procPixel : Single ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Predefined or custom mask to be applied.
      /// </summary>
      property MaskType   : TGIS_PixelFilterMaskType
                            read  fget_MaskType
                            write fset_MaskType ;
      /// <summary>
      ///   Custom mask definition as a comma-separated list of integer values;
      ///   the square root of the number of values must be an odd integer.
      /// </summary>
      property CustomMask : String
                            read  fget_CustomMask
                            write fset_CustomMask ;
  end ;


  /// <summary>
  ///   Abstract class for all pixel layer block filters.
  /// </summary>
  TGIS_PixelFilterBlockStats = {$IFDEF OXYGENE} public abstract {$ENDIF}
                               class( TGIS_PixelFilterAbstract )
    protected
      /// <summary>
      ///   Gets the BlockSize property value.
      /// </summary>
      /// <returns>
      ///   BlockSize property value
      /// </returns>
      function  fget_BlockSize : Integer ; virtual ;
      /// <summary>
      ///   Sets the BlockSize property value.
      /// </summary>
      /// <param name="_val">
      ///   new BlockSize value
      /// </param>
      procedure fset_BlockSize ( const _val : Integer
                               ) ; virtual ;
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      procedure prepFilter ; override ;
      /// <inheritdoc/>
      function  procPixel : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Size of the moving block (number of columns/rows); default is 3
      ///   (for a 3x3 block).
      /// </summary>
      property BlockSize : Integer
                           read  fget_BlockSize
                           write fset_BlockSize ;
  end ;


  /// <summary>
  ///   Sobel magnitude filter for pixel layers - performs edge detection, the
  ///   pixel value is proportional to the value difference at the edge.
  /// </summary>
  TGIS_PixelFilterSobelMagnitude = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_PixelFilterBlockStats )
    private
      hMask : TGIS_SingleArray ;
      vMask : TGIS_SingleArray ;
    protected
      /// <inheritdoc/>
      function  fget_BlockSize : Integer ; override ;
      /// <inheritdoc/>
      procedure fset_BlockSize ( const _val : Integer
                               ) ; override ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Range filter for pixel layers - transforms the pixel value to the
  ///   difference between the maximum and the minimum value in the block.
  /// </summary>
  TGIS_PixelFilterRange = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Midpoint filter for pixel layers - transforms the pixel value to the
  ///   average between the maximum and the minimum value in the block.
  /// </summary>
  TGIS_PixelFilterMidpoint = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Minimum filter for pixel layers - transforms the pixel value to the
  ///   smallest value in the block.
  /// </summary>
  TGIS_PixelFilterMinimum = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Maximum filter for pixel layers - transforms the pixel value to the
  ///   greatest value in the block.
  /// </summary>
  TGIS_PixelFilterMaximum = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Arithmetic mean filter for pixel layers - transforms the pixel value to
  ///   the arithmetic mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterArithmeticMean = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Alpha-trimmed mean filter for pixel layers - transforms the pixel value
  ///   to the alpha-trimmed mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterAlphaTrimmedMean = {$IFDEF OXYGENE} public {$ENDIF}
                                     class( TGIS_PixelFilterBlockStats )
    private
      oValues : TList<Single> ;
    private
      FAlpha : Integer ;
    private
      function  fget_Alpha : Integer ;
      procedure fset_Alpha ( const _val : Integer
                           ) ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Number of values to be trimmed from the top and the bottom
      ///   of the ordered block values list before the mean calculation;
      ///   default is 1.
      /// </summary>
      property Alpha : Integer
                       read  fget_Alpha
                       write fset_Alpha ;
  end ;


  /// <summary>
  ///   Contra-harmonic mean filter for pixel layers - transforms the pixel
  ///   value to the contra-harmonic mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterContraHarmonicMean = {$IFDEF OXYGENE} public {$ENDIF}
                                       class( TGIS_PixelFilterBlockStats )
    private
      FOrder : Integer ;
    private
      function  fget_Order : Integer ;
      procedure fset_Order ( const _val : Integer
                           ) ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Order (exponent) of the mean calculation; default is 2.
      /// </summary>
      property Order : Integer
                       read  fget_Order
                       write fset_Order ;
  end ;


  /// <summary>
  ///   Geometric mean filter for pixel layers - transforms the pixel value to
  ///   the geometric mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterGeometricMean = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Harmonic mean filter for pixel layers - transforms the pixel value to
  ///   the harmonic mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterHarmonicMean = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Weighted mean filter for pixel layers - transforms the pixel value to
  ///   the weighted mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterWeightedMean = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_PixelFilterBlockStats )
    private
      aWeightMask : TGIS_SingleArray ;
    private
      function  fget_WeightMask : String ;
      procedure fset_WeightMask ( const _val : String
                                ) ;
    protected
      /// <inheritdoc/>
      function  fget_BlockSize : Integer ; override ;
      /// <inheritdoc/>
      procedure fset_BlockSize ( const _val : Integer
                               ) ; override ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Weight mask definition as a comma-separated list of integer values;
      ///   the square root of the number of values must be an odd integer.
      /// </summary>
      property WeightMask : String
                            read  fget_WeightMask
                            write fset_WeightMask ;
  end ;


  /// <summary>
  ///   Yp mean filter for pixel layers - transforms the pixel value to the
  ///   Yp mean of the values within the block.
  /// </summary>
  TGIS_PixelFilterYpMean = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_PixelFilterBlockStats )
    private
      FOrder : Integer ;
    private
      function  fget_Order : Integer ;
      procedure fset_Order ( const _val : Integer
                           ) ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Order (exponent) of the mean calculation; default is -2.
      /// </summary>
      property Order : Integer
                       read  fget_Order
                       write fset_Order ;
  end ;


  /// <summary>
  ///   Majority filter for pixel layers - transforms the pixel value to the
  ///   most frequent value in the block (if exists).
  /// </summary>
  TGIS_PixelFilterMajority = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Minority filter for pixel layers- transforms the pixel value to the
  ///   least frequent value in the block (if exists).
  /// </summary>
  TGIS_PixelFilterMinority = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Median filter for pixel layers - transforms the pixel value to the
  ///   median of the values within the block.
  /// </summary>
  TGIS_PixelFilterMedian = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_PixelFilterBlockStats )
    private
      oValues : TList<Single> ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Weighted median filter for pixel layers - transforms the pixel value to
  ///   the weighted median of the values within the block.
  /// </summary>
  TGIS_PixelFilterWeightedMedian = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_PixelFilterBlockStats )
    private
      oValues     : TList<Single> ;
      aWeightMask : TGIS_SingleArray ;
    private
      function  fget_WeightMask : String ;
      procedure fset_WeightMask ( const _val : String
                                ) ;
    protected
      /// <inheritdoc/>
      function  fget_BlockSize : Integer ; override ;
      /// <inheritdoc/>
      procedure fset_BlockSize ( const _val : Integer
                               ) ; override ;
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Weight mask definition as a comma-separated list of integer values;
      ///   the square root of the number of values must be an odd integer.
      /// </summary>
      property WeightMask : String
                            read  fget_WeightMask
                            write fset_WeightMask ;
  end ;


  /// <summary>
  ///   Sum filter for pixel layers - transforms the pixel value to the sum of
  ///   the values within the block.
  /// </summary>
  TGIS_PixelFilterSum = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Standard deviation filter for pixel layers - transforms the pixel value
  ///   to the standard deviation of the values within the block.
  /// </summary>
  TGIS_PixelFilterStandardDeviation = {$IFDEF OXYGENE} public {$ENDIF}
                                      class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Unique value count filter for pixel layers - transforms the pixel value
  ///   to the number of unique values within the block.
  /// </summary>
  TGIS_PixelFilterUniqueCount = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_PixelFilterBlockStats )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Predefined structuring element types.
  /// </summary>
  TGIS_PixelFilterStructuringElementType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Flat square.
    /// </summary>
    Square,
    /// <summary>
    ///   Flat diamond.
    /// </summary>
    Diamond,
    /// <summary>
    ///   Flat disk.
    /// </summary>
    Disk,
    /// <summary>
    ///   Flat horizontal line.
    /// </summary>
    LineHorizontal,
    /// <summary>
    ///   Flat vertical line.
    /// </summary>
    LineVertical,
    /// <summary>
    ///   Flat left diagonal line.
    /// </summary>
    LineLeftDiagonal,
    /// <summary>
    ///   Flat right diagonal line.
    /// </summary>
    LineRightDiagonal,
    /// <summary>
    ///   Custom (user-defined) element.
    /// </summary>
    Custom
  ) ;


  /// <summary>
  ///   Abstract class for all pixel layer morphological filters.
  /// </summary>
  TGIS_PixelFilterMorphological = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                  class( TGIS_PixelFilterBlockStats )
    private
      aMask       : TGIS_SingleArray ;
      aCustomMask : TGIS_SingleArray ;
    private
      FStructuringElementType : TGIS_PixelFilterStructuringElementType ;
    private
      function  fget_StructuringElementType
                               : TGIS_PixelFilterStructuringElementType ;
      procedure fset_StructuringElementType
                               ( const _val :
                                   TGIS_PixelFilterStructuringElementType
                               ) ;
      function  fget_CustomStructuringElement
                               : String ;
      procedure fset_CustomStructuringElement
                               ( const _val : String
                               ) ;
    private
      procedure resetSE  ;
      procedure makeSESquare ;
      procedure makeSEDiamond ;
      procedure makeSEDisk ;
      procedure makeSELineHorizontal ;
      procedure makeSELineVertical ;
      procedure makeSELineLeftDiagonal ;
      procedure makeSELineRightDiagonal ;
    protected
      /// <inheritdoc/>
      function  fget_BlockSize : Integer ; override ;
      /// <inheritdoc/>
      procedure fset_BlockSize ( const _val : Integer
                               ) ; override ;
    protected
      /// <summary>
      ///   Returns the structuring element.
      /// </summary>
      /// <returns>
      ///   Structuring element.
      /// </returns>
      function  getStructuringElement : TGIS_SingleArray ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Predefined or custom structuring element type.
      /// </summary>
      property StructuringElementType
                          : TGIS_PixelFilterStructuringElementType
                            read  fget_StructuringElementType
                            write fset_StructuringElementType ;
      /// <summary>
      ///   Custom structuring element definition as a comma-separated list of
      ///   zeros and ones; the square root of the number of values must be an
      ///   odd integer.
      /// </summary>
      property CustomStructuringElement
                          : String
                            read  fget_CustomStructuringElement
                            write fset_CustomStructuringElement ;
  end ;


  /// <summary>
  ///   Erosion filter for pixel layers - transforms the pixel value to the
  ///   smallest difference of the value in the block and the mask value.
  /// </summary>
  TGIS_PixelFilterErosion = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Dilation filter for pixel layers - transforms the pixel value to the
  ///   greatest sum of the value in the block and the mask value.
  /// </summary>
  TGIS_PixelFilterDilation = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Opening filter for pixel layers - two-stage filter, applies erosion
  ///   followed by dilation.
  /// </summary>
  TGIS_PixelFilterOpening = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Closing filter for pixel layers - two-stage filter, applies dilation
  ///   followed by erosion.
  /// </summary>
  TGIS_PixelFilterClosing = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Top-hat filter for pixel layers - three-stage filter, computes the
  ///   difference of the original image with its opening (erosion followed
  ///   by dilation).
  /// </summary>
  TGIS_PixelFilterTopHat = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


  /// <summary>
  ///   Bottom-hat filter for pixel layers - three-stage filter, computes the
  ///   difference of the closing (dilation followed by erosion) of the image
  ///   with the original image.
  /// </summary>
  TGIS_PixelFilterBottomHat = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PixelFilterMorphological )
    protected
      /// <inheritdoc/>
      procedure procCompound ; override ;
      /// <inheritdoc/>
      function  procBlock : Single ; override ;
    public
      /// <inheritdoc/>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,

    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoPipeline ;
{$ENDIF}

const
  GIS_PIXEL_FILTER_MASK_NEUTRAL_3 : String =
    '0,0,0,' +
    '0,1,0,' +
    '0,0,0' ;

  GIS_PIXEL_FILTER_MASK_LOWPASS_3 : String =
    '1,1,1,' +
    '1,4,1,' +
    '1,1,1' ;
  GIS_PIXEL_FILTER_MASK_LOWPASS_3_STRONG : String =
    '1,2,1,' +
    '2,4,2,' +
    '1,2,1' ;
  GIS_PIXEL_FILTER_MASK_LOWPASS_5 : String =
    '1,1,1,1,1,' +
    '1,2,2,2,1,' +
    '1,2,8,2,1,' +
    '1,2,2,2,1,' +
    '1,1,1,1,1' ;
  GIS_PIXEL_FILTER_MASK_LOWPASS_5_STRONG : String =
     '1,1,1,1,1,' +
     '1,4,4,4,1,' +
     '1,4,12,4,1,' +
     '1,4,4,4,1,' +
     '1,1,1,1,1' ;
  GIS_PIXEL_FILTER_MASK_LOWPASS_7 : String =
    '1,1,1,1,1,1,1,' +
    '1,1,1,1,1,1,1,' +
    '1,1,4,4,4,1,1,' +
    '1,1,4,12,4,1,1,' +
    '1,1,4,4,4,1,1,' +
    '1,1,1,1,1,1,1,' +
    '1,1,1,1,1,1,1' ;

  GIS_PIXEL_FILTER_MASK_HIGHPASS_3 : String =
    '0,-1,0,' +
    '-1,5,-1,' +
    '0,-1,0' ;
  GIS_PIXEL_FILTER_MASK_HIGHPASS_3_STRONG : String =
    '-1,-1,-1,' +
    '-1,9,-1,' +
    '-1,-1,-1' ;
  GIS_PIXEL_FILTER_MASK_HIGHPASS_5 : String =
    '0,-1,-1,-1,0,' +
    '-1,2,-4,2,-1,' +
    '-1,-4,13,-4,-1,' +
    '-1,2,-4,2,-1,' +
    '0,-1,-1,-1,0' ;
  GIS_PIXEL_FILTER_MASK_HIGHPASS_7 : String =
    '0,0,-1,-2,-1,0,0,' +
    '0,1,-2,-4,-2,1,0,' +
    '-1,-2,2,-6,2,-2,-1,' +
    '-2,-1,0,12,0,-1,-2,' +
    '-1,-2,2,-6,2,-2,-1,' +
    '0,1,-2,-4,-2,1,0,' +
    '0,0,-1,-2,-1,0,0' ;

  GIS_PIXEL_FILTER_MASK_GAUSSIAN_3 : String =
    '1,4,1,' +
    '4,12,4,' +
    '1,4,1' ;
  GIS_PIXEL_FILTER_MASK_GAUSSIAN_5 : String =
    '0,1,2,1,0,' +
    '1,4,8,4,1,' +
    '2,8,16,8,2,' +
    '1,4,8,4,1,' +
    '0,1,2,1,0' ;
  GIS_PIXEL_FILTER_MASK_GAUSSIAN_7 : String =
    '1,1,2,2,2,1,1,' +
    '1,2,2,4,2,2,1,' +
    '2,2,4,8,4,2,2,' +
    '2,4,8,16,8,4,2,' +
    '2,2,4,8,4,2,2,' +
    '1,2,2,4,2,2,1,' +
    '1,1,2,2,2,1,1' ;

  GIS_PIXEL_FILTER_MASK_LAPLACIAN_3 : String =
    '0,-1,0,' +
    '-1,4,-1,' +
    '0,-1,0' ;
  GIS_PIXEL_FILTER_MASK_LAPLACIAN_5 : String =
    '-1,-1,-1,-1,-1,' +
    '-1,-1,-1,-1,-1,' +
    '-1,-1,24,-1,-1,' +
    '-1,-1,-1,-1,-1,' +
    '-1,-1,-1,-1,-1' ;

  GIS_PIXEL_FILTER_MASK_GRADIENT_NORTH : String =
    '-1,-2,-1,' +
    '0,0,0,' +
    '1,2,1' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_EAST : String =
    '1,0,-1,' +
    '2,0,-2,' +
    '1,0,-1' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTH : String =
    '1,2,1,' +
    '0,0,0,' +
    '-1,-2,-1' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_WEST : String =
    '-1,0,1,' +
    '-2,0,2,' +
    '-1,0,1' ;

  GIS_PIXEL_FILTER_MASK_GRADIENT_NORTHWEST : String =
    '-2,-1,0,' +
    '-1,0,1,' +
    '0,1,2' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_NORTHEAST : String =
    '0,-1,-2,' +
    '1,0,-1,' +
    '2,1,0' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTHWEST : String =
    '0,1,2,' +
    '-1,0,1,' +
    '-2,-1,0' ;
  GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTHEAST : String =
    '2,1,0,' +
    '1,0,-1,' +
    '0,-1,-2' ;

  GIS_PIXEL_FILTER_MASK_POINT_DETECTOR : String =
    '-1,-1,-1,' +
    '-1,8,-1,' +
    '-1,-1,-1' ;

  GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_HORIZONTAL : String =
    '-1,-1,-1,' +
    '2,2,2,' +
    '-1,-1,-1' ;
  GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_VERTICAL : String =
    '-1,2,-1,' +
    '-1,2,-1,' +
    '-1,2,-1' ;
  GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_LEFT_DIAGONAL : String =
    '2,-1,-1,' +
    '-1,2,-1,' +
    '-1,-1,2' ;
  GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_RIGHT_DIAGONAL : String =
    '-1,-1,2,' +
    '-1,2,-1,' +
    '2,-1,-1' ;

  GIS_PIXEL_FILTER_MASK_WEIGHT : String =
    '1,1,1,1,1,' +
    '1,2,2,2,1,' +
    '1,2,3,2,1,' +
    '1,2,2,2,1,' +
    '1,1,1,1,1' ;

type

  T_Pipeline_FilterThreshold = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterNoiseSaltPepper = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterNoiseGaussian = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterConvolution = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterSobelMagnitude = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterRange = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMidpoint = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMinimum = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMaximum = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterArithmeticMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterAlphaTrimmedMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterContraHarmonicMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterGeometricMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterHarmonicMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterWeightedMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterYpMean = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMajority = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMinority = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterMedian = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterWeightedMedian = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterSum = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterStandardDeviation = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterUniqueCount = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterErosion = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterDilation = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterOpening = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterClosing = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterTopHat = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_Pipeline_FilterBottomHat = class( TGIS_PipelineOperationExtendedAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;


type

  T_random = class( TGIS_BaseObjectDisposable )
    const LOCAL_MAX_RANDOM : Integer = 10000 ;
    const LOCAL_CHARS : array[0..61] of Char = (
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
      'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
      'U', 'V', 'W', 'X', 'Y', 'Z',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
      'u', 'v', 'w', 'x', 'y', 'z'
    ) ;
    const LOCAL_CHARS_LENGTH : Integer = 62 ;
    {$IFDEF CLR}
      private
        oGenerator : Random ;
    {$ENDIF}
    {$IFDEF JAVA}
      private
        oGenerator : Random ;
    {$ENDIF}
    private
      bGen : Boolean ;
      Z1   : Single ;
    public
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      function  Uniform  ( const _range  : Integer
                         ) : Integer ;
      function  Gaussian ( const _mean   : Single ;
                           const _sdev   : Single
                         ) : Single ;
      function  AlphanumericString
                         ( const _length : Integer
                         ) : String ;
  end ;

var
  oRandom : T_random ;

type

  /// <summary>
  ///   Types of pixel operations.
  /// </summary>
  T_pixelOperationType = (
    /// <summary>
    ///   Result is a pixel-by-pixel sum of source layers
    ///   (SourceLayer1 + SourceLayer2).
    /// </summary>
    Add,
    /// <summary>
    ///   Result is a pixel-by-pixel difference of source layers
    ///   (SourceLayer1 - SourceLayer2).
    /// </summary>
    Subtract,
    /// <summary>
    ///   Result is a pixel-by-pixel product of source layers
    ///   (SourceLayer1 * SourceLayer2).
    /// </summary>
    Multiply,
    /// <summary>
    ///   Result is a pixel-by-pixel quotient of source layers
    ///   (SourceLayer1 / SourceLayer2).
    /// </summary>
    Divide,
    /// <summary>
    ///   Result is the first source layer to the power of the second source
    ///   layer (pixel-by-pixel, SourceLayer1 ^ SourceLayer2).
    /// </summary>
    Power
  ) ;


  /// <summary>
  ///   Encapsulates basic pixel (raster, image) arithmetic routines.
  /// </summary>
  T_pixelArithmetic = class( TGIS_Object )
    private
      FSourceLayer1     : TGIS_LayerPixel ;
      FSourceLayer2     : TGIS_LayerPixel ;
      FDestinationLayer : TGIS_LayerPixel ;
      FBand             : Integer ;
      FColorSpace       : TGIS_PixelFilterColorSpace ;
      FExtent           : TGIS_Extent ;
      FPixelOperation   : T_pixelOperationType ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FOnBusy : TGIS_BusyEvent ;
    private
      function  fget_Band : Integer ;
      procedure fset_Band ( const _val : Integer
                          ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Prepares and raises the BusyEvent.
      /// </summary>
      /// <param name="_sender">
      ///   object which raises the event
      /// </param>
      /// <param name="_pos">
      ///   progress indicator
      /// </param>
      /// <param name="_end">
      ///   final value
      /// </param>
      /// <returns>
      ///   True if the process should be aborted
      /// </returns>
      function  raiseBusyEvent ( const _sender : TObject ;
                                 const _pos    : Int64 ;
                                 const _end    : Int64
                               ) : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Performs the pixel operation.
      /// </summary>
      procedure Execute ;
    public
      /// <summary>
      ///   First source pixel layer (image or grid).
      /// </summary>
      property SourceLayer1     : TGIS_LayerPixel
                                  read  FSourceLayer1
                                  write FSourceLayer1 ;
      /// <summary>
      ///   Second source pixel layer (image or grid).
      /// </summary>
      property SourceLayer2     : TGIS_LayerPixel
                                  read  FSourceLayer2
                                  write FSourceLayer2 ;
      /// <summary>
      ///  Destination pixel layer (image or grid).
      /// </summary>
      property DestinationLayer : TGIS_LayerPixel
                                  read  FDestinationLayer
                                  write FDestinationLayer ;
      /// <summary>
      ///   The extent in which the operation is to be performed.
      /// </summary>
      property Extent           : TGIS_Extent
                                  read  FExtent
                                  write FExtent ;
      /// <summary>
      ///   Defines the band for which the operation is to be performed; if set
      ///   to zero then all bands will be used; default is 1.
      /// </summary>
      property Band             : Integer
                                  read  fget_Band
                                  write fset_Band ;
      /// <summary>
      ///   Defines the color space in which the operation is to be performed;
      ///   default is HSL.
      /// </summary>
      property ColorSpace       : TGIS_PixelFilterColorSpace
                                  read  FColorSpace
                                  write FColorSpace ;
      /// <summary>
      ///   The pixel operation to be performed.
      /// </summary>
      property Operation        : T_pixelOperationType
                                  read  FPixelOperation
                                  write FPixelOperation ;
    published
      /// <summary>
      ///   Event fired upon progress of the process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  FOnBusy
                             write FOnBusy ;
      {$ENDIF}
  end ;


//==============================================================================
// local utilities
//==============================================================================

  function RandomGenerator : T_random ;
  begin
    if not assigned( oRandom ) then
      oRandom := T_random.Create ;

    Result := oRandom ;
  end ;


//==============================================================================
// T_random
//==============================================================================

  constructor T_random.Create ;
  begin
    inherited ;

    bGen := False ;

    {$IFDEF DCC}
      Randomize ;
    {$ENDIF}
    {$IFDEF CLR}
      oGenerator := new Random ;
    {$ENDIF}
    {$IFDEF JAVA}
      oGenerator := new Random ;
    {$ENDIF}
  end ;


  procedure T_random.doDestroy ;
  begin

    inherited ;
  end ;


  function T_random.Uniform(
    const _range : Integer
  ) : Integer ;
  begin
    {$IFDEF DCC}
      Result := Random( _range ) ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := oGenerator.Next( _range ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := oGenerator.nextInt( _range ) ;
    {$ENDIF}
  end ;


  function T_random.Gaussian(
    const _mean : Single ;
    const _sdev : Single
  ) : Single ;
  var
    res : Single ;
    u1  : Single ;
    u2  : Single ;
    z0  : Single ;
  begin
    bGen := not bGen ;

    if bGen then begin
      repeat
        u1 := Uniform( LOCAL_MAX_RANDOM )*( 1.0/LOCAL_MAX_RANDOM ) ;
        u2 := Uniform( LOCAL_MAX_RANDOM )*( 1.0/LOCAL_MAX_RANDOM ) ;
      until( u1 > 0.0 ) ;

      u1 := Sqrt( -2.0*Ln( u1 ) ) ;
      u2 := 2*Pi*u2 ;

      z0 := u1*Cos( u2 ) ;
      Z1 := u1*Sin( u2 ) ;

      res := _mean + z0*_sdev ;
    end
    else
      res := _mean + Z1*_sdev ;

    Result := res ;
  end ;


  function T_random.AlphanumericString(
    const _length : Integer
  ) : String ;
  var
    sb  : TStringBuilder ;
    res : String ;
    i   : Integer ;
  begin
    sb := TStringBuilder.Create ;
    try
      for i := 0 to _length - 1 do
        sb.Append(LOCAL_CHARS[Uniform( LOCAL_CHARS_LENGTH )]) ;
      res := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


//==============================================================================
// T_pixelArithmetic
//==============================================================================

  constructor T_pixelArithmetic.Create ;
  begin
    inherited ;

    FSourceLayer1 := nil ;
    FSourceLayer2 := nil ;
    FDestinationLayer := nil ;
    FBand := 1 ;
    FColorSpace := TGIS_PixelFilterColorSpace.HSL ;
    FExtent := GisWholeWorld ;
    FPixelOperation := T_pixelOperationType.Add ;
  end ;


  procedure T_pixelArithmetic.doDestroy ;
  begin

    inherited ;
  end ;


  function T_pixelArithmetic.fget_Band
    : Integer ;
  begin
    Result := FBand ;
  end ;


  procedure T_pixelArithmetic.fset_Band(
    const _val : Integer
  ) ;
  begin
    if _val < 0 then
      exit ;

    FBand := _val ;
  end ;


  function T_pixelArithmetic.raiseBusyEvent(
    const _sender : TObject ;
    const _pos    : Int64 ;
    const _end    : Int64
  ) : Boolean ;
  var
    abrt   : Boolean ;
    {$IFDEF OXYGENE}
      args : TGIS_BusyEventArgs ;
    {$ENDIF}
  begin
    Result := False ;

    if assigned( FOnBusy ) then begin
      abrt := False ;
      {$IFDEF OXYGENE}
        args := TGIS_BusyEventArgs.Create( _pos, _end, abrt ) ;
        FOnBusy( _sender, args ) ;
        Result := args.Abort ;
      {$ELSE}
        FOnBusy( _sender, _pos, _end, abrt ) ;
        Result := abrt ;
      {$ENDIF}
    end ;
  end ;


  procedure T_pixelArithmetic.Execute ;
  const
    LOCAL_PARAM_SOURCE_1    : String = 'SourceLayer1' ;
    LOCAL_PARAM_SOURCE_2    : String = 'SourceLayer2' ;
    LOCAL_PARAM_DESTINATION : String = 'DestinationLayer' ;
    LOCAL_PARAM_BAND        : String = 'Band' ;
    LOCAL_PARAM_EXTENT      : String = 'Extent' ;
  var
    src1     : TGIS_LayerPixel ;
    src2     : TGIS_LayerPixel ;
    dst      : TGIS_LayerPixel ;
    src1_lck : TGIS_LayerPixelLock ;
    src2_lck : TGIS_LayerPixelLock ;
    dst_lck  : TGIS_LayerPixelLock ;
    ext      : TGIS_Extent ;
    ext1     : TGIS_Extent ;
    ext2     : TGIS_Extent ;
    extd     : TGIS_Extent ;
    left1    : Integer ;
    top1     : Integer ;
    left2    : Integer ;
    top2     : Integer ;
    leftd    : Integer ;
    topd     : Integer ;
    wdth     : Integer ;
    hght     : Integer ;
    sband    : Integer ;
    eband    : Integer ;
    bgrd     : Boolean ;
    val1     : Single ;
    val2     : Single ;
    val      : Single ;
    vmin     : Single ;
    vmax     : Single ;
    w        : Integer ;
    h        : Integer ;
    i        : Integer ;

    // color extraction RGB->HSL
    clr      : TGIS_Color ;
    hsl1     : TGIS_DoubleArray ;
    hsl2     : TGIS_DoubleArray ;

    // busy event
    total    : Integer ;
    prgs     : Integer ;
    abrt     : Boolean ;

    procedure event_start ;
    begin
      abrt := raiseBusyEvent( Self, 0, total ) ;
    end ;

    procedure event_progress ;
    begin
      abrt := raiseBusyEvent( Self, prgs, total ) ;
    end ;

    procedure event_end ;
    begin
      abrt := raiseBusyEvent( Self, -1, -1 ) ;
    end ;

    function get_value1 : Single ;
    var
      rr : Single ;
    begin
      if bgrd then begin
        rr := src1_lck.Grid[top1+h,left1+w] ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          rr := src1_lck.Band[top1+h,left1+w] ;
        end
        else begin
          clr := TGIS_Color.FromARGB(
            Cardinal( src1_lck.Bitmap[src1_lck.BitmapPos( left1+w, top1+h )] )
          ) ;
          clr.ToAHSL( hsl1[0], hsl1[1], hsl1[2], hsl1[3] ) ;
          rr := hsl1[3] ;
        end ;
      end ;

      Result := rr ;
    end ;

    function get_value2 : Single ;
    var
      rr : Single ;
    begin
      if bgrd then begin
        rr := src2_lck.Grid[top2+h,left2+w] ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          rr := src2_lck.Band[top2+h,left2+w] ;
        end
        else begin
          clr := TGIS_Color.FromARGB(
            Cardinal( src2_lck.Bitmap[src2_lck.BitmapPos( left2+w, top2+h )] )
          ) ;
          clr.ToAHSL( hsl2[0], hsl2[1], hsl2[2], hsl2[3] ) ;
          rr := hsl2[3] ;
        end ;
      end ;

      Result := rr ;
    end ;

    function get_valued : Single ;
    var
      rr : Single ;
    begin
      if bgrd then begin
        rr := dst_lck.Grid[top2+h,left2+w] ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          rr := dst_lck.Band[top2+h,left2+w] ;
        end
        else begin
          clr := TGIS_Color.FromARGB(
            Cardinal( dst_lck.Bitmap[dst_lck.BitmapPos( left2+w, top2+h )] )
          ) ;
          clr.ToAHSL( hsl2[0], hsl2[1], hsl2[2], hsl2[3] ) ;
          rr := hsl2[3] ;
        end ;
      end ;

      Result := rr ;
    end ;

    procedure set_value ;
    begin
      if bgrd then begin
        dst_lck.Grid[topd+h,leftd+w] := val ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          dst_lck.Band[topd+h,leftd+w] := val ;
        end
        else begin
          if val < 0.0 then
            val := 0.0
          else
          if val > 1.0 then
            val := 1.0 ;
          clr := TGIS_Color.FromAHSL( hsl1[0], hsl1[1], hsl1[2], val ) ;
          dst_lck.Bitmap[dst_lck.BitmapPos( leftd+w, topd+h )] :=
            Integer( clr.ToARGB ) ;
        end ;
      end ;
    end ;

    procedure set_min_max ;
    begin
      if not bgrd then
        exit ;

      dst.MinHeight := Min( vmin, dst.MinHeight ) ;
      dst.MaxHeight := Max( vmax, dst.MaxHeight ) ;
    end ;

  begin
    src1 := FSourceLayer1 ;
    src2 := FSourceLayer2 ;
    dst := FDestinationLayer ;

    if not assigned( src1 ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_1, 1
      ) ;

    if not assigned( src2 ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_2, 1
      ) ;

    if not assigned( dst ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DESTINATION, 1
      ) ;

    if src1.IsGrid <> dst.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_1, 2
      ) ;

    if src2.IsGrid <> dst.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_2, 2
      ) ;

    if FBand > dst.BandsCount then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_BAND, 1
      ) ;

    extd := dst.Extent ;
    ext1 := dst.CS.ExtentFromCS( src1.CS, src1.Extent ) ;
    ext2 := dst.CS.ExtentFromCS( src2.CS, src2.Extent ) ;
    ext := FExtent ;
    ext := GisCommonExtent( ext, extd ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_EXTENT, 1
      ) ;

    ext := GisCommonExtent( ext, ext1 ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_1, 3
      ) ;

    ext := GisCommonExtent( ext, ext2 ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE_2, 3
      ) ;

    bgrd := src1.IsGrid and ( FBand = 1 ) ;

    src1.Open ;
    src2.Open ;
    dst.Open ;

    if ( not bgrd ) and
       ( ColorSpace = TGIS_PixelFilterColorSpace.HSL ) then begin
      SetLength( hsl1, 4 ) ;
      SetLength( hsl2, 4 ) ;
    end ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;
    if bgrd then begin
      sband := 0 ;
      eband := 0 ;
    end
    else
    if FBand = 0 then begin
      sband := 1 ;
      eband := src1.BandsCount ;
    end
    else begin
      sband := FBand ;
      eband := FBand ;
    end ;

    for i := sband to eband do begin

      if bgrd or ( FColorSpace = TGIS_PixelFilterColorSpace.HSL ) then begin
        dst_lck := dst.LockPixels( ext, dst.CS, True ) ;
        src1_lck := src1.LockPixels( ext, dst.CS, dst_lck.PixelSize.X, False ) ;
        src2_lck := src2.LockPixels( ext, dst.CS, dst_lck.PixelSize.X, False ) ;
      end
      else begin
        dst_lck := dst.LockPixels( ext, dst.CS, i, True ) ;
        src1_lck := src1.LockPixels( ext, dst.CS, dst_lck.PixelSize.X, i, False ) ;
        src2_lck := src2.LockPixels( ext, dst.CS, dst_lck.PixelSize.X, i, False ) ;
      end ;
      try
        left1 := src1_lck.Bounds.Left ;
        top1  := src1_lck.Bounds.Top ;
        left2 := src2_lck.Bounds.Left ;
        top2  := src2_lck.Bounds.Top ;
        leftd := dst_lck.Bounds.Left ;
        topd  := dst_lck.Bounds.Top ;

        wdth := src1_lck.Bounds.Width ;
        wdth := Min( wdth, src2_lck.Bounds.Width ) ;
        wdth := Min( wdth, dst_lck.Bounds.Width ) ;

        hght := src1_lck.Bounds.Height ;
        hght := Min( wdth, src2_lck.Bounds.Height ) ;
        hght := Min( wdth, dst_lck.Bounds.Height ) ;

        total := ( eband - sband + 1 )*( hght + 1 )*( wdth + 1 ) ;
        event_start ;
        prgs := 0 ;
        for h := 0 to hght do begin
          event_progress ;
          if abrt then begin
            set_min_max ;
            exit ;
          end ;

          for w := 0 to wdth do begin
            prgs := ( i-sband )*( hght + 1 )*( wdth + 1 ) +
                    h*( wdth + 1 ) + w + 1 ;

            val1 := get_value1 ;
            val2 := get_value2 ;
            if ( val1 = src1.NoDataValue ) or
               ( val2 = src2.NoDataValue ) then begin
              val := get_valued ;
            end
            else begin
              case FPixelOperation of
                T_pixelOperationType.Add :
                  val := val1 + val2 ;
                T_pixelOperationType.Subtract :
                  val := val1 - val2 ;
                T_pixelOperationType.Multiply :
                  val := val1 * val2 ;
                T_pixelOperationType.Divide :
                  val := val1 / val2 ;
                T_pixelOperationType.Power :
                  val := Power( val1, val2 ) ;
              end ;
            end ;

            set_value ;
            if bgrd and ( val <> dst.NoDataValue ) then begin
              vmin := Min( val, vmin ) ;
              vmax := Max( val, vmax ) ;
            end ;
          end ;
        end ;
        event_end ;
        set_min_max ;

      finally
        src1.UnlockPixels( src1_lck ) ;
        src2.UnlockPixels( src2_lck ) ;
        dst.UnlockPixels( dst_lck ) ;
      end ;
    end ;
  end ;


//==============================================================================
// T_Pipeline_FilterThreshold
//==============================================================================

  procedure T_Pipeline_FilterThreshold.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'Threshold',
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      True,
      '0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterThreshold.Execute ;
  var
    fltr   : TGIS_PixelFilterThreshold ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    thr    : Single ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    thr := ParamAsFloat( 'Threshold' ) ;

    fltr := TGIS_PixelFilterThreshold.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.Threshold := thr ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterNoiseSaltPepper
//==============================================================================

  procedure T_Pipeline_FilterNoiseSaltPepper.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'Amount',
      TGIS_PipelineParameterType.Float,
      0.0,
      100.0,
      False,
      '10',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterNoiseSaltPepper.Execute ;
  var
    fltr   : TGIS_PixelFilterNoiseSaltPepper ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    amount : Single ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    amount := ParamAsFloat( 'Amount' ) ;

    fltr := TGIS_PixelFilterNoiseSaltPepper.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.Amount := amount ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterNoiseGaussian
//==============================================================================

  procedure T_Pipeline_FilterNoiseGaussian.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'Amount',
      TGIS_PipelineParameterType.Float,
      0.0,
      100.0,
      False,
      '10',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterNoiseGaussian.Execute ;
  var
    fltr   : TGIS_PixelFilterNoiseGaussian ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    amount : Single ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    amount := ParamAsFloat( 'Amount' ) ;

    fltr := TGIS_PixelFilterNoiseGaussian.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.Amount := amount ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}

      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterConvolution
//==============================================================================

  procedure T_Pipeline_FilterConvolution.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'Type',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      True,
      '',
      '!LowPass3x3|LowPass5x5|LowPass7x7|'+
      'HighPass3x3|HighPass5x5|HighPass7x7|' +
      'Gaussian3x3|Gaussian5x5|Gaussian7x7|' +
      'Laplacian3x3|Laplacian5x5|' +
      'GradientNorth|GradientEast|GradientSouth|GradientWest|' +
      'GradientNorthwest|GradientNortheast|' +
      'GradientSouthwest|GradientSoutheast|' +
      'PointDetector|' +
      'LineDetectorHorizontal|LineDetectorVertical|' +
      'LineDetectorLeftDiagonal|LineDetectorRightDiagonal|' +
      'Custom'
    );
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterConvolution.Execute ;
  var
    fltr   : TGIS_PixelFilterConvolution ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    itype  : TGIS_PixelFilterMaskType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    stype := ParamAsString( 'Type' ) ;
    itype := TGIS_PixelFilterMaskType.LowPass3x3 ;
    if CompareText( stype, 'LowPass3x3' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LowPass3x3
    else
    if CompareText( stype, 'LowPass5x5' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LowPass5x5
    else
    if CompareText( stype, 'LowPass7x7' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LowPass7x7
    else
    if CompareText( stype, 'HighPass3x3' ) = 0 then
      itype := TGIS_PixelFilterMaskType.HighPass3x3
    else
    if CompareText( stype, 'HighPass5x5' ) = 0 then
      itype := TGIS_PixelFilterMaskType.HighPass5x5
    else
    if CompareText( stype, 'HighPass7x7' ) = 0 then
      itype := TGIS_PixelFilterMaskType.HighPass7x7
    else
    if CompareText( stype, 'Gaussian3x3' ) = 0 then
      itype := TGIS_PixelFilterMaskType.Gaussian3x3
    else
    if CompareText( stype, 'Gaussian5x5' ) = 0 then
      itype := TGIS_PixelFilterMaskType.Gaussian5x5
    else
    if CompareText( stype, 'Gaussian7x7' ) = 0 then
      itype := TGIS_PixelFilterMaskType.Gaussian7x7
    else
    if CompareText( stype, 'Laplacian3x3' ) = 0 then
      itype := TGIS_PixelFilterMaskType.Laplacian3x3
    else
    if CompareText( stype, 'Laplacian5x5' ) = 0 then
      itype := TGIS_PixelFilterMaskType.Laplacian5x5
    else
    if CompareText( stype, 'GradientNorth' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientNorth
    else
    if CompareText( stype, 'GradientEast' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientEast
    else
    if CompareText( stype, 'GradientSouth' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientSouth
    else
    if CompareText( stype, 'GradientWest' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientWest
    else
    if CompareText( stype, 'GradientNorthwest' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientNorthwest
    else
    if CompareText( stype, 'GradientNortheast' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientNortheast
    else
    if CompareText( stype, 'GradientSouthwest' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientSouthwest
    else
    if CompareText( stype, 'GradientSoutheast' ) = 0 then
      itype := TGIS_PixelFilterMaskType.GradientSoutheast
    else
    if CompareText( stype, 'PointDetector' ) = 0 then
      itype := TGIS_PixelFilterMaskType.PointDetector
    else
    if CompareText( stype, 'LineDetectorHorizontal' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LineDetectorHorizontal
    else
    if CompareText( stype, 'LineDetectorVertical' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LineDetectorVertical
    else
    if CompareText( stype, 'LineDetectorLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LineDetectorLeftDiagonal
    else
    if CompareText( stype, 'LineDetectorRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterMaskType.LineDetectorRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterMaskType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterConvolution.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.MaskType := itype ;
      if itype = TGIS_PixelFilterMaskType.Custom then
        fltr.CustomMask := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterSobelMagnitude
//==============================================================================

  procedure T_Pipeline_FilterSobelMagnitude.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
  end ;


  procedure T_Pipeline_FilterSobelMagnitude.Execute ;
  var
    fltr   : TGIS_PixelFilterSobelMagnitude ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;

    fltr := TGIS_PixelFilterSobelMagnitude.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterRange
//==============================================================================

  procedure T_Pipeline_FilterRange.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterRange.Execute ;
  var
    fltr   : TGIS_PixelFilterRange ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterRange.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMidpoint
//==============================================================================

  procedure T_Pipeline_FilterMidpoint.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMidpoint.Execute ;
  var
    fltr   : TGIS_PixelFilterMidpoint ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMidpoint.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMinimum
//==============================================================================

  procedure T_Pipeline_FilterMinimum.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMinimum.Execute ;
  var
    fltr   : TGIS_PixelFilterMinimum ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMinimum.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMaximum
//==============================================================================

  procedure T_Pipeline_FilterMaximum.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMaximum.Execute ;
  var
    fltr   : TGIS_PixelFilterMaximum ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMaximum.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterArithmeticMean
//==============================================================================

  procedure T_Pipeline_FilterArithmeticMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterArithmeticMean.Execute ;
  var
    fltr   : TGIS_PixelFilterArithmeticMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterArithmeticMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterAlphaTrimmedMean
//==============================================================================

  procedure T_Pipeline_FilterAlphaTrimmedMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Alpha',
      TGIS_PipelineParameterType.Int,
      0,
      NaN,
      False,
      '1',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterAlphaTrimmedMean.Execute ;
  var
    fltr   : TGIS_PixelFilterAlphaTrimmedMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    alpha  : Integer ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    alpha := ParamAsInt( 'Alpha', 1 ) ;

    fltr := TGIS_PixelFilterAlphaTrimmedMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.Alpha := alpha ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterContraHarmonicMean
//==============================================================================

  procedure T_Pipeline_FilterContraHarmonicMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Order',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '2',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterContraHarmonicMean.Execute ;
  var
    fltr   : TGIS_PixelFilterContraHarmonicMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    order  : Integer ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    order := ParamAsInt( 'Order', 2 ) ;

    fltr := TGIS_PixelFilterContraHarmonicMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.Order := order ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterGeometricMean
//==============================================================================

  procedure T_Pipeline_FilterGeometricMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterGeometricMean.Execute ;
  var
    fltr   : TGIS_PixelFilterGeometricMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterGeometricMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterHarmonicMean
//==============================================================================

  procedure T_Pipeline_FilterHarmonicMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterHarmonicMean.Execute ;
  var
    fltr   : TGIS_PixelFilterHarmonicMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterHarmonicMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterWeightedMean
//==============================================================================

  procedure T_Pipeline_FilterWeightedMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'WeightMask',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      GIS_PIXEL_FILTER_MASK_WEIGHT,
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterWeightedMean.Execute ;
  var
    fltr   : TGIS_PixelFilterWeightedMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    wmask  : String ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    wmask := ParamAsString( 'WeightMask' ) ;

    fltr := TGIS_PixelFilterWeightedMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.WeightMask := wmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterYpMean
//==============================================================================

  procedure T_Pipeline_FilterYpMean.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Order',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '-2',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterYpMean.Execute ;
  var
    fltr   : TGIS_PixelFilterYpMean ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    order  : Integer ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    order := ParamAsInt( 'Order', -2 ) ;

    fltr := TGIS_PixelFilterYpMean.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.Order := order ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMajority
//==============================================================================

  procedure T_Pipeline_FilterMajority.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMajority.Execute ;
  var
    fltr   : TGIS_PixelFilterMajority ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMajority.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMinority
//==============================================================================

  procedure T_Pipeline_FilterMinority.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMinority.Execute ;
  var
    fltr   : TGIS_PixelFilterMinority ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMinority.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterMedian
//==============================================================================

  procedure T_Pipeline_FilterMedian.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterMedian.Execute ;
  var
    fltr   : TGIS_PixelFilterMedian ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterMedian.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterWeightedMedian
//==============================================================================

  procedure T_Pipeline_FilterWeightedMedian.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'WeightMask',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      GIS_PIXEL_FILTER_MASK_WEIGHT,
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterWeightedMedian.Execute ;
  var
    fltr   : TGIS_PixelFilterWeightedMedian ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    wmask  : String ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    wmask := ParamAsString( 'WeightMask' ) ;

    fltr := TGIS_PixelFilterWeightedMedian.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.WeightMask := wmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterSum
//==============================================================================

  procedure T_Pipeline_FilterSum.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterSum.Execute ;
  var
    fltr   : TGIS_PixelFilterSum ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterSum.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterStandardDeviation
//==============================================================================

  procedure T_Pipeline_FilterStandardDeviation.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;

  procedure T_Pipeline_FilterStandardDeviation.Execute ;
  var
    fltr   : TGIS_PixelFilterStandardDeviation ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterStandardDeviation.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterUniqueCount
//==============================================================================

  procedure T_Pipeline_FilterUniqueCount.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterUniqueCount.Execute ;
  var
    fltr   : TGIS_PixelFilterUniqueCount ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;

    fltr := TGIS_PixelFilterUniqueCount.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterErosion
//==============================================================================

  procedure T_Pipeline_FilterErosion.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterErosion.Execute ;
  var
    fltr   : TGIS_PixelFilterErosion ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterErosion.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterDilation
//==============================================================================

  procedure T_Pipeline_FilterDilation.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterDilation.Execute ;
  var
    fltr   : TGIS_PixelFilterDilation ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterDilation.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterOpening
//==============================================================================

  procedure T_Pipeline_FilterOpening.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterOpening.Execute ;
  var
    fltr   : TGIS_PixelFilterOpening ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterOpening.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterClosing
//==============================================================================

  procedure T_Pipeline_FilterClosing.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterClosing.Execute ;
  var
    fltr   : TGIS_PixelFilterClosing ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterClosing.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterTopHat
//==============================================================================

  procedure T_Pipeline_FilterTopHat.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterTopHat.Execute ;
  var
    fltr   : TGIS_PixelFilterTopHat ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterTopHat.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// T_Pipeline_FilterBottomHat
//==============================================================================

  procedure T_Pipeline_FilterBottomHat.Initialize ;
  begin
    inherited ;

    defineParam(
      'Band',
      TGIS_PipelineParameterType.Int,
      1,
      NaN,
      False,
      '1',
      ''
    ) ;
    defineParam(
      'ColorSpace',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'HSL',
      '!RGB|HSL'
    ) ;
    defineParam(
      'BlockSize',
      TGIS_PipelineParameterType.Int,
      3,
      NaN,
      False,
      '3',
      ''
    ) ;
    defineParam(
      'Structure',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Disk',
      '!Square|Diamond|Disk|' +
      'LineHorizontal|LineVertical|' +
      'LineLeftDiagonal|LineRightDiagonal|' +
      'Custom'
    ) ;
    defineParam(
      'Custom',
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '0,0,0,0,1,0,0,0,0',
      ''
    ) ;
  end ;


  procedure T_Pipeline_FilterBottomHat.Execute ;
  var
    fltr   : TGIS_PixelFilterBottomHat ;
    src    : TGIS_LayerPixel ;
    dst    : TGIS_LayerPixel ;
    band   : Integer ;
    sspace : String ;
    ispace : TGIS_PixelFilterColorSpace ;
    siz    : Integer ;
    itype  : TGIS_PixelFilterStructuringElementType ;
    stype  : String ;
    cmask  : String ;
    res    : String ;
    o      : TObject ;
  begin
    src := nil ;
    dst := nil ;
    res := ParamAsString( 'Source' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      src := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    res := ParamAsString( 'Destination' ) ;
    o := Parent.GetVar( res ) ;
    if assigned( o ) then
      dst := TGIS_LayerPixel( o )
    else
      LogError( Format( 'Layer "%s" does not exists or is wrong type',
                [ res ] ) ) ;

    band := ParamAsInt( 'Band', 1 ) ;
    sspace := ParamAsString( 'ColorSpace' ) ;
    if CompareText( sspace, 'RGB' ) = 0 then
      ispace := TGIS_PixelFilterColorSpace.RGB
    else
      ispace := TGIS_PixelFilterColorSpace.HSL ;
    siz := ParamAsInt( 'BlockSize', 3 ) ;
    stype := ParamAsString( 'Structure' ) ;
    itype := TGIS_PixelFilterStructuringElementType.Disk ;
    if CompareText( stype, 'Square' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Square
    else
    if CompareText( stype, 'Diamond' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Diamond
    else
    if CompareText( stype, 'Disk' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.Disk
    else
    if CompareText( stype, 'LineHorizontal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineHorizontal
    else
    if CompareText( stype, 'LineVertical' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineVertical
    else
    if CompareText( stype, 'LineLeftDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineLeftDiagonal
    else
    if CompareText( stype, 'LineRightDiagonal' ) = 0 then
      itype := TGIS_PixelFilterStructuringElementType.LineRightDiagonal
    else
    if CompareText( stype, 'Custom' ) = 0 then begin
      itype := TGIS_PixelFilterStructuringElementType.Custom ;
      cmask := ParamAsString( 'Custom' ) ;
    end ;

    fltr := TGIS_PixelFilterBottomHat.Create ;
    try
      fltr.SourceLayer := src ;
      fltr.DestinationLayer := dst ;
      fltr.Band := band ;
      fltr.ColorSpace := ispace ;
      fltr.BlockSize := siz ;
      fltr.StructuringElementType := itype ;
      if itype = TGIS_PixelFilterStructuringElementType.Custom then
        fltr.CustomStructuringElement := cmask ;
      {$IFDEF OXYGENE}
        fltr.BusyEvent := @DoBusyEvent ;
      {$ELSE}
        fltr.BusyEvent := DoBusyEvent ;
      {$ENDIF}
      fltr.Execute ;
    finally
      FreeObject( fltr ) ;
    end ;

    inherited ;
  end ;


//==============================================================================
// TGIS_PixelFilterAbstract
//==============================================================================

  constructor TGIS_PixelFilterAbstract.Create ;
  begin
    inherited ;

    bPrepare := False ;
    bCompound := False ;
    bProcPixel := False ;

    FSourceLayer := nil ;
    FDestinationLayer := nil ;
    FBand := 1 ;
    FColorSpace := TGIS_PixelFilterColorSpace.HSL ;
  end ;


  procedure TGIS_PixelFilterAbstract.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterAbstract.fget_Band
    : Integer ;
  begin
    Result := FBand ;
  end ;


  procedure TGIS_PixelFilterAbstract.fset_Band(
    const _val : Integer
  ) ;
  begin
    if _val < 0 then
      exit ;

    FBand := _val ;
  end ;


  procedure TGIS_PixelFilterAbstract.prepareMask(
    const _str  : String ;
      var _mask : TGIS_SingleArray
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    cnt : Integer ;
    val : Integer ;
    dbl : Double ;
    i   : Integer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _str, [','] ) ;
      cnt := tkn.Result.Count ;

      if ( cnt < 9 ) or ( cnt mod 2 = 0 ) then
        exit ;

      dbl := Sqrt( 1.0*cnt ) ;
      if dbl - 1.0*FloorS( dbl ) <> 0 then
        exit ;

      for i := 0 to tkn.Result.Count - 1 do begin
        if not TryStrToInt( tkn.Result[i], val ) then
          exit ;
      end ;

      SetLength( _mask, cnt ) ;

      for i := 0 to cnt - 1 do
        _mask[i] := 1.0*StrToInt( tkn.Result[i] ) ;

      iBlockSize := FloorS( dbl ) ;

    finally
      FreeObject( tkn ) ;
    end ;
  end ;


  function TGIS_PixelFilterAbstract.maskToString(
    var _mask : TGIS_SingleArray
  ) : String ;
  var
    sb  : TStringBuilder ;
    res : String ;
    i   : Integer ;
  begin
    sb := TStringBuilder.Create ;
    try
      for i := 0 to length( _mask ) - 1 do begin
        if i > 0 then
          sb.Append( ',' ) ;
        sb.Append( IntToStr( RoundS( _mask[i] ) ) ) ;
      end ;
      res := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


  function TGIS_PixelFilterAbstract.getValue(
    const _h : Integer ;
    const _w : Integer
  ) : Single ;
  var
    clr  : TGIS_Color ;
    calp : Double ;
    chue : Double ;
    csat : Double ;
    clig : Double ;
    res  : Single ;
  begin
    if ( _h >= oBounds.Top  ) and ( _h <= oBounds.Bottom ) and
       ( _w >= oBounds.Left ) and ( _w <= oBounds.Right  ) then begin
      if FSourceLayer.IsGrid then begin
        res := srcLock.Grid[_h,_w] ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          res := srcLock.Band[_h,_w] ;
        end
        else begin
          clr := TGIS_Color.FromARGB(
            Cardinal( srcLock.Bitmap[srcLock.BitmapPos( _w, _h )] )
          ) ;
          clr.ToAHSL( calp, chue, csat, clig ) ;
          res := clig ;
        end ;
      end;
    end
    else
      res := srcNoData ;

    Result := res ;
  end ;


  function TGIS_PixelFilterAbstract.raiseBusyEvent(
    const _sender : TObject ;
    const _pos    : Int64 ;
    const _end    : Int64
  ) : Boolean ;
  var
    abrt   : Boolean ;
    {$IFDEF OXYGENE}
      args : TGIS_BusyEventArgs ;
    {$ENDIF}
  begin
    Result := False ;

    if assigned( FOnBusy ) then begin
      abrt := False ;
      {$IFDEF OXYGENE}
        args := TGIS_BusyEventArgs.Create( _pos, _end, abrt ) ;
        FOnBusy( _sender, args ) ;
        Result := args.Abort ;
      {$ELSE}
        FOnBusy( _sender, _pos, _end, abrt ) ;
        Result := abrt ;
      {$ENDIF}
    end ;
  end ;


  procedure TGIS_PixelFilterAbstract.Execute ;
  const
    LOCAL_PARAM_SOURCE      : String = 'SourceLayer' ;
    LOCAL_PARAM_DESTINATION : String = 'DestinationLayer' ;
    LOCAL_PARAM_BAND        : String = 'Band' ;
  var
    src     : TGIS_LayerPixel ;
    dst     : TGIS_LayerPixel ;
    src_lck : TGIS_LayerPixelLock ;
    dst_lck : TGIS_LayerPixelLock ;
    ahsl    : TGIS_GridArray ;
    sband   : Integer ;
    eband   : Integer ;
    bgrd    : Boolean ;
    bhsl    : Boolean ;
    bdst    : Boolean ;
    mrgn    : Integer ;
    val     : Single ;
    vmin    : Single ;
    vmax    : Single ;
    w       : Integer ;
    h       : Integer ;
    ww      : Integer ;
    hh      : Integer ;
    i       : Integer ;
    k       : Integer ;

    // color extraction RGB->HSL
    clr     : TGIS_Color ;
    calp    : Double ;
    chue    : Double ;
    csat    : Double ;
    clig    : Double ;

    // busy event
    total   : Integer ;
    prgs    : Integer ;
    abrt    : Boolean ;

    procedure event_start ;
    begin
      abrt := raiseBusyEvent( Self, 0, total ) ;
    end ;

    procedure event_progress ;
    begin
      abrt := raiseBusyEvent( Self, prgs, total ) ;
    end ;

    procedure event_end ;
    begin
      abrt := raiseBusyEvent( Self, -1, -1 ) ;
    end ;

    procedure prepare_hsl ;
    var
      www : Integer ;
      hhh : Integer ;
      ii  : Integer ;
      kk  : Integer ;
    begin
      if h = oBounds.Top then begin
        SetLength( ahsl, iBlockSize, 4*src.BitWidth ) ;
        for ii := 0 to mrgn do begin
          hhh := mrgn + ii ;
          for kk := oBounds.Left to oBounds.Right do begin
            clr := TGIS_Color.FromARGB(
              Cardinal( src_lck.Bitmap[src_lck.BitmapPos( kk, h+ii )] )
            ) ;
            clr.ToAHSL( calp, chue, csat, clig ) ;
            www := 4*( kk - oBounds.Left ) ;
            ahsl[hhh,www  ] := calp ;
            ahsl[hhh,www+1] := chue ;
            ahsl[hhh,www+2] := csat ;
            ahsl[hhh,www+3] := clig ;
          end ;
        end ;
      end
      else begin
        for ii := 0 to iBlockSize - 2 do
          ahsl[ii] := TGIS_SingleArray( Copy( ahsl[ii+1], 0, 4*src.BitWidth ) ) ;

        if h >= oBounds.Bottom - mrgn then
          exit ;

        hhh := iBlockSize - 1 ;
        for kk := oBounds.Left to oBounds.Right do begin
          clr := TGIS_Color.FromARGB(
            Cardinal( src_lck.Bitmap[src_lck.BitmapPos( kk, h+mrgn )] )
          ) ;
          clr.ToAHSL( calp, chue, csat, clig ) ;
          www := 4*( kk - oBounds.Left ) ;
          ahsl[hhh,www  ] := calp ;
          ahsl[hhh,www+1] := chue ;
          ahsl[hhh,www+2] := csat ;
          ahsl[hhh,www+3] := clig ;
        end ;
      end ;
    end ;

    function get_value : Single ;
    var
      rr : Single ;
    begin
      if ( hh >= oBounds.Top  ) and ( hh <= oBounds.Bottom ) and
         ( ww >= oBounds.Left ) and ( ww <= oBounds.Right  ) then begin
        if bgrd then begin
          rr := src_lck.Grid[hh,ww] ;
        end
        else begin
          if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
            rr := src_lck.Band[hh,ww] ;
          end
          else begin
            if bProcPixel then begin
              clr := TGIS_Color.FromARGB(
                Cardinal( src_lck.Bitmap[src_lck.BitmapPos( ww, hh )] )
              ) ;
              clr.ToAHSL( calp, chue, csat, clig ) ;
              rr := clig ;
            end
            else
             rr := ahsl[mrgn + hh - h,4*( ww - oBounds.Left ) + 3] ;
          end ;
        end ;
      end
      else
        rr := srcNoData ;

      if rr <> srcNoData then
        inc( iValueCnt ) ;

      Result := rr ;
    end ;

    procedure set_value ;
    var
      ii : Integer ;
    begin
      if bgrd then begin
        dst_lck.Grid[h,w] := val ;
      end
      else begin
        if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
          dst_lck.Band[h,w] := val ;
        end
        else begin
          if not bProcPixel then begin
            ii := 4*( w - oBounds.Left ) ;
            calp := ahsl[mrgn,ii    ] ;
            chue := ahsl[mrgn,ii + 1] ;
            csat := ahsl[mrgn,ii + 2] ;
          end ;

          clig := val ;
          if clig < 0.0 then
            clig := 0.0
          else
          if clig > 1.0 then
            clig := 1.0 ;
          clr := TGIS_Color.FromAHSL( calp, chue, csat, clig ) ;
          dst_lck.Bitmap[dst_lck.BitmapPos( w, h )] := Integer( clr.ToARGB ) ;
        end ;
      end ;
    end ;

    procedure set_min_max ;
    begin
      if not bgrd then
        exit ;

      dst.MinHeight := vmin ;
      dst.MaxHeight := vmax ;
    end ;

  begin
    if not assigned( FSourceLayer ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SOURCE, 1
      ) ;

    if FBand > FSourceLayer.BandsCount then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_BAND, 1
      ) ;

    if bCompound then begin
      procCompound ;
      exit ;
    end ;

    src := FSourceLayer ;
    bgrd := src.IsGrid and ( FBand = 1 ) ;
    bhsl := ( FColorSpace = TGIS_PixelFilterColorSpace.HSL ) and
            ( not bgrd ) and ( not bProcPixel ) ;
    bdst := assigned( FDestinationLayer ) and
            ( FSourceLayer.Name <> FDestinationLayer.Name ) ;

    if bdst and ( FSourceLayer.IsGrid <> FDestinationLayer.IsGrid ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DESTINATION, 1
      ) ;

    if bdst then begin
      dst := FDestinationLayer
    end
    else begin
      dst := TGIS_LayerPixel.Create ;
      dst.Build( src.IsGrid, src.CS, src.Extent, src.BitWidth, src.BitHeight ) ;
    end ;

    src.Open ;
    dst.Open ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;
    if bgrd then begin
      srcNoData := src.NoDataValue ;
      srcMinVal := src.MinHeight ;
      srcMaxVal := src.MaxHeight ;
      dstNoData := dst.NoDataValue ;
    end
    else begin
      srcNoData := GIS_GRID_NOVALUE ;
      if FColorSpace = TGIS_PixelFilterColorSpace.RGB then begin
        srcMinVal := 0.0 ;
        srcMaxVal := 255.0 ;
      end
      else begin
        srcMinVal := 0.0 ;
        srcMaxVal := 1.0 ;
      end ;
      dstNoData := GIS_GRID_NOVALUE ;
    end ;
    SetLength( aBlock, iBlockSize*iBlockSize ) ;

    if bgrd then begin
      sband := 0 ;
      eband := 0 ;
    end
    else
    if FBand = 0 then begin
      sband := 1 ;
      eband := src.BandsCount ;
    end
    else begin
      sband := FBand ;
      eband := FBand ;
    end ;

    for i := sband to eband do begin

      if bgrd or ( FColorSpace = TGIS_PixelFilterColorSpace.HSL ) then begin
        src_lck := src.LockPixels( src.Extent, src.CS, False ) ;
        dst_lck := dst.LockPixels( src.Extent, src.CS, True ) ;
      end
      else begin
        src_lck := src.LockPixels( src.Extent, src.CS, i, False ) ;
        dst_lck := dst.LockPixels( src.Extent, src.CS, i, True ) ;
      end ;
      try
        srcLock := src_lck ;

        mrgn := iBlockSize div 2 ;
        oBounds := Rect(
          src_lck.Bounds.Left, src_lck.Bounds.Top,
          src_lck.Bounds.Right, src_lck.Bounds.Bottom
        ) ;

        prepFilter ;

        total := ( eband - sband + 1 ) *
                 ( oBounds.Height + 1 ) *
                 ( oBounds.Width + 1 ) ;
        event_start ;
        prgs := 0 ;
        for h := oBounds.Top to oBounds.Bottom do begin
            event_progress ;
            if abrt then begin
              set_min_max ;
              exit ;
            end ;

          if bhsl then
            prepare_hsl ;
          for w := oBounds.Left to oBounds.Right do begin

            hh := h ;
            ww := w ;

            prgs := ( i-sband )*( oBounds.Height + 1 )*( oBounds.Width + 1 ) +
                    ( h - oBounds.Top )*( oBounds.Width + 1 ) +
                    ( w - oBounds.Left ) + 1 ;

            val := get_value ;
            if val = srcNoData then begin
              val := dstNoData ;
              set_value ;
              continue ;
            end ;

            srcValue := val ;
            if bProcPixel then begin
              val := procPixel ;
            end
            else begin
              iValueCnt := 0 ;
              k := 0 ;
              for hh := h - mrgn to h + mrgn do begin
                for ww := w - mrgn to w + mrgn do begin
                  aBlock[k] := get_value ;
                  inc( k ) ;
                end ;
              end ;

              if iValueCnt = 0 then begin
                val := dstNoData ;
                set_value ;
                continue ;
              end ;

              val := procBlock ;
            end ;

            set_value ;
            if bgrd and ( val <> dstNoData ) then begin
              vmin := Min( val, vmin ) ;
              vmax := Max( val, vmax ) ;
            end ;

          end ;
        end ;
        event_end ;
        set_min_max ;

      finally
        src.UnlockPixels( src_lck ) ;
        dst.UnlockPixels( dst_lck ) ;
      end ;
    end ;

    if not bdst then begin
      src.ImportLayer( dst, src.Extent ) ;
      FreeObject( dst ) ;
    end ;
  end ;


//==============================================================================
// TGIS_PixelFilterThreshold
//==============================================================================

  constructor TGIS_PixelFilterThreshold.Create ;
  begin
    inherited ;

    bProcPixel := True ;

    FThreshold := 0.0 ;
  end ;


  procedure TGIS_PixelFilterThreshold.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterThreshold.procCompound ;
  begin
    // do nothing
  end ;


  procedure TGIS_PixelFilterThreshold.prepFilter ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterThreshold.procPixel
    : Single ;
  var
    res : Single ;
  begin
    if srcValue <= FThreshold then
      res := srcMinVal
    else
      res := srcMaxVal ;

    Result := res ;
  end ;


  function TGIS_PixelFilterThreshold.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterNoise
//==============================================================================

  constructor TGIS_PixelFilterNoise.Create ;
  begin
    inherited ;

    bProcPixel := True ;

    FAmount := 1.0 ;
  end ;


  procedure TGIS_PixelFilterNoise.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterNoise.fget_Amount
    : Single ;
  begin
    Result := FAmount ;
  end ;


  procedure TGIS_PixelFilterNoise.fset_Amount(
    const _val : Single
  ) ;
  begin
    if ( _val < 0.0 ) and ( _val > 100.0 ) then
      exit ;

    FAmount := _val ;
  end ;


  procedure TGIS_PixelFilterNoise.procCompound ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterNoise.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterNoiseSaltPepper
//==============================================================================

  constructor TGIS_PixelFilterNoiseSaltPepper.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterNoiseSaltPepper.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterNoiseSaltPepper.prepFilter ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterNoiseSaltPepper.procPixel
    : Single ;
  var
    res : Single ;
  begin
    res := RandomGenerator.Uniform( 10000 )/100 ;
    if res < Amount then begin
      if RandomGenerator.Uniform( 100 ) < 50 then
        res := srcMinVal
      else
        res := srcMaxVal ;
    end
    else
      res := srcValue ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterNoiseGaussian
//==============================================================================

  constructor TGIS_PixelFilterNoiseGaussian.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterNoiseGaussian.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterNoiseGaussian.prepFilter ;
  const
    LOCAL_NUMBER_OF_SAMPLES : Integer = 100 ;
  var
    arr  : TGIS_SingleArray ;
    wdth : Integer ;
    hght : Integer ;
    val  : Single ;
    w    : Integer ;
    h    : Integer ;
    i    : Integer ;
  begin
    SetLength( arr, LOCAL_NUMBER_OF_SAMPLES ) ;

    wdth := oBounds.Right - oBounds.Left ;
    hght := oBounds.Bottom - oBounds.Top ;

    val := 0.0 ;
    for i := 0 to LOCAL_NUMBER_OF_SAMPLES - 1 do begin
      w := RandomGenerator.Uniform( wdth ) ;
      h := RandomGenerator.Uniform( hght ) ;
      arr[i] := getValue( oBounds.Top + h, oBounds.Left + w ) ;
    end ;

    for i := 0 to LOCAL_NUMBER_OF_SAMPLES - 1 do
      val := val + arr[i] ;

    sMean := val/LOCAL_NUMBER_OF_SAMPLES ;

    val := 0.0 ;
    for i := 0 to LOCAL_NUMBER_OF_SAMPLES - 1 do
      val := val + ( arr[i] - sMean )*( arr[i] - sMean ) ;

    val := val/( LOCAL_NUMBER_OF_SAMPLES - 1 ) ;

    sStdDev := Sqrt( val ) ;
  end ;


  function TGIS_PixelFilterNoiseGaussian.procPixel
    : Single ;
  var
    res : Single ;
  begin
    res := 1.0*RandomGenerator.Uniform( 10000 )/100.0 ;
    if res < Amount then
      res := RandomGenerator.Gaussian( sMean, sStdDev/3.0 )
    else
      res := srcValue ;

    if res < srcMinVal then
      res := srcMinVal
    else
    if res > srcMaxVal then
      res := srcMaxVal ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterConvolution
//==============================================================================

  constructor TGIS_PixelFilterConvolution.Create ;
  begin
    inherited ;

    prepareMask( GIS_PIXEL_FILTER_MASK_NEUTRAL_3, aCustomMask ) ;
    prepareMask( GIS_PIXEL_FILTER_MASK_LOWPASS_3, aMask ) ;
    FMaskType := TGIS_PixelFilterMaskType.LowPass3x3 ;
  end ;


  procedure TGIS_PixelFilterConvolution.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterConvolution.fget_MaskType
    : TGIS_PixelFilterMaskType ;
  begin
    Result := FMaskType ;
  end ;


  procedure TGIS_PixelFilterConvolution.fset_MaskType(
    const _val : TGIS_PixelFilterMaskType
  ) ;
  begin
    case _val of
      TGIS_PixelFilterMaskType.LowPass3x3  :
        prepareMask( GIS_PIXEL_FILTER_MASK_LOWPASS_3, aMask ) ;
      TGIS_PixelFilterMaskType.LowPass5x5  :
        prepareMask( GIS_PIXEL_FILTER_MASK_LOWPASS_5, aMask ) ;
      TGIS_PixelFilterMaskType.LowPass7x7  :
        prepareMask( GIS_PIXEL_FILTER_MASK_LOWPASS_7, aMask ) ;
      TGIS_PixelFilterMaskType.HighPass3x3 :
        prepareMask( GIS_PIXEL_FILTER_MASK_HIGHPASS_3, aMask ) ;
      TGIS_PixelFilterMaskType.HighPass5x5 :
        prepareMask( GIS_PIXEL_FILTER_MASK_HIGHPASS_5, aMask ) ;
      TGIS_PixelFilterMaskType.HighPass7x7 :
        prepareMask( GIS_PIXEL_FILTER_MASK_HIGHPASS_7, aMask ) ;
      TGIS_PixelFilterMaskType.Gaussian3x3 :
        prepareMask( GIS_PIXEL_FILTER_MASK_GAUSSIAN_3, aMask ) ;
      TGIS_PixelFilterMaskType.Gaussian5x5 :
        prepareMask( GIS_PIXEL_FILTER_MASK_GAUSSIAN_5, aMask ) ;
      TGIS_PixelFilterMaskType.Gaussian7x7 :
        prepareMask( GIS_PIXEL_FILTER_MASK_GAUSSIAN_7, aMask ) ;
      TGIS_PixelFilterMaskType.Laplacian3x3 :
        prepareMask( GIS_PIXEL_FILTER_MASK_LAPLACIAN_3, aMask ) ;
      TGIS_PixelFilterMaskType.Laplacian5x5 :
        prepareMask( GIS_PIXEL_FILTER_MASK_LAPLACIAN_5, aMask ) ;
      TGIS_PixelFilterMaskType.GradientNorth :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_NORTH, aMask ) ;
      TGIS_PixelFilterMaskType.GradientEast :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_EAST, aMask ) ;
      TGIS_PixelFilterMaskType.GradientSouth :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTH, aMask ) ;
      TGIS_PixelFilterMaskType.GradientWest :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_WEST, aMask ) ;
      TGIS_PixelFilterMaskType.GradientNorthwest :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_NORTHWEST, aMask ) ;
      TGIS_PixelFilterMaskType.GradientNortheast :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_NORTHEAST, aMask ) ;
      TGIS_PixelFilterMaskType.GradientSouthwest :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTHWEST, aMask ) ;
      TGIS_PixelFilterMaskType.GradientSoutheast :
        prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_SOUTHEAST, aMask ) ;
      TGIS_PixelFilterMaskType.PointDetector :
        prepareMask( GIS_PIXEL_FILTER_MASK_POINT_DETECTOR, aMask ) ;
      TGIS_PixelFilterMaskType.LineDetectorHorizontal :
        prepareMask( GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_HORIZONTAL, aMask ) ;
      TGIS_PixelFilterMaskType.LineDetectorVertical :
        prepareMask( GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_VERTICAL, aMask ) ;
      TGIS_PixelFilterMaskType.LineDetectorLeftDiagonal :
        prepareMask( GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_LEFT_DIAGONAL, aMask ) ;
      TGIS_PixelFilterMaskType.LineDetectorRightDiagonal :
        prepareMask( GIS_PIXEL_FILTER_MASK_LINE_DETECTOR_RIGHT_DIAGONAL, aMask ) ;
      TGIS_PixelFilterMaskType.Custom :
        iBlockSize := FloorS( Sqrt( 1.0*length( aCustomMask ) ) ) ;
    end ;

    FMaskType := _val ;
  end ;


  function TGIS_PixelFilterConvolution.fget_CustomMask
    : String ;
  begin
    Result := maskToString( aCustomMask ) ;
  end ;


  procedure TGIS_PixelFilterConvolution.fset_CustomMask(
    const _val : String
  ) ;
  begin
    prepareMask( _val, aCustomMask ) ;
  end ;


  procedure TGIS_PixelFilterConvolution.procCompound ;
  begin
    // do nothing
  end ;


  procedure TGIS_PixelFilterConvolution.prepFilter ;
  var
    mask : TGIS_SingleArray ;
    mus  : Single ;
    i    : Integer ;
  begin
    if MaskType = TGIS_PixelFilterMaskType.Custom then
      mask := aCustomMask
    else
      mask := aMask ;

    mus := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do
      mus := mus + mask[i] ;

    bZeroSum := mus = 0.0 ;
  end ;


  function TGIS_PixelFilterConvolution.procPixel
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


  function TGIS_PixelFilterConvolution.procBlock
    : Single ;
  var
    mask : TGIS_SingleArray ;
    res  : Single ;
    val  : Single ;
    mus  : Single ;
    wgh  : Single ;
    i    : Integer ;
  begin
    if MaskType = TGIS_PixelFilterMaskType.Custom then
      mask := aCustomMask
    else
      mask := aMask ;

    mus := 0.0 ;
    wgh := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then begin
        if bZeroSum then
          val := srcValue
        else
          continue ;
      end ;

      mus := mus + mask[i]*val ;
      wgh := wgh + mask[i] ;
    end ;

    if wgh = 0.0 then
      res := mus
    else
      res := mus/wgh ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterBlockStatistics
//==============================================================================

  constructor TGIS_PixelFilterBlockStats.Create ;
  begin
    inherited ;

    iBlockSize := 3 ;
  end ;


  procedure TGIS_PixelFilterBlockStats.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterBlockStats.fget_BlockSize
    : Integer ;
  begin
    Result := iBlockSize ;
  end ;


  procedure TGIS_PixelFilterBlockStats.fset_BlockSize(
    const _val : Integer
  ) ;
  begin
    if ( _val < 3 ) or ( _val mod 2 = 0 ) then
      exit ;

    iBlockSize := _val ;
  end ;


  procedure TGIS_PixelFilterBlockStats.procCompound ;
  begin
    // do nothing
  end ;


  procedure TGIS_PixelFilterBlockStats.prepFilter ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterBlockStats.procPixel
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterSobelMagnitude
//==============================================================================

  constructor TGIS_PixelFilterSobelMagnitude.Create ;
  begin
    inherited ;

    prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_NORTH, hMask ) ;
    prepareMask( GIS_PIXEL_FILTER_MASK_GRADIENT_WEST, vMask ) ;

    iBlockSize := 3 ;
  end ;


  procedure TGIS_PixelFilterSobelMagnitude.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterSobelMagnitude.fget_BlockSize
    : Integer ;
  begin
    Result := iBlockSize ;
  end ;


  procedure TGIS_PixelFilterSobelMagnitude.fset_BlockSize(
    const _val : Integer
  ) ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterSobelMagnitude.procBlock
    : Single ;
  var
    val  : Single ;
    hmus : Single ;
    vmus : Single ;
    i    : Integer ;
  begin
    hmus := 0.0 ;
    vmus := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        val := srcValue ;

      hmus := hmus + hMask[i]*val ;
      vmus := vmus + vMask[i]*val ;
    end ;

    Result := Abs( hmus ) + Abs( vmus ) ;
  end ;


//==============================================================================
// TGIS_PixelFilterRange
//==============================================================================

  constructor TGIS_PixelFilterRange.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterRange.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterRange.procBlock
    : Single ;
  var
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    i    : Integer ;
  begin
    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      vmin := Min( val, vmin ) ;
      vmax := Max( val, vmax ) ;
    end ;

    Result := vmax - vmin ;
  end ;


//==============================================================================
// TGIS_PixelFilterMidpoint
//==============================================================================

  constructor TGIS_PixelFilterMidpoint.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterMidpoint.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMidpoint.procBlock
    : Single ;
  var
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    i    : Integer ;
  begin
    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      vmin := Min( val, vmin ) ;
      vmax := Max( val, vmax ) ;
    end ;

    Result := ( vmin + vmax )/2.0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterMinimum
//==============================================================================

  constructor TGIS_PixelFilterMinimum.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterMinimum.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMinimum.procBlock
    : Single ;
  var
    val  : Single ;
    vmin : Single ;
    i    : Integer ;
  begin
    vmin := GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      vmin := Min( val, vmin ) ;
    end ;

    Result := vmin ;
  end ;


//==============================================================================
// TGIS_PixelFilterMaximum
//==============================================================================

  constructor TGIS_PixelFilterMaximum.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterMaximum.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMaximum.procBlock
    : Single ;
  var
    val  : Single ;
    vmax : Single ;
    i    : Integer ;
  begin
    vmax := -GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      vmax := Max( val, vmax ) ;
    end ;

    Result := vmax ;
  end ;


//==============================================================================
// TGIS_PixelFilterArithmeticMean
//==============================================================================

  constructor TGIS_PixelFilterArithmeticMean.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterArithmeticMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterArithmeticMean.procBlock
    : Single ;
  var
    val : Single ;
    mus : Single ;
    i   : Integer ;
  begin
    mus := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      mus := mus + val ;
    end ;

    Result := mus/iValueCnt ;
  end ;


//==============================================================================
// TGIS_PixelFilterAlphaTrimmedMean
//==============================================================================

  constructor TGIS_PixelFilterAlphaTrimmedMean.Create ;
  begin
    inherited ;

    oValues := TList<Single>.Create ;

    FAlpha := 1 ;
  end ;


  procedure TGIS_PixelFilterAlphaTrimmedMean.doDestroy ;
  begin
    FreeObject( oValues ) ;

    inherited ;
  end ;


  function TGIS_PixelFilterAlphaTrimmedMean.fget_Alpha
    : Integer ;
  begin
    Result := FAlpha ;
  end ;


  procedure TGIS_PixelFilterAlphaTrimmedMean.fset_Alpha(
    const _val : Integer
  ) ;
  begin
    if _val >= 0 then
      FAlpha := _val ;
  end ;


  function TGIS_PixelFilterAlphaTrimmedMean.procBlock
    : Single ;
  var
    val : Single ;
    mus : Single ;
    cnt : Integer ;
    b   : Boolean ;
    i   : Integer ;
    k   : Integer ;
  begin
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      b := False ;
      for k := 0 to oValues.Count - 1 do begin
        if oValues[k] > val then begin
          oValues.Insert( k, val ) ;
          b := True ;
          break ;
        end ;
      end ;

      if not b then
        oValues.Add( val ) ;
    end ;

    mus := 0.0 ;
    cnt := 0 ;
    for i := FAlpha to oValues.Count - FAlpha - 1 do begin
      val := oValues[i] ;

      if val = srcNoData then
        continue ;

      mus := mus + val ;
      inc( cnt ) ;
    end ;

    if cnt = 0 then begin
      mus := srcValue ;
      cnt := 1 ;
    end ;

    oValues.Clear ;

    Result := mus/cnt ;
  end ;


//==============================================================================
// TGIS_PixelFilterContraHarmonicMean
//==============================================================================

  constructor TGIS_PixelFilterContraHarmonicMean.Create ;
  begin
    inherited ;

    FOrder := 2 ;
  end ;


  procedure TGIS_PixelFilterContraHarmonicMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterContraHarmonicMean.fget_Order
    : Integer ;
  begin
    Result := FOrder ;
  end ;


  procedure TGIS_PixelFilterContraHarmonicMean.fset_Order(
    const _val : Integer
  ) ;
  begin
    if _val > 1 then
      FOrder := _val ;
  end ;


  function TGIS_PixelFilterContraHarmonicMean.procBlock
    : Single ;
  var
    val : Single ;
    num : Single ;
    den : Single ;
    i   : Integer ;
  begin
    num := 0.0 ;
    den := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      num := num + Power( val, FOrder + 1 ) ;
      den := den + Power( val, FOrder ) ;
    end ;

    if den = 0.0 then begin
      num := srcValue ;
      den := 1.0 ;
    end ;

    Result := num/den ;
  end ;


//==============================================================================
// TGIS_PixelFilterGeometricMean
//==============================================================================

  constructor TGIS_PixelFilterGeometricMean.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterGeometricMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterGeometricMean.procBlock
    : Single ;
  var
    val : Single ;
    pro : Single ;
    cnt : Integer ;
    d   : Double ;
    i   : Integer ;
  begin
    cnt := 0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      inc( cnt ) ;
    end ;

    pro := 1.0 ;
    d := 1.0/cnt ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      pro := pro*Power( val, d ) ;
    end ;

    Result := pro ;
  end ;


//==============================================================================
// TGIS_PixelFilterHarmonicMean
//==============================================================================

  constructor TGIS_PixelFilterHarmonicMean.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterHarmonicMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterHarmonicMean.procBlock
    : Single ;
  var
    val : Single ;
    mus : Single ;
    cnt : Integer ;
    i   : Integer ;
  begin
    mus := 0.0 ;
    cnt := 0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if ( val = srcNoData ) or ( val = 0.0 ) then
        continue ;

      mus := mus + 1.0/val ;
      inc( cnt ) ;
    end ;

    Result := cnt/mus ;
  end ;


//==============================================================================
// TGIS_PixelFilterWeightedMean
//==============================================================================

  constructor TGIS_PixelFilterWeightedMean.Create ;
  begin
    inherited ;

    prepareMask( GIS_PIXEL_FILTER_MASK_WEIGHT, aWeightMask ) ;
  end ;


  procedure TGIS_PixelFilterWeightedMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterWeightedMean.fget_WeightMask
    : String ;
  begin
    Result := maskToString( aWeightMask ) ;
  end ;


  procedure TGIS_PixelFilterWeightedMean.fset_WeightMask(
    const _val : String
  ) ;
  begin
    prepareMask( _val, aWeightMask ) ;
  end ;


  function TGIS_PixelFilterWeightedMean.fget_BlockSize
    : Integer ;
  begin
    Result := iBlockSize ;
  end ;


  procedure TGIS_PixelFilterWeightedMean.fset_BlockSize(
    const _val : Integer
  ) ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterWeightedMean.procBlock
    : Single ;
  var
    val : Single ;
    num : Single ;
    den : Single ;
    i   : Integer ;
  begin
    num := 0.0 ;
    den := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      num := num + aWeightMask[i]*val ;
      den := den + aWeightMask[i] ;
    end ;

    if den = 0.0 then begin
      num := srcValue ;
      den := 1.0 ;
    end ;

    Result := num/den ;
  end ;


//==============================================================================
// TGIS_PixelFilterYpMean
//==============================================================================

  constructor TGIS_PixelFilterYpMean.Create ;
  begin
    inherited ;

    FOrder := -2 ;
  end ;


  procedure TGIS_PixelFilterYpMean.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterYpMean.fget_Order
    : Integer ;
  begin
    Result := FOrder ;
  end ;


  procedure TGIS_PixelFilterYpMean.fset_Order(
    const _val : Integer
  ) ;
  begin
    if _val <> 0 then
      FOrder := _val ;
  end ;


  function TGIS_PixelFilterYpMean.procBlock
    : Single ;
  var
    val : Single ;
    mus : Single ;
    i   : Integer ;
  begin
    mus := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      mus := mus + Power( val, FOrder ) ;
    end ;

    Result := Power( mus/iValueCnt, 1.0/FOrder ) ;
  end ;


//==============================================================================
// TGIS_PixelFilterMajority
//==============================================================================

  constructor TGIS_PixelFilterMajority.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterMajority.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMajority.procBlock
    : Single ;
  var
    res  : Single ;
    val  : Single ;
    tmp  : Single ;
    cnt  : Integer ;
    vmax : Single ;
    cmax : Integer ;
    imax : Integer ;
    i    : Integer ;
    k    : Integer ;
  begin
    vmax := 0.0 ;
    cmax := 0 ;

    imax := iBlockSize*iBlockSize - 1 ;
    for i := 0 to imax do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      cnt := 1 ;
      for k := 0 to imax do begin
        if k = i then
          continue ;

        tmp := aBlock[k] ;

        if tmp = srcNoData then
          continue ;

        if tmp = val then
          inc( cnt ) ;
      end ;

      if cnt > cmax then begin
        vmax := val ;
        cmax := cnt ;
      end ;
    end ;

    if ( cmax >= 2 ) and ( cmax >= FloorS( iValueCnt/2.0 ) ) then
      res := vmax
    else
      res := srcValue ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterMinority
//==============================================================================

  constructor TGIS_PixelFilterMinority.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterMinority.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMinority.procBlock
    : Single ;
  var
    res  : Single ;
    val  : Single ;
    tmp  : Single ;
    cnt  : Integer ;
    vmin : Single ;
    cmin : Integer ;
    imax : Integer ;
    uniq : Boolean ;
    i    : Integer ;
    k    : Integer ;
  begin
    vmin := 0.0 ;
    cmin := iValueCnt ;

    uniq := True ;
    imax := iBlockSize*iBlockSize - 1 ;
    for i := 0 to imax do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      cnt := 1 ;
      for k := 0 to imax do begin
        if k = i then
          continue ;

        tmp := aBlock[k] ;

        if tmp = srcNoData then
          continue ;

        if tmp = val then
          inc( cnt ) ;
      end ;

      if cnt = cmin then begin
        uniq := False ;
      end
      else
      if cnt < cmin then begin
        vmin := val ;
        cmin := cnt ;
        uniq := True ;
      end ;
    end ;

    if ( uniq = True ) and ( cmin <= FloorS( iValueCnt/2.0 ) ) then
      res := vmin
    else
      res := srcValue ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterMedian
//==============================================================================

  constructor TGIS_PixelFilterMedian.Create ;
  begin
    inherited ;

    oValues := TList<Single>.Create ;
  end ;


  procedure TGIS_PixelFilterMedian.doDestroy ;
  begin
    FreeObject( oValues ) ;

    inherited ;
  end ;


  function TGIS_PixelFilterMedian.procBlock
    : Single ;
  var
    val : Single ;
    res : Single ;
    b   : Boolean ;
    i   : Integer ;
    k   : Integer ;
  begin
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      b := False ;
      for k := 0 to oValues.Count - 1 do begin
        if oValues[k] > val then begin
          oValues.Insert( k, val ) ;
          b := True ;
          break ;
        end ;
      end ;

      if not b then
        oValues.Add( val ) ;
    end ;

    i := FloorS( 1.0*iValueCnt/2.0 ) ;
    if iValueCnt mod 2 <> 0 then
      res := oValues[i]
    else
      res := ( oValues[i-1] + oValues[i] )/2.0 ;

    oValues.Clear ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterWeightedMedian
//==============================================================================

  constructor TGIS_PixelFilterWeightedMedian.Create ;
  begin
    inherited ;

    oValues := TList<Single>.Create ;

    prepareMask( GIS_PIXEL_FILTER_MASK_WEIGHT, aWeightMask ) ;
  end ;


  procedure TGIS_PixelFilterWeightedMedian.doDestroy ;
  begin
    FreeObject( oValues ) ;

    inherited ;
  end ;


  function TGIS_PixelFilterWeightedMedian.fget_WeightMask
    : String ;
  begin
    Result := maskToString( aWeightMask ) ;
  end ;


  procedure TGIS_PixelFilterWeightedMedian.fset_WeightMask(
    const _val : String
  ) ;
  begin
    prepareMask( _val, aWeightMask ) ;
  end ;


  function TGIS_PixelFilterWeightedMedian.fget_BlockSize
    : Integer ;
  begin
    Result := iBlockSize ;
  end ;


  procedure TGIS_PixelFilterWeightedMedian.fset_BlockSize(
    const _val : Integer
  ) ;
  begin
    // do nothing
  end ;


  function TGIS_PixelFilterWeightedMedian.procBlock
    : Single ;
  var
    val : Single ;
    res : Single ;
    cnt : Integer ;
    b   : Boolean ;
    i   : Integer ;
    k   : Integer ;
    kk  : Integer ;
    wm  : Integer ;
  begin
    cnt := 0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      b := False ;
      for k := 0 to oValues.Count - 1 do begin
        if oValues[k] > val then begin
          wm := FloorS( aWeightMask[i] ) - 1 ;
          for kk := 0 to wm do begin
            oValues.Insert( k, val ) ;
            inc( cnt ) ;
          end ;
          b := True ;
          break ;
        end ;
      end ;

      if not b then begin
        wm := FloorS( aWeightMask[i] ) - 1 ;
        for kk := 0 to wm do begin
          oValues.Add( val ) ;
          inc( cnt ) ;
        end ;
      end ;
    end ;

    i := FloorS( 1.0*cnt/2.0 ) ;
    if cnt mod 2 <> 0 then
      res := oValues[i]
    else
      res := ( oValues[i-1] + oValues[i] )/2.0 ;

    oValues.Clear ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_PixelFilterSum
//==============================================================================

  constructor TGIS_PixelFilterSum.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterSum.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterSum.procBlock
    : Single ;
  var
    val : Single ;
    mus : Single ;
    i   : Integer ;
  begin
    mus := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        val := srcValue ;

      mus := mus + val ;
    end ;

    Result := mus ;
  end ;


//==============================================================================
// TGIS_PixelFilterStandardDeviation
//==============================================================================

  constructor TGIS_PixelFilterStandardDeviation.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterStandardDeviation.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterStandardDeviation.procBlock
    : Single ;
  var
    val  : Single ;
    mean : Single ;
    sqsm : Single ;
    i    : Integer ;
  begin
    if iValueCnt = 1 then begin
      Result := 0.0 ;
      exit ;
    end ;

    mean := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      mean := mean + val ;
    end ;
    mean := mean/iValueCnt ;

    sqsm := 0.0 ;
    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      sqsm := sqsm + ( val - mean )*( val - mean ) ;
    end ;
    sqsm := sqsm/( iValueCnt - 1 ) ;

    Result := Sqrt( sqsm ) ;
  end ;


//==============================================================================
// TGIS_PixelFilterUniqueCount
//==============================================================================

  constructor TGIS_PixelFilterUniqueCount.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterUniqueCount.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterUniqueCount.procBlock
    : Single ;
  var
    val  : Single ;
    cnt  : Integer ;
    imax : Integer ;
    i    : Integer ;
    k    : Integer ;
  begin
    cnt := 0 ;
    imax := iBlockSize*iBlockSize - 1 ;
    for i := 0 to imax do begin
      val := aBlock[i] ;

      if val = srcNoData then
        continue ;

      inc( cnt ) ;
      for k := i + 1 to imax do begin
        if aBlock[k] = val then
          aBlock[k] := srcNoData ;
      end ;
    end ;

    Result := 1.0*cnt ;
  end ;


//==============================================================================
// TGIS_PixelFilterMorphological
//==============================================================================

  constructor TGIS_PixelFilterMorphological.Create ;
  begin
    inherited ;

    prepareMask( GIS_PIXEL_FILTER_MASK_NEUTRAL_3, aCustomMask ) ;
    makeSESquare ;
    FStructuringElementType := TGIS_PixelFilterStructuringElementType.Square ;
  end ;


  procedure TGIS_PixelFilterMorphological.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterMorphological.fget_StructuringElementType
    : TGIS_PixelFilterStructuringElementType ;
  begin
    Result := FStructuringElementType;
  end ;


  procedure TGIS_PixelFilterMorphological.fset_StructuringElementType(
    const _val : TGIS_PixelFilterStructuringElementType
  ) ;
  begin
    case _val of
      TGIS_PixelFilterStructuringElementType.Square :
        makeSESquare ;
      TGIS_PixelFilterStructuringElementType.Diamond :
        makeSEDiamond ;
      TGIS_PixelFilterStructuringElementType.Disk :
        makeSEDisk ;
      TGIS_PixelFilterStructuringElementType.LineHorizontal :
        makeSELineHorizontal ;
      TGIS_PixelFilterStructuringElementType.LineVertical :
        makeSELineVertical ;
      TGIS_PixelFilterStructuringElementType.LineLeftDiagonal :
        makeSELineLeftDiagonal ;
      TGIS_PixelFilterStructuringElementType.LineRightDiagonal :
        makeSELineRightDiagonal ;
      TGIS_PixelFilterStructuringElementType.Custom :
        iBlockSize := FloorS( Sqrt( 1.0*length( aCustomMask ) ) ) ;
    end ;

    FStructuringElementType := _val ;
  end ;


  function TGIS_PixelFilterMorphological.fget_CustomStructuringElement
    : String ;
  begin
    Result := maskToString( aCustomMask ) ;
  end ;


  procedure TGIS_PixelFilterMorphological.fset_CustomStructuringElement(
    const _val : String
  ) ;
  begin
    prepareMask( _val, aCustomMask ) ;
  end ;


  function TGIS_PixelFilterMorphological.fget_BlockSize
    : Integer ;
  begin
    Result := iBlockSize ;
  end ;


  procedure TGIS_PixelFilterMorphological.fset_BlockSize(
    const _val : Integer
  ) ;
  begin
    if FStructuringElementType =
        TGIS_PixelFilterStructuringElementType.Custom then
      exit ;

    if ( _val < 3 ) or ( _val mod 2 = 0 ) then
      exit ;

    iBlockSize := _val ;

    fset_StructuringElementType( FStructuringElementType ) ;
  end ;


  function TGIS_PixelFilterMorphological.getStructuringElement
    : TGIS_SingleArray ;
  begin
    if FStructuringElementType =
        TGIS_PixelFilterStructuringElementType.Custom then
      Result := aCustomMask
    else
      Result := aMask ;
  end ;


  procedure TGIS_PixelFilterMorphological.resetSE ;
  var
    w : Integer ;
    h : Integer ;
  begin
    SetLength( aMask, iBlockSize*iBlockSize ) ;

    for h := 0 to iBlockSize - 1 do begin
      for w := 0 to iBlockSize - 1 do begin
        aMask[h*iBlockSize+w] := 0.0 ;
      end ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSESquare ;
  var
    w : Integer ;
    h : Integer ;
  begin
    resetSE ;

    for h := 0 to iBlockSize - 1 do begin
      for w := 0 to iBlockSize - 1 do begin
        aMask[h*iBlockSize+w] := 1.0 ;
      end ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSEDiamond ;
  var
    w : Integer ;
    h : Integer ;
    c : Integer ;
  begin
    resetSE ;

    c := iBlockSize div 2 ;

    for h := 0 to iBlockSize - 1 do begin
      if h < c then begin
        for w := c-h to c+h do begin
          aMask[h*iBlockSize+w] := 1.0 ;
        end ;
      end
      else
      if h > c then begin
        for w := 0 to iBlockSize - 1 do begin
          aMask[h*iBlockSize+w] := aMask[(2*c-h)*iBlockSize+w] ;
        end ;
      end
      else begin
        for w := 0 to iBlockSize - 1 do begin
          aMask[h*iBlockSize+w] := 1.0 ;
        end ;
      end ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSEDisk ;
  var
    w  : Integer ;
    h  : Integer ;
    c  : Integer ;
    ww : Integer ;
    hh : Integer ;
    r  : Single ;
  begin
    resetSE ;

    c := iBlockSize div 2 ;

    for h := 0 to iBlockSize - 1 do begin
      hh := h - c ;
      hh := hh*hh ;
      for w := 0 to iBlockSize - 1 do begin
        ww := w - c ;
        ww := ww*ww ;
        r := Sqrt( 1.0*( hh + ww ) ) ;
        if r <= 1.0*c then
          aMask[h*iBlockSize+w] := 1.0 ;
      end ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSELineHorizontal ;
  var
    w : Integer ;
    h : Integer ;
  begin
    resetSE ;

    h := iBlockSize div 2 ;

    for w := 0 to iBlockSize - 1 do begin
      aMask[h*iBlockSize+w] := 1.0 ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSELineVertical ;
  var
    w : Integer ;
    h : Integer ;
  begin
    resetSE ;

    w := iBlockSize div 2 ;

    for h := 0 to iBlockSize - 1 do begin
      aMask[h*iBlockSize+w] := 1.0 ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSELineLeftDiagonal ;
  var
    c : Integer ;
  begin
    resetSE ;

    for c := 0 to iBlockSize - 1 do begin
      aMask[c*iBlockSize+c] := 1.0 ;
    end ;
  end ;


  procedure TGIS_PixelFilterMorphological.makeSELineRightDiagonal ;
  var
    c : Integer ;
  begin
    resetSE ;

    for c := 0 to iBlockSize - 1 do begin
      aMask[c*iBlockSize+(iBlockSize-1-c)] := 1.0 ;
    end ;
  end ;


//==============================================================================
// TGIS_PixelFilterErosion
//==============================================================================

  constructor TGIS_PixelFilterErosion.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterErosion.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterErosion.procBlock
    : Single ;
  var
    mask : TGIS_SingleArray ;
    val  : Single ;
    vmin : Single ;
    i    : Integer ;
  begin
    mask := getStructuringElement ;

    vmin := GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if ( val = srcNoData ) or ( mask[i] = 0.0 ) then
        continue ;

      vmin := Min( val, vmin ) ;
    end ;

    Result := vmin ;
  end ;


//==============================================================================
// TGIS_PixelFilterDilation
//==============================================================================

  constructor TGIS_PixelFilterDilation.Create ;
  begin
    inherited ;

  end ;


  procedure TGIS_PixelFilterDilation.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_PixelFilterDilation.procBlock
    : Single ;
  var
    mask : TGIS_SingleArray ;
    val  : Single ;
    vmax : Single ;
    i    : Integer ;
  begin
    mask := getStructuringElement ;

    vmax := -GIS_MAX_SINGLE ;

    for i := 0 to iBlockSize*iBlockSize - 1 do begin
      val := aBlock[i] ;

      if ( val = srcNoData ) or ( mask[i] = 0.0 ) then
        continue ;

      vmax := Max( val, vmax ) ;
    end ;

    Result := vmax ;
  end ;


//==============================================================================
// TGIS_PixelFilterOpening
//==============================================================================

  constructor TGIS_PixelFilterOpening.Create ;
  begin
    inherited ;

    bCompound := True ;
  end ;


  procedure TGIS_PixelFilterOpening.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterOpening.procCompound ;
  var
    imdt : TGIS_LayerPixel ;
    fltr : TGIS_PixelFilterMorphological ;
  begin
    imdt := TGIS_LayerPixel.Create ;
    try
      imdt.Build(
        SourceLayer.IsGrid, SourceLayer.CS, SourceLayer.Extent,
        SourceLayer.BitWidth, SourceLayer.BitHeight
      ) ;
      imdt.Name := RandomGenerator.AlphanumericString( 8 ) ;

      fltr := TGIS_PixelFilterErosion.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := SourceLayer ;
        fltr.DestinationLayer := imdt ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

      fltr := TGIS_PixelFilterDilation.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := imdt ;
        fltr.DestinationLayer := DestinationLayer ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

    finally
      FreeObject( imdt ) ;
    end ;
  end ;


  function TGIS_PixelFilterOpening.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterClosing
//==============================================================================

  constructor TGIS_PixelFilterClosing.Create ;
  begin
    inherited ;

    bCompound := True ;
  end ;


  procedure TGIS_PixelFilterClosing.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterClosing.procCompound ;
  var
    imdt : TGIS_LayerPixel ;
    fltr : TGIS_PixelFilterMorphological ;
  begin
    imdt := TGIS_LayerPixel.Create ;
    try
      imdt.Build(
        SourceLayer.IsGrid, SourceLayer.CS, SourceLayer.Extent,
        SourceLayer.BitWidth, SourceLayer.BitHeight
      ) ;
      imdt.Name := RandomGenerator.AlphanumericString( 9 ) ;

      fltr := TGIS_PixelFilterDilation.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := SourceLayer ;
        fltr.DestinationLayer := imdt ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

      fltr := TGIS_PixelFilterErosion.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := imdt ;
        fltr.DestinationLayer := DestinationLayer ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

    finally
      FreeObject( imdt ) ;
    end ;
  end ;


  function TGIS_PixelFilterClosing.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterTopHat
//==============================================================================

  constructor TGIS_PixelFilterTopHat.Create ;
  begin
    inherited ;

    bCompound := True ;
  end ;


  procedure TGIS_PixelFilterTopHat.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterTopHat.procCompound ;
  var
    imdt : TGIS_LayerPixel ;
    fltr : TGIS_PixelFilterMorphological ;
    popr : T_pixelArithmetic ;
  begin
    imdt := TGIS_LayerPixel.Create ;
    try
      imdt.Build(
        SourceLayer.IsGrid, SourceLayer.CS, SourceLayer.Extent,
        SourceLayer.BitWidth, SourceLayer.BitHeight
      ) ;
      imdt.Name := RandomGenerator.AlphanumericString( 10 ) ;

      fltr := TGIS_PixelFilterOpening.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := SourceLayer ;
        fltr.DestinationLayer := imdt ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

      popr := T_pixelArithmetic.Create ;
      try
        if assigned( FOnBusy ) then
          popr.BusyEvent := BusyEvent ;

        popr.SourceLayer1 := SourceLayer ;
        popr.SourceLayer2 := imdt ;
        popr.DestinationLayer := DestinationLayer ;
        popr.Band := Band ;
        popr.ColorSpace := ColorSpace ;
        popr.Operation := T_pixelOperationType.Subtract ;

        popr.Execute ;
      finally
        FreeObject( popr ) ;
      end ;

    finally
      FreeObject( imdt ) ;
    end ;
  end ;


  function TGIS_PixelFilterTopHat.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// TGIS_PixelFilterBottomHat
//==============================================================================

  constructor TGIS_PixelFilterBottomHat.Create ;
  begin
    inherited ;

    bCompound := True ;
  end ;


  procedure TGIS_PixelFilterBottomHat.doDestroy ;
  begin

    inherited ;
  end ;


  procedure TGIS_PixelFilterBottomHat.procCompound ;
  var
    imdt : TGIS_LayerPixel ;
    fltr : TGIS_PixelFilterMorphological ;
    popr : T_pixelArithmetic ;
  begin
    imdt := TGIS_LayerPixel.Create ;
    try
      imdt.Build(
        SourceLayer.IsGrid, SourceLayer.CS, SourceLayer.Extent,
        SourceLayer.BitWidth, SourceLayer.BitHeight
      ) ;
      imdt.Name := RandomGenerator.AlphanumericString( 11 ) ;

      fltr := TGIS_PixelFilterClosing.Create ;
      try
        if assigned( FOnBusy ) then
          fltr.BusyEvent := BusyEvent ;

        fltr.SourceLayer := SourceLayer ;
        fltr.DestinationLayer := imdt ;
        fltr.Band := Band ;
        fltr.ColorSpace := ColorSpace ;
        fltr.StructuringElementType := StructuringElementType ;
        if StructuringElementType =
            TGIS_PixelFilterStructuringElementType.Custom then
          fltr.CustomStructuringElement := CustomStructuringElement ;

        fltr.Execute ;
      finally
        FreeObject( fltr ) ;
      end ;

      popr := T_pixelArithmetic.Create ;
      try
        if assigned( FOnBusy ) then
          popr.BusyEvent := BusyEvent ;

        popr.SourceLayer1 := imdt ;
        popr.SourceLayer2 := SourceLayer ;
        popr.DestinationLayer := DestinationLayer ;
        popr.Band := Band ;
        popr.ColorSpace := ColorSpace ;
        popr.Operation := T_pixelOperationType.Subtract ;

        popr.Execute ;
      finally
        FreeObject( popr ) ;
      end ;

    finally
      FreeObject( imdt ) ;
    end ;
  end ;


  function TGIS_PixelFilterBottomHat.procBlock
    : Single ;
  begin
    // do nothing
    Result := 0 ;
  end ;


//==============================================================================
// Lider.CG.GIS.GeoPixelFilter
//==============================================================================

  class procedure GisPixelFilter.SelfRegisterPipeline ;
  begin
    RegisterPipeline(
      'Filter.Threshold',
      T_Pipeline_FilterThreshold
    ) ;
    RegisterPipeline(
      'Filter.NoiseSaltPepper',
      T_Pipeline_FilterNoiseSaltPepper
    ) ;
    RegisterPipeline(
      'Filter.NoiseGaussian',
      T_Pipeline_FilterNoiseGaussian
    ) ;
    RegisterPipeline(
      'Filter.Convolution',
      T_Pipeline_FilterConvolution
    ) ;
    RegisterPipeline(
      'Filter.SobelMagnitude',
      T_Pipeline_FilterSobelMagnitude
    ) ;
    RegisterPipeline(
      'Filter.Range',
      T_Pipeline_FilterRange
    ) ;
    RegisterPipeline(
      'Filter.Midpoint',
      T_Pipeline_FilterMidpoint
    ) ;
    RegisterPipeline(
      'Filter.Minimum',
      T_Pipeline_FilterMinimum
    ) ;
    RegisterPipeline(
      'Filter.Maximum',
      T_Pipeline_FilterMaximum
    ) ;
    RegisterPipeline(
      'Filter.ArithmeticMean',
      T_Pipeline_FilterArithmeticMean
    ) ;
    RegisterPipeline(
      'Filter.AlphaTrimmedMean',
      T_Pipeline_FilterAlphaTrimmedMean
    ) ;
    RegisterPipeline(
      'Filter.ContraHarmonicMean',
      T_Pipeline_FilterContraHarmonicMean
    ) ;
    RegisterPipeline(
      'Filter.GeometricMean',
      T_Pipeline_FilterGeometricMean
    ) ;
    RegisterPipeline(
      'Filter.HarmonicMean',
      T_Pipeline_FilterHarmonicMean
    ) ;
    RegisterPipeline(
      'Filter.WeightedMean',
      T_Pipeline_FilterWeightedMean
    ) ;
    RegisterPipeline(
      'Filter.YpMean',
      T_Pipeline_FilterYpMean
    ) ;
    RegisterPipeline(
      'Filter.Majority',
      T_Pipeline_FilterMajority
    ) ;
    RegisterPipeline(
      'Filter.Minority',
      T_Pipeline_FilterMinority
    ) ;
    RegisterPipeline(
      'Filter.Median',
      T_Pipeline_FilterMedian
    ) ;
    RegisterPipeline(
      'Filter.WeightedMedian',
      T_Pipeline_FilterWeightedMedian
    ) ;
    RegisterPipeline(
      'Filter.Sum',
      T_Pipeline_FilterSum
    ) ;
    RegisterPipeline(
      'Filter.StandardDeviation',
      T_Pipeline_FilterStandardDeviation
    ) ;
    RegisterPipeline(
      'Filter.UniqueCount',
      T_Pipeline_FilterUniqueCount
    ) ;
    RegisterPipeline(
      'Filter.Erosion',
      T_Pipeline_FilterErosion
    ) ;
    RegisterPipeline(
      'Filter.Dilation',
      T_Pipeline_FilterDilation
    ) ;
    RegisterPipeline(
      'Filter.Opening',
      T_Pipeline_FilterOpening
    ) ;
    RegisterPipeline(
      'Filter.Closing',
      T_Pipeline_FilterClosing
    ) ;
    RegisterPipeline(
      'Filter.TopHat',
      T_Pipeline_FilterTopHat
    ) ;
    RegisterPipeline(
      'Filter.BottomHat',
      T_Pipeline_FilterBottomHat
    ) ;
  end ;


//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  GisPixelFilter.SelfRegisterPipeline ;

finalization
  FreeObject( oRandom ) ;
{$ENDIF}

//==================================== END =====================================
end.
