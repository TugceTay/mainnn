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
  Encapsulation of Contour Generation method.

  This unit was partially based on GDAL Contour Generator API.

  Copyright (c) 2003, Frank Warmerdam <warmerdam@pobox.com>
  Copyright (c) 2003, Applied Coherent Technology Corporation, www.actgate.com

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit GisContour ;
  {$HPPEMIT '#pragma link "GisContour"'}
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
    System.Collections,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Generics.Collections,
    GisClasses,
    GisLayerPixel,
    GisLayerVector,
    GisRtl,
    GisTypes;
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
  Unit_GisContour = class
    public
      class procedure SelfRegisterPipeline ;
  end ;

  /// <summary>
  ///   Specifies output type for contouring process.
  /// </summary>
  TGIS_ContourGeneratorMode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Plain polylines like the contouring algorithm provide.
    /// </summary>
    Polylines,

    /// <summary>
    ///   Force polylines' closure.
    /// </summary>
    ClosedPolylines,

    /// <summary>
    ///   Overlapping polygon shells where the lower limit value increase by
    ///   ContourInterval from ContourBase and upper equals maximum value of
    ///   the source grid; for example 0..100, 10..100, 20..100, etc.
    /// </summary>
    OverlappingPolygons,

    /// <summary>
    ///   Non-overlapping polygon bands with range of lower and upper limit
    ///   equals ContourInterval; for example 0..10, 10..20, 20..30, etc.
    /// </summary>
    /// <remarks>
    ///   To get ribbon-like output polygons set FixedLevels property,
    ///   instead of ContourBase and ContourInterval.
    /// </remarks>
    Polygons
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   Implementation of the contour (isohypse) generation algorithm for
  ///   grid/DEM layers.
  /// </summary>
  TGIS_ContourGenerator = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_BaseObjectDisposable )
    private
      const
        // Field name for temporary writing contour values
        FIELD_VALUE                 = 'cntVALUE' ;
        FIELD_INTERVAL              = 'cntINTERV' ;
        // fields params
        FIELD_WIDTH                 = 10 ;
        FIELD_DECIMAL               = 6 ;

    private
      FMode                         : TGIS_ContourGeneratorMode ;
      FContourInterval              : Double ;
      FContourBase                  : Double ;
      FFixedLevels                  : TGIS_DoubleArray ;
      FCustomNoData                 : Boolean ;
      FNoDataValue                  : Double ;
      FSmoothen                     : Boolean ;
      FSmoothFactor                 : Integer ;
      FMinSize                      : Double ;

    private
      // number of contour level values
      contourCount                  : Integer ;
      // maximum contour find in base contour algorithm
      maxLevel                      : Double ;
      // event manager prototype for smart progress changing and event raising
      busyEventManager                  : TGIS_BusyEventManager ;

    private
      function fget_FixedLevelCount : Integer ;
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Calculates ContourInterval for default 20 levels and
      ///   sets ContourBase to the lowest value of the source layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      procedure CalculateInterval   ( const _src    : TGIS_LayerPixel
                                    ) ; overload;

      /// <summary>
      ///   Calculates ContourInterval for the given number of levels and
      ///   sets ContourBase to the lowest value of the source layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_cnt">
      ///   number of levels
      /// </param>
      procedure CalculateInterval   ( const _src    : TGIS_LayerPixel ;
                                      const _cnt    : Integer
                                    ) ; overload;

      /// <summary>
      ///   Creates vector contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer, must have the same CS as the source layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      /// <remarks>
      ///   '_dstfld' must already exist in '_dst' layer.
      /// </remarks>
      function Generate             ( const _src    : TGIS_LayerPixel ;
                                      const _dst    : TGIS_LayerVector ;
                                      const _dstfld : String
                                    ) : Boolean ; overload ;

      /// <summary>
      ///   Creates vector contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer, must have the same CS as the source layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the field of the specified name will be populated
      ///   with the elevation values of the contours
      /// </param>
      /// <param name="_dstfld_interval">
      ///   if not empty the field of the specified name will be populated
      ///   with the contours' intervals; only for polygonal contours
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      /// <remarks>
      ///   '_dstfld' and '_dstfld_interval' must already exist in '_dst' layer.
      ///   '_dstfld' and '_dstfld_interval' must be different.
      /// </remarks>
      function Generate             ( const _src             : TGIS_LayerPixel ;
                                      const _dst             : TGIS_LayerVector ;
                                      const _dstfld          : String ;
                                      const _dstfld_interval : String
                                    ) : Boolean ; overload ;

      /// <summary>
      ///   Type of contour generator output; default is Polylines.
      /// </summary>
      property Mode                 : TGIS_ContourGeneratorMode
                                      read  FMode
                                      write FMode ;

      /// <summary>
      ///   The elevation interval between generated contours.
      /// </summary>
      property ContourInterval      : Double
                                      read  FContourInterval
                                      write FContourInterval ;

      /// <summary>
      ///   The "base" relative to which contour intervals are applied.
      ///   To generate 10m contours at 5, 15, 25, ... the ContourBase would be
      ///   5.
      /// </summary>
      property ContourBase          : Double
                                      read  FContourBase
                                      write FContourBase ;

      /// <summary>
      ///   The number of elements in FixedLevels property.
      /// </summary>
      property FixedLevelCount      : Integer
                                      read  fget_FixedLevelCount ;

      /// <summary>
      ///   The list of fixed levels at which contours should be generated.
      ///   If not empty then ContourInterval and ContourBase are ignored.
      /// </summary>
      property FixedLevels          : TGIS_DoubleArray
                                      read  FFixedLevels
                                      write FFixedLevels ;

      /// <summary>
      ///   If True the NoDataValue will be used as nodata value;
      ///   default is False
      /// </summary>
      property CustomNoData         : Boolean
                                      read  FCustomNoData
                                      write FCustomNoData ;

      /// <summary>
      ///   The value to use as a NoData value. A pixel value which should be
      ///   ignored in generating contours; default is -9999
      /// </summary>
      property NoDataValue          : Double
                                      read  FNoDataValue
                                      write FNoDataValue ;

      /// <summary>
      ///   If True then the generated contours will be smoothened.
      ///   Use carefully when creating polygon output because it may produce
      ///   invalid geometry.
      /// </summary>
      property Smoothen             : Boolean
                                      read  FSmoothen
                                      write FSmoothen ;

      /// <summary>
      ///   Smooth factor. Default value is 5.
      /// </summary>
      property SmoothFactor         : Integer
                                      read  FSmoothFactor
                                      write FSmoothFactor ;

      /// <summary>
      ///   Minimum dimension in meters for generated features;
      ///   Minimum length for polylines, minimum area for polygons.
      /// </summary>
      property MinSize              : Double
                                      read  FMinSize
                                      write FMinSize ;

    published // events
      /// <summary>
      ///   Event fired upon progress of the contour generation process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent             : TGIS_BusyEvent
                                      add fadd_BusyEvent
                                      remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent          : TGIS_BusyEvent
                                      read  fget_BusyEvent
                                      write fset_BusyEvent ;
      {$ENDIF}

    private
      /// <summary>
      ///   Creates polyline contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function generatePolylines    ( const _src    : TGIS_LayerPixel ;
                                      const _dst    : TGIS_LayerVector ;
                                      const _dstfld : String
                                    ) : Boolean ;

      /// <summary>
      ///   Creates closed polyline contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function generateClosedPolylines ( const _src    : TGIS_LayerPixel ;
                                         const _dst    : TGIS_LayerVector ;
                                         const _dstfld : String
                                       ) : Boolean ;

      /// <summary>
      ///   Creates overlapping polygon contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_dstfld_interval">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with contours' intervals
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function generateOverlappingPolygons ( const _src             : TGIS_LayerPixel ;
                                             const _dst             : TGIS_LayerVector ;
                                             const _dstfld          : String ;
                                             const _dstfld_interval : String
                                           ) : Boolean ;

      /// <summary>
      ///   Creates non-overlapping polygon contours from a raster grid layer.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_dstfld_interval">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with contours' intervals
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function generatePolygons          ( const _src             : TGIS_LayerPixel ;
                                           const _dst             : TGIS_LayerVector ;
                                           const _dstfld          : String ;
                                           const _dstfld_interval : String
                                         ) : Boolean ;

      /// <summary>
      ///   Generate closed polylines and convert them to polygons.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_dstfld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_dstfld_interval">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with contours' intervals
      /// </param>
      /// <param name="_overlapping">
      ///   if False polygons with rings will be generated
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function generateContoursAndConvertToPolygons
                                    ( const _src             : TGIS_LayerPixel ;
                                      const _dst             : TGIS_LayerVector ;
                                      const _dstfld          : String;
                                      const _dstfld_interval : String ;
                                      const _overlapping     : Boolean
                                    ) : Boolean ;

      /// <summary>
      ///   Convert contour polylines to polygons.
      /// </summary>
      /// <param name="_arc_lv">
      ///   source - contour polylines
      /// </param>
      /// <param name="_polyg_lv">
      ///   destination - polygon vector layer
      /// </param>
      /// <param name="_fld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_fld_interval">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with contours' intervals
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function convertArcToPolygon  ( const _arc_lv          : TGIS_LayerVector ;
                                      const _polyg_lv        : TGIS_LayerVector ;
                                      const _fld             : String ;
                                      const _fld_interval    : String ;
                                      const _uids_to_delete  : TList< Int64 >
                                    ) : Boolean ;

      /// <summary>
      ///   Expand grid layer by defined radius.
      /// </summary>
      /// <param name="_src">
      ///   source grid layer
      /// </param>
      /// <param name="_radius">
      ///   radius of buffer in grid cells
      /// </param>
      /// <param name="_fillNoData">
      ///   if True NoData cells will be filled with indicated value
      /// </param>
      /// <param name="_val">
      ///   value to write for NoData cells
      /// </param>
      /// <returns>
      ///   Expanded grid layer.
      /// </returns>
      function expandGrid           ( const _src        : TGIS_LayerPixel ;
                                      const _radius     : Integer ;
                                      const _fillNoData : Boolean ;
                                            _val        : Single
                                    ) : TGIS_LayerPixel ;

      /// <summary>
      ///   Applies difference on all features polygons
      ///   to generate non-overlapping polygons
      /// </summary>
      /// <param name="_polyg_lv">
      ///   polygon vector layer
      /// </param>
      /// <param name="_fld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_delete_last_band">
      ///   if True last polygon band will be deleted
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function erasePolygons        ( const _polyg_lv         : TGIS_LayerVector ;
                                      const _fld              : String ;
                                      const _delete_last_band : Boolean ;
                                      const _uids_to_delete   : TList< Int64 >
                                    ) : Boolean ;

      /// <summary>
      ///   Deletes unnecessary shapes by passing Uid list.
      /// </summary>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_uids_to_delete">
      ///   shapes' Uids selected to deletion
      /// </param>

      procedure deleteUnnecessaryShapes ( const _dst            : TGIS_LayerVector ;
                                          const _uids_to_delete : TList<Int64>
                                        ) ;

      /// <summary>
      ///   Fill Elevation and Measure (geometry Z and M) with proper values.
      /// </summary>
      /// <param name="_dst">
      ///   destination vector layer
      /// </param>
      /// <param name="_fld">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with the elevation values of the contours
      /// </param>
      /// <param name="_fld_interval">
      ///   if not empty the attribute field of the specified name will be
      ///   populated with contours' intervals
      /// </param>
      /// <returns>
      ///   True if the process was completed successfully; False otherwise
      /// </returns>
      function fillZMValue          ( const _dst              : TGIS_LayerVector ;
                                      const _fld           : String ;
                                      const _fld_interval  : String
                                    ) : Boolean ;


      /// <summary>
      ///   Build safe SQL query for given contour value.
      ///   Safe means that it uses floating-point comparison.
      /// </summary>
      /// <param name="_fld">
      ///   name of attribute field
      /// </param>
      /// <param name="_val">
      ///   contour value
      /// </param>
      /// <returns>
      ///   SQL query string
      /// </returns>
      /// <remarks>
      ///   For example, with _fld='FIELD' and _val=5 we get string like below
      ///   "(FIELD &gt; 4.999999 AND FIELD &lt 5.000001)"
      /// </remarks>
      function buildContourQuery    ( const _fld              : String ;
                                      const _val              : Double
                                    ) : String ;

      /// <summary>
      ///   Calculate current "i-th" contour value.
      /// </summary>
      /// <param name="_i">
      ///   contour index
      /// </param>
      /// <returns>
      ///   contour value with index "i"
      /// </returns>
      function getContourValue      ( const _i                : Integer
                                    ) : Double ;


      // Count number of contour values (levels) to be generated.
      function getContourCount                                : Integer ;

      // Number of algorithm steps; needed for multistep progress;
      // fortunately it depends on FMode
      function getNumberOfSteps                               : Integer ;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Math,
  System.SysUtils,
  System.Variants,

  GisCsSystems,
  GisFunctions,
  GisGeometryFactory,
  GisInternals,
  GisPipeline,
  GisResource,
  GisTopology;
{$ENDIF}

type

  T_CONTOUR_CONST = class
    public const
      // The amount of a contour interval that pixels should be fudged by if they
      // match a contour level exactly.
      FUDGE_EXACT      = 0.001 ;

      // The amount of a pixel that line ends need to be within to be considered to
      // match for joining purposes.
      JOIN_DIST        = 0.0001 ;

      // radius of buffer for expanding grid.
      RADIUS           = 1 ;

      // class fields
      NO_INTERVALS     = 20 ;
      CONTOUR_BASE     = 0.0 ;
      CONTOUR_INTERVAL = 100.0 ;
      CUSTOM_NODATA    = False ;
      NODATA_VALUE     = -9999.0 ;
      MIN_SIZE         = 0.0 ;
      SMOOTHEN         = False ;
      SMOOTH_FACTOR    = 5 ;
  end ;

  // To avoid compiler error in T_Pipeline_Contour generation
  T_Dummy = class( TGIS_PipelineOperationExtendedAbstract )
  end ;

  T_Pipeline_Contour = class( TGIS_PipelineOperationExtendedAbstract )
    const
      PARAM_CONTOUR_BASE     = 'ContourBase';
      PARAM_CONTOUR_INTERVAL = 'ContourInterval';
      PARAM_CUSTOM_NODATA    = 'CustomNoData';
      PARAM_FIXED_LEVELS     = 'FixedLevels';
      PARAM_MIN_SIZE         = 'MinSize';
      PARAM_MODE             = 'Mode';
      PARAM_NODATA_VALUE     = 'NoDataValue';
      PARAM_SMOOTHEN         = 'Smoothen';
      PARAM_SMOOTH_FACTOR    = 'SmoothFactor';
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  // T_ContourItem class.
  T_ContourItem =  class( TGIS_Object )
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      FUsed       : Boolean ;
      FLevel      : Double ;
      FPoints     : Integer ;
      FMaxPoints  : Integer ;
      FTailX      : Double ;
      FLeftIsHigh : Boolean ;
      FX          : TGIS_SingleArray ;
      FY          : TGIS_SingleArray ;
    protected

      // Allocate arrays for points.
      // _newPoints  new points count
      procedure makeRoomFor( const _newPoints : Integer
                           ) ;

      // Coumpute the square of the euclidian distance between (x0;y0)-(x1;y1).
      // _x0 _y0 _x1 _y1  coordinates
      function distanceSqr ( const _x0 : Double ;
                             const _y0 : Double ;
                             const _x1 : Double ;
                             const _y1 : Double
                           ) : Double ;
      // Try to find a match case between line ends..
      // _ax0 _ay0 _ax1 _ay1 _bx0 _by0 _bx1 _by1 coordinates
      function mergeCase   ( const _ax0 : Double ;
                             const _ay0 : Double ;
                             const _ax1 : Double ;
                             const _ay1 : Double ;
                             const _bx0 : Double ;
                             const _by0 : Double ;
                             const _bx1 : Double ;
                             const _by1 : Double
                           ) : Integer ;
    protected

      // Destructor.
      procedure doDestroy ; override;

    public

      // Constructor.
      // _level   level value
      constructor Create   ( const _level     : Double
                           ) ;

      // Add segment.
      // _XStart    start X coordinate
      // _YStart    start Y coordinate
      // _XEnd      end X coordinate
      // _YEnd      end Y coordinate
      // _leftHigh  if True, left site is higher
      // return     True if segment was added
      function  addSegment ( const _XStart    : Double ;
                             const _YStart    : Double ;
                             const _XEnd      : Double ;
                             const _YEnd      : Double ;
                             const _leftHigh  : Boolean
                           ) : Boolean ;

      // Merge items.
      // _other  item handle
      // return  True if items were merged
      function  Merge      ( const _other     : T_ContourItem
                           ) : Boolean ;

      // Prepare buffer for ejection.
      //
      procedure PrepareEjection ;

  end ;

  // T_ContourLevel class.
  T_ContourLevel = class( TGIS_Object )
    protected
      FLevel      : Double ;
      FEntryMax   : Integer ;
      FEntryCount : Integer ;
      FContours   : TList<T_ContourItem> ;

    protected
      function fget_GetContour ( const _i            : Integer
                               ) : T_ContourItem ;

    protected
      // Destructor.
      procedure doDestroy ; override;

    public
      // Constructor.
      // _level  level value
      constructor Create     ( const _level        : Double
                             ) ;

      // Adjust contour.
      // Assume the indicated contour's tail may have changed, and adjust it up
      // or down in the list of contours to re-establish proper ordering.
      // _changed  changed index
      procedure AdjustContour( const _changed      : Integer
                             ) ;

      // Remove contour.
      // _target  index
      procedure RemoveContour( const _target       : Integer
                             ) ;

      // Find contour.
      // Perform a binary search to find the requested "tail" location.  If not
      // available return -1. In theory there can be more than one contour with the
      // same tail X and different Y tails ... ensure we check against them all.
      // _fX     x coordinate
      // _fY     y coordinate
      // return  index
      function  FindContour  ( const _fX           : Double ;
                               const _fY           : Double
                             ) : Integer ;

      // Insert contour.
      // Ensure the newly added contour is placed in order according to the X
      // value relative to the other contours.
      // _newContour  contour
      // return       position
      function  InsertContour( const _newContour   : T_ContourItem
                             ) : Integer ;
    public
      property Level                         : Double        read FLevel ;
      property ContourCount                  : Integer       read FEntryCount ;
      property Contour[ const _i : Integer ] : T_ContourItem read fget_GetContour ;
  end ;

  // T_ContourWriterInfo record.
  T_ContourWriterInfo = record
    VLayer       : TObject ;
    GeoTransform : array of Double ;
    ElevField    : String ;
    Smooth       : Boolean ;
    SmoothFactor : Integer ;
    MinSize      : Double ;
  end ;

  // T_OnContourWrite method.
  T_OnContourWrite = function( const _level    : Double ;
                               const _points   : Integer ;
                               const _adfX     : TGIS_SingleArray ;
                               const _adfY     : TGIS_SingleArray ;
                               const _info     : T_ContourWriterInfo
                             ) : Boolean ;

  // T_ContourGenerator class.
  T_ContourGenerator = class( TGIS_Object )
    private
      FWidth            : Integer ;
      FHeight           : Integer ;
      FLine             : Integer ;
      FLastLine         : TGIS_SingleArray ;
      FThisLine         : TGIS_SingleArray ;
      FLevelMax         : Double ;
      FLevelCount       : Integer ;
      FLevels           : TList<T_ContourLevel> ;
      FNoDataActive     : Boolean ;
      FNoDataValue      : Double  ;
      FFixedLevels      : Boolean ;
      FContourInterval  : Double  ;
      FContourOffset    : Double  ;
      FWriter           : T_OnContourWrite ;
      FWriterInfo       : T_ContourWriterInfo ;

    protected

      // Add segment.
      // _level     level value
      // _fX1       coordinate
      // _fY1       coordinate
      // _fX2       coordinate
      // _fY2       coordinate
      // _leftHigh  if True, left site is higher
      // return     False if segment was added
      function  addSegment      ( const _level      : Double ;
                                  const _fX1        : Double ;
                                  const _fY1        : Double ;
                                  const _fX2        : Double ;
                                  const _fY2        : Double ;
                                  const _leftHigh   : Boolean
                                 ) : Boolean ;

      // Process pixel.
      // _iPixel  pixel position
      function  processPixel    ( const _iPixel     : Integer
                                ) : Boolean ;

      // Process rectangle.
      // _fUpLeft    coordinate
      // _fUpLeftX   coordinate
      // _fUpLeftY   coordinate
      // _fLoLeft    coordinate
      // _fLoLeftX   coordinate
      // _fLoLeftY   coordinate
      // _fLoRight   coordinate
      // _fLoRightX  coordinate
      // _fLoRightY  coordinate
      // _fUpRight   coordinate
      // _fUpRightX  coordinate
      // _fUpRightY  coordinate
      // return      True if rectangle was processed correctly
      function  processRect     ( const _fUpLeft    : Double ;
                                  const _fUpLeftX   : Double ;
                                  const _fUpLeftY   : Double ;
                                  const _fLoLeft    : Double ;
                                  const _fLoLeftX   : Double ;
                                  const _fLoLeftY   : Double ;
                                  const _fLoRight   : Double ;
                                  const _fLoRightX  : Double ;
                                  const _fLoRightY  : Double ;
                                  const _fUpRight   : Double ;
                                  const _fUpRightX  : Double ;
                                  const _fUpRightY  : Double
                                ) : Boolean ;

      // Intersect rectangles.
      // _val1    value
      // _fX1     coordinate
      // _fY1     coordinate
      // _val2    value
      // _fX2     coordinate
      // _fY2     coordinate
      // _fNext   level
      // _level   level
      // _points  number of points
      // _adfX    array of coordinates
      // _adfY    array of coordinates
      procedure intersect       ( const _val1       : Double  ;
                                  const _fX1        : Double  ;
                                  const _fY1        : Double  ;
                                  const _val2       : Double  ;
                                  const _fX2        : Double  ;
                                  const _fY2        : Double  ;
                                  const _fNext      : Double  ;
                                  const _level      : Double  ;
                                    var _points     : Integer ;
                                  const _adfX       : TGIS_SingleArray ;
                                  const _adfY       : TGIS_SingleArray
                                ) ;

      // Find level.
      // _level  level id
      // return  level object
      function  findLevel       ( const _level           : Double
                                ) : T_ContourLevel ;

      // Eject contours.
      // _onlyUnused only used contours
      // return  True if contour was generated
      function  ejectContours   ( const _onlyUnused      : Boolean
                                ) : Boolean ;

    protected

      // Destructor.
      procedure doDestroy ; override;

    public
      // Get max level. Available only after completing the countouring algorithm
      property LevelMax : Double
                          read FLevelMax ;


      // Constructor.
      // _width       grid line width
      // _height      grid height
      // _writer      contour writer handle
      // _writerInfo  contour writer additional info
      constructor Create        ( const _width           : Integer ;
                                  const _height          : Integer ;
                                  const _writer          : T_OnContourWrite ;
                                  const _writerInfo      : T_ContourWriterInfo
                                ) ;

      // Set NoData value.
      // _noDataValue  value
      procedure SetNoData       ( const _noDataValue     : Double
                                ) ;

      // Set contour levels.
      // _contourInterval  interval between contours
      // _contourOffset    offset between contours
      procedure SetContourLevels( const _contourInterval : Double ;
                                  const _contourOffset   : Double
                                 ) ;

      // Set fixed levels.
      // _fixedLevelCount  number of predefined levels
      // _fixedLevels      predefined levels array
      procedure SetFixedLevels  ( const _fixedLevelCount : Integer ;
                                  const _fixedLevels     : TGIS_DoubleArray
                                ) ;

      // Feed line data.
      // _scanline  grid data
      // return     True if line was added
      function  FeedLine        ( const _scanline        : TGIS_SingleArray
                                ) : Boolean ;

  end ;

//==============================================================================
// T_ContourItem
//==============================================================================

  constructor T_ContourItem.Create(
    const _level : Double
  ) ;
  begin
    inherited Create ;

    FLevel      := _level ;
    FUsed       := False ;
    FPoints     := 0 ;
    FMaxPoints  := 0 ;
    FX          := nil ;
    FY          := nil ;
    FTailX      := 0.0 ;
    FLeftIsHigh := False ;
  end ;

  procedure T_ContourItem.doDestroy ;
  begin
    FX := nil ;
    FY := nil ;

    inherited ;
  end ;

  function  T_ContourItem.addSegment(
     const _XStart    : Double ;
     const _YStart    : Double ;
     const _XEnd      : Double ;
     const _YEnd      : Double ;
     const _leftHigh  : Boolean
   ) : Boolean ;
  begin
    Result := False ;

    makeRoomFor( FPoints + 1 ) ;

    // If there are no segments, just add now.
    if ( FPoints = 0 ) then begin
      FPoints := 2 ;
      FX[0]   := _XStart ;
      FY[0]   := _YStart ;
      FX[1]   := _XEnd ;
      FY[1]   := _YEnd ;
      FUsed   := True ;
      FTailX  := FX[1] ;
      FLeftIsHigh := _leftHigh ;
      Result := True ;
      exit ;
    end ;

    // Try to matching up with one of the ends, and insert.
    if ( Abs( FX[ FPoints-1 ] - _XStart ) < T_CONTOUR_CONST.JOIN_DIST ) and
       ( Abs( FY[ FPoints-1 ] - _YStart ) < T_CONTOUR_CONST.JOIN_DIST ) then begin

      FX[FPoints] := _XEnd ;
      FY[FPoints] := _YEnd ;
      FUsed       := True ;
      FTailX      := _XEnd ;
      inc( FPoints ) ;

      Result := True ;
    end
    else if ( Abs( FX[ FPoints-1 ] - _XEnd ) < T_CONTOUR_CONST.JOIN_DIST ) and
            ( Abs( FY[ FPoints-1 ] - _YEnd ) < T_CONTOUR_CONST.JOIN_DIST ) then begin

      FX[FPoints] := _XStart ;
      FY[FPoints] := _YStart ;
      FUsed       := True ;
      FTailX      := _XStart ;
      inc( FPoints ) ;

      Result := True ;
    end
  end ;

  procedure T_ContourItem.makeRoomFor(
    const _newPoints : Integer
  ) ;
  begin
    if ( _newPoints > FMaxPoints ) then begin
      FMaxPoints := _newPoints * 2 + 50 ;
      SetLength( FX, FMaxPoints ) ;
      SetLength( FY, FMaxPoints ) ;
    end
  end;

  function T_ContourItem.distanceSqr (
    const _x0 : Double ;
    const _y0 : Double ;
    const _x1 : Double ;
    const _y1 : Double
  ) : Double ;
  var
    dx, dy : Double ;
  begin
    dx := _x0 - _x1 ;
    dy := _y0 - _y1 ;

    Result := dx * dx + dy * dy ;
  end ;

  function T_ContourItem.mergeCase(
    const _ax0 : Double ;
    const _ay0 : Double ;
    const _ax1 : Double ;
    const _ay1 : Double ;
    const _bx0 : Double ;
    const _by0 : Double ;
    const _bx1 : Double ;
    const _by1 : Double
  ) : Integer ;
  var
    jds  : Double ;
    cs   : Integer ;
    dmin : Double ;
    dd   : Double ;
  begin
    // Calculate all possible distances and choose the closest if less than JOIN_DIST.
    jds := T_CONTOUR_CONST.JOIN_DIST * T_CONTOUR_CONST.JOIN_DIST ;

    // Case 1 e-b.
    cs := 1 ;
    dmin := distanceSqr( _ax1, _ay1, _bx0, _by0 ) ;
    // Case 2 b-e.
    dd := distanceSqr( _ax0, _ay0, _bx1, _by1 ) ;
    if ( dd < dmin ) then begin
      dmin := dd ;
      cs   := 2 ;
    end ;
    // Case 3 e-e.
    dd := distanceSqr( _ax1, _ay1, _bx1, _by1 ) ;
    if ( dd < dmin ) then begin
      dmin := dd ;
      cs   := 3 ;
    end ;
    // Case 4 b-b.
    dd := distanceSqr( _ax0, _ay0, _bx0, _by0 ) ;
    if ( dd < dmin ) then begin
      dmin := dd ;
      cs   := 4 ;
    end ;
    if ( dmin > jds ) then
      cs := 0 ;

    Result := cs ;
  end ;

  function T_ContourItem.Merge(
    const _other : T_ContourItem
  ) : Boolean ;
  var
    i  : Integer ;
    mc : Integer ;
  begin
    Result := False ;

    assert( _other <> nil ) ;
    if ( _other.FLevel <> FLevel ) then exit ;

    // Try to matching up with one of the ends, and insert.

    mc := mergeCase( FX[0]                      , FY[0],
                     FX[FPoints-1]              , FY[FPoints-1],
                     _other.FX[0]               , _other.FY[0],
                     _other.FX[_other.FPoints-1], _other.FY[_other.FPoints-1]
                   ) ;

    if mc = 1 then begin // Case 1 e-b.
      makeRoomFor( FPoints + _other.FPoints - 1 ) ;

      GisCopyMemoryEx( _other.FX, 1, FX, FPoints, _other.FPoints-1 ) ;
      GisCopyMemoryEx( _other.FY, 1, FY, FPoints, _other.FPoints-1 ) ;

      inc( FPoints, _other.FPoints - 1 ) ;
      FUsed  := True ;
      FTailX := FX[ FPoints-1 ] ;
      Result := True ;
    end
    else if mc = 2 then begin // Case 2 b-e.
      makeRoomFor( FPoints + _other.FPoints - 1 ) ;

      GisCopyMemoryEx( FX, 0, FX, _other.FPoints - 1, FPoints ) ;
      GisCopyMemoryEx( FY, 0, FY, _other.FPoints - 1, FPoints ) ;
      GisCopyMemoryEx( _other.FX, 0, FX, 0, _other.FPoints - 1 ) ;
      GisCopyMemoryEx( _other.FY, 0, FY, 0, _other.FPoints - 1 ) ;

      inc( FPoints, _other.FPoints - 1 ) ;
      FUsed  := True ;
      FTailX := FX[ FPoints-1 ] ;
      Result := True ;
    end
    else if mc = 3 then begin // Case 3 e-e.
      makeRoomFor( FPoints + _other.FPoints - 1 ) ;

      i := 0 ;
      while i < _other.FPoints-1 do begin
        FX[ i+FPoints ] := _other.FX[ _other.FPoints-i-2 ] ;
        FY[ i+FPoints ] := _other.FY[ _other.FPoints-i-2 ] ;
        inc( i ) ;
      end ;

      inc( FPoints, _other.FPoints - 1 ) ;
      FUsed  := True ;
      FTailX := FX[ FPoints-1 ] ;
      Result := True ;
    end
    else if mc = 4 then begin // Case 3 b-b.
      makeRoomFor( FPoints + _other.FPoints - 1 ) ;

      GisCopyMemoryEx( FX, 0, FX, _other.FPoints - 1, FPoints ) ;
      GisCopyMemoryEx( FY, 0, FY, _other.FPoints - 1, FPoints ) ;

      i := 0 ;
      while i < _other.FPoints - 1 do begin
        FX[ i ] := _other.FX[ _other.FPoints - i - 1 ] ;
        FY[ i ] := _other.FY[ _other.FPoints - i - 1 ] ;
        inc( i ) ;
      end ;

      inc( FPoints, _other.FPoints - 1 ) ;
      FUsed  := True ;
      FTailX := FX[ FPoints-1 ] ;
      Result := True ;
    end ;
  end ;

  procedure T_ContourItem.PrepareEjection ;
  var
    i      : Integer ;
    np     : Integer ;
    dftemp : Double ;
  begin
    // If left side is the high side, then reverse to get curve normal pointing
    //  downwards
    if FLeftIsHigh then begin
      np := FPoints div 2 ;
      i  := 0 ;
      // Reverse the arrays
      while i < np do begin
        dftemp   := FX[i];
        FX[i] := FX[ FPoints - i - 1];
        FX[ FPoints - i - 1] := dftemp;

        dftemp := FY[i];
        FY[i] := FY[ FPoints - i - 1];
        FY[ FPoints - i - 1] := dftemp;
        inc( i ) ;
      end ;
    end ;
  end ;

//==============================================================================
// T_ContourLevel
//==============================================================================

  constructor T_ContourLevel.Create(
    const _level : Double
  ) ;
  begin
    inherited Create ;

    FLevel      := _level ;
    FEntryMax   := 0 ;
    FEntryCount := 0 ;
    FContours   := TList<T_ContourItem>.Create ;
  end ;

  procedure T_ContourLevel.doDestroy ;
  var
    i : Integer ;
  begin
    {$IFNDEF NEXTGEN}
    for i := 0 to FEntryCount - 1 do
      FreeObjectNotNil( FContours[ i ] ) ;
    {$ENDIF}

    FreeObject( FContours ) ;

    inherited ;
  end ;

  function T_ContourLevel.fget_GetContour(
    const _i : Integer
  ) : T_ContourItem ;
  begin
    Result := FContours[ _i ] ;
  end;

  procedure T_ContourLevel.AdjustContour(
    const _changed : Integer
  ) ;
  var
    potemp   : T_ContourItem ;
    ichanged : Integer ;
  begin
    ichanged := _changed ;
    while ( ichanged > 0 ) and
          ( FContours[ichanged].FTailX < FContours[ichanged-1].FTailX ) do
    begin
      potemp                := FContours[ichanged  ] ;
      FContours[ichanged]   := FContours[ichanged-1] ;
      FContours[ichanged-1] := potemp ;
      dec( ichanged ) ;
    end ;

    while ( ichanged < FEntryCount-1 ) and
          ( FContours[ichanged].FTailX > FContours[ichanged+1].FTailX ) do
    begin
      potemp                := FContours[ichanged  ] ;
      FContours[ichanged]   := FContours[ichanged+1] ;
      FContours[ichanged+1] := potemp ;
      inc( ichanged ) ;
    end ;
  end ;

  procedure T_ContourLevel.RemoveContour(
    const _target : Integer
  ) ;
  begin
    {$IFDEF OXYGENE}
      FContours.RemoveAt( _target ) ;
    {$ELSE}
      FContours.Delete( _target ) ;
    {$ENDIF}
    dec( FEntryCount ) ;
  end ;

  function T_ContourLevel.FindContour(
    const _fX  : Double ;
    const _fY  : Double
   ) : Integer ;
  var
    nstart    : Integer ;
    nend      : Integer ;
    nmiddle   : Integer ;
    dfMiddleX : Double  ;
  begin
    Result := -1 ;
    nstart := 0 ;
    nend   := FEntryCount-1 ;

    while ( nend >= nstart ) do begin
      nmiddle := (nend + nstart) div 2 ;

      dfMiddleX := FContours[nmiddle].FTailX ;

      if ( dfMiddleX < _fX ) then
        nstart := nmiddle + 1
      else if ( dfMiddleX > _fX ) then
        nend := nmiddle - 1
      else begin
        while ( nmiddle > 0 ) and
              ( Abs( FContours[nmiddle].FTailX - _fX ) < T_CONTOUR_CONST.JOIN_DIST ) do
          dec( nmiddle ) ;

        while ( nmiddle < FEntryCount ) and
              ( Abs( FContours[nmiddle].FTailX - _fX ) < T_CONTOUR_CONST.JOIN_DIST ) do begin

          if ( Abs( FContours[nmiddle].FY[ FContours[nmiddle].FPoints-1 ] - _fY)
              < T_CONTOUR_CONST.JOIN_DIST ) then begin
            Result := nmiddle ;
            exit ;
          end;
          inc( nmiddle ) ;
        end ;

        Result := -1 ;
        exit ;
      end ;
    end ;
  end ;

  function T_ContourLevel.InsertContour(
    const _newContour : T_ContourItem
  ) : Integer ;
  var
    nstart    : Integer ;
    nend      : Integer ;
    nmiddle   : Integer ;
    dfMiddleX : Double  ;
  begin
    nstart := 0 ;
    nend   := FEntryCount-1 ;

    // Find where to insert by binary search.
    while ( nend >= nstart ) do begin
      nmiddle := (nend + nstart) div 2 ;

      dfMiddleX := FContours[nmiddle].FTailX ;

      if ( dfMiddleX < _newContour.FLevel ) then
        nstart := nmiddle + 1
      else if ( dfMiddleX > _newContour.FLevel ) then
        nend := nmiddle - 1
      else begin
        nend := nmiddle - 1 ;
        break ;
      end ;
    end ;

    // Insert the new contour at the appropriate location.
    FContours.Insert( nend+1, _newContour ) ;
    inc( FEntryCount ) ;

    Result := nend+1 ;
  end ;

//==============================================================================
// T_ContourGenerator
//==============================================================================

  constructor T_ContourGenerator.Create(
    const _width      : Integer ;
    const _height     : Integer ;
    const _writer     : T_OnContourWrite ;
    const _writerInfo : T_ContourWriterInfo
   ) ;
  begin
    inherited Create ;

    FWidth  := _width ;
    FHeight := _height ;

    SetLength( FLastLine, FWidth ) ;
    SetLength( FThisLine, FWidth ) ;

    FWriter          := _writer ;
    FWriterInfo      := _writerInfo ;
    FLine            := -1 ;
    FNoDataActive    := False ;
    FNoDataValue     := T_CONTOUR_CONST.NODATA_VALUE ;
    FContourInterval := T_CONTOUR_CONST.CONTOUR_INTERVAL ;
    FContourOffset   := T_CONTOUR_CONST.CONTOUR_BASE ;
    FLevelMax        := -GIS_MAX_DOUBLE ;
    FLevelCount      := 0 ;
    FLevels          := TList<T_ContourLevel>.Create ;
    FFixedLevels     := False ;
  end ;

  procedure T_ContourGenerator.doDestroy ;
  var
    i : Integer ;
  begin
    {$IFNDEF NEXTGEN}
    for i := 0 to FLevelCount-1 do
      FreeObjectNotNil( FLevels[i] ) ;
    {$ENDIF}

    FreeObject( FLevels ) ;
    FLastLine := nil ;
    FThisLine := nil ;

    inherited ;
  end ;

  procedure T_ContourGenerator.SetContourLevels(
    const _contourInterval : Double ;
    const _contourOffset   : Double
   ) ;
  begin
    if _contourInterval > 0 then
      FContourInterval := _contourInterval ;

    FContourOffset := _contourOffset ;
  end;

  procedure T_ContourGenerator.SetFixedLevels(
    const _fixedLevelCount : Integer ;
    const _fixedLevels     : TGIS_DoubleArray
   ) ;
  var
    i    : Integer ;
    dmin : Double ;
  begin
    FFixedLevels := True ;
    for i := 0 to _fixedLevelCount-1 do
      findLevel( _fixedLevels[i] ) ;

    dmin := GIS_MAX_DOUBLE ;
    for i := 0 to FLevels.Count - 2 do
      dmin := Min( dmin, Abs( FLevels[i+1].Level - FLevels[i].Level ) ) ;

    FContourInterval := dmin ;
  end ;

  procedure T_ContourGenerator.SetNoData(
    const _noDataValue : Double
  ) ;
  begin
    FNoDataActive := True ;
    FNoDataValue  := _noDataValue ;
  end ;

  function T_ContourGenerator.processPixel(
    const _iPixel : Integer
  ) : Boolean ;
  var
    dfupleft   : Double ;
    dfupright  : Double ;
    dfloleft   : Double ;
    dfloright  : Double ;
    bsubdivide : Boolean ;
    ngoodcount : Integer ;
    dfasum     : Double ;
    dfcenter   : Double ;
    dftop      : Double ;
    dfright    : Double ;
    dfleft     : Double ;
    dfbottom   : Double ;
    err        : Boolean ;
  begin
    bsubdivide := False ;

    // Collect the four corner pixel values. Value left or right of the scanline
    // are taken from the nearest pixel on the scanline itself.

    dfupleft  := FLastLine[ Max(0,_iPixel-1) ] ;
    dfupright := FLastLine[ Min(FWidth-1,_iPixel) ] ;

    dfloleft  := FThisLine[ Max(0,_iPixel-1) ] ;
    dfloright := FThisLine[ Min(FWidth-1,_iPixel) ] ;

    // Check if we have any NoData values.
    if ( FNoDataActive ) and
       ( ( dfupleft  = FNoDataValue ) or
         ( dfloleft  = FNoDataValue ) or
         ( dfloright = FNoDataValue ) or
         ( dfupright = FNoDataValue ) ) then
      bsubdivide := True ;

    // Check if we have any NoData, if so, go to a special case of code.
    if ( ( _iPixel > 0 ) and ( _iPixel < FWidth ) and
         ( FLine > 0  ) and ( FLine < FHeight ) and not bsubdivide ) then begin

      Result :=  processRect( dfupleft,  _iPixel - 0.5, FLine - 0.5,
                              dfloleft,  _iPixel - 0.5, FLine + 0.5,
                              dfloright, _iPixel + 0.5, FLine + 0.5,
                              dfupright, _iPixel + 0.5, FLine - 0.5
                             ) ;
      exit ;
    end ;

    // Prepare subdivisions.
    ngoodcount  := 0 ;
    dfasum      := 0.0 ;

    if ( dfupleft <> FNoDataValue ) then begin
      dfasum := dfasum + dfupleft ;
      inc( ngoodcount ) ;
    end ;

    if ( dfloleft <> FNoDataValue ) then begin
      dfasum := dfasum + dfloleft ;
      inc( ngoodcount ) ;
    end ;

    if ( dfloright <> FNoDataValue ) then begin
      dfasum := dfasum + dfloright ;
      inc( ngoodcount ) ;
    end ;

    if ( dfupright <> FNoDataValue ) then begin
      dfasum := dfasum + dfupright ;
      inc( ngoodcount ) ;
    end ;

    if ( ngoodcount = 0.0 )  then begin
      Result := False ;
      exit ;
    end ;

    dfcenter := dfasum / ngoodcount ;

    if ( dfupleft <> FNoDataValue ) then begin

      if ( dfupright <> FNoDataValue ) then
        dftop := (dfupleft + dfupright) / 2.0
      else
        dftop := dfupleft;

      if ( dfloleft <> FNoDataValue ) then
        dfleft := (dfupleft + dfloleft) / 2.0
      else
        dfleft := dfupleft ;
    end
    else begin
      dftop  := dfupright ;
      dfleft := dfloleft ;
    end ;

    if ( dfloright <> FNoDataValue ) then begin

      if ( dfupright <> FNoDataValue ) then
        dfright := (dfloright + dfupright) / 2.0
      else
        dfright := dfloright ;

      if ( dfloleft <> FNoDataValue ) then
        dfbottom := (dfloright + dfloleft) / 2.0
      else
        dfbottom := dfloright ;
    end
    else begin
      dfbottom := dfloleft ;
      dfright  := dfupright ;
    end ;

    // Process any quadrants that aren't "NoData" anchored.
    err := False ;

    if ( dfupleft <> FNoDataValue ) and ( _iPixel > 0 ) and ( FLine > 0 )  then
      err := processRect( dfupleft, _iPixel - 0.5, FLine - 0.5,
                          dfleft,   _iPixel - 0.5, FLine,
                          dfcenter, _iPixel      , FLine,
                          dftop,    _iPixel      , FLine - 0.5
                        ) ;

    if ( dfloleft <> FNoDataValue ) and ( err = False ) and
       ( _iPixel > 0 ) and ( FLine < FHeight ) then
      err := processRect( dfleft,   _iPixel - 0.5, FLine,
                          dfloleft, _iPixel - 0.5, FLine + 0.5,
                          dfbottom, _iPixel      , FLine + 0.5,
                          dfcenter, _iPixel      , FLine
                        ) ;

    if ( dfloright <> FNoDataValue ) and ( _iPixel < FWidth ) and ( FLine < FHeight ) then
      err := processRect( dfcenter,  _iPixel      , FLine,
                          dfbottom,  _iPixel      , FLine + 0.5,
                          dfloright, _iPixel + 0.5, FLine + 0.5,
                          dfright,   _iPixel + 0.5, FLine
                        ) ;

    if ( dfupright <> FNoDataValue ) and ( _iPixel < FWidth ) and ( FLine > 0 ) then
      err := processRect( dftop,     _iPixel      , FLine - 0.5,
                          dfcenter,  _iPixel      , FLine,
                          dfright,   _iPixel + 0.5, FLine,
                          dfupright, _iPixel + 0.5, FLine - 0.5
                        ) ;
    Result := err ;
  end ;

  function T_ContourGenerator.processRect(
    const _fUpLeft   : Double ;
    const _fUpLeftX  : Double ;
    const _fUpLeftY  : Double ;
    const _fLoLeft   : Double ;
    const _fLoLeftX  : Double ;
    const _fLoLeftY  : Double ;
    const _fLoRight  : Double ;
    const _fLoRightX : Double ;
    const _fLoRightY : Double ;
    const _fUpRight  : Double ;
    const _fUpRightX : Double ;
    const _fUpRightY : Double
  ) : Boolean ;
  var
    istartlevel,
    iendlevel     : Integer ;
    nstart, nend,
    nmiddle       : Integer ;
    dfmin, dfmax  : Double ;
    dfmiddlelevel : Double ;
    dflevel       : Double ;
    ilevel        : Integer ;
    npoints       : Integer ;
    nPoints1      : Integer ;
    nPoints2      : Integer ;
    nPoints3      : Integer ;
    adfx          : TGIS_SingleArray ;
    adfy          : TGIS_SingleArray ;
    err           : Boolean ;
  begin
    // Identify the range of elevations over this rect.
    dfmin := Min( Min( _fUpLeft,_fUpRight ), Min(_fLoLeft,_fLoRight) ) ;
    dfmax := Max( Max( _fUpLeft,_fUpRight ), Max(_fLoLeft,_fLoRight) ) ;

    SetLength( adfx, 4 ) ;
    SetLength( adfy, 4 ) ;

    // Compute the set of levels to compute contours for.
    // If we are using fixed levels, then find the min/max in the levels table.
    if ( FFixedLevels ) then begin

      nstart      := 0 ;
      nend        := FLevelCount-1 ;
      istartlevel := -1 ;

      while ( nstart <= nend ) do begin

        nmiddle := (nend + nstart) div 2 ;

        dfmiddlelevel := FLevels[nmiddle].Level ;

        if ( dfmiddlelevel < dfmin ) then
          nstart := nmiddle + 1
        else if ( dfmiddlelevel > dfmin ) then
          nend := nmiddle - 1
        else begin
          istartlevel := nmiddle ;
          break ;
        end ;
      end ;

      if ( istartlevel = -1 ) then
        istartlevel := nend + 1 ;

      iendlevel := istartlevel ;
      while ( iendlevel < FLevelCount-1 ) and
            ( FLevels[iendlevel+1].Level < dfmax ) do
        inc( iendlevel ) ;

      if ( istartlevel >= FLevelCount ) then begin
        Result := False ;
        exit ;
      end ;

      assert( (istartlevel >= 0) and (istartlevel < FLevelCount) ) ;
      assert( (iendlevel   >= 0) and (iendlevel   < FLevelCount) ) ;

      if iendlevel > 0 then
        FContourInterval := FLevels[iendlevel].Level - FLevels[iendlevel-1].Level ;
    end
    else begin
      // Otherwise figure out the start and end using the base and offset.
      istartlevel := CeilS((dfmin - FContourOffset) / FContourInterval) ;
      iendlevel   := FloorS((dfmax - FContourOffset) / FContourInterval) ;
    end ;

    if ( istartlevel > iendlevel ) then begin
      Result := False ;
      exit ;
    end ;

    // Loop over them.
    for ilevel := istartlevel to iendlevel do begin
      if ( FFixedLevels ) then
        dflevel := FLevels[ilevel].Level
      else
        dflevel := ilevel * FContourInterval + FContourOffset ;

      npoints  := 0;

      // Logs how many points we have af left + bottom, and left + bottom + right.
      intersect( _fUpLeft, _fUpLeftX, _fUpLeftY,
                 _fLoLeft, _fLoLeftX, _fLoLeftY,
                 _fLoRight, dflevel, npoints, adfx, adfy
                ) ;
      nPoints1 := npoints;
      intersect( _fLoLeft,  _fLoLeftX,  _fLoLeftY,
                 _fLoRight, _fLoRightX, _fLoRightY,
                 _fUpRight, dflevel, npoints, adfx, adfy
                ) ;
      nPoints2 := npoints;
      intersect( _fLoRight, _fLoRightX, _fLoRightY,
                 _fUpRight, _fUpRightX, _fUpRightY,
                 _fUpLeft,  dflevel, npoints, adfx, adfy
                ) ;
      nPoints3 := npoints;
      intersect( _fUpRight, _fUpRightX, _fUpRightY,
                 _fUpLeft,  _fUpLeftX,  _fUpLeftY,
                 _fLoLeft, dflevel, npoints, adfx, adfy
                ) ;
      err := False ;

      if ( npoints >= 2 ) then begin
        if ( nPoints1 = 1 ) and ( nPoints2 = 2) then begin
          // left + bottom
          err := addSegment( dflevel,
                              adfx[0], adfy[0], adfx[1], adfy[1],
                              _fUpRight > _fLoLeft
                             ) ;
        end
        else if ( nPoints1 = 1 ) and ( nPoints3 = 2 ) then begin
          // left + right
          err := addSegment( dflevel,
                              adfx[0], adfy[0], adfx[1], adfy[1],
                              _fUpLeft > _fLoRight
                             ) ;
        end
        else if ( nPoints1 = 1 ) and ( npoints = 2 ) then begin
          // left + top
          // Do not do vertical contours on the left, due to symmetry
          if not ( (_fUpLeft = dflevel) and (_fLoLeft = dflevel) ) then
            err := addSegment( dflevel,
                                adfx[0], adfy[0], adfx[1], adfy[1],
                                _fUpLeft > _fLoRight
                               ) ;
        end
        else if ( nPoints2 = 1 ) and ( nPoints3 = 2) then begin
          // bottom + right
          err := addSegment( dflevel,
                              adfx[0], adfy[0], adfx[1], adfy[1],
                              _fUpLeft > _fLoRight
                             ) ;
        end
        else if ( nPoints2 = 1 ) and ( npoints = 2 ) then begin
          // bottom + top
          err := addSegment( dflevel,
                              adfx[0], adfy[0], adfx[1], adfy[1],
                              _fLoLeft > _fUpRight
                             ) ;
        end
        else if ( nPoints3 = 1 ) and ( npoints = 2 ) then begin
          // right + top
          // Do not do horizontal contours on upside, due to symmetry
          if not ( (_fUpRight = dflevel) and (_fUpLeft = dflevel) ) then
            err := addSegment( dflevel,
                                adfx[0], adfy[0], adfx[1], adfy[1],
                                _fLoLeft > _fUpRight
                               ) ;
        end
        else begin
          assert(False, 'Contour state not implemented') ;
        end ;

        if ( err <> False ) then begin
          Result := err ;
          exit ;
        end ;
      end ;

      if ( npoints = 4 ) then begin
        // Do not do horizontal contours on upside, due to symmetry
        if not ( (_fUpRight = dflevel) and (_fUpLeft = dflevel) ) then begin
          // If we get here, we know the first was left+bottom, so we are at
          // right+top, therefore "left is high" if low-left is larger than up-right.
          // We do not do a diagonal check here as we are dealing with
          // a saddle point.

          err := addSegment( dflevel, adfx[2], adfy[2], adfx[3], adfy[3],
                              ( _fLoRight > _fUpRight)
                             ) ;
          if ( err <> False ) then begin
            Result := err ;
            exit ;
          end ;
        end ;
      end ;
    end ;

    Result :=  False ;
  end ;

  procedure T_ContourGenerator.intersect(
     const _val1    : Double  ;
     const _fX1     : Double  ;
     const _fY1     : Double  ;
     const _val2    : Double  ;
     const _fX2     : Double  ;
     const _fY2     : Double  ;
     const _fNext   : Double  ;
     const _level   : Double  ;
       var _points  : Integer ;
     const _adfX    : TGIS_SingleArray ;
     const _adfY    : TGIS_SingleArray
    ) ;
  var
    dfratio : Double ;
  begin
    if ( _val1 < _level ) and ( _val2 >= _level ) then begin

      dfratio := (_level - _val1) / (_val2 - _val1) ;

      _adfX[_points] := _fX1 * (1.0 - dfratio) + _fX2 * dfratio ;
      _adfY[_points] := _fY1 * (1.0 - dfratio) + _fY2 * dfratio ;
      inc( _points ) ;
    end
    else if ( _val1 > _level ) and ( _val2 <= _level ) then begin

      dfratio := (_level - _val2) / (_val1 - _val2) ;

      _adfX[_points] := _fX2 * (1.0 - dfratio) + _fX1 * dfratio ;
      _adfY[_points] := _fY2 * (1.0 - dfratio) + _fY1 * dfratio ;
      inc( _points ) ;
    end
    else if ( _val1 = _level ) and ( _val2 = _level ) and ( _fNext <> _level ) then begin

      _adfX[_points] := _fX2 ;
      _adfY[_points] := _fY2 ;
      inc( _points ) ;
    end ;
  end ;

  function T_ContourGenerator.addSegment(
    const _level    : Double ;
    const _fX1      : Double ;
    const _fY1      : Double ;
    const _fX2      : Double ;
    const _fY2      : Double ;
    const _leftHigh : Boolean
   ) : Boolean ;
  var
    polevel   : T_ContourLevel ;
    potarget  : T_ContourItem ;
    itarget   : Integer ;
  begin
    polevel := findLevel( _level ) ;
    assert( polevel <> nil ) ;

    // Check all active contours for any that this might attach to. Eventually
    // this should be recoded to find the contours of the correct level more
    // efficiently.

    if ( _fY1 < _fY2 ) then
      itarget := polevel.FindContour( _fX1, _fY1 )
    else
      itarget := polevel.FindContour( _fX2, _fY2 );

    if ( itarget <> -1 ) then begin
      potarget := polevel.Contour[ itarget ] ;

      potarget.addSegment( _fX1, _fY1, _fX2, _fY2,_leftHigh ) ;
      polevel.AdjustContour( itarget ) ;

      Result := False ;
      exit ;
    end ;

    // No existing contour found, lets create a new one.
    potarget := T_ContourItem.Create( _level ) ;
    potarget.addSegment( _fX1, _fY1, _fX2, _fY2,_leftHigh ) ;
    polevel.InsertContour( potarget ) ;

    Result := False ;
  end ;

  function T_ContourGenerator.findLevel(
    const _level : Double
   ) : T_ContourLevel ;
  var
    nstart, nend, nmiddle : Integer ;
    dfmiddlelevel : Double ;
    polevel : T_ContourLevel ;
  begin
    nstart := 0 ;
    nend   := FLevelCount-1 ;

    // Binary search to find the requested level.
    while ( nstart <= nend ) do begin

      nmiddle := (nend + nstart) div 2 ;

      dfmiddlelevel := FLevels[nmiddle].Level ;

      if ( dfmiddlelevel < _level ) then
        nstart := nmiddle + 1
      else if ( dfmiddlelevel > _level ) then
        nend := nmiddle - 1
      else begin
        Result := FLevels[nmiddle] ;
        exit ;
      end;
    end ;

    // Didn't find the level, create a new one and insert it in order.
    polevel := T_ContourLevel.Create( _level ) ;

    FLevels.Insert( nend+1, polevel ) ;
    inc( FLevelCount ) ;

    if polevel.Level > FLevelMax then
      FLevelMax := polevel.Level ;

    Result := polevel ;
  end ;

  function T_ContourGenerator.FeedLine(
    const _scanline : TGIS_SingleArray
  ) : Boolean ;
  var
    templine : TGIS_SingleArray ;
    ipixel   : Integer ;
    dflevel  : Double ;
    ilevel   : Integer ;
    icontour : Integer ;
    polevel  : T_ContourLevel ;
    err      : Boolean ;
  begin
    // Switch current line to "lastline" slot, and copy new data into new "this line".
    templine  := FLastLine ;
    FLastLine := FThisLine ;
    FThisLine := templine ;

    // If this is the end of the lines (NULL passed in), copy the last line.
    if ( _scanline = nil ) then
      GisCopyMemoryEx( FLastLine, 0, FThisLine, 0, FWidth )
    else
      GisCopyMemoryEx( _scanline, 0, FThisLine, 0, FWidth ) ;

    // Perturb any values that occur exactly on level boundaries.
    for ipixel := 0 to FWidth-1 do begin

      if ( FNoDataActive and ( FThisLine[ipixel] = FNoDataValue ) ) then
        continue ;

      dflevel := (FThisLine[ipixel] - FContourOffset) / FContourInterval ;

      if ( dflevel - TruncS( dflevel ) = 0 ) then
        FThisLine[ipixel] := FThisLine[ipixel] + FContourInterval * T_CONTOUR_CONST.FUDGE_EXACT ;
    end ;

    // If this is the first line we need to initialize the previous line from the
    // first line of data.
    if ( FLine = -1 ) then begin
      GisCopyMemoryEx( FThisLine, 0, FLastLine, 0, FWidth ) ;
      FLine := 0 ;
    end ;

    // Clear the recently used flags on the contours so we can check later which
    // ones were touched for this scanline.
    for ilevel := 0 to FLevelCount-1 do begin

      polevel := FLevels[ilevel] ;

      for icontour := 0 to polevel.ContourCount-1 do
        polevel.Contour[ icontour ].FUsed := False ;
    end ;

    // Process each pixel.
    for ipixel := 0 to FWidth do begin

      err := processPixel( ipixel ) ;
      if ( err <> False ) then begin
        Result := err ;
        exit ;
      end;
    end ;

    // eject any pending contours.
    err := ejectContours( _scanline <> nil ) ;

    inc( FLine ) ;

    if ( FLine = FHeight ) and ( err = False ) then
      Result := FeedLine( nil )
    else
      Result := err;
  end ;

  function T_ContourGenerator.ejectContours(
    const _onlyUnused : Boolean
  ) : Boolean ;
  var
    ilevel    : Integer ;
    err       : Boolean ;
    polevel   : T_ContourLevel ;
    icontour  : Integer ;
    ic2       : Integer ;
    potarget  : T_ContourItem ;
    poother   : T_ContourItem ;
  begin
    err := False ;

    // Process all contours of all levels that match our criteria.
    ilevel := 0 ;
    while ( ilevel < FLevelCount ) and ( err = False ) do begin

      polevel := FLevels[ilevel] ;

      icontour := 0 ;
      while ( icontour < polevel.ContourCount ) and ( err = False ) do begin

        potarget := polevel.Contour[ icontour ] ;

        if ( _onlyUnused and potarget.FUsed ) then begin
          inc( icontour ) ;
          continue ;
        end ;

        polevel.RemoveContour( icontour ) ;

        // Try to find another contour we can merge with in this level.
        ic2 := 0 ;
        while ic2 < polevel.ContourCount do begin
          poother := polevel.Contour[ ic2 ] ;

          if poother.Merge( potarget ) then
            break ;
          inc( ic2 ) ;
        end ;

         // If we didn't merge it, then eject (write) it out.
        if ( ic2 = polevel.ContourCount ) then begin

          if assigned( FWriter ) then begin
             // If direction is wrong, then reverse before ejecting.
            potarget.PrepareEjection ;
            err := FWriter( potarget.FLevel,
                            potarget.FPoints,
                            potarget.FX,
                            potarget.FY,
                            FWriterInfo
                          ) ;
          end;
        end ;

        FreeObject( potarget ) ;
      end ;
      inc( ilevel ) ;
    end ;

    Result := err ;
  end ;

  // Write contour method.
  function GISContourWriter(
    const _level  : Double ;
    const _points : Integer ;
    const _adfX   : TGIS_SingleArray ;
    const _adfY   : TGIS_SingleArray ;
    const _info   : T_ContourWriterInfo
   ) : Boolean ;
  var
    shp            : TGIS_Shape ;
    ipoint         : Integer ;
    ptg            : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    lv             : TGIS_LayerVector ;
    contour_length : Double ;
  begin
    assert( _info.VLayer is TGIS_LayerVector ) ;

    lv := TGIS_LayerVector( _info.VLayer ) ;

    shp := nil ;
    if TGIS_DimensionType.XYZM in lv.SupportedDimensions then
      shp := TGIS_LayerVector( _info.VLayer ).CreateShape(
               TGIS_ShapeType.Arc,
               TGIS_DimensionType.XYZM
             )
    else
    if TGIS_DimensionType.XYZ in lv.SupportedDimensions then
      shp := TGIS_LayerVector( _info.VLayer ).CreateShape(
               TGIS_ShapeType.Arc,
               TGIS_DimensionType.XYZ
             )
    else
    if TGIS_DimensionType.XY in lv.SupportedDimensions then
      shp := TGIS_LayerVector( _info.VLayer ).CreateShape(
               TGIS_ShapeType.Arc,
               TGIS_DimensionType.XY
             ) ;

    if not assigned( shp ) then begin
      Result := True ;
      exit ;
    end ;

    shp.Reset ;
    shp.Lock( TGIS_Lock.Projection ) ;
    shp.AddPart ;
    for ipoint := 0 to _points-1 do begin
      ptg.X := _info.GeoTransform[0] +
               _info.GeoTransform[1] * _adfX[ipoint] +
               _info.GeoTransform[2] * _adfY[ipoint] ;
      ptg.Y := _info.GeoTransform[3] +
               _info.GeoTransform[4] * _adfX[ipoint] +
               _info.GeoTransform[5] * _adfY[ipoint] ;
      ptg.Z := _level ;
      ptg.M := 0 ;

      shp.AddPoint3D( ptg ) ;
    end ;
    shp.Unlock ;

    if _info.Smooth then
      shp.Smooth( 5, False ) ;

    contour_length := shp.LengthCS ;
    if ( 0 < contour_length ) and ( contour_length < _info.MinSize ) then begin
      shp.Delete ;
    end
    else begin
      if not IsStringEmpty( _info.ElevField ) then
        shp.SetField( _info.ElevField, _level ) ;
    end;

    Result := False ;
  end ;

//==============================================================================
// TGIS_ContourGenerator
//==============================================================================

  constructor TGIS_ContourGenerator.Create ;
  begin
    inherited ;

    FMode := TGIS_ContourGeneratorMode.Polylines ;

    FContourInterval := T_CONTOUR_CONST.CONTOUR_INTERVAL ;
    FContourBase := T_CONTOUR_CONST.CONTOUR_BASE ;
    FMinSize := T_CONTOUR_CONST.MIN_SIZE ;
    FCustomNoData := T_CONTOUR_CONST.CUSTOM_NODATA ;
    FSmoothen := T_CONTOUR_CONST.SMOOTHEN ;
    FSmoothFactor := T_CONTOUR_CONST.SMOOTH_FACTOR ;

    maxLevel := 0.0 ;
    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_ContourGenerator.doDestroy ;
  begin
    FreeObject( busyEventManager );

    inherited ;
  end ;

  function TGIS_ContourGenerator.fget_FixedLevelCount : Integer ;
  begin
    Result := length ( FFixedLevels ) ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_ContourGenerator.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_ContourGenerator.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_ContourGenerator.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_ContourGenerator.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  function TGIS_ContourGenerator.getNumberOfSteps : Integer ;
  begin
    case FMode of
      TGIS_ContourGeneratorMode.Polylines           : Result := 1 ;
      TGIS_ContourGeneratorMode.ClosedPolylines     : Result := 2 ;
      TGIS_ContourGeneratorMode.OverlappingPolygons : Result := 3 ;
      TGIS_ContourGeneratorMode.Polygons            : Result := 4 ;
    else                                              Result := 0 ;
    end ;
  end ;

  procedure TGIS_ContourGenerator.CalculateInterval(
    const _src : TGIS_LayerPixel
  ) ;
  begin
    CalculateInterval( _src, T_CONTOUR_CONST.NO_INTERVALS ) ;
  end ;

  procedure TGIS_ContourGenerator.CalculateInterval(
    const _src : TGIS_LayerPixel ;
    const _cnt : Integer
  ) ;
  var
    src : TGIS_LayerPixel ;
    cnt : Integer ;
  begin
    assert( _src <> nil ) ;
    assert( _src is TGIS_LayerPixel  ) ;

    src := _src as TGIS_LayerPixel ;
    assert( src.IsGridImage or src.IsNativeGridImage ) ;

    if _cnt < 1 then
      cnt := 1
    else
      cnt := _cnt ;

    ContourInterval := ( src.MaxHeight - src.MinHeight )/( cnt ) ;
    if ContourInterval = 0.0 then begin
      ContourInterval := 100.0 ;
      ContourBase := src.MinHeight ;
    end
    else
      ContourBase := src.MinHeight ;//+ ContourInterval ;
  end ;

  function TGIS_ContourGenerator.Generate(
    const _src    : TGIS_LayerPixel ;
    const _dst    : TGIS_LayerVector ;
    const _dstfld : String
  ) : Boolean ;
  begin
    Result := Generate( _src, _dst, _dstfld, '' ) ;
  end;

  function TGIS_ContourGenerator.Generate(
    const _src             : TGIS_LayerPixel ;
    const _dst             : TGIS_LayerVector ;
    const _dstfld          : String ;
    const _dstfld_interval : String
  ) : Boolean ;
  begin
    assert( _src <> nil ) ;
    assert( _src is TGIS_LayerPixel  ) ;
    assert( _dst is TGIS_LayerVector ) ;
    if not IsStringEmpty( _dstfld ) then
      assert( _dstfld <> _dstfld_interval ) ;

    _dst.Open ;

    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_CONTOUR_GENERATE ), getNumberOfSteps
    ) ;
    try
      case FMode of
        TGIS_ContourGeneratorMode.Polylines :
          Result := generatePolylines( _src, _dst, _dstfld ) ;
        TGIS_ContourGeneratorMode.ClosedPolylines :
          Result := generateClosedPolylines( _src, _dst, _dstfld ) ;
        TGIS_ContourGeneratorMode.OverlappingPolygons :
          Result := generateOverlappingPolygons( _src, _dst, _dstfld, _dstfld_interval ) ;
        TGIS_ContourGeneratorMode.Polygons :
          Result := generatePolygons( _src, _dst, _dstfld, _dstfld_interval ) ;
      else
        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
        Result := False ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end;
  end ;

  function TGIS_ContourGenerator.generateClosedPolylines(
    const _src    : TGIS_LayerPixel ;
    const _dst    : TGIS_LayerVector ;
    const _dstfld : String
  ) : Boolean ;
  var
    expanded_grid : TGIS_LayerPixel ;
    fill_nodata   : Boolean ;
    fill_val      : Single ;
  begin
    Result := False ;
    fill_nodata := True ;
    fill_val :=  _src.MinHeight - 1 ;
    try
      expanded_grid := expandGrid( _src, T_CONTOUR_CONST.RADIUS, fill_nodata, fill_val ) ;
      Result := generatePolylines( expanded_grid, _dst, _dstfld ) ;
    finally
      FreeObject( expanded_grid ) ;
    end;
  end ;

  function TGIS_ContourGenerator.generateOverlappingPolygons(
    const _src             : TGIS_LayerPixel ;
    const _dst             : TGIS_LayerVector ;
    const _dstfld          : String ;
    const _dstfld_interval : String
  ) : Boolean ;
  var
    overlapping : Boolean ;
  begin
    overlapping := True ;
    Result := generateContoursAndConvertToPolygons( _src, _dst, _dstfld, _dstfld_interval, overlapping ) ;
  end ;

  function TGIS_ContourGenerator.generatePolygons(
    const _src             : TGIS_LayerPixel ;
    const _dst             : TGIS_LayerVector ;
    const _dstfld          : String ;
    const _dstfld_interval : String
  ) : Boolean ;
  var
    overlapping   : Boolean ;
  begin
    overlapping := False ;
    Result := generateContoursAndConvertToPolygons( _src, _dst, _dstfld, _dstfld_interval, overlapping ) ;
  end;

  function TGIS_ContourGenerator.generateContoursAndConvertToPolygons(
    const _src             : TGIS_LayerPixel ;
    const _dst             : TGIS_LayerVector ;
    const _dstfld          : String ;
    const _dstfld_interval : String ;
    const _overlapping     : Boolean
  ) : Boolean ;
  const
    CONTOUR_NAME     = 'contour_arc' ;
  var
    contour_arc      : TGIS_LayerVector ;
    fld              : String ;
    fld_interval     : String ;
    delete_last_band : Boolean ;
    uids_to_delete   : TList< Int64 > ;

  begin
    Result := False ;

    if IsStringEmpty( _dstfld ) then
      fld := FIELD_VALUE
    else
      fld := _dstfld ;

    // field for contour intervals (max/min difference)
    if IsStringEmpty( _dstfld_interval ) then begin
      fld_interval := FIELD_INTERVAL ;
      _dst.AddField( fld_interval, TGIS_FieldType.Float, FIELD_WIDTH, FIELD_DECIMAL ) ;
    end
    else
      fld_interval := _dstfld_interval;

    // the algorithm collects Uids of unnecessary polygons
    uids_to_delete := TList< Int64 >.Create ;
    contour_arc := TGIS_LayerVector.Create ;
    try
      contour_arc.Name := CONTOUR_NAME ;
      contour_arc.CS := _src.CS ;
      contour_arc.Open ;
      contour_arc.DefaultDimension := TGIS_DimensionType.XYZM ;
      contour_arc.AddField( fld, TGIS_FieldType.Float, FIELD_WIDTH, FIELD_DECIMAL ) ;

      if not generateClosedPolylines( _src, contour_arc, fld ) then Exit ;

      contourCount := getContourCount ;  //? why global

      if not convertArcToPolygon(
        contour_arc,
        _dst,
        fld,
        fld_interval,
        uids_to_delete
      ) then exit ;

      // create polygons with rings
      if not _overlapping then begin
        delete_last_band :=
          ( FixedLevelCount > 0 ) and
          ( _src.MaxHeight > FFixedLevels[FixedLevelCount - 1] ) ;

        if not erasePolygons(
          _dst,
          fld,
          delete_last_band,
          uids_to_delete
        ) then exit ;
      end ;

      deleteUnnecessaryShapes( _dst, uids_to_delete  ) ;

      if not fillZMValue( _dst, fld, fld_interval ) then exit ;

      if IsStringEmpty( _dstfld ) then begin
        _dst.DeleteField( fld ) ;
      end ;

      if IsStringEmpty( _dstfld_interval ) then begin
        _dst.DeleteField( fld_interval ) ;
      end ;

      _dst.SaveData ;
      Result := True ;
    finally
      FreeObject( uids_to_delete );
      FreeObject( contour_arc ) ;
    end ;
  end ;

  function TGIS_ContourGenerator.getContourCount : Integer ;
  begin
    if FixedLevelCount = 0 then
      Result := FloorS(( maxLevel - FContourBase ) / FContourInterval ) + 1
    else
      Result := FixedLevelCount ;
  end;

  function TGIS_ContourGenerator.generatePolylines(
    const _src    : TGIS_LayerPixel ;
    const _dst    : TGIS_LayerVector ;
    const _dstfld : String
  ) : Boolean ;
  var
    ocwi         : T_ContourWriterInfo ;
    nxsize       : Integer ;
    nysize       : Integer ;
    ocg          : T_ContourGenerator ;
    fline        : Integer ;
    err          :  Boolean ;
    padfsl       : TGIS_SingleArray ;
    abrt         : Boolean ;
    hraster      : TGIS_LayerPixel ;
    to_read      : Integer ;
    h            : Integer ;
    j            : Integer ;
    plock        : TGIS_LayerPixelLock ;
    hextent      : TGIS_Extent ;
  begin
    hraster := _src as TGIS_LayerPixel ;
    assert( hraster.IsGridImage or hraster.IsNativeGridImage ) ;

    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_CONTOUR_POLYLINES ), hraster.BitHeight
    ) ;
    try
      ocwi.VLayer       := _dst ;
      ocwi.ElevField    := _dstfld ;
      ocwi.Smooth       := Smoothen ;
      ocwi.SmoothFactor := SmoothFactor ;

      if FMode in [TGIS_ContourGeneratorMode.Polylines, TGIS_ContourGeneratorMode.ClosedPolylines] then
        ocwi.MinSize := MinSize
      else
        ocwi.MinSize := 0 ;

      SetLength( ocwi.GeoTransform, 6 ) ;
      ocwi.GeoTransform[0] := hraster.Extent.XMin ;
      ocwi.GeoTransform[1] := ( hraster.Extent.XMax -
                                hraster.Extent.XMin ) / hraster.BitWidth ;
      ocwi.GeoTransform[2] := 0.0 ;
      ocwi.GeoTransform[3] := hraster.Extent.YMax ;
      ocwi.GeoTransform[4] := 0.0 ;
      ocwi.GeoTransform[5] := -( hraster.Extent.YMax -
                                 hraster.Extent.YMin ) / hraster.BitHeight ;

      nxsize := hraster.BitWidth ;
      nysize := hraster.BitHeight ;

      ocg := T_ContourGenerator.Create(
        nxsize, nysize,
        {$IFDEF OXYGENE}@{$ENDIF}GISContourWriter,
        ocwi
      ) ;
      try
        if ( FixedLevelCount > 0 ) then
          ocg.SetFixedLevels( FixedLevelCount, FixedLevels )
        else
          ocg.SetContourLevels( ContourInterval, ContourBase ) ;

        if CustomNoData then
          ocg.SetNoData( NoDataValue )
        else
          ocg.SetNoData( hraster.NoDataValue ) ;

        err := False ;
        abrt := False ;
        SetLength( padfsl, nxsize ) ;
        assert( padfsl <> nil ) ;

        fline := 0 ;
        hraster.SetCurrentFileScale( 1, 1 ) ;

        if (nxsize <> hraster.CellWidth) or (nysize <> hraster.CellHeight) then
        begin // Tiled case
          to_read := nysize ;

          if to_read >  hraster.CellHeight then
            h := hraster.CellHeight
          else
            h := to_read ;

          hextent := hraster.Extent ;
          hextent.YMin := hextent.YMax + h*ocwi.GeoTransform[5] ;

          while to_read > 0 do begin
            plock := hraster.LockPixels( hextent, nil, False ) ;
            try
              for j := plock.Bounds.Top to plock.Bounds.Bottom do
                err := ocg.FeedLine( TGIS_SingleArray(plock.Grid[j]) ) ;
            finally
              hraster.UnlockPixels( plock ) ;
            end ;

            fline := fline + h ;

            abrt := busyEventManager.PushEvent ;

            if ( err <> False ) or abrt then
              break ;

            hextent.YMax := hextent.YMin ;
            hextent.YMin := hextent.YMax + h*ocwi.GeoTransform[5] ;

            to_read := to_read - h ;
            if to_read >  hraster.CellHeight then
              h :=  hraster.CellHeight
            else
              h := to_read ;
          end ;
        end
        else begin
          while ( fline < nysize ) and ( err = False ) do begin

            hraster.ReadGridLine( padfsl, fline, 0, nxsize ) ;

            err := ocg.FeedLine( padfsl ) ;

            inc( fline ) ;

            abrt := busyEventManager.PushEvent ;

            if ( err <> False ) or abrt then
              break ;
          end ;
        end ;
        maxLevel := ocg.LevelMax ;
      finally
        FreeObject( ocg ) ;
      end ;
    finally
      busyEventManager.EndEvent
    end ;

    padfsl := nil ;
    Result := not err ;
  end ;

  // enlarge source grid layer with buffer defined by _radius
  // and assign _value to NoData
  function TGIS_ContourGenerator.expandGrid (
    const _src        : TGIS_LayerPixel ;
    const _radius     : Integer ;
    const _fillNoData : Boolean ;
          _val        : Single
  ) : TGIS_LayerPixel ;
  const
    GRID_NAME         = 'extended' ;
  var
    out_lp            : TGIS_LayerPixel ;
    out_rows          : Integer ;
    out_cols          : Integer ;
    ext               : TGIS_Extent ;
    cell_wdth         : Double ;
    cell_hgth         : Double ;
    out_lck           : TGIS_LayerPixelLock ;
    src_lck           : TGIS_LayerPixelLock ;
    nodata            : Single ;
    x                 : Integer ;
    y                 : Integer ;
    x_src             : Integer ;
    y_src             : Integer ;
  begin
    assert( _radius >= 1 ) ;

    nodata := _src.NoDataValue ;
    if not _fillNoData then
      _val := nodata ;

    // cell size
    cell_wdth := ( _src.Extent.XMax - _src.Extent.XMin ) / _src.BitWidth  ;
    cell_hgth := ( _src.Extent.YMax - _src.Extent.YMin ) / _src.BitHeight ;

    // enlarge extent
    ext := _src.Extent ;
    ext := GisExtent(
      ext.XMin - _radius * cell_wdth,
      ext.YMin - _radius * cell_hgth,
      ext.XMax + _radius * cell_wdth,
      ext.YMax + _radius * cell_hgth
    ) ;
    out_rows := _src.BitWidth  + 2 * _radius ;
    out_cols := _src.BitHeight + 2 * _radius ;

    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_CONTOUR_EXPANDGRID ), out_cols * out_rows
    ) ;

    // create pixel layer
    out_lp := TGIS_LayerPixel.Create ;
    try
      // build layer
      out_lp.Build( True, _src.CS, ext,  out_rows, out_cols ) ;
      out_lp.Name := GRID_NAME ;

      // lock layers
      src_lck := _src.LockPixels( _src.Extent, _src.CS, False ) ;  // read-only
      out_lck := out_lp.LockPixels( out_lp.Extent, out_lp.CS, True ) ;  // read-write

      // contour limits set during creation of the grid
      for x := out_lck.Bounds.Left to out_lck.Bounds.Right do begin
        x_src := x - _radius ;

        for y := out_lck.Bounds.Top to out_lck.Bounds.Bottom do begin
          y_src := y - _radius ;

          // cell in buffer
          if (( x < _radius ) or ( x >= src_lck.Bounds.Right + _radius + 1 )) or
             (( y < _radius ) or ( y >= src_lck.Bounds.Bottom + _radius + 1 ))
          then begin
            out_lck.Grid[y, x] := _val ;
          end
          else begin
            if _fillNoData and
               GisIsSameValue( src_lck.Grid[y_src, x_src], nodata, GIS_SINGLE_RESOLUTION ) then
              out_lck.Grid[y, x] := _val
            else
              out_lck.Grid[y, x] := src_lck.Grid[y_src, x_src] ;
          end;

          // progress loop
          if busyEventManager.PushEvent then
            Exit ;
        end ;
      end;

      out_lp.MinHeight := _src.MaxHeight ;
      out_lp.MaxHeight := _src.MaxHeight ;

      //set MIN/MAX for pixel layer
      if _fillNoData then begin
        if ( _val < _src.MinHeight ) then
          out_lp.MinHeight := _val
        else if ( _val > _src.MaxHeight ) then
          out_lp.MaxHeight := _val ;
      end ;

    finally
      out_lp.UnlockPixels( out_lck ) ;
      _src.UnlockPixels( src_lck ) ;
      busyEventManager.EndEvent ;
      Result := out_lp ;
    end ;
  end;

  function TGIS_ContourGenerator.convertArcToPolygon(
    const _arc_lv         : TGIS_LayerVector ;
    const _polyg_lv       : TGIS_LayerVector ;
    const _fld          : String ;
    const _fld_interval : String ;
    const _uids_to_delete : TList< Int64 >
  ) : Boolean ;
  var
    topo                  : TGIS_Topology ;
    tmp_polyg_lv          : TGIS_LayerVector ;
    contour_current       : Double ;
    contour_next          : Double ;
    contour_prev          : Double ;
    contour_interval_next : Double ;
    contour_interval_prev : Double ;
    contour_area          : Double ;
    i_contour             : Integer ;
    query_current         : String ;
    line_shp              : TGIS_Shape ;
    polyg_shp             : TGIS_Shape ;
    contour_as_polygon    : TGIS_Shape ;
    contour_as_polygon_CS : TGIS_Shape ;
    {$IFDEF DCC}
      obj                 : TGIS_Shape ;
    {$ENDIF}
    isClockwise           : Boolean ;
    parts                 : TGIS_IntegerArray;
    windings              : TGIS_IntegerArray;

  begin
    Result := False ;

    _polyg_lv.ImportStructure( _arc_lv ) ;

    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_CONTOUR_ARCTOPOLYG ), contourCount
    ) ;
    topo := TGIS_Topology.Create;
    tmp_polyg_lv := TGIS_LayerVector.Create ;
    try
      tmp_polyg_lv.CS := _polyg_lv.CS ;

      // loop for every contour level
      for i_contour := 0 to contourCount - 1 do begin

        contour_current := getContourValue( i_contour ) ;
        contour_next := getContourValue( i_contour + 1 ) ;
        contour_prev := getContourValue( i_contour -1 ) ;
        contour_interval_next := contour_next - contour_current ;
        contour_interval_prev := contour_current - contour_prev ;

        query_current := buildContourQuery( _fld, contour_current ) ;

        // loop all shapes with the same contour value
        for obj in _arc_lv.Loop(
          _arc_lv.Extent,
          query_current
        ) do begin
          {$IFDEF DCC}
            line_shp := obj ;
          {$ELSE}
            line_shp := TGIS_Shape( obj ) ;
          {$ENDIF}

          // convert to polygon
          contour_as_polygon := line_shp.AsPolygon ;

          // check winding
          isClockwise := True ;
          TGIS_GeometryFactory.GisPolygonPartsStatus(
            contour_as_polygon,
            parts,
            windings
          ) ;
          if windings[0] < 0 then begin
            topo.FixShape( contour_as_polygon ) ;
            isClockwise := False ;
          end ;

          // check area condition and add new polygon;
          // if no CS then always pass
          contour_as_polygon_CS := tmp_polyg_lv.AddShape( contour_as_polygon ) ;
          contour_area := contour_as_polygon_CS.AreaCS ;
          if ( 0 > contour_area ) or ( contour_area >= FMinSize ) then begin
            polyg_shp := _polyg_lv.AddShape( contour_as_polygon ) ;

            //if clockwise winding then values greater than contour are inside polygon
            if isClockwise then begin
              polyg_shp.CopyFields( line_shp ) ;
              polyg_shp.SetField( _fld_interval, contour_interval_next ) ;
            end
            // if counter-clockwise -> values greater than contour are outside!
            else begin
              polyg_shp.SetField( _fld, contour_prev ) ;
              polyg_shp.SetField( _fld_interval, contour_interval_prev ) ;

              // we don't want to store values below base or first fixed level
              if i_contour = 0 then begin
                _uids_to_delete.Add( polyg_shp.Uid ) ;
              end ;
            end ;
          end ;

          FreeObject( contour_as_polygon ) ;
          contour_as_polygon_CS.Delete ;
        end ;

        if busyEventManager.PushEvent then
          Exit ;
      end ;

    finally
      FreeObject( tmp_polyg_lv ) ;
      FreeObject( topo ) ;
      busyEventManager.EndEvent ;
    end ;

    Result := True ;
  end;

  function TGIS_ContourGenerator.getContourValue(
    const _i : Integer
  ) : Double ;
  begin
    if FixedLevelCount = 0 then begin
      Result := FContourBase + _i * FContourInterval ;
    end
    else begin
      if _i < 0 then
        Result := 2 * FFixedLevels[0] - FFixedLevels[1]
      else if _i > contourCount - 1 then
        Result := FFixedLevels[contourCount-1]
      else
        Result := FFixedLevels[_i] ;
    end ;
  end;

  function TGIS_ContourGenerator.buildContourQuery(
    const _fld : String ;
    const _val : Double
  ) : String ;
    const
      QUERY_EPSILON     = 1E-6 ;
      PRECISION         = 6 ;
    var
      contour_val_left  : Double ;
      contour_val_right : Double ;
  begin
    contour_val_left := _val - QUERY_EPSILON ;
    contour_val_right := _val + QUERY_EPSILON ;
    Result := Format(
      '((%s >= %s) AND (%s <= %s))',
      [_fld,
      DotFloatToStrPrec( contour_val_left, PRECISION ),
      _fld,
      DotFloatToStrPrec( contour_val_right, PRECISION )]
    ) ;
  end;

  function TGIS_ContourGenerator.erasePolygons(
    const _polyg_lv         : TGIS_LayerVector ;
    const _fld           : String ;
    const _delete_last_band : Boolean ;
    const _uids_to_delete   : TList< Int64 >
  ) : Boolean ;
  var
    original_polyg_lv : TGIS_LayerVector ;
    topo              : TGIS_Topology ;
    i_contour         : Integer ;
    contour_current   : Double ;
    contour_next      : Double ;
    contour_prev      : Double ;
    query_current     : String ;
    query_next        : String ;
    query_prev        : String ;
    query_next_prev   : String ;
    query_builder     : TStringBuilder ;
    polygs            : TGIS_ShapeList ;
    polyg_current     : TGIS_Shape ;
    polyg_next_prev   : TGIS_Shape ;
    polygs_union      : TGIS_Shape ;
    polyg_erased      : TGIS_Shape ;
    {$IFDEF DCC}
      obj             : TGIS_Shape ;
      obj_next_prev   : TGIS_Shape ;
    {$ENDIF}
  begin
    Result := False ;

    // list to union all next polygons before difference
    original_polyg_lv := TGIS_LayerVector.Create ;
    original_polyg_lv.ImportLayer( _polyg_lv, GisWholeWorld, TGIS_ShapeType.Polygon , '', False ) ;

    busyEventManager.StartEvent(
       _rsrc( GIS_RS_BUSY_CONTOUR_ERASEPOLYG ), contourCount
    ) ;
    polygs := TGIS_ShapeList.Create( False ) ;  // True -> list manages objects
    topo := TGIS_Topology.Create;
    query_builder := TStringBuilder.Create ;
    try
      for i_contour := 0 to contourCount - 1 do begin

        // outer loop query
        contour_current := getContourValue( i_contour ) ;
        query_current := buildContourQuery( _fld, contour_current ) ;

        // inner loop query
        contour_prev := getContourValue( i_contour - 1 ) ;
        query_prev := buildContourQuery( _fld, contour_prev ) ;
        query_builder.Append( query_prev) ;

        if ( i_contour < contourCount - 1 ) then begin
          contour_next := getContourValue( i_contour + 1 ) ;
          query_next := buildContourQuery( _fld, contour_next ) ;
          query_builder.Append(' OR ') ;
          query_builder.Append( query_next)  ;
        end ;
        query_next_prev := query_builder.ToString ;

        // loop all shapes with the same contour value - current
        for obj in _polyg_lv.Loop(
          _polyg_lv.Extent,
          query_current
        ) do begin
          {$IFDEF DCC}
            polyg_current := obj ;
          {$ELSE}
            polyg_current := TGIS_Shape( obj ) ;
          {$ENDIF}

          // for fixed levels, last polygon is deleted to get ribbon-like polygon
          if ( i_contour = contourCount - 1 ) and _delete_last_band then begin
            _uids_to_delete.Add( polyg_current.Uid ) ;
          end
          // erasing process
          else begin
            // loop all shapes with the next/previous contour value
            for obj_next_prev in original_polyg_lv.Loop(
              polyg_current.Extent,
              query_next_prev,
              polyg_current,
              RELATE_CONTAINS_COMPLETELY
            ) do begin
              {$IFDEF DCC}
                polyg_next_prev := obj_next_prev ;
              {$ELSE}
                polyg_next_prev := TGIS_Shape( obj_next_prev ) ;
              {$ENDIF}
              // we collect shapes to make difference on their union
              polygs.Add( polyg_next_prev ) ;
            end ;

            // erase
            if polygs.Count > 0 then begin
              polygs_union := topo.UnionOnList( polygs, False ) ;  // False -> no fix
              try
                polyg_erased := polyg_current.Difference( polygs_union ) ;
                try
                  polyg_current.CopyGeometry( polyg_erased ) ;
                finally
                  FreeObject( polyg_erased ) ;
                end ;
              finally
                FreeObject( polygs_union ) ;
              end ;
            end ;
            polygs.Clear ;
          end ;
        end ;

        query_builder.Clear ;

        if busyEventManager.PushEvent then
          Exit ;
      end ;

    finally
      FreeObject( topo ) ;
      FreeObject( polygs ) ;
      FreeObject( query_builder ) ;
      FreeObject( original_polyg_lv );
      busyEventManager.EndEvent ;
    end ;

    Result := True ;
  end ;

  procedure TGIS_ContourGenerator.deleteUnnecessaryShapes (
    const _dst            : TGIS_LayerVector ;
    const _uids_to_delete : TList<Int64>
  ) ;
  var
    shp : TGIS_Shape ;
    {$IFDEF DCC}
    uid : Int64 ;
    {$ENDIF}
  begin
    for uid in _uids_to_delete do begin
      shp := _dst.GetShape( uid ) ;
      if assigned( shp ) then
        shp.Delete ;
    end ;
  end;

   function TGIS_ContourGenerator.fillZMValue (
    const _dst            : TGIS_LayerVector ;
    const _fld          : String ;
    const _fld_interval : String
  ) : Boolean ;
  var
    Z             : Single ;
    M             : Single ;
    i_part        : Integer ;
    i_point       : Integer ;
    ptg           : TGIS_Point3D ;
    polyg_current : TGIS_Shape ;
    polyg_new     : TGIS_Shape ;
    {$IFDEF DCC}
      obj         : TGIS_Shape ;
    {$ENDIF}
  begin
    Result := False ;

    polyg_new := TGIS_ShapePolygon.Create( TGIS_DimensionType.XYZM ) ;
    try
      for obj in _dst.Loop do begin
        {$IFDEF DCC}
          polyg_current := obj ;
        {$ELSE}
          polyg_current := TGIS_Shape( obj ) ;
        {$ENDIF}

        Z := VarToSingle( polyg_current.GetField( _fld ) ) ;
        M := VarToSingle( polyg_current.GetField( _fld_interval ) ) ;

        polyg_new.Lock( TGIS_Lock.Extent ) ;

        // replace geometry with new Z and M values
        for i_part := 0 to polyg_current.GetNumParts - 1 do begin
          polyg_new.AddPart ;
          polyg_new.SetPartType( i_part, polyg_current.GetPartType( i_part ) ) ;

          for i_point := 0 to polyg_current.GetPartSize( i_part ) - 1 do begin
            ptg := polyg_current.GetPoint3D( i_part, i_point)  ;
            polyg_new.AddPoint3D( GisPoint3D( ptg.X, ptg.Y, Z, M ) ) ;
          end ;
        end ;

        polyg_new.Unlock ;
        polyg_current.CopyGeometry( polyg_new );
        polyg_new.Reset ;
      end ;
    finally
      FreeObject( polyg_new );
    end ;

    Result := True ;
  end;

//==============================================================================
// T_Pipeline_Contour
//==============================================================================

  class procedure Unit_GisContour.SelfRegisterPipeline ;
  begin
    RegisterPipeline(
      'Contour',
      T_Pipeline_Contour
    ) ;
  end ;

  procedure T_Pipeline_Contour.Initialize ;
  begin
    inherited ;

    defineParam(
      GIS_PIPELINE_PARAM_FIELD,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '',
      ''
    ) ;
    defineParam(
      PARAM_CONTOUR_BASE,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      FloatToStr( T_CONTOUR_CONST.CONTOUR_BASE ),
      ''
    ) ;
    defineParam(
      PARAM_CONTOUR_INTERVAL,
      TGIS_PipelineParameterType.Float,
      0.1,
      NaN,
      False,
      FloatToStr( T_CONTOUR_CONST.CONTOUR_INTERVAL ),
      ''
    ) ;
    defineParam(
      PARAM_CUSTOM_NODATA,
      TGIS_PipelineParameterType.Boolean,
      NaN,
      NaN,
      False,
      BoolToStr( T_CONTOUR_CONST.CUSTOM_NODATA ),
      ''
    ) ;
    defineParam(
      PARAM_FIXED_LEVELS,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '',
      ''
    ) ;
    defineParam(
      PARAM_MIN_SIZE,
      TGIS_PipelineParameterType.Float,
      0.0,
      NaN,
      False,
      FloatToStr( T_CONTOUR_CONST.MIN_SIZE ),
      ''
    ) ;
    defineParam(
      PARAM_MODE,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Polylines',
      '!Polylines|ClosedPolylines|OverlappingPolygons|Polygons'
    ) ;
    defineParam(
      PARAM_NODATA_VALUE,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      FloatToStr( T_CONTOUR_CONST.NODATA_VALUE ),
      ''
    ) ;
    defineParam(
      PARAM_SMOOTHEN,
      TGIS_PipelineParameterType.Boolean,
      NaN,
      NaN,
      False,
      BoolToStr( T_CONTOUR_CONST.SMOOTHEN ),
      ''
    ) ;
    defineParam(
      PARAM_SMOOTH_FACTOR,
      TGIS_PipelineParameterType.Int,
      1,
      100,
      False,
      IntToStr( T_CONTOUR_CONST.SMOOTH_FACTOR ),
      ''
    ) ;
  end ;

  procedure T_Pipeline_Contour.Execute;
  var
    param        : String ;
    src          : TGIS_LayerPixel ;
    dst          : TGIS_LayerVector ;
    dst_fld      : String ;
    i            : Integer ;
    tokenizer    : TGIS_Tokenizer  ;
    token        : String ;
    token_count  : Integer ;
    level        : Double ;
    fixed_levels : TGIS_DoubleArray ;
    mode         : TGIS_ContourGeneratorMode ;
    contour_gen  : TGIS_ContourGenerator ;
  begin
    src := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
    dst := ParamAsLayerVector( GIS_PIPELINE_PARAM_DESTINATION ) ;
    dst_fld := ParamAsField( dst, GIS_PIPELINE_PARAM_FIELD, '') ;

    // FixedLevels
    param := ParamAsString( PARAM_FIXED_LEVELS ) ;
    tokenizer := TGIS_Tokenizer.Create ;
    try
      tokenizer.ExecuteEx( param, ',' ) ;
      token_count := tokenizer.Result.Count ;

      SetLength( fixed_levels, token_count ) ;

      for i := 0 to token_count - 1 do begin
        token := tokenizer.Result[i] ;
        if TryStrToFloat( token, level ) then begin
          fixed_levels[i] := level ;
        end
        else begin
          fixed_levels := nil ;
          Break ;
        end ;
      end ;
    finally
      FreeObject( tokenizer ) ;
    end;

    // Mode
    param := ParamAsString( PARAM_MODE ) ;
    if CompareText( param, 'Polylines' ) = 0 then
      mode := TGIS_ContourGeneratorMode.Polylines
    else if CompareText( param, 'ClosedPolylines' ) = 0 then
      mode := TGIS_ContourGeneratorMode.ClosedPolylines
    else if CompareText( param, 'OverlappingPolygons' ) = 0 then
      mode := TGIS_ContourGeneratorMode.OverlappingPolygons
    else if CompareText( param, 'Polygons' ) = 0 then
      mode := TGIS_ContourGeneratorMode.Polygons
    else
      mode := TGIS_ContourGeneratorMode.Polylines ;

    // execute contour generator
   contour_gen := TGIS_ContourGenerator.Create ;
    try
      contour_gen.ContourBase:= ParamAsFloat( PARAM_CONTOUR_BASE, T_CONTOUR_CONST.CONTOUR_BASE ) ;
      contour_gen.ContourInterval := ParamAsFloat( PARAM_CONTOUR_INTERVAL, T_CONTOUR_CONST.CONTOUR_INTERVAL ) ;
      contour_gen.CustomNoData := ParamAsBool( PARAM_CUSTOM_NODATA, T_CONTOUR_CONST.CUSTOM_NODATA ) ;
      contour_gen.FixedLevels := fixed_levels ;
      contour_gen.MinSize := ParamAsFloat( PARAM_MIN_SIZE, T_CONTOUR_CONST.MIN_SIZE ) ;
      contour_gen.Mode := mode ;
      contour_gen.NoDataValue := ParamAsFloat( PARAM_NODATA_VALUE, T_CONTOUR_CONST.NODATA_VALUE ) ;
      contour_gen.Smoothen := ParamAsBool( PARAM_SMOOTHEN, T_CONTOUR_CONST.SMOOTHEN ) ;
      contour_gen.SmoothFactor := ParamAsInt( PARAM_SMOOTH_FACTOR, T_CONTOUR_CONST.SMOOTH_FACTOR ) ;
      contour_gen.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        contour_gen.BusyEvent += DoBusyEvent ;
      {$ELSE}
        contour_gen.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      contour_gen.Generate( src, dst, dst_fld ) ;
    finally
      FreeObject( contour_gen ) ;
    end ;

    inherited ;
  end ;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  Unit_GisContour.SelfRegisterPipeline ;
{$ENDIF}

//==================================== END =====================================
end.
