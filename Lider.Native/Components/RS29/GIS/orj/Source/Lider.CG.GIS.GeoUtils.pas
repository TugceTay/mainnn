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

{$IFDEF DCC}
  unit GisUtils ;
  {$HPPEMIT '#pragma link "GisUtils"'}
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

{$IFDEF DCC}
uses
  System.Variants,
  System.Types,

  GisClasses,
  GisFunctions,
  GisTypes,
  GisLayerVector,
  GisTypesUI,
  GisLayer,
  GisCsSystems,
  GisCsProjections,
  GisCsBase,
  GisSymbol,
  GisOpenCL,
  GisCUDA,
  GisPipeline,
  GisRendererAbstract ;
{$ENDIF}
{$IFDEF CLR}
uses
  System.Runtime.InteropServices,
  System.Drawing,
  System.Collections.Generic,
  TatukGIS.RTL,
  {$IFNDEF BLAZOR}
    TatukGIS.NDK.OpenCL,
    TatukGIS.NDK.CUDA,
    TatukGIS.NDK.SkiaSharp,
  {$ENDIF}
  TatukGIS.NDK;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl,
    tatukgis.rtl;

  {$IFDEF ANDROID}
     {$DEFINE ANDROID_JAVA}
     {$WARNING 'Verify HAVA ANDROID code'}
  {$ENDIF}
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   A class that groups public defines, functions and properties.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_Utils = {$IFDEF OXYGENE} public {$ENDIF} class

    public
      {$IFDEF GIS_XDK}
        /// <summary>
        ///   Create a new point.
        /// </summary>
        /// <param name="_x">
        ///   coordinate
        /// </param>
        /// <param name="_y">
        ///   coordinate
        /// </param>
        /// <returns>
        ///   New point.
        /// </returns>
        class function Point          ( const _x              : Integer  ;
                                        const _y              : Integer
                                      ) : TPoint ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
        /// <summary>
        ///   Create a new rectangle.
        /// </summary>
        /// <param name="_left">
        ///   coordinate
        /// </param>
        /// <param name="_top">
        ///   coordinate
        /// </param>
        /// <param name="_right">
        ///   coordinate
        /// </param>
        /// <param name="_bottom">
        ///   coordinate
        /// </param>
        /// <returns>
        ///   New rectangle.
        /// </returns>
        class function Rect           ( const _left             : Integer  ;
                                        const _top              : Integer  ;
                                        const _right            : Integer  ;
                                        const _bottom           : Integer
                                      ) : TRect ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$ENDIF}
      {$IFDEF GENDOC_XDK}
        /// <summary>
        ///   Test object inheriance.
        /// </summary>
        /// <param name="_interface">
        ///   DK object to be tested
        /// </param>
        /// <param name="_name">
        ///   parent name to be tested
        /// </param>
        /// <returns>
        ///   True if inherited.
        /// </returns>
        function IsInherited          ( const _interface        : TBaseObject;
                                        const _name             : String
                                      ) : Boolean;
      {$ENDIF}

      /// <summary>
      ///   Create a new point.
      /// </summary>
      /// <param name="_x">
      ///   coordinate
      /// </param>
      /// <param name="_y">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   New point.
      /// </returns>
      class function GisPoint         ( const _x              : Double  ;
                                        const _y              : Double
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a new 3D point.
      /// </summary>
      /// <param name="_x">
      ///   coordinate
      /// </param>
      /// <param name="_y">
      ///   coordinate
      /// </param>
      /// <param name="_z">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   New 3D point.
      /// </returns>
      class function GisPoint3D       ( const _x              : Double  ;
                                        const _y              : Double  ;
                                        const _z              : Double
                                      ) : TGIS_Point3D ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a new 3D point.
      /// </summary>
      /// <param name="_x">
      ///   coordinate
      /// </param>
      /// <param name="_y">
      ///   coordinate
      /// </param>
      /// <param name="_z">
      ///   coordinate
      /// </param>
      /// <param name="_m">
      ///   measure
      /// </param>
      /// <returns>
      ///   New 3D point.
      /// </returns>
      class function GisPoint3D       ( const _x              : Double  ;
                                        const _y              : Double  ;
                                        const _z              : Double  ;
                                        const _m              : Double
                                      ) : TGIS_Point3D ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert 2D point into 3D point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   New 3D point converted from 2D.
      /// </returns>
      class function GisPoint3DFrom2D ( const _ptg      : TGIS_Point
                                      ) : TGIS_Point3D ;
                                      static;
      /// <summary>
      ///   Convert 3D point into 2D point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   New point converted from 3D.
      /// </returns>
      class function GisPoint2DFrom3D ( const _ptg      : TGIS_Point3D
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a new line.
      /// </summary>
      /// <param name="_a">
      ///   beginning of a line
      /// </param>
      /// <param name="_b">
      ///   end of line
      /// </param>
      /// <returns>
      ///   New line.
      /// </returns>
      class function  GisLine         ( const _a        : TGIS_Point ;
                                        const _b        : TGIS_Point
                                      ) : TGIS_Line ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a new line.
      /// </summary>
      /// <param name="_ax">
      ///   beginning of a line, x coordinate
      /// </param>
      /// <param name="_ay">
      ///   beginning of a line, y coordinate
      /// </param>
      /// <param name="_bx">
      ///   end of line, x coordinate
      /// </param>
      /// <param name="_by">
      ///   end of line, y coordinate
      /// </param>
      /// <returns>
      ///   New line.
      /// </returns>
      class function  GisLine         ( const _ax       : Double ;
                                        const _ay       : Double ;
                                        const _bx       : Double ;
                                        const _by       : Double
                                      ) : TGIS_Line ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a new line.
      /// </summary>
      /// <param name="_a">
      ///   beginning of a line
      /// </param>
      /// <param name="_b">
      ///   end of line
      /// </param>
      /// <returns>
      ///   New line.
      /// </returns>
      class function  GisLine3D       ( const _a        : TGIS_Point3D ;
                                        const _b        : TGIS_Point3D
                                      ) : TGIS_Line3D ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert 3D point into 2D point.
      /// </summary>
      /// <param name="_ext">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   New 3D extent converted from 2D.
      /// </returns>
      class function GisExtent3DFrom2D( const _ext      : TGIS_Extent
                                      ) : TGIS_Extent3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Convert 3D extent into 2D extent.
      /// </summary>
      /// <param name="_ext">
      ///   extent to be converted
      /// </param>
      /// <returns>
      ///   New extent converted from 3D.
      /// </returns>
      class function GisExtent2DFrom3D( const _ext      : TGIS_Extent3D
                                      ) : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create new extent.
      /// </summary>
      /// <param name="_xmin">
      ///   coordinate
      /// </param>
      /// <param name="_ymin">
      ///   coordinate
      /// </param>
      /// <param name="_xmax">
      ///   coordinate
      /// </param>
      /// <param name="_ymax">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   New extent.
      /// </returns>
      class function GisExtent        ( const _xmin           : Double ;
                                        const _ymin           : Double ;
                                        const _xmax           : Double ;
                                        const _ymax           : Double
                                      ) : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create new extent.
      /// </summary>
      /// <param name="_xmin">
      ///   coordinate
      /// </param>
      /// <param name="_ymin">
      ///   coordinate
      /// </param>      ///
      /// <param name="_zmin">
      ///   coordinate
      /// </param>
      /// <param name="_xmax">
      ///   coordinate
      /// </param>
      /// <param name="_ymax">
      ///   coordinate
      /// </param>
      /// <param name="_zmax">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   New 3D extent.
      /// </returns>
      class function GisExtent3D      ( const _xmin           : Double ;
                                        const _ymin           : Double ;
                                        const _zmin           : Double ;
                                        const _xmax           : Double ;
                                        const _ymax           : Double ;
                                        const _zmax           : Double
                                      ) : TGIS_Extent3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate area of extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   Area of the extent.
      /// </returns>
      class function GisExtentArea    ( const _extent   : TGIS_Extent
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate area of 3D extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   Area of the 3D extent.
      /// </returns>
      class function GisExtentArea3D  ( const _extent   : TGIS_Extent3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate cubature of extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   Cubature of the extent.
      /// </returns>
      class function GisExtentCubature( const _extent   : TGIS_Extent
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate cubature of 3D extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   Cubature of the 3D extent.
      /// </returns>
      class function GisExtentCubature3D
                                      ( const _extent   : TGIS_Extent3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Are two points equal?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <returns>
      ///   True if points are equal.
      /// </returns>
      class function GisIsSamePoint   ( const _ptg1     : TGIS_Point ;
                                        const _ptg2     : TGIS_Point
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two points equal?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      class function GisIsSamePoint   ( const _ptg1     : TGIS_Point ;
                                        const _ptg2     : TGIS_Point ;
                                        const _tol      : Double
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D points equal in 3D?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <returns>
      ///   True if points are equal.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored. To compare with measure use GisIsSamePoint3DM().
      ///   </note>
      /// </remarks>
      class function GisIsSamePoint3D ( const _ptg1     : TGIS_Point3D ;
                                        const _ptg2     : TGIS_Point3D
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


      /// <summary>
      ///   Are two 3D points equal in 3D?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored. To compare with measure use GisIsSamePoint3DM().
      ///   </note>
      /// </remarks>
      class function GisIsSamePoint3D ( const _ptg1     : TGIS_Point3D ;
                                        const _ptg2     : TGIS_Point3D ;
                                        const _tol      : Double
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D points equal in 3D?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is not ignored. To compare without measure
      ///     use GisIsSamePoint3D().
      ///   </note>
      /// </remarks>
      class function GisIsSamePoint3DM( const _ptg1     : TGIS_Point3D ;
                                        const _ptg2     : TGIS_Point3D
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D points equal in 3D?
      /// </summary>
      /// <param name="_ptg1">
      ///   point to compare
      /// </param>
      /// <param name="_ptg2">
      ///   point to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is not ignored. To compare without measure
      ///     use GisIsSamePoint3D().
      ///   </note>
      /// </remarks>
      class function GisIsSamePoint3DM( const _ptg1     : TGIS_Point3D ;
                                        const _ptg2     : TGIS_Point3D ;
                                        const _tol      : Double
                                      ) : Boolean ; overload ;
                                     {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if extents are equal.
      /// </returns>
      class function GisIsSameExtent  ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      class function GisIsSameExtent  ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent ;
                                        const _tol      : Double
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored. To compare with measure use
      ///     GisIsSameExtent3DM().
      ///   </note>
      /// </remarks>
      class function GisIsSameExtent3D( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D ;
                                        const _tol      : Double
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if 3D extents are equal.
      /// </returns>
      class function GisIsSameExtent3D( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is not ignored. To compare without measure
      ///     use GisIsSameExtent3D().
      ///   </note>
      /// </remarks>
      class function GisIsSameExtent3DM(
                                        const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : Boolean ;
                                      overload; static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Are two 3D extents equal?
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <param name="_tol">
      ///   tolerance of comparison
      /// </param>
      /// <returns>
      ///   True if are equal
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is not ignored. To compare without measure
      ///     use GisIsSameExtent3D().
      ///   </note>
      /// </remarks>
      class function GisIsSameExtent3DM(
                                        const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D ;
                                        const _tol      : Double
                                      ) : Boolean ; overload ;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Is _extent2 wholly encompass by _extent1.
      /// </summary>
      /// <param name="_extent1">
      ///   extent to compare
      /// </param>
      /// <param name="_extent2">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if _extent2 is wholly encompass by _extent1.
      /// </returns>
      class function GisContainExtent ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is extent empty? Extent is empty if (XMin&gt;=XMax) or
      ///   (YMin&gt;=YMax)
      /// </summary>
      /// <param name="_extent">
      ///   extent to be verified
      /// </param>
      /// <returns>
      ///   True if extent is empty.
      /// </returns>
      class function GisIsEmptyExtent ( const _extent   : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is extent empty? Extent is empty if (XMin&gt;=XMax) or
      ///   (YMin&gt;=YMax) or (ZMin&gt;=ZMax)
      /// </summary>
      /// <param name="_extent">
      ///   extent to be verified
      /// </param>
      /// <returns>
      ///   True if 3D extent is empty.
      /// </returns>
      class function GisIsEmptyExtent3D
                                      ( const _extent   : TGIS_Extent3D
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns an infinite extent
      /// </summary>
      /// <returns>
      ///   Infinite extent.
      /// </returns>
      class function GisWholeWorld    : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns an infinite 3D extent
      /// </summary>
      /// <returns>
      ///   Infinite extent.
      /// </returns>
      class function GisWholeWorld3D  : TGIS_Extent3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is extent same as whole world?
      /// </summary>
      /// <param name="_extent">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if extent is the same as extent of whole world.
      /// </returns>
      class function GisIsWholeWorld  ( const _extent   : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is 3D extent same as whole world?
      /// </summary>
      /// <param name="_extent">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if 3D extent is the same as extent of whole world.
      /// </returns>
      class function GisIsWholeWorld3D( const _extent   : TGIS_Extent3D
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns an nil extent.
      /// </summary>
      /// <returns>
      ///   Nil extent.
      /// </returns>
      class function GisNoWorld       : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns an nil 3D extent.
      /// </summary>
      /// <returns>
      ///   Nil 3D extent.
      /// </returns>
      class function GisNoWorld3D     : TGIS_Extent3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is extent same as no world (non existing extent)?
      /// </summary>
      /// <param name="_extent">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if extent is the same as no world.
      /// </returns>
      class function GisIsNoWorld     ( const _extent   : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Is 3D extent same as no world (non existing extent)?
      /// </summary>
      /// <param name="_extent">
      ///   extent to compare
      /// </param>
      /// <returns>
      ///   True if 3D extent is the same as no world.
      /// </returns>
      class function GisIsNoWorld3D   ( const _extent   : TGIS_Extent3D
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Minimum of two doubles.
      /// </summary>
      /// <param name="_val1">
      ///   values to compare
      /// </param>
      /// <param name="_val2">
      ///   values to compare
      /// </param>
      /// <returns>
      ///   Minimum value computed out of two doubles.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Obsoleted!
      ///   </note>
      /// </remarks>
      class function GisDMin          ( const _val1           : Double ;
                                        const _val2           : Double
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Maximum of two doubles.
      /// </summary>
      /// <param name="_val1">
      ///   value to compare
      /// </param>
      /// <param name="_val2">
      ///   value to compare
      /// </param>
      /// <returns>
      ///   Maximum value computed out of two doubles.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Obsoleted!
      ///   </note>
      /// </remarks>
      class function GisDMax          ( const _val1           : Double ;
                                        const _val2           : Double
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Maximum of two extents.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   Maximum extent computed out of two extents.
      /// </returns>
      /// <remarks>
      ///   Resulting value is the extent that covers the whole area of
      ///   _extent1 and extent2.
      /// </remarks>
      class function GisMaxExtent     ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Maximum of two 3D extents.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   Maximum 3D extent computed out of two extents.
      /// </returns>
      /// <remarks>
      ///   Resulting value is the extent that covers the whole area of
      ///   _extent1 and extent2.
      /// </remarks>
      class function GisMaxExtent3D   ( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : TGIS_Extent3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Have two extents a common part?.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   True if two extents have common part.
      /// </returns>
      class function GisIsCommonExtent( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Have two 3D extents a common part?.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   True if two 3D extents have common part.
      /// </returns>
      class function GisIsCommonExtent3D
                                      ( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   True if _extent2 fully covers _extent1.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   True if _extent2 fully covers _extent.
      /// </returns>
      class function GisIsContainExtent
                                      ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   True if _extent2 fully covers _extent1 in 3D.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   True if _extent2 fully covers _extent1.
      /// </returns>
      class function GisIsContainExtent3D
                                      ( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Common area of two extents.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   New extent as common area of two extents.
      /// </returns>
      /// <remarks>
      ///   Resulting value is an extent that covers the common area of
      ///   _extent1 and _extent2.
      /// </remarks>
      class function GisCommonExtent  ( const _extent1  : TGIS_Extent ;
                                        const _extent2  : TGIS_Extent
                                      ) : TGIS_Extent ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Common area of two 3D extents.
      /// </summary>
      /// <param name="_extent1">
      ///   area to compare
      /// </param>
      /// <param name="_extent2">
      ///   area to compare
      /// </param>
      /// <returns>
      ///   New 3D extent as common area of two 3D extents.
      /// </returns>
      /// <remarks>
      ///   Resulting value is an extent that covers the common area of
      ///   _extent1 and _extent2.
      /// </remarks>
      class function GisCommonExtent3D( const _extent1  : TGIS_Extent3D ;
                                        const _extent2  : TGIS_Extent3D
                                      ) : TGIS_Extent3D ;
                                      static;
      /// <summary>
      ///   Calculates line length.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of line
      /// </param>
      /// <param name="_lineB">
      ///   ending of line
      /// </param>
      /// <returns>
      ///   Length of the line.
      /// </returns>
      class function GisLineLength    ( const _lineA    : TGIS_Point ;
                                        const _lineB    : TGIS_Point
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates point-to-point distance.
      /// </summary>
      /// <param name="_ptg1">
      ///   first point
      /// </param>
      /// <param name="_ptg2">
      ///   second point
      /// </param>
      /// <returns>
      ///   Distance between two points.
      /// </returns>
      class function GisPoint2Point   ( const _ptg1     : TGIS_Point ;
                                        const _ptg2     : TGIS_Point
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates point-to-point distance in 3D.
      /// </summary>
      /// <param name="_ptg1">
      ///   first point
      /// </param>
      /// <param name="_ptg2">
      ///   second point
      /// </param>
      /// <returns>
      ///   Distance between two 3D points.
      /// </returns>
      class function GisPoint2Point3D ( const _ptg1     : TGIS_Point3D ;
                                        const _ptg2     : TGIS_Point3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to point distance.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of line
      /// </param>
      /// <param name="_lineB">
      ///   ending of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Distance between point and line.
      /// </returns>
      class function GisLine2Point    ( const _lineA    : TGIS_Point ;
                                        const _lineB    : TGIS_Point ;
                                        const _ptg      : TGIS_Point
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to point distance.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of line
      /// </param>
      /// <param name="_lineB">
      ///   ending of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Distance between point and line.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     For precise calculation use GisLine2Point.
      ///   </note>
      /// </remarks>
      class function GisLine2PointFuzzy
                                      ( const _lineA    : TGIS_Point ;
                                        const _lineB    : TGIS_Point ;
                                        const _ptg      : TGIS_Point
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to point distance in 3D.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of line
      /// </param>
      /// <param name="_lineB">
      ///   ending of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Distance between 3D point and 3D line.
      /// </returns>
      class function GisLine2Point3D  ( const _lineA    : TGIS_Point3D ;
                                        const _lineB    : TGIS_Point3D ;
                                        const _ptg      : TGIS_Point3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to point distance.
      /// </summary>
      /// <param name="_lineA">
      ///   beginning of line
      /// </param>
      /// <param name="_lineB">
      ///   ending of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Distance between 3D point and 3D line.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     For precise calculation use GisLine2Point3D.
      ///   </note>
      /// </remarks>
      class function GisLine2Point3DFuzzy
                                      ( const _lineA    : TGIS_Point3D ;
                                        const _lineB    : TGIS_Point3D ;
                                        const _ptg      : TGIS_Point3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to line distance.
      /// </summary>
      /// <param name="_line1A">
      ///   beginning of line 1
      /// </param>
      /// <param name="_line1B">
      ///   ending of line 1
      /// </param>
      /// <param name="_line2A">
      ///   beginning of line 2
      /// </param>
      /// <param name="_line2B">
      ///   ending of line 2
      /// </param>
      /// <returns>
      ///   Distance between two lines.
      /// </returns>
      class function GisLine2Line     ( const _line1A   : TGIS_Point ;
                                        const _line1B   : TGIS_Point ;
                                        const _line2A   : TGIS_Point ;
                                        const _line2B   : TGIS_Point
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates line to line distance in 3D.
      /// </summary>
      /// <param name="_line1A">
      ///   beginning of line 1
      /// </param>
      /// <param name="_line1B">
      ///   ending of line 1
      /// </param>
      /// <param name="_line2A">
      ///   beginning of line 2
      /// </param>
      /// <param name="_line2B">
      ///   ending of line 2
      /// </param>
      /// <returns>
      ///   Distance between two 3D lines.
      /// </returns>
      class function GisLine2Line3D   ( const _line1A   : TGIS_Point3D ;
                                        const _line1B   : TGIS_Point3D ;
                                        const _line2A   : TGIS_Point3D ;
                                        const _line2B   : TGIS_Point3D
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates a projection of a point into the line.
      /// </summary>
      /// <param name="_lineA">
      ///   begin of line
      /// </param>
      /// <param name="_lineB">
      ///   end of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Projection of a point into the line.
      /// </returns>
      class function GisPointOnLine   ( const _lineA    : TGIS_Point ;
                                        const _lineB    : TGIS_Point ;
                                        const _ptg      : TGIS_Point
                                      ) : TGIS_Point ;
                                      static;
      /// <summary>
      ///   Calculates a projection of a point into the line in 3D.
      /// </summary>
      /// <param name="_lineA">
      ///   begin of line
      /// </param>
      /// <param name="_lineB">
      ///   end of line
      /// </param>
      /// <param name="_ptg">
      ///   point
      /// </param>
      /// <returns>
      ///   Projection of a 3D point into the 3D line.
      /// </returns>
      class function GisPointOnLine3D ( const _lineA    : TGIS_Point3D ;
                                        const _lineB    : TGIS_Point3D ;
                                        const _ptg      : TGIS_Point3D
                                      ) : TGIS_Point3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates delta between two points.
      /// </summary>
      /// <param name="_pt1">
      ///   point to comparison
      /// </param>
      /// <param name="_pt2">
      ///   point to comparison
      /// </param>
      /// <returns>
      ///   Delta between two points.
      /// </returns>
      class function GisPointsDelta   ( const _pt1      : TGIS_Point ;
                                        const _pt2      : TGIS_Point
                                      ) : TGIS_Point ;
                                      static;
      /// <summary>
      ///   Calculates delta between two points in 3D.
      /// </summary>
      /// <param name="_pt1">
      ///   point to comparison
      /// </param>
      /// <param name="_pt2">
      ///   point to comparison
      /// </param>
      /// <returns>
      ///   Delta between two 3D points.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored.
      ///   </note>
      /// </remarks>
      class function GisPointsDelta3D ( const _pt1      : TGIS_Point3D ;
                                        const _pt2      : TGIS_Point3D
                                      ) : TGIS_Point3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Move point by a given delta value.
      /// </summary>
      /// <param name="_ptg">
      ///   point to move
      /// </param>
      /// <param name="_delta">
      ///   movement
      /// </param>
      /// <returns>
      ///   Moved point.
      /// </returns>
      class function GisMovePoint     ( const _ptg      : TGIS_Point ;
                                        const _delta    : TGIS_Point
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Move point by a given delta value in 3D.
      /// </summary>
      /// <param name="_ptg">
      ///   point to move
      /// </param>
      /// <param name="_delta">
      ///   movement
      /// </param>
      /// <returns>
      ///   Moved 3D point.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored.
      ///   </note>
      /// </remarks>
      class function GisMovePoint3D   ( const _ptg      : TGIS_Point3D ;
                                        const _delta    : TGIS_Point3D
                                      ) : TGIS_Point3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Scale point by a given delta value.
      /// </summary>
      /// <param name="_ref">
      ///   reference point (origin )
      /// </param>
      /// <param name="_ptg">
      ///   point to scale
      /// </param>
      /// <param name="_scale">
      ///   movement
      /// </param>
      /// <returns>
      ///   Scaled point.
      /// </returns>
      class function GisScalePoint    ( const _ref      : TGIS_Point ;
                                        const _ptg      : TGIS_Point ;
                                             _scale     : Double
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Scale point by a given delta value in 3D.
      /// </summary>
      /// <param name="_ref">
      ///   reference point (origin )
      /// </param>
      /// <param name="_ptg">
      ///   point to scale
      /// </param>
      /// <param name="_scale">
      ///   movement
      /// </param>
      /// <returns>
      ///   Scaled 3D point.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Measure is ignored.
      ///   </note>
      /// </remarks>
      class function GisScalePoint3D  ( const _ref      : TGIS_Point3D ;
                                        const _ptg      : TGIS_Point3D ;
                                             _scale     : Double
                                      ) : TGIS_Point3D ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Rotate point by a given angle.
      /// </summary>
      /// <param name="_ref">
      ///   reference point (origin )
      /// </param>
      /// <param name="_ptg">
      ///   point to rotate
      /// </param>
      /// <param name="_angle">
      ///   angle (in radians)
      /// </param>
      /// <returns>
      ///   Rotated point.
      /// </returns>
      class function GisRotatePoint   ( const _ref      : TGIS_Point ;
                                        const _ptg      : TGIS_Point ;
                                             _angle     : Double
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculates center point of given extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   Center point of given extent.
      /// </returns>
      class function GisCenterPoint   ( const _extent   : TGIS_Extent
                                      ) : TGIS_Point ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Converts an angle in radians to compass (NW, SW etc)
      ///   representation.
      /// </summary>
      /// <param name="_angle">
      ///   angle in radians
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisRadToCompass  ( const _angle          : Double
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create a reprojected shape.
      /// </summary>
      /// <param name="_shp">
      ///   source shape to reproject
      /// </param>
      /// <param name="_src_cs">
      ///   source coordinate system
      /// </param>
      /// <param name="_dst_cs">
      ///   destination coordinate system
      /// </param>
      /// <returns>
      ///   Reprojected shape.
      /// </returns>
      {#ownership:result:take}
      class function  GisCreateReprojectedShape
                                      ( const _shp     : TGIS_Shape               ;
                                        const _src_cs  : TGIS_CSCoordinateSystem  ;
                                        const _dst_cs  : TGIS_CSCoordinateSystem
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Build a polygon from a list of non complex edges.
      /// </summary>
      /// <param name="_edges">
      ///   list of non complex edges of TGIS_ShapeArc type
      /// </param>
      /// <param name="_tolerance">
      ///   tolerance into which two arcs are close enough to be joined
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <param name="_fixShape">
      ///   if True, the newly created shape will be fixed using TGIS_Topology.FixShape
      /// </param>
      /// <returns>
      ///   Constructed polygon.
      /// </returns>
      class function  GisBuildPolygonFromEdges
                                      ( const _edges     : TGIS_ObjectList ;
                                        const _tolerance : Double ;
                                        const _source    : TGIS_Shape ;
                                        {$IFDEF MANAGED}
                                          const _ptr     : TGIS_Bytes ;
                                        {$ELSE}
                                          const _ptr     : Pointer    ;
                                        {$ENDIF}
                                        const _mapped    : Boolean    ;
                                        const _uid       : Integer    ;
                                        const _layer     : TGIS_LayerVector ;
                                        const _fixShape  : Boolean
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Build a line string from a list of non complex edges.
      /// </summary>
      /// <param name="_edges">
      ///   list of non complex edges of TGIS_ShapeArc type
      /// </param>
      /// <param name="_tolerance">
      ///   tolerance into which two arcs are close enough to be joined
      /// </param>
      /// <param name="_source">
      ///   If not nil, then the base shape will be based on this shape.
      ///   Otherwise _ptr, _uid and _layer will be used.
      /// </param>
      /// <param name="_mapped">
      ///   True if pointer is mapped to the file
      /// </param>
      /// <param name="_ptr">
      ///   Address in memory where shape data exists.
      /// </param>
      /// <param name="_uid">
      ///   Unique identifier for shape.
      /// </param>
      /// <param name="_layer">
      ///   Reference to the layer on which the shape will be created.
      /// </param>
      /// <param name="_fixShape">
      ///   if True, the newly created shape topology will be fixed.
      /// </param>
      /// <returns>
      ///   Constructed shape.
      /// </returns>
      class function  GisBuildLineStringFromEdges
                                      ( const _edges     : TGIS_ObjectList ;
                                        const _tolerance : Double ;
                                        const _source    : TGIS_Shape ;
                                        {$IFDEF MANAGED}
                                          const _ptr     : TGIS_Bytes ;
                                        {$ELSE}
                                          const _ptr     : Pointer    ;
                                        {$ENDIF}
                                        const _mapped    : Boolean    ;
                                        const _uid       : Integer    ;
                                        const _layer     : TGIS_LayerVector ;
                                        const _fixShape  : Boolean
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate circle based on any three points from the circle
      ///   outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   second provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie (currently always 0)
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie (currently always 2Pi)
      /// </param>
      /// <returns>
      ///   True if constructed.
      /// </returns>
      class function GisCircleFrom3Points
                                      ( const _ptg1     : TGIS_Point ;
                                        const _ptg2     : TGIS_Point ;
                                        const _ptg3     : TGIS_Point ;
                                        var _center     : TGIS_Point ;
                                        var _radius     : Double     ;
                                        var _start      : Double     ;
                                        var _stop       : Double
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate circle based on any three 3D points from the circle
      ///   outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   second provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie (currently always 0)
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie (currently always 2Pi)
      /// </param>
      /// <returns>
      ///   True if constructed.
      /// </returns>
      class function GisCircleFrom3Points3D
                                     ( const _ptg1   : TGIS_Point3D ;
                                       const _ptg2   : TGIS_Point3D ;
                                       const _ptg3   : TGIS_Point3D ;
                                       var _center   : TGIS_Point3D ;
                                       var _radius   : Double     ;
                                       var _start    : Double     ;
                                       var _stop     : Double
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate arc based on any three points from the circle
      ///   outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   second provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie
      /// </param>
      /// <returns>
      ///   True if constructed.
      /// </returns>
      class function GisArcFrom3Points
                                      ( const _ptg1   : TGIS_Point ;
                                       const _ptg2   : TGIS_Point ;
                                       const _ptg3   : TGIS_Point ;
                                       var _center   : TGIS_Point ;
                                       var _radius   : Double     ;
                                       var _start    : Double     ;
                                       var _stop     : Double
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Calculate arc based on any three 3D points from the circle
      ///   outline.
      /// </summary>
      /// <param name="_ptg1">
      ///   first provided point
      /// </param>
      /// <param name="_ptg2">
      ///   second provided point
      /// </param>
      /// <param name="_ptg3">
      ///   second provided point
      /// </param>
      /// <param name="_center">
      ///   center of the calculated string
      /// </param>
      /// <param name="_radius">
      ///   radius of the calculated string
      /// </param>
      /// <param name="_start">
      ///   start angle for pie
      /// </param>
      /// <param name="_stop">
      ///   stop angle for pie
      /// </param>
      /// <returns>
      ///   True if constructed.
      /// </returns>
      class function GisArcFrom3Points3D
                                      ( const _ptg1   : TGIS_Point3D ;
                                       const _ptg2   : TGIS_Point3D ;
                                       const _ptg3   : TGIS_Point3D ;
                                       var _center   : TGIS_Point3D ;
                                       var _radius   : Double     ;
                                       var _start    : Double     ;
                                       var _stop     : Double
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Get default value for selected field type
      /// </summary>
      /// <param name="_type">
      ///   field type
      /// </param>
      /// <returns>
      ///   Default value.
      /// </returns>
      class function GisDefaultField  ( const _type           : TGIS_FieldType
                                      ) : Variant ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns directory in which samples data was installed.
      /// </summary>
      /// <returns>
      ///   Directory path.
      /// </returns>
      /// <remarks>
      ///   Directory is define in \Windows\TatukGIS.ini file. See
      ///   example.
      /// </remarks>
      class function GisSamplesDataDir: String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Download if necessary and returns directory in which samples data
      ///   was installed.
      /// </summary>
      /// <returns>
      ///   full path
      /// </returns>
      /// <remarks>
      ///   Download not implemented yet on .NET nor Java.
      /// </remarks>
      class function GisSamplesDataDirDownload: String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Returns path part of a given path.
      /// </summary>
      /// <param name="_path">
      ///   given path
      /// </param>
      /// <returns>
      ///   Path part of a given path.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Function expands any embedded aliases (see GisAliasList)
      ///   </note>
      /// </remarks>
      class function  GisFilePath       ( const _path : String ) : String  ;

      /// <summary>
      ///   Returns a path given by _path but relative to directory given by _dir.
      ///   Path recognized as URL and path with CE delimited strings will not be
      ///   altered.
      /// </summary>
      /// <param name="_dir">
      ///   reference directory
      /// </param>
      /// <param name="_path">
      ///   given path
      /// </param>
      /// <returns>
      ///   Path given by _path but relative to directory given by _dir.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Function expands any embedded aliases (see GisAliasList)
      ///   </note>
      /// </remarks>
      class function  GisPathRelative   ( const _dir  : String ;
                                          const _path : String ) : String  ;

      /// <summary>
      ///   Returns canonical SQL name.
      /// </summary>
      /// <param name="_name">
      ///   name to be canonized
      /// </param>
      /// <returns>
      ///   Canonical SQL name.
      /// </returns>
      /// <remarks>
      ///   Spaces, non ASCII or special characters will be replaced with
      ///   underscore. If too many replacement are required then table name will
      ///   be substituted with _template + '_' + ticknumber.
      /// </remarks>
      class function  GisCanonicalSQLName( const _name      : String
                                          ) : String ; overload ;

      /// <summary>
      ///   Returns canonical SQL name.
      /// </summary>
      /// <param name="_name">
      ///   name to be canonized
      /// </param>
      /// <param name="_template">
      ///   templates for name which can not be canonized
      /// </param>
      /// <returns>
      ///   Canonical SQL name.
      /// </returns>
      /// <remarks>
      ///   Spaces, non ASCII or special characters will be replaced with
      ///   underscore. If too many replacement are required then table name will
      ///   be substituted with _template + '_' + ticknumber.
      /// </remarks>
      class function  GisCanonicalSQLName  ( const _name        : String ;
                                              const _template   : String
                                            ) : String ;
                                               overload;
      /// <summary>
      ///   Prepare normalized version of a field name.
      /// </summary>
      /// <param name="_name">
      ///   field name to be normalized
      /// </param>
      /// <returns>
      ///   Normalized version of a field name.
      /// </returns>
      class function  GisNormalizedSQLName ( const _name    : String
                                           ) : String ;

      /// <summary>
      ///   Prepare de-normalized version of a field name.
      /// </summary>
      /// <param name="_name">
      ///   field name to be de-normalized
      /// </param>
      /// <returns>
      ///   De-normalized version of a field name.
      /// </returns>
      class function  GisDeNormalizedSQLName ( const _name  : String
                                             ) : String ;
      /// <summary>
      ///   Decode angle value into degrees, minutes, seconds. If you
      ///   want to construct your own string based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   latitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeAngle  ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Decode angle value into degrees, minutes, seconds. If you
      ///   want to construct your own string based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   latitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <param name="_prec">
      ///   precision - number of digits after decimal point measured on
      ///   seconds (0..15) ; default is 2
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeAngle  ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer    ;
                                        const _prec     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Decode latitude value into degrees, minutes, seconds. If you
      ///   want to construct your own String based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   latitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeLatitude
                                      ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Decode latitude value into degrees, minutes, seconds. If you
      ///   want to construct your own String based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   latitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <param name="_prec">
      ///   precision - number of digits after decimal point measured on
      ///   seconds (0..15) ; default is 2
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeLatitude
                                      ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer    ;
                                        const _prec     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Decode longitude value into degrees, minutes, seconds. If you
      ///   want to construct your own String based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   longitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeLongitude
                                      ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Decode longitude value into degrees, minutes, seconds. If you
      ///   want to construct your own String based on such values keep
      ///   in mind rounding problem. So we recommend to use truncated
      ///   values of degrees and minutes.
      /// </summary>
      /// <param name="_val">
      ///   longitude to be decoded (in radians)
      /// </param>
      /// <param name="_deg">
      ///   degrees
      /// </param>
      /// <param name="_min">
      ///   minutes
      /// </param>
      /// <param name="_sec">
      ///   seconds
      /// </param>
      /// <param name="_frac">
      ///   fraction of seconds
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <param name="_prec">
      ///   precision - number of digits after decimal point measured on
      ///   seconds (0..15) ; default is 2
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      class procedure GisDecodeLongitude
                                      ( const _val      : Double     ;
                                        var   _deg      : Integer    ;
                                        var   _min      : Integer    ;
                                        var   _sec      : Integer    ;
                                        var   _frac     : Integer    ;
                                        var   _sign     : Integer    ;
                                        const _prec     : Integer
                                      ) ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode angle value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value; if &lt;0 then force sign of result
      ///   regardless off _sign value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded angle.
      /// </returns>
      class function GisEncodeAngle   ( const _deg      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode angle value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value; if &lt;0 then force sign of result
      ///   regardless off _sign value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded angle.
      /// </returns>
      class function GisEncodeAngle   ( const _deg      : Double     ;
                                        const _min      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode angle value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value; if &lt;0 then force sign of result
      ///   regardless off _sign value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded angle.
      /// </returns>
      class function GisEncodeAngle   ( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode angle value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value; if &lt;0 then force sign of result
      ///   regardless off _sign value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as &lt;0; if &lt; 0, then treated as
      ///   &gt;0
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded angle.
      /// </returns>
      class function GisEncodeAngle   ( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double     ;
                                        const _sign     : Integer
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded latitude.
      /// </returns>
      class function GisEncodeLatitude( const _deg      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded latitude.
      /// </returns>
      class function GisEncodeLatitude( const _deg      : Double     ;
                                        const _min      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded latitude.
      /// </returns>
      class function GisEncodeLatitude( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as North; if &lt; 0, then treated as
      ///   South
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded latitude.
      /// </returns>
      class function GisEncodeLatitude( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double     ;
                                        const _sign     : Integer
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded longitude.
      /// </returns>
      class function GisEncodeLongitude
                                      ( const _deg      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded longitude.
      /// </returns>
      class function GisEncodeLongitude
                                      ( const _deg      : Double     ;
                                        const _min      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded longitude.
      /// </returns>
      class function GisEncodeLongitude
                                      ( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Encode longitude value from the values specified as the
      ///     degrees, minutes, seconds.
      ///   </para>
      ///   <para>
      ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to
      ///     achieve the same result. If you provide ( 18, 54,5, 30, 1 )
      ///     it will be the same as ( 19, 0, 0, 1 )
      ///   </para>
      /// </summary>
      /// <param name="_deg">
      ///   degrees; absolute value
      /// </param>
      /// <param name="_min">
      ///   minutes; absolute value
      /// </param>
      /// <param name="_sec">
      ///   seconds; absolute value
      /// </param>
      /// <param name="_sign">
      ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as
      ///   West
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_COORDINATE_INVALID
      /// </exception>
      /// <returns>
      ///   Encoded longitude.
      /// </returns>
      class function GisEncodeLongitude
                                      ( const _deg      : Double     ;
                                        const _min      : Double     ;
                                        const _sec      : Double     ;
                                        const _sign     : Integer
                                      ) : Double ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a angle in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   angle in radians
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisAngleToStr    ( const _val      : Double
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a angle in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   angle in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisAngleToStr    ( const _val      : Double     ;
                                        const _spaces   : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a angle in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   angle in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <param name="_prec">
      ///   number of precise digits
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisAngleToStr    ( const _val      : Double     ;
                                        const _spaces   : Boolean    ;
                                        const _prec     : Integer
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert latitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   latitude in radians
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLatitudeToStr ( const _val      : Double
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert latitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   latitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLatitudeToStr ( const _val      : Double     ;
                                        const _spaces   : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert latitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   latitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <param name="_prec">
      ///   number of precise digits
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLatitudeToStr ( const _val      : Double     ;
                                        const _spaces   : Boolean    ;
                                        const _prec     : Integer
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert latitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   latitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <param name="_prec">
      ///   number of precise digits
      /// </param>
      /// <param name="_leading">
      ///   if True, then leading zeros will be presented
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLatitudeToStr ( const _val      : Double     ;
                                        const _spaces   : Boolean    ;
                                        const _prec     : Integer    ;
                                        const _leading  : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a longitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   longitude in radians
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLongitudeToStr( const _val      : Double
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a longitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   longitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLongitudeToStr( const _val      : Double     ;
                                        const _spaces   : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a longitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   longitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <param name="_prec">
      ///   number of precise digits
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLongitudeToStr( const _val      : Double     ;
                                        const _spaces   : Boolean    ;
                                        const _prec     : Integer
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a longitude in radians to a string representation.
      /// </summary>
      /// <param name="_val">
      ///   longitude in radians
      /// </param>
      /// <param name="_spaces">
      ///   if True, then text will be space separated
      /// </param>
      /// <param name="_prec">
      ///   number of precise digits
      /// </param>
      /// <param name="_leading">
      ///   if True, then leading zeros will be presented
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisLongitudeToStr( const _val      : Double     ;
                                        const _spaces   : Boolean    ;
                                        const _prec     : Integer    ;
                                        const _leading  : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a angle in a string representation into value in
      ///   radians.
      /// </summary>
      /// <param name="_txt">
      ///   angle in a string format; must be in range -180..360
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisStrToAngle    ( const _txt      : String
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert longitude in a string representation into value in
      ///   radians.
      /// </summary>
      /// <param name="_txt">
      ///   latitude in a string format; must be in range -180..360
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisStrToLatitude ( const _txt      : String
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   <para>
      ///     Convert a latitude in a string representation into value in
      ///     radians.
      ///   </para>
      /// </summary>
      /// <param name="_txt">
      ///   latitude in a string format; must be in range -90..90
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function GisStrToLongitude( const _txt      : String
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Test if a given point is inside a given extent.
      /// </summary>
      /// <param name="_ptg">
      ///   point to test
      /// </param>
      /// <param name="_extent">
      ///   polygon to test
      /// </param>
      /// <returns>
      ///   True if point is located inside of the extent.
      /// </returns>
      class function GisIsPointInsideExtent
                                      ( const _ptg      : TGIS_Point ;
                                        const _extent   : TGIS_Extent
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$IFNDEF GIS_BASIC_PROJECTION}
        /// <summary>
        ///   Test if a given point is inside a given polygon.
        /// </summary>
        /// <param name="_ptg">
        ///   point to test
        /// </param>
        /// <param name="_shape">
        ///   polygon to test
        /// </param>
        /// <returns>
        ///   True if point is inside of polygon.
        /// </returns>
        /// <remarks>
        ///   See GisIsPointInsideExtent for example.
        /// </remarks>
        class function GisIsPointInsidePolygon
                                      ( const _ptg      : TGIS_Point ;
                                        const _shape    : TGIS_ShapePolygon
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$ENDIF}

      /// <summary>
      ///   Test if given set of lines have a common point.
      /// </summary>
      /// <param name="_line1">
      ///   first line
      /// </param>
      /// <param name="_line2">
      ///   second line
      /// </param>
      /// <returns>
      ///   True if two lines have common point.
      /// </returns>
      /// <remarks>
      ///   See GisIsPointInsideExtent for example.
      /// </remarks>
      class function GisIsLinesCommonPoint
                                      ( const _line1    : TGIS_Line ;
                                        const _line2    : TGIS_Line
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Get any common line point. Be certain to free result.
      /// </summary>
      /// <param name="_line1">
      ///   first line
      /// </param>
      /// <param name="_line2">
      ///   second line
      /// </param>
      /// <param name="_ptg">
      ///   crossing point
      /// </param>
      /// <returns>
      ///   True if lines are crossing each other.
      /// </returns>
      /// <remarks>
      ///   See GisIsPointInsideExtent for example.
      /// </remarks>
      class function GisGetLinesCrossing
                                      ( const _line1    : TGIS_Line ;
                                        const _line2    : TGIS_Line ;
                                        var   _ptg      : TGIS_Point
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create the shape from a Well Known Text (see:
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_wkt">
      ///   WKT text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDWKT
      /// </exception>
      /// <returns>
      ///   Shape created from WKT.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     This function will do basic check; if shape can not be
      ///     create then nil will be returned. Faster version of this
      ///     function is an overload which requires all parameters.
      ///   </note>
      /// </remarks>
      {#ownership:result:take}
      class function GisCreateShapeFromWKT
                                      ( const _wkt      : String
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create the shape from Well Known Binary (see:
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_wkb">
      ///   WKB array
      /// </param>
      /// <returns>
      ///   Shape created from WKB.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     This function will do basic check; if shape can not be
      ///     create then nil will be returned. Faster version of this
      ///     function is an overload which requires all parameters.
      ///   </note>
      /// </remarks>
      {#ownership:result:take}
      class function  GisCreateShapeFromWKB
                                      ( _wkb            : Variant
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create the shape from GeoMedia Database Object (GDO).
      /// </summary>
      /// <param name="_gdo">
      ///   GDO array
      /// </param>
      /// <returns>
      ///   Shape created from GDO.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     This function will do basic check; if shape can not be
      ///     create then nil will be returned. Faster version of this
      ///     function is an overload which requires all parameters.
      ///   </note>
      /// </remarks>
      {#ownership:result:take}
      class function  GisCreateShapeFromGDO
                                      ( _gdo            : Variant
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create the shape from JSON.
      /// </summary>
      /// <param name="_json">
      ///   JSON text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDJSON
      /// </exception>
      /// <returns>
      ///   Created shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     This function will do basic check; if shape can not be
      ///     create then nil will be returned. Faster version of this
      ///     function is an overload which requires all parameters.
      ///   </note>
      /// </remarks>
      {#ownership:result:take}
      class function  GisCreateShapeFromJSON
                                      ( const _json     : String
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Create the shape from GML.
      /// </summary>
      /// <param name="_gml">
      ///   JSON text
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_UNSUPPORTEDJSON
      /// </exception>
      /// <returns>
      ///   Created shape.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     This function will do basic check; if shape can not be
      ///     create then nil will be returned. Faster version of this
      ///     function is an overload which requires all parameters.
      ///   </note>
      /// </remarks>
      {#ownership:result:take}
      class function  GisCreateShapeFromGML
                                      ( const _gml      : String
                                      ) : TGIS_Shape ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into variant geometry. For
      ///   PostgeSQL ODBC driver purposes
      /// </summary>
      /// <param name="_shp">
      ///   exported Shape
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportGeometry2Hex
                                      (  const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   exported Shape
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportWKB2Hex
                                      (  const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPointToWKT
                                      ( const _shp      : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GML (see www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPointToGML
                                      ( const _shp      : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      class procedure GisExportPointToWKB
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _wkb     : System.Object
                                        {$ELSE}
                                          var _wkb     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object
      ///   (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportPointToGDO
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _gdo     : System.Object
                                        {$ELSE}
                                          var _gdo     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportPointToVAR
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _var     : System.Object
                                        {$ELSE}
                                          var _var     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoJSON (see
      ///   www.geojson.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPointToJSON
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportMultiPointToWKT
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GML (see www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportMultiPointToGML
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      class procedure GisExportMultiPointToWKB
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _wkb     : System.Object
                                        {$ELSE}
                                          var _wkb     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object
      ///   (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportMultiPointToGDO
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _gdo     : System.Object
                                        {$ELSE}
                                          var _gdo     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportMultiPointToVAR
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _var     : System.Object
                                        {$ELSE}
                                          var _var     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoJSON (see
      ///   www.geojson.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportMultiPointToJSON
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportArcToWKT
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GML (see www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportArcToGML
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      class procedure GisExportArcToWKB
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _wkb     : System.Object
                                        {$ELSE}
                                          var _wkb     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object
      ///   (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportArcToGDO
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _gdo     : System.Object
                                        {$ELSE}
                                          var _gdo     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportArcToVAR
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                         var _var     : System.Object
                                        {$ELSE}
                                          var _var     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoJSON (see
      ///   www.geojson.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportArcToJSON
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPolygonToWKT
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GML (see www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPolygonToGML
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      class procedure GisExportPolygonToWKB
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _wkb     : System.Object
                                        {$ELSE}
                                          var _wkb     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportPolygonToVAR
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _var     : System.Object
                                        {$ELSE}
                                          var _var     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object
      ///   (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportPolygonToGDO
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _gdo     : System.Object
                                        {$ELSE}
                                          var _gdo     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoJSON (see
      ///   www.geojson.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportPolygonToJSON
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Text (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportComplexToWKT
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GML (see www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportComplexToGML
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a Well Known Binary (see
      ///   www.opengis.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_wkb">
      ///   exported WKB array
      /// </param>
      class procedure GisExportComplexToWKB
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _wkb     : System.Object
                                        {$ELSE}
                                          var _wkb     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoMedia Database Object
      ///   (GDO).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_gdo">
      ///   exported GDO array
      /// </param>
      class procedure GisExportComplexToGDO
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _gdo     : System.Object
                                        {$ELSE}
                                          var _gdo     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into an internal SHP format.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_var">
      ///   exported VAR array
      /// </param>
      class procedure GisExportComplexToVAR
                                      ( const _shp     : TGIS_Shape    ;
                                        {$IFDEF CLR}
                                          var _var     : System.Object
                                        {$ELSE}
                                          var _var     : OleVariant
                                        {$ENDIF}
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Export the Shape geometry into a GeoJSON (see
      ///   www.geojson.org).
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <returns>
      ///   Shape geometry as string.
      /// </returns>
      class function  GisExportComplexToJSON
                                      ( const _shp     : TGIS_Shape
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Create a layer given by a name.
      /// </summary>
      /// <param name="_name">
      ///   the name under which the layer will be recognized; if empty,
      ///   then the default name will be assigned (same as path)
      /// </param>
      /// <param name="_path">
      ///   path (with extension)
      /// </param>
      /// <returns>
      ///   Created layer.
      /// </returns>
      {#ownership:result:take}
      class function GisCreateLayer   ( const _name     : String ;
                                        const _path     : String
                                      ) : TGIS_Layer ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Prepare the list of supported files for use by open/save dialog box.
      /// </summary>
      /// <param name="_mode">
      ///   mode of query (only vector files, all files etc.)
      /// </param>
      /// <param name="_save">
      ///   if true, the string for save dialog box context will be
      ///   prepared; if false then string for open dialog box
      ///   context will be prepared;
      /// </param>
      /// <returns>
      ///   string for dialog box filter in a format: names|ext|name|ext|...
      ///   or list of registered names in a format .EXT=format_nameCRLF
      /// </returns>
      /// <returns>
      ///   Supported files.
      /// </returns>
      class function GisSupportedFiles( _mode           : TGIS_FileTypes ;
                                        _save           : Boolean
                                      ) : String ; overload ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Updates resource string variables with the localized content
      ///   loaded from string.
      /// </summary>
      /// <param name="_localization">
      ///   string with localization KEY=TRANSLATION pairs
      /// </param>
      /// <param name="_errpath">
      ///   if not empty then missed symbols will be saved to the file
      ///   named _errpath
      /// </param>
      class procedure GisLocalizedStringsFromString
                                      ( const _localization
                                                        : String ;
                                        const _errpath  : String
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Updates resource string variables with the localized content
      ///   loaded from file
      /// </summary>
      /// <param name="_path">
      ///   name of the file with localization strings
      /// </param>
      /// <param name="_language">
      ///   name of the language
      /// </param>
      /// <param name="_errpath">
      ///   if not empty then missed symbols will be saved to the file
      ///   named _errpath
      /// </param>
      class procedure GisLocalizedStringsFromFile
                                      ( const _path     : String ;
                                        const _language : String ;
                                        const _errpath  : String
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Get charset for system (current).
      /// </summary>
      /// <returns>
      ///   Charset value.
      /// </returns>
      class function GisSystemCodePage: Integer   ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Access function for a general purpose data container.
      /// </summary>
      /// <returns>
      ///   Metadata object.
      /// </returns>
      class function GisMetadata      : TGIS_StringList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Read global metadata value
      /// </summary>
      /// <param name="_name">
      ///   _name of the value
      /// </param>
      /// <param name="_default">
      ///   default value to be used if value does not exist
      /// </param>
      /// <returns>
      ///   Read value or default.
      /// </returns>
      class function GisMetadataAsBoolean(
        const _name    : String ;
        const _default : Boolean
      ) : Boolean ;

      /// <summary>
      ///   Read global metadata value
      /// </summary>
      /// <param name="_name">
      ///   _name of the value
      /// </param>
      /// <param name="_default">
      ///   default value to be used if value does not exist
      /// </param>
      /// <returns>
      ///   Read value or default.
      /// </returns>
      class function GisMetadataAsInteger(
        const _name    : String ;
        const _default : Integer
      ) : Integer ;

      /// <summary>
      ///   Read global metadata value
      /// </summary>
      /// <param name="_name">
      ///   _name of the value
      /// </param>
      /// <param name="_default">
      ///   default value to be used if value does not exist
      /// </param>
      /// <returns>
      ///   Read value or default.
      /// </returns>
      class function GisMetadataAsFloat(
        const _name    : String ;
        const _default : Double
      ) : Double ;

      /// <summary>
      ///   Read global metadata value
      /// </summary>
      /// <param name="_name">
      ///   _name of the value
      /// </param>
      /// <param name="_default">
      ///   default value to be used if value does not exist
      /// </param>
      /// <returns>
      ///   Read value or default.
      /// </returns>
      class function GisMetadataAsString(
        const _name    : String ;
        const _default : String
      ) : String ;

      /// <summary>
      ///   Size which indicates that renderer must be used.
      /// </summary>
      /// <returns>
      ///   Size to be used by renderer.
      /// </returns>
      class function GIS_RENDER_SIZE  : Integer   ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_EQUALITY
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_DISJOINT
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT1
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT2
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT3
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_TOUCH_INTERIOR
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_TOUCH : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_CROSS : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_CROSS_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_WITHIN: String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_CONTAINS
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_OVERLAP
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_OVERLAP_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_LINE_CROSS_POLYGON
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_POLYGON_CROSSED_BY_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_LINE_CROSS_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_LINE_TRAVERS_POLYGON
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_POLYGON_TRAVERSED_BY_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_LINE_CROSSTRAVERS_POLYGON
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_POLYGON_CROSSTRAVERSED_BY_LINE
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT_INTERIOR_INTERIOR
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT_INTERIOR_BOUNDARY
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT_BOUNDARY_INTERIOR
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_INTERSECT_BOUNDARY_BOUNDARY
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_TOUCH_BOUNDARY_INTERIOR
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_TOUCH_INTERIOR_BOUNDARY
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Predefine DE-9IM array.
      /// </summary>
      /// <returns>
      ///   DE-9IM matrix.
      /// </returns>
      class function GIS_RELATE_TOUCH_BOUNDARY_BOUNDARY
                                      : String    ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: SINGLE, LEFTJUSTIFY, CENTER,
      ///   RIGHTJUSTIFY, FOLLOW
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamAlignment   ( const _value    : String ;
                                        const _default  : TGIS_LabelAlignment
                                      ) : TGIS_LabelAlignment ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate;
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamAlignment
                                      ( const _value    : TGIS_LabelAlignment
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; bitmap will be loaded into memory; please
      ///   remember to free it on your own;
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamBitmap      ( const _value    : String ;
                                        const _default  : TGIS_Bitmap
                                      ) : TGIS_Bitmap ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation
      /// </returns>
      class function ParamBoolean     ( const _value    : String;
                                        const _default  : Boolean
                                      ) : Boolean ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamBoolean
                                      ( const _value    : Boolean
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   : value to translate; can be\: * hexadecimal BGR color
      ///   ($0000FF for red) * integer BGR color (255 for red ) *
      ///   red\:green\:blue (255\:0\:0 for red) * color names:\ * AQUA *
      ///   BLACK * BLUE, * FUCHSIA, * GRAY * GREEN * LIME, * MAROON, *
      ///   NAVY * OLIVE, * PURPLE, * RED, * SILVER * TEAL * WHITE, *
      ///   YELLOW if value RENDER, then predefined value for renderer
      ///   will be assigned.
      /// </param>
      /// <param name="_default">
      ///   : default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamColor       ( const _value    : String ;
                                        const _default  : TGIS_Color
                                      ) : TGIS_Color ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamColor
                                      ( const _value    : TGIS_Color
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: PIE, BAR
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamChart       ( const _value    : String;
                                        const _default  : TGIS_ChartStyle
                                      ) : TGIS_ChartStyle ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamChart
                                      ( const _value    : TGIS_ChartStyle
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: OFF, STANDARD, AGGRESSIVE
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamDormant     ( const _value    : String;
                                        const _default  : TGIS_LayerDormantMode
                                      ) : TGIS_LayerDormantMode ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamDormant
                                      ( const _value    : TGIS_LayerDormantMode
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: val1:val2:...:valN (BOLD:ITALIC)
      ///   from set of NORMAL, BOLD, ITALIC, UNDERLINE, STRIKEOUT;
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamFontStyle   ( const _value    : String ;
                                        const _default  : TGIS_FontStyles
                                      ) : TGIS_FontStyles ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamFontStyle
                                      ( const _value    : TGIS_FontStyles
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; if value RENDER then predefined value for
      ///   renderer will be assigned.
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamInteger     ( const _value    : String;
                                        const _default  : Integer
                                      ) : Integer ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamInteger
                                      ( const _value    : Integer
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: BOX, CIRCLE, CROSS, DIAGCROSS,
      ///   TRIANGLEUP, TRIANGLEDOWN, TRIANGLELEFT, TRIANGLERIGHT
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamMarker      ( const _value    : String;
                                        const _default  : TGIS_MarkerStyle
                                      ) : TGIS_MarkerStyle ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate;
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamMarker
                                      ( const _value    : TGIS_MarkerStyle
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: SOLID, BDIAGONAL, FDIAGONAL,
      ///   CROSS, DIAGCROSS, HORIZONTAL, VERTICAL, TRANSPARENT
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamPattern     ( const _value    : String;
                                        const _default  : TGIS_BrushStyle
                                      ) : TGIS_BrushStyle ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamPattern
                                      ( const _value    : TGIS_BrushStyle
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: SOLID, BDIAGONAL, FDIAGONAL,
      ///   CROSS, DIAGCROSS, HORIZONTAL, VERTICAL, TRANSPARENT
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamSymbol      ( const _value    : String;
                                        const _default  : TGIS_SymbolAbstract
                                      ) : TGIS_SymbolAbstract ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: SOLID, DASH, DOT, DASHDOT,
      ///   DASHDOTDOT, CLEAR
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamPen         ( const _value    : String;
                                        const _default  : TGIS_PenStyle
                                      ) : TGIS_PenStyle ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamPen( const _value    : TGIS_PenStyle
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamFloat       ( const _value    : String;
                                        const _default  : Double
                                      ) : Double ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamFloat
                                      ( const _value    : Double
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; _value string is expected to be in C/C#
      ///   format: so '\\' will be converted to '\' and '\n' to CRLF
      ///   (new line - codes 13+10); if value starts with '
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamString      ( const _value    : String;
                                        const _default  : String
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamString
                                      ( const _value    : String
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a literal value into an internal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate; can be: UPLEFT, UPCENTER, UPRIGHT,
      ///   MIDDLELEFT, MIDDLECENTER, MIDDLERIGHT, DOWNLEFT, DOWNCENTER,
      ///   DOWNRIGHT, UPRRIGHTFIRST, UPLEFTFIRST, MIDDLEFIRST, FLOW, ANY
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   Internal representation.
      /// </returns>
      class function ParamPosition    ( const _value    : String;
                                        const _default  : TGIS_LabelPositions
                                      ) : TGIS_LabelPositions ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert an internal representation of a value into the
      ///   literal representation.
      /// </summary>
      /// <param name="_value">
      ///   value to translate
      /// </param>
      /// <returns>
      ///   Literal representation.
      /// </returns>
      class function ConstructParamPosition
                                      ( const _value    : TGIS_LabelPositions
                                      ) : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Set default topological operation mode.
      /// </summary>
      /// <param name="_mode">
      ///   If True then topological operations will force shape fixing.
      ///   See TGIS_Topology.ForceShapeFixing for details.
      /// </param>
      class procedure GisTopologyForceShapeFixing
                                      ( const _mode     : Boolean
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Set global proxy settings.
      /// </summary>
      /// <param name="_server">
      ///   Host to be used as a proxy
      /// </param>
      /// <param name="_port">
      ///   Port used to communicate to the proxy
      /// </param>
      /// <param name="_user">
      ///   UserName needed to be authenticated to the proxy
      /// </param>
      /// <param name="_pass">
      ///   PassWord needed to be authenticated to the proxy
      /// </param>
      /// <param name="_domain">
      ///   Domain to be used with the proxy
      /// </param>
      class procedure GisSetProxySettings
                                      ( const _server   : String   ;
                                        const _port     : Integer  ;
                                        const _user     : String   ;
                                        const _pass     : String   ;
                                        const _domain   : String
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Convert a String to extended.
      /// </summary>
      /// <param name="_val">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function  DotStrToFloat( const _val : String
                                   ) : Double ;
      /// <summary>
      ///   Convert an extended to String. DecimalSeparator will be a dot.
      /// </summary>
      /// <param name="_val">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Converted value.
      /// </returns>
      class function  DotFloatToStr( const _val : Double
                                   ) : String ;

      {$IFDEF CLR}
        {$IFDEF MSWINDOWS_OS}
        /// <summary>
        ///   Set PrivateFontCollection object to take into account by the WinForms renderer.
        /// </summary>
        /// <param name="_fonts">
        ///   private font collection
        /// </param>
        class procedure UsePrivateFontCollection
                                     ( const _fonts : System.Drawing.Text.PrivateFontCollection
                                     ) ; overload ;

        /// <summary>
        ///   Create a private font collection.
        /// </summary>
        /// <param name="_fonts">
        ///   list of font file paths
        /// </param>
        class procedure UsePrivateFontCollection
                                     ( const _fonts : List<String>
                                     ) ; overload ;
        {$ENDIF}
      {$ENDIF}

      /// <summary>
      ///   Set license code.
      /// </summary>
      /// <param name="_code">
      ///   license code
      /// </param>
      class procedure SetLicense  ( const _code : String
                                  ) ;
      {$IFNDEF JAVA OR ISLAND OR BLAZOR}
        /// <summary>
        ///   Returns a global OpenCL engine object.
        /// </summary>
        /// <returns>
        ///   OpenCL engine object.
        /// </returns>
        class function  GisOpenCLEngine : TGIS_OpenCLEngine ;
      {$ENDIF}

      {$IFNDEF JAVA OR ISLAND or BLAZOR}
        /// <summary>
        ///   Returns a global OpenCL engine object.
        /// </summary>
        /// <returns>
        ///   OpenCL engine object.
        /// </returns>
        class function  GisCUDAEngine : TGIS_CUDAEngine ;
      {$ENDIF}


      /// <summary>
      ///   Get location of executable file.
      /// </summary>
      /// <returns>
      ///   Path to the calling process.
      /// </returns>
      class function ExecutingFolder : String ;

      {$IFDEF JAVA}
        /// <summary>
        ///   Set search path to files of additional third party libraries
        ///   required by some layers.
        /// </summary>
        /// <param name="_path">
        ///   path to libraries
        /// </param>
        class procedure SetLibrariesSearchPath( const _path : String
                                              ) ;

      {$ENDIF}


      /// <summary>
      ///   Calculate sun's position based on time, date, and location on Earth
      /// </summary>
      /// <param name="_ptg">
      ///   observer's location in geographic coordinate system expressed in radians
      /// </param>
      /// <param name="_utc_time">
      ///   observer's time given in UTC (Coordinated Universal Time);
      /// </param>
      /// <param name="_altitude">
      ///   angular height in radians of the sun in the sky measured from the
      ///   horizontal; values from 0 at sunrise and sunset to Pi/2 at solar noon
      ///   (also known as an elevation angle)
      /// </param>
      /// <param name="_azimuth">
      ///   angle in radians in the horizontal plane measured clockwise from north
      ///   to the horizontal projection of the sun's rays; values from 0 to 2*Pi
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     This method is recommended to use to avoid the confusion
      ///     associated with time zones and daylight saving time.
      ///   </note>
      /// </remarks>
      class procedure  GisSunPosition     ( const _ptg        : TGIS_Point ;
                                            const _utc_time   : TDateTime ;
                                            var   _altitude   : Double ;
                                            var   _azimuth    : Double
                                          ) ; overload ;

      /// <summary>
      ///   Calculate sun's position based on time, date, and location on Earth
      /// </summary>
      /// <param name="_ptg">
      ///   observer's location in geographic coordinate system expressed in radians
      /// </param>
      /// <param name="_local_time">
      ///   observer's local time
      /// </param>
      /// <param name="_utc_offset">
      ///    time difference in hours between UTC and local time for a particular
      ///    place and date; Daylight Saving Time must be taken into account.
      /// </param>
      /// <param name="_altitude">
      ///   angular height in radians of the sun in the sky measured from the
      ///   horizontal; values from 0 at sunrise and sunset to 90 at solar noon
      ///   (also known as an elevation angle)
      /// </param>
      /// <param name="_azimuth">
      ///   angle in radians in the horizontal plane measured clockwise from north
      ///   to the horizontal projection of the sun's rays; values from 0 to 2*Pi
      /// </param>
      class procedure  GisSunPosition     ( const _ptg        : TGIS_Point ;
                                            const _local_time : TDateTime ;
                                            const _utc_offset : Double ;
                                            var   _altitude   : Double ;
                                            var   _azimuth    : Double
                                          ) ; overload ;
      /// <summary>
      ///   Create a color map for ramps.
      /// </summary>
      /// <param name="_index">
      ///   Range value related with RGB color, normalized to 0-100
      /// </param>
      /// <param name="_color">
      ///   Color assigned to given range.
      /// </param>
      /// <returns>
      ///   color map
      /// </returns>
      class function GisColorMap          ( const _index : Double ;
                                            const _color : TGIS_Color
                                          ) : TGIS_ColorMap ;

      /// <summary>
      ///   Create extended color map for ramps.
      /// </summary>
      /// <param name="_class">
      ///   Class value related with ramp
      /// </param>
      /// <param name="_colors">
      ///   Array of colors assigned to given class.
      /// </param>
      /// <returns>
      ///   extended color map
      /// </returns>
      class function GisColorMapEx        ( const _class  : Integer ;
                                            const _colors : TGIS_ColorArray
                                          ) : TGIS_ColorMapEx ;

      /// <summary>
      ///   Initialize grid array and set values to NODATA
      /// </summary>
      /// <param name="_height">
      ///   height of the grid
      /// </param>
      /// <param name="_width">
      ///   width of the grid
      /// </param>
      /// <returns>
      ///   Initialized grid.
      /// </returns>
      class function GisGridArray         ( const _height : Integer ;
                                            const _width  : Integer
                                          ) : TGIS_GridArray ;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Initialize a bitmap factory.
      /// </summary>
      /// <returns>
      ///   bitmap factory
      /// </returns>
      class function GisBitmapFactoryUniversal
                                          : TGIS_BitmapFactory ;

    protected
      class function  fget_CSUnitsList: TGIS_CSUnitsList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSPrimeMeridianList
                                      : TGIS_CSPrimeMeridianList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSEllipsoidList
                                      : TGIS_CSEllipsoidList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSTransformList
                                      : TGIS_CSTransformList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSDatumList: TGIS_CSDatumList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSAreaList : TGIS_CSAreaList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSProjList : TGIS_CSProjList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSGeographicCoordinateSystemList
                                      : TGIS_CSGeographicCoordinateSystemList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSProjectedCoordinateSystemList
                                      : TGIS_CSProjectedCoordinateSystemList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_CSUnknownCoordinateSystem
                                      : TGIS_CSUnknownCoordinateSystem ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_RendererManager : TGIS_RendererManager ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_SymbolList : TGIS_SymbolList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisPasswordList
                                      : TGIS_PasswordList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisKeyList : TGIS_PasswordList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisAliasList
                                      : TGIS_AliasList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisColorRampList
                                      : TGIS_ColorRampList ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisEnvironmentInfo
                                      : TGIS_EnvironmentInfo ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_ForcedRTreePath
                                      : String ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class procedure fset_ForcedRTreePath
                                      ( const _value : String
                                      ) ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      class function  fget_GisProxySettings
                                      : TGIS_ProxySettings ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$IFNDEF ANDROID_JAVA}
        class function  fget_PipelineOperations
                                      : TGIS_PipelineOperations ;
                                      static;
                                      {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$ENDIF}

    public

        /// <summary>
        ///   Global Instance of list of all units.
        /// </summary>
        class property  CSUnitsList
                        : TGIS_CSUnitsList
                          read fget_CSUnitsList ;

        /// <summary>
        ///   Global Instance of list of all prime meridians.
        /// </summary>
        class property CSPrimeMeridianList
                       : TGIS_CSPrimeMeridianList
                         read fget_CSPrimeMeridianList ;

        /// <summary>
        ///   Global Instance of list of all ellipsoids.
        /// </summary>
        class property CSEllipsoidList
                       : TGIS_CSEllipsoidList
                         read fget_CSEllipsoidList ;

        /// <summary>
        ///   Global Instance of list of all transformations.
        /// </summary>
        class property CSTransformList
                       : TGIS_CSTransformList
                         read fget_CSTransformList ;

        /// <summary>
        ///   Global Instance of list of all datums.
        /// </summary>
        class property CSDatumList
                       : TGIS_CSDatumList
                         read fget_CSDatumList ;

        /// <summary>
        ///   Global Instance of list of all areas.
        /// </summary>
        class property CSAreaList
                       : TGIS_CSAreaList
                         read fget_CSAreaList ;

        /// <summary>
        ///   Global Instance of list of all projections.
        /// </summary>
        class property CSProjList
                       : TGIS_CSProjList
                         read fget_CSProjList ;

        /// <summary>
        ///   Global Instance of list of all GCS.
        /// </summary>
        class property CSGeographicCoordinateSystemList
                       : TGIS_CSGeographicCoordinateSystemList
                         read fget_CSGeographicCoordinateSystemList ;

        /// <summary>
        ///   Global Instance of list of all PCS.
        /// </summary>
        class property CSProjectedCoordinateSystemList
                       : TGIS_CSProjectedCoordinateSystemList
                         read fget_CSProjectedCoordinateSystemList ;

        /// <summary>
        ///   Global Instance of UNKNOWN cs (1 object only).
        /// </summary>
        class property CSUnknownCoordinateSystem
                       : TGIS_CSUnknownCoordinateSystem
                         read fget_CSUnknownCoordinateSystem ;

        /// <summary>
        ///   List of registered renderers.
        /// </summary>
        class property RendererManager
                       : TGIS_RendererManager
                         read fget_RendererManager ;

        /// <summary>
        ///   List of symbols.
        /// </summary>
        class property SymbolList
                       : TGIS_SymbolList
                         read fget_SymbolList ;

        /// <summary>
        ///   PasswordList storage.
        /// </summary>
        class property GisPasswordList
                       : TGIS_PasswordList
                         read fget_GisPasswordList ;

        /// <summary>
        ///   KeyList storage.
        /// </summary>
        class property GisKeyList
                       : TGIS_PasswordList
                         read fget_GisKeyList ;

        /// <summary>
        ///   AliasList storage.
        /// </summary>
        class property GisAliasList
                       : TGIS_AliasList
                         read fget_GisAliasList ;

        /// <summary>
        ///   Color ramps list.
        /// </summary>
        /// <remarks>
        ///   Color ramp names are available through the TGIS_ColorRampNames class.
        /// </remarks>
        class property GisColorRampList
                       : TGIS_ColorRampList
                         read fget_GisColorRampList ;
        /// <summary>
        ///   Runtime environment info.
        /// </summary>
        class property GisEnvironmentInfo
                       : TGIS_EnvironmentInfo
                         read fget_GisEnvironmentInfo ;

        /// <summary>
        ///   Forced RTree Path.
        /// </summary>
        class property ForcedRTreePath
                       : String
                         read  fget_ForcedRTreePath
                         write fset_ForcedRTreePath ;

        /// <summary>
        ///   Proxy Settings.
        /// </summary>
        class property GisProxySettings
                       : TGIS_ProxySettings
                         read  fget_GisProxySettings ;

        {$IFNDEF ANDROID_JAVA}
          /// <summary>
          ///   Global list of all defined pipeline's operations.
          /// </summary>
          class property PipelineOperations
                         : TGIS_PipelineOperations
                           read  fget_PipelineOperations ;
        {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.Math,

  GisRtl,
  GisBaseObject,
  GisParams,
  GisInternals,
  GisTopology,
  GisContour,
  GisResourceEx,
  GisGeometryFactory,
  GisRegistredLayers ;
{$ENDIF}

{$IFDEF GIS_XDK}
class function TGIS_Utils.Point(
  const _x              : Integer  ;
  const _y              : Integer
) : TPoint ;
begin
  Result := System.Types.Point( _x, _y ) ;
end ;

class function TGIS_Utils.Rect(
  const _left             : Integer  ;
  const _top              : Integer  ;
  const _right            : Integer  ;
  const _bottom           : Integer
) : TRect ;
begin
  Result := System.Types.Rect( _left, _top, _right, _bottom ) ;
end ;
{$ENDIF}

class function TGIS_Utils.GisPoint( const _x      : Double  ;
                                    const _y      : Double
                                  ) : TGIS_Point ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint( _x, _y ) ;
end ;

class function TGIS_Utils.GisPoint3D(
 const  _x : Double ;
 const  _y : Double ;
 const  _z : Double
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint3D( _x, _y, _z ) ;
end;

class function TGIS_Utils.GisPoint3D(
  const _x : Double ;
  const _y : Double ;
  const _z : Double ;
  const _m : Double
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint3D( _x, _y, _z, _m ) ;
end ;

class function TGIS_Utils.GisPoint3DFrom2D(
  const _ptg: TGIS_Point
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint3DFrom2D( _ptg ) ;
end ;

class function TGIS_Utils.GisPoint2DFrom3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint2DFrom3D( _ptg ) ;
end ;

class function TGIS_Utils.GisLine(
  const _a : TGIS_Point ;
  const _b : TGIS_Point
) : TGIS_Line ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine( _a, _b ) ;
end;

class function TGIS_Utils.GisLine(
  const _ax : Double ;
  const _ay : Double ;
  const _bx : Double ;
  const _by : Double
) : TGIS_Line ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine( _ax, _ay, _bx, _by ) ;
end;

class function TGIS_Utils.GisLine3D(
  const _a : TGIS_Point3D ;
  const _b : TGIS_Point3D
) : TGIS_Line3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine3D( _a, _b ) ;
end;

class function TGIS_Utils.GisExtent3DFrom2D(
  const _ext : TGIS_Extent
) : TGIS_Extent3D;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtent3DFrom2D( _ext ) ;
end ;

class function TGIS_Utils.GisExtent2DFrom3D(
  const _ext : TGIS_Extent3D
) : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtent2DFrom3D( _ext ) ;
end ;

class function TGIS_Utils.GisExtent(
  const _xmin : Double ;
  const _ymin : Double ;
  const _xmax : Double ;
  const _ymax : Double
) : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtent( _xmin, _ymin, _xmax, _ymax ) ;
end ;

class function TGIS_Utils.GisExtent3D(
  const _xmin : Double ;
  const _ymin : Double ;
  const _zmin : Double ;
  const _xmax : Double ;
  const _ymax : Double ;
  const _zmax : Double
) : TGIS_Extent3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtent3D( _xmin, _ymin, _zmin, _xmax, _ymax, _zmax ) ;
end ;

class function TGIS_Utils.GisExtentArea(
  const _extent : TGIS_Extent
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtentArea( _extent ) ;
end ;

class function TGIS_Utils.GisExtentArea3D(
  const _extent : TGIS_Extent3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtentArea3D( _extent ) ;
end ;

class function TGIS_Utils.GisExtentCubature(
  const _extent : TGIS_Extent
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtentCubature( _extent ) ;
end ;

class function TGIS_Utils.GisExtentCubature3D(
  const _extent : TGIS_Extent3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisExtentCubature3D( _extent ) ;
end ;

class function TGIS_Utils.GisIsSamePoint(
  const _ptg1 : TGIS_Point ;
  const _ptg2 : TGIS_Point
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint( _ptg1, _ptg2 ) ;
end ;

class function TGIS_Utils.GisIsSamePoint(
  const _ptg1 : TGIS_Point ;
  const _ptg2 : TGIS_Point ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint( _ptg1, _ptg2, _tol ) ;
end ;

class function TGIS_Utils.GisIsSamePoint3D(
  const _ptg1 : TGIS_Point3D ;
  const _ptg2 : TGIS_Point3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint3D( _ptg1, _ptg2 ) ;
end ;

class function TGIS_Utils.GisIsSamePoint3D(
  const _ptg1 : TGIS_Point3D ;
  const _ptg2 : TGIS_Point3D ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint3D( _ptg1, _ptg2, _tol ) ;
end ;

class function TGIS_Utils.GisIsSamePoint3DM(
  const _ptg1 : TGIS_Point3D ;
  const _ptg2 : TGIS_Point3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint3DM( _ptg1, _ptg2 ) ;
end ;

class function TGIS_Utils.GisIsSamePoint3DM(
  const _ptg1 : TGIS_Point3D ;
  const _ptg2 : TGIS_Point3D ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSamePoint3DM( _ptg1, _ptg2, _tol ) ;
end ;

class function TGIS_Utils.GisIsSameExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsSameExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent( _extent1, _extent2, _tol ) ;
end ;

class function TGIS_Utils.GisIsSameExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent3D( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsSameExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent3D( _extent1, _extent2, _tol ) ;
end ;

class function TGIS_Utils.GisIsSameExtent3DM(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent3DM( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsSameExtent3DM(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D ;
  const _tol  : Double
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsSameExtent3DM( _extent1, _extent2, _tol ) ;
end ;

class function TGIS_Utils.GisContainExtent(
  const _extent1 : TGIS_Extent;
  const _extent2 : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisContainExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsEmptyExtent(
  const _extent : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsEmptyExtent( _extent ) ;
end;

class function TGIS_Utils.GisIsEmptyExtent3D(
  const _extent : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsEmptyExtent3D( _extent ) ;
end;

class function TGIS_Utils.GisWholeWorld
 : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisWholeWorld ;
end ;

class function TGIS_Utils.GisWholeWorld3D
 : TGIS_Extent3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisWholeWorld3D ;
end ;

class function TGIS_Utils.GisIsWholeWorld(
  const _extent : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsWholeWorld( _extent ) ;
end ;

class function TGIS_Utils.GisIsWholeWorld3D(
  const _extent : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsWholeWorld3D( _extent ) ;
end ;

class function TGIS_Utils.GisNoWorld
 : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisNoWorld ;
end ;

class function TGIS_Utils.GisNoWorld3D
 : TGIS_Extent3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisNoWorld3D ;
end ;

class function TGIS_Utils.GisIsNoWorld(
  const _extent : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsNoWorld( _extent ) ;
end ;

class function TGIS_Utils.GisIsNoWorld3D(
  const _extent: TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsNoWorld3D( _extent ) ;
end ;

class function TGIS_Utils.GisDMax(
  const _val1 : Double ;
  const _val2 : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisDMax( _val1, _val2 ) ;
end ;

class function TGIS_Utils.GisDMin(
  const _val1 : Double ;
  const _val2 : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisDMin( _val1, _val2 ) ;
end ;

class function TGIS_Utils.GisMaxExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMaxExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisMaxExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : TGIS_Extent3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMaxExtent3D( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsCommonExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsCommonExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsCommonExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsCommonExtent3D( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsContainExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsContainExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisIsContainExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsContainExtent3D( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisCommonExtent(
  const _extent1 : TGIS_Extent ;
  const _extent2 : TGIS_Extent
) : TGIS_Extent ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisCommonExtent( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisCommonExtent3D(
  const _extent1 : TGIS_Extent3D ;
  const _extent2 : TGIS_Extent3D
) : TGIS_Extent3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisCommonExtent3D( _extent1, _extent2 ) ;
end ;

class function TGIS_Utils.GisLineLength(
  const _lineA : TGIS_Point ;
  const _lineB : TGIS_Point
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLineLength( _lineA, _lineB ) ;
end ;

class function TGIS_Utils.GisPoint2Point(
  const _ptg1 : TGIS_Point ;
  const _ptg2 : TGIS_Point
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint2Point( _ptg1, _ptg2 ) ;
end ;

class function TGIS_Utils.GisPoint2Point3D(
  const _ptg1 : TGIS_Point3D ;
  const _ptg2 : TGIS_Point3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPoint2Point3D( _ptg1, _ptg2 ) ;
end ;

class function TGIS_Utils.GisLine2Point(
  const _lineA : TGIS_Point ;
  const _lineB : TGIS_Point ;
  const _ptg   : TGIS_Point
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2Point( _lineA, _lineB, _ptg ) ;
end ;

class function TGIS_Utils.GisLine2PointFuzzy(
  const _lineA    : TGIS_Point ;
  const _lineB    : TGIS_Point ;
  const _ptg      : TGIS_Point
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2PointFuzzy( _lineA, _lineB, _ptg ) ;
end ;

class function TGIS_Utils.GisLine2Point3D(
  const _lineA : TGIS_Point3D ;
  const _lineB : TGIS_Point3D ;
  const _ptg   : TGIS_Point3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2Point3D( _lineA, _lineB, _ptg ) ;
end ;

class function TGIS_Utils.GisLine2Point3DFuzzy(
  const _lineA    : TGIS_Point3D ;
  const _lineB    : TGIS_Point3D ;
  const _ptg      : TGIS_Point3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2Point3DFuzzy( _lineA, _lineB, _ptg ) ;
end;

class function TGIS_Utils.GisLine2Line(
  const _line1A : TGIS_Point ;
  const _line1B : TGIS_Point ;
  const _line2A : TGIS_Point ;
  const _line2B : TGIS_Point
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2Line( _line1A, _line1B, _line2A, _line2B ) ;
end ;

class function TGIS_Utils.GisLine2Line3D(
  const _line1A : TGIS_Point3D ;
  const _line1B : TGIS_Point3D ;
  const _line2A : TGIS_Point3D ;
  const _line2B : TGIS_Point3D
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLine2Line3D( _line1A, _line1B, _line2A, _line2B ) ;
end ;

class function TGIS_Utils.GisPointOnLine(
  const _lineA : TGIS_Point ;
  const _lineB : TGIS_Point ;
  const _ptg   : TGIS_Point
) : TGIS_Point ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPointOnLine( _lineA, _lineB, _ptg ) ;
end ;

class function TGIS_Utils.GisPointOnLine3D(
  const _lineA : TGIS_Point3D ;
  const _lineB : TGIS_Point3D ;
  const _ptg   : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPointOnLine3D( _lineA, _lineB, _ptg ) ;
end ;

class function TGIS_Utils.GisPointsDelta(
  const _pt1 : TGIS_Point ;
  const _pt2 : TGIS_Point
): TGIS_Point;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPointsDelta( _pt1, _pt2 ) ;
end ;

class function TGIS_Utils.GisPointsDelta3D(
  const _pt1 : TGIS_Point3D ;
  const _pt2 : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPointsDelta3D( _pt1, _pt2 ) ;
end ;

class function TGIS_Utils.GisMovePoint(
  const _ptg   : TGIS_Point ;
  const _delta : TGIS_Point
): TGIS_Point;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMovePoint( _ptg, _delta ) ;
end ;

class function TGIS_Utils.GisMovePoint3D(
  const _ptg   : TGIS_Point3D ;
  const _delta : TGIS_Point3D
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMovePoint3D( _ptg, _delta ) ;
end ;

class function TGIS_Utils.GisScalePoint(
  const _ref   : TGIS_Point ;
  const _ptg   : TGIS_Point ;
        _scale : Double
) : TGIS_Point ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisScalePoint( _ref, _ptg, _scale ) ;
end ;

class function TGIS_Utils.GisScalePoint3D(
  const _ref   : TGIS_Point3D ;
  const _ptg   : TGIS_Point3D ;
        _scale : Double
) : TGIS_Point3D ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisScalePoint3D( _ref, _ptg ,_scale ) ;
end ;

class function TGIS_Utils.GisRotatePoint(
  const _ref   : TGIS_Point ;
  const _ptg   : TGIS_Point ;
        _angle : Double
) : TGIS_Point ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisRotatePoint( _ref, _ptg, _angle ) ;
end ;

class function TGIS_Utils.GisCenterPoint(
  const _extent : TGIS_Extent
) : TGIS_Point ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisCenterPoint( _extent ) ;
end ;

class function TGIS_Utils.GisRadToCompass(
 const  _angle : Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisRadToCompass( _angle ) ;
end ;


class function TGIS_Utils.GisCreateReprojectedShape(
  const _shp     : TGIS_Shape               ;
  const _src_cs  : TGIS_CSCoordinateSystem  ;
  const _dst_cs  : TGIS_CSCoordinateSystem
) : TGIS_Shape ;
begin
  Result := {$IFDEF DCC}GisGeometryFactory.{$ENDIF}TGIS_GeometryFactory.GisCreateReprojectedShape( _shp,
                                                _src_cs,
                                                _dst_cs
                                              ) ;
end ;

class function TGIS_Utils.GisBuildPolygonFromEdges(
  const _edges     : TGIS_ObjectList ;
  const _tolerance : Double ;
  const _source    : TGIS_Shape ;
  {$IFDEF MANAGED}
    const _ptr     : TGIS_Bytes ;
  {$ELSE}
    const _ptr     : Pointer    ;
  {$ENDIF}
  const _mapped    : Boolean    ;
  const _uid       : Integer    ;
  const _layer     : TGIS_LayerVector ;
  const _fixShape  : Boolean
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisBuildShapeFromEdges(
              _edges,
              TGIS_ShapeType.Polygon,
             _tolerance,
             _source,
             _ptr,
             _mapped,
             _uid,
             _layer,
             _fixShape
           ) ;
end ;

class function TGIS_Utils.GisBuildLineStringFromEdges(
  const _edges     : TGIS_ObjectList ;
  const _tolerance : Double ;
  const _source    : TGIS_Shape ;
  {$IFDEF MANAGED}
    const _ptr     : TGIS_Bytes ;
  {$ELSE}
    const _ptr     : Pointer    ;
  {$ENDIF}
  const _mapped    : Boolean    ;
  const _uid       : Integer    ;
  const _layer     : TGIS_LayerVector ;
  const _fixShape  : Boolean
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisBuildShapeFromEdges(
              _edges,
              TGIS_ShapeType.Arc,
             _tolerance,
             _source,
             _ptr,
             _mapped,
             _uid,
             _layer,
             _fixShape
           ) ;
end ;

class function TGIS_Utils.GisCircleFrom3Points(
  const _ptg1   : TGIS_Point ;
  const _ptg2   : TGIS_Point ;
  const _ptg3   : TGIS_Point ;
  var _center   : TGIS_Point ;
  var _radius   : Double     ;
  var _start    : Double     ;
  var _stop     : Double
) : Boolean;
begin
  Result := TGIS_GeometryFactory.GisCircleFrom3Points(
              _ptg1, _ptg2, _ptg3, _center, _radius, _start, _stop
            ) ;
end ;

class function TGIS_Utils.GisCircleFrom3Points3D(
  const _ptg1   : TGIS_Point3D ;
  const _ptg2   : TGIS_Point3D ;
  const _ptg3   : TGIS_Point3D ;
  var _center   : TGIS_Point3D ;
  var _radius   : Double     ;
  var _start    : Double     ;
  var _stop     : Double
) : Boolean ;
begin
  Result := TGIS_GeometryFactory.GisCircleFrom3Points3D(
              _ptg1, _ptg2, _ptg3, _center, _radius, _start, _stop
            ) ;
end ;

class function TGIS_Utils.GisArcFrom3Points(
  const _ptg1   : TGIS_Point ;
  const _ptg2   : TGIS_Point ;
  const _ptg3   : TGIS_Point ;
  var _center   : TGIS_Point ;
  var _radius   : Double     ;
  var _start    : Double     ;
  var _stop     : Double
) : Boolean ;
begin
  Result := TGIS_GeometryFactory.GisArcFrom3Points(
              _ptg1, _ptg2, _ptg3, _center, _radius, _start, _stop
            ) ;
end ;

class function TGIS_Utils.GisArcFrom3Points3D(
  const _ptg1   : TGIS_Point3D ;
  const _ptg2   : TGIS_Point3D ;
  const _ptg3   : TGIS_Point3D ;
  var _center   : TGIS_Point3D ;
  var _radius   : Double     ;
  var _start    : Double     ;
  var _stop     : Double
) : Boolean ;
begin
  Result := TGIS_GeometryFactory.GisArcFrom3Points3D(
              _ptg1, _ptg2, _ptg3, _center, _radius, _start, _stop
            ) ;
end ;


class function TGIS_Utils.GisDefaultField(
  const _type : TGIS_FieldType
) : Variant ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisDefaultField( _type ) ;
end ;

class function TGIS_Utils.GisSamplesDataDir
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSamplesDataDir ;
end ;

class function TGIS_Utils.GisSamplesDataDirDownload
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSamplesDataDirDownload ;
end ;

class function TGIS_Utils.GisPathRelative(const _dir: String; const _path : String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisInternals{$ENDIF}.GetPathRelative(_dir, _path) ;
end;

class function TGIS_Utils.GisFilePath(const _path : String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisInternals{$ENDIF}.GetFilePath(_path) ;
end;

class function TGIS_Utils.GisCanonicalSQLName(const _name: String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisCanonicalSQLName(_name) ;
end;

class function TGIS_Utils.GisCanonicalSQLName(const _name: String; const _template: String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisCanonicalSQLName(_name, _template) ;
end;

class function TGIS_Utils.GisNormalizedSQLName(const _name: String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisNormalizedSQLName(_name) ;
end;

class function TGIS_Utils.GisDeNormalizedSQLName(const _name: String): String;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisDeNormalizedSQLName(_name) ;
end;

class procedure TGIS_Utils.GisDecodeAngle(
  const _val  : Double  ;
  var   _deg  : Integer ;
  var   _min  : Integer ;
  var   _sec  : Integer ;
  var   _frac : Integer ;
  var   _sign : Integer
) ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeAngle( _val, _deg, _min, _sec, _frac, s ) ;
  _sign := s ;
end ;



class procedure TGIS_Utils.GisDecodeAngle(
  const _val      : Double     ;
  var   _deg      : Integer    ;
  var   _min      : Integer    ;
  var   _sec      : Integer    ;
  var   _frac     : Integer    ;
  var   _sign     : Integer    ;
  const _prec     : Integer
) ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeAngle( _val, _deg, _min, _sec, _frac, s, _prec ) ;
  _sign := s ;
end;


class procedure TGIS_Utils.GisDecodeLatitude(
  const _val  : Double  ;
  var   _deg  : Integer ;
  var   _min  : Integer ;
  var   _sec  : Integer ;
  var   _frac : Integer ;
  var   _sign : Integer
)  ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeLatitude( _val, _deg, _min, _sec, _frac, s ) ;
  _sign := s ;
end ;



class procedure TGIS_Utils.GisDecodeLatitude(
  const _val  : Double  ;
  var   _deg  : Integer ;
  var   _min  : Integer ;
  var   _sec  : Integer ;
  var   _frac : Integer ;
  var   _sign : Integer ;
  const _prec : Integer
)  ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeLatitude( _val, _deg, _min, _sec, _frac, s, _prec ) ;
  _sign := s ;
end ;


class procedure TGIS_Utils.GisDecodeLongitude(
  const _val  : Double  ;
  var   _deg  : Integer ;
  var   _min  : Integer ;
  var   _sec  : Integer ;
  var   _frac : Integer ;
  var   _sign : Integer
) ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeLongitude( _val, _deg, _min, _sec, _frac, s ) ;
  _sign := s ;
end ;



class procedure TGIS_Utils.GisDecodeLongitude(
  const _val  : Double  ;
  var   _deg  : Integer ;
  var   _min  : Integer ;
  var   _sec  : Integer ;
  var   _frac : Integer ;
  var   _sign : Integer ;
  const _prec : Integer
)  ;
var
  s : TValueSign ;
begin
  s := _sign ;
  {$IFDEF OXYGENE}__Global.{$ELSE}GisFunctions.{$ENDIF}GisDecodeLongitude( _val, _deg, _min, _sec, _frac, s, _prec ) ;
  _sign := s ;
end ;


class function TGIS_Utils.GisEncodeAngle(
  const _deg : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeAngle( _deg ) ;
end ;

class function TGIS_Utils.GisEncodeAngle(
  const _deg : Double ;
  const _min : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeAngle( _deg, _min ) ;
end ;

class function TGIS_Utils.GisEncodeAngle(
  const _deg : Double ;
  const _min : Double ;
  const _sec : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeAngle( _deg, _min, _sec ) ;
end ;

class function TGIS_Utils.GisEncodeAngle(
  const _deg  : Double ;
  const _min  : Double ;
  const _sec  : Double ;
  const _sign : Integer
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeAngle( _deg, _min, _sec, _sign ) ;
end ;

class function TGIS_Utils.GisEncodeLatitude(
  const _deg  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLatitude( _deg ) ;
end;

class function TGIS_Utils.GisEncodeLatitude(
  const _deg  : Double ;
  const _min  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLatitude( _deg, _min ) ;
end;

class function TGIS_Utils.GisEncodeLatitude(
  const _deg  : Double ;
  const _min  : Double ;
  const _sec  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLatitude( _deg, _min, _sec ) ;
end;

class function TGIS_Utils.GisEncodeLatitude(
  const _deg  : Double ;
  const _min  : Double ;
  const _sec  : Double ;
  const _sign : Integer
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLatitude( _deg, _min, _sec, _sign ) ;
end;

class function TGIS_Utils.GisEncodeLongitude(
  const _deg  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLongitude( _deg ) ;
end;

class function TGIS_Utils.GisEncodeLongitude(
  const _deg  : Double ;
  const _min  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLongitude( _deg, _min ) ;
end;

class function TGIS_Utils.GisEncodeLongitude(
  const _deg  : Double ;
  const _min  : Double ;
  const _sec  : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLongitude( _deg, _min, _sec ) ;
end;

class function TGIS_Utils.GisEncodeLongitude(
  const _deg  : Double ;
  const _min  : Double ;
  const _sec  : Double ;
  const _sign : Integer
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEncodeLongitude( _deg, _min, _sec, _sign ) ;
end;

class function TGIS_Utils.GisAngleToStr(
  const _val : Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisAngleToStr( _val ) ;
end ;

class function TGIS_Utils.GisAngleToStr(
  const _val    : Double  ;
  const _spaces : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisAngleToStr( _val, _spaces ) ;
end ;

class function TGIS_Utils.GisAngleToStr(
  const _val    : Double  ;
  const _spaces : Boolean ;
  const _prec   : Integer
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisAngleToStr( _val, _spaces, _prec ) ;
end ;

class function TGIS_Utils.GisLatitudeToStr(
  const _val: Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLatitudeToStr( _val ) ;
end ;

class function TGIS_Utils.GisLatitudeToStr(
  const _val    : Double  ;
  const _spaces : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLatitudeToStr( _val, _spaces ) ;
end ;

class function TGIS_Utils.GisLatitudeToStr(
  const _val    : Double  ;
  const _spaces : Boolean ;
  const _prec   : Integer
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLatitudeToStr( _val, _spaces, _prec ) ;
end ;

class function TGIS_Utils.GisLatitudeToStr(
  const _val     : Double  ;
  const _spaces  : Boolean ;
  const _prec    : Integer ;
  const _leading : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLatitudeToStr( _val, _spaces, _prec, _leading ) ;
end ;

class function TGIS_Utils.GisLongitudeToStr(
  const _val: Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLongitudeToStr( _val ) ;
end ;

class function TGIS_Utils.GisLongitudeToStr(
  const _val    : Double     ;
  const _spaces : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLongitudeToStr( _val, _spaces ) ;
end ;

class function TGIS_Utils.GisLongitudeToStr(
  const _val    : Double     ;
  const _spaces : Boolean    ;
  const _prec   : Integer
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLongitudeToStr( _val, _spaces, _prec ) ;
end ;

class function TGIS_Utils.GisLongitudeToStr(
  const _val     : Double     ;
  const _spaces  : Boolean    ;
  const _prec    : Integer    ;
  const _leading : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisLongitudeToStr( _val, _spaces, _prec, _leading ) ;
end ;

class function TGIS_Utils.GisStrToAngle(
  const _txt : String
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisStrToAngle( _txt ) ;
end ;

class function TGIS_Utils.GisStrToLatitude(
  const _txt: String
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisStrToLatitude( _txt ) ;
end ;

class function TGIS_Utils.GisStrToLongitude(
  const _txt: String
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisStrToLongitude( _txt ) ;
end ;

class function TGIS_Utils.GisIsPointInsideExtent
                                   ( const _ptg      : TGIS_Point ;
                                     const _extent   : TGIS_Extent
                                   ) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsPointInsideExtent( _ptg, _extent ) ;
end ;

{$IFNDEF GIS_BASIC_PROJECTION}

class function TGIS_Utils.GisIsPointInsidePolygon(
  const _ptg   : TGIS_Point ;
  const _shape : TGIS_ShapePolygon
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisLayerVector{$ENDIF}.GisIsPointInsidePolygon( _ptg, _shape ) ;
end ;
{$ENDIF}

class function TGIS_Utils.GisIsLinesCommonPoint(
  const _line1 : TGIS_Line ;
  const _line2 : TGIS_Line
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisIsLinesCommonPoint( _line1, _line2 ) ;
end ;

class function TGIS_Utils.GisGetLinesCrossing(
  const _line1 : TGIS_Line ;
  const _line2 : TGIS_Line ;
  var   _ptg   : TGIS_Point
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisGetLinesCrossing( _line1, _line2, _ptg ) ;
end ;



class function TGIS_Utils.GisCreateShapeFromWKT(
  const _wkt : String
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisCreateShapeFromWKT( _wkt ) ;
end ;

class function TGIS_Utils.GisCreateShapeFromWKB(
  _wkb : Variant
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisCreateShapeFromWKB( _wkb ) ;
end ;

class function TGIS_Utils.GisCreateShapeFromGDO(
  _gdo : Variant
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisCreateShapeFromGDO( _gdo ) ;
end ;

class function TGIS_Utils.GisCreateShapeFromJSON(
  const _json     : String
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisCreateShapeFromJSON( _json ) ;
end ;

class function TGIS_Utils.GisCreateShapeFromGML(
  const _gml      : String
) : TGIS_Shape ;
begin
  Result := TGIS_GeometryFactory.GisCreateShapeFromGML( _gml ) ;
end ;

class function TGIS_Utils.GisExportGeometry2Hex(
  const _shp     : TGIS_Shape
  ) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportGeometry2Hex( _shp ) ;
end ;

class function TGIS_Utils.GisExportWKB2Hex(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportWKB2Hex( _shp ) ;
end ;

class function TGIS_Utils.GisExportPointToWKT(
  const _shp      : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPointToWKT( _shp ) ;
end ;

class function TGIS_Utils.GisExportPointToGML(
  const _shp      : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPointToGML( _shp ) ;
end ;

class procedure TGIS_Utils.GisExportPointToWKB(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _wkb     : System.Object
  {$ELSE}
    var _wkb     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPointToWKB( _shp, _wkb ) ;
end ;

class procedure TGIS_Utils.GisExportPointToGDO(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _gdo     : System.Object
  {$ELSE}
    var _gdo     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPointToGDO( _shp, _gdo ) ;
end ;

class procedure TGIS_Utils.GisExportPointToVAR(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _var     : System.Object
  {$ELSE}
    var _var     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPointToVAR( _shp, _var ) ;
end ;

class function TGIS_Utils.GisExportPointToJSON(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPointToJSON( _shp ) ;
end ;

class function TGIS_Utils.GisExportMultiPointToWKT(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportMultiPointToWKT( _shp ) ;
end ;

class function TGIS_Utils.GisExportMultiPointToGML(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportMultiPointToGML( _shp ) ;
end ;

class procedure TGIS_Utils.GisExportMultiPointToWKB(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _wkb     : System.Object
  {$ELSE}
    var _wkb     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportMultiPointToWKB( _shp, _wkb ) ;
end ;

class procedure TGIS_Utils.GisExportMultiPointToGDO(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _gdo     : System.Object
  {$ELSE}
    var _gdo     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportMultiPointToGDO( _shp, _gdo ) ;
end ;

class procedure TGIS_Utils.GisExportMultiPointToVAR(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _var     : System.Object
  {$ELSE}
    var _var     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportMultiPointToVAR( _shp, _var ) ;
end ;

class function TGIS_Utils.GisExportMultiPointToJSON(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportMultiPointToJSON( _shp ) ;
end ;

class function TGIS_Utils.GisExportArcToWKT(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportArcToWKT( _shp ) ;
end ;

class function TGIS_Utils.GisExportArcToGML(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportArcToGML( _shp ) ;
end ;

class procedure TGIS_Utils.GisExportArcToWKB(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _wkb     : System.Object
  {$ELSE}
    var _wkb     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportArcToWKB( _shp, _wkb ) ;
end ;

class procedure TGIS_Utils.GisExportArcToGDO(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _gdo     : System.Object
  {$ELSE}
    var _gdo     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportArcToGDO( _shp, _gdo ) ;
end ;

class procedure TGIS_Utils.GisExportArcToVAR(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _var     : System.Object
  {$ELSE}
    var _var     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportArcToVAR( _shp, _var ) ;
end ;

class function TGIS_Utils.GisExportArcToJSON(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportArcToJSON( _shp ) ;
end ;

class function TGIS_Utils.GisExportPolygonToWKT(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPolygonToWKT( _shp ) ;
end ;

class function TGIS_Utils.GisExportPolygonToGML(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPolygonToGML( _shp ) ;
end ;

class procedure TGIS_Utils.GisExportPolygonToWKB(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _wkb     : System.Object
  {$ELSE}
    var _wkb     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPolygonToWKB( _shp, _wkb ) ;
end ;

class procedure TGIS_Utils.GisExportPolygonToVAR(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _var     : System.Object
  {$ELSE}
    var _var     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPolygonToVAR( _shp, _var ) ;
end ;

class procedure TGIS_Utils.GisExportPolygonToGDO(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _gdo     : System.Object
  {$ELSE}
    var _gdo     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportPolygonToGDO( _shp, _gdo ) ;
end ;

class function TGIS_Utils.GisExportPolygonToJSON(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportPolygonToJSON( _shp ) ;
end ;

class function TGIS_Utils.GisExportComplexToWKT(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportComplexToWKT( _shp ) ;
end ;

class function TGIS_Utils.GisExportComplexToGML(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportComplexToGML( _shp ) ;
end ;

class procedure TGIS_Utils.GisExportComplexToWKB(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _wkb     : System.Object
  {$ELSE}
    var _wkb     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportComplexToWKB( _shp, _wkb ) ;
end ;

class procedure TGIS_Utils.GisExportComplexToGDO(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
    var _gdo     : System.Object
  {$ELSE}
    var _gdo     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportComplexToGDO( _shp, _gdo ) ;
end ;

class procedure TGIS_Utils.GisExportComplexToVAR(
  const _shp     : TGIS_Shape    ;
  {$IFDEF CLR}
     var _var     : System.Object
  {$ELSE}
    var _var     : OleVariant
  {$ENDIF}
) ;
begin
  TGIS_GeometryFactory.GisExportComplexToVAR( _shp, _var ) ;
end ;

class function TGIS_Utils.GisExportComplexToJSON(
  const _shp     : TGIS_Shape
) : String ;
begin
  Result := TGIS_GeometryFactory.GisExportComplexToJSON( _shp ) ;
end ;


class function TGIS_Utils.GisCreateLayer(
  const _name : String ;
  const _path : String
) : TGIS_Layer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisRegistredLayers{$ENDIF}.GisCreateLayer( _name, _path ) ;
end ;


class function TGIS_Utils.GisSupportedFiles(
  _mode : TGIS_FileTypes ;
  _save : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisRegistredLayers{$ENDIF}.GisSupportedFiles( _mode, _save ) ;
end ;

class procedure TGIS_Utils.GisLocalizedStringsFromString(
  const _localization : String;
  const _errpath      : String
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisResourceEx{$ENDIF}.GisLocalizedStringsFromString( _localization, _errpath ) ;
end ;

class procedure TGIS_Utils.GisLocalizedStringsFromFile(
  const _path     : String ;
  const _language : String ;
  const _errpath  : String
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisResourceEx{$ENDIF}.GisLocalizedStringsFromFile( _path, _language, _errpath ) ;
end ;

class function TGIS_Utils.GisSystemCodePage
 : Integer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSystemCodePage ;
end ;

class function TGIS_Utils.GisMetadata : TGIS_StringList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMetadata ;
end ;

class function TGIS_Utils.GisMetadataAsBoolean(
  const _name    : String ;
  const _default : Boolean
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMetadataAsBoolean(
              _name,
              _default
            )  ;
end ;

class function TGIS_Utils.GisMetadataAsInteger(
  const _name    : String ;
  const _default : Integer
) : Integer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMetadataAsInteger(
              _name,
              _default
            )  ;
end ;

class function TGIS_Utils.GisMetadataAsFloat(
  const _name    : String ;
  const _default : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMetadataAsFloat(
              _name,
              _default
            )  ;
end ;

class function TGIS_Utils.GisMetadataAsString(
  const _name    : String ;
  const _default : String
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisMetadataAsString(
              _name,
              _default
            )  ;
end ;


class function TGIS_Utils.GIS_RENDER_SIZE
  : Integer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTypes{$ENDIF}.GIS_RENDER_SIZE ;
end ;

class function TGIS_Utils.GIS_RELATE_EQUALITY
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_EQUALITY ;
end ;

class function TGIS_Utils.GIS_RELATE_DISJOINT
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_DISJOINT ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT  ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT1
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT1 ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT2
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT2 ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT3
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT3 ;
end ;

class function TGIS_Utils.GIS_RELATE_TOUCH_INTERIOR
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_TOUCH_INTERIOR ;
end ;

class function TGIS_Utils.GIS_RELATE_TOUCH
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_TOUCH ;
end ;

class function TGIS_Utils.GIS_RELATE_CROSS
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_CROSS ;
end ;

class function TGIS_Utils.GIS_RELATE_CROSS_LINE
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_CROSS_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_WITHIN
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_WITHIN ;
end ;

class function TGIS_Utils.GIS_RELATE_CONTAINS
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_CONTAINS ;
end ;

class function TGIS_Utils.GIS_RELATE_OVERLAP
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_OVERLAP ;
end ;

class function TGIS_Utils.GIS_RELATE_OVERLAP_LINE
 : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_OVERLAP_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_LINE_CROSS_POLYGON
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_LINE_CROSS_POLYGON ;
end ;

class function TGIS_Utils.GIS_RELATE_POLYGON_CROSSED_BY_LINE
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_POLYGON_CROSSED_BY_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_LINE_CROSS_LINE
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_LINE_CROSS_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_LINE_TRAVERS_POLYGON
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_LINE_TRAVERS_POLYGON ;
end ;

class function TGIS_Utils.GIS_RELATE_POLYGON_TRAVERSED_BY_LINE
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_POLYGON_TRAVERSED_BY_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_LINE_CROSSTRAVERS_POLYGON
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_LINE_CROSSTRAVERS_POLYGON ;
end ;

class function TGIS_Utils.GIS_RELATE_POLYGON_CROSSTRAVERSED_BY_LINE
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_POLYGON_CROSSTRAVERSED_BY_LINE ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT_INTERIOR_INTERIOR
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT_INTERIOR_INTERIOR ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT_INTERIOR_BOUNDARY
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT_INTERIOR_BOUNDARY ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT_BOUNDARY_INTERIOR
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT_BOUNDARY_INTERIOR ;
end ;

class function TGIS_Utils.GIS_RELATE_INTERSECT_BOUNDARY_BOUNDARY
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_INTERSECT_BOUNDARY_BOUNDARY ;
end ;

class function TGIS_Utils.GIS_RELATE_TOUCH_BOUNDARY_INTERIOR
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_TOUCH_BOUNDARY_INTERIOR ;
end ;

class function TGIS_Utils.GIS_RELATE_TOUCH_INTERIOR_BOUNDARY
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_TOUCH_INTERIOR_BOUNDARY ;
end ;

class function TGIS_Utils.GIS_RELATE_TOUCH_BOUNDARY_BOUNDARY
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.RELATE_TOUCH_BOUNDARY_BOUNDARY ;
end ;

class function TGIS_Utils.ParamAlignment(
  const _value   : String ;
  const _default : TGIS_LabelAlignment
) : TGIS_LabelAlignment ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamAlignment( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamAlignment(
  const _value : TGIS_LabelAlignment
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamAlignment( _value ) ;
end ;

class function TGIS_Utils.ParamBitmap(
  const _value   : String ;
  const _default : TGIS_Bitmap
) : TGIS_Bitmap ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamBitmap( _value, _default ) ;
end ;

class function TGIS_Utils.ParamBoolean(
  const _value   : String ;
  const _default : Boolean
) : Boolean ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamBoolean( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamBoolean(
  const _value : Boolean
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamBoolean( _value ) ;
end ;



class function TGIS_Utils.ParamColor(
  const _value   : String ;
  const _default : TGIS_Color
) : TGIS_Color ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamColor( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamColor(
  const _value : TGIS_Color
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamColor( _value ) ;
end ;


class function TGIS_Utils.ParamChart(
  const _value   : String;
  const _default : TGIS_ChartStyle
) : TGIS_ChartStyle ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamChart( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamChart(
  const _value : TGIS_ChartStyle
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamChart( _value ) ;
end ;

class function TGIS_Utils.ParamDormant(
  const _value   : String ;
  const _default : TGIS_LayerDormantMode
) : TGIS_LayerDormantMode ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamDormant( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamDormant(
  const _value : TGIS_LayerDormantMode
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamDormant( _value ) ;
end ;



class function TGIS_Utils.ParamFontStyle(
  const _value   : String ;
  const _default : TGIS_FontStyles
) : TGIS_FontStyles ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamFontStyle( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamFontStyle(
  const _value : TGIS_FontStyles
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamFontStyle( _value ) ;
end ;


class function TGIS_Utils.ParamInteger(
  const _value   : String ;
  const _default : Integer
) : Integer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamInteger( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamInteger(
  const _value : Integer
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamInteger( _value ) ;
end ;

class function TGIS_Utils.ParamMarker(
  const _value   : String ;
  const _default : TGIS_MarkerStyle
) : TGIS_MarkerStyle ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamMarker( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamMarker(
  const _value : TGIS_MarkerStyle
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamMarker( _value ) ;
end ;

class function TGIS_Utils.ParamPattern(
  const _value   : String ;
  const _default : TGIS_BrushStyle
) : TGIS_BrushStyle ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamPattern( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamPattern(
  const _value : TGIS_BrushStyle
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamPattern( _value ) ;
end ;



class function TGIS_Utils.ParamSymbol(
  const _value   : String ;
  const _default : TGIS_SymbolAbstract
) : TGIS_SymbolAbstract ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamSymbol( _value, _default ) ;
end ;


class function TGIS_Utils.ParamPen(
  const _value   : String ;
  const _default : TGIS_PenStyle
) : TGIS_PenStyle ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamPen( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamPen(
  const _value : TGIS_PenStyle
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamPen( _value ) ;
end ;

class function TGIS_Utils.ParamFloat(
  const _value   : String ;
  const _default : Double
) : Double ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamFloat( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamFloat(
  const _value : Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamFloat( _value ) ;
end ;

class function TGIS_Utils.ParamString(
  const _value   : String ;
  const _default : String
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamString( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamString(
  const _value : String
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamString( _value ) ;
end ;

class function TGIS_Utils.ParamPosition(
  const _value   : String ;
  const _default : TGIS_LabelPositions
) : TGIS_LabelPositions ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ParamPosition( _value, _default ) ;
end ;

class function TGIS_Utils.ConstructParamPosition(
  const _value : TGIS_LabelPositions
) : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisParams{$ENDIF}.ConstructParamPosition( _value ) ;
end ;

class procedure TGIS_Utils.GisTopologyForceShapeFixing(
  const _mode : Boolean
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisTopology{$ENDIF}.GisTopologyForceShapeFixing( _mode ) ;
end;

class procedure TGIS_Utils.GisSetProxySettings(
   const _server : String ;
   const _port   : Integer ;
   const _user   : String ;
   const _pass   : String ;
   const _domain : String
  ) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSetProxySettings( _server, _port, _user, _pass, _domain ) ;
end ;


class function TGIS_Utils.DotStrToFloat(
  const _val : String
) : Double ;
begin
  Result := {$IFDEF OXYGENE}TatukGIS.RTL.{$ELSE}GisRtl.{$ENDIF}DotStrToFloat( _val ) ;
end ;

class function TGIS_Utils.DotFloatToStr(
  const _val : Double
) : String ;
begin
  Result := {$IFDEF OXYGENE}TatukGIS.RTL.{$ELSE}GisRtl.{$ENDIF}DotFloatToStr( _val ) ;
end ;

{$IFDEF CLR}
  {$IFDEF MSWINDOWS_OS}
  class procedure TGIS_Utils.UsePrivateFontCollection(
    const _fonts : System.Drawing.Text.PrivateFontCollection
  ) ;
  begin
    if assigned( FontCollection ) then exit ;
    FontCollection := _fonts ;
  end;

  class procedure TGIS_Utils.UsePrivateFontCollection(
    const _fonts : List<String>
  ) ;
  var
    i : Integer ;
  begin
    if _fonts.Count = 0 then exit ;
    if assigned( FontCollection ) then exit ;
    // WinForms
    FontCollection := System.Drawing.Text.PrivateFontCollection.Create ;
    for i := 0 to _fonts.Count - 1 do
      FontCollection.AddFontFile( _fonts[i] ) ;
    CustomFontList := _fonts.ToArray ;
  end ;
  {$ENDIF}
{$ENDIF}

class procedure TGIS_Utils.SetLicense(
  const _code : String
) ;
begin
  TGIS_LicenseManager.SetLicense( _code ) ;
end ;

{$IFNDEF JAVA OR ISLAND OR BLAZOR}
  class function TGIS_Utils.GisOpenCLEngine : TGIS_OpenCLEngine ;
  begin
    Result := {$IFDEF OXYGENE}TatukGIS.NDK.OpenCL.{$ELSE}GisOpenCL.{$ENDIF}GisOpenCLEngine ;
  end ;
{$ENDIF}

{$IFNDEF JAVA OR ISLAND OR BLAZOR}
  class function TGIS_Utils.GisCUDAEngine : TGIS_CUDAEngine ;
  begin
    Result := {$IFDEF OXYGENE}TatukGIS.NDK.CUDA.{$ELSE}GisCUDA.{$ENDIF}GisCUDAEngine ;
  end ;
{$ENDIF}

class function TGIS_Utils.ExecutingFolder : String ;
var
  stmp : String ;
begin
  {$IFDEF DCC}
    stmp := ParamStr(0);
  {$ENDIF}

  {$IFDEF JAVA}
    var sroot := java.net.URLDecoder.decode(
                   typeOf(TGIS_Utils).getProtectionDomain().getCodeSource().getLocation().getPath(),
                   'UTF-8'
                 );
    stmp := (new java.io.File( sroot )).getPath();
  {$ENDIF}

  {$IFDEF CLR}
    var asmb  := System.Reflection.Assembly.GetExecutingAssembly ;
    var u     := Uri.Create( asmb.EscapedCodeBase ) ;
    stmp := u.LocalPath + Uri.UnescapeDataString(u.Fragment).Replace('/', '\\') ;
  {$ENDIF}

  Result := GetPathDirSep( GetFilePath( stmp ) ) ;
end ;

{$IFDEF JAVA}
  class procedure TGIS_Utils.SetLibrariesSearchPath(
    const _path : String
  ) ;
  begin
    System.setProperty( 'jna.library.path', _path ) ;
  end ;
{$ENDIF}

{ Property CSUnitsList access function.
}
class function TGIS_Utils.fget_CSUnitsList
  : TGIS_CSUnitsList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSUnitsList ;
end ;

{ Property CSPrimeMeridianList access function.
}
class function TGIS_Utils.fget_CSPrimeMeridianList
  : TGIS_CSPrimeMeridianList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSPrimeMeridianList ;
end ;

{ Property CSEllipsoidList access function.
}
class function TGIS_Utils.fget_CSEllipsoidList
  : TGIS_CSEllipsoidList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSEllipsoidList ;
end ;

{ Property CSTransformList access function.
}
class function TGIS_Utils.fget_CSTransformList
  : TGIS_CSTransformList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSTransformList ;
end ;

{ Property CSDatumList access function.
}
class function TGIS_Utils.fget_CSDatumList
  : TGIS_CSDatumList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSDatumList ;
end ;

{ Property CSAreaList access function.
}
class function TGIS_Utils.fget_CSAreaList
  : TGIS_CSAreaList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsBase{$ENDIF}.CSAreaList ;
end ;

{ Property CSProjList access function.
}
class function TGIS_Utils.fget_CSProjList
  : TGIS_CSProjList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsProjections{$ENDIF}.CSProjList ;
end ;

{ Property CSGeographicCoordinateSystemList access function.
}
class function TGIS_Utils.fget_CSGeographicCoordinateSystemList
  : TGIS_CSGeographicCoordinateSystemList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsSystems{$ENDIF}.CSGeographicCoordinateSystemList
end ;

{ Property CSProjectedCoordinateSystemList access function.
}
class function TGIS_Utils.fget_CSProjectedCoordinateSystemList
  : TGIS_CSProjectedCoordinateSystemList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsSystems{$ENDIF}.CSProjectedCoordinateSystemList ;
end ;

{ Property CSUnknownCoordinateSystem access function.
}
class function TGIS_Utils.fget_CSUnknownCoordinateSystem
  : TGIS_CSUnknownCoordinateSystem ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisCsSystems{$ENDIF}.CSUnknownCoordinateSystem
end ;

  { Property RendererManager access function.
  }
  class function TGIS_Utils.fget_RendererManager
    : TGIS_RendererManager ;
  begin
    Result := {$IFDEF OXYGENE}__Global{$ELSE}GisRendererAbstract{$ENDIF}.RendererManager ;
  end ;

  { Property SymolList access function.
  }
  class function TGIS_Utils.fget_SymbolList
    : TGIS_SymbolList ;
  begin
    Result := {$IFDEF OXYGENE}__Global{$ELSE}GisSymbol{$ENDIF}.SymbolList ;
  end ;

{ Property GisPasswordList access function.
}
class function TGIS_Utils.fget_GisPasswordList
  : TGIS_PasswordList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisPasswordList ;
end;

{ Property GisKeyList access function.
}
class function TGIS_Utils.fget_GisKeyList
  : TGIS_PasswordList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisKeyList ;
end;

{ Property GisAliasList access function.
}
class function TGIS_Utils.fget_GisAliasList
  : TGIS_AliasList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisAliasList ;
end;

{ Property GisColorRampList access function.
}
class function TGIS_Utils.fget_GisColorRampList
  : TGIS_ColorRampList ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisColorRampList ;
end;

{ Property GisEnvironmentInfo access function.
}
class function TGIS_Utils.fget_GisEnvironmentInfo
  : TGIS_EnvironmentInfo ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisEnvironmentInfo ;
end;

{ Property ForcedRTreePath access function.
}
class function TGIS_Utils.fget_ForcedRTreePath
  : String ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisInternals{$ENDIF}.ForcedRTreePath ;
end ;

{ Property ForcedRTreePath access function.
}
class procedure TGIS_Utils.fset_ForcedRTreePath(
  const _value : String
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisInternals{$ENDIF}.ForcedRTreePath := _value ;
end ;

{ Property ForcedRTreePath access function.
}
class function TGIS_Utils.fget_GisProxySettings
  : TGIS_ProxySettings ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisProxySettings ;
end ;

{$IFNDEF ANDROID_JAVA}
  { Property PipelineOperations access function.
  }
  class function TGIS_Utils.fget_PipelineOperations
    : TGIS_PipelineOperations ;
  begin
    Result := {$IFDEF OXYGENE}__Global{$ELSE}GisPipeline{$ENDIF}.PipelineOperations ;
  end ;
{$ENDIF}

class procedure TGIS_Utils.GisSunPosition(
  const _ptg        : TGIS_Point ;
  const _local_time : TDateTime ;
  const _utc_offset : Double ;
  var   _altitude   : Double ;
  var   _azimuth    : Double
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSunPosition(
    _ptg, _local_time, _utc_offset, _altitude, _azimuth
  ) ;
end;

class procedure TGIS_Utils.GisSunPosition(
  const _ptg      : TGIS_Point ;
  const _utc_time : TDateTime ;
  var   _altitude : Double ;
  var   _azimuth  : Double
) ;
begin
  {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisSunPosition(
    _ptg, _utc_time, _altitude, _azimuth
  ) ;
end;

class function TGIS_Utils.GisColorMap(
  const _index : Double ;
  const _color : TGIS_Color
) : TGIS_ColorMap ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisColorMap(
    _index, _color
  ) ;
end;

class function TGIS_Utils.GisColorMapEx(
  const _class  : Integer ;
  const _colors : TGIS_ColorArray
) : TGIS_ColorMapEx ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisFunctions{$ENDIF}.GisColorMapEx(
    _class, _colors
  ) ;
end;

class function TGIS_Utils.GisGridArray(
   const _height : Integer ;
   const _width  : Integer
) : TGIS_GridArray ;
var
 k, i, imaxr, imaxc : Integer ;
begin
  Result := {$IFDEF OXYGENE}__Global{$ELSE}GisInternals{$ENDIF}.InitializeGrid(
    _height,
    _width
  ) ;

  imaxr := length(Result) -1 ;
  imaxc := length(Result[0]) -1 ;
  for k := 0 to imaxr do
    for i := 0 to imaxc do
      Result[k][i] := GIS_GRID_NOVALUE ;
end ;

class function TGIS_Utils.GisBitmapFactoryUniversal
  : TGIS_BitmapFactory ;
var
  factory : TGIS_BitmapFactoryClass ;
begin
  Result := nil ;
  factory := nil ;
  {$IFDEF DCC}
    if not assigned( Result ) then begin
      BitmapFactoryList.TryGetValue( 'TGIS_BitmapFactoryVCL', factory ) ;
      assert( assigned( factory ) ) ;
      Result := factory.Create ;
    end ;
  {$ENDIF}
  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
      Result := TGIS_BitmapFactoryJava.create ;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF CLR}
    {$IFDEF GIS_IS}
      {$IFDEF DOTNET_STANDARD}
        Result := SkiaSharp.TGIS_BitmapFactorySkia.Create ;
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF BLAZOR} //?
      if not assigned( result ) then
        Result := TGIS_BitmapFactoryCLR.Create ;
    {$ENDIF}
  {$ENDIF}
end ;

{==================================== END =====================================}
end.
