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
  This is a primary definitions unit.

  Some very common functions and types are defined here.
}

{$IFDEF DCC}
  unit GisFunctions ;
  {$HPPEMIT '#pragma link "GisFunctions"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    System.Data,
    System.Runtime.InteropServices,
    System.Runtime.InteropServices.ComTypes,
    System.Text,
    System.IO,
    System.IO.Compression,
    System.Globalization,
    System.Net,
    {$IFDEF MSWINDOWS}
      System.Web,
    {$ENDIF}
    System.Xml,
    System.Collections,
    System.Collections.Generic,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.SysUtils,
    System.Math,
    System.StrUtils,
    System.Variants,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
      System.Win.Registry,
    {$ENDIF}
    {$IFNDEF GIS_NODB}
      Data.DB,
      {$IFDEF MSWINDOWS}
        {$HPPEMIT '#pragma comment( lib, "dbrtl" )'}
      {$ENDIF}
    {$ENDIF}
    Data.FmtBcd,
    GisTypes,
    GisTypesUI,
    GisRtl,
    GisClasses ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util.*,
    java.nio.*,
    java.io.*,
    java.net.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

  {$REGION 'public functions'}
  //----------------------------------------------------------------------------
  //  GIS public functions
  //----------------------------------------------------------------------------

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
  ///   new point
  /// </returns>
  function  GisPoint            ( const _x, _y : Double
                                ) : TGIS_Point ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  {#gendoc:hide}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Create a new 3D Integer point.
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
  ///   new point
  /// </returns>
  function  GisPoint3DInt       ( const _x, _y, _z : Integer
                                ) : TGIS_Point3DInt ;
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
  ///   new point
  /// </returns>
  function  GisPoint3D          ( const _x, _y, _z : Double
                                ) : TGIS_Point3D ; overload;
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
  ///   new point
  /// </returns>
  function  GisPoint3D          ( const _x, _y, _z : Double ;
                                  const _m : Double
                                ) : TGIS_Point3D ; overload;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Convert 2D point into 3D point.
  /// </summary>
  /// <param name="_ptg">
  ///   point to be converted
  /// </param>
  /// <returns>
  ///   new point
  /// </returns>
  function  GisPoint3DFrom2D    ( const _ptg : TGIS_Point
                                ) : TGIS_Point3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Convert 3D point into 2D point.
  /// </summary>
  /// <param name="_ptg">
  ///   point to be converted
  /// </param>
  /// <returns>
  ///   new point
  /// </returns>
  function  GisPoint2DFrom3D    ( const _ptg : TGIS_Point3D
                                ) : TGIS_Point ;
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
  ///   new extent
  /// </returns>
  function  GisExtent           ( const _xmin, _ymin : Double ;
                                  const _xmax, _ymax : Double
                                ) : TGIS_Extent ;
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
  ///   new extent
  /// </returns>
  function  GisExtent3D         ( const _xmin, _ymin, _zmin : Double ;
                                  const _xmax, _ymax, _zmax : Double
                                ) : TGIS_Extent3D ; overload;
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
  /// <param name="_zmin">
  ///   coordinate
  /// </param>
  /// <param name="_mmin">
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
  /// <param name="_mmax">
  ///   coordinate
  /// </param>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisExtent3D         ( const _xmin, _ymin, _zmin, _mmin : Double ;
                                  const _xmax, _ymax, _zmax, _mmax : Double
                                ) : TGIS_Extent3D ; overload;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Convert 2D extent into 3D extent.
  /// </summary>
  /// <param name="_ext">
  ///   extent to be converted
  /// </param>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisExtent3DFrom2D   ( const _ext : TGIS_Extent
                                ) : TGIS_Extent3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Convert 3D extent into 2D extent.
  /// </summary>
  /// <param name="_ext">
  ///   extent to be converted
  /// </param>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisExtent2DFrom3D   ( const _ext : TGIS_Extent3D
                                ) : TGIS_Extent ;
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
  function  GisLine             ( const _a        : TGIS_Point ;
                                  const _b        : TGIS_Point
                                ) : TGIS_Line ; overload ;
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
  function  GisLine             ( const _ax       : Double ;
                                  const _ay       : Double ;
                                  const _bx       : Double ;
                                  const _by       : Double
                                ) : TGIS_Line ; overload ;
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
  function  GisLine3D           ( const _a        : TGIS_Point3D ;
                                  const _b        : TGIS_Point3D
                                ) : TGIS_Line3D ; overload ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Are two double values equal?
  /// </summary>
  /// <param name="_a">
  ///   value to compare
  /// </param>
  /// <param name="_b">
  ///   value to compare
  /// </param>
  /// <returns>
  ///   True if are equal
  /// </returns>
  /// <remarks>
  ///   comparing is processed with double resolution set to 1e-12
  /// </remarks>
  function  GisIsSameValue      ( const _a        : Double ;
                                  const _b        : Double
                                ) : Boolean ; overload;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Are two double values equal?
  /// </summary>
  /// <param name="_a">
  ///   value to compare
  /// </param>
  /// <param name="_b">
  ///   value to compare
  /// </param>
  /// <param name="_tol">
  ///   tolerance of comparison
  /// </param>
  /// <returns>
  ///   True if are equal
  /// </returns>
  function  GisIsSameValue      ( const _a        : Double ;
                                  const _b        : Double ;
                                  const _tol      : Double
                                ) : Boolean ; overload;
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
  ///   True if are equal
  /// </returns>
  /// <remarks>
  ///   Uses GisIsSameValue.
  /// </remarks>
  function  GisIsSamePoint      ( const _ptg1     : TGIS_Point ;
                                  const _ptg2     : TGIS_Point
                                ) : Boolean ; overload ;
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
  /// <remarks>
  ///   Uses GisIsSameValue.
  /// </remarks>
  function  GisIsSamePoint      ( const _ptg1     : TGIS_Point ;
                                  const _ptg2     : TGIS_Point ;
                                  const _tol      : Double
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is ignored. To compare with measure use GisIsSamePoint3DM().
  ///   </note>
  /// </remarks>
  function  GisIsSamePoint3D    ( const _ptg1     : TGIS_Point3D ;
                                  const _ptg2     : TGIS_Point3D
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is ignored. To compare with measure use GisIsSamePoint3DM().
  ///   </note>
  /// </remarks>
  function  GisIsSamePoint3D    ( const _ptg1     : TGIS_Point3D ;
                                  const _ptg2     : TGIS_Point3D ;
                                  const _tol      : Double
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is not ignored. To compare without measure
  ///     use GisIsSamePoint3D().
  ///   </note>
  /// </remarks>
  function  GisIsSamePoint3DM   ( const _ptg1     : TGIS_Point3D ;
                                  const _ptg2     : TGIS_Point3D
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is not ignored. To compare without measure
  ///     use GisIsSamePoint3D().
  ///   </note>
  /// </remarks>
  function  GisIsSamePoint3DM   ( const _ptg1     : TGIS_Point3D ;
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
  ///   True if are equal
  /// </returns>
  /// <remarks>
  ///   Uses GisIsSameValue.
  /// </remarks>
  function  GisIsSameExtent     ( const _extent1  : TGIS_Extent ;
                                  const _extent2  : TGIS_Extent
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
  /// <param name="_tol">
  ///   tolerance of comparison
  /// </param>
  /// <returns>
  ///   True if are equal
  /// </returns>
  /// <remarks>
  ///   Uses GisIsSameValue.
  /// </remarks>
  function  GisIsSameExtent     ( const _extent1  : TGIS_Extent ;
                                  const _extent2  : TGIS_Extent ;
                                  const _tol      : Double
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is ignored. To compare with measure use GisIsSameExtent3DM().
  ///   </note>
  /// </remarks>
  function  GisIsSameExtent3D   ( const _extent1  : TGIS_Extent3D ;
                                  const _extent2  : TGIS_Extent3D
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is ignored. To compare with measure use GisIsSameExtent3DM().
  ///   </note>
  /// </remarks>
  function  GisIsSameExtent3D   ( const _extent1  : TGIS_Extent3D ;
                                  const _extent2  : TGIS_Extent3D ;
                                  const _tol      : Double
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is not ignored. To compare without measure
  ///     use GisIsSameExtent3D().
  ///   </note>
  /// </remarks>
  function  GisIsSameExtent3DM  ( const _extent1  : TGIS_Extent3D ;
                                  const _extent2  : TGIS_Extent3D
                                ) : Boolean ; overload ;
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
  ///   Uses GisIsSameValue.
  ///   <note type="note">
  ///     Measure is not ignored. To compare without measure
  ///     use GisIsSameExtent3D().
  ///   </note>
  /// </remarks>
  function  GisIsSameExtent3DM  ( const _extent1  : TGIS_Extent3D ;
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
  ///   True if _extent1 encompass _extent2
  /// </returns>
  function  GisContainExtent    ( const _extent1, _extent2 : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is extent empty? Extent is empty if (XMin&gt;=XMax) or (YMin&gt;=YMax)
  /// </summary>
  /// <param name="_extent">
  ///   extent to be verified
  /// </param>
  /// <returns>
  ///   True if extent is empty
  /// </returns>
  function  GisIsEmptyExtent    ( const _extent : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is extent empty? Extent is empty if (XMin&gt;=XMax) or (YMin&gt;=YMax)
  ///   or (ZMin&gt;=ZMax)
  /// </summary>
  /// <param name="_extent">
  ///   extent to be verified
  /// </param>
  /// <returns>
  ///   True if extent is empty
  /// </returns>
  function  GisIsEmptyExtent3D  ( const _extent : TGIS_Extent3D
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Returns an infinite extent
  /// </summary>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisWholeWorld       : TGIS_Extent   ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Returns an infinite 3D extent
  /// </summary>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisWholeWorld3D     : TGIS_Extent3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is extent same as whole world?
  /// </summary>
  /// <param name="_extent">
  ///   extent to compare
  /// </param>
  /// <returns>
  ///   True if extent same as whole world
  /// </returns>
  function  GisIsWholeWorld     ( const _extent : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is 3D extent same as whole world?
  /// </summary>
  /// <param name="_extent">
  ///   extent to compare
  /// </param>
  /// <returns>
  ///   True if extent same as whole world
  /// </returns>
  function  GisIsWholeWorld3D   ( const _extent : TGIS_Extent3D
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Check if values of the point are reasonable (&lt;GIS_MAX_SINGLE). Used for
  ///   example to test projection results.
  /// </summary>
  /// <param name="_ptg">
  ///   point to be verified
  /// </param>
  /// <returns>
  ///   True if values are reasonable.
  /// </returns>
  function GisIsValidPtg        ( const _ptg : TGIS_Point
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Check if values of the point are reasonable (&lt;GIS_MAX_SINGLE). Used for
  ///   example to test projection results.
  /// </summary>
  /// <param name="_ptg">
  ///   point to be verified
  /// </param>
  /// <returns>
  ///   True if values are reasonable.
  /// </returns>
  function GisIsValidPtg3D      ( const _ptg : TGIS_Point3D
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Check if values of the extent are reasonable (&lt;GIS_MAX_SINGLE). Used for
  ///   example to test projection results.
  /// </summary>
  /// <param name="_extent">
  ///   extent to be verified
  /// </param>
  /// <returns>
  ///   True if values are reasonable.
  /// </returns>
  function GisIsValidExtent     ( const _extent : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Check if values of the extent are reasonable (&lt;GIS_MAX_SINGLE). Used for
  ///   example to test projection results.
  /// </summary>
  /// <param name="_extent">
  ///   extent to be verified
  /// </param>
  /// <returns>
  ///   True if values are reasonable.
  /// </returns>
  function GisIsValidExtent3D   ( const _extent : TGIS_Extent3D
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Returns an nil extent.
  /// </summary>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisNoWorld          : TGIS_Extent   ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Returns an nil 3D extent.
  /// </summary>
  /// <returns>
  ///   new extent
  /// </returns>
  function  GisNoWorld3D        : TGIS_Extent3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is extent same as no world (non existing extent)?
  /// </summary>
  /// <param name="_extent">
  ///   extent to compare
  /// </param>
  /// <returns>
  ///   True if extent same as no world
  /// </returns>
  function  GisIsNoWorld        ( const _extent : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Is 3D extent same as no world (non existing extent)?
  /// </summary>
  /// <param name="_extent">
  ///   extent to compare
  /// </param>
  /// <returns>
  ///   True if extent same as no world
  /// </returns>
  function  GisIsNoWorld3D      ( const _extent : TGIS_Extent3D
                                ) : Boolean ;
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
  ///   minimum value
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Obsoleted!
  ///   </note>
  /// </remarks>
  function  GisDMin             ( const _val1, _val2 : Double
                                ) : Double ;
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
  ///   maximum value
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Obsoleted!
  ///   </note>
  /// </remarks>
  function  GisDMax             ( const _val1, _val2 : Double
                                ) : Double ;
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
  ///   new extent
  /// </returns>
  /// <remarks>
  ///   Resulting value is the extent that covers the whole area of _extent1
  ///   and extent2.
  /// </remarks>
  function  GisMaxExtent        ( const _extent1, _extent2 : TGIS_Extent
                                ) : TGIS_Extent ;
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
  ///   new extent
  /// </returns>
  /// <remarks>
  ///   Resulting value is the extent that covers the whole area of _extent1
  ///   and extent2.
  /// </remarks>
  function  GisMaxExtent3D      ( const _extent1, _extent2 : TGIS_Extent3D
                                ) : TGIS_Extent3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Calculate area of extent.
  /// </summary>
  /// <param name="_extent">
  ///   given extent
  /// </param>
  /// <returns>
  ///   area value
  /// </returns>
  function  GisExtentArea       ( const _extent : TGIS_Extent
                                ) : Double ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Calculate area of 3D extent.
  /// </summary>
  /// <param name="_extent">
  ///   given extent
  /// </param>
  /// <returns>
  ///   area value
  /// </returns>
  function  GisExtentArea3D     ( const _extent : TGIS_Extent3D
                                ) : Double ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Calculate cubature of extent.
  /// </summary>
  /// <param name="_extent">
  ///   given extent
  /// </param>
  /// <returns>
  ///   cubature value
  /// </returns>
  function  GisExtentCubature   ( const _extent : TGIS_Extent
                                ) : Double ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Calculate cubature of 3D extent.
  /// </summary>
  /// <param name="_extent">
  ///   given extent
  /// </param>
  /// <returns>
  ///   cubature value
  /// </returns>
  function  GisExtentCubature3D ( const _extent : TGIS_Extent3D
                                ) : Double ;
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
  ///   True if extents have common part
  /// </returns>
  function  GisIsCommonExtent   ( const _extent1, _extent2 : TGIS_Extent
                                ) : Boolean ;
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
  ///   True if extents have common part
  /// </returns>
  function  GisIsCommonExtent3D ( const _extent1, _extent2 : TGIS_Extent3D
                                ) : Boolean ;
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
  ///   True if _extent2 fully covers _extent1
  /// </returns>
  function  GisIsContainExtent  ( const _extent1, _extent2 : TGIS_Extent
                                ) : Boolean ;
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
  ///   True if _extent2 fully covers _extent1
  /// </returns>
  function  GisIsContainExtent3D( const _extent1, _extent2 : TGIS_Extent3D
                                ) : Boolean ;
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
  ///   new extent
  /// </returns>
  /// <remarks>
  ///   Resulting value is an extent that covers the common area of _extent1
  ///   and _extent2.
  /// </remarks>
  function  GisCommonExtent     ( const _extent1, _extent2 : TGIS_Extent
                                ) : TGIS_Extent;
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
  ///   new extent
  /// </returns>
  /// <remarks>
  ///   Resulting value is an extent that covers the common area of _extent1
  ///   and _extent2.
  /// </remarks>
  function  GisCommonExtent3D   ( const _extent1, _extent2 : TGIS_Extent3D
                                ) : TGIS_Extent3D ;
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
  ///   True point is inside an extent
  /// </returns>
  function  GisIsPointInsideExtent
                                ( const _ptg    : TGIS_Point ;
                                  const _extent : TGIS_Extent
                                ) : Boolean ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

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
  ///   line length
  /// </returns>
  function  GisLineLength       ( const _lineA, _lineB : TGIS_Point
                                ) : Double ;
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
  ///   distance
  /// </returns>
  function  GisPoint2Point      ( const _ptg1, _ptg2 : TGIS_Point
                                ) : Double ;

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
  ///   distance
  /// </returns>
  function  GisPoint2Point3D    ( const _ptg1, _ptg2 : TGIS_Point3D
                                ) : Double ;
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
  ///   distance
  /// </returns>
  function  GisLine2Point       ( const _lineA, _lineB, _ptg : TGIS_Point
                                ) : Double ;

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
  ///   distance
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     For precise calculation use GisLine2Point.
  ///   </note>
  /// </remarks>
  function  GisLine2PointFuzzy  ( const _lineA, _lineB, _ptg : TGIS_Point
                                ) : Double ;

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
  ///   distance
  /// </returns>
  /// <remarks>
  ///   If the line has zero length, GisPoint2Point3D is returned.
  /// </remarks>
  function  GisLine2Point3D     ( const _lineA, _lineB, _ptg : TGIS_Point3D
                                ) : Double ;

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
  ///   distance
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Works the same as GisLine2Point3D.
  ///   </note>
  /// </remarks>
  function  GisLine2Point3DFuzzy( const _lineA, _lineB, _ptg : TGIS_Point3D
                                ) : Double ;
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
  ///   distance
  /// </returns>
  function  GisLine2Line        ( const _line1A, _line1B : TGIS_Point ;
                                  const _line2A, _line2B : TGIS_Point
                                ) : Double ;
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
  ///   distance
  /// </returns>
  function  GisLine2Line3D      ( const _line1A, _line1B : TGIS_Point3D ;
                                  const _line2A, _line2B : TGIS_Point3D
                                ) : Double ;
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
  ///   point on the line
  /// </returns>
  function  GisPointOnLine      ( const _lineA, _lineB, _ptg : TGIS_Point
                                ) : TGIS_Point ;

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
  ///   point on the line
  /// </returns>
  function  GisPointOnLine3D    ( const _lineA, _lineB, _ptg : TGIS_Point3D
                                ) : TGIS_Point3D ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


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
  ///   True if lines have a common point
  /// </returns>
  /// <remarks>
  ///   See GisIsPointInsideExtent for example.
  /// </remarks>
  function  GisIsLinesCommonPoint
                                ( const _line1 : TGIS_Line ;
                                  const _line2 : TGIS_Line
                                ) : Boolean ;

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
  ///   True if lines have a common point
  /// </returns>
  /// <remarks>
  ///   See GisIsPointInsideExtent for example.
  /// </remarks>
  function GisGetLinesCrossing  ( const _line1 : TGIS_Line ;
                                  const _line2 : TGIS_Line ;
                                  var   _ptg   : TGIS_Point
                                ) : Boolean ;

  /// <summary>
  ///   Calculates delta between two points.
  /// </summary>
  /// <param name="_ptg1">
  ///   point to comparison
  /// </param>
  /// <param name="_ptg2">
  ///   point to comparison
  /// </param>
  /// <returns>
  ///   delta between two points
  /// </returns>
  function  GisPointsDelta      ( const _ptg1, _ptg2 : TGIS_Point
                                ) : TGIS_Point ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


  /// <summary>
  ///   Calculates delta between two points in 3D.
  /// </summary>
  /// <param name="_ptg1">
  ///   point to comparison
  /// </param>
  /// <param name="_ptg2">
  ///   point to comparison
  /// </param>
  /// <returns>
  ///   delta between two points
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Measure is ignored.
  ///   </note>
  /// </remarks>
  function  GisPointsDelta3D    ( const _ptg1, _ptg2 : TGIS_Point3D
                                ) : TGIS_Point3D ;
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
  ///   new point
  /// </returns>
  function  GisMovePoint        ( const _ptg, _delta : TGIS_Point
                                ) : TGIS_Point ;
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
  ///   new point
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Measure is ignored.
  ///   </note>
  /// </remarks>
  function  GisMovePoint3D      ( const _ptg, _delta : TGIS_Point3D
                                ) : TGIS_Point3D ;
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
  ///   new point
  /// </returns>
  function  GisScalePoint       ( const _ref   : TGIS_Point ;
                                  const _ptg   : TGIS_Point ;
                                  const _scale : Double
                                ) : TGIS_Point ;
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
  ///   new point
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     Measure is ignored.
  ///   </note>
  /// </remarks>
  function  GisScalePoint3D     ( const _ref   : TGIS_Point3D ;
                                  const _ptg   : TGIS_Point3D ;
                                  const _scale : Double
                                ) : TGIS_Point3D ;
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
  ///   new point
  /// </returns>
  function  GisRotatePoint      ( const _ref   : TGIS_Point ;
                                  const _ptg   : TGIS_Point ;
                                  const _angle : Double
                                ) : TGIS_Point ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Calculates center point of given extent.
  /// </summary>
  /// <param name="_extent">
  ///   given extent
  /// </param>
  /// <returns>
  ///   new point
  /// </returns>
  function  GisCenterPoint      ( const _extent : TGIS_Extent
                                ) : TGIS_Point ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


  /// <summary>
  ///   Converts an angle in radians to compass (NW, SW etc) representation.
  /// </summary>
  /// <param name="_angle">
  ///   angle in radians
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function  GisRadToCompass     ( const _angle : Double
                                ) : String ;

  /// <summary>
  ///   Get default value for selected field type
  /// </summary>
  /// <param name="_type">
  ///   field type
  /// </param>
  /// <returns>
  ///   value
  /// </returns>
  function  GisDefaultField     ( const _type : TGIS_FieldType
                                ) : Variant ;

  /// <summary>
  ///   Returns directory in which samples data was installed.
  /// </summary>
  /// <returns>
  ///   full path
  /// </returns>
  /// <remarks>
  ///   Directory is define in \Windows\TatukGIS.ini file. See example.
  /// </remarks>
  function  GisSamplesDataDir   : String ;

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
  /// <remarks>
  ///   Directory is define in \Windows\TatukGIS.ini file. See example.
  /// </remarks>
  function GisSamplesDataDirDownload : String ;

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
  function  GisCanonicalSQLName ( const _name      : String
                                ) : String ; overload;

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
  function  GisCanonicalSQLName  ( const _name      : String ;
                                const _template  : String
                              ) : String ; overload;

  /// <summary>
  ///   Prepare normalized version of a field name.
  /// </summary>
  /// <param name="_name">
  ///   field name to be normalized
  /// </param>
  /// <returns>
  ///   Normalized version of a field name.
  /// </returns>
  function  GisNormalizedSQLName ( const _name      : String
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
  function  GisDeNormalizedSQLName ( const _name      : String
                              ) : String ; {$IFDEF GIS_INLINE} inline ; {$ENDIF}


  /// <summary>
  ///   Decode angle value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and
  ///   minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeAngle      ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign
                                ) ; overload;

  /// <summary>
  ///   Decode angle value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <param name="_prec">
  ///   precision - number of digits after decimal point measured on seconds
  ///   (0..15) ; default is 2
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeAngle      ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign ;
                                  const _prec : Integer
                                ) ; overload;

  /// <summary>
  ///   Decode latitude value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and
  ///   minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeLatitude   ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign
                                ) ; overload;

  /// <summary>
  ///   Decode latitude value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <param name="_prec">
  ///   precision - number of digits after decimal point measured on seconds
  ///   (0..15) ; default is 2
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeLatitude   ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign ;
                                  const _prec : Integer
                                ) ; overload;

  /// <summary>
  ///   Decode longitude value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and
  ///   minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeLongitude  ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign
                                ) ; overload;

  /// <summary>
  ///   Decode longitude value into degrees, minutes, seconds. If you want to
  ///   construct your own String based on such values keep in mind rounding
  ///   problem. So we recommend to use truncated values of degrees and minutes.
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
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <param name="_prec">
  ///   precision - number of digits after decimal point measured on seconds
  ///   (0..15) ; default is 2
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  procedure GisDecodeLongitude  ( const _val  : Double     ;
                                  var   _deg  : Integer    ;
                                  var   _min  : Integer    ;
                                  var   _sec  : Integer    ;
                                  var   _frac : Integer    ;
                                  var   _sign : TValueSign ;
                                  const _prec : Integer
                                ) ; overload;

  /// <summary>
  ///   <para>
  ///     Encode angle value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   angle value
  /// </returns>
  function GisEncodeAngle       ( const _deg  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode angle value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   angle value
  /// </returns>
  function GisEncodeAngle       ( const _deg  : Double     ;
                                  const _min  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode angle value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   angle value
  /// </returns>
  function GisEncodeAngle       ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode angle value from the values specified as the degrees, minutes,
  ///     seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
  ///   </para>
  /// </summary>
  /// <param name="_deg">
  ///   degrees; absolute value; if &lt;0 then force sign of result regardless
  ///   off _sign value
  /// </param>
  /// <param name="_min">
  ///   minutes; absolute value
  /// </param>
  /// <param name="_sec">
  ///   seconds; absolute value
  /// </param>
  /// <param name="_sign">
  ///   if &gt;= 0, then treated as &lt;0; if &lt; 0, then treated as &gt;0
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  /// <returns>
  ///   angle value
  /// </returns>
  function GisEncodeAngle       ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double     ;
                                  const _sign : TValueSign
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode latitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   latitude value
  /// </returns>
  function GisEncodeLatitude    ( const _deg  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode latitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   latitude value
  /// </returns>
  function GisEncodeLatitude    ( const _deg  : Double     ;
                                  const _min  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode latitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   latitude value
  /// </returns>
  function GisEncodeLatitude    ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode latitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
  ///   </para>
  /// </summary>
  /// <param name="_deg">
  ///   degrees; absolute value; if &lt;0 then force sign of result regardless
  ///   off _sign value
  /// </param>
  /// <param name="_min">
  ///   minutes; absolute value
  /// </param>
  /// <param name="_sec">
  ///   seconds; absolute value
  /// </param>
  /// <param name="_sign">
  ///   if &gt;= 0, then treated as North; if &lt; 0, then treated as South
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  /// <returns>
  ///   latitude value
  /// </returns>
  function GisEncodeLatitude    ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double     ;
                                  const _sign : TValueSign
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode longitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   longitude value
  /// </returns>
  function GisEncodeLongitude   ( const _deg  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode longitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   longitude value
  /// </returns>
  function GisEncodeLongitude   ( const _deg  : Double     ;
                                  const _min  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode longitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
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
  ///   longitude value
  /// </returns>
  function GisEncodeLongitude   ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double
                                ) : Double ; overload;

  /// <summary>
  ///   <para>
  ///     Encode longitude value from the values specified as the degrees,
  ///     minutes, seconds.
  ///   </para>
  ///   <para>
  ///     You can call ( 18, 54.5, 0, 1 ) or ( 18, 54, 30, 1 ) to achieve the
  ///     same result. If you provide ( 18, 54,5, 30, 1 ) it will be the same
  ///     as ( 19, 0, 0, 1 )
  ///   </para>
  /// </summary>
  /// <param name="_deg">
  ///   degrees; absolute value; if &lt;0 then force sign of result regardless
  ///   off _sign value
  /// </param>
  /// <param name="_min">
  ///   minutes; absolute value
  /// </param>
  /// <param name="_sec">
  ///   seconds; absolute value
  /// </param>
  /// <param name="_sign">
  ///   if &gt;= 0, then treated as East; if &lt; 0, then treated as West
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_COORDINATE_INVALID
  /// </exception>
  /// <returns>
  ///   longitude value
  /// </returns>
  function GisEncodeLongitude   ( const _deg  : Double     ;
                                  const _min  : Double     ;
                                  const _sec  : Double     ;
                                  const _sign : TValueSign
                                ) : Double ; overload;

  /// <summary>
  ///   Convert a angle in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisAngleToStr        ( const _val    : Double
                                ) : String ; overload;

  /// <summary>
  ///   Convert a angle in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <param name="_spaces">
  ///   if True, then text will be space separated
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisAngleToStr        ( const _val    : Double   ;
                                  const _spaces : Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert a angle in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisAngleToStr        ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert a angle in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisAngleToStr        ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer  ;
                                  const _leading: Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert latitude in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisLatitudeToStr     ( const _val    : Double
                                ) : String ; overload;

  /// <summary>
  ///   Convert latitude in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <param name="_spaces">
  ///   if True, then text will be space separated
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisLatitudeToStr     ( const _val    : Double   ;
                                  const _spaces : Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert latitude in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisLatitudeToStr     ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert latitude in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisLatitudeToStr     ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer  ;
                                  const _leading: Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert a longitude in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisLongitudeToStr    ( const _val    : Double
                                ) : String ; overload;

  /// <summary>
  ///   Convert a longitude in radians to a String representation.
  /// </summary>
  /// <param name="_val">
  ///   longitude in radians
  /// </param>
  /// <param name="_spaces">
  ///   if True, then text will be space separated
  /// </param>
  /// <returns>
  ///   text representation
  /// </returns>
  function GisLongitudeToStr    ( const _val    : Double   ;
                                  const _spaces : Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert a longitude in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisLongitudeToStr    ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert a longitude in radians to a String representation.
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
  ///   text representation
  /// </returns>
  function GisLongitudeToStr    ( const _val    : Double   ;
                                  const _spaces : Boolean  ;
                                  const _prec   : Integer  ;
                                  const _leading: Boolean
                                ) : String ; overload;

  /// <summary>
  ///   Convert a angle in a String representation into value in radians.
  /// </summary>
  /// <param name="_txt">
  ///   angle in a String format; must be in range -180..360
  /// </param>
  /// <returns>
  ///   angle value
  /// </returns>
  function GisStrToAngle        ( const _txt : String
                                ) : Double ;

  /// <summary>
  ///   Convert a latitude in a String representation into value in radians.
  /// </summary>
  /// <param name="_txt">
  ///   latitude in a String format; must be in range -90..90
  /// </param>
  /// <returns>
  ///   latitude value
  /// </returns>
  function GisStrToLatitude     ( const _txt : String
                                ) : Double ;

  /// <summary>
  ///   Convert longitude in a String representation into value in radians.
  /// </summary>
  /// <param name="_txt">
  ///   longitude in a String format; must be in range -180..360
  /// </param>
  /// <returns>
  ///   longitude value
  /// </returns>
  function GisStrToLongitude    ( const _txt : String
                                ) : Double ;

  /// <summary>
  ///   Structure for determining basic environment information.
  /// </summary>
  /// <returns>
  ///   info structure
  /// </returns>
  function GisEnvironmentInfo   : TGIS_EnvironmentInfo ;

  /// <summary>
  ///   List of keys using for use 3rd party products like PDF engine etc.
  /// </summary>
  /// <returns>
  ///   list handle
  /// </returns>
  /// <remarks>
  ///   Use KeyList.Add( name, key, value ) to store the key.
  /// </remarks>
  function GisKeyList           : TGIS_PasswordList ;

  /// <summary>
  ///   List of passwords for password protected layers.
  /// </summary>
  /// <returns>
  ///   list handle
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     List will be filled automatically upon responding for password event.
  ///   </note>
  ///   Use PasswordList.Add( layername, 'username', value ) etc. to store
  ///   password for selected layer.
  /// </remarks>
  function GisPasswordList      : TGIS_PasswordList ;

  /// <summary>
  ///   List of aliases.
  /// </summary>
  /// <returns>
  ///   list handle
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///     List will be field automatically upon responding for password event.
  ///   </note>
  ///   Use AliasList.Add( alias, value ) etc. to store alias. Use
  ///   GisAliasList.Resolve( 'text &lt;#ALIAS1#&gt; text &lt;&lt;#ALIAS2#&gt;
  ///   text' ) to resolve aliases.
  /// </remarks>
  function GisAliasList         : TGIS_AliasList ;


  /// <summary>
  ///   Access function for a general purpose data container.
  /// </summary>
  /// <returns>
  ///   list handle
  /// </returns>
  function GisMetadata          : TGIS_StringList ;

  /// <summary>
  ///   Check if global metadata list is assigned.
  /// </summary>
  /// <returns>
  ///   True if the list is assigned
  /// </returns>
  function GisMetadataAssigned  : Boolean ;

  /// <summary>
  ///   Read global metadata value.
  /// </summary>
  /// <param name="_name">
  ///   _name of the value
  /// </param>
  /// <param name="_default">
  ///   default value to be used if value does not exist
  /// </param>
  /// <returns>
  ///   read value or default
  /// </returns>
  function GisMetadataAsBoolean ( const _name    : String ;
                                  const _default : Boolean
                                ) : Boolean ;

  /// <summary>
  ///   Read global metadata value.
  /// </summary>
  /// <param name="_name">
  ///   _name of the value
  /// </param>
  /// <param name="_default">
  ///   default value to be used if value does not exist
  /// </param>
  /// <returns>
  ///   read value or default
  /// </returns>
  function GisMetadataAsInteger ( const _name    : String ;
                                  const _default : Integer
                                ) : Integer ;

  /// <summary>
  ///   Read global metadata value.
  /// </summary>
  /// <param name="_name">
  ///   _name of the value
  /// </param>
  /// <param name="_default">
  ///   default value to be used if value does not exist
  /// </param>
  /// <returns>
  ///   read value or default
  /// </returns>
  function GisMetadataAsFloat   ( const _name    : String ;
                                  const _default : Double
                                ) : Double ;

  /// <summary>
  ///   Read global metadata value.
  /// </summary>
  /// <param name="_name">
  ///   _name of the value
  /// </param>
  /// <param name="_default">
  ///   default value to be used if value does not exist
  /// </param>
  /// <returns>
  ///   read value or default
  /// </returns>
  function GisMetadataAsString  ( const _name    : String ;
                                  const _default : String
                                ) : String ;

  /// <summary>
  ///   Access function for the color ramp list.
  /// </summary>
  /// <returns>
  ///   list handle
  /// </returns>
  function GisColorRampList : TGIS_ColorRampList ;

  /// <summary>
  ///   Get system (current) code page.
  /// </summary>
  /// <returns>
  ///   code page
  /// </returns>
  function  GisSystemCodePage   : Integer ;

  /// <summary>
  ///   Checks if the system is metric.
  /// </summary>
  /// <returns>
  ///   True if is metric system
  /// </returns>
  function  GisIsMetricSystem   : Boolean ;


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
  procedure GisSetProxySettings ( const _server : String ;
                                  const _port   : Integer ;
                                  const _user   : String ;
                                  const _pass   : String ;
                                  const _domain : String
                                ) ;

  /// <summary>
  ///  Global proxy settings.
  /// </summary>
  /// <returns>
  ///   object handle
  /// </returns>
  function  GisProxySettings    : TGIS_ProxySettings ;

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
  procedure  GisSunPosition     ( const _ptg        : TGIS_Point ;
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
  procedure  GisSunPosition     ( const _ptg        : TGIS_Point ;
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
  function GisColorMap          ( const _index : Double ;
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
  function GisColorMapEx        ( const _class  : Integer ;
                                  const _colors : TGIS_ColorArray
                                ) : TGIS_ColorMapEx ;
  {$ENDREGION}

var
  {$IFDEF OXYGENE}
    /// <summary>
    ///  Type of core library.
    /// </summary>
    /// <remarks>
    ///   <note type="caution">
    ///     Only for internal use of TatukGIS.
    ///   </note>
    /// </remarks>
    GisDeveloperKernelType : TGIS_DeveloperKernelTypes ;
  {$ELSE}
    /// <summary>
    ///  Type of core library.
    /// </summary>
    /// <remarks>
    ///   <note type="caution">
    ///     Only for internal use of TatukGIS.
    ///   </note>
    /// </remarks>
    GisDeveloperKernelType : TGIS_DeveloperKernelTypesSet ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.DateUtils,
    System.SyncObjs,
    System.IOUtils,
    System.Zip,
    GisParams,
    GisInternals,
    GisResource,
    GisIniFiles ;
{$ENDIF}

{$IFDEF DCC}
  {$INCLUDE GisVersion.inc}
{$ENDIF}

var
  { KeyList storage. }
    FGisKeyList         : TGIS_PasswordList = nil ;
  { PasswordList storage. }
    FGisPasswordList    : TGIS_PasswordList = nil ;
  { GisMetadata storage. }
    FGisMetadata        : TGIS_StringList = nil ;
  { EnvironmentInfo storage. }
    FGisEnvironmentInfo : TGIS_EnvironmentInfo = nil ;
  { ProxySettings storage. }
    FGisProxySettings   : TGIS_ProxySettings = nil ;
  { ColorRamp storage. }
    FGisColorRampList : TGIS_ColorRampList = nil ;

{$REGION 'public functions'}
//==============================================================================
// GIS public functions
//==============================================================================

  function GisPoint( const _x, _y : Double ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point(_x, _y) ;
    {$ELSE}
      Result.X := _x ;
      Result.Y := _y ;
    {$ENDIF}
  end ;

  function GisPoint3DInt( const _x, _y, _z : Integer ) : TGIS_Point3DInt ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3DInt( _x, _y, _z ) ;
    {$ELSE}
      Result.X := _x ;
      Result.Y := _y ;
      Result.Z := _z ;
    {$ENDIF}
  end ;

  function GisPoint3D( const _x, _y, _z : Double
                     ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D(_x, _y, _z, 0) ;
    {$ELSE}
      Result.X := _x ;
      Result.Y := _y ;
      Result.Z := _z ;
      Result.M := 0  ;
    {$ENDIF}
  end ;

  function GisPoint3D( const _x, _y, _z : Double;
                       const _m : Double
                     ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D(_x, _y, _z, _m) ;
    {$ELSE}
      Result.X := _x ;
      Result.Y := _y ;
      Result.Z := _z ;
      Result.M := _m ;
    {$ENDIF}
  end ;

  function GisPoint3DFrom2D( const _ptg : TGIS_Point ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D(_ptg.X, _ptg.Y, 0, 0) ;
    {$ELSE}
      Result.X := _ptg.X ;
      Result.Y := _ptg.Y ;
      Result.Z := 0 ;
      Result.M := 0 ;
    {$ENDIF}
  end ;

  function GisPoint2DFrom3D( const _ptg : TGIS_Point3D ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point(_ptg.X, _ptg.Y) ;
    {$ELSE}
      Result.X := _ptg.X ;
      Result.Y := _ptg.Y ;
    {$ENDIF}
  end ;

  function GisExtent( const _xmin, _ymin, _xmax, _ymax : Double ) : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent(_xmin, _ymin, _xmax, _ymax) ;
    {$ELSE}
      Result.XMin := _xmin ;
      Result.YMin := _ymin ;
      Result.XMax := _xmax ;
      Result.YMax := _ymax ;
    {$ENDIF}
  end ;

  function GisExtent3D( const _xmin, _ymin, _zmin : Double ;
                        const _xmax, _ymax, _zmax : Double
                      ) : TGIS_Extent3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D(_xmin, _ymin, _zmin, 0, _xmax, _ymax, _zmax, 0) ;
    {$ELSE}
      Result.XMin := _xmin ;
      Result.YMin := _ymin ;
      Result.ZMin := _zmin ;
      Result.XMax := _xmax ;
      Result.YMax := _ymax ;
      Result.ZMax := _zmax ;
      Result.MMin := 0 ;
      Result.MMax := 0 ;
    {$ENDIF}
  end ;

  function GisExtent3D( const _xmin, _ymin, _zmin, _mmin : Double ;
                        const _xmax, _ymax, _zmax, _mmax : Double
                      ) : TGIS_Extent3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D(_xmin, _ymin, _zmin, _mmin, _xmax, _ymax, _zmax, _mmax) ;
    {$ELSE}
      Result.XMin := _xmin ;
      Result.YMin := _ymin ;
      Result.ZMin := _zmin ;
      Result.XMax := _xmax ;
      Result.YMax := _ymax ;
      Result.ZMax := _zmax ;
      Result.MMin := _mmin ;
      Result.MMax := _mmax ;
    {$ENDIF}
  end ;

  function  GisExtent3DFrom2D ( const _ext : TGIS_Extent
                              ) : TGIS_Extent3D ;
  begin
    // divide by 2 in order to avoid calculation overruns
    // and value is still enough to accommodate the whole world
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D ;
    {$ENDIF}
    Result.XMin :=   _ext.XMin ;
    Result.YMin :=   _ext.YMin ;
    Result.ZMin := - GIS_HALF_MAX_DOUBLE ;
    Result.MMin := - GIS_HALF_MAX_DOUBLE ;
    Result.XMax :=   _ext.XMax ;
    Result.YMax :=   _ext.YMax ;
    Result.ZMax :=   GIS_HALF_MAX_DOUBLE ;
    Result.MMax :=   GIS_HALF_MAX_DOUBLE ;
  end ;

  function  GisExtent2DFrom3D  ( const _ext : TGIS_Extent3D
                              ) : TGIS_Extent ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}
    Result.XMin :=   _ext.XMin ;
    Result.YMin :=   _ext.YMin ;
    Result.XMax :=   _ext.XMax ;
    Result.YMax :=   _ext.YMax ;
  end ;


  function GisLine(
    const _a : TGIS_Point ;
    const _b : TGIS_Point
  ) : TGIS_Line ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Line() ;
    {$ENDIF}
    Result.A := _TGIS_Point( _a ) ;
    Result.B := _TGIS_Point( _b ) ;
  end;

  function GisLine(
    const _ax : Double ;
    const _ay : Double ;
    const _bx : Double ;
    const _by : Double
  ) : TGIS_Line ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Line() ;
    {$ENDIF}
    Result.A := GisPoint( _ax, _ay ) ;
    Result.B := GisPoint( _bx, _by ) ;
  end;

  function GisLine3D(
    const _a : TGIS_Point3D ;
    const _b : TGIS_Point3D
  ) : TGIS_Line3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Line3D() ;
    {$ENDIF}
    Result.A := _TGIS_Point3D( _a ) ;
    Result.B := _TGIS_Point3D( _b ) ;
  end;

  function GisExtentArea ( const _extent : TGIS_Extent ) : Double ;
  begin
    Result := ( _extent.XMax - _extent.XMin ) * ( _extent.YMax - _extent.YMin ) ;
  end ;

  function GisExtentArea3D ( const _extent : TGIS_Extent3D ) : Double ;
  begin
    Result := GisExtentArea( GisExtent2DFrom3D( _extent ) ) ;
  end ;

  function GisExtentCubature( const _extent : TGIS_Extent ) : Double ;
  begin
    Result := 0 ;
  end ;

  function GisExtentCubature3D( const _extent : TGIS_Extent3D ) : Double ;
  begin
    Result := ( _extent.XMax - _extent.XMin ) *
              ( _extent.YMax - _extent.YMin ) *
              ( _extent.ZMax - _extent.ZMin ) ;
  end ;

  function GisIsSameValue(
    const _a : Double;
    const _b : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _a, _b, 0 ) ;
  end ;

  function GisIsSameValue(
    const _a    : Double ;
    const _b    : Double ;
    const _tol  : Double
  ) : Boolean ;
  var
    aIsNan  : Boolean ;
    bIsNan  : Boolean ;
    epsilon : Double ;
  begin
    aIsNan := IsNan( _a ) ;
    bIsNan := IsNan( _b ) ;

    if aIsNan or bIsNan then begin
      if aIsNan and bIsNan then
        Result := True
      else
        Result := False ;

      exit;
    end ;

    if _tol = 0 then
      epsilon := Max( Min( Abs(_a), Abs(_b) ) * GIS_DOUBLE_RESOLUTION, GIS_DOUBLE_RESOLUTION )
    else
      epsilon := _tol;

    if _a > _b then
      Result := (_a - _b) <= epsilon
    else
      Result := (_b - _a) <= epsilon ;
  end ;

  function GisIsSamePoint(
    const _ptg1 : TGIS_Point ;
    const _ptg2 : TGIS_Point
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y ) ;
  end ;

  function GisIsSamePoint(
    const _ptg1 : TGIS_Point ;
    const _ptg2 : TGIS_Point ;
    const _tol  : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X, _tol ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y, _tol ) ;
  end ;

  function GisIsSamePoint3D(
    const _ptg1 : TGIS_Point3D ;
    const _ptg2 : TGIS_Point3D
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y ) and
              GisIsSameValue( _ptg1.Z, _ptg2.Z ) ;
  end ;

  function GisIsSamePoint3D(
    const _ptg1 : TGIS_Point3D ;
    const _ptg2 : TGIS_Point3D ;
    const _tol  : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X, _tol ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y, _tol ) and
              GisIsSameValue( _ptg1.Z, _ptg2.Z, _tol ) ;
  end ;

  function GisIsSamePoint3DM(
    const _ptg1 : TGIS_Point3D ;
    const _ptg2 : TGIS_Point3D
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y ) and
              GisIsSameValue( _ptg1.Z, _ptg2.Z ) and
              GisIsSameValue( _ptg1.M, _ptg2.M ) ;
  end ;

  function GisIsSamePoint3DM(
    const _ptg1 : TGIS_Point3D ;
    const _ptg2 : TGIS_Point3D ;
    const _tol  : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _ptg1.X, _ptg2.X, _tol ) and
              GisIsSameValue( _ptg1.Y, _ptg2.Y, _tol ) and
              GisIsSameValue( _ptg1.Z, _ptg2.Z, _tol ) and
              GisIsSameValue( _ptg1.M, _ptg2.M, _tol ) ;
  end ;

  function GisIsSameExtent(
    const _extent1 : TGIS_Extent ;
    const _extent2 : TGIS_Extent
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax ) ;
  end ;

  function GisIsSameExtent(
    const _extent1 : TGIS_Extent ;
    const _extent2 : TGIS_Extent ;
    const _tol     : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin, _tol ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin, _tol ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax, _tol ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax, _tol ) ;
  end ;

  function GisIsSameExtent3D(
    const _extent1 : TGIS_Extent3D ;
    const _extent2 : TGIS_Extent3D
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin ) and
              GisIsSameValue( _extent1.ZMin, _extent2.ZMin ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax ) and
              GisIsSameValue( _extent1.ZMax, _extent2.ZMax ) ;
  end ;

  function GisIsSameExtent3D(
    const _extent1 : TGIS_Extent3D ;
    const _extent2 : TGIS_Extent3D ;
    const _tol     : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin, _tol ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin, _tol ) and
              GisIsSameValue( _extent1.ZMin, _extent2.ZMin, _tol ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax, _tol ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax, _tol ) and
              GisIsSameValue( _extent1.ZMax, _extent2.ZMax, _tol ) ;
  end ;

  function GisIsSameExtent3DM(
    const _extent1 : TGIS_Extent3D ;
    const _extent2 : TGIS_Extent3D
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin ) and
              GisIsSameValue( _extent1.ZMin, _extent2.ZMin ) and
              GisIsSameValue( _extent1.MMin, _extent2.MMin ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax ) and
              GisIsSameValue( _extent1.ZMax, _extent2.ZMax ) and
              GisIsSameValue( _extent1.MMax, _extent2.MMax ) ;
  end ;

  function GisIsSameExtent3DM(
    const _extent1 : TGIS_Extent3D ;
    const _extent2 : TGIS_Extent3D ;
    const _tol     : Double
  ) : Boolean ;
  begin
    Result := GisIsSameValue( _extent1.XMin, _extent2.XMin, _tol ) and
              GisIsSameValue( _extent1.YMin, _extent2.YMin, _tol ) and
              GisIsSameValue( _extent1.ZMin, _extent2.ZMin, _tol ) and
              GisIsSameValue( _extent1.MMin, _extent2.MMin, _tol ) and
              GisIsSameValue( _extent1.XMax, _extent2.XMax, _tol ) and
              GisIsSameValue( _extent1.YMax, _extent2.YMax, _tol ) and
              GisIsSameValue( _extent1.ZMax, _extent2.ZMax, _tol ) and
              GisIsSameValue( _extent1.MMax, _extent2.MMax, _tol ) ;
  end ;

  function GisContainExtent( const _extent1, _extent2 : TGIS_Extent ) : Boolean ;
  begin
    Result := ( _extent1.XMin <= _extent2.XMin ) and
              ( _extent1.YMin <= _extent2.YMin ) and
              ( _extent1.XMax >= _extent2.XMax ) and
              ( _extent1.YMax >= _extent2.YMax ) ;
  end ;

  function GisIsEmptyExtent( const _extent : TGIS_Extent
                           ) : Boolean ;
  begin
    Result := ( _extent.XMax <= _extent.XMin ) or
              ( _extent.YMax <= _extent.YMin ) ;
  end ;

  function GisIsEmptyExtent3D( const _extent : TGIS_Extent3D
                             ) : Boolean ;
  begin
    Result := ( _extent.XMax <= _extent.XMin ) or
              ( _extent.YMax <= _extent.YMin ) or
              ( _extent.ZMax <= _extent.ZMin ) ;
  end ;

  function GisWholeWorld : TGIS_Extent;
  begin
    // divide by 2 in order to avoid calculation overruns
    // and value is still enough to accommodate the whole world
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}
    Result.XMin := - GIS_HALF_MAX_DOUBLE ;
    Result.XMax :=   GIS_HALF_MAX_DOUBLE ;
    Result.YMin := - GIS_HALF_MAX_DOUBLE ;
    Result.YMax :=   GIS_HALF_MAX_DOUBLE ;
  end ;

  function GisIsValidPtg(
    const _ptg : TGIS_Point
  ) : Boolean ;
  begin
    Result := not ( ( _ptg.X > GIS_MAX_SINGLE ) or
                    ( _ptg.Y > GIS_MAX_SINGLE )
                  ) ;
  end;

  function GisIsValidPtg3D(
    const _ptg : TGIS_Point3D
  ) : Boolean ;
  begin
    Result := not ( ( _ptg.X > GIS_MAX_SINGLE ) or
                    ( _ptg.Y > GIS_MAX_SINGLE ) or
                    ( _ptg.X > GIS_MAX_SINGLE )
                  ) ;
  end;

  function GisIsValidExtent(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    Result := not ( ( _extent.XMin > GIS_MAX_SINGLE ) or
                    ( _extent.YMin > GIS_MAX_SINGLE ) or
                    ( _extent.XMax > GIS_MAX_SINGLE ) or
                    ( _extent.YMax > GIS_MAX_SINGLE )
                  ) ;
  end;

  function GisIsValidExtent3D(
    const _extent : TGIS_Extent3D
  ) : Boolean ;
  begin
    Result := not ( ( _extent.XMin > GIS_MAX_SINGLE ) or
                    ( _extent.XMax > GIS_MAX_SINGLE ) or
                    ( _extent.YMin > GIS_MAX_SINGLE ) or
                    ( _extent.YMax > GIS_MAX_SINGLE ) or
                    ( _extent.ZMin > GIS_MAX_SINGLE ) or
                    ( _extent.ZMax > GIS_MAX_SINGLE )
                  ) ;
  end;


  function GisWholeWorld3D : TGIS_Extent3D;
  begin
    // divide by 2 in order to avoid calculation overruns
    // and value is still enough to accommodate the whole world
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D ;
    {$ENDIF}
    Result.XMin := - GIS_HALF_MAX_DOUBLE ;
    Result.YMin := - GIS_HALF_MAX_DOUBLE ;
    Result.ZMin := - GIS_HALF_MAX_DOUBLE ;
    Result.MMin := - GIS_HALF_MAX_DOUBLE ;
    Result.XMax :=   GIS_HALF_MAX_DOUBLE ;
    Result.YMax :=   GIS_HALF_MAX_DOUBLE ;
    Result.ZMax :=   GIS_HALF_MAX_DOUBLE ;
    Result.MMax :=   GIS_HALF_MAX_DOUBLE ;
  end ;

  function GisIsWholeWorld( const _extent : TGIS_Extent ) : Boolean ;
  var
    d  : Double ;
  begin
    d := GisWholeWorld.XMax / 10 ;
    Result := ( _extent.XMin < -d ) and
              ( _extent.YMin < -d ) and
              ( _extent.XMax >  d ) and
              ( _extent.YMax >  d ) ;
  end ;

  function GisIsWholeWorld3D( const _extent : TGIS_Extent3D ) : Boolean ;
  var
    d : Double ;
  begin
    d := GisWholeWorld.XMax / 10 ;
    Result := ( _extent.XMin < -d ) and
              ( _extent.YMin < -d ) and
              ( _extent.ZMin < -d ) and
              ( _extent.XMax >  d ) and
              ( _extent.YMax >  d ) and
              ( _extent.ZMax >  d ) ;
  end ;

  function GisNoWorld : TGIS_Extent;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}
    Result.XMin :=   1 ;
    Result.XMax := - 1 ;
    Result.YMin :=   1 ;
    Result.YMax := - 1 ;
  end ;

  function GisNoWorld3D : TGIS_Extent3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D ;
    {$ENDIF}
    Result.XMin :=   1 ;
    Result.YMin :=   1 ;
    Result.ZMin :=   1 ;
    Result.MMin :=   1 ;
    Result.XMax := - 1 ;
    Result.YMax := - 1 ;
    Result.ZMax := - 1 ;
    Result.MMax := - 1 ;
  end ;

  function GisIsNoWorld( const _extent : TGIS_Extent ) : Boolean ;
  begin
    Result := ( _extent.XMax < _extent.XMin ) or
              ( _extent.YMax < _extent.YMin ) ;
  end ;

  function GisIsNoWorld3D( const _extent : TGIS_Extent3D ) : Boolean ;
  begin
    Result := ( _extent.XMax < _extent.XMin ) or
              ( _extent.YMax < _extent.YMin ) or
              ( _extent.ZMax < _extent.ZMin ) ;
  end ;

  function GisDMin( const _val1, _val2 : Double ) : Double ;
  begin
    if _val1 < _val2 then Result := _val1
                     else Result := _val2 ;
  end ;

  function GisDMax( const _val1, _val2 : Double ) : Double ;
  begin
    if _val1 > _val2 then Result := _val1
                     else Result := _val2 ;
  end ;

  function GisMaxExtent( const _extent1, _extent2 : TGIS_Extent ) : TGIS_Extent;
  begin
    if GisIsNoWorld( _extent1 ) or GisIsNoWorld( _extent2 ) then begin
      if GisIsNoWorld( _extent1 ) then Result := _TGIS_Extent( _extent2 )
                                  else Result := _TGIS_Extent( _extent1 ) ;
    end
    else begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent ;
      {$ENDIF}
      Result.XMin := Min( _extent1.XMin, _extent2.XMin ) ;
      Result.YMin := Min( _extent1.YMin, _extent2.YMin ) ;
      Result.XMax := Max( _extent1.XMax, _extent2.XMax ) ;
      Result.YMax := Max( _extent1.YMax, _extent2.YMax ) ;
    end ;
  end ;

  function GisMaxExtent3D( const _extent1, _extent2 : TGIS_Extent3D
                         ) : TGIS_Extent3D ;
  begin
    if GisIsNoWorld3D( _extent1 ) or GisIsNoWorld3D( _extent2 ) then begin
      if GisIsNoWorld3D( _extent1 ) then Result := _TGIS_Extent3D( _extent2 )
                                    else Result := _TGIS_Extent3D( _extent1 ) ;
    end
    else begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent3D ;
      {$ENDIF}
      Result.XMin := Min( _extent1.XMin, _extent2.XMin ) ;
      Result.YMin := Min( _extent1.YMin, _extent2.YMin ) ;
      Result.ZMin := Min( _extent1.ZMin, _extent2.ZMin ) ;
      if not IsNan( _extent1.MMin ) and not IsNan( _extent2.MMin ) then
      Result.MMin := Min( _extent1.MMin, _extent2.MMin ) ;
      Result.XMax := Max( _extent1.XMax, _extent2.XMax ) ;
      Result.YMax := Max( _extent1.YMax, _extent2.YMax ) ;
      Result.ZMax := Max( _extent1.ZMax, _extent2.ZMax ) ;
      if not IsNan( _extent1.MMax ) and not IsNan( _extent2.MMax ) then
      Result.MMax := Max( _extent1.MMax, _extent2.MMax ) ;
    end ;
  end ;

  function GisIsCommonExtent( const _extent1, _extent2 : TGIS_Extent
                            ) : Boolean ;
  begin
    if GisIsNoWorld( _extent1 ) or
       GisIsNoWorld( _extent2 )
    then begin
      Result := False ;
      exit ;
    end;

    Result := ( _extent1.XMin <= _extent2.XMax ) and
              ( _extent1.XMax >= _extent2.XMin ) and
              ( _extent1.YMin <= _extent2.YMax ) and
              ( _extent1.YMax >= _extent2.YMin )
  end ;

  function GisIsCommonExtent3D( const _extent1, _extent2 : TGIS_Extent3D
                              ) : Boolean ;
  begin
    if GisIsNoWorld3D( _extent1 ) or
       GisIsNoWorld3D( _extent2 )
    then begin
      Result := False ;
      exit ;
    end;

    Result := ( _extent1.XMin <= _extent2.XMax ) and
              ( _extent1.XMax >= _extent2.XMin ) and
              ( _extent1.YMin <= _extent2.YMax ) and
              ( _extent1.YMax >= _extent2.YMin ) and
              ( _extent1.ZMin <= _extent2.ZMax ) and
              ( _extent1.ZMax >= _extent2.ZMin )
  end ;

  function GisIsContainExtent( const _extent1, _extent2 : TGIS_Extent
                             ) : Boolean ;
  begin
    Result := (_extent1.XMin >= _extent2.XMin ) and
              (_extent1.XMax <= _extent2.XMax ) and
              (_extent1.YMin >= _extent2.YMin ) and
              (_extent1.YMax <= _extent2.YMax ) ;
  end ;

  function GisIsContainExtent3D( const _extent1, _extent2 : TGIS_Extent3D
                               ) : Boolean ;
  begin
    Result := (_extent1.XMin >= _extent2.XMin ) and
              (_extent1.XMax <= _extent2.XMax ) and
              (_extent1.YMin >= _extent2.YMin ) and
              (_extent1.YMax <= _extent2.YMax ) and
              (_extent1.ZMin >= _extent2.ZMin ) and
              (_extent1.ZMax <= _extent2.ZMax ) ;
  end ;

  function GisCommonExtent( const _extent1, _extent2 : TGIS_Extent
                          ) : TGIS_Extent;
  begin
    if not GisIsCommonExtent( _extent1, _extent2 ) then begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent ;
      {$ENDIF}
      Result.XMin := 0 ;
      Result.XMax := 0 ;
      Result.YMin := 0 ;
      Result.YMax := 0 ;

      exit ;
    end ;
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}
    Result.XMin := Max( _extent1.XMin, _extent2.XMin ) ;
    Result.YMin := Max( _extent1.YMin, _extent2.YMin ) ;
    Result.XMax := Min( _extent1.XMax, _extent2.XMax ) ;
    Result.YMax := Min( _extent1.YMax, _extent2.YMax ) ;
  end ;

  function GisCommonExtent3D( const _extent1, _extent2 : TGIS_Extent3D
                            ) : TGIS_Extent3D ;
  begin
    if not GisIsCommonExtent3D( _extent1, _extent2 ) then begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent3D ;
      {$ENDIF}
      Result.XMin := 0 ;
      Result.XMax := 0 ;
      Result.YMin := 0 ;
      Result.YMax := 0 ;
      Result.ZMin := 0 ;
      Result.ZMax := 0 ;
      Result.MMin := 0 ;
      Result.MMax := 0 ;

      exit ;
    end
    else if GisIsNoWorld3D( _extent1 ) or GisIsNoWorld3D( _extent2 ) then begin
      if GisIsNoWorld3D( _extent1 ) then Result := _TGIS_Extent3D( _extent2 )
                                    else Result := _TGIS_Extent3D( _extent1 ) ;
    end
    else begin
      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Extent3D ;
      {$ENDIF}
      Result.XMin := Max( _extent1.XMin, _extent2.XMin ) ;
      Result.YMin := Max( _extent1.YMin, _extent2.YMin ) ;
      Result.XMax := Min( _extent1.XMax, _extent2.XMax ) ;
      Result.YMax := Min( _extent1.YMax, _extent2.YMax ) ;
      Result.ZMin := Max( _extent1.ZMin, _extent2.ZMin ) ;
      Result.ZMax := Min( _extent1.ZMax, _extent2.ZMax ) ;
      if not IsNan( _extent1.MMin ) and not IsNan( _extent2.MMin ) then
      Result.MMin := Max( _extent1.MMin, _extent2.MMin ) ;
      if not IsNan( _extent1.MMax ) and not IsNan( _extent2.MMax ) then
      Result.MMax := Min( _extent1.MMax, _extent2.MMax ) ;
    end ;
  end ;

  function GisIsPointInsideExtent( const _ptg    : TGIS_Point ;
                                   const _extent : TGIS_Extent
                                 ) : Boolean ;
  begin
    if ( ( _ptg.X >= _extent.XMin ) and
         ( _ptg.X <= _extent.XMax )
       ) and
       ( ( _ptg.Y >= _extent.YMin ) and
         ( _ptg.Y <= _extent.YMax )
       )
    then Result := True
    else Result := False ;
  end ;

  function GisLineLength( const _lineA, _lineB : TGIS_Point ) : Double ;
  begin
    Result := Sqrt( Sqr( _lineA.X - _lineB.X ) +
                    Sqr( _lineA.Y - _lineB.Y)
                  ) ;
  end ;

  function GisPoint2Point( const _ptg1, _ptg2 : TGIS_Point ) : Double ;
  begin
    Result := Sqrt( Sqr( _ptg1.X - _ptg2.X ) +
                    Sqr( _ptg1.Y - _ptg2.Y)
                  ) ;
  end ;

  function GisPoint2Point3D( const _ptg1, _ptg2 : TGIS_Point3D ) : Double ;
  begin
    Result := Sqrt( Sqr( _ptg1.X - _ptg2.X ) +
                    Sqr( _ptg1.Y - _ptg2.Y ) +
                    Sqr( _ptg1.Z - _ptg2.Z )
                  ) ;
  end ;

  function GisLine2Point(  const _lineA, _lineB, _ptg : TGIS_Point ) : Double ;
  var
    s,
    r, m, m2  : Double ;
  begin
    // is line length = 0 ?
    if (( _lineA.X = _lineB.X ) and ( _lineA.Y = _lineB.Y ) ) then
      Result := Sqrt( Sqr( _lineA.X - _ptg.X ) +
                      Sqr( _lineA.Y - _ptg.Y)
                    )
    else begin
      m2 := Sqr(_lineB.X -_lineA.X) + Sqr(_lineB.Y -_lineA.Y) ;
      r := ( (_lineA.Y - _ptg.Y) * (_lineA.Y -_lineB.Y) -
             (_lineA.X - _ptg.X) * (_lineB.X -_lineA.X) ) / m2;

      if ( (r >= 0) and (r <= 1) ) then  // is point left-bottom or right-up
      begin
        s := ( ( _lineA.Y - _ptg.Y ) * ( _lineB.X -_lineA.X ) -
               ( _lineA.X - _ptg.X ) * ( _lineB.Y -_lineA.Y ) ) / m2;
        m := Sqrt( m2 ) ;

        Result := Abs( s*m ) ;
      end
      else begin
        // r < 0: Result := GisPoint2Point( _lineA, _ptg ) ;
        if r < 0 then
          Result := Sqrt( Sqr( _lineA.X - _ptg.X ) +
                      Sqr( _lineA.Y - _ptg.Y)
                    )
        else
        // r > 1: Result := GisPoint2Point( _lineB, _ptg ) ;
          Result := Sqrt( Sqr( _lineB.X - _ptg.X ) +
                      Sqr( _lineB.Y - _ptg.Y)
                    ) ;
      end ;
    end ;
  end ;

  function GisLine2PointFuzzy(
    const _lineA, _lineB, _ptg : TGIS_Point
  ) : Double ;
  var
    s,
    r, m, m2  : Double ;
    res       : Double ;
  const
    DIVIDER   = 10000 ;
  begin
    // is line length = 0 ?
    if (( _lineA.X = _lineB.X ) and ( _lineA.Y = _lineB.Y ) ) then
      // Result := GisPoint2Point( _lineA, _ptg )
      Result := Sqrt( Sqr( _lineA.X - _ptg.X ) +
                      Sqr( _lineA.Y - _ptg.Y)
                    )
    else begin
      m2 := Sqr(_lineB.X -_lineA.X) + Sqr(_lineB.Y -_lineA.Y) ;
      r := ( (_lineA.Y - _ptg.Y) * (_lineA.Y -_lineB.Y) -
             (_lineA.X - _ptg.X) * (_lineB.X -_lineA.X) ) / m2;

      if ( (r >= 0) and (r <= 1) ) then  // is point left-bottom or right-up
      begin
        s := ( ( _lineA.Y - _ptg.Y ) * ( _lineB.X -_lineA.X ) -
               ( _lineA.X - _ptg.X ) * ( _lineB.Y -_lineA.Y ) ) / m2;
        m := Sqrt( m2 ) ;

        Result := Abs( s*m ) ;
      end
      else begin
        // r < 0: Result := GisPoint2Point( _lineA, _ptg ) ;
        if r < 0 then begin
          res := Sqrt( Sqr( _lineA.X - _ptg.X ) +
                       Sqr( _lineA.Y - _ptg.Y)
                     ) ;
          Result := res * ( 1 + Abs( r ) / DIVIDER ) ;
        end
        else begin
        // r > 1: Result := GisPoint2Point( _lineB, _ptg ) ;
          res := Sqrt( Sqr( _lineB.X - _ptg.X ) +
                       Sqr( _lineB.Y - _ptg.Y)
                     ) ;
          Result := res * ( 1 + Abs( r-1 ) / DIVIDER ) ;
        end ;
      end ;
    end ;
  end ;

//? TODO: publish to DK
type
  TVector = record
    X : Double ;
    Y : Double ;
    Z : Double ;

    constructor Create( const _x, _y, _z : Double ) ;
    function Magnitude : Double ;

    class operator Add( v1, v2 : TVector ) : TVector ;
    class operator Subtract( v1, v2 : TVector ) : TVector ;
    class operator Multiply( v1, v2 : TVector ) : TVector ;
  end;

  constructor TVector.Create( const _x, _y, _z : Double ) ;
  begin
    X := _x ;
    Y := _y ;
    Z := _z ;
  end;

  function TVector.Magnitude : Double ;
  begin
    Result := Sqrt( Power( X, 2 ) + Power( Y, 2 ) + Power( Z, 2 ) ) ;
  end;

  class operator TVector.Add( v1, v2 : TVector ) : TVector ;
  var
    x, y, z : Double ;
  begin
    x := v1.X + v2.X ;
    y := v1.Y + v2.Y ;
    z := v1.Z + v2.Z ;

    Result := TVector.Create( x, y, z ) ;
  end;

  class operator TVector.Subtract( v1, v2 : TVector ) : TVector ;
  var
    x, y, z : Double ;
  begin
    x := v1.X - v2.X ;
    y := v1.Y - v2.Y ;
    z := v1.Z - v2.Z ;

    Result := TVector.Create( x, y, z ) ;
  end;

  class operator TVector.Multiply( v1, v2 : TVector ) : TVector ;
  var
    x, y, z : Double ;
  begin
    x := v1.Y * v2.Z - v1.Z * v2.Y ;
    y := v1.Z * v2.X - v1.X * v2.Z ;
    z := v1.X * v2.Y - v1.Y * v2.X ;

    Result := TVector.Create( x, y, z ) ;
  end;

  //? TODO: make inline, but first make TVector public
  function GisLine2Point3D( const _lineA, _lineB, _ptg : TGIS_Point3D ) : Double ;
  var
    vec_lineA : TVector;
    vec_lineB : TVector;
    vec_ptg   : TVector;
    vec_AB    : TVector;
    vec_AP    : TVector;
    area      : Double ;
    len_AB    : Double ;
    u         : Double ;
  begin
    vec_lineA := TVector.Create( _lineA.X, _lineA.Y, _lineA.Z ) ;
    vec_lineB := TVector.Create( _lineB.X, _lineB.Y, _lineB.Z ) ;
    vec_ptg := TVector.Create( _ptg.X, _ptg.Y, _ptg.Z ) ;

    vec_AB := vec_lineB - vec_lineA ;
    vec_AP := vec_ptg - vec_lineA ;
    area := ( vec_AB * vec_AP ).Magnitude ;
    len_AB := vec_AB.Magnitude ;

    if GisIsSameValue( len_AB, 0 ) then begin
      Result := GisPoint2Point3D( _lineA, _ptg ) ;
      Exit;
    end;

    // determine the perpendicular position:
    // u < 0     - perpendicular outside line start
    // 0 < u < 1 - perpendicular inside line
    // 1 < u     - perpendicular outside line end
    u := ( ( ( vec_ptg.X - vec_lineA.X ) * ( vec_lineB.X - vec_lineA.X ) ) +
               ( ( vec_ptg.Y - vec_lineA.Y ) * ( vec_lineB.Y - vec_lineA.Y ) ) +
               ( ( vec_ptg.Z - vec_lineA.Z ) * ( vec_lineB.Z - vec_lineA.Z ) ) ) /
               ( len_AB * len_AB );

    if u < 0 then
      Result := GisPoint2Point3D( _lineA, _ptg )
    else if u > 1 then
      Result := GisPoint2Point3D( _lineB, _ptg )
    else
      // perpendicular distance between a point and a line
      Result := area / len_AB ;
  end ;

  function GisLine2Point3DFuzzy(
    const _lineA, _lineB, _ptg : TGIS_Point3D
  ) : Double ;
  begin
    Result := GisLine2Point3D( _lineA, _lineB, _ptg ) ;
  end ;

  function  GisLine2Line( const _line1A, _line1B : TGIS_Point ;
                          const _line2A, _line2B : TGIS_Point
                        ) : Double ;
  begin
    Result := GIS_MAX_DOUBLE ;

    Result := Min( Result, GisLine2Point( _line1A, _line1B, _line2A ) ) ;
    Result := Min( Result, GisLine2Point( _line1A, _line1B, _line2B ) ) ;
    Result := Min( Result, GisLine2Point( _line2A, _line2B, _line1A ) ) ;
    Result := Min( Result, GisLine2Point( _line2A, _line2B, _line1B ) ) ;
  end ;

  function  GisLine2Line3D( const _line1A, _line1B : TGIS_Point3D ;
                            const _line2A, _line2B : TGIS_Point3D
                          ) : Double ;
  begin
    Result := GIS_MAX_DOUBLE ;

    Result := Min( Result, GisLine2Point3D( _line1A, _line1B, _line2A ) ) ;
    Result := Min( Result, GisLine2Point3D( _line1A, _line1B, _line2B ) ) ;
    Result := Min( Result, GisLine2Point3D( _line2A, _line2B, _line1A ) ) ;
    Result := Min( Result, GisLine2Point3D( _line2A, _line2B, _line1B ) ) ;
  end ;

  function GisPointOnLine( const _lineA, _lineB, _ptg : TGIS_Point
                         ) : TGIS_Point ;
  var
    proj    : TGIS_Point ;
    r, m2   : Double ;
    d1, d2  : Double ;
  begin
    // is line length = 0 ?
    if (( _lineA.X = _lineB.X ) and ( _lineA.Y = _lineB.Y ) ) then
       proj := _lineA
    else begin
      m2 := Sqr(_lineB.X -_lineA.X) + Sqr(_lineB.Y -_lineA.Y) ;
      r := ( (_lineA.Y - _ptg.Y) * (_lineA.Y -_lineB.Y) -
             (_lineA.X - _ptg.X) * (_lineB.X -_lineA.X) ) / m2;
      if ( (r >= 0) and (r <= 1) ) then begin // is point left-bottom or right-up
        proj := GisPoint( _lineA.X + r*(_lineB.X - _lineA.X),
                          _lineA.Y + r*(_lineB.Y - _lineA.Y)
                         )  ;
      end
      else begin
        // d1 := GisPoint2Point( _lineA, _ptg ) ;
        d1 := Sqrt( Sqr( _lineA.X - _ptg.X ) +
                    Sqr( _lineA.Y - _ptg.Y)
                  ) ;

        // d2 := GisPoint2Point( _lineB, _ptg ) ;
        d2 := Sqrt( Sqr( _lineB.X - _ptg.X ) +
                    Sqr( _lineB.Y - _ptg.Y)
                  ) ;

        if d1 < d2 then proj := _lineA
                   else proj := _lineB
      end ;
    end ;

    Result := proj ;
  end ;

  function GisPointOnLine3D( const _lineA, _lineB, _ptg : TGIS_Point3D
                         ) : TGIS_Point3D ;
  begin
    Result := GisPoint3DFrom2D( GisPointOnLine( GisPoint2DFrom3D( _lineA ),
                                                GisPoint2DFrom3D( _lineB ),
                                                GisPoint2DFrom3D( _ptg   )
                                               )
                              ) ;
  end ;

  function GisIsLinesCommonPoint( const _line1 : TGIS_Line ;
                                  const _line2 : TGIS_Line ) : Boolean ;
  var
    r, s   : Double;
    m   : Double;
  begin

    m := (_line1.B.X-_line1.A.X)*(_line2.B.Y-_line2.A.Y)
       - (_line1.B.Y-_line1.A.Y)*(_line2.B.X-_line2.A.X);

    if m <> 0 then
    begin
      r := ((_line1.A.Y-_line2.A.Y)*(_line2.B.X-_line2.A.X)
           - (_line1.A.X-_line2.A.X)*(_line2.B.Y-_line2.A.Y))/m;

      if (r>=0) and (r<=1) then begin

        s := ((_line1.A.Y-_line2.A.Y)*(_line1.B.X-_line1.A.X)
             - (_line1.A.X-_line2.A.X)*(_line1.B.Y-_line1.A.Y))/m;
        if (s>=0) and (s<=1) then
          Result := True
        else
          Result := False ;
      end
      else
        Result := False ;
    end
    else
      Result := False ;
  end ;

  function GisGetLinesCrossing( const _line1 : TGIS_Line ;
                                const _line2 : TGIS_Line ;
                                var   _ptg   : TGIS_Point
                              ) : Boolean ;
  var
    r, s   : Double;
    m   : Double;
  begin

    m := (_line1.B.X-_line1.A.X)*(_line2.B.Y-_line2.A.Y)
       - (_line1.B.Y-_line1.A.Y)*(_line2.B.X-_line2.A.X);
    if m <> 0 then
    begin
      r := ((_line1.A.Y-_line2.A.Y)*(_line2.B.X-_line2.A.X)
           - (_line1.A.X-_line2.A.X)*(_line2.B.Y-_line2.A.Y))/m;

      if (r>=0) and (r<=1) then begin

        s := ((_line1.A.Y-_line2.A.Y)*(_line1.B.X-_line1.A.X)
             - (_line1.A.X-_line2.A.X)*(_line1.B.Y-_line1.A.Y))/m;
        if (s>=0) and (s<=1) then begin
          _ptg.X := _line1.A.X + r *(_line1.B.X - _line1.A.X);
          _ptg.Y := _line1.A.Y + r *(_line1.B.Y - _line1.A.Y);
          Result := True ;
        end
        else
          Result := False ;
      end
      else
        Result := False ;
    end
    else
      Result := False ;
  end ;

  function GisPointsDelta( const _ptg1, _ptg2 : TGIS_Point ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := _ptg1.X - _ptg2.X ;
    Result.Y := _ptg1.Y - _ptg2.Y ;
  end ;

  function GisPointsDelta3D( const _ptg1, _ptg2 : TGIS_Point3D ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg1.X - _ptg2.X ;
    Result.Y := _ptg1.Y - _ptg2.Y ;
    Result.Z := _ptg1.Z - _ptg2.Z ;
    Result.M := _ptg1.M - _ptg2.M ;
  end ;

  function GisMovePoint( const _ptg, _delta : TGIS_Point ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := _ptg.X + _delta.X ;
    Result.Y := _ptg.Y + _delta.Y ;
  end ;

  function GisMovePoint3D( const _ptg, _delta : TGIS_Point3D ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg.X + _delta.X ;
    Result.Y := _ptg.Y + _delta.Y ;
    Result.Z := _ptg.Z + _delta.Z ;
    Result.M := _ptg.M + _delta.M ;
  end ;

  function GisScalePoint( const _ref : TGIS_Point ;
                          const _ptg : TGIS_Point ;
                          const _scale : Double
                        ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := _ref.X + ( _ptg.X - _ref.X ) * _scale ;
    Result.Y := _ref.Y + ( _ptg.Y - _ref.Y ) * _scale ;
  end ;

  function GisScalePoint3D( const _ref : TGIS_Point3D ;
                            const _ptg : TGIS_Point3D ;
                            const _scale : Double
                          ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ref.X + ( _ptg.X - _ref.X ) * _scale ;
    Result.Y := _ref.Y + ( _ptg.Y - _ref.Y ) * _scale ;
    Result.Z := _ref.Z + ( _ptg.Z - _ref.Z ) * _scale ;
    Result.M := _ref.M ;
  end ;

  function GisRotatePoint( const _ref   : TGIS_Point ;
                            const _ptg   : TGIS_Point ;
                            const _angle : Double
                          ) : TGIS_Point ;
  var
    ssin : Double ;
    scos : Double ;
    dx   : Double ;
    dy   : Double ;
  begin
    SinCos( _angle, ssin, scos ) ;
    dx := _ptg.X - _ref.X ;
    dy := _ptg.Y - _ref.Y ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := _ref.X + ( dx * scos  -  dy * ssin ) ;
    Result.Y := _ref.Y + ( dx * ssin  +  dy * scos ) ;
  end ;

  function GisCenterPoint( const _extent : TGIS_Extent ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := _extent.XMin + (_extent.XMax-_extent.XMin) / 2 ;
    Result.Y := _extent.YMin + (_extent.YMax-_extent.YMin) / 2 ;
  end ;

  function GisRadToCompass( const _angle : Double ) : String ;
  var
    ang : Double ;
  begin
    {$IFDEF OXYGENE}
      assert( False, 'Must be tested' ) ;
    {$ELSE}
      ang := RadToDeg( _angle ) ;
    {$ENDIF}

    if ang < 0 then ang := 360 + ang ;

    if                           ( ang <  22.5 ) then Result := 'N'
    else if (  22.5 <= ang ) and ( ang <  67.5 ) then Result := 'NE'
    else if (  67.5 <= ang ) and ( ang < 112.5 ) then Result := 'E'
    else if ( 112.5 <= ang ) and ( ang < 157.5 ) then Result := 'SE'
    else if ( 157.5 <= ang ) and ( ang < 202.5 ) then Result := 'S'
    else if ( 202.5 <= ang ) and ( ang < 247.5 ) then Result := 'SW'
    else if ( 247.5 <= ang ) and ( ang < 292.5 ) then Result := 'W'
    else if ( 292.5 <= ang ) and ( ang < 337.5 ) then Result := 'NW'
    else if ( 337.5 <= ang )                     then Result := 'N' ;

  end ;

  function GisDefaultField( const _type : TGIS_FieldType ) : Variant ;
  var
    d : Double ;
  begin
    d := 0 ;
    {$IFDEF CLR}
    case _type of
      TGIS_FieldType.String  : Result := ''    ;
      TGIS_FieldType.Number  : Result := d     ; // strange, but this is the  only
      TGIS_FieldType.Float   : Result := d     ; // way to make a correct assignment
      TGIS_FieldType.Boolean : Result := False ;
      TGIS_FieldType.Date    : Result := EncodeDate( 1899, 12, 31 ) ;
      else                     Result := NullVar ;
    end ;
    {$ELSE}
    case _type of
      TGIS_FieldType.String  : Result := ''    ;
      TGIS_FieldType.Number  : Result := d     ; // strange, but this is the  only
      TGIS_FieldType.Float   : Result := d     ; // way to make a correct assignment
      TGIS_FieldType.Boolean : Result := False ;
      TGIS_FieldType.Date    : Result := EncodeDate( 1899, 12, 31 ) ;
    end ;
    {$ENDIF}
  end ;

  {$IFDEF ISLAND}
    function GisSamplesDataDir : String ;
    begin
      Result := '' ;
      {$WARNING '### Verify ISLAND code'}
    end ;
  {$ELSE}
    function GisSamplesDataDir : String ;
    var
      path : String ;
      key  : String ;
      {$IFDEF MSWINDOWS}
        ini   : TGIS_IniFile  ;
        reg   : TRegistry ;
        keyw  : String ;
        keya  : Cardinal ;
      {$ENDIF}

      lst  : TStringList ;
    const
      DIR = 'SamplesDataDir' ;
    begin
      Result := '' ;
      path   := '' ;

      try
      key := '.TatukGIS' + GisEnvironmentInfo.DirSep +
             GIS_PRODUCTID + GisEnvironmentInfo.DirSep +
             DIR ;
      {$IFDEF MSWINDOWS}
        keyw := 'SOFTWARE\TatukGIS\' + GIS_PRODUCTID ;
      {$ENDIF}

      {$IFDEF DCC}
        {$IFDEF MSWINDOWS_OS}
          path := GetEnvironmentVariable( 'USERPROFILE' );
        {$ENDIF}
        {$IFDEF MACOSX_OS}
          path := TPath.GetHomePath ;
        {$ENDIF}
        {$IFDEF LINUX_OS}
          path := TPath.GetHomePath ;
        {$ENDIF}
        {$IFDEF IOS_OS}
          path := TPath.GetDocumentsPath ;
        {$ENDIF}
        {$IFDEF ANDROID}
          path := TPath.GetDocumentsPath ;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF CLR}
        path := Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
      {$ENDIF}
      {$IFDEF JAVA} 
        if System.getProperty( 'os.name' ).toLowerCase.contains('win') then
          path := System.getenv("PUBLIC") + GisEnvironmentInfo.DirSep + 'Documents' 
        else
          path := System.getProperty('user.home') ;
      {$ENDIF}
      assert( path <> '' ) ;

      if not path.EndsWith(GisEnvironmentInfo.DirSep) then
        path := path + GisEnvironmentInfo.DirSep;
      path := path + key;
      try
        lst := TStringList.Create ;
        try
          if FileExists( path ) then begin
            lst.LoadFromFile( path ) ;
            if lst.Text <> '' then
              Result := Trim( lst[0] ) ;
          end ;
        except
          // do nothing
        end;
      finally
        FreeObject( lst ) ;
      end;
      if Result <> '' then exit;

      {$IFDEF MSWINDOWS}
        {$IFDEF DCC}
          keya := KEY_READ or KEY_WOW64_32KEY ;
        {$ELSE}
          keya := 131097 or 512 ;
        {$ENDIF}
        reg := TRegistry.Create( keya ) ;
        try
          if reg.OpenKey( keyw, False ) then begin
            Result := Trim( reg.ReadString( DIR ) ) ;
            reg.CloseKey ;
          end
          else
            Result := '' ;
        finally
          FreeObject( reg ) ;
        end;
      {$ENDIF}
      if Result <> '' then exit ;

      {$IFDEF MSWINDOWS}
        { TODO : DK-13249 whiole ini reading to be removed later}

        {$IFDEF CLR}
          ini := TGIS_IniFile.Create( Environment.GetEnvironmentVariable("SystemRoot" )
                  + '\' + GIS_CONFIG_FILE ) ;
        {$ELSE}
          ini := TGIS_IniFile.Create( GetEnvironmentVariable( 'SystemRoot' )
                 + '\' + GIS_CONFIG_FILE ) ;
        {$ENDIF}
        try
          Result := ini.ReadString( GIS_CONFIG_FILE_SAMPLES,
                                    GIS_CONFIG_FILE_DATADIR,
                                    ''
                                  ) ;
          if not ( Result = '' ) then
            if not Result.EndsWith(GisEnvironmentInfo.DirSep)  then
              Result := Result + GisEnvironmentInfo.DirSep;
        finally
          FreeObject( ini ) ;
        end ;
      {$ENDIF}

      {$IFDEF DOTNET_STANDARD}
        if Result = '' then
          Result := '/app/Samples11/';
      {$ENDIF}

      finally
        if not ( Result = '' ) then
          if not Result.EndsWith(GisEnvironmentInfo.DirSep)  then
            Result := Result + GisEnvironmentInfo.DirSep;
        if not DirectoryExists( Result ) then
          Result := '' ;
      end ;

    end ;
  {$ENDIF}

{$IFNDEF DCC}
  {$IFDEF JAVA}
    function  GisSamplesDataDirDownload
      : String ;
    var
      pDestination : Path ;
      zip : ZipInputStream ;
      entry : ZipEntry ;
      pDecompression : Path ;
      uZip : URL ;
      path : String ;
      key : String ;
      lst : TStringList ;      
      // IMPORTANT !
      // Has to be defined as var due to checking OS
      // NOT TO BE CHANGED AFTER ASSIGN !!!!
      DOWNLOADDIR : String ;                   // Path to directory to which we download the .zip to
      ZIPDIR : String ;                        // Path to the downloaded .zip file
    const
      DIR = 'SamplesDataDir' ;
      DATASET = 'Samples11' ;                  // Name of the .zip file and directory in which samples are going to be
      DOWNLOADURL = "https://download.tatukgis.com/pub/SampleData/" + DATASET + ".zip" ;

    method extractFile( 
      zip     : ZipInputStream; 
      outPath : Path
    );
    var
      buffer: array of SByte;
      bytesRead: Integer;
    begin
      //  Ensure parent directories exist before creating the file
      //  Create all necessary parent directories before writing the file.
      Files.createDirectories( outPath.Parent );
      using bos := BufferedOutputStream.Create( Files.newOutputStream( outPath ) ) do begin
        SetLengthArray( buffer, 4096 ) ;
        loop begin
          bytesRead := zip.read( buffer );
          if not bytesRead > -1 then begin
            break;
          end;
          bos.write( buffer, 0, bytesRead );
        end;
      end;
    end;

    method downloadZip( 
      url : URL 
    );
    var
      cZip : ReadableByteChannel ;
      cOutput : FileChannel ;
      sOutput : FileOutputStream ;
    begin
      // Open channel from the zip url
      cZip := Channels.newChannel( url.openStream );

      // Create stream for the zip file
      sOutput := FileOutputStream.create( ZIPDIR );
      // Open channel from the zip stream
      cOutput := sOutput.Channel ;
 
      // Copy data from the zip url channel to the zip file
      cOutput.transferFrom( cZip, 0, Long.MAX_VALUE );

      // Close both streams
      cZip.close;
      cOutput.close;
    end;

    begin
      Result := GisSamplesDataDir ;  

      if Result <> '' then
        exit ;

      if System.getProperty( 'os.name' ).toLowerCase.contains('win') then
        DOWNLOADDIR := 
          System.getenv("PUBLIC") +
          GisEnvironmentInfo.DirSep +
          'Documents' +
          GisEnvironmentInfo.DirSep +
          'TatukGIS' +
          GisEnvironmentInfo.DirSep +
          'Data' +
          GisEnvironmentInfo.DirSep 
      else
        DOWNLOADDIR := 
          System.getProperty('user.home') +
          GisEnvironmentInfo.DirSep +
          'TatukGIS' +
          GisEnvironmentInfo.DirSep +
          'Data' +
          GisEnvironmentInfo.DirSep ;

      ZIPDIR := DOWNLOADDIR + DATASET + '.zip'; // Path to the downloaded .zip file

      uZip := URL.create( DOWNLOADURL ) ;

      pDestination := Paths.get( DOWNLOADDIR + DATASET );

      // Create if directory doesnt exist
      if not Files.exists( pDestination ) then begin
        Files.createDirectories( pDestination );
      end;
      
      // Download .zip file from given URL
      downloadZip( uZip ) ;

      // Decompress .zip
      using zip := ZipInputStream.Create( Files.newInputStream( Paths.get( ZIPDIR ) ) ) do begin
        entry := zip.NextEntry;
        while Assigned( entry ) do begin
          pDecompression := pDestination.resolve( entry.Name ).normalize;
          //  Prevent Zip Slip vulnerability
          if not pDecompression.startsWith( pDestination ) then begin
            raise IOException.Create( "Entry is outside of target dir: " + entry.Name );
          end;
          if entry.isDirectory() then begin
            Files.createDirectories( pDecompression );
          end
          else begin
            extractFile( zip, pDecompression );
          end;
          zip.closeEntry;
          entry := zip.NextEntry;
        end;
      end;

      // Close zip stream if persists
      if Assigned( zip ) then
        zip.close;

      // Create file variable in order to deleted .zip file
      java.io.File.Create( ZIPDIR ).delete;

      Result := pDestination.toFile.AbsolutePath;

      //Save information to the file
      path := System.getProperty('user.home') ;
      if not path.EndsWith(GisEnvironmentInfo.DirSep) then
        path := path + GisEnvironmentInfo.DirSep;

      key := '.TatukGIS' + GisEnvironmentInfo.DirSep +
        GIS_PRODUCTID + GisEnvironmentInfo.DirSep ;

      path := path + key + DIR;

      try
        lst := TStringList.Create ;
        try
          lst.Text := Result ;
          lst.SaveToFile( path ) ;
        except
          // do nothing
        end;
      finally
        FreeObject( lst ) ;
      end;
    end;
  {$ELSE}
    function  GisSamplesDataDirDownload
      : String ;
    var
      webClient : WebClient ;
      path : String ;
      key  : String ;
      lst : TStringList ;
      {$IFDEF MSWINDOWS}
        ini   : TGIS_IniFile  ;
        reg   : TRegistry ;
        keyw  : String ;
        keya  : Cardinal ;
      {$ENDIF}
    const
      DIR = 'SamplesDataDir' ;
      DATASET= 'Samples11' ;
      DOWNLOADURL = "https://download.tatukgis.com/pub/SampleData/" + DATASET + ".zip" ;
    begin
      Result := GisSamplesDataDir ;      

      if Result <> '' then
        exit ;

      path := Environment.GetFolderPath(Environment.SpecialFolder.CommonDocuments) +
              GisEnvironmentInfo.DirSep +
              'TatukGIS' +
              GisEnvironmentInfo.DirSep +
              'Data' +
              GisEnvironmentInfo.DirSep +
              DATASET ;

      // Download .zip file
      webClient := WebClient.Create ;
      webClient.DownloadFile( Uri.create( DOWNLOADURL ), path + ".zip" ) ;
      
      // Create if directory doesnt exist
      if not Directory.Exists( path ) then
        Directory.CreateDirectory( path );

      // Extract .zip file
      ZipFile.ExtractToDirectory( path + ".zip", path ) ;

      // Remove extracted .zip
      File.Delete( path + ".zip" ) ;

      Result := path ;

      {$IFDEF MSWINDOWS}
        // Save information to the register
        keyw := 'SOFTWARE\TatukGIS\' + GIS_PRODUCTID ;

        reg := TRegistry.Create;
        try
          reg.OpenKey( keyw, True ) ;
          reg.WriteString( DIR, Result ) ;
        finally
          reg.CloseKey ;
          FreeObject( reg ) ;
        end;
      {$ELSE}
        //Save information to the file
        path := Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
      
        key := '.TatukGIS' + GisEnvironmentInfo.DirSep +
               GIS_PRODUCTID + GisEnvironmentInfo.DirSep ;

        if not path.EndsWith(GisEnvironmentInfo.DirSep)  then
          path := path + GisEnvironmentInfo.DirSep;

        path := path + key ;
        Directory.CreateDirectory( path ) ;

        if not path.EndsWith(GisEnvironmentInfo.DirSep)  then
          path := path + GisEnvironmentInfo.DirSep;
        path := path + DIR ;
            
        try
          lst := TStringList.Create ;
          try
            lst.Text := Result ;
            lst.SaveToFile( path ) ;
          except
            // do nothing
          end;
        finally
          FreeObject( lst ) ;
        end;
      {$ENDIF}
    end;
  {$ENDIF}
{$ELSE}
  function  GisSamplesDataDirDownload
    : String ;
  var
    r    : TGIS_HttpResponse ;
    strm : TMemoryStream ;
    zip  : TZipFile ;
    path : String ;
    key  : String ;
    ini  : TGIS_IniFile ;
    lst  : TStringList ;
    {$IFDEF MSWINDOWS_OS}
      reg  : TRegistry ;
      keyw : String ;
    {$ENDIF}
  const
    DIR = 'SamplesDataDir' ;
    DATASET= 'Samples11' ;
  begin
    Result := GisSamplesDataDir ;

    if Result <> '' then
      exit ;

    {$IFDEF DCC}
      {$IFDEF MSWINDOWS_OS}
        path := TPath.GetSharedDocumentsPath + '\TatukGIS\Data\' + DATASET ;
      {$ENDIF}
      {$IFDEF MACOSX_OS}
        path := TPath.GetDocumentsPath + '/TatukGIS/Data/' + DATASET ;
      {$ENDIF}
      {$IFDEF LINUX_OS}
        path := TPath.GetDocumentsPath + '/TatukGIS/Data/' + DATASET ;
      {$ENDIF}
      {$IFDEF IOS_OS}
        path := TPath.GetDocumentsPath + '/SampleData' ;
      {$ENDIF}
      {$IFDEF ANDROID}
        path := TPath.GetDocumentsPath + '/SampleData' ;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF CLR}
      path := Environment.GetFolderPath(Environment.SpecialFolder.CommonDocuments) +
              GisEnvironmentInfo.DirSep +
              'TatukGIS' +
              GisEnvironmentInfo.DirSep +
              'Data' +
              GisEnvironmentInfo.DirSep +
              DATASET ;
    {$ENDIF}
    {$IFDEF JAVA}
      path := System.getProperty('user.home')
              GisEnvironmentInfo.DirSep +
              'TatukGIS' +
              GisEnvironmentInfo.DirSep +
              'Data' +
              GisEnvironmentInfo.DirSep +
              DATASET ;
    {$ENDIF}
    assert( path <> '' ) ;

    strm := TMemoryStream.Create ;
    try
      r := TGIS_WebUtils.HttpFetch(
             Format( 'https://download.tatukgis.com/pub/SampleData/%s.zip', [DATASET] ),
             strm,
             nil, False, 0, '', '', '', '', False
           ) ;

      if  r.Status <> GIS_HTTP_OK then begin
        raise Exception.Create('Sample Data cannot be downloaded: ' + IntToStr( r.Status ));
      end ;

      strm.Position := 0 ;

      TDirectory.CreateDirectory( path ) ;
      zip := TZipFile.Create ;
      try
        zip.Open( strm, TZipMode.zmRead );
        zip.ExtractAll( path + PathDelim ) ;
      finally
        zip.Free ;
      end;
    finally
      strm.Free ;
    end;
    Result := path ;

    if not Result.EndsWith(GisEnvironmentInfo.DirSep)  then
      Result := Result + GisEnvironmentInfo.DirSep;

    key := '.TatukGIS' + GisEnvironmentInfo.DirSep +
           GIS_PRODUCTID + GisEnvironmentInfo.DirSep ;
    {$IFDEF MSWINDOWS}
      keyw := 'SOFTWARE\TatukGIS\' + GIS_PRODUCTID ;
    {$ENDIF}

    {$IFDEF MSWINDOWS}
      reg := TRegistry.Create( KEY_WRITE or KEY_WOW64_32KEY ) ;
      try
        reg.OpenKey( keyw, True ) ;
        reg.WriteString( DIR, Result) ;
      finally
       reg.Free ;
      end;
    {$ELSE}
      {$IFDEF DCC}
        {$IFDEF MSWINDOWS_OS}
          path := GetEnvironmentVariable( USERPROFILE );
        {$ENDIF}
        {$IFDEF MACOSX_OS}
          path := TPath.GetHomePath  ;
        {$ENDIF}
        {$IFDEF LINUX_OS}
          path := TPath.GetHomePath ;
        {$ENDIF}
        {$IFDEF IOS_OS}
          path := TPath.GetDocumentsPath ;
        {$ENDIF}
        {$IFDEF ANDROID}
          path := TPath.GetDocumentsPath ;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF CLR}
        path := Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
      {$ENDIF}
      {$IFDEF JAVA}
        path := System.getProperty('user.home') ;
      {$ENDIF}
      assert( path <> '' ) ;

      path := TPath.Combine( path, key ) ;
      TDirectory.CreateDirectory( path ) ;
      path := TPath.Combine( path, DIR ) ;
      try
        lst := TStringList.Create ;
        try
          lst.Text := Result ;
          lst.SaveToFile( path ) ;
        except
          // do nothing
        end;
      finally
        FreeObject( lst ) ;
      end;

    {$ENDIF}

  end;
{$ENDIF}


  function GisCanonicalSQLName( const _name : String
                              ) : String ;
  begin
    Result := GisCanonicalSQLName( _name, 'NAME' ) ;
  end;

  function GisCanonicalSQLName( const _name     : String ;
                                const _template : String
                              ) : String ;
  var
    i     : Integer        ;
    j     : Integer        ;
    c     : Char           ;
    sname : String         ;
    tmp   : TStringBuilder ;
  begin
    tmp := TStringBuilder.Create ;
    try
      sname := UpperCase( _name ) ;

      j := 0 ;
      for i := StringFirst to StringLast( sname )  do begin
        c := sname[i] ;
        if i = StringFirst then begin
          if ord( c ) > 127 then begin
            // first character should be an ANSI letter
            tmp.Append( '_' ) ;
            inc( j ) ;
          end
          else if not InCharSet( c, ['AZ'] ) then begin
            // first character should be a letter
            tmp.Append( '_' ) ;
            inc( j ) ;
          end
          else
            tmp.Append( c ) ;
        end
        else begin
          if ord( c ) > 127 then  begin
            // character should be a ANSI letter
            tmp.Append( '_' ) ;
            inc( j ) ;
          end
          else if not InCharSet( c, ['AZ', '09', '_'] ) then begin
            tmp.Append( '_' ) ;
            inc( j ) ;
          end
          else
            tmp.Append( c ) ;
        end ;
      end ;

      if j > length( sname ) / 4 then begin
        // name is to strange - must be replace with predefined
        Result := Format( '%s_%x',
                          [ _template, GetTickCount() ]
                        ) ;
      end
      else
        Result := tmp.ToString ;
    finally
      FreeObject( tmp ) ;
    end;
  end ;

  function GisNormalizedSQLName( const _name : String ) : String ;
  var
    c       : Char    ;
    tmpname : String  ;
    tmp     : String  ;
    bl      : Boolean ;

    function do_parse : Boolean ;
    var
      i : Integer ;
    begin
      if c = '"' then begin  // "xxx" syntax
        tmp := '' ;
        for i:= StringFirst+1 to StringLast( tmpname ) do begin
          c := tmpname[i] ;
          tmp := tmp + c ;
          if c = '"' then break ;
        end ;

        Result := tmp = tmpname ;
        exit ;
      end
      else if c = '[' then begin  // [xxx] syntax
        tmp := c ;
        for i:= StringFirst+1 to StringLast( tmpname ) do begin
          c := tmpname[i] ;
          tmp := tmp + c ;
          if c = ']' then break ;
        end ;
        Result := tmp = tmpname ;
        exit ;
      end
      else begin                   // xxx syntax /without "" or []/
        for i:= StringFirst to StringLast( tmpname ) do begin
          c := tmpname[i] ;
          if ( ord( c ) > 127 ) or
             ( not InCharSet(
                     c,
                     [ '.','AZ','az','09','_','@','$','#']
                   )
             )
          then begin
            Result := False ;
            exit ;
          end ;
        end ;
        Result := True ;
        exit ;
      end ;
    end ;
  begin
    tmpname := Trim( _name ) ;

    if IsStringEmpty( tmpname ) then begin
      Result := '' ;
      exit ;
    end ;

    c := tmpname[StringFirst] ;

    bl := False ;

    if ( ord( c ) < 128 ) and
       InCharSet( c, ['AZ', 'az', '"', '[' ] )
    then
      bl := do_parse ;

    if bl then Result := tmpname
          else Result := '[' + tmpname + ']' ;
  end ;

  function GisDeNormalizedSQLName( const _name : String ) : String ;
  begin
    Result := _name ;
    if not IsStringEmpty( _name ) then begin
      if ( _name[ StringFirst         ] = '[' ) and
         ( _name[ StringLast( _name ) ] = ']' )
      then begin
        Result := Copy( _name, StringFirst + 1, StringLast( _name ) - StringFirst -1 ) ;
      end ;
    end ;
  end;

  procedure GisDecodeAngle( const _val  : Double     ;
                            var   _deg  : Integer    ;
                            var   _min  : Integer    ;
                            var   _sec  : Integer    ;
                            var   _frac : Integer    ;
                            var   _sign : TValueSign
                          )  ;
  begin
    GisDecodeAngle( _val, _deg, _min, _sec, _frac, _sign, 2 ) ;
  end ;

  procedure GisDecodeAngle( const _val  : Double     ;
                            var   _deg  : Integer    ;
                            var   _min  : Integer    ;
                            var   _sec  : Integer    ;
                            var   _frac : Integer    ;
                            var   _sign : TValueSign ;
                            const _prec : Integer
                          )  ;
  var
    oval  : Double  ;
    prec  : Integer ;
  begin
    oval := _val ;

    if ( oval < -2*Pi ) or ( oval > 2*Pi ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( oval ) ), 0 ) ;

    _sign := Sign( oval ) ;
    if _sign = 0 then _sign := 1 ;

    prec := _prec ;
    if prec > 15     then prec := 15
    else if prec < 0 then prec := 0 ;

    oval := Abs( RadToDeg( oval ) ) ;

    if Abs( RoundS( oval ) - oval ) < 1e-9 then
      oval := RoundS( oval ) ;

    _deg := FloorS( oval ) ;

    oval := Frac( oval ) * 60 ;

    _min := FloorS( oval ) ;

    oval := Frac( oval ) * 60 ;
    _sec := FloorS( oval ) ;

    oval := Frac( oval ) * IntPower( 10, prec ) ;
    _frac := RoundS( oval ) ;
    if _frac >= 1 * Power( 10, prec ) then begin
      inc( _sec ) ;
      _frac := 0 ;
    end ;
    if _sec >= 60 then begin
      inc( _min ) ;
      _sec := 0 ;
    end ;
    if _min >= 60 then begin
      inc( _deg ) ;
      _min := 0 ;
    end ;
  end ;

  procedure GisDecodeLatitude( const _val  : Double     ;
                               var   _deg  : Integer    ;
                               var   _min  : Integer    ;
                               var   _sec  : Integer    ;
                               var   _frac : Integer    ;
                               var   _sign : TValueSign
                             )  ;
  begin
    GisDecodeLatitude( _val, _deg, _min, _sec, _frac, _sign, 2 ) ;
  end ;

  procedure GisDecodeLatitude( const _val  : Double     ;
                               var   _deg  : Integer    ;
                               var   _min  : Integer    ;
                               var   _sec  : Integer    ;
                               var   _frac : Integer    ;
                               var   _sign : TValueSign ;
                               const _prec : Integer
                             )  ;
  begin
    if ( _val < -Pi/2 ) or ( _val > Pi/2 ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( _val ) ), 0 ) ;

    GisDecodeAngle( _val, _deg, _min, _sec, _frac, _sign, _prec ) ;
  end ;

  procedure GisDecodeLongitude( const _val  : Double     ;
                                var   _deg  : Integer    ;
                                var   _min  : Integer    ;
                                var   _sec  : Integer    ;
                                var   _frac : Integer    ;
                                var   _sign : TValueSign
                              )  ;
  begin
    GisDecodeLongitude( _val, _deg, _min, _sec, _frac, _sign, 2 ) ;
  end ;

  procedure GisDecodeLongitude( const _val  : Double     ;
                                var   _deg  : Integer    ;
                                var   _min  : Integer    ;
                                var   _sec  : Integer    ;
                                var   _frac : Integer    ;
                                var   _sign : TValueSign ;
                                const _prec : Integer
                              )  ;
  var
    oval : Double  ;
  begin
    oval := _val ;

    if oval > 2*Pi then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( oval ) ), 0 ) ;

    if oval > Pi then oval := oval - 2 * Pi ;

    if ( oval < -Pi ) or ( oval > Pi ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( oval ) ), 0 ) ;

    GisDecodeAngle( oval, _deg, _min, _sec, _frac, _sign, _prec ) ;
  end ;

  function GisEncodeAngle( const _deg  : Double
                         ) : Double ;
  begin
    Result := GisEncodeAngle( _deg, 0, 0, 1 ) ;
  end ;

  function GisEncodeAngle( const _deg  : Double ;
                           const _min  : Double
                         ) : Double ;
  begin
    Result := GisEncodeAngle( _deg, _min, 0, 1 ) ;
  end ;

  function GisEncodeAngle( const _deg  : Double ;
                           const _min  : Double ;
                           const _sec  : Double
                         ) : Double ;
  begin
    Result := GisEncodeAngle( _deg, _min, _sec, 1 ) ;
  end ;

  function GisEncodeAngle( const _deg  : Double     ;
                           const _min  : Double     ;
                           const _sec  : Double     ;
                           const _sign : TValueSign
                         ) : Double ;
  var
    ssign : TValueSign ;
  begin
    if _deg < 0 then ssign := -1
                else ssign := _sign ;

    if ssign = 0 then ssign := 1 ;

    Result :=  ssign * DegToRad( Abs( _deg ) +
                                Abs( _min / 60 ) +
                                Abs( _sec / 60 / 60 )
                              ) ;

    if Result > 2*Pi then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( Result ) ), 0 ) ;

    if ( Result < -2*Pi ) or ( Result > 2*Pi ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( Result ) ), 0 ) ;
  end ;

  function GisEncodeLatitude( const _deg  : Double
                            ) : Double ;
  begin
    Result := GisEncodeLatitude( _deg, 0, 0, 1 ) ;
  end ;

  function GisEncodeLatitude( const _deg  : Double ;
                              const _min  : Double
                            ) : Double ;
  begin
    Result := GisEncodeLatitude( _deg, _min, 0, 1 ) ;
  end ;

  function GisEncodeLatitude( const _deg  : Double ;
                              const _min  : Double ;
                              const _sec  : Double
                            ) : Double ;
  begin
    Result := GisEncodeLatitude( _deg, _min, _sec, 1 ) ;
  end ;

  function GisEncodeLatitude( const _deg  : Double     ;
                              const _min  : Double     ;
                              const _sec  : Double     ;
                              const _sign : TValueSign
                            ) : Double ;
  begin
    Result := GisEncodeAngle( _deg, _min, _sec, _sign ) ;

    if ( Result < -Pi/2 ) or ( Result > Pi/2 ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( Result ) ), 0 ) ;
  end ;

  function GisEncodeLongitude( const _deg : Double
                             ) : Double ;
  begin
    Result := GisEncodeLongitude( _deg, 0, 0, 1 ) ;
  end ;

  function GisEncodeLongitude( const _deg : Double ;
                               const _min : Double
                             ) : Double ;
  begin
    Result := GisEncodeLongitude( _deg, _min, 0, 1 ) ;
  end ;

  function GisEncodeLongitude( const _deg : Double ;
                               const _min : Double ;
                               const _sec : Double
                             ) : Double ;
  begin
    Result := GisEncodeLongitude( _deg, _min, _sec, 1 ) ;
  end ;

  function GisEncodeLongitude( const _deg  : Double     ;
                               const _min  : Double     ;
                               const _sec  : Double     ;
                               const _sign : TValueSign
                             ) : Double ;
  begin
    Result := GisEncodeAngle( _deg, _min, _sec, _sign ) ;
    if Result > Pi then Result := Result - 2 * Pi ;

    if ( Result < -Pi ) or ( Result > Pi ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ),
                                   FloatToStr( DegToRad( Result ) ), 0 ) ;
  end ;

  // Convert a coordinate in radians to a String representation.
  // For internal use of GisLongitudeToStr and GisLatitudeToStr.
  // _val        coordinate in radians
  // _longitude  if 0, then Angle format;
  //             if 1, then Longitude format;
  //             if 2, then Latitude format
  // _spaces     if True, then text will be space separated
  // _prec       number of precise digits
  // _leading    if True ,then leading zeros will be presented
  // return      coordinate converted to a String representation
  //             deg min sec
  function coordinate_to_str( const _val     : Double   ;
                              const _format  : Integer  ;
                              const _spaces  : Boolean  ;
                              const _prec    : Integer  ;
                              const _leading : Boolean
                            ) : String ;
  var
    deg  : Integer    ;
    min  : Integer    ;
    sec  : Integer    ;
    frac : Integer    ;
    ipos : String     ;
    sgn  : TValueSign ;
    txt  : String     ;
    fmt  : String     ;
  begin

    case _format of
      0 :  begin
             GisDecodeAngle( _val, deg, min, sec, frac, sgn, _prec ) ;
             if sgn < 0 then deg := -deg ;
             ipos := '' ;

             if _prec > 0 then begin
               if _leading then
                 fmt := '%.3d'+DEGREE_SIGN+' %.2d'' %.2d.%.*d"'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d.%.*d"' ;

               txt := Format( fmt,
                              [ deg, min, sec, _prec, frac ]
                            ) ;
             end
             else begin
               if _leading then
                 fmt := '%.3d'+DEGREE_SIGN+' %.2d'' %.2d"'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d"' ;

               txt := Format( fmt,
                              [ deg, min, sec ]
                            ) ;
             end ;

           end ;
      1 :  begin
             GisDecodeLongitude( _val, deg, min, sec, frac, sgn, _prec ) ;
             if sgn < 0 then ipos := 'W'
                        else ipos := 'E' ;

             if _prec > 0 then begin
               if _leading then
                 fmt := '%.3d'+DEGREE_SIGN+' %.2d'' %.2d.%.*d" %s'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d.%.*d" %s'  ;

               txt := Format( fmt,
                              [ deg, min, sec, _prec, frac, ipos ]
                            ) ;
             end
             else begin
               if _leading then
                 fmt := '%.3d'+DEGREE_SIGN+' %.2d'' %.2d" %s'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d" %s'  ;

               txt := Format( fmt,
                              [ deg, min, sec, ipos ]
                            ) ;
             end ;
           end ;
      2 :  begin
             GisDecodeLatitude ( _val, deg, min, sec, frac, sgn, _prec ) ;
             if sgn < 0 then ipos := 'S'
                        else ipos := 'N' ;

             if _prec > 0 then begin
               if _leading then
                 fmt := '%.2d'+DEGREE_SIGN+' %.2d'' %.2d.%.*d" %s'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d.%.*d" %s' ;

               txt := Format( fmt,
                              [ deg, min, sec, _prec, frac, ipos ]
                            ) ;
             end
             else begin
               if _leading then
                 fmt := '%.2d'+DEGREE_SIGN+' %.2d'' %.2d" %s'
               else
                 fmt := '%.1d'+DEGREE_SIGN+' %.1d'' %.1d" %s' ;

               txt := Format( fmt,
                              [ deg, min, sec, ipos ]
                            ) ;
             end ;
            end ;
      else begin
              assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
           end ;
    end ;

    if _spaces then
      Result := txt
    else
      {$IFDEF JAVA}
        Result := StringReplaceAll( txt, ' ', '' ) ;
      {$ELSE}
        Result := StringReplace( txt, ' ', '', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
      {$ENDIF}
  end ;

  function GisAngleToStr( const _val : Double
                        ) : String ;
  begin
    Result := GisAngleToStr( _val, True, 2, False ) ;
  end ;

  function GisAngleToStr( const _val    : Double ;
                          const _spaces : Boolean
                        ) : String ;
  begin
    Result := GisAngleToStr( _val, _spaces, 2, False ) ;
  end ;

  function GisAngleToStr( const _val     : Double  ;
                          const _spaces  : Boolean ;
                          const _prec    : Integer
                        ) : String ;
  begin
    Result := GisAngleToStr( _val, _spaces, _prec, False ) ;
  end ;

  function GisAngleToStr( const _val     : Double  ;
                          const _spaces  : Boolean ;
                          const _prec    : Integer ;
                          const _leading : Boolean
                        ) : String ;
  begin
    Result := coordinate_to_str( _val, 0, _spaces, _prec, _leading ) ;
  end ;

  function GisLatitudeToStr( const _val : Double
                           ) : String ;
  begin
    Result := GisLatitudeToStr( _val, True, 2, False ) ;
  end ;

  function GisLatitudeToStr( const _val    : Double ;
                             const _spaces : Boolean
                           ) : String ;
  begin
    Result := GisLatitudeToStr( _val, _spaces, 2, False ) ;
  end ;

  function GisLatitudeToStr( const _val    : Double  ;
                             const _spaces : Boolean ;
                             const _prec   : Integer
                           ) : String ;
  begin
    Result := GisLatitudeToStr( _val, _spaces, _prec, False ) ;
  end ;

  function GisLatitudeToStr( const _val     : Double  ;
                             const _spaces  : Boolean ;
                             const _prec    : Integer ;
                             const _leading : Boolean
                           ) : String ;
  begin
    Result := coordinate_to_str( _val, 2, _spaces, _prec, _leading ) ;
  end ;

  function GisLongitudeToStr( const _val : Double
                            ) : String ;
  begin
    Result := GisLongitudeToStr( _val, True, 2, False ) ;
  end ;

  function GisLongitudeToStr( const _val    : Double ;
                              const _spaces : Boolean
                            ) : String ;
  begin
    Result := GisLongitudeToStr( _val, _spaces, 2, False ) ;
  end ;

  function GisLongitudeToStr( const _val    : Double  ;
                              const _spaces : Boolean ;
                              const _prec   : Integer
                            ) : String ;
  begin
    Result := GisLongitudeToStr( _val, _spaces, _prec, False ) ;
  end ;

  function GisLongitudeToStr( const _val     : Double  ;
                              const _spaces  : Boolean ;
                              const _prec    : Integer ;
                              const _leading : Boolean
                            ) : String ;
  begin
    Result := coordinate_to_str( _val, 1, _spaces, _prec, _leading ) ;
  end ;

  // Decode coordinate in strings into the parts.
  // For internal use of GisStrToLongitude and GisStrToLatitude.
  // _val        coordinate in radians
  // _longitude  if True, then Longitude format;
  //             if False, then Latitude format
  // _spaces     if True, then text will be space separated
  // return      coordinate converted to a String representation
  //             deg min sec
  procedure str_to_coordinate( const _txt       : String ;
                               var   _deg       : Double ;
                               var   _min       : Double ;
                               var   _sec       : Double ;
                               var   _hem       : Char
                             ) ;
  var
    i   : Integer    ;
    txt : String     ;
    deg : String     ;
    min : String     ;
    sec : String     ;
    tkn : TGIS_Tokenizer ;
    c   : Char       ;
    chs : TCharSet   ;

    procedure check_number( const _s : String ) ;
    var
      i : Integer ;
    begin
      for i := StringFirst to StringLast( _s ) do begin
        case _s[i] of
         '0','1','2','3','4','5','6','7','8','9' :
           continue ;
         '.',',' :
           continue ;
         '+','-' :
           continue ;
         else
           Abort ;
        end ;
      end ;
    end ;

  begin
    _deg := 0 ;
    _min := 0 ;
    _sec := 0 ;
    _hem := ' ' ;

    txt := '' ;

    for i := StringFirst to StringLast( _txt ) do
      if _txt[i] = ',' then txt := txt + '.'
                       else txt := txt + UpCase( _txt[i] ) ;

    txt := Trim( txt ) ;

    if length( txt ) > 1 then begin
      chs := PrepareCharSet( ['N','S','W','E'] ) ;
      c := txt[ StringLast( txt ) ] ;
      if ( ord(c) < 128 ) and
         ( InCharSet( c, chs ) ) then
      begin
        _hem := c ;
        SetLengthStr( txt, length( txt ) - 1 ) ;
      end ;
      c := txt[ StringFirst ] ;
      if ( ord(c) < 128 ) and
         ( InCharSet( c, chs ) ) then
      begin
        _hem := c ;
        txt := Copy( txt, StringFirst + 1, length( txt ) - 1 ) ;
      end ;
    end ;

    try
      check_number( txt ) ;
      _deg := DotStrToFloat( txt ) ;

      if _deg < 0 then begin
        _hem := '-' ;
        _deg := Abs( _deg ) ;
      end ;

    except
      tkn := TGIS_Tokenizer.Create ;
      try

        tkn.Execute( txt, [ DEGREE_SIGN, '''', '"', ':', '/', ' ' ] ) ;

        if tkn.Result.Count <= 0 then Abort ;
        if tkn.Result.Count >  3 then Abort ;
        if tkn.Result.Count >  2 then sec := tkn.Result[2] ;
        if tkn.Result.Count >  1 then min := tkn.Result[1] ;
        if tkn.Result.Count >  0 then deg := tkn.Result[0] ;

        if not IsStringEmpty( deg ) then
          _deg := DotStrToFloat( deg )
        else
          _deg := 0 ;

        if not IsStringEmpty( min ) then begin
          if Pos( String('.'), deg ) >= StringFirst then Abort ; // decimal point already exist?
          _min := DotStrToFloat( min ) ;
          if ( _min < 0 ) or ( _min >= 60 ) then Abort ;

          if not IsStringEmpty( sec ) then begin
            if Pos( String('.'), min ) >= StringFirst then Abort ; // already decimal points
            _sec := DotStrToFloat( sec ) ;
            if ( _sec < 0 ) or ( _sec >= 60 ) then Abort ;
          end
        end ;

        if Sign( _deg ) < 0 then begin
          if  _hem <> ' ' then Abort ;
          _hem := '-' ;
        end ;

      finally
        FreeObject( tkn ) ;
      end ;
    end ;
  end ;

  function GisStrToAngle(
    const _txt : String
  ) : Double ;
  var
    deg  : Double ;
    min  : Double ;
    sec  : Double ;
    hem  : Char   ;
    sgn  : TValueSign ;
  begin
    try
      str_to_coordinate( _txt, deg, min, sec, hem ) ;

      sgn := 1 ;
      case hem of
        ' ',
        '+' : sgn :=  1 ;
        '-' : sgn := -1 ;
        else  Abort ;
      end ;
      Result := GisEncodeAngle( deg, min, sec, sgn ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ), _txt, 0 ) ;
    end ;
  end ;

  function GisStrToLatitude(
    const _txt : String
  ) : Double ;
  var
    deg  : Double ;
    min  : Double ;
    sec  : Double ;
    hem  : Char   ;
    sgn  : TValueSign ;
  begin
    try
      str_to_coordinate( _txt, deg, min, sec, hem ) ;

      sgn := 1 ;
      case hem of
        ' ',
        '+',
        'N' : sgn :=  1 ;
        '-',
        'S' : sgn := -1 ;
        else  Abort ;
      end ;
      Result := GisEncodeLatitude( deg, min, sec, sgn ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ), _txt, 0 ) ;
    end ;
  end ;

  function GisStrToLongitude(
    const _txt : String
  ) : Double ;
  var
    deg  : Double ;
    min  : Double ;
    sec  : Double ;
    hem  : Char   ;
    sgn  : TValueSign ;
  begin
    try
      str_to_coordinate( _txt, deg, min, sec, hem ) ;

      sgn := 1 ;
      case hem of
        ' ',
        '+',
        'E' : sgn := 1  ;
        '-',
        'W' : sgn := -1 ;
        else  Abort ;
      end ;
      Result := GisEncodeLongitude( deg, min, sec, sgn ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_COORDINATE_INVALID ), _txt, 0 ) ;
    end ;
  end ;

  function GisEnvironmentInfo : TGIS_EnvironmentInfo ;
  begin
    if not assigned( FGisEnvironmentInfo ) then
      FGisEnvironmentInfo := TGIS_EnvironmentInfo.Create ;
    Result := FGisEnvironmentInfo ;
  end ;

  function GisKeyList : TGIS_PasswordList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( FGisKeyList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( FGisKeyList ) then
            FGisKeyList
              := TGIS_PasswordList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := FGisKeyList ;
  end ;

  function GisPasswordList : TGIS_PasswordList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( FGisPasswordList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( FGisPasswordList ) then
            FGisPasswordList
              := TGIS_PasswordList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := FGisPasswordList ;
  end ;

  function GisAliasList : TGIS_AliasList ;
  begin
    Result := ThreadStorage.GisAliasList ;
  end ;

  function GisMetadata : TGIS_StringList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( FGisMetadata ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( FGisMetadata ) then
            FGisMetadata
              := TGIS_StringList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := FGisMetadata ;
  end ;

  function GisMetadataAssigned : Boolean ;
  begin
    Result := assigned( FGisMetadata ) ;
  end ;

  function GisMetadataAsBoolean(
    const _name    : String ;
    const _default : Boolean
  ) : Boolean ;
  var
    tmp : String ;
  begin
    Result := _default ;

    tmp := GisMetadataAsString( _name, '' ) ;

    Result := ParamBoolean( tmp, _default ) ;
  end;

  function GisMetadataAsInteger(
    const _name    : String ;
    const _default : Integer
  ) : Integer ;
  var
    tmp : String ;
  begin
    Result := _default ;

    tmp := GisMetadata.Values[_name] ;

    if tmp <> '' then begin
      try
        Result := StrToInt( tmp ) ;
      except
      end;
    end;
  end;

  function GisMetadataAsFloat(
    const _name    : String ;
    const _default : Double
  ) : Double ;
  var
    tmp : String ;
  begin
    Result := _default ;

    tmp := GisMetadata.Values[_name] ;

    if tmp <> '' then begin
      try
        Result := DotStrToFloat( tmp ) ;
      except
      end;
    end;
  end;

  function GisMetadataAsString(
    const _name    : String ;
    const _default : String
  ) : String ;
  var
    tmp : String ;
  begin
    Result := _default ;

    tmp := GisMetadata.Values[_name] ;

    if not IsStringEmpty( tmp ) then begin
      Result := tmp ;
    end;
  end;

  function GisColorRampList : TGIS_ColorRampList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( FGisColorRampList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( FGisColorRampList ) then
            FGisColorRampList := TGIS_ColorRampList.Create ;
        finally
          thc.UnlockThread ;
        end ;
      finally
        FreeObject( thc );
      end ;
    end ;

    Result := FGisColorRampList ;
  end ;

  function GisIsMetricSystem : Boolean ;
  begin
    Result := IsMetricSystem ;
  end ;

  function GisSystemCodePage : Integer ;
  begin
    Result := GetACP ;
  end ;

  function GisProxySettings : TGIS_ProxySettings ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( FGisProxySettings ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( FGisProxySettings ) then
            FGisProxySettings := TGIS_ProxySettings.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := FGisProxySettings ;
  end ;

  procedure GisSetProxySettings(
    const _server : String ;
    const _port   : Integer ;
    const _user   : String ;
    const _pass   : String ;
    const _domain : String
 ) ;
  begin
    GisProxySettings.Server := _server ;
    GisProxySettings.Port   := _port   ;
    GisProxySettings.User   := _user   ;
    GisProxySettings.Pass   := _pass   ;
    GisProxySettings.Domain := _domain ;
  end ;

  // SunPosition algorithm and helper functions

  // Coordinates in WGS (radians) to Lat/Long variables
  procedure getLatLong(
    const _ptg  : TGIS_Point;
    var   _lat  : Double ;
    var   _long : Double
  ) ;
  begin
    _long := _ptg.X ;  // deg
    _lat  := _ptg.Y ;  // deg
  end;

  // The day of the year, expressed as a value between 1 and 366
  function getDayOfTheYear (
      const _datetime : TDateTime
  ) : Word ;
  begin
    {$IFDEF DCC}
      Result := DayOfTheYear( _datetime ) ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := _datetime.DayOfYear ;
    {$ENDIF}
    {$IFDEF JAVA}
      var cal: Calendar := Calendar.getInstance(TimeZone.getTimeZone('GMT'));
      cal.setTime(_datetime);
      Result := cal.get(Calendar.DAY_OF_YEAR);
    {$ENDIF}
  end;

  // The equation of time, in minutes, is an empirical equation that corrects
  // for the eccentricity of the Earth's orbit and the Earth's axial tilt.
  function equationOfTime(
    const _day : Word
  ) : Double ;
  var
    b : Double ;
  begin
    b := DegToRad( 360.0 / 365.0 * ( _day - 81 ) ) ;  // radians
    Result := 9.87 * Sin( 2*b ) - 7.53 * Cos( b ) - 1.5 * Sin( b ) ;
  end;

  // Calculates sun altitude (elevation) and azimuth based on latitude,
  // local solar time and day of the year.
  procedure getSunPosition(
    const _lat      : Double ;
    const _lst      : Double ;
    const _d        : Word ;
    var   _altitude : Double ;
    var   _azimuth  : Double
  ) ;
  var
    hra, delta, alpha, gamma : Double ;
  begin
    // Hour Angle (HRA), in radians
    hra := DegToRad( 15 * ( _lst - 12 ) ) ;

    // Declination angle (delta), in radians
    delta := DegToRad( -23.45 * Cos( DegToRad( 360.0/365.0 * ( _d+10 ) ) ) ) ;

    // Elevation angle (alpha), in radians
    alpha := ArcSin(
      Sin( delta ) * Sin( _lat ) +
      Cos( delta ) * Cos( _lat ) * Cos( hra )
    ) ;

    // Azimuth angle (gamma), in radians
    gamma := ArcCos(
      ( Sin( delta ) * Cos( _lat ) -
        Cos( delta ) * Sin( _lat ) * Cos( hra )
      ) / Cos( alpha )
    ) ;

    // Results, in radians
    _altitude := alpha ;
    _azimuth := gamma ;
    if ( _lst > 12 ) or ( hra > 0 ) then
      _azimuth := 2*Pi - _azimuth ;
  end;

  // Main algorithm based on location, local time, and UTC offset
  procedure GisSunPosition(
    const _ptg        : TGIS_Point ;
    const _local_time : TDateTime ;
    const _utc_offset : Double ;
    var   _altitude   : Double ;
    var   _azimuth    : Double
  ) ;
  var
    long, lat      : Double ;
    d, h, m, s, ms : Word ;
    eot, tc        : Double ;
    lt, lst, lstm  : Double ;
  begin
    // location, in radians
    getLatLong( _ptg, lat, long ) ;

    // Local Standard Time Meridian (LSTM), in degrees
    lstm := 15 * _utc_offset ;  // deg

    // the day of the year, with Jan 1 as d = 1
    d := getDayOfTheYear( _local_time ) ;

    // Equation of Time (EoT), in minutes
    eot := equationOfTime( d ) ;

    // Time Correction Factor (TC), in minutes
    tc := 4 * ( RadToDeg( long ) - lstm ) + eot ;

    // Local Time (LT), in hours
    DecodeTime( _local_time, h, m, s, ms ) ;
    lt := h + m / 60 + ( s + 0.001*ms ) / 3600 ;

    // Local Solar Time (LST), in hours
    lst := lt + tc / 60 ;

    getSunPosition( lat, lst, d, _altitude, _azimuth ) ;
  end;

  // This algorithm is based on previous but works with '_utc_time'
  procedure GisSunPosition(
    const _ptg      : TGIS_Point ;
    const _utc_time : TDateTime ;
    var   _altitude : Double ;
    var   _azimuth  : Double
  ) ;
  var
    long, long_deg, lat      : Double ;
    d, h, m, s, ms : Word ;
    eot, lt, lst   : Double ;
  begin
    // location, in radians
    getLatLong( _ptg, lat, long ) ;

    // the day of the year, with Jan 1 as d = 1
    d := getDayOfTheYear( _utc_time ) ;

    // Local Time (LT) in this algorithm is actually UTC time, in hours
    DecodeTime( _utc_time, h, m, s, ms ) ;
    lt := h + m / 60 + ( s + 0.001*ms ) / 3600 ;  // hr

    // conversion to UTC, may cause change of the day
    long_deg := RadToDeg( long ) ;
    if ( lt + long_deg / 15 < 0 ) then begin
      dec( d );
      lt := lt + 24 ;
    end
    else if ( lt + long_deg / 15 >= 24 ) then begin
      inc( d ) ;
      lt := lt - 24 ;
    end ;

    // Equation of Time (EoT), in minutes
    eot := equationOfTime( d ) ;

    // Time Correction Factor (TC) is omitted

    // Local Solar Time (LST) equation is modified, in hours
    lst := lt + ( 4 * long_deg + eot ) / 60 ;

    getSunPosition( lat, lst, d, _altitude, _azimuth ) ;
  end;

  function GisColorMap(
    const _index : Double ;
    const _color : TGIS_Color
  ) : TGIS_ColorMap ;
  begin
    Result.Index := _index ;
    Result.RGB   := _color ;
  end ;

  function GisColorMapEx(
    const _class  : Integer ;
    const _colors : TGIS_ColorArray
  ) : TGIS_ColorMapEx ;
  begin
    Result.Index  := _class ;
    Result.Colors := _colors ;
  end ;
{$ENDREGION}

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
    GisEnvironmentInfo ;

    GisKeyList ;
    GisPasswordList ;
    GisAliasList ;
    GisProxySettings ;

    GisDeveloperKernelType := [] ;
    {$IFDEF JAVA}
      GisDeveloperKernelType := [TGIS_DeveloperKernelTypes.JAVA] ;
    {$ENDIF}
    {$IFDEF CLR}
      GisDeveloperKernelType := [TGIS_DeveloperKernelTypes.NET] ;
    {$ENDIF}
    {$IFDEF DCC}
      {$IFDEF ACTIVEX}
        GisDeveloperKernelType := [TGIS_DeveloperKernelTypes.ACTIVEX] ;
     {$ELSE}
        GisDeveloperKernelType := GisDeveloperKernelType +
          [TGIS_DeveloperKernelTypes.DELPHI] ;
      {$ENDIF}
    {$ENDIF}

  finalization
    FreeObject( FGisEnvironmentInfo) ;
    FreeObject( FGisKeyList        ) ;
    FreeObject( FGisPasswordList   ) ;
    FreeObject( FGisMetadata       ) ;
    FreeObject( FGisProxySettings  ) ;
    FreeObject( FGisColorRampList  ) ;
{$ENDIF}

//==================================== END =====================================
end.
