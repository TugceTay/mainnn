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
  This is a primary types unit.

  Some very common functions and types are defined here.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoTypes ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoTypes"'}
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
    System.Text,
    System.Drawing,
    System.Runtime.InteropServices,
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Math,
    System.Classes,
    System.SysUtils,
    System.Generics.Defaults,
    System.Generics.Collections,

    Lider.CG.GIS.GeoTypesUI ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF COCOA}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL,
    Foundation ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  /// Alias for generic list of integers;
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ListOfIntegers = {$IFDEF OXYGENE} public {$ENDIF}
                          TList< Integer > ;
  {$ELSE}
    TGIS_ListOfIntegers = class (
                            TList< Integer >
                          ) ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  /// Alias for generic list of strings;
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ListOfStrings = {$IFDEF OXYGENE} public {$ENDIF}
                         TList< String > ;
  {$ELSE}
    TGIS_ListOfStrings = class (
                           TList< String >
                         ) ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  /// Alias for generic list of doubles;
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ListOfDoubles = {$IFDEF OXYGENE} public {$ENDIF}
                         TList< Double > ;
  {$ELSE}
    TGIS_ListOfDoubles = class (
                           TList< Double >
                         ) ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  /// Alias for generic list of variants;
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ListOfVariants = {$IFDEF OXYGENE} public {$ENDIF}
                          TList< Variant > ;
  {$ELSE}
    TGIS_ListOfVariants = class (
                            TList< Variant >
                          ) ;
  {$ENDIF}

  /// <summary>
  ///   Uid type definition
  /// </summary>
  TGIS_Uid = {$IFDEF OXYGENE} public {$ENDIF} Int64 ;

  /// <summary>
  ///   Like TPoint, but based on doubles instead of integers.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_Point = {$IFDEF OXYGENE} public {$ENDIF}
                {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    {$IFDEF OXYGENE} public {$ENDIF}
      /// <summary>
      ///   horizontal position
      /// </summary>
      X : Double ;

      /// <summary>
      ///   vertical position
      /// </summary>
      Y : Double ;

    {$IFDEF OXYGENE}
      public
        {$IFDEF GIS_NORECORDS}
          /// <summary>
          ///  Standard constructor.
          /// </summary>
          constructor Create  ; overload;
        {$ENDIF}
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        constructor Create    ( const _x, _y : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        constructor Create    ( const _x, _y : Double
                              ) ; overload;
        {$IFDEF GIS_NORECORDS}
          function MakeCopy : TGIS_Point ;
        {$ENDIF}
      /// <inheritdoc/>
        function ToString     : String ; override ;
    {$ENDIF}
  end ;
  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Point = ^TGIS_Point ;
  {$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  type
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_Point = TGIS_Point ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_Point( const _pt : TGIS_Point ) : TGIS_Point ;
  {$ENDIF}

type
  //? Make TGIS_Point generic in future.
  {#gendoc:hide}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Like TPoint, but 3rd dimension (Z) added.
  ///   For internal use only.
  /// </summary>
  TGIS_Point3DInt = {$IFDEF OXYGENE} public {$ENDIF}
                    {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

    {$IFDEF OXYGENE} public {$ENDIF}
      /// <summary>
      ///   horizontal position
      /// </summary>
      X : Integer ;

      /// <summary>
      ///   vertical position
      /// </summary>
      Y : Integer ;

      /// <summary>
      ///   altitude
      /// </summary>
      Z : Integer ;
    {$IFDEF OXYGENE}
      public
        {$IFDEF JAVA}
          /// <summary>
          ///  Standard constructor.
          /// </summary>
          constructor Create ; overload;
        {$ENDIF}
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        /// <param name="_z">
        ///   z coordinate
        /// </param>
        constructor Create   ( const _x, _y, _z : Integer
                             ) ; overload;
    {$ENDIF}
  end ;

type
  /// <summary>
  ///   Like TGIS_Point, but 3rd dimension (Z) and measure (M) added.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Measure is added for Shapefile compatibility.
  ///    </note>
  /// </remarks>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_Point3D = {$IFDEF OXYGENE} public {$ENDIF}
                 {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   horizontal position
      /// </summary>
      X : Double ;

      /// <summary>
      ///   vertical position
      /// </summary>
      Y : Double ;

      /// <summary>
      ///   altitude
      /// </summary>
      Z : Double ;

      /// <summary>
      ///   measure
      /// </summary>
      M : Double ;

    {$IFDEF OXYGENE}
      public
        {$IFDEF GIS_NORECORDS}
          /// <summary>
          ///  Standard constructor.
          /// </summary>
          constructor Create  ; overload;
        {$ENDIF}
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        constructor Create    ( const _x, _y : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        /// <param name="_z">
        ///   z coordinate
        /// </param>
        constructor Create    ( const _x, _y, _z : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        /// <param name="_z">
        ///   z coordinate
        /// </param>
        /// <param name="_m">
        ///   measure
        /// </param>
        constructor Create    ( const _x, _y, _z, _m : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        constructor Create    ( const _x, _y : Double
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        /// <param name="_z">
        ///   z coordinate
        /// </param>
        constructor Create    ( const _x, _y, _z : Double
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_x">
        ///   x coordinate
        /// </param>
        /// <param name="_y">
        ///   y coordinate
        /// </param>
        /// <param name="_z">
        ///   z coordinate
        /// </param>
        /// <param name="_m">
        ///   measure
        /// </param>
        constructor Create    ( const _x, _y, _z, _m : Double
                              ) ; overload;
        {$IFDEF GIS_NORECORDS}
          function MakeCopy   : TGIS_Point3D ;
        {$ENDIF}
      /// <inheritdoc/>
        function ToString     : String ; override ;
    {$ENDIF}
  end ;
  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Point3D = ^TGIS_Point3D ;
  {$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  type
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_Point3D = TGIS_Point3D ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_Point3D( const _pt : TGIS_Point3D ) : TGIS_Point3D ;
  {$ENDIF}

  type
  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}

  /// <summary>
  ///   Like TRect, but based on doubles instead of integers.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_Extent = {$IFDEF OXYGENE} public {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   left border
      /// </summary>
      XMin : Double ;

      /// <summary>
      ///   bottom border
      /// </summary>
      YMin : Double ;

      /// <summary>
      ///   right border
      /// </summary>
      XMax : Double ;

      /// <summary>
      ///   top border
      /// </summary>
      YMax : Double ;

    {$IFDEF OXYGENE}
      public
        {$IFDEF GIS_NORECORDS}
          /// <summary>
          ///  Standard constructor.
          /// </summary>
          constructor Create  ; overload;
        {$ENDIF}
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _xmax, _ymax : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _xmax, _ymax : Double
                              ) ; overload;
        {$IFDEF GIS_NORECORDS}
          function MakeCopy  : TGIS_Extent ;
        {$ENDIF}
      /// <inheritdoc/>
        function ToString     : String ; override ;
    {$ENDIF}
  end ;

  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Extent = ^TGIS_Extent ;
  {$ENDIF}

  {$IFNDEF GIS_NORECORDS}
    type
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_Extent = {$IFDEF OXYGENE} public {$ENDIF} TGIS_Extent ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_Extent( const _ext : TGIS_Extent ) : TGIS_Extent ;
  {$ENDIF}

  type
  /// <summary>
  ///   Like TGIS_Extent, but 3rd dimension added.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_Extent3D = {$IFDEF OXYGENE} public {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   left border
      /// </summary>
      XMin : Double ;

      /// <summary>
      ///   bottom border
      /// </summary>
      YMin : Double ;

      /// <summary>
      ///   height border
      /// </summary>
      ZMin : Double ;

      /// <summary>
      ///   value border
      /// </summary>
      MMin : Double ;

      /// <summary>
      ///   right border
      /// </summary>
      XMax : Double ;

      /// <summary>
      ///   top border
      /// </summary>
      YMax : Double ;

      /// <summary>
      ///   height border
      /// </summary>
      ZMax : Double ;

      /// <summary>
      ///   value border
      /// </summary>
      MMax : Double ;

    {$IFDEF OXYGENE}
      public
        {$IFDEF GIS_NORECORDS}
          /// <summary>
          ///  Standard constructor.
          /// </summary>
          constructor Create  ; overload;
        {$ENDIF}
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _xmax, _ymax : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_zmin">
        ///   z-min limit
        /// </param>
        /// <param name="_mmin">
        ///   m-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        /// <param name="_zmax">
        ///   z-max limit
        /// </param>
        /// <param name="_mmax">
        ///   z-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _zmin, _mmin, _xmax, _ymax, _zmax, _mmax : Integer
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _xmax, _ymax : Double
                              ) ; overload;
        /// <summary>
        ///   Create object on given coordinates.
        /// </summary>
        /// <param name="_xmin">
        ///   x-min limit
        /// </param>
        /// <param name="_ymin">
        ///   y-min limit
        /// </param>
        /// <param name="_zmin">
        ///   z-min limit
        /// </param>
        /// <param name="_mmin">
        ///   m-min limit
        /// </param>
        /// <param name="_xmax">
        ///   x-max limit
        /// </param>
        /// <param name="_ymax">
        ///   y-max limit
        /// </param>
        /// <param name="_zmax">
        ///   z-max limit
        /// </param>
        /// <param name="_mmax">
        ///   m-max limit
        /// </param>
        constructor Create    ( const _xmin, _ymin, _zmin, _mmin, _xmax, _ymax, _zmax, _mmax : Double
                              ) ; overload;
        {$IFDEF GIS_NORECORDS}
          function MakeCopy   : TGIS_Extent3D ;
        {$ENDIF}
      /// <inheritdoc/>
        function ToString     : String ; override ;

    {$ENDIF}
  end ;
  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Extent3D = ^TGIS_Extent3D ;
  {$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  type
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_Extent3D = TGIS_Extent3D ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_Extent3D( const _ext : TGIS_Extent3D ) : TGIS_Extent3D ;
  {$ENDIF}

  type
  /// <summary>
  ///   Line between two points.
  /// </summary>
  TGIS_Line = {$IFDEF OXYGENE} public {$ENDIF}
              {$IFDEF GIS_PACKED} packed {$ENDIF} record
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   begin point
      /// </summary>
      A : TGIS_Point ;

      /// <summary>
      ///   end point
      /// </summary>
      B : TGIS_Point ;

  end ;
  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Line = ^TGIS_Line ;
  {$ENDIF}

  /// <summary>
  ///   Line between two 3D points.
  /// </summary>
  TGIS_Line3D = {$IFDEF OXYGENE} public {$ENDIF}
                {$IFDEF GIS_PACKED} packed {$ENDIF} record
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   begin point
      /// </summary>
      A : TGIS_Point3D ;

      /// <summary>
      ///   end point
      /// </summary>
      B : TGIS_Point3D ;
  end ;
  {$IFNDEF MANAGED}
    {#gendoc:hide}
    PGIS_Line3D = ^TGIS_Line3D ;
  {$ENDIF}

type
  /// <summary>
  ///   Defines an interpolation method.
  /// </summary>
  TGIS_InterpolationMethod = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Exponential interpolation.
      /// </summary>
      Exponential,

      /// <summary>
      ///   Linear interpolation.
      /// </summary>
      Linear,

      /// <summary>
      ///   Logarithmic interpolation.
      /// </summary>
       Logarithmic
  ) ;

  /// <summary>
  ///   Shows what scope of the layers must be drawn.
  /// </summary>
  TGIS_DrawMode = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Draw all layers; also draw selection.
      /// </summary>
      All,

      /// <summary>
      ///   Draw all layers except top non-cached layers (for example GPS tracking
      ///   layers); also draw selections.
      /// </summary>
      AllExceptTop,

      /// <summary>
      ///   Draw only top non-cached layers (for example GPS tracking layers).
      /// </summary>
      Top,

      /// <summary>
      ///   Draw only selected shapes for all layers; also draw selection.
      /// </summary>
      OnlySelectedAll,

      /// <summary>
      ///   Draw only selected shapes all layers except top non-cached layers
      ///   (for example GPS tracking layers); also draw selections.
      /// </summary>
      OnlySelectedAllExceptTop,

      /// <summary>
      ///   Draw only selected shapes for top non-cached layers
      ///   (for example GPS tracking layers).
      /// </summary>
      OnlySelectedTop,

      /// <summary>
      ///   Draw flashing shapes
      /// </summary>
      Flash,

      /// <summary>
      ///   Draw all layer except 3d features
      /// </summary>
      AllExcept3D
  ) ;



  /// <summary>
  ///   Compression types.
  /// </summary>
  TGIS_CompressionType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   No compression, in BGR format.
      /// </summary>
      None,

      /// <summary>
      ///   JPEG compression.
      /// </summary>
      JPEG,

      /// <summary>
      ///   PNG compression.
      /// </summary>
      PNG,

      /// <summary>
      ///   Pack Bits compression.
      /// </summary>
      PackBits,

      /// <summary>
      ///   RLE (Run Length Encoding) compression.
      /// </summary>
      RLE,

      /// <summary>
      ///   CCITT Huffman-RLE compression.
      /// </summary>
      CCITTHuffmanRLE,

      /// <summary>
      ///   LZW (Lempel-Ziv-Welch) compression.
      /// </summary>
      LZW,

      /// <summary>
      ///   CCITT Fax Group 3 compression.
      /// </summary>
      CCITTFaxGroup3,

      /// <summary>
      ///   CCITT Fax Group 4 compression.
      /// </summary>
      CCITTFaxGroup4,

      /// <summary>
      ///   ZLIB compression.
      /// </summary>
      ZLIB,

      /// <summary>
      ///   LZ77 compression.
      /// </summary>
      LZ77,

      /// <summary>
      ///   JPEG 2000 compression.
      /// </summary>
      JP2,

      /// <summary>
      ///   JPEG Plus compression.
      /// </summary>
      JPEGPlus,

      /// <summary>
      ///   No compression, in ARGB format.
      /// </summary>
      ARGB
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisCompressionTypeNone            = TGIS_CompressionType.None            ;
      gisCompressionTypeJPEG            = TGIS_CompressionType.JPEG            ;
      gisCompressionTypePNG             = TGIS_CompressionType.PNG             ;
      gisCompressionTypePackBits        = TGIS_CompressionType.PackBits        ;
      gisCompressionTypeRLE             = TGIS_CompressionType.RLE             ;
      gisCompressionTypeCCITTHuffmanRLE = TGIS_CompressionType.CCITTHuffmanRLE ;
      gisCompressionTypeLZW             = TGIS_CompressionType.LZW             ;
      gisCompressionTypeCCITTFaxGroup3  = TGIS_CompressionType.CCITTFaxGroup3  ;
      gisCompressionTypeCCITTFaxGroup4  = TGIS_CompressionType.CCITTFaxGroup4  ;
      gisCompressionTypeZLIB            = TGIS_CompressionType.ZLIB            ;
      gisCompressionTypeLZ77            = TGIS_CompressionType.LZ77            ;
      gisCompressionTypeJP2             = TGIS_CompressionType.JP2             ;
      gisCompressionTypeJPEGPlus        = TGIS_CompressionType.JPEGPlus        ;
  {$ENDIF}

type

  /// <summary>
  ///   Basic modes of TGIS_ViewerWnd operations:
  /// </summary>
  TGIS_ViewerMode = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Mouse is in selecting mode.
      /// </summary>
      Select,

      /// <summary>
      ///   Mouse is in dragging mode.
      /// </summary>
      Drag,

      /// <summary>
      ///   Mouse is in zooming mode.
      /// </summary>
      Zoom,

      /// <summary>
      ///   Mouse is in editing mode.
      /// </summary>
      Edit,

      /// <summary>
      ///   Mouse is in zooming mode (visual effect).
      /// </summary>
      ZoomEx,

      /// <summary>
      ///   User defined mode - without any built-in action.
      /// </summary>
      UserDefined
  ) ;

  /// <summary>
  ///   Pointer mode for operations.
  /// </summary>
  TGIS_PointerMode = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Mouse operation.
    /// </summary>
    Mouse,

    /// <summary>
    ///   Pen touch operation.
    /// </summary>
    Pen,

    /// <summary>
    ///   Finger touch operation.
    /// </summary>
    Touch
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisSelect       = TGIS_ViewerMode.Select       ;
      gisDrag         = TGIS_ViewerMode.Drag         ;
      gisZoom         = TGIS_ViewerMode.Zoom         ;
      gisEdit         = TGIS_ViewerMode.Edit         ;
      gisZoomEx       = TGIS_ViewerMode.ZoomEx       ;
      gisUserDefined  = TGIS_ViewerMode.UserDefined  ;
  {$ENDIF}

type
  /// <summary>
  ///   North arrow stock styles
  /// </summary>
  TGIS_ControlNorthArrowStyle =
  (
    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Arrow1,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Arrow2,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Needle1,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Needle2,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Needle3,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Rose1,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Rose2,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Rose3,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Disk1,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Disk2,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Disk3,

    /// <summary>
    ///   north arrow symbol
    /// </summary>
    Triangle1
  ) ;

type
  /// <summary>
  ///   RGB color representation.
  /// </summary>
  TGIS_RGBTriple = {$IFDEF OXYGENE} public {$ENDIF}
                   {$IFDEF GIS_PACKED} packed {$ENDIF} record
   {$IFDEF OXYGENE} public {$ENDIF}
     /// <summary>
     ///   Blue value.
     /// </summary>
     rgbtBlue    : Byte ;
     /// <summary>
     ///   Green value.
     /// </summary>
     rgbtGreen   : Byte ;
     /// <summary>
     ///   Red value.
     /// </summary>
     rgbtRed     : Byte ;
  end ;

type
  {$IFDEF GIS_NODB}
    TFieldType = {$IFDEF OXYGENE} public {$ENDIF}
    ( ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
      ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
      ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
      ftLargeInt, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
      ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd ) ;
  {$ENDIF}

  {$IFNDEF OXYGENE}

    /// <summary>
    ///   Type for TClass casting. Provided for .NET source compatibility with
    /// </summary>
    typeOf = TClass ;
  {$ENDIF}

  {$IFDEF CLR}
    {$IFDEF OXYGENE}
      {#gendoc:hide}
      ConvertPoint = TPoint ;
    {$ELSE}
      // See ConvertPoint()
    {$ENDIF}
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Type for TPoint casting. Provided for .NET source compatibility with
    ///   ConvertPoint() functions.
    /// </summary>
    ConvertPoint = TPoint ;
  {$ENDIF}

  {$IFDEF CLR}
    {$IFDEF OXYGENE}
      {#gendoc:hide}
      ConvertRect = TRect ;
    {$ELSE}
      // See ConvertRect()
    {$ENDIF}
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Type for TRect casting. Provided for .NET source compatibility with
    ///   ConvertRect() functions.
    /// </summary>
    ConvertRect = TRect ;
  {$ENDIF}

  {$IFNDEF MANAGED}

    {$IFNDEF LEVEL_XE2_RTL}

        /// <summary>
        ///   Provided for .NET source compatibility.
        /// </summary>
        IntPtr = Pointer ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CLR}
      {#gendoc:hide}
      /// <summary>
      ///   Provided for D2009 source compatibility.
      /// </summary>
      TEncoding = Encoding ;
  {$ENDIF}

  /// <summary>
  ///   Type for multi-user work mode.
  /// </summary>
  TGIS_MultiUser = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Default mode.
      /// </summary>
      Default,

      /// <summary>
      ///   Single user mode.
      /// </summary>
      SingleUser,

      /// <summary>
      ///   Multi-user mode.
      /// </summary>
      MultiUser
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisDefault    = TGIS_MultiUser.Default    ;
      gisSingleUser = TGIS_MultiUser.SingleUser ;
      gisMultiUser  = TGIS_MultiUser.MultiUser  ;
  {$ENDIF}

type

  {$IFDEF OXYGENE}
    /// <summary>
    ///  Provides data for the Busy event.
    /// </summary>
    TGIS_BusyEventArgs = public class ( EventArgs )
      private
        FPos    : Int64 ;
        FEnd    : Int64 ;
        FAbort  : Boolean ;

      public
        /// <summary>
        ///   Create an object
        /// </summary>
        /// <param name="_pos">
        ///   initial position within 0.._end
        /// </param>
        /// <param name="_end">
        ///   maximal position
        /// </param>
        constructor Create( _pos    : Int64 ;
                            _end    : Int64
                          ) ; overload;
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_pos">
        ///   initial position within 0.._end
        /// </param>
        /// <param name="_end">
        ///   maximal position
        /// </param>
        /// <param name="_abort">
        ///   if true then operation should be aborted
        /// </param>
        constructor Create( _pos    : Int64 ;
                            _end    : Int64 ;
                            _abort  : Boolean
                          ) ; overload;

      public
        /// <summary>
        ///   Current position; -1 at the end of the run
        /// </summary>
        property Pos      : Int64  read FPos ;

        /// <summary>
        ///   Max position; -1 at the end of the run.
        /// </summary>
        property EndPos   : Int64  read FEnd ;

        /// <summary>
        ///   If set to True inside message handler then an abort request.
        /// </summary>
        property Abort    : Boolean  read FAbort write FAbort ;
    end;


    /// <summary>
    ///   Provides data for the help event.
    /// </summary>
    TGIS_HelpEventArgs = public class ( EventArgs )
      private
        FName : String ;

      public
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_name">
        ///   name of a control for which help should be provided
        /// </param>
        constructor Create( _name : String
                          ) ;

      public
        /// <summary>
        ///   Name of a control for which help should be provided.
        /// </summary>
        property Name   : String  read FName ;
    end;


    /// <summary>
    ///   Provides data for the TemplateProducer event.
    /// </summary>
    TGIS_TemplateProducerEventArgs = public class ( EventArgs )
      private
        FToken : String ;

      public
        /// <summary>
        ///   Create object based on token string.
        /// </summary>
        /// <param name="_token">
        ///   name of a token for which replacement should be provided
        /// </param>
        constructor Create( _token : String
                          ) ;

      public
        /// <summary>
        ///   Name of a token for which replacement should be provided.
        /// </summary>
        property Token    : String  read FToken ;
    end;

    /// <summary>
    ///   Provides data for the Read/Write event.
    /// </summary>
    TGIS_ReadWriteEventArgs = public class( EventArgs )
      private
        FPos    : LongInt ;
        FBuffer : TBytes  ;
        FCount  : Integer ;

      public
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_pos">
        ///   position of the buffer within the file
        /// </param>
        /// <param name="_buffer">
        ///   buffer to be encoded/decoded
        /// </param>
        /// <param name="_count">
        ///   number of bytes within buffer
        /// </param>
        constructor Create( const _pos     : LongInt ;
                            const _buffer  : TBytes  ;
                            const _count   : Integer
                          ) ;

      public
        /// <summary>
        ///   Position of the buffer within the file.
        /// </summary>
        property Pos      : LongInt read FPos ;

        /// <summary>
        ///   Buffer to be encoded/decoded.
        /// </summary>
        property Buffer   : TBytes  read FBuffer ;

        /// <summary>
        ///   Number of bytes within buffer.
        /// </summary>
        property Count    : Integer read FCount ;
    end;

    /// <summary>
    ///   Provides data for the BeforePaint/AfterPaint event.
    /// </summary>
    TGIS_PaintEventArgs = public class( EventArgs )
      private
        FGraphics : TObject ;

      public
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_graphics">
        ///   graphics object
        /// </param>
        constructor Create( const _graphics : TObject
                          ) ;

      public
        /// <summary>
        ///   Graphics object.
        /// </summary>
        property Graphics : TObject read FGraphics ;
    end;
  {$ENDIF}

  {$IFDEF OXYGENE}
     /// <summary>
     ///   Event handler for Help messages.
     /// </summary>
     /// <param name="_sender">
     ///   event originator
     /// </param>
     /// <param name="_e">
     ///   event data
     /// </param>
    TGIS_HelpEvent = public procedure(
      _sender  : Object ;
      _e       : TGIS_HelpEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event handler for OnHelp messages.
    /// </summary>
    /// <param name="_sender">
    ///   event originator
    /// </param>
    /// <param name="_name">
    ///   name of a control for which help should be provided.
    /// </param>
    TGIS_HelpEvent = procedure(
      _sender  : TObject ;
      _name    : String
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}
     /// <summary>
     ///   Event handler for Template producer.
     /// </summary>
     /// <param name="_sender">
     ///   event originator
     /// </param>
     /// <param name="_e">
     ///   event parameters
     /// </param>
     /// <returns>
     ///   replacement for token
     /// </returns>
    TGIS_TemplateProducerEvent = public function(
      _sender : TObject;
      _e    : TGIS_TemplateProducerEventArgs
    ) : String of object ;
  {$ELSE}

    /// <summary>
    ///   Event handler for Template producer.
    /// </summary>
    /// <param name="_sender">
    ///   event originator
    /// </param>
    /// <param name="_token">
    ///   name of a token for which replacement should be provided
    /// </param>
    TGIS_TemplateProducerEvent = function(
      _sender : TObject;
      _token  : String
    ) : String of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}
     /// <summary>
     ///   Event handler for file Read/Write events for the purpose of
     ///   decoding/encoding
     /// </summary>
     /// <param name="_sender">
     ///   event originator
     /// </param>
     /// <param name="_e">
     ///   event data
     /// </param>
    TGIS_ReadWriteEvent = public procedure(
      _sender : Object ;
      _e      : TGIS_ReadWriteEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event handler for file Read/Write events for the purpose of
    ///   decoding/encoding.
    /// </summary>
    /// <param name="_sender">
    ///   event originator
    /// </param>
    /// <param name="_pos">
    ///   position of the buffer within the file
    /// </param>
    /// <param name="_buffer">
    ///   buffer to be encoded/decoded
    /// </param>
    /// <param name="_count">
    ///   number of bytes within buffer
    /// </param>
    {$IFDEF GENXDK}
      TGIS_ReadWriteEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject ;
            _pos        : LongInt ;
            _buffer     : Pointer ;
            _count      : Integer
      ) of object ;
    {$ELSE}
      TGIS_ReadWriteEvent = procedure(
        _sender     : TObject ;
        _pos        : LongInt ;
        _buffer     : Pointer ;
        _count      : Integer
      ) of object ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OXYGENE}
     /// <summary>
     ///   Event handler for file BeforePaint/AfterPaint events.
     /// </summary>
     /// <param name="_sender">
     ///   event originator
     /// </param>
     /// <param name="_e">
     ///   event data
     /// </param>
    TGIS_PaintEvent = public procedure(
      _sender : Object ;
      _e      : TGIS_PaintEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event handler for file BeforePaint/AfterPaint events.
    /// </summary>
    /// <param name="_sender">
    ///   event originator
    /// </param>
    /// <param name="_canvas">
    ///   canvas object
    /// </param>
    {$IFDEF GENXDK}
      TGIS_PaintEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject ;
            _canvas     : TObject
      ) of object ;
    {$ELSE}
      TGIS_PaintEvent = procedure(
        _sender     : TObject ;
        _canvas     : TObject
      ) of object ;
    {$ENDIF}
  {$ENDIF}

type

  /// <summary>
  ///   Types of GIS shapes.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_ShapeType  = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF}
    (

      /// <summary>
      ///   undefined shape
      /// </summary>
      Unknown     {$IFDEF ISLAND}= 0{$ENDIF},

      /// <summary>
      ///   shape marked as a deleted item
      /// </summary>
      Deleted     {$IFDEF ISLAND}= 1{$ENDIF},

      /// <summary>
      ///   single point
      /// </summary>
      Point       {$IFDEF ISLAND}= 2{$ENDIF},

      /// <summary>
      ///   multiple point shape
      /// </summary>
      MultiPoint  {$IFDEF ISLAND}= 3{$ENDIF},

      /// <summary>
      ///   line shape
      /// </summary>
      Arc         {$IFDEF ISLAND}= 4{$ENDIF},

      /// <summary>
      ///   area shape
      /// </summary>
      Polygon     {$IFDEF ISLAND}= 5{$ENDIF},

      /// <summary>
      ///   complex
      /// </summary>
      Complex     {$IFDEF ISLAND}= 6{$ENDIF},

      /// <summary>
      ///   multipatch
      /// </summary>
      MultiPatch  {$IFDEF ISLAND}= 7{$ENDIF},

      /// <summary>
      ///   null shape
      /// </summary>
      Null        {$IFDEF ISLAND}= 8{$ENDIF}
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;


  {$IFDEF JAVA}
  {#gendoc:hide}
  /// <summary>
  ///   Emulation DialogResult for Java.
  /// </summary>
  DialogResult = public
  (

      /// <summary>
      ///   cancel option
      /// </summary>
      Cancel     ,

      /// <summary>
      ///   ok option
      /// </summary>
      Ok
  ) of Integer ;
  {$ENDIF}

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
     const
       gisShapeTypeUnknown     = TGIS_ShapeType.Unknown     ;
       gisShapeTypeDeleted     = TGIS_ShapeType.Deleted     ;
       gisShapeTypePoint       = TGIS_ShapeType.Point       ;
       gisShapeTypeMultiPoint  = TGIS_ShapeType.MultiPoint  ;
       gisShapeTypeArc         = TGIS_ShapeType.Arc         ;
       gisShapeTypePolygon     = TGIS_ShapeType.Polygon     ;
       gisShapeTypeComplex     = TGIS_ShapeType.Complex     ;
       gisShapeTypeMultiPatch  = TGIS_ShapeType.MultiPatch  ;
       gisShapeTypeNull        = TGIS_ShapeType.Null        ;
   {$ENDIF}

type

  /// <summary>
  ///   Set for checking shape supported by layer.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
    TGIS_ShapeTypes = public set of TGIS_ShapeType ;
    {$ELSE}
    TGIS_ShapeTypes = TGIS_ShapeType ;
    {$ENDIF}
  {$ELSE}
    TGIS_ShapeTypes = set of TGIS_ShapeType ;
  {$ENDIF}

type

  /// <summary>
  ///   Types of shape parts.
  /// </summary>
  TGIS_PartType  = {$IFDEF CLR}  public flags {$ENDIF}
                   {$IFDEF JAVA} public       {$ENDIF}
                   {$IFDEF ISLAND} public       {$ENDIF}
  (

      /// <summary>
      ///   A linked strip of triangles
      /// </summary>
      TriangleStrip  {$IFDEF CLR} = 1  {$ENDIF},

      /// <summary>
      ///   A linked fan of triangles
      /// </summary>
      TriangleFan    {$IFDEF CLR} = 2  {$ENDIF},

      /// <summary>
      ///   The outer ring of a polygon
      /// </summary>
      OuterRing      {$IFDEF CLR} = 4  {$ENDIF},

      /// <summary>
      ///   A hole of a polygon.
      /// </summary>
      InnerRing      {$IFDEF CLR} = 8  {$ENDIF},

      /// <summary>
      ///   The first ring of a polygon of an unspecified type.
      /// </summary>
      FirstRing      {$IFDEF CLR} = 16 {$ENDIF},

      /// <summary>
      ///   A ring of a polygon of an unspecified type.
      /// </summary>
      Ring           {$IFDEF CLR} = 32 {$ENDIF},

      /// <summary>
      ///   Triangle
      /// </summary>
      Triangle       {$IFDEF CLR} = 64 {$ENDIF}
  ) ;


  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisPartTypeTriangleStrip  = TGIS_PartType.TriangleStrip ;
      gisPartTypeTriangleFan    = TGIS_PartType.TriangleFan   ;
      gisPartTypeOuterRing      = TGIS_PartType.OuterRing     ;
      gisPartTypeInnerRing      = TGIS_PartType.InnerRing     ;
      gisPartTypeFirstRing      = TGIS_PartType.FirstRing     ;
      gisPartTypeRing           = TGIS_PartType.Ring          ;
      gisPartTypeTriangle       = TGIS_PartType.Triangle      ;
  {$ENDIF}

type

  /// <summary>
  ///   Types of dimension.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_DimensionType = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF}
  (

      /// <summary>
      ///   has undefined dimension
      /// </summary>
      Unknown {$IFDEF ISLAND}= 0{$ENDIF},

      /// <summary>
      ///   has 2D coordinate
      /// </summary>
      XY      {$IFDEF ISLAND}= 1{$ENDIF},

      /// <summary>
      ///   has Z coordinate
      /// </summary>
      XYZ     {$IFDEF ISLAND}= 2{$ENDIF},

      /// <summary>
      ///   has M measure
      /// </summary>
      XYM     {$IFDEF ISLAND}= 3{$ENDIF},

      /// <summary>
      ///   has Z coordinate and M measure
      /// </summary>
      XYZM    {$IFDEF ISLAND}= 4{$ENDIF}

  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisDimensionTypeUnknown = TGIS_DimensionType.Unknown ;
      gisDimensionType2D      = TGIS_DimensionType.XY      ;
      gisDimensionTypeZ       = TGIS_DimensionType.XYZ     ;
      gisDimensionTypeM       = TGIS_DimensionType.XYM     ;
      gisDimensionTypeZM      = TGIS_DimensionType.XYZM    ;
   {$ENDIF}

type

  /// <summary>
  ///   Set for checking dimension supported by layer.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      TGIS_DimensionTypes = public set of TGIS_DimensionType ;
    {$ELSE}
      TGIS_DimensionTypes = TGIS_DimensionType ;
    {$ENDIF}
  {$ELSE}
    TGIS_DimensionTypes = set of TGIS_DimensionType ;
  {$ENDIF}

type
  /// <summary>
  ///   Virtual fields.
  /// </summary>
  {$IFDEF ISLAND}[Export]{$ENDIF}
  TGIS_VirtualField  = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF}
  (
    /// <summary>
    ///   a shape identifier
    /// </summary>
    GisUid             {$IFDEF ISLAND}= 0{$ENDIF},

    /// <summary>
    ///   the selected state indicator
    /// </summary>
    GisSelected        {$IFDEF ISLAND}= 1{$ENDIF},

    /// <summary>
    ///   the hidden state indicator
    /// </summary>
    GisHidden          {$IFDEF ISLAND}= 2{$ENDIF},

    /// <summary>
    ///   the area of the shape
    /// </summary>
    GisArea            {$IFDEF ISLAND}= 3{$ENDIF},

    /// <summary>
    ///   the length of the shape
    /// </summary>
    GisLength          {$IFDEF ISLAND}= 4{$ENDIF},

    /// <summary>
    ///   the first Z-coordinate of the shape
    /// </summary>
    GisCoordZ          {$IFDEF ISLAND}= 5{$ENDIF},

    /// <summary>
    ///   the first M-value of the shape
    /// </summary>
    GisCoordM          {$IFDEF ISLAND}= 6{$ENDIF},

    /// <summary>
    ///   the current date and time
    /// </summary>
    GisNow             {$IFDEF ISLAND}= 7{$ENDIF},

    /// <summary>
    ///   the minimum X-coordinate of the shape
    /// </summary>
    GisMinX            {$IFDEF ISLAND}= 8{$ENDIF},

    /// <summary>
    ///   the minimum Y-coordinate of the shape
    /// </summary>
    GisMinY            {$IFDEF ISLAND}= 9{$ENDIF},

    /// <summary>
    ///   the minimum Z-coordinate of the shape
    /// </summary>
    GisMinZ            {$IFDEF ISLAND}= 10{$ENDIF},

    /// <summary>
    ///   the minimum M-value of the shape
    /// </summary>
    GisMinM            {$IFDEF ISLAND}= 11{$ENDIF},

    /// <summary>
    ///   the maximum X-coordinate of the shape
    /// </summary>
    GisMaxX            {$IFDEF ISLAND}= 12{$ENDIF},

    /// <summary>
    ///   the maximum Y-coordinate of the shape
    /// </summary>
    GisMaxY            {$IFDEF ISLAND}= 13{$ENDIF},

    /// <summary>
    ///   the maximum Z-coordinate of the shape
    /// </summary>
    GisMaxZ            {$IFDEF ISLAND}= 14{$ENDIF},

    /// <summary>
    ///   the maximum M-value of the shape
    /// </summary>
    GisMaxM            {$IFDEF ISLAND}= 15{$ENDIF},

    /// <summary>
    ///   the center X-coordinate of the shape's extent
    /// </summary>
    GisCenterX         {$IFDEF ISLAND}= 16{$ENDIF},

    /// <summary>
    ///   the center Y-coordinate of the shape's extent
    /// </summary>
    GisCenterY         {$IFDEF ISLAND}= 17{$ENDIF},

    /// <summary>
    ///   the center Z-coordinate of the shape's extent
    /// </summary>
    GisCenterZ         {$IFDEF ISLAND}= 18{$ENDIF},

    /// <summary>
    ///   the center M-value of the shape
    /// </summary>
    GisCenterM         {$IFDEF ISLAND}= 19{$ENDIF},

    /// <summary>
    ///   the centeroid X-coordinate of the shape
    /// </summary>
    GixCentroidX       {$IFDEF ISLAND}= 20{$ENDIF},

    /// <summary>
    ///   the centeroid Y-coordinate of the shape
    /// </summary>
    GixCentroidY       {$IFDEF ISLAND}= 21{$ENDIF},

    /// <summary>
    ///    the number of points in a shape's geometry
    /// </summary>
    GisNumPoints       {$IFDEF ISLAND}= 22{$ENDIF},

    /// <summary>
    ///   the number of parts in a shape's geometry
    /// </summary>
    GisNumParts        {$IFDEF ISLAND}= 23{$ENDIF},

    /// <summary>
    ///   the aggregator count (for internal use)
    /// </summary>
    GisAggregatedCount {$IFDEF ISLAND}= 24{$ENDIF},

    /// <summary>
    ///   the aggregator value (for internal use)
    /// </summary>
    GisAggregatedValue {$IFDEF ISLAND}= 25{$ENDIF},

    /// <summary>
    ///   the shape's type
    /// </summary>
    GisShapeType       {$IFDEF ISLAND}= 26{$ENDIF},

    /// <summary>
    ///   the scale in which the shape is currently displayed in a viewer
    /// </summary>
    GisViewerScale     {$IFDEF ISLAND}= 27{$ENDIF},

    /// <summary>
    ///   the level in which the shape is currently displayed in a viewer
    /// </summary>
    GisViewerLevel {$IFDEF ISLAND}= 28{$ENDIF}
) {$IFDEF JAVA} of Integer {$ENDIF} ;

type

  /// <summary>
  ///   Set of the virtual fields for displaying in controls.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
    TGIS_VirtualFields = public set of TGIS_VirtualField ;
    {$ELSE}
    TGIS_VirtualFields = TGIS_VirtualField ;
    {$ENDIF}
  {$ELSE}
    TGIS_VirtualFields = set of TGIS_VirtualField ;
  {$ENDIF}

type

  /// <summary>
  ///   Types of operations.
  /// </summary>
  TGIS_OperationType = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF}
  (
      /// <summary>
      ///   select a layer
      /// </summary>
      Select,

      /// <summary>
      ///   edit a layer
      /// </summary>
      Edit

  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

type

  /// <summary>
  ///   Set for checking operations supported by layer.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      TGIS_OperationTypes = public set of TGIS_OperationType ;
    {$ELSE}
      TGIS_OperationTypes = TGIS_OperationType ;
    {$ENDIF}
  {$ELSE}
    TGIS_OperationTypes = set of TGIS_OperationType ;
  {$ENDIF}

type

  /// <summary>
  ///   Layer subtype. For handling additional information about layer type etc.
  /// </summary>
  TGIS_LayerSubType = {$IFDEF OXYGENE} public {$ENDIF}
  (

      /// <summary>
      ///   layer is persistent - is not temporary, has file representation.
      /// </summary>
      Persistent  ,

      /// <summary>
      ///   layer content can be exported (saved).
      /// </summary>
      Exportable  ,

      /// <summary>
      ///   whole layer resides in memory.
      /// </summary>
      InMemory
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisLayerSubTypePersistent  = TGIS_LayerSubType.Persistent ;
      gisLayerSubTypeExportable  = TGIS_LayerSubType.Exportable ;
      gisLayerSubTypeInMemory    = TGIS_LayerSubType.InMemory   ;
  {$ENDIF}

type

  /// <summary>
  ///   Set for checking shape supported by layer.
  /// </summary>
  TGIS_LayerSubTypeSet = {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_LayerSubType ;

type

  /// <summary>
  ///   Basic modes of layer Dormant operations:
  /// </summary>
  TGIS_LayerDormantMode = {$IFDEF OXYGENE} public {$ENDIF}
  (

      /// <summary>
      ///   layer will not become dormant.
      /// </summary>
      Off,

      /// <summary>
      ///   layer will become dormant if it is not visible.
      /// </summary>
      Standard,

      /// <summary>
      ///   layer will become dormant just after pain operations (can slow down
      ///   the viewer).
      /// </summary>
      Agressive
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisLayerDormantOff        = TGIS_LayerDormantMode.Off       ;
      gisLayerDormantStandard   = TGIS_LayerDormantMode.Standard  ;
      gisLayerDormantAgressive  = TGIS_LayerDormantMode.Agressive ;
  {$ENDIF}

type

  /// <summary>
  ///   Alignment within label.
  /// </summary>
  TGIS_LabelAlignment = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   No alignment; single line label.
      /// </summary>
      Single,

      /// <summary>
      ///   Align to left; multi-line allowed.
      /// </summary>
      LeftJustify,

      /// <summary>
      ///   Center text; multi-line allowed.
      /// </summary>
      Center,

      /// <summary>
      ///   Align to right; multi-line allowed.
      /// </summary>
      RightJustify,

      /// <summary>
      ///   label will follow the flow of shape; valid only for Arcs; ideal for
      ///   river and streets labeling; any position will be tested for fitting
      ///   with 10% tolerance; to place a label on a top of the shape set
      ///   TGIS_ParamsLabel.Position to TGIS_LabelPosition.MiddleCenter; to
      ///   force a label to display on arcs shorter than a label width set
      ///   TGIS_ParamsLabel.Position to TGIS_LabelPosition.Flow.
      /// </summary>
      Follow
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisLabelAlignmentSingle        = TGIS_LabelAlignment.Single       ;
      gisLabelAlignmentLeftJustify   = TGIS_LabelAlignment.LeftJustify  ;
      gisLabelAlignmentCenter        = TGIS_LabelAlignment.Center       ;
      gisLabelAlignmentRightJustify  = TGIS_LabelAlignment.RightJustify ;
      gisLabelAlignmentFollow        = TGIS_LabelAlignment.Follow       ;
  {$ENDIF}

type

  /// <summary>
  ///   Position of a label.
  /// </summary>
  TGIS_LabelPosition = {$IFDEF OXYGENE} public {$IFNDEF JAVA} flags {$ENDIF} {$ENDIF}
  (

      /// <summary>
      ///   Undefined.
      /// </summary>
      Undefined     ,

      /// <summary>
      ///   up-left to the insertion point.
      /// </summary>
      UpLeft        ,

      /// <summary>
      ///   Up-center to the insertion point.
      /// </summary>
      UpCenter      ,

      /// <summary>
      ///   Up-right to the insertion point.
      /// </summary>
      UpRight       ,

      /// <summary>
      ///   Middle-left to the insertion point.
      /// </summary>
      MiddleLeft    ,

      /// <summary>
      ///   Middle-center to the insertion point.
      /// </summary>
      MiddleCenter  ,

      /// <summary>
      ///   Middle-right to the insertion point.
      /// </summary>
      MiddleRight   ,

      /// <summary>
      ///   down-left to the insertion point.
      /// </summary>
      DownLeft      ,

      /// <summary>
      ///   Down-center to the insertion point.
      /// </summary>
      DownCenter    ,

      /// <summary>
      ///   down-right to the insertion point.
      /// </summary>
      DownRight     ,

      /// <summary>
      ///   Labels will be positioned based on the visible part of extent.
      /// </summary>
      Flow
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisLabelPositionUndefined     = TGIS_LabelPosition.Undefined     ;
      gisLabelPositionUpLeft        = TGIS_LabelPosition.UpLeft        ;
      gisLabelPositionUpCenter      = TGIS_LabelPosition.UpCenter      ;
      gisLabelPositionUpRight       = TGIS_LabelPosition.UpRight       ;
      gisLabelPositionMiddleLeft    = TGIS_LabelPosition.MiddleLeft    ;
      gisLabelPositionMiddleCenter  = TGIS_LabelPosition.MiddleCenter  ;
      gisLabelPositionMiddleRight   = TGIS_LabelPosition.MiddleRight   ;
      gisLabelPositionDownLeft      = TGIS_LabelPosition.DownLeft      ;
      gisLabelPositionDownCenter    = TGIS_LabelPosition.DownCenter    ;
      gisLabelPositionDownRight     = TGIS_LabelPosition.DownRight     ;
      gisLabelPositionFlow          = TGIS_LabelPosition.Flow          ;
   {$ENDIF}

type

  /// <summary>
  ///   Set for label positions.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      TGIS_LabelPositions = public set of TGIS_LabelPosition ;
    {$ELSE}
      TGIS_LabelPositions = public TGIS_LabelPosition ;
    {$ENDIF}
  {$ELSE}
    TGIS_LabelPositions = set of TGIS_LabelPosition ;
  {$ENDIF}

type

  /// <summary>
  ///   Position of offset.
  /// </summary>
  TGIS_OffsetPosition = {$IFDEF OXYGENE} public {$ENDIF} (
      /// <summary>
      ///   up-left to the original position.
      /// </summary>
      UpLeft        ,

      /// <summary>
      ///   Up-right to the original position.
      /// </summary>
      UpRight       ,

      /// <summary>
      ///   down-left to the original position.
      /// </summary>
      DownLeft      ,

      /// <summary>
      ///   down-right to the original position.
      /// </summary>
      DownRight
  ) ;

type

  /// <summary>
  ///   Types of shape locking.
  /// </summary>
  TGIS_Lock = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Unlocked.
      /// </summary>
      None,

      /// <summary>
      ///   Extent locked.
      /// </summary>
      Extent,

      /// <summary>
      ///   Extent locked and projections turn off.
      /// </summary>
      Projection,

      /// <summary>
      ///   Only for internal use.
      /// </summary>
      Internal,

      /// <summary>
      ///   Only for internal use.
      /// </summary>
      Internal2 // used to mark shapes as converted to screen units
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisLockNone        = TGIS_Lock.None       ;
      gisLockExtent      = TGIS_Lock.Extent     ;
      gisLockProjection  = TGIS_Lock.Projection ;
      gisLockInternal    = TGIS_Lock.Internal   ;
      gisLockInternal2   = TGIS_Lock.Internal2  ;
  {$ENDIF}

type

  /// <summary>
  ///   Type of operation for combining shapes.
  /// </summary>
  TGIS_TopologyCombineType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   ShapeA + ShapeB.
      /// </summary>
      Union,

      /// <summary>
      ///   ShapeA and ShapeB.
      /// </summary>
      Intersection,

      /// <summary>
      ///   ShapeA - ShapeB
      /// </summary>
      Difference,

      /// <summary>
      ///   ShapeA xor ShapeB.
      /// </summary>
      SymmetricalDifference
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisTopologyCombineTypeUnion
        = TGIS_TopologyCombineType.Union ;
      gisTopologyCombineTypeIntersection
        = TGIS_TopologyCombineType.Intersection ;
      gisTopologyCombineTypeDifference
        = TGIS_TopologyCombineType.Difference ;
      gisTopologyCombineTypeSymmetricalDifference
        = TGIS_TopologyCombineType.SymmetricalDifference ;
  {$ENDIF}

type

  /// <summary>
  ///   Types of IsInside checking.
  /// </summary>
  TGIS_InsideType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Shape must be totally covered.
      /// </summary>
      Full,

      /// <summary>
      ///   Only centroid must be covered.
      /// </summary>
      Centroid,

      /// <summary>
      ///   Any part must be covered.
      /// </summary>
      Partial
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisInsideTypeFull      = TGIS_InsideType.Full     ;
      gisInsideTypeCentroid  = TGIS_InsideType.Centroid ;
      gisInsideTypePartial   = TGIS_InsideType.Partial  ;
  {$ENDIF}

type

  /// <summary>
  ///   Style of markers.
  /// </summary>
  TGIS_MarkerStyle = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Marker is box.
      /// </summary>
      Box,

      /// <summary>
      ///   Marker is circle.
      /// </summary>
      Circle,

      /// <summary>
      ///   Marker is cross.
      /// </summary>
      Cross,

      /// <summary>
      ///   Marker is box.
      /// </summary>
      DiagCross,

      /// <summary>
      ///   Marker is triangle headed up.
      /// </summary>
      TriangleUp,

      /// <summary>
      ///   Marker is triangle headed down.
      /// </summary>
      TriangleDown,

      /// <summary>
      ///   Marker is triangle headed left.
      /// </summary>
      TriangleLeft,

      /// <summary>
      ///   Marker is triangle headed right.
      /// </summary>
      TriangleRight
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisMarkerStyleBox           = TGIS_MarkerStyle.Box           ;
      gisMarkerStyleCircle        = TGIS_MarkerStyle.Circle        ;
      gisMarkerStyleCross         = TGIS_MarkerStyle.Cross         ;
      gisMarkerStyleDiagCross     = TGIS_MarkerStyle.DiagCross     ;
      gisMarkerStyleTriangleUp    = TGIS_MarkerStyle.TriangleUp    ;
      gisMarkerStyleTriangleDown  = TGIS_MarkerStyle.TriangleDown  ;
      gisMarkerStyleTriangleLeft  = TGIS_MarkerStyle.TriangleLeft  ;
      gisMarkerStyleTriangleRight = TGIS_MarkerStyle.TriangleRight ;
  {$ENDIF}

type

  /// <summary>
  ///   Style of charts.
  /// </summary>
  TGIS_ChartStyle = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Pie-chart.
      /// </summary>
      Pie,

      /// <summary>
      ///   Bar-chart.
      /// </summary>
      Bar
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisChartStylePie = TGIS_ChartStyle.Pie ;
      gisChartStyleBar = TGIS_ChartStyle.Bar ;
  {$ENDIF}

type

  /// <summary>
  ///   Content type types.
  /// </summary>
  TGIS_ContentType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown content.
    /// </summary>
    Unknown,
    /// <summary>
    ///   Gif image.
    /// </summary>
    Gif,
    /// <summary>
    ///   Jpg image.
    /// </summary>
    Jpg,
    /// <summary>
    ///   Png image.
    /// </summary>
    Png,
    /// <summary>
    ///   Png24 image.
    /// </summary>
    Png24,
    /// <summary>
    ///   Binary.
    /// </summary>
    Binary
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisContentTypeUnknown = TGIS_ContentType.Unknown ;
      gisContentTypeGif     = TGIS_ContentType.Gif     ;
      gisContentTypeJpg     = TGIS_ContentType.Jpg     ;
      gisContentTypePng     = TGIS_ContentType.Png     ;
      gisContentTypePng24   = TGIS_ContentType.Png24   ;
  {$ENDIF}

type

  /// <summary>
  ///   Simplified variant types. Used by to GetVariantType() to simplify
  ///   variant test by grouping similar types.
  /// </summary>
  TGIS_VariantType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   <empty/>
    /// </summary>
    Unknown,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Unsupported,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Nothing,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Int,
    /// <summary>
    ///   <empty/>
    /// </summary>
    UInt,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Int64,
    /// <summary>
    ///   <empty/>
    /// </summary>
    UInt64,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Float,
    /// <summary>
    ///   <empty/>
    /// </summary>
    Fixed,
    /// <summary>
    ///   <empty/>
    /// </summary>
    DateTime,
    /// <summary>
    ///   <empty/>
    /// </summary>
    &Boolean,
    /// <summary>
    ///   <empty/>
    /// </summary>
    &AnsiString,
    /// <summary>
    ///   <empty/>
    /// </summary>
    &WideString
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisVarUnknown      = TGIS_VariantType.Unknown     ;
      gisVarUnsupported  = TGIS_VariantType.Unsupported ;
      gisVarNothing      = TGIS_VariantType.Nothing     ;
      gisVarInt          = TGIS_VariantType.Int         ;
      gisVarUInt         = TGIS_VariantType.UInt        ;
      gisVarInt64        = TGIS_VariantType.Int64       ;
      gisVarUInt64       = TGIS_VariantType.UInt64      ;
      gisVarFloat        = TGIS_VariantType.Float       ;
      gisVarFixed        = TGIS_VariantType.Fixed       ;
      gisVarDateTime     = TGIS_VariantType.DateTime    ;
      gisVarBoolean      = TGIS_VariantType.Boolean     ;
      gisVarAnsiString   = TGIS_VariantType.AnsiString  ;
      gisVarWideString   = TGIS_VariantType.WideString  ;
  {$ENDIF}

type

  /// <summary>
  ///   Type of DK compilation - for internal use mainly.
  /// </summary>
  TGIS_DeveloperKernelTypes = {$IFDEF OXYGENE} public {$ENDIF}
  (
      /// <summary>
      ///   DK was compiled as Embarcadero Delphi/C++ Builder.
      /// </summary>
      DELPHI   ,

      /// <summary>
      ///   DK was compiled as ActiveX.
      /// </summary>
      ACTIVEX  ,

      /// <summary>
      ///   DK was compiled as CLR.NET
      /// </summary>
      NET      ,

      /// <summary>
      ///   DK was compiled as Java bytecode
      /// </summary>
      JAVA     ,

      /// <summary>
      ///   DK was compiled as a Server Application.
      /// </summary>
      SERVER
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;


  /// <summary>
  ///   Set for DK compilation.
  /// </summary>
  TGIS_DeveloperKernelTypesSet = {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_DeveloperKernelTypes ;

type
  /// <summary>
  ///   Array of Points for drawing purposes,
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_DrawBuf = {$IFDEF OXYGENE} public {$ENDIF} Array of TPoint ;
  {$ELSE}
    {#typehint:array:TPoint}
    TGIS_DrawBuf = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TPoint
          read  dummy
          write dummy;
     end ;
  {$ENDIF}

  /// <summary>
  ///   Array of TGIS_Points for drawing purposes,
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_DrawBufF = {$IFDEF OXYGENE} public {$ENDIF} Array of TGIS_Point ;
  {$ELSE}
    {#typehint:array:TGIS_Point}
    TGIS_DrawBufF = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_Point
          read  dummy
          write dummy;
     end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Single.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_SingleArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Single;
  {$ELSE}
    {#typehint:array:Single}
    TGIS_SingleArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Single
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Double.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_DoubleArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Double;
  {$ELSE}
    {#typehint:array:Double}
    TGIS_DoubleArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Double
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Cardinal.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_CardinalArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Cardinal;
  {$ELSE}
    {#typehint:array:Cardinal}
    TGIS_CardinalArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Cardinal
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Integer.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_IntegerArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Integer;
  {$ELSE}
    {#typehint:array:Integer}
    TGIS_IntegerArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Integer
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Byte.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_ByteArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Byte;
  {$ELSE}
    {#typehint:array:Byte}
    TGIS_ByteArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Byte
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Variant.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_VariantArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Variant;
  {$ELSE}
    {#typehint:array:Variant}
    TGIS_VariantArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Variant
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Chars.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_CharArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Char;
  {$ELSE}
    {#typehint:array:Char}
    TGIS_CharArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : Char
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of TGIS_Point3D
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_Point3DArray = {$IFDEF OXYGENE} public {$ENDIF} Array of TGIS_Point3D;
  {$ELSE}
    {#typehint:array:TGIS_Point3D}
    TGIS_Point3DArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_Point3D
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Array of Integer
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_IntegerGridArray = {$IFDEF OXYGENE} public {$ENDIF} Array of TGIS_IntegerArray ;
  {$ELSE}
    {#typehint:array:TGIS_IntegerArray}
    TGIS_IntegerGridArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_rows">
        ///   number of rows
        /// </param>
        /// <param name="_columns">
        ///   number of columns
        /// </param>
        procedure SetSize( const _rows    : Integer ;
                           const _columns : Integer
                         ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Rows : Integer
          read  dummy ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Columns : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_col">
        ///   column index
        /// </param>
        /// <param name="_col">
        ///   raw index
        /// </param>
        property Value[ const _row : Integer;
                        const _col : Integer
                      ] : Integer
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Array of Single (array of grid image lines-
  ///   point[row][column]).
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_GridArray = {$IFDEF OXYGENE} public {$ENDIF} Array of TGIS_SingleArray ;
  {$ELSE}
    {#typehint:array:TGIS_SingleArray}
    TGIS_GridArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_rows">
        ///   number of rows
        /// </param>
        /// <param name="_columns">
        ///   number of columns
        /// </param>
        procedure SetSize( const _rows    : Integer ;
                           const _columns : Integer
                         ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Rows : Integer
          read  dummy ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Columns : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_col">
        ///   column index
        /// </param>
        /// <param name="_col">
        ///   raw index
        /// </param>
        property Value[ const _row : Integer;
                        const _col : Integer
                      ] : Single
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of Uids
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_UidArray = {$IFDEF OXYGENE} public {$ENDIF} Array of TGIS_Uid ;
  {$ELSE}
    {#typehint:array:TGIS_Uid}
    TGIS_UidArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_Uid
          read  dummy
          write dummy;
    end ;
  {$ENDIF}


  {$IFNDEF GEN_ARRAY_WRAPPER}
      /// <summary>
      ///   Matrix type.
      /// </summary>
      {$IFDEF DCC}
        TGIS_Matrix3x3 = array[1..3, 1..3] of Double ;
      {$ENDIF}
      {$IFDEF CLR}
        TGIS_Matrix3x3 = public array[1..3, 1..3] of Double ;
      {$ENDIF}
      {$IFDEF JAVA}
        {$WARNING '### Verify if still required on current JAVA; IS IT on 0 based?'}
        TGIS_Matrix3x3 = public array[0..4] of array[0..4] of Double ;
      {$ENDIF}
      {$IFDEF COCOA}
        {$WARNING '### Verify if still required on current JAVA; IS IT on 0 based?'}
        TGIS_Matrix3x3 = public array[0..4] of array[0..4] of Double ;
      {$ENDIF}
      {$IFDEF ISLAND}
        {$WARNING '### Verify if still required on current JAVA; IS IT on 0 based?'}
        TGIS_Matrix3x3 = public array[0..4] of array[0..4] of Double ;
      {$ENDIF}
  {$ELSE}
    /// <summary>
    ///   Matrix type.
    /// </summary>
    {#typehint:array:TGIS_DoubleArray}
    TGIS_Matrix3x3 =
      record
        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_col">
        ///   index of the element
        /// </param>
        /// <param name="_row">
        ///   index of the element
        /// </param>
        property Value[ const _col : Integer; const _row : Integer ] : Double
          read  dummy
          write dummy;
     end ;
  {$ENDIF}


  /// <summary>
  ///   Complex number.
  /// </summary>
  TGIS_Complex = {$IFDEF OXYGENE} public {$ENDIF} record
     /// <summary>
     ///   real part.
     /// </summary>
     r : Double ;
     /// <summary>
     ///   imaginary part.
     /// </summary>
     i : Double ;
  end ;

  /// <summary>
  ///   Supported files types.
  /// </summary>
  TGIS_FileType = {$IFDEF OXYGENE} public flags {$ENDIF}
  (

      /// <summary>
      ///   All supported files.
      /// </summary>
      All      ,

      /// <summary>
      ///   Project files (.ttkproject, .ttkgp, etc.)
      /// </summary>
      Project  ,

      /// <summary>
      ///   Vector files (.shp etc.)
      /// </summary>
      Vector   ,

      /// <summary>
      ///   Pixel files (.bmp etc.)
      /// </summary>
      Pixel    ,

      /// <summary>
      ///   Grid files (.grd etc.)
      /// </summary>
      Grid,

      /// <summary>
      ///   Vector 3D layer (.ply etc.)
      /// </summary>
      Vector3D
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisFileTypeAll      = TGIS_FileType.All      ;
      gisFileTypeProject  = TGIS_FileType.Project  ;
      gisFileTypeVector   = TGIS_FileType.Vector   ;
      gisFileTypePixel    = TGIS_FileType.Pixel    ;
      gisFileTypeGrid     = TGIS_FileType.Grid     ;
  {$ENDIF}

type


  /// <summary>
  ///   Set for files types.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
    TGIS_FileTypes = public set of TGIS_FileType ;
    {$ELSE}
    TGIS_FileTypes = TGIS_FileType ;
    {$ENDIF}
  {$ELSE}
    TGIS_FileTypes = set of TGIS_FileType ;
  {$ENDIF}

type

  /// <summary>
  ///   Supported field types.
  /// </summary>
  TGIS_FieldType = {$IFDEF OXYGENE} public {$ENDIF} (
      /// <summary>
      ///   Field is a String.
      /// </summary>
      &String,

      /// <summary>
      ///   Field is a number.
      /// </summary>
      Number,

      /// <summary>
      ///   Field is a float.
      /// </summary>
      Float,

      /// <summary>
      ///   Field is a boolean.
      /// </summary>
      Boolean ,

      /// <summary>
      ///   Field is a date.
      /// </summary>
      Date
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisFieldTypeString   = TGIS_FieldType.String  ;
      gisFieldTypeNumber   = TGIS_FieldType.Number  ;
      gisFieldTypeFloat    = TGIS_FieldType.Float   ;
      gisFieldTypeBoolean  = TGIS_FieldType.Boolean ;
      gisFieldTypeDate     = TGIS_FieldType.Date    ;
  {$ENDIF}

type

  /// <summary>
  ///   Supported field flags.
  /// </summary>
  TGIS_FieldFlags = {$IFDEF OXYGENE} public {$ENDIF}
  (

      /// <summary>
      ///   Is field exportable.
      /// </summary>
      Exportable   ,

      /// <summary>
      ///   Is field saveable.
      /// </summary>
      Saveable     ,

      /// <summary>
      ///   Is field read only.
      /// </summary>
      &ReadOnly    ,

      /// <summary>
      ///   Is field visible.
      /// </summary>
      Visible      ,

      /// <summary>
      ///   Is field required.
      /// </summary>
      Required
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;


  /// <summary>
  ///   Set for field flags.
  /// </summary>
  TGIS_FieldFlagsSet =  {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_FieldFlags ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisFieldFlagExportable = TGIS_FieldFlags.Exportable ;
      gisFieldFlagSaveable   = TGIS_FieldFlags.Saveable   ;
      gisFieldFlagReadOnly   = TGIS_FieldFlags.ReadOnly   ;
      gisFieldFlagVisible    = TGIS_FieldFlags.Visible    ;
      gisFieldFlagRequired   = TGIS_FieldFlags.Required   ;
  {$ENDIF}

type

  /// <summary>
  ///   Layer save options.
  /// </summary>
  TGIS_LayerSaveOptions = {$IFDEF OXYGENE} public {$ENDIF}
  (

      /// <summary>
      ///   save to field attributes.
      /// </summary>
      ParamsAsAttributes   ,

      /// <summary>
      ///   save to a shape.
      /// </summary>
      ParamsWithShape      ,

      /// <summary>
      ///   text labels always as text.
      /// </summary>
      TextLabelsForce      ,

      /// <summary>
      ///   text labels as text if labels not empty.
      /// </summary>
      TextLabelsSmart      ,

      /// <summary>
      ///   text labels always as points/markers .
      /// </summary>
      TextLabelsAsPoints
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   Set of layer save options.
  /// </summary>
  TGIS_LayerSaveOptionsSet = {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_LayerSaveOptions ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisParamsAsAttributes = TGIS_LayerSaveOptions.ParamsAsAttributes  ;
      gisParamsWithShape    = TGIS_LayerSaveOptions.ParamsWithShape     ;
      gisTextLabelsForce    = TGIS_LayerSaveOptions.TextLabelsForce     ;
      gisTextLabelsSmart    = TGIS_LayerSaveOptions.TextLabelsSmart     ;
      gisTextLabelsAsPoints = TGIS_LayerSaveOptions.TextLabelsAsPoints  ;
  {$ENDIF}

type

  /// <summary>
  ///   Basic modes of TGIS_Viewer3D operations:
  /// </summary>
  TGIS_Viewer3DMode = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Mouse is in camera positioning mode (scene rotation).
      /// </summary>
      CameraPosition,

      /// <summary>
      ///   Mouse is in dragging mode.
      /// </summary>
      CameraXYZ,

      /// <summary>
      ///   Mouse is in dragging mode.
      /// </summary>
      CameraXY,

      /// <summary>
      ///   Mouse is in camera rotating mode.
      /// </summary>
      CameraRotation,

      /// <summary>
      ///   Mouse is in sun positioning mode.
      /// </summary>
      SunPosition,

      /// <summary>
      ///   Mouse is in zooming mode.
      /// </summary>
      Zoom,

      /// <summary>
      ///   Mouse is in select mode.
      /// </summary>
      Select
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gis3DModeCameraPosition = TGIS_Viewer3DMode.CameraPosition ;
      gis3DModeCameraXYZ      = TGIS_Viewer3DMode.CameraXYZ      ;
      gis3DModeCameraXY       = TGIS_Viewer3DMode.CameraXY       ;
      gis3DModeCameraRotatio  = TGIS_Viewer3DMode.CameraRotation ;
      gis3DModeSunPosition    = TGIS_Viewer3DMode.SunPosition    ;
      gis3DModeZoom           = TGIS_Viewer3DMode.Zoom           ;
      gis3DModeSelect         = TGIS_Viewer3DMode.Select         ;
  {$ENDIF}

type

  /// <summary>
  ///   3D interpretation mode for layer (for 3D mode only).
  /// </summary>
  TGIS_3DLayerType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Layer does not represent 3D content.
      /// </summary>
      Off,

      /// <summary>
      ///   Layer should be interpreted as terrain model.
      /// </summary>
      Dem,

      /// <summary>
      ///   Layer should be interpreted as 3D objects.
      /// </summary>
      Shapes
  ) ;
  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gis3DLayerTypeOff     = TGIS_3DLayerType.Off     ;
      gis3DLayerTypeDem     = TGIS_3DLayerType.Dem     ;
      gis3DLayerTypeShapes  = TGIS_3DLayerType.Shapes  ;
  {$ENDIF}

type

  /// <summary>
  ///   Z value relations enforcement (for 3D mode only).
  /// </summary>
  TGIS_3DGroundType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   3D objects should be presented as relative to 0.
      /// </summary>
      AboveZero,

      /// <summary>
      ///   3D objects should be offset by an actual terrain model value.
      /// </summary>
      AboveDem,

      /// <summary>
      ///   3D objects should be laid on terrain (Z values will be ignored).
      /// </summary>
      OnDem
  ) ;
  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gis3DGroundTypeAboveZero = TGIS_3DGroundType.AboveZero ;
      gis3DGroundTypeAboveDem  = TGIS_3DGroundType.AboveDem  ;
      gis3DGroundTypeOnDem     = TGIS_3DGroundType.OnDem     ;
  {$ENDIF}

type

  /// <summary>
  ///   Shape on DEM basement enforcement (for 3D mode only).
  /// </summary>
  TGIS_3DBasementType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   3D objects basement will take Z value from DEM.
      /// </summary>
      Off,

      /// <summary>
      ///   3D objects basement level will have Z equal to the
      ///   lowest value of object basement.
      /// </summary>
      Lowest
  ) ;
  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gis3DBasementTypeOff     = TGIS_3DBasementType.Off    ;
      gis3DBasementTypeLowest  = TGIS_3DBasementType.Lowest ;
  {$ENDIF}

type

  /// <summary>
  ///   Types of 3D Normalization.
  /// </summary>
  TGIS_3DNormalizationType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   No normalization.
      /// </summary>
      Off          ,

      /// <summary>
      ///   Normalization to max layer value.
      /// </summary>
      Max          ,

      /// <summary>
      ///   Normalization to layer range value.
      /// </summary>
      Range
  ) ;


  /// <summary>
  ///   Definition of layer interpretation.
  /// </summary>
  TGIS_LayerPixelInterpretation = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Image as recognized (pixel or grid)
    /// </summary>
    Default,

    /// <summary>
    ///   Forced to ARGB image
    /// </summary>
    Pixel,

    /// <summary>
    ///   Forced to grid (DEM) image.
    /// </summary>
    Grid
  );


type
  {$IFDEF OXYGENE}
     /// <summary>
     ///   Will be fired regularly during long-drawn operations.
     /// </summary>
     /// <param name="_sender">
     ///   event originator
     /// </param>
     /// <param name="_e">
     ///   event parameters
     /// </param>
    TGIS_BusyEvent = public procedure(
      _sender    : Object ;
      _e         : TGIS_BusyEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for BusyEvent.
    /// </summary>
    /// <param name="_sender">
    ///   event originator
    /// </param>
    /// <param name="_pos">
    ///   current position; -1 at the end of the run
    /// </param>
    /// <param name="_end">
    ///   max position; -1 at the end of the run
    /// </param>
    /// <param name="_abort">
    ///   if set to True inside message handler then an abort request.
    /// </param>
    /// <remarks>
    ///   Will be fired regularly during long-drawn operations.
    /// </remarks>
    TGIS_BusyEvent = procedure(
          _sender : TObject ;
          _pos    : Integer ;
          _end    : Integer ;
      var _abort  : Boolean
    ) of object ;
  {$ENDIF}

  /// <summary>
  ///   Callback for TemplateProducer.
  /// </summary>
  /// <param name="_token">
  ///   token text to replace.
  /// </param>
  TGIS_TemplateProducerCallBack = {$IFDEF OXYGENE} public {$ENDIF} function(
                                    const _token : String
                                  ) : String of object ;

   /// <summary>
   ///   Single type vector.
   /// </summary>
   TGIS_SingleVector = {$IFDEF OXYGENE} public {$ENDIF} record
     public
     /// <summary>
     ///   X value.
     /// </summary>
     X : Single ;
     /// <summary>
     ///   Y value.
     /// </summary>
     Y : Single ;
     /// <summary>
     ///   Z value.
     /// </summary>
     Z : Single ;
   end ;

   /// <summary>
   ///   Single type color.
   /// </summary>
   TGIS_SingleColor = {$IFDEF OXYGENE} public {$ENDIF} record
     public
     /// <summary>
     ///   Red value.
     /// </summary>
     r : Single ;
     /// <summary>
     ///   Green value.
     /// </summary>
     g : Single ;
     /// <summary>
     ///   Blue value.
     /// </summary>
     b : Single ;
     /// <summary>
     ///   Alpha value.
     /// </summary>
     a : Single ;
   end ;

   /// <summary>
   ///   Single type vector array.
   /// </summary>
   {$IFNDEF GEN_ARRAY_WRAPPER}
     TGIS_SingleVectorArray = array of TGIS_SingleVector ;
   {$ELSE}
     {#typehint:array:TGIS_SingleVector}
     TGIS_SingleVectorArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
       // XDK stub definition
       public
         /// <summary>
         ///   Set array size.
         /// </summary>
         /// <param name="_size">
         ///   array size
         /// </param>
         procedure SetLength( const _size : Integer ) ;

         /// <summary>
         ///   Array size.
         /// </summary>
         property Length : Integer
           read  dummy ;

         /// <summary>
         ///   Array element value.
         /// </summary>
         /// <param name="_idx">
         ///   index of the element
         /// </param>
         property Value[ const _idx : Integer ] : TGIS_SingleVector
           read  dummy
           write dummy;
      end ;
   {$ENDIF}

   /// <summary>
   ///   Single type color array.
   /// </summary>
   {$IFNDEF GEN_ARRAY_WRAPPER}
     TGIS_SingleColorArray = array of TGIS_SingleColor ;
   {$ELSE}
     {#typehint:array:TGIS_SingleColor}
     TGIS_SingleColorArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
       // XDK stub definition
       public
         /// <summary>
         ///   Set array size.
         /// </summary>
         /// <param name="_size">
         ///   array size
         /// </param>
         procedure SetLength( const _size : Integer ) ;

         /// <summary>
         ///   Array size.
         /// </summary>
         property Length : Integer
           read  dummy ;

         /// <summary>
         ///   Array element value.
         /// </summary>
         /// <param name="_idx">
         ///   index of the element
         /// </param>
         property Value[ const _idx : Integer ] : TGIS_SingleColor
           read  dummy
           write dummy;
      end ;
   {$ENDIF}

   /// <summary>
   ///   Material.
   /// </summary>
   TGIS_Material = {$IFDEF OXYGENE} public {$ENDIF} record
     public
     /// <summary>
     ///   Type of material compression.
     /// </summary>
     CompressionType  : TGIS_CompressionType ;

     /// <summary>
     ///   True if material has defined color.
     /// </summary>
     HasColor         : Boolean ;

     /// <summary>
     ///   Fill Color.
     /// </summary>
     Color            : TGIS_Color ;

     /// <summary>
     ///   True if material has defined edge color.
     /// </summary>
     HasEdgeColor     : Boolean ;

     /// <summary>
     ///   Edge color.
     /// </summary>
     EdgeColor        : TGIS_Color ;

     /// <summary>
     ///   True if material has defined edge width.
     /// </summary>
     HasEdgeWidth     : Boolean ;

     /// <summary>
     ///   Edge width [0-255].
     /// </summary>
     EdgeWidth        : Byte ;

     /// <summary>
     ///   True if material has defined transparency.
     /// </summary>
     HasTransparency  : Boolean ;

     /// <summary>
     ///   Percent transparency [0-100].
     /// </summary>
     Transparency     : Byte ;

     /// <summary>
     ///   True if material has defined shininess.
     /// </summary>
     HasShininess     : Boolean ;

     /// <summary>
     ///   Percent shininess [0-100].
     /// </summary>
     Shininess        : Byte ;

     /// <summary>
     ///   True if material has defined texture map.
     /// </summary>
     HasTextureMap    : Boolean ;

     /// <summary>
     ///   Texture bytes per pixel.
     /// </summary>
     Bpp              : Byte ;

     /// <summary>
     ///   Texture width.
     /// </summary>
     Width            : Integer ;

     /// <summary>
     ///   Texture height.
     /// </summary>
     Height           : Integer ;

     /// <summary>
     ///   Texture buffer size.
     /// </summary>
     Size             : Integer ;

     /// <summary>
     ///   Texture buffer.
     /// </summary>
     Buffer           : TBytes ;

     /// <summary>
     ///   True if material has shared texture.
     /// </summary>
     HasSharedTexture : Boolean ;

     /// <summary>
     ///   Index of shared texture map.
     /// </summary>
     SharedTextureIndex : Integer ;

     /// <summary>
     ///   True if CullBackFaces.
     /// </summary>
     HasCullBackFaces : Boolean ;

     /// <summary>
     ///   True if material has defined colors in MaterialColors.
     /// </summary>
     HasMaterialColor : Boolean ;

     /// <summary>
     ///  Material colors [index : type] :
     ///  0 : diffuse,
     ///  1 : ambient,
     ///  2 : specular,
     ///  3 : emissive,
     ///  4 : transparent,
     ///  5 : reflective
     /// </summary>
     MaterialColors : TGIS_SingleColorArray ;
   end ;

   /// <summary>
   ///   Part descriptor.
   /// </summary>
   TGIS_PartDescriptor = {$IFDEF OXYGENE} public {$ENDIF} record
    public

     /// <summary>
     ///   Type of part. Look at TGIS_PartType for values.
     /// </summary>
     PartType      : Integer ;

     /// <summary>
     ///   Level of detail.
     /// </summary>
     LevelOfDetail : Integer ;

     /// <summary>
     ///   Priority.
     /// </summary>
     Priority      : Integer ;

     /// <summary>
     ///   Material ID.
     /// </summary>
     Material      : Integer ;
   end ;

type

   /// <summary>
   ///   Http response.
   /// </summary>
   TGIS_HttpResponse = {$IFDEF OXYGENE} public {$ENDIF} record

     /// <summary>
     ///   Response status.
     /// </summary>
     Status        : Integer ;

     /// <summary>
     ///   Content length.
     /// </summary>
     ContentLength : Int64 ;

     /// <summary>
     ///   Content type.
     /// </summary>
     ContentType   : String ;

     /// <summary>
     ///   Content expires date in UTC.
     /// </summary>
     ContentExpires : TDateTime ;

     /// <summary>
     ///   Stream with data.
     /// </summary>
     Stream        : TStream  ;

     /// <summary>
     ///   All headers CRLF separated
     /// </summary>
     Headers : String ;
   end ;

  /// <summary>
  ///   Web utils.
  /// </summary>
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  TGIS_WebUtils = {$IFDEF OXYGENE} public {$ENDIF} class
    public

      /// <summary>
      ///   Fetch data from http address
      /// </summary>
      /// <param name="_url">
      ///   http address (or file path)
      /// </param>
      /// <param name="_strm">
      ///   stream to write returned data; if nil then stream will be
      ///   allocated
      /// </param>
      /// <param name="_onBusy">
      ///   busy event handle
      /// </param>
      /// <param name="_cache">
      ///   True if you caching
      /// </param>
      /// <param name="_timeout">
      ///   client time out in ms
      /// </param>
      /// <param name="_agent">
      ///   client agent
      /// </param>
      /// <param name="_referer">
      ///   referer address
      /// </param>
      /// <param name="_user">
      ///   user name required for authentication
      /// </param>
      /// <param name="_pass">
      ///   user password required for authentication
      /// </param>
      /// <param name="_decompress">
      ///   if true, then procedure will attmept to decompress downloaded file
      /// </param>
      /// <returns>
      ///   server response record defined as TGIS_HttpResponse
      /// </returns>
      class function HttpFetch    ( const _url        : String                ;
                                    const _strm       : TStream        = nil  ;
                                    const _onBusy     : TGIS_BusyEvent = nil  ;
                                    const _cache      : Boolean        = True ;
                                    const _timeout    : Integer        = 0    ;
                                    const _agent      : String         = ''   ;
                                    const _referer    : String         = ''   ;
                                    const _user       : String         = ''   ;
                                    const _pass       : String         = ''   ;
                                    const _decompress : Boolean        = True
                                  ) : TGIS_HttpResponse ;
  end ;

  /// <summary>
  ///   Alias for platform specific list.
  /// </summary>
  {$IFDEF OXYGENE}
    TGIS_List       =  public TListEx ;
  {$ELSE}
    TGIS_List       = TList   ;
  {$ENDIF}

  /// <summary>
  ///   Comparator function type used in sort.
  /// </summary>
  /// <param name="Item1">
  ///   first item to compare
  /// </param>
  /// <param name="Item2">
  ///   second item to compare
  /// </param>
  TGIS_ObjectListSortCompare = function (const Item1, Item2: TObject) : Integer ;

  /// <summary>
  ///   Alias for platform specific object list.
  /// </summary>
  {$IFDEF OXYGENE}
    TGIS_ObjectList = public TObjectList<TObject> ;
  {$ELSE}
    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    {#gendoc:hide:GENSCR}
    /// <summary>
    ///   Alias for platform specific object list.
    /// </summary>
    TGIS_ObjectList = class( TObjectList<TObject> )
      public
        {#gendoc:hide}
        /// <summary>
        ///  Standard constructor.
        /// </summary>
        /// <param name="_ownsobjects">
        ///   comparator function
        /// </param>
        /// <remarks>
        ///   Only to avoid C++Builder vs Delphi limitation in
        ///   generics support.
        /// </remarks>
        constructor Create ( _ownsobjects: Boolean ) ; overload;

        {#gendoc:hide}
        /// <summary>
        ///  Standard constructor.
        /// </summary>
        /// <remarks>
        ///   Provided only to avoid C++Builder vs Delphi limitation in
        ///   generics support.
        /// </remarks>
        constructor Create ; overload;

        {#gendoc:hide:GENXDK}
        /// <summary>
        ///   Sort a list.
        /// </summary>
        /// <param name="_compare">
        ///   comparator function
        /// </param>
        procedure  Sort  ( const _compare  : TGIS_ObjectListSortCompare  ) ;

        /// <summary>
        ///   Assign a list.
        /// </summary>
        /// <param name="_list">
        ///   list to assign
        /// </param>
        procedure  Assign( const _list     : TGIS_ObjectList             ) ;
    end ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Alias for string list.
  /// </summary>
  TGIS_Strings    = {$IFDEF OXYGENE} public {$ENDIF} TStrings ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Alias for string list.
  /// </summary>
  TGIS_StringList = {$IFDEF OXYGENE} public {$ENDIF} TStringList ;

type

  /// <summary>
  ///   Registered layer type.
  /// </summary>
  TGIS_RegisteredLayerType = {$IFDEF OXYGENE} public {$ENDIF}
  (
     /// <summary>
     ///   vector layer.
     /// </summary>
     Vector ,
     /// <summary>
     ///   pixel layer.
     /// </summary>
     Pixel  ,
     /// <summary>
     ///   grid layer.
     /// </summary>
     Grid   ,
     /// <summary>
     ///   project layer.
     /// </summary>
     Project,
     /// <summary>
     ///   Vector 3D layer.
     /// </summary>
     Vector3D
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisRegisteredLayerTypeVector  = TGIS_RegisteredLayerType.Vector  ;
      gisRegisteredLayerTypePixel   = TGIS_RegisteredLayerType.Pixel   ;
      gisRegisteredLayerTypeGrid    = TGIS_RegisteredLayerType.Grid    ;
      gisRegisteredLayerTypeProject = TGIS_RegisteredLayerType.Project ;
  {$ENDIF}


type

  {$REGION 'TGIS_LayerInfo'}
    /// <summary>
    ///   Class storing a basic layer information.
    /// </summary>
    TGIS_LayerInfo = {$IFDEF OXYGENE} public {$ENDIF} class
      public
        /// <summary>
        ///   Layer name.
        /// </summary>
        Name         : String ;
        /// <summary>
        ///   Layer caption
        /// </summary>
        Caption      : String ;
        /// <summary>
        ///   Layer type (vector or pixel).
        /// </summary>
        LayerType    : TGIS_RegisteredLayerType ;
        /// <summary>
        ///   Shape type of a layer, if is vector.
        /// </summary>
        ShapeType    : TGIS_ShapeType ;
        /// <summary>
        ///   Image type of a layer, if is pixel.
        /// </summary>
        PixelType    : TGIS_PixelSubFormat ;
        /// <summary>
        ///   Extended data object.
        /// </summary>
        ExtendedTag  : TObject ;
        /// <summary>
        ///   Number of vector features. if unknown then -1.
        /// </summary>
        FeatureCount : TGIS_Uid ;
      public
        /// <summary>
        ///   Constructor.
        /// </summary>
        /// <param name="_name">
        ///   Layer name.
        /// </param>
        /// <param name="_layerType">
        ///   Layer type.
        /// </param>
        /// <param name="_shapeType">
        ///   Shape type of a layer, if is vector.
        /// </param>
        constructor Create( const _name      : String ;
                            const _layerType : TGIS_RegisteredLayerType ;
                            const _shapeType : TGIS_ShapeType
                           ) ; overload ;

        /// <summary>
        ///   Constructor.
        /// </summary>
        /// <param name="_name">
        ///   Layer name.
        /// </param>
        /// <param name="_layerType">
        ///   Layer type.
        /// </param>
        /// <param name="_shapeType">
        ///   Shape type of a layer, if is vector.
        /// </param>
        /// <param name="_featureCount">
        ///   Number of features, if is vector.
        /// </param>
        constructor Create( const _name         : String ;
                            const _layerType    : TGIS_RegisteredLayerType ;
                            const _shapeType    : TGIS_ShapeType ;
                            const _featureCount : TGIS_Uid
                           ) ; overload ;
        /// <summary>
        ///   Constructor.
        /// </summary>
        /// <param name="_name">
        ///   Layer name.
        /// </param>
        /// <param name="_caption">
        ///   Layer caption.
        /// </param>
        /// <param name="_layerType">
        ///   Layer type.
        /// </param>
        /// <param name="_shapeType">
        ///   Shape type of a layer, if is vector.
        /// </param>
        /// <param name="_pixelType">
        ///   Image type of a layer, if is pixel.
        /// </param>
        /// <param name="_extended">
        ///   Extended data object.
        /// </param>
        constructor Create( const _name      : String ;
                            const _caption   : String ;
                            const _layerType : TGIS_RegisteredLayerType ;
                            const _shapeType : TGIS_ShapeType ;
                            const _pixelType : TGIS_PixelSubFormat ;
                            const _extended  : TObject
                           ) ; overload ;

        /// <summary>
        ///   Constructor.
        /// </summary>
        /// <param name="_name">
        ///   Layer name.
        /// </param>
        /// <param name="_caption">
        ///   Layer caption.
        /// </param>
        /// <param name="_layerType">
        ///   Layer type.
        /// </param>
        /// <param name="_shapeType">
        ///   Shape type of a layer, if is vector.
        /// </param>
        /// <param name="_pixelType">
        ///   Image type of a layer, if is pixel.
        /// </param>
        /// <param name="_extended">
        ///   Extended data object.
        /// </param>
        /// <param name="_featureCount">
        ///   Number of features, if is vector.
        /// </param>
        constructor Create( const _name         : String ;
                            const _caption      : String ;
                            const _layerType    : TGIS_RegisteredLayerType ;
                            const _shapeType    : TGIS_ShapeType ;
                            const _pixelType    : TGIS_PixelSubFormat ;
                            const _extended     : TObject ;
                            const _featureCount : TGIS_Uid
                           ) ; overload ;
    end ;
  {$ENDREGION}


  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   List of layer information objects.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_LayerInfoList = {$IFDEF OXYGENE} public
                           TList< TGIS_LayerInfo > ;
                         {$ELSE}
                           TObjectList< TGIS_LayerInfo > ;
                         {$ENDIF}
  {$ELSE}
    TGIS_LayerInfoList = class (
                           TObjectList< TGIS_LayerInfo >
                         ) ;
  {$ENDIF}


  {$IFDEF LEVEL_XE8_RTL}
    // for purely magical reasons (someone can call this a bug), XE8 and up
    // sometimes, but only sometimes, does not emit this line of code
    {$IFNDEF LEVEL_RX12_RTL}
      {$HPPEMIT END 'typedef System::Generics::Collections::TObjectList__1<TGIS_LayerInfo*> TGIS_LayerInfoList;'}
    {$ENDIF}
  {$ENDIF}

  /// <summary>
  ///   Pixel file capabilities.
  /// </summary>
  TGIS_LayerPixelSubFormat = {$IFDEF OXYGENE} public {$ENDIF}
                             {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    private // internal properties values
      /// <summary>
      ///   Format of pixel files (number of bits per pixel).
      /// </summary>
      FPixelFormat : TGIS_PixelFormat ;

      /// <summary>
      ///   True if file is grayscale.
      /// </summary>
      FGrayScale : Boolean ;

      /// <summary>
      ///   Sub-format used. For files capable to support more the one underlying
      ///   formats (like PixelsStore: PNG and JPEG).
      /// </summary>
      FSubFormat : TGIS_PixelSubFormat ;

      /// <summary>
      ///   Supported compression type.
      /// </summary>
      FCompression : TGIS_CompressionType ;

      /// <summary>
      ///   Compression level.
      /// </summary>
      FCompressionLevel : Integer ;

      {$IFDEF DCC}
        FDummy : Boolean  ; // bypass C++ Builder problems on record 4 Bytes long
      {$ENDIF}

    public
      /// <summary>
      ///   Format of pixel files (number of bits per pixel).
      /// </summary>
      property PixelFormat : TGIS_PixelFormat read FPixelFormat ;

      /// <summary>
      ///   True if file is grayscale.
      /// </summary>
      property GrayScale : Boolean read FGrayScale ;

      /// <summary>
      ///   Sub-format used. For files capable to support more the one underlying
      ///   formats (like PixelsStore: PNG and JPEG).
      /// </summary>
      property Subformat : TGIS_PixelSubFormat read FSubFormat ;

      /// <summary>
      ///   Supported compression type.
      /// </summary>
      property Compression : TGIS_CompressionType read FCompression ;

      /// <summary>
      ///   Compression level.
      /// </summary>
      property CompressionLevel : Integer read FCompressionLevel ;

    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_pixelformat">
      ///   Format of pixel files (number of bits per pixel).
      /// </param>
      /// <param name="_grayscale">
      ///   True if file is grayscale.
      /// </param>
      /// <param name="_subformat">
      ///   Sub-format used.
      /// </param>
      /// <param name="_compression">
      ///   Compression type.
      /// </param>
      /// <param name="_level">
      ///   Compression level.
      /// </param>
      constructor Create     ( const _pixelformat : TGIS_PixelFormat ;
                               const _grayscale   : Boolean      ;
                               const _subformat   : TGIS_PixelSubFormat      ;
                               const _compression : TGIS_CompressionType ;
                               const _level       : Integer
                             ) ;

      /// <summary>
      ///   Clone object.
      /// </summary>
      /// <returns>
      ///   New cloned object.
      /// </returns>
      function    CreateCopy : TGIS_LayerPixelSubFormat ;

      /// <summary>
      ///   Get text description.
      /// </summary>
      /// <returns>
      ///   Description.
      /// </returns>
      function    ToString : String ; {$IFDEF OXYGENE} override; {$ENDIF}


      {$IFNDEF GENDOC}
        class operator Equal   ( _value1, _value2 : TGIS_LayerPixelSubFormat ) : Boolean ;
        class operator NotEqual( _value1, _value2 : TGIS_LayerPixelSubFormat ) : Boolean ;
      {$ENDIF}

      /// <summary>
      ///   Get default subformat.
      /// </summary>
      /// <returns>
      ///   subformat.
      /// </returns>
      class function DefaultSubFormat : TGIS_LayerPixelSubFormat ; static ;

      {$IFDEF GIS_NORECORDS}
        function MakeCopy : TGIS_LayerPixelSubFormat;
      {$ENDIF}
  end ;

  {$IFNDEF GIS_NORECORDS}
  type
    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_LayerPixelSubFormat = TGIS_LayerPixelSubFormat;
  {$ELSE}
    /// <summary>
    ///   Helper to ensure record copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_LayerPixelSubFormat ( const _lpsf : TGIS_LayerPixelSubFormat ) : TGIS_LayerPixelSubFormat ;
  {$ENDIF}

  type
    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   List of pixel file capabilities.
    /// </summary>
    {$IFDEF OXYGENE}
      TGIS_LayerPixelSubFormatList = public TList<TGIS_LayerPixelSubFormat> ;
    {$ELSE}
      TGIS_LayerPixelSubFormatList = class( TList<TGIS_LayerPixelSubFormat> )
      end ;
    {$ENDIF}


  /// <summary>
  ///   Mode of multipass shape rendering. Used for nice street junction drawing.
  ///   Used by TGIS_Shape.Draw.
  /// </summary>
  TGIS_RendererMultipassMode = {$IFDEF OXYGENE} public {$ENDIF}
  (
     /// <summary>
     ///   Draw only line itself.
     /// </summary>
     Line      = $01,

     /// <summary>
     ///   Draw only outline .
     /// </summary>
     Outline   = $02,

     /// <summary>
     ///   Draw shape line single step.
     /// </summary>
     Single    = $03
  ) ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   A single update run context It could be a for example in a thread context
  /// </summary>
  TGIS_ViewerHelperRun = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    public
      /// <summary>
      ///   Perform final step in a thread safe context. For example replace a
      ///   cache bitmap with a new content
      /// </summary>
      /// <param name="_final">
      ///   if true then final synchronize should be performed; progressive mode
      ///   otherwise
      /// </param>
      procedure DoSynchronize( const _final : Boolean ) ; virtual ; abstract ;
  end ;

const
  // various constants

    // based on System.Math
    /// <summary>
    ///   The maximum amount by which two double values can differ.
    /// </summary>
    GIS_DOUBLE_RESOLUTION : Double = 1e-12 ;

    /// <summary>
    ///   The maximum amount by which two single values can differ.
    /// </summary>
    GIS_SINGLE_RESOLUTION : Single = 1e-7 ;

    /// <summary>
    ///   A maximum value for a double. Used as "far away" distance.
    /// </summary>
    GIS_MAX_DOUBLE = 1.7e308 ;

    /// <summary>
    ///   A half of the maximum value for a double.
    /// </summary>
    GIS_HALF_MAX_DOUBLE = GIS_MAX_DOUBLE / 2 ;

    /// <summary>
    ///   A maximum value for a single. Used as "far away" distance.
    /// </summary>
    GIS_MAX_SINGLE = 3.4e38 ;

    /// <summary>
    ///   A half of the maximum value for a single.
    /// </summary>
    GIS_HALF_MAX_SINGLE = 3.4e38 ;

    /// <summary>
    ///   A maximum value for Integer.
    /// </summary>
    {$IFDEF CLR}
      GIS_MAX_INTEGER = Integer.MaxValue ;
    {$ENDIF}
    {$IFDEF JAVA}
      GIS_MAX_INTEGER = Integer.MAX_VALUE ;
    {$ENDIF}
    {$IFDEF DCC}
      GIS_MAX_INTEGER = high( Integer ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      GIS_MAX_INTEGER = Integer.MaxValue ;
    {$ENDIF}

    /// <summary>
    ///   A minimum value for Integer.
    /// </summary>
    {$IFDEF CLR}
      GIS_MIN_INTEGER = Integer.MinValue ;
    {$ENDIF}
    {$IFDEF JAVA}
      GIS_MIN_INTEGER = Integer.MIN_VALUE ;
    {$ENDIF}
    {$IFDEF DCC}
      GIS_MIN_INTEGER = low( Integer ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      GIS_MIN_INTEGER = Integer.MinValue ;
    {$ENDIF}

    /// <summary>
    ///   A maximum value for Cardinal.
    /// </summary>
    {$IFDEF CLR}
      GIS_MAX_CARDINAL = Cardinal.MaxValue ;
    {$ENDIF}
    {$IFDEF JAVA}
      GIS_MAX_CARDINAL = Cardinal.MAX_VALUE ;
    {$ENDIF}
    {$IFDEF DCC}
      GIS_MAX_CARDINAL = high( Cardinal ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      GIS_MAX_CARDINAL = Cardinal.MaxValue ;
    {$ENDIF}

    /// <summary>
    ///   A maximum value for DWORD.
    /// </summary>
    {$IFDEF CLR}
      GIS_MAX_DWORD = DWORD.MaxValue ;
    {$ENDIF}
    {$IFDEF JAVA}
      GIS_MAX_DWORD = DWORD.MAX_VALUE ;
    {$ENDIF}
    {$IFDEF DCC}
      GIS_MAX_DWORD = high( DWORD ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      GIS_MAX_DWORD = DWORD.MaxValue ;
    {$ENDIF}

    /// <summary>
    ///   Define how often a busy.hourglass will be updated (read this as
    ///   "every GIS_PROGRESS_THRESHOLD shape".
    /// </summary>
    GIS_PROGRESS_TRESHOLD = 1000 ;

    /// <summary>
    ///   Define how often the algorithms raise busy events in ticks (miliseconds).
    /// </summary>
    GIS_PROGRESS_TICK_TRESHOLD = 200 ;

    /// <summary>
    ///   Define how often the algorithms raise busy events in percents.
    /// </summary>
    GIS_PROGRESS_PERCENTAGE_THRESHOLD = 0.1 ;

    /// <summary>
    ///   Default base value for logarithmic interpolation using InterpolateValue function.
    /// </summary>
    GIS_INTERPOLATE_BASE_LOG = 0.2 ;

    /// <summary>
    ///   Default base value for exponential interpolation using InterpolateValue function.
    /// </summary>
    GIS_INTERPOLATE_BASE_EXP = 10 ;

  // pixel/grid bands

    /// <summary>
    ///   Alpha channel of the pixel layer
    /// </summary>
    GIS_BAND_A : String      = 'A';

    /// <summary>
    ///   Red channel of the pixel layer
    /// </summary>
    GIS_BAND_R : String       = 'R' ;

    /// <summary>
    ///   Green channel of the pixel layer
    /// </summary>
    GIS_BAND_G : String       = 'G' ;

    /// <summary>
    ///   Blue channel of the pixel layer
    /// </summary>
    GIS_BAND_B : String       = 'B' ;

    /// <summary>
    ///   Hue level of the color in the HSL/HSV color model (pixel layer)
    /// </summary>
    GIS_BAND_H : String       = 'H' ;

    /// <summary>
    ///   Saturation level of the color in the HSL/HSV color model (pixel layer)
    /// </summary>
    GIS_BAND_S : String       = 'S' ;

    /// <summary>
    ///   Lightness level of the color in the HSL color model (pixel layer)
    /// </summary>
    GIS_BAND_L : String       = 'L' ;

    /// <summary>
    ///   Value level of the color in the HSV color model (pixel layer)
    /// </summary>
    GIS_BAND_V : String       = 'V' ;

    /// <summary>
    ///   Unknown content in the band
    /// </summary>
    GIS_BAND_U : String       = 'U' ;

    /// <summary>
    ///   Photo content in the band
    /// </summary>
    GIS_BAND_P : String       = 'P' ;

    /// <summary>
    ///   Dem content in the band
    /// </summary>
    GIS_BAND_D : String       = 'D' ;

    /// <summary>
    ///   Value of the grid layer
    /// </summary>
    GIS_BAND_GRID : String    = 'Value' ;

    /// <summary>
    ///   Default band of the grid layer
    /// </summary>
    GIS_BAND_DEFAULT : String = '0' ;

  // related to a display

    /// <summary>
    ///   Pixels to Twips multiplier (assuming 96 dpi)
    /// </summary>
    GIS_PIXELS_TO_TWIPS = 1440 div 96 ;

    /// <summary>
    ///   Space between the border of a window and the display area.
    /// </summary>
    GIS_GAP_SIZE = 1 ;

    /// <summary>
    ///   Default size of tracking points (6 pixels at 96ppi).
    /// </summary>
    GIS_TRACKING_POINT_SIZE = 6 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default minimal mouse move (5 pixels at 96ppi) to make editing
    ///   possible. To avoid accidental movements caused by "shaking hands".
    /// </summary>
    GIS_MIN_EDIT_MOVE = 5 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default minimal zoom size rectangle (5 pixels at 96ppi). To avoid
    ///   crazy zooming by "shaking hands".
    /// </summary>
    GIS_MIN_ZOOM_SIZE = 5 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default selection precision (5 pixels at 96ppi) of vertices upon
    ///   editing.
    /// </summary>
    GIS_SELECT_TOLERANCE = 5 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default selection precision (19 pixels at 96ppi) of vertices upon
    ///   editing for pen gesture.
    /// </summary>
    GIS_SELECT_TOLERANCE_PEN = 8 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default selection precision (19 pixels at 96ppi) of vertices upon
    ///   editing for touch gesture.
    /// </summary>
    GIS_SELECT_TOLERANCE_TOUCH = 19 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Default margin (15 pixels at 96ppi) below which points will be
    ///   snapped to existing shape during editing.
    /// </summary>
    GIS_SNAP_MARGIN = 15 * GIS_PIXELS_TO_TWIPS ;

    /// <summary>
    ///   Initial zoom value - crazy to avoid casual similarity.
    /// </summary>
    GIS_INITIAL_ZOOM = 1 + Pi / 1000 ;

    /// <summary>
    ///   Level precision - used to minimize Level-Scale-Level conversion error.
    /// </summary>
    GIS_LEVEL_PRECISION = 0.001 ;

  // related to configuration file (TGIS_Ini)

    /// <summary>
    ///   Maximum number of layers.
    /// </summary>
    GIS_MAX_LAYERS_CNT = 999999 ;

    /// <summary>
    ///   Maximum number of sections in configuration file.
    /// </summary>
    GIS_MAX_SECTION_CNT = 999999 ;

    /// <summary>
    ///   Maximum number of zones in configuration file.
    /// </summary>
    GIS_MAX_ZONE_CNT = 999999 ;

  // related to renderer

    /// <summary>
    ///   Value to be added to size parameters to indicate auto-scaling
    ///   in real units (metric etc).
    /// </summary>
    GIS_AUTOSIZE_SIZE = 1000000000 ;

    /// <summary>
    ///   Value to be added to size parameters to indicate auto-scaling
    ///   in map units.
    /// </summary>
    GIS_AUTOSIZE_SIZE_MU = 2000000000 ;

    /// <summary>
    ///   Size which indicates that renderer must be used.
    /// </summary>
    GIS_RENDER_SIZE = -$0FFFFFFF ;

  // related to grid

    /// <summary>
    ///   Unassigned value of grid.
    /// </summary>
    GIS_GRID_NOVALUE = -99999 ;

  // predefined fields

    /// <summary>
    ///   Predefine offset for joined fields.
    /// </summary>
    GIS_JOIN_FIELD_ID             = 10000 ;

    /// <summary>
    ///   Predefine field name for UID.
    /// </summary>
    GIS_FIELD_UID                 = 'GIS_UID' ;

    /// <summary>
    ///   Predefine field index for UID.
    /// </summary>
    GIS_FIELD_ID_UID              = 100000 ;

    /// <summary>
    ///   Predefine field name for SELECTED.
    /// </summary>
    GIS_FIELD_SELECTED            = 'GIS_SELECTED' ;

    /// <summary>
    ///   Predefine field index for SELECTED.
    /// </summary>
    GIS_FIELD_ID_SELECTED         = GIS_FIELD_ID_UID + 1 ;

    /// <summary>
    ///   Predefine field name for HIDDEN.
    /// </summary>
    GIS_FIELD_HIDDEN              = 'GIS_HIDDEN' ;

    /// <summary>
    ///   Predefine field index for HIDDEN.
    /// </summary>
    GIS_FIELD_ID_HIDDEN           = GIS_FIELD_ID_SELECTED + 1 ;

    /// <summary>
    ///   Predefine field name for AREA.
    /// </summary>
    GIS_FIELD_AREA                = 'GIS_AREA' ;

    /// <summary>
    ///   Predefine field index for AREA.
    /// </summary>
    GIS_FIELD_ID_AREA             = GIS_FIELD_ID_HIDDEN + 1 ;

    /// <summary>
    ///   Predefine field name for LENGTH.
    /// </summary>
    GIS_FIELD_LENGTH              = 'GIS_LENGTH' ;

    /// <summary>
    ///   Predefine field index for LENGTH.
    /// </summary>
    GIS_FIELD_ID_LENGTH           = GIS_FIELD_ID_AREA + 1 ;

    /// <summary>
    ///   Predefine field name for COORD_Z.
    /// </summary>
    GIS_FIELD_COORD_Z             = 'GIS_COORD_Z' ;

    /// <summary>
    ///   Predefine field index for COORD_Z.
    /// </summary>
    GIS_FIELD_ID_COORD_Z          = GIS_FIELD_ID_LENGTH + 1 ;

    /// <summary>
    ///   Predefine field name for COORD_M.
    /// </summary>
    GIS_FIELD_COORD_M             = 'GIS_COORD_M' ;

    /// <summary>
    ///   Predefine field index for COORD_M.
    /// </summary>
    GIS_FIELD_ID_COORD_M          = GIS_FIELD_ID_COORD_Z + 1 ;

    /// <summary>
    ///   Predefine field name for NOW.
    /// </summary>
    GIS_FIELD_NOW                 = 'GIS_NOW' ;

    /// <summary>
    ///   Predefine field index for NOW.
    /// </summary>
    GIS_FIELD_ID_NOW              = GIS_FIELD_ID_COORD_M + 1;

    /// <summary>
    ///   Predefine field name for MIN_X.
    /// </summary>
    GIS_FIELD_MIN_X               = 'GIS_MIN_X' ;

    /// <summary>
    ///   Predefine field index for MIN_X.
    /// </summary>
    GIS_FIELD_ID_MIN_X            = GIS_FIELD_ID_NOW + 1 ;

    /// <summary>
    ///   Predefine field name for MIN_Y.
    /// </summary>
    GIS_FIELD_MIN_Y               = 'GIS_MIN_Y' ;

    /// <summary>
    ///   Predefine field index for MIN_X.
    /// </summary>
    GIS_FIELD_ID_MIN_Y            = GIS_FIELD_ID_MIN_X + 1 ;

    /// <summary>
    ///   Predefine field name for MIN_Z.
    /// </summary>
    GIS_FIELD_MIN_Z               = 'GIS_MIN_Z' ;

    /// <summary>
    ///   Predefine field index for MIN_Z.
    /// </summary>
    GIS_FIELD_ID_MIN_Z            = GIS_FIELD_ID_MIN_Y + 1 ;

    /// <summary>
    ///   Predefine field name for MIN_M.
    /// </summary>
    GIS_FIELD_MIN_M               = 'GIS_MIN_M' ;

    /// <summary>
    ///   Predefine field index for MIN_M.
    /// </summary>
    GIS_FIELD_ID_MIN_M            = GIS_FIELD_ID_MIN_Z + 1 ;

    /// <summary>
    ///   Predefine field name for MAX_X.
    /// </summary>
    GIS_FIELD_MAX_X               = 'GIS_MAX_X' ;

    /// <summary>
    ///   Predefine field index for MAX_X.
    /// </summary>
    GIS_FIELD_ID_MAX_X            = GIS_FIELD_ID_MIN_M + 1 ;

    /// <summary>
    ///   Predefine field name for MAX_Y.
    /// </summary>
    GIS_FIELD_MAX_Y               = 'GIS_MAX_Y' ;

    /// <summary>
    ///   Predefine field index for MAX_Y.
    /// </summary>
    GIS_FIELD_ID_MAX_Y            = GIS_FIELD_ID_MAX_X + 1 ;

    /// <summary>
    ///   Predefine field name for MAX_Z.
    /// </summary>
    GIS_FIELD_MAX_Z               = 'GIS_MAX_Z' ;

    /// <summary>
    ///   Predefine field index for MAX_Z.
    /// </summary>
    GIS_FIELD_ID_MAX_Z            = GIS_FIELD_ID_MAX_Y + 1 ;

    /// <summary>
    ///   Predefine field name for MAX_M.
    /// </summary>
    GIS_FIELD_MAX_M               = 'GIS_MAX_M' ;

    /// <summary>
    ///   Predefine field index for MAX_M.
    /// </summary>
    GIS_FIELD_ID_MAX_M            = GIS_FIELD_ID_MAX_Z + 1 ;

    /// <summary>
    ///   Predefine field name for CENTER_X.
    /// </summary>
    GIS_FIELD_CENTER_X            = 'GIS_CENTER_X' ;

    /// <summary>
    ///   Predefine field index for CENTER_X.
    /// </summary>
    GIS_FIELD_ID_CENTER_X         = GIS_FIELD_ID_MAX_M + 1 ;

    /// <summary>
    ///   Predefine field name for CENTER_Y.
    /// </summary>
    GIS_FIELD_CENTER_Y            = 'GIS_CENTER_Y' ;

    /// <summary>
    ///   Predefine field index for CENTER_Y.
    /// </summary>
    GIS_FIELD_ID_CENTER_Y         = GIS_FIELD_ID_CENTER_X + 1 ;

    /// <summary>
    ///   Predefine field name for CENTER_Z.
    /// </summary>
    GIS_FIELD_CENTER_Z            = 'GIS_CENTER_Z' ;

    /// <summary>
    ///   Predefine field index for CENTER_Z.
    /// </summary>
    GIS_FIELD_ID_CENTER_Z         = GIS_FIELD_ID_CENTER_Y + 1 ;

    /// <summary>
    ///   Predefine field name for CENTER_M.
    /// </summary>
    GIS_FIELD_CENTER_M            = 'GIS_CENTER_M' ;

    /// <summary>
    ///   Predefine field index for CENTER_M.
    /// </summary>
    GIS_FIELD_ID_CENTER_M         = GIS_FIELD_ID_CENTER_Z + 1 ;

    /// <summary>
    ///   Predefine field name for CENTROID_X.
    /// </summary>
    GIS_FIELD_CENTROID_X          = 'GIS_CENTROID_X' ;

    /// <summary>
    ///   Predefine field index for CENTROID_X.
    /// </summary>
    GIS_FIELD_ID_CENTROID_X       = GIS_FIELD_ID_CENTER_M + 1 ;

    /// <summary>
    ///   Predefine field name for CENTROID_Y.
    /// </summary>
    GIS_FIELD_CENTROID_Y          = 'GIS_CENTROID_Y' ;

    /// <summary>
    ///   Predefine field index for CENTROID_Y.
    /// </summary>
    GIS_FIELD_ID_CENTROID_Y       = GIS_FIELD_ID_CENTROID_X + 1 ;

    /// <summary>
    ///   Predefine field name for NUM_PARTS.
    /// </summary>
    GIS_FIELD_NUM_PARTS           = 'GIS_NUM_PARTS' ;

    /// <summary>
    ///   Predefine field index for NUM_PARTS.
    /// </summary>
    GIS_FIELD_ID_NUM_PARTS        = GIS_FIELD_ID_CENTROID_Y + 1 ;

    /// <summary>
    ///   Predefine field name for NUM_POINTS.
    /// </summary>
    GIS_FIELD_NUM_POINTS          = 'GIS_NUM_POINTS' ;

    /// <summary>
    ///   Predefine field index for NUM_POINTS.
    /// </summary>
    GIS_FIELD_ID_NUM_POINTS       = GIS_FIELD_ID_NUM_PARTS + 1 ;

    /// <summary>
    ///   Predefine field name for AGGREGATED_COUNT.
    /// </summary>
    GIS_FIELD_AGGREGATED_COUNT    = 'GIS_AGGREGATED_COUNT' ;

    /// <summary>
    ///   Predefine field index for AGGRGATED_COUINT.
    /// </summary>
    GIS_FIELD_ID_AGGREGATED_COUNT = GIS_FIELD_ID_NUM_POINTS + 1 ;

    /// <summary>
    ///   Predefine field name for AGGREGATED_VALUE.
    /// </summary>
    GIS_FIELD_AGGREGATED_VALUE    = 'GIS_AGGREGATED_VALUE' ;

    /// <summary>
    ///   Predefine field index for AGGRGATED_VALUE.
    /// </summary>
    GIS_FIELD_ID_AGGREGATED_VALUE = GIS_FIELD_ID_AGGREGATED_COUNT + 1 ;

    /// <summary>
    ///   Predefine field name for AGGREGATED_UIDS.
    /// </summary>
    GIS_FIELD_AGGREGATED_UIDS     = 'GIS_AGGREGATED_UIDS' ;

    /// <summary>
    ///   Predefine field index for AGGRGATED_VALUE.
    /// </summary>
    GIS_FIELD_ID_AGGREGATED_UIDS  = GIS_FIELD_ID_AGGREGATED_VALUE + 1 ;

    /// <summary>
    ///   Predefine field name for SHAPE_TYPE.
    /// </summary>
    GIS_FIELD_SHAPE_TYPE          = 'GIS_SHAPE_TYPE' ;

    /// <summary>
    ///   Predefine field index for SHAPE_TYPE.
    /// </summary>
    GIS_FIELD_ID_SHAPE_TYPE       = GIS_FIELD_ID_AGGREGATED_UIDS + 1 ;

    /// <summary>
    ///   Predefine field name for VIEWER_SCALE.
    /// </summary>
    GIS_FIELD_VIEWER_SCALE        = 'GIS_VIEWER_SCALE' ;

    /// <summary>
    ///   Predefine field index for VIEWER_SCALE.
    /// </summary>
    GIS_FIELD_ID_VIEWER_SCALE     = GIS_FIELD_ID_SHAPE_TYPE + 1 ;

    /// <summary>
    ///   Predefine field name for VIEWER_LEVEL.
    /// </summary>
    GIS_FIELD_VIEWER_LEVEL        = 'GIS_VIEWER_LEVEL' ;

    /// <summary>
    ///   Predefine field index for VIEWER_LEVEL.
    /// </summary>
    GIS_FIELD_ID_VIEWER_LEVEL     = GIS_FIELD_ID_VIEWER_SCALE + 1 ;

    /// <summary>
    ///   Predefine field name for STYLE.
    /// </summary>
    GIS_FIELD_STYLE               = 'GIS_STYLE' ;

    /// <summary>
    ///   Dummy field for layer that requires at least one column.
    /// </summary>
    GIS_FIELD_DUMMY               = '_DUMMY' ;

    /// <summary>
    ///   All predefined fields.
    /// </summary>
    GIS_FIELDS_PREDEFINED = GIS_FIELD_UID              + #13#10 +
                            GIS_FIELD_SELECTED         + #13#10 +
                            GIS_FIELD_AREA             + #13#10 +
                            GIS_FIELD_LENGTH           + #13#10 +
                            GIS_FIELD_COORD_Z          + #13#10 +
                            GIS_FIELD_COORD_M          + #13#10 +
                            GIS_FIELD_NOW              + #13#10 +
                            GIS_FIELD_MIN_X            + #13#10 +
                            GIS_FIELD_MIN_Y            + #13#10 +
                            GIS_FIELD_MIN_Z            + #13#10 +
                            GIS_FIELD_MIN_M            + #13#10 +
                            GIS_FIELD_MAX_X            + #13#10 +
                            GIS_FIELD_MAX_Y            + #13#10 +
                            GIS_FIELD_MAX_Z            + #13#10 +
                            GIS_FIELD_MAX_M            + #13#10 +
                            GIS_FIELD_CENTER_X         + #13#10 +
                            GIS_FIELD_CENTER_Y         + #13#10 +
                            GIS_FIELD_CENTER_Z         + #13#10 +
                            GIS_FIELD_CENTER_M         + #13#10 +
                            GIS_FIELD_CENTROID_X       + #13#10 +
                            GIS_FIELD_CENTROID_Y       + #13#10 +
                            GIS_FIELD_NUM_PARTS        + #13#10 +
                            GIS_FIELD_NUM_POINTS       + #13#10 +
                            GIS_FIELD_AGGREGATED_COUNT + #13#10 +
                            GIS_FIELD_AGGREGATED_VALUE + #13#10 +
                            GIS_FIELD_SHAPE_TYPE       + #13#10 +
                            GIS_FIELD_VIEWER_SCALE     + #13#10 +
                            GIS_FIELD_VIEWER_LEVEL ;

  // common database defines

    /// <summary>
    ///   Maximum size of Oracle Clob.
    /// </summary>
    GIS_SQL_ORACLOB_SIZE  = 4000 ;

    /// <summary>
    ///   Default size of memo fields.
    /// </summary>
    GIS_SQL_MEMO_SIZE     = 1999 ;

  // Coordinate system definitions

    /// <summary>
    ///   Starting EPSG number for auto-generated codes.
    /// </summary>
    GIS_EPSG_AUTO           = 7000000 ;

    /// <summary>
    ///   Default WGS85 EPSG mapping.
    /// </summary>
    GIS_EPSG_WGS84          = 4326 ;

    /// <summary>
    ///   Default WGS72 EPSG mapping.
    /// </summary>
    GIS_EPSG_WGS72          = 4322 ;

  // portability mapping

    /// <summary>
    ///   Index of first character in string.
    /// </summary>
    {$IFDEF CLR}
      PORTABLE_FIRSTCHAR = 0 ;
    {$ELSE}
      PORTABLE_FIRSTCHAR = 1 ;
    {$ENDIF}

 // Database provider indicators

    /// <summary>
    ///   Ado OleDb provider.
    /// </summary>
    GIS_SQL_PROVIDER_ADO     = 1 ;

    /// <summary>
    ///   DbExpress provider.
    /// </summary>
    GIS_SQL_PROVIDER_DBX     = 2 ;

    /// <summary>
    ///   Sqlite provider.
    /// </summary>
    GIS_SQL_PROVIDER_SQLITE  = 3 ;

    /// <summary>
    ///   ADO.NET provider.
    /// </summary>
    GIS_SQL_PROVIDER_ADONET  = 4 ;

    /// <summary>
    ///   Oracle OCI provider.
    /// </summary>
    GIS_SQL_PROVIDER_OCI     = 5 ;

    /// <summary>
    ///   PostgreSQL provider.
    /// </summary>

    GIS_SQL_PROVIDER_LIBPQ   = 6 ;

    /// <summary>
    ///   JDBC provider.
    /// </summary>
    GIS_SQL_PROVIDER_JDBC    = 7 ;

    /// <summary>
    ///   FireDAC provider.
    /// </summary>
    GIS_SQL_PROVIDER_FIREDAC = 8 ;

 // Subscription events

    /// <summary>
    ///   Subscribed "Controller Destroy" event.
    /// </summary>
    GIS_SUBSCRIBED_DESTROY            = 0 ;

    /// <summary>
    ///   Subscribed "After Paint" event.
    /// </summary>
    GIS_SUBSCRIBED_AFTERPAINT         = 1 ;

    /// <summary>
    ///   Subscribed "Project Close" event.
    /// </summary>
    GIS_SUBSCRIBED_PROJECT_CLOSE      = 4 ;

    /// <summary>
    ///   Subscribed "3D View update" event.
    /// </summary>
    GIS_SUBSCRIBED_3D_UPDATE          = 5 ;

    /// <summary>
    ///   Subscribed control paint for transparent controls like
    ///   TGIS_ControlNorthArrow. Paint means shallow redraw upon map painting
    ///   form cache
    /// </summary>
    GIS_SUBSCRIBED_TRANSPARENT_CONTROL_PAINT = 6 ;

    /// <summary>
    ///   Subscribed control update for transparent controls like
    ///   TGIS_ControlNorthArrow. Update means a deep redraw upon full map
    ///    update.
    /// </summary>
    GIS_SUBSCRIBED_TRANSPARENT_CONTROL_UPDATE = 7 ;

    /// <summary>
    ///   Subscribed touch event.
    ///   Used only for FMX for Windows.
    /// </summary>
    GIS_SUBSCRIBED_TOUCH              = 8 ;

  // Internet code

    /// <summary>
    ///   FETCH_UNKNOWN code.
    /// </summary>
    GIS_HTTP_FETCH_UNKNOWN          = 0   ;
    /// <summary>
    ///   FETCH_INTERNALERROR code.
    /// </summary>
    GIS_HTTP_FETCH_INTERNALERROR    = 1   ;
    /// <summary>
    ///   FETCH_TIMEOUT code.
    /// </summary>
    GIS_HTTP_FETCH_TIMEOUT          = 2   ;
    /// <summary>
    ///   FETCH_NOTCOMPLETED code.
    /// </summary>
    GIS_HTTP_FETCH_NOTCOMPLETED     = 3   ;
    /// <summary>
    ///   OK code.
    /// </summary>
    GIS_HTTP_OK                     = 200 ;
    /// <summary>
    ///   AUTHORIZATIONREQUIRED code.
    /// </summary>
    GIS_HTTP_AUTHORIZATIONREQUIRED  = 401 ;
    /// <summary>
    ///   NOTFOUND code.
    /// </summary>
    GIS_HTTP_NOTFOUND               = 404 ;
    /// <summary>
    ///   SERVICEUNAVAILABLE code.
    /// </summary>
    GIS_HTTP_SERVICEUNAVAILABLE     = 503 ;
    /// <summary>
    ///   GATEWAYTIMEOUT code.
    /// </summary>
    GIS_HTTP_GATEWAYTIMEOUT         = 504 ;

  // Content Type code
    /// <summary>
    ///   JPEG image.
    /// </summary>
    GIS_CONTENTTYPE_JPEG          = 'image/jpeg'            ;
    /// <summary>
    ///   JPEG image.
    /// </summary>
    GIS_CONTENTTYPE_JPG           = 'image/jpg'             ;
    /// <summary>
    ///   PNG image.
    /// </summary>
    GIS_CONTENTTYPE_PNG           = 'image/png'             ;
    /// <summary>
    ///   PNG24 image.
    /// </summary>
    GIS_CONTENTTYPE_PNG24         = 'image/png; mode=24bit' ;
    /// <summary>
    ///   GIF image.
    /// </summary>
    GIS_CONTENTTYPE_GIF           = 'image/gif'             ;
    /// <summary>
    ///   Binary data.
    /// </summary>
    GIS_CONTENTTYPE_BINARY        = 'octet-stream'   ;
    /// <summary>
    ///   Binary data.
    /// </summary>
    GIS_CONTENTTYPE_BINARY2       = 'x-protobuf'   ;
    /// <summary>
    ///   Binary data.
    /// </summary>
    GIS_CONTENTTYPE_BINARY3       = 'mapbox-vector-tile'   ;

  {$IFDEF OXYGENE}
    NaN   =  0.0 / 0.0 ;
  {$ENDIF}

  {#gendoc:hide}
  HALFPI  = Pi/2;
  {#gendoc:hide}
  FORTPI  = Pi/4;
  {#gendoc:hide}
  THIRPI  = Pi/3;
  {#gendoc:hide}
  TWOPI   = 2*Pi;
  {#gendoc:hide}
  HALFPI2 = HALFPI * HALFPI ;

  {#gendoc:hide}
  DEGREE_SIGN = #176 ;

  { Predefined sizeOf(X) for different GIS types. }
  {#gendoc:hide}
  {$IFDEF OXYGENE}
    SIZEOF_TGIS_POINT = 2 * sizeOf( Double )  ;
  {$ELSE}
    SIZEOF_TGIS_POINT = sizeOf(TGIS_Point) ;
  {$ENDIF}
  {#gendoc:hide}
  {$IFDEF OXYGENE}
    SIZEOF_TGIS_POINT3D = 4 * sizeOf( Double )  ;
  {$ELSE}
    SIZEOF_TGIS_POINT3D = sizeOf(TGIS_Point3D) ;
  {$ENDIF}
  {#gendoc:hide}
  {$IFDEF OXYGENE}
    SIZEOF_TGIS_EXTENT = 4 * sizeOf( Double )  ;
  {$ELSE}
    SIZEOF_TGIS_EXTENT = sizeOf(TGIS_Extent) ;
  {$ENDIF}

  { Number of internal/reserved cursors. }
  {#gendoc:hide}
  BUILTIN_CURSORS = 2 ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCompression ;
{$ENDIF}

//=============================================================================
// TGIS_ObjectList
//=============================================================================

{$IFNDEF OXYGENE}

  constructor TGIS_ObjectList.Create(
    _ownsobjects: Boolean
  );
  begin
    inherited Create( _ownsObjects ) ;
  end ;

  constructor TGIS_ObjectList.Create;
  begin
    inherited Create;
  end ;

  procedure TGIS_ObjectList.Sort(
    const _compare : TGIS_ObjectListSortCompare
  ) ;
  begin
    inherited Sort( TComparer<TObject>.Construct( _compare ) ) ;
  end;

  procedure TGIS_ObjectList.Assign(
    const _list : TGIS_ObjectList
  ) ;
  {$IFDEF DCC}
    var
      itm : TObject ;
  {$ENDIF}
  begin
    assert( assigned( _list ) ) ;
    Clear ;
    for itm in _list do
      Add( itm ) ;
  end;

{$ENDIF}

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_Point
//=============================================================================

  {$IFDEF GIS_NORECORDS}
    constructor TGIS_Point.Create ;
    begin
      inherited Create ;
    end ;
  {$ENDIF}

  constructor TGIS_Point.Create(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
  end ;

  constructor TGIS_Point.Create(
    const _x : Double ;
    const _y : Double
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_Point.MakeCopy : TGIS_Point ;
    begin
      Result := new TGIS_Point( X, Y ) ;
    end ;
  {$ENDIF}

  function TGIS_Point.ToString : String ;
  begin
    Result := Format( 'X = %s Y = %s', [ X.ToString, Y.ToString ] );
  end ;

{$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  {$ELSE}
    function _TGIS_Point( const _pt : TGIS_Point ) : TGIS_Point ;
    begin
      Result := _pt.MakeCopy() ;
    end ;
  {$ENDIF}

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_Point3DInt
//=============================================================================

  {$IFDEF JAVA}
    constructor TGIS_Point3DInt.Create ;
    begin
      inherited Create ;
    end ;
  {$ENDIF}

  constructor TGIS_Point3DInt.Create(
    const _x : Integer ;
    const _y : Integer ;
    const _z : Integer
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
    Z := _z ;
  end ;
{$ENDIF}

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_Point3D
//=============================================================================

  {$IFDEF GIS_NORECORDS}
    constructor TGIS_Point3D.Create ;
    begin
      inherited Create ;
    end ;
  {$ENDIF}

  constructor TGIS_Point3D.Create(
    const _x : Integer ;
    const _y : Integer
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
    Z :=  0 ;
    M :=  0 ;
  end ;

  constructor TGIS_Point3D.Create(
    const _x : Integer ;
    const _y : Integer ;
    const _z : Integer
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
    Z := _z ;
    M :=  0 ;
  end ;

  constructor TGIS_Point3D.Create(
    const _x : Integer ;
    const _y : Integer ;
    const _z : Integer ;
    const _m : Integer
  ) ;
  begin
    inherited Create ;
    X := _x ;
    Y := _y ;
    Z := _z ;
    M := _m ;
  end ;

  constructor TGIS_Point3D.Create(
    const _x : Double ;
    const _y : Double
  ) ;
  begin
    inherited Create ;
    X :=  _x ;
    Y :=  _y ;
    Z := 0.0 ;
    M := 0.0 ;
  end ;

  constructor TGIS_Point3D.Create(
    const _x : Double ;
    const _y : Double ;
    const _z : Double
  ) ;
  begin
    inherited Create ;
    X :=  _x ;
    Y :=  _y ;
    Z :=  _z ;
    M := 0.0 ;
  end ;

  constructor TGIS_Point3D.Create(
    const _x : Double ;
    const _y : Double ;
    const _z : Double ;
    const _m : Double
  ) ;
  begin
    inherited Create ;
    X :=  _x ;
    Y :=  _y ;
    Z :=  _z ;
    M :=  _m ;
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_Point3D.MakeCopy : TGIS_Point3D ;
    begin
      Result := new TGIS_Point3D( X, Y, Z, M ) ;
    end ;
  {$ENDIF}

  function TGIS_Point3D.ToString : String ;
  begin
    Result := Format( 'X = %s Y = %s Z = %s M = %s',
                     [ X.ToString, Y.ToString, Z.ToString, M.ToString ]
                    );
  end ;
{$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  {$ELSE}
    function _TGIS_Point3D( const _pt : TGIS_Point3D ) : TGIS_Point3D ;
    begin
      Result := _pt.MakeCopy() ;
    end ;
  {$ENDIF}

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_Extent
//=============================================================================

  {$IFDEF GIS_NORECORDS}
    constructor TGIS_Extent.Create ;
    begin
      inherited Create ;
    end ;
  {$ENDIF}

  constructor TGIS_Extent.Create(
    const _xmin : Integer ;
    const _ymin : Integer ;
    const _xmax : Integer ;
    const _ymax : Integer
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    XMax := _xmax ;
    YMax := _ymax ;
  end ;

  constructor TGIS_Extent.Create(
    const _xmin : Double ;
    const _ymin : Double ;
    const _xmax : Double ;
    const _ymax : Double
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    XMax := _xmax ;
    YMax := _ymax ;
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_Extent.MakeCopy : TGIS_Extent ;
    begin
      Result := new TGIS_Extent( XMin, YMin, XMax, YMax ) ;
    end ;
  {$ENDIF}

  function TGIS_Extent.ToString : String ;
  begin
    exit Format( 'XMin = %s YMin = %s XMax = %s YMax = %s',
                 [ XMin.ToString, YMin.ToString, XMax.ToString, YMax.ToString ]
               );
  end ;

//=============================================================================
// TGIS_Extent3D
//=============================================================================

  {$IFDEF GIS_NORECORDS}
    constructor TGIS_Extent3D.Create ;
    begin
      inherited Create ;
    end ;
  {$ENDIF}

  constructor TGIS_Extent3D.Create(
    const _xmin : Integer ;
    const _ymin : Integer ;
    const _xmax : Integer ;
    const _ymax : Integer
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    ZMin :=     0 ;
    XMax := _xmax ;
    YMax := _ymax ;
    ZMax :=     0 ;
  end ;

  constructor TGIS_Extent3D.Create(
    const _xmin : Integer ;
    const _ymin : Integer ;
    const _zmin : Integer ;
    const _mmin : Integer ;
    const _xmax : Integer ;
    const _ymax : Integer ;
    const _zmax : Integer ;
    const _mmax : Integer
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    ZMin := _zmin ;
    MMin := _mmin ;
    XMax := _xmax ;
    YMax := _ymax ;
    ZMax := _zmax ;
    MMax := _mmax ;
  end ;

  constructor TGIS_Extent3D.Create(
    const _xmin : Double ;
    const _ymin : Double ;
    const _xmax : Double ;
    const _ymax : Double
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    ZMin :=   0.0 ;
    XMax := _xmax ;
    YMax := _ymax ;
    ZMax :=   0.0 ;
  end ;

  constructor TGIS_Extent3D.Create(
    const _xmin : Double ;
    const _ymin : Double ;
    const _zmin : Double ;
    const _mmin : Double ;
    const _xmax : Double ;
    const _ymax : Double ;
    const _zmax : Double ;
    const _mmax : Double
  ) ;
  begin
    inherited Create ;
    XMin := _xmin ;
    YMin := _ymin ;
    ZMin := _zmin ;
    MMin := _mmin ;
    XMax := _xmax ;
    YMax := _ymax ;
    ZMax := _zmax ;
    MMax := _mmax ;
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_Extent3D.MakeCopy : TGIS_Extent3D ;
    begin
      Result := new TGIS_Extent3D( XMin, YMin, ZMin, MMin, XMax, YMax, ZMax, MMax ) ;
    end ;
  {$ENDIF}

  function TGIS_Extent3D.ToString : String ;
  begin
    exit Format( 'XMin = %s YMin = %s ZMin = %s MMin = %s XMax = %s YMax = %s ZMax = %s MMax = %s',
                [ XMin.ToString, YMin.ToString, ZMin.ToString, MMin.ToString,
                  XMax.ToString, YMax.ToString, ZMax.ToString, MMax.ToString ]
               );
  end ;
{$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  {$ELSE}
    function _TGIS_Extent3D( const _ext : TGIS_Extent3D ) : TGIS_Extent3D ;
    begin
      Result := _ext.MakeCopy() ;
    end ;

    function _TGIS_Extent( const _ext : TGIS_Extent ) : TGIS_Extent ;
    begin
      Result := _ext.MakeCopy() ;
    end ;
  {$ENDIF}

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_HelpEventArgs
//=============================================================================

  constructor TGIS_HelpEventArgs.Create( _name : String ) ;
  begin
    inherited Create ;
    FName := _name ;
  end;

//=============================================================================
// TGIS_TemplateProducerEventArgs
//=============================================================================

  constructor TGIS_TemplateProducerEventArgs.Create( _token : String ) ;
  begin
    inherited Create ;
    FToken := _token ;
  end;

//=============================================================================
// TGIS_ReadWriteEventArgs
//=============================================================================

  constructor TGIS_ReadWriteEventArgs.Create( const _pos     : LongInt ;
                                              const _buffer  : TBytes  ;
                                              const _count   : Integer
                                            ) ;
  begin
    inherited Create ;
    FPos    := _pos ;
    FBuffer := _buffer ;
    FCount  := _count ;
  end ;

//=============================================================================
// TGIS_PaintEventArgs
//=============================================================================

  constructor TGIS_PaintEventArgs.Create(
    const _graphics : TObject
  ) ;
  begin
    inherited Create ;
    FGraphics := _graphics ;
  end;

//=============================================================================
// TGIS_BusyEventArgs
//=============================================================================

  constructor TGIS_BusyEventArgs.Create( _pos    : Int64 ;
                                         _end    : Int64
                                       ) ;
  begin
    inherited Create ;
    FPos    := _pos   ;
    FEnd    := _end   ;
    FAbort  := False  ;
  end;

  constructor TGIS_BusyEventArgs.Create( _pos    : Int64 ;
                                         _end    : Int64 ;
                                         _abort  : Boolean
                                       ) ;
  begin
    inherited Create ;
    FPos    := _pos   ;
    FEnd    := _end   ;
    FAbort  := _abort ;
  end ;
{$ENDIF}

//=============================================================================
// TGIS_WebUtils
//=============================================================================

  class function TGIS_WebUtils.HttpFetch(
    const _url        : String         ;
    const _strm       : TStream        ;
    const _onBusy     : TGIS_BusyEvent ;
    const _cache      : Boolean        ;
    const _timeout    : Integer        ;
    const _agent      : String         ;
    const _referer    : String         ;
    const _user       : String         ;
    const _pass       : String         ;
    const _decompress : Boolean
  ) : TGIS_HttpResponse ;
  var
    url     : String    ;
    ext     : String    ;
    agent   : String    ;
    fstream : TGIS_FileStream ;
    options : TGIS_HttpSettings ;
    buf     : TBytes ;
    stm     : TStream ;
  begin
    Result.Status        := GIS_HTTP_FETCH_UNKNOWN ;
    Result.ContentType   := ''    ;
    Result.ContentLength := 0     ;
    Result.Stream        := nil   ;

    url := URLFixed( _url ) ;

    if IsStringEmpty( url ) then
      exit ; // we only zeroed result

    if not assigned( _strm ) then
      Result.Stream := TMemoryStream.Create
    else
      Result.Stream := _strm ;

    if Pos( '://', url ) < StringFirst then begin
      // seems to be local file
      try
        if FileExists( url ) then begin
          fstream := TGIS_FileStream.Create( url,
                                             fmOpenRead or
                                             fmShareDenyWrite
                                           ) ;

          try
            Result.Stream.CopyFrom( fstream, fstream.Size ) ;
          finally
            FreeObject( fstream ) ;
          end ;
          Result.Status := GIS_HTTP_OK ;

          ext := ExtractFileExt( url ) ;
          if      CompareText( ext, '.gif'  ) = 0 then
                  Result.ContentType := 'image/gif'
          else if CompareText( ext, '.png'  ) = 0 then
                  Result.ContentType := 'image/png'
          else if CompareText( ext, '.jpg'  ) = 0 then
                  Result.ContentType := 'image/jpeg'
          else if CompareText( ext, '.htm'  ) = 0 then
                  Result.ContentType := 'image/html'
          else if CompareText( ext, '.html' ) = 0 then
                  Result.ContentType := 'image/html'
          else    Result.ContentType := 'text/plain' ;

          Result.ContentLength := Result.Stream.Size ;

          Result.Headers := '' ;
        end
        else
          Result.Status := GIS_HTTP_NOTFOUND ;
      except
        Result.Status := GIS_HTTP_FETCH_INTERNALERROR ;
      end;
      exit ;
    end ;

    if IsStringEmpty( _agent ) then
      agent := GetDefaultUserAgent('')
    else
      agent := _agent ;

    options := TGIS_HttpSettings.CreateDefault ;
    options.User        := _user ;
    options.Pass        := _pass ;
    options.UseCache    := _cache ;
    options.Timeout     := _timeout ;
    options.Agent       := agent ;
    options.Referer     := _referer ;

    if GisProxySettings.CanUseProxy( url ) then begin
      options.ProxyServer := GisProxySettings.Server ;
      options.ProxyPort   := GisProxySettings.Port ;
      options.ProxyUser   := GisProxySettings.User ;
      options.ProxyPass   := GisProxySettings.Pass ;
      options.ProxyDomain := GisProxySettings.Domain ;
    end ;

    try
      HttpFetchStream(
         url,
         options,
         Result.Status,
         Result.ContentLength,
         Result.ContentType,
         Result.ContentExpires,
         Result.Headers,
         Result.Stream
        ) ;
      Result.Stream.Position := 0 ;

      {$IFDEF MSWINDOWS}
        if Result.ContentLength > Result.Stream.Size then begin
          Result.Status := GIS_HTTP_FETCH_NOTCOMPLETED ;
          exit ;
        end ;
      {$ENDIF}

      if not _decompress then
        exit ;

      // recognize compression
      SetLength( buf, 4 ) ;
      Result.Stream.Read( buf, 4 ) ;
      Result.Stream.Position := 0 ;

      stm := nil ;
      try
        if      (buf[0] = $1F) and (buf[1] = $8B ) then
          stm := DecompressGZipStream( Result.Stream )
        else if (buf[0] = $78) and (buf[1] = $9C ) then
          stm := DecompressDeflateStream( Result.Stream )
        else if (buf[0] = $50) and (buf[1] = $4B ) and
                (buf[2] = $03) and (buf[3] = $04 ) then
          stm := DecompressZipFile( Result.Stream ) ;
        if assigned( stm ) then begin
          stm.Position := 0 ;
          {$IFDEF OXYGENE}
            Result.Stream.Position := 0 ;
            Result.Stream.CopyFrom( stm, stm.Size ) ;
          {$ELSE}
            Result.Stream.Position := 0 ;
            Result.Stream.CopyFrom( stm, stm.Size ) ;
          {$ENDIF}
        end ;
      finally
        {$IFNDEF OXYGENE}
          if assigned( stm ) then
            FreeObject( stm ) ;
        {$ENDIF}
      end;

      Result.Stream.Position := 0 ;
    except
      if Result.Status = GIS_HTTP_FETCH_UNKNOWN then
        Result.Status := GIS_HTTP_FETCH_INTERNALERROR ;
    end;

  end ;


//=============================================================================
// TGIS_LayerInfo
//=============================================================================

  constructor TGIS_LayerInfo.Create(
    const _name      : String ;
    const _layerType : TGIS_RegisteredLayerType ;
    const _shapeType : TGIS_ShapeType
   ) ;
  begin
    inherited Create ;

    Name          := _name ;
    Caption       := _name ;
    LayerType     := _layerType ;
    ShapeType     := _shapeType ;
    PixelType     := TGIS_PixelSubFormat.None ;
    ExtendedTag   := nil ;
    FeatureCount  := -1 ;
  end ;

  constructor TGIS_LayerInfo.Create(
    const _name         : String ;
    const _layerType    : TGIS_RegisteredLayerType ;
    const _shapeType    : TGIS_ShapeType ;
    const _featureCount : TGIS_Uid
   ) ;
  begin
    inherited Create ;

    Name          := _name ;
    Caption       := _name ;
    LayerType     := _layerType ;
    ShapeType     := _shapeType ;
    PixelType     := TGIS_PixelSubFormat.None ;
    ExtendedTag   := nil ;
    FeatureCount  := _featureCount ;
  end ;

  constructor TGIS_LayerInfo.Create(
    const _name      : String ;
    const _caption   : String ;
    const _layerType : TGIS_RegisteredLayerType ;
    const _shapeType : TGIS_ShapeType ;
    const _pixelType : TGIS_PixelSubFormat ;
    const _extended  : TObject
   ) ;
  begin
    inherited Create ;

    Name          := _name ;
    Caption       := _caption ;
    LayerType     := _layerType ;
    ShapeType     := _shapeType ;
    PixelType     := _pixelType ;
    ExtendedTag   := _extended ;
    FeatureCount  := -1 ;
  end ;

  constructor TGIS_LayerInfo.Create(
    const _name         : String ;
    const _caption      : String ;
    const _layerType    : TGIS_RegisteredLayerType ;
    const _shapeType    : TGIS_ShapeType ;
    const _pixelType    : TGIS_PixelSubFormat ;
    const _extended     : TObject ;
    const _featureCount : TGIS_Uid
   ) ;
  begin
    inherited Create ;

    Name          := _name ;
    Caption       := _caption ;
    LayerType     := _layerType ;
    ShapeType     := _shapeType ;
    PixelType     := _pixelType ;
    ExtendedTag   := _extended ;
    FeatureCount  := _featureCount ;
  end ;

//=============================================================================
// TGIS_LayerPixelSubFormat
//=============================================================================

  constructor TGIS_LayerPixelSubFormat.Create(
    const _pixelformat : TGIS_PixelFormat ;
    const _grayscale   : Boolean ;
    const _subformat   : TGIS_PixelSubFormat ;
    const _compression : TGIS_CompressionType ;
    const _level       : Integer
  ) ;
  begin
    FPixelFormat      := _pixelformat ;
    FGrayScale        := _grayscale   ;
    FSubFormat        := _subformat   ;
    FCompression      := _compression ;
    FCompressionLevel := _level       ;
  end ;

  function TGIS_LayerPixelSubFormat.CreateCopy : TGIS_LayerPixelSubFormat ;
  begin
    Result := TGIS_LayerPixelSubFormat.Create(
                FPixelFormat ,
                FGrayScale   ,
                FSubFormat   ,
                FCompression ,
                FCompressionLevel
              ) ;
  end ;


  class function TGIS_LayerPixelSubFormat.DefaultSubFormat : TGIS_LayerPixelSubFormat ;
  begin
    Result := TGIS_LayerPixelSubFormat.Create(
                TGIS_PixelFormat.RGB,
                False,
                TGIS_PixelSubFormat.None,
                TGIS_CompressionType.None,
                90
              ) ;
  end ;

  {$IFNDEF GENDOC}
  class operator TGIS_LayerPixelSubFormat.Equal(
    _value1  : TGIS_LayerPixelSubFormat ;
    _value2  : TGIS_LayerPixelSubFormat
  ) : Boolean ;
  begin
    Result := ( _value1.PixelFormat  = _value2.PixelFormat ) and
              ( _value1.GrayScale    = _value2.GrayScale   ) and
              ( _value1.Subformat    = _value2.Subformat   ) and
              ( _value1.Compression  = _value2.Compression ) ;
  end;

  class operator TGIS_LayerPixelSubFormat.NotEqual(
    _value1  : TGIS_LayerPixelSubFormat ;
    _value2  : TGIS_LayerPixelSubFormat
  ) : Boolean ;
  begin
    Result := ( _value1.PixelFormat  <> _value2.PixelFormat ) or
              ( _value1.GrayScale    <> _value2.GrayScale   ) or
              ( _value1.Subformat    <> _value2.Subformat   ) or
              ( _value1.Compression  <> _value2.Compression ) ;
  end;
  {$ENDIF}

  function TGIS_LayerPixelSubFormat.ToString : String ;
  begin
    Result := '' ;

    case FPixelFormat of
      TGIS_PixelFormat.Bit1:    Result := '1 bit' ;
      TGIS_PixelFormat.Bit4:    Result := '4 bit' ;
      TGIS_PixelFormat.Bit8:    Result := '8 bit' ;
      TGIS_PixelFormat.RGB:     Result := '24 bit' ;
      TGIS_PixelFormat.ARGB:    Result := '32 bit' ;
      TGIS_PixelFormat.Custom:  if FSubFormat = TGIS_PixelSubFormat.GRID then
                                  Result := 'Grid' ;
    end ;

    case FCompression of
      TGIS_CompressionType.JPEG : Result := Result + ' JPEG' ;
      TGIS_CompressionType.PNG  : Result := Result + ' PNG' ;
      TGIS_CompressionType.LZW  : Result := Result + ' LZW' ;
      TGIS_CompressionType.ZLIB : Result := Result + ' ZLIB' ;
      TGIS_CompressionType.JP2  : Result := Result + ' JP2000' ;
    end ;

    if FGrayScale then
      Result := Result + ' Grayscale' ;

    case FSubFormat of
      TGIS_PixelSubFormat.BMP   : Result := Result + ' [BMP]' ;
      TGIS_PixelSubFormat.JPEG  : Result := Result + ' [JPEG]' ;
      TGIS_PixelSubFormat.PNG   : Result := Result + ' [PNG]' ;
      TGIS_PixelSubFormat.JP2   : Result := Result + ' [JP2000]' ;
      TGIS_PixelSubFormat.GRID  : Result := Result + ' [GRID]' ;
    end ;
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_LayerPixelSubFormat.MakeCopy : TGIS_LayerPixelSubFormat ;
    begin
      Result := TGIS_LayerPixelSubFormat.Create( FPixelFormat,
                                                 FGrayScale,
                                                 FSubFormat,
                                                 FCompression,
                                                 FCompressionLevel
                                               ) ;
    end;

  {$ENDIF}

  {$IFNDEF GIS_NORECORDS}
  {$ELSE}
    function _TGIS_LayerPixelSubFormat(
      const _lpsf : TGIS_LayerPixelSubFormat
    ) : TGIS_LayerPixelSubFormat ;
    begin
      Result := _lpsf.MakeCopy ;
    end ;
  {$ENDIF}

//==================================== END =====================================
end.
