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
  Basic types for the 3D rendering code.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoTypes3D ;
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
uses
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI ;
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
  ///   Definition of a camera for the 3D view.
  /// </summary>
  TGIS_3DCamera = {$IFDEF OXYGENE} public {$ENDIF}
                  {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    {$IFDEF OXYGENE} public {$ENDIF}
    /// <summary>
    ///   Name of the camera.
    /// </summary>
    Name       : String ;
    /// <summary>
    ///   True if the camera is active.
    /// </summary>
    Active     : Boolean ;
    /// <summary>
    ///   Position of the camera.
    /// </summary>
    Position   : TGIS_Point3D ;
    /// <summary>
    ///   Location of the camera.
    /// </summary>
    PositionEx : TGIS_Point3D ;
    /// <summary>
    ///   Rotation of the camera.
    /// </summary>
    Rotation   : TGIS_Point3D ;
    /// <summary>
    ///   Reference point of the camera.
    /// </summary>
    Reference  : TGIS_Point3D ;
  end ;

  /// <summary>
  ///   Types of reference level mode ( Z value of rotation point).
  /// </summary>
  TGIS_Viewer3DReferenceMode = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   reference level at default level ie. min Z value among DEMs
      /// </summary>
      Base,

      /// <summary>
      ///   reference level at 0
      /// </summary>
      Zero,

      /// <summary>
      ///   reference level on DEM surface
      /// </summary>
      OnDem,

      /// <summary>
      ///   reference level equal to lowest value of DEMs&amp;Vectors in
      ///   currently displayed project
      /// </summary>
      Lowest,

      /// <summary>
      ///   reference level equal to highest value of DEMs&amp;Vectors in
      ///   currently displayed project
      /// </summary>
      Highest,

      /// <summary>
      ///   reference level will follow DEM surface
      /// </summary>
      FlyOnDem
  ) ;

  /// <summary>
  ///   Types of DEM walls.
  /// </summary>
  TGIS_Viewer3DDemWall  = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   texture wall
      /// </summary>
      Texture,

      /// <summary>
      ///   invisible wall
      /// </summary>
      Invisible,

      /// <summary>
      ///   solid color wall
      /// </summary>
      Solid
  ) ;

  /// <summary>
  ///   Structure for flood management.
  /// </summary>
  TGIS_Viewer3DFlood = {$IFDEF OXYGENE} public {$ENDIF} record
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   Flood on/off switch, default is off.
      /// </summary>
      Active       : Boolean ;

      /// <summary>
      ///   Flood level.
      /// </summary>
      Level        : Double  ;

      /// <summary>
      ///   Flood color.
      /// </summary>
      Color        : TGIS_Color  ;

      /// <summary>
      ///   Flood transparency 0%-100% .
      /// </summary>
      Transparency : Integer ;
  end;

  /// <summary>
  ///   Structure for BasePlane management.
  /// </summary>
  TGIS_Viewer3DBasePlane = {$IFDEF OXYGENE} public {$ENDIF} record
    {$IFDEF OXYGENE} public {$ENDIF}

      /// <summary>
      ///   BasePlane on/off switch, default is on.
      /// </summary>
      Active          : Boolean ;

      /// <summary>
      ///   BasePlane level.
      /// </summary>
      Level           : Double  ;

      /// <summary>
      ///   BasePlane transparency 0%-100% .
      /// </summary>
      Transparency    : Integer ;

      /// <summary>
      ///   BasePlane background color.
      /// </summary>
      BackgroundColor : TGIS_Color  ;

      /// <summary>
      ///   BasePlane grid color.
      /// </summary>
      GridColor       : TGIS_Color  ;
  end;

  /// <summary>
  ///   Types of camera position restriction.
  /// </summary>
  TGIS_Viewer3DViewRestriction =  {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   no restrictions, camera can be placed anywhere
      /// </summary>
      None          ,

      /// <summary>
      ///   camera can be placed only above horizon
      /// </summary>
      AboveHorizon
  ) ;

  /// <summary>
  ///   Grid size used to display regular DEMs &amp; IMAGEs.
  /// </summary>
  TGIS_Viewer3DDemCacheSize = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Medium = 512 x 512
      /// </summary>
      Medium  ,

      /// <summary>
      ///   Window = window size based
      /// </summary>
      Window
  ) ;


  /// <summary>
  ///   Types of reference level (Z value of rotation point)
  /// </summary>
  TGIS_Viewer3DReferenceLevelMode  = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Rotation point relative to 0 (const value)
      /// </summary>
      AboveZero,

      /// <summary>
      ///   Rotation point will follow DEM surface
      /// </summary>
      AboveDem,

      /// <summary>
      ///   Rotation point at default level ie. min Z value in DEM
      /// </summary>
      Default
  ) ;

  /// <summary>
  ///   Types of transparency priority in case that both DEM &amp; vector are
  ///   transparent.
  /// </summary>
  TGIS_Viewer3DTransparencyPriority =  {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   if DEM not transparent, then if vector is transparent, user will
      ///   see DEM through the vector; if DEM is transparent, then user will
      ///   see vector laying below the DEM
      /// </summary>
      Auto   ,

      /// <summary>
      ///   transparent vector - user will see DEM through the vector; Dem will
      ///   be rendered first
      /// </summary>
      Vector ,

      /// <summary>
      ///   transparent DEM - user will see vector laying below the DEM; vector
      ///   will be rendered first
      /// </summary>
      Dem
  ) ;


//##############################################################################
implementation


//==================================== END =====================================
end.

