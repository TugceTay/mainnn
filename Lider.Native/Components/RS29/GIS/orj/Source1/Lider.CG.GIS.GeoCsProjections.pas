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
  Projections unit for Coordinate System.

  This unit was partially based on PROJ4:

  Copyright (c) 2000, Frank Warmerdam

  Copyright (c) 2008   Gerald I. Evenden (for high precision Transverse Mercator)

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
  Kernel License.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCsProjections ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCsProjections"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsBase ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Possible projection parameters.
  /// </summary>
  TGIS_CSProjParameter = {$IFDEF OXYGENE} public {$ENDIF}
  (
    /// <summary>
    ///   Central Meridian. Valid for selected projections.
    /// </summary>
    CentralMeridian           ,

    /// <summary>
    ///   Origin latitude. Valid for selected projections.
    /// </summary>
    LatitudeOfOrigin          ,

    /// <summary>
    ///   False shift in meters. Valid for selected projections.
    /// </summary>
    FalseEasting              ,

    /// <summary>
    ///   False shift in meters. Valid for selected projections.
    /// </summary>
    FalseNorthing             ,

    /// <summary>
    ///   First Pseudo Standard parallel. Valid for selected projections.
    /// </summary>
    StandardParallel_1        ,

    /// <summary>
    ///   Second Standard parallel. Valid for selected projections.
    /// </summary>
    StandardParallel_2        ,

    /// <summary>
    ///   First Pseudo Standard parallel. Valid for selected projections.
    /// </summary>
    PseudoStandardParallel_1  ,

    /// <summary>
    ///   Zone. Valid for selected projections.
    /// </summary>
    Zone                      ,

    /// <summary>
    ///   Scale factor. Valid for selected projections.
    /// </summary>
    ScaleFactor               ,

    /// <summary>
    ///   Center Longitude. Valid for selected projections.
    /// </summary>
    LongitudeOfCenter         ,

    /// <summary>
    ///   Center Latitude. Valid for selected projections.
    /// </summary>
    LatitudeOfCenter          ,
    /// <summary>
    ///   Azimuth angle. Valid for selected projections.
    /// </summary>
    Azimuth                   ,

    /// <summary>
    ///   Longitude of the first point. Valid for selected projections.
    /// </summary>
    LongitudeOfPoint_1        ,

    /// <summary>
    ///   Latitude of the first point. Valid for selected projections.
    /// </summary>
    LatitudeOfPoint_1         ,

    /// <summary>
    ///   Longitude of the second point. Valid for selected projections.
    /// </summary>
    LongitudeOfPoint_2        ,

    /// <summary>
    ///   Latitude of the second point. Valid for selected projections.
    /// </summary>
    LatitudeOfPoint_2         ,

    /// <summary>
    ///   X Scale. Valid for selected projections.
    /// </summary>
    XScale                    ,

    /// <summary>
    ///   Y Scale. Valid for selected projections.
    /// </summary>
    YScale                    ,

    /// <summary>
    ///   Plane rotation. Valid for selected projections.
    /// </summary>
    XYPlaneRotation           ,

    /// <summary>
    ///   Custom1. Valid and specific for selected projections.
    /// </summary>
    Custom1                   ,

    /// <summary>
    ///   Custom2. Valid and specific for selected projections.
    /// </summary>
    Custom2                   ,

    /// <summary>
    ///   Custom3. Valid and specific for selected projections.
    /// </summary>
    Custom3                   ,

    /// <summary>
    ///   Custom4. Valid and specific for selected projections.
    /// </summary>
    Custom4                   ,

    /// <summary>
    ///   Custom5. Valid and specific for selected projections.
    /// </summary>
    Custom5                   ,

    /// <summary>
    ///   Custom6. Valid and specific for selected projections.
    /// </summary>
    Custom6

  ) {$IFDEF JAVA} of Integer {$ENDIF} ;


  /// <summary>
  ///   Set of possible projection parameters.
  /// </summary>
  TGIS_CSProjParameterSet = {$IFDEF OXYGENE} public {$ENDIF} set of TGIS_CSProjParameter ;


  /// <summary>
  ///   Test if particular parameter is supported by a given projection.
  /// </summary>
  /// <param name="_val">
  ///   parameter to be tested
  /// </param>
  /// <param name="_set">
  ///   set of parameters supported by projection
  /// </param>
  /// <returns>
  ///   True if parameter is supported.
  /// </returns>
  /// <remarks>
  ///   Defined as a platform independent set tester.
  /// </remarks>
  function InCSProjParameterSet(
    const _val : TGIS_CSProjParameter   ;
    const _set : TGIS_CSProjParameterSet
  ) : Boolean ;


type

  /// <summary>
  ///   Projection parameters.
  /// </summary>
  TGIS_CSProjParameters = {$IFDEF OXYGENE} public {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    public

    /// <summary>
    ///   Central Meridian. Valid for selected projections.
    /// </summary>
    CentralMeridian          : Double  ;

    /// <summary>
    ///   Origin latitude. Valid for selected projections.
    /// </summary>
    LatitudeOfOrigin         : Double  ;

    /// <summary>
    ///   False shift in meters. Valid for selected projections.
    /// </summary>
    FalseEasting             : Double  ;

    /// <summary>
    ///   False shift in meters. Valid for selected projections.
    /// </summary>
    FalseNorthing            : Double  ;

    /// <summary>
    ///   First Pseudo Standard parallel. Valid for selected projections.
    /// </summary>
    StandardParallel_1       : Double  ;

    /// <summary>
    ///   Second Standard parallel. Valid for selected projections.
    /// </summary>
    StandardParallel_2       : Double  ;

    /// <summary>
    ///   First Pseudo Standard parallel. Valid for selected projections.
    /// </summary>
    PseudoStandardParallel_1 : Double  ;

    /// <summary>
    ///   Zone. Valid for selected projections.
    /// </summary>
    Zone                     : Integer ;

    /// <summary>
    ///   Scale factor. Valid for selected projections.
    /// </summary>
    ScaleFactor              : Double  ;

    /// <summary>
    ///   Center Longitude. Valid for selected projections.
    /// </summary>
    LongitudeOfCenter        : Double  ;

    /// <summary>
    ///   Center Latitude. Valid for selected projections.
    /// </summary>
    LatitudeOfCenter         : Double  ;

    /// <summary>
    ///   Azimuth angle. Valid for selected projections.
    /// </summary>
    Azimuth                  : Double  ;

    /// <summary>
    ///   Longitude of the first point. Valid for selected projections.
    /// </summary>
    LongitudeOfPoint_1       : Double  ;

    /// <summary>
    ///   Latitude of the first point. Valid for selected projections.
    /// </summary>
    LatitudeOfPoint_1        : Double  ;

    /// <summary>
    ///   Longitude of the second point. Valid for selected projections.
    /// </summary>
    LongitudeOfPoint_2       : Double  ;

    /// <summary>
    ///   Latitude of the second point. Valid for selected projections.
    /// </summary>
    LatitudeOfPoint_2        : Double  ;

    /// <summary>
    ///   X Scale. Valid for selected projections.
    /// </summary>
    XScale                   : Double  ;

    /// <summary>
    ///   Y Scale. Valid for selected projections.
    /// </summary>
    YScale                   : Double  ;

    /// <summary>
    ///   Plane rotation. Valid for selected projections.
    /// </summary>
    XYPlaneRotation          : Double  ;

    /// <summary>
    ///   Custom1. Valid and specifiv for selected projections.
    /// </summary>
    Custom1                 : Double  ;

    /// <summary>
    ///   Custom1Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom1Name             : String  ;

    /// <summary>
    ///   Custom2. Valid and specifiv for selected projections.
    /// </summary>
    Custom2                 : Double  ;

    /// <summary>
    ///   Custom2Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom2Name             : String  ;

    /// <summary>
    ///   Custom3. Valid and specifiv for selected projections.
    /// </summary>
    Custom3                 : Double  ;

    /// <summary>
    ///   Custom3Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom3Name             : String  ;

    /// <summary>
    ///   Custom4. Valid and specifiv for selected projections.
    /// </summary>
    Custom4                 : Double  ;

    /// <summary>
    ///   Custom4Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom4Name             : String  ;

    /// <summary>
    ///   Custom5. Valid and specifiv for selected projections.
    /// </summary>
    Custom5                 : Double  ;

    /// <summary>
    ///   Custom5Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom5Name             : String  ;

    /// <summary>
    ///   Custom6. Valid and specifiv for selected projections.
    /// </summary>
    Custom6                 : Double  ;

    /// <summary>
    ///   Custom6Name. Valid and specifiv for selected projections.
    /// </summary>
    Custom6Name             : String  ;

  end ;

  /// <summary>
  ///   Element of Projection parameters. For TGIS_CSProjParametersInternal use.
  /// </summary>
  TGIS_CSProjParametersInternalElement = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

    {#gendoc:hide}
    Id    : Byte ;

    {#gendoc:hide}
    Value : Double ;

    {#gendoc:hide}
    CustomName : String ;
  end ;

  /// <summary>
  ///   Projection parameters. Memory optimized version.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    For internal purposes mainly due to record incompatibility with
  ///    C++Builder.
  ///    </note>
  /// </remarks>
  TGIS_CSProjParametersInternal = {$IFDEF OXYGENE} public {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    private
      ar    : array of TGIS_CSProjParametersInternalElement ;
      bLock : Boolean ;
    private

      /// <summary>
      ///   Get parameter from the internal array
      /// </summary>
      /// <param name="_id">
      ///   parameter identifier
      /// </param>
      function  get_Parameter                 ( const _id    : Byte
                                              ) : Double ;

      /// <summary>
      ///   Set parameter to the internal internal array
      /// </summary>
      /// <param name="_id">
      ///   parameter identifier
      /// </param>
      /// <param name="_value">
      ///   if 0 or Nan then parameter will be deleted
      /// </param>
      procedure set_Parameter                 ( const _id    : Byte ;
                                                const _value : Double
                                              ) ;

      /// <summary>
      ///   Get parameter name from the internal array
      /// </summary>
      /// <param name="_id">
      ///   parameter identifier
      /// </param>
      function  get_ParameterName             ( const _id    : Byte
                                              ) : String ;

      /// <summary>
      ///   Set parameter name to the internal internal array
      /// </summary>
      /// <param name="_id">
      ///   parameter identifier
      /// </param>
      /// <param name="_value">
      ///   custom name or empty string
      /// </param>
      procedure set_ParameterName             ( const _id    : Byte ;
                                                const _value : String
                                              ) ;

    private

      function  fget_CentralMeridian          : Double  ;
      procedure fset_CentralMeridian          ( const _value : Double ) ;
      function  fget_LatitudeOfOrigin         : Double  ;
      procedure fset_LatitudeOfOrigin         ( const _value : Double ) ;
      function  fget_FalseEasting             : Double  ;
      procedure fset_FalseEasting             ( const _value : Double ) ;
      function  fget_FalseNorthing            : Double  ;
      procedure fset_FalseNorthing            ( const _value : Double ) ;
      function  fget_StandardParallel_1       : Double  ;
      procedure fset_StandardParallel_1       ( const _value : Double ) ;
      function  fget_StandardParallel_2       : Double  ;
      procedure fset_StandardParallel_2       ( const _value : Double ) ;
      function  fget_PseudoStandardParallel_1 : Double  ;
      procedure fset_PseudoStandardParallel_1 ( const _value : Double ) ;
      function  fget_Zone                     : Integer ;
      procedure fset_Zone                     ( const _value : Integer) ;
      function  fget_ScaleFactor              : Double  ;
      procedure fset_ScaleFactor              ( const _value : Double ) ;
      function  fget_LongitudeOfCenter        : Double  ;
      procedure fset_LongitudeOfCenter        ( const _value : Double ) ;
      function  fget_LatitudeOfCenter         : Double  ;
      procedure fset_LatitudeOfCenter         ( const _value : Double ) ;
      function  fget_Azimuth                  : Double  ;
      procedure fset_Azimuth                  ( const _value : Double ) ;
      function  fget_LongitudeOfPoint_1       : Double  ;
      procedure fset_LongitudeOfPoint_1       ( const _value : Double ) ;
      function  fget_LatitudeOfPoint_1        : Double  ;
      procedure fset_LatitudeOfPoint_1        ( const _value : Double ) ;
      function  fget_LongitudeOfPoint_2       : Double  ;
      procedure fset_LongitudeOfPoint_2       ( const _value : Double ) ;
      function  fget_LatitudeOfPoint_2        : Double  ;
      procedure fset_LatitudeOfPoint_2        ( const _value : Double ) ;
      function  fget_XScale                   : Double  ;
      procedure fset_XScale                   ( const _value : Double ) ;
      function  fget_YScale                   : Double  ;
      procedure fset_YScale                   ( const _value : Double ) ;
      function  fget_XYPlaneRotation          : Double  ;
      procedure fset_XYPlaneRotation          ( const _value : Double ) ;
      function  fget_Custom1                  : Double  ;
      procedure fset_Custom1                  ( const _value : Double ) ;
      function  fget_Custom1Name              : String ;
      procedure fset_Custom1Name              ( const _value : String ) ;
      function  fget_Custom2                  : Double  ;
      procedure fset_Custom2                  ( const _value : Double ) ;
      function  fget_Custom2Name              : String ;
      procedure fset_Custom2Name              ( const _value : String ) ;
      function  fget_Custom3                  : Double  ;
      procedure fset_Custom3                  ( const _value : Double ) ;
      function  fget_Custom3Name              : String ;
      procedure fset_Custom3Name              ( const _value : String ) ;
      function  fget_Custom4                  : Double  ;
      procedure fset_Custom4                  ( const _value : Double ) ;
      function  fget_Custom4Name              : String ;
      procedure fset_Custom4Name              ( const _value : String ) ;
      function  fget_Custom5                  : Double  ;
      procedure fset_Custom5                  ( const _value : Double ) ;
      function  fget_Custom5Name              : String ;
      procedure fset_Custom5Name              ( const _value : String ) ;
      function  fget_Custom6                  : Double  ;
      procedure fset_Custom6                  ( const _value : Double ) ;
      function  fget_Custom6Name              : String ;
      procedure fset_Custom6Name              ( const _value : String ) ;
      function  fget_All                      : TGIS_CSProjParameters ;
      procedure fset_All                      ( const _value : TGIS_CSProjParameters ) ;
    public

      /// <summary>
      ///   Central Meridian. Valid for selected projections.
      /// </summary>
      property CentralMeridian          : Double
                                          read  fget_CentralMeridian
                                          write fset_CentralMeridian          ;

      /// <summary>
      ///   Origin latitude. Valid for selected projections.
      /// </summary>
      property LatitudeOfOrigin         : Double
                                          read  fget_LatitudeOfOrigin
                                          write fset_LatitudeOfOrigin         ;

      /// <summary>
      ///   False shift in meters. Valid for selected projections.
      /// </summary>
      property FalseEasting             : Double
                                          read  fget_FalseEasting
                                          write fset_FalseEasting             ;

      /// <summary>
      ///   False shift in meters. Valid for selected projections.
      /// </summary>
      property FalseNorthing            : Double
                                          read  fget_FalseNorthing
                                          write fset_FalseNorthing            ;

      /// <summary>
      ///   First Standard parallel. Valid for selected projections.
      /// </summary>
      property StandardParallel_1       : Double
                                          read  fget_StandardParallel_1
                                          write fset_StandardParallel_1       ;

      /// <summary>
      ///   Second Standard parallel. Valid for selected projections.
      /// </summary>
      property StandardParallel_2       : Double
                                          read  fget_StandardParallel_2
                                          write fset_StandardParallel_2       ;

      /// <summary>
      ///   First Pseudo Standard parallel. Valid for selected projections.
      /// </summary>
      property PseudoStandardParallel_1 : Double
                                          read  fget_PseudoStandardParallel_1
                                          write fset_PseudoStandardParallel_1 ;

      /// <summary>
      ///   Zone. Valid for selected projections.
      /// </summary>
      property Zone                     : Integer
                                          read  fget_Zone
                                          write fset_Zone                     ;

      /// <summary>
      ///   Scale factor. Valid for selected projections.
      /// </summary>
      property ScaleFactor              : Double
                                          read  fget_ScaleFactor
                                          write fset_ScaleFactor              ;

      /// <summary>
      ///   Center Longitude. Valid for selected projections.
      /// </summary>
      property LongitudeOfCenter        : Double
                                          read  fget_LongitudeOfCenter
                                          write fset_LongitudeOfCenter        ;

      /// <summary>
      ///   Center Latitude. Valid for selected projections.
      /// </summary>
      property LatitudeOfCenter         : Double
                                          read  fget_LatitudeOfCenter
                                          write fset_LatitudeOfCenter         ;

      /// <summary>
      ///   Azimuth angle. Valid for selected projections.
      /// </summary>
      property Azimuth                  : Double
                                          read  fget_Azimuth
                                          write fset_Azimuth                  ;

      /// <summary>
      ///   Longitude of the first point. Valid for selected projections.
      /// </summary>
      property LongitudeOfPoint_1       : Double
                                          read  fget_LongitudeOfPoint_1
                                          write fset_LongitudeOfPoint_1       ;

      /// <summary>
      ///   Latitude of the first point. Valid for selected projections.
      /// </summary>
      property LatitudeOfPoint_1        : Double
                                          read  fget_LatitudeOfPoint_1
                                          write fset_LatitudeOfPoint_1        ;

      /// <summary>
      ///   Longitude of the second point. Valid for selected projections.
      /// </summary>
      property LongitudeOfPoint_2       : Double
                                          read  fget_LongitudeOfPoint_2
                                          write fset_LongitudeOfPoint_2       ;

      /// <summary>
      ///   Latitude of the second point. Valid for selected projections.
      /// </summary>
      property LatitudeOfPoint_2        : Double
                                          read  fget_LatitudeOfPoint_2
                                          write fset_LatitudeOfPoint_2        ;

      /// <summary>
      ///   X Scale. Valid for selected projections.
      /// </summary>
      property XScale                   : Double
                                          read  fget_XScale
                                          write fset_XScale                   ;

      /// <summary>
      ///   Y Scale. Valid for selected projections.
      /// </summary>
      property YScale                   : Double
                                          read  fget_YScale
                                          write fset_YScale                   ;

      /// <summary>
      ///   XY Rotation. Valid for selected projections.
      /// </summary>
      property XYPlaneRotation          : Double
                                          read  fget_XYPlaneRotation
                                          write fset_XYPlaneRotation          ;

      /// <summary>
      ///   Custom1. Valid and specific for selected projections.
      /// </summary>
      property Custom1                  : Double
                                          read  fget_Custom1
                                          write fset_Custom1                  ;

      /// <summary>
      ///   Custom1Name. Valid and specific for selected projections.
      /// </summary>
      property Custom1Name              : String
                                          read  fget_Custom1Name
                                          write fset_Custom1Name              ;

      /// <summary>
      ///   Custom2. Valid and specific for selected projections.
      /// </summary>
      property Custom2                  : Double
                                          read  fget_Custom2
                                          write fset_Custom2                  ;

      /// <summary>
      ///   Custom2Name. Valid and specific for selected projections.
      /// </summary>
      property Custom2Name              : String
                                          read  fget_Custom2Name
                                          write fset_Custom2Name              ;

      /// <summary>
      ///   Custom3. Valid and specific for selected projections.
      /// </summary>
      property Custom3                  : Double
                                          read  fget_Custom3
                                          write fset_Custom3                  ;

      /// <summary>
      ///   Custom3Name. Valid and specific for selected projections.
      /// </summary>
      property Custom3Name              : String
                                          read  fget_Custom3Name
                                          write fset_Custom3Name              ;

      /// <summary>
      ///   Custom4. Valid and specific for selected projections.
      /// </summary>
      property Custom4                  : Double
                                          read  fget_Custom4
                                          write fset_Custom4                  ;

      /// <summary>
      ///   Custom4Name. Valid and specific for selected projections.
      /// </summary>
      property Custom4Name              : String
                                          read  fget_Custom4Name
                                          write fset_Custom4Name              ;

      /// <summary>
      ///   Custom5. Valid and specific for selected projections.
      /// </summary>
      property Custom5                  : Double
                                          read  fget_Custom5
                                          write fset_Custom5                  ;

      /// <summary>
      ///   Custom1Name. Valid and specific for selected projections.
      /// </summary>
      property Custom5Name              : String
                                          read  fget_Custom5Name
                                          write fset_Custom5Name              ;

      /// <summary>
      ///   Custom6. Valid and specific for selected projections.
      /// </summary>
      property Custom6                  : Double
                                          read  fget_Custom6
                                          write fset_Custom6                  ;

      /// <summary>
      ///   Custom6Name. Valid and specific for selected projections.
      /// </summary>
      property Custom6Name              : String
                                          read  fget_Custom6Name
                                          write fset_Custom6Name              ;

      /// <summary>
      ///   Full record of parameters.
      /// </summary>
      property All                      : TGIS_CSProjParameters
                                          read  fget_All
                                          write fset_All                      ;
  end ;

  /// <summary>
  ///   Projection type.
  /// </summary>
  TGIS_CSProjType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Miscellaneous projection.
    /// </summary>
    Miscellaneous,

    /// <summary>
    ///   Grid projection.
    /// </summary>
    Grid,

    /// <summary>
    ///   Azimuthal projection.
    /// </summary>
    Azimuthal,

    /// <summary>
    ///   Modified azimuthal projection.
    /// </summary>
    AzimuthalModified,

    /// <summary>
    ///   Psuedoazimuthal projection.
    /// </summary>
    Pseudoazimuthal,

    /// <summary>
    ///   Conical projection.
    /// </summary>
    Conical,

    /// <summary>
    ///   Pseudoconical projection.
    /// </summary>
    PseudoConical,

    /// <summary>
    ///   Cylindrical projection.
    /// </summary>
    Cylindrical,

    /// <summary>
    ///   Modified cylindrical projection.
    /// </summary>
    CylindricalModified,

    /// <summary>
    ///   Oblique cylindrical projection.
    /// </summary>
    CylindricalOblique,

    /// <summary>
    ///   Transverse cylindrical projection.
    /// </summary>
    CylindricalTransverse,

    /// <summary>
    ///   Pseudocylindrical projection
    /// </summary>
    Pseudocylindrical
  ) ;

  /// <summary>
  ///   Class which encapsulates projection.
  /// </summary>
  TGIS_CSProjAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class( TGIS_CSAbstract )
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FParameters        : TGIS_CSProjParametersInternal ;

    // property values
    protected

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FError             : Integer ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FSubtype           : TGIS_CSProjType ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FParametersSet     : TGIS_CSProjParameterSet ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsPureCylindrical : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsRevertible      : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsEllipsoid       : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsSpheroid        : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsGrid            : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FIsXYZ             : Boolean ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FDescription       : String  ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FDatum             : TGIS_CSDatum ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FEllipsoid         : TGIS_CSEllipsoid ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FValidityExtentWGS : TGIS_Extent ;

      /// <summary>
      ///   Property internal storage.
      /// </summary>
      FValidityOriginWGS : TGIS_Point  ;

    private // projection internal values
      bParamsModified : Boolean ;

    {$IFDEF GENDOC}
    {$ELSE}
      {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
    {$ENDIF}
      bUseValidity : Boolean ;
      bDatumReady : Boolean ;

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      ///   Over-range flag.
      /// </summary>
      P_over       : Boolean ;

      /// <summary>
      ///   Internal projection value.
      ///   Geocentric latitude flag.
      /// </summary>
      P_geoc       : Boolean ;

      /// <summary>
      ///   Internal projection value.
      ///   Major axis or radius if es=0.
      /// </summary>
      P_a          : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   Eccentricity.
      /// </summary>
      P_e          : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   (e^2)
      /// </summary>
      P_es         : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///  (1 / A)
      /// </summary>
      P_ra         : Double  ;

      /// <summary>
      ///  Internal projection value.
      ///  (1 - e^2)
      /// </summary>
      P_one_es     : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   (1 / one_es)
      /// </summary>
      P_rone_es    : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   Central longitude.
      /// </summary>
      P_lam0       : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   Central latitude.
      /// </summary>
      P_phi0       : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   Easting.
      /// </summary>
      P_x0         : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///   Northing.
      /// </summary>
      P_y0         : Double  ;

      /// <summary>
      ///   Internal projection value.
      ///  General scaling factor.
      /// </summary>
      P_k0         : Double  ;

      /// <summary>
      ///   Reference shape object.
      /// </summary>
      oShp : TObject ;

    protected // property access routines

      function  fget_Description              : String  ; override;
      function  fget_IsStandard               : Boolean ;
      procedure fset_Datum                    ( const _value : TGIS_CSDatum ) ;
      function  fget_CentralMeridian          : Double ;
      procedure fset_CentralMeridian          ( const _value : Double       ) ;
      function  fget_LatitudeOfOrigin         : Double ;
      procedure fset_LatitudeOfOrigin         ( const _value : Double       ) ;
      function  fget_FalseEasting             : Double ;
      procedure fset_FalseEasting             ( const _value : Double       ) ;
      function  fget_FalseNorthing            : Double ;
      procedure fset_FalseNorthing            ( const _value : Double       ) ;
      function  fget_StandardParallel_1       : Double ;
      procedure fset_StandardParallel_1       ( const _value : Double       ) ;
      function  fget_StandardParallel_2       : Double ;
      procedure fset_StandardParallel_2       ( const _value : Double       ) ;
      function  fget_PseudoStandardParallel_1 : Double ;
      procedure fset_PseudoStandardParallel_1 ( const _value : Double       ) ;
      function  fget_Zone                     : Integer ;
      procedure fset_Zone                     ( const _value : Integer      ) ;
      function  fget_ScaleFactor              : Double ;
      procedure fset_ScaleFactor              ( const _value : Double       ) ;
      function  fget_LongitudeOfCenter        : Double ;
      procedure fset_LongitudeOfCenter        ( const _value : Double       ) ;
      function  fget_LatitudeOfCenter         : Double ;
      procedure fset_LatitudeOfCenter         ( const _value : Double       ) ;
      function  fget_Azimuth                  : Double ;
      procedure fset_Azimuth                  ( const _value : Double       ) ;
      function  fget_LongitudeOfPoint_1       : Double ;
      procedure fset_LongitudeOfPoint_1       ( const _value : Double       ) ;
      function  fget_LatitudeOfPoint_1        : Double ;
      procedure fset_LatitudeOfPoint_1        ( const _value : Double       ) ;
      function  fget_LongitudeOfPoint_2       : Double ;
      procedure fset_LongitudeOfPoint_2       ( const _value : Double       ) ;
      function  fget_LatitudeOfPoint_2        : Double ;
      procedure fset_LatitudeOfPoint_2        ( const _value : Double       ) ;
      function  fget_XScale                   : Double ;
      procedure fset_XScale                   ( const _value : Double       ) ;
      function  fget_YScale                   : Double ;
      procedure fset_YScale                   ( const _value : Double       ) ;
      function  fget_XYPlaneRotation          : Double ;
      procedure fset_XYPlaneRotation          ( const _value : Double       ) ;
      function  fget_Custom1                  : Double ;
      procedure fset_Custom1                  ( const _value : Double       ) ;
      function  fget_Custom1Name              : String ;
      function  fget_Custom2                  : Double ;
      procedure fset_Custom2                  ( const _value : Double       ) ;
      function  fget_Custom2Name              : String ;
      function  fget_Custom3                  : Double ;
      procedure fset_Custom3                  ( const _value : Double       ) ;
      function  fget_Custom3Name              : String ;
      function  fget_Custom4                  : Double ;
      procedure fset_Custom4                  ( const _value : Double       ) ;
      function  fget_Custom4Name              : String ;
      function  fget_Custom5                  : Double ;
      procedure fset_Custom5                  ( const _value : Double       ) ;
      function  fget_Custom5Name              : String ;
      function  fget_Custom6                  : Double ;
      procedure fset_Custom6                  ( const _value : Double       ) ;
      function  fget_Custom6Name              : String ;
      function  fget_Parameters               : TGIS_CSProjParameters ;
      procedure fset_Parameters               ( const _value :
                                                      TGIS_CSProjParameters ) ;

      /// <summary>
      ///   Set validity extent for the projection (in radians)
      /// </summary>
      /// <param name="_origin_x">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_x">
      ///   x zone size (+- origin)
      /// </param>
      /// <param name="_origin_y">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_y">
      ///   y zone size (+- origin)
      /// </param>
      procedure set_Validity                 ( const _origin_x   : Double ;
                                               const _zone_x     : Double ;
                                               const _origin_y   : Double ;
                                               const _zone_y     : Double
                                             ) ; overload;

      /// <summary>
      ///   Set validity extent for the projection (in radians)
      /// </summary>
      /// <param name="_origin_x">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_x">
      ///   x zone size (+- origin)
      /// </param>
      /// <param name="_origin_y">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_y">
      ///   y zone size (+- origin)
      /// </param>
      /// <param name="_zone_y_min">
      ///   minimum y value; if not specified then minimum value is not check
      /// </param>
      /// <param name="_zone_y_max">
      ///   maximum y value; if not specified then maximum value is not check
      /// </param>
      procedure set_Validity                 ( const _origin_x   : Double ;
                                               const _zone_x     : Double ;
                                               const _origin_y   : Double ;
                                               const _zone_y     : Double ;
                                               const _zone_y_min : Double ;
                                               const _zone_y_max : Double
                                             ) ; overload;

      /// <summary>
      ///   Set validity extent for the projection (in radians)
      /// </summary>
      /// <param name="_origin_x">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_x">
      ///   x zone size (+- origin)
      /// </param>
      /// <param name="_zone_x_min">
      ///   minimum x value; if not specified then minimum value is not check
      /// </param>
      /// <param name="_zone_x_max">
      ///   maximum x value; if not specified then maximum value is not check
      /// </param>
      /// <param name="_origin_y">
      ///   extent origin; should be within extent zone
      /// </param>
      /// <param name="_zone_y">
      ///   y zone size (+- origin)
      /// </param>
      /// <param name="_zone_y_min">
      ///   minimum y value; if not specified then minimum value is not check
      /// </param>
      /// <param name="_zone_y_max">
      ///   maximum y value; if not specified then maximum value is not check
      /// </param>
      procedure set_Validity                 ( const _origin_x   : Double ;
                                               const _zone_x     : Double ;
                                               const _zone_x_min : Double ;
                                               const _zone_x_max : Double ;
                                               const _origin_y   : Double ;
                                               const _zone_y     : Double ;
                                               const _zone_y_min : Double ;
                                               const _zone_y_max : Double
                                             ) ; overload;
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      {#gendoc:hide}
      function utQsfn    ( _sinphi, _e, _one_es : Double
                         ) : Double ;

      {#gendoc:hide}
      function utPhi1    ( _qs, _te, _tone_es : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAdjlon  ( _lon : Double
                         ) : Double ;

      {#gendoc:hide}
      function utEnfn    ( _es : Double
                         ) : TGIS_DoubleArray ;

      {#gendoc:hide}
      function utMlfn    ( _phi, _sphi, _cphi : Double;
                           const _en : TGIS_DoubleArray
                         ) : Double ;

      {#gendoc:hide}
      function utInv_mlfn( _arg, _es : Double ;
                           const _en : TGIS_DoubleArray
                         ) : Double ;

      {#gendoc:hide}
      function utMsfn    ( _sinphi, _cosphi, _es : Double
                         ) : Double ;

      {#gendoc:hide}
      function utTsfn    ( _phi, _sinphi, _e : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAasin   ( _v : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAacos   ( _v : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAsqrt   ( _v : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAatan2  ( _n, _d : Double
                         ) : Double ;

      {#gendoc:hide}
      function utAuthset ( _es : Double
                         ) : TGIS_DoubleArray ;

      {#gendoc:hide}
      function utAuthlat (       _beta : Double               ;
                           const _apa  : TGIS_DoubleArray
                         ) : Double ;

      {#gendoc:hide}
      function utPhi2    ( _ts, _e : Double
                         ) : Double ;

      // Utility function. Evaluate complex polynomial.
      {#gendoc:hide}
      function utZpoly1  ( const _z    : TGIS_Complex          ;
                           const _c    : array of TGIS_Complex
                         ) : TGIS_Complex ;

      //  Utility function. Evaluate complex polynomial and derivative.
      {#gendoc:hide}
      function utZpolyd1 ( const _z    : TGIS_Complex          ;
                           const _c    : array of TGIS_Complex ;
                           var _der : TGIS_Complex
                         ) : TGIS_Complex ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <summary>
      ///   Procedure will be called upon any datum change. Purpose is to
      ///   notify the derived objects.
      /// </summary>
      procedure datumChanged   ; virtual;

    protected
      /// <summary>
      ///   Procedure will be called from .Project before any projection code.
      ///   Purpose is to setup proper coordinates.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be translated into _lp_lan &amp; _lp_phi
      /// </param>
      /// <param name="_lp_lam">
      ///   longitude
      /// </param>
      /// <param name="_lp_phi">
      ///   latitude
      /// </param>
      procedure projectPre     ( const _coords : TGIS_Point3D ;
                                 var   _lp_lam : Double       ;
                                 var   _lp_phi : Double
                               ) ;

      /// <summary>
      ///   Procedure will be called from .Project after any projection code.
      ///   Purpose is to setup proper coordinates.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be translated _xy_x and _xy_y
      /// </param>
      /// <param name="_xy_x">
      ///   easting
      /// </param>
      /// <param name="_xy_y">
      ///   northing
      /// </param>
      procedure projectPost    ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D ;
                                 const _xy_x   : Double       ;
                                 const _xy_y   : Double
                               ) ;

      /// <summary>
      ///   Procedure will be called from .Unproject before any projection
      ///   code. Purpose is to setup proper coordinates.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be translated xy structure
      /// </param>
      /// <param name="_xy_x">
      ///   easting
      /// </param>
      /// <param name="_xy_y">
      ///   northing
      /// </param>
      procedure unprojectPre   ( const _coords : TGIS_Point3D ;
                                 var   _xy_x   : Double       ;
                                 var   _xy_y   : Double
                               ) ;

      /// <summary>
      ///   Procedure will be called from. Unproject after any projection code.
      ///   Purpose is to setup proper coordinates.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be translated into lp structure
      /// </param>
      /// <param name="_lp_lam">
      ///   longitude
      /// </param>
      /// <param name="_lp_phi">
      ///   latitude
      /// </param>
      procedure unprojectPost  ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D ;
                                 const _lp_lam : Double       ;
                                 const _lp_phi : Double
                               ) ;

    protected

      procedure doDestroy      ; override;

    public // public methods

      /// <inheritdoc/>
      /// <summary>
      ///   Creates an instance of the projection object and sets the default
      ///   datum to WGS84.
      /// </summary>
      constructor Create       ( const _epsg   : Integer ;
                                 const _wkt    : String
                               ) ; virtual;

      /// <summary>
      ///   Create a copy of the object.
      /// </summary>
      /// <returns>
      ///   Newly created copy of object..
      /// </returns>
      function  CreateCopy     : TGIS_CSProjAbstract ; virtual; abstract;

      /// <summary>
      ///   This function projects geodetic (latitude and longitude)
      ///   coordinates into projected (easting and northing in most cases)
      ///   coordinates, according to the current ellipsoid and projection
      ///   parameters.
      /// </summary>
      /// <param name="_coords">
      ///   geodetic coordinates (in radians)
      /// </param>
      procedure Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                               ) ; virtual;
      /// <summary>
      ///   This function unprojects the projected (easting and northing in
      ///   most cases) coordinates into unprojected geodetic (latitude and
      ///   longitude) coordinates, according to the current ellipsoid and
      ///   projection parameters.
      /// </summary>
      /// <param name="_coords">
      ///   projected coordinates (in meters)
      /// </param>
      procedure Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                               ) ; virtual;

      /// <summary>
      ///   This function projects geodetic (latitude and longitude)
      ///   coordinates into projected (easting and northing) coordinates,
      ///   according to the current ellipsoid and projection parameters.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be projected (in radians)
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function  Project        ( const _coords : TGIS_Point
                               ) : TGIS_Point ;

      /// <summary>
      ///   This function unprojects the projected (easting and northing)
      ///   coordinates into unprojected geodetic (latitude and longitude)
      ///   coordinates, according to the current ellipsoid and projection
      ///   parameters.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be unprojected (in meters)
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function  Unproject      ( const _coords : TGIS_Point
                               ) : TGIS_Point ;

      /// <summary>
      ///   This function projects geodetic (latitude and longitude)
      ///   coordinates into projected (easting and northing) coordinates,
      ///   according to the current ellipsoid and projection parameters.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be projected (in radians)
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function  Project3D      ( const _coords : TGIS_Point3D
                               ) : TGIS_Point3D ;

      /// <summary>
      ///   This function unprojects the projected (easting and northing)
      ///   coordinates into unprojected geodetic (latitude and longitude)
      ///   coordinates, according to the current ellipsoid and projection
      ///   parameters.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates to be unprojected (in meters)
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function  Unproject3D    ( const _coords : TGIS_Point3D
                               ) : TGIS_Point3D ;

    public // public properties

        /// <summary>
        ///   Is result of projection pure cylindicial (graticules are parallel).
        /// </summary>
        property IsPureCylindrical : Boolean
                                      read FIsPureCylindrical ;

        /// <summary>
        ///   Is projection revertible (so data can be projected and
        ///   unprojected).
        /// </summary>
        property IsRevertible       : Boolean
                                      read FIsRevertible ;

        /// <summary>
        ///   Does the projection work on ellipsoid.
        /// </summary>
        property IsEllipsoid        : Boolean
                                      read FIsEllipsoid  ;

        /// <summary>
        ///   Does the projection work on sphenoid.
        /// </summary>
        property IsSpheroid         : Boolean
                                      read FIsSpheroid   ;

        /// <summary>
        ///   Does the projection is grid based.
        /// </summary>
        property IsGrid             : Boolean
                                      read FIsGrid       ;

        /// <summary>
        ///   Does the projection is XYZ based.
        /// </summary>
        property IsXYZ              : Boolean
                                      read FIsXYZ        ;

        /// <summary>
        ///   Does the projection is standard (not IsGrid, not IsXYZ and
        ///   IsRevertible.
        /// </summary>
        property IsStandard         : Boolean
                                      read fget_IsStandard  ;

        /// <summary>
        ///   Validity extent of the projection (in radians). Projection should
        ///   give reasonable results within validity extent.
        /// </summary>
        property ValidityExtentWGS  : TGIS_Extent
                                      read FValidityExtentWGS ;

        /// <summary>
        ///   Origin of the validity extent of the projection (in radians).
        ///   Should be within the ValidityExtent.
        /// </summary>
        property ValidityOriginWGS  : TGIS_Point
                                      read FValidityOriginWGS ;

        /// <summary>
        ///   Additional projection description. Optional - exits only for
        ///   selected projections.
        /// </summary>
        property Description        : String
                                      read fget_Description ;
                                      {$IFDEF OXYGENE}
                                        reintroduce ;
                                      {$ENDIF}

        /// <summary>
        ///   Error number.
        /// </summary>
        /// <value>
        ///   <para>
        ///     CLEAR (0) - no errors
        ///   </para>
        ///   <para>
        ///     UNSOLVABLE (100) - projection can not be solved
        ///   </para>
        ///   <para>
        ///     WRONGPARAMS (101) - wrong projection setup improper
        ///     parameters
        ///   </para>
        ///   <para>
        ///     NOTONSPHERE (102) - projection must be constructed on sphere
        ///   </para>
        /// </value>
        /// <remarks>
        ///   <para>
        ///     The error number is only for informational purposes.
        ///     On multithread application result can be unpredictable.
        ///   </para>
        ///   <para>
        ///     To test projection validity test if result values are lower
        ///     then 1e38.
        ///   </para>
        /// </remarks>
        property Error              : Integer
                                      read FError ;

        /// <summary>
        ///   Set of supported parameters.
        /// </summary>
        property ParametersSet      : TGIS_CSProjParameterSet
                                      read FParametersSet ;

        /// <summary>
        ///   Current Datum.
        /// </summary>
        property Datum              : TGIS_CSDatum
                                      read FDatum
                                      write fset_Datum ;

        /// <summary>
        ///   Current Ellipsoid. Can be changed only by the changing the Datum
        ///   to a new one.
        /// </summary>
        property Ellipsoid          : TGIS_CSEllipsoid
                                      read FEllipsoid ;

        /// <summary>
        ///   Central Meridian. Valid for selected projections.
        /// </summary>
        property CentralMeridian    : Double
                                      read  fget_CentralMeridian
                                      write fset_CentralMeridian                ;

        /// <summary>
        ///   Origin latitude. Valid for selected projections.
        /// </summary>
        property LatitudeOfOrigin   : Double
                                      read  fget_LatitudeOfOrigin
                                      write fset_LatitudeOfOrigin               ;

        /// <summary>
        ///   False shift in meters. Valid for selected projections.
        /// </summary>
        property FalseEasting       : Double
                                      read  fget_FalseEasting
                                      write fset_FalseEasting                   ;

        /// <summary>
        ///   False shift in meters. Valid for selected projections.
        /// </summary>
        property FalseNorthing      : Double
                                      read  fget_FalseNorthing
                                      write fset_FalseNorthing                  ;

        /// <summary>
        ///   First Standard parallel. Valid for selected projections.
        /// </summary>
        property StandardParallel_1 : Double
                                      read  fget_StandardParallel_1
                                      write fset_StandardParallel_1             ;

        /// <summary>
        ///   Second Standard parallel. Valid for selected projections.
        /// </summary>
        property StandardParallel_2 : Double
                                      read  fget_StandardParallel_2
                                      write fset_StandardParallel_2             ;

        /// <summary>
        ///   First Pseudo Standard parallel. Valid for selected projections.
        /// </summary>
        property PseudoStandardParallel_1
                                    : Double
                                      read  fget_PseudoStandardParallel_1
                                      write fset_PseudoStandardParallel_1       ;

        /// <summary>
        ///   Zone. Valid for selected projections.
        /// </summary>
        property Zone               : Integer
                                      read  fget_Zone
                                      write fset_Zone                           ;

        /// <summary>
        ///   Scale factor. Valid for selected projections.
        /// </summary>
        property ScaleFactor        : Double
                                      read  fget_ScaleFactor
                                      write fset_ScaleFactor                    ;

        /// <summary>
        ///   Center Longitude. Valid for selected projections.
        /// </summary>
        property LongitudeOfCenter  : Double
                                      read  fget_LongitudeOfCenter
                                      write fset_LongitudeOfCenter              ;

        /// <summary>
        ///   Center Latitude. Valid for selected projections.
        /// </summary>
        property LatitudeOfCenter   : Double
                                      read  fget_LatitudeOfCenter
                                      write fset_LatitudeOfCenter               ;

        /// <summary>
        ///   Azimuth angle. Valid for selected projections.
        /// </summary>
        property Azimuth            : Double
                                      read  fget_Azimuth
                                      write fset_Azimuth                        ;

        /// <summary>
        ///   Longitude of the first point. Valid for selected projections.
        /// </summary>
        property LongitudeOfPoint_1 : Double
                                      read  fget_LongitudeOfPoint_1
                                      write fset_LongitudeOfPoint_1             ;

        /// <summary>
        ///   Latitude of the first point. Valid for selected projections.
        /// </summary>
        property LatitudeOfPoint_1  : Double
                                      read  fget_LatitudeOfPoint_1
                                      write fset_LatitudeOfPoint_1              ;

        /// <summary>
        ///   Longitude of the second point. Valid for selected projections.
        /// </summary>
        property LongitudeOfPoint_2 : Double
                                      read  fget_LongitudeOfPoint_2
                                      write fset_LongitudeOfPoint_2             ;

        /// <summary>
        ///   Latitude of the second point. Valid for selected projections.
        /// </summary>
        property LatitudeOfPoint_2  : Double
                                      read  fget_LatitudeOfPoint_2
                                      write fset_LatitudeOfPoint_2              ;

        /// <summary>
        ///   X Scale. Valid for selected projections.
        /// </summary>
        property XScale             : Double
                                      read  fget_XScale
                                      write fset_XScale                         ;

        /// <summary>
        ///   Y Scale. Valid for selected projections.
        /// </summary>
        property YScale             : Double
                                      read  fget_YScale
                                      write fset_YScale                         ;

        /// <summary>
        ///   XY Scale. Valid for selected projections.
        /// </summary>
        property XYPlaneRotation    : Double
                                      read  fget_XYPlaneRotation
                                      write fset_XYPlaneRotation                ;

        /// <summary>
        ///   Custom1. Valid and specific for selected projections.
        /// </summary>
        property Custom1            : Double
                                      read  fget_Custom1
                                      write fset_Custom1                        ;

        /// <summary>
        ///   Custom1Name. Valid and specific for selected projections.
        /// </summary>
        property Custom1Name        : String
                                      read  fget_Custom1Name                    ;

        /// <summary>
        ///   Custom2. Valid and specific for selected projections.
        /// </summary>
        property Custom2            : Double
                                      read  fget_Custom2
                                      write fset_Custom2                        ;

        /// <summary>
        ///   Custom2Name. Valid and specific for selected projections.
        /// </summary>
        property Custom2Name        : String
                                      read  fget_Custom2Name                    ;

        /// <summary>
        ///   Custom3. Valid and specific for selected projections.
        /// </summary>
        property Custom3            : Double
                                      read  fget_Custom3
                                      write fset_Custom3                        ;

        /// <summary>
        ///   Custom3Name. Valid and specific for selected projections.
        /// </summary>
        property Custom3Name        : String
                                      read  fget_Custom3Name                    ;

        /// <summary>
        ///   Custom4. Valid and specific for selected projections.
        /// </summary>
        property Custom4            : Double
                                      read  fget_Custom4
                                      write fset_Custom4                        ;

        /// <summary>
        ///   Custom4Name. Valid and specific for selected projections.
        /// </summary>
        property Custom4Name        : String
                                      read  fget_Custom4Name                    ;

        /// <summary>
        ///   Custom5. Valid and specific for selected projections.
        /// </summary>
        property Custom5            : Double
                                      read  fget_Custom5
                                      write fset_Custom5                        ;

        /// <summary>
        ///   Custom5Name. Valid and specific for selected projections.
        /// </summary>
        property Custom5Name        : String
                                      read  fget_Custom5Name                    ;

        /// <summary>
        ///   Custom6. Valid and specific for selected projections.
        /// </summary>
        property Custom6            : Double
                                      read  fget_Custom6
                                      write fset_Custom6                        ;

        /// <summary>
        ///   Custom6Name. Valid and specific for selected projections.
        /// </summary>
        property Custom6Name        : String
                                      read  fget_Custom6Name                    ;

        /// <summary>
        ///   All projection parameters.
        /// </summary>
        property Parameters         : TGIS_CSProjParameters
                                      read  fget_Parameters
                                      write fset_Parameters ;
  end ;
  TGIS_CSProjAbstractClass =  class of TGIS_CSProjAbstract ;

  /// <summary>
  ///   <para>
  ///     Projection Albers Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Albers Equal Area Conic
  ///     projection coordinates (easting and northing in meters) defined by
  ///     two standard parallels.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 29.5 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>default 45.5 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjAlbers = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ec     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_dd     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n2     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho0   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi2   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en     : TGIS_DoubleArray ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                  override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lambert Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lambert Equal Area Conic
  ///     projection coordinates (easting and northing in meters) defined by
  ///     standard parallel.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>if &gt; 0 then north pole case; if &lt; 0 then
  ///         south pole case</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLambert = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_CSProjAlbers )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Azimuthal Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Azimuthal Equidistant
  ///     projection coordinates (easting and northing in meters) defined by
  ///     standard parallel.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjAzimuthalEquidistant = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinph0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosph0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en     : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_M1     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_N1     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Mp     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_He     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_G      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ; override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Airy.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Airy projection coordinates
  ///     (easting and northing in meters) defined by standard parallel.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>the range from the center of the region in which
  ///         distortion is to be minimized</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjAiry = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_p_halfpi : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinph0   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosph0   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Cb       : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_am1      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode     : Integer;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Aitof.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Aitof projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjAitof = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode    : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Winkel Tripel.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Winkel Tripel projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWinkelTripel = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSProjAitof )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection August.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and August projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjAugust = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Bonne.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Bonne projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 30 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBonne = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cphi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_am1   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en    : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_south : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Bonne South Oriented.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Bonne South Oriented
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 30 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBonneSouthOriented = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjBonne )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Werner.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Werner projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWerner = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjBonne )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Cassini.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Cassini projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjCassini = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_t  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_a1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_r  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_dd : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_d2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_a2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_tn : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en : TGIS_DoubleArray ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Central Cylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Central Cylindrical
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Valid lattitudes are -80..80.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjCentralCylindrical = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Equal Area Cylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Equal Area Cylindrical
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEqualAreaCylindrical = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_qp  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_apa : TGIS_DoubleArray ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <summary>
      ///   Procedure will be called upon any datum change.
      /// </summary>
     procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Behrmann.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Behrmann projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBehrmann = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjEqualAreaCylindrical )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Gall Orthographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Gall Orthographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGallOrthographic = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjEqualAreaCylindrical )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ecket I projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_I = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert II.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Eckert II projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_II = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert III.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ecket III projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_III = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_A   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_B   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P1.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P1 projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P1 = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjEckert_III )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner VI.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner VI projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_VI = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjEckert_III )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Kavraisky VII.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Kavraisky VII projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKavraisky_VII = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSProjEckert_III )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert IV.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ecket IV projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_IV = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert V.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ecket V projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_V = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Plate Carree (Equidistant Cylindrical).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Plate Carree projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPlateCarree = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rc : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Equidistant Conic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Equidistant Conic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 20 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEquidistantConic = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en   : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rc   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Sinusoidal (Sanson-Flamsteed).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Sinusoidal projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjSinusoidal = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en   : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sphr : Boolean ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Eckert VI.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Eckert VI projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEckert_VI = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjSinusoidal )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Flat-Polar Sinusoidal (McBryde-Thomas Flat-Polar Sinusoidal).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Flat-Polar Sinusoidal
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFlatPolarSinusoidal = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_CSProjSinusoidal )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Gnomic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Gnomic projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Valid lattitudes are -80..80.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGnomic = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinph0 : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosph0 : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lambert Azimuthal Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lambert Azimuthal Equal
  ///     Area projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLambertAzimuthalEqualArea = {$IFDEF OXYGENE} public {$ENDIF}
                                         class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinb1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosb1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_xmf    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ymf    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mmf    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_qp     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_dd     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rq     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_apa    : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinph0 : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosph0 : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Larrivee.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Larrivee projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLarrivee = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lambert Conformal Conic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lambert Conformal Conic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 20 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 1</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLambertConformalConic = {$IFDEF OXYGENE} public {$ENDIF}
                                     class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_west : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lambert Conformal Conic 1SP.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lambert Conformal Conic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 1</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLambertConformalConic_1SP = {$IFDEF OXYGENE} public {$ENDIF}
                                         class( TGIS_CSProjLambertConformalConic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lambert Conformal Conic West Oriented.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lambert Conformal Conic
  ///     West Oriented projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 1</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLambertConformalConicWestOriented =
     {$IFDEF OXYGENE} public {$ENDIF}
     class( TGIS_CSProjLambertConformalConic_1SP )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Loximuthal.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Loximuthal projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLoximuthal = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_tanphi1 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Mercator.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Mercator projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMercator = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      ///   Over-range flag.
      /// </summary>
      semiMinor : Double ;

      /// <summary>
      ///   Internal projection value.
      ///   Over-range flag.
      /// </summary>
      semiMajor : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Mercator.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Mercator projection
  ///     coordinates (easting and northing in meters). This version is a
  ///     EPSG_1024 mutation of Mercator projection for pure spherical
  ///     transformations like "Google Mercator"
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMercator_1SP_Spherical = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSProjMercator )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Miller Cylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Miller projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMillerCylindrical = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Miller Oblated Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Miller Oblated
  ///     Stereographic projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMillerOblatedStereographic = {$IFDEF OXYGENE} public {$ENDIF}
                                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_zcoeff : array of TGIS_Complex ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cchio  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_schio  : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lee Oblated Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Miller Oblated
  ///     Stereographic projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLeeOblatedStereographic =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjMillerOblatedStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection GS48 (Modified Stererographics of 48 U.S).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and GS48 projection coordinates
  ///     (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     Should be used only on Clarke 1866 ellipsoid/sphere.
  ///   </note>
  ///   <note type="caution">
  ///     Projected coordinates should be +-30 degree around point 95W:39N.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGS48 =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjMillerOblatedStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection GS50 (Modified Stererographics of 50 U.S).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and GS50. projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     Should be used only on Clarke 1866 ellipsoid/sphere.
  ///   </note>
  ///   <note type="caution">
  ///     Projected coordinates should be +-30 degree around point 120W:45N.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGS50 =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjMillerOblatedStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Modified Stererographics of Alaska.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Modified Stererographics
  ///     For Alaska . projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     Should be used only on Clarke 1866 ellipsoid/sphere.
  ///   </note>
  ///   <note type="caution">
  ///     Projected coordinates should be +-30 degree around point 152W:64N.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjModifiedStererographicsForAlaska =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjMillerOblatedStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Mollweide.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Mollweide projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMollweide = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_p : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner IV.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner IV projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_IV = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjMollweide )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner V.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner V projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_V = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjMollweide )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   New Zealand Map Grid. This component provides conversions between
  ///   geodetic coordinates (latitude and longitude in radians) and New
  ///   Zealand Map Grid projection coordinates (easting and northing in
  ///   meters).
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     Should be used only on International 1924 ellipsoid.
  ///   </note>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjNewZealandMapGrid = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_bf   : array of TGIS_Complex ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_tphi : array[0..8] of Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_tpsi : array[0..9] of Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hotine Oblique Mercator Two Point.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hotine Oblique Mercator Two
  ///     Point projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_1</term>
  ///       <description>default 80 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_1</term>
  ///       <description>default 180 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_2</term>
  ///       <description>default 0 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_2</term>
  ///       <description>default 6 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHotineObliqueMercatorTwoPoint = {$IFDEF OXYGENE} public {$ENDIF}
                                             class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_alpha  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lamc   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lam1   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lam2   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi2   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_gamma  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_al     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_bl     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_el     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_singam : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosgam : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinrot : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosrot : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_u_0    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_azi    : Boolean ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hotine Oblique Mercator Azimuth.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hotine Oblique Mercator
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatngitudeOfCenter</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitidudeOfCenter</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>XYPlaneRotation</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHotineObliqueMercator =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_prj : TGIS_CSProjAbstract ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    protected

      procedure doDestroy        ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hotine Oblique Mercator Azimuth Natural Origin.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hotine Oblique Mercator
  ///     Azimuth Natural Origin projection coordinates (easting and northing
  ///     in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatngitudeOfCenter</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitidudeOfCenter</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>XYPlaneRotation</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_prj : TGIS_CSProjAbstract ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    protected

      procedure doDestroy        ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hotine Oblique Mercator Azimuth Center.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hotine Oblique Mercator
  ///     Azimuth Center projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatngitudeOfCenter</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitidudeOfCenter</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 90 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHotineObliqueMercatorAzimuthCenter =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Euler.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Euler projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEuler = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho_c  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rho_0  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sig    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c1     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c2     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Tissot.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Tissot projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjTissot = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Murdoch I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Murdoch I projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMurdoch_I = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Murdoch II.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Murdoch II projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMurdoch_II = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Murdoch III.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Murdoch III projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjMurdoch_III = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Vitkovsky I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Vitkovsky I projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjVitkovsky_I = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Perspective Conic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Perspective Conic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPerspectiveConic = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjEuler )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Stereographic projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjStereographic = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phits : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinX1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosX1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_akm1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mf    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode  : Integer ;

    private   // internal procedures

      /// <summary>
      ///   Utility function.
      /// </summary>
      function  ssfn            (       _phit   : Double     ;
                                        _sinphi : Double     ;
                                        _eccen  : Double
                                ) : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Polar Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Polar Stereographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>if &gt; 0 then north pole case; if &lt; 0 then
  ///         south pole case</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPolarStereographic = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjStereographic )

    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Polar Stereographic B.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Polar Stereographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPolarStereographic_B = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Polar Stereographic C.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Polar Stereographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPolarStereographic_C = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Universal Polar Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Universal Polar
  ///     Stereographic projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>if &gt; 0 then north pole case; if &lt; 0 then
  ///         south pole case</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjUniversalPolarStereographic =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjStereographic )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Oblique Stereographic aka Double Stereographic
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Oblique stereographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjObliqueStereographic = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phic0     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosc0     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinc0     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_r2        : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en_C      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en_K      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en_e      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en_ratexp : Double ;

    private

      /// <summary>
      ///   Utility function.
      /// </summary>
      function  srat            (      _a   : Double ;
                                       _exp : Double
                                ) : Double ;

      /// <summary>
      ///   Utility function.
      /// </summary>
      procedure gaussini        (     _e    : Double ;
                                      _phi0 : Double ;
                                  var _chi  : Double ;
                                  var _rc   : Double
                                ) ;

      /// <summary>
      ///   Utility function.
      /// </summary>
      procedure gauss           ( out _lam  : Double ;
                                  out _phi  : Double
                                ) ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Orthographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Orthographic projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjOrthographic = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinph0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosph0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Polyconic (American).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Polyconic (American)
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPolyconicAmerican = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ml0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en  : TGIS_DoubleArray ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Robinson.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Robinson projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjRobinson = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )
    private
      {$IFDEF JAVA}
        Q_X : array[0..18] of array[0..3] of Double ;
        Q_Y : array[0..18] of array[0..3] of Double ;
      {$ELSE}
        Q_X : array[0..18, 0..3] of Double ;
        Q_Y : array[0..18, 0..3] of Double ;
      {$ENDIF}

    private

      /// <summary>
      ///   Internal Project/Unproject - done to better organize code.
      /// </summary>
      /// <param name="_forward">
      ///   if True, then project; if False, then Unproject
      /// </param>
      /// <param name="_lp_lam">
      ///   longitide
      /// </param>
      /// <param name="_lp_phi">
      ///   latitude
      /// </param>
      /// <param name="_xy_x">
      ///   easting
      /// </param>
      /// <param name="_xy_y">
      ///   norting
      /// </param>
      procedure doProj           ( const _forward : Boolean ;
                                   var   _lp_lam  : Double  ;
                                   var   _lp_phi  : Double  ;
                                   var   _xy_x    : Double  ;
                                   var   _xy_y    : Double
                                 ) ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Swiss Oblique Mercator.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Swiss Oblique Mercator
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatngitudeOfCenter</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjSwissObliqueMercator = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_K     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_hlf_e : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_kR    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosp0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinp0 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Transverse Mercator - old definition.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Transverse Mercator
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  ///     <note type="caution">
  ///       Instead of this definition TGIS_CSProjTransverseMercator should be
  ///       used due to a much better precision.
  ///     </note>
  /// </remarks>
  TGIS_CSProjTransverseMercatorOld = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_esp : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ml0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en  : TGIS_DoubleArray ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Transverse Mercator.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Transverse Mercator
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  ///     <note type="caution">
  ///       This definition superceeds old (TGIS_CSProjTransverseMercatorOld)
  ///       definition due to a much better precision and revertibility,
  ///       epecially in a bigger distance to CentralMeridian.
  ///     </note>
  /// </remarks>
  TGIS_CSProjTransverseMercator = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_b : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cgb  : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cbg  : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_utg  : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_gtu  : TGIS_DoubleArray ;

    // private
      function gatg         ( const _ar    : TGIS_DoubleArray ;
                              const _b     : Double
                            ) : Double ; //inline ;
      function clens        ( const _ar    : TGIS_DoubleArray;
                              const _arg_r : Double ;
                              const _arg_i : Double;
                              var   _i     : Double
                            ) : Double ; //inline ;


    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Transverse Mercator South Oriented.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Transverse Mercator South
  ///     Oriented projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjTransverseMercatorSouthOriented =
                                  {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjTransverseMercator )
    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                  override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Universal Transverse Mercator (UTM).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Universal Transverse
  ///     Mercator projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <param name="Zone">
  ///   if 0, then will be set of first call of Projected; negative means
  ///   south; positive means north
  /// </param>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///   </list>
  /// </remarks>
  TGIS_CSProjUniversalTransverseMercator =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjTransverseMercator )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Equidistant Two Point.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Equidistant Two Point
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_1</term>
  ///       <description>default 180 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_1</term>
  ///       <description>default 80 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_2</term>
  ///       <description>default 0 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_2</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjEquidistantTwoPoint = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cp1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sp1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cp2    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sp2    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ccs    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cs     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sc     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_r2z0   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_z02    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_dlam2  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_hz0    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_thz0   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rhshz0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ca     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sa     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lp     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lamc   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection van der Grinten (I).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and van der Grinten (I)
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjVanDerGrinten_I = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection van der Grinten II.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and van der Grinten II
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjVanDerGrinten_II = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection van der Grinten III.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and van der Grinten III
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjVanDerGrinten_III = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjVanDerGrinten_II )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection van der Grinten IV.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and van der Grinten IV
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjVanDerGrinten_IV = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner II.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner II projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_II = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner III.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner III projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_III = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner VII.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Wagner VII projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_VII = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Winkel I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Winkel I projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWinkel_I = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphi1 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Winkel II.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Winkel_II projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>latitude of true scale</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWinkel_II = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphi1 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Oblique Cylindrical Equal Area (Azimuth).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Oblique Cylindrical Equal
  ///     Area projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LongitudeOfCenter</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azmiuth</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjObliqueCylindricalEqualAreaAzimuth =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjAbstract )

    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinphi : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphi : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_singam : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosgam : Double  ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Oblique Cylindrical Equal Area (Two Point).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Oblique Cylindrical Equal
  ///     Area projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_1</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_1</term>
  ///       <description>default 60 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfPoint_2</term>
  ///       <description>default 0 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfPoint_2</term>
  ///       <description>default 0 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint =
    {$IFDEF OXYGENE} public {$ENDIF}
    class( TGIS_CSProjObliqueCylindricalEqualAreaAzimuth )

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Oblated Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Oblated Equal Area
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>XScale</term>
  ///       <description>m oval</description>
  ///     </item>
  ///     <item>
  ///       <term>YScale</term>
  ///       <description>n oval</description>
  ///     </item>
  ///     <item>
  ///       <term>XYPlaneRotation</term>
  ///       <description>rotation</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjOblatedEqualArea = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_theta   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m       : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n       : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_two_r_m : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_two_r_n : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rm      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rn      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_hm      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_hn      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cp0     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sp0     : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Krovak.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Krovak projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Easting</description>
  ///     </item>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Northing</description>
  ///     </item>
  ///     <item>
  ///       <term>PseudoStandardParallel_1</term>
  ///       <description>default 78.5 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 0.9999 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 30.28813975277778 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>XScale</term>
  ///       <description>-1</description>
  ///     </item>
  ///     <item>
  ///       <term>YScale</term>
  ///       <description>1</description>
  ///     </item>
  ///     <item>
  ///       <term>XYPlaneRotation</term>
  ///       <description>90 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKrovak = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_am      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lonc    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phic    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_k1      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_kx      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ky      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinrot  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosrot  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_alph    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinam   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosam   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinphic : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_cosphic : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_u0      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_g       : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n0      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n       : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ro0     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mod     : Boolean ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_north   : Boolean ;

      Q_kx_def  : Double ;
      Q_ky_def  : Double ;
      Q_rot_def : Double ;


      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_X0  : Double =  1089000 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_Y0  : Double =   654000 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C1  : Double =  0.02946529277   ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C2  : Double =  0.02515965696   ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C3  : Double =  1.193845912E-07 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C4  : Double = -4.668270147E-07 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C5  : Double =  9.233980362E-12 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C6  : Double =  1.523735715E-12 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C7  : Double =  1.696780024E-18 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C8  : Double =  4.408314235E-18 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C9  : Double = -8.331083518E-24 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      const Q_C10 : Double = -3.689471323E-24 ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Krovak North Oriented.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Krovak North Oriented
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Easting</description>
  ///     </item>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Northing</description>
  ///     </item>
  ///     <item>
  ///       <term>PseudoStandardParallel_1</term>
  ///       <description>default 78.5 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 0.9999 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 30.28813975277778 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKrovakNorthOriented = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_CSProjKrovak )
    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Krovak Modified.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Krovak Modified projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Easting default 5000000</description>
  ///     </item>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Northing default 5000000</description>
  ///     </item>
  ///     <item>
  ///       <term>PseudoStandardParallel_1</term>
  ///       <description>default 78.5 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>0.9999 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 30.28813975277778 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfCenter</term>
  ///       <description>default 49.5 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKrovakModified = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_CSProjKrovak )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Krovak Modified North Oriented.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Krovak Modified North
  ///     Oriented projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Easting default 5000000</description>
  ///     </item>
  ///     <item>
  ///       <term>False</term>
  ///       <description>Northing default 5000000</description>
  ///     </item>
  ///     <item>
  ///       <term>PseudoStandardParallel_1</term>
  ///       <description>default 78.5 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>0.9999 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 30.28813975277778 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LongitudeOfCenter</term>
  ///       <description>default 24.83333333333333 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfCenter</term>
  ///       <description>default 49.5 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKrovakModifiedNorthOriented = {$IFDEF OXYGENE} public {$ENDIF}
                                           class( TGIS_CSProjKrovakModified )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Bacon Globular.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Bacon Globular projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBaconGlobular = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_bacn : Boolean ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_ortl : Boolean ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   Apian Globular I This component provides conversions between geodetic
  ///   coordinates (latitude and longitude in radians) and Apian Globular I
  ///   projection coordinates (easting and northing in meters).
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjApianGlobular_I = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_CSProjBaconGlobular )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Ortelius Oval.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ortelius Oval projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjOrteliusOval = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSProjBaconGlobular )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Bipolar conic of western hemisphere.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Bipolar conic of western
  ///     hemisphere projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBipolarConic = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_noskew : Boolean ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Boggs Eumorphic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Boggs Eumorphic projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjBoggsEumorphic = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Collignon Pseudocylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Collignon projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjCollignon = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Parabolic Craster.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Parabolic Craster
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjCrasterParabolic = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Fahey.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Fahey projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFahey = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Foucaut Sinusoidal.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Foucaut Sinusoidal
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFoucautSinusoidal = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n1 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Ginsburg VIII (TsNIIGAiK).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Ginsburg VIII projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGinsburg_VIII = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Gall Stereographic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Gall Stereographic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGallStereographic = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Lagrange.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Lagrange projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom1</term>
  ///       <description>parameter "W", default 1.4</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLagrange = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rw  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_hrw : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_a1  : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hammer &amp; Eckert-Greifendorff.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hammer &amp;
  ///     Eckert-Greifendorff projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHammer = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rm  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_w   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Hatano Asymmetrical Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Hatano Asymmetrical Equal
  ///     Area projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjHatano = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Laskowski.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Laskowski projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLaskowski = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Flat-Polar Quartic (McBryde-Thomas Flat-Polar Quartic).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and McBryde-Thomas Flat-Polar
  ///     Quartic projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFlatPolarQuartic = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Flat-Polar Parabolic (McBryde-Thomas Flat-Polar Parabolic).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and McBryde-Thomas Flat-Polar
  ///     Parabolic projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFlatPolarParabolic = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection McBryde-Thomas Flat-Polar Sine (No. 2).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and McBryde-Thomas Flat-Polar
  ///     Sine projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFlatPolarSine2 = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P2.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P2 projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P2 = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P3.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P3 projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P3 = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_A : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P3'.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P3' projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P3p = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjPutnins_P3 )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P4'.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P4' projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P4p = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Werenskiold I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Werenskiold_I projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWerenskiold_I = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSProjPutnins_P4p )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P5.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P5 projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P5 = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_A : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_B : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P5'.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P5' projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P5p = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjPutnins_P5 )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P6.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and PutninsP6 projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P6 = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_A   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_B   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_D   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Putnins P6'.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Putnins P6' projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjPutnins_P6p = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjPutnins_P6 )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Rectangular Polyconic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Rectangular Polyconic
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>default 45 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjRectangularPolyconic = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_fxb  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_fxa  : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Kavraisky V.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and KavraiskyV projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjKavraisky_V = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_x      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_p      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_tan_mode : Integer ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Quartic Authalic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Quartic Authalic projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjQuarticAuthalic = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_CSProjKavraisky_V )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection McBryde-Thomas Flat-Polar Sine (No. 1).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and McBryde-Thomas Flat Polar
  ///     Sine (No. 1) projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFlatPolarSine1 = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_CSProjKavraisky_V )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Prepetit Foucaut Pseudocylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Prepetit Foucaut projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjFoucautPseudocylindrical = {$IFDEF OXYGENE} public {$ENDIF}
                                        class( TGIS_CSProjKavraisky_V )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Transverse Central Cylindrical.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Transverse Central
  ///     Cylindrical projection coordinates (easting and northing in
  ///     meters).=
  ///   </para>
  /// </summary>
  TGIS_CSProjTransverseCentralCylindrical = {$IFDEF OXYGENE} public {$ENDIF}
                                            class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Transverse Cylindrical Equal Area.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Transverse Cylindrical
  ///     Equal Area projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjTransverseCylindricalEqualArea = {$IFDEF OXYGENE} public {$ENDIF}
                                              class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rk0 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Urmaev V.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Urmaev V projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom1</term>
  ///       <description>parameter "n", default 0.8</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom1</term>
  ///       <description>parameter "q", default 1/3</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom3</term>
  ///       <description>parameter "alpha", default 0.5</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjUrmaev_V = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_m   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rmn : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_q3  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n   : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Urmaev Flat-Polar Sinusoidal.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Urmaev Flat-Polar
  ///     Sinusoidal projection coordinates (easting and northing in meters)
  ///     defined by two standard parallels.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>ScaleFactor</term>
  ///       <description>default 0.7</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjUrmaevFlatPolarSinusoidal = {$IFDEF OXYGENE} public {$ENDIF}
                                         class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C_y  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_n    : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Wagner I.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Urmaev Wagner I projection
  ///     coordinates (easting and northing in meters) defined by two
  ///     standard parallels.
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjWagner_I = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_CSProjUrmaevFlatPolarSinusoidal )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Nell.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Nell projection coordinates
  ///     (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjNell = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Nell-Hammer.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Nell-Hammer projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjNellHammer = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Nicolosi Globular.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Nicolosi Globular
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjNicolosiGlobular = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_CSProjAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Goode Homolosine.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Goode Homolosine projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjGoodeHomolosine = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sinu : TGIS_CSProjSinusoidal ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_moll : TGIS_CSProjMollweide  ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    protected

      procedure doDestroy        ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection International Map of the World Polyconic.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and International Map of the
  ///     World Polyconic projection coordinates (easting and northing in
  ///     meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_1</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>StandardParallel_2</term>
  ///       <description>supported</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjInternationalMapWorldPolyconic = {$IFDEF OXYGENE} public {$ENDIF}
                                              class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_P      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Pp     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Q      : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Qp     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_R_1    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_R_2    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sphi_1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_sphi_2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C2     : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi_1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_phi_2  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_lam_1  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_en     : TGIS_DoubleArray ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_mode   : Double ;

    private

      /// <summary>
      ///   Procedure will be called upon any datum change.
      /// </summary>
      procedure locFor           ( var   _yc     : Double ;
                                   const _lp_lam : Double ;
                                   const _lp_phi : Double ;
                                   var   _xy_x   : Double ;
                                   var   _xy_y   : Double
                                 )  ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   Helper record for Chamberlin Trimetric.
  /// </summary>
  TGIS_CSProjChamberlinTrimetric_Record1 = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Internal projection value.
    /// </summary>
    phi    : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    lam    : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    cosphi : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    sinphi : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    v_r    : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    v_az   : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    p_x    : Double ;

    /// <summary>
    ///   Internal projection value.
    /// </summary>
    p_y    : Double ;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Chamberlin Trimetric.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Chamberlin Trimetric
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom1</term>
  ///       <description>parameter "phi[0]", default 55 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom2</term>
  ///       <description>parameter "lam[0]", default 150 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom3</term>
  ///       <description>parameter "phi[1]", default 55 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom4</term>
  ///       <description>parameter "lam[1]", default 35 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom5</term>
  ///       <description>parameter "phi[2]", default 15 deg</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom6</term>
  ///       <description>parameter "lam[2]", default 92.5 deg</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjChamberlinTrimetric = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_c      : Array [0..2] of TGIS_CSProjChamberlinTrimetric_Record1 ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_p_x    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_p_y    : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_beta_0 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_beta_1 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_beta_2 : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rv_r   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rv_az  : Double ;

    private

      /// <summary>
      ///   Procedure for internal use only
      /// </summary>
      procedure    vect         (       _dphi  : Double   ;
                                        _c1    : Double   ;
                                        _s1    : Double   ;
                                        _c2    : Double   ;
                                        _s2    : Double   ;
                                        _dlam  : Double
                                ) ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

  end ;

  /// <summary>
  ///   <para>
  ///     Projection Laborde Special for Madagaskar.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Laborde projection
  ///     coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>default 46°26'14"</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>default -18°54'</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Azimuth</term>
  ///       <description>default 18°54'</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLabordeMadagaskar = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSProjAbstract )
    private // projection internal values
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_A   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_C   : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_p0s : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Ca  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Cb  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Cc  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_Cd  : Double ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_rot : Integer ;

      /// <summary>
      ///   Internal projection value.
      /// </summary>
      Q_kRg : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Space Oblique Mercator for Landsat
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and Space oblique for LANDSAT
  ///     projection coordinates (easting and northing in meters).
  ///   </para>
  /// </summary>
  /// <remarks>
  ///   Parameters, which can be set for the projection:
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Parameter</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>CentralMeridian</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>LatitudeOfOrigin</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseEasting</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>FalseNorthing</term>
  ///       <description>supported</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom1</term>
  ///       <description>parameter "land", default 1</description>
  ///     </item>
  ///     <item>
  ///       <term>Custom2</term>
  ///       <description>parameter "path", default 1</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_CSProjLandsat = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_CSProjAbstract )
      Q_a2   : Double ;
      Q_a4   : Double ;
      Q_b    : Double ;
      Q_c1   : Double ;
      Q_c3   : Double ;
      Q_q    : Double ;
      Q_t    : Double ;
      Q_u    : Double ;
      Q_w    : Double ;
      Q_p22  : Double ;
      Q_sa   : Double ;
      Q_ca   : Double ;
      Q_xj   : Double ;
      Q_rlm  : Double ;
      Q_rlm2 : Double ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;


      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Base class for string grids systems.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and MGRS coordinates string.
  ///   </para>
  /// </summary>
  TGIS_CSProjGridAbstract = {$IFDEF OXYGENE} public abstract{$ENDIF}
                            class( TGIS_CSProjAbstract )
    public // public methods
      /// <summary>
      ///   This function projects geodetic (latitude and longitude)
      ///   coordinates into projected (string) coordinates, according to
      ///   the current ellipsoid and projection parameters. Default precision
      ///   is 5.
      /// </summary>
      /// <param name="_coords">
      ///   geodetic coordinates (in radians)
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function ProjectedTxt  ( const _coords : TGIS_Point
                             ) :  String ; overload; virtual; abstract;
      /// <summary>
      ///   This function projects geodetic (latitude and longitude)
      ///   coordinates into projected (string) coordinates, according to
      ///   the current ellipsoid and projection parameters.
      /// </summary>
      /// <param name="_coords">
      ///   geodetic coordinates (in radians)
      /// </param>
      /// <param name="_prec">
      ///   precision (0..5); 5 is default
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function ProjectedTxt  ( const _coords : TGIS_Point ;
                               const _prec   : Integer
                             ) :  String ; overload; virtual; abstract;
      /// <summary>
      ///   This function unprojects the projected (string) coordinates
      ///   into unprojected geodetic (latitude and longitude) coordinates,
      ///   according to the current ellipsoid and projection parameters.
      /// </summary>
      /// <param name="_coords">
      ///   string coordinate
      /// </param>
      /// <returns>
      ///   converted coordinate
      /// </returns>
      function UnprojectedTxt( const _coords : String
                             ) : TGIS_Point ; virtual; abstract;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Military Reference Grid System (MGRS).
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and MGRS coordinates string.
  ///   </para>
  /// </summary>
  TGIS_CSProjMGRS = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_CSProjGridAbstract )
    private
      Q_utm : TGIS_CSProjUniversalTransverseMercator ;
      Q_ups : TGIS_CSProjUniversalPolarStereographic ;

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    protected

      procedure doDestroy    ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create     ( const _epsg   : Integer    ;
                               const _wkt    : String
                             ) ; override;

      /// <inheritdoc/>
      function CreateCopy    : TGIS_CSProjAbstract ;
                             override;

      /// <inheritdoc/>
      function ProjectedTxt  ( const _coords : TGIS_Point
                             ) :  String ; overload; override;

      /// <inheritdoc/>
      function ProjectedTxt  ( const _coords : TGIS_Point ;
                               const _prec   : Integer
                             ) :  String ; overload; override;

      /// <inheritdoc/>
      function UnprojectedTxt( const _coords : String
                             ) : TGIS_Point ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection F-16 Grid Reference System.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and F16GRS coordinates string.
  ///   </para>
  /// </summary>
  TGIS_CSProjF16GRS = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjMGRS )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function CreateCopy    : TGIS_CSProjAbstract ;
                             override;

      /// <inheritdoc/>
      function ProjectedTxt  ( const _coords : TGIS_Point
                             ) :  String ; overload; override;

      /// <inheritdoc/>
      function ProjectedTxt  ( const _coords : TGIS_Point ;
                               const _prec   : Integer
                             ) :  String ; overload; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection GEOREF.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and GEOREF string.
  ///   </para>
  /// </summary>
  TGIS_CSProjGEOREF = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_CSProjGridAbstract )

    // various protected routines
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected

      /// <inheritdoc/>
      procedure   datumChanged   ; override;

    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      function    ProjectedTxt   ( const _coords : TGIS_Point
                                 ) :  String ; overload; override;

      /// <inheritdoc/>
      function    ProjectedTxt   ( const _coords : TGIS_Point ;
                                   const _prec   : Integer
                                 ) :  String ; overload; override;

      /// <inheritdoc/>
      function    UnprojectedTxt ( const _coords : String
                                 ) : TGIS_Point ; override;
  end ;

  /// <summary>
  ///   <para>
  ///     Projection Geocentric.
  ///   </para>
  ///   <para>
  ///     This component provides conversions between geodetic coordinates
  ///     (latitude and longitude in radians) and geocentric coordinate (XYZ).
  ///   </para>
  /// </summary>
  TGIS_CSProjGeocentric = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_CSProjAbstract )
    public // public methods

      /// <inheritdoc/>
      constructor Create         ( const _epsg   : Integer    ;
                                   const _wkt    : String
                                 ) ; override;

      /// <inheritdoc/>
      function    CreateCopy     : TGIS_CSProjAbstract ;
                                 override;

      /// <inheritdoc/>
      procedure   Project3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure   Unproject3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                 ) ; override;
  end ;

  /// <summary>
  ///   List of all projections.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSProjList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSProjAbstract}
  TGIS_CSProjList = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

    protected // property access routines

      function    fget_Projection( _idx     : Integer
                                 ) : TGIS_CSProjAbstract ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_prj">
      ///   projection object
      /// </param>
      /// <param name="_master">
      ///   if &gt; 0 then EPSG code of the master item (for creating alias)
      /// </param>
      procedure   Add        ( const _prj    : TGIS_CSProjAbstract ;
                               const _master : Integer
                             ) ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function    ByEPSG     ( const _epsg   : Integer
                             ) : TGIS_CSProjAbstract ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function    ByWKT      ( const _wkt    : String
                             ) : TGIS_CSProjAbstract ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure   Init       ; override;

    public // public properites

        /// <summary>
        ///   Default property for retrieving Projection.
        /// </summary>
        /// <param name="_idx">
        ///   position on the list
        /// </param>
        property Projection[ _idx : Integer ] : TGIS_CSProjAbstract
                                                read fget_Projection ;
                                                default ;
  end ;

const
  // EPSG/CODE for Projections

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Popular_Visualisation_Pseudo_Mercator          =   1024 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Azimuthal_Equal_Area_Spherical         =   1027 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equidistant_Cylindrical                        =   1028 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equidistant_Cylindrical_Spherical              =   1029 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Krovak_North_Oriented                          =   1041 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Krovak_Modified                                =   1042 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Krovak_Modified_North_Oriented                 =   1043 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Conformal_Conic_1SP                    =   9801 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Conformal_Conic_2SP                    =   9802 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Conformal_Conic_2SP_Belgium            =   9803 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator_1SP                                   =   9804 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator                                       =   9805 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Cassini_Soldner                                =   9806 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Transverse_Mercator                            =   9807 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Transverse_Mercator_South_Oriented             =   9808 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Oblique_Stereographic                          =   9809 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Polar_Stereographic                            =   9810 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_New_Zealand_Map_Grid                           =   9811 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hotine_Oblique_Mercator_Azimuth_Natural_Origin =   9812 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Swiss_Oblique_Mercator                         =   9814 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hotine_Oblique_Mercator_Variant_B              =   9815 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Polyconic                                      =   9818 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Near_Conformal_Conic                   =   9817 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Krovak                                         =   9819 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Azimuthal_Equal_Area                           =   9820 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Azimuthal_Equal_Area                   =   9821 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Albers                                         =   9822 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Plate_Carree                                   =   9823 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Conformal_Conic_West_Oriented          =   9826 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Bonne                                          =   9827 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Bonne_South_Oriented                           =   9828 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Polar_Stereographic_B                          =   9829 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Polar_Stereographic_C                          =   9830 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator_1SP_Spherical                         =   9841 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Albers_Conic_Equal_Area                        = 810001 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert                                        = 810002 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Azimuthal_Equidistance                         = 810003 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Airy                                           = 810004 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Aitof                                          = 810005 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Winkel_Tripel                                  = 810006 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_August_Epicycloidal                            = 810007 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Werner                                         = 810009 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Central_Cylindrical                            = 810011 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Cylindrical_Equal_Area                         = 810012 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Behrmann                                       = 810013 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Gall_Orthographic                              = 810014 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_I                                       = 810015 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_II                                      = 810016 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_III                                     = 810017 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P1                                     = 810018 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_VI                                      = 810019 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Kavraisky_VII                                  = 810020 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_IV                                      = 810021 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_V                                       = 810022 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equidistant_Conic                              = 810024 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Sinusoidal                                     = 810025 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Eckert_VI                                      = 810026 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Flat_Polar_Sinusoidal                          = 810027 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Gnomic                                         = 810028 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Larrivee                                       = 810030 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lambert_Conformal_Conic                        = 810031 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Loximuthal                                     = 810032 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator_2SP                                   = 810033 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Miller_Cylindrical                             = 810034 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Miller_Oblated_Stereographic                   = 810035 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lee_Oblated_Stereographic                      = 810036 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_GS48                                           = 810037 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_GS50                                           = 810038 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Modified_Stererographics_For_Alaska            = 810039 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mollweide                                      = 810040 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_IV                                      = 810041 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_V                                       = 810042 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hotine_Oblique_Mercator_Two_Point              = 810044 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hotine_Oblique_Mercator                        = 810045 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Euler                                          = 810047 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Tissot                                         = 810048 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Murdoch_I                                      = 810049 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Murdoch_II                                     = 810050 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Murdoch_III                                    = 810051 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Vitkovsky_I                                    = 810052 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Perspective_Conic                              = 810053 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Stereographic                                  = 810054 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Universal_Polar_Stereographic                  = 810056 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Double_Stereographic                           = 810057 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Orthographic                                   = 810058 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Polyconic_American                             = 810059 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Robinson                                       = 810060 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Oblique_Mercator                               = 810061 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Gauss_Kruger                                   = 810062;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Universal_Transverse_Mercator                  = 810063 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equidistant_Two_Point                          = 810064 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_van_der_Grinten_I                              = 810065 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_van_der_Grinten_II                             = 810066 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_van_der_Grinten_III                            = 810067 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_van_der_Grinten_IV                             = 810068 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_II                                      = 810069 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_III                                     = 810070 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_VII                                     = 810071 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Winkel_I                                       = 810072 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Winkel_II                                      = 810073 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Oblique_Cylindrical_Equal_Area_Azimuth         = 810074 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Oblique_Cylindrical_Equal_Area_Area_Two_Point  = 810075 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Oblated_Equal_Area                             = 810076 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Bacon_Globular                                 = 810078 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Apian_Globular_I                               = 810079 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Ortelius_Oval                                  = 810080 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Bipolar_Conic                                  = 810081 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Boggs_Eumorphic                                = 810082 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Collignon                                      = 810083 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Craster_Parabolic                              = 810084 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Fahey                                          = 810085 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Foucaut_Sinusoidal                             = 810086 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Ginsburg_VIII                                  = 810087 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Gall_Stereographic                             = 810088 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Lagrange                                       = 810089 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hammer                                         = 810090 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hatano                                         = 810091 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Laskowski                                      = 810092 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Flat_Polar_Quartic                             = 810093 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Flat_Polar_Parabolic                           = 810094 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Flat_Polar_Sine_2                              = 810095 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P2                                     = 810096 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P3                                     = 810097 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P3p                                    = 810098 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P4p                                    = 810099 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Werenskiold_I                                  = 810100 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P5                                     = 810101 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P5p                                    = 810102 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P6                                     = 810103 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Putnins_P6p                                    = 810104 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Rectangular_Polyconic                          = 810105 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Kavraisky_V                                    = 810106 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Quartic_Authalic                               = 810107 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Flat_Polar_Sine_1                              = 810108 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Foucaut_Pseudocylindrical                      = 810109 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Transverse_Central_Cylindrical                 = 810110 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Transverse_Cylindrical_Equal_Area              = 810111 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Urmaev_V                                       = 810112 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Urmaev_Flat_Polar_Sinusoidal                   = 810113 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Wagner_I                                       = 810114 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Nell                                           = 810115 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Nell_Hammer                                    = 810116 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Nicolosi_Globular                              = 810117 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Goode_Homolosine                               = 810118 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_International_Map_World_Polyconic              = 810119 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Chamberlin_Trimetric                           = 810120 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Laborde_Madagaskar                             = 810121 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Landsat                                        = 810122 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_MGRS                                           = 810123 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_F16GRS                                         = 810124 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_GEOREF                                         = 810125 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equal_Area_Cylindrical                         = 810126 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator_1SP_Google                            = 810127 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Kavraisky_VI                                   = 810128 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Geocentric                                     = 810129 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Rectified_Skew_Orthomorphic_Natural_Origin     = 810130 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Equirectangular                                = 810131 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Hotine_Oblique_Mercator_Azimuth_Center         = 810132 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Mercator_Auxiliary_Sphere                      = 810133 ;

  /// <summary>
  ///   Unique code of projection. EPSG whenever possible.
  /// </summary>
  CSPROJ_Transverse_Mercator_Old                        = 810134 ;

const
  /// <summary>
  ///   Projection error code.
  ///   All OK.
  /// </summary>
  ERROR_CLEAR       =   0 ;

  /// <summary>
  ///   Projection error code.
  ///   Projection can not be solved.
  /// </summary>
  ERROR_UNSOLVABLE  = 100 ;

  /// <summary>
  ///   Projection error code.
  ///   Wrong projection setup.
  /// </summary>
  ERROR_WRONGPARAMS = 101 ;

  /// <summary>
  ///   Projection error code.
  ///   Projection must be constructed on ellipsoid.
  /// </summary>
  ERROR_NOTONSPHERE = 102 ;

  /// <summary>
  ///   Projection error code.
  ///   Bounding polygon can not be created.
  /// </summary>
  ERROR_NOBOUNDING  = 103 ;

  /// <summary>
  ///   List of all projections.
  /// </summary>
  /// <returns>
  ///   Global list of all projections.
  /// </returns>
  function CSProjList : TGIS_CSProjList ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gppCentralMeridian          = TGIS_CSProjParameter.CentralMeridian          ;
      gppLatitudeOfOrigin         = TGIS_CSProjParameter.LatitudeOfOrigin         ;
      gppFalseEasting             = TGIS_CSProjParameter.FalseEasting             ;
      gppFalseNorthing            = TGIS_CSProjParameter.FalseNorthing            ;
      gppStandardParallel_1       = TGIS_CSProjParameter.StandardParallel_1       ;
      gppStandardParallel_2       = TGIS_CSProjParameter.StandardParallel_2       ;
      gppPseudoStandardParallel_1 = TGIS_CSProjParameter.PseudoStandardParallel_1 ;
      gppZone                     = TGIS_CSProjParameter.Zone                     ;
      gppScaleFactor              = TGIS_CSProjParameter.ScaleFactor              ;
      gppLongitudeOfCenter        = TGIS_CSProjParameter.LongitudeOfCenter        ;
      gppLatitudeOfCenter         = TGIS_CSProjParameter.LatitudeOfCenter         ;
      gppAzimuth                  = TGIS_CSProjParameter.Azimuth                  ;
      gppLongitudeOfPoint_1       = TGIS_CSProjParameter.LongitudeOfPoint_1       ;
      gppLatitudeOfPoint_1        = TGIS_CSProjParameter.LatitudeOfPoint_1        ;
      gppLongitudeOfPoint_2       = TGIS_CSProjParameter.LongitudeOfPoint_2       ;
      gppLatitudeOfPoint_2        = TGIS_CSProjParameter.LatitudeOfPoint_2        ;
      gppXScale                   = TGIS_CSProjParameter.XScale                   ;
      gppYScale                   = TGIS_CSProjParameter.YScale                   ;
      gppXYPlaneRotation          = TGIS_CSProjParameter.XYPlaneRotation          ;

      gisProjectionTypeMiscellaneous      = TGIS_CSProjType.Miscellaneous      ;
      gisProjectionTypeAzimuthal          = TGIS_CSProjType.Azimuthal          ;
      gisProjectionTypeAzimuthalModified  = TGIS_CSProjType.AzimuthalModified  ;
      gisProjectionTypeConical            = TGIS_CSProjType.Conical            ;
      gisProjectionTypeCylindrical        = TGIS_CSProjType.Cylindrical        ;
      gisProjectionTypePseudocylindrical  = TGIS_CSProjType.Pseudocylindrical  ;

  {$ENDIF}


  var
    /// <summary>
    ///   Checking projection validity.
    ///   If Ture (Default) will box projections within ValidityExtentWGS.
    /// </summary>
    CSProjUseValidity : Boolean = True ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}

type
  T_ChamberlinTrimetric_Record2 = record
    r  : Double ;
    az : Double ;
  end ;

  { MGRS alphabet for TGIS_ProjectionMGRS. }
  T_MGRS = class
    const mgrsA = 01 ;
    const mgrsB = 02 ;
    const mgrsC = 03 ;
    const mgrsD = 04 ;
    const mgrsE = 05 ;
    const mgrsF = 06 ;
    const mgrsG = 07 ;
    const mgrsH = 08 ;
    const mgrsJ = 09 ;
    const mgrsK = 10 ;
    const mgrsL = 11 ;
    const mgrsM = 12 ;
    const mgrsN = 13 ;
    const mgrsP = 14 ;
    const mgrsQ = 15 ;
    const mgrsR = 16 ;
    const mgrsS = 17 ;
    const mgrsT = 18 ;
    const mgrsU = 19 ;
    const mgrsV = 20 ;
    const mgrsW = 21 ;
    const mgrsX = 22 ;
    const mgrsY = 23 ;
    const mgrsZ = 24 ;
  end ;

const
  EULER        = 0 ;
  MURD1        = 1 ;
  MURD2        = 2 ;
  MURD3        = 3 ;
  PCONIC       = 4 ;
  TISSOT       = 5 ;
  VITK1        = 6 ;

  DEGREE10     = 10.0 /180 * Pi ;
  DEGREE5      = 5.0 /180 * Pi ;
  DEGREE1      = 1.0 /180 * Pi ;
  EPS7         = 1E-7  ;
  EPS9         = 1E-10 ;
  EPS10        = 1E-10 ;
  EPS12        = 1E-12 ;
  AEPS         = 1E-50 ;
  ONETOL6      = 1.000001 ;
  ONETOL8      = 1.00000001 ;
  ONETOL13     = 1.00000000000001;
  N_POLE       = 0 ;
  S_POLE       = 1 ;
  EQUIT        = 2 ;
  OBLIQ        = 3 ;

  ZONE_180     = 179.99 * Pi / 180 ;
  ZONE_170     = 169.99 * Pi / 180 ;
  ZONE_160     = 159.99 * Pi / 180 ;
  ZONE_90      =  89.99 * Pi / 180 ;
  ZONE_87      =  86.99 * Pi / 180 ;
  ZONE_85      =  84.99 * Pi / 180 ;
  ZONE_80      =  79.99 * Pi / 180 ;
  ZONE_70      =  69.99 * Pi / 180 ;
  ZONE_60      =  59.99 * Pi / 180 ;
  ZONE_45      =  44.99 * Pi / 180 ;
  ZONE_35      =  34.99 * Pi / 180 ;
  ZONE_20      =  19.99 * Pi / 180 ;
  ZONE_10      =   9.99 * Pi / 180 ;
  ZONE_0       =   0.01 * Pi / 180 ;

  IS_ANAL_XL_YL = 01  ; // derivatives of lon analytic
  IS_ANAL_XP_YP = 02  ; // derivatives of lat analytic
  IS_ANAL_HK    = 04  ; // h and k analytic
  IS_ANAL_CONV  = 010 ; // convergence analytic

var
  cs_ProjList : TGIS_CSProjList = nil ;

  function InCSProjParameterSet(
    const _val : TGIS_CSProjParameter   ;
    const _set : TGIS_CSProjParameterSet
  ) : Boolean ;
  begin
    Result := _val in _set ;
  end;


  function GisComplex( _r, _i : Double ) : TGIS_Complex ;
  begin
    Result.r := _r ;
    Result.i := _i ;
  end;

const
  PARAM_First                    =  1 ;
  PARAM_CentralMeridian          =  1 ;
  PARAM_LatitudeOfOrigin         =  2 ;
  PARAM_FalseEasting             =  3 ;
  PARAM_FalseNorthing            =  4 ;
  PARAM_StandardParallel_1       =  5 ;
  PARAM_StandardParallel_2       =  6 ;
  PARAM_PseudoStandardParallel_1 =  7 ;
  PARAM_Zone                     =  9 ;
  PARAM_ScaleFactor              = 10 ;
  PARAM_LongitudeOfCenter        = 11 ;
  PARAM_LatitudeOfCenter         = 12 ;
  PARAM_Azimuth                  = 13 ;
  PARAM_LongitudeOfPoint_1       = 14 ;
  PARAM_LatitudeOfPoint_1        = 15 ;
  PARAM_LongitudeOfPoint_2       = 16 ;
  PARAM_LatitudeOfPoint_2        = 17 ;
  PARAM_XScale                   = 19 ;
  PARAM_YScale                   = 20 ;
  PARAM_XYPlaneRotation          = 21 ;
  PARAM_Custom1                  = 22 ;
  PARAM_Custom2                  = 23 ;
  PARAM_Custom3                  = 24 ;
  PARAM_Custom4                  = 25 ;
  PARAM_Custom5                  = 26 ;
  PARAM_Custom6                  = 27 ;
  PARAM_Last                     = 27 ;

//------------------------------------------------------------------------------
// TGIS_CSProjParametersInternal
//------------------------------------------------------------------------------

  function TGIS_CSProjParametersInternal.get_Parameter(
    const _id : Byte
  ) : Double ;
  var
    i : Integer ;
  begin
    Result := 0 ;

    for i := low( ar ) to high( ar ) do begin

      if ar[ i ].Id = _id then begin
        Result := ar[ i ].Value ;
        exit ;
      end;
    end;

  end;

  procedure TGIS_CSProjParametersInternal.set_Parameter(
    const _id    : Byte   ;
    const _value : Double
  ) ;
  var
    i : Integer ;

    procedure delete_parameter( const _idx : Integer ) ;
    var
      ii : Integer ;
    begin
      for ii := _idx + 1 to high( ar ) do begin
        ar[ ii - 1 ] := ar[ ii ] ;
      end;

      SetLength( ar, length( ar ) - 1 ) ;
    end;

  begin

    for i := low( ar ) to high( ar ) do begin

      if ar[ i ].Id = _id then begin
        if not bLock then begin
          if IsNan( _value ) then begin
            delete_parameter( i ) ;
            exit ;
          end;
          if _value = 0 then begin
            delete_parameter( i ) ;
            exit ;
          end;
        end ;

        ar[ i ].Value := _value ;
        exit ;
      end;
    end;

    assert( not bLock ) ;

    if IsNan( _value ) then begin
      exit ;
    end;
    if _value = 0 then begin
      exit ;
    end;

    SetLength( ar, length( ar ) + 1 ) ;
    i := high(ar) ;
    {$IFDEF GIS_NORECORDS}
      ar[ i ] := new TGIS_CSProjParametersInternalElement ;
    {$ENDIF}
    ar[ i ].Id    := _id ;
    ar[ i ].Value := _value ;
  end;


  function TGIS_CSProjParametersInternal.get_ParameterName(
    const _id : Byte
  ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;

    for i := low( ar ) to high( ar ) do begin

      if ar[ i ].Id = _id then begin
        Result := ar[ i ].CustomName ;
        exit ;
      end;
    end;

  end;

  procedure TGIS_CSProjParametersInternal.set_ParameterName(
    const _id    : Byte   ;
    const _value : String
  ) ;
  var
    i : Integer ;
  begin
    for i := low( ar ) to high( ar ) do begin

      if ar[ i ].Id = _id then begin
        ar[ i ].CustomName := _value ;
        exit ;
      end;
    end;

    assert( not bLock ) ;

    SetLength( ar, length( ar ) + 1 ) ;
    i := high(ar) ;
    {$IFDEF GIS_NORECORDS}
      ar[ i ] := new TGIS_CSProjParametersInternalElement ;
    {$ENDIF}
    ar[ i ].Id    := _id ;
    ar[ i ].Value := NaN ;
    ar[ i ].CustomName := _value ;
  end;


  function TGIS_CSProjParametersInternal.fget_CentralMeridian
    : Double  ;
  begin
    Result := get_Parameter( PARAM_CentralMeridian ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_CentralMeridian(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_CentralMeridian, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LatitudeOfOrigin
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LatitudeOfOrigin ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LatitudeOfOrigin(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LatitudeOfOrigin, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_FalseEasting
    : Double  ;
  begin
    Result := get_Parameter( PARAM_FalseEasting ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_FalseEasting(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_FalseEasting, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_FalseNorthing
    : Double  ;
  begin
    Result := get_Parameter( PARAM_FalseNorthing ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_FalseNorthing(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_FalseNorthing, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_StandardParallel_1
    : Double  ;
  begin
    Result := get_Parameter( PARAM_StandardParallel_1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_StandardParallel_1(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_StandardParallel_1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_StandardParallel_2
    : Double  ;
  begin
    Result := get_Parameter( PARAM_StandardParallel_2 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_StandardParallel_2(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_StandardParallel_2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_PseudoStandardParallel_1
    : Double  ;
  begin
    Result := get_Parameter( PARAM_PseudoStandardParallel_1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_PseudoStandardParallel_1(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_PseudoStandardParallel_1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Zone
    : Integer ;
  begin
    Result := RoundS( get_Parameter( PARAM_Zone ) ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Zone(
    const _value : Integer
  ) ;
  begin
    set_Parameter( PARAM_Zone, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_ScaleFactor
    : Double  ;
  begin
    Result := get_Parameter( PARAM_ScaleFactor ) ;
    if Result = 0 then
      Result := 1 ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_ScaleFactor(
    const _value : Double
  ) ;
  begin
    if IsNan( _value ) or ( _value = 1 ) then begin
      // delete default value; getScaleFactor will revert this
      set_Parameter( PARAM_ScaleFactor, 0      )
    end
    else begin
      set_Parameter( PARAM_ScaleFactor, _value )
    end;
  end;

  function TGIS_CSProjParametersInternal.fget_LongitudeOfCenter
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LongitudeOfCenter ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LongitudeOfCenter(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LongitudeOfCenter, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LatitudeOfCenter
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LatitudeOfCenter ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LatitudeOfCenter(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LatitudeOfCenter, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Azimuth
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Azimuth ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Azimuth(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Azimuth, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LongitudeOfPoint_1
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LongitudeOfPoint_1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LongitudeOfPoint_1(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LongitudeOfPoint_1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LatitudeOfPoint_1
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LatitudeOfPoint_1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LatitudeOfPoint_1(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LatitudeOfPoint_1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LongitudeOfPoint_2
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LongitudeOfPoint_2 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LongitudeOfPoint_2(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_LongitudeOfPoint_2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_LatitudeOfPoint_2
    : Double  ;
  begin
    Result := get_Parameter( PARAM_LatitudeOfPoint_2 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_LatitudeOfPoint_2(
    const _value : Double
  ) ;
  begin
   set_Parameter( PARAM_LatitudeOfPoint_2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_XScale
    : Double  ;
  begin
    Result := get_Parameter( PARAM_XScale ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_XScale(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_XScale, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_YScale
    : Double  ;
  begin
    Result := get_Parameter( PARAM_YScale ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_YScale(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_YScale, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_XYPlaneRotation
    : Double  ;
  begin
    Result := get_Parameter( PARAM_XYPlaneRotation ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_XYPlaneRotation(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_XYPlaneRotation, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom1
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom1(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom1Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom1Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom1, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom2
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom2 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom2(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom2Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom2 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom2Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom3
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom3 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom3(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom3, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom3Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom3 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom3Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom3, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom4
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom4 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom4(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom4, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom4Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom4Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom4, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom5
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom5 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom5(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom5, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom5Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom1 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom5Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom2, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom6
    : Double  ;
  begin
    Result := get_Parameter( PARAM_Custom6 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom6(
    const _value : Double
  ) ;
  begin
    set_Parameter( PARAM_Custom6, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_Custom6Name
    : String ;
  begin
    Result := get_ParameterName( PARAM_Custom5 ) ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_Custom6Name(
    const _value : String
  ) ;
  begin
    set_ParameterName( PARAM_Custom6, _value ) ;
  end;

  function TGIS_CSProjParametersInternal.fget_All
    : TGIS_CSProjParameters ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_CSProjParameters ;
    {$ENDIF}
    Result.CentralMeridian          := CentralMeridian          ;
    Result.LatitudeOfOrigin         := LatitudeOfOrigin         ;
    Result.FalseEasting             := FalseEasting             ;
    Result.FalseNorthing            := FalseNorthing            ;
    Result.StandardParallel_1       := StandardParallel_1       ;
    Result.StandardParallel_2       := StandardParallel_2       ;
    Result.PseudoStandardParallel_1 := PseudoStandardParallel_1 ;
    Result.Zone                     := Zone                     ;
    Result.ScaleFactor              := ScaleFactor              ;
    Result.LongitudeOfCenter        := LongitudeOfCenter        ;
    Result.LatitudeOfCenter         := LatitudeOfCenter         ;
    Result.Azimuth                  := Azimuth                  ;
    Result.LongitudeOfPoint_1       := LongitudeOfPoint_1       ;
    Result.LatitudeOfPoint_1        := LatitudeOfPoint_1        ;
    Result.LongitudeOfPoint_2       := LongitudeOfPoint_2       ;
    Result.LatitudeOfPoint_2        := LatitudeOfPoint_2        ;
    Result.XScale                   := XScale                   ;
    Result.YScale                   := YScale                   ;
    Result.XYPlaneRotation          := XYPlaneRotation          ;
    Result.Custom1                  := Custom1                  ;
    Result.Custom2                  := Custom2                  ;
    Result.Custom3                  := Custom3                  ;
    Result.Custom4                  := Custom4                  ;
    Result.Custom5                  := Custom5                  ;
    Result.Custom6                  := Custom6                  ;
  end;

  procedure TGIS_CSProjParametersInternal.fset_All(
    const _value : TGIS_CSProjParameters
  ) ;
  var
    i, j, cnt, len : Integer ;
  begin
    bLock := True ;
    try
      SetLength( ar, PARAM_Last-PARAM_First+1 ) ;
      for i:= PARAM_First to PARAM_Last do begin
        {$IFDEF GIS_NORECORDS}
          ar[i-1] := new TGIS_CSProjParametersInternalElement ;
        {$ENDIF}
        ar[ i-1].Id    := i ;
        ar[ i-1].Value := 0 ;
      end ;

      CentralMeridian          := _value.CentralMeridian          ;
      LatitudeOfOrigin         := _value.LatitudeOfOrigin         ;
      FalseEasting             := _value.FalseEasting             ;
      FalseNorthing            := _value.FalseNorthing            ;
      StandardParallel_1       := _value.StandardParallel_1       ;
      StandardParallel_2       := _value.StandardParallel_2       ;
      PseudoStandardParallel_1 := _value.PseudoStandardParallel_1 ;
      Zone                     := _value.Zone                     ;
      ScaleFactor              := _value.ScaleFactor              ;
      LongitudeOfCenter        := _value.LongitudeOfCenter        ;
      LatitudeOfCenter         := _value.LatitudeOfCenter         ;
      Azimuth                  := _value.Azimuth                  ;
      LongitudeOfPoint_1       := _value.LongitudeOfPoint_1       ;
      LatitudeOfPoint_1        := _value.LatitudeOfPoint_1        ;
      LongitudeOfPoint_2       := _value.LongitudeOfPoint_2       ;
      LatitudeOfPoint_2        := _value.LatitudeOfPoint_2        ;
      XScale                   := _value.XScale                   ;
      YScale                   := _value.YScale                   ;
      XYPlaneRotation          := _value.XYPlaneRotation          ;
    finally
      bLock := False ;
    end ;

    len := length( ar ) ;
    cnt := len ;
    for i:= 0 to len-1 do
      if IsNan( ar[i].Value ) or
         ( ar[i].Value = 0 )  then
      begin
        for j := i+1 to len-1 do begin
          if ( not IsNan( ar[j].Value ) ) and
             ( ar[j].Value <> 0 )         then
          begin
            ar[i].Id    := ar[j].Id ;
            ar[i].Value := ar[j].Value ;
            ar[j].Value := 0 ;
            cnt := i + 1 ;
            break ;
          end ;
        end ;

      end;

    SetLength( ar, cnt ) ;

  end;

//------------------------------------------------------------------------------
// TGIS_CSProjAbstract
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAbstract.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FDatum := CSDatumList.ByEPSG( 6030 ) ;
    FEllipsoid := FDatum.Ellipsoid ;

    {$IFDEF GIS_NORECORDS}
      FParameters := new TGIS_CSProjParametersInternal ;
    {$ENDIF}
    FParameters.CentralMeridian          := 0 ;
    FParameters.LatitudeOfOrigin         := 0 ;
    FParameters.FalseEasting             := 0 ;
    FParameters.FalseNorthing            := 0 ;
    FParameters.StandardParallel_1       := 0 ;
    FParameters.StandardParallel_2       := 0 ;
    FParameters.PseudoStandardParallel_1 := 0 ;
    FParameters.Zone                     := 0 ;
    FParameters.ScaleFactor              := 1 ;
    FParameters.LongitudeOfCenter        := 0 ;
    FParameters.LatitudeOfCenter         := 0 ;
    FParameters.Azimuth                  := 0 ;
    FParameters.LongitudeOfPoint_1       := 0 ;
    FParameters.LatitudeOfPoint_1        := 0 ;
    FParameters.LongitudeOfPoint_2       := 0 ;
    FParameters.LatitudeOfPoint_2        := 0 ;
    FParameters.XScale                   := 0 ;
    FParameters.YScale                   := 0 ;
    FParameters.XYPlaneRotation          := 0 ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FIsXYZ         := False ;
    FIsGrid        := False ;

    P_k0   := 1 ;
    P_x0   := 0 ;
    P_y0   := 0 ;
    P_lam0 := 0 ;
    P_phi0 := 0 ;

    {$IFDEF GIS_NORECORDS}
      FValidityOriginWGS := new TGIS_Point ;
      FValidityExtentWGS := new TGIS_Extent ;
    {$ENDIF}

  end ;

  procedure TGIS_CSProjAbstract.doDestroy ;
  begin
    FreeObject( oShp ) ;

    inherited ;
  end ;

  function TGIS_CSProjAbstract.utQsfn(
    _sinphi : Double ;
    _e      : Double ;
    _one_es : Double
  ) : Double ;
  var
    con : Double ;
  begin
    if _e >= EPS7 then begin
      con := _e * _sinphi ;
      Result := _one_es * ( _sinphi / (1 - con * con) -
                            ( 0.5 / _e ) * Ln( (1 - con) / (1 + con) )
                          ) ;
    end
    else
      Result := _sinphi + _sinphi ;
  end ;

  function TGIS_CSProjAbstract.utPhi1(
    _qs      : Double ;
    _te      : Double ;
    _tone_es : Double
  ) : Double ;
  const
    N_ITER = 10 ;
  var
    i      : Integer ;
    phi,
    sinphi,
    cosphi,
    con,
    com,
    dphi   : Double ;
  begin
    if Abs( 0.5 * _qs ) > 1 then begin
      Result := GIS_MAX_DOUBLE ;
      exit ;
    end ;

    phi := ArcSin( 0.5 * _qs ) ;
    if _te < EPS7 then begin
      Result := phi ;
      exit ;
    end ;

    i := N_ITER ;
    repeat
      SinCos( phi, sinphi, cosphi ) ;
      con    := _te * sinphi   ;
      com    := 1 - con * con ;
      dphi   := 0.5 * com * com / cosphi *
               ( _qs / _tone_es - sinphi / com + 0.5 / _te *
                 Ln( (1 - con) / (1 + con) )
               );
      phi    := phi + dphi ;
      dec( i ) ;
    until ( Abs( dphi ) <= EPS10 ) or ( i <= 0  ) ;
     if i > 0 then Result := phi
              else Result := GIS_MAX_DOUBLE ;
  end ;

  function TGIS_CSProjAbstract.utAdjlon(
    _lon : Double
  ) : Double ;
  begin
    try
      // bit weaker test then original to help function which are close to Pi
      if Abs( _lon ) - Pi > 1e-6 then begin
        // adjust to 0..2pi rad
           _lon := _lon + Pi ;
        // remove integral # of 'revolutions'
           _lon := _lon - 2*Pi * FloorS( _lon / (2*Pi) ) ;
        // adjust back to -pi..pi rad
           _lon := _lon - Pi ;
      end ;
      Result := _lon ;
    except
      Result := _lon ;

    end;
  end ;

  function TGIS_CSProjAbstract.utEnfn(
    _es : Double
  ) : TGIS_DoubleArray ;
  const
    C00 = 1                      ;
    C02 = 0.25                   ;
    C04 = 0.046875               ;
    C06 = 0.01953125             ;
    C08 = 0.01068115234375       ;
    C22 = 0.75                   ;
    C44 = 0.46875                ;
    C46 = 0.01302083333333333333 ;
    C48 = 0.00712076822916666666 ;
    C66 = 0.36458333333333333333 ;
    C68 = 0.00569661458333333333 ;
    C88 = 0.3076171875           ;
  var
    t : Double ;
  begin
    SetLength( Result, 5 ) ;
    Result[0] := C00 - _es * (C02 + _es * (C04 + _es * (C06 + _es * C08)));
    Result[1] := _es * (C22 - _es * (C04 + _es * (C06 + _es * C08)));
    t := _es * _es ;
    Result[2] := t * (C44 - _es * (C46 + _es * C48));
    t := t * _es ;
    Result[3] := t * (C66 - _es * C68);
    Result[4] := t * _es * C88;
  end ;

  function TGIS_CSProjAbstract.utMlfn(
          _phi  : Double ;
          _sphi : Double ;
          _cphi : Double ;
    const _en   : TGIS_DoubleArray
  ) : Double ;
  begin
    _cphi := _cphi * _sphi ;
    _sphi := _sphi * _sphi ;

    Result := _en[0] * _phi -
              _cphi * ( _en[1] +
                        _sphi*( _en[2] +
                                _sphi*( _en[3] + _sphi*_en[4] )
                              )
                      );
  end ;

  function TGIS_CSProjAbstract.utInv_mlfn(
          _arg : Double ;
          _es  : Double ;
    const _en  : TGIS_DoubleArray
  ) : Double ;
  const
    N_ITER = 15 ;
  var
    s,t,phi,k : Double  ;
    i         : Integer ;
  begin
    k := 1 / (1-_es) ;

    phi := _arg;

    i := N_ITER ;
    while i > 0 do begin
      // rarely goes over 2 iterations
      s := Sin(phi);
      t := 1 - _es * s * s;
      t := ( utMlfn(phi, s, Cos( phi ), _en ) - _arg ) * ( t * Sqrt( t ) ) * k ;
      phi := phi -t ;
      if Abs( t ) < EPS7 then begin
        Result := phi;
        exit ;
      end ;

      dec( i ) ;
    end ;

    FError := ERROR_UNSOLVABLE ;
    Result := GIS_MAX_DOUBLE ;
  end ;

  function TGIS_CSProjAbstract.utMsfn(
    _sinphi : Double ;
    _cosphi : Double ;
    _es     : Double
  ) : Double ;
  begin
    Result := _cosphi / Sqrt( 1 - _es * _sinphi * _sinphi ) ;
  end ;

  function TGIS_CSProjAbstract.utTsfn(
    _phi    : Double ;
    _sinphi : Double ;
    _e      : Double
  ) : Double ;
  begin
    _sinphi := _sinphi * _e ;
    Result  := Tan( 0.5 * ( HALFPI - _phi ) ) /
               Power( (1 - _sinphi) / (1 + _sinphi), 0.5 * _e ) ;
  end ;

  function TGIS_CSProjAbstract.utAasin( _v : Double ) : Double ;
  var
    av : Double ;
  begin
    av := Abs( _v ) ;
    if av >= 1 then begin
      if av > ONETOL13 then begin
        FError := ERROR_UNSOLVABLE ;
        Result := GIS_MAX_DOUBLE ;
      end;
      if _v < 0 then Result := - HALFPI
                else Result :=   HALFPI ;
    end
    else
      Result := ArcSin( _v ) ;
  end ;

  function TGIS_CSProjAbstract.utAacos(
    _v : Double
  ) : Double ;
  var
    av : Double ;
  begin
    av := Abs( _v ) ;
    if av >= 1 then begin
      if av > ONETOL13 then begin
        FError := ERROR_UNSOLVABLE ;
        Result := GIS_MAX_DOUBLE ;
      end;
      if _v < 0 then Result := Pi
                else Result := 0 ;
    end
    else
      Result := ArcCos( _v ) ;
  end ;

  function TGIS_CSProjAbstract.utAsqrt(
    _v : Double
  ) : Double ;
  begin
    if _v <= 0 then Result := 0
               else Result := Sqrt( _v ) ;
  end ;

  function TGIS_CSProjAbstract.utAatan2(
    _n : Double ;
    _d : Double
  ) : Double ;
  begin
    if ( Abs( _n ) < AEPS ) and ( Abs( _d ) < AEPS )
      then Result := 0
      else Result := ArcTan2( _n, _d ) ;
  end ;

  function TGIS_CSProjAbstract.utAuthset(
    _es : Double
  ) : TGIS_DoubleArray ;
  const
    P00 = 0.33333333333333333333 ;
    P01 = 0.17222222222222222222 ;
    P02 = 0.10257936507936507936 ;
    P10 = 0.06388888888888888888 ;
    P11 = 0.06640211640211640211 ;
    P20 = 0.01641501294219154443 ;
  var
    t : Double ;
  begin
    SetLength( Result, 3 ) ;

    Result[0] := _es * P00           ;
    t         := _es * _es           ;
    Result[0] := Result[0] + t * P01 ;
    Result[1] := t * P10             ;
    t         := t * _es             ;
    Result[0] := Result[0] + t * P02 ;
    Result[1] := Result[1] + t * P11 ;
    Result[2] := t * P20             ;
  end ;

  function TGIS_CSProjAbstract.utAuthlat(
          _beta : Double;
    const _apa  : TGIS_DoubleArray
  ) : Double ;
  var
    t : Double ;
  begin
    t := _beta + _beta ;
    Result := _beta +
              _apa[0] * Sin( t     ) +
              _apa[1] * Sin( t+t   ) +
              _apa[2] * Sin( t+t+t ) ;
  end ;

  function TGIS_CSProjAbstract.utPhi2(
    _ts : Double ;
    _e  : Double
  ) : Double ;
  const
    N_ITER = 15 ;
  var
    i      : Integer ;
    eccnth,
    Phi,
    con,
    dphi   : Double  ;
  begin
    eccnth := 0.5 * _e ;
    Phi := HALFPI - 2 * ArcTan( _ts ) ;
    i := N_ITER ;
    repeat
      con  := _e * Sin( Phi ) ;
      dphi := HALFPI - 2 *
              ArcTan( _ts * Power( (1 - con) / (1 + con), eccnth ) ) - Phi ;
      Phi := Phi + dphi ;
      dec( i )
    until ( Abs(dphi) <= EPS10 ) or ( i <= 0 ) ;
    if i <= 0 then begin
      FError := ERROR_UNSOLVABLE ;
      Result := GIS_MAX_DOUBLE ;
    end
    else
      Result := Phi ;
  end ;

  function TGIS_CSProjAbstract.utZpoly1(
    const _z : TGIS_Complex          ;
    const _c : array of TGIS_Complex
  ) : TGIS_Complex ;
  var
    a : TGIS_Complex ;
    t : Double   ;
    n : Integer  ;
  begin
    n := high( _c ) ;
    a := _c[ n ] ;
    while n > 0 do begin
      dec( n ) ;
      t   := a.r ;
      a.r := _c[n].r + _z.r * t - _z.i * a.i;
      a.i := _c[n].i + _z.r * a.i + _z.i * t;
    end ;
    t   := a.r ;
    a.r := _z.r * t - _z.i * a.i;
    a.i := _z.r * a.i + _z.i * t;

    Result := a ;
  end ;

  function TGIS_CSProjAbstract.utZpolyd1(
    const _z   : TGIS_Complex          ;
    const _c   : array of TGIS_Complex ;
    var   _der : TGIS_Complex
  ) : TGIS_Complex ;
  var
    a, b  : TGIS_Complex ;
    t     : Double   ;
    first : Boolean  ;
    n     : Integer  ;
  begin
    first := True ;

    n := high( _c ) ;
    a := _c[ n ] ;
    while n > 0 do begin
      dec( n ) ;
      if first then begin
        first := False ;
        b := a ;
      end
      else begin
        t   := b.r ;
        b.r := a.r + _z.r * t   - _z.i * b.i ;
        b.i := a.i + _z.r * b.i + _z.i * t   ;
      end ;
      t   := a.r ;
      a.r := _c[n].r + _z.r * t - _z.i * a.i;
      a.i := _c[n].i + _z.r * a.i + _z.i * t;
    end ;
    t   := b.r ;
    b.r := a.r + _z.r * t - _z.i * b.i;
    b.i := a.i + _z.r * b.i + _z.i * t;
    t   := a.r ;
    a.r := _z.r * t - _z.i * a.i;
    a.i := _z.r * a.i + _z.i * t;

    _der   := b ;
    Result := a ;
  end ;

  function TGIS_CSProjAbstract.fget_Description
    : String ;
  begin
    Result := Trim( FDescription ) ;
  end ;

  function TGIS_CSProjAbstract.fget_IsStandard
    : Boolean ;
  begin
    Result := IsRevertible and ( not IsGrid ) and ( not IsXYZ ) ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Datum(
    const _value : TGIS_CSDatum
  ) ;
  begin
    FDatum := _value ;
    datumChanged ;
  end ;

  function TGIS_CSProjAbstract.fget_CentralMeridian
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.CentralMeridian ;
  end ;

  procedure TGIS_CSProjAbstract.fset_CentralMeridian(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.CentralMeridian  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LatitudeOfOrigin
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LatitudeOfOrigin ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LatitudeOfOrigin(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LatitudeOfOrigin  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_FalseEasting
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.FalseEasting ;
  end ;

  procedure TGIS_CSProjAbstract.fset_FalseEasting(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.FalseEasting  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_FalseNorthing
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.FalseNorthing,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.FalseNorthing ;
  end ;

  procedure TGIS_CSProjAbstract.fset_FalseNorthing(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.FalseNorthing,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.FalseNorthing  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_StandardParallel_1
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.StandardParallel_1 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_StandardParallel_1(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.StandardParallel_1  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_StandardParallel_2
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.StandardParallel_2 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_StandardParallel_2(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.StandardParallel_2  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_PseudoStandardParallel_1
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.PseudoStandardParallel_1 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_PseudoStandardParallel_1(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.PseudoStandardParallel_1  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Zone
    : Integer ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                                 FParametersSet
                               )
    then
      Result := 0
    else
      Result := FParameters.Zone ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Zone(
    const _value : Integer
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Zone  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_ScaleFactor
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.ScaleFactor ;
  end ;

  procedure TGIS_CSProjAbstract.fset_ScaleFactor(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.ScaleFactor  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LongitudeOfCenter
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LongitudeOfCenter ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LongitudeOfCenter(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LongitudeOfCenter  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LatitudeOfCenter
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LatitudeOfCenter ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LatitudeOfCenter(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet(  TGIS_CSProjParameter.LatitudeOfCenter,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LatitudeOfCenter  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Azimuth
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Azimuth ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Azimuth(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Azimuth  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LongitudeOfPoint_1
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LongitudeOfPoint_1 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LongitudeOfPoint_1(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LongitudeOfPoint_1  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LatitudeOfPoint_1
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LatitudeOfPoint_1 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LatitudeOfPoint_1(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LatitudeOfPoint_1  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LongitudeOfPoint_2
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LongitudeOfPoint_2 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LongitudeOfPoint_2(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LongitudeOfPoint_2  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_LatitudeOfPoint_2
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.LatitudeOfPoint_2  ;
  end ;

  procedure TGIS_CSProjAbstract.fset_LatitudeOfPoint_2(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.LatitudeOfPoint_2  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_XScale
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.XScale ;
  end ;

  procedure TGIS_CSProjAbstract.fset_XScale(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.XScale  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_YScale
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.YScale ;
  end ;

  procedure TGIS_CSProjAbstract.fset_YScale(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.YScale  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_XYPlaneRotation
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.XYPlaneRotation ;
  end ;

  procedure TGIS_CSProjAbstract.fset_XYPlaneRotation(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.XYPlaneRotation  := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom1
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom1 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom1(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom1 := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom1Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom1Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom2
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom2,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom2 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom2(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom2,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom2 := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom2Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom2,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom2Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom3
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom3,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom3 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom3(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom3,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom3 := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom3Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom3,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom3Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom4
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom4,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom4 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom4(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom4,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom4 := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom4Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom4,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom4Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom5
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom5,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom5 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom5(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom5,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom5 := _value ;

    bParamsModified := True ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom5Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom5,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom5Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Custom6
    : Double ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom6,
                                 FParametersSet
                               )
    then
      Result := NaN
    else
      Result := FParameters.Custom6 ;
  end ;

  procedure TGIS_CSProjAbstract.fset_Custom6(
    const _value : Double
  ) ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom6,
                                 FParametersSet
                               )
    then
      exit ;

    FParameters.Custom6 := _value ;

    bParamsModified := True ;
  end ;


  function TGIS_CSProjAbstract.fget_Custom6Name
    : String ;
  begin
    if not InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                                 FParametersSet
                               )
    then
      Result := ''
    else
      Result := FParameters.Custom6Name ;
  end ;

  function TGIS_CSProjAbstract.fget_Parameters
    : TGIS_CSProjParameters ;

  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_CSProjParameters ;
    {$ENDIF}
    Result.CentralMeridian          := NaN  ;
    Result.LatitudeOfOrigin         := NaN  ;
    Result.FalseEasting             := NaN  ;
    Result.FalseNorthing            := NaN  ;
    Result.StandardParallel_1       := NaN  ;
    Result.StandardParallel_2       := NaN  ;
    Result.PseudoStandardParallel_1 := NaN  ;
    Result.Zone                     := 0    ;
    Result.ScaleFactor              := NaN  ;
    Result.LongitudeOfCenter        := NaN  ;
    Result.LatitudeOfCenter         := NaN  ;
    Result.Azimuth                  := NaN  ;
    Result.LongitudeOfPoint_1       := NaN  ;
    Result.LatitudeOfPoint_1        := NaN  ;
    Result.LongitudeOfPoint_2       := NaN  ;
    Result.LatitudeOfPoint_2        := NaN  ;
    Result.XScale                   := NaN  ;
    Result.YScale                   := NaN  ;
    Result.XYPlaneRotation          := NaN  ;
    Result.Custom1                  := NaN  ;
    Result.Custom2                  := NaN  ;
    Result.Custom3                  := NaN  ;
    Result.Custom4                  := NaN  ;
    Result.Custom5                  := NaN  ;
    Result.Custom6                  := NaN  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                             ParametersSet
                           )
    then
      Result.CentralMeridian          := CentralMeridian          ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                             ParametersSet
                           )
    then
      Result.LatitudeOfOrigin         := LatitudeOfOrigin         ;

    if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                             ParametersSet
                           )
    then
      Result.FalseEasting             := FalseEasting             ;

    if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                             ParametersSet
                           )
    then
      Result.FalseNorthing            := FalseNorthing            ;

    if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                             ParametersSet
                           )
    then
      Result.StandardParallel_1       := StandardParallel_1       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                             ParametersSet
                           )
    then
      Result.StandardParallel_2       := StandardParallel_2       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                             ParametersSet
                           )
    then
      Result.PseudoStandardParallel_1 := PseudoStandardParallel_1 ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                             ParametersSet
                           )
    then
      Result.Zone                     := Zone                     ;

    if InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                             ParametersSet
                           )
    then
      Result.ScaleFactor              := ScaleFactor              ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                             ParametersSet
                           )
    then
      Result.LongitudeOfCenter        := LongitudeOfCenter        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                             ParametersSet
                           )
    then
      Result.LatitudeOfCenter         := LatitudeOfCenter         ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                             ParametersSet
                           )
    then
      Result.Azimuth                  := Azimuth                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                             ParametersSet
                           )
    then
      Result.LongitudeOfPoint_1       := LongitudeOfPoint_1       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                             ParametersSet
                           )
    then
      Result.LatitudeOfPoint_1        := LatitudeOfPoint_1        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                             ParametersSet
                           )
    then
      Result.LongitudeOfPoint_2       := LongitudeOfPoint_2       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                             ParametersSet
                           )
    then
      Result.LatitudeOfPoint_2         := LatitudeOfPoint_2        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                             ParametersSet
                           )
    then
      Result.XScale                   := XScale                   ;

    if InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                             ParametersSet
                           )
    then
      Result.YScale                   := YScale                   ;


    if InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                             ParametersSet
                           )
    then
      Result.XYPlaneRotation          := XYPlaneRotation          ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                             ParametersSet
                           )
    then begin
      Result.Custom1                  := Custom1                  ;
      Result.Custom1Name              := Custom1Name              ;
    end;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom2,
                             ParametersSet
                           )
    then
      Result.Custom2                  := Custom2                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom3,
                             ParametersSet
                           )
    then
      Result.Custom3                  := Custom3                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom4,
                             ParametersSet
                           )
    then
      Result.Custom4                  := Custom4                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom5,
                             ParametersSet
                           )
    then
      Result.Custom5                  := Custom5                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom6,
                             ParametersSet
                           )
    then
      Result.Custom6                  := Custom6                  ;

  end ;

  procedure TGIS_CSProjAbstract.fset_Parameters(
    const _value : TGIS_CSProjParameters
  ) ;
  begin
    if InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                             ParametersSet
                           )
    then
      FParameters.CentralMeridian          := _value.CentralMeridian          ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                             ParametersSet
                           )
    then
      FParameters.LatitudeOfOrigin         := _value.LatitudeOfOrigin         ;

    if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                             ParametersSet
                           )
    then
      FParameters.FalseEasting             := _value.FalseEasting             ;

    if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                             ParametersSet
                           )
    then
      FParameters.FalseNorthing            := _value.FalseNorthing            ;

    if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                             ParametersSet
                           )
    then
      FParameters.StandardParallel_1       := _value.StandardParallel_1       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                             ParametersSet
                           )
    then
      FParameters.StandardParallel_2       := _value.StandardParallel_2       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                             ParametersSet
                           )
    then
      FParameters.PseudoStandardParallel_1 := _value.PseudoStandardParallel_1 ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                             ParametersSet
                           )
    then
      FParameters.Zone                     := _value.Zone                     ;

    if InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                             ParametersSet
                           )
    then
      FParameters.ScaleFactor              := _value.ScaleFactor              ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                             ParametersSet
                           )
    then
      FParameters.LongitudeOfCenter        := _value.LongitudeOfCenter        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                             ParametersSet
                           )
    then
      FParameters.LatitudeOfCenter         := _value.LatitudeOfCenter         ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                             ParametersSet
                           )
    then
      FParameters.Azimuth                  := _value.Azimuth                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                             ParametersSet
                           )
    then
      FParameters.LongitudeOfPoint_1       := _value.LongitudeOfPoint_1       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                             ParametersSet
                           )
    then
      FParameters.LatitudeOfPoint_1        := _value.LatitudeOfPoint_1        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                             ParametersSet
                           )
    then
      FParameters.LongitudeOfPoint_2       := _value.LongitudeOfPoint_2       ;

    if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                             ParametersSet
                           )
    then
      FParameters.LatitudeOfPoint_2        := _value.LatitudeOfPoint_2        ;

    if InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                             ParametersSet
                           )
    then
      FParameters.XScale                   := _value.XScale                   ;

    if InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                             ParametersSet
                           )
    then
      FParameters.YScale                   := _value.YScale                   ;

    if InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                             ParametersSet
                           )
    then
      FParameters.XYPlaneRotation          := _value.XYPlaneRotation          ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom1,
                             ParametersSet
                           )
    then
      FParameters.Custom1                  := _value.Custom1                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom2,
                             ParametersSet
                           )
    then
      FParameters.Custom2                  := _value.Custom2                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom3,
                             ParametersSet
                           )
    then
      FParameters.Custom3                  := _value.Custom3                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom4,
                             ParametersSet
                           )
    then
      FParameters.Custom4                  := _value.Custom4                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom5,
                             ParametersSet
                           )
    then
      FParameters.Custom5                  := _value.Custom5                  ;

    if InCSProjParameterSet( TGIS_CSProjParameter.Custom6,
                             ParametersSet
                           )
    then
      FParameters.Custom6                  := _value.Custom6                  ;

    bParamsModified := True ;
  end ;

  procedure TGIS_CSProjAbstract.set_Validity(
   const _origin_x   : Double ;
   const _zone_x     : Double ;
   const _origin_y   : Double ;
   const _zone_y     : Double
  ) ;
  begin
    bUseValidity := True ;

    FValidityOriginWGS.X    := _origin_x ;
    FValidityOriginWGS.Y    := _origin_y ;
    FValidityExtentWGS.XMin := _origin_x - _zone_x ;
    FValidityExtentWGS.XMax := _origin_x + _zone_x ;
    FValidityExtentWGS.YMin := _origin_y - _zone_y ;
    FValidityExtentWGS.YMax := _origin_y + _zone_y ;

    FValidityExtentWGS.XMin := Max( FValidityExtentWGS.XMin, -Pi ) ;
    FValidityExtentWGS.XMax := Min( FValidityExtentWGS.XMax,  Pi ) ;
    FValidityExtentWGS.YMin := Max( FValidityExtentWGS.YMin, -Pi/2 ) ;
    FValidityExtentWGS.YMax := Min( FValidityExtentWGS.YMax,  Pi/2 ) ;

  end;

  procedure TGIS_CSProjAbstract.set_Validity(
   const _origin_x   : Double ;
   const _zone_x     : Double ;
   const _origin_y   : Double ;
   const _zone_y     : Double ;
   const _zone_y_min : Double ;
   const _zone_y_max : Double
  ) ;
  begin
    set_Validity( _origin_x, _zone_x,
                  _origin_y, _zone_y
                ) ;
    FValidityExtentWGS.YMin := Max( FValidityExtentWGS.YMin, _zone_y_min ) ;
    FValidityExtentWGS.YMax := Min( FValidityExtentWGS.YMax, _zone_y_max ) ;
  end;

  procedure TGIS_CSProjAbstract.set_Validity(
   const _origin_x   : Double ;
   const _zone_x     : Double ;
   const _zone_x_min : Double ;
   const _zone_x_max : Double ;
   const _origin_y   : Double ;
   const _zone_y     : Double ;
   const _zone_y_min : Double ;
   const _zone_y_max : Double
  ) ;
  begin
    set_Validity( _origin_x, _zone_x,
                  _origin_y, _zone_y
                ) ;
    FValidityExtentWGS.XMin := Max( FValidityExtentWGS.XMin, _zone_x_min ) ;
    FValidityExtentWGS.XMax := Min( FValidityExtentWGS.XMax, _zone_x_max ) ;
    FValidityExtentWGS.YMin := Max( Max( FValidityExtentWGS.YMin, _zone_y_min ),-Pi/2 ) ;
    FValidityExtentWGS.YMax := Min( Min( FValidityExtentWGS.YMax, _zone_y_max ), Pi/2 ) ;
  end;

  procedure TGIS_CSProjAbstract.datumChanged ;
  begin
    FEllipsoid := FDatum.Ellipsoid ;

    P_a      := Ellipsoid.SemiMajor  ;
    P_es     := Ellipsoid.EcntrMajor ;
    P_e      := Sqrt( P_es ) ;
    P_ra     := 1 / P_a      ;

    if Abs( P_es ) > EPS7 then begin
      P_one_es  := 1 - P_es     ;
      P_rone_es := 1 / P_one_es ;
    end ;

    P_lam0 := FParameters.CentralMeridian  ;
    P_phi0 := FParameters.LatitudeOfOrigin ;
    P_x0   := FParameters.FalseEasting     ;
    P_y0   := FParameters.FalseNorthing    ;

    bParamsModified := False ;

    bDatumReady := True ;
  end ;

  procedure TGIS_CSProjAbstract.projectPre(
    const _coords : TGIS_Point3D ;
    var   _lp_lam : Double       ;
    var   _lp_phi : Double
  ) ;
  var
    t   : Double ;
  begin
    if bParamsModified then datumChanged ;

    _lp_lam := _coords.X ;
    _lp_phi := _coords.Y ;

    if bUseValidity and CSProjUseValidity then begin
      with ValidityExtentWGS do begin
        if ( _lp_lam < XMin ) or ( _lp_lam > XMax ) then begin
          if _lp_lam <  (XMin + XMax) / 2 - Pi  then
            t := _lp_lam + 2 * Pi
          else
            t := _lp_lam ;
          if ( t < XMin ) or ( t > XMax ) then begin
            if      _lp_lam <  XMin then
                    _lp_lam := XMin
            else if _lp_lam >  XMax then
                    _lp_lam := XMax ;
          end ;
        end ;

        if      _lp_phi <  YMin then
                _lp_phi := YMin
        else if _lp_phi >  YMax then
                _lp_phi := YMax ;
      end;
    end;

    // check for forward and latitude or longitude over-range }
    t := Abs(_lp_phi) - HALFPI;
    if ( t > EPS7 ) or ( Abs(_lp_lam) > 10 ) then begin
      FError := ERROR_UNSOLVABLE ;
      _lp_lam := GIS_MAX_DOUBLE ;
      _lp_phi := GIS_MAX_DOUBLE ;
      exit ;
    end ;

    FError := ERROR_CLEAR ;
    if Abs( t ) <= EPS7 then begin
      if _lp_phi < 0 then _lp_phi := -HALFPI
                     else _lp_phi :=  HALFPI ;
    end
    else
      if P_geoc then _lp_phi := ArcTan( P_rone_es * Tan(_lp_phi) );

    _lp_lam := _lp_lam - P_lam0 ;
    if not P_over then
      _lp_lam := utAdjlon( _lp_lam );
  end ;

  procedure TGIS_CSProjAbstract.projectPost(
    {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D ;
    const _xy_x   : Double       ;
    const _xy_y   : Double
  ) ;
  begin
    if (_xy_x > GIS_MAX_SINGLE) or (_xy_y > GIS_MAX_SINGLE) then begin
      _coords.Y := GIS_MAX_DOUBLE ;
      _coords.X := GIS_MAX_DOUBLE ;
    end
    else begin
      // adjust for major axis and easting/northing
      _coords.X := P_a * _xy_x + P_x0 ;
      _coords.Y := P_a * _xy_y + P_y0 ;
    end;
  end ;

  procedure TGIS_CSProjAbstract.unprojectPre(
    const _coords : TGIS_Point3D ;
    var   _xy_x   : Double       ;
    var   _xy_y   : Double
  ) ;
  begin
    if bParamsModified then datumChanged ;

    _xy_x := _coords.X ;
    _xy_y := _coords.Y ;

    // can't do as much preliminary checking as with forward
    if (_xy_x > GIS_MAX_SINGLE ) or (_xy_y > GIS_MAX_SINGLE ) then begin
      FError := ERROR_UNSOLVABLE ;
      _xy_x := GIS_MAX_DOUBLE ;
      _xy_y := GIS_MAX_DOUBLE ;
      exit ;
    end;
    FError := ERROR_CLEAR ;
    _xy_x := ( _xy_x - P_x0 ) * P_ra ; // de-scale and de-offset
    _xy_y := ( _xy_y - P_y0 ) * P_ra ;
  end ;

  procedure TGIS_CSProjAbstract.unprojectPost(
    {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D ;
    const _lp_lam : Double       ;
    const _lp_phi : Double
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
  begin
    lp_lam := _lp_lam ;
    lp_phi := _lp_phi ;

    if ( Abs(lp_lam) > 100 ) or ( Abs(lp_phi) > 100 ) then begin
      FError := ERROR_UNSOLVABLE ;
      _coords.X := GIS_MAX_DOUBLE ;
      _coords.Y := GIS_MAX_DOUBLE ;
      exit ;
    end
    else begin
      lp_lam := lp_lam + P_lam0 ; // reduce from del lp_lam
      if not P_over then
        lp_lam := utAdjlon(lp_lam); // adjust longitude to CM
      if P_geoc and ( Abs( Abs(lp_phi) - HALFPI ) > EPS7 ) then
        lp_phi := ArcTan( P_one_es * Tan(lp_phi) ) ;
    end;

    _coords.X := lp_lam ;
    _coords.Y := lp_phi ;
  end ;

  procedure TGIS_CSProjAbstract.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_CSProjAbstract.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing
  end ;

  function TGIS_CSProjAbstract.Project(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D ;
  begin
    ptg.X := _coords.X ;
    ptg.Y := _coords.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    Project3D_Ref( ptg ) ;

    Result.X := ptg.X ;
    Result.Y := ptg.Y ;
  end ;

  function TGIS_CSProjAbstract.Unproject(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D ;
  begin
    ptg.X := _coords.X ;
    ptg.Y := _coords.Y ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    Unproject3D_Ref( ptg ) ;

    Result.X := ptg.X ;
    Result.Y := ptg.Y ;
  end ;

  function TGIS_CSProjAbstract.Project3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    ptg : TGIS_Point3D ;
  begin
    ptg := _coords ;

    Project3D_Ref( ptg ) ;

    Result := ptg ;
  end ;

  function TGIS_CSProjAbstract.Unproject3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    ptg : TGIS_Point3D ;
  begin
    ptg := _coords ;

    Unproject3D_Ref( ptg ) ;

    Result := ptg ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjAlbers
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAlbers.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := 'Equal Area' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjAlbers.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjAlbers.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjAlbers.datumChanged ;
  var
    cosphi,
    sinphi : Double  ;
    secant : Boolean ;
    ml1,
    m1     : Double  ;
    ml2,
    m2     : Double  ;
  begin
    inherited datumChanged ;

    Q_phi1 := FParameters.StandardParallel_1 ;
    Q_phi2 := FParameters.StandardParallel_2 ;

    if Abs( Q_phi1 + Q_phi2 ) < EPS10 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    SinCos( Q_phi1, sinphi, cosphi ) ;
    Q_n    := sinphi ;
    secant := Abs( Q_phi1 - Q_phi2) >= EPS10 ;

    if P_es <> 0 then begin
      Q_en := utEnfn( P_es ) ;
      m1  := utMsfn( sinphi, cosphi, P_es    ) ;
      ml1 := utQsfn( sinphi, P_e,    P_one_es) ;
      if secant then begin // secant cone
        SinCos( Q_phi2, sinphi, cosphi ) ;
        m2     := utMsfn( sinphi, cosphi, P_es    ) ;
        ml2    := utQsfn( sinphi, P_e,    P_one_es) ;
        Q_n    := ( m1 * m1 - m2 * m2 ) / ( ml2 - ml1 ) ;
      end ;
      Q_ec   := 1 - 0.5 * P_one_es *
                Ln( (1 - P_e) / (1 + P_e) ) / P_e ;
      Q_c    := m1 * m1 + Q_n * ml1 ;
      Q_dd   := 1 / Q_n ;
      Q_rho0 := Q_dd * Sqrt( Q_c - Q_n *
                             utQsfn( Sin( P_phi0 ), P_e, P_one_es )
                           );
    end
    else begin
      if secant then Q_n := 0.5 * ( Q_n + Sin( Q_phi2 ) ) ;
      Q_n2   := Q_n + Q_n ;
      Q_c    := cosphi * cosphi + Q_n2 * sinphi ;
      Q_dd   := 1 / Q_n ;
      Q_rho0 := Q_dd * Sqrt( Q_c - Q_n2 * Sin(P_phi0) ) ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjAlbers.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then Q_rho := Q_c - Q_n  * utQsfn( Sin(lp_phi), P_e, P_one_es )
                   else Q_rho := Q_c - Q_n2 * Sin( lp_phi ) ;
      if Q_rho  < 0 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      Q_rho  := Q_dd * Sqrt( Q_rho ) ;
      lp_lam := lp_lam * Q_n ;
      xy_x := Q_rho * Sin( lp_lam );
      xy_y := Q_rho0 - Q_rho * Cos( lp_lam ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjAlbers.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := Q_rho0 - xy_y ;
      Q_rho := Hypot( xy_x, xy_y ) ;
      if Q_rho <> 0 then begin
        if Q_n < 0 then begin
          Q_rho := - Q_rho ;
          xy_x  := - xy_x  ;
          xy_y  := - xy_y  ;
        end ;

        lp_phi := Q_rho / Q_dd ;

        if P_es <> 0 then begin
          lp_phi := ( Q_c - lp_phi * lp_phi ) / Q_n ;
          if Abs( Q_ec - Abs(lp_phi)) > EPS7 then begin
            lp_phi := utPhi1( lp_phi, P_e, P_one_es ) ;
            if lp_phi > GIS_MAX_SINGLE  then begin
              FError := ERROR_UNSOLVABLE ;
              unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
              exit ;
            end ;
          end
          else begin
            if lp_phi < 0 then lp_phi := -HALFPI
                          else lp_phi :=  HALFPI ;
          end ;
        end
        else begin
          lp_phi := ( Q_c - lp_phi * lp_phi ) / Q_n2 ;
          if Abs( lp_phi ) <= 1 then
            lp_phi := ArcSin( lp_phi )
          else begin
            if lp_phi < 0 then lp_phi := -HALFPI
                          else lp_phi :=  HALFPI ;
          end ;
        end ;
        lp_lam := ArcTan2( xy_x, xy_y ) / Q_n ;
      end
      else begin
        lp_lam := 0 ;
        if Q_n > 0 then lp_phi :=  HALFPI
                   else lp_phi := -HALFPI;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambert
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLambert.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := 'Equal Area' ;

    Q_phi1 := HALFPI ;
    Q_phi2 := 0      ;

    datumChanged ;
  end ;

  function TGIS_CSProjLambert.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLambert.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLambert.datumChanged ;
  begin
    if FParameters.StandardParallel_2 < 0
       then FParameters.StandardParallel_2 := -HALFPI
       else FParameters.StandardParallel_2 :=  HALFPI ;

    // all parameters are same as in Albers
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjAzimuthalEquidistant
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAzimuthalEquidistant.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := 'Equal Area' ;

    datumChanged ;
  end ;

  function TGIS_CSProjAzimuthalEquidistant.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjAzimuthalEquidistant.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjAzimuthalEquidistant.datumChanged ;
  begin
    inherited datumChanged ;

    if Abs( Abs( P_phi0 ) - HALFPI ) < EPS10 then begin
      if P_phi0 < 0 then Q_mode   := S_POLE
                    else Q_mode   := N_POLE ;
      if P_phi0 < 0 then Q_sinph0 := -1
                    else Q_sinph0 :=  1 ;
      Q_cosph0 := 0 ;
    end
    else if Abs( P_phi0 ) < EPS10 then begin
      Q_mode   := EQUIT ;
      Q_sinph0 := 0 ;
      Q_cosph0 := 1 ;
    end
    else begin
      Q_mode   := OBLIQ ;
      SinCos( P_phi0, Q_sinph0, Q_cosph0 ) ;
    end ;

    if P_es <> 0 then begin
      Q_en := utEnfn( P_es ) ;
      case Q_mode of
        N_POLE : Q_Mp := utMlfn(  HALFPI, 1, 0, Q_en ) ;
        S_POLE : Q_Mp := utMlfn( -HALFPI,-1, 0, Q_en ) ;
        EQUIT,
        OBLIQ  : begin
                   Q_N1 := 1 / Sqrt( 1 - P_es * Q_sinph0 * Q_sinph0 ) ;
                   Q_He := P_e / Sqrt( P_one_es ) ;
                   Q_G  := Q_sinph0 * Q_He ;
                   Q_He := Q_He * Q_cosph0 ;
                 end ;
      end ;
    end ;

    case Q_mode of
      N_POLE :
        set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                      0                           , ZONE_180, -ZONE_60 , ZONE_90
                    ) ;

      S_POLE :
        set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                      0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                    ) ;
      EQUIT  :
        set_Validity( FParameters.CentralMeridian , ZONE_90 , -ZONE_180, ZONE_180,
                      FParameters.LatitudeOfOrigin, ZONE_90 , -ZONE_90 , ZONE_90
                    ) ;
      OBLIQ  :
        set_Validity( FParameters.CentralMeridian , ZONE_70 , -ZONE_90 , ZONE_90 ,
                      FParameters.LatitudeOfOrigin, ZONE_70 , -ZONE_90 , ZONE_90
                    )
    end ;

  end ;

  procedure TGIS_CSProjAzimuthalEquidistant.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    coslam,
    cosphi,
    sinphi,
    rho,
    s,
    H,
    H2,
    c,
    Az,
    t,
    ct,
    st,
    cA,
    sA     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoidal

        coslam := Cos( lp_lam ) ;
        SinCos( lp_phi, sinphi, cosphi ) ;
        case Q_mode of
          N_POLE ,
          S_POLE : begin
                     if Q_mode = N_POLE then
                       coslam := - coslam ;
                     rho  := Abs( Q_Mp - utMlfn( lp_phi, sinphi, cosphi, Q_en ) ) ;
                     xy_x := rho * Sin( lp_lam ) ;
                     xy_y := rho * coslam ;
                   end ;
          EQUIT,
          OBLIQ  : begin
                     if ( Abs( lp_lam          ) < EPS10 ) and
                        ( Abs( lp_phi - P_phi0 ) < EPS10 )
                     then begin
                       xy_x := 0 ;
                       xy_y := 0 ;
                     end
                     else begin
                       t := ArcTan2( P_one_es * sinphi + P_es * Q_N1 * Q_sinph0 *
                                     Sqrt( 1 - P_es * sinphi * sinphi ),
                                     cosphi
                                    ) ;
                       SinCos( t, st, ct ) ;
                       Az := ArcTan2( Sin( lp_lam ) * ct, Q_cosph0 * st -
                                      Q_sinph0 * coslam * ct
                                    ) ;
                       SinCos( Az, sA, cA ) ;

                       if Abs( sA ) < EPS10
                         then s := utAasin( ( Q_cosph0 * st -
                                              Q_sinph0 * coslam * ct
                                            ) / cA
                                          )
                         else s := utAasin( Sin( lp_lam ) * ct / sA ) ;

                       H  := Q_He * cA ;
                       H2 := H * H     ;
                       c  := Q_N1 * s *
                             ( 1 + s * s *
                               ( - H2 * (1 - H2)/6 +
                                 s * ( Q_G * H *
                                       ( 1 - 2 * H2 * H2) / 8.0 +
                                       s * ( ( H2 * (4 - 7 * H2) -
                                               3 * Q_G * Q_G * (1 - 7 * H2)
                                             ) / 120 - s * Q_G * H / 48
                                           )
                                     )
                               )
                             ) ;
                       xy_x := c * sA ;
                       xy_y := c * cA ;
                     end ;
                   end ;
          else     begin
                     // untested case
                     xy_x := 0 ;
                     xy_y := 0 ;
                   end ;
        end ;
      end
      else begin // spherical

        SinCos( lp_phi, sinphi, cosphi ) ;
        coslam := Cos( lp_lam ) ;

        case Q_mode of
          EQUIT,
          OBLIQ  : begin
                     if Q_mode = EQUIT
                       then xy_y := cosphi * coslam
                       else xy_y := Q_sinph0 * sinphi + Q_cosph0 * cosphi * coslam ;
                     if Abs( Abs( xy_y ) - 1 ) < EPS10 then begin
                       if xy_y < 0 then begin
                         FError := ERROR_UNSOLVABLE ;
                         projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                         exit ;
                       end
                       else begin
                         xy_x := 0 ;
                         xy_y := 0 ;
                       end ;
                     end
                     else begin
                       xy_y := ArcCos( xy_y ) ;
                       xy_y := xy_y / Sin( xy_y ) ;
                       xy_x := xy_y * cosphi * Sin( lp_lam ) ;
                       if Q_mode = EQUIT then
                         xy_y := xy_y * sinphi
                       else
                         xy_y := xy_y * Q_cosph0 * sinphi -
                                        Q_sinph0 * cosphi * coslam ;
                     end ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = N_POLE then begin
                       lp_phi := -lp_phi ;
                       coslam := -coslam ;
                     end ;
                     if Abs( lp_phi - HALFPI ) < EPS10 then begin
                       FError := ERROR_UNSOLVABLE ;
                       projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                       exit ;
                     end ;
                     xy_y := HALFPI + lp_phi ;
                     xy_x := xy_y * Sin( lp_lam ) ;
                     xy_y := xy_y * coslam ;
                   end ;
          else     begin
                     // untested case
                     xy_x := 0 ;
                     xy_y := 0 ;
                   end ;
        end ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjAzimuthalEquidistant.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    cosc,
    c_rh,
    sinc,
    c,
    az,
    cosaz,
    a,
    b,
    d,
    e,
    f,
    psi,
    t     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoidal

        c := Hypot( xy_x, xy_y ) ;
        if c < EPS10 then begin
          lp_phi := P_phi0 ;
          lp_lam := 0 ;
        end
        else if ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT ) then begin
          az :=ArcTan2( xy_x, xy_y ) ;
          cosaz := Cos( az );
          t := Q_cosph0 * cosaz;
          b := P_es * t / P_one_es ;
          a := - b * t ;
          b := b * 3 * (1 - a) * Q_sinph0 ;
          d := c / Q_N1 ;
          e := d * ( 1 - d * d * (a * (1 + a) / 6 + b * (1 + 3*a) * d / 24) ) ;
          f := 1 - e * e * ( a / 2 + b * e / 6 );
          psi    := utAasin( Q_sinph0 * Cos(e) + t * Sin(e) ) ;
          lp_lam := utAasin( Sin(az) * Sin(e) / Cos(psi) ) ;
          t := Abs( psi ) ;
          if t < EPS10 then
            lp_phi := 0
          else if Abs( t - HALFPI ) < 0 then
            lp_phi := HALFPI
          else
            lp_phi := ArcTan( ( 1 - P_es * f * Q_sinph0 / Sin(psi) ) * Tan(psi) /
                               P_one_es
                            );
        end
        else begin // Polar
          if Q_mode = N_POLE
            then lp_phi := utInv_mlfn( Q_Mp - c, P_es, Q_en )
            else lp_phi := utInv_mlfn( Q_Mp + c, P_es, Q_en ) ;
          if lp_phi > GIS_MAX_SINGLE then begin
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end;

          if Q_mode = N_POLE
            then lp_lam := ArcTan2( xy_x, -xy_y )
            else lp_lam := ArcTan2( xy_x,  xy_y ) ;
        end ;
      end
      else begin // spherical
        c_rh := Hypot( xy_x, xy_y ) ;
        if c_rh > Pi then begin
          if (c_rh - EPS10 ) > Pi then begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;
          c_rh := Pi ;
        end
        else if c_rh < EPS10 then begin
          lp_phi := P_phi0 ;
          lp_lam := 0 ;
          unprojectPost( _coords, lp_lam, lp_phi ) ;
          exit ;
        end ;

        if ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT ) then begin
          SinCos( c_rh, sinc, cosc ) ;
          if Q_mode = EQUIT then begin
            lp_phi := utAasin( xy_y * sinc / c_rh ) ;
            xy_x := xy_x * sinc ;
            xy_y := cosc * c_rh;
          end
          else begin
            lp_phi := utAasin( cosc * Q_sinph0 + xy_y * sinc * Q_cosph0 / c_rh ) ;
            xy_y := ( cosc - Q_sinph0 * Sin( lp_phi ) ) * c_rh ;
            xy_x := xy_x * sinc * Q_cosph0 ;
          end ;
          if xy_y = 0 then lp_lam := 0
                      else lp_lam := ArcTan2( xy_x, xy_y ) ;
        end
        else if Q_mode = N_POLE then begin
          lp_phi := HALFPI - c_rh ;
          lp_lam := ArcTan2( xy_x, -xy_y ) ;
        end
        else begin
          lp_phi := c_rh - HALFPI ;
          lp_lam := ArcTan2( xy_x, xy_y );
        end ;

      end ;

      if lp_phi > GIS_MAX_SINGLE then begin
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjAiry
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAiry.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Azimuth
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjAiry.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjAiry.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjAiry.datumChanged ;
  var
    beta : Double ;
  begin
    inherited datumChanged ;

    Q_am1 := FParameters.Azimuth ;

    if (Q_am1 > 0) and ( Q_am1 < HALFPI / 2 ) then
      beta := 0.5 * (HALFPI - RadToDeg( 45 ) )
    else
      beta := 0.5 * (HALFPI - Q_am1 );

    if Abs( beta ) < EPS7 then
      Q_Cb := -0.5
    else begin
      Q_Cb := 1 / Tan( beta ) ;
      Q_Cb := Q_Cb * Q_Cb * Ln( Cos( beta ) ) ;
    end ;

    if Abs( Abs( P_phi0 ) - HALFPI ) < EPS7 then begin
      if P_phi0 < 0 then begin
        Q_p_halfpi := -HALFPI ;
        Q_mode     := S_POLE  ;
      end
      else begin
        Q_p_halfpi := HALFPI ;
        Q_mode     := N_POLE ;
      end ;
    end
    else begin
      if Abs( P_phi0 ) < EPS7 then
        Q_mode := EQUIT
      else begin
        Q_mode   := OBLIQ ;
        SinCos( P_phi0, Q_sinph0, Q_cosph0 ) ;
      end ;
    end ;

    // not reversible - no bounding polygon
  end ;

  procedure TGIS_CSProjAiry.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    sinlam,
    coslam,
    cosphi,
    sinphi,
    t,
    s,
    Krho,
    cosz    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_lam, sinlam, coslam ) ;

      case Q_mode of
        EQUIT,
        OBLIQ :  begin
                   SinCos( lp_phi, sinphi, cosphi ) ;
                   cosz := cosphi * coslam ;
                   if Q_mode = OBLIQ then
                     cosz := Q_sinph0 * sinphi + Q_cosph0 * cosz ;
                   if cosz < -EPS7 then begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;

                   s := 1 - cosz ;
                   if Abs( s ) > EPS7 then begin
                     t    := 0.5 * (1 + cosz) ;
                     Krho := -Ln( t ) / s  -  Q_Cb / t ;
                   end
                   else
                     Krho := 0.5 - Q_Cb ;

                   xy_x := Krho * cosphi * sinlam ;
                   if Q_mode = OBLIQ then
                     xy_y := Krho * ( Q_cosph0 * sinphi -
                                      Q_sinph0 * cosphi * coslam )
                   else
                     xy_y := Krho * sinphi ;
                 end ;
        S_POLE,
        N_POLE : begin
                   lp_phi := Abs( Q_p_halfpi - lp_phi ) ;
                   if (lp_phi - EPS7) > HALFPI then begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;

                   lp_phi := lp_phi * 0.5 ;
                   if lp_phi > EPS7 then begin
                     t    := Tan( lp_phi ) ;
                     Krho := -2*( Ln( Cos( lp_phi ) ) / t + t * Q_Cb ) ;
                     xy_x := Krho * sinlam ;
                     xy_y := Krho * coslam ;
                     if Q_mode = N_POLE then xy_y := -xy_y
                   end
                   else begin
                     xy_x := 0 ;
                     xy_y := 0 ;
                   end ;
                 end ;
        else     begin
                   // untested case
                   xy_x := 0 ;
                   xy_y := 0 ;
                 end ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjAitof
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAitof.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudoazimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjAitof.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjAitof.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjAitof.datumChanged ;
  begin
    inherited datumChanged ;

    Q_mode := 0 ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjAitof.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    c,
    d      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      c := 0.5 * lp_lam ;
      d := ArcCos( Cos( lp_phi ) * Cos(c) ) ;
      if d <> 0 then begin // basic Aitoff
        xy_y := 1 / Sin( d ) ;
        xy_x := 2 * d * Cos( lp_phi ) * Sin( c ) * xy_y ;
        xy_y := xy_y * d * Sin( lp_phi ) ;
      end
      else begin
        xy_x := 0 ;
        xy_y := 0 ;
      end ;
      if Q_mode <> 0 then begin // Winkel Tripel
        xy_x := ( xy_x + lp_lam * Q_cosphi1 ) * 0.5 ;
        xy_y := ( xy_y + lp_phi ) * 0.5 ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWinkelTripel
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWinkelTripel.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudoazimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 50.0 + 28.0/60 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjWinkelTripel.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWinkelTripel.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWinkelTripel.datumChanged ;
  begin
    inherited datumChanged ;

    Q_cosphi1 := FParameters.StandardParallel_1 ;
    if Q_cosphi1 = 0 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False;
      exit ;
    end ;

    Q_mode := 1 ;

    // not reversible - no bounding polygon
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjAugust
//------------------------------------------------------------------------------

  constructor TGIS_CSProjAugust.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjAugust.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjAugust.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjAugust.datumChanged ;
  begin
    inherited datumChanged ;

    // not reversible - no bounding polygon
  end ;

  procedure TGIS_CSProjAugust.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    M = 1 + 1.0/3 ;
  var
    t, c1, c, x1, x12, y1, y12 : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      t  := Tan( 0.5 * lp_phi ) ;
      c1 := Sqrt( 1 - t * t ) ;
      lp_lam := lp_lam * 0.5 ;
      c  := 1 + c1 * Cos( lp_lam ) ;
      x1 := Sin( lp_lam ) *  c1 / c ;
      y1 :=  t / c ;
      x12 := x1 * x1 ;
      y12 := y1 * y1 ;
      xy_x := M * x1 * ( 3 + x12 - 3 * y12 );
      xy_y := M * y1 * ( 3 + 3 * x12 - y12 );

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBonne
//------------------------------------------------------------------------------

  constructor TGIS_CSProjBonne.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 30.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjBonne.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBonne.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBonne.datumChanged ;
  var
    c : Double ;
  begin
    inherited datumChanged ;

    Q_phi1 := FParameters.StandardParallel_1 ;
    Q_south := 1 ; // normal orientation

    if Abs( Q_phi1 ) < EPS10 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    if P_es <> 0 then begin // ellipsoid
      Q_en  := utEnfn( P_es ) ;
      SinCos( Q_phi1, Q_am1, c ) ;
      Q_m1  := utMlfn( Q_phi1, Q_am1, c, Q_en ) ;
      Q_am1 := c / ( Sqrt( 1 - P_es * Q_am1 * Q_am1 ) * Q_am1 ) ;
    end
    else begin // spheroid
      if Abs( Q_phi1 ) + EPS10 >= HALFPI
        then Q_cphi1 := 0
        else Q_cphi1 := 1 / Tan( Q_phi1 ) ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjBonne.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    rh, E, c : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoid

        SinCos( lp_phi, E, c ) ;
        rh := Q_am1 + Q_m1 - utMlfn( lp_phi, E, c, Q_en ) ;
        E  := c * lp_lam / ( rh * Sqrt( 1 - P_es * E * E ) ) ;
        xy_x := Q_south * rh * Sin( E ) ;
        xy_y := Q_south * ( Q_am1 - rh * Cos( E ) ) ;

      end
      else begin // spheroid

        rh := Q_cphi1 + Q_phi1 - lp_phi ;
        if Abs( rh ) > EPS10 then begin
          E := lp_lam * Cos( lp_phi ) / rh ;
          xy_x := Q_south * rh * Sin( E ) ;
          xy_y := Q_south * ( Q_cphi1 - rh * Cos( E ) );
        end
        else begin
          xy_x := 0 ;
          xy_y := 0 ;
        end ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjBonne.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    s, rh : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    if Q_south < 0  then begin
      // fix false easting & northing
      _coords.X := -_coords.X + 2* P_x0 ;
      _coords.Y := -_coords.Y + 2* P_y0 ;
    end ;

    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoid

        xy_y   := Q_am1 - xy_y ;
        rh     := Hypot( xy_x, xy_y ) ;
        lp_phi := utInv_mlfn( Q_am1 + Q_m1 - rh, P_es, Q_en ) ;

        if lp_phi > GIS_MAX_SINGLE then begin
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE );
          exit ;
        end;

        s      := Abs( lp_phi ) ;
        if s < HALFPI then begin
          s      := Sin( lp_phi ) ;
          lp_lam := rh * ArcTan2( xy_x, xy_y ) *
                    Sqrt( 1 - P_es * s * s ) / Cos( lp_phi ) ;
        end
        else begin
          if Abs( s - HALFPI ) <= EPS10 then
            lp_lam := 0
          else begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;
        end ;

      end
      else begin // spheroid

        xy_y   := Q_cphi1 - xy_y ;
        rh     := Hypot( xy_x, xy_y ) ;
        lp_phi := Q_cphi1 + Q_phi1 - rh ;
        if Abs( lp_phi ) > HALFPI then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;
        if Abs( Abs( lp_phi ) - HALFPI ) <= EPS10 then
          lp_lam := 0
        else
          lp_lam := rh * ArcTan2( xy_x, xy_y ) / Cos( lp_phi );

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBonneSouthOriented
//------------------------------------------------------------------------------

  function TGIS_CSProjBonneSouthOriented.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBonneSouthOriented.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBonneSouthOriented.datumChanged ;
  begin
    inherited datumChanged ;

    Q_south := -1 ; // west aspect
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWerner
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWerner.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := Pi/2 ;

    datumChanged ;
  end ;

  function TGIS_CSProjWerner.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWerner.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWerner.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjCassini
//------------------------------------------------------------------------------

  constructor TGIS_CSProjCassini.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalTransverse ;
    FParametersSet := [
                       TGIS_CSProjParameter.CentralMeridian,
                       TGIS_CSProjParameter.LatitudeOfOrigin,
                       TGIS_CSProjParameter.FalseEasting,
                       TGIS_CSProjParameter.FalseNorthing
                     ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjCassini.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjCassini.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjCassini.datumChanged ;
  begin
    inherited datumChanged ;

    if P_es <> 0 then begin // ellipsoid
      Q_en := utEnfn( P_es ) ;
      Q_m0 := utMlfn( P_phi0, Sin( P_phi0), Cos( P_phi0 ), Q_en ) ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_80,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjCassini.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C1 = 0.16666666666666666666 ;
    C2 = 0.00833333333333333333 ;
    C3 = 0.04166666666666666666 ;
    C4 = 0.33333333333333333333 ;
    C5 = 0.06666666666666666666 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoid
        SinCos( lp_phi, Q_n, Q_c ) ;
        xy_y := utMlfn( lp_phi, Q_n, Q_c, Q_en ) ;
        Q_n  := 1 / Sqrt( 1 - P_es * Q_n * Q_n ) ;
        Q_tn := Tan( lp_phi ) ;
        Q_t  := Q_tn * Q_tn   ;
        Q_a1 := lp_lam * Q_c  ;
        Q_c  := Q_c * P_es * Q_c / ( 1 - P_es ) ;
        Q_a2 := Q_a1 * Q_a1 ;
        xy_x := Q_n * Q_a1 *
               ( 1 - Q_a2 * Q_t *
                 ( C1 - ( 8 - Q_t + 8 * Q_c ) * Q_a2 * C2 )
               ) ;
        xy_y := xy_y -
                ( Q_m0 - Q_n * Q_tn * Q_a2 *
                  ( 0.5 + ( 5 - Q_t + 6 * Q_c ) * Q_a2 * C3 )
                ) ;

      end
      else begin // spheroid

        xy_x := ArcSin ( Cos( lp_phi ) * Sin( lp_lam ) ) ;
        xy_y := ArcTan2( Tan( lp_phi ),  Cos( lp_lam ) ) - P_phi0 ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjCassini.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C1 = 0.16666666666666666666 ;
    C2 = 0.00833333333333333333 ;
    C3 = 0.04166666666666666666 ;
    C4 = 0.33333333333333333333 ;
    C5 = 0.06666666666666666666 ;
  var
    ph1    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoid

        ph1    := utInv_mlfn( Q_m0 + xy_y, P_es, Q_en ) ;

        if ph1 > GIS_MAX_SINGLE then begin
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end;

        Q_tn   := Tan( ph1 )  ;
        Q_t    := Q_tn * Q_tn ;
        Q_n    := Sin( ph1 )  ;
        Q_r    := 1 / (1 - P_es * Q_n * Q_n ) ;
        Q_n    := Sqrt( Q_r ) ;
        Q_r    := Q_r * ( 1 - P_es ) * Q_n ;
        Q_dd   := xy_x / Q_n  ;
        Q_d2   := Q_dd * Q_dd ;
        lp_phi := ph1 - ( Q_n * Q_tn / Q_r ) * Q_d2 *
                  ( 0.5 - ( 1 + 3 * Q_t) * Q_d2 * C3 ) ;
        lp_lam := Q_dd *
                  ( 1 + Q_t * Q_d2 *
                    ( -C4 + ( 1 + 3 * Q_t ) * Q_d2 * C5 )
                  ) / Cos( ph1 ) ;

      end
      else begin // spheroid

        Q_dd   := xy_y + P_phi0 ;
        lp_phi := ArcSin( Sin( Q_dd  ) * Cos( xy_x ) ) ;
        lp_lam := ArcTan2( Tan( xy_x ), Cos( Q_dd ) ) ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjCentralCylindrical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjCentralCylindrical.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjCentralCylindrical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjCentralCylindrical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjCentralCylindrical.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_80
                ) ;
  end ;

  procedure TGIS_CSProjCentralCylindrical.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Abs( Abs( lp_phi ) - HALFPI ) < DEGREE10 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;
      xy_x := lp_lam ;
      xy_y := Tan( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjCentralCylindrical.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := ArcTan( xy_y ) ;
      lp_lam := xy_x ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEqualAreaCylindrical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEqualAreaCylindrical.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjEqualAreaCylindrical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEqualAreaCylindrical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEqualAreaCylindrical.datumChanged ;
  var
    t : Double ;
  begin
    inherited datumChanged ;

    t := FParameters.StandardParallel_1 ;

    P_k0 := Cos( t ) ;
    if P_k0 < 0 then begin
      FError := ERROR_WRONGPARAMS ;
      exit ;
    end
    else
      t := 0 ;

    if P_es <> 0 then begin
      t := Sin( t ) ;
      P_k0 := P_k0 / Sqrt( 1 - P_es * t * t ) ;
      P_e  := Sqrt( P_es ) ;
      Q_apa := utAuthset( P_es ) ;
      Q_qp  := utQsfn( 1, P_e, P_one_es ) ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEqualAreaCylindrical.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoid

        xy_x := P_k0 * lp_lam ;
        xy_y := 0.5 * utQsfn( Sin( lp_phi ), P_e, P_one_es ) / P_k0 ;

      end
      else begin // spheroid

        xy_x := P_k0 * lp_lam ;
        xy_y := Sin( lp_phi ) / P_k0 ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEqualAreaCylindrical.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t : Double ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
    vsin   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := xy_y * P_k0 ;
      t    := Abs( xy_y  ) ;
      if ( t - EPS7 ) <= 1 then begin
        if P_es <> 0 then begin // ellipsoid
          vsin := 2 * xy_y / Q_qp ;
          if Abs( vsin ) > 1 then begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end;

          lp_phi := utAuthlat( ArcSin( vsin ), Q_apa ) ;
          lp_lam := xy_x / P_k0 ;
        end
        else begin // spheroid
          if t >= 1 then begin
            if xy_y < 0 then lp_phi := -HALFPI
                        else lp_phi :=  HALFPI ;
          end
          else
            lp_phi := ArcSin( xy_y ) ;
          lp_lam := xy_x / P_k0 ;
        end ;
      end
      else begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBehrmann
//------------------------------------------------------------------------------

  constructor TGIS_CSProjBehrmann.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 30.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjBehrmann.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBehrmann.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBehrmann.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGallOrthographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGallOrthographic.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 45.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjGallOrthographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGallOrthographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGallOrthographic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_I.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_I.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEckert_I.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FC = 0.92131773192356127802 ;
    RP = 0.31830988618379067154 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := FC * lp_lam * (1 - RP * Abs( lp_phi ) ) ;
      xy_y := FC * lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEckert_I.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FC = 0.92131773192356127802 ;
    RP = 0.31830988618379067154 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / FC;
      lp_lam := xy_x / ( FC * (1 - RP * Abs( lp_phi ) ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_II
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_II.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_II.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_II.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_II.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEckert_II.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FXC = 0.46065886596178063902 ;
    FYC = 1.44720250911653531871 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y := Sqrt( 4 - 3 * Sin( Abs( lp_phi ) ) )  ;
      xy_x := FXC * lp_lam * xy_y ;
      xy_y := FYC * ( 2 - xy_y ) ;

      if ( lp_phi < 0 ) then xy_y := -xy_y;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEckert_II.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FXC = 0.46065886596178063902 ;
    FYC = 1.44720250911653531871 ;
    C13 = 0.33333333333333333333 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := 2 - Abs( xy_y ) / FYC ;
      lp_lam := xy_x / ( FXC * lp_phi ) ;
      lp_phi := ( 4 - lp_phi * lp_phi ) * C13 ;
      if Abs( lp_phi ) >= 1 then begin
        if Abs( lp_phi ) > EPS7 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if lp_phi < 0 then lp_phi := -HALFPI
                        else lp_phi :=  HALFPI ;
        end ;
      end
      else
        lp_phi := ArcSin( lp_phi ) ;

      if xy_y < 0 then lp_phi := -lp_phi ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_III
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_III.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_C_x := 0.42223820031577120149 ;
    Q_C_y := 0.84447640063154240298 ;
    Q_A   := 1 ;
    Q_B   := 0.4052847345693510857755 ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_III.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_III.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_III.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEckert_III.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y := Q_C_y * lp_phi ;
      xy_x := Q_C_x * lp_lam * ( Q_A + utAsqrt( 1 - Q_B * lp_phi * lp_phi ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEckert_III.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / Q_C_y;
      lp_lam := xy_x / ( Q_C_x *
                         ( Q_A + utAsqrt( 1 - Q_B * lp_phi * lp_phi ) )
                       ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P1
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P1.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_C_x :=  1.89490 ;
    Q_C_y :=  0.94745 ;
    Q_A   := -0.5     ;
    Q_B   :=  0.30396355092701331433 ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P1.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P1.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P1.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_VI
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_VI.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_C_x := 0.94745 ;
    Q_C_y := 0.94745 ;
    Q_A   := 0.0     ;
    Q_B   := 0.30396355092701331433 ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_VI.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_VI.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_VI.datumChanged ;
  begin
    // parameters same as Eckert_III
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjKavraisky_VII
//------------------------------------------------------------------------------

  constructor TGIS_CSProjKavraisky_VII.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_C_x := 0.2632401569273184856851 ;
    Q_C_x := 0.8660254037844          ;
    Q_C_y := 1 ;
    Q_A   := 0 ;
    Q_B   := 0.30396355092701331433   ;

    datumChanged ;
  end ;

  function TGIS_CSProjKavraisky_VII.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKavraisky_VII.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjKavraisky_VII.datumChanged ;
  begin
    // paramtyers same as Eckert_III
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_IV
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_IV.Create(
    const _epsg  : Integer ;
    const _wkt   : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_IV.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_IV.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_IV.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEckert_IV.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C_x    = 0.42223820031577120149 ;
    C_y    = 1.32650042817700232218 ;
    RC_y   = 0.75386330736002178205 ;
    C_p    = 3.57079632679489661922 ;
    RC_p   = 0.28004957675577868795 ;
    N_ITER = 6 ;
  var
    p, V,
    s, c   : Double ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      p := C_p * Sin( lp_phi ) ;
      V := lp_phi * lp_phi ;
      lp_phi := lp_phi * ( 0.895168 + V * ( 0.0218849 + V * 0.00826809 ) );
      i := N_ITER ;
      while i > 0 do begin
        dec( i ) ;
        SinCos( lp_phi, s, c ) ;
        V := ( lp_phi + s * (c + 2) - p     ) /
             ( 1      + c * (c + 2) - s * s ) ;
        lp_phi := lp_phi - V ;
        if Abs( V ) < EPS7 then break ;
      end ;

      if i = 0 then begin
        xy_x := C_x * lp_lam ;
        if lp_phi < 0 then xy_y := -C_y
                      else xy_y :=  C_y ;
      end
      else begin
        xy_x := C_x * lp_lam * ( 1 + Cos( lp_phi ) ) ;
        xy_y := C_y * Sin( lp_phi ) ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEckert_IV.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C_x   = 0.42223820031577120149 ;
    C_y   = 1.32650042817700232218 ;
    C_p   = 3.57079632679489661922 ;
  var
    c      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := utAasin( xy_y / C_y ) ;
      c      := Cos( lp_phi ) ;
      lp_lam := xy_x / ( C_x * (1 + c) );
      lp_phi := utAasin( ( lp_phi + Sin( lp_phi ) * (c + 2) ) / C_p ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_V
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_V.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_V.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_V.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_V.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEckert_V.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    XF  = 0.44101277172455148219 ;
    YF  = 0.88202554344910296438 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := XF * ( 1 + Cos( lp_phi ) ) * lp_lam ;
      xy_y := YF * ( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEckert_V.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RXF = 2.26750802723822639137 ;
    RYF = 1.13375401361911319568 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := RYF * xy_y ;
      lp_lam := RXF * xy_x / (1 + Cos( lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPlateCarree
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPlateCarree.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := 'Equidistant Cylindrical' ;

    datumChanged ;
  end ;

  function TGIS_CSProjPlateCarree.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPlateCarree.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPlateCarree.datumChanged ;
  begin
    inherited datumChanged ;

    Q_rc := Cos( FParameters.StandardParallel_1 ) ;
    if Q_rc < 0 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPlateCarree.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := Q_rc * lp_lam ;
      xy_y := lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPlateCarree.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y ;
      lp_lam := xy_x / Q_rc ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEquidistantConic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEquidistantConic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 20.0 ) ;
    FParameters.StandardParallel_2 := DegToRad( 60.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjEquidistantConic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEquidistantConic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEquidistantConic.datumChanged ;
  var
    cosphi,
    sinphi : Double  ;
    secant : Boolean ;
    ml1,
    m1     : Double  ;
  begin
    inherited datumChanged ;

    Q_phi1 := FParameters.StandardParallel_1 ;
    Q_phi1 := FParameters.StandardParallel_2 ;

    if Abs( Q_phi1 + Q_phi2 ) < EPS10 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    Q_en := utEnfn( P_es ) ;

    SinCos( Q_phi1, sinphi, cosphi ) ;
    Q_n    := sinphi        ;
    secant := Abs( Q_phi1 - Q_phi2) >= EPS10 ;
    if P_es > 0 then begin

      m1   := utMsfn( sinphi, cosphi, P_es ) ;
      ml1  := utMlfn( Q_phi1, sinphi, cosphi, Q_en ) ;
      if secant then begin // secant cone
        SinCos( Q_phi2, sinphi, cosphi ) ;
        Q_n    := ( m1 - utMsfn( sinphi, cosphi, P_es ) ) /
                  ( utMlfn( Q_phi2, sinphi, cosphi, Q_en ) - ml1 );
      end ;
      Q_c    := ml1 + m1 / Q_n ;
      Q_rho0 := Q_c - utMlfn( P_phi0, Sin( P_phi0 ), Cos( P_phi0 ), Q_en ) ;
    end
    else begin
      if secant then
        Q_n := ( cosphi - Cos( Q_phi2 ) ) / ( Q_phi2 - Q_phi1 ) ;

      Q_c    := Q_phi1 + Cos( Q_phi1 ) / Q_n ;
      Q_rho0 := Q_c - P_phi0 ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjEquidistantConic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then
        Q_rho := Q_c - utMlfn( lp_phi, Sin( lp_phi ), Cos( lp_phi ), Q_en )
      else
        Q_rho := Q_c - lp_phi ;

      lp_lam := lp_lam * Q_n ;
      xy_x   := Q_rho * Sin( lp_lam );
      xy_y   := Q_rho0 - Q_rho * Cos( lp_lam ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEquidistantConic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y  := Q_rho0 - xy_y ;
      Q_rho := Hypot( xy_x, xy_y ) ;
      if Q_rho <> 0.0 then begin
        if Q_n < 0 then begin
          Q_rho := -Q_rho ;
          xy_x  := -xy_x  ;
          xy_y  := -xy_y  ;
        end ;
        lp_phi := Q_c - Q_rho ;
        if P_es <> 0 then
          lp_phi := utInv_mlfn( lp_phi, P_es, Q_en ) ;
        lp_lam := ArcTan2( xy_x, xy_y ) / Q_n ;
      end
      else begin
        lp_lam := 0 ;
        if Q_n > 0 then lp_phi :=  HALFPI
                   else lp_phi := -HALFPI ;
      end ;

      if lp_phi > GIS_MAX_SINGLE  then begin
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE );
        exit ;
      end;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjSinusoidal
//------------------------------------------------------------------------------

  constructor TGIS_CSProjSinusoidal.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '(Sanson-Flamsteed)' ;

    Q_n    := 1 ;
    Q_m    := 0 ;
    Q_sphr := False ;

    datumChanged ;
  end ;

  function TGIS_CSProjSinusoidal.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjSinusoidal.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjSinusoidal.datumChanged ;
  begin
    inherited datumChanged ;

    if Q_sphr then P_es := 0 ; ;

    Q_en := utEnfn( P_es ) ;

    if P_es = 0 then begin
      Q_C_y := Sqrt( (Q_m + 1) / Q_n ) ;
      Q_C_x := Q_C_y / (Q_m + 1) ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjSinusoidal.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 8 ;
  var
    s, c   : Double  ;
    k, V   : Double  ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoid

        SinCos( lp_phi, s, c ) ;
        xy_y := utMlfn( lp_phi, s, c, Q_en ) ;
        xy_x := lp_lam * c / Sqrt( 1 - P_es * s * s ) ;

      end
      else begin // sphere

        if Q_m = 0 then begin
          if Q_n <> 1 then lp_phi := utAasin( Q_n * Sin( lp_phi ) ) ;
        end
        else begin
          k := Q_n * Sin( lp_phi ) ;
          i := N_ITER ;
          while i > 0 do begin
            dec( i ) ;
            V      := Q_m * lp_phi + Sin( lp_phi ) - k ;
            lp_phi := lp_phi - V / ( Q_m + Cos( lp_phi ) ) ;
            if Abs( V ) < EPS7 then
              break ;
          end ;
          if i = 0 then begin
            FError := ERROR_UNSOLVABLE ;
            projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;
        end ;
        xy_x := Q_C_x * lp_lam * ( Q_m + Cos( lp_phi ) ) ;
        xy_y := Q_C_y * lp_phi ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjSinusoidal.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    s      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoid

        lp_phi := utInv_mlfn( xy_y, P_es, Q_en ) ;

        if lp_phi > GIS_MAX_SINGLE then begin
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end;

        s := Abs( lp_phi ) ;
        if s < HALFPI then begin
          s      := Sin( lp_phi );
          lp_lam := xy_x * Sqrt( 1 - P_es * s * s ) / Cos( lp_phi ) ;
        end
        else if ( s - EPS10) < HALFPI then begin
          lp_lam := 0 ;
        end
        else begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

      end
      else begin // sphere
        xy_y := xy_y / Q_C_y ;

        if      Q_m <> 0 then
                lp_phi := utAasin( ( Q_m * xy_y + Sin( xy_y ) ) / Q_n )
        else if Q_n <> 1 then
                lp_phi := utAasin( Sin( xy_y ) / Q_n)
        else    lp_phi := xy_y ;

        lp_lam := xy_x / ( Q_C_x * ( Q_m + Cos( xy_y ) ) ) ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEckert_VI
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEckert_VI.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_m    := 1 ;
    Q_n    := 2.570796326794896619231321691 ;
    Q_sphr := True ;

    datumChanged ;
  end ;

  function TGIS_CSProjEckert_VI.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEckert_VI.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEckert_VI.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFlatPolarSinusoidal
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFlatPolarSinusoidal.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '(McBryde-Thomas)' ;

    Q_m    := 0.5 ;
    Q_n    := 1.785398163397448309615660845 ;
    Q_sphr := True ;

    datumChanged ;
  end ;

  function TGIS_CSProjFlatPolarSinusoidal.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFlatPolarSinusoidal.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFlatPolarSinusoidal.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGnomic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGnomic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjGnomic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGnomic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGnomic.datumChanged ;
  begin
    inherited datumChanged ;

    if Abs( Abs( P_phi0 ) - HALFPI ) < EPS10 then begin
      if P_phi0 < 0 then Q_mode := S_POLE
                    else Q_mode := N_POLE ;
    end
    else if Abs( P_phi0 ) < EPS10 then begin
      Q_mode := EQUIT ;
    end
    else begin
      Q_mode   := OBLIQ ;
      SinCos( P_phi0, Q_sinph0, Q_cosph0 ) ;
    end ;

    set_Validity( FParameters.CentralMeridian , ZONE_60,
                  FParameters.LatitudeOfOrigin, ZONE_60
                ) ;
  end ;

  procedure TGIS_CSProjGnomic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    coslam,
    cosphi,
    sinphi : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_phi, sinphi, cosphi ) ;
      coslam := Cos( lp_lam ) ;
      case Q_mode of
        EQUIT :  xy_y :=  cosphi * coslam ;
        OBLIQ :  xy_y :=  Q_sinph0 * sinphi + Q_cosph0 * cosphi * coslam ;
        S_POLE:  xy_y := -sinphi ;
        N_POLE:  xy_y :=  sinphi ;
        else     begin
                   // untested case
                   xy_y := 0 ;
                 end ;
      end ;
      if xy_y < DEGREE10 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      xy_y := 1 / xy_y ;
      xy_x := xy_y * cosphi * Sin( lp_lam ) ;

      case Q_mode of
        EQUIT  :  xy_y   :=  xy_y * sinphi ;
        OBLIQ  :  xy_y   :=  xy_y *
                             ( Q_cosph0 * sinphi -Q_sinph0 * cosphi * coslam );
        N_POLE :  xy_y   := -xy_y * cosphi * coslam ;
        S_POLE :  xy_y   :=  xy_y * cosphi * coslam ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjGnomic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    rh,
    cosz,
    sinz   : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      rh     := Hypot( xy_x, xy_y ) ;
      lp_phi := ArcTan( rh ) ;
      sinz   := Sin( lp_phi );
      cosz   := Sqrt( 1 - sinz * sinz ) ;
      if Abs( rh ) <= EPS10 then begin
        lp_phi := P_phi0 ;
        lp_lam := 0 ;
      end
      else begin
        case Q_mode of
          OBLIQ :  begin
                     lp_phi := cosz * Q_sinph0 + xy_y * sinz * Q_cosph0 / rh ;
                     if Abs( lp_phi ) >= 1 then begin
                       if lp_phi > 0 then lp_phi :=  HALFPI
                                     else lp_phi := -HALFPI ;
                     end
                     else
                       lp_phi := ArcSin( lp_phi ) ;
                     xy_y := ( cosz - Q_sinph0 * Sin( lp_phi ) ) * rh ;
                     xy_x := xy_x * sinz * Q_cosph0 ;
                   end ;
          EQUIT :  begin
                     lp_phi := xy_y * sinz / rh ;
                     if Abs( lp_phi ) >= 1 then begin
                       if lp_phi > 0 then lp_phi :=  HALFPI
                                     else lp_phi := -HALFPI ;
                     end
                     else
                       lp_phi := ArcSin( lp_phi ) ;
                     xy_y := cosz * rh ;
                     xy_x := xy_x * sinz ;
                   end ;
          S_POLE : begin
                     lp_phi := lp_phi - HALFPI ;
                   end ;
          N_POLE : begin
                     lp_phi := HALFPI - lp_phi;
                     xy_y   := -xy_y;
                   end ;
        end ;
        lp_lam := ArcTan2( xy_x, xy_y ) ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambertAzimuthalEqualArea
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLambertAzimuthalEqualArea.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '(Lambert)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjLambertAzimuthalEqualArea.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLambertAzimuthalEqualArea.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLambertAzimuthalEqualArea.datumChanged ;
  var
    t      : Double ;
    sinphi : Double ;
  begin
    inherited datumChanged ;

    t := Abs( P_phi0 ) ;
    if Abs( t - HALFPI ) < EPS10 then begin
      if P_phi0 < 0 then Q_mode := S_POLE
                    else Q_mode := N_POLE ;
    end
    else if Abs( t ) < EPS10 then begin
      Q_mode := EQUIT
    end
    else begin
      Q_mode := OBLIQ ;
    end ;

    if P_es <> 0 then begin
      P_e   := Sqrt( P_es ) ;
      Q_qp  := utQsfn( 1, P_e, P_one_es ) ;
      Q_mmf := 0.5 / ( 1 - P_es ) ;
      Q_apa := utAuthset( P_es ) ;
      case Q_mode of
        N_POLE,
        S_POLE : begin
                   Q_dd := 1 ;
                 end ;
        EQUIT  : begin
                   Q_rq  := Sqrt( 0.5 * Q_qp ) ;
                   Q_dd  := 1 / Q_rq ;
                   Q_xmf := 1 ;
                   Q_ymf := 0.5 * Q_qp ;
                 end ;
        OBLIQ  : begin
                   Q_rq    := Sqrt( 0.5 * Q_qp ) ;
                   sinphi  := Sin( P_phi0 ) ;
                   Q_sinb1 := utQsfn( sinphi, P_e, P_one_es ) / Q_qp ;
                   Q_cosb1 := Sqrt( 1 - Q_sinb1 * Q_sinb1 ) ;
                   Q_dd    := Cos( P_phi0 ) /
                              ( Sqrt( 1 - P_es * sinphi * sinphi ) *
                                Q_rq * Q_cosb1
                              ) ;
                   Q_xmf   := Q_rq ;
                   Q_ymf   := Q_xmf / Q_dd ;
                   Q_xmf   := Q_xmf * Q_dd ;
                 end ;
      end ;
    end
    else begin
      if Q_mode = OBLIQ then begin
        SinCos( P_phi0, Q_sinph0, Q_cosph0 ) ;
      end
    end ;

    case Q_mode of
      N_POLE :
        set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                      Pi/4                        , ZONE_180,  ZONE_0  , ZONE_90
                    ) ;

      S_POLE :
        set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180,  ZONE_180,
                      -Pi/4                       , ZONE_180, -ZONE_90 , -ZONE_0
                    ) ;
      EQUIT,
      OBLIQ  :
        set_Validity( FParameters.CentralMeridian , ZONE_170, -ZONE_180, ZONE_180,
                      FParameters.LatitudeOfOrigin, ZONE_90 , -ZONE_180 , ZONE_180
                    ) ;
    end;
  end ;

  procedure TGIS_CSProjLambertAzimuthalEqualArea.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    coslam : Double ;
    sinlam : Double ;
    sinphi : Double ;
    cosphi : Double ;
    q_     : Double ;
    sinb   : Double ;
    cosb   : Double ;
    b      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoid

        SinCos( lp_lam, sinlam, coslam ) ;
        sinphi := Sin( lp_phi ) ;
        q_ := utQsfn( sinphi, P_e, P_one_es ) ;
        if ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT) then begin
          sinb := q_ / Q_qp ;
          cosb := Sqrt( 1 - sinb * sinb ) ;
        end
        else begin
          sinb := 0 ;
          cosb := 0 ;
        end;
        case Q_mode of
          OBLIQ  : b := 1 + Q_sinb1 * sinb + Q_cosb1 * cosb * coslam ;
          EQUIT  : b := 1 + cosb * coslam ;
          N_POLE : begin
                     b  := HALFPI + lp_phi ;
                     q_ := Q_qp - q_ ;
                   end ;
          S_POLE : begin
                     b  := lp_phi - HALFPI ;
                     q_ := Q_qp + q_ ;
                   end ;
          else     b := 0 ;
        end ;
        if Abs( b ) < EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        case Q_mode of
          OBLIQ  : begin
                     b    := Sqrt( 2 / b ) ;
                     xy_y := Q_ymf * b *
                            ( Q_cosb1 * sinb - Q_sinb1 * cosb * coslam ) ;
                     xy_x := Q_xmf * b * cosb * sinlam ;
                   end ;
          EQUIT  : begin
                     b    := Sqrt( 2 / ( 1 + cosb * coslam ) ) ;
                     xy_y := b * sinb * Q_ymf ;
                     xy_x := Q_xmf * b * cosb * sinlam ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if q_ > 0 then begin
                       b    := Sqrt( q_ ) ;
                       xy_x := b * sinlam ;
                       if Q_mode = S_POLE then xy_y :=   coslam * b
                                          else xy_y := - coslam * b ;
                     end
                     else begin
                       xy_x := 0 ;
                       xy_y := 0 ;
                     end ;
                   end ;
        else     begin
                   // untested case
                   xy_x := 0 ;
                   xy_y := 0 ;
                 end ;
        end ;
      end
      else begin // sphere

        SinCos( lp_phi, sinphi, cosphi ) ;
        coslam := Cos( lp_lam ) ;

        case Q_mode of
          EQUIT,
          OBLIQ  : begin
                     if Q_mode = EQUIT
                       then xy_y := 1 + cosphi * coslam
                       else xy_y := 1 + Q_sinph0 * sinphi +
                                        Q_cosph0 * cosphi * coslam ;

                     if xy_y <= EPS10 then begin
                       FError := ERROR_UNSOLVABLE ;
                       projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                       exit ;
                     end ;
                     xy_y := Sqrt( 2 / xy_y ) ;
                     xy_x := xy_y * cosphi * Sin( lp_lam ) ;
                     if Q_mode = EQUIT then
                       xy_y := xy_y * sinphi
                     else
                       xy_y := xy_y *
                               ( Q_cosph0 * sinphi -
                                 Q_sinph0 * cosphi * coslam
                               ) ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = N_POLE then
                       coslam := -coslam ;

                     if Abs( lp_phi + P_phi0 ) < EPS10 then begin
                       FError := ERROR_UNSOLVABLE ;
                       projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                       exit ;
                     end ;
                     xy_y := FORTPI - lp_phi * 0.5 ;

                     if Q_mode = S_POLE then xy_y := 2 * Cos( xy_y )
                                        else xy_y := 2 * Sin( xy_y ) ;
                     xy_x := xy_y * Sin( lp_lam ) ;
                     xy_y := xy_y * coslam;
                   end ;
          else     begin
                     // untested case
                     xy_x := 0 ;
                     xy_y := 0 ;
                   end ;
        end ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjLambertAzimuthalEqualArea.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    cCe    : Double ;
    sCe    : Double ;
    q_     : Double ;
    rho    : Double ;
    ab     : Double ;
    cosz   : Double ;
    rh     : Double ;
    sinz   : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoid

        case Q_mode of
          EQUIT,
          OBLIQ  : begin
                     xy_x := xy_x / Q_dd ;
                     xy_y := xy_y * Q_dd ;
                     rho  := Hypot( xy_x , xy_y ) ;
                     if rho < EPS10 then begin
                       lp_lam := 0      ;
                       lp_phi := P_phi0 ;
                       unprojectPost( _coords, lp_lam, lp_phi ) ;
                       exit ;
                     end ;
                     if ( 0.5 * rho / Q_rq ) > 1 then begin
                       lp_lam := 0      ;
                       lp_phi := P_phi0 ;
                       unprojectPost( _coords, lp_lam, lp_phi ) ;
                       exit ;
                     end ;
                     sCe := 2 * ArcSin( 0.5 * rho / Q_rq ) ;
                     cCe := Cos( sCe ) ;
                     sCe := Sin( sCe ) ;
                     xy_x := xy_x * sCe ;

                     if Q_mode = OBLIQ then begin
                       ab   := cCe * Q_sinb1 + xy_y * sCe * Q_cosb1 / rho ;
                       xy_y := rho * Q_cosb1 * cCe - xy_y * Q_sinb1 * sCe ;
                     end
                     else begin
                       ab   := xy_y * sCe / rho ;
                       xy_y := rho * cCe ;
                     end ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = N_POLE then
                       xy_y := -xy_y ;
                     q_   := xy_x * xy_x + xy_y * xy_y ;
                     if q_ = 0 then begin
                       lp_lam := 0      ;
                       lp_phi := P_phi0 ;
                       unprojectPost( _coords, lp_lam, lp_phi ) ;
                       exit ;
                     end ;
                     ab := 1 - q_ / Q_qp ;
                     if Q_mode = S_POLE then
                       ab := - ab ;
                   end ;
          else     ab := 0 ;
        end ;
        lp_lam := ArcTan2( xy_x, xy_y ) ;
        lp_phi := utAuthlat( ArcSin( ab ), Q_apa ) ;

      end
      else begin // sphere

        rh     := Hypot( xy_x, xy_y ) ;
        lp_phi := rh * 0.5 ;
        if lp_phi > 1 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        lp_phi := 2 * ArcSin( lp_phi ) ;
        if ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT ) then begin
          SinCos( lp_phi, sinz, cosz ) ;
        end
        else begin
          sinz := 0 ;
          cosz := 0 ;
        end;

        case Q_mode of
          EQUIT  : begin
                     if Abs( rh ) <= EPS10
                       then lp_phi := 0
                       else lp_phi := ArcSin( xy_y * sinz / rh ) ;

                     xy_x := xy_x * sinz ;
                     xy_y := cosz * rh   ;
                   end ;
          OBLIQ  : begin
                     if Abs( rh ) <= EPS10
                       then lp_phi := P_phi0
                       else lp_phi := ArcSin( cosz * Q_sinph0 +
                                              xy_y * sinz * Q_cosph0 / rh
                                            ) ;
                     xy_x := xy_x * sinz * Q_cosph0 ;
                     xy_y := ( cosz - Sin( lp_phi ) * Q_sinph0 ) * rh ;
                   end ;
          N_POLE : begin
                     xy_y   := -xy_y ;
                     lp_phi := HALFPI - lp_phi ;
                   end ;
          S_POLE : begin
                     lp_phi := lp_phi - HALFPI ;
                   end ;
        end ;
        if ( xy_y = 0 ) and ( ( Q_mode = EQUIT ) or ( Q_mode =OBLIQ ) )
          then lp_lam := 0
          else lp_lam := ArcTan2( xy_x, xy_y ) ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLarrive
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLarrivee.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjLarrivee.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLarrivee.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLarrivee.datumChanged ;
  begin
    inherited datumChanged ;

    // not reversible - no bounding polygon
  end ;

  procedure TGIS_CSProjLarrivee.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    SIXTH = 1.0/6 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := 0.5 * lp_lam * ( 1 + Sqrt( Cos( lp_phi ) ) ) ;
      xy_y := lp_phi / ( Cos( 0.5 * lp_phi ) * Cos( SIXTH * lp_lam ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambertConformalConic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLambertConformalConic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 20.0 ) ;
    FParameters.StandardParallel_2 := DegToRad( 60.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjLambertConformalConic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLambertConformalConic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLambertConformalConic.datumChanged ;
  var
    cosphi : Double  ;
    sinphi : Double  ;
    secant : Boolean ;
    ml1    : Double  ;
    m1     : Double  ;
    lat    : Double  ;
  begin
    inherited datumChanged ;

    P_k0   := FParameters.ScaleFactor        ;
    Q_phi1 := FParameters.StandardParallel_1 ;
    Q_phi2 := FParameters.StandardParallel_2 ;
    Q_west := 1 ;  // normally east aspect

    if Abs( Q_phi1 + Q_phi2 ) < EPS10 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    SinCos( Q_phi1, sinphi, cosphi ) ;
    Q_n    := sinphi        ;
    secant := Abs( Q_phi1 - Q_phi2) >= EPS10 ;

    if P_es <> 0 then begin
      P_e := Sqrt( P_es ) ;
      m1  := utMsfn( sinphi, cosphi, P_es ) ;
      ml1 := utTsfn( Q_phi1, sinphi, P_e  ) ;
      if secant then begin // secant cone
        sinphi := Sin( Q_phi2 ) ;
        Q_n    := Ln( m1 / utMsfn( sinphi, Cos( Q_phi2 ), P_es ) );
        Q_n    := Q_n / Ln( ml1 / utTsfn( Q_phi2, sinphi, P_e) ) ;
      end ;
      Q_rho0 := m1 * Power(ml1, -Q_n) / Q_n ;
      Q_c    := Q_rho0 ;
      if Abs( Abs( P_phi0 ) - HALFPI ) < EPS10
        then Q_rho0 := 0
        else Q_rho0 := Q_rho0 * Power( utTsfn( P_phi0, Sin( P_phi0), P_e), Q_n ) ;
    end
    else begin
      if secant then
        Q_n := Ln( cosphi / Cos( Q_phi2) ) /
               Ln( Tan( FORTPI + 0.5 * Q_phi2 ) /
                   Tan( FORTPI + 0.5 * Q_phi1 )
                 ) ;
      Q_c := cosphi * Power( Tan( FORTPI + 0.5 * Q_phi1 ), Q_n ) / Q_n ;

      if Abs( Abs( P_phi0 ) - HALFPI ) < EPS10
        then Q_rho0 := 0
        else Q_rho0 := Q_c * Power( Tan( FORTPI + 0.5 * P_phi0 ), -Q_n ) ;
    end ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 ) / 2 ;

    set_Validity( FParameters.CentralMeridian, ZONE_180, -ZONE_180, ZONE_180,
                  lat                        , ZONE_90 , -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjLambertConformalConic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Abs( Abs( lp_phi ) - HALFPI ) < EPS10 then begin
        if ( lp_phi * Q_n ) <= 0 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;
        Q_rho := 0 ;
      end
      else begin
        if P_es <> 0
          then Q_rho := Q_c * Power( utTsfn( lp_phi, Sin( lp_phi ), P_e ), Q_n )
          else Q_rho := Q_c * Power( Tan( FORTPI + 0.5 * lp_phi ), -Q_n ) ;
      end ;
      lp_lam := lp_lam * Q_n ;
      xy_x := Q_west * P_k0 * ( Q_rho * Sin( lp_lam ) ) ;
      xy_y :=          P_k0 * ( Q_rho0 - Q_rho * Cos( lp_lam ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjLambertConformalConic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    if Q_west < 0 then begin
      // fix False westing
      _coords.X := -_coords.X + 2* P_x0 ;
    end ;

    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_x := xy_x / P_k0 ;
      xy_y := xy_y / P_k0 ;
      xy_y := Q_rho0 - xy_y ;
      Q_rho := Hypot( xy_x, xy_y ) ;
      if Q_rho <> 0 then begin
        if Q_n < 0 then begin
          Q_rho := -Q_rho ;
          xy_x  := -xy_x  ;
          xy_y  := -xy_y  ;
        end ;
        if P_es <> 0 then begin
          lp_phi := utPhi2( Power( Q_rho / Q_c, 1/Q_n ), P_e ) ;
          if lp_phi > GIS_MAX_SINGLE  then begin
            FError:= ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;
        end
        else
          lp_phi := 2 * ArcTan( Power( Q_c / Q_rho, 1/Q_n ) ) - HALFPI ;
        lp_lam := ArcTan2( xy_x, xy_y ) / Q_n;
      end
      else begin
        lp_lam := 0 ;
        if Q_n > 0 then lp_phi :=  HALFPI
                   else lp_phi := -HALFPI;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambertConformalConic_1SP
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLambertConformalConic_1SP.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin := DegToRad( 45.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjLambertConformalConic_1SP.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLambertConformalConic_1SP.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLambertConformalConic_1SP.datumChanged ;
  begin

    P_k0   := FParameters.ScaleFactor       ;
    P_phi0 := FParameters.LatitudeOfOrigin  ;

    FParameters.StandardParallel_1 := P_phi0 ;
    FParameters.StandardParallel_2 := P_phi0 ;

    inherited datumChanged ;

    if P_phi0 >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    P_phi0                     , ZONE_90, -ZONE_85, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    P_phi0                     , ZONE_90, -ZONE_90, ZONE_85
                  ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambertConformalConicWestOriented
//------------------------------------------------------------------------------

  function TGIS_CSProjLambertConformalConicWestOriented.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLambertConformalConicWestOriented.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLambertConformalConicWestOriented.datumChanged ;
  begin
    inherited datumChanged ;

    Q_west := -1 ; // west aspect
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLambertLoximuthal
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLoximuthal.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjLoximuthal.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLoximuthal.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLoximuthal.datumChanged ;
  begin
    inherited datumChanged ;

    Q_phi1 := FParameters.StandardParallel_1 ;

    Q_cosphi1 := Cos( Q_phi1 ) ;
    if Q_cosphi1 < EPS7 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;
    Q_tanphi1 := Tan( FORTPI + 0.5 * Q_phi1 ) ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjLoximuthal.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y := lp_phi - Q_phi1 ;
      if Abs( xy_y ) < EPS7 then
        xy_x := lp_lam * Q_cosphi1
      else begin
        xy_x := FORTPI + 0.5 * lp_phi ;
        if ( Abs( xy_x ) < EPS7 ) or ( Abs( Abs( xy_x ) - HALFPI ) < EPS7 )
          then xy_x := 0
          else xy_x := lp_lam * xy_y / Ln( Tan( xy_x ) / Q_tanphi1 ) ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjLoximuthal.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y + Q_phi1 ;
      if Abs( xy_y ) < EPS7 then
        lp_lam := xy_x / Q_cosphi1
      else begin
        lp_lam := FORTPI + 0.5 * lp_phi ;
        if ( Abs( lp_lam ) < EPS7 ) or
           ( Abs( Abs( lp_lam ) - HALFPI ) < EPS7 )
        then lp_lam := 0
        else lp_lam := xy_x * Ln( Tan( lp_lam ) / Q_tanphi1 ) / xy_y ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMercator
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMercator.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    semiMajor := 0 ;
    semiMinor := 0 ;

    FParameters.StandardParallel_1 := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjMercator.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMercator.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMercator.datumChanged ;
  var
    sp1    : Double  ;
    bext   : Boolean ;
    dflt   : Double  ;
    dmajor : Double  ;
    dminor : Double  ;
  begin
    inherited datumChanged ;

    bext := False ;

    if semiMajor = 0 then
      dmajor := Ellipsoid.SemiMajor
    else begin
      dmajor := semiMajor ;
      bext   := True ;
    end;

    if semiMinor = 0 then
      dminor := Ellipsoid.SemiMajor
    else begin
      dminor := semiMinor ;
      bext   := True ;
    end;

    if bext then begin
      dflt := ( dmajor - dminor ) / dmajor ;
      if dflt <> 0 then
        dflt := 1 / dflt ;

      if ( dmajor <> Ellipsoid.SemiMajor ) or
         ( dminor <> Ellipsoid.SemiMinor )
      then begin
        FEllipsoid := CSEllipsoidList.Prepare( -1, '', dmajor, dflt ) ;

        P_a      := Ellipsoid.SemiMajor  ;
        P_es     := Ellipsoid.EcntrMajor ;
        P_e      := Sqrt( P_es ) ;
        P_ra     := 1 / P_a      ;

        if Abs( P_es ) > EPS7 then begin
          P_one_es  := 1 - P_es     ;
          P_rone_es := 1 / P_one_es ;
        end ;
      end ;
    end ;

    sp1 := Abs( FParameters.StandardParallel_1 ) ;
    if sp1 <> 0 then begin
      if P_es <> 0 then begin // ellipsoidal
        P_k0 := utMsfn( Sin(sp1), Cos( sp1 ), P_es ) ;
      end
      else begin //spheroid
        P_k0 := Cos( sp1 ) ;
      end ;
    end ;

    if Abs( FParameters.ScaleFactor ) <> 1 then
    begin
      P_k0 := FParameters.ScaleFactor ;
    end;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_87
                ) ;
  end ;

  procedure TGIS_CSProjMercator.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x : Double ;
    xy_y : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi  ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoidal

        if (  Abs( lp_phi ) > HALFPI )
           or
          ( Abs( Abs( lp_phi ) - HALFPI ) <= EPS10 ) then
        begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        xy_x := P_k0 * lp_lam ;
        xy_y := - P_k0 * Ln( utTsfn( lp_phi, Sin( lp_phi ), P_e ) );

      end
      else begin //spheroid

        if Abs( Abs( lp_phi ) - HALFPI ) <= EPS7 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        xy_x := P_k0 * lp_lam ;
        xy_y := P_k0 * Ln( Tan( FORTPI + 0.5 * lp_phi ) ) ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjMercator.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x : Double ;
    xy_y : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoidal

        lp_phi := utPhi2( Exp( -xy_y  / P_k0 ), P_e ) ;
        if lp_phi > GIS_MAX_SINGLE then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        lp_lam := xy_x / P_k0 ;

      end
      else begin //spheroid

        lp_phi := HALFPI - 2 * ArcTan( Exp( -xy_y  / P_k0 ) ) ;
        lp_lam := xy_x / P_k0;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMercator_1SP_Speherical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMercator_1SP_Spherical.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.ScaleFactor        := 1 ;
    FParameters.StandardParallel_1 := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjMercator_1SP_Spherical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMercator_1SP_Spherical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMercator_1SP_Spherical.datumChanged ;
  begin
    semiMajor := Datum.Ellipsoid.SemiMajor ;
    semiMinor := Datum.Ellipsoid.SemiMajor ;

    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_87
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMillerCylindrical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMillerCylindrical.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjMillerCylindrical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMillerCylindrical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMillerCylindrical.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian , ZONE_180,
                  0                           , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjMillerCylindrical.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := lp_lam ;
      xy_y := Ln( Tan( FORTPI + lp_phi * 0.4 ) ) * 1.25 ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjMillerCylindrical.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_lam := xy_x ;
      lp_phi := 2.5 * ( ArcTan( Exp( 0.8 * xy_y ) ) - FORTPI ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMillerOblatedStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMillerOblatedStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.AzimuthalModified ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian   := DegToRad( 20.0 ) ;
    FParameters.LatitudeOfOrigin  := DegToRad( 18.0 ) ;

    SetLength( Q_zcoeff, 2 + 1 ) ;

    Q_zcoeff[0] := GisComplex( 0.924500, 0 ) ;
    Q_zcoeff[1] := GisComplex( 0       , 0 ) ;
    Q_zcoeff[2] := GisComplex( 0.019430, 0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjMillerOblatedStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMillerOblatedStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMillerOblatedStereographic.datumChanged ;
  var
    esphi, chio : Double ;
  begin
    inherited datumChanged ;

    if P_es <> 0 then begin
      esphi := P_e * Sin( P_phi0 ) ;
      chio  := 2 * ArcTan( Tan( ( HALFPI + P_phi0 ) * 0.5) *
               Power( (1-esphi) / (1+esphi), P_e * 0.5 ) ) - HALFPI ;
    end
    else
      chio := P_phi0 ;

    SinCos( chio, Q_schio, Q_cchio ) ;

    set_Validity( FParameters.CentralMeridian , ZONE_90,
                  FParameters.LatitudeOfOrigin, ZONE_90, -ZONE_90, ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjMillerOblatedStereographic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    sinlon,
    coslon,
    esphi,
    chi,
    schi,
    cchi,
    s      : Double ;
    p_     : TGIS_Complex ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_lam, sinlon, coslon ) ;
      esphi  := P_e * Sin( lp_phi ) ;
      chi    := 2 * ArcTan( Tan( ( HALFPI + lp_phi ) * 0.5 )  *
                Power( (1-esphi) / (1+esphi), P_e * 0.5) ) - HALFPI ;
      SinCos( chi, schi, cchi ) ;
      s      := 2 / ( 1 + Q_schio * schi + Q_cchio * cchi * coslon ) ;
      p_.r   := s * cchi * sinlon ;
      p_.i   := s * ( Q_cchio * schi - Q_schio * cchi * coslon ) ;
      p_     := utZpoly1( p_, Q_zcoeff ) ;
      xy_x   := p_.r ;
      xy_y   := p_.i ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjMillerOblatedStereographic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 20 ;
  var
    nn                    : Integer      ;
    p_, fxy, fpxy, dp     : TGIS_Complex ;
    den, rh, z            : Double       ;
    sinz, cosz            : Double       ;
    chi, phi, dphi, esphi : Double       ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      p_.r := xy_x ;
      p_.i := xy_y ;

      nn := N_ITER ;
      phi  := 0 ;
      rh   := 0 ;
      sinz := 0 ;
      cosz := 0 ;
      while nn > 0 do begin
        dec( nn ) ;

        fxy   := utZpolyd1( p_, Q_zcoeff, fpxy ) ;
        fxy.r := fxy.r - xy_x ;
        fxy.i := fxy.i - xy_y ;

        den  := fpxy.r * fpxy.r + fpxy.i * fpxy.i ;
        dp.r := -( fxy.r * fpxy.r + fxy.i * fpxy.i ) / den ;
        dp.i := -( fxy.i * fpxy.r - fxy.r * fpxy.i ) / den ;
        p_.r := p_.r + dp.r ;
        p_.i := p_.i + dp.i ;
        if ( Abs( dp.r ) + Abs(dp.i ) ) <= EPS10 then
          break;
      end ;
      if nn <> 0 then begin
        rh := Hypot( p_.r, p_.i ) ;
        z  := 2 * ArcTan( 0.5 * rh ) ;
        SinCos( z, sinz, cosz ) ;
        lp_lam := P_lam0 ;
        if Abs( rh ) <= EPS10 then begin
          lp_phi := P_phi0 ;
          unprojectPost( _coords, lp_lam, lp_phi ) ;
          exit ;
        end ;

        chi := utAasin( cosz * Q_schio + p_.i * sinz * Q_cchio / rh ) ;
        phi := chi ;

        nn := N_ITER ;
        while nn > 0 do begin
          dec( nn ) ;
          esphi := P_e * Sin( phi ) ;
          dphi  := 2 * ArcTan( Tan( ( HALFPI + chi ) * 0.5 ) *
                   Power( (1+esphi) / (1-esphi), P_e * 0.5) ) - HALFPI - phi ;
          phi := phi + dphi ;
          if Abs( dphi ) <= EPS10 then
            break;
        end ;

      end ;
      if nn <> 0 then begin
        lp_phi := phi ;
        lp_lam := ArcTan2( p_.r * sinz, rh * Q_cchio * cosz -
                           p_.i * Q_schio * sinz
                         ) ;
      end
      else begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLeeOblatedStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLeeOblatedStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.AzimuthalModified ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian   := DegToRad( -165.0 ) ;
    FParameters.LatitudeOfOrigin  := DegToRad( - 10.0 ) ;

    SetLength( Q_zcoeff, 2 + 1 ) ;

    Q_zcoeff[0] := GisComplex(  0.721316 ,  0          ) ;
    Q_zcoeff[1] := GisComplex(  0        ,  0          ) ;
    Q_zcoeff[2] := GisComplex( -0.0088162, -0.00617325 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjLeeOblatedStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLeeOblatedStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLeeOblatedStereographic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian , ZONE_90, -ZONE_180, ZONE_180,
                  FParameters.LatitudeOfOrigin, ZONE_90, -ZONE_90 , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGS48
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGS48.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.AzimuthalModified ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(Modified Stereographic For 48 U.S.)' ;

    FParameters.CentralMeridian   := DegToRad( - 96.0 ) ;
    FParameters.LatitudeOfOrigin  := DegToRad(   39.0 ) ;

    SetLength( Q_zcoeff, 4 + 1 ) ;
    Q_zcoeff[0] := GisComplex(  0.98879 , 0 ) ;
    Q_zcoeff[1] := GisComplex(  0       , 0 ) ;
    Q_zcoeff[2] := GisComplex( -0.050909, 0 ) ;
    Q_zcoeff[3] := GisComplex(  0       , 0 ) ;
    Q_zcoeff[4] := GisComplex(  0.075528, 0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjGS48.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGS48.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGS48.datumChanged ;
  var
    chio : Double ;
  begin
    inherited datumChanged ;

    P_es := 0       ;
    P_a  := 6370997 ;
    P_ra := 1 / P_a ;

    P_one_es  := 0 ;
    P_rone_es := 0 ;

    chio := P_phi0 ;

    SinCos( chio, Q_schio, Q_cchio ) ;

    set_Validity( FParameters.CentralMeridian , ZONE_45,
                  FParameters.LatitudeOfOrigin, ZONE_45
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGS50
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGS50.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.AzimuthalModified ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FDescription   := '(Modified Stereographic For 50 U.S.)' ;

    FParameters.CentralMeridian   := DegToRad( -120.0 ) ;
    FParameters.LatitudeOfOrigin  := DegToRad(   45.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjGS50.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGS50.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGS50.datumChanged ;
  var
    esphi, chio : Double ;
  begin
    inherited datumChanged ;

    SetLength( Q_zcoeff, 9 + 1 ) ;

    if P_es <> 0 then begin// fixed ellipsoid/sphere
      P_a  := 6378206.4 ;
      P_es := 0.00676866 ;
      P_e  := Sqrt( P_es ) ;

      // GS50 ellipsoid
      Q_zcoeff[0] := GisComplex(  0.9827497,  0.0       ) ;
      Q_zcoeff[1] := GisComplex(  0.0210669,  0.0053804 ) ;
      Q_zcoeff[2] := GisComplex( -0.1031415, -0.0571664 ) ;
      Q_zcoeff[3] := GisComplex( -0.0323337, -0.0322847 ) ;
      Q_zcoeff[4] := GisComplex(  0.0502303,  0.1211983 ) ;
      Q_zcoeff[5] := GisComplex(  0.0251805,  0.0895678 ) ;
      Q_zcoeff[6] := GisComplex( -0.0012315, -0.1416121 ) ;
      Q_zcoeff[7] := GisComplex(  0.0072202, -0.1317091 ) ;
      Q_zcoeff[8] := GisComplex( -0.0194029,  0.0759677 ) ;
      Q_zcoeff[9] := GisComplex( -0.0210072,  0.0834037 ) ;

      esphi := P_e * Sin( P_phi0 ) ;
      chio  := 2 * ArcTan( Tan( ( HALFPI + P_phi0 ) * 0.5) *
               Power( (1-esphi) / (1+esphi), P_e * 0.5 ) ) - HALFPI ;

    end
    else begin
      P_a  := 6370997 ;

      // GS50 sphere
      Q_zcoeff[0] := GisComplex(  0.9842990,  0.0       ) ;
      Q_zcoeff[1] := GisComplex(  0.0211642,  0.0037608 ) ;
      Q_zcoeff[2] := GisComplex( -0.1036018, -0.0575102 ) ;
      Q_zcoeff[3] := GisComplex( -0.0329095, -0.0320119 ) ;
      Q_zcoeff[4] := GisComplex(  0.0499471,  0.1223335 ) ;
      Q_zcoeff[5] := GisComplex(  0.0260460,  0.0899805 ) ;
      Q_zcoeff[6] := GisComplex(  0.0007388, -0.1435792 ) ;
      Q_zcoeff[7] := GisComplex(  0.0075848, -0.1334108 ) ;
      Q_zcoeff[8] := GisComplex( -0.0216473,  0.0776645 ) ;
      Q_zcoeff[9] := GisComplex( -0.0225161,  0.0853673 ) ;

      chio := P_phi0 ;
    end ;

    P_one_es  := 0 ;
    P_rone_es := 0 ;

    SinCos( chio, Q_schio, Q_cchio ) ;

    set_Validity( FParameters.CentralMeridian , ZONE_35,
                  FParameters.LatitudeOfOrigin, ZONE_35
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjModifiedStererographicsForAlaska
//------------------------------------------------------------------------------

  constructor TGIS_CSProjModifiedStererographicsForAlaska.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.AzimuthalModified ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.CentralMeridian   := DegToRad( -152.0 ) ;
    FParameters.LatitudeOfOrigin  := DegToRad(   64.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjModifiedStererographicsForAlaska.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjModifiedStererographicsForAlaska.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjModifiedStererographicsForAlaska.datumChanged ;
  var
    esphi, chio : Double ;
  begin
    inherited datumChanged ;

    SetLength( Q_zcoeff, 9 + 1 ) ;

    if P_es <> 0 then begin// fixed ellipsoid/sphere
      P_a  := 6378206.4 ;
      P_es := 0.00676866 ;
      P_e  := Sqrt( P_es ) ;

      // Alaska ellipsoid
      Q_zcoeff[0] := GisComplex(  0.9945303,  0.0       ) ;
      Q_zcoeff[1] := GisComplex(  0.0052083, -0.0027404 ) ;
      Q_zcoeff[2] := GisComplex(  0.0072721,  0.0048181 ) ;
      Q_zcoeff[3] := GisComplex( -0.0151089, -0.1932526 ) ;
      Q_zcoeff[4] := GisComplex(  0.0642675, -0.1381226 ) ;
      Q_zcoeff[5] := GisComplex(  0.3582802, -0.2884586 ) ;

      esphi := P_e * Sin( P_phi0 ) ;
      chio  := 2 * ArcTan( Tan( ( HALFPI + P_phi0 ) * 0.5) *
               Power( (1-esphi) / (1+esphi), P_e * 0.5 ) ) - HALFPI ;
    end
    else begin
      P_a  := 6370997 ;

      // Alaska sphere
      Q_zcoeff[0] := GisComplex(  0.9972523,  0.0       ) ;
      Q_zcoeff[1] := GisComplex(  0.0052513, -0.0041175 ) ;
      Q_zcoeff[2] := GisComplex(  0.0074606,  0.0048125 ) ;
      Q_zcoeff[3] := GisComplex( -0.0153783, -0.1968253 ) ;
      Q_zcoeff[4] := GisComplex(  0.0636871, -0.1408027 ) ;
      Q_zcoeff[5] := GisComplex(  0.3660976, -0.2937382 ) ;

      chio := P_phi0 ;
    end ;

    P_one_es  := 0 ;
    P_rone_es := 0 ;

    SinCos( chio, Q_schio, Q_cchio ) ;

    set_Validity( FParameters.CentralMeridian , ZONE_20,
                  FParameters.LatitudeOfOrigin, ZONE_20, -ZONE_90, ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMollweide
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMollweide.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  var
     r, sp, p2, p_ : Double ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    p_ := HALFPI ;
    p2 := p_ + p_ ;

    sp := Sin( p_ ) ;
    r  := Sqrt( TWOPI * sp / ( p2 + Sin( p2 ) ) ) ;
    Q_C_x := 2 * r / Pi ;
    Q_C_y := r / sp ;
    Q_C_p := p2 + Sin( p2 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjMollweide.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMollweide.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMollweide.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjMollweide.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
  var
    k, V   : Double  ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      k := Q_C_p * Sin( lp_phi ) ;
      i := N_ITER ;
      while i > 0 do begin
        dec( i ) ;
        V := ( lp_phi + Sin( lp_phi ) - k ) / ( 1 + Cos( lp_phi ) ) ;
        lp_phi := lp_phi - V ;
        if Abs( V ) < EPS7 then
          break;
      end ;
      if i = 0 then begin
        if lp_phi < 0 then lp_phi := -HALFPI
                      else lp_phi :=  HALFPI ;
      end
      else
        lp_phi := lp_phi * 0.5 ;
      xy_x := Q_C_x * lp_lam * Cos( lp_phi ) ;
      xy_y := Q_C_y * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjMollweide.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := utAasin( xy_y / Q_C_y ) ;
      lp_lam := xy_x / ( Q_C_x * Cos( lp_phi ) ) ;
      lp_phi := lp_phi + lp_phi;
      lp_phi := utAasin( ( lp_phi + Sin( lp_phi ) ) / Q_C_p ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_IV
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_IV.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  var
     r, sp, p2, p_ : Double ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    p_ := Pi / 3 ;
    p2 := p_ + p_ ;

    sp := Sin( p_ ) ;
    r  := Sqrt( TWOPI * sp / ( p2 + Sin( p2 ) ) ) ;
    Q_C_x := 2 * r / Pi ;
    Q_C_y := r / sp ;
    Q_C_p := p2 + Sin( p2 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_IV.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_IV.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_IV.datumChanged ;
  begin
    // parametrs same as Mollewide
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_V
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_V.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := 0.90977 ;
    Q_C_y := 1.65014 ;
    Q_C_p := 3.00896 ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_V.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_V.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_V.datumChanged ;
  begin
    // parametrs same as Mollweide
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjNewZealandMapGrid
//------------------------------------------------------------------------------

  constructor TGIS_CSProjNewZealandMapGrid.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '' ;

    FParameters.FalseEasting     := 2510000 ;
    FParameters.FalseNorthing    := 6023150 ;
    FParameters.CentralMeridian  := DegToRad( 173.0 ) ;
    FParameters.LatitudeOfOrigin := DegToRad( -41.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjNewZealandMapGrid.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjNewZealandMapGrid.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjNewZealandMapGrid.datumChanged ;
  begin
    inherited datumChanged ;

  SetLength( Q_bf, 6 ) ;
    Q_bf[0] := GisComplex(  0.7557853228, 0.0        ) ;
    Q_bf[1] := GisComplex(  0.249204646 , 0.003371507) ;
    Q_bf[2] := GisComplex( -0.001541739 , 0.041058560) ;
    Q_bf[3] := GisComplex( -0.10162907  , 0.01727609 ) ;
    Q_bf[4] := GisComplex( -0.26623489  ,-0.36249218 ) ;
    Q_bf[5] := GisComplex( -0.6870983   ,-1.1651967  ) ;

    Q_tphi[0] :=  1.5627014243 ;
    Q_tphi[1] :=  0.5185406398 ;
    Q_tphi[2] := -0.03333098 ;
    Q_tphi[3] := -0.1052906 ;
    Q_tphi[4] := -0.0368594 ;
    Q_tphi[5] :=  0.007317 ;
    Q_tphi[6] :=  0.01220 ;
    Q_tphi[7] :=  0.00394 ;
    Q_tphi[8] := -0.0013 ;

    Q_tpsi[0] :=  0.6399175073 ;
    Q_tpsi[1] := -0.1358797613 ;
    Q_tpsi[2] :=  0.063294409 ;
    Q_tpsi[3] := -0.02526853 ;
    Q_tpsi[4] :=  0.0117879 ;
    Q_tpsi[5] := -0.0055161 ;
    Q_tpsi[6] :=  0.0026906 ;
    Q_tpsi[7] := -0.001333 ;
    Q_tpsi[8] :=  0.00067 ;
    Q_tpsi[9] := -0.00034 ;

    set_Validity( FParameters.CentralMeridian , ZONE_10,
                  FParameters.LatitudeOfOrigin, ZONE_10
                ) ;
  end ;

  procedure TGIS_CSProjNewZealandMapGrid.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RAD_TO_SEC5 = 2.062648062470963551564733573  ;
  var
    p_     : TGIS_Complex ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := ( lp_phi - P_phi0 ) * RAD_TO_SEC5 ;
      p_.r := Q_tpsi[ high(Q_tpsi) ] ;
      for i:=high(Q_tpsi)-1 downto 0 do
        p_.r := Q_tpsi[i] + lp_phi * p_.r ;
      p_.r := p_.r * lp_phi ;
      p_.i := lp_lam ;
      p_   := utZpoly1( p_, Q_bf ) ;
      xy_x := p_.i ;
      xy_y := p_.r ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjNewZealandMapGrid.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    SEC5_TO_RAD = 0.4848136811095359935899141023 ;
    N_ITER      = 20 ;
  var
    nn, i  : Integer ;
    p_, f,
    fp, dp : TGIS_Complex ;
    den    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      p_.r := xy_y ;
      p_.i := xy_x ;
      nn := N_ITER ;
      while nn > 0 do begin
        dec( nn ) ;

        f := utZpolyd1( p_, Q_bf, fp ) ;
        f.r := f.r - xy_y ;
        f.i := f.i - xy_x ;
        den := fp.r * fp.r + fp.i * fp.i;

        dp.r := -(f.r * fp.r + f.i * fp.i) / den ;
        p_.r := p_.r + dp.r ;
        dp.i := -(f.i * fp.r - f.r * fp.i) / den;
        p_.i := p_.i + dp.i ;
        if Abs(dp.r) + Abs(dp.i) <= EPS10 then
          break;
      end ;
      if nn <> 0 then begin
        lp_lam := p_.i ;
        lp_phi := Q_tphi[ high(Q_tphi) ] ;
        for i := high(Q_tphi)-1 downto 0 do
          lp_phi := Q_tphi[i] + p_.r * lp_phi ;
        lp_phi := P_phi0 + p_.r * lp_phi * SEC5_TO_RAD;
      end
      else begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueMercatorTwoPoint
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHotineObliqueMercatorTwoPoint.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfPoint_1,
                        TGIS_CSProjParameter.LatitudeOfPoint_1,
                        TGIS_CSProjParameter.LongitudeOfPoint_2,
                        TGIS_CSProjParameter.LatitudeOfPoint_2,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin   := DegToRad(  45.0 ) ;
    FParameters.LongitudeOfPoint_1 := DegToRad( 180.0 ) ;
    FParameters.LatitudeOfPoint_1  := DegToRad(  80.0 ) ;
    FParameters.LongitudeOfPoint_2 := DegToRad(   0.0 ) ;
    FParameters.LatitudeOfPoint_2  := DegToRad(  60.0 ) ;

    Q_azi := False ;

    datumChanged ;
  end ;

  function TGIS_CSProjHotineObliqueMercatorTwoPoint.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHotineObliqueMercatorTwoPoint.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorTwoPoint.datumChanged ;
  var
    con, com,
    cosph0,
    sinph0,
    d, f, h,
    l, p_, j : Double  ;
  begin
    inherited datumChanged ;

    P_k0 := FParameters.ScaleFactor   ;

    if Q_azi then begin
      Q_lamc  := FParameters.LongitudeOfCenter ;
      P_phi0  := FParameters.LatitudeOfCenter  ;
      Q_alpha := FParameters.Azimuth           ;
    end
    else begin
      P_phi0 := FParameters.LatitudeOfOrigin   ;
      Q_lam1 := FParameters.LongitudeOfPoint_1 ;
      Q_phi1 := FParameters.LatitudeOfPoint_1  ;
      Q_lam2 := FParameters.LongitudeOfPoint_2 ;
      Q_phi2 := FParameters.LatitudeOfPoint_2  ;
    end ;

    con := Abs( Q_phi1 ) ;
    if ( Abs( Q_phi1 - Q_phi2 )        <= EPS10 ) or
       ( con                           <= EPS10 ) or
       ( Abs( con - HALFPI )           <= EPS10 ) or
       ( Abs( Abs( P_phi0 ) - HALFPI ) <= EPS10 ) or
       ( Abs( Abs( Q_phi2 ) - HALFPI ) <= EPS10 ) then
    begin
      FError:= ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    if P_es > 0 then
      com := Sqrt( P_one_es )
    else
      com := 1 ;

    if Abs( P_phi0 ) > EPS7 then begin
      SinCos( P_phi0, sinph0, cosph0 ) ;
      if P_es <> 0 then begin
        con := 1 - P_es * sinph0 * sinph0 ;
        Q_bl := cosph0 * cosph0;
        Q_bl := Sqrt( 1 + P_es * Q_bl * Q_bl / P_one_es ) ;
        Q_al := Q_bl * P_k0 * com / con ;
        d    := Q_bl * com / ( cosph0 * Sqrt( con ) ) ;
      end
      else begin
        Q_bl := 1    ;
        Q_al := P_k0 ;
        d    := 1 / cosph0 ;
      end ;

      f := d * d - 1 ;
      if f <= 0 then
        f := 0
      else begin
        f := Sqrt( f ) ;
        if P_phi0 < 0 then
          f := -f ;
      end ;

      f    := f + d;
      Q_el := f ;
      if P_es <> 0
        then Q_el := Q_el * Power( utTsfn( P_phi0, sinph0, P_e ), Q_bl )
        else Q_el := Q_el * Tan( 0.5 * ( HALFPI - ( P_phi0 ) ) )
    end
    else begin
      Q_bl := 1 / com ;
      Q_al := P_k0 ;
      f    := 1 ;
      d    := 1 ;
      Q_el := 1 ;
    end ;

    if Q_azi then begin
      Q_gamma := ArcSin( Sin( Q_alpha ) / d ) ;
      P_lam0  := Q_lamc - ArcSin( ( 0.5 * ( f - 1/f ) ) * Tan( Q_gamma ) ) /
                 Q_bl ;
    end
    else begin
      if P_es <> 0 then begin
        h := Power( utTsfn( Q_phi1, Sin( Q_phi1 ), P_e ), Q_bl ) ;
        l := Power( utTsfn( Q_phi2, Sin( Q_phi2 ), P_e ), Q_bl ) ;
      end
      else begin
        h := Tan( 0.5 * ( HALFPI - ( Q_phi1 ) ) ) ;
        l := Tan( 0.5 * ( HALFPI - ( Q_phi2 ) ) ) ;
      end ;

      f  := Q_el / h ;
      p_ := (l - h) / (l + h) ;
      j  := Q_el * Q_el ;
      j  := (j - l * h) / (j + l * h) ;

      con := Q_lam1 - Q_lam2 ;
      if      con < -Pi then Q_lam2 := Q_lam2 - TWOPI
      else if con >  Pi then Q_lam2 := Q_lam2 + TWOPI ;

      P_lam0  := utAdjlon( 0.5 * (Q_lam1 + Q_lam2) -
                           ArcTan( j * Tan( 5 * Q_bl * (Q_lam1 - Q_lam2) ) / p_
                                 ) / Q_bl
                         ) ;
      Q_gamma := ArcTan( 2 * Sin( Q_bl * utAdjlon( Q_lam1 - P_lam0 ) ) /
                         ( f - 1/f )
                       ) ;
      Q_alpha := ArcSin( d * Sin( Q_gamma ) ) ;
    end ;

    SinCos( Q_gamma, Q_singam, Q_cosgam ) ;

    SinCos( Q_alpha, Q_sinrot, Q_cosrot ) ;

    Q_u_0    := Abs( Q_al * ArcTan( Sqrt( d * d - 1) / Q_cosrot ) / Q_bl);

    if P_phi0 < 0 then
      Q_u_0 := - Q_u_0 ;

    if FParameters.XYPlaneRotation <> Q_alpha then begin
      SinCos( FParameters.XYPlaneRotation, Q_sinrot, Q_cosrot ) ;
    end;

    set_Validity( P_lam0, ZONE_60,
                  P_phi0, ZONE_60, -ZONE_90, ZONE_90
                )
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorTwoPoint.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    con, q_, s, ul, us, vl, vs : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      vl := Sin( Q_bl * lp_lam ) ;
      if Abs( Abs( lp_phi ) - HALFPI ) <= EPS7 then begin
        if lp_phi < 0 then ul := -Q_singam
                      else ul :=  Q_singam ;
        us := Q_al * lp_phi / Q_bl ;
      end
      else begin
        if P_es <> 0
          then q_ := Q_el / Power( utTsfn( lp_phi, Sin( lp_phi ), P_e ), Q_bl )
          else q_ := Q_el / Tan( 0.5 * ( HALFPI - ( lp_phi ) ) ) ;

        s   := 0.5 * ( q_ - 1/q_ ) ;
        ul  := 2 * ( s * Q_singam - vl * Q_cosgam ) / ( q_ + 1/q_ ) ;
        con := Cos( Q_bl * lp_lam ) ;

        if Abs( con ) >= EPS10 then begin
          us := Q_al * ArcTan( ( s * Q_cosgam + vl * Q_singam ) / con ) / Q_bl ;
          if con < 0 then
            us := us + ( Pi * Q_al / Q_bl ) ;
        end
        else
          us := Q_al * Q_bl * lp_lam ;
      end ;

      if Abs( Abs( ul ) - 1 ) <= EPS7 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      vs := 0.5 * Q_al * Ln( (1 - ul) / (1 + ul) ) / Q_bl ;
      us := us - Q_u_0 ;

      xy_x := vs * Q_cosrot + us * Q_sinrot ;
      xy_y := us * Q_cosrot - vs * Q_sinrot ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorTwoPoint.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    q_, s, ul, us, vl, vs : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      vs := xy_x * Q_cosrot - xy_y * Q_sinrot;
      us := xy_y * Q_cosrot + xy_x * Q_sinrot;

      us := us + Q_u_0 ;

      q_ := Exp( - Q_bl * vs / Q_al ) ;
      s  := 0.5 * ( q_ - 1/q_ ) ;
      vl := Sin( Q_bl * us / Q_al ) ;
      ul := 2 * ( vl * Q_cosgam + s * Q_singam ) / ( q_ + 1/q_ ) ;
      if Abs( Abs(ul) - 1 ) < EPS7 then begin
        lp_lam := 0 ;
        if ul < 0 then lp_phi := -HALFPI
                  else lp_phi :=  HALFPI ;
      end
      else begin
        lp_phi := Q_el / Sqrt( (1 + ul) / (1 - ul) ) ;
        if P_es <> 0 then begin
          lp_phi := utPhi2( Power( lp_phi, 1 / Q_bl ), P_e ) ;
          if lp_phi > GIS_MAX_SINGLE  then begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end
        end
        else
          lp_phi := HALFPI - 2 * ArcTan( lp_phi ) ;
        lp_lam := -ArcTan2( s * Q_cosgam -  vl * Q_singam,
                            Cos( Q_bl * us / Q_al )
                          ) / Q_bl ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueMercator
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHotineObliqueMercator.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.XYPlaneRotation,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;
    Q_prj          := nil ;

    FParameters.LongitudeOfCenter  := 0.0              ;
    FParameters.LatitudeOfCenter   := DegToRad( 45.0 ) ;
    FParameters.Azimuth            := DegToRad( 60.0 ) ;
    FParameters.XYPlaneRotation    := DegToRad( 60.0 ) ;

    datumChanged ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercator.doDestroy ;
  begin
    FreeObject( Q_prj ) ;

    inherited ;
  end ;

  function TGIS_CSProjHotineObliqueMercator.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHotineObliqueMercator.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercator.datumChanged ;
  begin
    FreeObject( Q_prj ) ;

    Q_prj := TGIS_CSProjHotineObliqueMercatorTwoPoint.Create( 0, '' ) ;
    TGIS_CSProjHotineObliqueMercatorTwoPoint( Q_prj ).Q_azi := True ;

    Q_prj.FParameters.LongitudeOfCenter  := FParameters.LongitudeOfCenter  ;
    Q_prj.FParameters.LatitudeOfCenter   := FParameters.LatitudeOfCenter   ;
    Q_prj.FParameters.FalseEasting       := FParameters.FalseEasting       ;
    Q_prj.FParameters.FalseNorthing      := FParameters.FalseNorthing      ;
    Q_prj.FParameters.Azimuth            := FParameters.Azimuth            ;
    Q_prj.FParameters.XYPlaneRotation    := FParameters.XYPlaneRotation    ;
    Q_prj.FParameters.ScaleFactor        := FParameters.ScaleFactor        ;

    Q_prj.Datum := Datum ;

    FValidityExtentWGS := Q_prj.ValidityExtentWGS ;
    FValidityOriginWGS := Q_prj.ValidityOriginWGS ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercator.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    Q_prj.Project3D_Ref( _coords ) ;
    FError := Q_prj.Error ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercator.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    Q_prj.Unproject3D_Ref( _coords ) ;
    FError := Q_prj.Error ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueMercatorAzimuthNaturalOrigin
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.XYPlaneRotation,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;
    Q_prj          := nil ;

    FParameters.LongitudeOfCenter  := 0.0              ;
    FParameters.LatitudeOfCenter   := DegToRad( 45.0 ) ;
    FParameters.Azimuth            := DegToRad( 60.0 ) ;
    FParameters.XYPlaneRotation    := DegToRad( 60.0 ) ;

    datumChanged ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.doDestroy ;
  begin
    FreeObject( Q_prj ) ;

    inherited ;
  end ;

  function TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.datumChanged ;
  begin
    {$IFDEF WEBASSEMBLY}
      {$WARNING '### WEBASSEMBLY compiler bug - review'}
      // compiler bug - AV crash
    {$ELSE}
      FreeObject( Q_prj ) ;
    {$ENDIF}

    if Abs( FParameters.Azimuth - HALFPI ) < EPS7 then
      Q_prj := TGIS_CSProjSwissObliqueMercator.Create( 0, '' )
    else begin
      Q_prj := TGIS_CSProjHotineObliqueMercatorTwoPoint.Create( 0, '' ) ;
      TGIS_CSProjHotineObliqueMercatorTwoPoint( Q_prj ).Q_azi := True ;
    end ;

    Q_prj.FParameters.LongitudeOfCenter  := FParameters.LongitudeOfCenter  ;
    Q_prj.FParameters.LatitudeOfCenter   := FParameters.LatitudeOfCenter   ;
    Q_prj.FParameters.FalseEasting       := FParameters.FalseEasting       ;
    Q_prj.FParameters.FalseNorthing      := FParameters.FalseNorthing      ;
    Q_prj.FParameters.Azimuth            := FParameters.Azimuth            ;
    Q_prj.FParameters.XYPlaneRotation    := FParameters.XYPlaneRotation    ;
    Q_prj.FParameters.ScaleFactor        := FParameters.ScaleFactor        ;

    Q_prj.Datum := Datum ;

    if ( Q_prj is TGIS_CSProjHotineObliqueMercatorTwoPoint             ) and
       ( self is  TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin )
    then
      TGIS_CSProjHotineObliqueMercatorTwoPoint( Q_prj ).Q_u_0 := 0 ;

    FValidityExtentWGS := Q_prj.ValidityExtentWGS ;
    FValidityOriginWGS := Q_prj.ValidityOriginWGS ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    Q_prj.Project3D_Ref( _coords ) ;
    FError := Q_prj.Error ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    Q_prj.Unproject3D_Ref( _coords ) ;
    FError := Q_prj.Error ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueMercatorAzimuthCenter
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHotineObliqueMercatorAzimuthCenter.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.LongitudeOfCenter  := 0.0              ;
    FParameters.LatitudeOfCenter   := DegToRad( 45.0 ) ;
    FParameters.Azimuth            := DegToRad( 90.0 ) ;

    datumChanged ;
  end ;

  procedure TGIS_CSProjHotineObliqueMercatorAzimuthCenter.datumChanged ;
  begin
    if FParameters.Azimuth < 0 then FParameters.Azimuth := DegToRad( -90.0 )
                               else FParameters.Azimuth := DegToRad(  90.0 ) ;

    inherited datumChanged ;

    FValidityExtentWGS := Q_prj.ValidityExtentWGS ;
    FValidityOriginWGS := Q_prj.ValidityOriginWGS ;
  end ;

  function TGIS_CSProjHotineObliqueMercatorAzimuthCenter.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHotineObliqueMercatorAzimuthCenter.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEuler
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEuler.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 30.0 ) ;
    FParameters.StandardParallel_2 := DegToRad( 60.0 ) ;

    Q_mode := EULER ;

    datumChanged ;
  end ;

  function TGIS_CSProjEuler.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEuler.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEuler.datumChanged ;
  var
    del, cs : Double ;
    p1, p2  : Double ;
    lat     : Double ;
  begin
    inherited datumChanged ;

    p1 := FParameters.StandardParallel_1 ;
    p2 := FParameters.StandardParallel_2 ;

    del   := 0.5 * ( p2 - p1 ) ;
    Q_sig := 0.5 * ( p2 + p1 ) ;

    if ( Abs( del ) < EPS7 ) or ( Abs( Q_sig ) < EPS7 ) then begin
      FError  := ERROR_WRONGPARAMS ;
      exit ;
    end ;

    case Q_mode of
      TISSOT : begin
                 Q_n     := Sin( Q_sig ) ;
                 cs      := Cos( del   ) ;
                 Q_rho_c := Q_n/cs + cs/Q_n;
                 Q_rho_0 := Sqrt( ( Q_rho_c - 2 * Sin( P_phi0 ) ) / Q_n ) ;
               end ;
      MURD1  : begin
                 Q_rho_c := Sin( del ) / ( del * Tan( Q_sig ) ) + Q_sig ;
                 Q_rho_0 := Q_rho_c - P_phi0 ;
                 Q_n     := Sin( Q_sig ) ;
               end ;
      MURD2  : begin
                 cs      := Sqrt( Cos( del ) ) ;
                 Q_rho_c := cs / Tan( Q_sig ) ;
                 Q_rho_0 := Q_rho_c + Tan( Q_sig - P_phi0 ) ;
                 Q_n     := Sin( Q_sig ) * cs ;
               end ;
      MURD3  : begin
                 Q_rho_c := del / ( Tan( Q_sig ) * Tan( del ) ) + Q_sig ;
                 Q_rho_0 := Q_rho_c - P_phi0 ;
                 Q_n     := Sin( Q_sig ) * Sin( del ) *
                            Tan( del ) / ( del * del ) ;
               end ;
      EULER  : begin
                 Q_n     := Sin( Q_sig ) * Sin( del ) / del ;
                 del     := del * 0.5 ;
                 Q_rho_c := del / ( Tan( del ) * Tan( Q_sig ) ) + Q_sig ;
                 Q_rho_0 := Q_rho_c - P_phi0 ;
               end ;
      PCONIC : begin
                 Q_n     := Sin( Q_sig ) ;
                 Q_c2    := Cos( del   ) ;
                 Q_c1    := 1 / Tan( Q_sig ) ;
                 del     := P_phi0 - Q_sig ;
                 if ( Abs( del ) - EPS10 ) >= HALFPI then begin
                   FError := ERROR_WRONGPARAMS ;
                   bDatumReady := False ;
                   exit ;
                 end ;
                 Q_rho_0 := Q_c2 * ( Q_c1 - Tan( del ) ) ;
               end ;
      VITK1  : begin
                 cs      := Tan( del ) ;
                 Q_n     := cs * Sin( Q_sig ) / del ;
                 Q_rho_c := del / ( cs * Tan( Q_sig ) ) + Q_sig ;
                 Q_rho_0 := Q_rho_c - P_phi0 ;
               end ;
      else     begin
                 assert( False ) ;
               end ;

    end ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
  end ;

  procedure TGIS_CSProjEuler.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    rho    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      case Q_mode of
        MURD2  : rho := Q_rho_c + Tan( Q_sig - lp_phi ) ;
        PCONIC : begin
                   if ( ( lp_phi <= Q_sig - HALFPI ) and ( Q_sig > 0 ) ) or
                      ( ( lp_phi >= Q_sig + HALFPI ) and ( Q_sig < 0 ) )
                   then begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;
                   rho := Q_c2 * (Cot( Q_sig ) - Tan( lp_phi - Q_sig ) ) ;
                 end ;
        else     rho := Q_rho_c - lp_phi ;
      end ;
      lp_lam := lp_lam * Q_n ;
      xy_x := rho * Sin( lp_lam ) ;
      xy_y := Q_rho_0 - rho * Cos( lp_lam ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEuler.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    rho    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := Q_rho_0 - xy_y ;
      rho  := Hypot( xy_x, xy_y ) ;
      if Q_n < 0 then begin
        rho  := - rho  ;
        xy_x := - xy_x ;
        xy_y := - xy_y ;
      end ;
      lp_lam := ArcTan2( xy_x, xy_y ) / Q_n ;
      case Q_mode of
        PCONIC : lp_phi := ArcTan( Q_c1 - rho / Q_c2 ) + Q_sig ;
        MURD2  : lp_phi := Q_sig - ArcTan( rho - Q_rho_c ) ;
        else     lp_phi := Q_rho_c - rho ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTissot
//------------------------------------------------------------------------------

  constructor TGIS_CSProjTissot.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    Q_mode := TISSOT ;

    datumChanged ;
  end ;

  function TGIS_CSProjTissot.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTissot.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTissot.datumChanged ;
  var
    lat : Double ;
  begin
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_1 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMurdoch_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMurdoch_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    Q_mode := MURD1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjMurdoch_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMurdoch_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMurdoch_I.datumChanged ;
  var
    lat : Double ;
  begin
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMurdoch_II
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMurdoch_II.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    Q_mode := MURD2 ;

    datumChanged ;
  end ;

  function TGIS_CSProjMurdoch_II.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMurdoch_II.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMurdoch_II.datumChanged ;
  var
    lat : Double ;
  begin
    // all parameters are same as in Euler
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_90 , -ZONE_35, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_90 , -ZONE_90, ZONE_35
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjMurdoch_III
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMurdoch_III.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    Q_mode := MURD3 ;

    datumChanged ;
  end ;

  function TGIS_CSProjMurdoch_III.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMurdoch_III.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMurdoch_III.datumChanged ;
  var
    lat : Double ;
  begin
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjVitkovsky_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjVitkovsky_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 60.0 ) ;
    FParameters.StandardParallel_2 := DegToRad( 30.0 ) ;

    Q_mode := VITK1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjVitkovsky_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjVitkovsky_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjVitkovsky_I.datumChanged ;
  var
    lat : Double ;
  begin
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_180, -ZONE_90, ZONE_90
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPerspectiveConic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPerspectiveConic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 29.5 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.5 ) ;

    Q_mode := PCONIC ;

    datumChanged ;
  end ;

  function TGIS_CSProjPerspectiveConic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPerspectiveConic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPerspectiveConic.datumChanged ;
  var
    lat : Double ;
  begin
    inherited datumChanged ;

    lat := ( FParameters.StandardParallel_1 + FParameters.StandardParallel_2 )
           / 2 ;

    if lat >= 0 then
      set_Validity( FParameters.CentralMeridian, ZONE_180,
                    lat                        , ZONE_70, -ZONE_90, ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian, ZONE_90,
                    lat                        , ZONE_70, -ZONE_90, ZONE_90
                  )
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_mf := 0 ;
    datumChanged ;
  end ;

  function TGIS_CSProjStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  function TGIS_CSProjStereographic.ssfn(
    _phit   : Double ;
    _sinphi : Double ;
    _eccen  : Double
  ) : Double ;
  begin
    _sinphi := _sinphi * _eccen ;
    Result := Tan( 0.5 * ( HALFPI + _phit ) ) *
              Power( (1 - _sinphi) / (1 + _sinphi), 0.5 * _eccen ) ;
  end ;

  procedure TGIS_CSProjStereographic.datumChanged ;
  var
    t        : Double ;
    x        : Double ;
    sinphits : Double ;
    cosphits : Double ;
  begin
    inherited datumChanged ;

    P_k0    := FParameters.ScaleFactor       ;
    Q_phits := FParameters.StandardParallel_1 ;

    t := Abs( P_phi0 ) ;
    if Abs( t - HALFPI ) < EPS10 then begin
      if P_phi0 < 0 then Q_mode := S_POLE
                    else Q_mode := N_POLE ;
    end
    else begin
      if t > EPS10  then Q_mode := OBLIQ
                    else Q_mode := EQUIT  ;
    end ;

    Q_phits := Abs( Q_phits ) ;
    if P_es <> 0 then begin
      case Q_mode of
        EQUIT ,
        OBLIQ  : begin
                   t := Sin( P_phi0 ) ;
                   x := 2 * ArcTan( ssfn( P_phi0, t, P_e ) ) - HALFPI ;
                   t := t * P_e ;
                   Q_akm1  := 2 * P_k0 * Cos( P_phi0 ) / Sqrt( 1 - t*t ) ;
                   SinCos( x, Q_sinX1, Q_cosX1 ) ;
                 end ;
        N_POLE,
        S_POLE : begin
                   SinCos( Q_phits, sinphits, cosphits ) ;

                   // Variant C modification
                   // not tested to north pole case
                   if Q_mf <> 0 then
                     Q_mf := cosphits / Sqrt( 1- P_e*P_e * sinphits*sinphits ) ;

                   if Abs( Q_phits - HALFPI ) < EPS10 then
                     Q_akm1 := 2 * P_k0 / Sqrt( Power( 1 + P_e, 1 + P_e ) *
                                                Power( 1 - P_e, 1 - P_e )
                                              )
                   else begin
                     t      := sinphits ;
                     Q_akm1 := cosphits / utTsfn( Q_phits, t, P_e ) ;
                     t      := t * P_e ;
                     Q_akm1 := Q_akm1 / Sqrt( 1 - t*t ) ;
                   end
                 end ;
      end
    end
    else begin
      case Q_mode of
        EQUIT  : begin
                   Q_akm1 := 2 * P_k0 ;
                 end ;
        OBLIQ  : begin
                   SinCos( P_phi0, Q_sinX1, Q_cosX1 ) ;
                   Q_akm1 := 2 * P_k0 ;
                 end ;
        S_POLE,
        N_POLE : begin
                   if Abs( Q_phits - HALFPI ) >= EPS10
                     then Q_akm1 := Cos( Q_phits ) / Tan( FORTPI - 0.5*Q_phits )
                     else Q_akm1 := 2 * P_k0 ;
                 end ;
      end ;
    end ;

    case Q_mode of
      N_POLE :
        set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                      0                           , ZONE_180, -ZONE_60 , ZONE_90
                    ) ;

      S_POLE :
         set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                       0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                     ) ;
      EQUIT,
      OBLIQ  :
        set_Validity(  FParameters.CentralMeridian , ZONE_160, -ZONE_180, ZONE_180,
                       FParameters.LatitudeOfOrigin, ZONE_90 , -ZONE_90 , ZONE_90
                    ) ;
    end ;

  end ;

  procedure TGIS_CSProjStereographic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    coslam, sinlam : Double ;
    sinx, cosx     : Double ;
    x, a           : Double ;
    sinphi, cosphi : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoidal
        SinCos( lp_lam, sinlam, coslam ) ;
        sinphi := Sin( lp_phi ) ;
        if ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT ) then begin
          x := 2 * ArcTan( ssfn( lp_phi, sinphi, P_e ) ) - HALFPI ;
          SinCos( x, sinx, cosx ) ;
        end
        else begin
          sinx := 0 ;
          cosx := 0 ;
        end ;

        case Q_mode of
          EQUIT  : begin
                     a := Q_akm1 / ( 1.0 + cosx * coslam ) ;
                     xy_y := a * sinx ;
                     xy_x := a * cosx ;
                   end ;
          OBLIQ  : begin
                     a := Q_akm1 / ( Q_cosX1 *
                                     ( 1.0 +
                                       Q_sinX1 * sinx +
                                       Q_cosX1 * cosx * coslam
                                     )
                                   );

                     xy_y := a * ( Q_cosX1 * sinx - Q_sinX1 * cosx * coslam ) ;
                     xy_x := a * cosx ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = S_POLE then begin
                       lp_phi := -lp_phi ;
                       coslam := -coslam ;
                       sinphi := -sinphi ;
                     end ;

                     xy_x := Q_akm1 * utTsfn( lp_phi, sinphi, P_e ) ;
                     xy_y := -( xy_x * coslam + Q_mf ) ;
                   end ;
          else     begin
                     // untested case
                     xy_x := 0 ;
                     xy_y := 0 ;
                 end ;
        end ;

        xy_x := xy_x * sinlam ;

      end
      else begin //spheroid

        SinCos( lp_phi, sinphi, cosphi ) ;
        SinCos( lp_lam, sinlam, coslam ) ;

        case Q_mode of
          EQUIT,
          OBLIQ  : begin
                     if Q_mode = EQUIT
                       then xy_y := 1 + cosphi * coslam
                       else xy_y := 1 + Q_sinX1 * sinphi +
                                    Q_cosX1 * cosphi * coslam ;

                     if xy_y <= EPS10 then begin
                       FError := ERROR_UNSOLVABLE ;
                       projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                       exit ;
                     end ;
                     xy_y := Q_akm1 / xy_y ;
                     xy_x := xy_y * cosphi * sinlam ;

                     if Q_mode = EQUIT
                       then xy_y := xy_y * sinphi
                       else xy_y := xy_y * ( Q_cosX1 * sinphi -
                                             Q_sinX1 * cosphi * coslam
                                           ) ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = N_POLE then begin
                       coslam := -coslam ;
                       lp_phi := -lp_phi ;
                     end ;

                     xy_y := Q_akm1 * Tan( FORTPI + 0.5 * lp_phi ) ;
                     xy_x := sinlam * xy_y ;
                     xy_y := xy_y * coslam;
                   end ;
          else     begin
                     // untested case
                     xy_x := 0 ;
                     xy_y := 0 ;
                   end ;
        end ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjStereographic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 8 ;
  var
    cosphi, sinphi : Double ;
    tp, phi_l, rho : Double ;
    halfe, halfpi_ : Double ;
    c, rh          : Double ;
    sinc, cosc     : Double ;
    i              : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoidal

        xy_y := xy_y + Q_mf ;
        rho := Hypot( xy_x, xy_y ) ;

        case Q_mode of
          EQUIT,
          OBLIQ  : begin
                     tp := 2 * ArcTan2( rho * Q_cosX1 , Q_akm1 ) ;
                     SinCos( tp, sinphi, cosphi ) ;
                     if rho = 0
                       then phi_l := ArcSin( cosphi * Q_sinX1 )
                       else phi_l := ArcSin( cosphi * Q_sinX1 +
                                             ( xy_y * sinphi * Q_cosX1 / rho )
                                           ) ;

                     tp   := Tan( 0.5 * ( HALFPI + phi_l ) ) ;
                     xy_x := xy_x * sinphi ;
                     xy_y := rho * Q_cosX1 * cosphi - xy_y * Q_sinX1 * sinphi ;
                     halfpi_ := HALFPI ;
                     halfe   := 0.5 * P_e ;
                   end ;
          N_POLE,
          S_POLE : begin
                     if Q_mode = N_POLE then
                       xy_y := -xy_y ;

                     tp := - rho / Q_akm1 ;
                     phi_l   := HALFPI - 2 * ArcTan( tp );
                     halfpi_ := -HALFPI ;
                     halfe   := -0.5 * P_e ;
                   end ;
          else     begin
                     // untested case
                     tp      := 0 ;
                     phi_l   := 0 ;
                     halfe   := 0 ;
                     halfpi_ := 0 ;
                   end ;
        end ;

        lp_phi := 0 ;

        i := N_ITER ;
        while i > 0 do begin
          dec( i ) ;
          sinphi := P_e * Sin( phi_l ) ;
          lp_phi := 2 * ArcTan( tp * Power( (1+sinphi)/(1-sinphi), halfe) ) -
                    halfpi_ ;
          if Abs( phi_l - lp_phi ) < EPS10 then
            break ;
          phi_l  := lp_phi ;
        end ;

        if i <= 0 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        if Q_mode = S_POLE then lp_phi := -lp_phi;
        if ( xy_x = 0 ) and ( xy_y = 0 ) then lp_lam := 0
                                         else lp_lam := ArcTan2( xy_x, xy_y ) ;
      end
      else begin //spheroid

        rh := Hypot( xy_x, xy_y ) ;

        if Abs( rh / Q_akm1 ) > 1 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        c  := 2 * ArcTan( rh / Q_akm1 ) ;
        SinCos( c, sinc, cosc ) ;
        lp_lam := 0 ;

        case Q_mode of
          EQUIT  : begin
                     if Abs( rh ) <= EPS10
                       then lp_phi := 0
                       else lp_phi := ArcSin( xy_y * sinc / rh ) ;

                     if ( cosc <> 0 ) or ( xy_x <> 0 ) then
                        lp_lam := ArcTan2( xy_x * sinc, cosc * rh ) ;
                   end ;
          OBLIQ  : begin
                     if Abs( rh ) <= EPS10
                       then lp_phi := P_phi0
                       else lp_phi := ArcSin( cosc * Q_sinX1 +
                                              xy_y * sinc * Q_cosX1 / rh
                                            ) ;
                     c := cosc - Q_sinX1 * Sin( lp_phi ) ;
                     if ( c <> 0 ) or ( xy_x <> 0 ) then
                       lp_lam := ArcTan2( xy_x * sinc * Q_cosX1, c * rh ) ;
                   end ;
          N_POLE,
          S_POLE:  begin
                     if Q_mode = N_POLE then xy_y := -xy_y ;
                     if Abs( rh ) <= EPS10
                       then lp_phi := P_phi0
                       else begin
                         if Q_mode = S_POLE then lp_phi := ArcSin( -cosc )
                                            else lp_phi := ArcSin(  cosc ) ;
                       end ;
                     if ( xy_x = 0 ) and ( xy_y = 0 )
                       then lp_lam := 0
                       else lp_lam := ArcTan2( xy_x, xy_y ) ;
                   end ;
          else     begin
                     // unexpected case
                     lp_phi := 0 ;
                   end;
        end ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPolarStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPolarStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype      := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin := 0 ;
    FParameters.ScaleFactor      := 1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjPolarStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPolarStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPolarStereographic.datumChanged ;
  begin
    FParameters.StandardParallel_1 := FParameters.LatitudeOfOrigin ;

    if FParameters.LatitudeOfOrigin >= 0
      then FParameters.LatitudeOfOrigin :=  HALFPI
      else FParameters.LatitudeOfOrigin := -HALFPI ;

    inherited datumChanged ;

    if FParameters.LatitudeOfOrigin > 0  then
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180, -ZONE_60 , ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                  ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPolarStereographic_B
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPolarStereographic_B.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin := - HALFPI ;
    FParameters.ScaleFactor      := 1        ;

    datumChanged ;
  end ;

  function TGIS_CSProjPolarStereographic_B.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPolarStereographic_B.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPolarStereographic_B.datumChanged ;
  begin
    if FParameters.StandardParallel_1 < 0
     then FParameters.LatitudeOfOrigin := -HALFPI
     else FParameters.LatitudeOfOrigin :=  HALFPI ;

    inherited datumChanged ;

    if FParameters.LatitudeOfOrigin > 0  then
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180, -ZONE_60 , ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                  ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPolarStereographic_C
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPolarStereographic_C.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype      := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin :=  HALFPI ;
    FParameters.ScaleFactor      := 1       ;

    datumChanged ;
  end ;

  function TGIS_CSProjPolarStereographic_C.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPolarStereographic_C.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPolarStereographic_C.datumChanged ;
  begin
    if FParameters.StandardParallel_1 < 0
     then FParameters.LatitudeOfOrigin := -HALFPI
     else FParameters.LatitudeOfOrigin :=  HALFPI ;

    Q_mf := 1 ;
    inherited datumChanged ;

    if FParameters.LatitudeOfOrigin > 0  then
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180, -ZONE_60 , ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                  ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjUniversalPolarStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjUniversalPolarStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.LatitudeOfOrigin
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '(UPS)' ;

    if FParameters.LatitudeOfOrigin < 0
      then FParameters.LatitudeOfOrigin := -HALFPI
      else FParameters.LatitudeOfOrigin :=  HALFPI ;

    FParameters.CentralMeridian    := 0       ;
    FParameters.ScaleFactor        := 0.994   ;
    FParameters.FalseEasting       := 2000000 ;
    FParameters.FalseNorthing      := 2000000 ;
    FParameters.StandardParallel_1 := HALFPI  ;

    datumChanged ;
  end ;

  function TGIS_CSProjUniversalPolarStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjUniversalPolarStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjUniversalPolarStereographic.datumChanged ;
  begin
    if FParameters.LatitudeOfOrigin < 0
     then FParameters.LatitudeOfOrigin := -HALFPI
     else FParameters.LatitudeOfOrigin :=  HALFPI ;

    inherited datumChanged ;

    if FParameters.LatitudeOfOrigin > 0  then
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180, -ZONE_60 , ZONE_90
                  )
    else
      set_Validity( FParameters.CentralMeridian , ZONE_180, -ZONE_180, ZONE_180,
                    0                           , ZONE_180 ,-ZONE_90 , ZONE_60
                  ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueStereographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjObliqueStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '(Rousilhe)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjObliqueStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjObliqueStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  function TGIS_CSProjObliqueStereographic.srat(
     _a   : Double ;
     _exp : Double
  ) : Double ;
  begin
    Result := Power( (1 - _a) / (1 + _a), _exp ) ;
  end ;

  procedure TGIS_CSProjObliqueStereographic.gaussini(
        _e    : Double ;
        _phi0 : Double ;
    var _chi  : Double ;
    var _rc   : Double
  ) ;
  var
    sphi, cphi, es : Double ;
  begin
    es          := _e * _e;
    Q_en_e      := _e;
    SinCos( _phi0, sphi, cphi ) ;
    cphi        := cphi * cphi ;
    _rc         := Sqrt( 1 - es ) / ( 1 - es * sphi * sphi ) ;
    Q_en_C      := Sqrt( 1 + es * cphi * cphi / (1 - es) ) ;
    _chi        := ArcSin( sphi / Q_en_C );
    Q_en_ratexp := 0.5 * Q_en_C * _e ;
    Q_en_K      := Tan( 0.5 * _chi + FORTPI ) /
                   ( Power( Tan( 0.5 * _phi0 + FORTPI ), Q_en_C ) *
                     srat( Q_en_e * sphi, Q_en_ratexp)
                   ) ;
  end ;

  procedure TGIS_CSProjObliqueStereographic.gauss(
    out _lam  : Double ;
    out _phi  : Double
  ) ;
  begin
    _phi := 2 * ArcTan( Q_en_K *
                        Power( Tan(0.5 * _phi + FORTPI), Q_en_C ) *
                        srat( Q_en_e * Sin( _phi ), Q_en_ratexp )
                      ) - HALFPI ;
    _lam := Q_en_C * _lam ;
  end ;

  procedure TGIS_CSProjObliqueStereographic.datumChanged ;
  var
    r : Double ;
  begin
    inherited datumChanged ;

    P_k0 := FParameters.ScaleFactor ;

    gaussini( P_e, P_phi0, Q_phic0, r ) ;
    SinCos( Q_phic0, Q_sinc0, Q_cosc0 ) ;
    Q_r2    := 2 * r         ;

    set_Validity( FParameters.CentralMeridian , ZONE_90, -ZONE_180, ZONE_180,
                  0                       , ZONE_90, -ZONE_180, ZONE_180
                ) ;
  end ;

  procedure TGIS_CSProjObliqueStereographic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    cosc   : Double ;
    sinc   : Double ;
    cosl   : Double ;
    k      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      gauss( lp_lam, lp_phi);
      SinCos( lp_phi, sinc, cosc ) ;
      cosl := Cos( lp_lam ) ;
      k := P_k0 * Q_r2 / (1 + Q_sinc0 * sinc + Q_cosc0 * cosc * cosl ) ;
      xy_x := k * cosc * Sin( lp_lam ) ;
      xy_y := k * ( Q_cosc0 * sinc - Q_sinc0 * cosc * cosl ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjObliqueStereographic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
  var
    rho    : Double ;
    c      : Double ;
    sinc   : Double ;
    cosc   : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;

    function srat(esinp : Double; exp : Double) : Double ;
    begin
       Result := Power( (1-esinp)/(1+esinp), exp ) ;
    end ;

    procedure pj_inv_gauss( out lp_lam, lp_phi : Double);
    var
      elp_lam : Double ;
      elp_phi : Double ;
      tmp_phi : Double ;
      i : Integer ;
      num : Double ;
    const
      N_ITER2 = 20 ;
    begin
      elp_lam := lp_lam / Q_en_C;
      num := Power( Tan( 0.5 * lp_phi + FORTPI ) / Q_en_K, 1/Q_en_C);
      tmp_phi := lp_phi ;

      i := N_ITER2 ;
      elp_phi := 0 ;
      while i > 0 do begin
        elp_phi := 2 * ArcTan(num * srat(Q_en_e * Sin( tmp_phi ), -0.5 * Q_en_e))
                - HALFPI;
        if Abs( elp_phi - tmp_phi ) < 1e-14 then break;
        tmp_phi := elp_phi;

        dec( i ) ;
      end ;
      if i = 0 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

     lp_lam := elp_lam ;
     lp_phi := elp_phi ;
    end ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_x := xy_x / P_k0 ;
      xy_y := xy_y / P_k0 ;
      rho := Hypot( xy_x, xy_y ) ;
      if rho <> 0 then begin
        c := 2 * ArcTan2( rho, Q_r2 ) ;
        SinCos( c, sinc, cosc ) ;
        lp_phi := ArcSin( cosc * Q_sinc0  +  xy_y * sinc * Q_cosc0 / rho);
        lp_lam := ArcTan2( xy_x * sinc,
                           rho * Q_cosc0 * cosc  -  xy_y * Q_sinc0 * sinc
                         ) ;
      end
      else begin
        lp_phi := Q_phic0 ;
        lp_lam := 0      ;
      end ;

      pj_inv_gauss(lp_lam, lp_phi);

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjOrthographic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjOrthographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Azimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjOrthographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjOrthographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjOrthographic.datumChanged ;
  begin
    inherited datumChanged ;

    if Abs( Abs( P_phi0 ) - HALFPI ) <= EPS10 then begin
      if P_phi0 < 0 then Q_mode := S_POLE
                    else Q_mode := N_POLE ;
    end
    else if Abs( P_phi0 ) > EPS10 then begin
      Q_mode   := OBLIQ ;
      SinCos( P_phi0, Q_sinph0, Q_cosph0 ) ;
    end
    else begin
      Q_mode := EQUIT ;
    end ;

    set_Validity( FParameters.CentralMeridian , ZONE_90,
                  FParameters.LatitudeOfOrigin, ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjOrthographic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    coslam,
    cosphi,
    sinphi : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;
      cosphi := Cos( lp_phi ) ;
      coslam := Cos( lp_lam ) ;

      case Q_mode of
        EQUIT  : begin
                   if cosphi * coslam < -EPS10 then begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;
                   xy_y := Sin( lp_phi ) ;
                 end ;
        OBLIQ  : begin
                   sinphi := Sin( lp_phi ) ;
                   if ( Q_sinph0 * sinphi +
                        Q_cosph0 * cosphi * coslam ) < -EPS10 then
                   begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;
                   xy_y := Q_cosph0 * sinphi - Q_sinph0 * cosphi * coslam ;
                 end ;
        N_POLE,
        S_POLE : begin
                   if Q_mode = N_POLE then coslam := -coslam ;

                   if Abs( lp_phi - P_phi0 ) - EPS10 > HALFPI then begin
                     FError := ERROR_UNSOLVABLE ;
                     projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                     exit ;
                   end ;
                   xy_y := cosphi * coslam ;
                 end ;
        else     begin
                   // untested case
                   xy_y := 0 ;
                 end ;
      end ;
      xy_x := cosphi * Sin( lp_lam ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjOrthographic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    rh,
    cosc,
    sinc   : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      rh   := Hypot( xy_x, xy_y ) ;
      sinc := rh ;
      if sinc > 1 then begin
        if (sinc - 1) > EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;
        sinc := 1 ;
      end ;

      cosc := Sqrt( 1 - sinc * sinc ) ; // in this range OK
      if Abs( rh ) <= EPS10 then begin
        lp_phi := P_phi0 ;
        lp_lam := 0      ;
      end
      else begin
        case Q_mode of
          N_POLE : begin
                     xy_y   := -xy_y ;
                     lp_phi := ArcCos( sinc ) ;
                   end ;
          S_POLE : begin
                     lp_phi := - ArcCos( sinc ) ;
                   end ;
          EQUIT,
          OBLIQ  : begin
                     if Q_mode = EQUIT then begin
                       lp_phi := xy_y * sinc / rh ;
                       xy_x   := xy_x * sinc ;
                       xy_y   := cosc * rh   ;
                     end
                     else begin
                       lp_phi := cosc * Q_sinph0 + xy_y * sinc * Q_cosph0 /rh ;
                       xy_y   := ( cosc - Q_sinph0 * lp_phi ) * rh ;
                       xy_x   := xy_x * ( sinc * Q_cosph0 ) ;
                     end ;
                     if Abs( lp_phi ) >= 1 then begin
                       if lp_phi < 0
                         then lp_phi := -HALFPI
                         else lp_phi :=  HALFPI ;
                     end
                     else
                       lp_phi := ArcSin( lp_phi ) ;
                   end ;
          else     begin
                     // untested case
                     lp_phi := 0 ;
                   end ;
        end ;

        if ( xy_y = 0) and
           ( ( Q_mode = OBLIQ ) or ( Q_mode = EQUIT ) ) then
        begin
          if      xy_x = 0 then lp_lam := 0
          else if xy_x < 0 then lp_lam := -HALFPI
                           else lp_lam :=  HALFPI ;
        end
        else
          lp_lam := ArcTan2( xy_x, xy_y ) ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPolyconicAmerican
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPolyconicAmerican.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjPolyconicAmerican.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPolyconicAmerican.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPolyconicAmerican.datumChanged ;
  begin
    inherited datumChanged ;

    if P_es <> 0 then begin
      Q_en  := utEnfn( P_es ) ;
      Q_ml0 := utMlfn( P_phi0, Sin( P_phi0), Cos( P_phi0 ), Q_en ) ;
    end
    else begin
      Q_ml0 := -P_phi0 ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_60,
                  0                          , ZONE_90
                )
  end ;

  procedure TGIS_CSProjPolyconicAmerican.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    ms, sp, cp : Double ;
    cot, E     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoidal

        if Abs( lp_phi ) <= EPS10 then begin
          xy_x := lp_lam ;
          xy_y := -Q_ml0 ;
        end
        else begin
          SinCos( lp_phi, sp, cp ) ;

          if Abs( cp ) > EPS10 then ms := utMsfn( sp, cp, P_es ) / sp
                               else ms := 0 ;

          lp_lam := lp_lam *sp ;
          xy_x   := ms * Sin( lp_lam ) ;
          xy_y   := ( utMlfn( lp_phi, sp, cp, Q_en ) - Q_ml0 ) +
                    ms * ( 1 - Cos( lp_lam ) ) ;
        end ;

      end
      else begin //spheroid

        if Abs( lp_phi ) <= EPS10 then begin
          xy_x := lp_lam ;
          xy_y := Q_ml0  ;
        end
        else begin
          cot := 1 / Tan( lp_phi ) ;
          E   := lp_lam * Sin( lp_phi ) ;
          xy_x := Sin( E ) * cot ;
          xy_y := lp_phi - P_phi0 + cot * ( 1 - Cos(E) );
        end ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPolyconicAmerican.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    I_ITER = 20 ;
    N_ITER = 10 ;
  var
    r, c         : Double ;
    sp, cp       : Double ;
    s2ph         : Double ;
    ml, mlb, mlp : Double ;
    dphi         : Double ;
    B, tp        : Double ;
    i            : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoidal

        xy_y := xy_y + Q_ml0 ;
        if Abs( xy_y ) <= EPS10 then begin
          lp_lam := xy_x ;
          lp_phi := 0    ;
        end
        else begin
          r := xy_y * xy_y + xy_x * xy_x ;

          i := I_ITER ;
          lp_phi := xy_y ;
          while i > 0 do begin
            dec( i ) ;
            SinCos( lp_phi, sp, cp ) ;
            s2ph := sp * cp ;
            if Abs( cp ) < EPS12 then begin
              FError := ERROR_UNSOLVABLE ;
              unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
              exit ;
            end ;
            mlp  := Sqrt( 1 - P_es * sp * sp ) ;
            c    := sp * mlp / cp ;
            ml   := utMlfn( lp_phi, sp, cp, Q_en ) ;
            mlb  := ml * ml + r ;
            mlp  := P_one_es / ( mlp * mlp * mlp ) ;
            dphi :=( ml + ml + c * mlb - 2 * xy_y * ( c * ml + 1 ) ) /
                   ( P_es * s2ph * ( mlb - 2 * xy_y * ml ) / c +
                     2 * ( xy_y - ml ) * ( c * mlp - 1/s2ph ) - mlp - mlp
                   ) ;
            lp_phi := lp_phi + dphi ;
            if Abs( dphi ) <= EPS12 then
              break;
          end ;
          if i = 0 then begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;

          c := Sin( lp_phi ) ;
          lp_lam := ArcSin( xy_x * Tan( lp_phi ) * Sqrt( 1 - P_es * c * c ) ) /
                    Sin( lp_phi ) ;
        end ;

      end
      else begin //spheroid

        xy_y := P_phi0 + xy_y ;
        if Abs( xy_y ) <= EPS10 then begin
          lp_lam := xy_x ;
          lp_phi := 0    ;
        end
        else begin
          lp_phi := xy_y ;
          B := xy_x * xy_x + xy_y * xy_y ;
          i := N_ITER;

          while i > 0 do begin
            dec( i ) ;

            tp   := Tan( lp_phi ) ;
            dphi := ( xy_y * ( lp_phi * tp + 1 ) - lp_phi -
                      0.5 * ( lp_phi * lp_phi + B ) * tp ) /
                    ( ( lp_phi - xy_y ) / tp - 1 ) ;
            lp_phi := lp_phi - dphi ;
            if Abs( dphi ) < EPS10 then
              break ;
          end ;
          if i = 0 then begin
            FError := ERROR_UNSOLVABLE ;
            unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end ;
          lp_lam := ArcSin( xy_x * Tan( lp_phi ) ) / Sin( lp_phi ) ;
        end ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjRobinson
//------------------------------------------------------------------------------

  constructor TGIS_CSProjRobinson.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    {$IFDEF JAVA}
      InitializeDoubleArray(Q_X,4);
      InitializeDoubleArray(Q_Y,4);
    {$ENDIF}
    Q_X[ 0,0] :=  1           ;
    Q_X[ 0,1] := -5.67239e-12 ;
    Q_X[ 0,2] := -7.15511e-05 ;
    Q_X[ 0,3] :=  3.11028e-06 ;

    Q_X[ 1,0] :=  0.9986      ;
    Q_X[ 1,1] := -0.000482241 ;
    Q_X[ 1,2] := -2.48970e-05 ;
    Q_X[ 1,3] :=- 1.33094e-06 ;

    Q_X[ 2,0] :=  0.9954      ;
    Q_X[ 2,1] := -0.000831031 ;
    Q_X[ 2,2] := -4.48610e-05 ;
    Q_X[ 2,3] :=- 9.86588e-07 ;

    Q_X[ 3,0] :=  0.99        ;
    Q_X[ 3,1] := -0.00135363  ;
    Q_X[ 3,2] := -5.96598e-05 ;
    Q_X[ 3,3] :=  3.67749e-06 ;

    Q_X[ 4,0] :=  0.9822      ;
    Q_X[ 4,1] := -0.00167442  ;
    Q_X[ 4,2] := -4.49750e-06 ;
    Q_X[ 4,3] :=- 5.72394e-06 ;

    Q_X[ 5,0] :=  0.973       ;
    Q_X[ 5,1] := -0.00214869  ;
    Q_X[ 5,2] := -9.03565e-05 ;
    Q_X[ 5,3] :=  1.88767e-08 ;

    Q_X[ 6,0] :=  0.96        ;
    Q_X[ 6,1] := -0.00305084  ;
    Q_X[ 6,2] := -9.00732e-05 ;
    Q_X[ 6,3] :=  1.64869e-06 ;

    Q_X[ 7,0] :=  0.9427      ;
    Q_X[ 7,1] := -0.00382792  ;
    Q_X[ 7,2] := -6.53428e-05 ;
    Q_X[ 7,3] :=- 2.61493e-06 ;

    Q_X[ 8,0] :=  0.9216      ;
    Q_X[ 8,1] := -0.00467747  ;
    Q_X[ 8,2] := -0.000104566 ;
    Q_X[ 8,3] :=  4.81220e-06 ;

    Q_X[ 9,0] :=  0.8962      ;
    Q_X[ 9,1] := -0.00536222  ;
    Q_X[ 9,2] := -3.23834e-05 ;
    Q_X[ 9,3] :=- 5.43445e-06 ;

    Q_X[10,0] :=  0.8679      ;
    Q_X[10,1] := -0.00609364  ;
    Q_X[10,2] := -0.0001139   ;
    Q_X[10,3] :=  3.32521e-06 ;

    Q_X[11,0] :=  0.835       ;
    Q_X[11,1] := -0.00698325  ;
    Q_X[11,2] := -6.40219e-05 ;
    Q_X[11,3] :=  9.34582e-07 ;

    Q_X[12,0] :=  0.7986      ;
    Q_X[12,1] := -0.00755337  ;
    Q_X[12,2] := -5.00038e-05 ;
    Q_X[12,3] :=  9.35532e-07 ;

    Q_X[13,0] :=  0.7597      ;
    Q_X[13,1] := -0.00798325  ;
    Q_X[13,2] := -3.59716e-05 ;
    Q_X[13,3] :=- 2.27604e-06 ;

    Q_X[14,0] :=  0.7186      ;
    Q_X[14,1] := -0.00851366  ;
    Q_X[14,2] := -7.01120e-05 ;
    Q_X[14,3] :=- 8.63072e-06 ;

    Q_X[15,0] :=  0.6732      ;
    Q_X[15,1] := -0.00986209  ;
    Q_X[15,2] := -0.000199572 ;
    Q_X[15,3] :=  1.91978e-05 ;

    Q_X[16,0] :=  0.6213      ;
    Q_X[16,1] := -0.010418    ;
    Q_X[16,2] :=  8.839480e-05;
    Q_X[16,3] :=  6.24031e-06 ;

    Q_X[17,0] :=  0.5722      ;
    Q_X[17,1] := -0.00906601  ;
    Q_X[17,2] :=  0.000181999 ;
    Q_X[17,3] :=  6.24033e-06 ;

    Q_X[18,0] :=  0.5322      ;
    Q_X[18,1] :=   0          ;
    Q_X[18,2] :=  0           ;
    Q_X[18,3] :=  0           ;

    Q_Y[ 0,0] :=  0           ;
    Q_Y[ 0,1] :=  0.0124      ;
    Q_Y[ 0,2] :=  3.72529e-10 ;
    Q_Y[ 0,3] :=  1.15484e-09 ;

    Q_Y[ 1,0] :=  0.062       ;
    Q_Y[ 1,1] :=  0.0124001   ;
    Q_Y[ 1,2] :=  1.76951e-08 ;
    Q_Y[ 1,3] :=- 5.92321e-09 ;

    Q_Y[ 2,0] :=  0.124       ;
    Q_Y[ 2,1] :=  0.0123998   ;
    Q_Y[ 2,2] := -7.09668e-08 ;
    Q_Y[ 2,3] :=  2.25753e-08 ;

    Q_Y[ 3,0] :=  0.186       ;
    Q_Y[ 3,1] :=  0.0124008   ;
    Q_Y[ 3,2] :=  2.66917e-07 ;
    Q_Y[ 3,3] :=- 8.44523e-08 ;

    Q_Y[ 4,0] :=  0.248       ;
    Q_Y[ 4,1] :=  0.0123971   ;
    Q_Y[ 4,2] := -9.99682e-07 ;
    Q_Y[ 4,3] :=  3.15569e-07 ;

    Q_Y[ 5,0] :=  0.31        ;
    Q_Y[ 5,1] :=  0.0124108   ;
    Q_Y[ 5,2] :=  3.73349e-06 ;
    Q_Y[ 5,3] :=- 1.17790e-06 ;

    Q_Y[ 6,0] :=  0.372       ;
    Q_Y[ 6,1] :=  0.0123598   ;
    Q_Y[ 6,2] := -1.39350e-05 ;
    Q_Y[ 6,3] :=  4.39588e-06 ;

    Q_Y[ 7,0] :=  0.434       ;
    Q_Y[ 7,1] :=  0.0125501   ;
    Q_Y[ 7,2] :=  5.20034e-05 ;
    Q_Y[ 7,3] :=- 1.00051e-05 ;

    Q_Y[ 8,0] :=  0.4968      ;
    Q_Y[ 8,1] :=  0.0123198   ;
    Q_Y[ 8,2] := -9.80735e-05 ;
    Q_Y[ 8,3] :=  9.22397e-06 ;

    Q_Y[ 9,0] :=  0.5571      ;
    Q_Y[ 9,1] :=  0.0120308   ;
    Q_Y[ 9,2] :=  4.02857e-05 ;
    Q_Y[ 9,3] :=- 5.29010e-06 ;

    Q_Y[10,0] :=  0.6176      ;
    Q_Y[10,1] :=  0.0120369   ;
    Q_Y[10,2] := -3.90662e-05 ;
    Q_Y[10,3] :=  7.36117e-07 ;

    Q_Y[11,0] :=  0.6769      ;
    Q_Y[11,1] :=  0.0117015   ;
    Q_Y[11,2] := -2.80246e-05 ;
    Q_Y[11,3] :=- 8.54283e-07 ;

    Q_Y[12,0] :=  0.7346      ;
    Q_Y[12,1] :=  0.0113572   ;
    Q_Y[12,2] := -4.08389e-05 ;
    Q_Y[12,3] :=- 5.18524e-07 ;

    Q_Y[13,0] :=  0.7903      ;
    Q_Y[13,1] :=  0.0109099   ;
    Q_Y[13,2] := -4.86169e-05 ;
    Q_Y[13,3] :=- 1.07180e-06 ;

    Q_Y[14,0] :=  0.8435      ;
    Q_Y[14,1] :=  0.0103433   ;
    Q_Y[14,2] := -6.46934e-05 ;
    Q_Y[14,3] :=  5.36384e-09 ;

    Q_Y[15,0] :=  0.8936      ;
    Q_Y[15,1] :=  0.00969679  ;
    Q_Y[15,2] := -6.46129e-05 ;
    Q_Y[15,3] :=- 8.54894e-06 ;

    Q_Y[16,0] :=  0.9394      ;
    Q_Y[16,1] :=  0.00840949  ;
    Q_Y[16,2] := -0.000192847 ;
    Q_Y[16,3] :=- 4.21023e-06 ;

    Q_Y[17,0] :=  0.9761      ;
    Q_Y[17,1] :=  0.00616525  ;
    Q_Y[17,2] := -0.000256001 ;
    Q_Y[17,3] :=- 4.21021e-06 ;

    Q_Y[18,0] :=  1           ;
    Q_Y[18,1] :=  0           ;
    Q_Y[18,2] :=  0           ;
    Q_Y[18,3] :=  0           ;

    datumChanged ;
  end ;

  function TGIS_CSProjRobinson.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjRobinson.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjRobinson.doProj(
    const _forward : Boolean ;
    var   _lp_lam  : Double  ;
    var   _lp_phi  : Double  ;
    var   _xy_x    : Double  ;
    var   _xy_y    : Double
  ) ;
  const
    NODES = 18 ;
    C1    = 11.45915590261646417544 ;
    RC1   =  0.08726646259971647884 ;
    FXC   =  0.8487 ;
    FYC   =  1.3523 ;

    function V(
      const _c0 : Double ;
      const _c1 : Double ;
      const _c2 : Double ;
      const _c3 : Double ;
      const _z  : Double
    ) : Double ;
    begin
      Result := _c0 + _z * ( _c1 + _z * ( _c2 + _z * _c3 ) ) ;
    end ;

    function DV(
      const _c0 : Double ;
      const _c1 : Double ;
      const _c2 : Double ;
      const _c3 : Double ;
      const _z  : Double
    ) : Double ;
    begin
      Result := _c1 + _z * ( _c2 + _c2 + _z * 3.0 * _c3 ) ;
    end ;

  var
    i     : Integer ;
    dphi  : Double  ;
    t, t1 : Double  ;
    tmp   : array[0..3] of Double  ;
  begin

    if _forward then begin // do project

      dphi := Abs( _lp_phi ) ;
      i := FloorS( dphi * C1 ) ;

      if i >= NODES then i := NODES - 1;

      dphi := RadToDeg( dphi - RC1 * i ) ;
      _xy_x := V( Q_X[i,0], Q_X[i,1], Q_X[i,2], Q_X[i,3], dphi ) * FXC * _lp_lam ;
      _xy_y := V( Q_Y[i,0], Q_Y[i,1], Q_Y[i,2], Q_Y[i,3], dphi ) * FYC ;
      if _lp_phi < 0 then _xy_y := -_xy_y ;

    end
    else begin // do unproject

      _lp_lam := _xy_x / FXC ;
      _lp_phi := Abs( _xy_y / FYC ) ;
      if _lp_phi >= 1 then begin // simple pathologic cases
        _lp_phi := 1 ;
        if _xy_y < 0 then _lp_phi := -HALFPI
                     else _lp_phi :=  HALFPI ;
        _lp_lam := _lp_lam / Q_X[ NODES, 0];
      end
      else begin // general problem
        // in Y space, reduce to table interval
        i := FloorS( _lp_phi * NODES ) ;
        while True do begin
          if      Q_Y[i  ,0] >  _lp_phi then dec( i )
          else if Q_Y[i+1,0] <= _lp_phi then inc( i )
          else break ;
        end ;
        tmp[0] := Q_Y[ i, 0 ] ;
        tmp[1] := Q_Y[ i, 1 ] ;
        tmp[2] := Q_Y[ i, 2 ] ;
        tmp[3] := Q_Y[ i, 3 ] ;
        // first guess, linear interp
        t := 5 * ( _lp_phi - tmp[0] ) / ( Q_Y[ i+1, 0 ] - tmp[0] ) ;
        // make into root
        tmp[0] := tmp[0] - _lp_phi ;
        while True do begin // Newton-Raphson reduction
          t1 := V ( tmp[0], tmp[1], tmp[2], tmp[3], t ) /
                DV( tmp[0], tmp[1], tmp[2], tmp[3], t ) ;
          t  := t - t1 ;
          if Abs( t1 ) < EPS7 then
            break ;
        end ;
        _lp_phi := DegToRad( 5.0 * i + t );
        if _xy_y < 0 then _lp_phi := -_lp_phi ;
        _lp_lam := _lp_lam / V( Q_X[i,0], Q_X[i,1], Q_X[i,2], Q_X[i,3], t ) ;
      end ;

      if      _lp_lam < -Pi then _lp_lam := -Pi
      else if _lp_lam >  Pi then _lp_lam :=  Pi ;
    end ;
  end ;

  procedure TGIS_CSProjRobinson.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjRobinson.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      doProj( True, lp_lam, lp_phi, xy_x, xy_y ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjRobinson.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      doProj( False, lp_lam, lp_phi, xy_x, xy_y ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjSwissObliqueMercator
//------------------------------------------------------------------------------

  constructor TGIS_CSProjSwissObliqueMercator.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := False ;
    FDescription   := '' ;

    FParameters.LongitudeOfCenter := DegToRad( 7.4395375    ) ; //  7°26'22.335E"
    FParameters.LatitudeOfCenter  := DegToRad( 46.952191667 ) ; // 46°57'07.890N"
    FParameters.FalseEasting      := 600000 ;
    FParameters.FalseNorthing     := 200000 ;
    FParameters.ScaleFactor       := 1      ;

    datumChanged ;
  end ;

  function TGIS_CSProjSwissObliqueMercator.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjSwissObliqueMercator.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjSwissObliqueMercator.datumChanged ;
  var
    cp, phip0, sp : Double ;
  begin
    inherited datumChanged ;

    P_lam0 := FParameters.LongitudeOfCenter ;
    P_phi0 := FParameters.LatitudeOfCenter  ;
    P_k0   := FParameters.ScaleFactor       ;

    Q_hlf_e := 0.5 * P_e     ;
    cp      := Cos( P_phi0 ) ;
    cp      := cp * cp       ;
    Q_c     := Sqrt( 1 + P_es * cp * cp * P_rone_es ) ;
    sp      := Sin( P_phi0 ) ;
    Q_sinp0 := sp / Q_c      ;
    phip0   := utAasin( Q_sinp0 ) ;
    Q_cosp0 := Cos( phip0 )  ;
    sp      := sp * P_e      ;
    Q_K     := Ln( Tan( FORTPI + 0.5 * phip0  ) ) -
               Q_c * ( Ln( Tan( FORTPI + 0.5 * P_phi0 ) ) -
                       Q_hlf_e * Ln( (1 + sp) / (1 - sp) )
                     ) ;
    Q_kR    := P_k0 * Sqrt( P_one_es ) / ( 1 - sp * sp ) ;

    set_Validity( FParameters.LongitudeOfCenter, ZONE_45,
                  FParameters.LatitudeOfCenter , ZONE_45, -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjSwissObliqueMercator.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    phip, lamp,
    phipp, lampp,
    sp, cp        : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      sp    := P_e * Sin( lp_phi ) ;
      phip  := 2* ArcTan( Exp( Q_c *
                               ( Ln( Tan( FORTPI + 0.5 * lp_phi ) ) -
                                 Q_hlf_e * Ln( (1 + sp) / (1 - sp) )
                               ) + Q_K
                             )
                        ) - HALFPI ;
      lamp  := Q_c * lp_lam ;
      cp    := Cos( phip ) ;
      phipp := utAasin( Q_cosp0 * Sin( phip ) - Q_sinp0 * cp * Cos( lamp ) ) ;
      lampp := utAasin( cp * Sin( lamp ) / Cos( phipp ) ) ;
      xy_x  := Q_kR * lampp ;
      xy_y  := Q_kR * Ln( Tan( FORTPI + 0.5 * phipp ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjSwissObliqueMercator.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 6 ;
  var
    phip, lamp,
    phipp, lampp,
    cp, esp,
    con, delp    : Double ;
    i            : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      phipp := 2 * ( ArcTan( Exp( xy_y / Q_kR ) ) - FORTPI ) ;
      lampp := xy_x / Q_kR ;
      cp    := Cos( phipp ) ;
      phip  := utAasin( Q_cosp0 * Sin( phipp ) + Q_sinp0 * cp * Cos( lampp ) ) ;
      lamp  := utAasin( cp * Sin( lampp ) / Cos( phip ) ) ;
      con   := ( Q_K - Ln( Tan( FORTPI + 0.5 * phip ) ) ) / Q_c ;

      i := N_ITER ;
      while i > 0 do begin
        dec( i ) ;

        esp  := P_e * Sin( phip ) ;
        delp := ( con + Ln( Tan( FORTPI + 0.5 * phip ) ) -
                  Q_hlf_e * Ln( (1 + esp) / (1 - esp) )
                ) *
                ( 1 - esp * esp ) * Cos( phip ) * P_rone_es ;
        phip := phip - delp ;
        if Abs( delp ) < EPS10 then
          break;
      end ;
      if i = 0 then begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      lp_phi := phip ;
      lp_lam := lamp / Q_c ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTransverseMercator
//------------------------------------------------------------------------------

  constructor TGIS_CSProjTransverseMercatorOld.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalTransverse ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjTransverseMercatorOld.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTransverseMercatorOld.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTransverseMercatorOld.datumChanged ;
  begin
    inherited datumChanged ;

    P_k0 := FParameters.ScaleFactor ;

    Q_en := utEnfn( P_es ) ;
    if P_es <> 0 then begin
      Q_ml0 := utMlfn( P_phi0, Sin( P_phi0 ), Cos( P_phi0 ), Q_en ) ;
      Q_esp := P_es / ( 1 - P_es ) ;
    end
    else begin
      Q_esp := P_k0 ;
      Q_ml0 := 0.5 * Q_esp ;
    end ;

    set_Validity( FParameters.CentralMeridian, ZONE_80, -ZONE_180, ZONE_180,
                  0                          , ZONE_90, -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjTransverseMercatorOld.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FC1 = 1                      ;
    FC2 = 0.5                    ;
    FC3 = 0.16666666666666666666 ;
    FC4 = 0.08333333333333333333 ;
    FC5 = 0.05                   ;
    FC6 = 0.03333333333333333333 ;
    FC7 = 0.02380952380952380952 ;
    FC8 = 0.01785714285714285714 ;
  var
    al, als, n, b,
    cosphi, sinphi, t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if P_es <> 0 then begin // ellipsoidal

        SinCos( lp_phi, sinphi, cosphi ) ;

        if Abs( cosphi ) > EPS10 then t := sinphi/cosphi
                                 else t := 0 ;
        t   := t * t ;
        al  := cosphi * lp_lam ;
        als := al * al ;
        al  := al / Sqrt( 1 - P_es * sinphi * sinphi ) ;
        n   := Q_esp * cosphi * cosphi ;
        xy_x := P_k0 * al *
                ( FC1 + FC3 * als *
                  ( 1 - t + n + FC5 * als *
                    ( 5 + t * ( t - 18 ) + n * (14 - 58 * t) +
                      FC7 * als * ( 61 + t * ( t * ( 179 - t ) - 479 ) )
                    )
                  )
                );

        xy_y := P_k0 *
                ( utMlfn( lp_phi, sinphi, cosphi, Q_en ) - Q_ml0 +
                  sinphi * al * lp_lam * FC2 *
                  ( 1 + FC4 * als *
                    ( 5 - t + n * ( 9 + 4.0 * n ) +
                      FC6 * als * ( 61 + t * ( t - 58 ) + n * ( 270 - 330 * t ) +
                                    FC8 * als * ( 1385 +
                                                  t * ( t * ( 543 - t ) - 3111 )
                                                )
                                  )
                    )
                  )
                ) ;

      end
      else begin //spheroid
        cosphi := Cos( lp_phi ) ;

        b := cosphi * Sin( lp_lam ) ;
        if Abs( Abs( b ) - 1 ) <= EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        xy_x := Q_ml0 * Ln( (1+b) / (1-b) ) ;
        xy_y := cosphi * Cos( lp_lam ) / Sqrt( 1 - b * b ) ;
        b    := Abs( xy_y ) ;
        if b >= 1 then begin
          if ( b - 1 ) > EPS10 then begin
            FError := ERROR_UNSOLVABLE ;
            projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end
          else
            xy_y := 0 ;
        end
        else
          xy_y := ArcCos( xy_y ) ;

        if lp_phi < 0 then xy_y := -xy_y ;

        xy_y := Q_esp * ( xy_y - P_phi0 ) ;

      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjTransverseMercatorOld.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FC1 = 1                      ;
    FC2 = 0.5                    ;
    FC3 = 0.16666666666666666666 ;
    FC4 = 0.08333333333333333333 ;
    FC5 = 0.05                   ;
    FC6 = 0.03333333333333333333 ;
    FC7 = 0.02380952380952380952 ;
    FC8 = 0.01785714285714285714 ;
  var
    h, g           : Double ;
    n, con         : Double ;
    cosphi, sinphi : Double ;
    d, ds, t       : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if P_es <> 0 then begin // ellipsoidal

        lp_phi := utInv_mlfn( Q_ml0 + xy_y / P_k0, P_es, Q_en ) ;
        if lp_phi > GIS_MAX_SINGLE then begin
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end;

        if Abs( lp_phi ) >= HALFPI then begin
          if xy_y < 0 then lp_phi := -HALFPI
                      else lp_phi :=  HALFPI ;
          lp_lam := 0 ;
        end
        else begin
          SinCos( lp_phi, sinphi, cosphi ) ;

          if Abs( cosphi ) > EPS10 then t := sinphi/cosphi
                                   else t := 0 ;
          n   := Q_esp * cosphi * cosphi ;
          con := 1 - P_es * sinphi * sinphi ;
          d   := xy_x * Sqrt( con ) / P_k0 ;
          con := con * t ;
          t   := t * t ;
          ds  := d * d;
          lp_phi := lp_phi -
                    ( ( con * ds / ( 1-P_es ) ) * FC2 *
                      ( 1 - ds * FC4 *
                        ( 5 + t * ( 3 - 9 *  n ) +
                          n * ( 1 - 4 * n ) -
                          ds * FC6 *
                          ( 61 + t * ( 90 - 252 * n + 45 * t ) +
                            46 * n - ds * FC8 *
                            ( 1385 + t * ( 3633 + t * ( 4095 + 1574 * t ) ) )
                          )
                        )
                      )
                    ) ;
          lp_lam := d *
                    ( FC1 - ds * FC3 *
                      ( 1 + 2 * t + n -
                        ds * FC5 *
                        ( 5 +
                          t * ( 28 + 24 * t + 8 * n ) + 6 * n -
                          ds * FC7 *
                          ( 61 + t * ( 662 + t * ( 1320 + 720 * t ) ) )
                        )
                      )
                    ) / cosphi ;
        end ;

      end
      else begin //spheroid

        h := Exp( xy_x / Q_esp ) ;
        g := 0.5 * ( h - 1 / h ) ;
        h := Cos( P_phi0 + xy_y / Q_esp ) ;
        lp_phi := ArcSin( Sqrt( ( 1 - h * h ) / ( 1 + g * g ) ) ) ;
        if xy_y < 0 then lp_phi := -lp_phi ;
        if ( g <> 0 ) or ( h <> 0 ) then lp_lam := ArcTan2( g, h )
                                    else lp_lam := 0 ;

      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTransverseMercatorEx
//------------------------------------------------------------------------------

  constructor TGIS_CSProjTransverseMercator.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalTransverse ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  const PROJ_ETMERC_ORDER = 6 ;

  function TGIS_CSProjTransverseMercator.gatg(
    const _ar   : TGIS_DoubleArray ;
    const _b    : Double
  ) : Double ;
  var
    h, h1, h2 : Double ;
    cos_2b    : Double ;
    idx       : Integer ;
  begin
    h  := 0 ;
    h1 := 0 ;
    h2 := 0 ;

    cos_2b := 2 * Cos( 2*_b );


    h1 := _ar[ PROJ_ETMERC_ORDER-1 ] ;
    for idx := PROJ_ETMERC_ORDER-2 downto 0 do begin
      h := -h2 + cos_2b*h1 + _ar[ idx ];
      h2 := h1 ;
      h1 := h ;
    end ;

    Result := _b + h * Sin( 2*_b );
  end ;

  function TGIS_CSProjTransverseMercator.clens(
    const _ar    : TGIS_DoubleArray ;
    const _arg_r : Double           ;
    const _arg_i : Double           ;
    var   _i     : Double
  ) : Double ;
  var
    idx : Integer ;
    r, i, hr, hr1, hr2, hi, hi1, hi2 : Double ;
    sin_arg_r, cos_arg_r, sinh_arg_i, cosh_arg_i : Double;
  begin
    SinCos(_arg_r, sin_arg_r, cos_arg_r);
    sinh_arg_i := Sinh(_arg_i);
    cosh_arg_i := Cosh(_arg_i);
    r          :=  2 * cos_arg_r*cosh_arg_i;
    i          := -2 * sin_arg_r*sinh_arg_i;

    hi1 := 0 ;
    hr1 := 0 ;
    hi := 0 ;
    hr := _ar[ PROJ_ETMERC_ORDER-1 ] ;
    for idx := PROJ_ETMERC_ORDER -2 downto 0 do begin
        hr2 := hr1;
        hi2 := hi1;
        hr1 := hr;
        hi1 := hi;
        hr  := -hr2 + r*hr1 - i*hi1 + _ar[ idx ];
        hi  := -hi2 + i*hr1 + r*hi1;
    end ;

    r   := sin_arg_r*cosh_arg_i;
    i   := cos_arg_r*sinh_arg_i;
    Result  := r*hr - i*hi;
    _i  := r*hi + i*hr;
  end;

  function TGIS_CSProjTransverseMercator.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTransverseMercator.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTransverseMercator.datumChanged ;
  var
    f, n, np, Z : Double ;
    tmp : Double ;
  begin
    inherited datumChanged ;

    P_k0 := FParameters.ScaleFactor ;

    f  := P_es / ( 1 + Sqrt( 1 -  P_es) );
    n  := f/(2 - f);
    np := n ;

    SetLength( Q_cgb, 6 ) ;
    SetLength( Q_cbg, 6 ) ;
    SetLength( Q_utg, 6 ) ;
    SetLength( Q_gtu, 6 ) ;

    Q_cgb[0] := n*( 2 + n*(-2/3.0  + n*(-2      + n*(116/45.0 + n*(26/45.0 +
                n*(-2854/675.0 ))))));
    Q_cbg[0] := n*(-2 + n*( 2/3.0  + n*( 4/3.0  + n*(-82/45.0 + n*(32/45.0 +
                n*( 4642/4725.0))))));

    np       := np * n;
    Q_cgb[1] := np*(7/3.0 + n*( -8/5.0  + n*(-227/45.0 + n*(2704/315.0 +
                n*( 2323/945.0)))));
    Q_cbg[1] := np*(5/3.0 + n*(-16/15.0 + n*( -13/9.0  + n*( 904/315.0 +
                n*(-1522/945.0)))));
    np       := np * n;
    Q_cgb[2] := np*( 56/15.0  + n*(-136/35.0 + n*(-1262/105.0 +
                n*( 73814/2835.0))));
    Q_cbg[2] := np*(-26/15.0  + n*(  34/21.0 + n*(    8/5.0   +
                n*(-12686/2835.0))));

    np       := np * n;
    Q_cgb[3] := np*(4279/630.0 + n*(-332/35.0 + n*(-399572/14175.0)));
    Q_cbg[3] := np*(1237/630.0 + n*( -12/5.0  + n*( -24832/14175.0)));

    np       := np * n;
    Q_cgb[4] := np*(4174/315.0 + n*(-144838/6237.0 ));
    Q_cbg[4] := np*(-734/315.0 + n*( 109598/31185.0));

    np       := np * n;
    Q_cgb[5] := np*(601676/22275.0 );
    Q_cbg[5] := np*(444337/155925.0);

    np := n*n ;
    Q_n := P_k0/(1 + n) * (1 + np*(1/4.0 + np*(1/64.0 + np/256.0)));

    Q_utg[0] := n*(-0.5  + n*( 2/3.0 + n*(-37/96.0 + n*( 1/360.0 +
                n*(  81/512.0 + n*(-96199/604800.0))))));
    Q_gtu[0] := n*( 0.5  + n*(-2/3.0 + n*(  5/16.0 + n*(41/180.0 +
                n*(-127/288.0 + n*(  7891/37800.0 ))))));
    Q_utg[1] := np*(-1/48.0 + n*(-1/15.0 + n*(437/1440.0 + n*(-46/105.0 +
                n*( 1118711/3870720.0)))));
    Q_gtu[1] := np*(13/48.0 + n*(-3/5.0  + n*(557/1440.0 + n*(281/630.0 +
                n*(-1983433/1935360.0)))));

    np       := np * n;
    Q_utg[2] := np*(-17/480.0 + n*(  37/840.0 + n*(  209/4480.0  +
                n*( -5569/90720.0 ))));
    Q_gtu[2] := np*( 61/240.0 + n*(-103/140.0 + n*(15061/26880.0 +
                n*(167603/181440.0))));

    np       := np * n;
    Q_utg[3] := np*(-4397/161280.0 + n*(  11/504.0 + n*( 830251/7257600.0)));
    Q_gtu[3] := np*(49561/161280.0 + n*(-179/168.0 + n*(6601661/7257600.0)));

    np       := np * n;
    Q_utg[4] := np*(-4583/161280.0 + n*(  108847/3991680.0));
    Q_gtu[4] := np*(34729/80640.0  + n*(-3418889/1995840.0));

    np       := np * n;
    Q_utg[5] := np*(-20648693/638668800.0);
    Q_gtu[5] := np*(212378941/319334400.0);

    Z := gatg( Q_cbg, P_phi0 );

    Q_b  := - Q_n * ( Z + clens( Q_gtu, 2*Z, 0, tmp ) );

    set_Validity( FParameters.CentralMeridian, ZONE_80, -ZONE_180, ZONE_180,
                  0                          , ZONE_90, -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjTransverseMercator.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
    n, e : Double ;
    sin_n, cos_n, cos_e, sin_e, d_e : Double ;
    cos_n_e : Double ;

  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

    n := gatg( Q_cbg, lp_phi );
    e := lp_lam ;

    SinCos(n, sin_n, cos_n);
    SinCos(e, sin_e, cos_e);


    cos_n_e := cos_n*cos_e ;

    n := ArcTan2( sin_n, cos_n_e ) ;
    e := ArcTan2( sin_e*cos_n, Hypot( sin_n, cos_n_e ) ) ;


    e := ArcSinh( Tan( e ) );

    n := n + clens( Q_gtu, 2*n, 2*e, d_e ) ;
    e := e + d_e ;
    if Abs( e ) <= 2.623395162778 then begin
      xy_y  := Q_n * n + Q_b ;
      xy_x  := Q_n * e ;
    end
    else begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjTransverseMercator.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
    e, n : Double ;
    sin_n, cos_n, cos_e, sin_e, d_e : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

    n := ( xy_y - Q_b ) / Q_n ;
    e := xy_x / Q_n ;

    if Abs(e) <= 2.623395162778 then begin
      n := n + clens( Q_utg, 2*n, 2*e, d_e ) ;
      e := e + d_e ;
      e := ArcTan( Sinh( e ) ) ;
      SinCos( n, sin_n, cos_n ) ;
      SinCos( e, sin_e, cos_e ) ;
      e := ArcTan2( sin_e, cos_e*cos_n ) ;
      n := ArcTan2( sin_n*cos_e, Hypot( sin_e, cos_e*cos_n ) ) ;
      lp_phi := gatg( Q_cgb, n ) ;
      lp_lam := e ;
    end
    else begin
      unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;


    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTransverseMercatorSouthOriented
//------------------------------------------------------------------------------

  function TGIS_CSProjTransverseMercatorSouthOriented.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTransverseMercatorSouthOriented.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTransverseMercatorSouthOriented.datumChanged ;
  begin
    FParameters.FalseEasting   := - FParameters.FalseEasting     ;
    FParameters.FalseNorthing  := - FParameters.FalseNorthing    ;

    inherited datumChanged ;

    FParameters.FalseEasting   := - FParameters.FalseEasting     ;
    FParameters.FalseNorthing  := - FParameters.FalseNorthing    ;
  end ;

  procedure TGIS_CSProjTransverseMercatorSouthOriented.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    inherited ;
    _coords.X := -_coords.X ;
    _coords.Y := -_coords.Y ;
  end ;

  procedure TGIS_CSProjTransverseMercatorSouthOriented.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    _coords.X := -_coords.X ;
    _coords.Y := -_coords.Y ;
    inherited ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjUniversalTransverseMercator
//------------------------------------------------------------------------------

  constructor TGIS_CSProjUniversalTransverseMercator.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalTransverse ;
    FParametersSet := [
                        TGIS_CSProjParameter.Zone
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := True ;
    FDescription   := '(UTM)' ;

    {$IFDEF TEST_BOUNDINGS}
      FParameters.Zone := 32 ;
    {$ENDIF}

    datumChanged ;
  end ;

  function TGIS_CSProjUniversalTransverseMercator.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjUniversalTransverseMercator.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjUniversalTransverseMercator.datumChanged ;
  var
    izone : Integer ;
  begin
    if Abs( FParameters.Zone ) > 60 then begin
      if FParameters.Zone > 0 then FParameters.Zone :=  60
                  else FParameters.Zone := -60 ;
    end
    else if FParameters.Zone = 0 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;
    izone := Abs( FParameters.Zone ) - 1 ;

    if FParameters.Zone < 0 then FParameters.FalseNorthing := 10000000   // south
                            else FParameters.FalseNorthing := 0        ; // north
    FParameters.FalseEasting := 500000 ;

    FParameters.CentralMeridian  := ( izone + 0.5 ) * Pi / 30 - Pi ;
    FParameters.LatitudeOfOrigin := 0      ;
    FParameters.ScaleFactor      := 0.9996 ;

    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_80, -ZONE_180, ZONE_180,
                  0                          , ZONE_90, -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjUniversalTransverseMercator.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    if FParameters.Zone = 0 then begin
      FParameters.Zone := FloorS( ( utAdjlon( _coords.X ) + Pi ) * 30 / Pi ) + 1 ;
      if      FParameters.Zone <  1 then FParameters.Zone :=  1
      else if FParameters.Zone > 60 then FParameters.Zone := 60 ;

      if _coords.Y < 0 then FParameters.Zone := -FParameters.Zone ;

      datumChanged ;
    end ;

    inherited Project3D_Ref( _coords ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjEquidistantTwoPoint
//------------------------------------------------------------------------------

  constructor TGIS_CSProjEquidistantTwoPoint.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfPoint_1,
                        TGIS_CSProjParameter.LatitudeOfPoint_1,
                        TGIS_CSProjParameter.LongitudeOfPoint_2,
                        TGIS_CSProjParameter.LatitudeOfPoint_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin   := DegToRad(  45.0 ) ;
    FParameters.LongitudeOfPoint_1 := DegToRad( 180.0 ) ;
    FParameters.LatitudeOfPoint_1  := DegToRad(  80.0 ) ;
    FParameters.LongitudeOfPoint_2 := DegToRad(   0.0 ) ;
    FParameters.LatitudeOfPoint_2  := DegToRad(  60.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjEquidistantTwoPoint.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjEquidistantTwoPoint.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjEquidistantTwoPoint.datumChanged ;
  var
    lam_1, lam_2,
    phi_1, phi_2,
    A12, pp       : Double ;

  begin
    inherited datumChanged ;

    lam_1 := FParameters.LongitudeOfPoint_1 ;
    phi_1 := FParameters.LatitudeOfPoint_1  ;
    lam_2 := FParameters.LongitudeOfPoint_2 ;
    phi_2 := FParameters.LatitudeOfPoint_2  ;

    if ( phi_1 = phi_2 ) and ( lam_1 = lam_2 ) then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    P_lam0   := utAdjlon( 0.5 * ( lam_1 + lam_2 ) ) ;
    Q_dlam2  := utAdjlon( lam_2 - lam_1 ) ;
    SinCos( phi_1, Q_sp1, Q_cp1 ) ;
    SinCos( phi_2, Q_sp2, Q_cp2 ) ;
    Q_cs     := Q_cp1 * Q_sp2 ;
    Q_sc     := Q_sp1 * Q_cp2 ;
    Q_ccs    := Q_cp1 * Q_cp2 * Sin( Q_dlam2 ) ;
    Q_z02    := utAacos( Q_sp1 * Q_sp2 + Q_cp1 * Q_cp2 * Cos( Q_dlam2 ) ) ;
    if Q_z02 > GIS_MAX_SINGLE then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;
    Q_hz0    := 0.5 * Q_z02 ;
    A12      := ArcTan2( Q_cp2 * Sin( Q_dlam2 ),
                         Q_cp1 * Q_sp2 - Q_sp1 * Q_cp2 * Cos( Q_dlam2 )
                       ) ;
    pp       := utAasin( Q_cp1 * Sin( A12 ) )  ;
    SinCos( pp, Q_sa, Q_ca ) ;
    Q_lp     := utAdjlon( ArcTan2( Q_cp1 * Cos( A12 ), Q_sp1 ) - Q_hz0 ) ;
    Q_dlam2  := 0.5 * Q_dlam2  ;
    Q_lamc   := HALFPI - ArcTan2( Sin( A12 ) * Q_sp1, Cos( A12 ) ) - Q_dlam2 ;
    Q_thz0   := Tan( Q_hz0 ) ;
    Q_rhshz0 := 0.5 / Sin( Q_hz0 ) ;
    Q_r2z0   := 0.5 / Q_z02 ;
    Q_z02    := Q_z02 * Q_z02 ;

    // equivalent to set_Validity
    FValidityOriginWGS.X    := P_lam0 ;
    FValidityOriginWGS.Y    := (phi_1+phi_2)/2 ;
    FValidityExtentWGS.XMin := -ZONE_180 ;
    FValidityExtentWGS.XMax :=  ZONE_180 ;

    if FValidityOriginWGS.Y >= 0 then begin
      FValidityExtentWGS.YMin :=  0       ;
      FValidityExtentWGS.YMax :=  ZONE_90 ;
    end
    else begin
      FValidityExtentWGS.YMin := -ZONE_90 ;
      FValidityExtentWGS.YMax :=  0       ;
    end;
  end ;

  procedure TGIS_CSProjEquidistantTwoPoint.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t, z1, z2,
    dl1, dl2,
    sp, cp    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_phi, sp, cp ) ;

      dl1  := lp_lam + Q_dlam2 ;
      dl2  := lp_lam - Q_dlam2 ;
      z1   := utAacos( Q_sp1 * sp + Q_cp1 * cp * Cos( dl1 ) ) ;
      if z1 > GIS_MAX_SINGLE then begin
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end;
      z2   := utAacos( Q_sp2 * sp + Q_cp2 * cp * Cos( dl2 ) ) ;
      if z2 > GIS_MAX_SINGLE then begin
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end;
      z1   := z1 * z1 ;
      z2   := z2 * z2 ;

      t    := z1 - z2 ;
      xy_x := Q_r2z0 * t ;
      t    := Q_z02 - t ;
      xy_y := Q_r2z0 * utAsqrt( 4 * Q_z02 * z2 - t * t ) ;
      if ( Q_ccs * sp - cp * ( Q_cs * Sin( dl1 ) - Q_sc * Sin( dl2 ) ) ) < 0
      then
        xy_y := -xy_y ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjEquidistantTwoPoint.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    cz1, cz2,
    s, d, cp, sp : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      cz1    := Cos( Hypot( xy_y, xy_x + Q_hz0 ) ) ;
      cz2    := Cos( Hypot( xy_y, xy_x - Q_hz0 ) ) ;
      s      := cz1 + cz2 ;
      d      := cz1 - cz2 ;
      lp_lam := - ArcTan2( d, ( s * Q_thz0 ) ) ;
      lp_phi := utAacos( Hypot( Q_thz0 * s, d ) * Q_rhshz0 ) ;
      if lp_phi > GIS_MAX_SINGLE then begin
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;
      if xy_y < 0 then lp_phi := -lp_phi ;

      // lam--phi now in system relative to P1--P2 base equator
      SinCos( lp_phi, sp, cp ) ;
      lp_lam := lp_lam- Q_lp  ;
      s      := Cos( lp_lam ) ;
      lp_phi := utAasin( Q_sa * sp + Q_ca * cp * s );
      lp_lam := ArcTan2( cp * Sin( lp_lam ), Q_sa*cp*s - Q_ca*sp ) + Q_lamc ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjVanDerGrinten_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjVanDerGrinten_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjVanDerGrinten_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjVanDerGrinten_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_I.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian , ZONE_180,
                  0                           , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_I.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    al, al2,
    g, g2, p2 : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      p2 := Abs( lp_phi / HALFPI ) ;
      if ( p2 - EPS10 ) > 1 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      if p2 > 1 then p2 := 1 ;

      if Abs( lp_phi ) <= EPS10 then begin
        xy_x := lp_lam ;
        xy_y := 0      ;
      end
      else if ( Abs( lp_lam ) <= EPS10 ) or ( Abs( p2 - 1 ) < EPS10 ) then begin
        xy_x := 0 ;
        xy_y := Pi * Tan( 0.5 * ArcSin( p2 ) ) ;
        if lp_phi < 0 then xy_y := -xy_y ;
      end
      else begin
        al   := 0.5 * Abs( Pi / lp_lam - lp_lam / Pi ) ;
        al2  := al * al ;
        g    := Sqrt( 1 - p2 * p2 ) ;
        g    := g / ( p2 + g - 1 ) ;
        g2   := g * g ;
        p2   := g * ( 2 / p2 - 1 ) ;
        p2   := p2 * p2  ;
        xy_x := g - p2   ;
        g    := p2 + al2 ;
        xy_x := Pi * ( al * xy_x +
                       Sqrt( al2 * xy_x * xy_x - g * ( g2 - p2 ) )
                     ) / g ;
        if lp_lam < 0 then xy_x := -xy_x ;

        xy_y := Abs( xy_x / Pi ) ;
        xy_y := 1 - xy_y * ( xy_y + 2 * al ) ;
        if xy_y < -EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;

        if xy_y < 0 then
          xy_y := 0
        else begin
          if lp_phi < 0 then xy_y := Sqrt( xy_y ) * -Pi
                        else xy_y := Sqrt( xy_y ) *  Pi ;
        end
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_I.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    THIRD    =  0.33333333333333333333 ;
    TWO_THRD =  0.66666666666666666666 ;
    C2_27    =  0.07407407407407407407 ;
    PI4_3    =  4.18879020478639098458 ;
    PISQ     =  9.86960440108935861869 ;
    TPISQ    = 19.73920880217871723738 ;
    HPISQ    =  4.93480220054467930934 ;
  var
    t,
    c0, c1, c2, c3,
    al,
    r2, r, m, d,
    ay, x2, y2      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      x2 := xy_x * xy_x ;
      ay := Abs( xy_y ) ;
      if ay < EPS10 then begin
        lp_phi := 0 ;
        t      := x2 * x2 + TPISQ * ( x2 + HPISQ ) ;

        if Abs( xy_x ) <= EPS10
          then lp_lam := 0
          else lp_lam := 0.5 * ( x2 - PISQ + Sqrt( t ) ) / xy_x ;
        unprojectPost( _coords, lp_lam, lp_phi ) ;
        exit ;
      end ;

      y2 := xy_y * xy_y ;
      r  := x2 + y2 ;
      r2 := r * r ;
      c1 := -Pi * ay * ( r + PISQ ) ;
      c3 := r2 + TWOPI * ( ay * r + Pi * ( y2 + Pi * ( ay + HALFPI ) ) ) ;
      c2 := c1 + PISQ * ( r - 3 *  y2 ) ;
      c0 := Pi * ay ;
      c2 := c2 / c3 ;
      al := c1 / c3 - THIRD * c2 * c2 ;
      m  := 2 * Sqrt( -THIRD * al ) ;
      d  := C2_27 * c2 * c2 * c2 + ( c0 * c0 - THIRD * c2 * c1 ) / c3 ;
      d  := 3 * d / ( al * m ) ;
      t  := Abs( d ) ;
      if ( t - EPS10 ) <= 1 then begin
        if t > 1 then begin
          if d > 0 then d := 0
                   else d := Pi ;
        end
        else
          d := ArcCos( d ) ;

        lp_phi := Pi * ( m * Cos( d * THIRD + PI4_3 ) - THIRD * c2 ) ;
        if xy_y < 0 then lp_phi := -lp_phi ;

        t := r2 + TPISQ * ( x2 - y2 + HPISQ ) ;

        if Abs( xy_x ) < EPS10 then
          lp_lam := 0
        else begin
          if t <= 0 then lp_lam := 0.5 * ( r - PISQ             ) / xy_x
                    else lp_lam := 0.5 * ( r - PISQ + Sqrt( t ) ) / xy_x ;
        end ;
      end
      else begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjVanDerGrinten_II
//------------------------------------------------------------------------------

  constructor TGIS_CSProjVanDerGrinten_II.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjVanDerGrinten_II.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjVanDerGrinten_II.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_II.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjVanDerGrinten_II.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    TWORPI = 0.63661977236758134308 ;
  var
    x1, at,
    bt, ct : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      bt := Abs( TWORPI * lp_phi ) ;
      ct := 1 - bt * bt ;
      if ct < 0 then ct := 0
                else ct := Sqrt( ct ) ;

      if Abs( lp_lam ) < EPS10 then begin
        xy_x := 0 ;
        if lp_phi < 0 then xy_y := Pi * -bt / ( 1 + ct )
                      else xy_y := Pi *  bt / ( 1 + ct ) ;
      end
      else begin
        at := 0.5 * Abs( Pi / lp_lam - lp_lam / Pi ) ;
        if Q_mode = 1 then begin
          x1   := bt / ( 1 + ct ) ;
          xy_x := Pi * ( Sqrt( at * at + 1 - x1 * x1 ) - at ) ;
          xy_y := Pi * x1 ;
        end
        else begin
          x1    := ( ct * Sqrt( 1 + at * at ) - at * ct * ct ) /
                   ( 1 + at * at * bt * bt ) ;
          xy_x  := Pi * x1 ;
          xy_y  := Pi * Sqrt( 1 - x1 * ( x1 + 2 * at ) + EPS10 ) ;
        end ;

        if lp_lam < 0 then xy_x := -xy_x ;
        if lp_phi < 0 then xy_y := -xy_y ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjVanDerGrinten_III
//------------------------------------------------------------------------------

  constructor TGIS_CSProjVanDerGrinten_III.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_mode := 1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjVanDerGrinten_III.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjVanDerGrinten_III.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_III.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjVanDerGrinten_IV
//------------------------------------------------------------------------------

  constructor TGIS_CSProjVanDerGrinten_IV.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjVanDerGrinten_IV.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjVanDerGrinten_IV.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjVanDerGrinten_IV.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjVanDerGrinten_IV.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    TWORPI = 0.63661977236758134308 ;
  var
    x1, t,
    bt, ct, ft,
    bt2, ct2,
    dt, dt2    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Abs( lp_phi ) < EPS10 then begin
        xy_x := lp_lam ;
        xy_y := 0      ;
      end
      else if ( Abs(lp_lam) < EPS10 ) or
              ( Abs( Abs( lp_phi ) - HALFPI ) < EPS10 )
      then begin
        xy_x := 0      ;
        xy_y := lp_phi ;
      end
      else begin
        bt  := Abs( TWORPI * lp_phi ) ;
        bt2 := bt * bt ;
        ct  := 0.5 * ( bt * ( 8 - bt * ( 2 + bt2 ) ) - 5 ) /
               ( bt2 * ( bt - 1 ) ) ;
        ct2 := ct * ct ;
        dt  := TWORPI * lp_lam ;
        dt  := dt + 1 / dt ;
        dt  := Sqrt( dt * dt - 4 ) ;
        if ( Abs( lp_lam ) - HALFPI ) < 0 then dt := -dt ;
        dt2 := dt * dt ;
        x1  := bt + ct ;
        x1  := x1 * x1 ;
        t   := bt + 3 * ct ;
        ft  := x1 * ( bt2 + ct2 * dt2 - 1 ) + ( 1 - bt2 ) *
               ( bt2 * ( t * t + 4 * ct2 ) +
                 ct2 * ( 12 * bt * ct + 4 * ct2 )
               ) ;
        x1  := ( dt * ( x1 + ct2 - 1 ) + 2 * Sqrt( ft ) ) /
               ( 4 * x1 + dt2 ) ;
        xy_x := HALFPI * x1 ;
        xy_y := HALFPI * Sqrt( 1 + dt * Abs( x1 ) - x1 * x1 ) ;

        if lp_lam < 0 then xy_x := -xy_x ;
        if lp_phi < 0 then xy_y := -xy_y ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_II
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_II.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_II.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_II.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_II.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjWagner_II.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C_x  = 0.92483 ;
    C_y  = 1.38725 ;
    C_p1 = 0.88022 ;
    C_p2 = 0.88550 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := utAasin( C_p1 * Sin( C_p2 * lp_phi ) ) ;
      xy_x   := C_x * lp_lam * Cos( lp_phi ) ;
      xy_y   := C_y * lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjWagner_II.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C_x  = 0.92483 ;
    C_y  = 1.38725 ;
    C_p1 = 0.88022 ;
    C_p2 = 0.88550 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / C_y ;
      lp_lam := xy_x / ( C_x * Cos( lp_phi ) ) ;
      lp_phi := utAasin( Sin( lp_phi ) / C_p1 ) / C_p2 ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_III
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_III.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_III.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_III.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_III.datumChanged ;
  var
    ts : Double ;
  begin
    inherited datumChanged ;

    ts := FParameters.StandardParallel_1 ;

    Q_C_x  := Cos( ts ) / Cos( 2 * ts / 3 ) ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjWagner_III.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := Q_C_x * lp_lam * Cos( 2.0/3 * lp_phi ) ;
      xy_y := lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjWagner_III.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y ;
      lp_lam := xy_x / ( Q_C_x * Cos( 2.0/3 * lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_VII
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_VII.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudoazimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_VII.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_VII.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_VII.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjWagner_VII.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    theta, ct, D : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y   := 0.90630778703664996 * Sin( lp_phi ) ;
      theta  := ArcSin( xy_y  ) ;
      lp_lam := lp_lam / 3 ;
      ct     := Cos( theta ) ;
      xy_x   := 2.66723 * ct * Sin( lp_lam  );
      D      := 1 / ( Sqrt( 0.5 * ( 1 + ct * Cos( lp_lam ) ) ) ) ;
      xy_y   := xy_y * ( 1.24104 * D ) ;
      xy_x   := xy_x * D ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWinkel_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWinkel_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjWinkel_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWinkel_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWinkel_I.datumChanged ;
  begin
    inherited datumChanged ;

    Q_cosphi1 := Cos( FParameters.StandardParallel_1 ) ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjWinkel_I.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := 0.5 * lp_lam * ( Q_cosphi1 + Cos( lp_phi ) ) ;
      xy_y := lp_phi;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjWinkel_I.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y ;
      lp_lam := 2 * xy_x / ( Q_cosphi1 + Cos( lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWinkel_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWinkel_II.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjWinkel_II.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWinkel_II.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWinkel_II.datumChanged ;
  begin
    inherited datumChanged ;

    Q_cosphi1 := Cos( FParameters.StandardParallel_1 ) ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjWinkel_II.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER   = 10 ;
    TWO_D_PI = 0.636619772367581343 ;
  var
    k, V : Double  ;
    i    : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y   := lp_phi * TWO_D_PI ;
      k      := Pi * Sin( lp_phi ) ;
      lp_phi := lp_phi * 1.8 ;

      i := N_ITER ;
      while i > 0 do begin
        dec( i ) ;

        V      := ( lp_phi + Sin( lp_phi ) - k ) / ( 1 + Cos( lp_phi ) ) ;
        lp_phi := lp_phi - V ;
        if Abs( V ) < EPS7 then
          break ;
      end ;

      if i = 0 then begin
        if lp_phi < 0 then lp_phi := -HALFPI
                      else lp_phi :=  HALFPI ;
      end
      else
        lp_phi := lp_phi * 0.5 ;

      xy_x := 0.5 * lp_lam * ( Cos( lp_phi ) + Q_cosphi1 ) ;
      xy_y := FORTPI * ( Sin( lp_phi ) + xy_y ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueCylindricalEqualAreaAzimuth
//------------------------------------------------------------------------------

  constructor TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.Azimuth
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.Azimuth := DegToRad( 90.0 ) ;

    Q_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.datumChanged ;
  var
    lonz, alpha   : Double ;
    phi_1, phi_2,
    lam_1, lam_2  : Double ;
  begin
    inherited datumChanged ;

    if Q_mode = 0 then begin
      alpha    := FParameters.Azimuth           ;
      lonz     := FParameters.LongitudeOfCenter ;

      if Abs( Sin( alpha ) ) > EPS7 then begin
        Q_singam := ArcTan ( -Cos( alpha ) / ( - Sin( alpha ) ) ) +
                    lonz ;
        Q_sinphi := ArcSin( Sin( alpha ) ) ;
      end
      else begin
        Q_singam := lonz ;
        Q_sinphi := 0 ;
      end ;
    end
    else begin
      lam_1 := FParameters.LongitudeOfPoint_1 ;
      phi_1 := FParameters.LatitudeOfPoint_1  ;
      lam_2 := FParameters.LongitudeOfPoint_2 ;
      phi_2 := FParameters.LatitudeOfPoint_1  ;

      Q_singam := ArcTan2( Cos( phi_1 ) * Sin( phi_2 ) * Cos( lam_1 ) -
                           Sin( phi_1 ) * Cos( phi_2 ) * Cos( lam_2 ),
                           Sin( phi_1 ) * Cos( phi_2 ) * Sin( lam_2 ) -
                           Cos( phi_1 ) * Sin( phi_2 ) * Sin( lam_1 )
                         ) ;
      Q_sinphi := ArcTan ( -Cos( Q_singam - lam_1 ) / Tan( phi_1 ) ) ;
    end ;

    P_lam0   := Q_singam + HALFPI;
    SinCos( Q_sinphi, Q_sinphi, Q_cosphi ) ;
    SinCos( Q_singam, Q_singam, Q_cosgam ) ;

    set_Validity( P_lam0, ZONE_90,
                       0, ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_lam, xy_y, t ) ;
      xy_x := ArcTan( ( Tan( lp_phi ) * Q_cosphi + Q_sinphi * xy_y ) / t ) ;
      if t < 0 then
        xy_x := xy_x + Pi;
      xy_x := xy_x / P_k0;
      xy_y := P_k0 *
              ( Q_sinphi * Sin( lp_phi ) - Q_cosphi * Cos( lp_phi ) * xy_y );

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t, s : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := xy_y * P_k0 ;
      xy_x := xy_x / P_k0 ;
      t := Sqrt( 1 - xy_y * xy_y ) ;
      s := Sin( xy_x ) ;
      lp_phi := ArcSin( xy_y * Q_sinphi + t * Q_cosphi * s );
      lp_lam := ArcTan2( t * Q_sinphi * s - xy_y * Q_cosphi, t * Cos( xy_x ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint
//------------------------------------------------------------------------------

  constructor TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.LongitudeOfPoint_1,
                        TGIS_CSProjParameter.LatitudeOfPoint_1,
                        TGIS_CSProjParameter.LongitudeOfPoint_2,
                        TGIS_CSProjParameter.LatitudeOfPoint_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.LongitudeOfPoint_1 := DegToRad(  60.0 ) ;
    FParameters.LatitudeOfPoint_1  := DegToRad(  60.0 ) ;
    FParameters.LongitudeOfPoint_2 := DegToRad(   0.0 ) ;
    FParameters.LatitudeOfPoint_2  := DegToRad(   0.0 ) ;

    Q_mode := 1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjOblatedEqualArea
//------------------------------------------------------------------------------

  constructor TGIS_CSProjOblatedEqualArea.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.XScale,
                        TGIS_CSProjParameter.YScale,
                        TGIS_CSProjParameter.XYPlaneRotation
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian   := DegToRad( -95.0    ) ;
    FParameters.LatitudeOfOrigin  := DegToRad(  48.0    ) ;
    FParameters.XScale            := 1.33               ;
    FParameters.YScale            := 2.27               ;
    FParameters.XYPlaneRotation   := DegToRad(  13.95 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjOblatedEqualArea.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjOblatedEqualArea.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjOblatedEqualArea.datumChanged ;
  begin
    inherited datumChanged ;

    Q_m     := FParameters.XScale          ;
    Q_n     := FParameters.YScale          ;
    Q_theta := FParameters.XYPlaneRotation ;

    if ( Q_m <= 0 ) or ( Q_n <= 0 ) then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end
    else begin
      SinCos( P_phi0, Q_sp0, Q_cp0 ) ;
      Q_rn      := 1   / Q_n  ;
      Q_rm      := 1   / Q_m  ;
      Q_two_r_n := 2   * Q_rn ;
      Q_two_r_m := 2   * Q_rm ;
      Q_hm      := 0.5 * Q_m  ;
      Q_hn      := 0.5 * Q_n  ;
    end ;

    set_Validity( FParameters.CentralMeridian , ZONE_90,
                  FParameters.LatitudeOfOrigin, ZONE_45, -ZONE_90, ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjOblatedEqualArea.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    az,
    m, n,
    ut,
    cp, sp, cl,
    shz         : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;
      xy_y := 0 ;
      xy_x := 0 ;

      cp   := Cos( lp_phi ) ;
      SinCos( lp_lam, sp, cl ) ;
      az   := utAatan2( cp * Sin( lp_lam ), Q_cp0 * sp - Q_sp0 * cp * cl ) +
              Q_theta;
      ut   :=  utAacos( Q_sp0 * sp + Q_cp0 * cp * cl );
      if ut > GIS_MAX_SINGLE then begin
        projectPost( _coords, xy_x, xy_y ) ;
        exit ;
      end;

      shz  := Sin( 0.5 * ut ) ;
      m    := utAasin( shz * Sin( az ) ) ;
      n    := utAasin( shz * Cos( az ) * Cos( m ) / Cos( m * Q_two_r_m ) ) ;
      xy_y := Q_n * Sin( n * Q_two_r_n ) ;
      xy_x := Q_m * Sin( m * Q_two_r_m ) * Cos( n ) / Cos( n * Q_two_r_n ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjOblatedEqualArea.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    n, m,
    xp, yp,
    z, az,
    cz, sz,
    caz     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      n      := Q_hn * utAasin( xy_y * Q_rn ) ;
      m      := Q_hm * utAasin( xy_x * Q_rm * Cos( n * Q_two_r_n ) / Cos( n ) ) ;
      xp     := 2 * Sin( m ) ;
      yp     := 2 * Sin( n ) * Cos( m * Q_two_r_m ) / Cos( m ) ;
      az     := utAatan2( xp, yp ) - Q_theta ;
      caz    := Cos( az ) ;
      z      := 2 * utAasin( 0.5 * Hypot( xp, yp ) ) ;
      SinCos( z, sz, cz ) ;
      lp_phi := utAasin( Q_sp0 * cz + Q_cp0 * sz * caz ) ;
      lp_lam := utAatan2( sz * Sin( az ), Q_cp0 * cz - Q_sp0 * sz * caz ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjKrovak
//------------------------------------------------------------------------------

  constructor TGIS_CSProjKrovak.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.PseudoStandardParallel_1,
                        TGIS_CSProjParameter.ScaleFactor,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter,
                        TGIS_CSProjParameter.XScale,
                        TGIS_CSProjParameter.YScale,
                        TGIS_CSProjParameter.XYPlaneRotation
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian          := 0                             ;
    FParameters.LatitudeOfCenter         := 0                             ;
    FParameters.PseudoStandardParallel_1 := DegToRad( 78.5              ) ; // 78°30'0.0"N
    FParameters.ScaleFactor              := 0.9999                        ;
    FParameters.Azimuth                  := DegToRad( 30.28813975277778 ) ;
    FParameters.LongitudeOfCenter        := DegToRad( 24.83333333333333 ) ; // 24°50'0.0"E
    FParameters.LatitudeOfCenter         := DegToRad( 49.5              ) ; // 49°30'0.0"N
    FParameters.XScale                   := 1                             ;
    FParameters.YScale                   := 1                             ;
    FParameters.XYPlaneRotation          := 0                             ;

    Q_kx_def  := -1 ;
    Q_ky_def  :=  1 ;
    Q_rot_def := Pi/2 ;

    datumChanged ;
  end ;

  function TGIS_CSProjKrovak.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKrovak.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjKrovak.datumChanged ;
  begin
    inherited datumChanged ;

    if FParameters.XScale = 0 then
      FParameters.XScale := 1 ;
    if FParameters.YScale = 0 then
      FParameters.YScale := 1 ;

    Q_mod   := False ;
    Q_north := False ;

    if ( FParameters.PseudoStandardParallel_1 = 0 ) and
       ( FParameters.Azimuth                  = 0 ) then
    begin
      // some WKT generated by Proj4 does not cntain valid definition
      FParameters.PseudoStandardParallel_1 := DegToRad( 78.5 ) ; // 78°30'0.0"N
      FParameters.Azimuth                  := DegToRad( 30.28813975277778 ) ;
    end ;

    P_k0     := FParameters.ScaleFactor              ;

    Q_phi1   := FParameters.PseudoStandardParallel_1 ;
    Q_am     := FParameters.Azimuth                  ;
    Q_lonc   := FParameters.LongitudeOfCenter        ;
    Q_phic   := FParameters.LatitudeOfCenter         ;
    Q_kx     := FParameters.XScale  * Q_kx_def       ;
    Q_ky     := FParameters.YScale  * Q_ky_def       ;
    SinCos(  Q_rot_def + FParameters.XYPlaneRotation, Q_sinrot, Q_cosrot ) ;

    SinCos( Q_am, Q_sinam, Q_cosam ) ;
    Q_alph    := Sqrt( 1 + ( P_es * Power( Cos(Q_phic), 4 ) ) / (1 - P_es ) );
    SinCos( Q_phic, Q_sinphic, Q_cosphic ) ;
    Q_u0      := ArcSin( Q_sinphic / Q_alph ) ;
    Q_g       := Power( (1 + P_e * Q_sinphic ) / (1 - P_e * Q_sinphic ) ,
                        Q_alph * P_e *0.5
                      ) ;
    Q_k1      := Tan( Q_u0 / 2 + FORTPI ) /
                 Power( Tan(Q_phic / 2 + FORTPI ) , Q_alph ) * Q_g ;
    Q_n0      := Sqrt( 1 - P_es) / ( 1 - P_es * Q_sinphic * Q_sinphic ) ;
    Q_n       := Sin( Q_phi1 ) ;

    if Q_phi1 = 0  then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    Q_ro0     := P_k0 * Q_n0 / Tan( Q_phi1 ) ;

    set_Validity( DegToRad(17.0), ZONE_10,
                  DegToRad(49.0), ZONE_10
                ) ;
  end ;

  procedure TGIS_CSProjKrovak.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    gfi      : Double ;
    sinphi   : Double ;
    u        : Double ;
    v        : Double ;
    sinu     : Double ;
    cosu     : Double ;
    s, d     : Double ;
    eps      : Double ;
    ro       : Double ;
    x1, y1   : Double ;
    xp, yp   : Double ;
    xr, yr   : Double ;
    xr2, yr2 : Double ;
    dx, dy   : Double ;

    lp_lam   : Double ;
    lp_phi   : Double ;
    xy_x     : Double ;
    xy_y     : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      sinphi := Sin(lp_phi) ;
      gfi    := Power( ( ( 1 + P_e * sinphi ) / ( 1 - P_e * sinphi ) ) ,
                       ( Q_alph * P_e * 0.5 )
                     ) ;

      u := 2 * ( ArcTan( Q_k1 * Power( Tan( lp_phi / 2 + FORTPI ),
                         Q_alph
                       ) / gfi ) -
                 FORTPI
               ) ;
      SinCos( u, sinu, cosu ) ;

      v := Q_alph * ( Q_lonc - lp_lam ) ;

      s := ArcSin( Q_cosam * sinu + Q_sinam * cosu * Cos( v ) ) ;
      d := ArcSin( cosu * Sin( v ) / Cos( s ) ) ;
      eps := Q_n * d ;
      ro  := Q_ro0 * Power( Tan( Q_phi1 / 2 + FORTPI ) , Q_n ) /
                     Power( Tan( s      / 2 + FORTPI ) , Q_n )   ;

      y1 := ro * Cos( eps ) * Q_kx ;
      x1 := ro * Sin( eps ) * Q_ky ;

      if Q_mod then begin
        xp := y1*P_a ;
        yp := x1*P_a ;
        xr := xp - Q_X0 ;
        yr := yp - Q_Y0 ;

        xr2 := xr*xr ;
        yr2 := yr*yr ;

        dx  := Q_C1 +
               Q_C3 * xr -
               Q_C4 * yr -
               2*Q_C6 * xr*yr +
               Q_C5 * (xr2 - yr2) +
               Q_C7 * xr * (xr2 - 3*yr2) -
               Q_C8 * yr * (3*xr2 - yr2) +
               4*Q_C9 * xr*yr * (xr2 - yr2) +
               Q_C10 * (xr2*xr2 + yr2*yr2 - 6 * xr2*yr2) ;
        dy  := Q_C2 +
               Q_C3 * yr +
               Q_C4 * xr +
               2*Q_C5 * xr*yr +
               Q_C6 * (xr2 - yr2) +
               Q_C8 * xr * (xr2 - 3*yr2) +
               Q_C7 * yr * (3*xr2 - yr2) -
               4*Q_C10 * xr*yr * (xr2 - yr2) +
               Q_C9 * (xr2*xr2 + yr2*yr2 - 6*xr2*yr2) ;

        xy_y   := ( yp - dy ) / P_a ;
        xy_x   := ( xp - dx ) / P_a ;

      end
      else begin
        xy_y := x1 * Q_cosrot + y1 * Q_sinrot ;
        xy_x := y1 * Q_cosrot - x1 * Q_sinrot ;
      end;

    projectPost( _coords, xy_x, xy_y ) ;

    if Q_north then begin
      x1 := _coords.X ;
      y1 := _coords.Y ;
      _coords.X := -y1 ;
      _coords.Y := -x1 ;
    end ;
  end ;

  procedure TGIS_CSProjKrovak.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
  var
    xy0      : Double  ;
    u        : Double  ;
    v        : Double  ;
    sins     : Double  ;
    coss     : Double  ;
    s, d     : Double  ;
    eps      : Double  ;
    ro       : Double  ;
    dphi     : Double  ;
    ephi     : Double  ;
    dtan     : Double  ;
    i        : Integer ;
    x1, y1   : Double  ;
    xp, yp   : Double ;
    xr, yr   : Double ;
    xr2, yr2 : Double ;
    dx, dy : Double ;

    lp_lam   : Double ;
    lp_phi   : Double ;
    xy_x     : Double ;
    xy_y     : Double ;
  begin
    if Q_phi1 = 0  then begin
      FError := ERROR_WRONGPARAMS ;
      unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end ;

    if Q_mod then begin
      if Q_north then begin
        xp := -_coords.Y ;
        yp := -_coords.X ;
      end
      else begin
        xp := _coords.X ;
        yp := _coords.Y ;
      end;
      xr := ( xp - P_x0 ) - Q_X0 ;
      yr := ( yp - P_y0 ) - Q_Y0 ;

      xr2 := xr*xr ;
      yr2 := yr*yr ;

      dx  := Q_C1 +
             Q_C3 * xr -
             Q_C4 * yr -
             2*Q_C6 * xr*yr +
             Q_C5 * (xr2 - yr2) +
             Q_C7 * xr * (xr2 - 3*yr2) -
             Q_C8 * yr * (3*xr2 - yr2) +
             4*Q_C9 *xr*yr * (xr2 - yr2) +
             Q_C10 * (xr2*xr2 + yr2*yr2 - 6*xr2*yr2) ;
      dy  := Q_C2 +
             Q_C3 * yr +
             Q_C4 * xr +
             2*Q_C5 *xr*yr +
             Q_C6 * (xr2 - yr2) +
             Q_C8 * xr * (xr2 - 3*yr2) +
             Q_C7 * yr * (3*xr2 - yr2) -
             4*Q_C10 * xr*yr * (xr2 -yr2) +
             Q_C9 * (xr2*xr2 + yr2*yr2 - 6*xr2*yr2) ;

      _coords.X := xp + dx ;
      _coords.Y := yp + dy ;
    end;

    unprojectPre( _coords, xy_x, xy_y ) ;

      x1 := ( xy_x * Q_cosrot + xy_y * Q_sinrot ) * Q_kx ;
      y1 := ( xy_y * Q_cosrot - xy_x * Q_sinrot ) * Q_ky ;

      xy_x := x1 ;
      xy_y := y1 ;

      xy0  := xy_x;
      xy_x := xy_y;
      xy_y := xy0 ;

      ro   := Sqrt( xy_x * xy_x + xy_y * xy_y ) ;
      eps  := ArcTan2( xy_x, xy_y ) ;
      d    := eps / Sin( Q_phi1 ) ;

      s    := 2 * ( ArcTan(  Power( Q_ro0 / ro, 1 / Q_n ) *
                             Tan( Q_phi1 * 0.5 + FORTPI )
                          ) -
                    FORTPI
                  ) ;
      SinCos( s, sins, coss ) ;

      u    := ArcSin( Q_cosam * sins - Q_sinam * coss * Cos( d ) ) ;
      v    := ArcSin( coss * Sin( d ) / Cos( u ) ) ;

      lp_lam := Q_lonc - v / Q_alph ;

      dphi := u ;
      dtan := Power( Q_k1, - 1 / Q_alph )  *
              Power( Tan( u / 2 + FORTPI ) , 1 / Q_alph ) ;

      lp_phi := 0 ;
      i := N_ITER ;
      while i > 0 do begin
        dec( i ) ;

        ephi := P_e * Sin( dphi )  ;

        lp_phi := 2 * ( ArcTan( dtan  *
                                Power( ( 1+ephi ) / ( 1-ephi ) , 0.5 * P_e  )
                              )  -
                        FORTPI
                      ) ;

        if Abs( dphi - lp_phi ) < EPS12 then break ;
        dphi := lp_phi ;
      end ;

      if i = 0 then begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjKrovakNorthOriented
//------------------------------------------------------------------------------

  constructor TGIS_CSProjKrovakNorthOriented.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;
    Q_rot_def := 0 ;
    Q_kx_def  := 1 ;
    Q_ky_def  := 1 ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.PseudoStandardParallel_1,
                        TGIS_CSProjParameter.ScaleFactor,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian          := 0                             ;
    FParameters.LatitudeOfCenter         := 0                             ;
    FParameters.PseudoStandardParallel_1 := DegToRad( 78.5              ) ; // 78°30'0.0"N
    FParameters.ScaleFactor              := 0.9999                        ;
    FParameters.Azimuth                  := DegToRad( 30.28813975277778 ) ;
    FParameters.LongitudeOfCenter        := DegToRad( 24.83333333333333 ) ; // 24°50'0.0"E
    FParameters.LatitudeOfCenter         := DegToRad( 49.5              ) ; // 49°30'0.0"N
    FParameters.XScale                   := -1                            ;
    FParameters.YScale                   := 1                             ;
    FParameters.XYPlaneRotation          := Pi/2                          ;

    datumChanged ;
  end ;

  function TGIS_CSProjKrovakNorthOriented.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKrovakNorthOriented.Create(
                self.EPSG, self.WKT
              ) ;
  end ;


//------------------------------------------------------------------------------
// TGIS_CSProjKrovakModified
//------------------------------------------------------------------------------

  constructor TGIS_CSProjKrovakModified.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;
    Q_rot_def := 0 ;
    Q_kx_def  := 1 ;
    Q_ky_def  := 1 ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.PseudoStandardParallel_1,
                        TGIS_CSProjParameter.ScaleFactor,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.LongitudeOfCenter,
                        TGIS_CSProjParameter.LatitudeOfCenter
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.CentralMeridian          := 0                             ;
    FParameters.LatitudeOfCenter         := 0                             ;
    FParameters.PseudoStandardParallel_1 := DegToRad( 78.5              ) ; // 78°30'0.0"N
    FParameters.ScaleFactor              := 0.9999                        ;
    FParameters.Azimuth                  := DegToRad( 30.28813975277778 ) ;
    FParameters.LongitudeOfCenter        := DegToRad( 24.83333333333333 ) ; // 24°50'0.0"E
    FParameters.LatitudeOfCenter         := DegToRad( 49.5              ) ; // 49°30'0.0"N
    FParameters.FalseEasting             := 5000000 ;
    FParameters.FalseNorthing            := 5000000 ;

    FParameters.XScale                   := 1                             ;
    FParameters.YScale                   := 1                             ;
    FParameters.XYPlaneRotation          := 0                             ;

    datumChanged ;
  end ;

  function TGIS_CSProjKrovakModified.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKrovakModified.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjKrovakModified.datumChanged ;
  begin
    inherited datumChanged ;

    Q_mod := True ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjKrovakModifiedNorthOriented
//------------------------------------------------------------------------------

  function TGIS_CSProjKrovakModifiedNorthOriented.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKrovakModifiedNorthOriented.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjKrovakModifiedNorthOriented.datumChanged ;
  begin
    inherited datumChanged ;

    Q_north := True ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBaconGlobular
//------------------------------------------------------------------------------

  constructor TGIS_CSProjBaconGlobular.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_bacn := True ;
    Q_ortl := False ;
    P_es   := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjBaconGlobular.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBaconGlobular.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBaconGlobular.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjBaconGlobular.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    ax, f : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Q_bacn then xy_y := HALFPI * Sin( lp_phi )
                else xy_y := lp_phi ;

      ax := Abs(lp_lam) ;
      if ax > EPS7 then begin
        if Q_ortl and ( ax > HALFPI ) then
          xy_x := Sqrt( HALFPI2 - lp_phi*lp_phi + EPS7 ) + ax - HALFPI
        else begin
          f := 0.5 * ( HALFPI2 / ax + ax ) ;
          xy_x := ax - f + Sqrt( f*f - xy_y * xy_y ) ;
        end ;
        if lp_lam < 0 then xy_x := -xy_x ;
      end
      else
        xy_x := 0 ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjApianGlobular_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjApianGlobular_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_bacn := False ;
    Q_ortl := False ;
    P_es   := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjApianGlobular_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjApianGlobular_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjApianGlobular_I.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjOrteliusOval
//------------------------------------------------------------------------------

  constructor TGIS_CSProjOrteliusOval.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    Q_bacn := False ;
    Q_ortl := True  ;
    P_es   := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjOrteliusOval.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjOrteliusOval.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjOrteliusOval.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBipolarConic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjBipolarConic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := 'of Western Hemisphere' ;

    Q_noskew := True ;

    datumChanged ;
  end ;

  function TGIS_CSProjBipolarConic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBipolarConic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBipolarConic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian , ZONE_20,
                  FParameters.LatitudeOfOrigin, ZONE_20, -ZONE_90, ZONE_90
                );
  end ;

  procedure TGIS_CSProjBipolarConic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    LAMB = -0.34894976726250681539 ;
    AZBA =  1.82261843856185925133 ;
    AZAB =  0.81650043674686363166 ;
    CAZC =  0.69691523038678375519 ;
    SAZC =  0.71715351331143607555 ;
    R104 =  1.81514242207410275904 ;
    R110 =  1.91986217719376253360 ;
    C20  =  0.93969262078590838411 ;
    C45  =  0.70710678118654752469 ;
    S20  = -0.34202014332566873287 ;
    S45  =  0.70710678118654752410 ;
    RHOC =  1.20709121521568721927 ;
    N    =  0.63055844881274687180 ;
    F    =  1.89724742567461030582 ;
    TC   =  1.27246578267089012270 ;
  var
    cphi, sphi, tphi, t, al, az,
    z, av, cdlam, sdlam, r        : Double  ;
    tag                           : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      cphi  := Cos( lp_phi ) ;
      sphi  := Sin( lp_phi ) ;
      sdlam := LAMB  - lp_lam ;
      cdlam := Cos( sdlam ) ;
      sdlam := Sin( sdlam ) ;

      if Abs( Abs( lp_phi ) - HALFPI ) < EPS10 then begin
        if lp_phi < 0 then az := Pi
                      else az := 0 ;
        tphi := GIS_MAX_DOUBLE ;
      end
      else begin
        tphi := sphi / cphi ;
        az   := ArcTan2( sdlam , C45 * ( tphi - cdlam ) ) ;
      end ;

      if az > AZBA then tag := 1
                   else tag := 0 ;

      if tag > 0 then begin
        sdlam := lp_lam + R110;
        cdlam := Cos( sdlam ) ;
        sdlam := Sin( sdlam ) ;
        z := S20 * sphi + C20 * cphi * cdlam ;
        if Abs( z ) > 1 then begin
          if Abs(z) > EPS10 then begin
            FError := ERROR_UNSOLVABLE ;
            projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end
          else begin
            if z < 0 then z := -1
                     else z :=  1 ;
          end ;
        end
        else
          z := ArcCos( z ) ;

        if tphi <> GIS_MAX_DOUBLE then
          az := ArcTan2( sdlam, ( C20 * tphi - S20 * cdlam ) ) ;
        av   := AZAB ;
        xy_y := RHOC ;
      end
      else begin
        z := S45 * ( sphi + cphi * cdlam ) ;
        if Abs( z ) > 1 then begin
          if Abs( z ) > EPS10 then begin
            FError := ERROR_UNSOLVABLE ;
            projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
            exit ;
          end
          else begin
            if z < 0 then z := -1
                     else z :=  1 ;
          end ;
        end
        else
          z := ArcCos( z ) ;

        av   := AZBA  ;
        xy_y := -RHOC ;
      end ;

      if z < 0 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      t := Power( Tan( 0.5 * z ), N ) ;
      r := F * t ;
      al := 0.5 * ( R104 - z ) ;

      if al < 0 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      al := ( t + Power( al, N ) ) / TC ;

      if Abs( al ) > 1 then begin
        if Abs(al) > EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if al < 0 then al := -1
                    else al :=  1 ;
        end ;
      end
      else
        al := ArcCos( al ) ;

      t := N * ( av - az ) ;
      if Abs( t ) < al then begin
        if tag = 0 then r := r / Cos( al - t )
                   else r := r / Cos( al + t ) ;
      end ;
      xy_x := r * Sin(t);

      if tag <> 0 then xy_y := xy_y - r * Cos( t )
                  else xy_y := xy_y + r * Cos( t ) ;

      if Q_noskew then begin
        t := xy_x;
        xy_x := -xy_x * CAZC - xy_y * SAZC ;
        xy_y := -xy_y * CAZC + t    * SAZC ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjBipolarConic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
    LAMB   = -0.34894976726250681539 ;
    AZBA   =  1.82261843856185925133 ;
    AZAB   =  0.81650043674686363166 ;
    CAZC   =  0.69691523038678375519 ;
    SAZC   =  0.71715351331143607555 ;
    R104   =  1.81514242207410275904 ;
    R110   =  1.91986217719376253360 ;
    C20    =  0.93969262078590838411 ;
    C45    =  0.70710678118654752469 ;
    S20    = -0.34202014332566873287 ;
    S45    =  0.70710678118654752410 ;
    RHOC   =  1.20709121521568721927 ;
    N      =  0.63055844881274687180 ;
    F      =  1.89724742567461030582 ;
    TC     =  1.27246578267089012270 ;
  var
    t, r, rp, rl, al,
    z, faz, az, s, c, av : Double ;
    neg, i : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if Q_noskew then begin
        t := xy_x;
        xy_x := -xy_x * CAZC + xy_y * SAZC ;
        xy_y := -xy_y * CAZC - t    * SAZC ;
      end ;

      if xy_x < 0 then neg := 1
                  else neg := 0 ;
      if neg <> 0 then begin
        xy_y := RHOC - xy_y ;
        s    := S20  ;
        c    := C20  ;
        av   := AZAB ;
      end
      else begin
        xy_y := xy_y + RHOC ;
        s    := S45  ;
        c    := C45  ;
        av   := AZBA ;
      end ;

      r   := Hypot( xy_x, xy_y ) ;
      rp  := r ;
      rl  := r ;
      az  := ArcTan2( xy_x, xy_y ) ;
      faz := Abs( az ) ;

      i := N_ITER ;
      z := 0 ;
      while i > 0 do begin
        z  := 2.0 * ArcTan( Power( r / F, 1 / N ) ) ;
        if ( R104 - z ) < 0 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end;

        al := ArcCos( ( Power( Tan( 0.5 * z ), N ) +
                        Power( Tan( 0.5 * ( R104 - z ) ), N )
                      ) / TC
                    ) ;
        if faz < al then begin
          if neg = 0 then az := - az ;
          r := rp * Cos( al + az ) ;
        end ;
        if Abs( rl - r ) < EPS7 then
          break;
        rl := r;

        dec( i ) ;
      end ;
      if i = 0 then begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;

      az := av - az / N ;
      if ( s * Cos( z ) + c * Sin( z ) * Cos( az ) ) > 1 then begin
        FError := ERROR_UNSOLVABLE ;
        unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;
      lp_phi := ArcSin( s * Cos( z ) + c * Sin( z ) * Cos( az ) ) ;
      lp_lam := ArcTan2( Sin( az ), c / Tan( z ) - s * Cos( az ) ) ;
      if neg <> 0 then lp_lam := lp_lam - R110
                  else lp_lam := LAMB - lp_lam ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjBoggsEumorphic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjBoggsEumorphic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    P_es := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjBoggsEumorphic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjBoggsEumorphic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjBoggsEumorphic.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjBoggsEumorphic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 20      ;
    FXC    = 2.00276 ;
    FXC2   = 1.11072 ;
    FYC    = 0.49931 ;
    FYC2   = 1.41421356237309504880 ;
  var
    theta, th1, c : Double ;
    i : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      theta := lp_phi;
      if Abs( Abs( lp_phi ) - HALFPI ) < EPS7 then
        xy_x := 0
      else begin
        c := Sin( theta ) * Pi ;
        for i := N_ITER downto 0 do begin
          th1 := ( theta + Sin( theta ) - c ) / ( 1 + Cos( theta ) ) ;
          theta := theta - th1 ;
          if Abs( th1 ) < EPS7 then break;
        end ;
        theta := theta * 0.5 ;
        xy_x := FXC * lp_lam / (1 / Cos( lp_phi ) + FXC2 / Cos( theta ) ) ;
      end ;
      xy_y := FYC * ( lp_phi + FYC2 * Sin( theta ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjCollignon
//------------------------------------------------------------------------------

  constructor TGIS_CSProjCollignon.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjCollignon.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjCollignon.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjCollignon.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjCollignon.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FXC = 1.12837916709551257390 ;
    FYC = 1.77245385090551602729 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_y := 1 - Sin(lp_phi) ;
      if xy_y <= 0 then xy_y := 0
                   else xy_y := Sqrt( xy_y ) ;

      xy_x := FXC * lp_lam * xy_y ;
      xy_y := FYC * ( 1 - xy_y ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjCollignon.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    FXC = 1.12837916709551257390 ;
    FYC = 1.77245385090551602729 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / FYC - 1 ;

      lp_phi := 1 - Sqr( lp_phi ) ;

      if      Abs( lp_phi ) < 1 then
                lp_phi := ArcSin(lp_phi)
      else if Abs( lp_phi ) > EPS10 then begin
                FError := ERROR_UNSOLVABLE ;
                projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                exit ;
              end
      else    begin
                if lp_phi < 0 then lp_phi := - HALFPI
                              else lp_phi :=   HALFPI ;
              end ;

      lp_lam := 1 - Sin( lp_phi ) ;
      if lp_lam  <= 0 then
        lp_lam := 0
      else
        lp_lam := xy_x / ( FXC * Sqrt( lp_lam ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjCrasterParabolic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjCrasterParabolic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjCrasterParabolic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjCrasterParabolic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjCrasterParabolic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjCrasterParabolic.Project3D_Ref(
             {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
           ) ;
  const
    XM    = 0.97720502380583984317 ;
    YM    = 3.06998012383946546542 ;
    THIRD = 0.33333333333333333333 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := lp_phi * THIRD ;
      xy_x := XM * lp_lam * (2 * Cos( lp_phi + lp_phi ) - 1) ;
      xy_y := YM * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjCrasterParabolic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RXM   = 1.02332670794648848847 ;
    RYM   = 0.32573500793527994772 ;
    THIRD = 0.33333333333333333333 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := 3 * ArcSin(xy_y * RYM);
      lp_lam := xy_x * RXM / ( 2 * Cos( ( lp_phi + lp_phi ) * THIRD) - 1 ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFahey
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFahey.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjFahey.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFahey.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFahey.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjFahey.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    asq : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := Tan( 0.5 * lp_phi ) ;
      xy_y := 1.819152 * xy_x ;
      asq  := 1 - xy_x * xy_x ;
      if asq <= 0 then asq := 0
                  else asq := Sqrt( asq ) ;

      xy_x := 0.819152 * lp_lam * asq ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjFahey.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y   := xy_y / 1.819152 ;
      lp_phi := 2 * ArcTan( xy_y ) ;
      xy_y   := 1 - xy_y * xy_y ;
      if Abs( xy_y ) < EPS7 then lp_lam := 0
                            else lp_lam := xy_x / ( 0.819152 * Sqrt( xy_y ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFoucautSinusoidal
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFoucautSinusoidal.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_n  := 0.75   ;
    Q_n1 := 1 -Q_n ;

    datumChanged ;
  end ;

  function TGIS_CSProjFoucautSinusoidal.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFoucautSinusoidal.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFoucautSinusoidal.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjFoucautSinusoidal.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      t := Cos( lp_phi ) ;
      xy_x := lp_lam * t / ( Q_n + Q_n1 * t ) ;
      xy_y := Q_n * lp_phi + Q_n1 * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjFoucautSinusoidal.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
  var
    v : Double ;
    i : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if Q_n <> 0 then begin
        lp_phi := xy_y;
        for i := N_ITER downto 0 do begin
          v := ( Q_n * lp_phi + Q_n1 * Sin( lp_phi ) - xy_y ) /
               ( Q_n + Q_n1 * Cos( lp_phi ) ) ;
          lp_phi := lp_phi - v ;
          if Abs(v) < EPS7 then
            break;
        end ;

        if i = 0 then begin
          if xy_y < 0 then lp_phi := - HALFPI
                      else lp_phi :=   HALFPI ;
        end ;
      end
      else
        lp_phi := utAasin( xy_y ) ;

      v := Cos(lp_phi);
      lp_lam := xy_x * (Q_n + Q_n1 * v) / v;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGinsburg_VIII
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGinsburg_VIII.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(TsNIIGAiK)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjGinsburg_VIII.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGinsburg_VIII.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGinsburg_VIII.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjGinsburg_VIII.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CL  = 0.000952426         ;
    CP  = 0.162388            ;
    C12 = 0.08333333333333333 ;
  var
    t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      t := lp_phi * lp_phi ;

      xy_y := lp_phi * ( 1 + t * C12 ) ;
      xy_x := lp_lam * ( 1 - CP * t  ) ;
      t := lp_lam * lp_lam ;
      xy_x := xy_x * ( 0.87 - CL * t * t ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGallStereograpphic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGallStereographic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjGallStereographic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGallStereographic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGallStereographic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjGallStereographic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    YF = 1.70710678118654752440 ;
    XF = 0.70710678118654752440 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := XF * lp_lam ;
      xy_y := YF * Tan( 0.5 * lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjGallStereographic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RYF = 0.58578643762690495119 ;
    RXF = 1.41421356237309504880 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_lam := RXF * xy_x ;
      lp_phi := 2 * ArcTan( xy_y * RYF ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLagrange
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLagrange.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Custom1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.Custom1Name := 'W' ;

    FParameters.Custom1 := 1.4 ;

    datumChanged ;
  end ;

  function TGIS_CSProjLagrange.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLagrange.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLagrange.datumChanged ;
  var
    phi1 : Double ;
  begin
    inherited datumChanged ;

    Q_rw := FParameters.Custom1 ;

    if Q_rw  <= 0 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    Q_rw  := 1 / Q_rw   ;
    Q_hrw := 0.5 * Q_rw ;

    phi1 := 0.5 ;
    phi1 := Sin( phi1 ) ;
    if Abs( Abs( phi1 ) - 1 ) < EPS10  then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    Q_a1 := Power( ( 1 - phi1 ) / (1 + phi1 ), Q_hrw ) ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjLagrange.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    v, c : Double;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Abs( Abs( lp_phi ) - HALFPI ) < EPS10 then begin
        xy_x := 0 ;
        if lp_phi < 0 then xy_y := -2
                      else xy_y :=  2 ;
      end
      else begin
        lp_phi := Sin( lp_phi ) ;
        v := Q_a1 * Power( ( 1 + lp_phi )/( 1 - lp_phi ), Q_hrw ) ;
        lp_lam := lp_lam * Q_rw ;
        c := 0.5 * ( v + 1/v ) + Cos( lp_lam ) ;
        if c < EPS10 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;
        xy_x := 2 * Sin( lp_lam ) / c ;
        xy_y := ( v - 1/v ) / c ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjHammer
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHammer.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudoazimuthal ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(Eckert-Greifendorff)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjHammer.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHammer.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjHammer.datumChanged ;
  begin
    inherited datumChanged ;

    Q_w  := 0.5 ;
    Q_m  := 1   ;
    Q_rm := 1   / Q_m ;
    Q_m  := Q_m / Q_w ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjHammer.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    cosphi, d : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      cosphi := Cos(lp_phi)  ;
      lp_lam := lp_lam * Q_w ;
      d := Sqrt( 2 / ( 1 + cosphi * Cos( lp_lam ) ) ) ;

      xy_x := Q_m  * d * cosphi * Sin( lp_lam ) ;
      xy_y := Q_rm * d * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjHatano
//------------------------------------------------------------------------------

  constructor TGIS_CSProjHatano.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := 'Asymmetrical Equal Area' ;

    datumChanged ;
  end ;

  function TGIS_CSProjHatano.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjHatano.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjHatano.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjHatano.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 20       ;
    CN     =  2.67595 ;
    CS     =  2.43763 ;
    FYCN   =  1.75859 ;
    FYCS   =  1.93052 ;
    FXC    =  0.85    ;
  var
    th1, c : Double  ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if lp_phi < 0 then c := Sin( lp_phi ) * CS
                    else c := Sin( lp_phi ) * CN ;

      for i := N_ITER downto 0 do begin
        th1    := lp_phi - ( lp_phi + Sin( lp_phi ) - c) / ( 1 + Cos( lp_phi ) ) ;
        lp_phi := th1 ;
        if Abs(th1) < EPS7 then break;
      end ;

      lp_phi := lp_phi * 0.5 ;

      xy_x := FXC * lp_lam * Cos( lp_phi ) ;
      if lp_phi < 0 then xy_y := Sin( lp_phi ) * FYCS
                    else xy_y := Sin( lp_phi ) * FYCN ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjHatano.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RYCN   = 0.56863737426006061674 ;
    RYCS   = 0.51799515156538134803 ;
    RXC    = 1.17647058823529411764 ;
    RCS    = 0.41023453108141924738 ;
    RCN    = 0.37369906014686373063 ;
  var
    th : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      if xy_y < 0 then th := xy_y * RYCS
                  else th := xy_y * RYCN ;

      if Abs( th ) > 1 then begin
        if Abs( th ) > ONETOL6 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if th > 0 then th :=  HALFPI
                    else th := -HALFPI ;
        end ;
      end
      else
        th := ArcSin( th ) ;

      lp_lam := RXC * xy_x / Cos( th ) ;
      th     := 2 * th ;

      if xy_y < 0 then lp_phi := ( th + Sin( th ) ) * RCS
                  else lp_phi := ( th + Sin( th ) ) * RCN ;

      if Abs( lp_phi ) > 1 then begin
        if Abs( lp_phi ) > ONETOL6 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, lp_lam, lp_phi ) ;
          exit ;
        end
        else begin
          if lp_phi > 0 then lp_phi :=  HALFPI
                        else lp_phi := -HALFPI ;
        end ;
      end
      else
        lp_phi := ArcSin( lp_phi ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjLaskowski
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLaskowski.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjLaskowski.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLaskowski.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLaskowski.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjLaskowski.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    A10 =  0.975534    ;
    A12 = -0.119161    ;
    A32 = -0.0143059   ;
    A14 = -0.0547009   ;
    B01 =  1.00384     ;
    B21 =  0.0802894   ;
    B03 =  0.0998909   ;
    B41 =  0.000199025 ;
    B23 = -0.0285500   ;
    B05 = -0.0491032   ;
  var
    l2, p2 : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      l2 := lp_lam * lp_lam ;
      p2 := lp_phi * lp_phi ;

      xy_x := lp_lam * ( A10 + p2 * ( A12 + l2 * A32 + p2 * A14 ) );
      xy_y := lp_phi * ( B01 + l2 * ( B21 + p2 * B23 + l2 * B41 ) +
                         p2 * ( B03 + p2 * B05 )
                       ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFlatPolarQuartic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFlatPolarQuartic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(McBryde-Thomas)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjFlatPolarQuartic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFlatPolarQuartic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFlatPolarQuartic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjFlatPolarQuartic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER  = 20 ;
    C       =  1.70710678118654752440 ;
    FYC     =  1.87475828462269495505 ;
    FXC     =  0.31245971410378249250 ;
  var
    th1, cc : Double  ;
    i       : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      cc := C * Sin( lp_phi ) ;

      for i := N_ITER downto 0 do begin
        th1 := (     Sin( 0.5*lp_phi ) + Sin( lp_phi ) - cc ) /
               ( 0.5*Cos( 0.5*lp_phi ) + Cos( lp_phi )      ) ;
        lp_phi := lp_phi - th1 ;
        if Abs( th1 ) < EPS7 then break ;
      end ;

      xy_x := FXC * lp_lam * ( 1.0 + 2*Cos( lp_phi ) / Cos( 0.5*lp_phi ) ) ;
      xy_y := FYC * Sin( 0.5*lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjFlatPolarQuartic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    RYC = 0.53340209679417701685 ;
    RXC = 3.20041258076506210122 ;
    RC  = 0.58578643762690495119 ;
  var
    t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := RYC * xy_y ;
      if Abs( lp_phi ) > 1 then begin
        if      Abs( lp_phi ) > ONETOL6 then begin
                  FError := ERROR_UNSOLVABLE ;
                  projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
                  exit ;
                end
        else if lp_phi < 0 then begin
                  t      := -1  ;
                  lp_phi := -Pi ;
                end
        else    begin
                  t      :=  1  ;
                  lp_phi :=  Pi ;
                end ;
      end
      else begin
        t      := lp_phi ;
        lp_phi := 2 * ArcSin( t );
      end ;

      lp_lam := RXC * xy_x / ( 1 + 2*Cos( lp_phi ) / Cos( 0.5*lp_phi ) ) ;
      lp_phi := RC * ( t + Sin( lp_phi ) ) ;

      if Abs( lp_phi ) > 1 then begin
        if Abs(lp_phi) > ONETOL6 then begin
          FError := ERROR_UNSOLVABLE ;
          projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if lp_phi < 0 then lp_phi := -HALFPI
                        else lp_phi :=  HALFPI ;
        end ;
      end
      else
        lp_phi := ArcSin( lp_phi ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFlatPolarParabololic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFlatPolarParabolic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(McBryde-Thomas)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjFlatPolarParabolic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFlatPolarParabolic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFlatPolarParabolic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjFlatPolarParabolic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CS  = 0.95257934441568037152 ;
    FXC = 0.92582009977255146156 ;
    FYC = 3.40168025708304504493 ;
    C23 = 0.66666666666666666666 ;
    C13 = 0.33333333333333333333 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := ArcSin( CS * Sin( lp_phi ) ) ;
      xy_x := FXC * lp_lam * ( 2*Cos( C23 * lp_phi ) - 1 ) ;
      xy_y := FYC * Sin( C13 * lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjFlatPolarParabolic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CS  = 0.95257934441568037152 ;
    C23 = 0.66666666666666666666 ;
    FXC = 0.92582009977255146156 ;
    FYC = 3.40168025708304504493 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / FYC ;
      if Abs( lp_phi ) >= 1 then begin
        if Abs( lp_phi ) > ONETOL8 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if lp_phi < 0 then lp_phi := -HALFPI
                        else lp_phi :=  HALFPI ;
        end ;
      end
      else
        lp_phi := ArcSin( lp_phi ) ;

      lp_phi := lp_phi * 3 ;
      lp_lam := xy_x / ( FXC * ( 2*Cos( C23 * lp_phi ) - 1 ) ) ;
      lp_phi := Sin( lp_phi ) / CS ;
      if Abs( lp_phi ) >= 1 then begin
        if Abs( lp_phi ) > ONETOL8 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end
        else begin
          if lp_phi < 0 then lp_phi := -HALFPI
                        else lp_phi :=  HALFPI ;
        end ;
      end
      else
        lp_phi := ArcSin(lp_phi);

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFlatPolarSine2
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFlatPolarSine2.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(McBryde-Thomas)' ;

    datumChanged ;
  end ;

  function TGIS_CSProjFlatPolarSine2.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFlatPolarSine2.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFlatPolarSine2.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjFlatPolarSine2.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
    C1     =  0.45503 ;
    C2     =  1.36509 ;
    C3     =  1.41546 ;
    CX     =  0.22248 ;
    CY     =  1.44492 ;
    C12    =  0.33333333333333333333333333 ;
  var
    k, v, t : Double  ;
    i       : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      k := C3 * Sin( lp_phi ) ;

      for i := N_ITER downto 0 do begin
        t := lp_phi / C2 ;
        v := ( C1* Sin( t ) + Sin( lp_phi ) - k ) /
             ( C12*Cos( t ) + Cos( lp_phi )     ) ;
        lp_phi := lp_phi - v ;
        if Abs( v ) < EPS7 then
          break;
      end ;

      t := lp_phi / C2 ;

      xy_x := CX * lp_lam * ( 1 + 3*Cos( lp_phi ) / Cos( t ) ) ;
      xy_y := CY * Sin( t ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjFlatPolarSine2.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C1 = 0.45503 ;
    C2 = 1.36509 ;
    C3 = 1.41546 ;
    CX = 0.22248 ;
    CY = 1.44492 ;
  var
    t : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      t := utAasin( xy_y / CY ) ;

      lp_phi := C2 * t ;
      lp_lam := xy_x / ( CX * ( 1 + 3*Cos( lp_phi )/ Cos( t ) ) ) ;
      lp_phi := utAasin( ( C1*Sin( t ) + Sin( lp_phi ) ) / C3 ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P2
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P2.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P2.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P2.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P2.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPutnins_P2.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
    CX     = 1.89490 ;
    CY     = 1.71848 ;
    CP     = 0.6141848493043784 ;
  var
    p, c, s, v : Double ;
    i : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      p := CP * Sin( lp_phi ) ;
      s := lp_phi * lp_phi ;
      lp_phi := lp_phi *( 0.615709 + s * ( 0.00909953 + s*0.0046292 ) ) ;

      for i := N_ITER downto 0 do begin
        SinCos( lp_phi, s, c ) ;
        v := ( lp_phi + s*( c - 1 ) - p ) / ( 1 + c*( c - 1 ) - s*s ) ;
        lp_phi := lp_phi - v ;
        if Abs(v) < EPS10 then
          break;
      end ;

      if i = 0 then begin
        if lp_phi < 0 then lp_phi := -THIRPI
                      else lp_phi :=  THIRPI ;
      end ;

      xy_x := CX * lp_lam * ( Cos( lp_phi ) - 0.5 ) ;
      xy_y := CY * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPutnins_P2.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CX = 1.89490 ;
    CY = 1.71848 ;
    CP = 0.6141848493043784 ;
  var
    c : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := utAasin( xy_y / CY ) ;
      c      := Cos( lp_phi ) ;

      lp_lam := xy_x / ( CX * ( c - 0.5 ) ) ;
      lp_phi := utAasin( ( lp_phi + Sin( lp_phi )*( c - 1 ) ) / CP ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P3
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P3.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    RPISQ = 0.1013211836 ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_A := 4 * RPISQ ;
    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P3.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P3.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P3.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPutnins_P3.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 0.79788456 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := C * lp_lam * ( 1 - Q_A * lp_phi*lp_phi ) ;
      xy_y := C * lp_phi;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPutnins_P3.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 0.79788456 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / C;
      lp_lam := xy_x / ( C*( 1 - Q_A * lp_phi*lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P3p
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P3p.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    RPISQ = 0.1013211836 ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_A := 2 * RPISQ ;
    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P3p.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P3p.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P3p.datumChanged ;
  begin
    // sape parameters as Putnins_p3
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P4p
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P4p.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    CX = 0.874038744 ;
    CY = 3.883251825 ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := CX ;
    Q_C_y := CY ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P4p.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P4p.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P4p.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPutnins_P4p.Project3D_Ref(
   {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 0.883883476 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := utAasin( C * Sin( lp_phi ) ) ;
      xy_x   := Q_C_x * lp_lam * Cos( lp_phi ) ;
      lp_phi := lp_phi / 3 ;

      xy_x := xy_x  / Cos( lp_phi ) ;
      xy_y := Q_C_y * Sin( lp_phi ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPutnins_P4p.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 1.13137085 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := utAasin( xy_y / Q_C_y ) ;
      lp_lam := xy_x * Cos( lp_phi ) / Q_C_x;
      lp_phi := 3 * lp_phi ;
      lp_lam := lp_lam / Cos( lp_phi ) ;
      lp_phi := utAasin( C * Sin( lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWerenskiold_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWerenskiold_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    CX = 1           ;
    CY = 4.442882938 ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := CX ;
    Q_C_y := CY ;

    datumChanged ;
  end ;

  function TGIS_CSProjWerenskiold_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWerenskiold_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWerenskiold_I.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P5
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P5.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_A := 2 ;
    Q_B := 1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P5.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P5.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P5.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPutnins_P5.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 1.01346   ;
    D = 1.2158542 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := C * lp_lam * ( Q_A - Q_B * Sqrt( 1 + D * lp_phi*lp_phi ) ) ;
      xy_y := C * lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPutnins_P5.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    C = 1.01346   ;
    D = 1.2158542 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / C;
      lp_lam := xy_x / ( C * ( Q_A - Q_B * Sqrt( 1 + D * lp_phi*lp_phi ) ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P5p
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P5p.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_A := 1.5 ;
    Q_B := 0.5 ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P5p.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P5p.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P5p.datumChanged ;
  begin
    // same parameters as Puntings_P5
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P6
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P6.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    CX = 1.01346 ;
    CY = 0.91910 ;
    A  = 4       ;
    B  = 2.1471437182129378784 ;
    D  = 2       ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := CX ;
    Q_C_y := CY ;
    Q_A   := A  ;
    Q_B   := B  ;
    Q_D   := D  ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P6.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P6.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P6.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjPutnins_P6.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER  = 10 ;
    CONPOLE =  1.732050807568877 ;
    C       =  1.10265779        ;
  var
    p, r, v : Double ;
    i       : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      p := Q_B * Sin( lp_phi ) ;
      lp_phi := lp_phi * C ;
      for i := N_ITER downto 0 do begin
        r := Sqrt( 1 + lp_phi*lp_phi ) ;
        v := ( ( Q_A - r ) * lp_phi  -  Ln( lp_phi + r ) - p ) /
             ( Q_A - 2*r ) ;
        lp_phi := lp_phi - v ;
        if Abs(v) < EPS10 then
          break;
      end ;
      if i = 0 then
        if p < 0 then lp_phi := -CONPOLE
                 else lp_phi :=  CONPOLE ;

      xy_x := Q_C_x * lp_lam * ( Q_D - Sqrt( 1 + lp_phi*lp_phi ) ) ;
      xy_y := Q_C_y * lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjPutnins_P6.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER  = 10 ;
    CONPOLE =  1.732050807568877 ;
  var
    r : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := xy_y / Q_C_y;
      r := Sqrt( 1 + lp_phi*lp_phi ) ;
      lp_lam := xy_x / ( Q_C_x *( Q_D - r ) );
      lp_phi := utAasin( ( ( Q_A - r )*lp_phi - Ln( lp_phi + r ) ) / Q_B );

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjPutnins_P6p
//------------------------------------------------------------------------------

  constructor TGIS_CSProjPutnins_P6p.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  const
    CX = 0.44329 ;
    CY = 0.80404 ;
    A  = 6       ;
    B  = 5.61125 ;
    D  = 3       ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := CX ;
    Q_C_y := CY ;
    Q_A   := A  ;
    Q_B   := B  ;
    Q_D   := D  ;

    datumChanged ;
  end ;

  function TGIS_CSProjPutnins_P6p.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjPutnins_P6p.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjPutnins_P6p.datumChanged ;
  begin
    // same parameters as Putnins_P6
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjRectangularPolyconic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjRectangularPolyconic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.StandardParallel_1 := DegToRad( 45.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjRectangularPolyconic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjRectangularPolyconic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjRectangularPolyconic.datumChanged ;
  begin
    inherited datumChanged ;

    Q_phi1 := FParameters.StandardParallel_1 ;
    Q_fxb  := 0.5 * Sin( Q_phi1 ) ;
    Q_fxa  := 0.5 / Q_fxb ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjRectangularPolyconic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    fa     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if Q_phi1 > 0 then fa := Tan( lp_lam * Q_fxb ) * Q_fxa
                    else fa := 0.5 * lp_lam ;

      if Abs( lp_phi ) < EPS10 then begin
        xy_x := fa + fa ;
        xy_y := - P_phi0 ;
      end
      else begin
        xy_y := 1 / Tan( lp_phi );
        fa   := 2 * ArcTan( fa * Sin( lp_phi ) ) ;
        xy_x := Sin( fa ) * xy_y ;
        xy_y := lp_phi - P_phi0 + ( 1 - Cos( fa ) )*xy_y ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjKavraisky_V
//------------------------------------------------------------------------------

  constructor TGIS_CSProjKavraisky_V.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := 1.35439 / 1.50488 ;
    Q_C_y := 1.50488 ;
    Q_C_p := 1 / 1.35439 ;
    Q_tan_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjKavraisky_V.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjKavraisky_V.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjKavraisky_V.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjKavraisky_V.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    c      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := Q_C_x * lp_lam * Cos( lp_phi ) ;
      xy_y := Q_C_y ;

      lp_phi := lp_phi * Q_C_p ;
      c := Cos( lp_phi ) ;

      if Q_tan_mode > 0 then begin
        xy_x := xy_x * c*c ;
        xy_y := xy_y * Tan( lp_phi ) ;
      end
      else begin
        xy_x := xy_x / c ;
        xy_y := xy_y * Sin( lp_phi ) ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjKavraisky_V.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    c      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := xy_y / Q_C_y ;

      if Q_tan_mode > 0 then lp_phi := ArcTan ( xy_y )
                        else lp_phi := utAasin (xy_y ) ;
      c := Cos( lp_phi ) ;
      lp_phi := lp_phi / Q_C_p ;
      lp_lam := xy_x / ( Q_C_x * Cos( lp_phi ) ) ;

      if Q_tan_mode > 0 then lp_lam := lp_lam / (c*c)
                        else lp_lam := lp_lam * c ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjQuarticAuthalic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjQuarticAuthalic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    Q_C_x := 1   ;
    Q_C_y := 2   ;
    Q_C_p := 0.5 ;
    Q_tan_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjQuarticAuthalic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjQuarticAuthalic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjQuarticAuthalic.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFlatPolarSine1
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFlatPolarSine1.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(McBryde-Thomas)' ;

    Q_C_x := 1.36509/1.48875 ;
    Q_C_y := 1.48875 ;
    Q_C_p := 1/1.36509 ;
    Q_tan_mode := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjFlatPolarSine1.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFlatPolarSine1.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFlatPolarSine1.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjFoucautPseudocylindrical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjFoucautPseudocylindrical.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(Prepetit)' ;

    Q_C_x := 1   ;
    Q_C_y := 2   ;
    Q_C_p := 0.5 ;
    Q_tan_mode := 1 ;

    datumChanged ;
  end ;

  function TGIS_CSProjFoucautPseudocylindrical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjFoucautPseudocylindrical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjFoucautPseudocylindrical.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTransverseCentralCylindrical
//------------------------------------------------------------------------------

  constructor TGIS_CSProjTransverseCentralCylindrical.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Cylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := True ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    P_es   := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjTransverseCentralCylindrical.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTransverseCentralCylindrical.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTransverseCentralCylindrical.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjTransverseCentralCylindrical.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    b, bt  : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      b := Cos( lp_phi ) * Sin( lp_lam ) ;
      bt := 1 - b*b ;
      if bt < EPS10 then begin
        FError := ERROR_UNSOLVABLE ;
        projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
        exit ;
      end ;
      xy_x := b / Sqrt(bt);
      xy_y := ArcTan2( Tan( lp_phi ) , Cos( lp_lam ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjTransverseCylindricalEqualArea
//------------------------------------------------------------------------------

  constructor TGIS_CSProjTransverseCylindricalEqualArea.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalTransverse ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := ''    ;

    datumChanged ;
  end ;

  function TGIS_CSProjTransverseCylindricalEqualArea.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjTransverseCylindricalEqualArea.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjTransverseCylindricalEqualArea.datumChanged ;
  begin
    inherited datumChanged ;

    P_k0  := FParameters.ScaleFactor ;

    Q_rk0 := 1 / P_k0 ;

    set_Validity( FParameters.CentralMeridian, ZONE_90, -ZONE_180, ZONE_180,
                  0                          , ZONE_90, -ZONE_90 , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjTransverseCylindricalEqualArea.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := Q_rk0 * Cos( lp_phi ) * Sin( lp_lam ) ;
      xy_y := P_k0 * ( ArcTan2( Tan( lp_phi ), Cos( lp_lam ) ) - P_phi0 ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjTransverseCylindricalEqualArea.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y := xy_y * Q_rk0 + P_phi0 ;
      xy_x := xy_x * P_k0 ;

      t := Sqrt(1 - xy_x * xy_x) ;

      lp_phi := ArcSin( t * Sin( xy_y ) ) ;
      lp_lam := ArcTan2( xy_x, t * Cos( xy_y ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjUrmaev_V
//------------------------------------------------------------------------------

  constructor TGIS_CSProjUrmaev_V.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Custom1,
                        TGIS_CSProjParameter.Custom2,
                        TGIS_CSProjParameter.Custom3
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.Custom1Name := 'n' ;
    FParameters.Custom2Name := 'q' ;
    FParameters.Custom3Name := 'alpha' ;

    FParameters.Custom1 := 0.8   ;
    FParameters.Custom2 := 1.0/3 ;
    FParameters.Custom3 := 0.5   ;

    datumChanged ;
  end ;

  function TGIS_CSProjUrmaev_V.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjUrmaev_V.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjUrmaev_V.datumChanged ;
  var
    alpha, t : Double ;
  begin
    inherited datumChanged ;

    P_es   := 0 ;

    Q_n   := FParameters.Custom1  ;
    Q_q3  := FParameters.Custom2 ;
    alpha := FParameters.Custom3 ;

    t := Q_n *Sin(alpha) ;
    Q_m := Cos(alpha) / Sqrt(1 -t * t) ;
    Q_rmn := 1/(Q_m * Q_n) ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjUrmaev_V.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t      : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := utAasin( Q_n * Sin( lp_phi ) ) ;
      t := lp_phi ;

      xy_x := Q_m * lp_lam * Cos( lp_phi ) ;
      t := Sqr( t ) ;
      xy_y := lp_phi * ( 1 + t * Q_q3 ) * Q_rmn ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjUrmaevFlatPolarSinusoidal
//------------------------------------------------------------------------------

  constructor TGIS_CSProjUrmaevFlatPolarSinusoidal.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.ScaleFactor
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    FParameters.ScaleFactor := 0.7 ;

    P_es  := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjUrmaevFlatPolarSinusoidal.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjUrmaevFlatPolarSinusoidal.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjUrmaevFlatPolarSinusoidal.datumChanged ;
  const
    CY = 1.139753528477 ;
  begin
    Q_n  := FParameters.ScaleFactor ;

    if ( Q_n < 0 ) or ( Q_n > 1 ) then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    Q_C_y := CY / Q_n ;

    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjUrmaevFlatPolarSinusoidal.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CX = 0.8773826753 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      lp_phi := utAasin( Q_n * Sin( lp_phi ) ) ;
      xy_x   := CX * lp_lam * Cos( lp_phi ) ;
      xy_y   := Q_C_y * lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjUrmaevFlatPolarSinusoidal.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    CX = 0.8773826753 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      xy_y   := xy_y /Q_C_y ;
      lp_phi := utAasin( Sin( xy_y ) / Q_n ) ;
      lp_lam := xy_x / ( CX * Cos( xy_y ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjWagner_I
//------------------------------------------------------------------------------

  constructor TGIS_CSProjWagner_I.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True ;
    FDescription   := '' ;

    P_es   := 0 ;

    datumChanged ;
  end ;

  function TGIS_CSProjWagner_I.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjWagner_I.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjWagner_I.datumChanged ;
  const
    QN = 0.8660254037844386467637231707 ;
  begin
    // parametrs same as UrmaevFlatPolarSinusoidal
    FParameters.ScaleFactor := QN ;

    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjNell
//------------------------------------------------------------------------------

  constructor TGIS_CSProjNell.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjNell.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjNell.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjNell.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjNell.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 10 ;
    A1     =  1.00371   ;
    A2     = -0.0935382 ;
    A3     = -0.011412  ;
  var
    k, v   : Double ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      k := 2 * Sin(lp_phi);
      v := lp_phi * lp_phi;
      lp_phi := lp_phi * ( A1 + v*( A2 + v*A3) ) ;
      for i := N_ITER downto 0 do begin
        v := ( lp_phi + Sin( lp_phi ) - k ) / ( 1 + Cos( lp_phi ) ) ;

        lp_phi := lp_phi - v ;
        if Abs( v ) < EPS7 then
          break;
      end ;

      xy_x := 0.5 * lp_lam * ( 1 + Cos( lp_phi ) );
      xy_y := lp_phi ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjNell.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_lam := 2*xy_x / ( 1 + Cos( xy_y ) );
      lp_phi := utAasin( 0.5*( xy_y + Sin( xy_y ) ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjNellHammer
//------------------------------------------------------------------------------

  constructor TGIS_CSProjNellHammer.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjNellHammer.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjNellHammer.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjNellHammer.datumChanged ;
  begin
    inherited datumChanged ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_70
                ) ;
  end ;

  procedure TGIS_CSProjNellHammer.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      xy_x := 0.5 * lp_lam * ( 1 + Cos( lp_phi ) ) ;
      xy_y := 2.0 * ( lp_phi - Tan( 0.5 * lp_phi ) ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjNellHammer.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER  = 9    ;
  var
    v, c, p : Double  ;
    i       : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := 0 ;
      p := 0.5 * xy_y ;
      for i := N_ITER downto 0 do begin
        c := Cos( 0.5 * lp_phi ) ;
        v := ( lp_phi - Tan( lp_phi / 2 ) - p ) / ( 1 - 0.5/( c*c ) ) ;
        lp_phi := lp_phi - v ;
        if Abs(v) < EPS7 then
          break;
      end ;
      if i = 0 then begin
        if lp_phi < 0 then lp_phi := - HALFPI
                      else lp_phi :=   HALFPI ;

        lp_lam := 2 * xy_x;
      end
      else
        lp_lam := 2 * xy_x / ( 1 + Cos( lp_phi ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjNicolosiGlobular
//------------------------------------------------------------------------------

  constructor TGIS_CSProjNicolosiGlobular.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjNicolosiGlobular.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjNicolosiGlobular.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjNicolosiGlobular.datumChanged ;
  begin
    inherited datumChanged ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjNicolosiGlobular.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    tb, c, d, m, n,
    r2, sp, x, y    : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if      Abs( lp_lam ) < EPS10 then begin
                xy_x := 0      ;
                xy_y := lp_phi ;
              end
      else if Abs( lp_phi ) < EPS10 then begin
                xy_x := lp_lam ;
                xy_y := 0      ;
              end
      else if Abs( Abs( lp_lam ) - HALFPI ) < EPS10 then begin
                xy_x := lp_lam * Cos( lp_phi ) ;
                xy_y := HALFPI * Sin( lp_phi ) ;
              end
      else if Abs( Abs( lp_phi ) - HALFPI ) < EPS10 then begin
                xy_x := 0      ;
                xy_y := lp_phi ;
              end
      else    begin
                tb := HALFPI / lp_lam - lp_lam / HALFPI ;
                c  := lp_phi / HALFPI ;
                sp := Sin( lp_phi ) ;
                d  := ( 1 - c*c ) / ( sp - c ) ;
                r2 := tb / d ;
                r2 := Sqr( r2 ) ;
                m := ( tb*sp / d - 0.5*tb )/ ( 1 + r2 ) ;
                n := ( sp / r2 + 0.5*d ) / (1 + 1/r2 ) ;
                xy_x := Cos( lp_phi ) ;
                xy_x := Sqrt( m*m + xy_x*xy_x / ( 1 + r2 ) ) ;
                if lp_lam < 0 then x := -xy_x
                              else x :=  xy_x ;
                xy_x := HALFPI * ( m + x) ;
                xy_y := Sqrt( n*n - ( sp*sp / r2 + d*sp - 1 ) / ( 1 + 1/r2 ) ) ;

                if lp_phi < 0 then y :=  xy_y
                              else y := -xy_y ;
                xy_y := HALFPI * ( n + y ) ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGoode
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGoodeHomolosine.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Pseudocylindrical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '(Fusion of Mollweide and Sinusoidal)' ;

    Q_sinu := TGIS_CSProjSinusoidal.Create( 0, '' ) ;
    Q_moll := TGIS_CSProjMollweide.Create ( 0, '' ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjGoodeHomolosine.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGoodeHomolosine.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGoodeHomolosine.doDestroy ;
  begin
    FreeObject( Q_sinu ) ;
    FreeObject( Q_moll ) ;

    inherited ;
  end ;

  procedure TGIS_CSProjGoodeHomolosine.datumChanged ;
  begin
    inherited datumChanged ;

    Q_sinu.Datum := Datum ;
    Q_moll.Datum := Datum ;

    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjGoodeHomolosine.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    PHI_LIM = 0.71093078197902358062 ;
    Y_COR   = 0.05280 ;
  var
    lp_lam : Double ;
    lp_phi : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

    if Abs( _coords.Y ) <= PHI_LIM then
      Q_sinu.Project3D_Ref(_coords)
    else begin
      Q_moll.Project3D_Ref(_coords) ;
      if lp_phi > 0 then _coords.Y := _coords.Y - Y_COR
                    else _coords.Y := _coords.Y + Y_COR ;
    end ;

  end ;

  procedure TGIS_CSProjGoodeHomolosine.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    PHI_LIM = 0.71093078197902358062 ;
    Y_COR   = 0.05280 ;
  begin
    if Abs( _coords.Y ) <= PHI_LIM then
      Q_sinu.Unproject3D_Ref( _coords )
    else begin
      if _coords.Y >= 0 then _coords.Y := _coords.Y + Y_COR
                        else _coords.Y := _coords.Y - Y_COR ;
      Q_moll.Unproject3D_Ref( _coords ) ;
    end ;

  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjInternationalMapWorldPolyconic
//------------------------------------------------------------------------------

  constructor TGIS_CSProjInternationalMapWorldPolyconic.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Conical ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.StandardParallel_1,
                        TGIS_CSProjParameter.StandardParallel_2
                      ] ;

    FIsPureCylindrical := False ;
    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := False ;
    FDescription   := '(International Map of the World Polyconic)' ;

    FParameters.StandardParallel_1 := DegToRad( 30.0 ) ;
    FParameters.StandardParallel_2 := DegToRad( 45.0 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjInternationalMapWorldPolyconic.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjInternationalMapWorldPolyconic.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjInternationalMapWorldPolyconic.locFor(
    var   _yc     : Double ;
    const _lp_lam : Double ;
    const _lp_phi : Double ;
    var   _xy_x   : Double ;
    var   _xy_y   : Double
  ) ;
  var
    xa, ya, xb, yb,
    xc, d, b, m,
    sp, t, r, c       : Double ;
  begin
    if _lp_phi = 0 then begin
      _xy_x := _lp_lam ;
      _xy_y := 0 ;
    end
    else begin
      sp := Sin( _lp_phi ) ;
      m := utMlfn( _lp_phi, sp, Cos( _lp_phi ), Q_en ) ;
      xa := Q_Pp + Q_Qp * m ;
      ya := Q_P  + Q_Q  * m ;
      r := 1 / ( Tan( _lp_phi ) * Sqrt( 1 - P_es * sp*sp ) ) ;
      c := Sqrt( Abs( r*r - xa*xa ) ) ;
      if _lp_phi < 0 then c := -c  ;

      c := c + ya - r ;
      if Q_mode < 0 then begin
        xb := _lp_lam ;
        yb := Q_C2   ;
      end
      else begin
        t := _lp_lam * Q_sphi_2 ;
        xb := Q_R_2 * Sin( t ) ;
        yb := Q_C2 + Q_R_2*( 1 - Cos( t ) ) ;
      end ;

      if Q_mode > 0 then begin
        xc := _lp_lam ;
        _yc := 0      ;
      end
      else begin
        t := _lp_lam * Q_sphi_1 ;
        xc := Q_R_1 * Sin(t) ;
        _yc := Q_R_1 * ( 1 - Cos( t ) ) ;
      end ;

      d := ( xb - xc ) / ( yb - _yc ) ;
      b := xc + d * ( c + r - _yc ) ;
      _xy_x := d * Sqrt( Abs( r*r * ( 1 + d*d ) - b*b ) ) ;
      if _lp_phi > 0 then _xy_x := -_xy_x ;

      _xy_x := ( b + _xy_x ) / ( 1 + d*d ) ;
      _xy_y := Sqrt( Abs( r*r - _xy_x*_xy_x ) ) ;
      if _lp_phi > 0 then _xy_y := -_xy_y ;

      _xy_y := _xy_y + c + r ;
    end ;
  end ;

  procedure TGIS_CSProjInternationalMapWorldPolyconic.datumChanged ;
  var
    lat         : Double ;
    del, sig, s : Double ;
    t, x1, x2   : Double ;
    t2, y1, m1  : Double ;
    m2, y2      : Double ;
    i           : Integer ;

    function phi12( var del, sig : Double ) : Integer ;
    begin
      Result := 0 ;

      if ( Q_phi_1 = 0 ) or ( Q_phi_2 = 0 ) then
        Result := -41
      else begin
        del := 0.5 * ( Q_phi_2 - Q_phi_1 ) ;
        sig := 0.5 * ( Q_phi_2 + Q_phi_1 ) ;
        lat := sig ;
        if ( Abs( del ) < EPS10 ) or ( Abs( sig ) < EPS10 ) then
          Result := -42 ;
      end ;
    end ;

    procedure xy_( var phi, x, y, sp, r : Double ) ;
    var
      f : Double ;
    begin
      sp := Sin( phi ) ;
      r  := 1 / ( Tan( phi ) * Sqrt( 1 - P_es * sp*sp ) ) ;
      f := Q_lam_1 * sp ;
      y := r * ( 1 - Cos( f ) ) ;
      x := r * Sin( f ) ;
    end;

  begin
    inherited datumChanged ;

    Q_phi_1 := FParameters.StandardParallel_1 ;
    Q_phi_2 := FParameters.StandardParallel_2 ;

    i := phi12( del, sig ) ;
    if i <> 0 then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    if Q_phi_2 < Q_phi_1 then begin // make sure Q_phi_1 most southerly
      del     := Q_phi_1 ;
      Q_phi_1 := Q_phi_2 ;
      Q_phi_2 := del     ;
    end ;

    // this implementation does not accept LongitudeOfPoint_1
    sig := Abs( RadToDeg( sig ) ) ;
    if      sig <= 60 then
            sig := 2
    else if sig <= 76 then
            sig := 4
    else    sig := 8 ;
    Q_lam_1 := DegToRad( sig ) ;

    if Q_phi_1 <> 0 then
      xy_(Q_phi_1, x1, y1, Q_sphi_1, Q_R_1)
    else begin
      Q_mode := 1 ;
      y1     := 0 ;
      x1     := Q_lam_1 ;
    end ;

    if Q_phi_2 <> 0 then
      xy_( Q_phi_2, x2, t2, Q_sphi_2, Q_R_2 )
    else begin
      Q_mode := -1 ;
      t2 := 0 ;
      x2 := Q_lam_1 ;
    end ;

    Q_en := utEnfn( P_es ) ;

    m1   := utMlfn( Q_phi_1, Q_sphi_1, Cos(Q_phi_1), Q_en ) ;
    m2   := utMlfn( Q_phi_2, Q_sphi_2, Cos(Q_phi_2), Q_en ) ;
    t    := m2 - m1 ;
    s    := x2 - x1 ;
    y2   := Sqrt( t*t - s*s ) + y1 ;
    Q_C2 := y2 - t2 ;
    t    := 1 / t ;
    Q_P  := ( m2*y1 - m1*y2 ) * t ;
    Q_Q  := ( y2 - y1) * t ;
    Q_Pp := ( m2*x1 - m1*x2 ) * t ;
    Q_Qp := ( x2 - x1) * t ;

    set_Validity( FParameters.CentralMeridian, ZONE_35,
                  lat                        , ZONE_35, -ZONE_90, ZONE_90
                )
  end ;

  procedure TGIS_CSProjInternationalMapWorldPolyconic.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    yc     : Double     ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      locFor( yc, lp_lam, lp_phi, xy_x, xy_y ) ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjInternationalMapWorldPolyconic.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    t_x    : Double  ;
    t_y    : Double  ;
    yc     : Double  ;
    i      : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lp_phi := Q_phi_2 ;
      lp_lam := xy_x / Cos( lp_phi ) ;
      i := 0 ;
      repeat
        locFor( yc, lp_lam, lp_phi, t_x, t_y ) ;
        lp_phi := ( ( lp_phi - Q_phi_1 )*( xy_y - yc ) / ( t_y - yc ) ) +
                  Q_phi_1 ;
        if t_x <> 0 then
          lp_lam := lp_lam * xy_x / t_x
        else
          lp_lam := 0 ;
        inc( i ) ;
        if i >100 then begin
          FError := ERROR_UNSOLVABLE ;
          unprojectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
          exit ;
        end ;
      until ( Abs( t_x - xy_x ) <= EPS10 ) or ( Abs( t_y - xy_y ) <= EPS10 ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// CSProjChamberlinTrimetric
//------------------------------------------------------------------------------

  constructor TGIS_CSProjChamberlinTrimetric.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Custom1,
                        TGIS_CSProjParameter.Custom2,
                        TGIS_CSProjParameter.Custom3,
                        TGIS_CSProjParameter.Custom4,
                        TGIS_CSProjParameter.Custom5,
                        TGIS_CSProjParameter.Custom6
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := False ;
    FIsEllipsoid   := False ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.Custom1Name := 'phi_0' ;
    FParameters.Custom2Name := 'lam_0' ;
    FParameters.Custom3Name := 'phi_1' ;
    FParameters.Custom4Name := 'lam_1' ;
    FParameters.Custom5Name := 'phi_2' ;
    FParameters.Custom6Name := 'lam_2' ;

    FParameters.Custom1 := DegToRad(  55.0 ) ;
    FParameters.Custom2 := DegToRad( 150.0 ) ;

    FParameters.Custom3 := DegToRad(  55.0 ) ;
    FParameters.Custom4 := DegToRad(  35.0 ) ;

    FParameters.Custom5 := DegToRad(  10.0 ) ;
    FParameters.Custom6 := DegToRad(  92.5 ) ;

    datumChanged ;
  end ;

  function TGIS_CSProjChamberlinTrimetric.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjChamberlinTrimetric.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjChamberlinTrimetric.vect(
    _dphi : Double ;
    _c1   : Double ;
    _s1   : Double ;
    _c2   : Double ;
    _s2   : Double ;
    _dlam : Double
  );
  var
    cdl, dp, dl : Double ;
  begin
    cdl := Cos( _dlam ) ;
    if ( Abs( _dphi ) > 1 ) or ( Abs( _dlam ) > 1 ) then
      Q_rv_r := utAacos( _s1 * _s2  +  _c1 * _c2 * cdl )
    else  begin  // more accurate for smaller distances
      dp := Sin( 0.5 * _dphi ) ;
      dl := Sin( 0.5 * _dlam );
      Q_rv_r := 2 * utAasin( Sqrt( dp * dp  +  _c1 * _c2 * dl * dl ) ) ;
    end ;

    if Q_rv_r > GIS_MAX_SINGLE then begin
      bDatumReady := False ;
      exit ;
    end;


    if Abs(Q_rv_r) > EPS9 then
      Q_rv_az := ArcTan2( _c2 * Sin( _dlam ), _c1 * _s2  -  _s1 * _c2 * cdl )
    else begin
      Q_rv_az := 0;
      Q_rv_r  := Q_rv_az ;
    end ;
  end ;

  procedure TGIS_CSProjChamberlinTrimetric.datumChanged ;
  var
    i, j : Integer ;
    function lc(b, c, a : Double) : Double ;
    begin
      Result := utAacos(0.5 * (b * b + c * c - a * a) / (b * c));
    end ;

  begin
    inherited datumChanged ;

    Q_c[0].phi := FParameters.Custom1 ;
    Q_c[0].lam := FParameters.Custom2 ;

    Q_c[1].phi := FParameters.Custom3 ;
    Q_c[1].lam := FParameters.Custom4 ;

    Q_c[2].phi := FParameters.Custom5 ;
    Q_c[2].lam := FParameters.Custom6 ;

    for i := 0 to 2 do begin  //control point locations
      Q_c[i].lam := utAdjlon(Q_c[i].lam -P_lam0) ;
      Q_c[i].cosphi := Cos(Q_c[i].phi) ;
      Q_c[i].sinphi := Sin(Q_c[i].phi) ;
    end ;

    for i := 0 to 2 do begin  // inter ctl pt. distances and azimuths
      if i = 2 then
        j := 0
      else
        j := i +1;

      vect(Q_c[j].phi - Q_c[i].phi, Q_c[i].cosphi, Q_c[i].sinphi,
           Q_c[j].cosphi, Q_c[j].sinphi, Q_c[j].lam - Q_c[i].lam);

      Q_c[i].v_r  := Q_rv_r  ;
      Q_c[i].v_az := Q_rv_az ;
      if ( Q_c[i].v_r = 0 ) or ( not bDatumReady ) then begin
        FError := ERROR_WRONGPARAMS ;
        bDatumReady := False ;
        exit ;
      end ;
      // co-linearity problem ignored for now
    end ;

    Q_beta_0 := lc(Q_c[0].v_r, Q_c[2].v_r, Q_c[1].v_r) ;
    if Q_beta_0 > GIS_MAX_SINGLE then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end;
    Q_beta_1 := lc(Q_c[0].v_r, Q_c[1].v_r, Q_c[2].v_r) ;
    if Q_beta_1 > GIS_MAX_SINGLE then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end;
    Q_beta_2 := Pi - Q_beta_0;

    Q_c[1].p_y := Q_c[2].v_r * Sin(Q_beta_0) ;
    Q_c[0].p_y := Q_c[1].p_y ;

    Q_p_y := 2 * Q_c[0].p_y ;
    Q_c[2].p_y := 0;
    Q_c[1].p_x := 0.5 * Q_c[0].v_r ;
    Q_c[0].p_x := - Q_c[1].p_x ;
    Q_c[2].p_x := Q_c[0].p_x + Q_c[2].v_r * Cos(Q_beta_0) ;
    Q_p_x := Q_c[2].p_x ;

    // not reverisible - no bounding polygon
  end ;

  procedure TGIS_CSProjChamberlinTrimetric.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    THIRD = 0.333333333333333333 ;
  var
    v      : array [0..2] of T_ChamberlinTrimetric_Record2 ;
    sinphi,
    cosphi,
    a      : Double ;
    i, j   : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;

    function lc(b, c, a : Double) : Double ;
    begin
      Result := utAacos( 0.5 * ( b*b + c*c - a*a ) / ( b*c ) ) ;
    end ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      SinCos( lp_phi, sinphi, cosphi ) ;

      for i := 0 to 2 do begin // dist/azimiths from control
        vect( lp_phi - Q_c[i].phi, Q_c[i].cosphi, Q_c[i].sinphi,
              cosphi, sinphi, lp_lam - Q_c[i].lam);
        v[i].r  := Q_rv_r ;
        v[i].az := Q_rv_az ;
        if v[i].r = 0 then
          break;
        v[i].az := utAdjlon(v[i].az - Q_c[i].v_az);
      end ;
      if i < 3 then begin// current point at control point
        xy_x := Q_c[i].p_x ;
        xy_y := Q_c[i].p_y ;
      end
      else begin  // point mean of intersepts
        xy_x := Q_p_x ;
        xy_y := Q_p_y ;

        for i := 0 to 2 do begin
          if i = 2 then j := 0
                   else j := i + 1 ;

          a := lc( Q_c[i].v_r, v[i].r, v[j].r ) ;

          if v[i].az < 0 then a := -a ;

          if      i = 0 then begin // coord comp unique to each arc
                          xy_x := xy_x + v[i].r * Cos( a ) ;
                          xy_y := xy_y - v[i].r * Sin( a ) ;
                        end
          else if i = 1 then begin
                          a := Q_beta_1 - a ;
                          xy_x := xy_x - v[i].r * Cos( a ) ;
                          xy_y := xy_y - v[i].r * Sin( a ) ;
                        end
          else          begin
                          a := Q_beta_2 - a;
                          xy_x := xy_x + v[i].r * Cos( a ) ;
                          xy_y := xy_y + v[i].r * Sin( a ) ;
                        end ;
        end ;
        xy_x := xy_x * THIRD ; // mean of arc intercepts
        xy_y := xy_y * THIRD ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_ProjectionLaborde
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLabordeMadagaskar.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Azimuth,
                        TGIS_CSProjParameter.XYPlaneRotation
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FDescription   := '' ;

    FParameters.LatitudeOfOrigin := DegToRad( -18.9 ) ;       // S18°54'
    FParameters.CentralMeridian  := DegToRad( 46.43722917 ) ; // E46°26'14"
    FParameters.Azimuth          := DegToRad( 18.9 ) ;        //  18°54'

    datumChanged ;
  end ;

  function TGIS_CSProjLabordeMadagaskar.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLabordeMadagaskar.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLabordeMadagaskar.datumChanged ;
  var
    az, sinp, r, n, t : Double;
  begin
    inherited datumChanged ;

    P_y0   := FParameters.ScaleFactor ;
    az     := FParameters.Azimuth     ;

    Q_rot  := 1 ;

    sinp   := Sin( P_phi0 ) ;
    t      := 1 - P_es * sinp * sinp ;
    n      := 1 / Sqrt(t) ;
    r      := P_one_es * n / t ;
    Q_kRg  := P_k0 * Sqrt( n * r ) ;
    Q_p0s  := ArcTan( Sqrt( r/n ) * Tan( P_phi0 ) ) ;
    Q_A    := sinp / Sin( Q_p0s ) ;
    t      := P_e * sinp ;
    Q_C    := 0.5 * P_e * Q_A * Ln( (1 + t) / (1 - t) ) +
              - Q_A * Ln( Tan( FORTPI + 0.5*P_phi0 ) )
              + Ln( Tan( FORTPI + 0.5*Q_p0s ) ) ;
    t      := az + az;
    Q_Cb   := 1 / ( 12 * Q_kRg*Q_kRg ) ;
    Q_Ca   := ( 1 - Cos(t) ) * Q_Cb ;
    Q_Cb   := Q_Cb * Sin( t ) ;
    Q_Cc   := 3 * ( Q_Ca*Q_Ca - Q_Cb*Q_Cb ) ;
    Q_Cd   := 6 * Q_Ca*Q_Cb ;

    set_Validity( FParameters.CentralMeridian , ZONE_10,
                  FParameters.LatitudeOfOrigin, ZONE_10
                ) ;
  end ;

  procedure TGIS_CSProjLabordeMadagaskar.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  var
    v1, v2, ps                    : Double ;
    sinps, cosps, sinps2, cosps2  : Double ;
    i1, i2, i3, i4, i5, i6        : Double ;
    x2, y2, t                     : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      v1     := Q_A * Ln( Tan( FORTPI + 0.5 * lp_phi ) ) ;
      t      := P_e * Sin( lp_phi ) ;
      v2     := 0.5 * P_e * Q_A * Ln ( (1 + t) / (1 - t) ) ;
      ps     := 2 * ( ArcTan( Exp( v1 - v2 + Q_C ) ) - FORTPI ) ;
      i1     := ps - Q_p0s ;
      cosps  := Cos( ps ) ;
      cosps2 := cosps*cosps ;
      sinps  := Sin( ps ) ;
      sinps2 := sinps*sinps ;
      i4     := Q_A * cosps ;
      i2     := 0.5 * Q_A * i4 * sinps ;
      i3     := i2 * Q_A*Q_A * ( 5 * cosps2 - sinps2 ) / 12 ;
      i6     := i4 * Q_A*Q_A ;
      i5     := i6 * ( cosps2 - sinps2 ) / 6 ;
      i6     := i6 * Q_A*Q_A *
                ( 5 * cosps2*cosps2 + sinps2 * ( sinps2 - 18*cosps2 ) ) / 120 ;
      t      := lp_lam*lp_lam ;
      xy_x   := Q_kRg * lp_lam * ( i4 + t * ( i5 + t * i6 ) ) ;
      xy_y   := Q_kRg * ( i1 + t * ( i2 + t * i3 ) ) ;
      x2     := xy_x * xy_x;
      y2     := xy_y * xy_y;
      v1     := 3 * xy_x * y2  -  xy_x * x2 ;
      v2     := xy_y * y2  -  3 * x2 * xy_y ;
      xy_x   := xy_x + Q_Ca * v1 + Q_Cb * v2 ;
      xy_y   := xy_y + Q_Ca * v2 - Q_Cb * v1 ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjLabordeMadagaskar.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 20 ;
  var
    x2, y2, v1, v2, v3, v4      : Double  ;
    t, t2, ps, pe, tpe, s       : Double  ;
    i7, i8, i9, i10, i11, d, re : Double  ;
    i                           : Integer ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      x2   := xy_x * xy_x;
      y2   := xy_y * xy_y;
      v1   := 3 * xy_x * y2 - xy_x * x2 ;
      v2   := xy_y * y2 - 3 * x2 * xy_y ;
      v3   := xy_x * ( 5 * y2 * y2 + x2 * (-10 * y2 + x2 ) );
      v4   := xy_y * ( 5 * x2 * x2 + y2 * (-10 * x2 + y2 ) );
      xy_x := xy_x - Q_Ca * v1 - Q_Cb * v2 + Q_Cc * v3 + Q_Cd * v4 ;
      xy_y := xy_y + Q_Cb * v1 - Q_Ca * v2 - Q_Cd * v3 + Q_Cc * v4 ;
      ps   := Q_p0s  +  xy_y / Q_kRg ;
      pe   := ps + P_phi0 - Q_p0s ;

      for i := N_ITER downto 0 do begin
        v1  := Q_A * Ln( Tan( FORTPI + 0.5 * pe ) ) ;
        tpe := P_e * Sin(pe) ;
        v2  := 0.5 * P_e * Q_A * Ln( (1 + tpe) / (1 - tpe) ) ;
        t   := ps - 2 * ( ArcTan( Exp( v1 - v2 + Q_C ) ) - FORTPI );
        pe  := pe + t ;
        if Abs(t) < EPS7 then
          break ;
      end ;

      t   := P_e * Sin( pe ) ;
      t   := 1 - t * t;
      re  := P_one_es / ( t * Sqrt(t) );
      t   := Tan( ps ) ;
      t2  := t * t ;
      s   := Q_kRg * Q_kRg ;
      d   := re * P_k0 * Q_kRg ;
      i7  := t / ( 2*d ) ;
      i8  := t * ( 5 + 3*t2) / ( 24 * d * s );
      d   := Cos( ps ) * Q_kRg * Q_A ;
      i9  := 1 / d ;
      d   := d * s ;
      i10 := ( 1 + 2 * t2 ) / ( 6 * d ) ;
      i11 := ( 5 + t2 * (28 + 24 * t2) ) / ( 120 * d * s ) ;
      x2  := xy_x * xy_x ;

      lp_phi := pe + x2 * ( -i7 + i8 * x2 ) ;
      lp_lam := xy_x * ( i9 + x2 * ( -i10 + x2 * i11 ) ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_ProjectionLandsat
//------------------------------------------------------------------------------

  constructor TGIS_CSProjLandsat.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.CylindricalOblique ;
    FParametersSet := [
                        TGIS_CSProjParameter.CentralMeridian,
                        TGIS_CSProjParameter.LatitudeOfOrigin,
                        TGIS_CSProjParameter.FalseEasting,
                        TGIS_CSProjParameter.FalseNorthing,
                        TGIS_CSProjParameter.Custom1,
                        TGIS_CSProjParameter.Custom2
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True ;
    FIsSpheroid    := False  ;
    FDescription   := 'Space oblique for LANDSAT' ;

    FParameters.Custom1Name := 'land' ;
    FParameters.Custom2Name := 'path' ;

    FParameters.Custom1 := 1   ;
    FParameters.Custom2 := 150 ;

    datumChanged ;
  end ;

  function TGIS_CSProjLandsat.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjLandsat.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjLandsat.datumChanged ;
  const
    DEG_TO_RAD  = 0.0174532925199432958 ;
  var
    land, path, l      : Integer ;
    lam, alf, esc, ess : Double  ;

    procedure seraz0(lam, mult : Double) ;
    var
      sdsq, h, s, fc : Double ;
      sd, sq, d__1   : Double ;
    begin

      lam  := lam * DEG_TO_RAD ;
      sd   := Sin( lam ) ;
      sdsq := sd * sd ;

      s := Q_p22 * Q_sa * Cos( lam ) * Sqrt( (1 + Q_t * sdsq ) /
           ( ( 1 + Q_w * sdsq ) * ( 1 + Q_q * sdsq ) ) ) ;

      d__1 := 1 + Q_q * sdsq ;

      h := Sqrt( ( 1 + Q_q * sdsq ) / ( 1 + Q_w * sdsq ) ) *
           ( (1 + Q_w * sdsq ) / ( d__1 * d__1 ) - Q_p22 * Q_ca ) ;

      sq := Sqrt( Q_xj * Q_xj + s*s ) ;
      fc := mult * ( h * Q_xj - s*s ) / sq ;
      Q_b  := Q_b  + fc ;
      Q_a2 := Q_a2 + fc * Cos( lam + lam ) ;
      Q_a4 := Q_a4 + fc * Cos( lam * 4   ) ;
      fc   := mult * s * ( h + Q_xj ) / sq ;
      Q_c1 := Q_c1 + fc * Cos( lam    ) ;
      Q_c3 := Q_c3 + fc * Cos( lam * 3) ;
    end ;

  begin
    inherited datumChanged ;

    land := RoundS( FParameters.Custom1 ) ;
    path := RoundS( FParameters.Custom2 ) ;

    if ( land <= 0 ) or ( land > 5 ) then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    if land <= 3 then l := 251
                 else l := 233 ;

    if ( path <= 0 ) or ( path > l ) then begin
      FError := ERROR_WRONGPARAMS ;
      bDatumReady := False ;
      exit ;
    end ;

    if land <= 3 then begin
      P_lam0 := DEG_TO_RAD * 128.87 - TWOPI / 251 * path ;
      Q_p22  := 103.2669323 ;
      alf   := DEG_TO_RAD * 99.092;
    end
    else begin
      P_lam0 := DEG_TO_RAD * 129.3 - TWOPI / 233 * path ;
      Q_p22  := 98.8841202 ;
      alf    := DEG_TO_RAD * 98.2 ;
    end ;

    Q_p22 := Q_p22 / 1440 ;
    SinCos( alf, Q_sa, Q_ca ) ;
    if Abs( Q_ca ) < EPS9 then Q_ca := EPS9 ;

    esc := P_es * Q_ca * Q_ca ;
    ess := P_es * Q_sa * Q_sa ;

    Q_w := (1 - esc) * P_rone_es ;
    Q_w := Q_w * Q_w - 1 ;
    Q_q := ess * P_rone_es ;
    Q_t := ess * (2 - P_es) * P_rone_es * P_rone_es ;
    Q_u := esc * P_rone_es ;

    Q_xj   := P_one_es * P_one_es * P_one_es ;
    Q_rlm  := Pi * ( 1.0/248 + 0.5161290322580645 ) ;
    Q_rlm2 := Q_rlm + TWOPI ;

    Q_c3 := 0 ;
    Q_c1 := Q_c3 ;
    Q_b  := Q_c3 ;
    Q_a4 := Q_c3 ;
    Q_a2 := Q_c3 ;

    seraz0( 0, 1 ) ;
    lam := 9.0 ;
    repeat
      seraz0( lam, 4.0 ) ;
      lam := lam + 18 ;
    until lam > 81.0001 ;

    lam := 18 ;
    repeat
      seraz0( lam, 2.0 ) ;
      lam := lam + 18 ;
    until lam > 72.0001 ;

    seraz0(90, 1) ;
    Q_a2 := Q_a2 / 30 ;
    Q_a4 := Q_a4 / 60 ;
    Q_b  := Q_b  / 30 ;
    Q_c1 := Q_c1 / 15 ;
    Q_c3 := Q_c3 / 45 ;

    // verify validity setup
    set_Validity( FParameters.CentralMeridian, ZONE_180,
                  0                          , ZONE_90
                ) ;
  end ;

  procedure TGIS_CSProjLandsat.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER       = 50 ;
    PI_HALFPI    =  4.71238898038468985766 ;
    TWOPI_HALFPI =  7.85398163397448309610 ;
  var
    l, nn : Integer ;
    lamt, xlam, sdsq, c, d  : Double ;
    s, lamdp, phidp, lampp  : Double ;
    tanph, lamtp, cl, sd    : Double ;
    sp, fac, sav, tanphi    : Double ;
    tm, inf_loop            : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    projectPre( _coords, lp_lam, lp_phi ) ;
    if not bDatumReady then begin
      projectPost( _coords, GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
      exit ;
    end;

      if      lp_phi >  HALFPI then lp_phi :=  HALFPI
      else if lp_phi < -HALFPI then lp_phi := -HALFPI ;

      if lp_phi >= 0 then lampp := HALFPI
                     else lampp := PI_HALFPI ;

      tanphi   := Tan( lp_phi ) ;
      nn       := 0 ;
      inf_loop := 0 ;
      repeat

        sav   := lampp ;
        lamtp := lp_lam + Q_p22 * lampp ;
        cl := Cos( lamtp ) ;

        if cl < 0 then tm := -HALFPI
                  else tm :=  HALFPI ;

        fac := lampp - Sin( lampp ) * tm ;
        for l := N_ITER downto 0 do begin
          lamt := lp_lam + Q_p22 * sav ;
          c    := Cos(lamt) ;
          if Abs( c ) < EPS7 then lamt := lamt - EPS7 ;

          xlam  := ( P_one_es * tanphi * Q_sa + Sin( lamt ) * Q_ca ) / c ;
          lamdp := ArcTan( xlam ) + fac ;
          if Abs( Abs( sav ) - Abs( lamdp ) ) < EPS7 then
            break ;
          sav := lamdp ;
        end ;

        inc( nn ) ;
        if ( l = 0 ) or ( nn >= 3 ) or
           ( ( lamdp > Q_rlm ) and ( lamdp < Q_rlm2 ) )
        then
          break;

        if      lamdp <= Q_rlm  then lampp := TWOPI_HALFPI
        else if lamdp >= Q_rlm2 then lampp := HALFPI ;

      until inf_loop > 0 ;

      if l <> 0 then begin
        sp := Sin(lp_phi);
        phidp := utAasin( ( P_one_es * Q_ca * sp - Q_sa * Cos( lp_phi ) *
                            Sin( lamt )
                          ) / Sqrt( 1 - P_es * sp * sp ) ) ;
        tanph := Ln( Tan( FORTPI + 0.5*phidp ) ) ;
        sd    := Sin( lamdp ) ;
        sdsq  := sd*sd ;
        s := Q_p22 * Q_sa * Cos( lamdp ) *
             Sqrt( ( 1 + Q_t * sdsq ) /
                   ( (1 + Q_w * sdsq ) * ( 1 + Q_q * sdsq ) )
                 ) ;

        d := Sqrt( Q_xj * Q_xj  +  s * s ) ;
        xy_x := Q_b * lamdp  +  Q_a2 * Sin( 2 * lamdp )  +
                Q_a4 * Sin( lamdp * 4 )  -  tanph * s / d ;
        xy_y := Q_c1 * sd  +  Q_c3 * Sin( lamdp * 3 )  +  tanph * Q_xj / d ;
      end
      else begin
        xy_x := GIS_MAX_DOUBLE ;
        xy_y := GIS_MAX_DOUBLE ;
      end ;

    projectPost( _coords, xy_x, xy_y ) ;
  end ;

  procedure TGIS_CSProjLandsat.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  const
    N_ITER = 50 ;
  var
    nn                    : Integer ;
    lamt, sdsq, s         : Double ;
    lamdp, phidp, sppsq   : Double ;
    dd, sd, sl, fac, scl  : Double ;
    sav, spp              : Double ;

    lp_lam : Double ;
    lp_phi : Double ;
    xy_x   : Double ;
    xy_y   : Double ;
  begin
    unprojectPre( _coords, xy_x, xy_y ) ;

      lamdp := xy_x / Q_b ;
      nn := N_ITER ;
      repeat
        sav   := lamdp ;
        sd    := Sin( lamdp ) ;
        sdsq  := sd * sd ;
        s     := Q_p22 * Q_sa * Cos( lamdp ) *
                 Sqrt( (1 + Q_t * sdsq) /
                       ( (1 + Q_w * sdsq) * (1 + Q_q * sdsq ) )
                     ) ;
        lamdp := xy_x + xy_y * s / Q_xj  -
                 Q_a2 * Sin( 2 * lamdp )  -  Q_a4 * Sin( lamdp * 4 )  -
                 s / Q_xj * ( Q_c1 * Sin( lamdp ) + Q_c3 * Sin( lamdp * 3 ) ) ;
        lamdp := lamdp / Q_b;
        if nn = 0 then break ;
      until Abs( lamdp - sav ) < EPS7 ;

      sl    := Sin( lamdp ) ;
      fac   := Exp( Sqrt( 1 + s * s / Q_xj / Q_xj ) *
                  ( xy_y - Q_c1 * sl - Q_c3 * Sin( lamdp * 3 ) )
                ) ;
      phidp := 2 * ( ArcTan( fac ) - FORTPI ) ;
      dd := sl * sl ;

      if Abs( Cos( lamdp ) ) < EPS7 then lamdp := lamdp - EPS7 ;

      spp   := Sin( phidp ) ;
      sppsq := spp * spp ;
      lamt  := ArcTan( ( 1 - sppsq * P_rone_es ) * Tan( lamdp ) * Q_ca  -
                       spp * Q_sa * Sqrt( ( 1 + Q_q * dd ) *
                                          ( 1 - sppsq) - sppsq * Q_u) /
                                          Cos( lamdp )
                                         ) /
                       ( 1 - sppsq * ( 1 + Q_u )
                     );

      if lamt       >= 9 then sl  :=  1
                         else sl  := -1 ;
      if Cos(lamdp) >= 9 then scl := 1
                         else scl := -1 ;

      lamt   := lamt - ( HALFPI * ( 1 - scl ) * sl ) ;
      lp_lam := lamt - Q_p22 * lamdp ;
      if Abs( Q_sa ) < EPS7 then
        lp_phi := utAasin( spp / Sqrt( P_one_es * P_one_es + P_es * sppsq ) )
      else
        lp_phi := ArcTan( ( Tan( lamdp ) * Cos( lamt ) - Q_ca * Sin( lamt ) ) /
                          ( P_one_es * Q_sa )
                        ) ;

    unprojectPost( _coords, lp_lam, lp_phi ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_ProjectionMGRS
//------------------------------------------------------------------------------

  constructor TGIS_CSProjMGRS.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;
    Q_utm := TGIS_CSProjUniversalTransverseMercator.Create( 0, '' ) ;
    Q_ups := TGIS_CSProjUniversalPolarStereographic.Create( 0, '' ) ;

    FSubtype       := TGIS_CSProjType.Grid ;
    FParametersSet := [
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FIsGrid        := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  procedure TGIS_CSProjMGRS.doDestroy ;
  begin
    FreeObject( Q_utm ) ;
    FreeObject( Q_ups ) ;

    inherited ;
  end ;

  function TGIS_CSProjMGRS.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjMGRS.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjMGRS.datumChanged ;
  begin
    inherited datumChanged ;

    bUseValidity := False ;

    // grid projection - no bounding polygon
  end ;

  function TGIS_CSProjMGRS.ProjectedTxt(
    const _coords : TGIS_Point
  ) :  String ;
  begin
    Result := ProjectedTxt( _coords, 5 ) ;
  end;

  function TGIS_CSProjMGRS.ProjectedTxt(
    const _coords : TGIS_Point ;
    const _prec   : Integer
  ) :  String ;
  var
    r           : TGIS_Point3D  {$IFDEF GIS_NORECORDS} = new TGIS_Point3D {$ENDIF};
    zone        : Integer      ;
    zoneex      : Integer      ;
    prec        : Integer      ;
    tmp1        : String       ;
    tmp2        : String       ;
    itmp        : Integer      ;
    base_char   : Integer      ;
    false_east  : Double       ;
    false_north : Double       ;
    ord_east    : Integer      ;
    ord_north   : Integer      ;

    // Adjust number to proper grid letter.
    //  _ord  number to adjusted
    //  _off  offset form origin
    //  _off  loop   size of cyclical buffer (after last char start from
    //               first); 0 means a whole MGRS alphabet
    //
    function adjust_grid(
      const _ord  : Integer ;
      const _off  : Integer ;
      const _loop : Integer
    ) : String ;
    var
      off : Integer ;
    begin
      off := _ord + _off ;
      if _loop > 0  then begin
        off := off mod _loop ;
        if off = 0 then
          off := _loop
      end
      else
        off := off mod (ord(T_MGRS.mgrsZ) + 1) ;

      case off of
        T_MGRS.mgrsA : Result := 'A' ;
        T_MGRS.mgrsB : Result := 'B' ;
        T_MGRS.mgrsC : Result := 'C' ;
        T_MGRS.mgrsD : Result := 'D' ;
        T_MGRS.mgrsE : Result := 'E' ;
        T_MGRS.mgrsF : Result := 'F' ;
        T_MGRS.mgrsG : Result := 'G' ;
        T_MGRS.mgrsH : Result := 'H' ;
        T_MGRS.mgrsJ : Result := 'J' ;
        T_MGRS.mgrsK : Result := 'K' ;
        T_MGRS.mgrsL : Result := 'L' ;
        T_MGRS.mgrsM : Result := 'M' ;
        T_MGRS.mgrsN : Result := 'N' ;
        T_MGRS.mgrsP : Result := 'P' ;
        T_MGRS.mgrsQ : Result := 'Q' ;
        T_MGRS.mgrsR : Result := 'R' ;
        T_MGRS.mgrsS : Result := 'S' ;
        T_MGRS.mgrsT : Result := 'T' ;
        T_MGRS.mgrsU : Result := 'U' ;
        T_MGRS.mgrsV : Result := 'V' ;
        T_MGRS.mgrsW : Result := 'W' ;
        T_MGRS.mgrsX : Result := 'X' ;
        T_MGRS.mgrsY : Result := 'Y' ;
        T_MGRS.mgrsZ : Result := 'Z' ;
        else           begin
                         Result := 'A' ;
                         Abort ;
                       end ;
      end ;
    end ;
  begin
    prec := _prec ;
    if prec < 0 then prec := 0 ;
    if prec > 5 then prec := 5 ;

    if ( _coords.Y >= -80 * Pi/180 ) and (_coords.Y <= 84 * Pi / 180 ) then begin
      //UTM

      zone := FloorS( ( utAdjlon( _coords.X ) + Pi ) * 30 / Pi ) + 1 ;
      if      zone <  1 then zone :=  1
      else if zone > 60 then zone := 60 ;

      if _coords.Y < 0 then zone := -zone ;

      zoneex := TruncS( ( RadToDeg( _coords.Y ) + 80 ) / 8.0 ) ;

      Q_utm.FParameters.Zone := zone ;
      Q_utm.datumChanged ;

      r := GisPoint3DFrom2D( _coords ) ;
      Q_utm.bUseValidity := False ;
      Q_utm.Project3D_Ref( r ) ;
      Q_utm.bUseValidity := True ;

      Result := Format( '%.2d%s',
                        [ Abs( zone ), adjust_grid( zoneex, T_MGRS.mgrsC, 0 ) ]
                      ) ;

      tmp1   := Format( '%.6d', [ TruncS( r.X ) ] ) ;
      itmp   := StrToInt( tmp1[StringFirst] ) ;
      Result := Result +
                adjust_grid( itmp, ( (Abs(zone)-1) mod 3 ) * 8, 0 ) ;

      tmp2   := Format( '%.7d',
                        [ TruncS( r.Y ) - ( TruncS( r.Y / 2000000 ) * 2000000 ) ]
                      ) ;
      itmp   := StrToInt( Copy( tmp2, StringFirst, 2 ) ) ;
      Result := Result +
                adjust_grid( itmp, (Abs(zone)-1) mod 2 * 5 + 1, 20 ) ;

      Result := Result + Copy( tmp1, StringFirst+1, prec ) +
                         Copy( tmp2, StringFirst+2, prec ) ;

    end
    else begin
      //UPS

      if _coords.Y > 0 then begin
        if _coords.X > 0 then begin
          // north eastern
          base_char   := T_MGRS.mgrsA ;
          false_east  := 2000000 ;
          false_north := 1300000 ;
          zone        := 1       ;
          Result      := 'Z' ;
        end
        else begin
          // north west
          base_char   := T_MGRS.mgrsJ ;
          false_east  := 800000  ;
          false_north := 1300000 ;
          zone        := 1       ;
          Result      := 'Y'     ;
        end ;
      end
      else begin
        if _coords.X > 0 then begin
          // south east
          base_char   := T_MGRS.mgrsA ;
          false_east  := 2000000 ;
          false_north := 800000  ;
          zone        := -1      ;
          Result      := 'B'     ;
        end
        else begin
          // south west
          base_char   := T_MGRS.mgrsJ ;
          false_east  := 800000  ;
          false_north := 800000  ;
          zone        := -1      ;
          Result      := 'A'     ;
        end ;
      end ;

      Q_ups.FParameters.LatitudeOfOrigin := zone  ;
      Q_ups.datumChanged ;
      r := GisPoint3DFrom2D( _coords ) ;
      Q_ups.bUseValidity := False ;
      Q_ups.Project3D_Ref( r ) ;
      Q_ups.bUseValidity := True ;

      if r.X >= 2000000 then begin
        if zone > 0 then Result := 'Z'
                    else Result := 'B' ;
      end ;

      ord_north := 1 + TruncS( ( RoundS( r.Y ) - false_north ) / 100000 ) ;
      ord_east  := base_char + TruncS( ( RoundS( r.X ) - false_east ) / 100000 ) ;
      if r.X < 2000000 then begin
        if ord_east > ord(T_MGRS.mgrsL) then
          ord_east := ord_east + 2 ;
        if ord_east > ord(T_MGRS.mgrsU) then
          ord_east := ord_east + 2 ;
      end
      else begin
        if ord_east > ord(T_MGRS.mgrsC) then
          ord_east := ord_east + 2 ;
        if ord_east > ord(T_MGRS.mgrsL) then
          ord_east := ord_east + 2 ;
      end ;

      Result := Result +
                adjust_grid( ord_east, 0, 24 ) +
                adjust_grid( ord_north, 0, 24 ) ;
      tmp1   := Format( '%.7d', [ RoundS( r.X ) ] ) ;
      tmp2   := Format( '%.7d',
                        [ RoundS( r.Y ) - ( TruncS( r.Y / 2000000 ) * 2000000 ) ]
                      ) ;
      Result := Result + Copy( tmp1, StringFirst+2, prec ) +
                         Copy( tmp2, StringFirst+2, prec ) ;

    end ;

  end ;

  function TGIS_CSProjMGRS.UnprojectedTxt(
    const _coords : String
  ) : TGIS_Point ;
  var
    i           : Integer      ;
    c           : Char         ;
    len         : Integer      ;
    tmp         : String       ;

    r           : TGIS_Point3D  {$IFDEF GIS_NORECORDS} = new TGIS_Point3D {$ENDIF};
    zone        : Integer      ;
    zoneex      : Integer      ;
    prec        : Integer      ;
    base_char   : Integer      ;
    false_east  : Double       ;
    false_north : Double       ;
    ord_char    : Integer      ;
    grid        : String       ;
    ylow        : Integer      ;
    yslow       : Integer      ;
    yltr        : Integer      ;
    st_digit    : TCharSet     ;
    st_char     : TCharSet     ;

    function adjust_grid(
      const _c    : Char    ;
      const _off  : Integer ;
      const _loop : Integer
    ) : Integer ;
    var
      c1 : Integer ;
    begin
      case _c of
        'A' : c1 := T_MGRS.mgrsA ;
        'B' : c1 := T_MGRS.mgrsB ;
        'C' : c1 := T_MGRS.mgrsC ;
        'D' : c1 := T_MGRS.mgrsD ;
        'E' : c1 := T_MGRS.mgrsE ;
        'F' : c1 := T_MGRS.mgrsF ;
        'G' : c1 := T_MGRS.mgrsG ;
        'H' : c1 := T_MGRS.mgrsH ;
        'J' : c1 := T_MGRS.mgrsJ ;
        'K' : c1 := T_MGRS.mgrsK ;
        'L' : c1 := T_MGRS.mgrsL ;
        'M' : c1 := T_MGRS.mgrsM ;
        'N' : c1 := T_MGRS.mgrsN ;
        'P' : c1 := T_MGRS.mgrsP ;
        'Q' : c1 := T_MGRS.mgrsQ ;
        'R' : c1 := T_MGRS.mgrsR ;
        'S' : c1 := T_MGRS.mgrsS ;
        'T' : c1 := T_MGRS.mgrsT ;
        'U' : c1 := T_MGRS.mgrsU ;
        'V' : c1 := T_MGRS.mgrsV ;
        'W' : c1 := T_MGRS.mgrsW ;
        'X' : c1 := T_MGRS.mgrsX ;
        'Y' : c1 := T_MGRS.mgrsY ;
        'Z' : c1 := T_MGRS.mgrsZ ;
        else  begin
                c1 := T_MGRS.mgrsA ;
                Abort ;
              end ;
      end ;

      if _loop > 0 then
      begin
        if ( c1 - _off ) < 0 then
          Result := ( ( c1 - _off ) + _loop ) mod _loop
      else
          Result := ( c1 - _off ) mod _loop
      end
      else
        Result := ( c1 - _off ) mod ( ord(T_MGRS.mgrsZ) + 1 ) ;
    end ;
  begin
    try
      st_digit := PrepareCharSet( [ '09' ] ) ;
      st_char  := PrepareCharSet( [ 'az', 'AZ' ] ) ;

      i := StringFirst ;

      // read zone
        tmp := '' ;
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if      InCharSet( c, st_digit )
                  then tmp := tmp + c
          else if c <> ' '
                  then break ;
          inc( i ) ;
        end ;
        if IsStringEmpty( tmp ) then tmp := '0' ;
        zone := StrToInt( tmp ) ;

      // read zone offset
        zoneex := 0 ;
        if i <= StringLast( _coords ) then begin
          c := _coords[i] ;
          if zone <> 0 then // UTM
            zoneex := adjust_grid( UpCase(c), T_MGRS.mgrsC, 0 )
          else
            zoneex := adjust_grid( UpCase(c), 0, 0 ) ;
          inc( i ) ;
        end
        else Abort ;

      // read grid
        grid := '' ;
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if      InCharSet( c, st_char )
                  then grid := grid + UpCase( c )
          else if c <> ' '
                  then break ;
          inc( i ) ;
        end ;
        if length( grid ) <> 2 then Abort ;

      // read offset
        tmp := '' ;
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if      InCharSet( c, st_digit )
                  then tmp := tmp + c
          else if c <> ' '
                  then break ;
          inc( i ) ;
        end ;
        len := length( tmp ) ;
        if length( tmp ) mod 2 <> 0 then Abort ;
        len := len div 2 ;

      // read trailing
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if c <> ' ' then Abort ;
          inc( i ) ;
        end ;

      case len of
        0 : prec := 1 ;
        1 : prec := 10000 ;
        2 : prec := 1000 ;
        3 : prec := 100 ;
        4 : prec := 10 ;
        5 : prec := 1 ;
        else begin
               prec := 0 ;
               Abort ;
             end ;
      end ;

      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point;
      {$ENDIF}

      if len > 0 then begin
        Result.X := StrToInt( Copy( tmp, StringFirst    , len ) ) * prec ;
        Result.Y := StrToInt( Copy( tmp, StringFirst+len, len ) ) * prec ;
      end
      else begin
        Result.X := 0 ;
        Result.Y := 0 ;
      end ;

      if zone <> 0 then begin
        // UTM

        Result.X := Result.X +
                    adjust_grid( grid[StringFirst],
                                 ( (zone-1) mod 3 ) * 8,
                                 0
                               )
                    * 100000 ;
        Result.Y := Result.Y +
                    adjust_grid( grid[StringFirst+1],
                                 ( (zone-1) mod 2 * 5 + 1 ),
                                 20
                               )
                    * 100000 ;
        if zoneex < ( ord(T_MGRS.mgrsN) - ord(T_MGRS.mgrsC) ) then zone := -zone ;

        r.X := DegToRad( -180.0 + (Abs(zone) -1 )* 6 ) ;
        r.Y := DegToRad( - 80.0 + zoneex * 8  ) ;
        r.Z := 0 ;
        r.M := 0 ;

        Q_utm.FParameters.Zone := zone ;
        Q_utm.datumChanged ;
        Q_utm.bUseValidity := False ;
        Q_utm.Project3D_Ref( r ) ;
        Q_utm.bUseValidity := True ;

        ylow := TruncS( r.Y / 100000 ) * 100000 ;
        yslow := ylow ;
        while yslow >= 2000000 do
          yslow := yslow - 2000000 ;
        yltr := RoundS( Result.Y ) ;

        if yltr >= 2000000 then yltr := yltr - 2000000 ;
        yltr := yltr - yslow ;
        if yltr < 0 then yltr := yltr + 2000000 ;
        r.X := Result.X ;
        r.Y := ylow + yltr ;

        Q_utm.Unproject3D_Ref( r ) ;
        Q_utm.bUseValidity := True ;

        Result.X := r.X ;
        Result.Y := r.Y ;

      end
      else begin
        // UPS
        case zoneex of
          T_MGRS.mgrsZ
                  : begin // north eastern
                    base_char   := T_MGRS.mgrsA ;
                    false_east  := 2000000 ;
                    false_north := 1300000 ;
                    zone        := 1       ;
                  end ;
          T_MGRS.mgrsY
                  : begin // north west
                   base_char   := T_MGRS.mgrsJ ;
                   false_east  := 800000  ;
                   false_north := 1300000 ;
                   zone        := 1       ;
                  end ;
          T_MGRS.mgrsB
                  : begin // south east
                    base_char   := T_MGRS.mgrsA ;
                    false_east  := 2000000 ;
                    false_north := 800000  ;
                    zone        := -1      ;
                  end ;
          T_MGRS.mgrsA
                  : begin // south west
                    base_char   := T_MGRS.mgrsJ ;
                    false_east  := 800000  ;
                    false_north := 800000  ;
                    zone        := -1      ;
                  end ;
          else    begin
                    base_char   := 0 ;
                    false_east  := 0 ;
                    false_north := 0 ;
                    zone        := 0 ;

                    Abort ;
                 end ;
      end ;

      Result.Y := Result.Y + false_north +
                  adjust_grid( grid[StringFirst+1], 1, 0 ) * 100000 ;
      ord_char := adjust_grid( grid[StringFirst  ], 0, 0 ) ;

      if base_char <> ord(T_MGRS.mgrsA) then begin
        if ord_char > ord(T_MGRS.mgrsL) then
          ord_char := ord_char - 2 ;
        if ord_char > ord(T_MGRS.mgrsU) then
          ord_char := ord_char - 2 ;
      end
      else begin
        if ord_char > ord(T_MGRS.mgrsC) then
          ord_char := ord_char - 2 ;
        if ord_char > ord(T_MGRS.mgrsL) then
          ord_char := ord_char - 2 ;
      end ;

      Result.X := Result.X + false_east +
                  ( ord_char - base_char ) * 100000  ;

      Q_ups.FParameters.LatitudeOfOrigin := zone ;
      Q_ups.datumChanged ;
      r := GisPoint3DFrom2D( Result ) ;
      Q_ups.bUseValidity := False ;
      Q_ups.Unproject3D_Ref( r ) ;
      Q_ups.bUseValidity := True ;

      Result := GisPoint2DFrom3D( r ) ;
    end ;

    except
      Result.X := GIS_MAX_DOUBLE ;
      Result.Y := GIS_MAX_DOUBLE ;
      FError := ERROR_UNSOLVABLE ;
    end
  end ;

//------------------------------------------------------------------------------
// TGIS_ProjectionF16GRS
//------------------------------------------------------------------------------

  constructor TGIS_CSProjF16GRS.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Grid ;
    FParametersSet := [
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FIsGrid        := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjF16GRS.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjF16GRS.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjF16GRS.datumChanged ;
  begin
    inherited datumChanged ;

    // grid projection - no bounding polygon
  end ;

  function TGIS_CSProjF16GRS.ProjectedTxt(
    const _coords : TGIS_Point
  ) : String ;
  begin
    Result := ProjectedTxt( _coords, 5 ) ;
  end;

  function TGIS_CSProjF16GRS.ProjectedTxt(
    const _coords : TGIS_Point ;
    const _prec   : Integer
  ) : String ;
  var
    prec : Integer ;
  begin

    prec := _prec ;
    if prec < 0 then prec := 0 ;
    if prec > 5 then prec := 5 ;

    Result := inherited ProjectedTxt( _coords, prec ) ;
    if prec = 0 then
      Result := Result + '00' ;
  end ;

//------------------------------------------------------------------------------
// TGIS_ProjectionGEOREF
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGEOREF.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Grid ;
    FParametersSet := [
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FIsGrid        := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjGEOREF.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGEOREF.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGEOREF.datumChanged ;
  begin
    inherited datumChanged ;

    // grid projection - no bounding polygon
  end ;

  function TGIS_CSProjGEOREF.ProjectedTxt(
    const _coords : TGIS_Point
  ) : String ;
  begin
    Result := ProjectedTxt( _coords, 5 ) ;
  end;

  function TGIS_CSProjGEOREF.ProjectedTxt(
    const _coords : TGIS_Point ;
    const _prec   : Integer
  ) : String ;
  var
    x           : Double  ;
    x1          : Integer ;
    x2          : Integer ;
    x3          : String  ;
    y           : Double  ;
    y1          : Integer ;
    y2          : Integer ;
    y3          : String  ;
    prec        : Integer ;

    // Adjust number to proper grid letter.
    //  _ord  number to adjusted
    //  _off  offset form origin
    //  _off  loop   size of cyclical buffer (after last char start from
    //               first); 0 means a whole MGRS alphabet
    //
    function adjust_grid(
      const _ord  : Integer
    ) : String ;
    begin
      case _ord + 1 of
        T_MGRS.mgrsA : Result := 'A' ;
        T_MGRS.mgrsB : Result := 'B' ;
        T_MGRS.mgrsC : Result := 'C' ;
        T_MGRS.mgrsD : Result := 'D' ;
        T_MGRS.mgrsE : Result := 'E' ;
        T_MGRS.mgrsF : Result := 'F' ;
        T_MGRS.mgrsG : Result := 'G' ;
        T_MGRS.mgrsH : Result := 'H' ;
        T_MGRS.mgrsJ : Result := 'J' ;
        T_MGRS.mgrsK : Result := 'K' ;
        T_MGRS.mgrsL : Result := 'L' ;
        T_MGRS.mgrsM : Result := 'M' ;
        T_MGRS.mgrsN : Result := 'N' ;
        T_MGRS.mgrsP : Result := 'P' ;
        T_MGRS.mgrsQ : Result := 'Q' ;
        T_MGRS.mgrsR : Result := 'R' ;
        T_MGRS.mgrsS : Result := 'S' ;
        T_MGRS.mgrsT : Result := 'T' ;
        T_MGRS.mgrsU : Result := 'U' ;
        T_MGRS.mgrsV : Result := 'V' ;
        T_MGRS.mgrsW : Result := 'W' ;
        T_MGRS.mgrsX : Result := 'X' ;
        T_MGRS.mgrsY : Result := 'Y' ;
        T_MGRS.mgrsZ : Result := 'Z' ;
        else    begin
                  Result := 'A' ;
                  Abort ;
                end ;
      end ;
    end ;

  begin
    prec := _prec ;
    if prec < 0 then prec := 0 ;
    if prec > 5 then prec := 5 ;

    try
      x := RadToDeg( utAdjlon( _coords.X ) ) + 180 ;
      y := RadToDeg( _coords.Y  ) + 90 ;

      x := TruncS( x * 100000.0 ) / 100000.0 ;
      y := TruncS( y * 100000.0 ) / 100000.0 ;

      if Abs( (y - 180) ) < 1e-7 then
        y := 180 - 0.001 / 60  ; // can't be 90N
      if Abs( (x - 360) ) < 1e-7 then
        x := 360 - 0.001 / 60  ; // can't be 180E

      if ( x < 0 ) or ( x > 360 ) then Abort ;
      if ( y < 0 ) or ( y > 360 ) then Abort ;

      x1 := TruncS( x / 15 ) ;
      x2 := TruncS( x - x1 * 15 ) ;
      x3 := Format( '%.5d',[ RoundS( Frac( x ) * 60 * 1000 ) ] ) ;

      y1 := TruncS( y / 15 ) ;
      y2 := TruncS( y - y1 * 15) ;
      y3 := Format( '%.5d',[ RoundS( Frac( y ) * 60 * 1000 ) ] ) ;

      Result := adjust_grid( x1 ) + adjust_grid( y1 ) +
                adjust_grid( x2 ) + adjust_grid( y2 ) +
                Copy( x3, StringFirst, prec ) +
                Copy( y3, StringFirst, prec ) ;
    except
      Result := '?' ;
      FError := ERROR_UNSOLVABLE ;
    end ;
  end ;

  function TGIS_CSProjGEOREF.UnprojectedTxt(
    const _coords : String
  ) : TGIS_Point ;
  var
    i        : Integer ;
    c        : Char    ;
    len      : Integer ;
    tmp      : String  ;

    prec     : Integer ;
    grid     : String  ;

    st_digit : TCharSet ;
    st_char  : TCharSet ;

    function adjust_grid(
      const _c : Char
    ) : Integer ;
    var
      c1 : Integer ;
    begin
      case _c of
        'A' : c1 := T_MGRS.mgrsA ;
        'B' : c1 := T_MGRS.mgrsB ;
        'C' : c1 := T_MGRS.mgrsC ;
        'D' : c1 := T_MGRS.mgrsD ;
        'E' : c1 := T_MGRS.mgrsE ;
        'F' : c1 := T_MGRS.mgrsF ;
        'G' : c1 := T_MGRS.mgrsG ;
        'H' : c1 := T_MGRS.mgrsH ;
        'J' : c1 := T_MGRS.mgrsJ ;
        'K' : c1 := T_MGRS.mgrsK ;
        'L' : c1 := T_MGRS.mgrsL ;
        'M' : c1 := T_MGRS.mgrsM ;
        'N' : c1 := T_MGRS.mgrsN ;
        'P' : c1 := T_MGRS.mgrsP ;
        'Q' : c1 := T_MGRS.mgrsQ ;
        'R' : c1 := T_MGRS.mgrsR ;
        'S' : c1 := T_MGRS.mgrsS ;
        'T' : c1 := T_MGRS.mgrsT ;
        'U' : c1 := T_MGRS.mgrsU ;
        'V' : c1 := T_MGRS.mgrsV ;
        'W' : c1 := T_MGRS.mgrsW ;
        'X' : c1 := T_MGRS.mgrsX ;
        'Y' : c1 := T_MGRS.mgrsY ;
        'Z' : c1 := T_MGRS.mgrsZ ;
        else  begin
                c1 := T_MGRS.mgrsA ;
                Abort ;
              end ;
      end ;

      Result := c1 -1 ;
    end ;
  begin
    try
      st_digit := PrepareCharSet( [ '09' ] ) ;
      st_char  := PrepareCharSet( [ 'az', 'AZ' ] ) ;

      i := StringFirst ;

      // read grid
        grid := '' ;
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if      InCharSet( c, st_char )
                  then grid := grid + UpCase( c )
          else if c <> ' '
                  then break ;
          inc( i ) ;
        end ;
        if length( grid ) <> 4 then Abort ;

      // read offset
        tmp := '' ;
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if      InCharSet(  c, st_digit )
                  then tmp := tmp + c
          else if c <> ' '
                  then break ;
          inc( i ) ;
        end ;
        len := length( tmp ) ;
        if length( tmp ) mod 2 <> 0 then Abort ;
        len := len div 2 ;

      // read trailing
        while i <= StringLast( _coords ) do begin
          c := _coords[ i ] ;
          if c <> ' ' then Abort ;
          inc( i ) ;
        end ;

      case len of
        0 : prec := 1 ;
        1 : prec := 10000 ;
        2 : prec := 1000 ;
        3 : prec := 100 ;
        4 : prec := 10 ;
        5 : prec := 1 ;
        else begin
               prec := 0 ;
               Abort ;
             end ;
      end ;

      {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point;
      {$ENDIF}

      if len > 0 then begin
        Result.X := DegToRad(
                      StrToInt( Copy( tmp, StringFirst    , len ) ) *
                      prec / 60.0 / 1000.0
                    ) ;
        Result.Y := DegToRad(
                      StrToInt( Copy( tmp, StringFirst+len, len ) ) *
                      prec / 60.0 / 1000.0
                    ) ;
      end
      else begin
        Result.X := 0 ;
        Result.Y := 0 ;
      end ;

     Result.X := Result.X +
                 DegToRad( adjust_grid( grid[StringFirst  ] ) * 15 - 180.0 ) +
                 DegToRad( adjust_grid( grid[StringFirst+2] ) ) ;
     Result.Y := Result.Y +
                 DegToRad( adjust_grid( grid[StringFirst+1] ) * 15 -  90.0 ) +
                 DegToRad( adjust_grid( grid[StringFirst+3] ) ) ;

    except
      Result.X := GIS_MAX_DOUBLE ;
      Result.Y := GIS_MAX_DOUBLE ;
      FError := ERROR_UNSOLVABLE ;
    end
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjGeocentric
//------------------------------------------------------------------------------

  constructor TGIS_CSProjGeocentric.Create(
    const _epsg : Integer ;
    const _wkt  : String
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FSubtype       := TGIS_CSProjType.Miscellaneous ;
    FParametersSet := [
                      ] ;

    FIsPureCylindrical := False ;
    FIsRevertible  := True  ;
    FIsEllipsoid   := True  ;
    FIsSpheroid    := True  ;
    FIsXYZ         := True  ;
    FDescription   := '' ;

    datumChanged ;
  end ;

  function TGIS_CSProjGeocentric.CreateCopy
    : TGIS_CSProjAbstract ;
  begin
    Result := TGIS_CSProjGeocentric.Create(
                self.EPSG, self.WKT
              ) ;
  end ;

  procedure TGIS_CSProjGeocentric.Project3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    _coords := Datum.Ellipsoid.ToGeocentric( _coords ) ;
  end;

  procedure TGIS_CSProjGeocentric.Unproject3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
  ) ;
  begin
    _coords := Datum.Ellipsoid.ToGeodetic( _coords ) ;
  end;

//------------------------------------------------------------------------------
// TGIS_CSProjList
//------------------------------------------------------------------------------

  constructor TGIS_CSProjList.Create ;
  begin
    inherited Create( cs_ProjList ) ;
    if not assigned( cs_ProjList ) then
      cs_ProjList := self ;
  end ;

  function TGIS_CSProjList.fget_Projection(
    _idx : Integer
  ) : TGIS_CSProjAbstract ;
  begin
    LockThread ;
    try
      Result := TGIS_CSProjAbstract( WKTObject[ _idx ] ) ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_CSProjList.Add(
    const _prj    : TGIS_CSProjAbstract ;
    const _master : Integer
  ) ;
  var
    o : TGIS_CSAbstract ;
  begin
    LockThread ;
    try
      assert( ByEPSG( _prj.EPSG ) = nil ) ;

      o := inherited Add( _prj ) ;

      if _master > 0  then
        Alias( o.EPSG, _master ) ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_CSProjList.ByEPSG(
    const _epsg : Integer
  ) : TGIS_CSProjAbstract ;
  begin
    LockThread ;
    try
      Result := TGIS_CSProjAbstract( inherited ByEPSG( _epsg ) ) ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_CSProjList.ByWKT(
    const _wkt : String
  ) : TGIS_CSProjAbstract ;
  begin
    LockThread ;
    try
      Result := TGIS_CSProjAbstract( inherited ByWKT( _wkt ) ) ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_CSProjList.Init ;
  begin
    LockThread ;
    try
      Clear ;

      Add( TGIS_CSProjGeocentric.Create
           ( CSPROJ_Geocentric,
             'Geocentric'
           ), 0
         ) ;
      Add( TGIS_CSProjAlbers.Create
           ( CSPROJ_Albers,
             'Albers'
           ), 0
         ) ;
      Add( TGIS_CSProjAlbers.Create
           ( CSPROJ_Albers_Conic_Equal_Area,
             'Albers_Conic_Equal_Area'
           ), CSPROJ_Albers
         ) ;
      Add( TGIS_CSProjLambert.Create
           ( CSPROJ_Lambert,
             'Lambert'
           ), 0
         ) ;
      Add( TGIS_CSProjAzimuthalEquidistant.Create
           ( CSPROJ_Azimuthal_Equidistance,
             'Azimuthal_Equidistance'
           ), 0
         ) ;
      Add( TGIS_CSProjAiry.Create
           ( CSPROJ_Airy,
             'Airy'
           ), 0
         ) ;
      Add( TGIS_CSProjAitof.Create
           ( CSPROJ_Aitof,
             'Aitof'
           ), 0
         ) ;
      Add( TGIS_CSProjWinkelTripel.Create
           ( CSPROJ_Winkel_Tripel,
             'Winkel_Tripel'
           ), 0
         ) ;
      Add( TGIS_CSProjAugust.Create
           ( CSPROJ_August_Epicycloidal,
             'August_Epicycloidal'
           ), 0
         ) ;
      Add( TGIS_CSProjBonne.Create
           ( CSPROJ_Bonne,
             'Bonne'
           ), 0
         ) ;
      Add( TGIS_CSProjBonneSouthOriented.Create
           ( CSPROJ_Bonne_South_Oriented,
             'Bonne_South_Oriented'
           ), 0
         ) ;
      Add( TGIS_CSProjWerner.Create
           ( CSPROJ_Werner,
             'Werner'
           ), 0
         ) ;
      Add( TGIS_CSProjCassini.Create
           ( CSPROJ_Cassini_Soldner,
             'Cassini_Soldner'
           ), 0
         ) ;
      Add( TGIS_CSProjCentralCylindrical.Create
           ( CSPROJ_Central_Cylindrical,
             'Central_Cylindrical'
           ), 0
         ) ;
      Add( TGIS_CSProjEqualAreaCylindrical.Create
           ( CSPROJ_Cylindrical_Equal_Area,
             'Cylindrical_Equal_Area'
           ), 0
         ) ;
      Add( TGIS_CSProjEqualAreaCylindrical.Create
           ( CSPROJ_Equal_Area_Cylindrical,
             'Equal_Area_Cylindrical'
           ), CSPROJ_Cylindrical_Equal_Area
         ) ;
      Add( TGIS_CSProjBehrmann.Create
           ( CSPROJ_Behrmann,
             'Behrmann'
           ), 0
         ) ;
      Add( TGIS_CSProjGallOrthographic.Create
           ( CSPROJ_Gall_Orthographic,
             'Gall_Orthographic'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_I.Create
           ( CSPROJ_Eckert_I,
             'Eckert_I'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_II.Create
           ( CSPROJ_Eckert_II,
             'Eckert_II'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_III.Create
           ( CSPROJ_Eckert_III,
             'Eckert_III'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P1.Create
           ( CSPROJ_Putnins_P1,
             'Putnins_P1'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_VI.Create
           ( CSPROJ_Wagner_VI,
             'Wagner_VI'
           ), 0
         ) ;
      Add( TGIS_CSProjKavraisky_VII.Create
           ( CSPROJ_Kavraisky_VII,
             'Kavraisky_VII'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_IV.Create
           ( CSPROJ_Eckert_IV,
             'Eckert_IV'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_V.Create
           ( CSPROJ_Eckert_V,
             'Eckert_V'
           ), 0
         ) ;
      Add( TGIS_CSProjPlateCarree.Create
           ( CSPROJ_Plate_Carree,
             'Plate_Carree'
           ), 0
         ) ;
      Add( TGIS_CSProjPlateCarree.Create
           ( CSPROJ_Equidistant_Cylindrical,
             'Equidistant_Cylindrical'
           ), CSPROJ_Plate_Carree // ?
         ) ;
      Add( TGIS_CSProjPlateCarree.Create
           ( CSPROJ_Equidistant_Cylindrical_Spherical,
             'Equidistant_Cylindrical_Spherical'
           ), CSPROJ_Plate_Carree // ?
         ) ;
      Add( TGIS_CSProjPlateCarree.Create
           ( CSPROJ_Equirectangular,
             'Equirectangular'
           ), CSPROJ_Plate_Carree // ?
         ) ;
      Add( TGIS_CSProjEquidistantConic.Create
           ( CSPROJ_Equidistant_Conic,
             'Equidistant_Conic'
           ), 0
         ) ;
      Add( TGIS_CSProjSinusoidal.Create
           ( CSPROJ_Sinusoidal,
             'Sinusoidal'
           ), 0
         ) ;
      Add( TGIS_CSProjEckert_VI.Create
           ( CSPROJ_Eckert_VI,
             'Eckert_VI'
           ), 0
         ) ;
      Add( TGIS_CSProjFlatPolarSinusoidal.Create
           ( CSPROJ_Flat_Polar_Sinusoidal,
             'Flat_Polar_Sinusoidals'
           ), 0
         ) ;
      Add( TGIS_CSProjGnomic.Create
           ( CSPROJ_Gnomic,
             'Gnomic'
           ), 0
         ) ;
      Add( TGIS_CSProjLambertAzimuthalEqualArea.Create
           ( CSPROJ_Lambert_Azimuthal_Equal_Area,
             'Lambert_Azimuthal_Equal_Area'
           ), 0
         ) ;
      Add( TGIS_CSProjLambertAzimuthalEqualArea.Create
           ( CSPROJ_Lambert_Azimuthal_Equal_Area_Spherical,
             'Lambert_Azimuthal_Equal_Area_Spherical'
           ), CSPROJ_Lambert_Azimuthal_Equal_Area
         ) ;
      Add( TGIS_CSProjLambertAzimuthalEqualArea.Create
           ( CSPROJ_Azimuthal_Equal_Area,
             'Azimuthal_Equal_Area'
           ), CSPROJ_Lambert_Azimuthal_Equal_Area
         ) ;
      Add( TGIS_CSProjLarrivee.Create
           ( CSPROJ_Larrivee,
             'Larrivee'
           ), 0
         ) ;
      Add( TGIS_CSProjLambertConformalConic.Create
           ( CSPROJ_Lambert_Conformal_Conic_2SP,
             'Lambert_Conformal_Conic_2SP'
           ), 0
         ) ;
      Add( TGIS_CSProjLambertConformalConic_1SP.Create
           ( CSPROJ_Lambert_Conformal_Conic_1SP,
             'Lambert_Conformal_Conic_1SP'
           ), 0
         ) ;
      Add( TGIS_CSProjLambertConformalConic_1SP.Create
           ( CSPROJ_Lambert_Near_Conformal_Conic,
             'Lambert_Near_Conformal_Conic'
           ), CSPROJ_Lambert_Conformal_Conic_1SP
         ) ;
      Add( TGIS_CSProjLambertConformalConic.Create
           ( CSPROJ_Lambert_Conformal_Conic,
             'Lambert_Conformal_Conic'
           ), CSPROJ_Lambert_Conformal_Conic_2SP
         ) ;
      Add( TGIS_CSProjLambertConformalConic.Create
           ( CSPROJ_Lambert_Conformal_Conic_2SP_Belgium,
             'Lambert_Conformal_Conic_SP2_Belgium'
           ), CSPROJ_Lambert_Conformal_Conic_2SP
         ) ;
      Add( TGIS_CSProjLambertConformalConicWestOriented.Create
           ( CSPROJ_Lambert_Conformal_Conic_West_Oriented,
             'Lambert_Conformal_Conic_West_Oriented'
           ), 0
         ) ;
      Add( TGIS_CSProjLoximuthal.Create
           ( CSPROJ_Loximuthal,
             'Loximuthal'
           ), 0
         ) ;
      Add( TGIS_CSProjMercator.Create
           ( CSPROJ_Mercator,
             'Mercator'
           ), 0
         ) ;
      Add( TGIS_CSProjMercator.Create
           ( CSPROJ_Mercator_1SP,
             'Mercator_1SP'
           ), CSPROJ_Mercator
         ) ;
      Add( TGIS_CSProjMercator.Create
           ( CSPROJ_Mercator_2SP,
             'Mercator_2SP'
           ), CSPROJ_Mercator
         ) ;
      Add( TGIS_CSProjMercator_1SP_Spherical.Create
           ( CSPROJ_Mercator_Auxiliary_Sphere,
             'Mercator_Auxiliary_Sphere'
           ), 0
         ) ;
      Add( TGIS_CSProjMercator_1SP_Spherical.Create
           ( CSPROJ_Mercator_1SP_Spherical,
            'Mercator_1SP_Spherical'
           ), CSPROJ_Mercator_Auxiliary_Sphere
         ) ;
      Add( TGIS_CSProjMercator_1SP_Spherical.Create
           ( CSPROJ_Popular_Visualisation_Pseudo_Mercator,
             'Popular_Visualisation_Pseudo_Mercator'
           ), CSPROJ_Mercator_Auxiliary_Sphere
         ) ;
      Add( TGIS_CSProjMercator_1SP_Spherical.Create
           ( CSPROJ_Mercator_1SP_Google,
             'Mercator_1SP_Google'
           ), CSPROJ_Mercator_Auxiliary_Sphere
         ) ;
      Add( TGIS_CSProjMillerCylindrical.Create
           ( CSPROJ_Miller_Cylindrical,
             'Miller_Cylindrical'
           ), 0
         ) ;
      Add( TGIS_CSProjMillerOblatedStereographic.Create
           ( CSPROJ_Miller_Oblated_Stereographic,
             'Miller_Oblated_Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjLeeOblatedStereographic.Create
           ( CSPROJ_Lee_Oblated_Stereographic,
             'Lee_Oblated_Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjGS48.Create
           ( CSPROJ_GS48,
             'GS48'
           ), 0
         ) ;
      Add( TGIS_CSProjGS50.Create
           ( CSPROJ_GS50,
             'GS50'
           ), 0
         ) ;
      Add( TGIS_CSProjModifiedStererographicsForAlaska.Create
           ( CSPROJ_Modified_Stererographics_For_Alaska,
             'Modified_Stererographics_For_Alaska'
           ), 0
         ) ;
      Add( TGIS_CSProjMollweide.Create
           ( CSPROJ_Mollweide,
             'Mollweide'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_IV.Create
           ( CSPROJ_Wagner_IV,
             'Wagner_IV'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_V.Create
           ( CSPROJ_Wagner_V,
             'Wagner_V'
           ), 0
         ) ;
      Add( TGIS_CSProjNewZealandMapGrid.Create
           ( CSPROJ_New_Zealand_Map_Grid,
             'New_Zealand_Map_Grid'
           ), 0
         ) ;
      Add( TGIS_CSProjHotineObliqueMercatorTwoPoint.Create
           ( CSPROJ_Hotine_Oblique_Mercator_Two_Point,
             'Hotine_Oblique_Mercator_Two_Point'
           ), 0
         ) ;
      Add( TGIS_CSProjHotineObliqueMercatorAzimuthCenter.Create
           ( CSPROJ_Hotine_Oblique_Mercator_Azimuth_Center,
             'Hotine_Oblique_Mercator_Azimuth_Center'
           ), 0
         ) ;
      Add( TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Create
           ( CSPROJ_Hotine_Oblique_Mercator_Azimuth_Natural_Origin,
             'Hotine_Oblique_Mercator_Azimuth_Natural_Origin'
           ), 0
         ) ;
      Add( TGIS_CSProjHotineObliqueMercator.Create
           ( CSPROJ_Hotine_Oblique_Mercator_Variant_B,
             'Hotine_Oblique_Mercator'
           ), 0
         ) ;
      Add( TGIS_CSProjHotineObliqueMercatorAzimuthNaturalOrigin.Create
           ( CSPROJ_Rectified_Skew_Orthomorphic_Natural_Origin,
             'Rectified_Skew_Orthomorphic_Natural_Origin'
           ), CSPROJ_Hotine_Oblique_Mercator_Azimuth_Natural_Origin
         ) ;

      Add( TGIS_CSProjEuler.Create
           ( CSPROJ_Euler,
             'Euler'
           ), 0
         ) ;
      Add( TGIS_CSProjTissot.Create
           ( CSPROJ_Tissot,
             'Tissot'
           ), 0
         ) ;
      Add( TGIS_CSProjMurdoch_I.Create
           ( CSPROJ_Murdoch_I,
             'Murdoch_I'
           ), 0
         ) ;
      Add( TGIS_CSProjMurdoch_II.Create
           ( CSPROJ_Murdoch_II,
             'Murdoch_II'
           ), 0
         ) ;
      Add( TGIS_CSProjMurdoch_III.Create
           ( CSPROJ_Murdoch_III,
             'Murdoch_III'
           ), 0
         ) ;
      Add( TGIS_CSProjVitkovsky_I.Create
           ( CSPROJ_Vitkovsky_I,
             'Vitkovsky_I'
           ), 0
         ) ;
      Add( TGIS_CSProjPerspectiveConic.Create
           ( CSPROJ_Perspective_Conic,
             'Perspective_Conic'
           ), 0
         ) ;
      Add( TGIS_CSProjStereographic.Create
           ( CSPROJ_Stereographic,
             'Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjPolarStereographic.Create
           ( CSPROJ_Polar_Stereographic,
             'Polar_Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjPolarStereographic_B.Create
           ( CSPROJ_Polar_Stereographic_B,
             'Polar_Stereographic_B'
           ), 0
         ) ;
      Add( TGIS_CSProjPolarStereographic_C.Create
           ( CSPROJ_Polar_Stereographic_C,
             'Polar_Stereographic_C'
           ), 0
         ) ;
      Add( TGIS_CSProjUniversalPolarStereographic.Create
           ( CSPROJ_Universal_Polar_Stereographic,
             'Universal_Polar_Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjObliqueStereographic.Create
           ( CSPROJ_Oblique_Stereographic,
             'Double_Stereographic' // Double_Sterograhic is the default
           ), 0
         ) ;
      Add( TGIS_CSProjObliqueStereographic.Create
           ( CSPROJ_Double_Stereographic,
             'Oblique_Stereographic' // Oblique_Stereographic is an alias
           ), CSPROJ_Oblique_Stereographic
         ) ;
      Add( TGIS_CSProjOrthographic.Create
           ( CSPROJ_Orthographic,
             'Orthographic'
           ), 0
         ) ;
      Add( TGIS_CSProjPolyconicAmerican.Create
           ( CSPROJ_Polyconic,
             'Polyconic'
           ), 0
         ) ;
      Add( TGIS_CSProjPolyconicAmerican.Create
           ( CSPROJ_Polyconic_American,
             'Polyconic_American'
           ), CSPROJ_Polyconic
         ) ;
      Add( TGIS_CSProjRobinson.Create
           ( CSPROJ_Robinson,
             'Robinson'
           ), 0
         ) ;
      Add( TGIS_CSProjSwissObliqueMercator.Create
           ( CSPROJ_Swiss_Oblique_Mercator,
             'Swiss_Oblique_Mercator'
           ), 0
         ) ;
      Add( TGIS_CSProjSwissObliqueMercator.Create
           ( CSPROJ_Oblique_Mercator,
             'Oblique_Mercator'
           ), CSPROJ_Swiss_Oblique_Mercator
         ) ;
      Add( TGIS_CSProjTransverseMercatorOld.Create
           ( CSPROJ_Transverse_Mercator_Old,
             'Transverse_Mercator_Old'
           ), 0
         ) ;
      Add( TGIS_CSProjTransverseMercator.Create
           ( CSPROJ_Transverse_Mercator,
             'Transverse_Mercator'
           ), 0
         ) ;
      Add( TGIS_CSProjTransverseMercatorSouthOriented.Create
           ( CSPROJ_Transverse_Mercator_South_Oriented,
             'Transverse_Mercator_South_Oriented'
           ), 0
         ) ;
      Add( TGIS_CSProjTransverseMercator.Create
           ( CSPROJ_Gauss_Kruger,
             'Gauss_Kruger'
           ), CSPROJ_Transverse_Mercator
         ) ;
      Add( TGIS_CSProjUniversalTransverseMercator.Create
           ( CSPROJ_Universal_Transverse_Mercator,
             'Universal_Transverse_Mercator'
           ), 0
         ) ;
      Add( TGIS_CSProjEquidistantTwoPoint.Create
           ( CSPROJ_Equidistant_Two_Point,
             'Equidistant_Two_Point'
           ), 0
         ) ;
      Add( TGIS_CSProjVanDerGrinten_I.Create
           ( CSPROJ_van_der_Grinten_I,
             'van_der_Grinten_I'
           ), 0
         ) ;
      Add( TGIS_CSProjVanDerGrinten_II.Create
           ( CSPROJ_van_der_Grinten_II,
             'van_der_Grinten_II'
           ), 0
         ) ;
      Add( TGIS_CSProjVanDerGrinten_III.Create
           ( CSPROJ_van_der_Grinten_III,
             'van_der_Grinten_III'
           ), 0
         ) ;
      Add( TGIS_CSProjVanDerGrinten_IV.Create
           ( CSPROJ_van_der_Grinten_IV,
             'van_der_Grinten_IV'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_II.Create
           ( CSPROJ_Wagner_II,
             'Wagner_II'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_III.Create
           ( CSPROJ_Wagner_III,
             'Wagner_III'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_VII.Create
           ( CSPROJ_Wagner_VII,
             'Wagner_VII'
           ), 0
         ) ;
      Add( TGIS_CSProjWinkel_I.Create
           ( CSPROJ_Winkel_I,
             'Winkel_I'
           ), 0
         ) ;
      Add( TGIS_CSProjWinkel_II.Create
           ( CSPROJ_Winkel_II,
             'Winkel_II'
           ), 0
         ) ;
      Add( TGIS_CSProjObliqueCylindricalEqualAreaAzimuth.Create
           ( CSPROJ_Oblique_Cylindrical_Equal_Area_Azimuth,
             'Oblique_Cylindrical_Equal_Area_Azimuth'
           ), 0
         ) ;
      Add( TGIS_CSProjObliqueCylindricalEqualAreaTwoPoint.Create
           ( CSPROJ_Oblique_Cylindrical_Equal_Area_Area_Two_Point,
             'Oblique_Cylindrical_Equal_Area_Area_Two_Point'
           ), 0
         ) ;
      Add( TGIS_CSProjOblatedEqualArea.Create
           ( CSPROJ_Oblated_Equal_Area,
             'Oblated_Equal_Area'
           ), 0
         ) ;
      Add( TGIS_CSProjKrovak.Create
           ( CSPROJ_Krovak,
             'Krovak'
           ), 0
         ) ;
      Add( TGIS_CSProjKrovakNorthOriented.Create
           ( CSPROJ_Krovak_North_Oriented,
             'Krovak_North_Oriented'
           ), 0
         ) ;
      Add( TGIS_CSProjKrovakModified.Create
           ( CSPROJ_Krovak_Modified,
             'Krovak_Modified'
           ), 0
         ) ;
      Add( TGIS_CSProjKrovakModifiedNorthOriented.Create
           ( CSPROJ_Krovak_Modified_North_Oriented,
             'Krovak_Modified_North_Oriented'
           ), 0
         ) ;
      Add( TGIS_CSProjBaconGlobular.Create
           ( CSPROJ_Bacon_Globular,
             'Bacon_Globular'
           ), 0
         ) ;
      Add( TGIS_CSProjApianGlobular_I.Create
           ( CSPROJ_Apian_Globular_I,
             'Apian_Globular_I'
           ), 0
         ) ;
      Add( TGIS_CSProjOrteliusOval.Create
           ( CSPROJ_Ortelius_Oval,
             'Ortelius_Oval'
           ), 0
         ) ;
      Add( TGIS_CSProjBipolarConic.Create
           ( CSPROJ_Bipolar_Conic,
             'Bipolar_Conic'
           ), 0
         ) ;
      Add( TGIS_CSProjBoggsEumorphic.Create
           ( CSPROJ_Boggs_Eumorphic,
             'Boggs_Eumorphic'
           ), 0
         ) ;
      Add( TGIS_CSProjCollignon.Create
           ( CSPROJ_Collignon,
             'Collignon'
           ), 0
         ) ;
      Add( TGIS_CSProjCrasterParabolic.Create
           ( CSPROJ_Craster_Parabolic,
             'Craster_Parabolic'
           ), 0
         ) ;
      Add( TGIS_CSProjFahey.Create
           ( CSPROJ_Fahey,
             'Fahey'
           ), 0
         ) ;
      Add( TGIS_CSProjFoucautSinusoidal.Create
           ( CSPROJ_Foucaut_Sinusoidal,
             'Foucaut_Sinusoidal'
           ), 0
         ) ;
      Add( TGIS_CSProjGinsburg_VIII.Create
           ( CSPROJ_Ginsburg_VIII,
             'Ginsburg_VIII'
           ), 0
         ) ;
      Add( TGIS_CSProjGallStereographic.Create
           ( CSPROJ_Gall_Stereographic,
             'Gall_Stereographic'
           ), 0
         ) ;
      Add( TGIS_CSProjLagrange.Create
           ( CSPROJ_Lagrange,
             'Lagrange'
           ), 0
         ) ;
      Add( TGIS_CSProjHammer.Create
           ( CSPROJ_Hammer,
             'Hammer'
           ), 0
         ) ;
      Add( TGIS_CSProjHatano.Create
           ( CSPROJ_Hatano,
             'Hatano'
           ), 0
         ) ;
      Add( TGIS_CSProjLaskowski.Create
           ( CSPROJ_Laskowski,
             'Laskowski'
           ), 0
         ) ;
      Add( TGIS_CSProjFlatPolarQuartic.Create
           ( CSPROJ_Flat_Polar_Quartic,
             'Flat_Polar_Quartic'
           ), 0
         ) ;
      Add( TGIS_CSProjFlatPolarParabolic.Create
           ( CSPROJ_Flat_Polar_Parabolic,
             'Flat_Polar_Parabolic'
           ), 0
         ) ;
      Add( TGIS_CSProjFlatPolarSine2.Create
           ( CSPROJ_Flat_Polar_Sine_2,
             'Flat_Polar_Sine_2'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P2.Create
           ( CSPROJ_Putnins_P2,
             'Putnins_P2'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P3.Create
           ( CSPROJ_Putnins_P3,
             'Putnins_P3'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P3p.Create
           ( CSPROJ_Putnins_P3p,
             'Putnins_P3p'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P4p.Create
           ( CSPROJ_Putnins_P4p,
             'Putnins_P4p'
           ), 0
         ) ;
      Add( TGIS_CSProjWerenskiold_I.Create
           ( CSPROJ_Werenskiold_I,
             'Werenskiold_I'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P5.Create
           ( CSPROJ_Putnins_P5,
             'Putnins_P5'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P5p.Create
           ( CSPROJ_Putnins_P5p,
             'Putnins_P5p'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P6.Create
           ( CSPROJ_Putnins_P6,
             'Putnins_P6'
           ), 0
         ) ;
      Add( TGIS_CSProjPutnins_P6p.Create
           ( CSPROJ_Putnins_P6p,
             'Putnins_P6p'
           ), 0
         ) ;
      Add( TGIS_CSProjRectangularPolyconic.Create
           ( CSPROJ_Rectangular_Polyconic,
             'Rectangular_Polyconic'
           ), 0
         ) ;
      Add( TGIS_CSProjKavraisky_V.Create
           ( CSPROJ_Kavraisky_V,
             'Kavraisky_V'
           ), 0
         ) ;
      Add( TGIS_CSProjQuarticAuthalic.Create
           ( CSPROJ_Quartic_Authalic,
             'Quartic_Authalic'
           ), 0
         ) ;
      Add( TGIS_CSProjFlatPolarSine1.Create
           ( CSPROJ_Flat_Polar_Sine_1,
             'Flat_Polar_Sine_1'
           ), 0
         ) ;
      Add( TGIS_CSProjFoucautPseudocylindrical.Create
           ( CSPROJ_Foucaut_Pseudocylindrical,
             'Foucaut_Pseudocylindrical'
           ), 0
         ) ;
      Add( TGIS_CSProjTransverseCentralCylindrical.Create
           ( CSPROJ_Transverse_Central_Cylindrical,
             'Transverse_Central_Cylindrical'
           ), 0
         ) ;
      Add( TGIS_CSProjTransverseCylindricalEqualArea.Create
           ( CSPROJ_Transverse_Cylindrical_Equal_Area,
             'Transverse_Cylindrical_Equal_Area'
           ), 0
         ) ;
      Add( TGIS_CSProjUrmaev_V.Create
           ( CSPROJ_Urmaev_V,
             'Urmaev_V'
           ), 0
         ) ;
      Add( TGIS_CSProjUrmaevFlatPolarSinusoidal.Create
           ( CSPROJ_Urmaev_Flat_Polar_Sinusoidal,
             'Urmaev_Flat_Polar_Sinusoidal'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_I.Create
           ( CSPROJ_Wagner_I,
             'Wagner_I'
           ), 0
         ) ;
      Add( TGIS_CSProjWagner_I.Create
           ( CSPROJ_Kavraisky_VI,
             'Kavraisky_VI'
           ), CSPROJ_Wagner_I
         ) ;
      Add( TGIS_CSProjNell.Create
           ( CSPROJ_Nell,
             'Nell'
           ), 0
         ) ;
      Add( TGIS_CSProjNellHammer.Create
           ( CSPROJ_Nell_Hammer,
             'Nell_Hammer'
           ), 0
         ) ;
      Add( TGIS_CSProjNicolosiGlobular.Create
           ( CSPROJ_Nicolosi_Globular,
             'Nicolosi_Globular'
           ), 0
         ) ;
      Add( TGIS_CSProjGoodeHomolosine.Create
           ( CSPROJ_Goode_Homolosine,
             'Goode_Homolosin'
           ), 0
         ) ;
      Add( TGIS_CSProjInternationalMapWorldPolyconic.Create
           ( CSPROJ_International_Map_World_Polyconic,
             'International_Map_World_Polyconic'
           ), 0
         ) ;
      Add( TGIS_CSProjChamberlinTrimetric.Create
           ( CSPROJ_Chamberlin_Trimetric,
             'Chamberlin_Trimetric'
           ), 0
         ) ;
      Add( TGIS_CSProjLabordeMadagaskar.Create
           ( CSPROJ_Laborde_Madagaskar,
             'Laborde_Madagaskar'
           ), 0
         ) ;
      Add( TGIS_CSProjLandsat.Create
           ( CSPROJ_Landsat,
            'Landsat'
           ), 0
         ) ;
      Add( TGIS_CSProjMGRS.Create
           ( CSPROJ_MGRS,
             'MGRS'
           ), 0
         ) ;
      Add( TGIS_CSProjF16GRS.Create
           ( CSPROJ_F16GRS,
             'F16GRS'
           ), 0
         ) ;
      Add( TGIS_CSProjGEOREF.Create
           ( CSPROJ_GEOREF,
             'GEOREF'
           ), 0
         ) ;
    finally
      UnlockThread ;
    end;
  end ;

//------------------------------------------------------------------------------
// Global lists
//------------------------------------------------------------------------------

  function CSProjList
    : TGIS_CSProjList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_ProjList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_ProjList ) then
            cs_ProjList
              := TGIS_CSProjList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_ProjList ;
  end;

//------------------------------------------------------------------------------
// Initialization
//------------------------------------------------------------------------------

{$IFDEF DCC}
  initialization

  finalization
    FreeObject( cs_ProjList ) ;
{$ENDIF}

{==================================== END =====================================}
end.
