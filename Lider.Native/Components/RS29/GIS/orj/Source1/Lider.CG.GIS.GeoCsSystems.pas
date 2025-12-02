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
  Coordinate System definitions and Coordinate System conversions methods.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCsSystems ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCsSystems"'}
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

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsProjections ;
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
  ///   A Coordinate system. Provides methods to convert between different
  ///   coordinate systems.
  /// </summary>
  TGIS_CSCoordinateSystem = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSAbstract     )
    protected
      /// <summary>
      ///   Error code of the last method.
      /// </summary>
      FError : Integer ;

      /// <summary>
      ///   Coordinates are in reversed order.
      /// </summary>
      FReversedCoordinates : Boolean ;

    protected

      function  fget_FullWKT           : String ; virtual;
      function  fget_PrettyWKT         : String ;
      function  fget_ValidityExtentWGS : TGIS_Extent ; virtual;
      function  fget_ValidityExtent    : TGIS_Extent ; virtual;

    protected

      /// <summary>
      ///   Get bounding polygon of coordinate system.
      /// </summary>
      /// <param name="_wgs">
      ///   if true, then polygon will be in WGS units; if false, then polygon
      ///   will be in coordinate system units
      /// </param>
      /// <returns>
      ///   TGIS_ShapePolygon object or nil
      /// </returns>
      function  getBoundings         ( const _wgs    : Boolean
                                     ) : TObject ; virtual;
    public // virtuals

      /// <inheritdoc/>
      procedure Assign               ( const _source : TObject
                                     ) ; override;

      /// <summary>
      ///   Convert provided coordinate from the current coordinate system into
      ///   GEOGCS (Geographic coordinate system).
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     this method is meaningless within TGIS_CSUnknownCoordinateSystem
      ///     scope.
      ///   </note>
      /// </remarks>
      function  ToGeocs              ( const _coords : TGIS_Point
                                     ) : TGIS_Point ; virtual;

      /// <summary>
      ///   Convert provided 3D coordinate from the current coordinate system
      ///   into GEOGCS (Geographic coordinate system).
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    this method is meaningful only within TGIS_CSProjectedCoordinateSystem
      ///    scope.
      ///    </note>
      /// </remarks>
      function  ToGeocs3D            ( const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D ; virtual;

      /// <summary>
      ///   Convert provided coordinate from the current coordinate system into
      ///   WGS84 expressed in radians.
      /// </summary>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      function  FromGeocs            ( const _coords : TGIS_Point
                                     ) : TGIS_Point ; virtual;

      /// <summary>
      ///   Convert provided 3D coordinate from WGS84 expressed in radians into
      ///   the current coordinate system.
      /// </summary>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      function  FromGeocs3D          ( const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D  ; virtual;

      /// <summary>
      ///   Convert provided 3D coordinate from the current coordinate system
      ///   into WGS84 expressed in radians.
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      procedure ToWGS3D_Ref          ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                     ) ; virtual;

      /// <summary>
      ///   Convert provided 3D coordinate from the WGS84 expressed in radians
      ///   into the current coordinate system.
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      procedure FromWGS3D_Ref        ( {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
                                     ) ; virtual;

      /// <summary>
      ///   Convert provided extent from the WGS84 expressed in radians into
      ///   the current coordinate system.
      /// </summary>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      /// <param name="_extent">
      ///   extent to be converted
      /// </param>
      function  ExtentFromWGS        ( const _extent : TGIS_Extent
                                     ) : TGIS_Extent ; virtual;

      /// <summary>
      ///   Convert provided extent from the current coordinate system into
      ///   WGS84 expressed in radians.
      /// </summary>
      /// <param name="_extent">
      ///   extent to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ExtentToWGS          ( const _extent : TGIS_Extent
                                     ) : TGIS_Extent ; virtual;

      /// <inheritdoc/>
      function  BoundingPolygon      ( const _cs     : TGIS_CSCoordinateSystem
                                     ) : TObject ; virtual;
    public // static

      /// <summary>
      ///   Test if provided coordinate system is same as the current one.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system to be tested
      /// </param>
      /// <returns>
      ///   True if is systems are same.
      /// </returns>
      function  IsSame               ( const _cs     : TGIS_CSCoordinateSystem
                                     ) : Boolean ;

      /// <summary>
      ///   Test if provided coordinate should and can be transformed to/from
      ///   the current one.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system to be tested
      /// </param>
      /// <returns>
      ///   True if system cam be converted.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     <list type="bullet">
      ///       <item>
      ///         If one of the coordinate systems is not defined then
      ///         result is False
      ///       </item>
      ///       <item>
      ///         If coordinate systems are same then result False (see
      ///         CanConvertEx for different test)
      ///       </item>
      ///     </list>
      ///   </note>
      /// </remarks>
      function  CanConvert           ( const _cs     : TGIS_CSCoordinateSystem
                                     ) : Boolean ;

      /// <summary>
      ///   Test if provided coordinate can be transformed to/from the current
      ///   one.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system to be tested
      /// </param>
      /// <returns>
      ///    True if system cam be converted.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     <list type="bullet">
      ///       <item>
      ///         If one of the coordinate systems is not defined then
      ///         result is False
      ///       </item>
      ///       <item>
      ///         If coordinate systems are same then result False (see
      ///         CanConvertEx for different test)
      ///       </item>
      ///     </list>
      ///   </note>
      /// </remarks>
      function  CanConvertEx         ( const _cs     : TGIS_CSCoordinateSystem
                                     ) : Boolean ;

      /// <summary>
      ///   Convert provided coordinate from the provided coordinate system
      ///   into the current coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   source coordinate system to be converted from
      /// </param>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  FromCS               ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _coords : TGIS_Point
                                     ) : TGIS_Point ;

      /// <summary>
      ///   Convert provided 3D coordinate from the provided coordinate system
      ///   into the current coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   source coordinate system to be converted from
      /// </param>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  FromCS3D             ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert provided 3D coordinate from the current coordinate system
      ///   into the provided coordinate system
      /// </summary>
      /// <param name="_cs">
      ///   desired coordinate system to be converted to
      /// </param>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ToCS                 ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _coords : TGIS_Point
                                     ) : TGIS_Point ;

      /// <summary>
      ///   Convert provided 3D coordinate from the current coordinate system
      ///   into the provided coordinate system
      /// </summary>
      /// <param name="_cs">
      ///   desired coordinate system to be converted to
      /// </param>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ToCS3D               ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert provided coordinate from the current coordinate system into
      ///   WGS84 expressed in radians.
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ToWGS                ( const _coords : TGIS_Point
                                     ) : TGIS_Point   ;

      /// <summary>
      ///   Convert provided 3D coordinate from the current coordinate system
      ///   into WGS84 expressed in radians.
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ToWGS3D              ( const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert provided coordinate from the WGS84 expressed in radians
      ///   into the current coordinate system.
      /// </summary>
      /// <param name="_coords">
      ///   point to converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  FromWGS              ( const _coords : TGIS_Point
                                     ) : TGIS_Point   ;

      /// <summary>
      ///   Convert provided 3D coordinate from the WGS84 expressed in radians
      ///   into the current coordinate system.
      /// </summary>
      /// <param name="_coords">
      ///   point to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  FromWGS3D            ( const _coords : TGIS_Point3D
                                     ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert provided extent from the provided coordinate system into
      ///   the current coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   source coordinate system to be converted from
      /// </param>
      /// <param name="_extent">
      ///   extent to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ExtentFromCS         ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _extent : TGIS_Extent
                                     ) : TGIS_Extent ;

      /// <summary>
      ///   Convert provided extent to the current coordinate system into the
      ///   provided coordinate system
      /// </summary>
      /// <param name="_cs">
      ///   desired coordinate system to be converted to
      /// </param>
      /// <param name="_extent">
      ///   extent to be converted
      /// </param>
      /// <returns>
      ///   Converted coordinates. If value of any member is &gt;= 1e30 that
      ///   means that conversion can not be done.
      /// </returns>
      function  ExtentToCS           ( const _cs     : TGIS_CSCoordinateSystem ;
                                       const _extent : TGIS_Extent
                                     ) : TGIS_Extent ;

      /// <summary>
      ///   Calculates ellipsoidal distance (expressed in meters) based on
      ///   current Coordinate System.
      /// </summary>
      /// <param name="_from">
      ///   first coordinate
      /// </param>
      /// <param name="_to">
      ///   first coordinate
      /// </param>
      /// <returns>
      ///   distance value or -1 if distance can not be computed
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    Coordinates must be in same Coordinate System. For calculations
      ///    between different Coordinate System see: DistanceFromCS().
      ///    </note>
      /// </remarks>
      function  Distance             ( const _from   : TGIS_Point              ;
                                       const _to     : TGIS_Point
                                     ) : Double ;

      /// <summary>
      ///   Calculates ellipsoidal distance (expressed in meters) based on
      ///   current coordinate system.
      /// </summary>
      /// <param name="_from_cs">
      ///   coordinate system of the first coordinate
      /// </param>
      /// <param name="_from">
      ///   first coordinate
      /// </param>
      /// <param name="_to_cs">
      ///   coordinate system of the first coordinate
      /// </param>
      /// <param name="_to">
      ///   first coordinate
      /// </param>
      /// <returns>
      ///   distance value or -1 if distance can not be computed
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    For calculations within same coordinate system see Distance().
      ///    </note>
      /// </remarks>
      function  DistanceFromCS       ( const _from_cs: TGIS_CSCoordinateSystem ;
                                       const _from   : TGIS_Point              ;
                                       const _to_cs  : TGIS_CSCoordinateSystem ;
                                       const _to     : TGIS_Point
                                     ) : Double ;

      /// <summary>
      ///   <para>
      ///     Save the current coordinate system to a WKT file (like .PRJ) file.
      ///   </para>
      ///   <para>
      ///     See also: SaveAsPrettyWKTFile.
      ///   </para>
      /// </summary>
      /// <param name="_path">
      ///   path (including file name ) of the file to be saved
      /// </param>
      procedure SaveAsWKTFile        ( const _path : String
                                     ) ;

      /// <summary>
      ///   <para>
      ///     Save the current coordinate system to a WKT file (like .PRJ)
      ///     file. Saved file will be formatted to a multiline, human readable
      ///     format.
      ///   </para>
      ///   <para>
      ///     See also: SaveAsWKTFile.
      ///   </para>
      /// </summary>
      /// <param name="_path">
      ///   path (including file name ) of the file to be saved
      /// </param>
      procedure SaveAsPrettyWKTFile  ( const _path : String
                                     ) ;

    public // properties

        /// <summary>
        ///   WKT string with current coordinate system. See also: PrettyWKT.
        /// </summary>
        property FullWKT   : String  read fget_FullWKT ;

        /// <summary>
        ///   WKT string with current coordinate system formatted to a
        ///   multiline, human readable format. See also: FullWKT.
        /// </summary>
        property PrettyWKT : String  read fget_PrettyWKT ;

        /// <summary>
        ///   Error code of the last method.
        ///   <list type="table">
        ///     <listheader>
        ///       <term>Value</term>
        ///       <description>Description</description>
        ///     </listheader>
        ///     <item>
        ///       <term>CLEAR (0)</term>
        ///       <description>no errors</description>
        ///     </item>
        ///     <item>
        ///       <term>NOBOUNDING (103)</term>
        ///       <description>bounding polygon can not be created</description>
        ///     </item>
        ///     <item>
        ///       <term>other</term>
        ///       <description>see TGIS_CSProjAbstract.Error</description>
        ///     </item>
        ///   </list>
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     The error number is only for informational purposes.
        ///     On multithread application result can be unpredictable.
        ///   </para>
        ///   <para>
        ///     To test coordinate validity test if result vales are lower
        ///     then 1e38.
        ///   </para>
        /// </remarks>
        property Error     : Integer read FError ;

        /// <summary>
        ///   Coordinates are in reversed order.
        ///   That means not Longitude-Latitude but Latitude-Longitude or not
        ///   Easting-Northing but Northing-Easting. This is only a hint to be
        ///   used upon interpreting specific formats like GML.
        /// </summary>
        property ReversedCoordinates : Boolean
                                       read  FReversedCoordinates
                                       write FReversedCoordinates ;

        /// <summary>
        ///   Validity extent of the Coordinate System (in radians). For
        ///   projected system it represent a common area of Projection
        ///   Validity zones and Coordinate System embedded validity.
        /// </summary>
        property ValidityExtentWGS : TGIS_Extent
                                     read fget_ValidityExtentWGS ;

        /// <summary>
        ///   Validity extent of the Coordinate System (in CS units). For
        ///   projected system it represent a common area of Projection
        ///   Validity zones and Coordinate System embedded validity.
        /// </summary>
        property ValidityExtent    : TGIS_Extent
                                     read fget_ValidityExtent ;
  end ;

  /// <summary>
  ///   A Geographic Coordinate system.
  /// </summary>
  TGIS_CSGeographicCoordinateSystem = {$IFDEF OXYGENE} public {$ENDIF}
                                      class ( TGIS_CSCoordinateSystem )
    private

        /// <summary>
        ///   Datum object.
        /// </summary>
        FDatum : TGIS_CSDatum ;

        /// <summary>
        ///   Prime Meridian object.
        /// </summary>
        FPrimeMeridian : TGIS_CSPrimeMeridian ;

        /// <summary>
        ///   Unit object.
        /// </summary>
        FUnits : TGIS_CSUnits ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      constructor  Create               ( const _epsg           : Integer ;
                                          const _wkt            : String  ;
                                          const _datum          : Integer ;
                                          const _prime_meridian : Integer ;
                                          const _units          : Integer
                                        ) ; reintroduce ; overload; virtual;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      constructor  Create               ( const _epsg           : Integer ;
                                          const _wkt            : String  ;
                                          const _datum          : Integer ;
                                          const _prime_meridian : Integer ;
                                          const _units          : Integer ;
                                          const _reversed       : Boolean ;
                                          const _area           : Integer
                                        ) ; reintroduce ; overload; virtual;

      /// <inheritdoc/>
      procedure Assign                  ( const _source         : TObject
                                        ) ; override;
    protected

      function  fget_FullWKT             : String ; override;
      function  fget_ValidityExtentWGS   : TGIS_Extent ; override;

    protected

      /// <inheritdoc/>
      function  getBoundings            ( const _wgs            : Boolean
                                        ) : TObject ; override;

    public

      /// <inheritdoc/>
      function  ToGeocs                 ( const _coords        : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  ToGeocs3D               ( const _coords        : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      function  FromGeocs               ( const _coords        : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  FromGeocs3D             ( const _coords        : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                 _coords        : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                 _coords        : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      function  ExtentFromWGS           ( const _extent        : TGIS_Extent
                                        ) : TGIS_Extent ; override;

      /// <inheritdoc/>
      function  ExtentToWGS             ( const _extent        : TGIS_Extent
                                        ) : TGIS_Extent ; override;

    public

        /// <summary>
        ///   Datum object.
        /// </summary>
        property Datum : TGIS_CSDatum read FDatum ;

        /// <summary>
        ///   Prime Meridian object.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///    PrimeMeridian can be different the TGIS_CSDatum.PrimeMeridian.
        ///    TGIS_Datum.PrimeMeridian will not be used and was provided only
        ///    for EPSG compatibility.
        ///    </note>
        /// </remarks>
        property PrimeMeridian : TGIS_CSPrimeMeridian read FPrimeMeridian ;

        /// <summary>
        ///   Unit object.
        /// </summary>
        property Units : TGIS_CSUnits read FUnits;
  end ;

  /// <summary>
  ///   A Projected Coordinate system.
  /// </summary>
  TGIS_CSProjectedCoordinateSystem = {$IFDEF OXYGENE} public {$ENDIF}
                                     class ( TGIS_CSCoordinateSystem )
    private

        /// <summary>
        ///   Underlying Geographic Coordinate System.
        /// </summary>
        FGeocs : TGIS_CSGeographicCoordinateSystem ;

        /// <summary>
        ///   Unit object.
        /// </summary>
        FUnits : TGIS_CSUnits ;

        /// <summary>
        ///   Projection object.
        /// </summary>
        FProjection : TGIS_CSProjAbstract ;

        /// <summary>
        ///   Projection parameters.
        /// </summary>
        FProjectionParams  : TGIS_CSProjParametersInternal ;

        /// <summary>
        ///   Projection supported parameters.
        /// </summary>
        FProjectionParamsSet : TGIS_CSProjParameterSet ;

        /// <summary>
        ///   Projection code.
        /// </summary>
        FProjectionEPSG : Integer ;
    private
      function  fget_Projection         : TGIS_CSProjAbstract ;

    protected
      procedure doDestroy               ; override;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the PCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      constructor Create                ( const _epsg         : Integer ;
                                          const _wkt          : String  ;
                                          const _geogcs       : Integer ;
                                          const _units        : Integer ;
                                          const _projection   : Integer ;
                                          const _parameters   : TGIS_CSProjParameters
                                        ) ; reintroduce ; overload; virtual;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the PCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      constructor Create                ( const _epsg         : Integer ;
                                          const _wkt          : String  ;
                                          const _geogcs       : Integer ;
                                          const _units        : Integer ;
                                          const _projection   : Integer ;
                                          const _parameters   : TGIS_CSProjParameters ;
                                          const _reversed     : Boolean ;
                                          const _area         : Integer
                                        ) ; reintroduce ; overload; virtual;

      /// <inheritdoc/>
      procedure   Assign                ( const _source       : TObject
                                        ) ; override;
    protected

      function  fget_FullWKT             : String ; override;
      function  fget_ValidityExtentWGS   : TGIS_Extent ; override;

    protected

      /// <inheritdoc/>
      function  getBoundings            ( const _wgs          : Boolean
                                        ) : TObject ; override;

      /// <summary>
      ///   Get polygon made on given extent.
      /// </summary>
      /// <param name="_extent">
      ///   given extent
      /// </param>
      /// <returns>
      ///   TGIS_ShapePolygon object.
      /// </returns>
      function  getExtentPolygon        ( const _extent       : TGIS_Extent
                                        ) : TObject ;

    public

      /// <inheritdoc/>
      function  ToGeocs                 ( const _coords       : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  ToGeocs3D               ( const _coords       : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      function  FromGeocs               ( const _coords       : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  FromGeocs3D             ( const _coords       : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}   _coords       : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}   _coords       : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      function  ExtentFromWGS           ( const _extent       : TGIS_Extent
                                        ) : TGIS_Extent ; override;

      /// <inheritdoc/>
      function  ExtentToWGS             ( const _extent       : TGIS_Extent
                                        ) : TGIS_Extent ; override;

      /// <summary>
      ///   Geographic Coordinate System object.
      /// </summary>
      property Geocs : TGIS_CSGeographicCoordinateSystem
                       read FGeocs ;

      /// <summary>
      ///   Projection object.
      /// </summary>
      property Projection : TGIS_CSProjAbstract
                            read fget_Projection ;

      /// <summary>
      ///   Unit object.
      /// </summary>
      property Units : TGIS_CSUnits
                       read FUnits;

      /// <summary>
      ///   Projection parameters.
      /// </summary>
      property ProjectionParams  : TGIS_CSProjParametersInternal
                                   read FProjectionParams;

      /// <summary>
      ///   Projection supported parameters.
      /// </summary>
      property ProjectionParamsSet : TGIS_CSProjParameterSet
                                     read FProjectionParamsSet ;

      /// <summary>
      ///   Projection code.
      /// </summary>
      property ProjectionEPSG : Integer
                                read FProjectionEPSG ;
  end ;

  /// <summary>
  ///   An Unknown Projected Coordinate system. This is not a real coordinate
  ///   system. This class is only to used a mark a any Coordinate System
  ///   reference in a code as "undefined".
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSUnknownCoordinateSystem instead of creating instance of
  ///    this class.
  ///    </note>
  /// </remarks>
  TGIS_CSUnknownCoordinateSystem = {$IFDEF OXYGENE} public {$ENDIF}
                                   class ( TGIS_CSCoordinateSystem )
    public

      /// <inheritdoc/>
      procedure Assign                  ( const _source : TObject
                                        ) ; override;

      /// <inheritdoc/>
      function  ToGeocs                 ( const _coords : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  ToGeocs3D               ( const _coords : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      function  FromGeocs               ( const _coords : TGIS_Point
                                        ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  FromGeocs3D             ( const _coords : TGIS_Point3D
                                        ) : TGIS_Point3D ; override;

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}   _coords : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      function  ExtentFromWGS           ( const _extent : TGIS_Extent
                                        ) : TGIS_Extent ; override;

      /// <inheritdoc/>
      function  ExtentToWGS             ( const _extent : TGIS_Extent
                                        ) : TGIS_Extent ; override;
  end ;

  /// <summary>
  ///   List of all Geographic Coordinate Systems.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSGeographicCoordinateSystemList instead of creating
  ///    instance of this class.
  ///    </note>
  /// </remarks>
  TGIS_CSGeographicCoordinateSystemList = {$IFDEF OXYGENE} public {$ENDIF}
                                          class ( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor    Create ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function Add    ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _datum          : Integer ;
                        const _prime_meridian : Integer ;
                        const _units          : Integer
                      ) : TGIS_CSGeographicCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function Add    ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _datum          : Integer ;
                        const _prime_meridian : Integer ;
                        const _units          : Integer ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSGeographicCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSGeographicCoordinateSystem object. If the matching
      ///   object does not exists then will be added to the list and returned.
      ///   If the object can be found then it will be returned from the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      /// <returns>
      ///   Found or newly created object.
      /// </returns>
      function Prepare( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _datum          : Integer ;
                        const _prime_meridian : Integer ;
                        const _units          : Integer
                      ) : TGIS_CSGeographicCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSGeographicCoordinateSystem object. If the matching
      ///   object does not exists then will be added to the list and returned.
      ///   If the object can be found then it will be returned from the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the GCS
      /// </param>
      /// <param name="_datum">
      ///   EPSG code for the datum
      /// </param>
      /// <param name="_prime_meridian">
      ///   EPSG code for the prime meridian
      /// </param>
      /// <param name="_units">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      /// <returns>
      ///   Found or newly created object.
      /// </returns>
      function Prepare( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _datum          : Integer ;
                        const _prime_meridian : Integer ;
                        const _units          : Integer ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSGeographicCoordinateSystem ;
                      reintroduce ; overload; virtual;

       /// <summary>
       ///   Fix item by substituting exiting item based on EPSG code.
       /// </summary>
       /// <param name="_epsg">
       ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
       ///   defined, temporary pool
       /// </param>
       /// <param name="_wkt">
       ///   WKT name for the GCS
       /// </param>
       /// <param name="_datum">
       ///   EPSG code for the datum
       /// </param>
       /// <param name="_prime_meridian">
       ///   EPSG code for the prime meridian
       /// </param>
       /// <param name="_units">
       ///   EPSG code for the unit
       /// </param>
       /// <param name="_reversed">
       ///   True if coordinates are not in Easting-Northing order
       /// </param>
       /// <param name="_area">
       ///   EPSG code for the area of validity
       /// </param>
       /// <remarks>
       ///   <note type="note">
       ///    function will not check _epsg and _wkt for being unique
       ///    </note>
       /// </remarks>
       /// <returns>
       ///   Newly created object.
       /// </returns>
       function Fix   ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _datum          : Integer ;
                        const _prime_meridian : Integer ;
                        const _units          : Integer ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSGeographicCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG ( const _epsg           : Integer
                       ) : TGIS_CSGeographicCoordinateSystem ;
                       reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT  ( const _wkt           : String
                       ) : TGIS_CSGeographicCoordinateSystem ;
                       reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init   ; override;
  end ;

  /// <summary>
  ///   List of all Projected Coordinate Systems.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSProjectedCoordinateSystemList instead of creating
  ///    instance of this class.
  ///    </note>
  /// </remarks>
  TGIS_CSProjectedCoordinateSystemList = {$IFDEF OXYGENE} public {$ENDIF}
                                         class ( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor     Create ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the PCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function Add    ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _geogcs         : Integer ;
                        const _units          : Integer ;
                        const _projection     : Integer ;
                        const _parameters     : TGIS_CSProjParameters
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the PCS
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function Add    ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _geogcs         : Integer ;
                        const _units          : Integer ;
                        const _projection     : Integer ;
                        const _parameters     : TGIS_CSProjParameters ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSProjectedCoordinateSystem object. If the matching
      ///   object does not exists then will be added to the list and returned.
      ///   If the object can be found then it will be returned from the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      function Prepare( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _geogcs         : Integer ;
                        const _units          : Integer ;
                        const _projection     : Integer ;
                        const _parameters     : TGIS_CSProjParameters
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSProjectedCoordinateSystem object. If the matching
      ///   object does not exists then will be added to the list and returned.
      ///   If the object can be found then it will be returned from the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      function Prepare( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _geogcs         : Integer ;
                        const _units          : Integer ;
                        const _projection     : Integer ;
                        const _parameters     : TGIS_CSProjParameters ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the GCS; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the PCS
      /// </param>
      /// <param name="_geogcs">
      ///   EPSG code for the GCS
      /// </param>
      /// <param name="_units">
      ///   EPSG code for units
      /// </param>
      /// <param name="_projection">
      ///   EPSG code for the projection
      /// </param>
      /// <param name="_parameters">
      ///   projection parameters
      /// </param>
      /// <param name="_reversed">
      ///   True if coordinates are not in Easting-Northing order
      /// </param>
      /// <param name="_area">
      ///   EPSG code for the area of validity
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function Fix    ( const _epsg           : Integer ;
                        const _wkt            : String  ;
                        const _geogcs         : Integer ;
                        const _units          : Integer ;
                        const _projection     : Integer ;
                        const _parameters     : TGIS_CSProjParameters ;
                        const _reversed       : Boolean ;
                        const _area           : Integer
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; overload; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG( const _epsg           : Integer
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT ( const _wkt           : String
                      ) : TGIS_CSProjectedCoordinateSystem ;
                      reintroduce ; virtual;

      /// <summary>
      ///   Return clear (initialized) projection parameters (to be used by
      ///   Prepare command)
      /// </summary>
      /// <returns>
      ///   Emptied parameters .
      /// </returns>
      function  EmptyParams
                      : TGIS_CSProjParameters ;

      /// <summary>
      ///   Return clear (initialized) projection parameters (to be used by
      ///   Prepare command)
      /// </summary>
      /// <param name="_proj_epsg">
      ///   EPSG code for projection
      /// </param>
      /// <returns>
      ///   Default parameters for selected projection.
      /// </returns>
      function  DefaultParams
                      ( const _proj_epsg     : Integer
                      ) : TGIS_CSProjParameters ;

      /// <summary>
      ///   Return initialized Transverse Mercator projection parameters for
      ///   Universal TransverseMercator zone. (to be used by Prepare command)
      /// </summary>
      /// <param name="_zone">
      ///   UTM zone; negative values represent south hemisphere zone
      /// </param>
      /// <returns>
      ///   Transverse Mercator object.
      /// </returns>
      function  DefaultParamsForUTM
                      ( const _zone          : Integer
                      ) : TGIS_CSProjParameters ;

      /// <inheritdoc/>
      procedure Init  ; override;
  end ;

 /// <summary>
 ///   Returns the list of all Geographical Coordinate Systems defined.
 /// </summary>
 /// <returns>
 ///   Global list of all Geographical Coordinate Systems objects.
 /// </returns>
 function CSGeographicCoordinateSystemList
   : TGIS_CSGeographicCoordinateSystemList ;

 /// <summary>
 ///   Returns the list of all Projected Coordinate Systems defined.
 /// </summary>
 /// <returns>
 ///   Global list of all Projected Coordinate Systems objects.
 /// </returns>
 function CSProjectedCoordinateSystemList
   : TGIS_CSProjectedCoordinateSystemList ;

 /// <summary>
 ///   Returns the predefined Unknown Coordinate System object.
 /// </summary>
 /// <returns>
 ///   Unknown Coordinate System object.
 /// </returns>
 function CSUnknownCoordinateSystem
   : TGIS_CSUnknownCoordinateSystem ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Classes,
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    {$IFNDEF GIS_BASIC_PROJECTION}
      Lider.CG.GIS.GeoLayerVector,
    {$ENDIF}
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}

type
  { Class to map ESRI names.
  }
  T_esriNamesMap = {$IFDEF OXYGENE} abstract {$ENDIF} class( TGIS_CSAbstractList )
    public
      constructor Create  ;
      function    Add     ( const _epsg               : Integer ;
                            const _wkt                : String
                          ) : TGIS_CSAbstract ; reintroduce ; virtual;
      procedure   Init    ; override;
      procedure   InitEx  ; virtual; abstract;
  end;

  { Class to map ESRI datum names.
  }
  T_esriDatumsMap = class( T_esriNamesMap )
    public
      procedure InitEx ; override;
  end;

  { Class to map ESRI geogcs names.
  }
  T_esriGeocsMap = class( T_esriNamesMap )
    public
      procedure InitEx ; override;
  end;

  constructor T_esriNamesMap.Create  ;
  begin
    inherited Create( nil, True, False ) ;
  end;

  function T_esriNamesMap.Add(
    const _epsg               : Integer ;
    const _wkt                : String
  ) : TGIS_CSAbstract ;
  begin
    LockThread ;
    try
      Result := TGIS_CSAbstract.Create(
                  _epsg,
                  uniqueWkt( _epsg,_wkt )
                ) ;
      inherited Add( Result ) ;
    finally
      UnlockThread ;
    end;
  end;

  procedure T_esriNamesMap.Init;
  begin
    LockThread ;
    try
      Clear ;

      InitEx ;
    finally
      UnlockThread ;
    end;
  end;

  procedure T_esriDatumsMap.InitEx;
  {$INCLUDE CsData/GisEsriDatums.inc}
  begin
    Init_ESRIDatumList( self ) ;
  end ;

  procedure T_esriGeocsMap.InitEx;
  {$INCLUDE CsData/GisEsriGeogcs.inc}
  begin
    Init_ESRIGeogcsList( self ) ;
  end;

var
  cs_esriDatumsMap : T_esriNamesMap = nil ;
  cs_esriGeocsMap  : T_esriNamesMap = nil  ;

  { Map ESRI datum names.
  }
  function esriDatumMapList : T_esriNamesMap ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_esriDatumsMap ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_esriDatumsMap ) then
            cs_esriDatumsMap := T_esriDatumsMap.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_esriDatumsMap ;
  end;

  { Map ESRI geogcs names.
  }
  function esriGeocsMapList : T_esriNamesMap ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_esriGeocsMap ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_esriGeocsMap ) then
            cs_esriGeocsMap := T_esriGeocsMap.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_esriGeocsMap ;
  end;

var
  { List of all GCS.ss }
  cs_GeographicCoordinateSystemList : TGIS_CSGeographicCoordinateSystemList = nil ;

  { List of all PCS.ss }
  cs_ProjectedCoordinateSystemList  : TGIS_CSProjectedCoordinateSystemList = nil ;

  { UNKNOWN CS (1 object only).ss }
  cs_UnknownCoordinateSystem  : TGIS_CSUnknownCoordinateSystem = nil ;

type
  T_A_GIS_Point3D = Array of TGIS_Point3D ;
  T_A_Integer     = Array of Integer ;

//------------------------------------------------------------------------------
// TGIS_CSCoordinateSystem
//------------------------------------------------------------------------------

  function TGIS_CSCoordinateSystem.fget_FullWKT()
    : String ;
  begin
    Result := 'UNKNOWN' ;
  end ;

  function TGIS_CSCoordinateSystem.fget_PrettyWKT()
    : String ;
  var
    wkt : String ;
    i : Integer ;
    indent : Integer ;
    c : Char ;

    function spc : String ;
    var
      k : Integer ;
    begin
      Result := '' ;

      for k:= 0 to indent do
        Result := Result + '  ' ;
    end ;
  begin
    Result := '' ;

    wkt := fget_FullWKT() ;

    indent := 0 ;

    for i := StringFirst to StringLast( wkt ) do begin
      c := wkt[i] ;

      case c of
        '[' : begin
                inc( indent ) ;
                Result := Result + c + #13#10 + spc ;
              end ;
        ',' : begin
                Result := Result + c + #13#10 + spc ;
              end ;
        ']' : begin
                dec( indent ) ;
                Result := Result + #13#10 + spc + c ;
              end ;
        else Result := Result + c ;
      end ;
    end ;
  end ;

  function TGIS_CSCoordinateSystem.fget_ValidityExtentWGS
    : TGIS_Extent ;
  begin
    Result := GisExtent( -Pi, -Pi/2, Pi, Pi/2 ) ;
  end;

  function TGIS_CSCoordinateSystem.fget_ValidityExtent
    : TGIS_Extent ;
  begin
    Result := ExtentFromWGS( ValidityExtentWGS ) ;
  end;

  function TGIS_CSCoordinateSystem.getBoundings(
    const _wgs : Boolean
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_CSCoordinateSystem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is TGIS_CSCoordinateSystem ) ;

    inherited ;

//    FError
//      := TGIS_CSCoordinateSystem( _source ).FError               ;
    FReversedCoordinates
      := TGIS_CSCoordinateSystem( _source ).FReversedCoordinates ;
  end;

  function TGIS_CSCoordinateSystem.ToGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    // do nothing - just for safe inheritance
    Result := _coords ;
  end ;

  function TGIS_CSCoordinateSystem.ToGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    // do nothing - just for safe inheritance
    Result := _coords ;
  end ;

  function  TGIS_CSCoordinateSystem.FromGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    // do nothing - just for safe inheritance
    Result := _coords ;
  end ;

  function  TGIS_CSCoordinateSystem.FromGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    // do nothing - just for safe inheritance
    Result := _coords ;
  end ;

  procedure TGIS_CSCoordinateSystem.ToWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing - just for safe inheritance
  end ;

  procedure TGIS_CSCoordinateSystem.FromWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing - just for safe inheritance
  end ;

  function TGIS_CSCoordinateSystem.IsSame(
    const _cs  : TGIS_CSCoordinateSystem
  ) : Boolean ;
  begin
    Result := ( assigned( _cs )      ) and
              ( self.EPSG = _cs.EPSG ) ;
  end ;

  function TGIS_CSCoordinateSystem.CanConvert(
    const _cs  : TGIS_CSCoordinateSystem
  ) : Boolean ;
  begin
    Result := ( assigned( _cs )       ) and
              ( self.EPSG <> 0        ) and
              ( _cs.EPSG  <> 0        ) and
              ( self.EPSG <> _cs.EPSG ) ;
  end ;

  function TGIS_CSCoordinateSystem.CanConvertEx(
    const _cs  : TGIS_CSCoordinateSystem
  ) : Boolean ;
  begin
    Result := IsSame( _cs ) or CanConvert( _cs ) ;
  end ;

  function TGIS_CSCoordinateSystem.FromCS(
    const _cs     : TGIS_CSCoordinateSystem;
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    assert( assigned( _cs ) ) ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _TGIS_Point(_coords)
    else begin
      Result := _cs.ToWGS( _coords ) ;

      if Result.X > GIS_MAX_SINGLE then begin
        FError := _cs.FError ;
        exit ;
      end;

      Result := self.FromWGS( Result )
    end;
  end ;

  function TGIS_CSCoordinateSystem.FromCS3D(
    const _cs     : TGIS_CSCoordinateSystem;
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    assert( assigned( _cs ) ) ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _coords
    else begin
      Result := _cs.ToWGS3D( _coords ) ;

      if Result.X > GIS_MAX_SINGLE then begin
        FError := _cs.FError ;
        exit ;
      end;

      Result := self.FromWGS3D( Result )
    end;
  end ;

  function TGIS_CSCoordinateSystem.ToCS(
    const _cs     : TGIS_CSCoordinateSystem;
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    assert( assigned( _cs ) ) ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _coords
    else begin
      Result := self.ToWGS( _coords ) ;

      if Result.X > GIS_MAX_SINGLE then begin
        FError := _cs.FError ;
        exit ;
      end;

      Result := _cs.FromWGS( Result ) ;
    end ;
  end ;

  function TGIS_CSCoordinateSystem.ToCS3D(
    const _cs     : TGIS_CSCoordinateSystem;
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    assert( assigned( _cs ) ) ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _coords
    else begin
      Result := self.ToWGS3D( _coords ) ;

      if Result.X > GIS_MAX_SINGLE then begin
        FError := _cs.FError ;
        exit ;
      end;

      Result := _cs.FromWGS3D( Result ) ;
    end ;
  end ;

  function TGIS_CSCoordinateSystem.ToWGS(
    const _coords : TGIS_Point
  ) : TGIS_Point   ;
  var
    coords : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      coords := new TGIS_Point3D ;
    {$ENDIF}
    coords.X := _coords.X ;
    coords.Y := _coords.Y ;
    coords.Z := 0 ;
    coords.M := 0 ;

    ToWGS3D_Ref( coords ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := coords.X ;
    Result.Y := coords.Y ;
  end ;

  function TGIS_CSCoordinateSystem.ToWGS3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    coords : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      coords := new TGIS_Point3D ;
    {$ENDIF}
    coords.X := _coords.X ;
    coords.Y := _coords.Y ;
    coords.Z := _coords.Z ;
    coords.M := _coords.M ;

    ToWGS3D_Ref( coords ) ;

    Result := coords ;
  end ;

  function TGIS_CSCoordinateSystem.FromWGS(
    const _coords : TGIS_Point
  ) : TGIS_Point   ;
  var
    coords : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      coords := new TGIS_Point3D ;
    {$ENDIF}
    coords.X := _coords.X ;
    coords.Y := _coords.Y ;
    coords.Z := 0 ;
    coords.M := 0 ;

    FromWGS3D_Ref( coords ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    Result.X := coords.X ;
    Result.Y := coords.Y ;
  end ;

  function TGIS_CSCoordinateSystem.FromWGS3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  var
    coords : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      coords := new TGIS_Point3D ;
    {$ENDIF}
    coords.X := _coords.X ;
    coords.Y := _coords.Y ;
    coords.Z := _coords.Z ;
    coords.M := _coords.M ;

    FromWGS3D_Ref( coords ) ;

    Result := coords ;
  end ;

  function TGIS_CSCoordinateSystem.ExtentFromCS(
    const _cs     : TGIS_CSCoordinateSystem;
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    ex : TGIS_Extent ;
  begin
    FError := ERROR_CLEAR ;

    assert( assigned( _cs ) ) ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _extent
    else begin
      if GisIsWholeWorld( _extent ) then begin
        ex := GisExtent( -Pi, -Pi/2, Pi, Pi/2 ) ;
        Result := ExtentFromWGS( ex ) ;
      end
      else begin
        ex := _cs.ExtentToWGS( _extent ) ;

        if ( ex.XMin > GIS_MAX_SINGLE ) or ( ex.XMin > GIS_MAX_SINGLE ) or
           ( ex.XMax > GIS_MAX_SINGLE ) or ( ex.XMax > GIS_MAX_SINGLE )
        then begin
          FError := _cs.FError ;
          Result := GisExtent( -GIS_MAX_DOUBLE, -GIS_MAX_DOUBLE,
                                GIS_MAX_DOUBLE,  GIS_MAX_DOUBLE
                              ) ;
          exit ;
        end;

        ex := GisCommonExtent( GisExtent( -Pi, -Pi/2, Pi, Pi/2 ), ex ) ;

        Result := ExtentFromWGS( ex ) ;
      end ;
    end ;
  end ;

  function TGIS_CSCoordinateSystem.ExtentToCS(
    const _cs     : TGIS_CSCoordinateSystem;
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    ex : TGIS_Extent ;
  begin
    FError := ERROR_CLEAR ;

    if ( _cs.EPSG  = 0         ) or
       ( self.EPSG = 0         ) or
       ( _cs.EPSG  = self.EPSG )
    then
      Result := _extent
    else begin
      if GisIsWholeWorld( _extent ) then begin
        ex := GisExtent( -Pi, -Pi/2, Pi, Pi/2 ) ;
        Result := _cs.ExtentFromWGS( ex ) ;
      end
      else begin
        ex := ExtentToWGS( _extent ) ;

        if ( ex.XMin > GIS_MAX_SINGLE ) or ( ex.XMin > GIS_MAX_SINGLE ) or
           ( ex.XMax > GIS_MAX_SINGLE ) or ( ex.XMax > GIS_MAX_SINGLE )
        then begin
          FError := _cs.FError ;
          Result := GisExtent( -GIS_MAX_DOUBLE, -GIS_MAX_DOUBLE,
                                GIS_MAX_DOUBLE,  GIS_MAX_DOUBLE
                              ) ;
        end;

        ex := GisCommonExtent( GisExtent( -Pi, -Pi/2, Pi, Pi/2 ), ex ) ;

        Result := _cs.ExtentFromWGS( ex ) ;

        FError := _cs.Error ;
      end ;
    end ;
  end ;

  function TGIS_CSCoordinateSystem.Distance(
    const _from   : TGIS_Point ;
    const _to     : TGIS_Point
  ) : Double ;
  begin
    Result := DistanceFromCS( self, _from, self, _to ) ;
  end;

  function TGIS_CSCoordinateSystem.DistanceFromCS(
    const _from_cs : TGIS_CSCoordinateSystem ;
    const _from    : TGIS_Point              ;
    const _to_cs   : TGIS_CSCoordinateSystem ;
    const _to      : TGIS_Point
  ) : Double ;
  var
    gcs : TGIS_CSGeographicCoordinateSystem ;
    pt_from : TGIS_Point ;
    pt_to   : TGIS_Point ;
  begin
    FError := ERROR_CLEAR ;

    Result := -1 ;

    if ( not assigned( _from_cs ) ) or
       ( not assigned( _to_cs   ) ) or
       ( self.EPSG    = 0         ) or
       ( _from_cs.EPSG = 0        ) or
       ( _to_cs.EPSG   = 0        )
    then
      exit ;

    if self is TGIS_CSGeographicCoordinateSystem then
      gcs := TGIS_CSGeographicCoordinateSystem( self )
    else if self is TGIS_CSProjectedCoordinateSystem then
      gcs := TGIS_CSProjectedCoordinateSystem( self ).Geocs
    else
      gcs := nil ;

    assert( assigned( gcs ) ) ;

    Result := GIS_MAX_DOUBLE ;

    pt_from := gcs.FromCS( _from_cs, _from ) ;

    if pt_from.X > GIS_MAX_SINGLE then begin
      self.FError := gcs.FError ;
      exit ;
    end;

    pt_to   := gcs.FromCS( _to_cs  , _to   ) ;

    if pt_to.X > GIS_MAX_SINGLE then begin
      self.FError := gcs.FError ;
      exit ;
    end;

    pt_from.X := gcs.Units.ToBase( pt_from.X ) ;
    pt_from.Y := gcs.Units.ToBase( pt_from.Y ) ;

    pt_to.X   := gcs.Units.ToBase( pt_to.X   ) ;
    pt_to.Y   := gcs.Units.ToBase( pt_to.Y   ) ;

    Result := gcs.Datum.Ellipsoid.Distance( pt_from, pt_to  )
  end;

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function  TGIS_CSCoordinateSystem.BoundingPolygon(
    const _cs     : TGIS_CSCoordinateSystem
  ) : TObject ;
  begin
  end ;
{$ELSE}

  function  TGIS_CSCoordinateSystem.BoundingPolygon(
    const _cs     : TGIS_CSCoordinateSystem
  ) : TObject ;
  var
    bnd_final  : TGIS_ShapePolygon ;
    bnd_self   : TGIS_ShapePolygon ;
    bnd_common : TGIS_Shape        ;

    // reproject a bnd_common polygon into new shape
    function reproject_boundings : TGIS_ShapePolygon ;
    var
      ipart  : Integer ;
      ipoint : Integer ;
      ptg    : TGIS_Point ;
    begin
      Result := TGIS_ShapePolygon.Create ;

      for ipart := 0 to bnd_common.GetNumParts - 1 do begin
        Result.AddPart ;
        for ipoint := 0 to bnd_common.GetPartSize( ipart ) - 1 do begin
          ptg := bnd_common.GetPoint( ipart, ipoint ) ;
          Result.AddPoint( FromWGS( ptg ) );
        end ;
      end ;
    end ;

  begin
    assert( assigned( _cs ) ) ;

    Result     := nil ;

    bnd_final  := nil ;
    bnd_self   := nil ;
    bnd_common := nil ;
    try
      // find projection area for the final projection
      bnd_final := TGIS_ShapePolygon( _cs.getBoundings( True ) ) ;
      if not assigned( bnd_final ) then exit ;

      // find projection area for the self projection
      bnd_self :=  TGIS_ShapePolygon( self.getBoundings( True ) ) ;
      if not assigned( bnd_self ) then exit ;

      // find intersection area between final and self
      bnd_common := bnd_final.Intersection( bnd_self, True ) ;
      if not assigned( bnd_common ) then exit ;

      // convert intersected area to self projection
      Result := reproject_boundings ;
    finally
      FreeObject( bnd_final  ) ;
      FreeObject( bnd_self   ) ;
      FreeObject( bnd_common ) ;
    end ;
  end ;
{$ENDIF}

  function  TGIS_CSCoordinateSystem.ExtentFromWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    // just for safe inheritance
  end ;

  function  TGIS_CSCoordinateSystem.ExtentToWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
    // just for safe inheritance
  end ;

  procedure TGIS_CSCoordinateSystem.SaveAsWKTFile(
    const _path : String
  ) ;
  var
    lst : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Text := FullWKT ;
      lst.SaveToFile( _path ) ;

    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_CSCoordinateSystem.SaveAsPrettyWKTFile(
    const _path : String
  ) ;
  var
    lst : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Text := PrettyWKT ;
      lst.SaveToFile( _path ) ;

    finally
      FreeObject( lst ) ;
    end ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSGeographicCoordinateSystem
//------------------------------------------------------------------------------

  constructor TGIS_CSGeographicCoordinateSystem.Create(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer
  ) ;
  begin
    Create(
      _epsg,
      _wkt,
      _datum,
      _prime_meridian,
      _units,
      True,
      0
    );
  end;

  constructor TGIS_CSGeographicCoordinateSystem.Create(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FReversedCoordinates := _reversed ;

    FDatum := CSDatumList.ByEPSG( _datum ) ;
    if not assigned( FDatum ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_PRJ_DATUM_NOEXIST ), '',
              _datum
            );

    FPrimeMeridian := CSPrimeMeridianList.ByEPSG( _prime_meridian ) ;
    if not assigned( FPrimeMeridian ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_PRJ_PRIMEMERIDIAN_NOEXIST ), '',
              _prime_meridian
            );

    FUnits := CSUnitsList.ByEPSG( _units ) ;
    if not assigned( FUnits ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_PRJ_UNITS_NOEXIST ), '',
              _units
            );
  end ;

  function TGIS_CSGeographicCoordinateSystem.fget_FullWKT
    : String ;
  var
    omap   : TGIS_CSAbstract ;
    sgeocs : String ;
    sdatum : String ;
  var
    angular_units : Double ;

    function authority( _o : TGIS_CSAbstract ) : String ;
    begin
      if _o.EPSG < 10000 then
        Result := Format( ',AUTHORITY["EPSG","%d"]', [_o.EPSG] )
      else
        Result := '' ;
    end ;

    function towgs84( _o : TGIS_CSTransformAbstract ) : String ;
    const
      _TOWGS84 = ',TOWGS84[%s,%s,%s,%s,%s,%s,%s]' ;
    begin
      if assigned( _o ) then begin
        case _o.Method of
          9603 : begin
                   Result := Format( _TOWGS84,
                                     [ DotFloatToStr(_o.A),
                                       DotFloatToStr(_o.B),
                                       DotFloatToStr(_o.C),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  )
                                     ]
                                  ) ;
                 end ;
          9606 : begin
                   Result := Format( _TOWGS84,
                                     [ DotFloatToStr(_o.A),
                                       DotFloatToStr(_o.B),
                                       DotFloatToStr(_o.C),
                                       DotFloatToStr( RadToDeg( _o.D ) * 3600 ),
                                       DotFloatToStr( RadToDeg( _o.E ) * 3600 ),
                                       DotFloatToStr( RadToDeg( _o.F ) * 3600 ),
                                       DotFloatToStr( _o.G * 1000000 )
                                     ]
                                   ) ;
                 end ;
          9607 : begin
                   Result := Format( _TOWGS84,
                                     [ DotFloatToStr(_o.A),
                                       DotFloatToStr(_o.B),
                                       DotFloatToStr(_o.C),
                                       DotFloatToStr( RadToDeg( -_o.D ) * 3600 ),
                                       DotFloatToStr( RadToDeg( -_o.E ) * 3600 ),
                                       DotFloatToStr( RadToDeg( -_o.F ) * 3600 ),
                                       DotFloatToStr( _o.G * 1000000 )
                            ]
                        ) ;
                 end ;
          else   begin
                   Result := '' ;
                 end ;
        end ;
      end
      else       begin
                   Result := Format( _TOWGS84,
                                     [ DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  ),
                                       DotFloatToStr( 0  )
                                     ]
                                  ) ;
                 end ;
    end ;

  begin
    assert( assigned( self.Datum ) ) ;
    assert( assigned( self.Datum.Ellipsoid ) ) ;
    assert( assigned( self.PrimeMeridian ) ) ;
    assert( assigned( self.Units ) ) ;
    assert( self.Units.Factor <> 0 ) ;

    omap := esriGeocsMapList.ByEPSG( self.EPSG ) ;
    if assigned( omap ) then
      sgeocs := omap.WKT
    else
      sgeocs := self.Datum.WKT ;

    omap := esriDatumMapList.ByEPSG( self.Datum.EPSG ) ;
    if assigned( omap ) then
      sdatum := omap.WKT
    else
      sdatum := self.Datum.WKT ;

    angular_units := self.Units.Factor ;

    Result := Format(
                'GEOGCS['          + // GEOGCS[
                  '"%s"'           + //   "Geocentric",
                  ',DATUM['        + //   DATUM[
                    '"%s"'         + //     "OSGB_1936",
                    ',SPHEROID['   + //     SPHEROID[
                      '"%s"'       + //       "Airy 1830",
                      ',%s'        + //       6377563.396,
                      ',%s'        + //       299.3249646
                      '%s'         + //       ,AUTHORITY[]
                    ']'            + //     ],
                    '%s'           + //     TOWGS[]
                    '%s'           + //     ,AUTHORITY[]
                  ']'              + //   ],
                  ',PRIMEM['       + //   PRIMEM[
                    '"%s"'         + //     "Greenwich",
                    ',%s'          + //     0
                    '%s'           + //     ,AUTHORITY[]
                  ']'              + //   ],
                  ',UNIT['         + //   UNIT[
                    '"%s"'         + //     "Meter",
                    ',%s'          + //     1
                    '%s'           + //     ,AUTHORITY[]
                  ']'              + //   ],
                  '%s'             + //   ,AUTHORITY[]
                ']'                , // ]
                [
                  sgeocs,
                  sdatum,
                  self.Datum.Ellipsoid.WKT,
                  DotFloatToStr(self.Datum.Ellipsoid.SemiMajor),
                  DotFloatToStr(self.Datum.Ellipsoid.InverseFlattering),
                  authority( self.Datum.Ellipsoid ),
                  towgs84( self.Datum.Transform ),
                  authority( self.Datum ),
                  self.PrimeMeridian.WKT,
                  DotFloatToStr( RadToDeg( self.PrimeMeridian.Longitude) ),
                  authority( self.PrimeMeridian ),
                  self.Units.WKT,
                  DotFloatToStr(angular_units),
                  authority( self.Units ),
                  authority( self )
                ]
             ) ;
  end ;

  function TGIS_CSGeographicCoordinateSystem.fget_ValidityExtentWGS
    : TGIS_Extent ;
  begin
    Result := GisExtent( -Pi, -Pi/2, Pi, Pi/2 ) ;
  end;

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function TGIS_CSGeographicCoordinateSystem.getBoundings(
    const _wgs : Boolean
  ) : TObject ;
  begin
  end ;
{$ELSE}

  function TGIS_CSGeographicCoordinateSystem.getBoundings(
    const _wgs : Boolean
  ) : TObject ;
  var
    ext : TGIS_Extent ;
    shp : TGIS_ShapePolygon ;
  begin
    ext := GisExtent( -Pi, -Pi/2, Pi, Pi/2 ) ;

    shp := TGIS_ShapePolygon.Create ;
    shp.AddPart ;

    shp.AddPoint( GisPoint( ext.XMin, ext.YMin ) ) ;
    shp.AddPoint( GisPoint( ext.XMin, ext.YMax ) ) ;
    shp.AddPoint( GisPoint( ext.XMax, ext.YMax ) ) ;
    shp.AddPoint( GisPoint( ext.XMax, ext.YMin ) ) ;
    shp.AddPoint( GisPoint( ext.XMin, ext.YMin ) ) ;

    Result := shp ;
  end ;
{$ENDIF}

  procedure TGIS_CSGeographicCoordinateSystem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is TGIS_CSGeographicCoordinateSystem ) ;

    inherited ;

    FDatum
      := TGIS_CSGeographicCoordinateSystem( _source ).FDatum         ;
    FPrimeMeridian
      := TGIS_CSGeographicCoordinateSystem( _source ).FPrimeMeridian ;
    FUnits
      := TGIS_CSGeographicCoordinateSystem( _source ).FUnits         ;
  end;

  function TGIS_CSGeographicCoordinateSystem.ToGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSGeographicCoordinateSystem.ToGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSGeographicCoordinateSystem.FromGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSGeographicCoordinateSystem.FromGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := _coords ;
  end ;

  procedure TGIS_CSGeographicCoordinateSystem.ToWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    _coords.X := _coords.X * FUnits.Factor ;
    _coords.Y := _coords.Y * FUnits.Factor ;

    _coords.X := _coords.X + PrimeMeridian.Longitude ;

    Datum.ToWGS3D_Ref( _coords ) ;
  end ;

  procedure TGIS_CSGeographicCoordinateSystem.FromWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    Datum.FromWGS3D_Ref( _coords )  ;

    _coords.X := _coords.X - PrimeMeridian.Longitude ;

    _coords.X := _coords.X / FUnits.Factor ;
    _coords.Y := _coords.Y / FUnits.Factor ;
  end ;

  function  TGIS_CSGeographicCoordinateSystem.ExtentFromWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    pt_a : TGIS_Point3D ;
    pt_b : TGIS_Point3D ;
  begin
    pt_a := GisPoint3D( _extent.XMin, _extent.YMin, 0 ) ;
    pt_b := GisPoint3D( _extent.XMax, _extent.YMax, 0 ) ;

    Datum.FromWGS3D_Ref( pt_a ) ;
    Datum.FromWGS3D_Ref( pt_b ) ;
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent;
    {$ENDIF}
    Result.XMin := pt_a.X / FUnits.Factor ;
    Result.YMin := pt_a.Y / FUnits.Factor ;
    Result.XMax := pt_b.X / FUnits.Factor ;
    Result.YMax := pt_b.Y / FUnits.Factor ;
  end ;

  function  TGIS_CSGeographicCoordinateSystem.ExtentToWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  var
    pt_a : TGIS_Point3D ;
    pt_b : TGIS_Point3D ;
  begin
    pt_a := GisPoint3D( _extent.XMin * FUnits.Factor,
                        _extent.YMin * FUnits.Factor,
                        0
                      ) ;
    pt_b := GisPoint3D( _extent.XMax * FUnits.Factor,
                        _extent.YMax * FUnits.Factor,
                        0
                      ) ;

    Datum.ToWGS3D_Ref( pt_a ) ;
    Datum.ToWGS3D_Ref( pt_b ) ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent;
    {$ENDIF}
    Result.XMin := pt_a.X ;
    Result.YMin := pt_a.Y ;
    Result.XMax := pt_b.X ;
    Result.YMax := pt_b.Y ;
  end ;

//==============================================================================
//  TGIS_CSProjectedCoordinateSystem
//==============================================================================

  constructor TGIS_CSProjectedCoordinateSystem.Create(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters
  ) ;
  begin
    Create(
      _epsg,
      _wkt,
      _geogcs,
      _units,
      _projection,
      _parameters,
      False,
      0
    );
  end;

  constructor TGIS_CSProjectedCoordinateSystem.Create(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) ;
  var
    primem : Double                ;
    proj   : TGIS_CSProjAbstract   ;
  begin
    inherited Create( _epsg, _wkt ) ;

    FReversedCoordinates := _reversed ;

    FGeocs  := CSGeographicCoordinateSystemList.ByEPSG( _geogcs ) ;
    if not assigned( FGeocs ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_PRJ_GEOCS_NOEXIST ), '',
              _geogcs
            );

    FProjectionEPSG := 0 ;
    proj := CSProjList.ByEPSG( _projection ) ;
    if not assigned( proj ) then begin
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_PRJ_PROJECTION_NOEXIST ), '',
              _projection
            );
    end ;
    FProjectionEPSG := _projection ;

    FUnits := CSUnitsList.ByEPSG( _units ) ;
    if not assigned( FUnits ) then
      raise EGIS_Exception.Create(
         _rsrc( GIS_RS_ERR_PRJ_UNITS_NOEXIST ), '',
         _units
         );

    {$IFDEF GIS_NORECORDS}
      FProjectionParams :=  new TGIS_CSProjParametersInternal ;
    {$ENDIF}
    FProjectionParams.All := _parameters ;
    FProjectionParamsSet := proj.ParametersSet ;

    primem := FGeocs.PrimeMeridian.Longitude ;

    if not IsNan( _parameters.CentralMeridian ) then
      FProjectionParams.CentralMeridian     := _parameters.CentralMeridian    +
                                               primem ;
    if not IsNan( _parameters.LongitudeOfCenter ) then
      FProjectionParams.LongitudeOfCenter   := _parameters.LongitudeOfCenter  +
                                               primem ;
    if not IsNan( _parameters.LongitudeOfPoint_1 ) then
      FProjectionParams.LongitudeOfPoint_1  := _parameters.LongitudeOfPoint_1 +
                                               primem ;
    if not IsNan( _parameters.LongitudeOfPoint_2 ) then
      FProjectionParams.LongitudeOfPoint_2  := _parameters.LongitudeOfPoint_2 +
                                               primem ;

    FProjection := nil ;
  end ;

  function TGIS_CSProjectedCoordinateSystem.fget_Projection
    : TGIS_CSProjAbstract ;
  var
    proj : TGIS_CSProjAbstract ;
  begin
    if not assigned( FProjection ) then begin
      proj := CSProjList.ByEPSG( FProjectionEPSG ) ;
      if not assigned( proj ) then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_PRJ_PROJECTION_NOEXIST ), '',
                FProjectionEPSG
              );
      FProjection := proj.CreateCopy ;
      FProjection.Parameters := FProjectionParams.All ;
      FProjection.Datum := FGeocs.Datum ;
    end ;
    Result := FProjection ;
  end;

  procedure TGIS_CSProjectedCoordinateSystem.doDestroy ;
  begin
    FreeObject( FProjection ) ;
    inherited ;
  end ;

  function TGIS_CSProjectedCoordinateSystem.fget_FullWKT
   : String ;
  var
    sparam : String ;
    linear_units  : Double ;
    angular_units : Double ;

    function authority( _o : TGIS_CSAbstract ) : String ;
    begin
      if _o.EPSG <= 32768 then
        Result := Format( ',AUTHORITY["EPSG","%d"]', [_o.EPSG] )
      else
        Result := '' ;
    end ;

    procedure add_param( const _name : String; const _value : String ) ;
    begin
      sparam := sparam + Format( ',PARAMETER["%s",%s]',
                                 [ _name, _value ]
                               ) ;
    end ;

    procedure add_dbl( const _name : String; const _value : Double ) ;
    begin
      add_param( _name, DotFloatToStr( _value ) ) ;
    end ;

    procedure add_lin( const _name : String; const _value : Double ) ;
    begin
      add_param( _name, DotFloatToStr( RoundTo( _value / linear_units, -4 ) ) ) ;
    end ;

    procedure add_deg( const _name : String; const _value : Double ) ;
    begin
      add_param( _name, DotFloatToStr( _value / angular_units ) ) ;
    end ;

    procedure add_int( const _name : String; const _value : Integer ) ;
    begin
      add_param( _name, IntToStr( _value ) ) ;
    end ;

    function param_wkt
      : String ;
    begin
      sparam := '' ;

      if InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Central_Meridian'           ,
                 self.Projection.CentralMeridian
                 - self.Geocs.PrimeMeridian.Longitude
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Latitude_Of_Origin'         ,
                 self.Projection.LatitudeOfOrigin
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                               self.Projection.ParametersSet
                             )
      then
        add_lin( 'False_Easting'              ,
                 self.Projection.FalseEasting
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.FalseNorthing,
                               self.Projection.ParametersSet
                             )
      then
        add_lin( 'False_Northing'             ,
                 self.Projection.FalseNorthing
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Standard_Parallel_1'        ,
                 self.Projection.StandardParallel_1
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Standard_Parallel_2'        ,
                 self.Projection.StandardParallel_2
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Pseudo_Standard_Parallel_1' ,
                 self.Projection.PseudoStandardParallel_1
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                               self.Projection.ParametersSet
                             )
      then
        add_int( 'Zone'                       ,
                 self.Projection.Zone
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                               self.Projection.ParametersSet
                             )
      then
        add_dbl( 'Scale_Factor'               ,
                 self.Projection.ScaleFactor
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Longitude_Of_Center'        ,
                 self.Projection.LongitudeOfCenter
                 - self.Geocs.PrimeMeridian.Longitude
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Latitude_Of_Center'         ,
                 self.Projection.LatitudeOfCenter
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Azimuth'                    ,
                 self.Projection.Azimuth
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Longitude_Of_Point_1'       ,
                 self.Projection.LongitudeOfPoint_1
                 - self.Geocs.PrimeMeridian.Longitude
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Latitude_Of_Point_1'        ,
                 self.Projection.LatitudeOfPoint_1
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Longitude_Of_Point_2'       ,
                 self.Projection.LongitudeOfPoint_2
                 - self.Geocs.PrimeMeridian.Longitude
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'Latitude_Of_Point_2'        ,
                 self.Projection.LatitudeOfPoint_2
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                               self.Projection.ParametersSet
                             )
      then
        add_dbl( 'X_Scale'                    ,
                 self.Projection.XScale
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                               self.Projection.ParametersSet
                             )
      then
        add_dbl( 'Y_Scale'                    ,
                 self.Projection.YScale
               ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                               self.Projection.ParametersSet
                             )
      then
        add_deg( 'XY_Plane_Rotation'          ,
                 self.Projection.XYPlaneRotation
               ) ;

      Result := sparam ;
    end ;

  begin
    assert( assigned( self.Geocs ) ) ;
    assert( assigned( self.Projection ) ) ;
    assert( assigned( self.Units ) ) ;
    assert( assigned( self.Geocs.Units ) ) ;
    assert( self.Units.Factor <> 0 ) ;
    assert( self.Geocs.Units.Factor <> 0 ) ;

    angular_units := self.Geocs.Units.Factor ;
    linear_units  := self.Units.Factor ;

    Result := Format(
                'PROJCS['          + // PROJCS[
                  '"%s"'           + //   "Pulkovo_1942_GK_Zone_24",
                  ',%s'            + //   GEOGCS[]
                  ',PROJECTION['   + //   PROJECTION[
                    '"%s"'         + //     "Gauss_Kruger"
                    '%s'           + //     ,AUTHORITY[]
                  ']'              + //   ],
                  '%s'             + //   ,PARAMETER[]
                  ',UNIT['         + //   UNIT[
                    '"%s"'         + //     "Meter",
                    ',%s'          + //     1
                    '%s'           + //     ,AUTHORITY[]
                  ']'              + //   ]
                  '%s'             + //   ,AUTHORITY[]
                ']'                , // ]
                [
                  self.WKT,
                  self.Geocs.FullWKT,
                  self.Projection.WKT,
                  authority( self.Projection ),
                  param_wkt,
                  self.Units.WKT,
                  DotFloatToStr( linear_units ),
                  authority( self.Units ),
                  authority( self )
                ]
             ) ;

  end ;

  function TGIS_CSProjectedCoordinateSystem.fget_ValidityExtentWGS
    : TGIS_Extent ;
  begin
    Result := Projection.ValidityExtentWGS ;
  end;

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function  TGIS_CSProjectedCoordinateSystem.getExtentPolygon
    ( const _extent    : TGIS_Extent
  ) : TObject ;
  begin
  end ;
{$ELSE}
  function  TGIS_CSProjectedCoordinateSystem.getExtentPolygon
    ( const _extent    : TGIS_Extent
  ) : TObject ;
  var
    pt, dpt : TGIS_Point ;
  const
    POINTS_ON_SIDE = 17 ;
    procedure add_side_points_x( _obj : TObject ) ;
    var
      i : Integer ;
    begin
      for i := 1 to POINTS_ON_SIDE do begin
        pt.X := pt.X +dpt.X ;
        TGIS_ShapePolygon(_obj).AddPoint(pt);
      end;
    end ;

    procedure add_side_points_y( _obj : TObject ) ;
    var
      i : Integer ;
    begin
      for i := 1 to POINTS_ON_SIDE do begin
        pt.Y := pt.Y +dpt.Y ;
        TGIS_ShapePolygon(_obj).AddPoint(pt);
      end;
    end ;

  begin
    Result := TGIS_ShapePolygon.Create ;
    TGIS_ShapePolygon(Result).Lock( TGIS_Lock.Extent ) ;
    TGIS_ShapePolygon(Result).AddPart ;
    //Left side
    pt := GisPoint( _extent.XMin, _extent.YMin ) ;
    TGIS_ShapePolygon(Result).AddPoint( pt ) ;
    {$IFDEF GIS_NORECORDS}
      dpt := new TGIS_Point;
    {$ENDIF}
    dpt.Y := (_extent.YMax -_extent.YMin)/(POINTS_ON_SIDE +1) ;
    add_side_points_y( Result ) ;
    //Up side
    pt := GisPoint( _extent.XMin, _extent.YMax ) ;
    TGIS_ShapePolygon(Result).AddPoint( pt ) ;
    dpt.X := (_extent.XMax -_extent.XMin)/(POINTS_ON_SIDE +1) ;
    add_side_points_x( Result ) ;
    //Right side
    pt := GisPoint( _extent.XMax, _extent.YMax ) ;
    TGIS_ShapePolygon(Result).AddPoint( pt ) ;
    dpt.Y := -dpt.Y ;
    add_side_points_y( Result ) ;
    //Down side
    pt := GisPoint( _extent.XMax, _extent.YMin ) ;
    TGIS_ShapePolygon(Result).AddPoint( pt ) ;
    dpt.X := -dpt.X ;
    add_side_points_x( Result ) ;

    TGIS_ShapePolygon(Result).Unlock ;
  end;
{$ENDIF}

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function TGIS_CSProjectedCoordinateSystem.getBoundings(
    const _wgs : Boolean
  ) : TObject ;
  begin
  end ;
{$ELSE}
  function TGIS_CSProjectedCoordinateSystem.getBoundings(
    const _wgs : Boolean
  ) : TObject ;
  var
    shp     : TGIS_ShapePolygon ;
    ext_val : TGIS_Extent ;
    s_org   : TGIS_Point ;


      procedure calculate_edge( _x_start, _x_end, _y_start, _y_end : Double  ) ;
      const
        EXTENT_STEPS  = 8    ;
        RAY_STEPS     = 10   ;
        EPS           = 1E-7 ;
      var
        was    : Boolean ;
        angle  : Double ;
        offset : Double ;
        step   : Double ;
        prec   : Double ;
        vx     : Double ;
        vy     : Double ;
        lx     : Double ;
        ly     : Double ;
        dx     : Double ;
        dy     : Double ;

        x      : Double ;
        y      : Double ;
        src_a  : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
        dst_b  : TGIS_Point  ;
        src_c  : TGIS_Point  ;
        dst_c  : TGIS_Point  ;
        tmp    : TGIS_Point  ;
        asin,acos: Double ;

        procedure change_step ;
        begin
          offset := offset - step ;
          step := step / 2 ;
          offset := offset + step ;
        end ;

      begin

        dx := ( _x_end - _x_start ) / EXTENT_STEPS ;
        dy := ( _y_end - _y_start ) / EXTENT_STEPS ;

        vx := _x_start ;
        vy := _y_start ;

        lx := Abs( _x_end - _x_start ) ;
        ly := Abs( _y_end - _y_start ) ;

        while ( lx > 0 ) or ( ly > 0 )   do begin

          angle := ArcTan2( vx - s_org.X, s_org.Y - vy ) ;

          step := GisPoint2Point( s_org, GisPoint( vx - s_org.X, vy - s_org.Y ) )
                  / RAY_STEPS ;
          offset := step ;

          prec := Max( step * EPS, 2e-11 ); // no more than 1mm on equator

          while step > prec do begin
            SinCos( angle, asin, acos ) ;
            x :=  asin * offset ;
            y := -acos * offset ;
            src_a.X := s_org.X + x ;
            src_a.Y := s_org.Y + y ;

            if not GisIsPointInsideExtent( src_a, ext_val ) then begin
              change_step ;
              continue ;
            end ;

            dst_b := FromWGS( src_a ) ;
            if dst_b.X > GIS_MAX_SINGLE then begin
              change_step ;
              continue ;
            end ;
            try
               tmp := ToWGS( dst_b ) ;
               if tmp.X > GIS_MAX_SINGLE then begin
                 change_step ;
                 continue ;
               end ;
            except
              change_step ;
              continue ;
            end ;

            was := True ;
            src_c := _TGIS_Point( src_a ) ;
            dst_c := _TGIS_Point( dst_b ) ;

            offset := offset + step ;
          end;

          if was then begin
            if _wgs then
              shp.AddPoint( src_c )
            else
              shp.AddPoint( dst_c ) ;
          end ;

          vx := vx + dx ;
          vy := vy + dy ;
          lx := lx - Abs( dx ) ;
          ly := ly - Abs( dy ) ;
        end;
      end;

    begin

      ext_val := Projection.ValidityExtentWGS ;
      s_org := Projection.ValidityOriginWGS ;

      shp := TGIS_ShapePolygon.Create ;
      shp.Lock( TGIS_Lock.Extent ) ;
      try
        shp.AddPart ;

        calculate_edge( ext_val.XMin, ext_val.XMax, ext_val.YMax, ext_val.YMax ) ;
        calculate_edge( ext_val.XMax, ext_val.XMax, ext_val.YMax, ext_val.YMin ) ;
        calculate_edge( ext_val.XMax, ext_val.XMin, ext_val.YMin, ext_val.YMin ) ;
        calculate_edge( ext_val.XMin, ext_val.XMin, ext_val.YMin, ext_val.YMax ) ;

      finally
        shp.Unlock ;
      end;

      Result := shp ;
    end;
{$ENDIF}

  procedure TGIS_CSProjectedCoordinateSystem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is TGIS_CSProjectedCoordinateSystem ) ;

    inherited ;

    FGeocs      := TGIS_CSProjectedCoordinateSystem( _source ).FGeocs      ;
    FProjection := TGIS_CSProjectedCoordinateSystem( _source ).FProjection ;
    FUnits      := TGIS_CSProjectedCoordinateSystem( _source ).FUnits      ;
  end;

  function TGIS_CSProjectedCoordinateSystem.ToGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D ;
  begin
    FError := ERROR_CLEAR ;

    ptg.X := _coords.X * FUnits.Factor ;
    ptg.Y := _coords.Y * FUnits.Factor ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Unproject3D_Ref( ptg ) ;

    if ptg.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    FGeocs.Datum.ToWGS3D_Ref( ptg ) ;

    Result.X := ptg.X / FGeocs.Units.Factor ;
    Result.Y := ptg.Y / FGeocs.Units.Factor ;
  end ;

  function TGIS_CSProjectedCoordinateSystem.ToGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    FError := ERROR_CLEAR ;

    Result.X := _coords.X * FUnits.Factor ;
    Result.Y := _coords.Y * FUnits.Factor ;
    Result.Z := _coords.Z ;
    Result.M := _coords.M ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Unproject3D_Ref( Result ) ;
    if Result.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    FGeocs.Datum.ToWGS3D_Ref( Result ) ;

    Result.X := Result.X / FGeocs.Units.Factor ;
    Result.Y := Result.Y / FGeocs.Units.Factor ;
  end ;

  function TGIS_CSProjectedCoordinateSystem.FromGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D ;
  begin
    FError := ERROR_CLEAR ;

    ptg.X := _coords.X * FGeocs.Units.Factor ;
    ptg.Y := _coords.Y * FGeocs.Units.Factor ;
    ptg.Z := 0 ;
    ptg.M := 0 ;

    FGeocs.Datum.FromWGS3D_Ref( ptg )  ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Project3D_Ref( ptg ) ;
    if ptg.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    Result.X := ptg.X / FUnits.Factor ;
    Result.Y := ptg.Y / FUnits.Factor ;
  end ;

  function TGIS_CSProjectedCoordinateSystem.FromGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    FError := ERROR_CLEAR ;

    Result.X := _coords.X * FGeocs.Units.Factor ;
    Result.Y := _coords.Y * FGeocs.Units.Factor ;
    Result.Z := _coords.Z ;
    Result.M := _coords.M ;

    FGeocs.Datum.FromWGS3D_Ref( Result )  ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Project3D_Ref( Result ) ;
    if Result.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    Result.X := Result.X / FUnits.Factor ;
    Result.Y := Result.Y / FUnits.Factor ;
  end ;

  procedure TGIS_CSProjectedCoordinateSystem.ToWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    FError := ERROR_CLEAR ;

    _coords.X := _coords.X * FUnits.Factor ;
    _coords.Y := _coords.Y * FUnits.Factor ;
    _coords.Z := _coords.Z * FUnits.Factor ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Unproject3D_Ref( _coords ) ;
    if _coords.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    FGeocs.Datum.ToWGS3D_Ref( _coords ) ;
  end ;

  procedure TGIS_CSProjectedCoordinateSystem.FromWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    FError := ERROR_CLEAR ;

    FGeocs.Datum.FromWGS3D_Ref( _coords )  ;

    if not assigned( FProjection ) then
      fget_Projection ;

    FProjection.Project3D_Ref( _coords ) ;
    if _coords.X > GIS_MAX_SINGLE then begin
      FError := FProjection.Error ;
      exit ;
    end ;

    _coords.X := _coords.X / FUnits.Factor ;
    _coords.Y := _coords.Y / FUnits.Factor ;
    _coords.Z := _coords.Z / FUnits.Factor ;
  end ;

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function TGIS_CSProjectedCoordinateSystem.ExtentFromWGS(
     const _extent : TGIS_Extent
   ) : TGIS_Extent ;
  begin
  end ;
{$ELSE}

  function TGIS_CSProjectedCoordinateSystem.ExtentFromWGS(
     const _extent : TGIS_Extent
  ) : TGIS_Extent ;

    function trivial_case : TGIS_Extent ;
    var
      ext         : TGIS_Extent ;
      p_xmin_ymin : TGIS_Point ;
      p_xmin_ymax : TGIS_Point ;
      p_xmax_ymin : TGIS_Point ;
      p_xmax_ymax : TGIS_Point ;
    begin
      ext := GisCommonExtent( Projection.ValidityExtentWGS , _extent ) ;

      p_xmin_ymin := FromWGS( GisPoint( ext.XMin, ext.YMin ) );
      p_xmin_ymax := FromWGS( GisPoint( ext.XMin, ext.YMax ) );
      p_xmax_ymin := FromWGS( GisPoint( ext.XMax, ext.YMin ) );
      p_xmax_ymax := FromWGS( GisPoint( ext.XMax, ext.YMax ) );

      assert( p_xmin_ymin.X = p_xmin_ymax.X ) ;
      assert( p_xmax_ymin.X = p_xmax_ymax.X ) ;
      assert( p_xmin_ymin.Y = p_xmin_ymin.Y ) ;
      assert( p_xmax_ymax.Y = p_xmax_ymax.Y ) ;

      ext := GisExtent( p_xmin_ymin.X, p_xmin_ymin.Y, p_xmax_ymax.X, p_xmax_ymax.Y ) ;

      assert( ext.XMin < GIS_MAX_SINGLE ) ;
      assert( ext.YMin < GIS_MAX_SINGLE ) ;
      assert( ext.XMax < GIS_MAX_SINGLE ) ;
      assert( ext.YMax < GIS_MAX_SINGLE ) ;

      FError := ERROR_CLEAR ;

      Result := ext ;
    end;

    function complex_case : TGIS_Extent ;
    var
      epsx : Double ;
      epsy : Double ;
      xmin : Double ;
      ymin : Double ;
      xmax : Double ;
      ymax : Double ;
      o1   : TGIS_Shape ;
      o2   : TGIS_Shape ;
      o3   : TGIS_Shape ;
      ptg  : TGIS_Point ;
      i    : Integer    ;
      any  : Boolean    ;
    begin

      o1 := nil ;
      o2 := nil ;
      o3 := nil ;
      try
        try
          o2 := TGIS_Shape( getBoundings( True ) ) ;

          if assigned( o2 ) then begin
            xmin := _extent.XMin ;
            ymin := _extent.YMin ;
            xmax := _extent.XMax ;
            ymax := _extent.YMax ;

            epsx := ( o2.Extent.XMax - o2.Extent.XMin ) * 1e-7 ;
            epsy := ( o2.Extent.YMax - o2.Extent.YMin ) * 1e-7 ;

            if _extent.XMax - _extent.XMin <= epsx then begin
              xmin := xmin - epsx ;
              xmax := xmax + epsx ;
            end;
            if _extent.YMax - _extent.YMin <= epsy then begin
              ymin := ymin - epsy ;
              ymax := ymax + epsy ;
            end;

            o1 := TGIS_Shape(
                    getExtentPolygon( GisExtent( xmin, ymin, xmax, ymax ) )
                  ) ;

            o3 := o2.Intersection( o1, True ) ;
          end ;
        finally
          FreeObject( o1 ) ;
          FreeObject( o2 ) ;
        end ;

        Result := GisExtent(  GIS_MAX_DOUBLE,  GIS_MAX_DOUBLE ,
                             -GIS_MAX_DOUBLE, -GIS_MAX_DOUBLE
                           )   ;

        any := False ;
        if assigned( o3 ) then begin
          for i:= 0 to o3.GetPartSize( 0 ) - 1 do begin
            ptg := self.FromWGS( o3.GetPoint( 0, i ) ) ;

            if ptg.X < GIS_MAX_SINGLE then begin
              any := True ;

              Result := GisExtent( Min( Result.XMin, ptg.X ),
                                   Min( Result.YMin, ptg.Y ),
                                   Max( Result.XMax, ptg.X ),
                                   Max( Result.YMax, ptg.Y )
                                 ) ;
            end ;
          end ;
        end ;
      finally
        FreeObject( o3 ) ;
      end ;

      if not any then
        FError := ERROR_NOBOUNDING
      else
        FError := ERROR_CLEAR ;
     end ;

  begin
    if Projection.IsPureCylindrical then
      Result := trivial_case
    else
      Result := complex_case ;
  end ;
{$ENDIF}

{$IFDEF GIS_BASIC_PROJECTION}
  {$IFDEF JAVA}{$WARNING '### Verify JAVA code'}{$ENDIF}
  function TGIS_CSProjectedCoordinateSystem.ExtentToWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;
  begin
  end ;
{$ELSE}

  function TGIS_CSProjectedCoordinateSystem.ExtentToWGS(
    const _extent : TGIS_Extent
  ) : TGIS_Extent ;

    function trivial_case : TGIS_Extent ;
    var
      ext         : TGIS_Extent ;
      p_xmin_ymin : TGIS_Point ;
      p_xmin_ymax : TGIS_Point ;
      p_xmax_ymin : TGIS_Point ;
      p_xmax_ymax : TGIS_Point ;
    begin
      ext := Projection.ValidityExtentWGS ;

      p_xmin_ymin := FromWGS( GisPoint( ext.XMin, ext.YMin ) );
      p_xmin_ymax := FromWGS( GisPoint( ext.XMin, ext.YMax ) );
      p_xmax_ymin := FromWGS( GisPoint( ext.XMax, ext.YMin ) );
      p_xmax_ymax := FromWGS( GisPoint( ext.XMax, ext.YMax ) );

      assert( p_xmin_ymin.X = p_xmin_ymax.X ) ;
      assert( p_xmax_ymin.X = p_xmax_ymax.X ) ;
      assert( p_xmin_ymin.Y = p_xmin_ymin.Y ) ;
      assert( p_xmax_ymax.Y = p_xmax_ymax.Y ) ;

      ext := GisExtent( p_xmin_ymin.X, p_xmin_ymin.Y, p_xmax_ymax.X, p_xmax_ymax.Y ) ;

      assert( ext.XMin < GIS_MAX_SINGLE ) ;
      assert( ext.YMin < GIS_MAX_SINGLE ) ;
      assert( ext.XMax < GIS_MAX_SINGLE ) ;
      assert( ext.YMax < GIS_MAX_SINGLE ) ;

      ext := GisCommonExtent( ext, _extent ) ;

      p_xmin_ymin := ToWGS( GisPoint( ext.XMin, ext.YMin ) );
      p_xmin_ymax := ToWGS( GisPoint( ext.XMin, ext.YMax ) );
      p_xmax_ymin := ToWGS( GisPoint( ext.XMax, ext.YMin ) );
      p_xmax_ymax := ToWGS( GisPoint( ext.XMax, ext.YMax ) );

      ext := GisExtent( p_xmin_ymin.X, p_xmin_ymin.Y, p_xmax_ymax.X, p_xmax_ymax.Y ) ;

      FError := ERROR_CLEAR ;

      Result := ext ;
    end;

    function complex_case : TGIS_Extent ;
    var
      epsx : Double ;
      epsy : Double ;
      xmin : Double ;
      ymin : Double ;
      xmax : Double ;
      ymax : Double ;
      o1   : TGIS_Shape ;
      o2   : TGIS_Shape ;
      o3   : TGIS_Shape ;
      ptg  : TGIS_Point ;
      i    : Integer    ;
      any  : Boolean    ;
    begin
      o1 := nil ;
      o2 := nil ;
      o3 := nil ;
      try
        try

          o2 := TGIS_Shape( getBoundings( False ) ) ;

          if assigned( o2 ) then begin
            xmin := _extent.XMin ;
            ymin := _extent.YMin ;
            xmax := _extent.XMax ;
            ymax := _extent.YMax ;

            epsx := ( o2.Extent.XMax - o2.Extent.XMin ) * 1e-7 ;
            epsy := ( o2.Extent.YMax - o2.Extent.YMin ) * 1e-7 ;

            if _extent.XMax - _extent.XMin <= epsx then begin
              xmin := xmin - epsx ;
              xmax := xmax + epsx ;
            end;
            if _extent.YMax - _extent.YMin <= epsy then begin
              ymin := ymin - epsy ;
              ymax := ymax + epsy ;
            end;

            o1 := TGIS_Shape(
                    getExtentPolygon( GisExtent( xmin, ymin, xmax, ymax ) )
                  ) ;

            o3 := o2.Intersection( o1, True ) ;
          end ;
        finally
          FreeObject( o1 ) ;
          FreeObject( o2 ) ;
        end ;

        Result := GisExtent(  GIS_MAX_DOUBLE,  GIS_MAX_DOUBLE,
                             -GIS_MAX_DOUBLE, -GIS_MAX_DOUBLE
                           )   ;

        any := False ;
        if assigned( o3 ) then begin
          for i:= 0 to o3.GetPartSize( 0 ) - 1 do begin
            ptg := self.ToWGS( o3.GetPoint( 0, i ) ) ;

            if ptg.X < GIS_MAX_SINGLE then begin
              any := True ;

              Result := GisExtent( Min( Result.XMin, ptg.X ),
                                   Min( Result.YMin, ptg.Y ),
                                   Max( Result.XMax, ptg.X ),
                                   Max( Result.YMax, ptg.Y )
                                 ) ;
            end ;
            FError := ERROR_CLEAR ;
          end ;
        end ;
      finally
        FreeObject( o3 ) ;
      end ;

      if not any then
        FError := ERROR_NOBOUNDING
      else
        FError := ERROR_CLEAR ;
    end ;

  begin
    FError := ERROR_CLEAR ;

    if Projection.IsPureCylindrical then
      Result := trivial_case
    else
      Result := complex_case ;
  end ;
{$ENDIF}

//------------------------------------------------------------------------------
// TGIS_CSUnknownCoordinateSystem
//------------------------------------------------------------------------------

  procedure TGIS_CSUnknownCoordinateSystem.Assign(
    const _source : TObject
  ) ;
  begin
    assert( _source is TGIS_CSUnknownCoordinateSystem ) ;

    inherited ;
  end;

  function TGIS_CSUnknownCoordinateSystem.ToGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSUnknownCoordinateSystem.ToGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSUnknownCoordinateSystem.FromGeocs(
    const _coords : TGIS_Point
  ) : TGIS_Point ;
  begin
    Result := _coords ;
  end ;

  function TGIS_CSUnknownCoordinateSystem.FromGeocs3D(
    const _coords : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    Result := _coords ;
  end ;

  procedure TGIS_CSUnknownCoordinateSystem.ToWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_CSUnknownCoordinateSystem.FromWGS3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _coords : TGIS_Point3D
  ) ;
  begin
    // do nothing
  end ;

  function  TGIS_CSUnknownCoordinateSystem.ExtentFromWGS(
     const _extent : TGIS_Extent
   ) : TGIS_Extent ;
  begin
     Result := GisExtent( RadToDeg( _extent.XMin ),
                          RadToDeg( _extent.YMin ),
                          RadToDeg( _extent.XMax ),
                          RadToDeg( _extent.YMax )
                        ) ;
  end ;

  function  TGIS_CSUnknownCoordinateSystem.ExtentToWGS(
     const _extent : TGIS_Extent
   ) : TGIS_Extent ;
  begin
     Result := GisExtent( DegToRad( _extent.XMin ),
                          DegToRad( _extent.YMin ),
                          DegToRad( _extent.XMax ),
                          DegToRad( _extent.YMax )
                        ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSGeographicCoordinateSystemList
//------------------------------------------------------------------------------

  constructor TGIS_CSGeographicCoordinateSystemList.Create ;
  begin
    inherited Create( cs_GeographicCoordinateSystemList, True, False ) ;
    if not assigned( cs_GeographicCoordinateSystemList ) then
      cs_GeographicCoordinateSystemList := self ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.Add(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  begin
    Result := Add(
                _epsg,
                _wkt,
                _datum,
                _prime_meridian,
                _units,
                True,
                0
              ) ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.Add(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  begin
    Result := TGIS_CSGeographicCoordinateSystem.Create(
                _epsg,
                _wkt,
                _datum,
                _prime_meridian,
                _units,
                _reversed,
                _area
              ) ;
    inherited Add( Result ) ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.Prepare(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  begin
    Result := Prepare(
                _epsg,
                _wkt,
                _datum,
                _prime_meridian,
                _units,
                True,
                0
              )
  end;

  function TGIS_CSGeographicCoordinateSystemList.Prepare(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  var
    i              : Integer ;
    wkt            : String  ;
    lookup         : Boolean ;
    tmp            : TGIS_CSAbstract ;
    datum          : Integer ;
    prime_meridian : Integer ;
    units          : Integer ;

    function check_match(
      const _obj : TGIS_CSGeographicCoordinateSystem
    ) : Boolean ;
    begin
      Result := assigned( _obj.Datum                       ) and
                assigned( _obj.PrimeMeridian               ) and
                assigned( _obj.Units                       ) and
                ( _obj.Datum.EPSG         = datum          ) and
                ( _obj.PrimeMeridian.EPSG = prime_meridian ) and
                ( _obj.Units.EPSG         = units          ) ;
    end ;

    function get_item(
      const _i : Integer
    ) : TGIS_CSGeographicCoordinateSystem ;
    begin
      if WKTObject[ _i ].MasterEPSG > 0 then
        Result := nil
      else
        Result := TGIS_CSGeographicCoordinateSystem( WKTObject[ i ] ) ;
    end ;

    function add_item : TGIS_CSGeographicCoordinateSystem ;
    begin
      if IsStringEmpty( wkt ) then
        wkt := 'Custom_GCS' ;
      Result := Add( -1, wkt, datum, prime_meridian, units, _reversed, _area ) ;
    end ;

  begin
    LockThread ;
    try

      datum := _datum ;
      tmp := CSDatumList.ByEPSG( datum ) ;
      if assigned( tmp ) then
        datum := tmp.EPSG ;

      prime_meridian := _prime_meridian ;
      tmp := CSPrimeMeridianList.ByEPSG( prime_meridian ) ;
      if assigned( tmp ) then
        prime_meridian := tmp.EPSG ;

      units := _units ;
      tmp := CSUnitsList.ByEPSG( units ) ;
      if assigned( tmp ) then
        units := tmp.EPSG ;

      lookup := True ;

      wkt := CanonicalWKT( _wkt ) ;

      Result := nil;

      if _epsg > 0 then
        Result := ByEPSG( _epsg ) ;

      if not IsStringEmpty( _wkt ) then begin
        if not assigned( Result ) then
          Result := ByWKT( wkt ) ;

        if not assigned( Result ) then begin
          // ESRI like names
          if Pos( 'GCS_', UpperCase( wkt ) ) = StringFirst then
            Result := TGIS_CSGeographicCoordinateSystem(ByWKT( Copy( wkt, StringFirst+4, 8192 ) ))
          else
            Result := ByWKT( 'GCS_' + wkt ) ;
        end ;
      end ;

      if assigned( Result ) then begin
        // check parameters
        if not check_match( Result ) then
          lookup := True
        else
          lookup := False ;
      end ;

      if lookup then begin
        Result := nil ;

        // find best matching geogcs by parameters
        for i:=0 to Count -1 do begin
          Result := get_item( i ) ;
          if not assigned( Result ) then
            continue ;

          if check_match( Result ) then
            break ;

          Result := nil ;
        end ;

        if not assigned( Result ) then begin
          // can we identify by EPSG?
          Result := ByEPSG( _epsg ) ;

          // create new
          if not assigned( Result ) then begin
            Result := add_item ;
          end ;

          assert( assigned( Result ) ) ;
        end ;
      end ;
    finally
      UnlockThread ;
    end ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.Fix(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _datum          : Integer ;
    const _prime_meridian : Integer ;
    const _units          : Integer ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  var
    o : TGIS_CSGeographicCoordinateSystem ;
  begin
    LockThread ;
    try
      o := TGIS_CSGeographicCoordinateSystem.Create(
             _epsg,
             uniqueWkt( _epsg,_wkt ),
             _datum,
             _prime_meridian,
             _units,
             _reversed,
             _area
           ) ;
      try
        Result := TGIS_CSGeographicCoordinateSystem( inherited Fix( o ) ) ;
      finally
        FreeObject( o ) ;
      end;
    finally
      UnlockThread ;
    end ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.ByEPSG(
    const _epsg : Integer
  ) : TGIS_CSGeographicCoordinateSystem ;
  begin
    Result := TGIS_CSGeographicCoordinateSystem( inherited ByEPSG( _epsg ) ) ;
  end ;

  function TGIS_CSGeographicCoordinateSystemList.ByWKT(
    const _wkt : String
  ) : TGIS_CSGeographicCoordinateSystem ;
  begin
    Result := TGIS_CSGeographicCoordinateSystem( inherited ByWKT( _wkt ) ) ;
  end ;

  procedure TGIS_CSGeographicCoordinateSystemList.Init ;
    {$INCLUDE CsData/GisCsGeogcs.inc}
    {$INCLUDE CsData/GisCsGeogcs2.inc}
    {$INCLUDE CsData/GisCsGeogcsEx.inc}
  begin
    Clear ;
    Init_CSGeographicCoordinateSystemList  ( self ) ;
    Init_CSGeographicCoordinateSystemListEx( self ) ;
    Init_CSGeographicCoordinateSystemList2 ( self ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSProjectedCoordinateSystemList
//------------------------------------------------------------------------------

  constructor TGIS_CSProjectedCoordinateSystemList.Create ;
  begin
    inherited Create( cs_ProjectedCoordinateSystemList, True, False ) ;
    if not assigned( cs_ProjectedCoordinateSystemList ) then
      cs_ProjectedCoordinateSystemList := self ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.Add(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters
  ) : TGIS_CSProjectedCoordinateSystem ;
  begin
    Result := Add(
                _epsg,
                _wkt,
                _geogcs,
                _units,
                _projection,
                _parameters,
                False,
                0
              ) ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.Add(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSProjectedCoordinateSystem ;
  begin
    Result := TGIS_CSProjectedCoordinateSystem.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _geogcs,
                _units,
                _projection,
                _parameters,
                _reversed,
                _area
              ) ;
    inherited Add( Result ) ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.Prepare(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters
  ) : TGIS_CSProjectedCoordinateSystem ;
  begin
    Result := Prepare(
                _epsg,
                _wkt,
                _geogcs,
                _units,
                _projection,
                _parameters,
                False,
                0
              ) ;
  end;

  function TGIS_CSProjectedCoordinateSystemList.Prepare(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSProjectedCoordinateSystem ;
  var
    i          : Integer ;
    wkt        : String  ;
    lookup     : Boolean ;
    tmp        : TGIS_CSAbstract ;
    geogcs     : Integer ;
    units      : Integer ;
    projection : Integer ;

    function check_match(
      const _obj : TGIS_CSProjectedCoordinateSystem
    ) : Boolean ;
    var
      primem : Double              ;
      newprj : TGIS_CSProjAbstract ;
    begin
      // use _obj.Projection.EPSG instead of _obj.ProjectionEPSG to
      // properly resolve aliases
      Result := assigned( _obj.Geocs                ) and
                assigned( _obj.Units                ) and
                assigned( _obj.Projection           ) and
                ( _obj.Geocs.EPSG     = geogcs      ) and
                ( _obj.Units.EPSG     = units       ) and
                (_obj.Projection.EPSG = _projection ) ;

      if not Result then
        exit ;

      newprj := CSProjList.ByEPSG( _projection ) ;
      if not assigned( newprj ) then begin
        Result := False ;
        exit ;
      end ;
      primem := _obj.Geocs.PrimeMeridian.Longitude ;

      if _obj.ProjectionParamsSet <> newprj.ParametersSet then begin
        Result := False ;
        exit ;
      end ;

      if InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.CentralMeridian - primem -
                         _parameters.CentralMeridian
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LatitudeOfOrigin -
                         _parameters.LatitudeOfOrigin
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.FalseEasting,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.FalseEasting -
                         _parameters.FalseEasting
                       )
                    < 1E-2
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.FalseNorthing,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.FalseNorthing -
                         _parameters.FalseNorthing
                       )
                    < 1E-2
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_1,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.StandardParallel_1 -
                         _parameters.StandardParallel_1
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.StandardParallel_2,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.StandardParallel_2 -
                         _parameters.StandardParallel_2
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.PseudoStandardParallel_1,
                               _obj.ProjectionParamsSet
                             )
      then
        Result  := Result and
                   ( Abs( _obj.ProjectionParams.PseudoStandardParallel_1 -
                          _parameters.PseudoStandardParallel_1
                         )
                     < 1E-7
                   ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.Zone,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.Zone -
                         _parameters.Zone
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.ScaleFactor,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.ScaleFactor -
                         _parameters.ScaleFactor
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LongitudeOfCenter - primem -
                         _parameters.LongitudeOfCenter
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LatitudeOfCenter -
                         _parameters.LatitudeOfCenter
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.Azimuth,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.Azimuth -
                         _parameters.Azimuth
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_1,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LongitudeOfPoint_1 - primem -
                         _parameters.LongitudeOfPoint_1
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_1,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LatitudeOfPoint_1 -
                         _parameters.LatitudeOfPoint_1
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfPoint_2,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LongitudeOfPoint_2 - primem -
                         _parameters.LongitudeOfPoint_2
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfPoint_2,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.LatitudeOfPoint_2 -
                         _parameters.LatitudeOfPoint_2
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.XScale,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.XScale -
                         _parameters.XScale
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.YScale,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.YScale -
                         _parameters.YScale
                       )
                    < 1E-7
                  ) ;

      if InCSProjParameterSet( TGIS_CSProjParameter.XYPlaneRotation,
                               _obj.ProjectionParamsSet
                             )
      then
        Result := Result and
                  ( Abs( _obj.ProjectionParams.XYPlaneRotation -
                         _parameters.XYPlaneRotation
                       )
                    < 1E-7
                  ) ;

    end ;

    function get_item(
      const _i : Integer
    ) : TGIS_CSProjectedCoordinateSystem ;
    begin
      if WKTObject[ _i ].MasterEPSG > 0 then
        Result := nil
      else
        Result := TGIS_CSProjectedCoordinateSystem( WKTObject[ i ] ) ;
    end ;

    function add_item : TGIS_CSProjectedCoordinateSystem ;
    begin
      if IsStringEmpty( wkt ) then
        wkt := 'Custom_PCS' ;
      Result := Add( -1, wkt, geogcs, units, projection, _parameters ) ;
    end ;

  begin
    LockThread ;
    try

      geogcs := _geogcs ;
      tmp := CSDatumList.ByEPSG( geogcs ) ;
      if assigned( tmp ) then
        geogcs := tmp.EPSG ;

      projection := _projection ;
      tmp := CSProjList.ByEPSG( projection ) ;
      if assigned( tmp ) then
        projection := tmp.EPSG ;

      units := _units ;
      tmp := CSUnitsList.ByEPSG( units ) ;
      if assigned( tmp ) then
        units := tmp.EPSG ;

      lookup := True ;

      wkt := CanonicalWKT( _wkt ) ;

      Result := nil;

      if _epsg > 0 then
        Result := ByEPSG( _epsg ) ;

      if not IsStringEmpty( _wkt ) then begin
        if not assigned( Result ) then
          Result := ByWKT( wkt ) ;

        if not assigned( Result ) then begin
          // ESRI like names
          if Pos( 'PCS_', UpperCase( wkt ) ) = StringFirst then
            Result := TGIS_CSProjectedCoordinateSystem(ByWKT( Copy( wkt, StringFirst+4, 8192 ) ))
          else
            Result := ByWKT( 'PCS_' + wkt ) ;
        end ;
      end ;

      if assigned( Result ) then begin
        // check parameters
        if not check_match( Result ) then
          lookup := True
        else
          lookup := False ;
      end ;

      if lookup then begin
        Result := nil ;

        // find best matching unit by parameters
        for i:=0 to Count -1 do begin
          Result := get_item( i ) ;
          if not assigned( Result ) then
            continue ;

          if check_match( Result ) then
            break ;

          Result := nil ;
        end ;

        if not assigned( Result ) then begin
          // can we identify by EPSG?
          Result := ByEPSG( _epsg ) ;

          // create new
          if not assigned( Result ) then begin
            Result := add_item ;
          end ;

          assert( assigned( Result ) ) ;
        end ;
      end ;
    finally
      UnlockThread ;
    end ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.Fix(
    const _epsg           : Integer ;
    const _wkt            : String  ;
    const _geogcs         : Integer ;
    const _units          : Integer ;
    const _projection     : Integer ;
    const _parameters     : TGIS_CSProjParameters ;
    const _reversed       : Boolean ;
    const _area           : Integer
  ) : TGIS_CSProjectedCoordinateSystem ;
  var
    o : TGIS_CSProjectedCoordinateSystem ;
  begin
    LockThread ;
    try
      o := TGIS_CSProjectedCoordinateSystem.Create(
             _epsg,
             uniqueWkt( _epsg,_wkt ),
             _geogcs,
             _units,
             _projection,
             _parameters,
             _reversed,
             _area
           ) ;
      try
        Result := TGIS_CSProjectedCoordinateSystem( inherited Fix( o ) ) ;
      finally
        FreeObject( o ) ;
      end;
    finally
      UnlockThread ;
    end ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.ByEPSG(
    const _epsg : Integer
  ) : TGIS_CSProjectedCoordinateSystem ;
  begin
    Result := TGIS_CSProjectedCoordinateSystem( inherited ByEPSG( _epsg ) ) ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.ByWKT(
    const _wkt : String
  ) : TGIS_CSProjectedCoordinateSystem ;
  begin
    Result := TGIS_CSProjectedCoordinateSystem( inherited ByWKT( _wkt ) ) ;
  end ;

  function TGIS_CSProjectedCoordinateSystemList.EmptyParams
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
  end;

  function TGIS_CSProjectedCoordinateSystemList.DefaultParams(
    const _proj_epsg : Integer
  ) : TGIS_CSProjParameters ;
  var
    prj : TGIS_CSProjAbstract ;
  begin
    Result := EmptyParams ;

    prj := CSProjList.ByEPSG( _proj_epsg ) ;

    if not assigned( prj ) then exit ;

    Result := prj.Parameters ;
  end;

  function TGIS_CSProjectedCoordinateSystemList.DefaultParamsForUTM(
    const _zone : Integer
  ) : TGIS_CSProjParameters ;
  var
    izone : Integer ;
  begin
    Result := DefaultParams( CSPROJ_Transverse_Mercator ) ;

    if Abs( _zone ) > 60 then exit ;
    if _zone       =  0  then exit ;

    izone := Abs( _zone ) - 1 ;

    if _zone < 0 then Result.FalseNorthing := 10000000   // south
                 else Result.FalseNorthing := 0        ; // north
    Result.FalseEasting := 500000 ;

    Result.CentralMeridian  := ( izone + 0.5 ) * Pi / 30 - Pi ;
    Result.LatitudeOfOrigin := 0      ;
    Result.ScaleFactor      := 0.9996 ;
  end;

  procedure TGIS_CSProjectedCoordinateSystemList.Init ;
  var
    pp : TGIS_CSProjParameters ;

    {$INCLUDE CsData/GisCsProjcs.inc}
    {$INCLUDE CsData/GisCsProjcs2.inc}
    {$INCLUDE CsData/GisCsProjcsEx.inc}

  begin
    Clear ;
    {$IFDEF GIS_NORECORDS}
      pp := new TGIS_CSProjParameters ;
    {$ENDIF}
    Init_CSProjectedCoordinateSystemList  ( self, pp ) ;
    Init_CSProjectedCoordinateSystemListEx( self, pp ) ;
    Init_CSProjectedCoordinateSystemList2 ( self, pp ) ;
  end ;

//------------------------------------------------------------------------------
// Global lists
//------------------------------------------------------------------------------

  function CSGeographicCoordinateSystemList
    : TGIS_CSGeographicCoordinateSystemList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_GeographicCoordinateSystemList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_GeographicCoordinateSystemList ) then
            cs_GeographicCoordinateSystemList
              := TGIS_CSGeographicCoordinateSystemList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_GeographicCoordinateSystemList ;
  end ;

  function CSProjectedCoordinateSystemList
    : TGIS_CSProjectedCoordinateSystemList ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_ProjectedCoordinateSystemList ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_ProjectedCoordinateSystemList ) then
            cs_ProjectedCoordinateSystemList
              := TGIS_CSProjectedCoordinateSystemList.Create ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_ProjectedCoordinateSystemList ;
  end ;

  function CSUnknownCoordinateSystem
    : TGIS_CSUnknownCoordinateSystem ;
  var
    thc : TGIS_ThreadClass ;
  begin
    if not assigned( cs_UnknownCoordinateSystem ) then begin
      thc := TGIS_ThreadClass.Create ;
      try
        thc.LockThread ;
        try
          if not assigned( cs_UnknownCoordinateSystem ) then
            cs_UnknownCoordinateSystem
              := TGIS_CSUnknownCoordinateSystem.Create( 0, 'UNKNOWN' )  ;
        finally
          thc.UnlockThread ;
        end;
      finally
        FreeObject( thc );
      end;
    end;

    Result := cs_UnknownCoordinateSystem ;
  end ;

//------------------------------------------------------------------------------
// Tests
//------------------------------------------------------------------------------
  {$IFNDEF OXYGENE}
    {$IFDEF DEBUG1}
    procedure check_structure ;
      procedure check_geogcs ;
      var
        i : Integer ;
        cs : TGIS_CSGeographicCoordinateSystem ;
      begin
        for i := 1 to CSGeographicCoordinateSystemList.Count - 1 do begin
          cs := TGIS_CSGeographicCoordinateSystem(
                 CSGeographicCoordinateSystemList[ i ]
                ) ;

          if cs.MasterEPSG > 0 then continue ;

          assert( cs.EPSG <> 0,
                  Format( 'GEOGCS =%d has no EPSG assigned',
                          [ i ]
                        )
                ) ;
          assert( not IsStringEmpty( cs.WKT ),
                  Format( 'GEOGCS EPSG=%d has no WKT assigned',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( assigned( cs.Datum ),
                  Format( 'GEOGCS EPSG=%d has no Datum assigned',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( assigned( cs.PrimeMeridian ),
                  Format( 'GEOGCS EPSG=%d has no PrimeMeridian',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( assigned( cs.Units ),
                  Format( 'GEOGCS EPSG=%d has no units assigned',
                          [ cs.EPSG ]
                        )
                ) ;
        end ;
      end ;

      procedure check_projcs ;
      var
        i : Integer ;
        cs : TGIS_CSProjectedCoordinateSystem ;
      begin
        for i := 1 to CSProjectedCoordinateSystemList.Count - 1 do begin
          cs := TGIS_CSProjectedCoordinateSystem(
                 CSProjectedCoordinateSystemList[ i ]
                ) ;

          if cs.MasterEPSG > 0 then continue ;

          assert( cs.EPSG <> 0,
                  Format( 'PROJCS =%d has no EPSG assigned',
                          [ i ]
                        )
                ) ;
          assert( not IsStringEmpty( cs.WKT ),
                  Format( 'PROJCS EPSG=%d has no WKT assigned',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( assigned( cs.Geocs ),
                  Format( 'PROJCS EPSG=%d has no Geocs assigned',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( cs.ProjectionEPSG > 0,
                  Format( 'PROJCS EPSG=%d has no Projection',
                          [ cs.EPSG ]
                        )
                ) ;
          assert( assigned( cs.Units ),
                  Format( 'PROJCS EPSG=%d has no units assigned',
                          [ cs.EPSG ]
                        )
                ) ;
        end ;
      end ;

      procedure check_esri_map ;
      var
        {$IFNDEF OXYGENE}
          itm : TGIS_CSAbstract ;
        {$ENDIF}
        res1 : TGIS_CSAbstract ;
        res2 : TGIS_CSAbstract ;
        epsg : Integer ;
      begin
        for itm {$IFDEF OXYGENE}: TGIS_CSAbstract {$ENDIF}
            in esriGeocsMapList do
        begin
          res1 := CSGeographicCoordinateSystemList.ByWKT( Copy( itm.WKT, 5, 1000 ) ) ;
          assert( assigned( res1 ),
                  Format( 'ESRI GEOGCS %d not a reversal name',
                          [ itm.EPSG ]
                        )
               ) ;
          if res1.MasterEPSG > 0 then
            epsg := res1.MasterEPSG
          else
            epsg := res1.EPSG ;

          assert( epsg = itm.EPSG,
                  Format( 'ESRI GEOGCS %d not a unique reversal',
                          [ itm.EPSG ]
                        )
                ) ;
        end;

        for itm {$IFDEF OXYGENE}: TGIS_CSAbstract {$ENDIF}
            in esriDatumMapList do
        begin
          res1 := CSDatumList.ByWKT( Copy( itm.WKT, 3, 1000 ) ) ;
          assert( assigned( res1 ),
                   Format( 'ESRI DATUM %d not a reversal name',
                           [ itm.EPSG ]
                         )
                ) ;
          assert( assigned( res1 ), itm.WKT ) ;

          if res1.MasterEPSG > 0 then
            epsg := res1.MasterEPSG
          else
            epsg := res1.EPSG ;

          if epsg <> itm.EPSG then begin
            res2 := esriDatumMapList.ByEPSG( res1.EPSG ) ;
            assert( assigned( res2 ), itm.WKT ) ;

            if res2.EPSG <> res1.EPSG then begin
              assert( False,
                      Format( 'ESRI DATUM %d not a unique reversal',
                              [ itm.EPSG ]
                            )
                    ) ;
            end;
          end;
        end;

      end;

    begin
      check_geogcs ;
      check_projcs ;
      check_esri_map ;
    end ;

    {$ENDIF}
  {$ENDIF}

//------------------------------------------------------------------------------
// Initialization
//------------------------------------------------------------------------------

{$IFDEF DCC}
  initialization
    {$IFDEF DEBUG1}
      check_structure ;
    {$ENDIF}

  finalization
    FreeObject( cs_GeographicCoordinateSystemList ) ;
    FreeObject( cs_ProjectedCoordinateSystemList  ) ;
    FreeObject( cs_UnknownCoordinateSystem        ) ;
    FreeObject( cs_esriDatumsMap                  ) ;
    FreeObject( cs_esriGeocsMap                   ) ;
{$ENDIF}

{==================================== END =====================================}
end.

