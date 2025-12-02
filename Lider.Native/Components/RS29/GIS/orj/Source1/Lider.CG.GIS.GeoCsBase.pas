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
  Units, prime meridians, ellipsoids, datums and geographic coordinate system
  definitions for
  Coordinate System. This unit also contains few common types and classes.

  General structure was based on EPSG/POSG numbering. However due lack of
  number of definitions, the original EPSG numbering was extended:
  numbers 100000..199999 represents persistent elements added by TatukGIS;
  numbers 7000000..2147483647  represent dynamically assigned EPSG numbers;
  all other numbers are available for user defined elements.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCsBase ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCsBase"'}
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
    System.Classes,
    System.Math,
    System.SyncObjs,
    System.Generics.Collections,
    System.Generics.Defaults,

    {$IFDEF LEVEL_XE3_RTL}
      System.Types,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoStreams;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Collections,
    System.Collections.Generic,
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
  TGIS_CSAbstract                            = class ;
  TGIS_CSAbstractList                        = class ;

  TGIS_CSUnits                               = class ;
  TGIS_CSPrimeMeridian                       = class ;
  TGIS_CSEllipsoid                           = class ;
  TGIS_CSArea                                = class ;
  TGIS_CSDatum                               = class ;
  TGIS_CSTransformAbstract                   = class ;
  TGIS_CSTransformGridShift                  = class ;
  TGIS_CSTransformGeocentricTranslations     = class ;
  TGIS_CSTransformPositionVector7Params      = class ;
  TGIS_CSTransformCoordinateFrameRotation    = class ;
  TGIS_CSTransformGeographicalAndHighOffsets = class ;
  TGIS_CSTransformGeographicalOffsets        = class ;
  TGIS_CSTransformMolodenskiBadekas          = class ;
  TGIS_CSTransformNadcon                     = class ;
  TGIS_CSTransformNTV2                       = class ;

  TGIS_CSUnitsList                           = class ;
  TGIS_CSPrimeMeridianList                   = class ;
  TGIS_CSEllipsoidList                       = class ;
  TGIS_CSAreaList                            = class ;
  TGIS_CSDatumList                           = class ;
  TGIS_CSTransformList                       = class ;

  /// <summary>
  ///   Base class for any WKT units
  /// </summary>
  TGIS_CSAbstract = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )

    // properties internal values
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      /// <summary>
      ///   EPSG code of the object.
      /// </summary>
      FEPSG : Integer  ;

      /// <summary>
      ///   EPSG code of the object for which current object is an alias.
      /// </summary>
      FMasterEPSG : Integer  ;

      /// <summary>
      ///   WKT name of an object.
      /// </summary>
      FWKT : String ;

      /// <summary>
      ///   Descriptive name; for internal use of TGIS_CSAbstractListHelper.
      /// </summary>
      FDescriptionEx : String ;

      /// <summary>
      ///   If true, then object is deprecated.
      /// </summary>
      FDeprecated : Boolean ;


    protected // properties internal values

      function fget_Description         : String ; virtual;
      function fget_FriendlyName        : String ; virtual;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create                ; overload;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      constructor Create                ( const _epsg   : Integer ;
                                          const _wkt    : String
                                        ) ; overload;

      /// <summary>
      ///   Copy content of the provided object into a current object
      /// </summary>
      /// <param name="_source">
      ///   object form which content will be copied
      /// </param>
      procedure   Assign                ( const _source : TObject
                                        ) ; virtual;

      /// <summary>
      ///   Set object depreciation state.
      /// </summary>
      /// <param name="_deprecated">
      ///   if true
      /// </param>
      procedure   MarkDeprecated        ( const _deprecated : Boolean
                                        ) ; virtual;

      {$IFDEF CLR}
        /// <inheritdoc/>
        function ToString               : String ; override;
      {$ENDIF}

    public // standard properties

      /// <summary>
      ///   EPSG code.
      /// </summary>
      property EPSG               : Integer
                                  read FEPSG ;

      /// <summary>
      ///   EPSG code of the object for which current object is an alias.
      /// </summary>
      property MasterEPSG         : Integer
                                  read FMasterEPSG ;

      /// <summary>
      ///   WKT name.
      /// </summary>
      property WKT                : String
                                  read FWKT ;

      /// <summary>
      ///   Additional description.
      /// </summary>
      property Description        : String
                                  read fget_Description ;



      /// <summary>
      ///   Descriptive name; for internal use of TGIS_CSAbstractListHelper.
      /// </summary>
      property DescriptionEx    : String
                                read  FDescriptionEx
                                write FDescriptionEx ;


      /// <summary>
      ///   Friendly name constructed based on WKT (without underscores) and
      ///   EPSG.
      /// </summary>
      property FriendlyName       : String
                                  read fget_FriendlyName ;

      /// <summary>
      ///   If true, then object is deprecated.
      /// </summary>
      property &Deprecated        : Boolean
                                  read FDeprecated ;
  end ;

  /// <summary>
  ///   Enumerator for TGIS_CSAbstractList.
  /// </summary>
  TGIS_CSAbstractListEnumerator = {$IFDEF OXYGENE} public {$ENDIF} class( TObject
                                      {$IFDEF CLR} , IEnumerator<TObject> {$ENDIF}
                                      {$IFDEF JAVA}, java.util.Iterator<TObject> {$ENDIF}
                                     )
    private
      FList  : TGIS_CSAbstractList ;
      FIndex : Integer ;
    private

      {$IFDEF CLR}
        function   fget_current_obj : TObject ;
      {$ENDIF}

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_list">
      ///   list handle
      /// </param>
      constructor Create         ( const _list : TGIS_CSAbstractList
                                 ) ;

      {$IFDEF CLR}

        /// <summary>
        ///   Destroy an object.
        /// </summary>
        procedure  Dispose       ;
      {$ENDIF}

      /// <summary>
      ///   Reset enumerator.
      /// </summary>
      procedure  Reset           ;

      /// <summary>
      ///   Move next.
      /// </summary>
      /// <returns>
      ///   If false then there is no more objects.
      /// </returns>
      function   MoveNext        : Boolean ;

    {$IFDEF GENXDK}
      protected
    {$ENDIF}

      /// <summary>
      ///   Property Current access function.
      /// </summary>
      /// <returns>
      ///    Shape itself or nil.
      /// </returns>
      function   GetCurrent      : TGIS_CSAbstract ;

    public

      {$IFDEF JAVA}
        /// <summary>
        ///   Java specific enumerator support method.
        /// </summary>
        /// <returns>
        ///   Java specific enumerator return value.
        /// </returns>
        method hasNext : Boolean ;

        /// <summary>
        ///   Java specific enumerator support method.
        /// </summary>
        /// <returns>
        ///   Java specific enumerator return value.
        /// </returns>
        method next : TObject;

        /// <summary>
        ///   Java specific enumerator support method.
        /// </summary>
        /// <returns>
        ///   Java specific enumerator return value.
        /// </returns>
        method &remove ;
      {$ENDIF}

    public

      /// <summary>
      ///   Current enumerator value.
      /// </summary>
      {$IFNDEF CLR}
        property  Current : TGIS_CSAbstract read GetCurrent      ;
      {$ELSE}
        property  Current : TObject         read fget_current_obj ;
      {$ENDIF}
  end ;

  /// <summary>
  ///   Base class for any WKT unit list
  /// </summary>
  TGIS_CSAbstractList =
      {$IFDEF OXYGENE} public abstract {$ENDIF}
      class( TGIS_ThreadClass{$IFDEF CLR}, IEnumerable{$ENDIF} )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create       ; overload;

      /// <summary>
      ///   Create an instance and attach it to existing global list.
      /// </summary>
      /// <param name="_lst">
      ///   pointer to existing global list
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    only for internal use of TatukGIS
      ///    </note>
      /// </remarks>
      constructor Create       ( const _lst    : TGIS_CSAbstractList
                               ) ; overload;

      /// <summary>
      ///   Create an instance and attach it to existing global list.
      /// </summary>
      /// <param name="_lst">
      ///   pointer to existing global list
      /// </param>
      /// <param name="_unique_id">
      ///   if true the exception will be raised if object ID is not unique
      /// </param>
      /// <param name="_unique_wkt">
      ///   if true the exception will be raised if object WKT is not unique
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    only for internal use of TatukGIS
      ///    </note>
      /// </remarks>
      constructor Create       ( const _lst    : TGIS_CSAbstractList ;
                                 const _unique_id  : Boolean ;
                                 const _unique_wkt : Boolean
                               ) ; overload;

    private // other internal properties
      bOwn       : Boolean ;
      bUniqueId  : Boolean ;
      bUniqueWKT : Boolean ;

      /// <summary>
      ///   list by WKT
      /// </summary>
      oList      : TList< TGIS_CSAbstract > ;

      /// <summary>
      ///   list by WKT
      /// </summary>
      oListWKT   : TDictionary< String , TGIS_CSAbstract > ;

      /// <summary>
      ///   list by EPSG
      /// </summary>
      oListID    : TDictionary< Integer, TGIS_CSAbstract > ;

    protected // property access routines

      function fget_WKTObject  ( _idx : Integer
                               ) : TGIS_CSAbstract ;

    protected // other protected  routines

      /// <summary>
      ///   Generate unique WKT name
      /// </summary>
      /// <param name="_epsg">
      ///   if provided EPSG already exists then returned value will be already
      ///   existing WKT name associated with EPSG
      /// </param>
      /// <param name="_wkt">
      ///   suggested WKT name; if it is not unique the returned will be names
      ///   xyz_1, xyz_2
      /// </param>
      /// <returns>
      ///   unique WKT string.
      /// </returns>
      function  uniqueWkt      ( const _epsg   : Integer ;
                                 const _wkt    : String
                               ) : String ;

    protected

      procedure doDestroy      ; override;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_obj">
      ///   object to be added
      /// </param>
      /// <returns>
      ///   Object itself.
      /// </returns>
      function  Add            ( const _obj    : TGIS_CSAbstract
                               ) : TGIS_CSAbstract ; virtual;

      /// <summary>
      ///   Fix item by substituting existing item based on EPSG code.
      /// </summary>
      /// <param name="_obj">
      ///   matching object to be fixed
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      function  Fix            ( const _obj    : TGIS_CSAbstract
                               ) : TGIS_CSAbstract ; virtual;

      {$IFDEF GIS_IMPORTEXTRA}
        procedure ChangeEPSG   ( const _epsg   : Integer    ;
                                 const _master : Integer
                               ) ; virtual;

        procedure DeleteEPSG   ( const _min : Integer    ;
                                 const _max : Integer
                               ) ; virtual;
      {$ENDIF}

      /// <summary>
      ///   Add new item being alias to an existing one.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_master">
      ///   code of the master item
      /// </param>
      procedure AddAlias       ( const _epsg   : Integer    ;
                                 const _wkt    : String     ;
                                 const _master : Integer
                               ) ; virtual;

      /// <summary>
      ///   Define alias between two items.
      /// </summary>
      /// <param name="_epsg">
      ///   code of the object which is an alias for the master object.
      /// </param>
      /// <param name="_master">
      ///   code of the master item
      /// </param>
      procedure Alias          ( const _epsg   : Integer    ;
                                 const _master : Integer
                               ) ; virtual;

      /// <summary>
      ///   Clear the list. Delete all objects.
      /// </summary>
      procedure Clear          ;

      /// <summary>
      ///   Number of items on the list.
      /// </summary>
      /// <returns>
      ///   Number of items on the list.
      /// </returns>
      function  Count          : Integer ;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG         ( const _epsg   : Integer
                               ) : TGIS_CSAbstract ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT          ( const _wkt    : String
                               ) : TGIS_CSAbstract ; virtual;

      /// <summary>
      ///   Initialize list form definition. Call this if you want to
      ///   reconstruct list.
      /// </summary>
      procedure Init           ; virtual; abstract;

      /// <summary>
      ///   Get enumerator.
      /// </summary>
      /// <returns>
      ///   Enumerator object.
      /// </returns>
      function GetEnumerator   : {$IFDEF CLR}
                                   IEnumerator ;
                                 {$ELSE}
                                   TGIS_CSAbstractListEnumerator ;
                                 {$ENDIF}

    public // public methods

        /// <summary>
        ///   Default property for retrieving list element.
        /// </summary>
        /// <param name="_idx">
        ///   position on the list
        /// </param>
        property WKTObject[ _idx : Integer ] : TGIS_CSAbstract
                                               read fget_WKTObject ;
                                               default ;
  end ;

  /// <summary>
  ///   Type of units.
  /// </summary>
  TGIS_CSUnitsType = {$IFDEF OXYGENE} public {$ENDIF}
  (

      /// <summary>
      ///   Linear units like meters, miles.
      /// </summary>
      Linear,

      /// <summary>
      ///   Angular units like radians, degrees.
      /// </summary>
      Angular,

      /// <summary>
      ///   Areal units like hectares, ares.
      /// </summary>
      Areal,

      /// <summary>
      ///   Autoselect units like metric or US.
      /// </summary>
      Auto
  ) ;

  /// <summary>
  ///   Unit of measurement (linear, angular or areal)
  /// </summary>
  TGIS_CSUnits = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSAbstract )

    private // properties internal values

        /// <summary>
        ///   Symbolic name.
        /// </summary>
        FSymbol : String ;

        /// <summary>
        ///   Type of units.
        /// </summary>
        FUnitsType : TGIS_CSUnitsType ;

        /// <summary>
        ///   Factor related to meter or Radian.
        /// </summary>
        FFactor : Double ;

        /// <summary>
        ///   List of units in auto mode.
        /// </summary>
        arUnits : array of Integer ;

    private
       function   prepareString         ( const _unt    : TGIS_CSUnits ;
                                          const _value  : Double  ;
                                          const _prec   : Boolean ;
                                          const _format : String
                                        ) : String ;

    public // public methods

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor Create                ; overload;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_symbol">
      ///   units symbol like 'km', 'm'
      /// </param>
      /// <param name="_type">
      ///   angular, linear or areal units
      /// </param>
      /// <param name="_factor">
      ///   factor between unit and basic unit (meter or radians); for example
      ///   factor for kilometer is 1000.
      /// </param>
      constructor Create                ( const _epsg     : Integer ;
                                          const _wkt      : String  ;
                                          const _symbol   : String  ;
                                          const _type     : TGIS_CSUnitsType ;
                                          const _factor   : Double
                                        ) ; overload;

      /// <summary>
      ///   Create an instance of automatic units
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_subunits">
      ///   list of units in a set ordered by factor; if contains areal units
      ///   then order should be by squared factor of all units except areas
      /// </param>
      constructor Create                ( const _epsg     : Integer ;
                                          const _wkt      : String  ;
                                          const _subunits : array of Integer
                                        ) ; overload;

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      procedure   Assign                ( const _source   : TObject
                                        ) ; override;

      /// <summary>
      ///   Convert value provided in current unit into basic unit (meter or
      ///   radians) . Basic units are meter, radian, square meter
      /// </summary>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Value converted to base units.
      /// </returns>
      function    ToBase                ( const _value    : Double
                                        ) : Double ;

      /// <summary>
      ///   Convert value provided in basic unit (meter or radians) into the
      ///   value represented by current unit. Basic units are meter, radian,
      ///   square meter
      /// </summary>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Value converted to current units.
      /// </returns>
      function    FromBase              ( const _value    : Double
                                        ) : Double ;

      /// <summary>
      ///   Convert value provided in basic unit (meter or radians) into the
      ///   value represented by current unit.
      /// </summary>
      /// <param name="_units">
      ///   desired units system to be converted to
      /// </param>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Value converted to defined units.
      /// </returns>
      function    ToUnits               ( const _units    : TGIS_CSUnits ;
                                          const _value    : Double
                                        ) : Double ;

      /// <summary>
      ///   Convert value provided in current unit into basic unit (meter or
      ///   radians) .
      /// </summary>
      /// <param name="_units">
      ///   desired units system to be converted to
      /// </param>
      /// <param name="_value">
      ///   value to be converted
      /// </param>
      /// <returns>
      ///   Value converted to current units.
      /// </returns>
      function    FromUnits             ( const _units    : TGIS_CSUnits ;
                                          const _value    : Double
                                        ) : Double ;

      /// <summary>
      ///   Convert units as linear to string representation with proper units
      ///   symbol.
      /// </summary>
      /// <param name="_value">
      ///   units in base metric
      /// </param>
      /// <param name="_prec">
      ///   if true then fixed decimal digits will be presented; if false then
      ///   only significant digits (no trailing zeros) will be presented
      /// </param>
      /// <returns>
      ///   Units converted to string.
      /// </returns>
      /// <remarks>
      ///    Units of returned string will be autoselected to choose the most
      ///   adequate is unit is a type of TGIS_CSUnitsType.Auto.
      /// </remarks>
      function    AsLinear              ( const _value    : Double       ;
                                          const _prec     : Boolean
                                        ) : String ;

      /// <summary>
      ///   Convert units as areal to string representation with proper units
      ///   symbol.
      /// </summary>
      /// <param name="_value">
      ///   units in base metric
      /// </param>
      /// <param name="_prec">
      ///   if true then fixed decimal digits will be presented; if false then
      ///   only significant digits (no trailing zeros) will be presented
      /// </param>
      /// <param name="_format">
      ///   hint of how to present linear symbols: for example by providing
      ///   'sq. %s' linear symbols will be presented as '1 sq. km'
      /// </param>
      /// <returns>
      ///   Units converted to string.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    Linear units will be autoconverted to areal.
      ///    </note>
      ///    Units of returned string will be autoselected to choose the most
      ///   adequate is unit is a type of TGIS_CSUnitsType.Auto.
      /// </remarks>
      function    AsAreal               ( const _value    : Double       ;
                                          const _prec     : Boolean      ;
                                          const _format   : String
                                        ) : String ;

      /// <summary>
      ///   Convert units as annular to string representation with proper units
      ///   symbol.
      /// </summary>
      /// <param name="_value">
      ///   units in base metric
      /// </param>
      /// <param name="_prec">
      ///   if true then fixed decimal digits will be presented; if false then
      ///   only significant digits (no trailing zeros) will be presented
      /// </param>
      /// <returns>
      ///   Units converted to string.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    Value will always be converted to normal numeric form. So if units
      ///    are degrees then result for '15 degrees and 30 seconds' will
      ///    always be '15.50 deg'
      ///    </note>
      /// </remarks>
      function    AsAngular             ( const _value    : Double       ;
                                          const _prec     : Boolean
                                        ) : String ;

      /// <summary>
      ///   Returned the most appropriate sub unit for selected value.
      /// </summary>
      /// <param name="_areal">
      ///   if true select areal units, if false select linear.
      /// </param>
      /// <param name="_value">
      ///   units in base metric
      /// </param>
      /// <returns>
      ///   Units converted to string.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    if units type is other the TGIS_CSUnitsType.Auto then returned
      ///    value is always converted based on object properties
      ///    </note>
      ///    Units will be autoselected to choose the most adequate is unit is
      ///   a type of TGIS_CSUnitsType.Auto.
      /// </remarks>
      function    AutoSelect            ( const _areal    : Boolean      ;
                                          var   _value    : Double
                                        ) : TGIS_CSUnits ;

    public // standard properties

        /// <summary>
        ///   Symbolic name.
        /// </summary>
        property Symbol : String read FSymbol ;

        /// <summary>
        ///   Type of units: angular, linear, areal.
        /// </summary>
        property UnitsType : TGIS_CSUnitsType read FUnitsType ;

        /// <summary>
        ///   Factor between unit and basic unit (meter or radians); for
        ///   example factor for kilometer is 1000.
        /// </summary>
        property Factor : Double read FFactor ;
  end ;

  /// <summary>
  ///   Prime meridians.
  /// </summary>
  TGIS_CSPrimeMeridian = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSAbstract )

    private // properties internal values

        /// <summary>
        ///   Longitude of prime meridian.
        /// </summary>
        FLongitude : Double ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_longitude">
      ///   Longitude of prime meridian (in radians)
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _longitude : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;

    public // public properties

        /// <summary>
        ///   Longitude of prime meridian (in radians).
        /// </summary>
        property Longitude : Double read FLongitude ;
  end ;

  /// <summary>
  ///   An ellipsoid item.
  /// </summary>
  TGIS_CSEllipsoid = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSAbstract )

    private // properties internal values

        /// <summary>
        ///   Semimajor axis.
        /// </summary>
        FSemiMajor : Double ;

        /// <summary>
        ///   Semiminor axis.
        /// </summary>
        FSemiMinor   : Double ;

        /// <summary>
        ///   Flattering. Equal 0 for spheroid.
        /// </summary>
        FFlattering  : Double ;

        /// <summary>
        ///   InverseFlattering. Equal 0 for spheroid.
        /// </summary>
        FInverseFlattering  : Double ;

        /// <summary>
        ///   Square of semimajor axis. Pre-calculated for optimization purposes.
        /// </summary>
        FSemiMajorSq : Double ;

        /// <summary>
        ///   Square of semiminor axis. Pre-calculated for optimization purposes.
        /// </summary>
        FSemiMinorSq : Double ;

        /// <summary>
        ///   Eccentricity squared. Pre-calculated for optimization purposes.
        /// </summary>
        FEcntrMajor : Double ;

        /// <summary>
        ///   Eccentricity sqrt. Pre-calculated for optimization purposes.
        /// </summary>
        FEcntrMajorSqrt : Double ;

        /// <summary>
        ///   Second eccentricity squared. Pre-calculated for optimization
        ///   purposes.
        /// </summary>
        FEcntrMinor : Double ;

        /// <summary>
        ///   Eccentricity sqrt. Pre-calculated for optimization purposes.
        /// </summary>
        FEcntrMinorSqrt : Double ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the ellipsoid
      /// </param>
      /// <param name="_semi_major">
      ///   semi major axis
      /// </param>
      /// <param name="_inv_flattering">
      ///   inverse flattering; 0 for spheroid
      /// </param>
      constructor Create                ( const _epsg           : Integer ;
                                          const _wkt            : String  ;
                                          const _semi_major     : Double  ;
                                          const _inv_flattering : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source         : TObject
                                        ) ; override;

      /// <summary>
      ///   Convert point from geocentric to geodetic system.
      /// </summary>
      /// <param name="_ptg">
      ///   point in geocentric system
      /// </param>
      procedure   ToGeodetic_Ref        ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Convert point from geodetic to geocentric system.
      /// </summary>
      /// <param name="_ptg">
      ///   point in geodetic system
      /// </param>
      procedure   ToGeocentric_Ref      ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Convert coordinates from an ellipsoid base (geocentric) to geodetic
      ///   mode.
      /// </summary>
      /// <param name="_coords">
      ///   coordinates in geocentric space (meters)
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function    ToGeodetic            ( const _coords         : TGIS_Point3D
                                        ) : TGIS_Point3D ;

      /// <summary>
      ///   Convert coordinates from geodetic mode to ellipsoid base
      ///   (geocentric).
      /// </summary>
      /// <param name="_coords">
      ///   coordinates in geodetic space (radians)
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function    ToGeocentric          ( const _coords         : TGIS_Point3D
                                        ) : TGIS_Point3D ;

      /// <summary>
      ///   Calculates distance over Great Circle
      /// </summary>
      /// <param name="_ptg_a">
      ///   start point; longitude &amp; latitude must be expressed in radians
      /// </param>
      /// <param name="_ptg_b">
      ///   end point; longitude &amp; latitude must be expressed in radians
      /// </param>
      /// <returns>
      ///   Computed distance in meters.
      /// </returns>
      function    Distance              ( const _ptg_a          : TGIS_Point ;
                                          const _ptg_b          : TGIS_Point
                                        ) : Double ;

    public // standard properties

        /// <summary>
        ///   Semimajor axis.
        /// </summary>
        property SemiMajor : Double read FSemiMajor ;

        /// <summary>
        ///   Semiminor axis.
        /// </summary>
        property SemiMinor : Double read FSemiMinor ;

        /// <summary>
        ///   Flattering. Equal 0 for spheroid.
        /// </summary>
        property Flattering : Double read FFlattering ;

        /// <summary>
        ///   Inverse Flattering. Equal 0 for spheroid.
        /// </summary>
        property InverseFlattering : Double read FInverseFlattering ;

        /// <summary>
        ///   Square of semimajor axis. Pre-calculated for optimization purposes.
        /// </summary>
        property SemiMajorSq : Double read FSemiMajorSq ;

        /// <summary>
        ///   Square of semiminor axis. Pre-calculated for optimization purposes.
        /// </summary>
        property SemiMinorSq : Double read FSemiMinorSq ;

        /// <summary>
        ///   Eccentricity squared. Pre-calculated for optimization purposes.
        /// </summary>
        property EcntrMajor : Double read FEcntrMajor ;

        /// <summary>
        ///   Eccentricity sqrt. Pre-calculated for optimization purposes.
        /// </summary>
        property EcntrMajorSqrt : Double read FEcntrMajorSqrt ;

        /// <summary>
        ///   Second eccentricity squared. Pre-calculated for optimization
        ///   purposes.
        /// </summary>
        property EcntrMinor : Double read FEcntrMinor ;

        /// <summary>
        ///   Eccentricity sqrt. Pre-calculated for optimization purposes.
        /// </summary>
        property EcntrMinorSqrt : Double read FEcntrMinorSqrt ;
  end ;

  /// <summary>
  ///   Area name.
  /// </summary>
  TGIS_CSArea = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSAbstract )

    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
     FDescription : String ;
     FBounds      : TGIS_Extent ;

    protected

     function fget_Description : String ; override;

    public
     /// <summary>
     ///   Area validity bounds.
     /// </summary>
     property Bounds : TGIS_Extent read FBounds ;
  end ;

  /// <summary>
  ///   A datum type.
  /// </summary>
  TGIS_CSDatum = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_CSAbstract )

    // properties internal values
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

        /// <summary>
        ///   Ellipsoid object.
        /// </summary>
        FEllipsoid : TGIS_CSEllipsoid ;

        /// <summary>
        ///   Default Coordinate Transformation object.
        /// </summary>
        FTransform : TGIS_CSTransformAbstract ;

        /// <summary>
        ///   Fallback Coordinate Transformation object. Used if main grid shift
        ///   based transformation is not available.
        /// </summary>
        FFallback : Integer ;

    // other internal values
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
      oEllipsoidWGS : TGIS_CSEllipsoid ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      constructor Create                ( const _epsg           : Integer ;
                                          const _wkt            : String  ;
                                          const _ellipsoid      : Integer ;
                                          const _transform      : Integer
                                        ) ; overload ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      /// <param name="_fallback">
      ///   EPSG code of fallback coordinate operation (used only for grid-shift
      ///   if grid-shift file can not be used)
      /// </param>
      constructor Create                ( const _epsg           : Integer ;
                                          const _wkt            : String  ;
                                          const _ellipsoid      : Integer ;
                                          const _transform      : Integer ;
                                          const _fallback       : Integer
                                        ) ; overload ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source         : TObject
                                        ) ; override;
    public // public methods

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to a self-datum
      ///   into a geodetic coordinate relative to WGS84.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to WGS84 into a
      ///   geodetic coordinate relative to a self-datum.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to a self-datum
      ///   into a geodetic coordinate relative to WGS84.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  ToWGS                   ( const _ptg            : TGIS_Point
                                        ) : TGIS_Point ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to WGS84 into a
      ///   geodetic coordinate relative to a self-datum .
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  FromWGS                 ( const _ptg            : TGIS_Point
                                        ) : TGIS_Point ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to a self-datum
      ///   into a geodetic coordinate relative to WGS84.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  ToWGS3D                 ( const _ptg            : TGIS_Point3D
                                        ) : TGIS_Point3D ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to WGS84 into a
      ///   geodetic coordinate relative to a self-datum .
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  FromWGS3D               ( const _ptg            : TGIS_Point3D
                                        ) : TGIS_Point3D ;

    public // standard properties

      /// <summary>
      ///   Ellipsoid object.
      /// </summary>
      property Ellipsoid : TGIS_CSEllipsoid read FEllipsoid ;

      /// <summary>
      ///   Default Coordinate Transformation object.
      /// </summary>
      property Transform : TGIS_CSTransformAbstract read FTransform ;

      /// <summary>
      ///   Fallback Coordinate Transformation object. Used if main grid shift
      ///   based transformation is not available.
      /// </summary>
      property Fallback : Integer read FFallback ;

  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  TGIS_CSTransformAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                             class( TGIS_CSAbstract )
    // properties internal values
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      /// <summary>
      ///   Method EPSG code.
      /// </summary>
      FMethod : Integer ;

      /// <summary>
      ///   Ellipsoid object.
      /// </summary>
      FEllipsoid : Integer ;

      /// <summary>
      ///   Serial number of transformation for current datum.
      /// </summary>
      FNumber : Integer ;

      /// <summary>
      ///   Area EPSG code.
      /// </summary>
      FArea : Integer ;

      FA : Double ;
      FB : Double ;
      FC : Double ;
      FD : Double ;
      FE : Double ;
      FF : Double ;
      FG : Double ;
      FH : Double ;
      FI : Double ;
      FJ : Double ;
      FGridShift : String ;

    // other internal values
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
      oEllipsoid    : TGIS_CSEllipsoid ;
      oEllipsoidWGS : TGIS_CSEllipsoid ; // WG84 Ellipsoid

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_b">
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_c">
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </param>
      /// <param name="_d">
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_e">
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_f">
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_g">
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of geocentric
      ///   relative to 1; For Methods 9603, 9618: unused;
      /// </param>
      /// <param name="_h">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_i">
      ///   For Method 9636: Y reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_j">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;

    public // public methods
      /// <summary>
      ///   This method shifts a geodetic coordinate relative to Datum into a
      ///   geodetic coordinate relative to WGS84.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; virtual; abstract;
      /// <summary>
      ///   This method shifts a geodetic coordinate relative to WGS84 into a
      ///   geodetic coordinate relative to Datum.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; virtual; abstract;

      /// <summary>
      ///   This function shifts a geodetic coordinate (in meters) relative to
      ///   Datum into a geodetic coordinate (in meters) relative to WGS84.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  ToWGS                   ( const _ptg       : TGIS_Point
                                        ) : TGIS_Point ;

      /// <summary>
      ///   This function shifts a geodetic coordinate (in meters) relative to
      ///   WGS84 to a geodetic coordinate (in meters) relative to Datum.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  FromWGS                 ( const _ptg       : TGIS_Point
                                        ) : TGIS_Point ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to Datum into a
      ///   geodetic coordinate relative to WGS84. Conversion will be done by
      ///   Coordinates-&gt;Geocentric-&gt;WGS pipeline.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  ToWGS3D                 ( const _ptg       : TGIS_Point3D
                                        ) : TGIS_Point3D ;

      /// <summary>
      ///   This function shifts a geodetic coordinate relative to WGS84 into a
      ///   geodetic coordinate relative to Datum. Conversion will be done by
      ///   WGS-&gt;Geocentric-&gt;Coordinates- pipeline.
      /// </summary>
      /// <param name="_ptg">
      ///   given coordinates
      /// </param>
      /// <returns>
      ///   Converted coordinates.
      /// </returns>
      function  FromWGS3D               ( const _ptg       : TGIS_Point3D
                                        ) : TGIS_Point3D ;

    public // standard properties

      /// <summary>
      ///   Method EPSG code.
      /// </summary>
      property &Method : Integer      read FMethod ;

      /// <summary>
      ///   Ellipsoid object.
      /// </summary>
      property EllipsoidEPSG : Integer  read FEllipsoid ;

      /// <summary>
      ///   Serial number of transformation for current datum.
      /// </summary>
      property Number : Integer         read FNumber ;

      /// <summary>
      ///   Area EPSG code.
      /// </summary>
      property AreaEPSG : Integer       read FArea ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </summary>
      property A : Double               read FA ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </summary>
      property B : Double               read FB ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </summary>
      property C : Double               read FC ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </summary>
      property D : Double               read FD ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </summary>
      property E : Double               read FE ;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </summary>
      property F : Double               read FF;

      /// <summary>
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of
      ///   geocentric relative to 1; For Methods 9603, 9618: unused;
      /// </summary>
      property G : Double               read FG ;

      /// <summary>
      ///   For Method 9636: X reference point in Cartesian system
      ///   expressed in meters; For Methods 9603, 9606, 9607, 9618, 9619:
      ///   unused;
      /// </summary>
      property H : Double               read FH ;

      /// <summary>
      ///   For Method 9636: Y reference point in Cartesian system
      ///   expressed in meters; For Methods 9603, 9606, 9607, 9618, 9619:
      ///   unused;
      /// </summary>
      property I : Double               read FI ;

      /// <summary>
      ///   For Method 9636: X reference point in Cartesian system
      ///   expressed in meters; For Methods 9603, 9606, 9607, 9618, 9619:
      ///   unused;
      /// </summary>
      property J : Double               read FJ ;

      /// <summary>
      ///   For Method 9613 and 91615: Grid Shift file.
      /// </summary>
      property GridShift : String       read FGridShift ;
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   Geocentric Translations. Transformation. EPSG=9603
  /// </remarks>
  TGIS_CSTransformGeocentricTranslations = {$IFDEF OXYGENE} public {$ENDIF}
                                           class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;
      dZ : Double ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   X delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_b">
      ///   Y delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_c">
      ///   Z delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_d">
      ///   unused
      /// </param>
      /// <param name="_e">
      ///   unused
      /// </param>
      /// <param name="_f">
      ///   unused
      /// </param>
      /// <param name="_g">
      ///   unused
      /// </param>
      /// <param name="_h">
      ///   unused
      /// </param>
      /// <param name="_i">
      ///   unused
      /// </param>
      /// <param name="_j">
      ///   unused
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;
    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   Position Vector 7-parameters Transformation, Bursa-Wolf formula.
  ///   EPSG=9606
  /// </remarks>
  TGIS_CSTransformPositionVector7Params = {$IFDEF OXYGENE} public {$ENDIF}
                                          class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;
      dZ : Double ;
      rX : Double ;
      rY : Double ;
      rZ : Double ;
      dM : Double ;
      arFwd : TGIS_Matrix3x3 ;
      arInv : TGIS_Matrix3x3 ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   X delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_b">
      ///   Y delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_c">
      ///   Z delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_d">
      ///   X rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_e">
      ///   Y rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_f">
      ///   Z rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_g">
      ///   scaled correction of geocentric relative to 1
      /// </param>
      /// <param name="_h">
      ///   unused
      /// </param>
      /// <param name="_i">
      ///   unused
      /// </param>
      /// <param name="_j">
      ///   unused
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;

    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   Coordinate Frame Rotation, Bursa-Wolf Formula. EPSG=9607
  /// </remarks>
  TGIS_CSTransformCoordinateFrameRotation = {$IFDEF OXYGENE} public {$ENDIF}
                                            class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;
      dZ : Double ;
      rX : Double ;
      rY : Double ;
      rZ : Double ;
      dM : Double ;

      arFwd : TGIS_Matrix3x3 ;
      arInv : TGIS_Matrix3x3 ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   X delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_b">
      ///   Y delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_c">
      ///   Z delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_d">
      ///   X rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_e">
      ///   Y rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_f">
      ///   Z rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_g">
      ///   scaled correction of geocentric relative to 1
      /// </param>
      /// <param name="_h">
      ///   unused
      /// </param>
      /// <param name="_i">
      ///   unused
      /// </param>
      /// <param name="_j">
      ///   unused
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;
    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   Geographical and Height Offsets. EPSG=9618
  /// </remarks>
  TGIS_CSTransformGeographicalAndHighOffsets = {$IFDEF OXYGENE} public {$ENDIF}
                                               class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;
      dZ : Double ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   latitude shift expressed in radians
      /// </param>
      /// <param name="_b">
      ///   longitude shift expressed in radians
      /// </param>
      /// <param name="_c">
      ///   gravity height over the ellipsoid height
      /// </param>
      /// <param name="_d">
      ///   unused
      /// </param>
      /// <param name="_e">
      ///   unused
      /// </param>
      /// <param name="_f">
      ///   unused
      /// </param>
      /// <param name="_g">
      ///   unused
      /// </param>
      /// <param name="_h">
      ///   unused
      /// </param>
      /// <param name="_i">
      ///   unused
      /// </param>
      /// <param name="_j">
      ///   unused
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;
    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   Geographical and Height Offsets. EPSG=9619
  /// </remarks>
  TGIS_CSTransformGeographicalOffsets = {$IFDEF OXYGENE} public {$ENDIF}
                                        class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   latitude shift expressed in radians
      /// </param>
      /// <param name="_b">
      ///   longitude shift expressed in radians
      /// </param>
      /// <param name="_c">
      ///   unused
      /// </param>
      /// <param name="_d">
      ///   unused
      /// </param>
      /// <param name="_e">
      ///   unused
      /// </param>
      /// <param name="_f">
      ///   unused
      /// </param>
      /// <param name="_g">
      ///   unused
      /// </param>
      /// <param name="_h">
      ///   unused
      /// </param>
      /// <param name="_i">
      ///   unused
      /// </param>
      /// <param name="_j">
      ///   unused
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;

    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    revers transformation with modified "Dutch Reverse" formula
  ///    </note>
  ///   Molodenski-Badekas transformation. EPSG=9636
  /// </remarks>
  TGIS_CSTransformMolodenskiBadekas = {$IFDEF OXYGENE} public {$ENDIF}
                                      class( TGIS_CSTransformAbstract )
    private
      dX : Double ;
      dY : Double ;
      dZ : Double ;
      rX : Double ;
      rY : Double ;
      rZ : Double ;
      dM : Double ;
      pX : Double ;
      pY : Double ;
      pZ : Double ;
      arFwd : TGIS_Matrix3x3 ;

      dMInv : Double ;
      arInv : TGIS_Matrix3x3 ;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   X delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_b">
      ///   Y delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_c">
      ///   Z delta of geocentric shift expressed in meters
      /// </param>
      /// <param name="_d">
      ///   X rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_e">
      ///   Y rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_f">
      ///   Z rotation of geocentric expressed in radians
      /// </param>
      /// <param name="_g">
      ///   scaled correction of geocentric relative to 1
      /// </param>
      /// <param name="_h">
      ///   X reference point in Cartesian system expressed in meters
      /// </param>
      /// <param name="_i">
      ///   Y reference point in Cartesian system expressed in meters
      /// </param>
      /// <param name="_j">
      ///   Z reference point in Cartesian system expressed in meters
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _a         : Double  ;
                                          const _b         : Double  ;
                                          const _c         : Double  ;
                                          const _d         : Double  ;
                                          const _e         : Double  ;
                                          const _f         : Double  ;
                                          const _g         : Double  ;
                                          const _h         : Double  ;
                                          const _i         : Double  ;
                                          const _j         : Double
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;
    private  // private member

      /// <summary>
      ///   Internal forward coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doForward               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   Internal reverse coordinate shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure doReverse               ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  TGIS_CSTransformGridShift = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSTransformAbstract )
    private
      bInitialized : Boolean ;

      oGrids       : TObjectList< TObject >;
      oFallback    : TGIS_CSTransformAbstract ;
      oCurrentGrid : TObject ;
      criticalSection : TCriticalSection ;
    protected

      procedure doDestroy ; override;


    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the transformation
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_grid_files">
      ///   list of supported grid files separated by coma
      /// </param>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _grid_files: String
                                        ) ;

      /// <inheritdoc/>
      procedure   Assign                ( const _source    : TObject
                                        ) ; override;
    private  // private member

      function  fget_Fallback          : Integer ;
      procedure fset_Fallback          ( const _value      : Integer
                                       ) ;

      /// <summary>
      ///   Initialize Grid. Load it into memory if exits.
      /// </summary>
      procedure initializeGrid ;

      /// <summary>
      ///   Find matching grid which validity extent covers point to
      ///   be translated.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure findGrid                ( const _ptg            : TGIS_Point3D
                                        ) ;

    public // public methods

      /// <inheritdoc/>
      procedure ToWGS3D_Ref             ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

      /// <inheritdoc/>
      procedure FromWGS3D_Ref           ( {$IFNDEF JAVA} var {$ENDIF}
                                                _ptg            : TGIS_Point3D
                                        ) ; override;

    public // public properties

      /// <summary>
      ///   Fallback Coordinate Transformation object. Used if main grid shift
      ///   based transformation is not available.
      /// </summary>
      property Fallback : Integer       read  fget_Fallback
                                        write fset_Fallback ;

    {$IFDEF UNIT_TEST}
      public // protected member
        procedure TestFwd               ( var   _ptg       : TGIS_Point3D
                                        ) ;
        procedure TestRev               ( var   _ptg       : TGIS_Point3D
                                        ) ;
    {$ENDIF}
  end ;


  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   NADCON Grid-Shift. EPSG=9613
  /// </remarks>
  TGIS_CSTransformNadcon = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSTransformGridShift )
    public // public methods

      /// <inheritdoc/>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _grid_files: String
                                        ) ;
  end ;

  /// <summary>
  ///   A datum transformation type.
  /// </summary>
  /// <remarks>
  ///   NTV2 Grid-Shift. EPSG=9615
  /// </remarks>
  TGIS_CSTransformNTV2 = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSTransformGridShift )
    public // public methods

      /// <inheritdoc/>
      constructor Create                ( const _epsg      : Integer ;
                                          const _wkt       : String  ;
                                          const _ellipsoid : Integer ;
                                          const _number    : Integer ;
                                          const _area      : Integer ;
                                          const _grid_files: String
                                        ) ;
  end ;


  /// <summary>
  ///   List of units.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSUnitsList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSUnits}
  TGIS_CSUnitsList = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected // property access routines

      function fget_Units( _idx : Integer ) : TGIS_CSUnits ;

    public // public methods

      /// <summary>
      ///   Add a new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_symbol">
      ///   units symbol like 'km', 'm'
      /// </param>
      /// <param name="_type">
      ///   angular, linear or areal units
      /// </param>
      /// <param name="_factor">
      ///   factor between unit and basic unit (meter or radians); for example
      ///   factor for kilometer is 1000.
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Add    ( const _epsg     : Integer          ;
                         const _wkt      : String           ;
                         const _symbol   : String           ;
                         const _type     : TGIS_CSUnitsType ;
                         const _factor   : Double
                       ) : TGIS_CSUnits ; reintroduce ; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_symbol">
      ///   units symbol like 'km', 'm'
      /// </param>
      /// <param name="_type">
      ///   angular, linear or areal units
      /// </param>
      /// <param name="_factor">
      ///   factor between unit and basic unit (meter or radians); for example
      ///   factor for kilometer is 1000.
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Fix    ( const _epsg     : Integer          ;
                         const _wkt      : String           ;
                         const _symbol   : String           ;
                         const _type     : TGIS_CSUnitsType ;
                         const _factor   : Double
                       ) : TGIS_CSUnits ; reintroduce ; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSUnits object. If the matching object does not
      ///   exists then will be added to the list and returned. If the object
      ///   can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_symbol">
      ///   units symbol like 'km', 'm'
      /// </param>
      /// <param name="_type">
      ///   angular, linear or areal units
      /// </param>
      /// <param name="_factor">
      ///   factor between unit and basic unit (meter or radians); for example
      ///   factor for kilometer is 1000.
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  Prepare( const _epsg     : Integer          ;
                         const _wkt      : String           ;
                         const _symbol   : String           ;
                         const _type     : TGIS_CSUnitsType ;
                         const _factor   : Double
                       ) : TGIS_CSUnits ; reintroduce ; virtual;

      /// <summary>
      ///   Add a new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the unit; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the unit
      /// </param>
      /// <param name="_subunits">
      ///   list of units in a set ordered by factor; if contains areal units
      ///   then order should be by squared factor of all units except areas
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  AddAuto( const _epsg     : Integer          ;
                         const _wkt      : String           ;
                         const _subunits : array of Integer
                       ) : TGIS_CSUnits ; virtual;


      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG ( const _epsg     : Integer
                       ) : TGIS_CSUnits ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT  ( const _wkt      : String
                       ) : TGIS_CSUnits ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init   ; override;

    public // public properties

      /// <summary>
      ///   Default property for retrieving unit.
      /// </summary>
      /// <param name="_idx">
      ///   position on the list
      /// </param>
      property Units[ _idx : Integer ] : TGIS_CSUnits
                                         read fget_Units ;
                                         default ;
  end ;

  /// <summary>
  ///   List of prime meridians.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSPrimeMeridianList instead of creating instance of this
  ///    class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSPrimeMeridian}
  TGIS_CSPrimeMeridianList = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected // property access routines

      function fget_PrimeMeridian( _idx : Integer ) : TGIS_CSPrimeMeridian ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the prime meridian; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the prime meridian
      /// </param>
      /// <param name="_longitude">
      ///   longitude of prime meridian (in radians)
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _longitude : Double
                        ) : TGIS_CSPrimeMeridian ; reintroduce ; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the prime meridian; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the prime meridian
      /// </param>
      /// <param name="_longitude">
      ///   longitude of prime meridian (in radians)
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _longitude : Double
                        ) : TGIS_CSPrimeMeridian ; reintroduce ; virtual;

      /// <summary>
      ///   Prepare a projected Coordinate System object. If the matching
      ///   object does not exists then will be added to the list and returned.
      ///   If the object can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the Prime Meridian; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the Prime Meridian
      /// </param>
      /// <param name="_longitude">
      ///   Longitude of Prime Meridian in radians; relative to Greenwich
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  Prepare ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _longitude : Double
                        ) : TGIS_CSPrimeMeridian ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG  ( const _epsg      : Integer
                        ) : TGIS_CSPrimeMeridian ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT   ( const _wkt       : String
                        ) : TGIS_CSPrimeMeridian ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init    ; override;

    public // public properties

      /// <summary>
      ///   Default property for retrieving Prime Meridian.
      /// </summary>
      /// <param name="_idx">
      ///   position on the list
      /// </param>
      property PrimeMeridian[ _idx : Integer ] : TGIS_CSPrimeMeridian
                                                 read fget_PrimeMeridian ;
                                                 default ;
  end ;

  /// <summary>
  ///   List of ellipsoids.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSEllipsoidList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSEllipsoid}
  TGIS_CSEllipsoidList = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected // property access routines

      function fget_Ellipsoid( _idx : Integer ) : TGIS_CSEllipsoid ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the ellipsoid; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the ellipsoid
      /// </param>
      /// <param name="_semi_major">
      ///   semi major axis
      /// </param>
      /// <param name="_inv_flattering">
      ///   inverse flattering; 0 for spheroid
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _semi_major     : Double  ;
                          const _inv_flattering : Double
                        ) : TGIS_CSEllipsoid ; reintroduce ; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the ellipsoid; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the ellipsoid
      /// </param>
      /// <param name="_semi_major">
      ///   semi major axis
      /// </param>
      /// <param name="_inv_flattering">
      ///   inverse flattering; 0 for spheroid
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _semi_major     : Double  ;
                          const _inv_flattering : Double
                        ) : TGIS_CSEllipsoid ; reintroduce ; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSEllipsoid object. If the matching object does not
      ///   exists then will be added to the list and returned. If the object
      ///   can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the ellipsoid; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the ellipsoid
      /// </param>
      /// <param name="_semi_major">
      ///   semi major axis
      /// </param>
      /// <param name="_inv_flattering">
      ///   inverse flattering; 0 for spheroid
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  Prepare ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _semi_major     : Double  ;
                          const _inv_flattering : Double
                        ) : TGIS_CSEllipsoid ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG  ( const _epsg           : Integer
                        ) : TGIS_CSEllipsoid  ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT   ( const _wkt            : String
                        ) : TGIS_CSEllipsoid  ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init    ; override;

    public // public properties

      /// <summary>
      ///   Default property for retrieving Ellipsoid.
      /// </summary>
      /// <param name="_idx">
      ///   position on the list
      /// </param>
      property Ellipsoid[ _idx : Integer ]   : TGIS_CSEllipsoid
                                               read fget_Ellipsoid ;
                                               default ;
  end ;

  /// <summary>
  ///   List of datum area. Datum area is a text representing geographic area
  ///   for which datum is valid. List is always sorted based on geographical
  ///   region string.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSAreaList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  TGIS_CSAreaList = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected

      procedure doDestroy ; override;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the area name; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_name">
      ///   area name
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///   </note>
      ///   Function create bound area as (-180,-90,180,90).
      ///   For custom area bounds see overloaded method.
      /// </remarks>
      function  Add     ( const _epsg : Integer ;
                          const _name : String
                        ) : TGIS_CSArea ; reintroduce ; overload; virtual;


      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the area name; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_name">
      ///   area bounds
      /// </param>
      /// <param name="_xmin">
      ///   area bounds
      /// </param>
      /// <param name="_ymin">
      ///   area bounds
      /// </param>
      /// <param name="_xmax">
      ///   area bounds
      /// </param>
      /// <param name="_ymax">
      ///   area bounds
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///   </note>
      /// </remarks>
      function Add      ( const _epsg : Integer ;
                          const _name : String  ;
                          const _xmin : Double  ;
                          const _ymin : Double  ;
                          const _xmax : Double  ;
                          const _ymax : Double
                        ) : TGIS_CSArea ; reintroduce ; overload ; virtual;


      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the area name; if 0 then EPSG will be assigned from
      ///   user defined, temporary pool
      /// </param>
      /// <param name="_name">
      ///   area name
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg : Integer ;
                          const _name : String
                        ) : TGIS_CSArea ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG  ( const _epsg : Integer
                        ) : TGIS_CSArea ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT   ( const _wkt  : String
                        ) : TGIS_CSArea ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init    ; override;
  end ;

  /// <summary>
  ///   List of all datums.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSDatumList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSDatum}
  TGIS_CSDatumList = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected // property access routines

      function fget_Datum( _idx : Integer ) : TGIS_CSDatum ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _transform      : Integer
                        ) : TGIS_CSDatum ; reintroduce ; overload ; virtual;

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      /// <param name="_fallback">
      ///   EPSG code of fallback coordinate operation (used only for grid-shift
      ///   if grid-shift file can not be used)
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _transform      : Integer ;
                          const _fallback       : Integer
                        ) : TGIS_CSDatum ; reintroduce ; overload ; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _transform      : Integer
                        ) : TGIS_CSDatum ; reintroduce ; overload; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation
      /// </param>
      /// <param name="_fallback">
      ///   EPSG code of fallback coordinate operation (used only for grid-shift
      ///   if grid-shift file can not be used)
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg and _wkt for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _transform      : Integer ;
                          const _fallback       : Integer
                        ) : TGIS_CSDatum ; reintroduce ; overload; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSDatum object. If the matching object does not
      ///   exists then will be added to the list and returned. If the object
      ///   can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_transform">
      ///   EPSG code of default coordinate operation if value is -1 then first
      ///   matching object will be found
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  Prepare ( const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _transform      : Integer
                        ) : TGIS_CSDatum ; reintroduce ; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSDatum object. If the matching object does not
      ///   exists then will be added to the list and returned. If the object
      ///   can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the datum; if 0 then EPSG will be assigned from user
      ///   defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   WKT name for the datum
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for ellipsoid
      /// </param>
      /// <param name="_method">
      ///   EPSG code for transformation method
      /// </param>
      /// <param name="_area">
      ///   EPSG code of default coordinate operation if value is -1 then first
      ///   matching object will be found
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_b">
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_c">
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </param>
      /// <param name="_d">
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_e">
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_f">
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_g">
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of geocentric
      ///   relative to 1; For Methods 9603, 9618: unused;
      /// </param>
      /// <param name="_h">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_i">
      ///   For Method 9636: Y reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_j">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  PrepareEx(
                          const _epsg           : Integer ;
                          const _wkt            : String  ;
                          const _ellipsoid      : Integer ;
                          const _method         : Integer ;
                          const _area           : Integer ;
                          const _a              : Double  ;
                          const _b              : Double  ;
                          const _c              : Double  ;
                          const _d              : Double  ;
                          const _e              : Double  ;
                          const _f              : Double  ;
                          const _g              : Double  ;
                          const _h              : Double  ;
                          const _i              : Double  ;
                          const _j              : Double
                        ) : TGIS_CSDatum ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByEPSG  ( const _epsg           : Integer
                        ) : TGIS_CSDatum ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function  ByWKT   ( const _wkt            : String
                        ) : TGIS_CSDatum ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure Init    ; override;

    public // public properties

      /// <summary>
      ///   Default property for retrieving Datum.
      /// </summary>
      /// <param name="_idx">
      ///   position on the list
      /// </param>
      property Datum[ _idx : Integer ] : TGIS_CSDatum
                                         read fget_Datum ;
                                         default ;
  end ;

  /// <summary>
  ///   List of all datum transformations.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    use global CSTransformList instead of creating instance of this class.
  ///    </note>
  /// </remarks>
  {#typehint:list-ro:TGIS_CSTransformAbstract}
  TGIS_CSTransformList = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor       Create ;

    protected // property access routines

      function fget_DatumTransform( _idx : Integer ) : TGIS_CSTransformAbstract ;

    public // public methods

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   wkt name for transformation
      /// </param>
      /// <param name="_method">
      ///   EPSG code for method
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_b">
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_c">
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </param>
      /// <param name="_d">
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_e">
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_f">
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_g">
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of geocentric
      ///   relative to 1; For Methods 9603, 9618: unused;
      /// </param>
      /// <param name="_h">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_i">
      ///   For Method 9636: Y reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_j">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _method    : Integer ;
                          const _ellipsoid : Integer ;
                          const _number    : Integer ;
                          const _area      : Integer ;
                          const _a         : Double  ;
                          const _b         : Double  ;
                          const _c         : Double  ;
                          const _d         : Double  ;
                          const _e         : Double  ;
                          const _f         : Double  ;
                          const _g         : Double  ;
                          const _h         : Double  ;
                          const _i         : Double  ;
                          const _j         : Double
                        ) : TGIS_CSTransformAbstract ;
                        reintroduce; overload ;virtual;

      /// <summary>
      ///   Add new item into the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   wkt name for transformation
      /// </param>
      /// <param name="_method">
      ///   EPSG code for method
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_grid_files">
      ///   list of supported grid files separated by coma
      /// </param>
      /// <returns>
      ///   Newly created object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///    </note>
      /// </remarks>
      function  Add     ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _method    : Integer ;
                          const _ellipsoid : Integer ;
                          const _number    : Integer ;
                          const _area      : Integer ;
                          const _grid_files: String
                        ) : TGIS_CSTransformAbstract ;
                        reintroduce; overload; virtual;

      /// <summary>
      ///   Fix item by substituting exiting item based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   wkt name for transformation
      /// </param>
      /// <param name="_method">
      ///   EPSG code for method
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_b">
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_c">
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </param>
      /// <param name="_d">
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_e">
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_f">
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_g">
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of geocentric
      ///   relative to 1; For Methods 9603, 9618: unused;
      /// </param>
      /// <param name="_h">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_i">
      ///   For Method 9636: Y reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_j">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <returns>
      ///   Fixed object.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    function will not check _epsg for being unique
      ///    </note>
      /// </remarks>
      function  Fix     ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _method    : Integer ;
                          const _ellipsoid : Integer ;
                          const _number    : Integer ;
                          const _area      : Integer ;
                          const _a         : Double  ;
                          const _b         : Double  ;
                          const _c         : Double  ;
                          const _d         : Double  ;
                          const _e         : Double  ;
                          const _f         : Double  ;
                          const _g         : Double  ;
                          const _h         : Double  ;
                          const _i         : Double  ;
                          const _j         : Double
                        ) : TGIS_CSTransformAbstract ; reintroduce ; virtual;

      /// <summary>
      ///   Prepare a TGIS_CSTransformAbstract object. If the matching object
      ///   does not exists then will be added to the list and returned. If the
      ///   object can be found then it will be returned form the list.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code for the transformation; if 0 then EPSG will be assigned
      ///   from user defined, temporary pool
      /// </param>
      /// <param name="_wkt">
      ///   wkt name for transformation
      /// </param>
      /// <param name="_method">
      ///   EPSG code for method
      /// </param>
      /// <param name="_ellipsoid">
      ///   EPSG code for the ellipsoid
      /// </param>
      /// <param name="_number">
      ///   serial number of transformation for current datum
      /// </param>
      /// <param name="_area">
      ///   EPSG area code
      /// </param>
      /// <param name="_a">
      ///   For Methods 9603, 9606, 9607, 9636: X delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: latitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_b">
      ///   For Methods 9603, 9606, 9607, 9636: Y delta of geocentric shift
      ///   expressed in meters; For Methods 9618, 9119: longitude shift
      ///   expressed in radians;
      /// </param>
      /// <param name="_c">
      ///   For Methods 9603, 9606, 9607, 9636: Z delta of geocentric shift
      ///   expressed in meters; For Method 9618: gravity height over the
      ///   ellipsoid height; For Method 9619: unused;
      /// </param>
      /// <param name="_d">
      ///   For Methods 9603, 9606, 9607, 9636: X rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_e">
      ///   For Methods 9603, 9606, 9607, 9636: Y rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_f">
      ///   For Methods 9603, 9606, 9607, 9636: Z rotation of geocentric
      ///   expressed in radians; For Methods 9603, 9618, 9619: unused;
      /// </param>
      /// <param name="_g">
      ///   For Methods 9603, 9606, 9607, 9636: scaled correction of geocentric
      ///   relative to 1; For Methods 9603, 9618: unused;
      /// </param>
      /// <param name="_h">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_i">
      ///   For Method 9636: Y reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <param name="_j">
      ///   For Method 9636: X reference point in Cartesian system expressed in
      ///   meters; For Methods 9603, 9606, 9607, 9618, 9619: unused;
      /// </param>
      /// <returns>
      ///   Newly created / Found object or nil.
      /// </returns>
      function  Prepare ( const _epsg      : Integer ;
                          const _wkt       : String  ;
                          const _method    : Integer ;
                          const _ellipsoid : Integer ;
                          const _number    : Integer ;
                          const _area      : Integer ;
                          const _a         : Double  ;
                          const _b         : Double  ;
                          const _c         : Double  ;
                          const _d         : Double  ;
                          const _e         : Double  ;
                          const _f         : Double  ;
                          const _g         : Double  ;
                          const _h         : Double  ;
                          const _i         : Double  ;
                          const _j         : Double
                        ) : TGIS_CSTransformAbstract ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on EPSG code.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function   ByEPSG  ( const _epsg   : Integer
                         ) : TGIS_CSTransformAbstract ; reintroduce ; virtual;

      /// <summary>
      ///   Find object on the list based on WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT name
      /// </param>
      /// <returns>
      ///   Found object or nil.
      /// </returns>
      function   ByWKT   ( const _wkt    : String
                         ) : TGIS_CSTransformAbstract ; reintroduce ; virtual;

      /// <inheritdoc/>
      procedure  Init    ; override;

    public // public properties

      /// <summary>
      ///   Default property for retrieving Transformation.
      /// </summary>
      /// <param name="_idx">
      ///   position on the list
      /// </param>
      property Transform[ _idx : Integer ] : TGIS_CSTransformAbstract
                                             read fget_DatumTransform ;
                                             default ;
  end ;

  /// <summary>
  ///   Make canonical WKT form the provided string.
  ///   Replace spaces with underscores etc.
  /// </summary>
  /// <param name="_wkt">
  ///    string to be altered
  /// </param>
  /// <returns>
  ///   String in canonical form.
  /// </returns>
  function CanonicalWKT( const _wkt : String ) : String ;

  /// <summary>
  ///   List of all units.
  /// </summary>
  /// <returns>
  ///   Global units list.
  /// </returns>
  function CSUnitsList : TGIS_CSUnitsList ;

  /// <summary>
  ///   List of all prime meridians.
  /// </summary>
  /// <returns>
  ///   Global prime meridians list.
  /// </returns>
  function CSPrimeMeridianList : TGIS_CSPrimeMeridianList ;

  /// <summary>
  ///   List of all ellipsoids.
  /// </summary>
  /// <returns>
  ///   Global ellipsoids list.
  /// </returns>
  function CSEllipsoidList : TGIS_CSEllipsoidList ;

  /// <summary>
  ///   List of all transformations.
  /// </summary>
  /// <returns>
  ///   Global transformations list.
  /// </returns>
  function CSTransformList : TGIS_CSTransformList ;

  /// <summary>
  ///   List of all datums.
  /// </summary>
  /// <returns>
  ///   Global datums list.
  /// </returns>
  function CSDatumList : TGIS_CSDatumList ;

  /// <summary>
  ///   List of all areas.
  /// </summary>
  /// <returns>
  ///   Global areas list.
  /// </returns>
  function CSAreaList : TGIS_CSAreaList ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisCSUnitsTypeLinear  = TGIS_CSUnitsType.Linear  ;
      gisCSUnitsTypeAngular = TGIS_CSUnitsType.Angular ;
      gisCSUnitsTypeAreal   = TGIS_CSUnitsType.Areal   ;
      gisCSUnitsTypeAuto    = TGIS_CSUnitsType.Auto    ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoUtils,
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}

const
  SORT_UNKNOWN = 0 ;
  SORT_BY_EPSG = 1 ;
  SORT_BY_WKT  = 2 ;

const
  // Seconds in a radian pi
  SECONDS_PER_RADIAN  = 206264.8062471      ;
  // Toms region 1 constant
  AD_C                = 1.0026000           ;
  // cosine of 67.5 degrees
  COS_67_5            = 0.38268343236508977 ;
  // Polar limit
  MOLODENSKY_MAX      = 89.75 * Pi / 180.0  ;

  // Subfolder with grid shift files
  GRIDSHIFT_FOLDER    = 'Datums' ;

  // Metadata for common Grid Shift files folder
  METADATA_GRIDSHIFT_FOLDER = 'TGIS_CSTransformGridShift.Folder' ;

var
  autoEPSG : Integer = GIS_EPSG_AUTO ;

  // List of all units.
  cs_UnitsList : TGIS_CSUnitsList = nil ;
  // List of all prime meridians.
  cs_PrimeMeridianList : TGIS_CSPrimeMeridianList = nil ;
  // List of all ellipsoids.
  cs_EllipsoidList : TGIS_CSEllipsoidList = nil ;
  // List of all datums.
  cs_TransformList : TGIS_CSTransformList = nil ;
  // List of all datums.
  cs_DatumList : TGIS_CSDatumList = nil ;
  // List of all datum areas.
  cs_AreaList : TGIS_CSAreaList = nil ;



{$REGION 'T_CSGridShift'}
type
  /// <summary>
  ///   Low level generic grid shift operations.
  /// </summary>
  T_CSGridShift = {$IFDEF OXYGENE} abstract {$ENDIF} class

    public
      /// <summary>
      ///   Validity Extent for grid expressed in Lat/Lon
      /// </summary>
      Extent : TGIS_Extent ;

      /// <summary>
      ///   Next gridshift in a chain
      /// </summary>
      Chain : T_CSGridShift ;

    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <summary>
      ///   Number of column i the grid.
      /// </summary>
      iCols  : Integer ;

      /// <summary>
      ///   Number of rows in the grid.
      /// </summary>
      iRows  : Integer ;

      /// <summary>
      ///   Grid in West-East, North->South orientation
      /// </summary>
      arGrid : array of TGIS_Point ;

      /// <summary>
      ///   Size of grid cell in Longitude direction.
      /// </summary>
      dXResolution : Double ;

      /// <summary>
      ///   Size of grid cell in latitude direction
      /// </summary>
      dYResolution : Double ;
    protected
      /// <summary>
      ///   Load grid form the stream.
      /// </summary>.
      /// <param name="_strm">
      ///   stream with grid content
      /// </param>
      procedure doLoad                  ( const _strm      : TGIS_BufferedStream
                                        ) ; virtual ; abstract ;

      /// <summary>
      ///   Transform single point.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      /// <param name="_forward">
      ///   if True then transformation is forward; if False then transformation
      ///   is revere
      /// </param>
      procedure doTransform             ( var   _ptg       : TGIS_Point3D ;
                                          const _forward   : Boolean
                                        ) ; virtual ;
    public
      /// <summary>
      ///   Create an instance of the object based on provided stream.
      /// </summary>
      /// <param name="_strm">
      ///   stream with grid content; if nil then a stub object created
      /// </param>
      constructor Create                ( const _strm      : TStream
                                        ) ; overload ;

      /// <summary>
      ///   This function shifts a geodetic based on grids shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure To_Ref                  ( var   _ptg       : TGIS_Point3D
                                        ) ;

      /// <summary>
      ///   This function shifts a geodetic based on grids shift.
      /// </summary>
      /// <param name="_ptg">
      ///   coordinates to be shifted
      /// </param>
      procedure From_Ref                ( var   _ptg       : TGIS_Point3D
                                        ) ;


      /// <summary>
      ///   Recognize file format.
      /// </summary>
      /// <param name="_strm">
      ///   stream with grid content
      /// </param>
      /// <returns>
      ///   If True, then stream contains format according to current type.
      /// </returns>
      class function Prerecognize       ( const _strm      : TStream
                                        ) : Boolean ; virtual ; abstract ;

      /// <summary>
      ///   Create grid shift object based on stream content
      /// </summary>
      /// <param name="_strm">
      ///   stream with grid content
      /// </param>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      class function Factory            ( const _strm      : TStream
                                        ) : T_CSGridShift ; overload ;
      /// <summary>
      ///   Create grid shift object based on gird shift file name.
      /// </summary>
      /// <param name="_name">
      ///   name of a file; do not use file extension - program will add
      ///   extensions on its own
      /// </param>
      /// <remarks>
      ///   Program will try to find file in a current executable location,
      ///   then 'Datums' subfolder.
      /// </remarks>
      /// <returns>
      ///   Newly created object or nil.
      /// </returns>
      class function Factory            ( const _name      : String
                                        ) : T_CSGridShift ; overload ;
  end;

  /// <summary>
  ///   Low level NTV2 grid shift operations. Support for reading NTV2
  ///   (GSB) files.
  /// </summary>
  T_CSGridShiftNTV2 = class( T_CSGridShift )
    protected
      /// <inheritdoc/>
      procedure doLoad                  ( const _strm      : TGIS_BufferedStream
                                        ) ; override ;
    public
      /// <inheritdoc/>
      class function Prerecognize       ( const _strm      : TStream
                                        ) : Boolean ; override ;
  end;

  /// <summary>
  ///   Low level TTKGS grid shift operations. Support for reading TatukGIS
  ///   Grid Shift (TTKGS) files.
  /// </summary>
  T_CSGridShiftTTKGS = class( T_CSGridShift )
    protected
      /// <inheritdoc/>
      procedure doLoad                  ( const _strm      : TGIS_BufferedStream
                                        ) ; override ;
    public
      /// <inheritdoc/>
      class function Prerecognize       ( const _strm      : TStream
                                        ) : Boolean ; override ;
  end;

{$ENDREGION}
{$REGION 'Various private utility functions'}

// Provide unique EPSG number
// _epsg   source _epsg
// return  if provided _epsg < 0  then unique number generated;
//         if provided _epsg >= 0 then provided value will be returned
function fnEPSG(
  const _epsg : Integer
) : Integer  ;
begin
  if _epsg > 0 then begin
    Result := _epsg ;
  end
  else if _epsg = 0 then begin
    Result := _epsg ;
  end
  else begin
    inc( autoEPSG ) ;
    Result := autoEPSG ;
  end
end ;

// Compute matrix inversion
// _src    matrix to be inverted
// return  inverted matrix
function invMatrix(
  const _src : TGIS_Matrix3x3
) : TGIS_Matrix3x3 ;
var
  det : Double  ;
begin

  det := _src[1,1] * ( _src[3,3] * _src[2,2] - _src[3,2] * _src[2,3] ) -
         _src[2,1] * ( _src[3,3] * _src[1,2] - _src[3,2] * _src[1,3] ) +
         _src[3,1] * ( _src[2,3] * _src[1,2] - _src[2,2] * _src[1,3] ) ;

  if det = 0 then exit ;
  {$IFDEF JAVA}
    InitializeMatrixArray(Result) ;
  {$ENDIF}
  Result[ 1, 1 ] :=   ( _src[3,3] * _src[2,2] - _src[3,2] * _src[2,3 ] ) / det ;
  Result[ 1, 2 ] := - ( _src[3,3] * _src[1,2] - _src[3,2] * _src[1,3 ] ) / det ;
  Result[ 1, 3 ] :=   ( _src[2,3] * _src[1,2] - _src[2,2] * _src[1,3 ] ) / det ;
  Result[ 2, 1 ] := - ( _src[3,3] * _src[2,1] - _src[3,1] * _src[2,3 ] ) / det ;
  Result[ 2, 2 ] :=   ( _src[3,3] * _src[1,1] - _src[3,1] * _src[1,3 ] ) / det ;
  Result[ 2, 3 ] := - ( _src[2,3] * _src[1,1] - _src[2,1] * _src[1,3 ] ) / det ;
  Result[ 3, 1 ] :=   ( _src[3,2] * _src[2,1] - _src[3,1] * _src[2,2 ] ) / det ;
  Result[ 3, 2 ] := - ( _src[3,2] * _src[1,1] - _src[3,1] * _src[1,2 ] ) / det ;
  Result[ 3, 3 ] :=   ( _src[2,2] * _src[1,1] - _src[2,1] * _src[1,2 ] ) / det ;
end ;

// Compare TGIS_WKTAbstract based on EPSG as required for TList.Sort
// _p1     first object
// _p2     second object
// return  0 if equal, 1 if _p1 > _p2; -1 if _p1 < _p2
{$IFDEF MANAGED}
  function SortByEPSG(
    _p1, _p2 : TObject
  ) : Integer ;
{$ELSE}
  function SortByEPSG(
    _p1, _p2 : Pointer
  ) : Integer ;
{$ENDIF}
begin
  if      TGIS_CSAbstract( _p1 ).EPSG > TGIS_CSAbstract( _p2 ).EPSG
          then Result := 1
  else if TGIS_CSAbstract( _p1 ).EPSG < TGIS_CSAbstract( _p2 ).EPSG
          then Result := -1
  else     Result := 0 ;
end ;

// Compare TGIS_WKTAbstract based on WKT as required for TList.Sort
// _p1     first object
// _p2     second object
// return  0 if equal, 1 if _p1 > _p2; -1 if _p1 < _p2
{$IFDEF MANAGED}
  function SortByWKT(
    _p1, _p2 : TObject
  ) : Integer ;
{$ELSE}
  function SortByWKT(
    _p1, _p2 : Pointer
  ) : Integer ;
{$ENDIF}
begin
  Result := CompareText( TGIS_CSAbstract( _p1 ).WKT,
                         TGIS_CSAbstract( _p2 ).WKT
                       ) ;
end ;

// Make canonical WKT name
// _wkt    wkt to be evaluated
// return  pure ASCII based WKT allow not alphanumeric characters
//         replaced by underscores
function CanonicalWKT(
  const _wkt : String
) : String ;
var
  i   : Integer ;
  r   : TStringBuilder ;
  chs : TCharSet ;
begin
  Result := '' ;
  r := TStringBuilder.Create( 255 ) ;
  try
    chs := PrepareCharSet( [ 'AZ', 'az', '_', '+', '-', '/', '09' ] ) ;
    for i := StringFirst to StringLast( _wkt ) do begin
      if InCharSet( _wkt[i], chs ) then
        r.Append( _wkt[i] )
      else begin
        if ( r.Length         >  0    ) and
           ( r[ r.Length -1 ] <> '_'  )
        then
          r.Append( '_' )
      end ;
    end ;
    if ( r.Length         >  0   ) and
       ( r[ r.Length -1 ] = '_'  )
    then
      r.Length := r.Length -1 ;

    Result := r.ToString() ;
  finally
    FreeObject( r )
  end;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSAbstract'}

constructor TGIS_CSAbstract.Create ;
begin
  inherited ;
  FEPSG       := 0     ;
  FMasterEPSG := 0     ;
  FWKT        := ''    ;
  FDeprecated := False ;
end ;

constructor TGIS_CSAbstract.Create(
  const _epsg   : Integer ;
  const _wkt    : String
) ;
begin
  inherited Create ;

  FEPSG := fnEPSG( _epsg ) ;
  FWKT  := CanonicalWKT( _wkt ) ;
end ;

function TGIS_CSAbstract.fget_Description
  : String ;
begin
  Result := StringReplaceAll( WKT, '_', ' ' ) ;
end ;

function TGIS_CSAbstract.fget_FriendlyName
  : String ;
var
  tmp : String ;
begin
  tmp := StringReplaceAll( WKT, '_', ' ' ) ;

  if MasterEPSG > 0 then begin
    if EPSG < GIS_EPSG_AUTO then
       Result := Format( '%s (epsg:%d alias:%d)', [ tmp, EPSG, MasterEPSG ] )
    else
       Result := Format( '%s (alias:%d)'        , [ tmp, MasterEPSG       ] )
  end
  else if EPSG < GIS_EPSG_AUTO then
    Result    := Format( '%s (epsg:%d)'         , [ tmp, EPSG             ] )
  else
    Result    := Format( '%s'                   , [ tmp                   ] ) ;
end ;

procedure TGIS_CSAbstract.Assign(
  const _source : TObject
) ;
begin
 assert( _source is TGIS_CSAbstract ) ;

 FEPSG := TGIS_CSAbstract( _source ).FEPSG ;
 FWKT  := TGIS_CSAbstract( _source ).FWKT  ;
end ;

procedure TGIS_CSAbstract.MarkDeprecated(
  const _deprecated : Boolean
) ;
begin
  FDeprecated := _deprecated ;
end;


{$IFDEF CLR}
  function TGIS_CSAbstract.ToString : String ;
  begin
    if not IsStringEmpty( DescriptionEx ) then
      Result := DescriptionEx
    else
      Result := Description ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSAbstractListEnumerator'}

constructor TGIS_CSAbstractListEnumerator.Create(
  const _list : TGIS_CSAbstractList
) ;
begin
  inherited Create ;

  FList  := _list ;
  FIndex := -1 ;
end ;

{$IFDEF CLR}
  procedure TGIS_CSAbstractListEnumerator.Dispose ;
  begin
    {$IFNDEF OXYGENE}
      inherited ;
    {$ENDIF}
  end ;
{$ENDIF}

procedure TGIS_CSAbstractListEnumerator.Reset ;
begin
  FIndex := -1 ;
end ;

function TGIS_CSAbstractListEnumerator.MoveNext : Boolean ;
begin
  inc( FIndex ) ;
  Result := FIndex < FList.Count ;
end ;

function TGIS_CSAbstractListEnumerator.GetCurrent : TGIS_CSAbstract ;
begin
  Result := FList[ FIndex ] ;
end ;

{$IFDEF CLR}
  function TGIS_CSAbstractListEnumerator.fget_current_obj
    : TObject ;
  begin
    Result := FList[ FIndex ]  ;
  end ;
{$ENDIF}

{$IFDEF JAVA}
  method TGIS_CSAbstractListEnumerator.hasNext : Boolean ;
  begin
    Result := MoveNext ;
  end ;

  method TGIS_CSAbstractListEnumerator.next : TObject;
  begin
    Result := GetCurrent ;
  end ;

  method TGIS_CSAbstractListEnumerator.&remove ;
  begin

  end ;
{$ENDIF}


{$ENDREGION}
{$REGION 'TGIS_CSAbstractList'}

constructor TGIS_CSAbstractList.Create ;
begin
  inherited ;
  bUniqueId := True ;
  bUniqueWKT := True ;
  oListWKT  := TDictionary< String , TGIS_CSAbstract>.Create(
                 {$IFDEF OXYGENE}
                   {$IFDEF JAVA}
                     java.lang.String.CASE_INSENSITIVE_ORDER
                   {$ENDIF}
                   {$IFDEF CLR}
                     StringComparer.OrdinalIgnoreCase
                   {$ENDIF}
                 {$ELSE}
                   TIStringComparer.Ordinal
                 {$ENDIF}
               ) ;
  oListID   := TDictionary< Integer, TGIS_CSAbstract>.Create() ;
  bOwn  := True ;
  Init ;
end ;

constructor TGIS_CSAbstractList.Create(
  const _lst : TGIS_CSAbstractList
) ;
begin
  inherited Create ;
  bUniqueId := True ;
  bUniqueWKT := True ;
  if not assigned( _lst ) then begin
    oList     := TList< TGIS_CSAbstract >.Create ;
    oListWKT  := TDictionary< String , TGIS_CSAbstract>.Create() ;
    oListID   := TDictionary< Integer, TGIS_CSAbstract>.Create() ;
    bOwn  := True ;
    Init ;
  end
  else begin
    oList    := _lst.oList    ;
    oListWKT := _lst.oListWKT ;
    oListID  := _lst.oListID  ;
    bOwn  := False ;
  end ;

end ;

constructor TGIS_CSAbstractList.Create(
  const _lst : TGIS_CSAbstractList ;
  const _unique_id : Boolean ;
  const _unique_wkt : Boolean
) ;
begin
  inherited Create ;

  bUniqueId := _unique_id ;
  bUniqueWKT := _unique_wkt ;

  if not assigned( _lst ) then begin
    oList     := TList< TGIS_CSAbstract >.Create ;
    oListWKT  := TDictionary< String , TGIS_CSAbstract>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ENDIF}
                     {$IFDEF CLR}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                 ) ;
    oListID   := TDictionary< Integer, TGIS_CSAbstract>.Create() ;
    bOwn  := True ;
    Init ;
  end
  else begin
    oList    := _lst.oList    ;
    oListWKT := _lst.oListWKT ;
    oListID  := _lst.oListID  ;
    bOwn  := False ;
  end ;

end ;

procedure TGIS_CSAbstractList.doDestroy ;
var
  {$IFNDEF NEXTGEN}
    s   : String ;
  {$ENDIF}
  {$IFNDEF OXYGENE}
    itm : TGIS_CSAbstract ;
  {$ENDIF}
begin
  if bOwn then begin
    {$IFNDEF NEXTGEN}
      for itm in oList do begin
        s := itm.WKT ;
        FreeObjectNotNil( itm ) ;
      end ;
    {$ENDIF}
    FreeObject( oList    ) ;
    FreeObject( oListWKT ) ;
    FreeObject( oListID  ) ;
  end ;

  inherited ;
end ;

function TGIS_CSAbstractList.fget_WKTObject(
  _idx : Integer
) : TGIS_CSAbstract ;
begin
  LockThread ;
  try
    Result := TGIS_CSAbstract( oList[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAbstractList.uniqueWkt(
  const _epsg : Integer ;
  const _wkt  : String
) : String ;
var
  no      : Integer ;
  wkt     : String  ;
  tmp_wkt : String  ;

  procedure find_base( const _wkt : String ; var _basewkt : String ; var _baseno : Integer ) ;
  var
    k   : Integer ;
    tmp : String  ;
    c   : Char    ;
    fnd : Boolean ;
    chs : TCharSet ;
  begin
    fnd := False ;
    tmp := '' ;
    k := StringLast( _wkt ) ;
    chs := PrepareCharSet( ['09'] ) ;
    while k >= StringFirst+2 do begin
      c := _wkt[k] ;
      if      InCharSet( c, chs ) then begin
                                         tmp := c + tmp ;
                                       end
    else if c = '_'                    then begin
                                         if length( tmp ) > 0 then
                                           fnd := True ;
                                         break ;
                                       end ;

      dec( k ) ;
    end ;

    _baseno  := StrToInt( '0' + tmp ) ;

    if _baseno > 20 then
      fnd := False ;

    if fnd then begin
      _basewkt := Copy( _wkt, StringFirst, k-StringFirst ) ;
    end
    else begin
      _baseno  := 1 ;
      _basewkt := _wkt ;
    end ;

  end ;
begin
  if fnEPSG( _epsg ) >= GIS_EPSG_AUTO then begin
    if not assigned( self.ByWKT( _wkt ) ) then begin
      Result := _wkt ;
    end
    else begin
      find_base( _wkt, tmp_wkt, no ) ;
      while True do begin
        wkt :=  tmp_wkt + '_' + IntToStr( no ) ;
        if not assigned( self.ByWKT( wkt ) ) then begin
          Result := wkt ;
          break ;
        end ;
        inc( no ) ;
      end ;
    end ;
  end
  else
    Result := _wkt ;
end ;

function TGIS_CSAbstractList.Add(
  const _obj : TGIS_CSAbstract
) : TGIS_CSAbstract ;
begin
  LockThread ;
  try
    Result := _obj ;

    assert( Result.EPSG > 0  ) ;
    assert( Result.WKT  > '' ) ;

    if oListWKT.ContainsKey( _obj.WKT ) then begin
      if bUniqueWKT then
        raise Exception.Create( 'Duplicated WKT' ) ;
    end
    else

    oListWKT.Add( _obj.WKT, _obj );

    if oListID.ContainsKey( _obj.EPSG ) then begin
      if bUniqueId then
        raise Exception.Create( 'Duplicated ID' ) ;
    end
    else

    oListID.Add( _obj.EPSG, _obj );

    oList.Add( _obj )
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAbstractList.Fix(
  const _obj : TGIS_CSAbstract
) : TGIS_CSAbstract ;
var
  o   : TGIS_CSAbstract ;
begin
  LockThread ;
  try
    Result := nil ;

    assert( _obj.EPSG   > 0   ) ;
    assert( _obj.WKT    > ''  ) ;

    if oListID.TryGetValue( _obj.EPSG, o ) then begin
      o.Assign( _obj ) ;

      oList.Remove( o );
      oListID.Remove( o.EPSG );
      oListWKT.Remove( o.WKT );

      Add( o ) ;

      Result := o ;
    end
    else
      raise Exception.Create( 'Non existing item' ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$IFDEF GIS_IMPORTEXTRA}
  procedure TGIS_CSAbstractList.ChangeEPSG(
    const _epsg   : Integer    ;
    const _master : Integer
  ) ;
  var
    o : TGIS_CSAbstract ;
  begin
    o := ByEPSG( _epsg ) ;
    oList.Remove( o ) ;
    oListID.Remove( o.EPSG );
    oListWKT.Remove( o.WKT );

    o.FEPSG := _master ;

    Add( o ) ;


  end ;

  procedure TGIS_CSAbstractList.DeleteEPSG(
    const _min : Integer    ;
    const _max : Integer
  ) ;
  var
    o   : TGIS_CSAbstract ;
    lst : TList<Integer>  ;
    i   : Integer ;
  begin
    lst := TList<Integer>.Create ;
    try
      for o in Self do begin
        if ( o.EPSG >= _min ) and ( o.EPSG <= _max ) then
          lst.Add( o.EPSG ) ;
      end ;
     for i in lst do begin
       if oListID.TryGetValue( i, o ) then begin
         oList.Remove( o ) ;
         oListID.Remove( o.EPSG );
         oListWKT.Remove( o.WKT );
         o.Free ;
       end;
      end ;
    finally
      FreeObject( lst ) ;
    end;
  end ;
{$ENDIF}


procedure TGIS_CSAbstractList.Alias(
  const _epsg   : Integer    ;
  const _master : Integer
) ;
var
  o : TGIS_CSAbstract ;
begin
  o := ByEPSG( _epsg ) ;

  assert( assigned( o ) ) ;
  assert( o.EPSG = _epsg ) ;
  assert( o.MasterEPSG = 0 ) ;

  o.FMasterEPSG := _master ;
end ;

procedure TGIS_CSAbstractList.AddAlias(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _master         : Integer
) ;
var
  o : TGIS_CSAbstract ;
begin
  LockThread ;
  try
    assert( ByEPSG( _master ) <> nil ) ;
    assert( _wkt               > ''  ) ;

    if assigned( ByWKT( _wkt ) ) and bUniqueWKT then begin
      raise Exception.Create( 'Duplicated WKT' ) ;
    end ;

    o :=  Add( TGIS_CSAbstract.Create( _epsg, uniqueWkt( _epsg,_wkt ) ) ) ;

    if _master > 0  then
      Alias( o.EPSG, _master ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSAbstractList.Clear ;
begin
  LockThread ;
  try
    oList.Clear ;
    oListWKT.Clear ;
    oListID.Clear  ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAbstractList.Count
  : Integer ;
begin
  LockThread ;
  try
    Result := oList.Count ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAbstractList.ByEPSG(
  const _epsg  : Integer
) : TGIS_CSAbstract ;
begin
  Result := nil ;
  if _epsg < 1 then exit ;

  if oListID.TryGetValue( _epsg, Result ) then begin
    if Result.MasterEPSG <> 0 then
      Result := ByEPSG( Result.MasterEPSG ) ;
  end
  else
    Result := nil ;
end ;

function TGIS_CSAbstractList.ByWKT(
  const _wkt   : String
) : TGIS_CSAbstract ;
var
  wkt   : String  ;
begin
  Result := nil ;
  if IsStringEmpty( _wkt )then exit ;

  wkt := CanonicalWKT( _wkt ) ;

  if oListWKT.TryGetValue( wkt, Result ) then begin
    if Result.MasterEPSG <> 0 then
      Result := ByEPSG( Result.MasterEPSG ) ;
  end
  else
    Result := nil ;
end ;

function TGIS_CSAbstractList.GetEnumerator
  : {$IFDEF CLR} IEnumerator {$ELSE} TGIS_CSAbstractListEnumerator {$ENDIF} ;
begin
  Result := TGIS_CSAbstractListEnumerator.Create( self ) ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSUnits}

constructor TGIS_CSUnits.Create;
begin
  inherited ;

  FUnitsType := TGIS_CSUnitsType.Linear ;
  FFactor    := 1 ;
end ;

constructor TGIS_CSUnits.Create(
  const _epsg   : Integer ;
  const _wkt    : String  ;
  const _symbol : String  ;
  const _type   : TGIS_CSUnitsType ;
  const _factor : Double
) ;
begin
  assert( _factor  > 0  ) ;

  inherited Create( _epsg, _wkt ) ;

  if not IsStringEmpty( _symbol ) then
    FSymbol := _symbol
  else
    FSymbol := StringReplaceAll( _wkt, '_', ' ' ) ;

  FUnitsType := _type   ;
  FFactor    := _factor ;
end ;

constructor TGIS_CSUnits.Create(
  const _epsg     : Integer ;
  const _wkt      : String  ;
  const _subunits : array of Integer
) ;
var
  i : Integer ;
begin
  inherited Create( _epsg, _wkt ) ;

  FSymbol := '' ;

  SetLength( arUnits, length( _subunits ) ) ;

  for i := 0 to high( _subunits ) do begin
    arUnits[i] := _subunits[i] ;
  end ;

  FUnitsType := TGIS_CSUnitsType.Auto ;
  FFactor    := 1 ;
end ;

function TGIS_CSUnits.prepareString(
  const _unt    : TGIS_CSUnits ;
  const _value  : Double ;
  const _prec   : Boolean ;
  const _format : String
) : String ;
var
  suffix : String ;
  prec   : Integer ;
const
  MAX_PREC   = 10 ;
begin
  if IsStringEmpty( _format ) or ( _unt.UnitsType = TGIS_CSUnitsType.Areal ) then
    suffix := _unt.Symbol
  else
    suffix := Format( _format, [_unt.Symbol] ) ;

  if _prec then begin
    if _value >= 1000 then
      Result := Format( '%.0f %s', [ _value, suffix ] )
    else if _value >= 100 then
      Result := Format( '%.1f %s', [ _value, suffix ] )
    else if _value >= 1 then
      Result := Format( '%.2f %s', [ _value, suffix ] )
    else if _value >= 0.01 then
      Result := Format( '%.3f %s', [ _value, suffix ] )
    else if _value >= 0.0001 then
      Result := Format( '%.4f %s', [ _value, suffix ] )
    else
      Result := Format( '%.5f %s', [ _value, suffix ] ) ;
  end
  else begin
    prec := TruncS( Log10( Abs( _value ) ) ) + 1 - MAX_PREC ;
    {$IFNDEF JAVA}
      Result := Format( '%g %s', [ RoundTo( _value, prec ), suffix ] ) ;
    {$ELSE}
      Result :=  new java.text.DecimalFormat().format(RoundTo( _value, prec )) + " " + suffix ;
    {$ENDIF}
  end ;
end ;

procedure TGIS_CSUnits.Assign(
  const _source : TObject
) ;
var
  i : Integer ;
begin
  assert( _source is TGIS_CSUnits ) ;

  inherited ;

  FSymbol    := TGIS_CSUnits( _source ).FSymbol    ;
  FUnitsType := TGIS_CSUnits( _source ).FUnitsType ;
  FFactor    := TGIS_CSUnits( _source ).FFactor    ;

  SetLength( arUnits, length( TGIS_CSUnits( _source ).arUnits ) ) ;

  for i:= 0 to high( TGIS_CSUnits( _source ).arUnits ) do begin
    arUnits[i] := TGIS_CSUnits( _source ).arUnits[i] ;
  end ;
end ;

function TGIS_CSUnits.ToBase(
  const _value : Double
) : Double ;
begin
  Result := _value * FFactor ;
end ;

function TGIS_CSUnits.FromBase(
  const _value : Double
) : Double ;
begin
  Result := _value / FFactor ;
end ;

function TGIS_CSUnits.ToUnits(
  const _units : TGIS_CSUnits ;
  const _value : Double
) : Double ;
var
  val : Double ;
begin
  assert( assigned( _units ) ) ;
  assert( _units.UnitsType = self.UnitsType ) ;

  val := self.ToBase( _value ) ;
  Result := _units.FromBase( val ) ;
end ;

function TGIS_CSUnits.FromUnits(
  const _units : TGIS_CSUnits ;
  const _value : Double
) : Double ;
var
  val : Double ;
begin
  assert( assigned( _units ) ) ;
  assert( _units.UnitsType = self.UnitsType ) ;

  val := _units.ToBase( _value ) ;
  Result := self.FromBase( val ) ;
end ;

function TGIS_CSUnits.AsLinear(
  const _value   : Double  ;
  const _prec    : Boolean
) : String ;
var
  ount : TGIS_CSUnits ;
  vtmp : Double       ;
begin
  vtmp := _value ;
  ount := AutoSelect( False, vtmp ) ;

  Result := prepareString( ount, vtmp, _prec, '' ) ;
end ;

function TGIS_CSUnits.AsAreal(
  const _value   : Double   ;
  const _prec    : Boolean  ;
  const _format  : String
) : String ;
var
  ount   : TGIS_CSUnits ;
  vtmp   : Double       ;
begin
  vtmp := _value ;
  ount := AutoSelect( True, vtmp ) ;

  Result := prepareString( ount, vtmp, _prec, _format ) ;
end ;

function TGIS_CSUnits.AsAngular(
  const _value   : Double   ;
  const _prec    : Boolean
) : String ;
begin
  if _prec then
    Result := Format( '%f.8 %s', [ _value, self.Symbol ] )
  else
    Result := Format( '%g %s', [ _value, self.Symbol ] ) ;
end ;

function TGIS_CSUnits.AutoSelect(
  const _areal  : Boolean ;
  var   _value  : Double
) : TGIS_CSUnits ;
var
  i        : Integer      ;
  ount     : TGIS_CSUnits ;
  dval     : Double       ;
  ount_sel : TGIS_CSUnits ;
  dval_sel : Double       ;
begin
  Result := self ;

   if UnitsType = TGIS_CSUnitsType.Auto then begin
    dval     := 0 ;
    dval_sel := 0 ;
    ount_sel := nil ;

    for i:= 0 to high( arUnits ) do begin
      ount := CSUnitsList.ByEPSG( arUnits[i] ) ;

      if _areal then begin
        case ount.UnitsType of
          TGIS_CSUnitsType.Linear  :
            dval := _value / ( ount.Factor * ount.Factor ) ;
          TGIS_CSUnitsType.Areal :
            dval := _value / ount.Factor ;
          else
            continue ;
        end ;
      end
      else begin
        case ount.UnitsType of
          TGIS_CSUnitsType.Linear  :
            dval := _value / ount.Factor ;
          else
            continue ;
        end ;
      end ;

      if i = 0 then begin
        dval_sel  := dval  ;
        ount_sel  := ount ;
      end ;

      if dval < 0.5 then
        break ;

      dval_sel := dval ;
      ount_sel := ount ;
    end ;

    if assigned( ount_sel ) then begin
      _value := dval_sel ;
      Result := ount_sel ;
    end
    else begin
      Result := CSUnitsList.ByEPSG( 9001 ) ;
    end ;
  end
  else begin
    if _areal then begin
      case UnitsType of
        TGIS_CSUnitsType.Linear  :
          _value := _value / ( Factor * Factor ) ;
        TGIS_CSUnitsType.Areal :
          _value := _value / Factor ;
        else begin
          _value := 0 ;
        end ;
      end ;
    end
    else begin
      case UnitsType of
        TGIS_CSUnitsType.Linear  :
          _value := _value / Factor ;
        else begin
          _value := 0 ;
        end ;
      end ;
    end ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSPrimeMeridian'}

constructor TGIS_CSPrimeMeridian.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _longitude : Double
) ;
begin
  inherited Create( _epsg, _wkt ) ;
  FLongitude := _longitude ;
end ;

procedure TGIS_CSPrimeMeridian.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSPrimeMeridian ) ;

  inherited ;

  FLongitude := TGIS_CSPrimeMeridian( _source ).FLongitude ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSEllipsoid'}

constructor TGIS_CSEllipsoid.Create(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _semi_major     : Double  ;
  const _inv_flattering : Double
) ;
begin
  assert( _semi_major  > 0  ) ;

  inherited Create( _epsg, _wkt ) ;

  FSemiMajor       := _semi_major      ;

  if _epsg = 0  then begin
    FSemiMinor       := 0 ;
    FSemiMajorSq     := 0 ;
    FSemiMinorSq     := 0 ;
    FEcntrMajor      := 0 ;
    FEcntrMajorSqrt  := 0 ;
    FEcntrMinor      := 0 ;
    FEcntrMinorSqrt  := 0 ;
  end
  else begin
    FInverseFlattering := _inv_flattering ;

    if _inv_flattering <> 0 then FFlattering := 1/_inv_flattering
                            else FFlattering := 0 ;

    FSemiMinor       := FSemiMajor - FFlattering * FSemiMajor ;
    FSemiMajorSq     := Sqr( SemiMajor ) ;
    FSemiMinorSq     := Sqr( SemiMinor ) ;
    FEcntrMajor      := (FSemiMajorSq - FSemiMinorSq) / FSemiMajorSq ;
    FEcntrMajorSqrt  := Sqrt( FEcntrMajor ) ;

    if FSemiMinorSq <> 0 then
      FEcntrMinor    := (FSemiMajorSq - FSemiMinorSq) / FSemiMinorSq
    else
      FEcntrMinor    := 0 ;
    FEcntrMinorSqrt  := Sqrt( FEcntrMinor ) ;
  end ;
end ;

procedure TGIS_CSEllipsoid.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSEllipsoid ) ;

  inherited ;

  FSemiMajor          := TGIS_CSEllipsoid( _source ).FSemiMajor          ;
  FSemiMinor          := TGIS_CSEllipsoid( _source ).FSemiMinor          ;
  FFlattering         := TGIS_CSEllipsoid( _source ).FFlattering         ;
  FInverseFlattering  := TGIS_CSEllipsoid( _source ).FInverseFlattering  ;
  FSemiMajorSq        := TGIS_CSEllipsoid( _source ).FSemiMajorSq        ;
  FSemiMinorSq        := TGIS_CSEllipsoid( _source ).FSemiMinorSq        ;
  FEcntrMajor         := TGIS_CSEllipsoid( _source ).FEcntrMajor         ;
  FEcntrMajorSqrt     := TGIS_CSEllipsoid( _source ).FEcntrMajorSqrt     ;
  FEcntrMinor         := TGIS_CSEllipsoid( _source ).FEcntrMinor         ;
  FEcntrMinorSqrt     := TGIS_CSEllipsoid( _source ).FEcntrMinorSqrt     ;
end ;

procedure  TGIS_CSEllipsoid.ToGeodetic_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  w         : Double  ; // distance from Z axis
  w_sq      : Double  ; // square of distance from Z axis
  t_tmp     : Double  ; // initial estimate of vertical component
  t         : Double  ; // corrected estimate of vertical component
  s_tmp     : Double  ; // initial estimate of horizontal component
  s         : Double  ; // corrected estimate of horizontal component
  sin_b_tmp : Double  ; // Sin(b0), b0 is estimate of Bowring aux variable
  cos_b_tmp : Double  ; // Cos(b0)
  sin_p_tmp : Double  ; // Sin(phi1), phi1 is estimated latitude
  cos_p_tmp : Double  ; // Cos(phi1)
  rn        : Double  ; // Earth radius at location
  sum       : Double  ; // numerator of Cos(phi1)
  pole      : Boolean ; // indicates location is in polar region
  res_x     : Double  ;
  res_y     : Double  ;
begin
  res_y := 0 ;
  pole  := False ;

  if _ptg.X <> 0 then
    res_x := ArcTan2( _ptg.Y, _ptg.X )
  else begin
    if      _ptg.Y > 0 then res_x :=  HALFPI
    else if _ptg.Y < 0 then res_x := -HALFPI
    else begin
      pole := True ;
      res_x := 0 ;
      if      _ptg.Z > 0 then res_y :=  HALFPI // north pole
      else if _ptg.Z < 0 then res_y := -HALFPI // south pole
      else begin
        // center of earth
        _ptg.X := res_x      ;
        _ptg.Y := HALFPI     ;
        _ptg.Z := -SemiMinor ;
        exit ;
      end ;
    end ;
  end ;

  w_sq      := _ptg.X*_ptg.X + _ptg.Y*_ptg.Y ;
  w         := Sqrt( w_sq );
  t_tmp     := _ptg.Z * AD_C;
  s_tmp     := Sqrt( t_tmp*t_tmp + w_sq ) ;
  sin_b_tmp := t_tmp / s_tmp ;
  cos_b_tmp := w     / s_tmp ;
  t         := _ptg.Z + SemiMinor *
               FEcntrMinor * ( sin_b_tmp * sin_b_tmp * sin_b_tmp ) ;
  sum       := w - SemiMajor *
               FEcntrMajor * ( cos_b_tmp * cos_b_tmp * cos_b_tmp ) ;
  s         := Sqrt( t*t + sum*sum ) ;
  sin_p_tmp := t   / s ;
  cos_p_tmp := sum / s ;
  rn      := SemiMajor / Sqrt( 1 - FEcntrMajor * sin_p_tmp * sin_p_tmp );

  if      cos_p_tmp >=  COS_67_5 then _ptg.Z := w /  cos_p_tmp - rn
  else if cos_p_tmp <= -COS_67_5 then _ptg.Z := w / -cos_p_tmp - rn
  else                                _ptg.Z := (_ptg.Z/sin_p_tmp) +
                                                rn * ( FEcntrMajor-1 ) ;

  if not pole then
    _ptg.Y := ArcTan( sin_p_tmp / cos_p_tmp )
  else
    _ptg.Y := res_y ;

  _ptg.X := res_x ;
end ;

procedure  TGIS_CSEllipsoid.ToGeocentric_Ref(
  {$IFNDEF JAVA} var {$ENDIF}  _ptg : TGIS_Point3D
) ;
var
  rn       : Double ; // Earth radius at this location
  long     : Double ; // temporary longitude value
  sin_lat  : Double ; // Sin(Latitude)
  cos_lat  : Double ; // Cos(Latitude)
  sl, cl   : Double ;
begin
  if      _ptg.Y < -HALFPI then _ptg.Y := -HALFPI
  else if _ptg.Y >  HALFPI then _ptg.Y :=  HALFPI ;
  if      _ptg.X < -Pi     then _ptg.X := -Pi     ;

  if ( _ptg.X > Pi ) then long := _ptg.X - ( 2*Pi )
                     else long := _ptg.X ;

  SinCos( _ptg.Y, sin_lat, cos_lat ) ;
  rn := SemiMajor / ( Sqrt( 1 - FEcntrMajor * ( sin_lat*sin_lat ) ) ) ;

  SinCos( long, sl, cl ) ;
  _ptg.X := ( rn + _ptg.Z ) * cos_lat * cl ;
  _ptg.Y := ( rn + _ptg.Z ) * cos_lat * sl ;
  _ptg.Z := ( (rn * (1 - FEcntrMajor) ) + _ptg.Z ) * sin_lat ;

end ;

function TGIS_CSEllipsoid.ToGeodetic(
  const _coords : TGIS_Point3D
) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _coords.X, _coords.Y, _coords.Z, _coords.M ) ;
  {$ELSE}
    tmp := _coords ;
  {$ENDIF}

  ToGeodetic_Ref( tmp );

  Result := tmp ;
end ;

function TGIS_CSEllipsoid.ToGeocentric(
  const _coords : TGIS_Point3D
) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _coords.X, _coords.Y, _coords.Z, _coords.M ) ;
  {$ELSE}
    tmp := _coords ;
  {$ENDIF}

  ToGeocentric_Ref( tmp );

  Result := tmp ;
end ;

function TGIS_CSEllipsoid.Distance(
  const _ptg_a : TGIS_Point ;
  const _ptg_b : TGIS_Point
) : Double ;
var
  si,co,th,ca    : Double ;
  a1,a2,l1,l2,ep : Double ;
  lon1, lon2     : Double ;
  lat1, lat2     : Double ;
  tmp            : Double ;
  v              : Double ;
  pifix          : Double ;
  mmsq           : Double ;
begin
  mmsq := FSemiMinorSq / FSemiMajorSq ;
  // latitudes as geocentric
  lat1 := ArcTan( mmsq * Tan( _ptg_a.Y ) ) ;
  lat2 := ArcTan( mmsq * Tan( _ptg_b.Y ) ) ;

  lon1 := _ptg_a.X ;
  lon2 := _ptg_b.X ;

  // central angle
  v := Sin( lat1 ) * Sin( lat2 ) +
       Cos( lat1 ) * Cos( lat2 ) * Cos( lon2 - lon1 ) ;

  if      v > 1 then v :=  1
  else if v< -1 then v := -1 ;

  ca := ArcCos( v ) ;

  // calculate angles along great ellipse from equator
  if lat1 > lat2 then begin
    tmp  := lat2 ;
    lat2 := lat1 ;
    lat1 := tmp  ;
  end ;

  si := Sin( lat1 ) * Sin( ca ) ;
  co := Sin( lat2 ) - Sin( lat1 ) * Cos( ca ) ;
  if co < 1e-10 then co := 0 ;
  a1 := ArcTan2( si, co ) ;
  a2 := a1 + ca ;

  // calculate ellipticity (squared) of great circle
  if Abs( a2 ) < 1e-10 then begin
    pifix := Abs( RoundS( ( lon1 - lon2 ) / Pi  )*Pi ) ;
    if Abs( lon1-lon2 ) - pifix < 1e-8 then begin
      if lon1 > lon2 then
        lon1 := lon1 - pifix
      else
        lon2 := lon2 - pifix ;
    end;

    Result := Sqrt( Sqr( lon1 - lon2 ) + Sqr( _ptg_a.Y - _ptg_b.Y ) ) ;
    Result := Result * FSemiMajor ;
  end
  else begin
    th := Sin( lat2 ) / Sin( a2 ) ;
    ep := ( 1 - mmsq ) * th*th ;

    // calculate great circle distance from equator for each
    l1 := FSemiMajor * ( a1 - ep * a1 / 4.0  +  ep * Sin(2.0 * a1 ) / 8.0 ) ;
    l2 := FSemiMajor * ( a2 - ep * a2 / 4.0  +  ep * Sin(2.0 * a2 ) / 8.0 ) ;

    Result := l2-l1 ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSArea'}

function TGIS_CSArea.fget_Description
  : String ;
begin
  Result := FDescription
end ;

{$ENDREGION}
{$REGION 'TGIS_CSDatum' }

constructor TGIS_CSDatum.Create(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer
) ;
begin
  Create( _epsg, _wkt, _ellipsoid, _transform, 0 ) ;
end ;

constructor TGIS_CSDatum.Create(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer ;
  const _fallback       : Integer
) ;
begin
  inherited Create( _epsg, _wkt ) ;

  FEllipsoid     := CSEllipsoidList.ByEPSG( _ellipsoid ) ;
  if not assigned( FEllipsoid ) then
    raise EGIS_Exception.Create(
            _rsrc( GIS_RS_ERR_PRJ_ELLIPSOID_NOEXIST ), '',
            _ellipsoid
          );

  oEllipsoidWGS  := CSEllipsoidList.ByEPSG( 7030 ) ;
  assert( assigned( oEllipsoidWGS ) ) ;

  FTransform     := CSTransformList.ByEPSG( _transform ) ;

  if FTransform is TGIS_CSTransformGridShift then
    TGIS_CSTransformGridShift( FTransform ).Fallback := _fallback ;
  FFallback := _fallback ;
end ;

procedure TGIS_CSDatum.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSDatum ) ;

  inherited ;

  FEllipsoid := TGIS_CSDatum( _source ).FEllipsoid ;
  FTransform := TGIS_CSDatum( _source ).FTransform ;

  if FTransform is TGIS_CSTransformGridShift then
    FFallback := TGIS_CSTransformGridShift( FTransform ).Fallback ;
end ;

procedure TGIS_CSDatum.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
const
  MAX_X = Pi-Pi/180/60 ;
  MAX_Y = Pi/2-Pi/180/60 ;
begin
  if assigned( FTransform) then begin

    // do not transform on edges to avoid sgift like:
    // -180 + shift makes 180
    if ( _ptg.X > -MAX_X ) and
       ( _ptg.X <  MAX_X ) and
       ( _ptg.Y > -MAX_Y ) and
       ( _ptg.Y <  MAX_Y )
    then begin
      FTransform.FromWGS3D_Ref( _ptg ) ;
    end ;
  end;
end ;

procedure TGIS_CSDatum.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
const
  MAX_X = Pi-Pi/180/60 ;
  MAX_Y = Pi/2-Pi/180/60 ;
begin
  if assigned( FTransform) then begin

    // do not transform on edges to avoid sgift like:
    // -180 + shift makes 180
    if ( _ptg.X > -MAX_X ) and
       ( _ptg.X <  MAX_X ) and
       ( _ptg.Y > -MAX_Y ) and
       ( _ptg.Y <  MAX_Y )
    then begin
      FTransform.ToWGS3D_Ref( _ptg )
    end ;

  end;
end ;

function TGIS_CSDatum.FromWGS(
  const _ptg : TGIS_Point
) : TGIS_Point ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D ;
  {$ENDIF}
  tmp.X := _ptg.X ;
  tmp.Y := _ptg.Y ;
  tmp.Z := 0 ;
  tmp.M := 0 ;

  FromWGS3D_Ref( tmp ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := tmp.X ;
  Result.Y := tmp.Y ;
end ;

function TGIS_CSDatum.ToWGS(
  const _ptg : TGIS_Point
) : TGIS_Point ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D ;
  {$ENDIF}
  tmp.X := _ptg.X ;
  tmp.Y := _ptg.Y ;
  tmp.Z := 0 ;
  tmp.M := 0 ;

  ToWGS3D_Ref( tmp ) ;

  {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point ;
  {$ENDIF}
  Result.X := tmp.X ;
  Result.Y := tmp.Y ;
end ;

function TGIS_CSDatum.FromWGS3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  FromWGS3D_Ref( tmp ) ;

  Result := tmp ;
end ;

function TGIS_CSDatum.ToWGS3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  ToWGS3D_Ref( tmp ) ;

  Result := tmp ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSTransformAbstract'}

constructor TGIS_CSTransformAbstract.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt ) ;

  FMethod := 0 ;

  FEllipsoid := _ellipsoid ;
  FNumber    := _number    ;
  FArea      := _area      ;

  FA := _a ;
  FB := _b ;
  FC := _c ;
  FD := _d ;
  FE := _e ;
  FF := _f ;
  FG := _g ;
  FH := _h ;
  FI := _i ;
  FJ := _j ;

  oEllipsoid  := CSEllipsoidList.ByEPSG( _ellipsoid ) ;
  if not assigned( oEllipsoid ) then
    raise EGIS_Exception.Create(
            _rsrc( GIS_RS_ERR_PRJ_ELLIPSOID_NOEXIST ), '',
            _ellipsoid
          );

  oEllipsoidWGS  := CSEllipsoidList.ByEPSG( 7030 ) ;
  assert( assigned( oEllipsoidWGS ) ) ;

end ;

procedure TGIS_CSTransformAbstract.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformAbstract ) ;

  inherited ;

  FMethod       := TGIS_CSTransformAbstract( _source ).FMethod       ;
  FEllipsoid    := TGIS_CSTransformAbstract( _source ).FEllipsoid    ;
  FNumber       := TGIS_CSTransformAbstract( _source ).FNumber       ;
  FArea         := TGIS_CSTransformAbstract( _source ).FArea         ;

  oEllipsoid    := TGIS_CSTransformAbstract( _source ).oEllipsoid    ;
  oEllipsoidWGS := TGIS_CSTransformAbstract( _source ).oEllipsoidWGS ;

  FA            := TGIS_CSTransformAbstract( _source ).FA            ;
  FB            := TGIS_CSTransformAbstract( _source ).FB            ;
  FC            := TGIS_CSTransformAbstract( _source ).FC            ;
  FD            := TGIS_CSTransformAbstract( _source ).FD            ;
  FE            := TGIS_CSTransformAbstract( _source ).FE            ;
  FF            := TGIS_CSTransformAbstract( _source ).FF            ;
  FG            := TGIS_CSTransformAbstract( _source ).FG            ;
  FH            := TGIS_CSTransformAbstract( _source ).FH            ;
  FI            := TGIS_CSTransformAbstract( _source ).FI            ;
  FJ            := TGIS_CSTransformAbstract( _source ).FJ            ;
  FGridShift    := TGIS_CSTransformAbstract( _source ).FGridShift    ;
end ;

function TGIS_CSTransformAbstract.ToWGS(
  const _ptg : TGIS_Point
) : TGIS_Point ;
var
  ptg : TGIS_Point3D ;
begin
  ptg := GisPoint3DFrom2D( _ptg ) ;
  ToWGS3D_Ref( ptg ) ;
  Result := GisPoint2DFrom3D( ptg ) ;
end ;

function TGIS_CSTransformAbstract.FromWGS(
  const _ptg : TGIS_Point
) : TGIS_Point ;
var
  ptg : TGIS_Point3D ;
begin
  ptg := GisPoint3DFrom2D( _ptg ) ;
  FromWGS3D_Ref( ptg ) ;
  Result := GisPoint2DFrom3D( ptg ) ;
end ;

function TGIS_CSTransformAbstract.ToWGS3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
var
  ptg : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    ptg := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    ptg := _ptg ;
  {$ENDIF}
  ToWGS3D_Ref( ptg ) ;
  Result := ptg ;
end ;

function TGIS_CSTransformAbstract.FromWGS3D(
  const _ptg : TGIS_Point3D
) : TGIS_Point3D ;
var
  ptg : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    ptg := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    ptg := _ptg ;
  {$ENDIF}
  FromWGS3D( ptg ) ;
  Result := ptg ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSTransformGeocentricTranslation'}

constructor TGIS_CSTransformGeocentricTranslations.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9603 ;

  dX := _a ;
  dY := _b ;
  dZ := _c ;
  assert( _d = 0 ) ;
  assert( _e = 0 ) ;
  assert( _f = 0 ) ;
  assert( _g = 0 ) ;
  assert( _h = 0 ) ;
  assert( _i = 0 ) ;
  assert( _j = 0 ) ;
end ;

procedure TGIS_CSTransformGeocentricTranslations.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X + dX ;
  _ptg.Y := _ptg.Y + dY ;
  _ptg.Z := _ptg.Z + dZ ;
end ;

procedure TGIS_CSTransformGeocentricTranslations.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X - dX ;
  _ptg.Y := _ptg.Y - dY ;
  _ptg.Z := _ptg.Z - dZ ;
end ;

procedure TGIS_CSTransformGeocentricTranslations.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoid.ToGeocentric_Ref( _ptg ) ;
  doForward( _ptg ) ;
  oEllipsoidWGS.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformGeocentricTranslations.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoidWGS.ToGeocentric_Ref( _ptg ) ;
  doReverse( _ptg ) ;
  oEllipsoid.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformGeocentricTranslations.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformGeocentricTranslations ) ;

  inherited ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformGeocentricTranslations.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformGeocentricTranslations.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformPositionVector7Params'}

constructor TGIS_CSTransformPositionVector7Params.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9606;

  dX := _a ;
  dY := _b ;
  dZ := _c ;
  rX := _d ;
  rY := _e ;
  rZ := _f ;
  dM := 1 + _g ;

  {$IFDEF JAVA}
    InitializeMatrixArray(arFwd) ;
  {$ENDIF}
  arFwd[1,1] :=   1   ;
  arFwd[1,2] :=   rZ  ;
  arFwd[1,3] := - rY  ;
  arFwd[2,1] := - rZ  ;
  arFwd[2,2] :=   1   ;
  arFwd[2,3] :=   rX  ;
  arFwd[3,1] :=   rY  ;
  arFwd[3,2] := - rX  ;
  arFwd[3,3] :=   1   ;

  arInv := invMatrix( arFwd ) ;
end ;

procedure TGIS_CSTransformPositionVector7Params.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := dM * ( tmp.X * arFwd[1,1] +
                   tmp.Y * arFwd[2,1] +
                   tmp.Z * arFwd[3,1]
                 ) + dX ;
  _ptg.Y := dM * ( tmp.X * arFwd[1,2] +
                   tmp.Y * arFwd[2,2] +
                   tmp.Z * arFwd[3,2]
                 ) + dY ;
  _ptg.Z := dM * ( tmp.X * arFwd[1,3] +
                   tmp.Y * arFwd[2,3] +
                   tmp.Z * arFwd[3,3]
                 ) + dZ ;
  _ptg.M := tmp.M ;

end ;

procedure TGIS_CSTransformPositionVector7Params.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := ( ( tmp.X - dX ) * arInv[1,1] +
              ( tmp.Y - dY ) * arInv[2,1] +
              ( tmp.Z - dZ ) * arInv[3,1]
            ) / dM ;
  _ptg.Y := ( ( tmp.X - dX ) * arInv[1,2] +
              ( tmp.Y - dY ) * arInv[2,2] +
              ( tmp.Z - dZ ) * arInv[3,2]
            ) / dM ;
  _ptg.Z := ( ( tmp.X - dX ) * arInv[1,3] +
              ( tmp.Y - dY ) * arInv[2,3] +
              ( tmp.Z - dZ ) * arInv[3,3]
            ) / dM ;
  _ptg.M := tmp.M ;
end ;

procedure TGIS_CSTransformPositionVector7Params.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoid.ToGeocentric_Ref( _ptg ) ;
  doForward( _ptg ) ;
  oEllipsoidWGS.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformPositionVector7Params.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoidWGS.ToGeocentric_Ref( _ptg ) ;
  doReverse( _ptg ) ;
  oEllipsoid.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformPositionVector7Params.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformPositionVector7Params ) ;

  inherited ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformPositionVector7Params.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformPositionVector7Params.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformCoordinateFrameRotation'}

constructor TGIS_CSTransformCoordinateFrameRotation.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9607;

  dX := _a ;
  dY := _b ;
  dZ := _c ;
  rX := _d ;
  rY := _e ;
  rZ := _f ;
  dM := 1 + _g ;

  {$IFDEF JAVA}
    InitializeMatrixArray(arFwd) ;
  {$ENDIF}
  arFwd[1,1] :=   1  ;
  arFwd[1,2] := - rZ ;
  arFwd[1,3] :=   rY ;
  arFwd[2,1] :=   rZ ;
  arFwd[2,2] :=   1  ;
  arFwd[2,3] := - rX ;
  arFwd[3,1] := - rY ;
  arFwd[3,2] :=   rX ;
  arFwd[3,3] :=   1  ;

  arInv := invMatrix( arFwd ) ;
end ;

procedure TGIS_CSTransformCoordinateFrameRotation.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := dM * ( tmp.X * arFwd[1,1] +
                   tmp.Y * arFwd[2,1] +
                   tmp.Z * arFwd[3,1]
                 ) + dX ;
  _ptg.Y := dM * ( tmp.X * arFwd[1,2] +
                   tmp.Y * arFwd[2,2] +
                   tmp.Z * arFwd[3,2]
                 ) + dY ;
  _ptg.Z := dM * ( tmp.X * arFwd[1,3] +
                   tmp.Y * arFwd[2,3] +
                   tmp.Z * arFwd[3,3]
                 ) + dZ ;
  _ptg.M := tmp.M ;
end ;

procedure TGIS_CSTransformCoordinateFrameRotation.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := ( ( tmp.X - dX ) * arInv[1,1] +
              ( tmp.Y - dY ) * arInv[2,1] +
              ( tmp.Z - dZ ) * arInv[3,1]
            ) / dM ;
  _ptg.Y := ( ( tmp.X - dX ) * arInv[1,2] +
              ( tmp.Y - dY ) * arInv[2,2] +
              ( tmp.Z - dZ ) * arInv[3,2]
            ) / dM ;
  _ptg.Z := ( ( tmp.X - dX ) * arInv[1,3] +
              ( tmp.Y - dY ) * arInv[2,3] +
              ( tmp.Z - dZ ) * arInv[3,3]
            ) / dM ;
  _ptg.M := tmp.M ;
end ;

procedure TGIS_CSTransformCoordinateFrameRotation.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoid.ToGeocentric_Ref( _ptg ) ;
  doForward( _ptg ) ;
  oEllipsoidWGS.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformCoordinateFrameRotation.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoidWGS.ToGeocentric_Ref( _ptg ) ;
  doReverse( _ptg ) ;
  oEllipsoid.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformCoordinateFrameRotation.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformCoordinateFrameRotation ) ;

  inherited ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformCoordinateFrameRotation.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformCoordinateFrameRotation.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformGeographicalAndHighOffsets'}

constructor TGIS_CSTransformGeographicalAndHighOffsets.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9618 ;

  dX := _b ;
  dY := _a ;
  dZ := _c ;
  assert( _d = 0 ) ;
  assert( _e = 0 ) ;
  assert( _f = 0 ) ;
  assert( _g = 0 ) ;
  assert( _h = 0 ) ;
  assert( _i = 0 ) ;
  assert( _j = 0 ) ;
end ;

procedure TGIS_CSTransformGeographicalAndHighOffsets.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X + dX ;
  _ptg.Y := _ptg.Y + dY ;
  _ptg.Z := _ptg.Z + dZ ;
end ;

procedure TGIS_CSTransformGeographicalAndHighOffsets.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X - dX ;
  _ptg.Y := _ptg.Y - dY ;
  _ptg.Z := _ptg.Z - dZ ;
end ;

procedure TGIS_CSTransformGeographicalAndHighOffsets.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  doForward( _ptg ) ;
end ;

procedure TGIS_CSTransformGeographicalAndHighOffsets.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  doReverse( _ptg ) ;
end ;

procedure TGIS_CSTransformGeographicalAndHighOffsets.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformGeographicalAndHighOffsets ) ;

  inherited ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformGeographicalAndHighOffsets.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformGeographicalAndHighOffsets.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformGeographicalOffsets'}

constructor TGIS_CSTransformGeographicalOffsets.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9619;

  dX := _b ;
  dY := _a ;
  assert( _c = 0 ) ;
  assert( _d = 0 ) ;
  assert( _e = 0 ) ;
  assert( _f = 0 ) ;
  assert( _g = 0 ) ;
  assert( _h = 0 ) ;
  assert( _i = 0 ) ;
  assert( _j = 0 ) ;
end ;

procedure TGIS_CSTransformGeographicalOffsets.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X + dX ;
  _ptg.Y := _ptg.Y + dY ;
end ;

procedure TGIS_CSTransformGeographicalOffsets.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  _ptg.X := _ptg.X - dX ;
  _ptg.Y := _ptg.Y - dY ;
end ;

procedure TGIS_CSTransformGeographicalOffsets.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  doForward( _ptg );
end ;

procedure TGIS_CSTransformGeographicalOffsets.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  doReverse( _ptg );
end ;

procedure TGIS_CSTransformGeographicalOffsets.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformGeographicalOffsets ) ;

  inherited ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformGeographicalOffsets.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformGeographicalOffsets.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformMolodenskiBadekas'}

constructor TGIS_CSTransformMolodenskiBadekas.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;

  FMethod := 9636 ;

  dX := _a ;
  dY := _b ;
  dZ := _c ;
  rX := _d ;
  rY := _e ;
  rZ := _f ;
  pX := _h ;
  pY := _i ;
  pZ := _j ;
  dM := 1 + _g ;

  {$IFDEF JAVA}
    InitializeMatrixArray(arFwd) ;
  {$ENDIF}
  arFwd[1,1] :=   1   ;
  arFwd[1,2] := - rZ  ;
  arFwd[1,3] :=   rY  ;
  arFwd[2,1] :=   rZ  ;
  arFwd[2,2] :=   1   ;
  arFwd[2,3] := - rX  ;
  arFwd[3,1] := - rY  ;
  arFwd[3,2] :=   rX  ;
  arFwd[3,3] :=   1   ;

  dMInv := 1 - _g ;

  {$IFDEF JAVA}
    InitializeMatrixArray(arInv) ;
  {$ENDIF}
  arInv[1,1] :=   1   ;
  arInv[1,2] :=   rZ  ;
  arInv[1,3] := - rY  ;
  arInv[2,1] := - rZ  ;
  arInv[2,2] :=   1   ;
  arInv[2,3] :=   rX  ;
  arInv[3,1] :=   rY  ;
  arInv[3,2] := - rX  ;
  arInv[3,3] :=   1   ;
end ;

procedure TGIS_CSTransformMolodenskiBadekas.doForward(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := dM * ( ( tmp.X - pX ) * arFwd[1,1] +
                   ( tmp.Y - pY ) * arFwd[2,1] +
                   ( tmp.Z - pZ ) * arFwd[3,1]
                 ) + pX  + dX ;
  _ptg.Y := dM * ( ( tmp.X - pX ) * arFwd[1,2] +
                   ( tmp.Y - pY ) * arFwd[2,2] +
                   ( tmp.Z - pZ ) * arFwd[3,2]
                 ) + pY + dY ;
  _ptg.Z := dM * ( ( tmp.X - pX ) * arFwd[1,3] +
                   ( tmp.Y - pY ) * arFwd[2,3] +
                   ( tmp.Z - pZ ) * arFwd[3,3]
                 ) + pZ + dZ ;
  _ptg.M := tmp.M ;

end ;

procedure TGIS_CSTransformMolodenskiBadekas.doReverse(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  tmp : TGIS_Point3D ;
begin
  {$IFDEF GIS_NORECORDS}
    tmp := new TGIS_Point3D( _ptg.X, _ptg.Y, _ptg.Z, _ptg.M ) ;
  {$ELSE}
    tmp := _ptg ;
  {$ENDIF}

  _ptg.X := dMInv * ( ( tmp.X - pX - dX ) * arInv[1,1] +
                      ( tmp.Y - pY - dY ) * arInv[2,1] +
                      ( tmp.Z - pZ - dZ ) * arInv[3,1]
                    ) + pX  ;
  _ptg.Y := dMInv * ( ( tmp.X - pX - dX ) * arInv[1,2] +
                      ( tmp.Y - pY - dY ) * arInv[2,2] +
                      ( tmp.Z - pZ - dZ ) * arInv[3,2]
                    ) + pY  ;
  _ptg.Z := dMInv * ( ( tmp.X - pX - dX ) * arInv[1,3] +
                      ( tmp.Y - pY - dY ) * arInv[2,3] +
                      ( tmp.Z - pZ - dZ ) * arInv[3,3]
                    ) + pZ ;
  _ptg.M := tmp.M ;

end ;

procedure TGIS_CSTransformMolodenskiBadekas.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformMolodenskiBadekas ) ;

  inherited ;
end ;

procedure TGIS_CSTransformMolodenskiBadekas.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoid.ToGeocentric_Ref( _ptg ) ;
  doForward( _ptg ) ;
  oEllipsoidWGS.ToGeodetic_Ref( _ptg ) ;
end ;

procedure TGIS_CSTransformMolodenskiBadekas.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
begin
  oEllipsoidWGS.ToGeocentric_Ref( _ptg ) ;
  doReverse( _ptg ) ;
  oEllipsoid.ToGeodetic_Ref( _ptg ) ;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformMolodenskiBadekas.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformMolodenskiBadekas.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'T_CSGridShift'}

constructor T_CSGridShift.Create(
  const _strm : TStream
) ;
var
  strm : TGIS_BufferedStream ;
begin
  Chain := nil ;

  Extent := GisExtent( 0, 0, 0, 0 );

  if not assigned (_strm ) then
    exit ;

  strm := TGIS_BufferedStream.Create( _strm );
  try
    doLoad( strm ) ;
  finally
    FreeObject( strm ) ;
  end;
end;

procedure T_CSGridShift.doTransform(
  var   _ptg       : TGIS_Point3D ;
  const _forward   : Boolean
) ;
var
  dcol,
  drow    : Double  ;
  icol,
  irow    : Integer ;
  icolnext,
  irownext: Integer ;
  fcol,
  frow    : Double  ;
  p00,
  p01,
  p10,
  p11     : TGIS_Point ;
  d1,
  d2      : Double ;
  xshift,
  yshift  : Double ;

begin
  dcol := ( _ptg.X - Extent.XMin ) / dXResolution ;
  icol := TruncS( dcol ) ;
  fcol := dcol - icol ;
  if icol < 0 then begin
    icol := 0 ;
    icolnext := irow ;
    fcol := 0 ;
  end
  else
  if icol < iCols then
    icolnext := icol + 1
  else begin
    icolnext := icol ;
    fcol := 0 ;
  end;

  drow := ( _ptg.Y - Extent.YMin ) / dYResolution ;
  irow := TruncS( drow ) ;
  frow := drow - irow ;
  if irow < 0 then begin
    irow := 0 ;
    irownext := irow ;
    frow := 0 ;
  end
  else
  if irow < iRows then
    irownext := irow + 1
  else begin
    irownext := irow ;
    frow := 0 ;
  end;

  p00 := arGrid[ ( irow     ) * iCols + icol     ] ;
  p01 := arGrid[ ( irow     ) * iCols + icolnext ] ;
  p10 := arGrid[ ( irownext ) * iCols + icol     ] ;
  p11 := arGrid[ ( irownext ) * iCols + icolnext ] ;

  d1 := ( 1 - fcol ) * p00.X + fcol * p01.X ;
  d2 := ( 1 - fcol ) * p10.X + fcol * p11.X ;

  xshift := ( 1 - frow ) * d1 + frow * d2 ;

  d1 := ( 1 - fcol ) * p00.Y + fcol * p01.Y ;
  d2 := ( 1 - fcol ) * p10.Y + fcol * p11.Y ;

  yshift := ( 1 - frow ) * d1 + frow * d2 ;

  if _forward then begin
    _ptg.X := _ptg.X + xshift ;
    _ptg.Y := _ptg.Y + yshift ;
  end
  else begin
    _ptg.X := _ptg.X - xshift ;
    _ptg.Y := _ptg.Y - yshift ;
  end;
end ;

procedure T_CSGridShift.To_Ref(
  var _ptg  : TGIS_Point3D
) ;
begin
  doTransform( _ptg, True ) ;
end;

procedure T_CSGridShift.From_Ref(
  var _ptg         : TGIS_Point3D
) ;
begin
  doTransform( _ptg, False ) ;
end;

class function T_CSGridShift.Factory(
  const _strm      : TStream
) : T_CSGridShift ;
begin
  Result := nil ;

  if T_CSGridShiftNTV2.Prerecognize( _strm ) then
    Result := T_CSGridShiftNTV2.Create( _strm )
  else
  if T_CSGridShiftTTKGS.Prerecognize( _strm ) then
    Result := T_CSGridShiftTTKGS.Create( _strm )
end;

class function T_CSGridShift.Factory(
  const _name      : String
) : T_CSGridShift ;
var
  strm : TGIS_BufferedFileStream ;

  function findFile(
    const _sname : String ;
    const _sext  : String
  ) :  TGIS_BufferedFileStream ;
  var
    sroot : String ;
    spath : String ;
  begin
    Result := nil ;

    {$IFDEF ISLAND}
      sroot := Environment.CurrentDirectory ;
    {$ELSE}
      sroot := TGIS_Utils.ExecutingFolder ;
    {$ENDIF}

    spath := sroot + _sname + _sext ;
    if FileExists( spath ) then begin
      Result := TGIS_BufferedFileStream.Create( spath, TGIS_StreamMode.Read );
      exit ;
    end;

    spath := sroot +
             GRIDSHIFT_FOLDER + GisEnvironmentInfo.DirSep + _sname + _sext ;
    if FileExists( spath ) then begin
      Result := TGIS_BufferedFileStream.Create( spath, TGIS_StreamMode.Read );
      exit ;
    end;

    sroot := GetPathDirSep( GisMetadata.Values[ METADATA_GRIDSHIFT_FOLDER ] ) ;
    if sroot <> '' then begin
      spath := sroot + GisEnvironmentInfo.DirSep + _sname + _sext ;
      if FileExists( spath ) then begin
        Result := TGIS_BufferedFileStream.Create( spath, TGIS_StreamMode.Read );
        exit ;
      end;
    end ;
  end;

begin
  try
    Result := nil;

    strm := nil ;
    try
      if not assigned( strm ) then
        strm := findFile( _name, '' ) ;

      if not assigned( strm ) then
        strm := findFile( _name, '.gsb' ) ;

      if not assigned( strm ) then
        strm := findFile( _name, '.ttkgs' ) ;

      if assigned( strm ) then
        Result := Factory( strm ) ;
    finally
      FreeObject( strm ) ;
    end;
  except
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), _name,  0 ) ;
  end;
end;

{$ENDREGION}
{$REGION 'T_GirdShiftNTV2'}

procedure T_CSGridShiftNTV2.doLoad(
  const _strm : TGIS_BufferedStream
) ;
var
  iNUM_OREC : Int64 ;
  iNUM_SREC : Int64 ;
  iNUM_FILE : Int64 ;
  sGS_TYPE  : String ;
  sVERSION  : String ;
  sSYSTEM_F : String ;
  sSYSTEM_T : String ;
  dMAJOR_F  : Double ;
  dMINOR_F  : Double ;
  dMAJOR_T  : Double ;
  dMINOR_T  : Double ;
  sSUB_NAME : String ;
  sPARENT   : String ;
  sCREATED  : String ;
  sUPDATED  : String ;
  dS_LAT    : Double ;
  dN_LAT    : Double ;
  dE_LONG   : Double ;
  dW_LONG   : Double ;
  dLAT_INC  : Double ;
  dLONG_INC : Double ;
  iGS_COUNT : Int64  ;

  i         : Integer ;
  dx, dy    : Single ;
  ix,iy     : Integer ;
  dtype     : Double ;

  part      : T_CSGridShift ;

  function read_str(
  ) : String ; overload ;
  var
    stmp : String ;
  begin
    _strm.ReadString( stmp, 8 );
    Result := Trim( stmp ) ;
  end;

  function read_str(
    const _check : String
  ) : String ; overload ;
  begin
    Result := read_str ;
    if Result <> _check  then
      Abort ;
  end;

  function read_str(
    const _check  : String ;
    const _check2 : String
  ) : String ; overload ;
  begin
    Result := read_str ;
    if ( Result <> _check ) and ( Result <> _check2 )  then
      Abort ;
  end;

  function read_end(
  ) : Boolean ;
  begin
    Result := Copy( read_str, 1,4 ) = 'END ' ;
  end;

  function read_i64(
  ) : Int64 ;
  begin
    _strm.ReadInt64( Result );
  end;

  function read_dbl(
  ) : Double ;
  begin
    _strm.ReadDouble( Result );
  end;

  function read_sng(
  ) : Single ;
  begin
    _strm.ReadSingle( Result );
  end;

begin
  try
    read_str( 'NUM_OREC' ) ;
    iNUM_OREC := read_i64 ;

    read_str( 'NUM_SREC' ) ;
    iNUM_SREC := read_i64 ;

    read_str( 'NUM_FILE' ) ;
    iNUM_FILE := read_i64 ;

    read_str( 'GS_TYPE' ) ;
    sGS_TYPE := read_str  ;

    assert( sGS_TYPE = 'SECONDS' ) ;

    if sGS_TYPE = 'SECONDS' then
      dtype := 60 * 60
    else
    if sGS_TYPE = 'MINUTES' then
      dtype := 60
    else
    if sGS_TYPE = 'DEGREES' then
      dtype := 1
    else
      Abort ;

    read_str( 'VERSION' ) ;
    sVERSION := read_str ;

    read_str( 'SYSTEM_F', 'DATUM_F' ) ;
    sSYSTEM_F := read_str ;

    read_str( 'SYSTEM_T', 'DATUM_T' ) ;
    sSYSTEM_T := read_str ;

    read_str( 'MAJOR_F' ) ;
    dMAJOR_F := read_dbl ;

    read_str( 'MINOR_F' ) ;
    dMINOR_F := read_dbl ;

    read_str( 'MAJOR_T' ) ;
    dMAJOR_T := read_dbl ;

    read_str( 'MINOR_T' ) ;
    dMINOR_T := read_dbl ;

    part := self ;

    read_str( 'SUB_NAME' ) ;

    while True do begin
      sSUB_NAME := read_str ;

      read_str( 'PARENT' ) ;
      sPARENT := read_str ;

      read_str( 'CREATED' ) ;
      sCREATED := read_str ;

      read_str( 'UPDATED' ) ;
      sUPDATED := read_str ;

      read_str( 'S_LAT' ) ;
      dS_LAT := read_dbl ;

      read_str( 'N_LAT' ) ;
      dN_LAT := read_dbl ;

      read_str( 'E_LONG' ) ;
      dE_LONG := read_dbl ;

      read_str( 'W_LONG' ) ;
      dW_LONG := read_dbl ;

      read_str( 'LAT_INC' ) ;
      dLAT_INC := read_dbl ;

      read_str( 'LONG_INC' ) ;
      dLONG_INC := read_dbl ;

      read_str( 'GS_COUNT' ) ;
      iGS_COUNT := read_i64 ;

      part.iCols := RoundS( ( dW_LONG - dE_LONG ) / dLONG_INC ) + 1 ;
      part.iRows := RoundS( ( dN_LAT  - dS_LAT  ) / dLAT_INC  ) + 1  ;

      assert( iGS_COUNT = part.iCols * part.iRows ) ;

      part.Extent.XMin := -dW_LONG / dtype * Pi / 180 ;
      part.Extent.XMax := -dE_LONG / dtype * Pi / 180 ;
      part.Extent.YMin :=  dS_LAT  / dtype * Pi / 180 ;
      part.Extent.YMax :=  dN_LAT  / dtype * Pi / 180 ;

      part.dXResolution := dLONG_INC / dtype * Pi / 180;
      part.dYResolution := dLAT_INC  / dtype * Pi / 180;

      SetLength( part.arGrid, iGS_COUNT ) ;

      i := 0  ;
      for iy := 1 to part.iRows do begin
        i := iy * part.iCols ;
        for ix := 1 to part.iCols do begin
          dec( i ) ;
          dy := read_sng ;
          dx := read_sng ;
            part.arGrid[ i ] :=  GisPoint( - dx / dtype * Pi / 180,
                                             dy / dtype * Pi / 180
                                         ) ;
          read_sng ; // skip - ignore error value
          read_sng ; // skip - ignore error value
        end;
      end;

      if read_str = 'END' then begin
        break ;
      end ;

      part.Chain := T_CSGridShiftNTV2.Create( nil ) ;
      part := part.Chain ;
    end ;
  except
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), 'gsb',  0 ) ;
  end;
end;

class function T_CSGridShiftNTV2.Prerecognize(
  const _strm      : TStream
) : Boolean ;
var
  pos  : Int64 ;
  stmp : String ;
  strm : TGIS_BufferedStream ;
begin
  pos := _strm.Position ;

  try
    strm := TGIS_BufferedStream.Create( _strm );
    try
      strm.ReadString( stmp, 8 );
      Result := Trim( stmp )= 'NUM_OREC'  ;
    finally
      FreeObject( strm )
    end;
  finally
    _strm.Position := pos ;
  end;
end;

{$ENDREGION}
{$REGION 'T_CSGridShiftTTKGS'}

procedure T_CSGridShiftTTKGS.doLoad(
  const _strm : TGIS_BufferedStream
) ;
var
  ival      : ShortInt ;

  i         : Integer  ;
  dx, dy    : Single   ;
  ix,iy     : Integer  ;

  part      : T_CSGridShift ;
begin
  try
    _strm.Position := 25 ;

    part := self ;

    while True do begin
      _strm.ReadDouble( part.Extent.XMin  ) ;
      _strm.ReadDouble( part.Extent.YMin  ) ;
      _strm.ReadDouble( part.Extent.XMax  ) ;
      _strm.ReadDouble( part.Extent.YMax  ) ;
      _strm.ReadDouble( part.dXResolution ) ;
      _strm.ReadDouble( part.dYResolution ) ;
      _strm.ReadInteger( part.iCols       ) ;
      _strm.ReadInteger( part.iRows       ) ;

      SetLength( part.arGrid, part.iCols * part.iRows ) ;


      for iy := 0 to part.iRows -1 do begin
        i := iy * part.iCols ;
        for ix := 0 to part.iCols - 1 do begin
          _strm.ReadSingle( dx );
          _strm.ReadSingle( dy );
          part.arGrid[ i ] :=  GisPoint( dx / (60*60) * Pi / 180,
                                         dy / (60*60) * Pi / 180
                                       ) ;
          inc( i ) ;
        end ;
      end;

     _strm.ReadShortInt( ival ) ;
     if ival = 0 then
       break ;

     if _strm.Eof then begin
       break ;
     end;

     part.Chain := T_CSGridShiftTTKGS.Create( nil ) ;
     part := part.Chain ;

    end;

  except
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), 'ttkgs',  0 ) ;
  end;
end;

class function T_CSGridShiftTTKGS.Prerecognize(
  const _strm      : TStream
) : Boolean ;
var
  pos  : Int64 ;
  stmp : String ;
  strm : TGIS_BufferedStream ;
begin
  pos := _strm.Position ;

  try
    strm := TGIS_BufferedStream.Create( _strm );
    try
      strm.Position := 4 ;
      strm.ReadString( stmp, 12 );
      Result := Trim( stmp )= 'ttkgridshift'  ;
    finally
      FreeObject( strm )
    end;
  finally
    _strm.Position := pos ;
  end;
end;

{$ENDREGION}
{$REGION 'TGIS_CSTransformGridShift'}

constructor TGIS_CSTransformGridShift.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _grid_files: String
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  ) ;

  FGridShift := _grid_files ;

  oGrids := TObjectList< TObject >.Create( True ) ;
  oCurrentGrid := nil ;
  oFallback    := nil ;
  criticalSection := TCriticalSection.Create ;
end ;

procedure TGIS_CSTransformGridShift.doDestroy ;
begin
  oCurrentGrid := nil ;
  FreeObject( criticalSection ) ;
  FreeObject( oGrids );
end;

function TGIS_CSTransformGridShift.fget_Fallback
  : Integer ;
begin
  Result := 0 ;

  if assigned( oFallback ) then
    Result := oFallback.EPSG ;
end;

procedure TGIS_CSTransformGridShift.fset_Fallback(
  const _value : Integer
) ;
begin
  oFallback := CSTransformList.ByEPSG( _value ) ;
end;

procedure TGIS_CSTransformGridShift.initializeGrid ;
var
  ogrid   : T_CSGridShift ;
  {$IFNDEF OXYGENE}
    sname : String ;
  {$ENDIF}
begin
  bInitialized := True ;

  for sname in FGridShift.Split( {$IFDEF OXYGENE}','{$ELSE}[',']{$ENDIF}) do begin
    ogrid := T_CSGridShift.Factory( sname ) ;

    if assigned( ogrid ) then begin
      oGrids.Add( ogrid ) ;
      while assigned( ogrid.Chain ) do begin
        ogrid := ogrid.Chain ;
        oGrids.Add( ogrid ) ;
      end ;
    end;
  end;
end;

procedure TGIS_CSTransformGridShift.findGrid(
  const _ptg : TGIS_Point3D
) ;
{$IFNDEF OXYGENE}
var
  grd : TObject ;
{$ENDIF}
begin
  if assigned( oCurrentGrid ) and
     GisIsPointInsideExtent(
       GisPoint2DFrom3D( _ptg ),
       T_CSGridShift( oCurrentGrid ).Extent
     )
  then
    exit ;

  oCurrentGrid := nil ;

  for grd in oGrids do begin
    if GisIsPointInsideExtent(
        GisPoint2DFrom3D( _ptg ),
        T_CSGridShift( grd ).Extent
       )
    then begin
      oCurrentGrid := grd ;
      break ;
    end;
  end;
end;

procedure TGIS_CSTransformGridShift.Assign(
  const _source : TObject
) ;
begin
  assert( _source is TGIS_CSTransformGridShift ) ;

  inherited ;
end ;

procedure TGIS_CSTransformGridShift.ToWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  ptg_tmp : TGIS_Point3D ;
begin
  criticalSection.Enter ;
  try
    if not bInitialized then
      initializeGrid ;

    findGrid( _ptg ) ;
    if assigned( oCurrentGrid ) then begin

      if assigned( oFallback ) then begin
        // fall back Z calculations
        ptg_tmp := _TGIS_Point3D( _ptg ) ;
        oFallback.ToWGS3D_Ref( ptg_tmp );
        _ptg.Z := ptg_tmp.Z ;
      end ;

      T_CSGridShift( oCurrentGrid ).To_Ref( _ptg );
    end
    else
    if assigned( oFallback ) then begin
      // fall back
      oFallback.ToWGS3D_Ref( _ptg );
    end
  finally
    criticalSection.Leave ;
  end;
end ;

procedure TGIS_CSTransformGridShift.FromWGS3D_Ref(
  {$IFNDEF JAVA} var {$ENDIF} _ptg : TGIS_Point3D
) ;
var
  ptg_tmp : TGIS_Point3D ;
begin
  criticalSection.Enter ;
  try
    if not bInitialized then
      initializeGrid ;

    findGrid( _ptg ) ;
    if assigned( oCurrentGrid ) then begin

      if assigned( oFallback ) then begin
        // fall back
        ptg_tmp := _TGIS_Point3D( _ptg ) ;
        oFallback.FromWGS3D_Ref( ptg_tmp );
        _ptg.Z := ptg_tmp.Z ;
      end ;

      T_CSGridShift( oCurrentGrid ).From_Ref( _ptg );
    end
    else
    if assigned( oFallback ) then begin
      // fall back
      oFallback.FromWGS3D_Ref( _ptg );
    end
  finally
    criticalSection.Leave ;
  end;
end ;

{$IFDEF UNIT_TEST}
  procedure TGIS_CSTransformGridShift.TestFwd(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doForward( _ptg ) ;
  end ;

  procedure TGIS_CSTransformGridShift.TestRev(
    var _ptg : TGIS_Point3D
  ) ;
  begin
    doReverse( _ptg ) ;
  end ;
{$ENDIF}

{$ENDREGION}
{$REGION 'TGIS_CSTransformNadcon'}

constructor TGIS_CSTransformNadcon.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _grid_files: String
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _grid_files
                  ) ;
  FMethod := 9613 ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSTransformNTV2'}

constructor TGIS_CSTransformNTV2.Create(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _grid_files: String
) ;
begin
  inherited Create( _epsg, _wkt, _ellipsoid, _number, _area,
                    _grid_files
                  ) ;
  FMethod := 9615 ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSUnitList'}

constructor TGIS_CSUnitsList.Create ;
begin
  inherited Create( cs_UnitsList, True, False ) ;
  if not assigned( cs_UnitsList ) then
    cs_UnitsList := self ;
end ;

function TGIS_CSUnitsList.fget_Units(
  _idx : Integer
) : TGIS_CSUnits ;
begin
  LockThread ;
  try
    Result := TGIS_CSUnits( WKTObject[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSUnitsList.Add(
  const _epsg   : Integer          ;
  const _wkt    : String           ;
  const _symbol : String           ;
  const _type   : TGIS_CSUnitsType ;
  const _factor : Double
) : TGIS_CSUnits ;
begin
  LockThread ;
  try
    Result := TGIS_CSUnits.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _symbol,
                _type,
                _factor
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSUnitsList.Fix(
  const _epsg   : Integer          ;
  const _wkt    : String           ;
  const _symbol : String           ;
  const _type   : TGIS_CSUnitsType ;
  const _factor : Double
) : TGIS_CSUnits ;
var
  o : TGIS_CSUnits ;
begin
  LockThread ;
  try
    o := TGIS_CSUnits.Create(
           _epsg,
           uniqueWkt( _epsg,_wkt ),
           _symbol,
           _type,
           _factor
         ) ;
    try
      Result := TGIS_CSUnits( inherited Fix( o ) ) ;
    finally
      FreeObject( o ) ;
    end;
  finally
    UnlockThread ;
  end ;
end ;

function  TGIS_CSUnitsList.Prepare(
  const _epsg   : Integer          ;
  const _wkt    : String           ;
  const _symbol : String           ;
  const _type   : TGIS_CSUnitsType ;
  const _factor : Double
) : TGIS_CSUnits ;
var
  i      : Integer ;
  wkt    : String  ;
  lookup : Boolean ;

  function check_match(
    const _obj : TGIS_CSUnits
  ) : Boolean ;
  begin
    Result := ( _obj.UnitsType               = _type  ) and
              ( Abs( _obj.Factor - _factor ) <  1E-10 )
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSUnits ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSUnits( WKTObject[ _i ] ) ;
  end ;

  function add_item : TGIS_CSUnits ;
  begin
    if IsStringEmpty( wkt ) then
      wkt := 'Custom_Unit' ;
    Result := Add( -1, wkt, _symbol, _type, _factor ) ;
  end ;

begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( Trim( _wkt ) ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      if not check_match( Result ) then
        lookup := True
      else
        lookup := False ;
    end ;

    if lookup then begin
      // find best matching object by parameters
      for i:=1 to Count -1 do begin
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

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSUnitsList.AddAuto(
   const _epsg     : Integer          ;
   const _wkt      : String           ;
   const _subunits : array of Integer
) : TGIS_CSUnits ;
begin
  LockThread ;
  try
    Result := TGIS_CSUnits.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _subunits
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSUnitsList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSUnits ;
begin
  LockThread ;
  try
    Result := TGIS_CSUnits( inherited ByEPSG( _epsg ) )
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSUnitsList.ByWKT(
  const _wkt : String
) : TGIS_CSUnits ;
begin
  LockThread ;
  try
    Result := TGIS_CSUnits( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;


procedure TGIS_CSUnitsList.Init ;

{$include CsData/GisCsUnits.inc}
{$include CsData/GisCsUnitsEx.inc}

begin
  LockThread ;
  try
    Clear ;
    Init_CSUnitsList( self ) ;
    Init_CSUnitsListEx( self ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSAbstractListEnumerator'}

constructor TGIS_CSPrimeMeridianList.Create ;
begin
  inherited Create( cs_PrimeMeridianList, True, True ) ;
  if not assigned( cs_PrimeMeridianList ) then
    cs_PrimeMeridianList := self ;
end ;

function TGIS_CSPrimeMeridianList.fget_PrimeMeridian(
  _idx : Integer
) : TGIS_CSPrimeMeridian ;
begin
  LockThread ;
  try
    Result := TGIS_CSPrimeMeridian( WKTObject[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSPrimeMeridianList.Add(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _longitude : Double
) : TGIS_CSPrimeMeridian ;
begin
  LockThread ;
  try
    Result := TGIS_CSPrimeMeridian.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _longitude
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSPrimeMeridianList.Fix(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _longitude : Double
) : TGIS_CSPrimeMeridian ;
var
  o : TGIS_CSPrimeMeridian ;
begin
  LockThread ;
  try
    o := TGIS_CSPrimeMeridian.Create(
           _epsg,
           uniqueWkt( _epsg,_wkt ),
           _longitude
         ) ;
    try
      Result := TGIS_CSPrimeMeridian( inherited Fix( o ) ) ;
    finally
      FreeObject( o ) ;
    end;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSPrimeMeridianList.Prepare(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _longitude : Double
) : TGIS_CSPrimeMeridian ;
var
  i      : Integer ;
  wkt    : String  ;
  lookup : Boolean ;

  function check_match(
    const _obj : TGIS_CSPrimeMeridian
  ) : Boolean ;
  begin
    Result := ( Abs( _obj.Longitude - _longitude ) <= 1E-10  ) ;
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSPrimeMeridian ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSPrimeMeridian( WKTObject[ _i ] ) ;
  end ;

  function add_item : TGIS_CSPrimeMeridian ;
  begin
    if IsStringEmpty( wkt ) then
      wkt := 'Custom_Meridian' ;
    Result := Add( -1, wkt, _longitude ) ;
  end ;

begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( _wkt ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      if not check_match( Result ) then
        lookup := True
      else
        lookup := False ;
    end ;

    if lookup then begin
      // find best matching object by parameters
      for i:=1 to Count -1 do begin
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

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSPrimeMeridianList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSPrimeMeridian ;
begin
  LockThread ;
  try
    Result := TGIS_CSPrimeMeridian( inherited ByEPSG( _epsg ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSPrimeMeridianList.ByWKT(
  const _wkt : String
) : TGIS_CSPrimeMeridian ;
begin
  LockThread ;
  try
    Result := TGIS_CSPrimeMeridian( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSPrimeMeridianList.Init ;
  {$include CsData/GisCsPrimeMeridians.inc}
begin
  LockThread ;
  try
    Clear ;
    Init_CSPrimeMeridianList( self ) ;
  finally
    UnlockThread ;
  end ;
end ;


{$ENDREGION}
{$REGION 'TGIS_CSEllipsoidList'}

constructor TGIS_CSEllipsoidList.Create ;
begin
  inherited Create( cs_EllipsoidList, True, True ) ;
  if not assigned( cs_EllipsoidList ) then
    cs_EllipsoidList := self ;
end ;

function TGIS_CSEllipsoidList.fget_Ellipsoid(
  _idx : Integer
) : TGIS_CSEllipsoid ;
begin
  LockThread ;
  try
    Result := TGIS_CSEllipsoid( WKTObject[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSEllipsoidList.Add(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _semi_major     : Double  ;
  const _inv_flattering : Double
) : TGIS_CSEllipsoid;
begin
  LockThread ;
  try
    Result := TGIS_CSEllipsoid.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _semi_major,
                _inv_flattering
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSEllipsoidList.Fix(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _semi_major     : Double  ;
  const _inv_flattering : Double
) : TGIS_CSEllipsoid ;
var
  o : TGIS_CSEllipsoid ;
begin
  LockThread ;
  try
    o := TGIS_CSEllipsoid.Create(
           _epsg,
           uniqueWkt( _epsg,_wkt ),
           _semi_major,
           _inv_flattering
         ) ;
    try
      Result := TGIS_CSEllipsoid( inherited Fix( o ) ) ;
    finally
      FreeObject( o ) ;
    end;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSEllipsoidList.Prepare (
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _semi_major     : Double  ;
  const _inv_flattering : Double
) : TGIS_CSEllipsoid ;
var
  i      : Integer ;
  wkt    : String  ;
  lookup : Boolean ;

  function check_match(
    const _obj : TGIS_CSEllipsoid
  ) : Boolean ;
  begin
    if _inv_flattering = 1 then begin
      Result := ( Abs( _obj.SemiMajor         - _semi_major     ) <=  1E-3 )
                and
                ( Abs( _obj.InverseFlattering - 0               ) <=  1E-6 ) ;
    end
    else begin
      Result := ( Abs( _obj.SemiMajor         - _semi_major     ) <=  1E-3 )
                and
                ( Abs( _obj.InverseFlattering - _inv_flattering ) <=  1E-6 ) ;
    end ;
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSEllipsoid ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSEllipsoid( WKTObject[ _i ] ) ;
  end ;

  function add_item : TGIS_CSEllipsoid ;
  begin
    if IsStringEmpty( wkt ) then
      wkt := 'Custom_Spheroid' ;
    Result := Add( -1, wkt, _semi_major, _inv_flattering ) ;
  end ;

begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( _wkt ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      if not check_match( Result ) then
        lookup := True
      else
        lookup := False ;
    end ;

    if lookup then begin
      // find best matching object by parameters
      for i:=1 to Count -1 do begin
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

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSEllipsoidList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSEllipsoid ;
begin
  LockThread ;
  try
    Result := TGIS_CSEllipsoid( inherited ByEPSG( _epsg ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSEllipsoidList.ByWKT(
  const _wkt : String
) : TGIS_CSEllipsoid ;
begin
  LockThread ;
  try
    Result := TGIS_CSEllipsoid( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSEllipsoidList.Init ;
  {$include CsData/GisCsEllipsoids.inc}
  {$include CsData/GisCsEllipsoidsEx.inc}
begin
  LockThread ;
  try
    Clear ;
    Init_CSEllipsoidList( self ) ;
    Init_CSEllipsoidListEx( self ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSAreaList'}

constructor TGIS_CSAreaList.Create ;
begin
  inherited Create( cs_AreaList, True, False ) ;
  if not assigned( cs_AreaList ) then
    cs_AreaList := self ;
end ;

procedure TGIS_CSAreaList.doDestroy ;
begin
  inherited ;
end ;

function TGIS_CSAreaList.Add(
  const _epsg : Integer ;
  const _name : String
) : TGIS_CSArea ;
begin
  LockThread ;
  try
    Result := TGIS_CSArea.Create( _epsg, _name ) ;
    Result.FBounds := GisExtent( -180, -90, 190, 90 ) ;
    Result.FDescription := _name ;

    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAreaList.Add(
  const _epsg : Integer ;
  const _name : String  ;
  const _xmin : Double  ;
  const _ymin : Double  ;
  const _xmax : Double  ;
  const _ymax : Double
) : TGIS_CSArea ;
begin
  LockThread ;
  try
    Result := TGIS_CSArea.Create( _epsg, _name ) ;

    Result.FBounds := GisExtent( _xmin, _ymin, _xmax, _ymax ) ;
    Result.FDescription := _name ;

    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;


function TGIS_CSAreaList.Fix(
  const _epsg : Integer ;
  const _name : String
) : TGIS_CSArea ;
var
  o : TGIS_CSArea ;
begin
  LockThread ;
  try
    o := TGIS_CSArea.Create( _epsg, '' ) ;
    try
      o.FDescription := _name ;
      Result := TGIS_CSArea( inherited Fix( o ) ) ;
    finally
      FreeObject( o )
    end;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAreaList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSArea ;
begin
  LockThread ;
  try
    Result := TGIS_CSArea( inherited ByEPSG( _epsg ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSAreaList.ByWKT(
  const _wkt : String
) : TGIS_CSArea ;
begin
  LockThread ;
  try
    Result := TGIS_CSArea( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSAreaList.Init ;
  {$include CsData/GisCsAreas.inc}
  {$include CsData/GisCsAreasEx.inc}
begin
  LockThread ;
  try
    Clear ;
    Init_CSAreaList( self ) ;
    Init_CSAreaListEx( self ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSDatumList'}

constructor TGIS_CSDatumList.Create ;
begin
  inherited Create( cs_DatumList, True, True ) ;
  if not assigned( cs_DatumList ) then
    cs_DatumList := self ;
end ;

function TGIS_CSDatumList.fget_Datum(
  _idx : Integer
) : TGIS_CSDatum ;
begin
  LockThread ;
  try
    Result := TGIS_CSDatum( WKTObject[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.Add(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer
) : TGIS_CSDatum ;
begin
  LockThread ;
  try
    Result := nil ;
    Result := TGIS_CSDatum.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _ellipsoid,
                _transform
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.Add(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer ;
  const _fallback       : Integer
) : TGIS_CSDatum ;
begin
  LockThread ;
  try
    Result := nil ;
    Result := TGIS_CSDatum.Create(
                _epsg,
                uniqueWkt( _epsg,_wkt ),
                _ellipsoid,
                _transform,
                _fallback
              ) ;
    inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.Fix(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer
) : TGIS_CSDatum ;
begin
  Result := Fix( _epsg, _wkt, _ellipsoid, _transform, 0 ) ;
end ;

function TGIS_CSDatumList.Fix(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer ;
  const _fallback       : Integer
) : TGIS_CSDatum ;
var
  o : TGIS_CSDatum ;
begin
  LockThread ;
  try
    o := TGIS_CSDatum.Create(
           _epsg,
           uniqueWkt( _epsg,_wkt ),
           _ellipsoid,
           _transform,
           _fallback
         ) ;
    try
      Result := TGIS_CSDatum( inherited Fix( o ) ) ;
    finally
      FreeObject( o ) ;
    end;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.Prepare(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _transform      : Integer
) : TGIS_CSDatum ;
var
  i      : Integer ;
  wkt    : String  ;
  lookup : Boolean ;

  function check_match(
    const _obj : TGIS_CSDatum
  ) : Boolean ;
  begin
    Result := assigned( _obj.Ellipsoid                 )  and
              ( _obj.Ellipsoid.MasterEPSG = 0          )  and
              ( _obj.Ellipsoid.EPSG       = _ellipsoid )  ;

    if _transform >= 0 then begin
      if assigned( _obj.FTransform ) then
        Result := Result
                  and
                  ( _obj.FTransform.EPSG   = _transform )
                  and
                  ( _obj.FTransform.Number = 1          )

      else
        Result := Result and ( _transform = 0 )
    end ;
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSDatum ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSDatum( WKTObject[ _i ] ) ;
  end ;

  function add_item : TGIS_CSDatum ;
  begin
    if IsStringEmpty( wkt ) then
      wkt := 'Custom_Datum' ;
    Result := Add( -1, wkt, _ellipsoid, _transform ) ;
  end ;

begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( _wkt ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;

      if not assigned( Result ) then begin
        // ESRI like names
        if Pos( 'D_', UpperCase( wkt ) ) = StringFirst then
          Result := ByWKT( Copy( wkt, StringFirst+2, 8192 ) )
        else
          Result := ByWKT( 'D_' + wkt ) ;
      end ;
    end ;

    if not assigned( Result ) then begin
      if _transform <= 0 then begin
        if _ellipsoid = 7030 then begin
          // find WGS84
          Result := ByEPSG( 6326 ) ;
        end
        else begin
          // find Unspecified_datum_based_on_ ...'
          Result := ByEPSG( _ellipsoid- 1000 ) ;
        end ;
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

      // try to find by similar name first
      for i:=1 to Count -1 do begin
        Result := get_item( i ) ;

        if not assigned( Result ) then
          continue ;
        if ( Pos( wkt, Result.WKT ) <> StringFirst ) and
           ( Pos( Result.WKT, wkt ) <> StringFirst ) then
        begin
          Result := nil ;
          continue ;
        end ;

        if check_match( Result ) then
          break ;

        Result := nil ;
      end ;

      // then find best matching object by parameters
      if not assigned( Result ) then begin
        if _transform >= 0 then begin
          for i:=1 to Count -1 do begin
            Result := get_item( i ) ;

            if not assigned( Result ) then
              continue ;

            if check_match( Result ) then
              break ;

            Result := nil ;
          end ;
        end
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

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.PrepareEx(
  const _epsg           : Integer ;
  const _wkt            : String  ;
  const _ellipsoid      : Integer ;
  const _method         : Integer ;
  const _area           : Integer ;
  const _a              : Double  ;
  const _b              : Double  ;
  const _c              : Double  ;
  const _d              : Double  ;
  const _e              : Double  ;
  const _f              : Double  ;
  const _g              : Double  ;
  const _h              : Double  ;
  const _i              : Double  ;
  const _j              : Double
) : TGIS_CSDatum ;
var
  i       : Integer ;
  wkt     : String  ;
  lookup  : Boolean ;
  trnsfrm : TGIS_CSTransformAbstract ;
  fallback: TGIS_CSTransformAbstract ;

  function check_match(
    const _obj : TGIS_CSTransformAbstract
  ) : Boolean ;
  begin
    Result := False ;

    if not assigned( _obj ) then begin
      Result := ( Abs(    0       -   _a  ) <  1E-4      ) and
                ( Abs(    0       -   _b  ) <  1E-4      ) and
                ( Abs(    0       -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if _obj.FMethod = _method then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -   _d  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -   _e  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -   _f  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9603 ) and ( _method = 9606 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9606 ) and ( _method = 9603 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -    0  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9603 ) and ( _method = 9607 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9607 ) and ( _method = 9603 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -    0  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9607 ) and ( _method = 9606 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  - (-_d) ) <  1E-10     ) and
                ( Abs(   _obj.FE  - (-_e) ) <  1E-10     ) and
                ( Abs(   _obj.FF  - (-_f) ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9606 ) and ( _method = 9607 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs( (-_obj.FD) -   _d  ) <  1E-10     ) and
                ( Abs( (-_obj.FE) -   _e  ) <  1E-10     ) and
                ( Abs( (-_obj.FF) -   _f  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9615 ) and ( _method = 9615 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs( (-_obj.FD) -   _d  ) <  1E-10     ) and
                ( Abs( (-_obj.FE) -   _e  ) <  1E-10     ) and
                ( Abs( (-_obj.FF) -   _f  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end ;
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSDatum ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSDatum( WKTObject[ _i ] ) ;
  end ;

  function get_item_by_trnsfrm(
    const _epsg : Integer
  ) : TGIS_CSDatum ;
  var
    i1  : Integer ;
    ocs : TGIS_CSAbstract ;
  begin
    Result := nil ;
    for i1 := 0 to Count - 1 do begin
      ocs := WKTObject[ i1 ] ;
      if ocs.MasterEPSG <> 0 then continue ;

      Result := TGIS_CSDatum( WKTObject[ i1 ] ) ;
      if assigned( Result.FTransform ) and
        ( Result.FTransform.EPSG = _epsg ) then
        break
      else
        Result := nil ;
    end ;
  end ;
begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( _wkt ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;

      if not assigned( Result ) then begin
        // ESRI like names
        if Pos( 'D_', UpperCase( wkt ) ) = StringFirst then
          Result := ByWKT( Copy( wkt, StringFirst+2, 8192 ) )
        else
          Result := ByWKT( 'D_' + wkt ) ;
      end ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      if Result.Fallback > 0 then begin
        if not check_match( CSTransformList.ByEPSG( Result.Fallback  ) ) then
          Result := nil ;
      end
      else
      if not check_match( Result.FTransform ) then
        Result := nil ;
    end;

    if not assigned( Result ) then begin
      if _ellipsoid = 7030 then begin
        // find WGS84
        Result := ByEPSG( 6326 ) ;
      end
      else begin
        // find Unspecified_datum_based_on_ ...'
        Result := ByEPSG( _ellipsoid- 1000 ) ;
      end ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      lookup := not check_match( Result.FTransform ) ;
      if lookup then begin
        fallback := CSTransformList.ByEPSG( Result.FFallback ) ;
        lookup := not check_match( fallback ) ;
      end;
    end ;

    if lookup then begin
      for i:=1 to Count -1 do begin
        Result := get_item( i ) ;
        if not assigned( Result ) then
          continue ;

        if assigned( Result.FTransform ) then
          if check_match( Result.FTransform ) then
            break ;
        Result := nil ;
      end ;

      if not assigned( Result ) then begin
        // can we identify by EPSG?
        Result := ByEPSG( _epsg ) ;
      end ;

      if not assigned( Result ) then begin
        // find matching transformation
        trnsfrm := nil ;
        for i := 0 to CSTransformList.Count -1 do begin
          trnsfrm := CSTransformList[ i ] ;
          if check_match( trnsfrm ) then begin
            Result := get_item_by_trnsfrm( trnsfrm.EPSG ) ;
            break ;
          end
          else
            trnsfrm := nil ;
        end ;

        if not assigned( Result ) then begin
          if IsStringEmpty( wkt ) then begin
            wkt := 'Custom_Datum' ;
          end ;

          if not assigned( trnsfrm ) then begin
            // add new transformation

            trnsfrm := CSTransformList.Prepare(
                         -1, _wkt,
                         _method, _ellipsoid, 1, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
          end ;

          if assigned( trnsfrm ) then
            Result := Prepare(
                        -1, wkt,
                        _ellipsoid, trnsfrm.EPSG
                      )
          else
            Result := Prepare(
                        -1, wkt,
                        _ellipsoid, 0
                      ) ;
        end ;

      end ;

    end ;

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

    assert( assigned( Result ) ) ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSDatum ;
begin
  LockThread ;
  try
    Result := TGIS_CSDatum( inherited ByEPSG( _epsg ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSDatumList.ByWKT(
  const _wkt : String
) : TGIS_CSDatum ;
begin
  LockThread ;
  try
    Result := TGIS_CSDatum( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSDatumList.Init ;
  {$include CsData/GisCsDatums.inc}
  {$include CsData/GisCsDatumsEx.inc}
begin
  LockThread ;
  try
    Clear ;
    Init_CSDatumList( self ) ;
    Init_CSDatumListEx( self ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$ENDREGION}
{$REGION 'TGIS_CSAbstractListEnumerator'}

constructor TGIS_CSTransformList.Create ;
begin
  inherited Create( cs_TransformList, True, True ) ;
  if not assigned( cs_TransformList ) then
    cs_TransformList := self ;
end ;

function TGIS_CSTransformList.fget_DatumTransform(
  _idx : Integer
) : TGIS_CSTransformAbstract ;
begin
  LockThread ;
  try
    Result := TGIS_CSTransformAbstract( WKTObject[ _idx ] ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSTransformList.Add(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _method    : Integer ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) : TGIS_CSTransformAbstract ;
var
  wkt : String ;
begin
  LockThread ;
  try
    wkt := uniqueWkt( _epsg,_wkt ) ;
    Result := nil ;

    case _method of
      9603 : Result := TGIS_CSTransformGeocentricTranslations.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      9606 : Result := TGIS_CSTransformPositionVector7Params.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      9607 : Result := TGIS_CSTransformCoordinateFrameRotation.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      9618 : Result := TGIS_CSTransformGeographicalAndHighOffsets.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      9619 : Result := TGIS_CSTransformGeographicalOffsets.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      9636 : Result := TGIS_CSTransformMolodenskiBadekas.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                       ) ;
      else   begin
                assert( False,
                  Format( 'Transformation EPSG=%d does not exist.',
                          [ _method ]
                      )
                ) ;

             end ;
    end ;

    if assigned( Result ) then
      inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSTransformList.Add(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _method    : Integer ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _grid_files: String
) : TGIS_CSTransformAbstract ;
var
  wkt : String ;
begin
  LockThread ;
  try
    wkt := uniqueWkt( _epsg,_wkt ) ;
    Result := nil ;

    case _method of
      9613 : Result := TGIS_CSTransformNadcon.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _grid_files
                       ) ;
      9615 : Result := TGIS_CSTransformNTV2.Create(
                         _epsg, wkt, _ellipsoid, _number, _area,
                         _grid_files
                       ) ;
      else   begin
                assert( False,
                  Format( 'Transformation EPSG=%d does not exist.',
                          [ _method ]
                      )
                ) ;
             end ;
    end ;

    if assigned( Result ) then
      inherited Add( Result ) ;
  finally
    UnlockThread ;
  end ;
end ;


function TGIS_CSTransformList.Fix(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _method    : Integer ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) : TGIS_CSTransformAbstract ;
var
  wkt : String ;
  o   : TGIS_CSTransformAbstract ;
begin
  Result := nil ;

  LockThread ;
  try
    wkt := uniqueWkt( _epsg,_wkt ) ;

    case _method of
      9603 : o := TGIS_CSTransformGeocentricTranslations.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      9606 : o := TGIS_CSTransformPositionVector7Params.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      9607 : o := TGIS_CSTransformCoordinateFrameRotation.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      9618 : o := TGIS_CSTransformGeographicalAndHighOffsets.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      9619 : o := TGIS_CSTransformGeographicalOffsets.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      9636 : o := TGIS_CSTransformMolodenskiBadekas.Create(
                    _epsg, wkt, _ellipsoid, _number, _area,
                    _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
                  ) ;
      else   begin
                assert( False,
                  Format( 'Transformation EPSG=%d does not exist.',
                          [ _method ]
                      )
                ) ;

             end ;
    end ;

    try
      if assigned( o ) then
        Result := TGIS_CSTransformAbstract( inherited Fix( o ) ) ;
    finally
      FreeObject( o ) ;
    end;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSTransformList.Prepare(
  const _epsg      : Integer ;
  const _wkt       : String  ;
  const _method    : Integer ;
  const _ellipsoid : Integer ;
  const _number    : Integer ;
  const _area      : Integer ;
  const _a         : Double  ;
  const _b         : Double  ;
  const _c         : Double  ;
  const _d         : Double  ;
  const _e         : Double  ;
  const _f         : Double  ;
  const _g         : Double  ;
  const _h         : Double  ;
  const _i         : Double  ;
  const _j         : Double
) : TGIS_CSTransformAbstract ;
var
  i      : Integer ;
  wkt    : String  ;
  lookup : Boolean ;

  function check_match(
    const _obj : TGIS_CSTransformAbstract
  ) : Boolean ;
  begin
    Result := False ;

    if not assigned( _obj ) then begin
      Result := ( Abs(    0       -   _a  ) <  1E-4      ) and
                ( Abs(    0       -   _b  ) <  1E-4      ) and
                ( Abs(    0       -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if _obj.FMethod = _method then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -   _d  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -   _e  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -   _f  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9603 ) and ( _method = 9606 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9606 ) and ( _method = 9603 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -    0  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9603 ) and ( _method = 9607 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(    0       -   _d  ) <  1E-10     ) and
                ( Abs(    0       -   _e  ) <  1E-10     ) and
                ( Abs(    0       -   _f  ) <  1E-10     ) and
                ( Abs(    0       -   _g  ) <  1E-10     ) and
                ( Abs(    0       -   _h  ) <  1E-10     ) and
                ( Abs(    0       -   _i  ) <  1E-10     ) and
                ( Abs(    0       -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod = 9607 ) and ( _method = 9603 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FE  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FF  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -    0  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -    0  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9607 ) and ( _method = 9606 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs(   _obj.FD  - (-_d) ) <  1E-10     ) and
                ( Abs(   _obj.FE  - (-_e) ) <  1E-10     ) and
                ( Abs(   _obj.FF  - (-_f) ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end
    else if ( _obj.FMethod =9606 ) and ( _method = 9607 ) then begin
      Result := ( _obj.FEllipsoid           = _ellipsoid ) and
                ( _obj.FArea                = _area      ) and
                ( Abs(   _obj.FA  -   _a  ) <  1E-4      ) and
                ( Abs(   _obj.FB  -   _b  ) <  1E-4      ) and
                ( Abs(   _obj.FC  -   _c  ) <  1E-4      ) and
                ( Abs( (-_obj.FD) -   _d  ) <  1E-10     ) and
                ( Abs( (-_obj.FE) -   _e  ) <  1E-10     ) and
                ( Abs( (-_obj.FF) -   _f  ) <  1E-10     ) and
                ( Abs(   _obj.FG  -   _g  ) <  1E-10     ) and
                ( Abs(   _obj.FH  -   _h  ) <  1E-10     ) and
                ( Abs(   _obj.FI  -   _i  ) <  1E-10     ) and
                ( Abs(   _obj.FJ  -   _j  ) <  1E-10     ) ;
    end ;
  end ;

  function get_item(
    const _i : Integer
  ) : TGIS_CSTransformAbstract ;
  begin
    if WKTObject[ _i ].MasterEPSG > 0 then
      Result := nil
    else
      Result := TGIS_CSTransformAbstract( WKTObject[ _i ] ) ;
  end ;

  function add_item : TGIS_CSTransformAbstract ;
  begin
    if IsStringEmpty( wkt ) then
      wkt := 'Custom_Transformation' ;

    if Pos( UpperCase( '_to_WGS_84' ), UpperCase( wkt ) ) < StringFirst
    then begin
      wkt := wkt + '_to_WGS_84' ;
    end ;

    Result := Add(
                -1, wkt, _method, _ellipsoid, _number, _area,
                _a, _b, _c, _d, _e, _f, _g, _h, _i, _j
             ) ;

    if Pos( UpperCase( '_to_WGS_84' ), UpperCase( Result.WKT ) ) < StringFirst
    then begin
      Result.FWKT := Result.WKT + '_to_WGS_84' ;
    end ;

  end ;

begin
  LockThread ;
  try
    lookup := True  ;

    wkt := CanonicalWKT( _wkt ) ;

    Result := nil;
    if _epsg > 0 then
      Result := ByEPSG( _epsg ) ;

    if not IsStringEmpty( _wkt ) then begin
      if not assigned( Result ) then
        Result := ByWKT( wkt ) ;
    end ;

    if assigned( Result ) then begin
      // check parameters
      if not check_match( Result ) then
        lookup := True
      else
        lookup := False ;
    end ;

    if lookup then begin
      // find best matching object by parameters
      for i:=1 to Count -1 do begin
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
    end
    else begin
      assert( assigned( Result ) ) ;
      Result := TGIS_CSTransformAbstract( inherited Fix( Result ) ) ;
    end ;

    if assigned( Result ) then begin
      // resolve aliases
      if Result.MasterEPSG <> 0 then
        Result := ByEPSG( Result.MasterEPSG ) ;
      assert( assigned( Result ) ) ;
    end ;

    assert( assigned( Result ) ) ;

  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSTransformList.ByEPSG(
  const _epsg : Integer
) : TGIS_CSTransformAbstract ;
begin
  LockThread ;
  try
    Result := TGIS_CSTransformAbstract( inherited ByEPSG( _epsg ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

function TGIS_CSTransformList.ByWKT(
  const _wkt : String
) : TGIS_CSTransformAbstract ;
begin
  LockThread ;
  try
    Result := TGIS_CSTransformAbstract( inherited ByWKT( _wkt ) ) ;
  finally
    UnlockThread ;
  end ;
end ;

procedure TGIS_CSTransformList.Init ;
  {$include CsData/GisCsTransforms.inc}
  {$include CsData/GisCsTransformsEx.inc}
begin
  LockThread ;
  try
    Clear ;
    Init_CSTranformList( self ) ;
    Init_CSTranformListEx( self ) ;
  finally
    UnlockThread ;
  end ;
end ;

{$IFNDEF OXYGENE}
  {$IFDEF DEBUG}
    procedure check_structure_cs_base ;

      procedure check_units ;
      var
        i, j  : Integer ;
        unt   : TGIS_CSUnits ;
        unt2  : TGIS_CSUnits ;
        v     : Double       ;
        vprev : Double       ;
      begin
        CSUnitsList.ByEPSG( 0 ) ; // force sorting
        for i:=0 to CSUnitsList.Count -1 do begin
          unt := CSUnitsList[ i ] ;
          if unt.MasterEPSG > 0 then continue ;

          vprev := 0 ;
          if unt.UnitsType = TGIS_CSUnitsType.Auto then begin
            assert( high( unt.arUnits ) > 0 ,
                      Format( 'Unit=%d subunits not defined',
                              [ unt.EPSG ]
                            )
                    ) ;

            for j:= 0 to high( unt.arUnits ) do begin
              unt2 := CSUnitsList.ByEPSG( unt.arUnits[j] ) ;
              assert( assigned( unt2 ),
                      Format( 'Unit=%d Subunit %d must be defined',
                              [ unt.EPSG, unt.arUnits[j] ]
                            )
                    ) ;
              assert( ( unt2.UnitsType = TGIS_CSUnitsType.Linear ) or
                      ( unt2.UnitsType = TGIS_CSUnitsType.Areal ) ,
                      Format( 'Unit=%d Subunit %d of unexpected type',
                              [ unt.EPSG, unt.arUnits[j] ]
                            )
              ) ;
              if unt2.UnitsType = TGIS_CSUnitsType.Linear then
                v := Sqr( unt2.Factor )
              else
                v := unt2.Factor ;

              if j > 0 then begin
                assert( v > vprev,
                        Format( 'Unit=%d Subunit %d in unexpected order',
                                [ unt.EPSG, unt.arUnits[j] ]
                              )
                      ) ;
              end ;
              vprev := v ;
            end ;

          end ;
        end ;
      end ;

      procedure check_datums ;
      var
        i : Integer ;
      begin
        for i:=0 to CSDatumList.Count -1 do begin
          if CSDatumList[i].MasterEPSG > 0 then continue ;
          assert( assigned( CSDatumList[i].Ellipsoid ),
                  Format( 'Datum EPSG=%d "%s" referring to non ' +
                          'existing Ellipsoid.',
                          [ CSDatumList[i].EPSG, CSDatumList[i].WKT ]
                        )
                ) ;
        end ;
        for i:=0 to CSDatumList.Count -1 do begin
          if not assigned( CSDatumList[i].FTransform ) then continue ;
          assert( assigned( CSDatumList[i].FTransform ),
                  Format( 'Datum EPSG=%d referring to non ' +
                          'existing transformation EPSG=%d.',
                          [ CSDatumList[i].EPSG, CSDatumList[i].FTransform.EPSG ]
                        )
                ) ;
        end ;
      end ;

      procedure check_transforms ;
      var
        i : Integer ;
      begin
        for i:=0 to CSTransformList.Count -1 do begin
          if CSTransformList[i].EPSG = 0 then continue ;
          assert(
            assigned( CSEllipsoidList.ByEPSG( CSTransformList[i].FEllipsoid ) ),
            Format( 'Transformation EPSG=%d referring ' +
                    'to non existing ellipsoid EPSG=%d',
                   [ CSTransformList[i].EPSG, CSTransformList[i].FEllipsoid ]
                  )
          ) ;
        end ;
      end ;

    begin
      assert( assigned( CSDatumList.ByEPSG( 6030 ) ),
              Format( 'Datum EPSG=%d must be defined',
                      [ 6030 ]
                    )
              ) ;

      check_units ;
      check_datums ;
      check_transforms ;
    end ;
  {$ENDIF}
{$ENDIF}


{$ENDREGION}
{$REGION 'Global lists'}

function CSUnitsList
  : TGIS_CSUnitsList ;
begin
  if not assigned( cs_UnitsList ) then
    cs_UnitsList := TGIS_CSUnitsList.Create() ;

  Result := cs_UnitsList ;
end ;

function CSPrimeMeridianList
  : TGIS_CSPrimeMeridianList ;
var
  thc : TGIS_ThreadClass ;
begin
  if not assigned( cs_PrimeMeridianList ) then begin
    thc := TGIS_ThreadClass.Create ;
    try
      thc.LockThread ;
      try
        if not assigned( cs_PrimeMeridianList ) then
          cs_PrimeMeridianList
            := TGIS_CSPrimeMeridianList.Create ;
      finally
        thc.UnlockThread ;
      end;
    finally
      FreeObject( thc );
    end;
  end;

  Result := cs_PrimeMeridianList ;
end ;

function CSEllipsoidList
  : TGIS_CSEllipsoidList ;
var
  thc : TGIS_ThreadClass ;
begin
  if not assigned( cs_EllipsoidList ) then begin
    thc := TGIS_ThreadClass.Create ;
    try
      thc.LockThread ;
      try
        if not assigned( cs_EllipsoidList ) then
          cs_EllipsoidList
            := TGIS_CSEllipsoidList.Create ;
      finally
        thc.UnlockThread ;
      end;
    finally
      FreeObject( thc );
    end;
  end;

  Result := cs_EllipsoidList ;
end ;

function CSTransformList : TGIS_CSTransformList ;
var
  thc : TGIS_ThreadClass ;
begin
  if not assigned( cs_TransformList ) then begin
    thc := TGIS_ThreadClass.Create ;
    try
      thc.LockThread ;
      try
        if not assigned( cs_TransformList ) then
          cs_TransformList
            := TGIS_CSTransformList.Create ;
      finally
        thc.UnlockThread ;
      end;
    finally
      FreeObject( thc );
    end;
  end;

  Result := cs_TransformList ;
end ;

function CSDatumList : TGIS_CSDatumList ;
var
  thc : TGIS_ThreadClass ;
begin
  if not assigned( cs_DatumList ) then begin
    thc := TGIS_ThreadClass.Create ;
    try
      thc.LockThread ;
      try
        if not assigned( cs_DatumList ) then
          cs_DatumList
            := TGIS_CSDatumList.Create ;
      finally
        thc.UnlockThread ;
      end;
    finally
      FreeObject( thc );
    end;
  end;

  Result := cs_DatumList ;
end ;

function CSAreaList : TGIS_CSAreaList ;
var
  thc : TGIS_ThreadClass ;
begin
  if not assigned( cs_AreaList ) then begin
    thc := TGIS_ThreadClass.Create ;
    try
      thc.LockThread ;
      try
        if not assigned( cs_AreaList ) then
          cs_AreaList
            := TGIS_CSAreaList.Create ;
      finally
        thc.UnlockThread ;
      end;
    finally
      FreeObject( thc );
    end;
  end;

  Result := cs_AreaList ;
end ;

{$ENDREGION}

{$IFDEF DCC}
  initialization
    {$IFDEF DEBUG}
      check_structure_cs_base ;
    {$ENDIF}

  finalization
    FreeObject( cs_AreaList          ) ;
    FreeObject( cs_DatumList         ) ;
    FreeObject( cs_TransformList     ) ;
    FreeObject( cs_EllipsoidList     ) ;
    FreeObject( cs_PrimeMeridianList ) ;
    FreeObject( cs_UnitsList         ) ;
{$ENDIF}

{==================================== END =====================================}
end.

