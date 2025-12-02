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
  Mappings between Mapinfo coordinate system and EPSG
}

{$IFDEF DCC}
  unit GisCsMapInfo ;
  {$HPPEMIT '#pragma link "GisCsMapInfo"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    GisTypes,
    GisCsBase,
    GisCsSystems ;
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
  ///   Map MapInfo projections to build to projections.
  /// </summary>
  TGIS_CSMapinfoProjectionsMap = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

    public // public methods

      /// <inheritdoc/>
      procedure   Init   ; override;
      
  end;

  /// <summary>
  ///   Map MapInfo datums to build to datums.
  /// </summary>
  TGIS_CSMapinfoDatumsMap = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

    public // public methods

      /// <inheritdoc/>
      procedure   Init   ; override;
      
  end;

  /// <summary>
  ///   Map MapInfo units (as literal) to build to units.
  /// </summary>
  TGIS_CSMapinfoUnitsMap = {$IFDEF OXYGENE} public {$ENDIF}
                           class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;
      
    public // public methods
    
      /// <inheritdoc/>
      procedure   Init   ; override;
      
  end;

  /// <summary>
  ///   Map MapInfo units (as id) to build to units.
  /// </summary>
  TGIS_CSMapinfoUnits2Map = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_CSAbstractList )
    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;
      
    public // public methods

      /// <inheritdoc/>
      procedure   Init   ; override;
      
  end;
  
  /// <summary>
  ///   Map MapInfo units (as id) to build to units.
  /// </summary>
  TGIS_CSMapinfoEllipsoidMap = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_CSAbstractList )

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;
    public // public methods

      /// <inheritdoc/>
      procedure   Init   ; override;
      
  end;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Class to create Coordinate System Object or extract identifiers and
  ///   parameters from Mapinfo data.
  /// </summary>
  TGIS_CSFactoryMapInfo = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Utility function which parse coordinate system and returns Mapinfo
      ///   identifiers and parameters.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system to be interpreted
      /// </param>
      /// <param name="_projId">
      ///   projection identifier (or 0 if can not be interpreted)
      /// </param>
      /// <param name="_datumId">
      ///   datum identifier (or 0 if can not be interpreted)
      /// </param>
      /// <param name="_unitsId">
      ///   units identifier (or 0 if can not be interpreted)
      /// </param>
      /// <param name="_ellipsoidId">
      ///   ellipsoid identifier (or 0 if can not be interpreted)
      /// </param>
      /// <param name="_params">
      ///   array of projection parameters; array will be allocated by the procedure
      /// </param>
      /// <param name="_paramsEx">
      ///   array of datum parameters; array will be allocated by the procedure
      /// </param>
      class procedure ParseCs    ( const _cs          : TGIS_CSCoordinateSystem ;
                                   var   _projId      : Integer ;
                                   var   _datumId     : Integer ;
                                   var   _unitsId     : Integer ;
                                   var   _ellipsoidId : Integer ;
                                   var   _params      : TGIS_DoubleArray ;
                                   var   _paramsEx    : TGIS_DoubleArray
                                  )  ;

      /// <summary>
      ///   Utility function which parse coordinate system and returns Mapinfo
      ///   string as defined by MIF specification.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system to be interpreted
      /// </param>
      /// <returns>
      ///    CS as MapInfo string.
      /// </returns>
      class function  CsToText   ( const _cs : TGIS_CSCoordinateSystem
                                  ) : String ;

      /// <summary>
      ///   Build a coordinate system based on parameters.
      /// </summary>
      /// <param name="_projId">
      ///   projection identifier (or -1 if can not be interpreted)
      /// </param>
      /// <param name="_datumId">
      ///   datum identifier (or -1 if can not be interpreted)
      /// </param>
      /// <param name="_ellipsoidId">
      ///   ellipsoid identifier (or -1 if can not be interpreted)
      /// </param>
      /// <param name="_unitsId">
      ///   units identifier (or -1 if can not be interpreted)
      /// </param>
      /// <param name="_params">
      ///   array of projection parameters; should have 6 elements
      /// </param>
      /// <param name="_paramsEx">
      ///   array of datum parameters; should have 8 or 3 elements
      /// </param>
      /// <returns>
      ///   Newly created/Found object or nil.
      /// </returns>
      class function  BuildCs    ( const   _projId      : Integer ;
                                   const   _datumId     : Integer ;
                                   const   _ellipsoidId : Integer ;
                                   const   _unitsId     : String ;
                                   const   _params      : TGIS_DoubleArray ;
                                   const   _paramsEx    : TGIS_DoubleArray
                                  ) : TGIS_CSCoordinateSystem ; overload;

      /// <summary>
      ///   Build a coordinate system based on parameters.
      /// </summary>
      /// <param name="_projection">
      ///   projection identifier
      /// </param>
      /// <returns>
      ///   Newly created/Found object or nil.
      /// </returns>
      class function  BuildCs    ( const   _projection : String
                                 ) : TGIS_CSCoordinateSystem ; overload;
  end;

  /// <summary>
  ///   Map of EPSG -&gt; Mapinfo Ellipsoid codes
  /// </summary>
  /// <returns>
  ///   Global list of mappings between Mapinfo definitions and
  ///   internal objects.
  /// </returns>
  function CSMapinfoEllipsoidMap   : TGIS_CSMapinfoEllipsoidMap   ;


  /// <summary>
  ///   Map of EPSG -&gt; Mapinfo projection codes
  /// </summary>
  /// <returns>
  ///   Global list of mappings between Mapinfo definitions and
  ///   internal objects.
  /// </returns>
  function CSMapinfoProjectionsMap : TGIS_CSMapinfoProjectionsMap ;

  /// <summary>
  ///   Map of EPSG -&gt; Mapinfo datum codes
  /// </summary>
  /// <returns>
  ///   Global list of mappings between Mapinfo definitions and
  ///   internal objects.
  /// </returns>
  function CSMapinfoDatumsMap      : TGIS_CSMapinfoDatumsMap      ;

  /// <summary>
  ///   Map of EPSG -&gt; Mapinfo unit codes
  /// </summary>
  /// <returns>
  ///   Global list of mappings between Mapinfo definitions and
  ///   internal objects.
  /// </returns>
  function CSMapinfoUnitsMap       : TGIS_CSMapinfoUnitsMap       ;

  /// <summary>
  ///   Map of EPSG -&gt; Mapinfo unit codes
  /// </summary>
  /// <returns>
  ///   Global list of mappings between Mapinfo definitions and
  ///   internal objects.
  /// </returns>
  function CSMapinfoUnits2Map      : TGIS_CSMapinfoUnits2Map      ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,

    GisRtl,
    GisClasses,
    GisCsProjections ;
{$ENDIF}

var
  cs_MapinfoProjectionsMap : TGIS_CSMapinfoProjectionsMap ;
  cs_MapinfoDatumsMap      : TGIS_CSMapinfoDatumsMap      ;
  cs_MapinfoUnitsMap       : TGIS_CSMapinfoUnitsMap       ;
  cs_MapinfoUnits2Map      : TGIS_CSMapinfoUnits2Map      ;
  cs_MapinfoEllipsoidMap   : TGIS_CSMapinfoEllipsoidMap   ;

  //  map creator helper
  function _t(
    const _epsg : Integer ;
    const _wkt  : String
  ) : TGIS_CSAbstract ;
  begin
    Result := TGIS_CSAbstract.Create( _epsg, _wkt ) ;
  end ;

//------------------------------------------------------------------------------
// TGIS_CSMapinfoProjectionsMap
//------------------------------------------------------------------------------

  constructor TGIS_CSMapinfoProjectionsMap.Create ;
  begin
    inherited Create( cs_MapinfoProjectionsMap, False, True ) ;
    if not assigned( cs_MapinfoProjectionsMap ) then
      cs_MapinfoProjectionsMap := self ;
  end ;

  procedure TGIS_CSMapinfoProjectionsMap.Init;

    function _master( const _epsg : Integer ) : Integer ;
    var
      prj : TGIS_CSProjAbstract ;
    begin
      prj := CSProjList.ByEPSG( _epsg ) ;
      assert( assigned( prj ) ) ;
      Result := prj.EPSG ;
    end ;

  begin
    // Cylindrical Equal�Area
    Add( _t( _master( CSPROJ_Cylindrical_Equal_Area              ),  '2' ) ) ;

    // Lambert Conformal Conic
    Add( _t( _master( CSPROJ_Lambert_Conformal_Conic_2SP         ),  '3' ) ) ;

    // Lambert Azimuthal Equal�Area (polar aspect only)
    Add( _t( _master( CSPROJ_Azimuthal_Equal_Area                ),  '4' ) ) ;

    // Azimuthal Equidistant (polar aspect only)
    Add( _t( _master( CSPROJ_Azimuthal_Equidistance              ),  '5' ) ) ;

    // Equidistant Conic, also known as Simple Conic
    Add( _t( _master( CSPROJ_Equidistant_Conic                   ),  '6' ) ) ;

    // Hotine Oblique Mercator
    Add( _t( _master( CSPROJ_Hotine_Oblique_Mercator_Two_Point   ),  '7' ) ) ;

    // Transverse Mercator, also known as Gauss�Kruger
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ),  '8' ) ) ;

    // Albers Equal�Area Conic
    Add( _t( _master( CSPROJ_Albers                              ),  '9' ) ) ;

    // Mercator
    Add( _t( _master( CSPROJ_Mercator_1SP                        ), '10' ) ) ;

    // Miller Cylindrical
    Add( _t( _master( CSPROJ_Miller_Cylindrical                  ), '11' ) ) ;

    // Robinson
    Add( _t( _master( CSPROJ_Robinson                            ), '12' ) ) ;

    // Mollweide
    Add( _t( _master( CSPROJ_Mollweide                           ), '13' ) ) ;

    // Eckert IV
    Add( _t( _master( CSPROJ_Eckert_IV                           ), '14' ) ) ;

    // Eckert VI
    Add( _t( _master( CSPROJ_Eckert_VI                           ), '15' ) ) ;

    // Sinusoidal
    Add( _t( _master( CSPROJ_Sinusoidal                          ), '16' ) ) ;

    // Gall
    Add( _t( _master( CSPROJ_Gall_Stereographic                  ), '17' ) ) ;

    // New Zealand Map Grid
    Add( _t( _master( CSPROJ_New_Zealand_Map_Grid                ), '18' ) ) ;

    // Lambert Conformal Conic (modified for Belgium 1972)
    Add( _t( _master( CSPROJ_Lambert_Conformal_Conic_2SP_Belgium ), '19' ) ) ;

    // Stereographic
    Add( _t( _master( CSPROJ_Stereographic                       ), '20' ) ) ;

    // Transverse Mercator, (modified for Danish System 34 Jylland�Fyn)
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ), '21' ) ) ;

    // Transverse Mercator, (modified for Sjaelland)
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ), '22' ) ) ;

    // Transverse Mercator, (modified for Danish System 45 Bornholm)
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ), '23' ) ) ;

    // Transverse Mercator, (modified for Finnish KKJ)
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ), '24' ) ) ;

    // Swiss Oblique Mercator
    Add( _t( _master( CSPROJ_Swiss_Oblique_Mercator              ), '25' ) ) ;

    // Regional Mercator
    Add( _t( _master( CSPROJ_Mercator_2SP                        ), '26' ) ) ;

    // Polyconic
    Add( _t( _master( CSPROJ_Polyconic                           ), '27' ) ) ;

    // Azimuthal Equidistant (all origin latitudes)
    Add( _t( _master( CSPROJ_Azimuthal_Equidistance              ), '28' ) ) ;

    // Lambert Azimuthal Equal�Area
    Add( _t( _master( CSPROJ_Azimuthal_Equal_Area                ), '29' ) ) ;

    // Cassini/Soldner
    Add( _t( _master( CSPROJ_Cassini_Soldner                     ), '30' ) ) ;

    // Double Stereographic
    Add( _t( _master( CSPROJ_Double_Stereographic                ), '31' ) ) ;

    // Krovak Oblique Conformal Conic (JTSKc)
    Add( _t( _master( CSPROJ_Krovak                              ), '32' ) ) ;

    // Equidistant Cylindrical
    Add( _t( _master( CSPROJ_Equidistant_Cylindrical             ), '33' ) ) ;

    // Extended Transverse Mercator
    Add( _t( _master( CSPROJ_Transverse_Mercator                 ), '34' ) ) ;
  end;

//------------------------------------------------------------------------------
// TGIS_CSMapinfoDatumsMap
//------------------------------------------------------------------------------

  constructor TGIS_CSMapinfoDatumsMap.Create ;
  begin
    inherited Create( cs_MapinfoDatumsMap, False, True ) ;
    if not assigned( cs_MapinfoDatumsMap ) then
      cs_MapinfoDatumsMap := self ;
  end ;

  procedure TGIS_CSMapinfoDatumsMap.Init ;

    function mkGCS( const _datumId : Integer ) : Integer ;
    var
      datum : TGIS_CSDatum ;
      gcs   : TGIS_CSGeographicCoordinateSystem ;
    begin
      Result := 4326 ;

      datum := CSDatumList.ByEPSG( _datumId ) ;

      if assigned( datum ) then begin
        gcs := CSGeographicCoordinateSystemList.Prepare(  -1,
                                                         'GCS_' + datum.WKT,
                                                         datum.EPSG,
                                                         8901, // Greenwich
                                                         9122  // degree
                                                       ) ;
        if assigned( gcs ) then
          Result := gcs.EPSG ;
      end
    end;

  begin
    Add( _t( 4201          ,    '1' ) ); // Adindan
    Add( _t( 4205          ,    '2' ) ); // Afgooye
    Add( _t( 4204          ,    '3' ) ); // Ain_el_Abd_1970
    Add( _t( mkGCS(906231 ),    '4' ) ); // Anna_1_Astro_1965
    Add( _t( 4209          ,    '5' ) ); // Arc_1950
    Add( _t( 4210          ,    '6' ) ); // Arc_1960
    Add( _t( 4712          ,    '7' ) ); // Ascension_Islands
    Add( _t( mkGCS(906212) ,    '8' ) ); // Astro_Beacon_E
  //Add( _t(    0          ,    '9' ) ); // Astro_B4_Sorol_Atoll
    Add( _t( mkGCS(906238) ,   '10' ) ); // Astro_Dos_71_4
    Add( _t( mkGCS(906214) ,   '11' ) ); // Astronomic_Station_1952
    Add( _t( 4202          ,   '12' ) ); // Australian_Geodetic_Datum_66
    Add( _t( 4203          ,   '13' ) ); // Australian_Geodetic_Datum_84
    Add( _t( 4714          ,   '14' ) ); // Bellevue_Ign
    Add( _t( 4216          ,   '15' ) ); // Bermuda_1957
    Add( _t( 4218          ,   '16' ) ); // Bogota
    Add( _t( 4221          ,   '17' ) ); // Campo_Inchauspe
    Add( _t( mkGCS(906216) ,   '18' ) ); // Canton_Astro_1966
    Add( _t( 4222          ,   '19' ) ); // Cape
    Add( _t( 4717          ,   '20' ) ); // Cape_Canaveral
    Add( _t( 4223          ,   '21' ) ); // Carthage
    Add( _t( 4672          ,   '22' ) ); // Chatham_1971
    Add( _t( 4224          ,   '23' ) ); // Chua
    Add( _t( 4225          ,   '24' ) ); // Corrego_Alegre
    Add( _t( 4211          ,   '25' ) ); // Batavia
    Add( _t( mkGCS(906218) ,   '26' ) ); // Dos_1968
    Add( _t( 4719          ,   '27' ) ); // Easter_Island_1967
    Add( _t( 4230          ,   '28' ) ); // European_Datum_1950
    Add( _t( 4668          ,   '29' ) ); // European_Datum_1979
    Add( _t( 4233          ,   '30' ) ); // Gandajika_1970
    Add( _t( 4272          ,   '31' ) ); // New_Zealand_GD49
    Add( _t( 4036          ,   '32' ) ); // GRS_67
    Add( _t( 4019          ,   '33' ) ); // GRS_80
    Add( _t( 4675          ,   '34' ) ); // Guam_1963
    Add( _t( mkGCS(906221) ,   '35' ) ); // Gux_1_Astro
    Add( _t( 4254          ,   '36' ) ); // Hito_XVIII_1963
    Add( _t( 4658          ,   '37' ) ); // Hjorsey_1955
    Add( _t( 4738          ,   '38' ) ); // Hong_Kong_1963
    Add( _t( 4236          ,   '39' ) ); // Hu_Tzu_Shan
    Add( _t( 4240          ,   '40' ) ); // Indian_Thailand_Vietnam
    Add( _t( 4240          ,   '41' ) ); // Indian_Bangladesh
    Add( _t( 4299          ,   '42' ) ); // Ireland_1965
    Add( _t( mkGCS(906233) ,   '43' ) ); // ISTS_073_Astro_1969
    Add( _t( 4725          ,   '44' ) ); // Johnston_Island_1961
    Add( _t( 4244          ,   '45' ) ); // Kandawala
    Add( _t( 4698          ,   '46' ) ); // Kerguelen_Island
    Add( _t( 4245          ,   '47' ) ); // Kertau
    Add( _t( mkGCS(906243) ,   '48' ) ); // L_C_5_Astro
    Add( _t( 4251          ,   '49' ) ); // Liberia_1964
    Add( _t( 4253          ,   '50' ) ); // Luzon_Phillippines
    Add( _t( 4253          ,   '51' ) ); // Luzon_Mindanao_Island
    Add( _t( 4256          ,   '52' ) ); // Mahe_1971
    Add( _t( mkGCS(  6616) ,   '53' ) ); // Marco_Astro
    Add( _t( 4262          ,   '54' ) ); // Massawa
    Add( _t( 4261          ,   '55' ) ); // Merchich
    Add( _t( 4727          ,   '56' ) ); // Midway_Astro_1961
    Add( _t( 4263          ,   '57' ) ); // Minna
    Add( _t( 4270          ,   '58' ) ); // Nahrwan_Masirah_Island
    Add( _t( 4270          ,   '59' ) ); // Nahrwan_Un_Arab_Emirates
    Add( _t( 4270          ,   '60' ) ); // Nahrwan_Saudi_Arabia
    Add( _t( 4271          ,   '61' ) ); // Naparima_1972
    Add( _t( 4609          ,   '62' ) ); // NAD_1927
    Add( _t( 4608          ,   '63' ) ); // NAD_27_Alaska
    Add( _t( 4608          ,   '64' ) ); // NAD_27_Bahamas
    Add( _t( 4608          ,   '65' ) ); // NAD_27_San_Salvador
    Add( _t( 4608          ,   '66' ) ); // NAD_27_Canada
    Add( _t( 4608          ,   '67' ) ); // NAD_27_Canal_Zone
    Add( _t( 4608          ,   '68' ) ); // NAD_27_Caribbean
    Add( _t( 4608          ,   '69' ) ); // NAD_27_Central_America
    Add( _t( 4608          ,   '70' ) ); // NAD_27_Cuba
    Add( _t( 4608          ,   '71' ) ); // NAD_27_Greenland
    Add( _t( 4608          ,   '72' ) ); // NAD_27_Mexico
    Add( _t( 4608          ,   '73' ) ); // NAD_27_Michigan
    Add( _t( 4269          ,   '74' ) ); // North_American_Datum_1983
    Add( _t( 4129          ,   '75' ) ); // Observatorio_1966
    Add( _t( 4229          ,   '76' ) ); // Old_Egyptian
    Add( _t( 4135          ,   '77' ) ); // Old_Hawaiian
    Add( _t( mkGCS(906206) ,   '78' ) ); // Oman
    Add( _t( 4277          ,   '79' ) ); // OSGB_1936
    Add( _t( 4728          ,   '80' ) ); // Pico_De_Las_Nieves
    Add( _t( 4729          ,   '81' ) ); // Pitcairn_Astro_1967
    Add( _t( 4248          ,   '82' ) ); // Provisional_South_American
    Add( _t( 4139          ,   '83' ) ); // Puerto_Rico
    Add( _t( 4285          ,   '84' ) ); // Qatar_National
    Add( _t( 4194          ,   '85' ) ); // Qornoq
    Add( _t( 4626          ,   '86' ) ); // Reunion
    Add( _t( 4265          ,   '87' ) ); // Monte_Mario
    Add( _t( 4730          ,   '88' ) ); // Santo_Dos
    Add( _t( mkGCS(906249) ,   '89' ) ); // Sao_Braz
    Add( _t( 4292          ,   '90' ) ); // Sapper_Hill_1943
    Add( _t( 4293          ,   '91' ) ); // Schwarzeck
    Add( _t( 4618          ,   '92' ) ); // South_American_Datum_1969
    Add( _t( mkGCS(906207) ,   '93' ) ); // South_Asia
    Add( _t( mkGCS(  6615) ,   '94' ) ); // Southeast_Base
    Add( _t( mkGCS(  6183) ,   '95' ) ); // Southwest_Base
    Add( _t( 4298          ,   '96' ) ); // Timbalai_1948
    Add( _t( 4301          ,   '97' ) ); // Tokyo
    Add( _t( 4734          ,   '98' ) ); // Tristan_Astro_1968
    Add( _t( 4731          ,   '99' ) ); // Viti_Levu_1916
    Add( _t( mkGCS(906229) ,  '100' ) ); // Wake_Entiwetok_1960
  //Add( _t(    0          ,  '101' ) ); // WGS_60
    Add( _t( 4760          ,  '102' ) ); // WGS_66
    Add( _t( 4322          ,  '103' ) ); // WGS_1972
    Add( _t( 4326          ,  '104' ) ); // WGS_1984
    Add( _t( 4309          ,  '105' ) ); // Yacare
    Add( _t( 4311          ,  '106' ) ); // Zanderij
    Add( _t( 4275          ,  '107' ) ); // NTF
    Add( _t( 4231          ,  '108' ) ); // European_Datum_1987
    Add( _t( 4289          ,  '109' ) ); // Netherlands_Bessel
    Add( _t( 4313          ,  '110' ) ); // Belgium_Hayford
  //Add( _t(    0          ,  '111' ) ); // NWGL_10
    Add( _t( 4124          ,  '112' ) ); // Rikets_koordinatsystem_1990
    Add( _t( mkGCS(906262) ,  '113' ) ); // Lisboa_DLX
    Add( _t( 4274          ,  '114' ) ); // Melrica_1973_D73
    Add( _t( 4258          ,  '115' ) ); // Euref_98
    Add( _t( 4283          ,  '116' ) ); // GDA94
    Add( _t( 4167          ,  '117' ) ); // NZGD2000
    Add( _t( 4169          ,  '118' ) ); // America_Samoa
  //Add( _t(    0          ,  '119' ) ); // Antigua_Astro_1965
    Add( _t( 4713          ,  '120' ) ); // Ayabelle_Lighthouse
    Add( _t( 4219          ,  '121' ) ); // Bukit_Rimpah
    Add( _t( mkGCS(906101) ,  '122' ) ); // Estonia_1937
    Add( _t( 4155          ,  '123' ) ); // Dabola
    Add( _t( 4736          ,  '124' ) ); // Deception_Island
    Add( _t( mkGCS(906240) ,  '125' ) ); // Fort_Thomas_1955
    Add( _t( mkGCS(906241) ,  '126' ) ); // Graciosa_base_1948
    Add( _t( 4255          ,  '127' ) ); // Herat_North
    Add( _t( mkGCS(906102) ,  '128' ) ); // Hermanns_Kogel
  //Add( _t(    0          ,  '129' ) ); // Indian
    Add( _t( 4239          ,  '130' ) ); // Indian_1954
    Add( _t( 4131          ,  '131' ) ); // Indian_1960
    Add( _t( 4240          ,  '132' ) ); // Indian_1975
    Add( _t( 4238          ,  '133' ) ); // Indonesian_Datum_1974
    Add( _t( mkGCS(906242) ,  '134' ) ); // ISTS061_Astro_1968
    Add( _t( 4735          ,  '135' ) ); // Kusaie_Astro_1951
    Add( _t( 4250          ,  '136' ) ); // Leigon
    Add( _t( 4604          ,  '137' ) ); // Montserrat_Astro_1958
    Add( _t( 4266          ,  '138' ) ); // Mporaloko
    Add( _t( 4307          ,  '139' ) ); // North_Sahara_1959
    Add( _t( 4129          ,  '140' ) ); // Observatorio_Met_1939
    Add( _t( 4620          ,  '141' ) ); // Point_58
    Add( _t( 4282          ,  '142' ) ); // Pointe_Noire
    Add( _t( 4615          ,  '143' ) ); // Porto_Santo_1936
    Add( _t( 4616          ,  '144' ) ); // Selvagem_Grande_1938
    Add( _t( 4175          ,  '145' ) ); // Sierra_Leone_1960
    Add( _t( 4818          ,  '146' ) ); // S_JTSK_Ferro
    Add( _t( 4297          ,  '147' ) ); // Tananarive_1925
    Add( _t( 4304          ,  '148' ) ); // Voirol_1874
    Add( _t( mkGCS(  6811) ,  '149' ) ); // Virol_1960
    Add( _t( 4148          ,  '150' ) ); // Hartebeesthoek94
    Add( _t( 4041          ,  '151' ) ); // ATS77
    Add( _t( 4612          ,  '152' ) ); // JGD2000
    Add( _t( 4314          , '1000' ) ); // DHDN_Potsdam_Rauenberg
    Add( _t( 4178          , '1001' ) ); // Pulkovo_1942
    Add( _t( 4807          , '1002' ) ); // NTF_Paris_Meridian
    Add( _t( 4149          , '1003' ) ); // Switzerland_CH_1903
    Add( _t( 4237          , '1004' ) ); // Hungarian_Datum_1972
    Add( _t( 4222          , '1005' ) ); // Cape_7_Parameter
    Add( _t( 4203          , '1006' ) ); // AGD84_7_Param_Aust
    Add( _t( 4202          , '1007' ) ); // AGD66_7_Param_ACT
    Add( _t( 4202          , '1008' ) ); // AGD66_7_Param_TAS
    Add( _t( 4202          , '1009' ) ); // AGD66_7_Param_VIC_NSW
    Add( _t( 4272          , '1010' ) ); // NZGD_7_Param_49
    Add( _t( mkGCS(  6124) , '1011' ) ); // Rikets_Tri_7_Param_1990
  //Add( _t(    0          , '1012' ) ); // Russia_PZ90
  //Add( _t(    0          , '1013' ) ); // Russia_SK42
  //Add( _t(    0          , '1014' ) ); // Russia_SK95
    Add( _t( 4301          , '1015' ) ); // Tokyo
    Add( _t( 4123          , '1016' ) ); // Finnish_KKJ

    Add( _t( 4610          , '1017' ) ); // Xian 1980
    Add( _t( 4284          , '1018' ) ); // Lithuanian Pulkovo 1942
    Add( _t( 4313          , '1019' ) ); // Belgian 1972 7 Parameter
    Add( _t( 4156          , '1020' ) ); // S-JTSK with Ferro prime meridian
    Add( _t( 3906          , '1021' ) ); // Serbia datum MGI 1901
    Add( _t( 4307          , '1022' ) ); // North Sahara 7-parameter

    Add( _t( 4237          , '1023' ) ); // Hungarian Projection System (EOV)
    Add( _t( 4156          , '1024' ) ); // S-JTSK (Krovak) Coordinate system
    Add( _t( 8351          , '1025' ) ); // JTSK03 (Slovak Republic)
    Add( _t( 7844          , '1028' ) ); // Geocentric Datum of Australia 2020
  end;

//------------------------------------------------------------------------------
// TGIS_CSMapinfoUnitsMap
//------------------------------------------------------------------------------

  constructor TGIS_CSMapinfoUnitsMap.Create ;
  begin
    inherited Create( cs_MapinfoUnitsMap, False, True ) ;
    if not assigned( cs_MapinfoUnitsMap ) then
      cs_MapinfoUnitsMap := self ;
  end ;

  procedure TGIS_CSMapinfoUnitsMap.Init;
  begin
    Add( _t( 9036  , 'km'          ) ) ;
    Add( _t( 904003, 'in'          ) ) ;
    Add( _t( 9002  , 'ft'          ) ) ;
    Add( _t( 9037  , 'yd'          ) ) ;
    Add( _t (1025  , 'mm'          ) ) ;
    Add( _t( 1033  , 'cm'          ) ) ;
    Add( _t( 9001  , 'm'           ) ) ;
    Add( _t( 9003  , 'survey foot' ) ) ;
    Add( _t( 9003  , 'survey ft'   ) ) ;
    Add( _t( 9030  , 'nmi'         ) ) ;
    Add( _t( 9039  , 'li'          ) ) ;
    Add( _t( 9038  , 'ch'          ) ) ;
  //Add( _t( 0     , 'rd'          ) ) ;
    Add( _t( 9093  , 'mi'          ) ) ;
  end;

//------------------------------------------------------------------------------
// TGIS_CSMapinfoUnits2Map
//------------------------------------------------------------------------------

  constructor TGIS_CSMapinfoUnits2Map.Create ;
  begin
    inherited Create( cs_MapinfoUnits2Map, False, True ) ;
    if not assigned( cs_MapinfoUnits2Map ) then
      cs_MapinfoUnits2Map := self ;
  end ;

  procedure TGIS_CSMapinfoUnits2Map.Init;
  begin
    Add( _t( 9036  , '1'  ) ) ;
    Add( _t( 904003, '2'  ) ) ;
    Add( _t( 9002  , '3'  ) ) ;
    Add( _t( 9037  , '4'  ) ) ;
    Add( _t (904001, '5'  ) ) ;
    Add( _t( 904002, '6'  ) ) ;
    Add( _t( 9001  , '7'  ) ) ;
    Add( _t( 9003  , '8'  ) ) ;
    Add( _t( 9030  , '9'  ) ) ;
    Add( _t( 9039  , '30' ) ) ;
    Add( _t( 9038  , '31' ) ) ;
    Add( _t( 9102  , '13' ) ) ;
    Add( _t( 9093  , '0'  ) ) ;
  //Add( _t( 0     , '32' ) ) ; // ROD
  //Add( _t( 0     , '12' ) ) ; // Pica
  //Add( _t( 0     , '10' ) ) ; // Twips
  //Add( _t( 0     , '11' ) ) ; // Points

  end;

//------------------------------------------------------------------------------
// TGIS_CSMapinfoEllipsoidMap
//------------------------------------------------------------------------------

  constructor TGIS_CSMapinfoEllipsoidMap.Create ;
  begin
    inherited Create( cs_MapinfoEllipsoidMap, False, True ) ;
    if not assigned( cs_MapinfoEllipsoidMap ) then
      cs_MapinfoEllipsoidMap := self ;
  end ;

  procedure TGIS_CSMapinfoEllipsoidMap.Init ;

    function mkELP( const _wkt        : String ;
                    const _semi_major : Double ;
                    const _semi_minor : Double
                  ) : Integer ;
    var
      ellp : TGIS_CSEllipsoid ;
      flat : Double ;
    begin
      flat := 0 ;
      
      if not SameValue( _semi_major, _semi_minor ) then 
        flat := _semi_major / ( _semi_major - _semi_minor ) ;

      ellp := CSEllipsoidList.Prepare( -1, _wkt, _semi_major, flat ) ;

      assert( ellp.EPSG > GIS_EPSG_AUTO ) ;

      Result := ellp.EPSG ;
    end;

  begin
    Add( _t( 7019  ,  '0' ) ) ; // GRS 80
    Add( _t( 7043  ,  '1' ) ) ; // WGS 72
    Add( _t( 7003  ,  '2' ) ) ; // Australian
    Add( _t( 7024  ,  '3' ) ) ; // Krassovsky
    Add( _t( 7022  ,  '4' ) ) ; // International 1924
    Add( _t( 7022  ,  '5' ) ) ; // Hayford
    Add( _t( 7012  ,  '6' ) ) ; // Clarke 1880
    Add( _t( 7008  ,  '7' ) ) ; // Clarke 1866
    Add( _t( 7009  ,  '8' ) ) ; // Clarke 1866 (modified for Michigan)
    Add( _t( 7001  ,  '9' ) ) ; // Airy 1930
    Add( _t( 7004  , '10' ) ) ; // Bessel 1841
    Add( _t( 7015  , '11' ) ) ; // Everest (India 1830)
    Add( _t( 7052  , '12' ) ) ; // Sphere
    Add( _t( 7002  , '13' ) ) ; // Airy 1930 (modified for Ireland 1965
    Add( _t( 7046  , '14' ) ) ; // Bessel 1841 (modified for Schwarzeck)
    Add( _t( 7013  , '15' ) ) ; // Clarke 1880 (modified for Arc 1950)
    Add( _t( 7014  , '16' ) ) ; // Clarke 1880 (modified for Merchich)
    Add( _t( 7018  , '17' ) ) ; // Everest (W. Malaysia and Singapore 1948)
    Add( _t( 905002, '18' ) ) ; // Fischer 1960
    Add( _t( 905004, '19' ) ) ; // Fischer 1960 (modified for South Asia)
    Add( _t( 905003, '20' ) ) ; // Fischer 1968
    Add( _t( 7036  , '21' ) ) ; // GRS 67
    Add( _t( 7020  , '22' ) ) ; // Helmert 1906
    Add( _t( 7053  , '23' ) ) ; // Hough
    Add( _t( 7003  , '24' ) ) ; // South American
    Add( _t( 7029  , '25' ) ) ; // War Office
    Add( _t( mkELP( 'WGS 60',
                     6378165.0, 298.3
                  ),'26' ) ) ; // WGS 60
    Add( _t( 7025 , '27' ) ) ; // WGS 66
    Add( _t( 7030 , '28' ) ) ; // WGS 84
    Add( _t( 7030 , '29' ) ) ; // WGS 84 (MAPINFO Datum 0)
    Add( _t( 7011 , '30' ) ) ; // Clarke 1880 (modified for IGN)
    Add( _t( 7049 , '31' ) ) ; // IAG 75
    Add( _t( mkELP( 'MERIT 83',
                     6378137.0, 298.257
                  ),'32' ) ) ; // MERIT 83
    Add( _t( mkELP( 'New International 1967',
                     6378157.5, 298.25
                  ),'33' ) ) ; // New International 1967
    Add( _t( 905005,'34' ) ) ; // Walbeck
    Add( _t( 7005 , '35' ) ) ; // Bessel 1841 (modified for NGO 1948)
    Add( _t( 7007 , '36' ) ) ; // Clarke 1858
    Add( _t( mkELP( 'Clarke 1880 (modified for Jamaica)',
                     6378249.136, 293.46631
                  ),'37' ) ) ; // Clarke 1880 (modified for Jamaica)
    Add( _t( 7010 , '38' ) ) ; // Clarke 1880 (modified for Palestine)
    Add( _t( 7016 , '39' ) ) ; // Everest (Brunei and East Malaysia)
    Add( _t( 7044 , '40' ) ) ; // Everest (India 1956)
    Add( _t( 7021 , '41' ) ) ; // Indonesian
    Add( _t( 7025 , '42' ) ) ; // NWL 9D
    Add( _t( 7043 , '43' ) ) ; // NWL 10D
    Add( _t( 7032 , '44' ) ) ; // OSU86F
    Add( _t( 7033 , '45' ) ) ; // OSU91A
    Add( _t( 7027 , '46' ) ) ; // Plessis 1817
    Add( _t( 7028 , '47' ) ) ; // Struve 1860
    Add( _t( 7018 , '48' ) ) ; // Everest (West Malaysia 1969)
    { TODO : Verify if it's still needed }
    //? Add( _t( 7001 , '49'  ) ) ; // Irish (WOFO)  in a future
    Add( _t( mkELP( 'Everest (Pakistan)',
                     6377309.613, 300.8017
                  ),'50' ) ) ; // Everest (Pakistan)
    Add( _t( 7041 , '51' ) ) ; // ATS77 (Average Terrestrial System 1977)
    Add( _t( 7054 , '52' ) ) ; // PZ90
    Add( _t( 7049 , '53' ) ) ; // Xian 1980
    Add( _t( 7030 , '54' ) ) ; // WGS 84 (MAPINFO Datum 157)
    Add( _t( 7059 , '55' ) ) ; // WGS 84 Sphere
    Add( _t( 1025 , '56' ) ) ; // GSK-2011
    Add( _t( 7054 , '57' ) ) ; // PZ-90.11
  end ;

//------------------------------------------------------------------------------
// Global lists
//------------------------------------------------------------------------------

  function CSMapinfoProjectionsMap
    : TGIS_CSMapinfoProjectionsMap ;
  begin
    if not assigned( cs_MapinfoProjectionsMap ) then
      cs_MapinfoProjectionsMap := TGIS_CSMapinfoProjectionsMap.Create ;

    Result := cs_MapinfoProjectionsMap ;
  end;

  function CSMapinfoDatumsMap
    : TGIS_CSMapinfoDatumsMap ;
  begin
    if not assigned( cs_MapinfoDatumsMap ) then
      cs_MapinfoDatumsMap := TGIS_CSMapinfoDatumsMap.Create ;

    Result := cs_MapinfoDatumsMap ;
  end;

  function CSMapinfoUnitsMap
    : TGIS_CSMapinfoUnitsMap ;
  begin
    if not assigned( cs_MapinfoUnitsMap ) then
      cs_MapinfoUnitsMap := TGIS_CSMapinfoUnitsMap.Create ;

    Result := cs_MapinfoUnitsMap ;
  end;

  function CSMapinfoUnits2Map
    : TGIS_CSMapinfoUnits2Map ;
  begin
    if not assigned( cs_MapinfoUnits2Map ) then
      cs_MapinfoUnits2Map := TGIS_CSMapinfoUnits2Map.Create ;

    Result := cs_MapinfoUnits2Map ;
  end;

  function CSMapinfoEllipsoidMap
    : TGIS_CSMapinfoEllipsoidMap ;
  begin
    if not assigned( cs_MapinfoEllipsoidMap ) then
      cs_MapinfoEllipsoidMap := TGIS_CSMapinfoEllipsoidMap.Create ;

    Result := cs_MapinfoEllipsoidMap ;
  end;

//------------------------------------------------------------------------------
// Public methods
//------------------------------------------------------------------------------

  class procedure TGIS_CSFactoryMapInfo.ParseCs(
    const _cs           : TGIS_CSCoordinateSystem ;
    var   _projId       : Integer ;
    var   _datumId      : Integer ;
    var   _unitsId      : Integer ;
    var   _ellipsoidId  : Integer ;
    var   _params       : TGIS_DoubleArray ;
    var   _paramsEx     : TGIS_DoubleArray
  )  ;
  var
    gcs       : TGIS_CSGeographicCoordinateSystem ;
    pcs       : TGIS_CSProjectedCoordinateSystem  ;
    gcs_epsg  : Integer ;
    prj_epsg  : Integer ;
    unt_epsg  : Integer ;
    objmap    : TGIS_CSAbstract ;

    // make GCS based on ellipsoid
    function mkGCS( const _ellp : TGIS_CSEllipsoid ) : Integer ;
    var
      new_dat : TGIS_CSDatum ;
      new_gcs : TGIS_CSGeographicCoordinateSystem ;
    begin
      Result := 0 ;

      // first prepare datum on ellipsoid
      new_dat := CSDatumList.Prepare(
                   -1,
                   gcs.Datum.Ellipsoid.WKT,
                   gcs.Datum.Ellipsoid.EPSG,
                   0
                 ) ;
      if not assigned( new_dat ) then
        exit ;

      // then prepare GCS
      new_gcs := CSGeographicCoordinateSystemList.Prepare(
                   -1,
                   'GCS_' + new_dat.WKT,
                   new_dat.EPSG,
                   8901, // Greenwich
                   9122  // Decimal degree
                 ) ;

      if not assigned( new_gcs ) then
        exit ;

      Result := new_gcs.EPSG ;
    end ;

  begin
     _projId      := 0 ;
     _datumId     := 0 ;
     _unitsId     := 0 ;
     _ellipsoidId := 0 ;

    if _cs is TGIS_CSProjectedCoordinateSystem then begin
      pcs := _cs as TGIS_CSProjectedCoordinateSystem ;
      gcs := pcs.Geocs ;

      gcs_epsg := gcs.EPSG ;
      prj_epsg := pcs.Projection.EPSG ;
      unt_epsg := pcs.Units.EPSG ;

      objmap := CSMapinfoProjectionsMap.ByEPSG( prj_epsg ) ;
      if assigned( objmap ) then
        _projId := StrToInt( objmap.WKT ) ;

      objmap := CSMapinfoUnits2Map.ByEPSG( unt_epsg ) ;
      if assigned( objmap ) then
        _unitsId := StrToInt( objmap.WKT ) ;

      objmap := CSMapinfoDatumsMap.ByEPSG( gcs_epsg ) ;
      if assigned( objmap ) then
        _datumId := StrToInt( objmap.WKT )
      else begin
        objmap := CSMapinfoDatumsMap.ByEPSG( mkGCS( gcs.Datum.Ellipsoid ) ) ;
        if assigned( objmap ) then
          _datumId := StrToInt( objmap.WKT )
      end;

      objmap := CSMapinfoEllipsoidMap.ByEPSG( gcs.Datum.Ellipsoid.EPSG ) ;
      if assigned( objmap ) then
        _ellipsoidId := StrToInt( objmap.WKT ) ;

      SetLength( _paramsEx, 8 ) ;
      if assigned( gcs.Datum.Transform ) and
         (( gcs.Datum.Transform.Method = 9606 ) or
          ( gcs.Datum.Transform.Method = 9603 )) then begin
        _paramsEx[ 0 ] := gcs.Datum.Transform.A ;
        _paramsEx[ 1 ] := gcs.Datum.Transform.B ;
        _paramsEx[ 2 ] := gcs.Datum.Transform.C ;
        _paramsEx[ 3 ] := gcs.Datum.Transform.D ;
        _paramsEx[ 4 ] := gcs.Datum.Transform.E ;
        _paramsEx[ 5 ] := gcs.Datum.Transform.F ;
        _paramsEx[ 6 ] := gcs.Datum.Transform.G ;
      end;

      case _projId of
        3,
        6,
        9,
        19 : begin
                SetLength( _params, 6 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;

               // MapInfo expected sorted parallels !
               _params[ 2 ] := RadToDeg(
                                 Min(    pcs.Projection.StandardParallel_1,
                                         pcs.Projection.StandardParallel_2
                                    )
                               );
               _params[ 3 ] := RadToDeg(
                                 Max(    pcs.Projection.StandardParallel_1,
                                         pcs.Projection.StandardParallel_2
                                    )
                               );
               _params[ 4 ] := pcs.Projection.FalseEasting ;
               _params[ 5 ] := pcs.Projection.FalseNorthing ;
             end;
        4,
        29 : begin
                SetLength( _params, 3 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := 90                                            ;
               if ( Abs( pcs.Projection.LatitudeOfOrigin ) - Pi/2 ) < 1e-10
               then
                 _projId := 4
               else
                 _projId := 29 ;
             end;
        5,
        28 :  begin
                SetLength( _params, 3 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := 90                                            ;
                if ( Abs( pcs.Projection.LatitudeOfOrigin ) - Pi/2 ) < 1e-10
                then
                  _projId := 5
                else
                  _projId := 28 ;
             end;
        2,
        26 : begin
                SetLength( _params, 2 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.StandardParallel_1 ) ;
             end;
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17 : begin
                SetLength( _params, 1 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
             end;
        7  : begin
                SetLength( _params, 6 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := RadToDeg( pcs.Projection.Azimuth )            ;
               _params[ 3 ] := pcs.Projection.ScaleFactor                    ;
               _params[ 4 ] := pcs.Projection.FalseEasting                   ;
               _params[ 5 ] := pcs.Projection.FalseNorthing                  ;
             end;
        18,
        25,
        27 : begin
                SetLength( _params, 4 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := pcs.Projection.FalseEasting                   ;
               _params[ 3 ] := pcs.Projection.FalseNorthing                  ;
             end;
        20: begin
                SetLength( _params, 5 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := pcs.Projection.ScaleFactor                    ;
               _params[ 3 ] := pcs.Projection.FalseEasting                   ;
               _params[ 4 ] := pcs.Projection.FalseNorthing                  ;
             end;
        8,
        21,
        22,
        23,
        24,
        34: begin
               _projId := 8 ; //? difference in a future by datum
                SetLength( _params, 5 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := pcs.Projection.ScaleFactor                    ;
               _params[ 3 ] := pcs.Projection.FalseEasting                   ;
               _params[ 4 ] := pcs.Projection.FalseNorthing                  ;
             end;
        30 : begin
                SetLength( _params, 4 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := pcs.Projection.FalseEasting                   ;
               _params[ 3 ] := pcs.Projection.FalseNorthing                  ;
             end;
        31 : begin
                SetLength( _params, 5 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := pcs.Projection.ScaleFactor                    ;
               _params[ 3 ] := pcs.Projection.FalseEasting                   ;
               _params[ 4 ] := pcs.Projection.FalseNorthing                  ;
             end;
        32 : begin
                SetLength( _params, 6 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.LatitudeOfOrigin   ) ;
               _params[ 2 ] := RadToDeg( pcs.Projection.StandardParallel_1 ) ;
               _params[ 3 ] := RadToDeg( pcs.Projection.Azimuth )            ;
               _params[ 4 ] := pcs.Projection.FalseEasting                   ;
               _params[ 5 ] := pcs.Projection.FalseNorthing                  ;
             end;
        33 : begin
                SetLength( _params, 4 ) ;
               _params[ 0 ] := RadToDeg( pcs.Projection.CentralMeridian    ) ;
               _params[ 1 ] := RadToDeg( pcs.Projection.StandardParallel_1 ) ;
               _params[ 2 ] := pcs.Projection.FalseEasting                   ;
               _params[ 3 ] := pcs.Projection.FalseNorthing                  ;
             end;
      end;
    end
    else if _cs is TGIS_CSGeographicCoordinateSystem then begin
      gcs := _cs as TGIS_CSGeographicCoordinateSystem ;
      gcs_epsg := gcs.EPSG ;

      _projId  := 1 ;

      objmap := CSMapinfoDatumsMap.ByEPSG( gcs_epsg ) ;
      if assigned( objmap ) then
        _datumId := StrToInt( objmap.WKT )
      else begin
        objmap := CSMapinfoEllipsoidMap.ByEPSG( gcs.Datum.Ellipsoid.EPSG ) ;
        if assigned( objmap ) then
          _ellipsoidId := StrToInt( objmap.WKT )
      end;

      SetLength( _paramsEx, 8 ) ;
      if assigned( gcs.Datum.Transform ) and
        (( gcs.Datum.Transform.Method = 9606 ) or
         ( gcs.Datum.Transform.Method = 9603 )) then begin
        _paramsEx[ 0 ] := gcs.Datum.Transform.A ;
        _paramsEx[ 1 ] := gcs.Datum.Transform.B ;
        _paramsEx[ 2 ] := gcs.Datum.Transform.C ;
        _paramsEx[ 3 ] := gcs.Datum.Transform.D ;
        _paramsEx[ 4 ] := gcs.Datum.Transform.E ;
        _paramsEx[ 5 ] := gcs.Datum.Transform.F ;
        _paramsEx[ 6 ] := gcs.Datum.Transform.G ;
      end;
    end
    else begin
      SetLength( _params, 5 ) ;
      SetLength( _paramsEx, 8 ) ;
    end;
  end;

  class function TGIS_CSFactoryMapInfo.CsToText(
    const _cs : TGIS_CSCoordinateSystem
  ) : String ;
  var
    i             : Integer ;
    proj_id       : Integer ;
    datum_id      : Integer ;
    units_id      : Integer ;
    ellipsoid_id  : Integer ;
    units         : TGIS_CSAbstract ;
    units_id2     : Integer ;
    units_txt2    : String  ;
    units2        : TGIS_CSAbstract ;
    aparams       : TGIS_DoubleArray ;
    dparams       : TGIS_DoubleArray ;
    sparams       : String  ;
    proper        : Boolean ;

    function to_deg( const _value : Double ) : String ;
    begin
      Result := DotFloatToStr( RadToDeg(  _value ) ) ;
    end;

    function to_dbl( const _value : Double ) : String ;
    begin
      Result := DotFloatToStr( _value ) ;
    end;

  begin
    TGIS_CSFactoryMapInfo.ParseCs( _cs, proj_id, datum_id, units_id, ellipsoid_id,
                       aparams, dparams
                      ) ;
    sparams := '';
    for i:= 0  to high( aparams ) do begin
      if i > 0 then
        sparams := sparams + ',' ;
      sparams := sparams + DotFloatToStr( aparams[i] ) ;
    end ;

    if datum_id <= 0 then
      datum_id := 104 ; //WGS 84

    proper := False ;

    try
      if proj_id = 1 then begin
        Result  := Format( ' CoordSys Earth Projection %d, %d',
                           [ proj_id, datum_id ]
                         ) ;
        proper := True ;
      end
      else if proj_id > 1 then begin
        units := CSMapinfoUnits2Map.ByWKT( IntToStr( units_id ) ) ;

        if assigned( units ) then begin

          units_id2 := units.EPSG ;

          units2 := CSMapinfoUnitsMap.ByEPSG( units_id2 ) ;
          if assigned( units2 ) then begin
            units_txt2 := units2.WKT ;

            Result := Format( ' CoordSys Earth Projection %d, %d, "%s", %s',
                              [ proj_id, datum_id, units_txt2, sparams ]
                            ) ;
            proper := True ;
          end ;
        end ;
      end ;
    except
      // do nothing ;
    end ;

    if not proper then
      Result := ' CoordSys NonEarth Units "m"' ;
  end;

  class function TGIS_CSFactoryMapInfo.BuildCs(
    const _projection : String
  ) : TGIS_CSCoordinateSystem ;
  var
    tkn         : TGIS_Tokenizer  ;
    proj_id     : String          ;
    unit_id     : String          ;
    datum_id    : String          ;
    ellipsoid_id: String          ;
    i, j        : Integer         ;
    params      : TGIS_DoubleArray ;
    paramsEx    : TGIS_DoubleArray ;

    function tkn_to_dbl( const _idx : Integer ) : Double ;
    begin
      Result := 0 ;
      if _idx > tkn.Result.Count then exit ;

      Result := DotStrToFloat( tkn.Result[ _idx ] ) ;
    end;

    function tkn_find( const _idx : Integer ) : Boolean ;
    begin
      Result := ( _idx < tkn.Result.Count ) and
                ( tkn.Result[ _idx ] <> 'BOUNDS' ) ;
    end;

  begin
    Result := CSUnknownCoordinateSystem ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( UpperCase( _projection ), [' ', #9, '(',')',','], True ) ;
      if tkn.Result.Count = 0 then exit ;

      ellipsoid_id := '-1' ;
      i := 6 ;
      if tkn.Result[ 0 ] = 'COORDSYS' then begin
        if ( tkn.Result.Count > 2           ) and
           ( tkn.Result[ 1 ] = 'EARTH'      ) and
           ( tkn.Result[ 2 ] = 'PROJECTION' ) then begin

          proj_id  := ( tkn.Result[ 3 ] ) ;
          datum_id := ( tkn.Result[ 4 ] ) ;
          unit_id  := '' ;
          if      ( proj_id <> '1'     ) and
                  ( datum_id <> '999'  ) and
                  ( datum_id <> '9999' ) then begin

                    if tkn.Result.Count > 5 then
                       unit_id  := tkn.Result[ 5 ] ;
          end
          else    if ( datum_id = '9999' ) then begin
                    if tkn.Result.Count > 14 then
                      unit_id  := tkn.Result[ 14 ] ;
                    i := 15 ;
                    ellipsoid_id := tkn.Result[ 5 ] ;
                    SetLength( paramsEx, 8 ) ;
                    j := 6 ;
                    while j < 14 do begin
                      if not tkn_find(j) then break ;
                      paramsEx[ j-6 ] := tkn_to_dbl( j ) ;
                      inc( j ) ;
                    end;
          end
          else    if ( datum_id = '999' ) then begin
                    if tkn.Result.Count > 19 then
                      unit_id  := tkn.Result[ 19 ] ;
                    i := 10 ;
                    ellipsoid_id := tkn.Result[ 5 ] ;
                    SetLength( paramsEx, 8 ) ;
                    j := 6 ;
                    while j < 9 do begin
                      if not tkn_find(j) then break ;
                      paramsEx[ j-6 ] := tkn_to_dbl( j ) ;
                      inc( j ) ;
                    end;
          end;
        end
        else if ( tkn.Result[ 1 ] = 'NONEARTH' ) then begin
          proj_id  := '0' ;
          datum_id := '0' ;
          if tkn.Result[ 2 ] = 'UNITS' then
            unit_id := tkn.Result[ 3 ] ;
          i := 4 ;
        end
        else begin
          proj_id      := '0' ;
          datum_id     := '0' ;
          ellipsoid_id := '0' ;
        end;
      end
      else begin
        if tkn.Result.Count > 0 then begin
          if ( tkn.Result[ 1 ] = '9999' ) or ( tkn.Result[ 1 ] = '999' ) then begin
            proj_id     := tkn.Result[ 0 ] ;
            datum_id    := tkn.Result[ 1 ] ;
            ellipsoid_id:= tkn.Result[ 2 ] ;
            i := 3 ;
            SetLength( paramsEx, 8 ) ;
            j := 3 ;
            while j < 11 do begin
              if not tkn_find(j) then break ;
              paramsEx[ j-3 ] := tkn_to_dbl( j ) ;
              inc( j ) ;
            end;
          end
          else begin
            proj_id  := tkn.Result[ 0 ] ;
            datum_id := tkn.Result[ 1 ] ;
            ellipsoid_id := '-1' ;
            i := 3 ;

            if tkn.Result.Count > 2 then
              unit_id := tkn.Result[ 2 ] ;
          end;
        end;
      end ;

      j := 0 ;
      SetLength( params, 6 ) ;
      try
        while j < 6 do begin
          if not tkn_find(i) then break ;

          params[ j ] := tkn_to_dbl( i ) ;
          inc( j ) ;
          inc( i ) ;
        end;
      except
        // in case of incomplete params
      end ;

      Result := TGIS_CSFactoryMapInfo.BuildCs( StrToInt( proj_id  ),
                                   StrToInt( datum_id ),
                                   StrToInt( ellipsoid_id ),
                                   unit_id,
                                   params,
                                   paramsEx
                                  ) ;
    finally
      FreeObject( tkn ) ;
    end;
  end ;

  class function TGIS_CSFactoryMapInfo.BuildCs(
    const _projId      : Integer ;
    const _datumId     : Integer ;
    const _ellipsoidId : Integer ;
    const _unitsId     : String ;
    const _params      : TGIS_DoubleArray ;
    const _paramsEx    : TGIS_DoubleArray
  ) : TGIS_CSCoordinateSystem ;
  var
    proj_param : TGIS_CSProjParameters ;
    gcs_map    : TGIS_CSAbstract ;
    unit_map   : TGIS_CSAbstract ;
    proj_map   : TGIS_CSAbstract ;
    unit_obj   : TGIS_CSUnits    ;
    trans_met  : Integer ;
    base_proj  : Integer ;

    function prepareCustomGcs : TGIS_CSGeographicCoordinateSystem ;
    const
      AC = (Pi/180.0) / 3600.0  ; // arc second
    var
      elp_map    : TGIS_CSAbstract ;
      elp        : TGIS_CSEllipsoid ;
      datum      : TGIS_CSAbstract ;
    begin
      Result := nil ;

      if (_projId = 1) and (_datumId = 0) and (_ellipsoidId = -1) then begin
        elp_map := CSMapinfoEllipsoidMap.ByWKT( '29' ) ;// MAPINFO Datum 0
        if assigned( elp_map ) then
          elp := CSEllipsoidList.ByEPSG( elp_map.EPSG )
        else
          elp := nil ;
        if assigned( elp ) then begin
          datum := CSDatumList.PrepareEx( -1, '', elp.EPSG, 9606, 1,
                                          0,0,0,0,0,0,0,0,0,0
                                         ) ;
          Result := CSGeographicCoordinateSystemList.Prepare( -1,
                                                              'GCS_' + datum.WKT,
                                                              datum.EPSG,
                                                              8901, // Greenwich
                                                              9122  // degree
                                                             ) ;
          exit ;
        end ;
      end
      else
        elp_map := CSMapinfoEllipsoidMap.ByWKT( IntToStr( _ellipsoidId ) ) ;

      if assigned( elp_map ) then
        elp := CSEllipsoidList.ByEPSG( elp_map.EPSG )
      else
        elp := nil ;

      if assigned( elp ) and ( length( _paramsEx ) > 7 ) then begin
        if ( _datumId = 9999 ) or ( _datumId = 0 ) then
          trans_met := 9607 ;

        // 9603 must have only 3 parameters
        if (_paramsEx[3]=0) and (_paramsEx[4]=0) and (_paramsEx[5]=0) and
           (_paramsEx[6]=0) and (_paramsEx[7]=0) then
          trans_met := 9603
        else
          trans_met := 9607 ;

        if trans_met = 9607 then begin
          _paramsEx[3] := _paramsEx[3] * AC ;
          _paramsEx[4] := _paramsEx[4] * AC ;
          _paramsEx[5] := _paramsEx[5] * AC ;
          _paramsEx[6] := _paramsEx[6] / 1000000 ;
        end ;

        datum := CSDatumList.PrepareEx( -1,
                                        '',
                                        elp.EPSG, trans_met, 1,
                                        _paramsEx[0],_paramsEx[1],_paramsEx[2],
                                        _paramsEx[3],_paramsEx[4],_paramsEx[5],
                                        _paramsEx[6],_paramsEx[7], 0, 0
                                       ) ;
        Result := CSGeographicCoordinateSystemList.Prepare( -1,
                                                            'GCS_' + datum.WKT,
                                                            datum.EPSG,
                                                            8901, // Greenwich
                                                            9122  // degree
                                                           ) ;
      end;
    end ;

  begin
    assert( length( _params ) > 5 ) ;
    Result := CSUnknownCoordinateSystem ;

    proj_map := CSMapinfoProjectionsMap.ByWKT( IntToStr( _projId ) ) ;
    if assigned( proj_map ) then
      proj_param := CSProjectedCoordinateSystemList.DefaultParams( proj_map.EPSG )
    else
      proj_param := CSProjectedCoordinateSystemList.EmptyParams ;

    base_proj := _projId ;
    if (base_proj >= 3000) then
      base_proj := base_proj - 3000
    else if (base_proj >= 2000) then
      base_proj := base_proj - 2000
    else if (base_proj >= 1000) then
      base_proj := base_proj - 1000 ;

    case base_proj of
      0 : begin // NonEarth
            Result := CSUnknownCoordinateSystem ;
          end ;
      1 : begin // lat/long
            gcs_map := CSMapinfoDatumsMap.ByWKT( IntToStr( _datumId ) ) ;
            if not assigned( gcs_map ) then begin
              gcs_map := prepareCustomGcs ;
              if not assigned( gcs_map ) then
                Result := CSUnknownCoordinateSystem
              else
                Result := CSGeographicCoordinateSystemList.ByEPSG( gcs_map.EPSG )
            end
            else
              Result := CSGeographicCoordinateSystemList.ByEPSG( gcs_map.EPSG )
          end ;
      2 : begin // Cylindrical Equal Area
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 1 ] ) ;
          end;
      3 : begin // Lambert Conic Conformal
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 2 ] ) ;
            proj_param.StandardParallel_2  := DegToRad( _params[ 3 ] ) ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      4 : begin // Lambert Azimuthal Equal Area
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := 0 ;
            proj_param.FalseNorthing       := 0 ;
          end;
      5 : begin // Azimuthal Equidistant (Polar aspect only)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := 0 ;
            proj_param.FalseNorthing       := 0 ;
          end;
      6 : begin // Equidistant Conic
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 2 ] ) ;
            proj_param.StandardParallel_2  := DegToRad( _params[ 3 ] ) ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      7 : begin // Hotine Oblique Mercator
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.Azimuth             := DegToRad( _params[ 2 ] ) ;
            proj_param.ScaleFactor         := _params[ 3 ] ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      8 : begin // Transverse Mercator
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      9 : begin // Albers Conic Equal Area
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 2 ] ) ;
            proj_param.StandardParallel_2  := DegToRad( _params[ 3 ] ) ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      10 : begin // Mercator
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      11 : begin // Miller Cylindrical
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      12 : begin // Robinson
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      13 : begin // Mollweide
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      14 : begin // Eckert IV
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      15 : begin // Eckert VI
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      16 : begin // Sinusoidal
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      17 : begin // Gall Stereographic
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
          end;
      18 : begin // New Zealand Map Grid
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := _params[ 2 ] ;
            proj_param.FalseNorthing       := _params[ 3 ] ;
          end;
      19 : begin // Lambert Conic Conformal (Belgium)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 2 ] ) ;
            proj_param.StandardParallel_2  := DegToRad( _params[ 3 ] ) ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      20 : begin // Stereographic
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      21 : begin // Transverse Mercator,(modified for Danish System 34 Jylland-Fyn)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      22 : begin // Transverse Mercator,(modified for Danish System 34 Sjaelland)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      23 : begin // Transverse Mercator,(modified for Danish System 34/45 Bornholm)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      24 : begin // Transverse Mercator,(modified for Finnish KKJ)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      25 : begin // Swiss Oblique Mercator / Cylindrical
            proj_param.LongitudeOfCenter   := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfCenter    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := _params[ 2 ] ;
            proj_param.FalseNorthing       := _params[ 3 ] ;
            proj_param.ScaleFactor         := 1 ;
          end;
      26 : begin // Regional Mercator (regular mercator with a latitude).
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := 0 ;
            proj_param.FalseNorthing       := 0 ;
            proj_param.ScaleFactor         := 1 ;
          end;
      27 : begin // Polyconic
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := _params[ 2 ] ;
            proj_param.FalseNorthing       := _params[ 3 ] ;
          end;
      28 : begin // Azimuthal Equidistant
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := 0 ;
            proj_param.FalseNorthing       := 0 ;
          end;
      29 : begin // Lambert Azimuthal Equal Area
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := 0 ;
            proj_param.FalseNorthing       := 0 ;
          end;
      30 : begin // Cassini/Soldner
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := _params[ 2 ] ;
            proj_param.FalseNorthing       := _params[ 3 ] ;
          end;
      31 : begin // Double Stereographic
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;
      32 : begin // Krovak Oblique Conformal Conic (JTSKc)
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 2 ] ) ;
            proj_param.Azimuth             := DegToRad( _params[ 3 ] ) ;
            proj_param.FalseEasting        := _params[ 4 ] ;
            proj_param.FalseNorthing       := _params[ 5 ] ;
          end;
      33 : begin // Equidistant Cylindrical
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.StandardParallel_1  := DegToRad( _params[ 1 ] ) ;
            proj_param.FalseEasting        := _params[ 2 ] ;
            proj_param.FalseNorthing       := _params[ 3 ] ;
          end;
      34 : begin // Extended Transverse Mercator
            proj_param.CentralMeridian     := DegToRad( _params[ 0 ] ) ;
            proj_param.LatitudeOfOrigin    := DegToRad( _params[ 1 ] ) ;
            proj_param.ScaleFactor         := _params[ 2 ] ;
            proj_param.FalseEasting        := _params[ 3 ] ;
            proj_param.FalseNorthing       := _params[ 4 ] ;
          end;

    end;

    if ( _projId <> 1 ) and ( _projId <> 0 ) then begin
      gcs_map := CSMapinfoDatumsMap.ByWKT( IntToStr( _datumId ) ) ;
      if not assigned( gcs_map ) then
        gcs_map := prepareCustomGcs ;

      unit_map := CSMapinfoUnits2Map.ByWKT( _unitsId ) ;
      if not assigned( unit_map ) then
        unit_map := CSMapinfoUnitsMap.ByWKT( _unitsId ) ;

      proj_map := CSMapinfoProjectionsMap.ByWKT( IntToStr( _projId ) ) ;

      if assigned( gcs_map  ) and
         assigned( unit_map ) and
         assigned( proj_map )
      then begin
        unit_obj := CSUnitsList.ByEPSG( unit_map.EPSG ) ;

        if assigned( unit_obj ) then begin
          proj_param.FalseEasting  := unit_obj.ToBase(
                                        proj_param.FalseEasting
                                      ) ;
          proj_param.FalseNorthing := unit_obj.ToBase(
                                        proj_param.FalseNorthing
                                      ) ;
        end ;

        Result := CSProjectedCoordinateSystemList.Prepare(
                   -1,
                   Format( 'MapInfo_%s_%s',
                           [ CSProjList.ByEPSG( proj_map.EPSG ).WKT,
                             CSGeographicCoordinateSystemList.ByEPSG(
                               gcs_map.EPSG
                             ).WKT
                           ]
                       ),
                  gcs_map.EPSG,
                  unit_map.EPSG,
                  proj_map.EPSG,
                  proj_param
                ) ;
      end
      else
        Result := CSUnknownCoordinateSystem ;
    end;
  end ;

//------------------------------------------------------------------------------
// Tests
//------------------------------------------------------------------------------

  {$IFNDEF OXYGENE}
    {$IFDEF DEBUG1}
      procedure check_structure ;
      var
        i : Integer ;
        o : TGIS_CSAbstract ;
      begin
        for i:=0 to CSMapinfoProjectionsMap.Count -1 do begin
          o := CSMapinfoProjectionsMap[i] ;
          if o.EPSG = 0 then continue ;
          assert( assigned( CSProjList.ByEPSG(o.EPSG) ),
                  Format( 'ProjectionMap WKT=%s referring to non ' +
                          'existing projection EPSG=%d.',
                          [ o.WKT, o.EPSG ]
                        )
                ) ;
        end ;

        for i:=0 to CSMapinfoDatumsMap.Count -1 do begin
          o := CSMapinfoDatumsMap[i] ;
          if o.EPSG = 0 then continue ;
          assert( assigned( CSGeographicCoordinateSystemList.ByEPSG(o.EPSG) ),
                  Format( 'DatumsMap WKT=%s referring to non ' +
                          'existing GEOGCS EPSG=%d.',
                          [ o.WKT, o.EPSG ]
                        )
                ) ;
        end ;

        for i:=0 to CSMapinfoUnitsMap.Count -1 do begin
          o := CSMapinfoUnitsMap[i] ;
          if o.EPSG = 0 then continue ;
          assert( assigned( CSUnitsList.ByEPSG(o.EPSG) ),
                  Format( 'UnitsMap WKT=%s referring to non ' +
                          'existing Unit EPSG=%d.',
                          [ o.WKT, o.EPSG ]
                        )
                ) ;
        end ;

        for i:=0 to CSMapinfoEllipsoidMap.Count -1 do begin
          o := CSMapinfoEllipsoidMap[i] ;
          if o.EPSG = 0 then continue ;
          assert( assigned( CSEllipsoidList.ByEPSG(o.EPSG) ),
                  Format( 'EllipsoidMap WKT=%s referring to non ' +
                          'existing Unit EPSG=%d.',
                          [ o.WKT, o.EPSG ]
                        )
                ) ;
        end ;
      end ;
    {$ENDIF}
  {$ENDIF}

//------------------------------------------------------------------------------
// Initialization
//------------------------------------------------------------------------------

{$IFDEF DCC}
  initialization
    {$IFDEF DEBUG1}
       {$IFNDEF NEXTGEN}
         check_structure ;
       {$ENDIF}
       {$MESSAGE WARN 'Verify why it crash on IOS' }
    {$ENDIF}

  finalization
    FreeObject( cs_MapinfoProjectionsMap  ) ;
    FreeObject( cs_MapinfoDatumsMap       ) ;
    FreeObject( cs_MapinfoUnitsMap        ) ;
    FreeObject( cs_MapinfoUnits2Map       ) ;
    FreeObject( cs_MapinfoEllipsoidMap    ) ;
{$ENDIF}

{==================================== END =====================================}
end.

