unit Lider.CG.Com.CoordinatSystemInt;

interface

uses
  Lider.CG.Com.GeoTypes;

(*type

  IlicgCoordinatSystem = interface
    ['{9C0EC3F9-02CB-4785-96BA-5DEC173C4B93}']
    function isLatLongCS(_epsg: integer): boolean; stdcall;
    function isInvalidProjection: boolean; stdcall;
    function GetEpsg: integer; stdcall;
    property Epsg: integer read GetEpsg;
    function GetWKT: WideString; stdcall;
    property WKT: WideString read GetWKT;
    function GetDescription: WideString; stdcall;
    property Description: WideString read GetDescription;
    function getFullWKT: WideString; stdcall;
    property FullWKT: WideString read getFullWKT;
    function getPrettyWKT: WideString; stdcall;
    property PrettyWKT: WideString read getPrettyWKT;
    function GetCSCoordinateSystem: TObject; stdcall;
    procedure SetCSCoordinateSystem(value: TObject); stdcall;
    property CSCoordinateSystem: TObject read GetCSCoordinateSystem write
      SetCSCoordinateSystem;
  end;

  TOntheFlyProjectionInfo = record
    Projection: IlicgCoordinatSystem;
    Enable: Boolean;
  end;  *)

//procedure SetInitProjection (p: IlicgCoordinatSystem);

//function isInvalidProjection(p: IlicgCoordinatSystem): boolean;

//function isLatLongProjection(P: integer): boolean;

implementation

uses
  Lider.CG.Com.ReferenceInt;

{
function isLatLongProjection(P: integer): boolean;
begin
  result := (p=ord(Gdal_ProjSystem_GEO));
end;
}

(*
function isInvalidProjection(p: IlicgCoordinatSystem): boolean;
begin
  result := true;

  if Assigned(p) then
  begin
    Result := p.isInvalidProjection;
  end;

//  Result := (p.ProjSystem  = -1) or
  //  (p.ProjDatum  = ord(Gdal_ProjDatum_UNKNOWN)) ;
end;
    *)
{
procedure SetInitProjection (p: IlicgCoordinatSystem);
begin
  if Assigned(p) then
    IlicgSpatialReference(p).init;
end;
}

(*

uses
  Windows;


type
   Gdal_ProjDatum = (
          Gdal_ProjDatum_Abidjan_1987,
          Gdal_ProjDatum_Accra,
          Gdal_ProjDatum_Adindan,
          Gdal_ProjDatum_Afgooye,
          Gdal_ProjDatum_Agadez,
          Gdal_ProjDatum_Ain_el_Abd_1970,
          Gdal_ProjDatum_Albanian_1987,
          Gdal_ProjDatum_American_Samoa_1962,
          Gdal_ProjDatum_Amersfoort,
          Gdal_ProjDatum_Ammassalik_1958,
          Gdal_ProjDatum_Ancienne_Triangulation_Francaise__Paris_,
          Gdal_ProjDatum_Anguilla_1957,
          Gdal_ProjDatum_Antigua_1943,
          Gdal_ProjDatum_Aratu,
          Gdal_ProjDatum_Arc_1950,
          Gdal_ProjDatum_Arc_1960,
          Gdal_ProjDatum_Australian_Antarctic_Datum_1998,
          Gdal_ProjDatum_Australian_Geodetic_Datum_1966,
          Gdal_ProjDatum_Australian_Geodetic_Datum_1984,
          Gdal_ProjDatum_Average_Terrestrial_System_1977,
          Gdal_ProjDatum_Azores_Central_Islands_1948,
          Gdal_ProjDatum_Azores_Occidental_Islands_1939,
          Gdal_ProjDatum_Azores_Oriental_Islands_1940,
          Gdal_ProjDatum_Barbados_1938,
          Gdal_ProjDatum_Batavia,
          Gdal_ProjDatum_Batavia__Jakarta_,
          Gdal_ProjDatum_Beduaram,
          Gdal_ProjDatum_Beijing_1954,
          Gdal_ProjDatum_Bermuda_1957,
          Gdal_ProjDatum_Bern_1938,
          Gdal_ProjDatum_Bissau,
          Gdal_ProjDatum_Bogota_1975,
          Gdal_ProjDatum_Bogota_1975__Bogota_,
          Gdal_ProjDatum_Bukit_Rimpah,
          Gdal_ProjDatum_CH1903,
          Gdal_ProjDatum_CH1903__Bern_,
          Gdal_ProjDatum_CH1903_,
          Gdal_ProjDatum_Camacupa,
          Gdal_ProjDatum_Campo_Inchauspe,
          Gdal_ProjDatum_Cape,
          Gdal_ProjDatum_Carthage,
          Gdal_ProjDatum_Carthage__Paris_,
          Gdal_ProjDatum_Chos_Malal_1914,
          Gdal_ProjDatum_Chua,
          Gdal_ProjDatum_Conakry_1905,
          Gdal_ProjDatum_Congo_1960_Pointe_Noire,
          Gdal_ProjDatum_Corrego_Alegre,
          Gdal_ProjDatum_Cote_d_Ivoire,
          Gdal_ProjDatum_Dabola_1981,
          Gdal_ProjDatum_Datum_73,
          Gdal_ProjDatum_Dealul_Piscului_1933,
          Gdal_ProjDatum_Dealul_Piscului_1970,
          Gdal_ProjDatum_Deir_ez_Zor,
          Gdal_ProjDatum_Deutsches_Hauptdreiecksnetz,
          Gdal_ProjDatum_Dominica_1945,
          Gdal_ProjDatum_Douala,
          Gdal_ProjDatum_Douala_1948,
          Gdal_ProjDatum_Egypt_1907,
          Gdal_ProjDatum_Egypt_1930,
          Gdal_ProjDatum_Estonia_1992,
          Gdal_ProjDatum_Estonia_1997,
          Gdal_ProjDatum_European_Datum_1950,
          Gdal_ProjDatum_European_Datum_1950_1977_,
          Gdal_ProjDatum_European_Datum_1987,
          Gdal_ProjDatum_European_Libyan_Datum_1979,
          Gdal_ProjDatum_European_Terrestrial_Reference_System_1989,
          Gdal_ProjDatum_Fahud,
          Gdal_ProjDatum_Final_Datum_1958,
          Gdal_ProjDatum_Gandajika_1970,
          Gdal_ProjDatum_Garoua,
          Gdal_ProjDatum_Garoua_,
          Gdal_ProjDatum_Geocentric_Datum_of_Australia_1994,
          Gdal_ProjDatum_Greek,
          Gdal_ProjDatum_Greek__Athens_,
          Gdal_ProjDatum_Greek_Geodetic_Reference_System_1987,
          Gdal_ProjDatum_Grenada_1953,
          Gdal_ProjDatum_Gunung_Segara,
          Gdal_ProjDatum_Gunung_Segara__Jakarta_,
          Gdal_ProjDatum_Guyane_Francaise,
          Gdal_ProjDatum_Hanoi_1972,
          Gdal_ProjDatum_Hartebeesthoek94,
          Gdal_ProjDatum_Herat_North,
          Gdal_ProjDatum_Hito_XVIII_1963,
          Gdal_ProjDatum_Hong_Kong_1980,
          Gdal_ProjDatum_Hu_Tzu_Shan,
          Gdal_ProjDatum_Hungarian_Datum_1972,
          Gdal_ProjDatum_IRENET95,
          Gdal_ProjDatum_Indian_1954,
          Gdal_ProjDatum_Indian_1960,
          Gdal_ProjDatum_Indian_1975,
          Gdal_ProjDatum_Indonesian_Datum_1974,
          Gdal_ProjDatum_Israel,
          Gdal_ProjDatum_Jamaica_1875,
          Gdal_ProjDatum_Jamaica_1969,
          Gdal_ProjDatum_Japanese_Geodetic_Datum_2000,
          Gdal_ProjDatum_Jednotne_Trigonometricke_Site_Katastralni,
          Gdal_ProjDatum_Kalianpur_1880,
          Gdal_ProjDatum_Kalianpur_1937,
          Gdal_ProjDatum_Kalianpur_1962,
          Gdal_ProjDatum_Kalianpur_1975,
          Gdal_ProjDatum_Kandawala,
          Gdal_ProjDatum_Kartasto_Koordinaati_Jarjestelma_1966,
          Gdal_ProjDatum_Kertau,
          Gdal_ProjDatum_Korean_Datum_1985,
          Gdal_ProjDatum_Korean_Datum_1995,
          Gdal_ProjDatum_Kousseri,
          Gdal_ProjDatum_Kuwait_Oil_Company,
          Gdal_ProjDatum_Kuwait_Utility,
          Gdal_ProjDatum_La_Canoa,
          Gdal_ProjDatum_Lake,
          Gdal_ProjDatum_Leigon,
          Gdal_ProjDatum_Liberia_1964,
          Gdal_ProjDatum_Lisbon_1890__Lisbon_,
          Gdal_ProjDatum_Lisbon_1937,
          Gdal_ProjDatum_Lisbon_1937__Lisbon_,
          Gdal_ProjDatum_Lithuania_1994__ETRS89_,
          Gdal_ProjDatum_Locodjo_1965,
          Gdal_ProjDatum_Loma_Quintana,
          Gdal_ProjDatum_Lome,
          Gdal_ProjDatum_Luxembourg_1930,
          Gdal_ProjDatum_Luzon_1911,
          Gdal_ProjDatum_M_poraloko,
          Gdal_ProjDatum_Madeira_1936,
          Gdal_ProjDatum_Madrid_1870__Madrid_,
          Gdal_ProjDatum_Madzansua,
          Gdal_ProjDatum_Mahe_1971,
          Gdal_ProjDatum_Makassar,
          Gdal_ProjDatum_Makassar__Jakarta_,
          Gdal_ProjDatum_Malongo_1987,
          Gdal_ProjDatum_Manoca,
          Gdal_ProjDatum_Manoca_1962,
          Gdal_ProjDatum_Massawa,
          Gdal_ProjDatum_Merchich,
          Gdal_ProjDatum_Mhast,
          Gdal_ProjDatum_Militar_Geographische_Institut,
          Gdal_ProjDatum_Militar_Geographische_Institut__Ferro_,
          Gdal_ProjDatum_Minna,
          Gdal_ProjDatum_Monte_Mario,
          Gdal_ProjDatum_Monte_Mario__Rome_,
          Gdal_ProjDatum_Montserrat_1958,
          Gdal_ProjDatum_Mount_Dillon,
          Gdal_ProjDatum_Moznet__ITRF94_,
          Gdal_ProjDatum_NAD_Michigan,
          Gdal_ProjDatum_NAD83__High_Accuracy_Regional_Network_,
          Gdal_ProjDatum_NAD83_Canadian_Spatial_Reference_System,
          Gdal_ProjDatum_NGO_1948,
          Gdal_ProjDatum_NGO_1948__Oslo_,
          Gdal_ProjDatum_NSWC_9Z_2,
          Gdal_ProjDatum_Nahrwan_1967,
          Gdal_ProjDatum_Naparima_1955,
          Gdal_ProjDatum_Naparima_1972,
          Gdal_ProjDatum_National_Geodetic_Network,
          Gdal_ProjDatum_New_Zealand_Geodetic_Datum_1949,
          Gdal_ProjDatum_New_Zealand_Geodetic_Datum_2000,
          Gdal_ProjDatum_Nord_Sahara_1959,
          Gdal_ProjDatum_Nord_Sahara_1959__Paris_,
          Gdal_ProjDatum_Nord_de_Guerre__Paris_,
          Gdal_ProjDatum_North_American_Datum_1927,
          Gdal_ProjDatum_North_American_Datum_1927__1976_,
          Gdal_ProjDatum_North_American_Datum_1927__CGQ77_,
          Gdal_ProjDatum_North_American_Datum_1983,
          Gdal_ProjDatum_Nouvelle_Triangulation_Francaise,
          Gdal_ProjDatum_Nouvelle_Triangulation_Francaise__Paris_,
          Gdal_ProjDatum_OS__SN__1980,
          Gdal_ProjDatum_OSGB_1936,
          Gdal_ProjDatum_OSGB_1970__SN_,
          Gdal_ProjDatum_OSNI_1952,
          Gdal_ProjDatum_Observatario,
          Gdal_ProjDatum_Old_Hawaiian,
          Gdal_ProjDatum_PDO_Survey_Datum_1993,
          Gdal_ProjDatum_Padang_1884,
          Gdal_ProjDatum_Padang_1884__Jakarta_,
          Gdal_ProjDatum_Palestine_1923,
          Gdal_ProjDatum_Pampa_del_Castillo,
          Gdal_ProjDatum_Porto_Santo,
          Gdal_ProjDatum_Posiciones_Geodesicas_Argentinas,
          Gdal_ProjDatum_Posiciones_Geodesicas_Argentinas_1998,
          Gdal_ProjDatum_Provisional_South_American_Datum_1956,
          Gdal_ProjDatum_Puerto_Rico,
          Gdal_ProjDatum_Pulkovo_1942,
          Gdal_ProjDatum_Pulkovo_1942_58,
          Gdal_ProjDatum_Pulkovo_1942_83,
          Gdal_ProjDatum_Pulkovo_1995,
          Gdal_ProjDatum_Qatar_1948,
          Gdal_ProjDatum_Qatar_1974,
          Gdal_ProjDatum_Qatar_National_Datum_1995,
          Gdal_ProjDatum_Qornoq,
          Gdal_ProjDatum_Qornoq_1927,
          Gdal_ProjDatum_Rassadiran,
          Gdal_ProjDatum_Red_Geodesica_Venezolana,
          Gdal_ProjDatum_Reseau_Geodesique_Francais_1993,
          Gdal_ProjDatum_Reseau_National_Licadge_1950,
          Gdal_ProjDatum_Reseau_National_Licadge_1950__Brussels_,
          Gdal_ProjDatum_Reseau_National_Licadge_1972,
          Gdal_ProjDatum_Rikets_koordinatsystem_1990,
          Gdal_ProjDatum_S_JTSK__Ferro_,
          Gdal_ProjDatum_Samboja,
          Gdal_ProjDatum_Sapper_Hill_1943,
          Gdal_ProjDatum_Schwarzeck,
          Gdal_ProjDatum_Scoresbysund_1952,
          Gdal_ProjDatum_Segora,
          Gdal_ProjDatum_Selvagem_Grande,
          Gdal_ProjDatum_Serindung,
          Gdal_ProjDatum_Sierra_Leone_1968,
          Gdal_ProjDatum_Sierra_Leone_Colony_1924,
          Gdal_ProjDatum_Sistema_de_Referencia_Geocentrico_para_America_del_Sur,
          Gdal_ProjDatum_South_American_Datum_1969,
          Gdal_ProjDatum_South_Yemen,
          Gdal_ProjDatum_St_George_Island,
          Gdal_ProjDatum_St_Kitts_1955,
          Gdal_ProjDatum_St_Lawrence_Island,
          Gdal_ProjDatum_St_Lucia_1955,
          Gdal_ProjDatum_St_Paul_Island,
          Gdal_ProjDatum_St_Vincent_1945,
          Gdal_ProjDatum_Stockholm_1938,
          Gdal_ProjDatum_Stockholm_1938__Stockholm_,
          Gdal_ProjDatum_Sudan,
          Gdal_ProjDatum_Swiss_Terrestrial_Reference_Frame_1995,
          Gdal_ProjDatum_TM65,
          Gdal_ProjDatum_TM75,
          Gdal_ProjDatum_Tananarive_1925,
          Gdal_ProjDatum_Tananarive_1925__Paris_,
          Gdal_ProjDatum_Tete,
          Gdal_ProjDatum_Timbalai_1948,
          Gdal_ProjDatum_Tokyo,
          Gdal_ProjDatum_Trinidad_1903,
          Gdal_ProjDatum_Trucial_Coast_1948,
          Gdal_ProjDatum_Voirol_1875,
          Gdal_ProjDatum_Voirol_1875__Paris_,
          Gdal_ProjDatum_WGS_72_Transit_Broadcast_Ephemeris,
          Gdal_ProjDatum_World_Geodetic_System_1972,
          Gdal_ProjDatum_World_Geodetic_System_1984,
          Gdal_ProjDatum_Xian_1980,
          Gdal_ProjDatum_Yacare,
          Gdal_ProjDatum_Yemen_National_Geodetic_Network_1996,
          Gdal_ProjDatum_Yoff,

          Gdal_ProjDatum_UNKNOWN = -1
          );

type

  Gdal_ProjSystem = (
	Gdal_ProjSystem_ALBERS    = 0,
	Gdal_ProjSystem_GEO       = 1,
	Gdal_ProjSystem_HOM       = 2,	// Hotine Oblique Mercator
	Gdal_ProjSystem_KROVAK    = 3,
	Gdal_ProjSystem_LAEA      = 4,	// Lambert Azimuthal Equal-Area
	Gdal_ProjSystem_LCC       = 5,	// Lambert Conformal Conic
	Gdal_ProjSystem_LCC1SP    = 6,	// Lambert Conformal Conic with 1 Standard Parallel
	Gdal_ProjSystem_NZMG      = 7,
	Gdal_ProjSystem_OS        = 8,
	Gdal_ProjSystem_PS        = 9,
	Gdal_ProjSystem_SINUS     = 10,
	Gdal_ProjSystem_STEREO    = 11,
	Gdal_ProjSystem_MERC      = 12,
	Gdal_ProjSystem_TM        = 13,
	Gdal_ProjSystem_UTM6      = 14,

	Gdal_ProjSystem_UTM3      = 35,

        Gdal_ProjSystem_UNKNOWN   = -1,

  EPSG_ProjSystem_2319 = 2319,    // # ED50 / TM27
  EPSG_ProjSystem_2320 = 2320,    // # ED50 / TM30
  EPSG_ProjSystem_2321 = 2321,    // # ED50 / TM33
  EPSG_ProjSystem_2322 = 2322,    // # ED50 / TM36
  EPSG_ProjSystem_2323 = 2323,    // # ED50 / TM39
  EPSG_ProjSystem_2324 = 2324,    // # ED50 / TM42
  EPSG_ProjSystem_2325 = 2325,    // # ED50 / TM45

  EPSG_ProjSystem_7834 = 7834,    // # ITRF96 / TM27
  EPSG_ProjSystem_7833 = 7833,    // # ITRF96 / TM30
  EPSG_ProjSystem_7836 = 7836,    // # ITRF96 / TM33
  EPSG_ProjSystem_7837 = 7837,    // # ITRF96 / TM36
  EPSG_ProjSystem_7838 = 7838,    // # ITRF96 / TM39
  EPSG_ProjSystem_7839 = 7839,    // # ITRF96 / TM42
  EPSG_ProjSystem_7840 = 7840    // # ITRF96 / TM45

        );

type
  PROJATTR  = integer;

const
    FIRST_STANDARD_PARALLEL       :  PROJATTR = 0;
    SECOND_STANDARD_PARALLEL      :  PROJATTR = 1;
    CENTRAL_LATITUDE              :  PROJATTR = 2;
    CENTRAL_LONGITUDE             :  PROJATTR = 3;
    FALSE_EASTING                 :  PROJATTR = 4;
    FALSE_NORTHING                :  PROJATTR = 5;
    SPHERE_RADIUS                 :  PROJATTR = 6;
    ORIGIN_LATITUDE               :  PROJATTR = 7;
    SEMI_MAJOR_AXIS               :  PROJATTR = 8;
    SEMI_MINOR_AXIS               :  PROJATTR = 9;
    CENTRAL_MERIDIAN              :  PROJATTR = 10;
    STANDARD_PARALLEL             :  PROJATTR = 11;
    TRUE_SCALE_LATITUDE           :  PROJATTR = 12;
    AZIMUTH_ANGLE                 :  PROJATTR = 13;
    AZIMUTH_POINT                 :  PROJATTR = 14;
    LATITUDE1                     :  PROJATTR = 15;
    LATITUDE2                     :  PROJATTR = 16;
    LONGITUDE1                    :  PROJATTR = 17;
    LONGITUDE2                    :  PROJATTR = 18;
    PROJ_CENTER_SCALE_FACTOR      :  PROJATTR = 19;
    LONGITUDE_POLE                :  PROJATTR = 20;
    CENTRAL_MERIDIAN_SCALE_FACTOR :  PROJATTR = 21;
    PERSPECTIVE_POINT_HEIGHT      :  PROJATTR = 22;
    INCLINATION_ANGLE             :  PROJATTR = 23;
    ASCENTION_LONGITUDE           :  PROJATTR = 24;
    SAT_REV_PERIOD                :  PROJATTR = 25;
    END_OF_PATH_FLAG              :  PROJATTR = 26;
    LANDSAT_SAT_NUM               :  PROJATTR = 27;
    OEA_SHAPE_PARM_M              :  PROJATTR = 28;
    OEA_SHAPE_PARM_N              :  PROJATTR = 29;
    OEA_OVAL_ROTATION_ANGLE       :  PROJATTR = 30;
    ZONE                          :  PROJATTR = 31;

    RECTANGLE_TO_SKEW             :  PROJATTR = 32;


    NUM_PROJ_ATTR_TYPES           :  PROJATTR = 33;
    Gdal_MAX_ATTR_PER_PROJ        :  PROJATTR = 16;  // maximum attributes a single projection might have

type
 Gdal_unit = integer;

const
    Gdal__UnitMETERS              : Gdal_unit = 0;
    Gdal__UnitARC_DEGREES         : Gdal_unit = 1;
    Gdal__UnitFEET_INT            : Gdal_unit = 2; //"Feet (U.S. Survey)")-
    Gdal__UnitLU_FEET_US          : Gdal_unit = 3; // "Feet (International)" -

    Gdal__UnitCount               = 4;


type
   Gdal_ProjAttrValue = record
    mAttr            : PROJATTR;
    mValue           : double;
   end {GM_ProjAttrValue_t};

   PGdal_PROJATTRVALUE = array [1..16] of  Gdal_PROJATTRVALUE;     //MAX_ATTR_PER_PROJ

   PIlicgCoordinatSystem = ^IlicgCoordinatSystem;
   IlicgCoordinatSystem  = record
      ProjSystem   : integer;
      ProjDatum    : integer;
      Proj Unit     : Gdal_unit;
      ProjAttrbCnt : integer;
      ProjAttrList : PGdal_PROJATTRVALUE;
   end;


   TOntheFlyProjectionInfo = record
     Projection : IlicgCoordinatSystem;
     Enable     : Boolean;
   end;

type
  Gdal_ProjectionTypeFromDialog =  function (const In_PT : PIlicgCoordinatSystem ; Out_PT : PIlicgCoordinatSystem) : Boolean; stdcall;


procedure SetInitProjection (var p: IlicgCoordinatSystem);

function isInvalidProjection (p: IlicgCoordinatSystem) : boolean ;

function isLatLongProjection(P: integer): boolean;

function GetProj4Parameter (epsg: integer) : WideString;

implementation



function isLatLongProjection(P: integer): boolean;
begin
  result := (p=ord(Gdal_ProjSystem_GEO));
end;

function isInvalidProjection (p: IlicgCoordinatSystem) : boolean ;
begin
  Result := (p.ProjSystem  = -1) or
    (p.ProjDatum  = ord(Gdal_ProjDatum_UNKNOWN)) ;
end;

procedure SetInitProjection (var p: IlicgCoordinatSystem);
begin
  fillChar (p, sizeof(IlicgCoordinatSystem), #0);
  p.ProjSystem  := -1;
  p.ProjDatum   := ord(Gdal_ProjDatum_UNKNOWN);
  p.Proj Unit    := Gdal__UnitMETERS;
  p.ProjAttrbCnt:= 0;
end;

function GetProj4Parameter (epsg: integer) : WideString;
begin
   Result := '';
   case epsg of

      4326 : Result := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';

      2319 : Result := '+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2320 : Result := '+proj=tmerc +lat_0=0 +lon_0=30 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2321 : Result := '+proj=tmerc +lat_0=0 +lon_0=33 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2322 : Result := '+proj=tmerc +lat_0=0 +lon_0=36 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2323 : Result := '+proj=tmerc +lat_0=0 +lon_0=39 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2324 : Result := '+proj=tmerc +lat_0=0 +lon_0=42 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';
      2325 : Result := '+proj=tmerc +lat_0=0 +lon_0=45 +k=1 +x_0=500000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +Units=m +no_defs ';

      7833 : Result := '+proj=tmerc +lat_0=0 +lon_0=30 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7834 : Result := '+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7836 : Result := '+proj=tmerc +lat_0=0 +lon_0=33 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7837 : Result := '+proj=tmerc +lat_0=0 +lon_0=36 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7838 : Result := '+proj=tmerc +lat_0=0 +lon_0=39 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7839 : Result := '+proj=tmerc +lat_0=0 +lon_0=42 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';
      7840 : Result := '+proj=tmerc +lat_0=0 +lon_0=45 +k=1 +x_0=500000 +y_0=0 +ellps=GRS80 +Units=m +no_defs';



   end;
end;
*)

end.


