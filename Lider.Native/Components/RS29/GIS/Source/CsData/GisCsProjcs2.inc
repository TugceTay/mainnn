//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  List of Projected Coordinate Systems.
  Extra ESRI compatible definitions.
  Generated automatically.
}


procedure Init_CSProjectedCoordinateSystemList2(
  const _cs : TGIS_CSProjectedCoordinateSystemList ;
  const _pp : TGIS_CSProjParameters
) ;

function p(
  _central_meridian           : Double  ;
  _latitude_of_origin         : Double  ;
  _false_easting              : Double  ;
  _false_northing             : Double  ;
  _standard_parallel_1        : Double  ;
  _standard_parallel_2        : Double  ;
  _pseudo_standard_parallel_1 : Double  ;
  _zone                       : Integer ;
  _scale_factor               : Double  ;
  _longitude_of_center        : Double  ;
  _latitude_of_center         : Double  ;
  _azimuth                    : Double  ;
  _longitude_of_point_1       : Double  ;
  _latitude_of_point_1        : Double  ;
  _longitude_of_point_2       : Double  ;
  _latitude_of_point_2        : Double  ;
  _x_scale                    : Double  ;
  _y_scale                    : Double  ;
  _xy_plane_rotation          : Double
) : TGIS_CSProjParameters ;
begin
  Result := _pp ;
  Result.CentralMeridian          := DegToRad( _central_meridian           );
  Result.LatitudeOfOrigin         := DegToRad( _latitude_of_origin         );
  Result.FalseEasting             :=           _false_easting               ;
  Result.FalseNorthing            :=           _false_northing              ;
  Result.StandardParallel_1       := DegToRad( _standard_parallel_1        );
  Result.StandardParallel_2       := DegToRad( _standard_parallel_2        );
  Result.PseudoStandardParallel_1 := DegToRad( _pseudo_standard_parallel_1 );
  Result.Zone                     :=           _zone                        ;
  Result.ScaleFactor              :=           _scale_factor                ;
  Result.LongitudeOfCenter        := DegToRad( _longitude_of_center        );
  Result.LatitudeOfCenter         := DegToRad( _latitude_of_center         );
  Result.Azimuth                  := DegToRad( _azimuth                    );
  Result.LongitudeOfPoint_1       := DegToRad( _longitude_of_point_1       );
  Result.LatitudeOfPoint_1        := DegToRad( _latitude_of_point_1        );
  Result.LongitudeOfPoint_2       := DegToRad( _longitude_of_point_2       );
  Result.LatitudeOfPoint_2        := DegToRad( _latitude_of_point_2        );
  Result.XScale                   :=           _x_scale                     ;
  Result.YScale                   :=           _y_scale                     ;
  Result.XYPlaneRotation          := DegToRad( _xy_plane_rotation          );
end ;
procedure prepare_1;
begin
  _cs.Add     (  53001, 'Sphere_Plate_Carree',
                         4035, 9001, 9823,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53002, 'Sphere_Equidistant_Cylindrical',
                         4035, 9001, 9823,
                         p(0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53003, 'Sphere_Miller_Cylindrical',
                         4035, 9001, 810034,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53004, 'Sphere_Mercator',
                         4035, 9001, 9805,
                         p(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53008, 'Sphere_Sinusoidal',
                         4035, 9001, 810025,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53009, 'Sphere_Mollweide',
                         4035, 9001, 810040,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53010, 'Sphere_Eckert_VI',
                         4035, 9001, 810026,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53011, 'Sphere_Eckert_V',
                         4035, 9001, 810022,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53012, 'Sphere_Eckert_IV',
                         4035, 9001, 810021,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53013, 'Sphere_Eckert_III',
                         4035, 9001, 810017,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53014, 'Sphere_Eckert_II',
                         4035, 9001, 810016,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53015, 'Sphere_Eckert_I',
                         4035, 9001, 810015,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53016, 'Sphere_Gall_Stereographic',
                         4035, 9001, 810088,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53017, 'Sphere_Behrmann',
                         4035, 9001, 810013,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53018, 'Sphere_Winkel_I',
                         4035, 9001, 810072,
                         p(0,0,0,0,50.4597762521898,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53021, 'Sphere_Polyconic',
                         4035, 9001, 9818,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53022, 'Sphere_Quartic_Authalic',
                         4035, 9001, 810107,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53023, 'Sphere_Loximuthal',
                         4035, 9001, 810032,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53024, 'Sphere_Bonne',
                         4035, 9001, 9827,
                         p(0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53026, 'Sphere_Stereographic',
                         4035, 9001, 810054,
                         p(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53027, 'Sphere_Equidistant_Conic',
                         4035, 9001, 810024,
                         p(0,0,0,0,60,60,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53028, 'Sphere_Cassini',
                         4035, 9001, 9806,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  53030, 'Sphere_Robinson',
                         4035, 9001, 810060,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias(  54001, 'World_Plate_Carree',
                         4087
              );
  _cs.Add     (  54002, 'World_Equidistant_Cylindrical',
                         4326, 9001, 9823,
                         p(0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54003, 'World_Miller_Cylindrical',
                         4326, 9001, 810034,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias(  54004, 'World_Mercator',
                         3395
              );
  _cs.Add     (  54008, 'World_Sinusoidal',
                         4326, 9001, 810025,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54009, 'World_Mollweide',
                         4326, 9001, 810040,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54010, 'World_Eckert_VI',
                         4326, 9001, 810026,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54011, 'World_Eckert_V',
                         4326, 9001, 810022,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54012, 'World_Eckert_IV',
                         4326, 9001, 810021,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54013, 'World_Eckert_III',
                         4326, 9001, 810017,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54014, 'World_Eckert_II',
                         4326, 9001, 810016,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54015, 'World_Eckert_I',
                         4326, 9001, 810015,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54016, 'World_Gall_Stereographic',
                         4326, 9001, 810088,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54017, 'World_Behrmann',
                         4326, 9001, 810013,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54018, 'World_Winkel_I',
                         4326, 9001, 810072,
                         p(0,0,0,0,50.4597762521898,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54021, 'World_Polyconic',
                         4326, 9001, 9818,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54022, 'World_Quartic_Authalic',
                         4326, 9001, 810107,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54023, 'World_Loximuthal',
                         4326, 9001, 810032,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54024, 'World_Bonne',
                         4326, 9001, 9827,
                         p(0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54026, 'World_Stereographic',
                         4326, 9001, 810054,
                         p(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54027, 'World_Equidistant_Conic',
                         4326, 9001, 810024,
                         p(0,0,0,0,60,60,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54028, 'World_Cassini',
                         4326, 9001, 9806,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  54030, 'World_Robinson',
                         4326, 9001, 810060,
                         p(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  65061, 'NAD_1927_StatePlane_Guam_FIPS_5400',
                         4267, 9003, 9818,
                         p(-144.748750705556,13.4724663527778,50000,50000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     (  65161, 'NAD_1983_StatePlane_Guam_FIPS_5400',
                         4269, 9001, 9818,
                         p(-144.748750705556,13.4724663527778,50000,50000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102001, 'Canada_Albers_Equal_Area_Conic',
                         4269, 9001, 9822,
                         p(-96,40,0,0,50,70,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102002, 'Canada_Lambert_Conformal_Conic',
                         4269, 9001, 9802,
                         p(-96,40,0,0,50,70,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102003, 'USA_Contiguous_Albers_Equal_Area_Conic',
                         4269, 9001, 9822,
                         p(-96,37.5,0,0,29.5,45.5,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102004, 'USA_Contiguous_Lambert_Conformal_Conic',
                         4269, 9001, 9802,
                         p(-96,39,0,0,33,45,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102005, 'USA_Contiguous_Equidistant_Conic',
                         4269, 9001, 810024,
                         p(-96,39,0,0,33,45,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102006, 'Alaska_Albers_Equal_Area_Conic',
                         3338
              );
  _cs.Add     ( 102007, 'Hawaii_Albers_Equal_Area_Conic',
                         4269, 9001, 9822,
                         p(-157,13,0,0,8,18,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102008, 'North_America_Albers_Equal_Area_Conic',
                         4269, 9001, 9822,
                         p(-96,40,0,0,20,60,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102009, 'North_America_Lambert_Conformal_Conic',
                         4269, 9001, 9802,
                         p(-96,40,0,0,20,60,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102010, 'North_America_Equidistant_Conic',
                         4269, 9001, 810024,
                         p(-96,40,0,0,20,60,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102011, 'Africa_Sinusoidal',
                         4326, 9001, 810025,
                         p(15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102012, 'Asia_Lambert_Conformal_Conic',
                         4326, 9001, 9802,
                         p(105,0,0,0,30,62,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102013, 'Europe_Albers_Equal_Area_Conic',
                         4230, 9001, 9822,
                         p(10,30,0,0,43,62,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102014, 'Europe_Lambert_Conformal_Conic',
                         4230, 9001, 9802,
                         p(10,30,0,0,43,62,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102015, 'South_America_Lambert_Conformal_Conic',
                         5527, 9001, 9802,
                         p(-60,-32,0,0,-5,-42,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102017, 'North_Pole_Lambert_Azimuthal_Equal_Area',
                         3973
              );
  _cs.Add     ( 102018, 'North_Pole_Stereographic',
                         4326, 9001, 810054,
                         p(0,90,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102020, 'South_Pole_Lambert_Azimuthal_Equal_Area',
                         3974
              );
  _cs.Add     ( 102021, 'South_Pole_Stereographic',
                         4326, 9001, 810054,
                         p(0,-90,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102022, 'Africa_Albers_Equal_Area_Conic',
                         4326, 9001, 9822,
                         p(25,0,0,0,20,-23,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102023, 'Africa_Equidistant_Conic',
                         4326, 9001, 810024,
                         p(25,0,0,0,20,-23,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102024, 'Africa_Lambert_Conformal_Conic',
                         4326, 9001, 9802,
                         p(25,0,0,0,20,-23,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102025, 'Asia_North_Albers_Equal_Area_Conic',
                         4326, 9001, 9822,
                         p(95,30,0,0,15,65,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102026, 'Asia_North_Equidistant_Conic',
                         4326, 9001, 810024,
                         p(95,30,0,0,15,65,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102027, 'Asia_North_Lambert_Conformal_Conic',
                         4326, 9001, 9802,
                         p(95,30,0,0,15,65,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102028, 'Asia_South_Albers_Equal_Area_Conic',
                         4326, 9001, 9822,
                         p(125,-15,0,0,7,-32,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102029, 'Asia_South_Equidistant_Conic',
                         4326, 9001, 810024,
                         p(125,-15,0,0,7,-32,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102030, 'Asia_South_Lambert_Conformal_Conic',
                         4326, 9001, 9802,
                         p(125,-15,0,0,7,-32,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102031, 'Europe_Equidistant_Conic',
                         4230, 9001, 810024,
                         p(10,30,0,0,43,62,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102032, 'South_America_Equidistant_Conic',
                         5527, 9001, 810024,
                         p(-60,-32,0,0,-5,-42,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102033, 'South_America_Albers_Equal_Area_Conic',
                         5527, 9001, 9822,
                         p(-60,-32,0,0,-5,-42,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102065, 'S_JTSK_Krovak',
                         4004, 9001, 9819,
                         p(0,0,0,0,0,0,78.5,0,0.9999,24.8333333333333,49.5,30.2881397527778,0,0,0,0,1,1,0)
              );
  _cs.Add     ( 102067, 'S_JTSK_Krovak_East_North',
                         4004, 9001, 9819,
                         p(0,0,0,0,0,0,78.5,0,0.9999,24.8333333333333,49.5,30.2881397527778,0,0,0,0,-1,1,90)
              );
  _cs.AddAlias( 102091, 'Monte_Mario_Italy_1',
                         3003
              );
  _cs.AddAlias( 102092, 'Monte_Mario_Italy_2',
                         3004
              );
  _cs.Add     ( 102101, 'NGO_1948_Norway_Zone_1',
                         4273, 9001, 9807,
                         p(6.05625,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102102, 'NGO_1948_Norway_Zone_2',
                         4273, 9001, 9807,
                         p(8.38958333333333,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102103, 'NGO_1948_Norway_Zone_3',
                         4273, 9001, 9807,
                         p(10.7229166666667,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102104, 'NGO_1948_Norway_Zone_4',
                         4273, 9001, 9807,
                         p(13.2229166666667,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102105, 'NGO_1948_Norway_Zone_5',
                         4273, 9001, 9807,
                         p(16.8895833333333,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102106, 'NGO_1948_Norway_Zone_6',
                         4273, 9001, 9807,
                         p(20.8895833333333,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102107, 'NGO_1948_Norway_Zone_7',
                         4273, 9001, 9807,
                         p(24.8895833333333,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102108, 'NGO_1948_Norway_Zone_8',
                         4273, 9001, 9807,
                         p(29.05625,58,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102110, 'RGF_1993_Lambert_93',
                         4171, 9001, 9802,
                         p(3,46.5,700000,6600000,44,49,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102114, 'Old_Hawaiian_UTM_Zone_4N',
                         4135, 9001, 9807,
                         p(-159,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102115, 'Old_Hawaiian_UTM_Zone_5N',
                         4135, 9001, 9807,
                         p(-153,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102120, 'NAD_1927_Michigan_GeoRef_Feet_US',
                         4267, 9003, 9815,
                         p(0,0,2546731.49596139,-4354009.81600203,0,0,0,0,0.9996,-86,45.3091666666667,337.255555555556,0,0,0,0,0,0,60)
              );
  _cs.Add     ( 102121, 'NAD_1983_Michigan_GeoRef_Feet_US',
                         4269, 9003, 9815,
                         p(0,0,2546731.49596139,-4354009.81600203,0,0,0,0,0.9996,-86,45.3091666666667,337.255555555556,0,0,0,0,0,0,60)
              );
  _cs.Add     ( 102122, 'NAD_1927_Michigan_GeoRef_Meters',
                         4267, 9001, 9815,
                         p(0,0,2546731.496,-4354009.816,0,0,0,0,0.9996,-86,45.3091666666667,337.255555555556,0,0,0,0,0,0,60)
              );
  _cs.Add     ( 102123, 'NAD_1983_Michigan_GeoRef_Meters',
                         4269, 9001, 9815,
                         p(0,0,2546731.496,-4354009.816,0,0,0,0,0.9996,-86,45.3091666666667,337.255555555556,0,0,0,0,0,0,60)
              );
  _cs.Add     ( 102132, 'NGO_1948_UTM_Zone_32N',
                         4273, 9001, 9807,
                         p(9,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102133, 'NGO_1948_UTM_Zone_33N',
                         4273, 9001, 9807,
                         p(15,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
end;

procedure prepare_2;
begin
  _cs.Add     ( 102134, 'NGO_1948_UTM_Zone_34N',
                         4273, 9001, 9807,
                         p(21,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102135, 'NGO_1948_UTM_Zone_35N',
                         4273, 9001, 9807,
                         p(27,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102140, 'Hong_Kong_1980_Grid',
                         2326
              );
  _cs.Add     ( 102141, 'Hong_Kong_1980_UTM_Zone_49N',
                         4611, 9001, 9807,
                         p(111,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102142, 'Hong_Kong_1980_UTM_Zone_50N',
                         4611, 9001, 9807,
                         p(117,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102151, 'Tokyo_UTM_Zone_51N',
                         3092
              );
  _cs.AddAlias( 102152, 'Tokyo_UTM_Zone_52N',
                         3093
              );
  _cs.AddAlias( 102153, 'Tokyo_UTM_Zone_53N',
                         3094
              );
  _cs.AddAlias( 102154, 'Tokyo_UTM_Zone_54N',
                         3095
              );
  _cs.AddAlias( 102155, 'Tokyo_UTM_Zone_55N',
                         3096
              );
  _cs.Add     ( 102156, 'Tokyo_UTM_Zone_56N',
                         4301, 9001, 9807,
                         p(153,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102160, 'Datum_73_Hayford_Gauss_IGeoE',
                         4274, 9001, 9807,
                         p(-8.13190611111111,39.6666666666667,200180.598,299913.01,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102161, 'Datum_73_Hayford_Gauss_IPCC',
                         27492
              );
  _cs.Add     ( 102162, 'Graciosa_Base_SW_1948_UTM_Zone_26N',
                         37241, 9001, 9807,
                         p(-27,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102163, 'Lisboa_Bessel_Bonne',
                         104105, 9001, 9827,
                         p(-8.13190611111111,0,0,0,39.6666666666667,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102164, 'Lisboa_Hayford_Gauss_IGeoE',
                         104106, 9001, 9807,
                         p(-8.13190611111111,39.6666666666667,200000,300000,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102165, 'Lisboa_Hayford_Gauss_IPCC',
                         104106, 9001, 9807,
                         p(-8.13190611111111,39.6666666666667,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102166, 'Observ_Meteorologico_1939_UTM_Zone_25N',
                         37245, 9001, 9807,
                         p(-33,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102167, 'Porto_Santo_1936_UTM_Zone_28N',
                         2942
              );
  _cs.Add     ( 102168, 'Sao_Braz_UTM_Zone_26N',
                         37249, 9001, 9807,
                         p(-27,0,500000,0,0,0,0,0,0.9996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102169, 'Selvagem_Grande_1938_UTM_Zone_28N',
                         2943
              );
  _cs.AddAlias( 102191, 'Nord_Maroc_Degree',
                         26191
              );
  _cs.AddAlias( 102192, 'Sud_Maroc_Degree',
                         26192
              );
  _cs.AddAlias( 102193, 'Sahara_Degree',
                         26193
              );
  _cs.AddAlias( 102229, 'NAD_1983_HARN_StatePlane_Alabama_East_FIPS_0101',
                         2759
              );
  _cs.AddAlias( 102230, 'NAD_1983_HARN_StatePlane_Alabama_West_FIPS_0102',
                         2760
              );
  _cs.Add     ( 102241, 'NAD_1983_HARN_StatePlane_California_I_FIPS_0401',
                         4152, 9001, 9802,
                         p(-122,39.3333333333333,2000000,500000,40,41.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102242, 'NAD_1983_HARN_StatePlane_California_II_FIPS_0402',
                         4152, 9001, 9802,
                         p(-122,37.6666666666667,2000000,500000,38.3333333333333,39.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102243, 'NAD_1983_HARN_StatePlane_California_III_FIPS_0403',
                         4152, 9001, 9802,
                         p(-120.5,36.5,2000000,500000,37.0666666666667,38.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102244, 'NAD_1983_HARN_StatePlane_California_IV_FIPS_0404',
                         4152, 9001, 9802,
                         p(-119,35.3333333333333,2000000,500000,36,37.25,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102245, 'NAD_1983_HARN_StatePlane_California_V_FIPS_0405',
                         4152, 9001, 9802,
                         p(-118,33.5,2000000,500000,34.0333333333333,35.4666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102246, 'NAD_1983_HARN_StatePlane_California_VI_FIPS_0406',
                         4152, 9001, 9802,
                         p(-116.25,32.1666666666667,2000000,500000,32.7833333333333,33.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102248, 'NAD_1983_HARN_StatePlane_Arizona_East_FIPS_0201',
                         2761
              );
  _cs.AddAlias( 102249, 'NAD_1983_HARN_StatePlane_Arizona_Central_FIPS_0202',
                         2762
              );
  _cs.AddAlias( 102250, 'NAD_1983_HARN_StatePlane_Arizona_West_FIPS_0203',
                         2763
              );
  _cs.Add     ( 102251, 'NAD_1983_HARN_StatePlane_Arkansas_North_FIPS_0301',
                         4152, 9001, 9802,
                         p(-92,34.3333333333333,400000,0,34.9333333333333,36.2333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102252, 'NAD_1983_HARN_StatePlane_Arkansas_South_FIPS_0302',
                         4152, 9001, 9802,
                         p(-92,32.6666666666667,400000,400000,33.3,34.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102253, 'NAD_1983_HARN_StatePlane_Colorado_North_FIPS_0501',
                         4152, 9001, 9802,
                         p(-105.5,39.3333333333333,914401.8289,304800.6096,39.7166666666667,40.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102254, 'NAD_1983_HARN_StatePlane_Colorado_Central_FIPS_0502',
                         4152, 9001, 9802,
                         p(-105.5,37.8333333333333,914401.8289,304800.6096,38.45,39.75,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102255, 'NAD_1983_HARN_StatePlane_Colorado_South_FIPS_0503',
                         4152, 9001, 9802,
                         p(-105.5,36.6666666666667,914401.8289,304800.6096,37.2333333333333,38.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102256, 'NAD_1983_HARN_StatePlane_Connecticut_FIPS_0600',
                         4152, 9001, 9802,
                         p(-72.75,40.8333333333333,304800.6096,152400.3048,41.2,41.8666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102257, 'NAD_1983_HARN_StatePlane_Delaware_FIPS_0700',
                         2776
              );
  _cs.AddAlias( 102258, 'NAD_1983_HARN_StatePlane_Florida_East_FIPS_0901',
                         2777
              );
  _cs.AddAlias( 102259, 'NAD_1983_HARN_StatePlane_Florida_West_FIPS_0902',
                         2778
              );
  _cs.Add     ( 102260, 'NAD_1983_HARN_StatePlane_Florida_North_FIPS_0903',
                         4152, 9001, 9802,
                         p(-84.5,29,600000,0,29.5833333333333,30.75,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102261, 'NAD_1983_HARN_StatePlane_Hawaii_1_FIPS_5101',
                         2782
              );
  _cs.AddAlias( 102262, 'NAD_1983_HARN_StatePlane_Hawaii_2_FIPS_5102',
                         2783
              );
  _cs.AddAlias( 102263, 'NAD_1983_HARN_StatePlane_Hawaii_3_FIPS_5103',
                         2784
              );
  _cs.AddAlias( 102264, 'NAD_1983_HARN_StatePlane_Hawaii_4_FIPS_5104',
                         2785
              );
  _cs.AddAlias( 102265, 'NAD_1983_HARN_StatePlane_Hawaii_5_FIPS_5105',
                         2786
              );
  _cs.AddAlias( 102266, 'NAD_1983_HARN_StatePlane_Georgia_East_FIPS_1001',
                         2780
              );
  _cs.AddAlias( 102267, 'NAD_1983_HARN_StatePlane_Georgia_West_FIPS_1002',
                         2781
              );
  _cs.AddAlias( 102268, 'NAD_1983_HARN_StatePlane_Idaho_East_FIPS_1101',
                         2787
              );
  _cs.AddAlias( 102269, 'NAD_1983_HARN_StatePlane_Idaho_Central_FIPS_1102',
                         2788
              );
  _cs.AddAlias( 102270, 'NAD_1983_HARN_StatePlane_Idaho_West_FIPS_1103',
                         2789
              );
  _cs.AddAlias( 102271, 'NAD_1983_HARN_StatePlane_Illinois_East_FIPS_1201',
                         2790
              );
  _cs.AddAlias( 102272, 'NAD_1983_HARN_StatePlane_Illinois_West_FIPS_1202',
                         2791
              );
  _cs.AddAlias( 102273, 'NAD_1983_HARN_StatePlane_Indiana_East_FIPS_1301',
                         2792
              );
  _cs.AddAlias( 102274, 'NAD_1983_HARN_StatePlane_Indiana_West_FIPS_1302',
                         2793
              );
  _cs.Add     ( 102277, 'NAD_1983_HARN_StatePlane_Kansas_North_FIPS_1501',
                         4152, 9001, 9802,
                         p(-98,38.3333333333333,400000,0,38.7166666666667,39.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102278, 'NAD_1983_HARN_StatePlane_Kansas_South_FIPS_1502',
                         4152, 9001, 9802,
                         p(-98.5,36.6666666666667,400000,400000,37.2666666666667,38.5666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102279, 'NAD_1983_HARN_StatePlane_Kentucky_North_FIPS_1601',
                         2798
              );
  _cs.Add     ( 102280, 'NAD_1983_HARN_StatePlane_Kentucky_South_FIPS_1602',
                         4152, 9001, 9802,
                         p(-85.75,36.3333333333333,500000,500000,36.7333333333333,37.9333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102281, 'NAD_1983_HARN_StatePlane_Louisiana_North_FIPS_1701',
                         4152, 9001, 9802,
                         p(-92.5,30.5,1000000,0,31.1666666666667,32.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102282, 'NAD_1983_HARN_StatePlane_Louisiana_South_FIPS_1702',
                         4152, 9001, 9802,
                         p(-91.3333333333333,28.5,1000000,0,29.3,30.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102283, 'NAD_1983_HARN_StatePlane_Maine_East_FIPS_1801',
                         2802
              );
  _cs.AddAlias( 102284, 'NAD_1983_HARN_StatePlane_Maine_West_FIPS_1802',
                         2803
              );
  _cs.Add     ( 102285, 'NAD_1983_HARN_StatePlane_Maryland_FIPS_1900',
                         4152, 9001, 9802,
                         p(-77,37.6666666666667,400000,0,38.3,39.45,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102286, 'NAD_1983_HARN_StatePlane_Massachusetts_Mainland_FIPS_2001',
                         4152, 9001, 9802,
                         p(-71.5,41,200000,750000,41.7166666666667,42.6833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102287, 'NAD_1983_HARN_StatePlane_Massachusetts_Island_FIPS_2002',
                         4152, 9001, 9802,
                         p(-70.5,41,500000,0,41.2833333333333,41.4833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102288, 'NAD_1983_HARN_StatePlane_Michigan_North_FIPS_2111',
                         4152, 9001, 9802,
                         p(-87,44.7833333333333,8000000,0,45.4833333333333,47.0833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102289, 'NAD_1983_HARN_StatePlane_Michigan_Central_FIPS_2112',
                         4152, 9001, 9802,
                         p(-84.3666666666667,43.3166666666667,6000000,0,44.1833333333333,45.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102290, 'NAD_1983_HARN_StatePlane_Michigan_South_FIPS_2113',
                         4152, 9001, 9802,
                         p(-84.3666666666667,41.5,4000000,0,42.1,43.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102291, 'NAD_1983_HARN_StatePlane_Minnesota_North_FIPS_2201',
                         4152, 9001, 9802,
                         p(-93.1,46.5,800000,100000,47.0333333333333,48.6333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102292, 'NAD_1983_HARN_StatePlane_Minnesota_Central_FIPS_2202',
                         4152, 9001, 9802,
                         p(-94.25,45,800000,100000,45.6166666666667,47.05,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102293, 'NAD_1983_HARN_StatePlane_Minnesota_South_FIPS_2203',
                         4152, 9001, 9802,
                         p(-94,43,800000,100000,43.7833333333333,45.2166666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102294, 'NAD_1983_HARN_StatePlane_Mississippi_East_FIPS_2301',
                         2813
              );
  _cs.AddAlias( 102295, 'NAD_1983_HARN_StatePlane_Mississippi_West_FIPS_2302',
                         2814
              );
  _cs.AddAlias( 102296, 'NAD_1983_HARN_StatePlane_Missouri_East_FIPS_2401',
                         2815
              );
  _cs.AddAlias( 102297, 'NAD_1983_HARN_StatePlane_Missouri_Central_FIPS_2402',
                         2816
              );
  _cs.AddAlias( 102298, 'NAD_1983_HARN_StatePlane_Missouri_West_FIPS_2403',
                         2817
              );
  _cs.Add     ( 102300, 'NAD_1983_HARN_StatePlane_Montana_FIPS_2500',
                         4152, 9001, 9802,
                         p(-109.5,44.25,600000,0,45,49,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102304, 'NAD_1983_HARN_StatePlane_Nebraska_FIPS_2600',
                         4152, 9001, 9802,
                         p(-100,39.8333333333333,500000,0,40,43,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102307, 'NAD_1983_HARN_StatePlane_Nevada_East_FIPS_2701',
                         2820
              );
  _cs.AddAlias( 102308, 'NAD_1983_HARN_StatePlane_Nevada_Central_FIPS_2702',
                         2821
              );
  _cs.AddAlias( 102309, 'NAD_1983_HARN_StatePlane_Nevada_West_FIPS_2703',
                         2822
              );
  _cs.AddAlias( 102310, 'NAD_1983_HARN_StatePlane_New_Hampshire_FIPS_2800',
                         2823
              );
  _cs.AddAlias( 102311, 'NAD_1983_HARN_StatePlane_New_Jersey_FIPS_2900',
                         2824
              );
  _cs.AddAlias( 102312, 'NAD_1983_HARN_StatePlane_New_Mexico_East_FIPS_3001',
                         2825
              );
  _cs.AddAlias( 102313, 'NAD_1983_HARN_StatePlane_New_Mexico_Central_FIPS_3002',
                         2826
              );
  _cs.AddAlias( 102314, 'NAD_1983_HARN_StatePlane_New_Mexico_West_FIPS_3003',
                         2827
              );
  _cs.AddAlias( 102315, 'NAD_1983_HARN_StatePlane_New_York_East_FIPS_3101',
                         2824
              );
  _cs.AddAlias( 102316, 'NAD_1983_HARN_StatePlane_New_York_Central_FIPS_3102',
                         2829
              );
  _cs.AddAlias( 102317, 'NAD_1983_HARN_StatePlane_New_York_West_FIPS_3103',
                         2830
              );
  _cs.Add     ( 102318, 'NAD_1983_HARN_StatePlane_New_York_Long_Island_FIPS_3104',
                         4152, 9001, 9802,
                         p(-74,40.1666666666667,300000,0,40.6666666666667,41.0333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102320, 'NAD_1983_HARN_StatePlane_North_Dakota_North_FIPS_3301',
                         4152, 9001, 9802,
                         p(-100.5,47,600000,0,47.4333333333333,48.7333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102321, 'NAD_1983_HARN_StatePlane_North_Dakota_South_FIPS_3302',
                         4152, 9001, 9802,
                         p(-100.5,45.6666666666667,600000,0,46.1833333333333,47.4833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102322, 'NAD_1983_HARN_StatePlane_Ohio_North_FIPS_3401',
                         4152, 9001, 9802,
                         p(-82.5,39.6666666666667,600000,0,40.4333333333333,41.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102323, 'NAD_1983_HARN_StatePlane_Ohio_South_FIPS_3402',
                         4152, 9001, 9802,
                         p(-82.5,38,600000,0,38.7333333333333,40.0333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102324, 'NAD_1983_HARN_StatePlane_Oklahoma_North_FIPS_3501',
                         4152, 9001, 9802,
                         p(-98,35,600000,0,35.5666666666667,36.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
end;

procedure prepare_3;
begin
  _cs.Add     ( 102325, 'NAD_1983_HARN_StatePlane_Oklahoma_South_FIPS_3502',
                         4152, 9001, 9802,
                         p(-98,33.3333333333333,600000,0,33.9333333333333,35.2333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102326, 'NAD_1983_HARN_StatePlane_Oregon_North_FIPS_3601',
                         4152, 9001, 9802,
                         p(-120.5,43.6666666666667,2500000,0,44.3333333333333,46,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102327, 'NAD_1983_HARN_StatePlane_Oregon_South_FIPS_3602',
                         4152, 9001, 9802,
                         p(-120.5,41.6666666666667,1500000,0,42.3333333333333,44,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102330, 'NAD_1983_HARN_StatePlane_Rhode_Island_FIPS_3800',
                         2840
              );
  _cs.Add     ( 102334, 'NAD_1983_HARN_StatePlane_South_Dakota_North_FIPS_4001',
                         4152, 9001, 9802,
                         p(-100,43.8333333333333,600000,0,44.4166666666667,45.6833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102335, 'NAD_1983_HARN_StatePlane_South_Dakota_South_FIPS_4002',
                         4152, 9001, 9802,
                         p(-100.333333333333,42.3333333333333,600000,0,42.8333333333333,44.4,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102336, 'NAD_1983_HARN_StatePlane_Tennessee_FIPS_4100',
                         4152, 9001, 9802,
                         p(-86,34.3333333333333,600000,0,35.25,36.4166666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102337, 'NAD_1983_HARN_StatePlane_Texas_North_FIPS_4201',
                         4152, 9001, 9802,
                         p(-101.5,34,200000,1000000,34.65,36.1833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102338, 'NAD_1983_HARN_StatePlane_Texas_North_Central_FIPS_4202',
                         4152, 9001, 9802,
                         p(-98.5,31.6666666666667,600000,2000000,32.1333333333333,33.9666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102339, 'NAD_1983_HARN_StatePlane_Texas_Central_FIPS_4203',
                         4152, 9001, 9802,
                         p(-100.333333333333,29.6666666666667,700000,3000000,30.1166666666667,31.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102340, 'NAD_1983_HARN_StatePlane_Texas_South_Central_FIPS_4204',
                         4152, 9001, 9802,
                         p(-99,27.8333333333333,600000,4000000,28.3833333333333,30.2833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102341, 'NAD_1983_HARN_StatePlane_Texas_South_FIPS_4205',
                         4152, 9001, 9802,
                         p(-98.5,25.6666666666667,300000,5000000,26.1666666666667,27.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102342, 'NAD_1983_HARN_StatePlane_Utah_North_FIPS_4301',
                         4152, 9001, 9802,
                         p(-111.5,40.3333333333333,500000,1000000,40.7166666666667,41.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102343, 'NAD_1983_HARN_StatePlane_Utah_Central_FIPS_4302',
                         4152, 9001, 9802,
                         p(-111.5,38.3333333333333,500000,2000000,39.0166666666667,40.65,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102344, 'NAD_1983_HARN_StatePlane_Utah_South_FIPS_4303',
                         4152, 9001, 9802,
                         p(-111.5,36.6666666666667,500000,3000000,37.2166666666667,38.35,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102345, 'NAD_1983_HARN_StatePlane_Vermont_FIPS_4400',
                         2852
              );
  _cs.Add     ( 102346, 'NAD_1983_HARN_StatePlane_Virginia_North_FIPS_4501',
                         4152, 9001, 9802,
                         p(-78.5,37.6666666666667,3500000,2000000,38.0333333333333,39.2,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102347, 'NAD_1983_HARN_StatePlane_Virginia_South_FIPS_4502',
                         4152, 9001, 9802,
                         p(-78.5,36.3333333333333,3500000,1000000,36.7666666666667,37.9666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102348, 'NAD_1983_HARN_StatePlane_Washington_North_FIPS_4601',
                         4152, 9001, 9802,
                         p(-120.833333333333,47,500000,0,47.5,48.7333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102349, 'NAD_1983_HARN_StatePlane_Washington_South_FIPS_4602',
                         4152, 9001, 9802,
                         p(-120.5,45.3333333333333,500000,0,45.8333333333333,47.3333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102350, 'NAD_1983_HARN_StatePlane_West_Virginia_North_FIPS_4701',
                         4152, 9001, 9802,
                         p(-79.5,38.5,600000,0,39,40.25,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102351, 'NAD_1983_HARN_StatePlane_West_Virginia_South_FIPS_4702',
                         4152, 9001, 9802,
                         p(-81,37,600000,0,37.4833333333333,38.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102352, 'NAD_1983_HARN_StatePlane_Wisconsin_North_FIPS_4801',
                         4152, 9001, 9802,
                         p(-90,45.1666666666667,600000,0,45.5666666666667,46.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102353, 'NAD_1983_HARN_StatePlane_Wisconsin_Central_FIPS_4802',
                         4152, 9001, 9802,
                         p(-90,43.8333333333333,600000,0,44.25,45.5,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102354, 'NAD_1983_HARN_StatePlane_Wisconsin_South_FIPS_4803',
                         4152, 9001, 9802,
                         p(-90,42,600000,0,42.7333333333333,44.0666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102355, 'NAD_1983_HARN_StatePlane_Wyoming_East_FIPS_4901',
                         2862
              );
  _cs.AddAlias( 102356, 'NAD_1983_HARN_StatePlane_Wyoming_East_Central_FIPS_4902',
                         2863
              );
  _cs.AddAlias( 102357, 'NAD_1983_HARN_StatePlane_Wyoming_West_Central_FIPS_4903',
                         2864
              );
  _cs.AddAlias( 102358, 'NAD_1983_HARN_StatePlane_Wyoming_West_FIPS_4904',
                         2865
              );
  _cs.Add     ( 102361, 'NAD_1983_HARN_StatePlane_Puerto_Rico_Virgin_Islands_FIPS_5200',
                         4152, 9001, 9802,
                         p(-66.4333333333333,17.8333333333333,200000,200000,18.0333333333333,18.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102491, 'Nord_Algerie_Ancienne_Degree',
                         30491
              );
  _cs.AddAlias( 102492, 'Sud_Algerie_Ancienne_Degree',
                         30492
              );
  _cs.Add     ( 102581, 'NTF_France_I_degrees',
                         4275, 9001, 9801,
                         p(2.33722916666667,49.5,600000,1200000,0,0,0,0,0.999877341,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102582, 'NTF_France_II_degrees',
                         4275, 9001, 9801,
                         p(2.33722916666667,46.8,600000,2200000,0,0,0,0,0.99987742,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102583, 'NTF_France_III_degrees',
                         4275, 9001, 9801,
                         p(2.33722916666667,44.1,600000,3200000,0,0,0,0,0.999877499,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102584, 'NTF_France_IV_degrees',
                         4275, 9001, 9801,
                         p(2.33722916666667,42.165,234.358,4185861.369,0,0,0,0,0.99994471,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102591, 'Nord_Algerie_Degree',
                         30791
              );
  _cs.AddAlias( 102592, 'Sud_Algerie_Degree',
                         30792
              );
  _cs.Add     ( 102629, 'NAD_1983_StatePlane_Alabama_East_FIPS_0101_Feet',
                         4269, 9003, 9807,
                         p(-85.8333333333333,30.5,200000,0,0,0,0,0,0.99996,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102630, 'NAD_1983_StatePlane_Alabama_West_FIPS_0102_Feet',
                         4269, 9003, 9807,
                         p(-87.5,30,600000,0,0,0,0,0,0.999933333333333,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102631, 'NAD_1983_StatePlane_Alaska_1_FIPS_5001_Feet',
                         4269, 9003, 9815,
                         p(0,0,5000000,-5000000,0,0,0,0,0.9999,-133.666666666667,57,-36.8698976458333,0,0,0,0,0,0,60)
              );
  _cs.Add     ( 102632, 'NAD_1983_StatePlane_Alaska_2_FIPS_5002_Feet',
                         4269, 9003, 9807,
                         p(-142,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102633, 'NAD_1983_StatePlane_Alaska_3_FIPS_5003_Feet',
                         4269, 9003, 9807,
                         p(-146,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102634, 'NAD_1983_StatePlane_Alaska_4_FIPS_5004_Feet',
                         4269, 9003, 9807,
                         p(-150,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102635, 'NAD_1983_StatePlane_Alaska_5_FIPS_5005_Feet',
                         4269, 9003, 9807,
                         p(-154,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102636, 'NAD_1983_StatePlane_Alaska_6_FIPS_5006_Feet',
                         4269, 9003, 9807,
                         p(-158,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102637, 'NAD_1983_StatePlane_Alaska_7_FIPS_5007_Feet',
                         4269, 9003, 9807,
                         p(-162,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102638, 'NAD_1983_StatePlane_Alaska_8_FIPS_5008_Feet',
                         4269, 9003, 9807,
                         p(-166,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102639, 'NAD_1983_StatePlane_Alaska_9_FIPS_5009_Feet',
                         4269, 9003, 9807,
                         p(-170,54,500000,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102640, 'NAD_1983_StatePlane_Alaska_10_FIPS_5010_Feet',
                         4269, 9003, 9802,
                         p(-176,51,1000000,0,51.8333333333333,53.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102641, 'NAD_1983_StatePlane_California_I_FIPS_0401_Feet',
                         4269, 9003, 9802,
                         p(-122,39.3333333333333,2000000,500000,40,41.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102642, 'NAD_1983_StatePlane_California_II_FIPS_0402_Feet',
                         4269, 9003, 9802,
                         p(-122,37.6666666666667,2000000,500000,38.3333333333333,39.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102643, 'NAD_1983_StatePlane_California_III_FIPS_0403_Feet',
                         4269, 9003, 9802,
                         p(-120.5,36.5,2000000,500000,37.0666666666667,38.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102644, 'NAD_1983_StatePlane_California_IV_FIPS_0404_Feet',
                         4269, 9003, 9802,
                         p(-119,35.3333333333333,2000000,500000,36,37.25,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102645, 'NAD_1983_StatePlane_California_V_FIPS_0405_Feet',
                         4269, 9003, 9802,
                         p(-118,33.5,2000000,500000,34.0333333333333,35.4666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102646, 'NAD_1983_StatePlane_California_VI_FIPS_0406_Feet',
                         4269, 9003, 9802,
                         p(-116.25,32.1666666666667,2000000,500000,32.7833333333333,33.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102648, 'NAD_1983_StatePlane_Arizona_East_FIPS_0201_Feet',
                         4269, 9003, 9807,
                         p(-110.166666666667,31,213360,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102649, 'NAD_1983_StatePlane_Arizona_Central_FIPS_0202_Feet',
                         4269, 9003, 9807,
                         p(-111.916666666667,31,213360,0,0,0,0,0,0.9999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102650, 'NAD_1983_StatePlane_Arizona_West_FIPS_0203_Feet',
                         4269, 9003, 9807,
                         p(-113.75,31,213360,0,0,0,0,0,0.999933333333333,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102651, 'NAD_1983_StatePlane_Arkansas_North_FIPS_0301_Feet',
                         4269, 9003, 9802,
                         p(-92,34.3333333333333,400000,0,34.9333333333333,36.2333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102652, 'NAD_1983_StatePlane_Arkansas_South_FIPS_0302_Feet',
                         4269, 9003, 9802,
                         p(-92,32.6666666666667,400000,400000,33.3,34.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102653, 'NAD_1983_StatePlane_Colorado_North_FIPS_0501_Feet',
                         4269, 9003, 9802,
                         p(-105.5,39.3333333333333,914401.8289,304800.6096,39.7166666666667,40.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102654, 'NAD_1983_StatePlane_Colorado_Central_FIPS_0502_Feet',
                         4269, 9003, 9802,
                         p(-105.5,37.8333333333333,914401.8289,304800.6096,38.45,39.75,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102655, 'NAD_1983_StatePlane_Colorado_South_FIPS_0503_Feet',
                         4269, 9003, 9802,
                         p(-105.5,36.6666666666667,914401.8289,304800.6096,37.2333333333333,38.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102656, 'NAD_1983_StatePlane_Connecticut_FIPS_0600_Feet',
                         4269, 9003, 9802,
                         p(-72.75,40.8333333333333,304800.6096,152400.3048,41.2,41.8666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102657, 'NAD_1983_StatePlane_Delaware_FIPS_0700_Feet',
                         2235
              );
  _cs.AddAlias( 102658, 'NAD_1983_StatePlane_Florida_East_FIPS_0901_Feet',
                         2236
              );
  _cs.AddAlias( 102659, 'NAD_1983_StatePlane_Florida_West_FIPS_0902_Feet',
                         2237
              );
  _cs.Add     ( 102660, 'NAD_1983_StatePlane_Florida_North_FIPS_0903_Feet',
                         4269, 9003, 9802,
                         p(-84.5,29,600000,0,29.5833333333333,30.75,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102661, 'NAD_1983_StatePlane_Hawaii_1_FIPS_5101_Feet',
                         4269, 9003, 9807,
                         p(-155.5,18.8333333333333,500000,0,0,0,0,0,0.999966666666667,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102662, 'NAD_1983_StatePlane_Hawaii_2_FIPS_5102_Feet',
                         4269, 9003, 9807,
                         p(-156.666666666667,20.3333333333333,500000,0,0,0,0,0,0.999966666666667,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102663, 'NAD_1983_StatePlane_Hawaii_3_FIPS_5103_Feet',
                         3759
              );
  _cs.Add     ( 102664, 'NAD_1983_StatePlane_Hawaii_4_FIPS_5104_Feet',
                         4269, 9003, 9807,
                         p(-159.5,21.8333333333333,500000,0,0,0,0,0,0.99999,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102665, 'NAD_1983_StatePlane_Hawaii_5_FIPS_5105_Feet',
                         4269, 9003, 9807,
                         p(-160.166666666667,21.6666666666667,500000,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102666, 'NAD_1983_StatePlane_Georgia_East_FIPS_1001_Feet',
                         2239
              );
  _cs.AddAlias( 102667, 'NAD_1983_StatePlane_Georgia_West_FIPS_1002_Feet',
                         2240
              );
  _cs.AddAlias( 102668, 'NAD_1983_StatePlane_Idaho_East_FIPS_1101_Feet',
                         2241
              );
  _cs.AddAlias( 102669, 'NAD_1983_StatePlane_Idaho_Central_FIPS_1102_Feet',
                         2242
              );
  _cs.AddAlias( 102670, 'NAD_1983_StatePlane_Idaho_West_FIPS_1103_Feet',
                         2243
              );
  _cs.AddAlias( 102671, 'NAD_1983_StatePlane_Illinois_East_FIPS_1201_Feet',
                         3435
              );
  _cs.AddAlias( 102672, 'NAD_1983_StatePlane_Illinois_West_FIPS_1202_Feet',
                         3436
              );
  _cs.AddAlias( 102673, 'NAD_1983_StatePlane_Indiana_East_FIPS_1301_Feet',
                         2965
              );
  _cs.AddAlias( 102674, 'NAD_1983_StatePlane_Indiana_West_FIPS_1302_Feet',
                         2966
              );
  _cs.Add     ( 102675, 'NAD_1983_StatePlane_Iowa_North_FIPS_1401_Feet',
                         4269, 9003, 9802,
                         p(-93.5,41.5,1500000,1000000,42.0666666666667,43.2666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102676, 'NAD_1983_StatePlane_Iowa_South_FIPS_1402_Feet',
                         4269, 9003, 9802,
                         p(-93.5,40,500000,0,40.6166666666667,41.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102677, 'NAD_1983_StatePlane_Kansas_North_FIPS_1501_Feet',
                         4269, 9003, 9802,
                         p(-98,38.3333333333333,400000,0,38.7166666666667,39.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102678, 'NAD_1983_StatePlane_Kansas_South_FIPS_1502_Feet',
                         4269, 9003, 9802,
                         p(-98.5,36.6666666666667,400000,400000,37.2666666666667,38.5666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102679, 'NAD_1983_StatePlane_Kentucky_North_FIPS_1601_Feet',
                         2246
              );
  _cs.Add     ( 102680, 'NAD_1983_StatePlane_Kentucky_South_FIPS_1602_Feet',
                         4269, 9003, 9802,
                         p(-85.75,36.3333333333333,500000,500000,36.7333333333333,37.9333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102681, 'NAD_1983_StatePlane_Louisiana_North_FIPS_1701_Feet',
                         4269, 9003, 9802,
                         p(-92.5,30.5,1000000,0,31.1666666666667,32.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102682, 'NAD_1983_StatePlane_Louisiana_South_FIPS_1702_Feet',
                         4269, 9003, 9802,
                         p(-91.3333333333333,28.5,1000000,0,29.3,30.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102683, 'NAD_1983_StatePlane_Maine_East_FIPS_1801_Feet',
                         26847
              );
  _cs.AddAlias( 102684, 'NAD_1983_StatePlane_Maine_West_FIPS_1802_Feet',
                         26848
              );
  _cs.Add     ( 102685, 'NAD_1983_StatePlane_Maryland_FIPS_1900_Feet',
                         4269, 9003, 9802,
                         p(-77,37.6666666666667,400000,0,38.3,39.45,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102686, 'NAD_1983_StatePlane_Massachusetts_Mainland_FIPS_2001_Feet',
                         4269, 9003, 9802,
                         p(-71.5,41,200000,750000,41.7166666666667,42.6833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102687, 'NAD_1983_StatePlane_Massachusetts_Island_FIPS_2002_Feet',
                         4269, 9003, 9802,
                         p(-70.5,41,500000,0,41.2833333333333,41.4833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102688, 'NAD_1983_StatePlane_Michigan_North_FIPS_2111_Feet',
                         4269, 9003, 9802,
                         p(-87,44.7833333333333,8000000,0,45.4833333333333,47.0833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102689, 'NAD_1983_StatePlane_Michigan_Central_FIPS_2112_Feet',
                         4269, 9003, 9802,
                         p(-84.3666666666667,43.3166666666667,6000000,0,44.1833333333333,45.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102690, 'NAD_1983_StatePlane_Michigan_South_FIPS_2113_Feet',
                         4269, 9003, 9802,
                         p(-84.3666666666667,41.5,4000000,0,42.1,43.6666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102691, 'NAD_1983_StatePlane_Minnesota_North_FIPS_2201_Feet',
                         4269, 9003, 9802,
                         p(-93.1,46.5,800000,100000,47.0333333333333,48.6333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
end;

procedure prepare_4;
begin
  _cs.Add     ( 102692, 'NAD_1983_StatePlane_Minnesota_Central_FIPS_2202_Feet',
                         4269, 9003, 9802,
                         p(-94.25,45,800000,100000,45.6166666666667,47.05,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102693, 'NAD_1983_StatePlane_Minnesota_South_FIPS_2203_Feet',
                         4269, 9003, 9802,
                         p(-94,43,800000,100000,43.7833333333333,45.2166666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102694, 'NAD_1983_StatePlane_Mississippi_East_FIPS_2301_Feet',
                         2254
              );
  _cs.AddAlias( 102695, 'NAD_1983_StatePlane_Mississippi_West_FIPS_2302_Feet',
                         2255
              );
  _cs.Add     ( 102696, 'NAD_1983_StatePlane_Missouri_East_FIPS_2401_Feet',
                         4269, 9003, 9807,
                         p(-90.5,35.8333333333333,250000,0,0,0,0,0,0.999933333333333,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102697, 'NAD_1983_StatePlane_Missouri_Central_FIPS_2402_Feet',
                         4269, 9003, 9807,
                         p(-92.5,35.8333333333333,500000,0,0,0,0,0,0.999933333333333,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102698, 'NAD_1983_StatePlane_Missouri_West_FIPS_2403_Feet',
                         4269, 9003, 9807,
                         p(-94.5,36.1666666666667,850000,0,0,0,0,0,0.999941176470588,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102700, 'NAD_1983_StatePlane_Montana_FIPS_2500_Feet',
                         4269, 9003, 9802,
                         p(-109.5,44.25,600000,0,45,49,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102704, 'NAD_1983_StatePlane_Nebraska_FIPS_2600_Feet',
                         4269, 9003, 9802,
                         p(-100,39.8333333333333,500000,0,40,43,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102707, 'NAD_1983_StatePlane_Nevada_East_FIPS_2701_Feet',
                         3421
              );
  _cs.AddAlias( 102708, 'NAD_1983_StatePlane_Nevada_Central_FIPS_2702_Feet',
                         3422
              );
  _cs.AddAlias( 102709, 'NAD_1983_StatePlane_Nevada_West_FIPS_2703_Feet',
                         3423
              );
  _cs.AddAlias( 102710, 'NAD_1983_StatePlane_New_Hampshire_FIPS_2800_Feet',
                         3437
              );
  _cs.AddAlias( 102711, 'NAD_1983_StatePlane_New_Jersey_FIPS_2900_Feet',
                         2260
              );
  _cs.AddAlias( 102712, 'NAD_1983_StatePlane_New_Mexico_East_FIPS_3001_Feet',
                         2257
              );
  _cs.AddAlias( 102713, 'NAD_1983_StatePlane_New_Mexico_Central_FIPS_3002_Feet',
                         2258
              );
  _cs.AddAlias( 102714, 'NAD_1983_StatePlane_New_Mexico_West_FIPS_3003_Feet',
                         2259
              );
  _cs.AddAlias( 102715, 'NAD_1983_StatePlane_New_York_East_FIPS_3101_Feet',
                         2260
              );
  _cs.AddAlias( 102716, 'NAD_1983_StatePlane_New_York_Central_FIPS_3102_Feet',
                         2261
              );
  _cs.AddAlias( 102717, 'NAD_1983_StatePlane_New_York_West_FIPS_3103_Feet',
                         2262
              );
  _cs.Add     ( 102718, 'NAD_1983_StatePlane_New_York_Long_Island_FIPS_3104_Feet',
                         4269, 9003, 9802,
                         p(-74,40.1666666666667,300000,0,40.6666666666667,41.0333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102719, 'NAD_1983_StatePlane_North_Carolina_FIPS_3200_Feet',
                         4269, 9003, 9802,
                         p(-79,33.75,609601.22,0,34.3333333333333,36.1666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102720, 'NAD_1983_StatePlane_North_Dakota_North_FIPS_3301_Feet',
                         4269, 9003, 9802,
                         p(-100.5,47,600000,0,47.4333333333333,48.7333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102721, 'NAD_1983_StatePlane_North_Dakota_South_FIPS_3302_Feet',
                         4269, 9003, 9802,
                         p(-100.5,45.6666666666667,600000,0,46.1833333333333,47.4833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102722, 'NAD_1983_StatePlane_Ohio_North_FIPS_3401_Feet',
                         4269, 9003, 9802,
                         p(-82.5,39.6666666666667,600000,0,40.4333333333333,41.7,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102723, 'NAD_1983_StatePlane_Ohio_South_FIPS_3402_Feet',
                         4269, 9003, 9802,
                         p(-82.5,38,600000,0,38.7333333333333,40.0333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102724, 'NAD_1983_StatePlane_Oklahoma_North_FIPS_3501_Feet',
                         4269, 9003, 9802,
                         p(-98,35,600000,0,35.5666666666667,36.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102725, 'NAD_1983_StatePlane_Oklahoma_South_FIPS_3502_Feet',
                         4269, 9003, 9802,
                         p(-98,33.3333333333333,600000,0,33.9333333333333,35.2333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102726, 'NAD_1983_StatePlane_Oregon_North_FIPS_3601_Feet',
                         4269, 9003, 9802,
                         p(-120.5,43.6666666666667,2500000,0,44.3333333333333,46,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102727, 'NAD_1983_StatePlane_Oregon_South_FIPS_3602_Feet',
                         4269, 9003, 9802,
                         p(-120.5,41.6666666666667,1500000,0,42.3333333333333,44,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102728, 'NAD_1983_StatePlane_Pennsylvania_North_FIPS_3701_Feet',
                         4269, 9003, 9802,
                         p(-77.75,40.1666666666667,600000,0,40.8833333333333,41.95,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102729, 'NAD_1983_StatePlane_Pennsylvania_South_FIPS_3702_Feet',
                         4269, 9003, 9802,
                         p(-77.75,39.3333333333333,600000,0,39.9333333333333,40.9666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102730, 'NAD_1983_StatePlane_Rhode_Island_FIPS_3800_Feet',
                         3438
              );
  _cs.Add     ( 102733, 'NAD_1983_StatePlane_South_Carolina_FIPS_3900_Feet',
                         4269, 9003, 9802,
                         p(-81,31.8333333333333,609600,0,32.5,34.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102734, 'NAD_1983_StatePlane_South_Dakota_North_FIPS_4001_Feet',
                         4269, 9003, 9802,
                         p(-100,43.8333333333333,600000,0,44.4166666666667,45.6833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102735, 'NAD_1983_StatePlane_South_Dakota_South_FIPS_4002_Feet',
                         4269, 9003, 9802,
                         p(-100.333333333333,42.3333333333333,600000,0,42.8333333333333,44.4,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102736, 'NAD_1983_StatePlane_Tennessee_FIPS_4100_Feet',
                         4269, 9003, 9802,
                         p(-86,34.3333333333333,600000,0,35.25,36.4166666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102737, 'NAD_1983_StatePlane_Texas_North_FIPS_4201_Feet',
                         4269, 9003, 9802,
                         p(-101.5,34,200000,1000000,34.65,36.1833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102738, 'NAD_1983_StatePlane_Texas_North_Central_FIPS_4202_Feet',
                         4269, 9003, 9802,
                         p(-98.5,31.6666666666667,600000,2000000,32.1333333333333,33.9666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102739, 'NAD_1983_StatePlane_Texas_Central_FIPS_4203_Feet',
                         4269, 9003, 9802,
                         p(-100.333333333333,29.6666666666667,700000,3000000,30.1166666666667,31.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102740, 'NAD_1983_StatePlane_Texas_South_Central_FIPS_4204_Feet',
                         4269, 9003, 9802,
                         p(-99,27.8333333333333,600000,4000000,28.3833333333333,30.2833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102741, 'NAD_1983_StatePlane_Texas_South_FIPS_4205_Feet',
                         4269, 9003, 9802,
                         p(-98.5,25.6666666666667,300000,5000000,26.1666666666667,27.8333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102742, 'NAD_1983_StatePlane_Utah_North_FIPS_4301_Feet',
                         4269, 9003, 9802,
                         p(-111.5,40.3333333333333,500000,1000000,40.7166666666667,41.7833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102743, 'NAD_1983_StatePlane_Utah_Central_FIPS_4302_Feet',
                         4269, 9003, 9802,
                         p(-111.5,38.3333333333333,500000,2000000,39.0166666666667,40.65,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102744, 'NAD_1983_StatePlane_Utah_South_FIPS_4303_Feet',
                         4269, 9003, 9802,
                         p(-111.5,36.6666666666667,500000,3000000,37.2166666666667,38.35,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102745, 'NAD_1983_StatePlane_Vermont_FIPS_4400_Feet',
                         5646
              );
  _cs.Add     ( 102746, 'NAD_1983_StatePlane_Virginia_North_FIPS_4501_Feet',
                         4269, 9003, 9802,
                         p(-78.5,37.6666666666667,3500000,2000000,38.0333333333333,39.2,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102747, 'NAD_1983_StatePlane_Virginia_South_FIPS_4502_Feet',
                         4269, 9003, 9802,
                         p(-78.5,36.3333333333333,3500000,1000000,36.7666666666667,37.9666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102748, 'NAD_1983_StatePlane_Washington_North_FIPS_4601_Feet',
                         4269, 9003, 9802,
                         p(-120.833333333333,47,500000,0,47.5,48.7333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102749, 'NAD_1983_StatePlane_Washington_South_FIPS_4602_Feet',
                         4269, 9003, 9802,
                         p(-120.5,45.3333333333333,500000,0,45.8333333333333,47.3333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102750, 'NAD_1983_StatePlane_West_Virginia_North_FIPS_4701_Feet',
                         4269, 9003, 9802,
                         p(-79.5,38.5,600000,0,39,40.25,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102751, 'NAD_1983_StatePlane_West_Virginia_South_FIPS_4702_Feet',
                         4269, 9003, 9802,
                         p(-81,37,600000,0,37.4833333333333,38.8833333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102752, 'NAD_1983_StatePlane_Wisconsin_North_FIPS_4801_Feet',
                         4269, 9003, 9802,
                         p(-90,45.1666666666667,600000,0,45.5666666666667,46.7666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102753, 'NAD_1983_StatePlane_Wisconsin_Central_FIPS_4802_Feet',
                         4269, 9003, 9802,
                         p(-90,43.8333333333333,600000,0,44.25,45.5,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102754, 'NAD_1983_StatePlane_Wisconsin_South_FIPS_4803_Feet',
                         4269, 9003, 9802,
                         p(-90,42,600000,0,42.7333333333333,44.0666666666667,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.AddAlias( 102755, 'NAD_1983_StatePlane_Wyoming_East_FIPS_4901_Feet',
                         3736
              );
  _cs.AddAlias( 102756, 'NAD_1983_StatePlane_Wyoming_East_Central_FIPS_4902_Feet',
                         3737
              );
  _cs.AddAlias( 102757, 'NAD_1983_StatePlane_Wyoming_West_Central_FIPS_4903_Feet',
                         3738
              );
  _cs.AddAlias( 102758, 'NAD_1983_StatePlane_Wyoming_West_FIPS_4904_Feet',
                         3739
              );
  _cs.Add     ( 102761, 'NAD_1983_StatePlane_Puerto_Rico_Virgin_Islands_FIPS_5200_Feet',
                         4269, 9003, 9802,
                         p(-66.4333333333333,17.8333333333333,200000,200000,18.0333333333333,18.4333333333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 102766, 'NAD_1983_StatePlane_Guam_FIPS_5400_Feet',
                         4269, 9003, 9818,
                         p(-144.748750705556,13.4724663527778,50000,50000,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
              );
  _cs.Add     ( 103300, 'Belge_Lambert_1972',
                         4313, 9001, 9802,
                         p(4.36748666666667,90,150000.01256,5400088.4378,49.8333339,51.1666673333333,0,0,1,0,0,0,0,0,0,0,0,0,0)
              );
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
  prepare_4;
end;

//==================================== END ====================================

