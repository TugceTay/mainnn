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
  List of Ellipsoids.
  Generated from EPSG 9.1 dataset.
}


procedure Init_CSEllipsoidList(
  const _cs : TGIS_CSEllipsoidList
) ;

procedure prepare_1;
begin
  _cs.Add     (   1024, 'CGCS2000',
                        6378137, 298.257222101
              );
  _cs.Add     (   1025, 'GSK_2011',
                        6378136.5, 298.2564151
              );
  _cs.Add     (   1026, 'Zach_1812',
                        6376045, 310
              );
  _cs.Add     (   7001, 'Airy_1830',
                        6377563.396, 299.3249646
              );
  _cs.Add     (   7002, 'Airy_Modified_1849',
                        6377340.189, 299.3249646
              );
  _cs.Add     (   7003, 'Australian_National_Spheroid',
                        6378160, 298.25
              );
  _cs.AddAlias(     -1, 'ANS',
                        7003
              );
  _cs.Add     (   7004, 'Bessel_1841',
                        6377397.155, 299.1528128
              );
  _cs.Add     (   7005, 'Bessel_Modified',
                        6377492.018, 299.1528128
              );
  _cs.Add     (   7006, 'Bessel_Namibia',
                        6377483.865, 299.1528128
              )         .MarkDeprecated( True );
  _cs.Add     (   7007, 'Clarke_1858',
                        6378293.64520876, 294.260676369261
              );
  _cs.Add     (   7008, 'Clarke_1866',
                        6378206.4, 294.978698213898
              );
  _cs.Add     (   7009, 'Clarke_1866_Michigan',
                        6378450.04754889, 294.978697164674
              )         .MarkDeprecated( True );
  _cs.Add     (   7010, 'Clarke_1880_Benoit',
                        6378300.789, 293.466315538981
              );
  _cs.Add     (   7011, 'Clarke_1880_IGN',
                        6378249.2, 293.466021293627
              );
  _cs.Add     (   7012, 'Clarke_1880_RGS',
                        6378249.145, 293.465
              );
  _cs.AddAlias(     -1, 'Clarke_Modified_1880',
                        7012
              );
  _cs.Add     (   7013, 'Clarke_1880_Arc',
                        6378249.145, 293.4663077
              );
  _cs.AddAlias(     -1, 'Clarke_1880_Cape',
                        7013
              );
  _cs.AddAlias(     -1, 'Modified_Clarke_1880_South_Africa',
                        7013
              );
  _cs.Add     (   7014, 'Clarke_1880_SGA_1922',
                        6378249.2, 293.46598
              );
  _cs.Add     (   7015, 'Everest_1830_1937_Adjustment',
                        6377276.345, 300.8017
              );
  _cs.Add     (   7016, 'Everest_1830_1967_Definition',
                        6377298.556, 300.8017
              );
  _cs.Add     (   7018, 'Everest_1830_Modified',
                        6377304.063, 300.8017
              );
  _cs.Add     (   7019, 'GRS_1980',
                        6378137, 298.257222101
              );
  _cs.AddAlias(     -1, 'International_1979',
                        7019
              );
  _cs.Add     (   7020, 'Helmert_1906',
                        6378200, 298.3
              );
  _cs.Add     (   7021, 'Indonesian_National_Spheroid',
                        6378160, 298.247
              );
  _cs.Add     (   7022, 'International_1924',
                        6378388, 297
              );
  _cs.AddAlias(     -1, 'Hayford_1909',
                        7022
              );
  _cs.Add     (   7024, 'Krassowsky_1940',
                        6378245, 298.3
              );
  _cs.Add     (   7025, 'NWL_9D',
                        6378145, 298.25
              );
  _cs.AddAlias(     -1, 'WGS_66',
                        7025
              );
  _cs.Add     (   7027, 'Plessis_1817',
                        6376523, 308.64
              );
  _cs.Add     (   7028, 'Struve_1860',
                        6378298.3, 294.73
              );
  _cs.Add     (   7029, 'War_Office',
                        6378300, 296
              );
  _cs.AddAlias(     -1, 'McCaw_1924',
                        7029
              );
  _cs.Add     (   7030, 'WGS_84',
                        6378137, 298.257223563
              );
  _cs.AddAlias(     -1, 'WGS84',
                        7030
              );
  _cs.Add     (   7031, 'GEM_10C',
                        6378137, 298.257223563
              );
  _cs.Add     (   7032, 'OSU86F',
                        6378136.2, 298.257223563
              );
  _cs.Add     (   7033, 'OSU91A',
                        6378136.3, 298.257223563
              );
  _cs.Add     (   7034, 'Clarke_1880',
                        6378249.14480801, 293.46630765563
              );
  _cs.Add     (   7035, 'Sphere',
                        6371000, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   7036, 'GRS_1967',
                        6378160, 298.247167427
              );
  _cs.AddAlias(     -1, 'International_1967',
                        7036
              );
  _cs.Add     (   7041, 'Average_Terrestrial_System_1977',
                        6378135, 298.257
              );
  _cs.Add     (   7042, 'Everest_1830_Definition',
                        6377299.36559538, 300.801725543362
              );
  _cs.Add     (   7043, 'WGS_72',
                        6378135, 298.26
              );
  _cs.AddAlias(     -1, 'NWL_10D',
                        7043
              );
  _cs.Add     (   7044, 'Everest_1830_1962_Definition',
                        6377301.243, 300.8017255
              );
  _cs.Add     (   7045, 'Everest_1830_1975_Definition',
                        6377299.151, 300.8017255
              );
  _cs.Add     (   7046, 'Bessel_Namibia_GLM',
                        6377483.86528042, 299.1528128
              );
  _cs.Add     (   7047, 'GRS_1980_Authalic_Sphere',
                        6370997, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   7048, 'GRS_1980_Authalic_Sphere_A',
                        6371007, 0
              );
  _cs.Add     (   7049, 'IAG_1975',
                        6378140, 298.257
              );
  _cs.AddAlias(     -1, 'Xian_1980',
                        7049
              );
  _cs.Add     (   7050, 'GRS_1967_Modified',
                        6378160, 298.25
              );
  _cs.AddAlias(     -1, 'GRS_1967_A',
                        7050
              );
  _cs.Add     (   7051, 'Danish_1876',
                        6377019.27, 300
              );
  _cs.Add     (   7052, 'Clarke_1866_Authalic_Sphere',
                        6370997, 0
              );
  _cs.Add     (   7053, 'Hough_1960',
                        6378270, 297
              );
  _cs.Add     (   7054, 'PZ_90',
                        6378136, 298.257839303
              );
  _cs.Add     (   7055, 'Clarke_1880_international_foot',
                        6378306.3696, 293.46630765563
              );
  _cs.Add     (   7056, 'Everest_1830_RSO_1969',
                        6377295.664, 300.8017
              );
  _cs.Add     (   7057, 'International_1924_Authalic_Sphere',
                        6371228, 0
              );
  _cs.Add     (   7058, 'Hughes_1980',
                        6378273, 298.279411123064
              );
  _cs.Add     (   7059, 'Popular_Visualisation_Sphere',
                        6378137, 0
              )         .MarkDeprecated( True );
end;

begin
  prepare_1;
end;

//==================================== END ====================================

