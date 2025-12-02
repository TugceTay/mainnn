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
  List of Geographic Coordinate Systems.
  Generated from EPSG 9.1 dataset.
}


procedure Init_CSGeographicCoordinateSystemList(
  const _cs : TGIS_CSGeographicCoordinateSystemList
) ;

procedure prepare_1;
begin
  _cs.Add     (   3819, 'HD1909',
                        1024, 8901, 9122,
                        True, 1119
              );
  _cs.Add     (   3821, 'TWD67',
                        1025, 8901, 9122,
                        True, 3315
              );
  _cs.Add     (   3824, 'TWD97',
                        1026, 8901, 9122,
                        True, 1228
              );
  _cs.Add     (   3889, 'IGRS',
                        1029, 8901, 9122,
                        True, 1124
              );
  _cs.Add     (   3906, 'MGI_1901',
                        1031, 8901, 9122,
                        True, 2370
              );
  _cs.Add     (   4001, 'Unknown_datum_based_upon_the_Airy_1830_ellipsoid',
                        6001, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4002, 'Unknown_datum_based_upon_the_Airy_Modified_1849_ellipsoid',
                        6002, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4003, 'Unknown_datum_based_upon_the_Australian_National_Spheroid',
                        6003, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4004, 'Unknown_datum_based_upon_the_Bessel_1841_ellipsoid',
                        6004, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4005, 'Unknown_datum_based_upon_the_Bessel_Modified_ellipsoid',
                        6005, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4006, 'Unknown_datum_based_upon_the_Bessel_Namibia_ellipsoid',
                        6006, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4007, 'Unknown_datum_based_upon_the_Clarke_1858_ellipsoid',
                        6007, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4008, 'Unknown_datum_based_upon_the_Clarke_1866_ellipsoid',
                        6008, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4009, 'Unknown_datum_based_upon_the_Clarke_1866_Michigan_ellipsoid',
                        6009, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4010, 'Unknown_datum_based_upon_the_Clarke_1880_Benoit_ellipsoid',
                        6010, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4011, 'Unknown_datum_based_upon_the_Clarke_1880_IGN_ellipsoid',
                        6011, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4012, 'Unknown_datum_based_upon_the_Clarke_1880_RGS_ellipsoid',
                        6012, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4013, 'Unknown_datum_based_upon_the_Clarke_1880_Arc_ellipsoid',
                        6013, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4014, 'Unknown_datum_based_upon_the_Clarke_1880_SGA_1922_ellipsoid',
                        6014, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4015, 'Unknown_datum_based_upon_the_Everest_1830_1937_Adjustment_ellipsoid',
                        6015, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4016, 'Unknown_datum_based_upon_the_Everest_1830_1967_Definition_ellipsoid',
                        6016, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4018, 'Unknown_datum_based_upon_the_Everest_1830_Modified_ellipsoid',
                        6018, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4019, 'Unknown_datum_based_upon_the_GRS_1980_ellipsoid',
                        6019, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4020, 'Unknown_datum_based_upon_the_Helmert_1906_ellipsoid',
                        6020, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4021, 'Unknown_datum_based_upon_the_Indonesian_National_Spheroid',
                        6021, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4022, 'Unknown_datum_based_upon_the_International_1924_ellipsoid',
                        6022, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4023, 'MOLDREF99',
                        1032, 8901, 9122,
                        True, 1162
              );
  _cs.Add     (   4024, 'Unknown_datum_based_upon_the_Krassowsky_1940_ellipsoid',
                        6024, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4025, 'Unknown_datum_based_upon_the_NWL_9D_ellipsoid',
                        6025, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4027, 'Unknown_datum_based_upon_the_Plessis_1817_ellipsoid',
                        6027, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4028, 'Unknown_datum_based_upon_the_Struve_1860_ellipsoid',
                        6028, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4029, 'Unknown_datum_based_upon_the_War_Office_ellipsoid',
                        6029, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4030, 'Unknown_datum_based_upon_the_WGS_84_ellipsoid',
                        6030, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4031, 'Unknown_datum_based_upon_the_GEM_10C_ellipsoid',
                        6031, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4032, 'Unknown_datum_based_upon_the_OSU86F_ellipsoid',
                        6032, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4033, 'Unknown_datum_based_upon_the_OSU91A_ellipsoid',
                        6033, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4034, 'Unknown_datum_based_upon_the_Clarke_1880_ellipsoid',
                        6034, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4035, 'Unknown_datum_based_upon_the_Authalic_Sphere',
                        6035, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4036, 'Unknown_datum_based_upon_the_GRS_1967_ellipsoid',
                        6036, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4041, 'Unknown_datum_based_upon_the_Average_Terrestrial_System_1977_ellipsoid',
                        6041, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4042, 'Unknown_datum_based_upon_the_Everest_1830_Definition_ellipsoid',
                        6042, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4043, 'Unknown_datum_based_upon_the_WGS_72_ellipsoid',
                        6043, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4044, 'Unknown_datum_based_upon_the_Everest_1830_1962_Definition_ellipsoid',
                        6044, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4045, 'Unknown_datum_based_upon_the_Everest_1830_1975_Definition_ellipsoid',
                        6045, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4046, 'RGRDC_2005',
                        1033, 8901, 9122,
                        True, 3613
              );
  _cs.Add     (   4047, 'Unspecified_datum_based_upon_the_GRS_1980_Authalic_Sphere',
                        6047, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4052, 'Unspecified_datum_based_upon_the_Clarke_1866_Authalic_Sphere',
                        6052, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4053, 'Unspecified_datum_based_upon_the_International_1924_Authalic_Sphere',
                        6053, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4054, 'Unspecified_datum_based_upon_the_Hughes_1980_ellipsoid',
                        6054, 8901, 9122,
                        True, 1263
              )         .MarkDeprecated( True );
  _cs.Add     (   4055, 'Popular_Visualisation_CRS',
                        6055, 8901, 9122,
                        True, 1262
              )         .MarkDeprecated( True );
  _cs.Add     (   4075, 'SREF98',
                        1034, 8901, 9122,
                        True, 4543
              );
  _cs.Add     (   4081, 'REGCAN95',
                        1035, 8901, 9122,
                        True, 3199
              );
  _cs.Add     (   4120, 'Greek',
                        6120, 8901, 9122,
                        True, 3254
              );
  _cs.Add     (   4121, 'GGRS87',
                        6121, 8901, 9122,
                        True, 3254
              );
  _cs.Add     (   4122, 'ATS77',
                        6122, 8901, 9122,
                        True, 1283
              );
  _cs.Add     (   4123, 'KKJ',
                        6123, 8901, 9122,
                        True, 3333
              );
  _cs.Add     (   4124, 'RT90',
                        6124, 8901, 9122,
                        True, 1225
              );
  _cs.Add     (   4125, 'Samboja',
                        6125, 8901, 9122,
                        True, 1328
              )         .MarkDeprecated( True );
  _cs.Add     (   4126, 'LKS94_ETRS89',
                        6126, 8901, 9122,
                        True, 1145
              )         .MarkDeprecated( True );
  _cs.Add     (   4127, 'Tete',
                        6127, 8901, 9122,
                        True, 3281
              );
  _cs.Add     (   4128, 'Madzansua',
                        6128, 8901, 9122,
                        True, 1315
              );
  _cs.Add     (   4129, 'Observatario',
                        6129, 8901, 9122,
                        True, 1329
              );
  _cs.Add     (   4130, 'Moznet',
                        6130, 8901, 9122,
                        True, 1167
              );
  _cs.Add     (   4131, 'Indian_1960',
                        6131, 8901, 9122,
                        True, 4007
              );
  _cs.Add     (   4132, 'FD58',
                        6132, 8901, 9122,
                        True, 1300
              );
  _cs.Add     (   4133, 'EST92',
                        6133, 8901, 9122,
                        True, 3246
              );
  _cs.Add     (   4134, 'PSD93',
                        6134, 8901, 9122,
                        True, 3288
              );
  _cs.Add     (   4135, 'Old_Hawaiian',
                        6135, 8901, 9122,
                        True, 1334
              );
  _cs.Add     (   4136, 'St_Lawrence_Island',
                        6136, 8901, 9122,
                        True, 1332
              );
  _cs.Add     (   4137, 'St_Paul_Island',
                        6137, 8901, 9122,
                        True, 1333
              );
  _cs.Add     (   4138, 'St_George_Island',
                        6138, 8901, 9122,
                        True, 1331
              );
  _cs.Add     (   4139, 'Puerto_Rico',
                        6139, 8901, 9122,
                        True, 1335
              );
  _cs.Add     (   4140, 'NAD83_CSRS98',
                        6140, 8901, 9122,
                        True, 1336
              )         .MarkDeprecated( True );
  _cs.Add     (   4141, 'Israel_1993',
                        6141, 8901, 9122,
                        True, 2603
              );
  _cs.Add     (   4142, 'Locodjo_1965',
                        6142, 8901, 9122,
                        True, 1075
              );
  _cs.Add     (   4143, 'Abidjan_1987',
                        6143, 8901, 9122,
                        True, 1075
              );
  _cs.Add     (   4144, 'Kalianpur_1937',
                        6144, 8901, 9122,
                        True, 1308
              );
  _cs.Add     (   4145, 'Kalianpur_1962',
                        6145, 8901, 9122,
                        True, 1184
              );
  _cs.Add     (   4146, 'Kalianpur_1975',
                        6146, 8901, 9122,
                        True, 3341
              );
  _cs.Add     (   4147, 'Hanoi_1972',
                        6147, 8901, 9122,
                        True, 3328
              );
  _cs.Add     (   4148, 'Hartebeesthoek94',
                        6148, 8901, 9122,
                        True, 4540
              );
  _cs.Add     (   4149, 'CH1903',
                        6149, 8901, 9122,
                        True, 1286
              );
  _cs.Add     (   4150, 'CH1903+',
                        6150, 8901, 9122,
                        True, 1286
              );
  _cs.Add     (   4151, 'CHTRS95',
                        6151, 8901, 9122,
                        True, 1286
              );
  _cs.Add     (   4152, 'NAD83_HARN',
                        6152, 8901, 9122,
                        True, 1337
              );
  _cs.Add     (   4153, 'Rassadiran',
                        6153, 8901, 9122,
                        True, 1338
              );
  _cs.Add     (   4154, 'ED50_ED77',
                        6154, 8901, 9122,
                        True, 1123
              );
  _cs.Add     (   4155, 'Dabola_1981',
                        6155, 8901, 9122,
                        True, 3257
              );
  _cs.Add     (   4156, 'S_JTSK',
                        6156, 8901, 9122,
                        True, 1306
              );
  _cs.Add     (   4157, 'Mount_Dillon',
                        6157, 8901, 9122,
                        True, 1322
              );
  _cs.Add     (   4158, 'Naparima_1955',
                        6158, 8901, 9122,
                        True, 3143
              );
  _cs.Add     (   4159, 'ELD79',
                        6159, 8901, 9122,
                        True, 1143
              );
  _cs.Add     (   4160, 'Chos_Malal_1914',
                        6160, 8901, 9122,
                        True, 4562
              );
  _cs.Add     (   4161, 'Pampa_del_Castillo',
                        6161, 8901, 9122,
                        True, 4563
              );
  _cs.Add     (   4162, 'Korean_1985',
                        6162, 8901, 9122,
                        True, 3266
              );
  _cs.Add     (   4163, 'Yemen_NGN96',
                        6163, 8901, 9122,
                        True, 1257
              );
  _cs.Add     (   4164, 'South_Yemen',
                        6164, 8901, 9122,
                        True, 1340
              );
  _cs.Add     (   4165, 'Bissau',
                        6165, 8901, 9122,
                        True, 3258
              );
  _cs.Add     (   4166, 'Korean_1995',
                        6166, 8901, 9122,
                        True, 3266
              );
  _cs.Add     (   4167, 'NZGD2000',
                        6167, 8901, 9122,
                        True, 1175
              );
end;

procedure prepare_2;
begin
  _cs.Add     (   4168, 'Accra',
                        6168, 8901, 9122,
                        True, 1104
              );
  _cs.Add     (   4169, 'American_Samoa_1962',
                        6169, 8901, 9122,
                        True, 3109
              );
  _cs.Add     (   4170, 'SIRGAS_1995',
                        6170, 8901, 9122,
                        True, 3448
              );
  _cs.Add     (   4171, 'RGF93_v1',
                        6171, 8901, 9122,
                        True, 1096
              );
  _cs.Add     (   4172, 'POSGAR',
                        6172, 8901, 9122,
                        True, 1033
              )         .MarkDeprecated( True );
  _cs.Add     (   4173, 'IRENET95',
                        6173, 8901, 9122,
                        True, 1305
              );
  _cs.Add     (   4174, 'Sierra_Leone_1924',
                        6174, 8901, 9122,
                        True, 1342
              );
  _cs.Add     (   4175, 'Sierra_Leone_1968',
                        6175, 8901, 9122,
                        True, 3306
              );
  _cs.Add     (   4176, 'Australian_Antarctic',
                        6176, 8901, 9122,
                        True, 1278
              );
  _cs.Add     (   4178, 'Pulkovo_1942_83',
                        6178, 8901, 9122,
                        True, 3900
              );
  _cs.Add     (   4179, 'Pulkovo_1942_58',
                        6179, 8901, 9122,
                        True, 3574
              );
  _cs.Add     (   4180, 'EST97',
                        6180, 8901, 9122,
                        True, 1090
              );
  _cs.Add     (   4181, 'LUREF',
                        6181, 8901, 9122,
                        True, 1146
              );
  _cs.Add     (   4182, 'Azores_Occidental_1939',
                        6182, 8901, 9122,
                        True, 1344
              );
  _cs.Add     (   4183, 'Azores_Central_1948',
                        6183, 8901, 9122,
                        True, 1301
              );
  _cs.Add     (   4184, 'Azores_Oriental_1940',
                        6184, 8901, 9122,
                        True, 1345
              );
  _cs.Add     (   4185, 'Madeira_1936',
                        6185, 8901, 9122,
                        True, 1314
              )         .MarkDeprecated( True );
  _cs.Add     (   4188, 'OSNI_1952',
                        6188, 8901, 9122,
                        True, 2530
              );
  _cs.Add     (   4189, 'REGVEN',
                        6189, 8901, 9122,
                        True, 1251
              );
  _cs.Add     (   4190, 'POSGAR_98',
                        6190, 8901, 9122,
                        True, 1033
              );
  _cs.Add     (   4191, 'Albanian_1987',
                        6191, 8901, 9122,
                        True, 3212
              );
  _cs.Add     (   4192, 'Douala_1948',
                        6192, 8901, 9122,
                        True, 2555
              );
  _cs.Add     (   4193, 'Manoca_1962',
                        6193, 8901, 9122,
                        True, 2555
              );
  _cs.Add     (   4194, 'Qornoq_1927',
                        6194, 8901, 9122,
                        True, 3362
              );
  _cs.Add     (   4195, 'Scoresbysund_1952',
                        6195, 8901, 9122,
                        True, 2570
              );
  _cs.Add     (   4196, 'Ammassalik_1958',
                        6196, 8901, 9122,
                        True, 2571
              );
  _cs.Add     (   4197, 'Garoua',
                        6197, 8901, 9122,
                        True, 2590
              );
  _cs.Add     (   4198, 'Kousseri',
                        6198, 8901, 9122,
                        True, 2591
              );
  _cs.Add     (   4199, 'Egypt_1930',
                        6199, 8901, 9122,
                        True, 3242
              );
  _cs.Add     (   4200, 'Pulkovo_1995',
                        6200, 8901, 9122,
                        True, 1198
              );
  _cs.Add     (   4201, 'Adindan',
                        6201, 8901, 9122,
                        True, 1271
              );
  _cs.Add     (   4202, 'AGD66',
                        6202, 8901, 9122,
                        True, 1279
              );
  _cs.Add     (   4203, 'AGD84',
                        6203, 8901, 9122,
                        True, 2576
              );
  _cs.Add     (   4204, 'Ain_el_Abd',
                        6204, 8901, 9122,
                        True, 1272
              );
  _cs.Add     (   4205, 'Afgooye',
                        6205, 8901, 9122,
                        True, 3308
              );
  _cs.Add     (   4206, 'Agadez',
                        6206, 8901, 9122,
                        True, 1177
              );
  _cs.Add     (   4207, 'Lisbon',
                        6207, 8901, 9122,
                        True, 1294
              );
  _cs.Add     (   4208, 'Aratu',
                        6208, 8901, 9122,
                        True, 1274
              );
  _cs.Add     (   4209, 'Arc_1950',
                        6209, 8901, 9122,
                        True, 1276
              );
  _cs.Add     (   4210, 'Arc_1960',
                        6210, 8901, 9122,
                        True, 1277
              );
  _cs.Add     (   4211, 'Batavia',
                        6211, 8901, 9122,
                        True, 3666
              );
  _cs.Add     (   4212, 'Barbados_1938',
                        6212, 8901, 9122,
                        True, 3218
              );
  _cs.Add     (   4213, 'Beduaram',
                        6213, 8901, 9122,
                        True, 2771
              );
  _cs.Add     (   4214, 'Beijing_1954',
                        6214, 8901, 9122,
                        True, 1067
              );
  _cs.Add     (   4215, 'BD50',
                        6215, 8901, 9122,
                        True, 1347
              );
  _cs.Add     (   4216, 'Bermuda_1957',
                        6216, 8901, 9122,
                        True, 3221
              );
  _cs.Add     (   4218, 'Bogota_1975',
                        6218, 8901, 9122,
                        True, 3686
              );
  _cs.Add     (   4219, 'Bukit_Rimpah',
                        6219, 8901, 9122,
                        True, 1287
              );
  _cs.Add     (   4220, 'Camacupa_1948',
                        6220, 8901, 9122,
                        True, 1288
              );
  _cs.Add     (   4221, 'Campo_Inchauspe',
                        6221, 8901, 9122,
                        True, 3843
              );
  _cs.Add     (   4222, 'Cape',
                        6222, 8901, 9122,
                        True, 1290
              );
  _cs.Add     (   4223, 'Carthage',
                        6223, 8901, 9122,
                        True, 1236
              );
  _cs.Add     (   4224, 'Chua',
                        6224, 8901, 9122,
                        True, 3356
              );
  _cs.Add     (   4225, 'Corrego_Alegre_1970_72',
                        6225, 8901, 9122,
                        True, 1293
              );
  _cs.Add     (   4226, 'Cote_d_Ivoire',
                        6226, 8901, 9122,
                        True, 1075
              )         .MarkDeprecated( True );
  _cs.Add     (   4227, 'Deir_ez_Zor',
                        6227, 8901, 9122,
                        True, 1623
              );
  _cs.Add     (   4228, 'Douala',
                        6228, 8901, 9122,
                        True, 1060
              )         .MarkDeprecated( True );
  _cs.Add     (   4229, 'Egypt_1907',
                        6229, 8901, 9122,
                        True, 1086
              );
  _cs.Add     (   4230, 'ED50',
                        6230, 8901, 9122,
                        True, 1296
              );
  _cs.Add     (   4231, 'ED87',
                        6231, 8901, 9122,
                        True, 1297
              );
  _cs.Add     (   4232, 'Fahud',
                        6232, 8901, 9122,
                        True, 4009
              );
  _cs.Add     (   4233, 'Gandajika_1970',
                        6233, 8901, 9122,
                        True, 1152
              )         .MarkDeprecated( True );
  _cs.Add     (   4234, 'Garoua_A',
                        6234, 8901, 9122,
                        True, 1060
              )         .MarkDeprecated( True );
  _cs.Add     (   4235, 'Guyane_Francaise',
                        6235, 8901, 9122,
                        True, 1097
              )         .MarkDeprecated( True );
  _cs.Add     (   4236, 'Hu_Tzu_Shan_1950',
                        6236, 8901, 9122,
                        True, 3315
              );
  _cs.Add     (   4237, 'HD72',
                        6237, 8901, 9122,
                        True, 1119
              );
  _cs.Add     (   4238, 'ID74',
                        6238, 8901, 9122,
                        True, 4020
              );
  _cs.Add     (   4239, 'Indian_1954',
                        6239, 8901, 9122,
                        True, 1304
              );
  _cs.Add     (   4240, 'Indian_1975',
                        6240, 8901, 9122,
                        True, 3741
              );
  _cs.Add     (   4241, 'Jamaica_1875',
                        6241, 8901, 9122,
                        True, 3342
              );
  _cs.Add     (   4242, 'JAD69',
                        6242, 8901, 9122,
                        True, 3342
              );
  _cs.Add     (   4243, 'Kalianpur_1880',
                        6243, 8901, 9122,
                        True, 1307
              );
  _cs.Add     (   4244, 'Kandawala',
                        6244, 8901, 9122,
                        True, 3310
              );
  _cs.Add     (   4245, 'Kertau_1968',
                        6245, 8901, 9122,
                        True, 4223
              );
  _cs.Add     (   4246, 'KOC',
                        6246, 8901, 9122,
                        True, 3267
              );
  _cs.Add     (   4247, 'La_Canoa',
                        6247, 8901, 9122,
                        True, 3327
              );
  _cs.Add     (   4248, 'PSAD56',
                        6248, 8901, 9122,
                        True, 1348
              );
  _cs.Add     (   4249, 'Lake',
                        6249, 8901, 9122,
                        True, 1312
              );
  _cs.Add     (   4250, 'Leigon',
                        6250, 8901, 9122,
                        True, 1104
              );
  _cs.Add     (   4251, 'Liberia_1964',
                        6251, 8901, 9122,
                        True, 3270
              );
  _cs.Add     (   4252, 'Lome',
                        6252, 8901, 9122,
                        True, 1232
              );
  _cs.Add     (   4253, 'Luzon_1911',
                        6253, 8901, 9122,
                        True, 3969
              );
  _cs.Add     (   4254, 'Hito_XVIII_1963',
                        6254, 8901, 9122,
                        True, 1303
              );
  _cs.Add     (   4255, 'Herat_North',
                        6255, 8901, 9122,
                        True, 1024
              );
  _cs.Add     (   4256, 'Mahe_1971',
                        6256, 8901, 9122,
                        True, 2369
              );
  _cs.Add     (   4257, 'Makassar',
                        6257, 8901, 9122,
                        True, 1316
              );
  _cs.Add     (   4258, 'ETRS89',
                        6258, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   4259, 'Malongo_1987',
                        6259, 8901, 9122,
                        True, 3180
              );
  _cs.Add     (   4260, 'Manoca',
                        6260, 8901, 9122,
                        True, 1060
              )         .MarkDeprecated( True );
  _cs.Add     (   4261, 'Merchich',
                        6261, 8901, 9122,
                        True, 4581
              );
  _cs.Add     (   4262, 'Massawa',
                        6262, 8901, 9122,
                        True, 1089
              );
  _cs.Add     (   4263, 'Minna',
                        6263, 8901, 9122,
                        True, 1178
              );
  _cs.Add     (   4264, 'Mhast',
                        6264, 8901, 9122,
                        True, 1318
              )         .MarkDeprecated( True );
  _cs.Add     (   4265, 'Monte_Mario',
                        6265, 8901, 9122,
                        True, 3343
              );
  _cs.Add     (   4266, 'M_poraloko',
                        6266, 8901, 9122,
                        True, 1100
              );
  _cs.Add     (   4267, 'NAD27',
                        6267, 8901, 9122,
                        True, 1349
              );
  _cs.Add     (   4268, 'NAD27_Michigan',
                        6268, 8901, 9122,
                        True, 1391
              )         .MarkDeprecated( True );
  _cs.Add     (   4269, 'NAD83',
                        6269, 8901, 9122,
                        True, 1350
              );
  _cs.Add     (   4270, 'Nahrwan_1967',
                        6270, 8901, 9122,
                        True, 1351
              );
  _cs.Add     (   4271, 'Naparima_1972',
                        6271, 8901, 9122,
                        True, 1322
              );
end;

procedure prepare_3;
begin
  _cs.Add     (   4272, 'NZGD49',
                        6272, 8901, 9122,
                        True, 3285
              );
  _cs.Add     (   4273, 'NGO_1948',
                        6273, 8901, 9122,
                        True, 1352
              );
  _cs.Add     (   4274, 'Datum_73',
                        6274, 8901, 9122,
                        True, 1294
              );
  _cs.Add     (   4275, 'NTF',
                        6275, 8901, 9122,
                        True, 3694
              );
  _cs.Add     (   4276, 'NSWC_9Z_2',
                        6276, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4277, 'OSGB36',
                        6277, 8901, 9122,
                        True, 4390
              );
  _cs.Add     (   4278, 'OSGB70',
                        6278, 8901, 9122,
                        True, 1264
              );
  _cs.Add     (   4279, 'OS_SN_80',
                        6279, 8901, 9122,
                        True, 1354
              );
  _cs.Add     (   4280, 'Padang',
                        6280, 8901, 9122,
                        True, 1355
              )         .MarkDeprecated( True );
  _cs.Add     (   4281, 'Palestine_1923',
                        6281, 8901, 9122,
                        True, 1356
              );
  _cs.Add     (   4282, 'Pointe_Noire',
                        6282, 8901, 9122,
                        True, 1072
              );
  _cs.Add     (   4283, 'GDA94',
                        6283, 8901, 9122,
                        True, 4177
              );
  _cs.Add     (   4284, 'Pulkovo_1942',
                        6284, 8901, 9122,
                        True, 2423
              );
  _cs.Add     (   4285, 'Qatar_1974',
                        6285, 8901, 9122,
                        True, 1195
              );
  _cs.Add     (   4286, 'Qatar_1948',
                        6286, 8901, 9122,
                        True, 1346
              );
  _cs.Add     (   4287, 'Qornoq',
                        6287, 8901, 9122,
                        True, 1107
              )         .MarkDeprecated( True );
  _cs.Add     (   4288, 'Loma_Quintana',
                        6288, 8901, 9122,
                        True, 1313
              );
  _cs.Add     (   4289, 'Amersfoort',
                        6289, 8901, 9122,
                        True, 1275
              );
  _cs.Add     (   4291, 'SAD69',
                        6291, 8901, 9122,
                        True, 1358
              )         .MarkDeprecated( True );
  _cs.Add     (   4292, 'Sapper_Hill_1943',
                        6292, 8901, 9122,
                        True, 3247
              );
  _cs.Add     (   4293, 'Schwarzeck',
                        6293, 8901, 9122,
                        True, 1169
              );
  _cs.Add     (   4294, 'Segora',
                        6294, 8901, 9122,
                        True, 1359
              )         .MarkDeprecated( True );
  _cs.Add     (   4295, 'Serindung',
                        6295, 8901, 9122,
                        True, 4005
              );
  _cs.Add     (   4296, 'Sudan',
                        6296, 8901, 9122,
                        True, 1361
              )         .MarkDeprecated( True );
  _cs.Add     (   4297, 'Tananarive',
                        6297, 8901, 9122,
                        True, 1149
              );
  _cs.Add     (   4298, 'Timbalai_1948',
                        6298, 8901, 9122,
                        True, 1362
              );
  _cs.Add     (   4299, 'TM65',
                        6299, 8901, 9122,
                        True, 1305
              );
  _cs.Add     (   4300, 'TM75',
                        6300, 8901, 9122,
                        True, 1305
              );
  _cs.Add     (   4301, 'Tokyo',
                        6301, 8901, 9122,
                        True, 1364
              );
  _cs.Add     (   4302, 'Trinidad_1903',
                        6302, 8901, 9122,
                        True, 1339
              );
  _cs.Add     (   4303, 'TC_1948',
                        6303, 8901, 9122,
                        True, 1363
              );
  _cs.Add     (   4304, 'Voirol_1875',
                        6304, 8901, 9122,
                        True, 1365
              );
  _cs.Add     (   4306, 'Bern_1938',
                        6306, 8901, 9122,
                        True, 1286
              );
  _cs.Add     (   4307, 'Nord_Sahara_1959',
                        6307, 8901, 9122,
                        True, 1026
              );
  _cs.Add     (   4308, 'RT38',
                        6308, 8901, 9122,
                        True, 3313
              );
  _cs.Add     (   4309, 'Yacare',
                        6309, 8901, 9122,
                        True, 3326
              );
  _cs.Add     (   4310, 'Yoff',
                        6310, 8901, 9122,
                        True, 1207
              );
  _cs.Add     (   4311, 'Zanderij',
                        6311, 8901, 9122,
                        True, 1222
              );
  _cs.Add     (   4312, 'MGI',
                        6312, 8901, 9122,
                        True, 1037
              );
  _cs.Add     (   4313, 'BD72',
                        6313, 8901, 9122,
                        True, 1347
              );
  _cs.Add     (   4314, 'DHDN',
                        6314, 8901, 9122,
                        True, 2326
              );
  _cs.Add     (   4315, 'Conakry_1905',
                        6315, 8901, 9122,
                        True, 3257
              );
  _cs.Add     (   4316, 'Dealul_Piscului_1930',
                        6316, 8901, 9122,
                        True, 3295
              );
  _cs.Add     (   4317, 'Dealul_Piscului_1970',
                        6317, 8901, 9122,
                        True, 1197
              )         .MarkDeprecated( True );
  _cs.Add     (   4318, 'NGN',
                        6318, 8901, 9122,
                        True, 3267
              );
  _cs.Add     (   4319, 'KUDAMS',
                        6319, 8901, 9122,
                        True, 1310
              );
  _cs.Add     (   4322, 'WGS_72',
                        6322, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4324, 'WGS_72BE',
                        6324, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4326, 'WGS_84',
                        6326, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4463, 'RGSPM06',
                        1038, 8901, 9122,
                        True, 1220
              );
  _cs.Add     (   4470, 'RGM04',
                        1036, 8901, 9122,
                        True, 1159
              );
  _cs.Add     (   4475, 'Cadastre_1997',
                        1037, 8901, 9122,
                        True, 3340
              );
  _cs.Add     (   4483, 'Mexico_ITRF92',
                        1042, 8901, 9122,
                        True, 1160
              );
  _cs.Add     (   4490, 'China_Geodetic_Coordinate_System_2000',
                        1043, 8901, 9122,
                        True, 1067
              );
  _cs.Add     (   4555, 'New_Beijing',
                        1045, 8901, 9122,
                        True, 3228
              );
  _cs.Add     (   4558, 'RRAF_1991',
                        1047, 8901, 9122,
                        True, 2824
              );
  _cs.Add     (   4600, 'Anguilla_1957',
                        6600, 8901, 9122,
                        True, 3214
              );
  _cs.Add     (   4601, 'Antigua_1943',
                        6601, 8901, 9122,
                        True, 1273
              );
  _cs.Add     (   4602, 'Dominica_1945',
                        6602, 8901, 9122,
                        True, 3239
              );
  _cs.Add     (   4603, 'Grenada_1953',
                        6603, 8901, 9122,
                        True, 1551
              );
  _cs.Add     (   4604, 'Montserrat_1958',
                        6604, 8901, 9122,
                        True, 3279
              );
  _cs.Add     (   4605, 'St_Kitts_1955',
                        6605, 8901, 9122,
                        True, 3297
              );
  _cs.Add     (   4606, 'St_Lucia_1955',
                        6606, 8901, 9122,
                        True, 3298
              );
  _cs.Add     (   4607, 'St_Vincent_1945',
                        6607, 8901, 9122,
                        True, 3300
              );
  _cs.Add     (   4608, 'NAD27_76',
                        6608, 8901, 9122,
                        True, 1367
              );
  _cs.Add     (   4609, 'NAD27_CGQ77',
                        6609, 8901, 9122,
                        True, 1368
              );
  _cs.Add     (   4610, 'Xian_1980',
                        6610, 8901, 9122,
                        True, 3228
              );
  _cs.Add     (   4611, 'Hong_Kong_1980',
                        6611, 8901, 9122,
                        True, 1118
              );
  _cs.Add     (   4612, 'JGD2000',
                        6612, 8901, 9122,
                        True, 1129
              );
  _cs.Add     (   4613, 'Segara',
                        6613, 8901, 9122,
                        True, 1360
              );
  _cs.Add     (   4614, 'QND95',
                        6614, 8901, 9122,
                        True, 1346
              );
  _cs.Add     (   4615, 'Porto_Santo',
                        6615, 8901, 9122,
                        True, 1314
              );
  _cs.Add     (   4616, 'Selvagem_Grande',
                        6616, 8901, 9122,
                        True, 2779
              );
  _cs.Add     (   4617, 'NAD83_CSRS',
                        6140, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   4618, 'SAD69_A',
                        6618, 8901, 9122,
                        True, 1358
              );
  _cs.Add     (   4619, 'SWEREF99',
                        6619, 8901, 9122,
                        True, 1225
              );
  _cs.Add     (   4620, 'Point_58',
                        6620, 8901, 9122,
                        True, 2790
              );
  _cs.Add     (   4621, 'Fort_Marigot',
                        6621, 8901, 9122,
                        True, 2828
              );
  _cs.Add     (   4622, 'Guadeloupe_1948',
                        6622, 8901, 9122,
                        True, 2829
              );
  _cs.Add     (   4623, 'CSG67',
                        6623, 8901, 9122,
                        True, 3105
              );
  _cs.Add     (   4624, 'RGFG95',
                        6624, 8901, 9122,
                        True, 1097
              );
  _cs.Add     (   4625, 'Martinique_1938',
                        6625, 8901, 9122,
                        True, 3276
              );
  _cs.Add     (   4626, 'Reunion_1947',
                        6626, 8901, 9122,
                        True, 3337
              );
  _cs.Add     (   4627, 'RGR92',
                        6627, 8901, 9122,
                        True, 3902
              );
  _cs.Add     (   4628, 'Tahiti_52',
                        6628, 8901, 9122,
                        True, 2811
              );
  _cs.Add     (   4629, 'Tahaa_54',
                        6629, 8901, 9122,
                        True, 2812
              );
  _cs.Add     (   4630, 'IGN72_Nuku_Hiva',
                        6630, 8901, 9122,
                        True, 3129
              );
  _cs.Add     (   4631, 'K0_1949',
                        6631, 8901, 9122,
                        True, 2816
              )         .MarkDeprecated( True );
  _cs.Add     (   4632, 'Combani_1950',
                        6632, 8901, 9122,
                        True, 3340
              );
  _cs.Add     (   4633, 'IGN56_Lifou',
                        6633, 8901, 9122,
                        True, 2814
              );
  _cs.Add     (   4634, 'IGN72_Grand_Terre',
                        6634, 8901, 9122,
                        True, 2822
              )         .MarkDeprecated( True );
  _cs.Add     (   4635, 'ST87_Ouvea',
                        6635, 8901, 9122,
                        True, 2813
              )         .MarkDeprecated( True );
  _cs.Add     (   4636, 'Petrels_1972',
                        6636, 8901, 9122,
                        True, 2817
              );
  _cs.Add     (   4637, 'Perroud_1950',
                        6637, 8901, 9122,
                        True, 2818
              );
  _cs.Add     (   4638, 'Saint_Pierre_et_Miquelon_1950',
                        6638, 8901, 9122,
                        True, 3299
              );
  _cs.Add     (   4639, 'MOP78',
                        6639, 8901, 9122,
                        True, 2815
              );
  _cs.Add     (   4640, 'RRAF_1991_A',
                        6640, 8901, 9122,
                        True, 2824
              )         .MarkDeprecated( True );
  _cs.Add     (   4641, 'IGN53_Mare',
                        6641, 8901, 9122,
                        True, 2819
              );
  _cs.Add     (   4642, 'ST84_Ile_des_Pins',
                        6642, 8901, 9122,
                        True, 2820
              );
  _cs.Add     (   4643, 'ST71_Belep',
                        6643, 8901, 9122,
                        True, 2821
              );
end;

procedure prepare_4;
begin
  _cs.Add     (   4644, 'NEA74_Noumea',
                        6644, 8901, 9122,
                        True, 2823
              );
  _cs.Add     (   4645, 'RGNC_1991',
                        6645, 8901, 9122,
                        True, 1174
              )         .MarkDeprecated( True );
  _cs.Add     (   4646, 'Grand_Comoros',
                        6646, 8901, 9122,
                        True, 2807
              );
  _cs.Add     (   4657, 'Reykjavik_1900',
                        6657, 8901, 9122,
                        True, 3262
              );
  _cs.Add     (   4658, 'Hjorsey_1955',
                        6658, 8901, 9122,
                        True, 3262
              );
  _cs.Add     (   4659, 'ISN93',
                        6659, 8901, 9122,
                        True, 1120
              );
  _cs.Add     (   4660, 'Helle_1954',
                        6660, 8901, 9122,
                        True, 2869
              );
  _cs.Add     (   4661, 'LKS92',
                        6661, 8901, 9122,
                        True, 1139
              );
  _cs.Add     (   4662, 'IGN72_Grande_Terre',
                        6634, 8901, 9122,
                        True, 2822
              );
  _cs.Add     (   4663, 'Porto_Santo_1995',
                        6663, 8901, 9122,
                        True, 1314
              );
  _cs.Add     (   4664, 'Azores_Oriental_1995',
                        6664, 8901, 9122,
                        True, 1345
              );
  _cs.Add     (   4665, 'Azores_Central_1995',
                        6665, 8901, 9122,
                        True, 1301
              );
  _cs.Add     (   4666, 'Lisbon_1890',
                        6666, 8901, 9122,
                        True, 1294
              );
  _cs.Add     (   4667, 'IKBD_92',
                        6667, 8901, 9122,
                        True, 2876
              );
  _cs.Add     (   4668, 'ED79',
                        6668, 8901, 9122,
                        True, 1297
              );
  _cs.Add     (   4669, 'LKS94',
                        6126, 8901, 9122,
                        True, 1145
              );
  _cs.Add     (   4670, 'IGM95',
                        6670, 8901, 9122,
                        True, 3343
              );
  _cs.Add     (   4671, 'Voirol_1879',
                        6671, 8901, 9122,
                        True, 1365
              );
  _cs.Add     (   4672, 'Chatham_Islands_1971',
                        6672, 8901, 9122,
                        True, 2889
              );
  _cs.Add     (   4673, 'Chatham_Islands_1979',
                        6673, 8901, 9122,
                        True, 2889
              );
  _cs.Add     (   4674, 'SIRGAS_2000',
                        6674, 8901, 9122,
                        True, 3418
              );
  _cs.Add     (   4675, 'Guam_1963',
                        6675, 8901, 9122,
                        True, 4525
              );
  _cs.Add     (   4676, 'Vientiane_1982',
                        6676, 8901, 9122,
                        True, 1138
              );
  _cs.Add     (   4677, 'Lao_1993',
                        6677, 8901, 9122,
                        True, 1138
              );
  _cs.Add     (   4678, 'Lao_1997',
                        6678, 8901, 9122,
                        True, 1138
              );
  _cs.Add     (   4679, 'Jouik_1961',
                        6679, 8901, 9122,
                        True, 2967
              );
  _cs.Add     (   4680, 'Nouakchott_1965',
                        6680, 8901, 9122,
                        True, 2968
              );
  _cs.Add     (   4681, 'Mauritania_1999',
                        6681, 8901, 9122,
                        True, 1157
              )         .MarkDeprecated( True );
  _cs.Add     (   4682, 'Gulshan_303',
                        6682, 8901, 9122,
                        True, 1041
              );
  _cs.Add     (   4683, 'PRS92',
                        6683, 8901, 9122,
                        True, 1190
              );
  _cs.Add     (   4684, 'Gan_1970',
                        6684, 8901, 9122,
                        True, 3274
              );
  _cs.Add     (   4685, 'Gandajika',
                        6685, 8901, 9122,
                        True, 1259
              )         .MarkDeprecated( True );
  _cs.Add     (   4686, 'MAGNA_SIRGAS',
                        6686, 8901, 9122,
                        True, 1070
              );
  _cs.Add     (   4687, 'RGPF',
                        6687, 8901, 9122,
                        True, 1098
              );
  _cs.Add     (   4688, 'Fatu_Iva_72',
                        6688, 8901, 9122,
                        True, 3133
              );
  _cs.Add     (   4689, 'IGN63_Hiva_Oa',
                        6689, 8901, 9122,
                        True, 3130
              );
  _cs.Add     (   4690, 'Tahiti_79',
                        6690, 8901, 9122,
                        True, 3124
              );
  _cs.Add     (   4691, 'Moorea_87',
                        6691, 8901, 9122,
                        True, 3125
              );
  _cs.Add     (   4692, 'Maupiti_83',
                        6692, 8901, 9122,
                        True, 3126
              );
  _cs.Add     (   4693, 'Nakhl_e_Ghanem',
                        6693, 8901, 9122,
                        True, 2362
              );
  _cs.Add     (   4694, 'POSGAR_94',
                        6694, 8901, 9122,
                        True, 1033
              );
  _cs.Add     (   4695, 'Katanga_1955',
                        6695, 8901, 9122,
                        True, 3147
              );
  _cs.Add     (   4696, 'Kasai_1953',
                        6696, 8901, 9122,
                        True, 3148
              );
  _cs.Add     (   4697, 'IGC_1962_6th_Parallel_South',
                        6697, 8901, 9122,
                        True, 3149
              );
  _cs.Add     (   4698, 'IGN_1962_Kerguelen',
                        6698, 8901, 9122,
                        True, 2816
              );
  _cs.Add     (   4699, 'Le_Pouce_1934',
                        6699, 8901, 9122,
                        True, 3209
              );
  _cs.Add     (   4700, 'IGN_Astro_1960',
                        6700, 8901, 9122,
                        True, 3277
              );
  _cs.Add     (   4701, 'IGCB_1955',
                        6701, 8901, 9122,
                        True, 3171
              );
  _cs.Add     (   4702, 'Mauritania_1999_A',
                        6702, 8901, 9122,
                        True, 1157
              );
  _cs.Add     (   4703, 'Mhast_1951',
                        6703, 8901, 9122,
                        True, 1318
              );
  _cs.Add     (   4704, 'Mhast_onshore',
                        6704, 8901, 9122,
                        True, 3179
              );
  _cs.Add     (   4705, 'Mhast_offshore',
                        6705, 8901, 9122,
                        True, 3180
              );
  _cs.Add     (   4706, 'Egypt_Gulf_of_Suez_S_650_TL',
                        6706, 8901, 9122,
                        True, 2341
              );
  _cs.Add     (   4707, 'Tern_Island_1961',
                        6707, 8901, 9122,
                        True, 3181
              );
  _cs.Add     (   4708, 'Cocos_Islands_1965',
                        6708, 8901, 9122,
                        True, 1069
              );
  _cs.Add     (   4709, 'Iwo_Jima_1945',
                        6709, 8901, 9122,
                        True, 3200
              );
  _cs.Add     (   4710, 'Astro_DOS_71',
                        6710, 8901, 9122,
                        True, 3183
              );
  _cs.Add     (   4711, 'Marcus_Island_1952',
                        6711, 8901, 9122,
                        True, 1872
              );
  _cs.Add     (   4712, 'Ascension_Island_1958',
                        6712, 8901, 9122,
                        True, 3182
              );
  _cs.Add     (   4713, 'Ayabelle_Lighthouse',
                        6713, 8901, 9122,
                        True, 1081
              );
  _cs.Add     (   4714, 'Bellevue',
                        6714, 8901, 9122,
                        True, 3193
              );
  _cs.Add     (   4715, 'Camp_Area_Astro',
                        6715, 8901, 9122,
                        True, 3205
              );
  _cs.Add     (   4716, 'Phoenix_Islands_1966',
                        6716, 8901, 9122,
                        True, 3196
              );
  _cs.Add     (   4717, 'Cape_Canaveral',
                        6717, 8901, 9122,
                        True, 3206
              );
  _cs.Add     (   4718, 'Solomon_1968',
                        6718, 8901, 9122,
                        True, 1213
              );
  _cs.Add     (   4719, 'Easter_Island_1967',
                        6719, 8901, 9122,
                        True, 3188
              );
  _cs.Add     (   4720, 'Fiji_1986',
                        6720, 8901, 9122,
                        True, 1094
              );
  _cs.Add     (   4721, 'Fiji_1956',
                        6721, 8901, 9122,
                        True, 3398
              );
  _cs.Add     (   4722, 'South_Georgia_1968',
                        6722, 8901, 9122,
                        True, 3529
              );
  _cs.Add     (   4723, 'GCGD59',
                        6723, 8901, 9122,
                        True, 3185
              );
  _cs.Add     (   4724, 'Diego_Garcia_1969',
                        6724, 8901, 9122,
                        True, 3189
              );
  _cs.Add     (   4725, 'Johnston_Island_1961',
                        6725, 8901, 9122,
                        True, 3201
              );
  _cs.Add     (   4726, 'SIGD61',
                        6726, 8901, 9122,
                        True, 3186
              );
  _cs.Add     (   4727, 'Midway_1961',
                        6727, 8901, 9122,
                        True, 3202
              );
  _cs.Add     (   4728, 'PN84',
                        6728, 8901, 9122,
                        True, 4598
              );
  _cs.Add     (   4729, 'Pitcairn_1967',
                        6729, 8901, 9122,
                        True, 3208
              );
  _cs.Add     (   4730, 'Santo_1965',
                        6730, 8901, 9122,
                        True, 3194
              );
  _cs.Add     (   4731, 'Viti_Levu_1916',
                        6731, 8901, 9122,
                        True, 3195
              )         .MarkDeprecated( True );
  _cs.Add     (   4732, 'Marshall_Islands_1960',
                        6732, 8901, 9122,
                        True, 3191
              );
  _cs.Add     (   4733, 'Wake_Island_1952',
                        6733, 8901, 9122,
                        True, 3190
              );
  _cs.Add     (   4734, 'Tristan_1968',
                        6734, 8901, 9122,
                        True, 3184
              );
  _cs.Add     (   4735, 'Kusaie_1951',
                        6735, 8901, 9122,
                        True, 3192
              );
  _cs.Add     (   4736, 'Deception_Island',
                        6736, 8901, 9122,
                        True, 3204
              );
  _cs.Add     (   4737, 'Korea_2000',
                        6737, 8901, 9122,
                        True, 1135
              );
  _cs.Add     (   4738, 'Hong_Kong_1963',
                        6738, 8901, 9122,
                        True, 1118
              );
  _cs.Add     (   4739, 'Hong_Kong_1963_67',
                        6739, 8901, 9122,
                        True, 1118
              );
  _cs.Add     (   4740, 'PZ_90',
                        6740, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4741, 'FD54',
                        6741, 8901, 9122,
                        True, 3248
              );
  _cs.Add     (   4742, 'GDM2000',
                        6742, 8901, 9122,
                        True, 1151
              );
  _cs.Add     (   4743, 'Karbala_1979',
                        6743, 8901, 9122,
                        True, 3625
              );
  _cs.Add     (   4744, 'Nahrwan_1934',
                        6744, 8901, 9122,
                        True, 4238
              );
  _cs.Add     (   4745, 'RD_83',
                        6745, 8901, 9122,
                        True, 2545
              );
  _cs.Add     (   4746, 'PD_83',
                        6746, 8901, 9122,
                        True, 2544
              );
  _cs.Add     (   4747, 'GR96',
                        6747, 8901, 9122,
                        True, 1107
              );
  _cs.Add     (   4748, 'Vanua_Levu_1915',
                        6748, 8901, 9122,
                        True, 3401
              );
  _cs.Add     (   4749, 'RGNC91_93',
                        6749, 8901, 9122,
                        True, 1174
              );
  _cs.Add     (   4750, 'ST87_Ouvea_A',
                        6750, 8901, 9122,
                        True, 2813
              );
  _cs.Add     (   4751, 'Kertau_RSO',
                        6751, 8901, 9122,
                        True, 1309
              );
  _cs.Add     (   4752, 'Viti_Levu_1912',
                        6752, 8901, 9122,
                        True, 3195
              );
  _cs.Add     (   4753, 'fk89',
                        6753, 8901, 9122,
                        True, 3248
              );
end;

procedure prepare_5;
begin
  _cs.Add     (   4754, 'LGD2006',
                        6754, 8901, 9122,
                        True, 1143
              );
  _cs.Add     (   4755, 'DGN95',
                        6755, 8901, 9122,
                        True, 1122
              );
  _cs.Add     (   4756, 'VN_2000',
                        6756, 8901, 9122,
                        True, 3328
              );
  _cs.Add     (   4757, 'SVY21',
                        6757, 8901, 9122,
                        True, 1210
              );
  _cs.Add     (   4758, 'JAD2001',
                        6758, 8901, 9122,
                        True, 1128
              );
  _cs.Add     (   4759, 'NAD83_NSRS2007',
                        6759, 8901, 9122,
                        True, 1511
              );
  _cs.Add     (   4760, 'WGS_66',
                        6760, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   4761, 'HTRS96',
                        6761, 8901, 9122,
                        True, 1076
              );
  _cs.Add     (   4762, 'BDA2000',
                        6762, 8901, 9122,
                        True, 1047
              );
  _cs.Add     (   4763, 'Pitcairn_2006',
                        6763, 8901, 9122,
                        True, 3208
              );
  _cs.Add     (   4764, 'RSRGD2000',
                        6764, 8901, 9122,
                        True, 3558
              );
  _cs.Add     (   4765, 'Slovenia_1996',
                        6765, 8901, 9122,
                        True, 1212
              );
  _cs.Add     (   4801, 'Bern_1898_Bern',
                        6801, 8907, 9122,
                        True, 1286
              );
  _cs.Add     (   4802, 'Bogota_1975_Bogota',
                        6802, 8904, 9122,
                        True, 3229
              );
  _cs.Add     (   4803, 'Lisbon_Lisbon',
                        6803, 8902, 9122,
                        True, 1294
              );
  _cs.Add     (   4804, 'Makassar_Jakarta',
                        6804, 8908, 9122,
                        True, 1316
              );
  _cs.Add     (   4805, 'MGI_Ferro',
                        6805, 8909, 9122,
                        True, 1321
              );
  _cs.Add     (   4806, 'Monte_Mario_Rome',
                        6806, 8906, 9122,
                        True, 3343
              );
  _cs.Add     (   4807, 'NTF_Paris',
                        6807, 8903, 9105,
                        True, 3694
              );
  _cs.Add     (   4808, 'Padang_Jakarta',
                        6808, 8908, 9122,
                        True, 1355
              )         .MarkDeprecated( True );
  _cs.Add     (   4809, 'BD50_Brussels',
                        6809, 8910, 9122,
                        True, 1347
              );
  _cs.Add     (   4810, 'Tananarive_Paris',
                        6810, 8903, 9105,
                        True, 3273
              );
  _cs.Add     (   4811, 'Voirol_1875_Paris',
                        6811, 8903, 9105,
                        True, 1365
              );
  _cs.Add     (   4813, 'Batavia_Jakarta',
                        6813, 8908, 9122,
                        True, 1285
              );
  _cs.Add     (   4814, 'RT38_Stockholm',
                        6814, 8911, 9122,
                        True, 3313
              );
  _cs.Add     (   4815, 'Greek_Athens',
                        6815, 8912, 9122,
                        True, 3254
              );
  _cs.Add     (   4816, 'Carthage_Paris',
                        6816, 8903, 9105,
                        True, 1618
              );
  _cs.Add     (   4817, 'NGO_1948_Oslo',
                        6817, 8913, 9122,
                        True, 1352
              );
  _cs.Add     (   4818, 'S_JTSK_Ferro',
                        6818, 8909, 9122,
                        True, 1306
              );
  _cs.Add     (   4819, 'Nord_Sahara_1959_Paris',
                        6819, 8903, 9105,
                        True, 1366
              )         .MarkDeprecated( True );
  _cs.Add     (   4820, 'Segara_Jakarta',
                        6820, 8908, 9122,
                        True, 1360
              );
  _cs.Add     (   4821, 'Voirol_1879_Paris',
                        6821, 8903, 9105,
                        True, 1365
              );
  _cs.Add     (   4823, 'Sao_Tome',
                        1044, 8901, 9122,
                        True, 3645
              );
  _cs.Add     (   4824, 'Principe',
                        1046, 8901, 9122,
                        True, 3646
              );
  _cs.Add     (   4901, 'ATF_Paris',
                        6901, 8903, 9105,
                        True, 1326
              );
  _cs.Add     (   4902, 'NDG_Paris',
                        6902, 8903, 9105,
                        True, 1369
              )         .MarkDeprecated( True );
  _cs.Add     (   4903, 'Madrid_1870_Madrid',
                        6903, 8905, 9122,
                        True, 2366
              );
  _cs.Add     (   4904, 'Lisbon_1890_Lisbon',
                        6904, 8902, 9122,
                        True, 1294
              );
  _cs.Add     (   5013, 'PTRA08',
                        1041, 8901, 9122,
                        True, 3670
              );
  _cs.Add     (   5132, 'Tokyo_1892',
                        1048, 8901, 9122,
                        True, 1364
              );
  _cs.Add     (   5228, 'S_JTSK_05',
                        1052, 8901, 9122,
                        True, 1079
              );
  _cs.Add     (   5229, 'S_JTSK_05_Ferro',
                        1055, 8909, 9122,
                        True, 1079
              );
  _cs.Add     (   5233, 'SLD99',
                        1053, 8901, 9122,
                        True, 3310
              );
  _cs.Add     (   5246, 'GDBD2009',
                        1056, 8901, 9122,
                        True, 1055
              );
  _cs.Add     (   5252, 'TUREF',
                        1057, 8901, 9122,
                        True, 1237
              );
  _cs.Add     (   5264, 'DRUKREF_03',
                        1058, 8901, 9122,
                        True, 1048
              );
  _cs.Add     (   5324, 'ISN2004',
                        1060, 8901, 9122,
                        True, 1120
              );
  _cs.Add     (   5340, 'POSGAR_2007',
                        1062, 8901, 9122,
                        True, 1033
              );
  _cs.Add     (   5354, 'MARGEN',
                        1063, 8901, 9122,
                        True, 1049
              );
  _cs.Add     (   5360, 'SIRGAS_Chile_2002',
                        1064, 8901, 9122,
                        True, 1066
              );
  _cs.Add     (   5365, 'CR05',
                        1065, 8901, 9122,
                        True, 1074
              );
  _cs.Add     (   5371, 'MACARIO_SOLIS',
                        1066, 8901, 9122,
                        True, 1186
              );
  _cs.Add     (   5373, 'Peru96',
                        1067, 8901, 9122,
                        True, 1189
              );
  _cs.Add     (   5381, 'SIRGAS_ROU98',
                        1068, 8901, 9122,
                        True, 1247
              );
  _cs.Add     (   5393, 'SIRGAS_ES2007_8',
                        1069, 8901, 9122,
                        True, 1087
              );
  _cs.Add     (   5451, 'Ocotepeque_1935',
                        1070, 8901, 9122,
                        True, 3876
              );
  _cs.Add     (   5464, 'Sibun_Gorge_1922',
                        1071, 8901, 9122,
                        True, 3219
              );
  _cs.Add     (   5467, 'Panama_Colon_1911',
                        1072, 8901, 9122,
                        True, 3290
              );
  _cs.Add     (   5489, 'RGAF09',
                        1073, 8901, 9122,
                        True, 2824
              );
  _cs.Add     (   5524, 'Corrego_Alegre_1961',
                        1074, 8901, 9122,
                        True, 3874
              );
  _cs.Add     (   5527, 'SAD69_96',
                        1075, 8901, 9122,
                        True, 1053
              );
  _cs.Add     (   5546, 'PNG94',
                        1076, 8901, 9122,
                        True, 1187
              );
  _cs.Add     (   5561, 'UCS_2000',
                        1077, 8901, 9122,
                        True, 1242
              );
  _cs.Add     (   5593, 'FEH2010',
                        1078, 8901, 9122,
                        True, 3889
              );
  _cs.Add     (   5681, 'DB_REF',
                        1081, 8901, 9122,
                        True, 3339
              );
  _cs.Add     (   5886, 'TGD2005',
                        1095, 8901, 9122,
                        True, 1234
              );
  _cs.Add     (   6135, 'CIGD11',
                        1100, 8901, 9122,
                        True, 1063
              );
  _cs.Add     (   6207, 'Nepal_1981',
                        1111, 8901, 9122,
                        True, 1171
              );
  _cs.Add     (   6311, 'CGRS93',
                        1112, 8901, 9122,
                        True, 3236
              );
  _cs.Add     (   6318, 'NAD83_2011',
                        1116, 8901, 9122,
                        True, 1511
              );
  _cs.Add     (   6322, 'NAD83_PA11',
                        1117, 8901, 9122,
                        True, 4162
              );
  _cs.Add     (   6325, 'NAD83_MA11',
                        1118, 8901, 9122,
                        True, 4167
              );
  _cs.Add     (   6365, 'Mexico_ITRF2008',
                        1120, 8901, 9122,
                        True, 1160
              );
  _cs.Add     (   6668, 'JGD2011',
                        1128, 8901, 9122,
                        True, 1129
              );
  _cs.Add     (   6706, 'RDN2008',
                        1132, 8901, 9122,
                        True, 3343
              );
  _cs.Add     (   6783, 'NAD83_CORS96',
                        1133, 8901, 9122,
                        True, 1511
              );
  _cs.Add     (   6881, 'Aden_1925',
                        1135, 8901, 9122,
                        True, 1340
              );
  _cs.Add     (   6882, 'Bekaa_Valley_1920',
                        1137, 8901, 9122,
                        True, 3269
              );
  _cs.Add     (   6883, 'Bioko',
                        1136, 8901, 9122,
                        True, 4220
              );
  _cs.Add     (   6892, 'South_East_Island_1943',
                        1138, 8901, 9122,
                        True, 4183
              );
  _cs.Add     (   6894, 'Gambia',
                        1139, 8901, 9122,
                        True, 3250
              );
  _cs.Add     (   6980, 'IGD05',
                        1143, 8901, 9122,
                        True, 1126
              )         .MarkDeprecated( True );
  _cs.Add     (   6983, 'IG05_Intermediate_CRS',
                        1142, 8901, 9122,
                        True, 2603
              );
  _cs.Add     (   6987, 'IGD05_12',
                        1145, 8901, 9122,
                        True, 1126
              )         .MarkDeprecated( True );
  _cs.Add     (   6990, 'IG05_12_Intermediate_CRS',
                        1144, 8901, 9122,
                        True, 2603
              );
  _cs.Add     (   7035, 'RGSPM06_lon_lat',
                        1038, 8901, 9122,
                        False, 1220
              );
  _cs.Add     (   7037, 'RGR92_lon_lat',
                        6627, 8901, 9122,
                        False, 3902
              );
  _cs.Add     (   7039, 'RGM04_lon_lat',
                        1036, 8901, 9122,
                        False, 1159
              );
  _cs.Add     (   7041, 'RGFG95_lon_lat',
                        6624, 8901, 9122,
                        False, 1097
              );
  _cs.Add     (   7073, 'RGTAAF07',
                        1113, 8901, 9122,
                        True, 4246
              );
  _cs.Add     (   7084, 'RGF93_v1_lon_lat',
                        6171, 8901, 9122,
                        False, 1096
              );
  _cs.Add     (   7086, 'RGAF09_lon_lat',
                        1073, 8901, 9122,
                        False, 2824
              );
  _cs.Add     (   7088, 'RGTAAF07_lon_lat',
                        1113, 8901, 9122,
                        False, 4246
              )         .MarkDeprecated( True );
  _cs.Add     (   7133, 'RGTAAF07_lon_lat_A',
                        1113, 8901, 9122,
                        False, 4246
              );
  _cs.Add     (   7136, 'IGD05_A',
                        1114, 8901, 9122,
                        True, 1126
              );
  _cs.Add     (   7139, 'IGD05_12_A',
                        1115, 8901, 9122,
                        True, 1126
              );
  _cs.Add     (   7373, 'ONGD14',
                        1147, 8901, 9122,
                        True, 1183
              );
  _cs.Add     (   7683, 'GSK_2011',
                        1159, 8901, 9122,
                        True, 1198
              );
  _cs.Add     (   7686, 'Kyrg_06',
                        1160, 8901, 9122,
                        True, 1137
              );
  _cs.Add     (   7798, 'BGS2005',
                        1167, 8901, 9122,
                        True, 1056
              );
end;

procedure prepare_6;
begin
  _cs.Add     (   7844, 'GDA2020',
                        1168, 8901, 9122,
                        True, 4177
              );
  _cs.Add     (   7881, 'St_Helena_Tritan',
                        1173, 8901, 9122,
                        True, 3183
              );
  _cs.Add     (   7886, 'SHGD2015',
                        1174, 8901, 9122,
                        True, 3183
              );
  _cs.Add     (   8042, 'Gusterberg_Ferro',
                        1188, 8909, 9122,
                        True, 4455
              );
  _cs.Add     (   8043, 'St_Stephen_Ferro',
                        1189, 8909, 9122,
                        True, 4456
              );
  _cs.Add     (   8086, 'ISN2016',
                        1187, 8901, 9122,
                        True, 1120
              );
  _cs.Add     (   8232, 'NAD83_CSRS96',
                        1192, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8237, 'NAD83_CSRS_v2',
                        1193, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8240, 'NAD83_CSRS_v3',
                        1194, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8246, 'NAD83_CSRS_v4',
                        1195, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8249, 'NAD83_CSRS_v5',
                        1196, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8252, 'NAD83_CSRS_v6',
                        1197, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8255, 'NAD83_CSRS_v7',
                        1198, 8901, 9122,
                        True, 1061
              );
  _cs.Add     (   8351, 'S_JTSK_JTSK03',
                        1201, 8901, 9122,
                        True, 1211
              );
  _cs.Add     (   8427, 'Hong_Kong_Geodetic_CS',
                        1209, 8901, 9122,
                        True, 1118
              );
  _cs.Add     (   8428, 'Macao_1920',
                        1207, 8901, 9122,
                        True, 1147
              );
  _cs.Add     (   8431, 'Macao_2008',
                        1208, 8901, 9122,
                        True, 1147
              );
  _cs.Add     (   8449, 'NAD83_FBN',
                        6152, 8901, 9122,
                        True, 4515
              )         .MarkDeprecated( True );
  _cs.Add     (   8545, 'NAD83_HARN_Corrected',
                        1212, 8901, 9122,
                        True, 3634
              );
  _cs.Add     (   8685, 'SRB_ETRS89',
                        1214, 8901, 9122,
                        True, 4543
              );
  _cs.Add     (   8694, 'Camacupa_2015',
                        1217, 8901, 9122,
                        True, 1029
              );
  _cs.Add     (   8699, 'RSAO13',
                        1220, 8901, 9122,
                        True, 1029
              );
  _cs.Add     (   8818, 'MTRF_2000',
                        1218, 8901, 9122,
                        True, 1206
              );
  _cs.Add     (   8860, 'NAD83_FBN_A',
                        1211, 8901, 9122,
                        True, 4515
              );
  _cs.Add     (   8888, 'WGS_84_Transit',
                        1166, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8900, 'RGWF96',
                        1223, 8901, 9122,
                        True, 1255
              );
  _cs.Add     (   8902, 'RGWF96_lon_lat',
                        1223, 8901, 9122,
                        False, 1255
              );
  _cs.Add     (   8907, 'CR_SIRGAS',
                        1225, 8901, 9122,
                        True, 1074
              );
  _cs.Add     (   8949, 'SIRGAS_Chile_2010',
                        1243, 8901, 9122,
                        True, 1066
              );
  _cs.Add     (   8972, 'SIRGAS_CON_DGF00P01',
                        1227, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8973, 'SIRGAS_CON_DGF01P01',
                        1228, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8974, 'SIRGAS_CON_DGF01P02',
                        1229, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8975, 'SIRGAS_CON_DGF02P01',
                        1230, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8976, 'SIRGAS_CON_DGF04P01',
                        1231, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8977, 'SIRGAS_CON_DGF05P01',
                        1232, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8978, 'SIRGAS_CON_DGF06P01',
                        1233, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8979, 'SIRGAS_CON_DGF07P01',
                        1234, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8980, 'SIRGAS_CON_DGF08P01',
                        1235, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8981, 'SIRGAS_CON_SIR09P01',
                        1236, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8982, 'SIRGAS_CON_SIR10P01',
                        1237, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8983, 'SIRGAS_CON_SIR11P01',
                        1238, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8984, 'SIRGAS_CON_SIR13P01',
                        1239, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8985, 'SIRGAS_CON_SIR14P01',
                        1240, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8986, 'SIRGAS_CON_SIR15P01',
                        1241, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8987, 'SIRGAS_CON_SIR17P01',
                        1242, 8901, 9122,
                        True, 4530
              );
  _cs.Add     (   8988, 'ITRF88',
                        6647, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8989, 'ITRF89',
                        6648, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8990, 'ITRF90',
                        6649, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8991, 'ITRF91',
                        6650, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8992, 'ITRF92',
                        6651, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8993, 'ITRF93',
                        6652, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8994, 'ITRF94',
                        6653, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8995, 'ITRF96',
                        6654, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8996, 'ITRF97',
                        6655, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8997, 'ITRF2000',
                        6656, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8998, 'ITRF2005',
                        6896, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   8999, 'ITRF2008',
                        1061, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9000, 'ITRF2014',
                        1165, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9003, 'IGS97',
                        1244, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9006, 'IGS00',
                        1245, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9009, 'IGb00',
                        1246, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9012, 'IGS05',
                        1247, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9014, 'IGS08',
                        1141, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9017, 'IGb08',
                        1248, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9019, 'IGS14',
                        1191, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9053, 'WGS_84_G730',
                        1152, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9054, 'WGS_84_G873',
                        1153, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9055, 'WGS_84_G1150',
                        1154, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9056, 'WGS_84_G1674',
                        1155, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9057, 'WGS_84_G1762',
                        1156, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9059, 'ETRF89',
                        1178, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9060, 'ETRF90',
                        1179, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9061, 'ETRF91',
                        1180, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9062, 'ETRF92',
                        1181, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9063, 'ETRF93',
                        1182, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9064, 'ETRF94',
                        1183, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9065, 'ETRF96',
                        1184, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9066, 'ETRF97',
                        1185, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9067, 'ETRF2000',
                        1186, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9068, 'ETRF2005',
                        1204, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9069, 'ETRF2014',
                        1206, 8901, 9122,
                        True, 1298
              );
  _cs.Add     (   9072, 'NAD83_MARP00',
                        1221, 8901, 9122,
                        True, 4167
              );
  _cs.Add     (   9075, 'NAD83_PACP00',
                        1249, 8901, 9122,
                        True, 4162
              );
  _cs.Add     (   9140, 'KOSOVAREF01',
                        1251, 8901, 9122,
                        True, 4542
              );
  _cs.Add     (   9148, 'SIRGAS_Chile_2013',
                        1252, 8901, 9122,
                        True, 1066
              );
  _cs.Add     (   9153, 'SIRGAS_Chile_2016',
                        1253, 8901, 9122,
                        True, 1066
              );
  _cs.Add     (   9184, 'SIRGAS_Chile',
                        1254, 8901, 9122,
                        True, 1066
              )         .MarkDeprecated( True );
  _cs.Add     (   9248, 'Tapi_Aike',
                        1257, 8901, 9122,
                        True, 4569
              );
  _cs.Add     (   9251, 'MMN',
                        1258, 8901, 9122,
                        True, 2357
              );
  _cs.Add     (   9253, 'MMS',
                        1259, 8901, 9122,
                        True, 2357
              );
  _cs.Add     (   9294, 'ONGD17',
                        1263, 8901, 9122,
                        True, 1183
              );
  _cs.Add     (   9299, 'HS2_IRF',
                        1264, 8901, 9122,
                        True, 4582
              );
  _cs.Add     (   9309, 'ATRF2014',
                        1291, 8901, 9122,
                        True, 4177
              );
  _cs.Add     (   9333, 'KSA_GRF17',
                        1268, 8901, 9122,
                        True, 1206
              );
  _cs.Add     (   9364, 'TPEN11_IRF',
                        1266, 8901, 9122,
                        True, 4583
              );
  _cs.Add     (   9372, 'MML07_IRF',
                        1271, 8901, 9122,
                        True, 4588
              );
  _cs.Add     (   9380, 'IGb14',
                        1272, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9384, 'AbInvA96_2020_IRF',
                        1273, 8901, 9122,
                        True, 4589
              );
  _cs.Add     (   9403, 'PN68',
                        1286, 8901, 9122,
                        True, 3873
              );
  _cs.Add     (   9453, 'GBK19_IRF',
                        1289, 8901, 9122,
                        True, 4607
              );
end;

procedure prepare_7;
begin
  _cs.Add     (   9470, 'SRGI2013',
                        1293, 8901, 9122,
                        True, 1122
              );
  _cs.Add     (   9474, 'PZ_90_02',
                        1157, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9475, 'PZ_90_11',
                        1158, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9547, 'LTF2004_G',
                        1295, 8901, 9122,
                        True, 4613
              );
  _cs.Add     (   9696, 'REDGEOMIN',
                        1304, 8901, 9122,
                        True, 1066
              );
  _cs.Add     (   9702, 'ETRF2000_PL',
                        1305, 8901, 9122,
                        True, 1192
              );
  _cs.Add     (   9739, 'EOS21_IRF',
                        1308, 8901, 9122,
                        True, 4620
              );
  _cs.Add     (   9755, 'WGS_84_G2139',
                        1309, 8901, 9122,
                        True, 1262
              );
  _cs.Add     (   9758, 'ECML14_NB_IRF',
                        1310, 8901, 9122,
                        True, 4621
              );
  _cs.Add     (   9763, 'EWR2_IRF',
                        1311, 8901, 9122,
                        True, 4622
              );
  _cs.Add     (   9777, 'RGF93_v2',
                        1312, 8901, 9122,
                        True, 1096
              );
  _cs.Add     (   9779, 'RGF93_v2_lon_lat',
                        1312, 8901, 9122,
                        False, 1096
              );
  _cs.Add     (   9782, 'RGF93_v2b',
                        1313, 8901, 9122,
                        True, 1096
              );
  _cs.Add     (   9784, 'RGF93_v2b_lon_lat',
                        1313, 8901, 9122,
                        False, 1096
              );
  _cs.Add     (   9866, 'MRH21_IRF',
                        1314, 8901, 9122,
                        True, 4652
              );
  _cs.Add     (   9871, 'MOLDOR11_IRF',
                        1315, 8901, 9122,
                        True, 4655
              );
  _cs.Add     ( 61206405, 'Greek_deg',
                        6120, 8901, 9122,
                        True, 1106
              )         .MarkDeprecated( True );
  _cs.Add     ( 61216405, 'GGRS87_deg',
                        6121, 8901, 9122,
                        True, 1106
              )         .MarkDeprecated( True );
  _cs.Add     ( 61226405, 'ATS77_deg',
                        6122, 8901, 9122,
                        True, 1283
              )         .MarkDeprecated( True );
  _cs.Add     ( 61236405, 'KKJ_deg',
                        6123, 8901, 9122,
                        True, 1095
              )         .MarkDeprecated( True );
  _cs.Add     ( 61246405, 'RT90_deg',
                        6124, 8901, 9122,
                        True, 1225
              )         .MarkDeprecated( True );
  _cs.Add     ( 61266405, 'LKS94_ETRS89_deg',
                        6126, 8901, 9122,
                        True, 1145
              )         .MarkDeprecated( True );
  _cs.Add     ( 61276405, 'Tete_deg',
                        6127, 8901, 9122,
                        True, 1167
              )         .MarkDeprecated( True );
  _cs.Add     ( 61286405, 'Madzansua_deg',
                        6128, 8901, 9122,
                        True, 1315
              )         .MarkDeprecated( True );
  _cs.Add     ( 61296405, 'Observatario_deg',
                        6129, 8901, 9122,
                        True, 1329
              )         .MarkDeprecated( True );
  _cs.Add     ( 61306405, 'Moznet_deg',
                        6130, 8901, 9122,
                        True, 1167
              )         .MarkDeprecated( True );
  _cs.Add     ( 61316405, 'Indian_1960_deg',
                        6131, 8901, 9122,
                        True, 1302
              )         .MarkDeprecated( True );
  _cs.Add     ( 61326405, 'FD58_deg',
                        6132, 8901, 9122,
                        True, 1300
              )         .MarkDeprecated( True );
  _cs.Add     ( 61336405, 'EST92_deg',
                        6133, 8901, 9122,
                        True, 1090
              )         .MarkDeprecated( True );
  _cs.Add     ( 61346405, 'PDO_Survey_Datum_1993_deg',
                        6134, 8901, 9122,
                        True, 1183
              )         .MarkDeprecated( True );
  _cs.Add     ( 61356405, 'Old_Hawaiian_deg',
                        6135, 8901, 9122,
                        True, 1334
              )         .MarkDeprecated( True );
  _cs.Add     ( 61366405, 'St_Lawrence_Island_deg',
                        6136, 8901, 9122,
                        True, 1332
              )         .MarkDeprecated( True );
  _cs.Add     ( 61376405, 'St_Paul_Island_deg',
                        6137, 8901, 9122,
                        True, 1333
              )         .MarkDeprecated( True );
  _cs.Add     ( 61386405, 'St_George_Island_deg',
                        6138, 8901, 9122,
                        True, 1331
              )         .MarkDeprecated( True );
  _cs.Add     ( 61396405, 'Puerto_Rico_deg',
                        6139, 8901, 9122,
                        True, 1335
              )         .MarkDeprecated( True );
  _cs.Add     ( 61406405, 'NAD83_CSRS_deg',
                        6140, 8901, 9122,
                        True, 2784
              )         .MarkDeprecated( True );
  _cs.Add     ( 61416405, 'Israel_deg',
                        6141, 8901, 9122,
                        True, 2603
              )         .MarkDeprecated( True );
  _cs.Add     ( 61426405, 'Locodjo_1965_deg',
                        6142, 8901, 9122,
                        True, 1075
              )         .MarkDeprecated( True );
  _cs.Add     ( 61436405, 'Abidjan_1987_deg',
                        6143, 8901, 9122,
                        True, 1075
              )         .MarkDeprecated( True );
  _cs.Add     ( 61446405, 'Kalianpur_1937_deg',
                        6144, 8901, 9122,
                        True, 1308
              )         .MarkDeprecated( True );
  _cs.Add     ( 61456405, 'Kalianpur_1962_deg',
                        6145, 8901, 9122,
                        True, 1184
              )         .MarkDeprecated( True );
  _cs.Add     ( 61466405, 'Kalianpur_1975_deg',
                        6146, 8901, 9122,
                        True, 1121
              )         .MarkDeprecated( True );
  _cs.Add     ( 61476405, 'Hanoi_1972_deg',
                        6147, 8901, 9122,
                        True, 1252
              )         .MarkDeprecated( True );
  _cs.Add     ( 61486405, 'Hartebeesthoek94_deg',
                        6148, 8901, 9122,
                        True, 1215
              )         .MarkDeprecated( True );
  _cs.Add     ( 61496405, 'CH1903_deg',
                        6149, 8901, 9122,
                        True, 1286
              )         .MarkDeprecated( True );
  _cs.Add     ( 61506405, 'CH1903+_deg',
                        6150, 8901, 9122,
                        True, 1286
              )         .MarkDeprecated( True );
  _cs.Add     ( 61516405, 'CHTRF95_deg',
                        6151, 8901, 9122,
                        True, 1286
              )         .MarkDeprecated( True );
  _cs.Add     ( 61526405, 'NAD83_HARN_deg',
                        6152, 8901, 9122,
                        True, 1337
              )         .MarkDeprecated( True );
  _cs.Add     ( 61536405, 'Rassadiran_deg',
                        6153, 8901, 9122,
                        True, 1338
              )         .MarkDeprecated( True );
  _cs.Add     ( 61546405, 'ED50_ED77_deg',
                        6154, 8901, 9122,
                        True, 1123
              )         .MarkDeprecated( True );
  _cs.Add     ( 61556405, 'Dabola_1981_deg',
                        6155, 8901, 9122,
                        True, 1112
              )         .MarkDeprecated( True );
  _cs.Add     ( 61566405, 'S_JTSK_deg',
                        6156, 8901, 9122,
                        True, 1306
              )         .MarkDeprecated( True );
  _cs.Add     ( 61576405, 'Mount_Dillon_deg',
                        6157, 8901, 9122,
                        True, 1322
              )         .MarkDeprecated( True );
  _cs.Add     ( 61586405, 'Naparima_1955_deg',
                        6158, 8901, 9122,
                        True, 1339
              )         .MarkDeprecated( True );
  _cs.Add     ( 61596405, 'ELD79_deg',
                        6159, 8901, 9122,
                        True, 1143
              )         .MarkDeprecated( True );
  _cs.Add     ( 61606405, 'Chos_Malal_1914_deg',
                        6160, 8901, 9122,
                        True, 1292
              )         .MarkDeprecated( True );
  _cs.Add     ( 61616405, 'Pampa_del_Castillo_deg',
                        6161, 8901, 9122,
                        True, 1265
              )         .MarkDeprecated( True );
  _cs.Add     ( 61626405, 'Korean_1985_deg',
                        6162, 8901, 9122,
                        True, 1135
              )         .MarkDeprecated( True );
  _cs.Add     ( 61636405, 'Yemen_NGN96_deg',
                        6163, 8901, 9122,
                        True, 1257
              )         .MarkDeprecated( True );
  _cs.Add     ( 61646405, 'South_Yemen_deg',
                        6164, 8901, 9122,
                        True, 1340
              )         .MarkDeprecated( True );
  _cs.Add     ( 61656405, 'Bissau_deg',
                        6165, 8901, 9122,
                        True, 1113
              )         .MarkDeprecated( True );
  _cs.Add     ( 61666405, 'Korean_1995_deg',
                        6166, 8901, 9122,
                        True, 1135
              )         .MarkDeprecated( True );
  _cs.Add     ( 61676405, 'NZGD2000_deg',
                        6167, 8901, 9122,
                        True, 1175
              )         .MarkDeprecated( True );
  _cs.Add     ( 61686405, 'Accra_deg',
                        6168, 8901, 9122,
                        True, 1104
              )         .MarkDeprecated( True );
  _cs.Add     ( 61696405, 'American_Samoa_1962_deg',
                        6169, 8901, 9122,
                        True, 1027
              )         .MarkDeprecated( True );
  _cs.Add     ( 61706405, 'SIRGAS_deg',
                        6170, 8901, 9122,
                        True, 1341
              )         .MarkDeprecated( True );
  _cs.Add     ( 61716405, 'RGF93_deg',
                        6171, 8901, 9122,
                        True, 1096
              )         .MarkDeprecated( True );
  _cs.Add     ( 61736405, 'IRENET95_deg',
                        6173, 8901, 9122,
                        True, 1305
              )         .MarkDeprecated( True );
  _cs.Add     ( 61746405, 'Sierra_Leone_1924_deg',
                        6174, 8901, 9122,
                        True, 1342
              )         .MarkDeprecated( True );
  _cs.Add     ( 61756405, 'Sierra_Leone_1968_deg',
                        6175, 8901, 9122,
                        True, 1209
              )         .MarkDeprecated( True );
  _cs.Add     ( 61766405, 'Australian_Antarctic_deg',
                        6176, 8901, 9122,
                        True, 1278
              )         .MarkDeprecated( True );
  _cs.Add     ( 61786405, 'Pulkovo_1942_83_deg',
                        6178, 8901, 9122,
                        True, 1343
              )         .MarkDeprecated( True );
  _cs.Add     ( 61796405, 'Pulkovo_1942_58_deg',
                        6179, 8901, 9122,
                        True, 1192
              )         .MarkDeprecated( True );
  _cs.Add     ( 61806405, 'EST97_deg',
                        6180, 8901, 9122,
                        True, 1090
              )         .MarkDeprecated( True );
  _cs.Add     ( 61816405, 'Luxembourg_1930_deg',
                        6181, 8901, 9122,
                        True, 1146
              )         .MarkDeprecated( True );
  _cs.Add     ( 61826405, 'Azores_Occidental_1939_deg',
                        6182, 8901, 9122,
                        True, 1344
              )         .MarkDeprecated( True );
  _cs.Add     ( 61836405, 'Azores_Central_1948_deg',
                        6183, 8901, 9122,
                        True, 1301
              )         .MarkDeprecated( True );
  _cs.Add     ( 61846405, 'Azores_Oriental_1940_deg',
                        6184, 8901, 9122,
                        True, 1345
              )         .MarkDeprecated( True );
  _cs.Add     ( 61886405, 'OSNI_1952_deg',
                        6188, 8901, 9122,
                        True, 2530
              )         .MarkDeprecated( True );
  _cs.Add     ( 61896405, 'REGVEN_deg',
                        6189, 8901, 9122,
                        True, 1251
              )         .MarkDeprecated( True );
  _cs.Add     ( 61906405, 'POSGAR_98_deg',
                        6190, 8901, 9122,
                        True, 1033
              )         .MarkDeprecated( True );
  _cs.Add     ( 61916405, 'Albanian_1987_deg',
                        6191, 8901, 9122,
                        True, 1025
              )         .MarkDeprecated( True );
  _cs.Add     ( 61926405, 'Douala_1948_deg',
                        6192, 8901, 9122,
                        True, 1060
              )         .MarkDeprecated( True );
  _cs.Add     ( 61936405, 'Manoca_1962_deg',
                        6193, 8901, 9122,
                        True, 1060
              )         .MarkDeprecated( True );
  _cs.Add     ( 61946405, 'Qornoq_1927_deg',
                        6194, 8901, 9122,
                        True, 1107
              )         .MarkDeprecated( True );
  _cs.Add     ( 61956405, 'Scoresbysund_1952_deg',
                        6195, 8901, 9122,
                        True, 2570
              )         .MarkDeprecated( True );
  _cs.Add     ( 61966405, 'Ammassalik_1958_deg',
                        6196, 8901, 9122,
                        True, 2571
              )         .MarkDeprecated( True );
  _cs.Add     ( 61976405, 'Garoua_deg',
                        6197, 8901, 9122,
                        True, 2590
              )         .MarkDeprecated( True );
  _cs.Add     ( 61986405, 'Kousseri_deg',
                        6198, 8901, 9122,
                        True, 2591
              )         .MarkDeprecated( True );
  _cs.Add     ( 61996405, 'Egypt_1930_deg',
                        6199, 8901, 9122,
                        True, 1086
              )         .MarkDeprecated( True );
  _cs.Add     ( 62006405, 'Pulkovo_1995_deg',
                        6200, 8901, 9122,
                        True, 1198
              )         .MarkDeprecated( True );
  _cs.Add     ( 62016405, 'Adindan_deg',
                        6201, 8901, 9122,
                        True, 1271
              )         .MarkDeprecated( True );
  _cs.Add     ( 62026405, 'AGD66_deg',
                        6202, 8901, 9122,
                        True, 1279
              )         .MarkDeprecated( True );
  _cs.Add     ( 62036405, 'AGD84_deg',
                        6203, 8901, 9122,
                        True, 2576
              )         .MarkDeprecated( True );
  _cs.Add     ( 62046405, 'Ain_el_Abd_deg',
                        6204, 8901, 9122,
                        True, 1272
              )         .MarkDeprecated( True );
  _cs.Add     ( 62056405, 'Afgooye_deg',
                        6205, 8901, 9122,
                        True, 1214
              )         .MarkDeprecated( True );
  _cs.Add     ( 62066405, 'Agadez_deg',
                        6206, 8901, 9122,
                        True, 1177
              )         .MarkDeprecated( True );
  _cs.Add     ( 62076405, 'Lisbon_deg',
                        6207, 8901, 9122,
                        True, 1294
              )         .MarkDeprecated( True );
  _cs.Add     ( 62086405, 'Aratu_deg',
                        6208, 8901, 9122,
                        True, 1274
              )         .MarkDeprecated( True );
  _cs.Add     ( 62096405, 'Arc_1950_deg',
                        6209, 8901, 9122,
                        True, 1276
              )         .MarkDeprecated( True );
end;

procedure prepare_8;
begin
  _cs.Add     ( 62106405, 'Arc_1960_deg',
                        6210, 8901, 9122,
                        True, 1277
              )         .MarkDeprecated( True );
  _cs.Add     ( 62116405, 'Batavia_deg',
                        6211, 8901, 9122,
                        True, 1285
              )         .MarkDeprecated( True );
  _cs.Add     ( 62126405, 'Barbados_1938_deg',
                        6212, 8901, 9122,
                        True, 1042
              )         .MarkDeprecated( True );
  _cs.Add     ( 62136405, 'Beduaram_deg',
                        6213, 8901, 9122,
                        True, 2771
              )         .MarkDeprecated( True );
  _cs.Add     ( 62146405, 'Beijing_1954_deg',
                        6214, 8901, 9122,
                        True, 1067
              )         .MarkDeprecated( True );
  _cs.Add     ( 62156405, 'Belge_1950_deg',
                        6215, 8901, 9122,
                        True, 1347
              )         .MarkDeprecated( True );
  _cs.Add     ( 62166405, 'Bermuda_1957_deg',
                        6216, 8901, 9122,
                        True, 1047
              )         .MarkDeprecated( True );
  _cs.Add     ( 62186405, 'Bogota_1975_deg',
                        6218, 8901, 9122,
                        True, 1070
              )         .MarkDeprecated( True );
  _cs.Add     ( 62196405, 'Bukit_Rimpah_deg',
                        6219, 8901, 9122,
                        True, 1287
              )         .MarkDeprecated( True );
  _cs.Add     ( 62206405, 'Camacupa_deg',
                        6220, 8901, 9122,
                        True, 1288
              )         .MarkDeprecated( True );
  _cs.Add     ( 62216405, 'Campo_Inchauspe_deg',
                        6221, 8901, 9122,
                        True, 1033
              )         .MarkDeprecated( True );
  _cs.Add     ( 62226405, 'Cape_deg',
                        6222, 8901, 9122,
                        True, 1290
              )         .MarkDeprecated( True );
  _cs.Add     ( 62236405, 'Carthage_deg',
                        6223, 8901, 9122,
                        True, 1236
              )         .MarkDeprecated( True );
  _cs.Add     ( 62246405, 'Chua_deg',
                        6224, 8901, 9122,
                        True, 1053
              )         .MarkDeprecated( True );
  _cs.Add     ( 62256405, 'Corrego_Alegre_deg',
                        6225, 8901, 9122,
                        True, 1293
              )         .MarkDeprecated( True );
  _cs.Add     ( 62276405, 'Deir_ez_Zor_deg',
                        6227, 8901, 9122,
                        True, 1623
              )         .MarkDeprecated( True );
  _cs.Add     ( 62296405, 'Egypt_1907_deg',
                        6229, 8901, 9122,
                        True, 1086
              )         .MarkDeprecated( True );
  _cs.Add     ( 62306405, 'ED50_deg',
                        6230, 8901, 9122,
                        True, 1296
              )         .MarkDeprecated( True );
  _cs.Add     ( 62316405, 'ED87_deg',
                        6231, 8901, 9122,
                        True, 1297
              )         .MarkDeprecated( True );
  _cs.Add     ( 62326405, 'Fahud_deg',
                        6232, 8901, 9122,
                        True, 1183
              )         .MarkDeprecated( True );
  _cs.Add     ( 62336405, 'Gandajika_1970_deg',
                        6233, 8901, 9122,
                        True, 1152
              )         .MarkDeprecated( True );
  _cs.Add     ( 62366405, 'Hu_Tzu_Shan_deg',
                        6236, 8901, 9122,
                        True, 1228
              )         .MarkDeprecated( True );
  _cs.Add     ( 62376405, 'HD72_deg',
                        6237, 8901, 9122,
                        True, 1119
              )         .MarkDeprecated( True );
  _cs.Add     ( 62386405, 'ID74_deg',
                        6238, 8901, 9122,
                        True, 1122
              )         .MarkDeprecated( True );
  _cs.Add     ( 62396405, 'Indian_1954_deg',
                        6239, 8901, 9122,
                        True, 1304
              )         .MarkDeprecated( True );
  _cs.Add     ( 62406405, 'Indian_1975_deg',
                        6240, 8901, 9122,
                        True, 1231
              )         .MarkDeprecated( True );
  _cs.Add     ( 62416405, 'Jamaica_1875_deg',
                        6241, 8901, 9122,
                        True, 1128
              )         .MarkDeprecated( True );
  _cs.Add     ( 62426405, 'JAD69_deg',
                        6242, 8901, 9122,
                        True, 1128
              )         .MarkDeprecated( True );
  _cs.Add     ( 62436405, 'Kalianpur_1880_deg',
                        6243, 8901, 9122,
                        True, 1307
              )         .MarkDeprecated( True );
  _cs.Add     ( 62446405, 'Kandawala_deg',
                        6244, 8901, 9122,
                        True, 1218
              )         .MarkDeprecated( True );
  _cs.Add     ( 62456405, 'Kertau_deg',
                        6245, 8901, 9122,
                        True, 1309
              )         .MarkDeprecated( True );
  _cs.Add     ( 62466405, 'KOC_deg',
                        6246, 8901, 9122,
                        True, 1136
              )         .MarkDeprecated( True );
  _cs.Add     ( 62476405, 'La_Canoa_deg',
                        6247, 8901, 9122,
                        True, 1251
              )         .MarkDeprecated( True );
  _cs.Add     ( 62486405, 'PSAD56_deg',
                        6248, 8901, 9122,
                        True, 1348
              )         .MarkDeprecated( True );
  _cs.Add     ( 62496405, 'Lake_deg',
                        6249, 8901, 9122,
                        True, 1312
              )         .MarkDeprecated( True );
  _cs.Add     ( 62506405, 'Leigon_deg',
                        6250, 8901, 9122,
                        True, 1104
              )         .MarkDeprecated( True );
  _cs.Add     ( 62516405, 'Liberia_1964_deg',
                        6251, 8901, 9122,
                        True, 1142
              )         .MarkDeprecated( True );
  _cs.Add     ( 62526405, 'Lome_deg',
                        6252, 8901, 9122,
                        True, 1232
              )         .MarkDeprecated( True );
  _cs.Add     ( 62536405, 'Luzon_1911_deg',
                        6253, 8901, 9122,
                        True, 1190
              )         .MarkDeprecated( True );
  _cs.Add     ( 62546405, 'Hito_XVIII_1963_deg',
                        6254, 8901, 9122,
                        True, 1303
              )         .MarkDeprecated( True );
  _cs.Add     ( 62556405, 'Herat_North_deg',
                        6255, 8901, 9122,
                        True, 1024
              )         .MarkDeprecated( True );
  _cs.Add     ( 62566405, 'Mahe_1971_deg',
                        6256, 8901, 9122,
                        True, 1208
              )         .MarkDeprecated( True );
  _cs.Add     ( 62576405, 'Makassar_deg',
                        6257, 8901, 9122,
                        True, 1316
              )         .MarkDeprecated( True );
  _cs.Add     ( 62586405, 'ETRS89_deg',
                        6258, 8901, 9122,
                        True, 1298
              )         .MarkDeprecated( True );
  _cs.Add     ( 62596405, 'Malongo_1987_deg',
                        6259, 8901, 9122,
                        True, 1317
              )         .MarkDeprecated( True );
  _cs.Add     ( 62616405, 'Merchich_deg',
                        6261, 8901, 9122,
                        True, 1166
              )         .MarkDeprecated( True );
  _cs.Add     ( 62626405, 'Massawa_deg',
                        6262, 8901, 9122,
                        True, 1089
              )         .MarkDeprecated( True );
  _cs.Add     ( 62636405, 'Minna_deg',
                        6263, 8901, 9122,
                        True, 1178
              )         .MarkDeprecated( True );
  _cs.Add     ( 62646405, 'Mhast_deg',
                        6264, 8901, 9122,
                        True, 1318
              )         .MarkDeprecated( True );
  _cs.Add     ( 62656405, 'Monte_Mario_deg',
                        6265, 8901, 9122,
                        True, 1127
              )         .MarkDeprecated( True );
  _cs.Add     ( 62666405, 'M_poraloko_deg',
                        6266, 8901, 9122,
                        True, 1100
              )         .MarkDeprecated( True );
  _cs.Add     ( 62676405, 'NAD27_deg',
                        6267, 8901, 9122,
                        True, 1349
              )         .MarkDeprecated( True );
  _cs.Add     ( 62686405, 'NAD27_Michigan_deg',
                        6268, 8901, 9122,
                        True, 1391
              )         .MarkDeprecated( True );
  _cs.Add     ( 62696405, 'NAD83_deg',
                        6269, 8901, 9122,
                        True, 1350
              )         .MarkDeprecated( True );
  _cs.Add     ( 62706405, 'Nahrwan_1967_deg',
                        6270, 8901, 9122,
                        True, 1351
              )         .MarkDeprecated( True );
  _cs.Add     ( 62716405, 'Naparima_1972_deg',
                        6271, 8901, 9122,
                        True, 1322
              )         .MarkDeprecated( True );
  _cs.Add     ( 62726405, 'NZGD49_deg',
                        6272, 8901, 9122,
                        True, 1175
              )         .MarkDeprecated( True );
  _cs.Add     ( 62736405, 'NGO_1948_deg',
                        6273, 8901, 9122,
                        True, 1352
              )         .MarkDeprecated( True );
  _cs.Add     ( 62746405, 'Datum_73_deg',
                        6274, 8901, 9122,
                        True, 1294
              )         .MarkDeprecated( True );
  _cs.Add     ( 62756405, 'NTF_deg',
                        6275, 8901, 9122,
                        True, 1353
              )         .MarkDeprecated( True );
  _cs.Add     ( 62766405, 'NSWC_9Z_2_deg',
                        6276, 8901, 9122,
                        True, 1262
              )         .MarkDeprecated( True );
  _cs.Add     ( 62776405, 'OSGB_1936_deg',
                        6277, 8901, 9122,
                        True, 1264
              )         .MarkDeprecated( True );
  _cs.Add     ( 62786405, 'OSGB70_deg',
                        6278, 8901, 9122,
                        True, 1264
              )         .MarkDeprecated( True );
  _cs.Add     ( 62796405, 'OS_SN_80_deg',
                        6279, 8901, 9122,
                        True, 1354
              )         .MarkDeprecated( True );
  _cs.Add     ( 62806405, 'Padang_deg',
                        6280, 8901, 9122,
                        True, 1355
              )         .MarkDeprecated( True );
  _cs.Add     ( 62816405, 'Palestine_1923_deg',
                        6281, 8901, 9122,
                        True, 1356
              )         .MarkDeprecated( True );
  _cs.Add     ( 62826405, 'Pointe_Noire_deg',
                        6282, 8901, 9122,
                        True, 1072
              )         .MarkDeprecated( True );
  _cs.Add     ( 62836405, 'GDA94_deg',
                        6283, 8901, 9122,
                        True, 1036
              )         .MarkDeprecated( True );
  _cs.Add     ( 62846405, 'Pulkovo_1942_deg',
                        6284, 8901, 9122,
                        True, 1357
              )         .MarkDeprecated( True );
  _cs.Add     ( 62856405, 'Qatar_1974_deg',
                        6285, 8901, 9122,
                        True, 1346
              )         .MarkDeprecated( True );
  _cs.Add     ( 62866405, 'Qatar_1948_deg',
                        6286, 8901, 9122,
                        True, 1346
              )         .MarkDeprecated( True );
  _cs.Add     ( 62886405, 'Loma_Quintana_deg',
                        6288, 8901, 9122,
                        True, 1313
              )         .MarkDeprecated( True );
  _cs.Add     ( 62896405, 'Amersfoort_deg',
                        6289, 8901, 9122,
                        True, 1275
              )         .MarkDeprecated( True );
  _cs.Add     ( 62926405, 'Sapper_Hill_1943_deg',
                        6292, 8901, 9122,
                        True, 1092
              )         .MarkDeprecated( True );
  _cs.Add     ( 62936405, 'Schwarzeck_deg',
                        6293, 8901, 9122,
                        True, 1169
              )         .MarkDeprecated( True );
  _cs.Add     ( 62956405, 'Serindung_deg',
                        6295, 8901, 9122,
                        True, 1360
              )         .MarkDeprecated( True );
  _cs.Add     ( 62976405, 'Tananarive_deg',
                        6297, 8901, 9122,
                        True, 1149
              )         .MarkDeprecated( True );
  _cs.Add     ( 62986405, 'Timbalai_1948_deg',
                        6298, 8901, 9122,
                        True, 1362
              )         .MarkDeprecated( True );
  _cs.Add     ( 62996405, 'TM65_deg',
                        6299, 8901, 9122,
                        True, 1125
              )         .MarkDeprecated( True );
  _cs.Add     ( 63006405, 'TM75_deg',
                        6300, 8901, 9122,
                        True, 1305
              )         .MarkDeprecated( True );
  _cs.Add     ( 63016405, 'Tokyo_deg',
                        6301, 8901, 9122,
                        True, 1364
              )         .MarkDeprecated( True );
  _cs.Add     ( 63026405, 'Trinidad_1903_deg',
                        6302, 8901, 9122,
                        True, 1322
              )         .MarkDeprecated( True );
  _cs.Add     ( 63036405, 'TC_1948_deg',
                        6303, 8901, 9122,
                        True, 1363
              )         .MarkDeprecated( True );
  _cs.Add     ( 63046405, 'Voirol_1875_deg',
                        6304, 8901, 9122,
                        True, 1365
              )         .MarkDeprecated( True );
  _cs.Add     ( 63066405, 'Bern_1938_deg',
                        6306, 8901, 9122,
                        True, 1286
              )         .MarkDeprecated( True );
  _cs.Add     ( 63076405, 'Nord_Sahara_1959_deg',
                        6307, 8901, 9122,
                        True, 1366
              )         .MarkDeprecated( True );
  _cs.Add     ( 63086405, 'RT38_deg',
                        6308, 8901, 9122,
                        True, 1225
              )         .MarkDeprecated( True );
  _cs.Add     ( 63096405, 'Yacare_deg',
                        6309, 8901, 9122,
                        True, 1247
              )         .MarkDeprecated( True );
  _cs.Add     ( 63106405, 'Yoff_deg',
                        6310, 8901, 9122,
                        True, 1207
              )         .MarkDeprecated( True );
  _cs.Add     ( 63116405, 'Zanderij_deg',
                        6311, 8901, 9122,
                        True, 1222
              )         .MarkDeprecated( True );
  _cs.Add     ( 63126405, 'MGI_deg',
                        6312, 8901, 9122,
                        True, 1321
              )         .MarkDeprecated( True );
  _cs.Add     ( 63136405, 'Belge_1972_deg',
                        6313, 8901, 9122,
                        True, 1347
              )         .MarkDeprecated( True );
  _cs.Add     ( 63146405, 'DHDN_deg',
                        6314, 8901, 9122,
                        True, 1295
              )         .MarkDeprecated( True );
  _cs.Add     ( 63156405, 'Conakry_1905_deg',
                        6315, 8901, 9122,
                        True, 1112
              )         .MarkDeprecated( True );
  _cs.Add     ( 63166405, 'Dealul_Piscului_1933_deg',
                        6316, 8901, 9122,
                        True, 1197
              )         .MarkDeprecated( True );
  _cs.Add     ( 63176405, 'Dealul_Piscului_1970_deg',
                        6317, 8901, 9122,
                        True, 1197
              )         .MarkDeprecated( True );
  _cs.Add     ( 63186405, 'NGN_deg',
                        6318, 8901, 9122,
                        True, 1136
              )         .MarkDeprecated( True );
  _cs.Add     ( 63196405, 'KUDAMS_deg',
                        6319, 8901, 9122,
                        True, 1310
              )         .MarkDeprecated( True );
  _cs.Add     ( 63226405, 'WGS_72_deg',
                        6322, 8901, 9122,
                        True, 1262
              )         .MarkDeprecated( True );
  _cs.Add     ( 63246405, 'WGS_72BE_deg',
                        6324, 8901, 9122,
                        True, 1262
              )         .MarkDeprecated( True );
end;

procedure prepare_9;
begin
  _cs.Add     ( 63266405, 'WGS_84_deg',
                        6326, 8901, 9122,
                        True, 1262
              )         .MarkDeprecated( True );
  _cs.Add     ( 66006405, 'Anguilla_1957_deg',
                        6600, 8901, 9122,
                        True, 1030
              )         .MarkDeprecated( True );
  _cs.Add     ( 66016405, 'Antigua_1943_deg',
                        6601, 8901, 9122,
                        True, 1273
              )         .MarkDeprecated( True );
  _cs.Add     ( 66026405, 'Dominica_1945_deg',
                        6602, 8901, 9122,
                        True, 1082
              )         .MarkDeprecated( True );
  _cs.Add     ( 66036405, 'Grenada_1953_deg',
                        6603, 8901, 9122,
                        True, 1551
              )         .MarkDeprecated( True );
  _cs.Add     ( 66046405, 'Montserrat_1958_deg',
                        6604, 8901, 9122,
                        True, 1165
              )         .MarkDeprecated( True );
  _cs.Add     ( 66056405, 'St_Kitts_1955_deg',
                        6605, 8901, 9122,
                        True, 1200
              )         .MarkDeprecated( True );
  _cs.Add     ( 66066405, 'St_Lucia_1955_deg',
                        6606, 8901, 9122,
                        True, 1201
              )         .MarkDeprecated( True );
  _cs.Add     ( 66076405, 'St_Vincent_1945_deg',
                        6607, 8901, 9122,
                        True, 1202
              )         .MarkDeprecated( True );
  _cs.Add     ( 66086405, 'NAD27_76_deg',
                        6608, 8901, 9122,
                        True, 1367
              )         .MarkDeprecated( True );
  _cs.Add     ( 66096405, 'NAD27_CGQ77_deg',
                        6609, 8901, 9122,
                        True, 1368
              )         .MarkDeprecated( True );
  _cs.Add     ( 66106405, 'Xian_1980_deg',
                        6610, 8901, 9122,
                        True, 1067
              )         .MarkDeprecated( True );
  _cs.Add     ( 66116405, 'Hong_Kong_1980_deg',
                        6611, 8901, 9122,
                        True, 1118
              )         .MarkDeprecated( True );
  _cs.Add     ( 66126405, 'JGD2000_deg',
                        6612, 8901, 9122,
                        True, 1129
              )         .MarkDeprecated( True );
  _cs.Add     ( 66136405, 'Segara_deg',
                        6613, 8901, 9122,
                        True, 1360
              )         .MarkDeprecated( True );
  _cs.Add     ( 66146405, 'QND95_deg',
                        6614, 8901, 9122,
                        True, 1346
              )         .MarkDeprecated( True );
  _cs.Add     ( 66156405, 'Porto_Santo_deg',
                        6615, 8901, 9122,
                        True, 1314
              )         .MarkDeprecated( True );
  _cs.Add     ( 66166405, 'Selvagem_Grande_deg',
                        6616, 8901, 9122,
                        True, 2779
              )         .MarkDeprecated( True );
  _cs.Add     ( 66186405, 'SAD69_deg',
                        6618, 8901, 9122,
                        True, 1358
              )         .MarkDeprecated( True );
  _cs.Add     ( 66196405, 'SWEREF99_deg',
                        6619, 8901, 9122,
                        True, 1225
              )         .MarkDeprecated( True );
  _cs.Add     ( 66206405, 'Point_58_deg',
                        6620, 8901, 9122,
                        True, 2790
              )         .MarkDeprecated( True );
  _cs.Add     ( 66216405, 'Fort_Marigot_deg',
                        6621, 8901, 9122,
                        True, 2828
              )         .MarkDeprecated( True );
  _cs.Add     ( 66226405, 'Sainte_Anne_deg',
                        6622, 8901, 9122,
                        True, 2829
              )         .MarkDeprecated( True );
  _cs.Add     ( 66236405, 'CSG67_deg',
                        6623, 8901, 9122,
                        True, 1097
              )         .MarkDeprecated( True );
  _cs.Add     ( 66246405, 'RGFG95_deg',
                        6624, 8901, 9122,
                        True, 1097
              )         .MarkDeprecated( True );
  _cs.Add     ( 66256405, 'Fort_Desaix_deg',
                        6625, 8901, 9122,
                        True, 1156
              )         .MarkDeprecated( True );
  _cs.Add     ( 66266405, 'Piton_des_Neiges_deg',
                        6626, 8901, 9122,
                        True, 1196
              )         .MarkDeprecated( True );
  _cs.Add     ( 66276405, 'RGR92_deg',
                        6627, 8901, 9122,
                        True, 1196
              )         .MarkDeprecated( True );
  _cs.Add     ( 66286405, 'Tahiti_deg',
                        6628, 8901, 9122,
                        True, 2811
              )         .MarkDeprecated( True );
  _cs.Add     ( 66296405, 'Tahaa_deg',
                        6629, 8901, 9122,
                        True, 2812
              )         .MarkDeprecated( True );
  _cs.Add     ( 66306405, 'IGN72_Nuku_Hiva_deg',
                        6630, 8901, 9122,
                        True, 2810
              )         .MarkDeprecated( True );
  _cs.Add     ( 66316405, 'K0_1949_deg',
                        6631, 8901, 9122,
                        True, 2816
              )         .MarkDeprecated( True );
  _cs.Add     ( 66326405, 'Combani_1950_deg',
                        6632, 8901, 9122,
                        True, 1159
              )         .MarkDeprecated( True );
  _cs.Add     ( 66336405, 'IGN56_Lifou_deg',
                        6633, 8901, 9122,
                        True, 2814
              )         .MarkDeprecated( True );
  _cs.Add     ( 66346405, 'IGN72_Grande_Terre_deg',
                        6634, 8901, 9122,
                        True, 2822
              )         .MarkDeprecated( True );
  _cs.Add     ( 66356405, 'ST87_Ouvea_deg',
                        6635, 8901, 9122,
                        True, 2813
              )         .MarkDeprecated( True );
  _cs.Add     ( 66366405, 'Petrels_1972_deg',
                        6636, 8901, 9122,
                        True, 2817
              )         .MarkDeprecated( True );
  _cs.Add     ( 66376405, 'Perroud_1950_deg',
                        6637, 8901, 9122,
                        True, 2818
              )         .MarkDeprecated( True );
  _cs.Add     ( 66386405, 'Saint_Pierre_et_Miquelon_1950_deg',
                        6638, 8901, 9122,
                        True, 1220
              )         .MarkDeprecated( True );
  _cs.Add     ( 66396405, 'MOP78_deg',
                        6639, 8901, 9122,
                        True, 2815
              )         .MarkDeprecated( True );
  _cs.Add     ( 66406405, 'RRAF_1991_deg',
                        6640, 8901, 9122,
                        True, 2824
              )         .MarkDeprecated( True );
  _cs.Add     ( 66416405, 'IGN53_Mare_deg',
                        6641, 8901, 9122,
                        True, 2819
              )         .MarkDeprecated( True );
  _cs.Add     ( 66426405, 'ST84_Ile_des_Pins_deg',
                        6642, 8901, 9122,
                        True, 2820
              )         .MarkDeprecated( True );
  _cs.Add     ( 66436405, 'ST71_Belep_deg',
                        6643, 8901, 9122,
                        True, 2821
              )         .MarkDeprecated( True );
  _cs.Add     ( 66446405, 'NEA74_Noumea_deg',
                        6644, 8901, 9122,
                        True, 2823
              )         .MarkDeprecated( True );
  _cs.Add     ( 66456405, 'RGNC_1991_deg',
                        6645, 8901, 9122,
                        True, 1174
              )         .MarkDeprecated( True );
  _cs.Add     ( 66466405, 'Grand_Comoros_deg',
                        6646, 8901, 9122,
                        True, 1071
              )         .MarkDeprecated( True );
  _cs.Add     ( 66576405, 'Reykjavik_1900_deg',
                        6657, 8901, 9122,
                        True, 1120
              )         .MarkDeprecated( True );
  _cs.Add     ( 66586405, 'Hjorsey_1955_deg',
                        6658, 8901, 9122,
                        True, 1120
              )         .MarkDeprecated( True );
  _cs.Add     ( 66596405, 'ISN93_deg',
                        6659, 8901, 9122,
                        True, 1120
              )         .MarkDeprecated( True );
  _cs.Add     ( 66606405, 'Helle_1954_deg',
                        6660, 8901, 9122,
                        True, 2869
              )         .MarkDeprecated( True );
  _cs.Add     ( 66616405, 'LKS92_deg',
                        6661, 8901, 9122,
                        True, 1139
              )         .MarkDeprecated( True );
  _cs.Add     ( 66636405, 'Porto_Santo_1995_deg',
                        6663, 8901, 9122,
                        True, 1314
              )         .MarkDeprecated( True );
  _cs.Add     ( 66646405, 'Azores_Oriental_1995_deg',
                        6664, 8901, 9122,
                        True, 1345
              )         .MarkDeprecated( True );
  _cs.Add     ( 66656405, 'Azores_Central_1995_deg',
                        6665, 8901, 9122,
                        True, 1301
              )         .MarkDeprecated( True );
  _cs.Add     ( 66666405, 'Lisbon_1890_deg',
                        6666, 8901, 9122,
                        True, 1294
              )         .MarkDeprecated( True );
  _cs.Add     ( 66676405, 'IKBD_92_deg',
                        6667, 8901, 9122,
                        True, 2876
              )         .MarkDeprecated( True );
  _cs.Add     ( 68016405, 'Bern_1898_Bern_deg',
                        6801, 8907, 9122,
                        True, 1286
              )         .MarkDeprecated( True );
  _cs.Add     ( 68026405, 'Bogota_1975_Bogota_deg',
                        6802, 8904, 9122,
                        True, 1070
              )         .MarkDeprecated( True );
  _cs.Add     ( 68036405, 'Lisbon_Lisbon_deg',
                        6803, 8902, 9122,
                        True, 1294
              )         .MarkDeprecated( True );
  _cs.Add     ( 68046405, 'Makassar_Jakarta_deg',
                        6804, 8908, 9122,
                        True, 1316
              )         .MarkDeprecated( True );
  _cs.Add     ( 68056405, 'MGI_Ferro_deg',
                        6805, 8909, 9122,
                        True, 1321
              )         .MarkDeprecated( True );
  _cs.Add     ( 68066405, 'Monte_Mario_Rome_deg',
                        6806, 8906, 9122,
                        True, 1127
              )         .MarkDeprecated( True );
  _cs.Add     ( 68086405, 'Padang_Jakarta_deg',
                        6808, 8908, 9122,
                        True, 1355
              )         .MarkDeprecated( True );
  _cs.Add     ( 68096405, 'Belge_1950_Brussels_deg',
                        6809, 8910, 9122,
                        True, 1347
              )         .MarkDeprecated( True );
  _cs.Add     ( 68136405, 'Batavia_Jakarta_deg',
                        6813, 8908, 9122,
                        True, 1285
              )         .MarkDeprecated( True );
  _cs.Add     ( 68146405, 'RT38_Stockholm_deg',
                        6814, 8911, 9122,
                        True, 1225
              )         .MarkDeprecated( True );
  _cs.Add     ( 68156405, 'Greek_Athens_deg',
                        6815, 8912, 9122,
                        True, 1106
              )         .MarkDeprecated( True );
  _cs.Add     ( 68186405, 'S_JTSK_Ferro_deg',
                        6818, 8909, 9122,
                        True, 1306
              )         .MarkDeprecated( True );
  _cs.Add     ( 68206405, 'Segara_Jakarta_deg',
                        6820, 8908, 9122,
                        True, 1285
              )         .MarkDeprecated( True );
  _cs.Add     ( 69036405, 'Madrid_1870_Madrid_deg',
                        6903, 8905, 9122,
                        True, 1217
              )         .MarkDeprecated( True );
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
  prepare_4;
  prepare_5;
  prepare_6;
  prepare_7;
  prepare_8;
  prepare_9;
end;

//==================================== END ====================================

