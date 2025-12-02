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
  List of Datums.
  Generated from EPSG 9.1 dataset.
}


procedure Init_CSDatumList(
  const _cs : TGIS_CSDatumList
) ;

procedure prepare_1;
begin
  _cs.Add     (   1024, 'Hungarian_Datum_1909',
                        7004, 3817
              );
  _cs.AddAlias(     -1, 'HD1909',
                        1024
              );
  _cs.Add     (   1025, 'Taiwan_Datum_1967',
                        7050, 0
              );
  _cs.AddAlias(     -1, 'TWD67',
                        1025
              );
  _cs.Add     (   1026, 'Taiwan_Datum_1997',
                        7019, 3830
              );
  _cs.AddAlias(     -1, 'TWD97',
                        1026
              );
  _cs.Add     (   1029, 'Iraqi_Geospatial_Reference_System',
                        7019, 3894
              );
  _cs.AddAlias(     -1, 'IGRS',
                        1029
              );
  _cs.Add     (   1031, 'MGI_1901',
                        7004, 8688
              );
  _cs.AddAlias(     -1, 'D48',
                        1031
              );
  _cs.AddAlias(     -1, 'HR1901',
                        1031
              );
  _cs.Add     (   1032, 'MOLDREF99',
                        7019, 5585
              );
  _cs.Add     (   1033, 'Reseau_Geodesique_de_la_RDC_2005',
                        7019, 4064
              );
  _cs.AddAlias(     -1, 'RGRDC_2005',
                        1033
              );
  _cs.Add     (   1034, 'Serbian_Reference_Network_1998',
                        7019, 4077
              );
  _cs.AddAlias(     -1, 'SREF98',
                        1034
              );
  _cs.Add     (   1035, 'Red_Geodesica_de_Canarias_1995',
                        7019, 4084
              );
  _cs.AddAlias(     -1, 'REGCAN95',
                        1035
              );
  _cs.Add     (   1036, 'Reseau_Geodesique_de_Mayotte_2004',
                        7019, 4476
              );
  _cs.AddAlias(     -1, 'RGM04',
                        1036
              );
  _cs.Add     (   1037, 'Cadastre_1997',
                        7022, 4290
              );
  _cs.Add     (   1038, 'Reseau_Geodesique_de_Saint_Pierre_et_Miquelon_2006',
                        7019, 4477
              );
  _cs.AddAlias(     -1, 'RGSPM06',
                        1038
              );
  _cs.Add     (   1041, 'Autonomous_Regions_of_Portugal_2008',
                        7019, 4905
              );
  _cs.AddAlias(     -1, 'PTRA08',
                        1041
              );
  _cs.Add     (   1042, 'Mexico_ITRF92',
                        7019, 4832
              );
  _cs.AddAlias(     -1, 'Mexican_Datum_of_1993',
                        1042
              );
  _cs.AddAlias(     -1, 'Red_Geodesica_Nacional_1992',
                        1042
              );
  _cs.Add     (   1043, 'China_2000',
                        7019, 0
              );
  _cs.Add     (   1044, 'Sao_Tome',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'Fortaleza',
                        1044
              );
  _cs.AddAlias(     -1, 'Island_of_Sao_Tome_datum',
                        1044
              );
  _cs.Add     (   1045, 'New_Beijing',
                        7024, 0
              );
  _cs.Add     (   1046, 'Principe',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'Island_of_Principe_datum',
                        1046
              );
  _cs.AddAlias(     -1, 'Morro_do_Papagaio',
                        1046
              );
  _cs.Add     (   1047, 'Reseau_de_Reference_des_Antilles_Francaises_1991',
                        7019, 4560
              );
  _cs.AddAlias(     -1, 'RRAF91',
                        1047
              );
  _cs.Add     (   1048, 'Tokyo_1892',
                        7004, 6712, 5191
              );
  _cs.AddAlias(     -1, 'Tokyo_1898',
                        1048
              );
  _cs.Add     (   1052, 'System_of_the_Unified_Trigonometrical_Cadastral_Network_05',
                        7004, 5227
              );
  _cs.AddAlias(     -1, 'S_JTSK_05',
                        1052
              );
  _cs.Add     (   1053, 'Sri_Lanka_Datum_1999',
                        7015, 5236
              );
  _cs.AddAlias(     -1, 'SLD99',
                        1053
              );
  _cs.Add     (   1055, 'System_of_the_Unified_Trigonometrical_Cadastral_Network_05_Ferro',
                        7004, 5227
              );
  _cs.AddAlias(     -1, 'S_JTSK_05_Ferro',
                        1055
              );
  _cs.Add     (   1056, 'Geocentric_Datum_Brunei_Darussalam_2009',
                        7019, 6701
              );
  _cs.AddAlias(     -1, 'GDBD2009',
                        1056
              );
(* ilker silme  _cs.Add     (   1057, 'Turkish_National_Reference_Frame',
                        7019, 5261
              ); *)
  _cs.Add     (   1057, 'Turkish_National_Reference_Frame',
                        7019, 0
              ); // ilker ekleme			  
  _cs.AddAlias(     -1, 'TUREF',
                        1057
              );
  _cs.Add     (   1058, 'Bhutan_National_Geodetic_Datum',
                        7019, 5267
              );
  _cs.AddAlias(     -1, 'DRUKREF_03',
                        1058
              );
  _cs.Add     (   1060, 'Islands_Net_2004',
                        7019, 5327
              );
  _cs.AddAlias(     -1, 'ISN2004',
                        1060
              );
  _cs.Add     (   1061, 'International_Terrestrial_Reference_Frame_2008',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ITRF2008',
                        1061
              );
  _cs.Add     (   1062, 'Posiciones_Geodesicas_Argentinas_2007',
                        7030, 9264
              );
  _cs.AddAlias(     -1, 'POSGAR_2007',
                        1062
              );
  _cs.Add     (   1063, 'Marco_Geodesico_Nacional_de_Bolivia',
                        7019, 5374
              );
  _cs.AddAlias(     -1, 'MARGEN',
                        1063
              );
  _cs.Add     (   1064, 'SIRGAS_Chile_realization_1_epoch_2002',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Chile_2002',
                        1064
              );
  _cs.Add     (   1065, 'Costa_Rica_2005',
                        7030, 9752
              );
  _cs.AddAlias(     -1, 'CR05',
                        1065
              );
  _cs.Add     (   1066, 'Sistema_Geodesico_Nacional_de_Panama_MACARIO_SOLIS',
                        7019, 5377
              );
  _cs.AddAlias(     -1, 'MACARIO_SOLIS',
                        1066
              );
  _cs.Add     (   1067, 'Peru96',
                        7019, 5378
              );
  _cs.Add     (   1068, 'SIRGAS_ROU98',
                        7030, 5384
              );
  _cs.Add     (   1069, 'SIRGAS_ES2007_8',
                        7019, 5395
              );
  _cs.Add     (   1070, 'Ocotepeque_1935',
                        7008, 5470
              );
  _cs.Add     (   1071, 'Sibun_Gorge_1922',
                        7007, 0
              );
  _cs.Add     (   1072, 'Panama_Colon_1911',
                        7008, 0
              );
  _cs.AddAlias(     -1, 'Balboa',
                        1072
              );
  _cs.Add     (   1073, 'Reseau_Geodesique_des_Antilles_Francaises_2009',
                        7019, 5501
              );
  _cs.AddAlias(     -1, 'RGAF09',
                        1073
              );
  _cs.Add     (   1074, 'Corrego_Alegre_1961',
                        7022, 5525
              );
  _cs.Add     (   1075, 'South_American_Datum_1969_96',
                        7050, 5529, 5881
              );
  _cs.Add     (   1076, 'Papua_New_Guinea_Geodetic_Datum_1994',
                        7019, 5553
              );
  _cs.AddAlias(     -1, 'PNG94',
                        1076
              );
  _cs.Add     (   1077, 'Ukraine_2000',
                        7024, 5840
              );
  _cs.Add     (   1078, 'Fehmarnbelt_Datum_2010',
                        7019, 5599
              );
  _cs.AddAlias(     -1, 'FEH10',
                        1078
              );
  _cs.Add     (   1081, 'Deutsche_Bahn_Reference_System',
                        7004, 5826
              );
  _cs.AddAlias(     -1, 'DB_REF',
                        1081
              );
  _cs.Add     (   1095, 'Tonga_Geodetic_Datum_2005',
                        7019, 9771
              );
  _cs.AddAlias(     -1, 'TGD2005',
                        1095
              );
  _cs.Add     (   1100, 'Cayman_Islands_Geodetic_Datum_2011',
                        7019, 6177
              );
  _cs.AddAlias(     -1, 'CIGD11',
                        1100
              );
  _cs.Add     (   1111, 'Nepal_1981',
                        7015, 6208
              );
  _cs.Add     (   1112, 'Cyprus_Geodetic_Reference_System_1993',
                        7030, 7445
              );
  _cs.AddAlias(     -1, 'CGRS93',
                        1112
              );
  _cs.Add     (   1113, 'Reseau_Geodesique_des_Terres_Australes_et_Antarctiques_Francaises_2007',
                        7019, 9770
              );
  _cs.AddAlias(     -1, 'Reseau_Geodesique_des_TAAF_2007',
                        1113
              );
  _cs.AddAlias(     -1, 'RGTAAF07',
                        1113
              );
  _cs.Add     (   1114, 'Israeli_Geodetic_Datum_2005',
                        7030, 0
              );
  _cs.Add     (   1115, 'Israeli_Geodetic_Datum_2005_2012',
                        7030, 0
              );
  _cs.Add     (   1116, 'NAD83_National_Spatial_Reference_System_2011',
                        7019, 9774
              );
  _cs.AddAlias(     -1, 'NAD83_2011',
                        1116
              );
  _cs.Add     (   1117, 'NAD83_National_Spatial_Reference_System_PA11',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_PA11',
                        1117
              );
end;

procedure prepare_2;
begin
  _cs.Add     (   1118, 'NAD83_National_Spatial_Reference_System_MA11',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_MA11',
                        1118
              );
  _cs.Add     (   1120, 'Mexico_ITRF2008',
                        7019, 6373
              );
  _cs.AddAlias(     -1, 'Red_Geodesica_Nacional_2008',
                        1120
              );
  _cs.Add     (   1128, 'Japanese_Geodetic_Datum_2011',
                        7019, 9936
              );
  _cs.AddAlias(     -1, 'JGD2011',
                        1128
              );
  _cs.Add     (   1132, 'Rete_Dinamica_Nazionale_2008',
                        7019, 6711
              );
  _cs.AddAlias(     -1, 'RDN2008',
                        1132
              );
  _cs.Add     (   1133, 'NAD83_Continuously_Operating_Reference_Station_1996',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CORS96',
                        1133
              );
  _cs.Add     (   1135, 'Aden_1925',
                        7012, 6910
              );
  _cs.Add     (   1136, 'Bioko',
                        7022, 6912
              );
  _cs.Add     (   1137, 'Bekaa_Valley_1920',
                        7012, 6911
              );
  _cs.Add     (   1138, 'South_East_Island_1943',
                        7012, 6914
              );
  _cs.Add     (   1139, 'Gambia',
                        7012, 6913
              );
  _cs.Add     (   1141, 'IGS08',
                        7019, 0
              );
  _cs.Add     (   1142, 'IG05_Intermediate_Datum',
                        7019, 0
              );
  _cs.Add     (   1143, 'Israeli_Geodetic_Datum_2005_A',
                        7019, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   1144, 'IG05_12_Intermediate_Datum',
                        7019, 0
              );
  _cs.Add     (   1145, 'Israeli_Geodetic_Datum_2005_2012_A',
                        7019, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   1147, 'Oman_National_Geodetic_Datum_2014',
                        7019, 7443
              );
  _cs.AddAlias(     -1, 'ONGD14',
                        1147
              );
  _cs.Add     (   1152, 'World_Geodetic_System_1984_G730',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G730',
                        1152
              );
  _cs.Add     (   1153, 'World_Geodetic_System_1984_G873',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G873',
                        1153
              );
  _cs.Add     (   1154, 'World_Geodetic_System_1984_G1150',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G1150',
                        1154
              );
  _cs.Add     (   1155, 'World_Geodetic_System_1984_G1674',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G1674',
                        1155
              );
  _cs.Add     (   1156, 'World_Geodetic_System_1984_G1762',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G1762',
                        1156
              );
  _cs.AddAlias(     -1, 'World_Geodetic_System_1984_G1762_A',
                        1156
              );
  _cs.Add     (   1157, 'Parametry_Zemli_1990_02',
                        7054, 0
              );
  _cs.AddAlias(     -1, 'PZ_90_02',
                        1157
              );
  _cs.Add     (   1158, 'Parametry_Zemli_1990_11',
                        7054, 0
              );
  _cs.AddAlias(     -1, 'PZ_90_11',
                        1158
              );
  _cs.Add     (   1159, 'Geodezicheskaya_Sistema_Koordinat_2011',
                        1025, 9773
              );
  _cs.AddAlias(     -1, 'GRS_2011',
                        1159
              );
  _cs.AddAlias(     -1, 'GSK_2011',
                        1159
              );
  _cs.Add     (   1160, 'Kyrgyzstan_Geodetic_Datum_2006',
                        7019, 9768
              );
  _cs.AddAlias(     -1, 'Kyrg_06',
                        1160
              );
  _cs.Add     (   1165, 'International_Terrestrial_Reference_Frame_2014',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ITRF2014',
                        1165
              );
  _cs.Add     (   1166, 'World_Geodetic_System_1984_Transit',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_Transit',
                        1166
              );
  _cs.Add     (   1167, 'Bulgaria_Geodetic_System_2005',
                        7019, 8890
              );
  _cs.AddAlias(     -1, 'BGS2005',
                        1167
              );
  _cs.Add     (   1168, 'Geocentric_Datum_of_Australia_2020',
                        7019, 8450
              );
  _cs.AddAlias(     -1, 'GDA2020',
                        1168
              );
  _cs.Add     (   1173, 'St_Helena_Tritan',
                        7030, 7898
              );
  _cs.AddAlias(     -1, 'WGS_84_Tritan_St_Helena',
                        1173
              );
  _cs.Add     (   1174, 'St_Helena_Geodetic_Datum_2015',
                        7019, 7892
              );
  _cs.AddAlias(     -1, 'SHGD2015',
                        1174
              );
  _cs.Add     (   1178, 'European_Terrestrial_Reference_Frame_1989',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF89',
                        1178
              );
  _cs.Add     (   1179, 'European_Terrestrial_Reference_Frame_1990',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF90',
                        1179
              );
  _cs.Add     (   1180, 'European_Terrestrial_Reference_Frame_1991',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF91',
                        1180
              );
  _cs.Add     (   1181, 'European_Terrestrial_Reference_Frame_1992',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF92',
                        1181
              );
  _cs.Add     (   1182, 'European_Terrestrial_Reference_Frame_1993',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF93',
                        1182
              );
  _cs.Add     (   1183, 'European_Terrestrial_Reference_Frame_1994',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF94',
                        1183
              );
  _cs.Add     (   1184, 'European_Terrestrial_Reference_Frame_1996',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF96',
                        1184
              );
  _cs.Add     (   1185, 'European_Terrestrial_Reference_Frame_1997',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF97',
                        1185
              );
  _cs.Add     (   1186, 'European_Terrestrial_Reference_Frame_2000',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF2000',
                        1186
              );
  _cs.Add     (   1187, 'Islands_Net_2016',
                        7019, 9291
              );
  _cs.AddAlias(     -1, 'ISN2016',
                        1187
              );
  _cs.Add     (   1188, 'Gusterberg_Ferro',
                        1026, 0
              );
  _cs.Add     (   1189, 'St_Stephen_Ferro',
                        1026, 0
              );
  _cs.Add     (   1191, 'IGS14',
                        7019, 0
              );
  _cs.Add     (   1192, 'North_American_Datum_of_1983_CSRS96',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v1',
                        1192
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS96',
                        1192
              );
  _cs.Add     (   1193, 'North_American_Datum_of_1983_CSRS_version_2',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_1997',
                        1193
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v2',
                        1193
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS98',
                        1193
              );
  _cs.Add     (   1194, 'North_American_Datum_of_1983_CSRS_version_3',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_1997_A',
                        1194
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v3',
                        1194
              );
  _cs.Add     (   1195, 'North_American_Datum_of_1983_CSRS_version_4',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_2002',
                        1195
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v4',
                        1195
              );
  _cs.Add     (   1196, 'North_American_Datum_of_1983_CSRS_version_5',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_2005',
                        1196
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v5',
                        1196
              );
  _cs.Add     (   1197, 'North_American_Datum_of_1983_CSRS_version_6',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_2010',
                        1197
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v6',
                        1197
              );
  _cs.Add     (   1198, 'North_American_Datum_of_1983_CSRS_version_7',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_2010_A',
                        1198
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS_v7',
                        1198
              );
  _cs.Add     (   1201, 'System_of_the_Unified_Trigonometrical_Cadastral_Network_JTSK03',
                        7004, 8368
              );
end;

procedure prepare_3;
begin
  _cs.AddAlias(     -1, 'S_JTSK_JTSK03',
                        1201
              );
  _cs.Add     (   1204, 'European_Terrestrial_Reference_Frame_2005',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF2005',
                        1204
              );
  _cs.Add     (   1206, 'European_Terrestrial_Reference_Frame_2014',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ETRF2014',
                        1206
              );
  _cs.Add     (   1207, 'Macao_1920',
                        7022, 8438
              );
  _cs.Add     (   1208, 'Macao_Geodetic_Datum_2008',
                        7019, 8436
              );
  _cs.AddAlias(     -1, 'Macao_2008',
                        1208
              );
  _cs.Add     (   1209, 'Hong_Kong_Geodetic',
                        7019, 8439
              );
  _cs.Add     (   1211, 'NAD83_Federal_Base_Network',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_FBN',
                        1211
              );
  _cs.Add     (   1212, 'NAD83_High_Accuracy_Reference_Network__Corrected',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_HARN_Corrected',
                        1212
              );
  _cs.Add     (   1214, 'Serbian_Spatial_Reference_System_2000',
                        7019, 8894
              );
  _cs.AddAlias(     -1, 'STRS00',
                        1214
              );
  _cs.AddAlias(     -1, 'SRB_ETRS89',
                        1214
              );
  _cs.Add     (   1217, 'Camacupa_2015',
                        7012, 8882
              );
  _cs.Add     (   1218, 'MOMRA_Terrestrial_Reference_Frame_2000',
                        7019, 8822
              );
  _cs.AddAlias(     -1, 'MGD_2000',
                        1218
              );
  _cs.AddAlias(     -1, 'MTRF_2000',
                        1218
              );
  _cs.Add     (   1220, 'Reference_System_de_Angola_2013',
                        7019, 8819
              );
  _cs.AddAlias(     -1, 'Rede_Permanentes_de_Estaciones_GNSS_de_Angola',
                        1220
              );
  _cs.AddAlias(     -1, 'REPANGOL',
                        1220
              );
  _cs.AddAlias(     -1, 'RSAO13',
                        1220
              );
  _cs.Add     (   1221, 'North_American_Datum_of_1983_MARP00',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_MARP00',
                        1221
              );
  _cs.Add     (   1223, 'Reseau_Geodesique_de_Wallis_et_Futuna_1996',
                        7019, 9769
              );
  _cs.AddAlias(     -1, 'RGWF96',
                        1223
              );
  _cs.Add     (   1225, 'CR_SIRGAS',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'CR14',
                        1225
              );
  _cs.Add     (   1227, 'SIRGAS_Continuously_Operating_Network_DGF00P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2000',
                        1227
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF00P01',
                        1227
              );
  _cs.Add     (   1228, 'SIRGAS_Continuously_Operating_Network_DGF01P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2001',
                        1228
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF01P01',
                        1228
              );
  _cs.Add     (   1229, 'SIRGAS_Continuously_Operating_Network_DGF01P02',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2001_extended',
                        1229
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF01P02',
                        1229
              );
  _cs.Add     (   1230, 'SIRGAS_Continuously_Operating_Network_DGF02P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2002',
                        1230
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF02P01',
                        1230
              );
  _cs.Add     (   1231, 'SIRGAS_Continuously_Operating_Network_DGF04P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2004',
                        1231
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF04P01',
                        1231
              );
  _cs.Add     (   1232, 'SIRGAS_Continuously_Operating_Network_DGF05P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2005',
                        1232
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF05P01',
                        1232
              );
  _cs.Add     (   1233, 'SIRGAS_Continuously_Operating_Network_DGF06P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2006',
                        1233
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF06P01',
                        1233
              );
  _cs.Add     (   1234, 'SIRGAS_Continuously_Operating_Network_DGF07P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2007',
                        1234
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF07P01',
                        1234
              );
  _cs.Add     (   1235, 'SIRGAS_Continuously_Operating_Network_DGF08P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2008',
                        1235
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_DGF08P01',
                        1235
              );
  _cs.Add     (   1236, 'SIRGAS_Continuously_Operating_Network_SIR09P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2009',
                        1236
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR09P01',
                        1236
              );
  _cs.Add     (   1237, 'SIRGAS_Continuously_Operating_Network_SIR10P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2010',
                        1237
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR10P01',
                        1237
              );
  _cs.Add     (   1238, 'SIRGAS_Continuously_Operating_Network_SIR11P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2011',
                        1238
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR11P01',
                        1238
              );
  _cs.Add     (   1239, 'SIRGAS_Continuously_Operating_Network_SIR13P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2013',
                        1239
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR13P01',
                        1239
              );
  _cs.Add     (   1240, 'SIRGAS_Continuously_Operating_Network_SIR14P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2014',
                        1240
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR14P01',
                        1240
              );
  _cs.Add     (   1241, 'SIRGAS_Continuously_Operating_Network_SIR15P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2015',
                        1241
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR15P01',
                        1241
              );
  _cs.Add     (   1242, 'SIRGAS_Continuously_Operating_Network_SIR17P01',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Multi_Year_Solution_2017',
                        1242
              );
  _cs.AddAlias(     -1, 'SIRGAS_CON_SIR17P01',
                        1242
              );
  _cs.Add     (   1243, 'SIRGAS_Chile_realization_2_epoch_2010',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Chile_2010',
                        1243
              );
  _cs.Add     (   1244, 'IGS97',
                        7019, 0
              );
  _cs.Add     (   1245, 'IGS00',
                        7019, 0
              );
  _cs.Add     (   1246, 'IGb00',
                        7019, 0
              );
  _cs.Add     (   1247, 'IGS05',
                        7019, 0
              );
  _cs.Add     (   1248, 'IGb08',
                        7019, 0
              );
  _cs.Add     (   1249, 'North_American_Datum_of_1983_PACP00',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'NAD83_PACP00',
                        1249
              );
  _cs.Add     (   1251, 'Kosovo_Reference_System_2001',
                        7019, 9144
              );
  _cs.AddAlias(     -1, 'KOSOVAREF01',
                        1251
              );
  _cs.AddAlias(     -1, 'KOSOVAREF01_2012',
                        1251
              );
  _cs.AddAlias(     -1, 'KOSOVAREF12',
                        1251
              );
  _cs.Add     (   1252, 'SIRGAS_Chile_realization_3_epoch_2013',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Chile_2013',
                        1252
              );
  _cs.Add     (   1253, 'SIRGAS_Chile_realization_4_epoch_2016',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'SIRGAS_Chile_2016',
                        1253
              );
  _cs.Add     (   1254, 'SIRGAS_Chile',
                        7019, 5375
              )         .MarkDeprecated( True );
  _cs.Add     (   1257, 'Tapi_Aike',
                        7022, 9260
              );
  _cs.Add     (   1258, 'Ministerio_de_Marina_Norte',
                        7022, 9261
              );
  _cs.AddAlias(     -1, 'MMN',
                        1258
              );
  _cs.Add     (   1259, 'Ministerio_de_Marina_Sur',
                        7022, 9262
              );
end;

procedure prepare_4;
begin
  _cs.AddAlias(     -1, 'MMS',
                        1259
              );
  _cs.Add     (   1263, 'Oman_National_Geodetic_Datum_2017',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ONGD17',
                        1263
              );
  _cs.Add     (   1264, 'HS2_Intermediate_Reference_Frame',
                        7019, 9302
              );
  _cs.AddAlias(     -1, 'HS2_IRF',
                        1264
              );
  _cs.Add     (   1266, 'TPEN11_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'TPEN11_IRF',
                        1266
              );
  _cs.Add     (   1268, 'Kingdom_of_Saudi_Arabia_Geodetic_Reference_Frame_2017',
                        7019, 9383
              );
  _cs.AddAlias(     -1, 'KSA_GRF17',
                        1268
              );
  _cs.Add     (   1271, 'MML07_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'MML07_IRF',
                        1271
              );
  _cs.Add     (   1272, 'IGb14',
                        7019, 0
              );
  _cs.Add     (   1273, 'AbInvA96_2020_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'AbInvA96_2020_IRF',
                        1273
              );
  _cs.Add     (   1286, 'Pico_de_las_Nieves_1968',
                        7022, 9743
              );
  _cs.AddAlias(     -1, 'PN68',
                        1286
              );
  _cs.Add     (   1289, 'GBK19_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'GBK19_IRF',
                        1289
              );
  _cs.Add     (   1291, 'Australian_Terrestrial_Reference_Frame_2014',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ATRF2014',
                        1291
              );
  _cs.Add     (   1293, 'Sistem_Referensi_Geospasial_Indonesia_2013',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'Indonesian_Geospatial_Reference_System_2013',
                        1293
              );
  _cs.AddAlias(     -1, 'Jaring_Kontrol_Geodesi_JKG',
                        1293
              );
  _cs.AddAlias(     -1, 'SRGI2013',
                        1293
              );
  _cs.Add     (   1295, 'Lyon_Turin_Ferroviaire_2004',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'LTF2004',
                        1295
              );
  _cs.Add     (   1304, 'Red_Geodesica_Para_Mineria_en_Chile',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'REDGEOMIN',
                        1304
              );
  _cs.Add     (   1305, 'ETRF2000_Poland',
                        7019, 9772
              );
  _cs.AddAlias(     -1, 'ETRF2000_PL',
                        1305
              );
  _cs.Add     (   1308, 'EOS21_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'EOS21_IRF',
                        1308
              );
  _cs.Add     (   1309, 'World_Geodetic_System_1984_G2139',
                        7030, 0
              );
  _cs.AddAlias(     -1, 'WGS_84_G2139',
                        1309
              );
  _cs.Add     (   1310, 'ECML14_NB_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ECML14_NB_IRF',
                        1310
              );
  _cs.Add     (   1311, 'EWR2_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'EWR2_IRF',
                        1311
              );
  _cs.Add     (   1312, 'Reseau_Geodesique_Francais_1993_v2',
                        7019, 9791
              );
  _cs.AddAlias(     -1, 'RGF93_v2',
                        1312
              );
  _cs.Add     (   1313, 'Reseau_Geodesique_Francais_1993_v2b',
                        7019, 9792
              );
  _cs.AddAlias(     -1, 'RGF93_v2b',
                        1313
              );
  _cs.Add     (   1314, 'MRH21_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'MRH21_IRF',
                        1314
              );
  _cs.Add     (   1315, 'MOLDOR11_Intermediate_Reference_Frame',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'MOLDOR11_IRF',
                        1315
              );
  _cs.Add     (   6001, 'Not_specified_based_on_Airy_1830_ellipsoid',
                        7001, 0
              );
  _cs.Add     (   6002, 'Not_specified_based_on_Airy_Modified_1849_ellipsoid',
                        7002, 0
              );
  _cs.Add     (   6003, 'Not_specified_based_on_Australian_National_Spheroid',
                        7050, 0
              );
  _cs.Add     (   6004, 'Not_specified_based_on_Bessel_1841_ellipsoid',
                        7004, 0
              );
  _cs.Add     (   6005, 'Not_specified_based_on_Bessel_Modified_ellipsoid',
                        7005, 0
              );
  _cs.Add     (   6006, 'Not_specified_based_on_Bessel_Namibia_ellipsoid',
                        7046, 0
              );
  _cs.Add     (   6007, 'Not_specified_based_on_Clarke_1858_ellipsoid',
                        7007, 0
              );
  _cs.Add     (   6008, 'Not_specified_based_on_Clarke_1866_ellipsoid',
                        7008, 0
              );
  _cs.Add     (   6009, 'Not_specified_based_on_Clarke_1866_Michigan_ellipsoid',
                        7009, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6010, 'Not_specified_based_on_Clarke_1880_Benoit_ellipsoid',
                        7010, 0
              );
  _cs.Add     (   6011, 'Not_specified_based_on_Clarke_1880_IGN_ellipsoid',
                        7011, 0
              );
  _cs.Add     (   6012, 'Not_specified_based_on_Clarke_1880_RGS_ellipsoid',
                        7012, 0
              );
  _cs.Add     (   6013, 'Not_specified_based_on_Clarke_1880_Arc_ellipsoid',
                        7013, 0
              );
  _cs.Add     (   6014, 'Not_specified_based_on_Clarke_1880_SGA_1922_ellipsoid',
                        7014, 0
              );
  _cs.Add     (   6015, 'Not_specified_based_on_Everest_1830_1937_Adjustment_ellipsoid',
                        7015, 0
              );
  _cs.Add     (   6016, 'Not_specified_based_on_Everest_1830_1967_Definition_ellipsoid',
                        7016, 0
              );
  _cs.Add     (   6018, 'Not_specified_based_on_Everest_1830_Modified_ellipsoid',
                        7018, 0
              );
  _cs.Add     (   6019, 'Not_specified_based_on_GRS_1980_ellipsoid',
                        7019, 0
              );
  _cs.Add     (   6020, 'Not_specified_based_on_Helmert_1906_ellipsoid',
                        7020, 0
              );
  _cs.Add     (   6021, 'Not_specified_based_on_Indonesian_National_Spheroid',
                        7021, 0
              );
  _cs.Add     (   6022, 'Not_specified_based_on_International_1924_ellipsoid',
                        7022, 0
              );
  _cs.Add     (   6024, 'Not_specified_based_on_Krassowsky_1940_ellipsoid',
                        7024, 0
              );
  _cs.Add     (   6025, 'Not_specified_based_on_NWL_9D_ellipsoid',
                        7025, 0
              );
  _cs.Add     (   6027, 'Not_specified_based_on_Plessis_1817_ellipsoid',
                        7027, 0
              );
  _cs.Add     (   6028, 'Not_specified_based_on_Struve_1860_ellipsoid',
                        7028, 0
              );
  _cs.Add     (   6029, 'Not_specified_based_on_War_Office_ellipsoid',
                        7029, 0
              );
  _cs.Add     (   6030, 'Not_specified_based_on_WGS_84_ellipsoid',
                        7030, 0
              );
  _cs.Add     (   6031, 'Not_specified_based_on_GEM_10C_ellipsoid',
                        7030, 0
              );
  _cs.Add     (   6032, 'Not_specified_based_on_OSU86F_ellipsoid',
                        7032, 0
              );
  _cs.Add     (   6033, 'Not_specified_based_on_OSU91A_ellipsoid',
                        7033, 0
              );
  _cs.Add     (   6034, 'Not_specified_based_on_Clarke_1880_ellipsoid',
                        7034, 0
              );
  _cs.Add     (   6035, 'Not_specified_based_on_Authalic_Sphere',
                        7035, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6036, 'Not_specified_based_on_GRS_1967_ellipsoid',
                        7036, 0
              );
  _cs.Add     (   6041, 'Not_specified_based_on_Average_Terrestrial_System_1977_ellipsoid',
                        7041, 0
              );
  _cs.Add     (   6042, 'Not_specified_based_on_Everest_1830_Definition_ellipsoid',
                        7042, 0
              );
  _cs.Add     (   6043, 'Not_specified_based_on_WGS_72_ellipsoid',
                        7043, 0
              );
  _cs.Add     (   6044, 'Not_specified_based_on_Everest_1830_1962_Definition_ellipsoid',
                        7044, 0
              );
  _cs.Add     (   6045, 'Not_specified_based_on_Everest_1830_1975_Definition_ellipsoid',
                        7045, 0
              );
  _cs.Add     (   6047, 'Not_specified_based_on_GRS_1980_Authalic_Sphere',
                        7048, 0
              );
  _cs.Add     (   6052, 'Not_specified_based_on_Clarke_1866_Authalic_Sphere',
                        7052, 0
              );
  _cs.Add     (   6053, 'Not_specified_based_on_International_1924_Authalic_Sphere',
                        7057, 0
              );
  _cs.Add     (   6054, 'Not_specified_based_on_Hughes_1980_ellipsoid',
                        7058, 0
              );
  _cs.Add     (   6055, 'Popular_Visualisation_Datum',
                        7059, 15973
              )         .MarkDeprecated( True );
  _cs.Add     (   6120, 'Greek',
                        7004, 0
              );
  _cs.Add     (   6121, 'Greek_Geodetic_Reference_System_1987',
                        7019, 1272
              );
  _cs.AddAlias(     -1, 'GGRS87',
                        6121
              );
  _cs.Add     (   6122, 'Average_Terrestrial_System_1977',
                        7041, 1688
              );
  _cs.AddAlias(     -1, 'ATS77',
                        6122
              );
  _cs.Add     (   6123, 'Kartastokoordinaattijarjestelma_1966',
                        7022, 10099
              );
  _cs.AddAlias(     -1, 'KKJ',
                        6123
              );
  _cs.Add     (   6124, 'Rikets_koordinatsystem_1990',
                        7004, 1896
              );
  _cs.AddAlias(     -1, 'RT90',
                        6124
              );
  _cs.Add     (   6125, 'Samboja',
                        7004, 1282
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Samboja_P2_exc_T9',
                        6125
              );
end;

procedure prepare_5;
begin
  _cs.Add     (   6126, 'Lithuania_1994_ETRS89',
                        7019, 1283
              );
  _cs.AddAlias(     -1, 'LKS94_ETRS89',
                        6126
              );
  _cs.Add     (   6127, 'Tete',
                        7008, 1684
              );
  _cs.AddAlias(     -1, 'Tete_1960',
                        6127
              );
  _cs.Add     (   6128, 'Madzansua',
                        7008, 0
              );
  _cs.Add     (   6129, 'Observatario',
                        7008, 6900
              );
  _cs.Add     (   6130, 'Moznet_ITRF94',
                        7030, 1302
              );
  _cs.AddAlias(     -1, 'Moznet',
                        6130
              );
  _cs.Add     (   6131, 'Indian_1960',
                        7015, 1542
              );
  _cs.AddAlias(     -1, 'Indian_DMA_Reduced',
                        6131
              );
  _cs.Add     (   6132, 'Final_Datum_1958',
                        7012, 1513
              );
  _cs.AddAlias(     -1, 'FD58',
                        6132
              );
  _cs.Add     (   6133, 'Estonia_1992',
                        7019, 1333
              );
  _cs.AddAlias(     -1, 'EST92',
                        6133
              );
  _cs.Add     (   6134, 'PDO_Survey_Datum_1993',
                        7012, 1439
              );
  _cs.AddAlias(     -1, 'PSD93',
                        6134
              );
  _cs.Add     (   6135, 'Old_Hawaiian',
                        7008, 15840, 15828
              );
  _cs.Add     (   6136, 'St_Lawrence_Island',
                        7008, 1699
              );
  _cs.Add     (   6137, 'St_Paul_Island',
                        7008, 1700
              );
  _cs.Add     (   6138, 'St_George_Island',
                        7008, 1698
              );
  _cs.Add     (   6139, 'Puerto_Rico',
                        7008, 15841, 1893
              );
  _cs.Add     (   6140, 'NAD83_Canadian_Spatial_Reference_System',
                        7019, 1946
              );
  _cs.AddAlias(     -1, 'NAD83_CSRS',
                        6140
              );
  _cs.Add     (   6141, 'Israel_1993',
                        7019, 9676
              );
  _cs.Add     (   6142, 'Locodjo_1965',
                        7012, 1469
              );
  _cs.Add     (   6143, 'Abidjan_1987',
                        7012, 1470
              );
  _cs.Add     (   6144, 'Kalianpur_1937',
                        7015, 1533
              );
  _cs.Add     (   6145, 'Kalianpur_1962',
                        7044, 15701
              );
  _cs.Add     (   6146, 'Kalianpur_1975',
                        7045, 1156
              );
  _cs.Add     (   6147, 'Hanoi_1972',
                        7024, 1544
              );
  _cs.Add     (   6148, 'Hartebeesthoek94',
                        7030, 1505
              );
  _cs.Add     (   6149, 'CH1903',
                        7004, 7673, 1766
              );
  _cs.Add     (   6150, 'CH1903+',
                        7004, 1676
              );
  _cs.Add     (   6151, 'Swiss_Terrestrial_Reference_System_1995',
                        7019, 1511
              );
  _cs.AddAlias(     -1, 'CHTRS95',
                        6151
              );
  _cs.AddAlias(     -1, 'Swiss_Terrestrial_Reference_Frame_1995_CHTRF95',
                        6151
              );
  _cs.Add     (   6152, 'NAD83_High_Accuracy_Reference_Network',
                        7019, 1901
              );
  _cs.AddAlias(     -1, 'Guam_Geodetic_Network_1993',
                        6152
              );
  _cs.AddAlias(     -1, 'NAD83_High_Accuracy_Regional_Network',
                        6152
              );
  _cs.AddAlias(     -1, 'NAD83_High_Precision_Geodetic_Network',
                        6152
              );
  _cs.AddAlias(     -1, 'NAD83_HARN',
                        6152
              );
  _cs.AddAlias(     -1, 'NAD83_HPGN',
                        6152
              );
  _cs.Add     (   6153, 'Rassadiran',
                        7022, 1512
              );
  _cs.Add     (   6154, 'European_Datum_1950_1977',
                        7022, 15745
              );
  _cs.AddAlias(     -1, 'ED50_ED77',
                        6154
              );
  _cs.Add     (   6155, 'Dabola_1981',
                        7011, 1518
              );
  _cs.Add     (   6156, 'System_of_the_Unified_Trigonometrical_Cadastral_Network',
                        7004, 1623
              );
  _cs.AddAlias(     -1, 'S_JTSK',
                        6156
              );
  _cs.Add     (   6157, 'Mount_Dillon',
                        7007, 0
              );
  _cs.Add     (   6158, 'Naparima_1955',
                        7022, 1555
              );
  _cs.Add     (   6159, 'European_Libyan_Datum_1979',
                        7022, 1860
              );
  _cs.AddAlias(     -1, 'ELD79',
                        6159
              );
  _cs.Add     (   6160, 'Chos_Malal_1914',
                        7022, 9257
              );
  _cs.AddAlias(     -1, 'Quini_Huao',
                        6160
              );
  _cs.Add     (   6161, 'Pampa_del_Castillo',
                        7022, 9259
              );
  _cs.Add     (   6162, 'Korean_Datum_1985',
                        7004, 5191
              );
  _cs.Add     (   6163, 'Yemen_National_Geodetic_Network_1996',
                        7030, 1540
              );
  _cs.AddAlias(     -1, 'YNGN96',
                        6163
              );
  _cs.Add     (   6164, 'South_Yemen',
                        7024, 1682
              );
  _cs.Add     (   6165, 'Bissau',
                        7022, 1547
              );
  _cs.Add     (   6166, 'Korean_Datum_1995',
                        7030, 1558
              );
  _cs.Add     (   6167, 'New_Zealand_Geodetic_Datum_2000',
                        7019, 1565
              );
  _cs.AddAlias(     -1, 'NZGD2000',
                        6167
              );
  _cs.Add     (   6168, 'Accra',
                        7029, 6896
              );
  _cs.Add     (   6169, 'American_Samoa_1962',
                        7008, 1578, 1577
              );
  _cs.Add     (   6170, 'Sistema_de_Referencia_Geocentrico_para_America_del_Sur_1995',
                        7019, 1581
              );
  _cs.AddAlias(     -1, 'SIRGAS_1995',
                        6170
              );
  _cs.Add     (   6171, 'Reseau_Geodesique_Francais_1993_v1',
                        7019, 1671
              );
  _cs.AddAlias(     -1, 'Reseau_Geodesique_Francais_1993',
                        6171
              );
  _cs.AddAlias(     -1, 'RGF93_v1',
                        6171
              );
  _cs.Add     (   6172, 'Posiciones_Geodesicas_Argentinas',
                        7019, 1598
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'POSGAR_A',
                        6172
              );
  _cs.Add     (   6173, 'IRENET95',
                        7019, 1678
              );
  _cs.Add     (   6174, 'Sierra_Leone_Colony_1924',
                        7029, 0
              );
  _cs.AddAlias(     -1, 'Sierra_Leone_Peninsular_1924',
                        6174
              );
  _cs.Add     (   6175, 'Sierra_Leone_1968',
                        7012, 1614
              );
  _cs.Add     (   6176, 'Australian_Antarctic_Datum_1998',
                        7019, 1890
              );
  _cs.AddAlias(     -1, 'AAD98',
                        6176
              );
  _cs.Add     (   6178, 'Pulkovo_1942_83',
                        7024, 1675
              );
  _cs.AddAlias(     -1, '42_83',
                        6178
              );
  _cs.Add     (   6179, 'Pulkovo_1942_58',
                        7024, 1645
              );
  _cs.AddAlias(     -1, '42_58',
                        6179
              );
  _cs.Add     (   6180, 'Estonia_1997',
                        7019, 1649
              );
  _cs.AddAlias(     -1, 'EST97',
                        6180
              );
  _cs.Add     (   6181, 'Luxembourg_Reference_Frame',
                        7022, 5484
              );
  _cs.AddAlias(     -1, 'LUREF',
                        6181
              );
  _cs.AddAlias(     -1, 'Luxembourg_1930',
                        6181
              );
  _cs.AddAlias(     -1, 'NTL',
                        6181
              );
  _cs.AddAlias(     -1, 'Nouvelle_Triangulation_du_Luxembourg',
                        6181
              );
  _cs.Add     (   6182, 'Azores_Occidental_Islands_1939',
                        7022, 1982
              );
  _cs.AddAlias(     -1, 'Azores_Occidental_1939',
                        6182
              );
  _cs.AddAlias(     -1, 'Observatario_Flores',
                        6182
              );
  _cs.AddAlias(     -1, 'Observatorio_1966',
                        6182
              );
  _cs.AddAlias(     -1, 'Observatorio_Meteorologico_1939',
                        6182
              );
  _cs.Add     (   6183, 'Azores_Central_Islands_1948',
                        7022, 1886
              );
  _cs.AddAlias(     -1, 'Azores_Central_1948',
                        6183
              );
  _cs.Add     (   6184, 'Azores_Oriental_Islands_1940',
                        7022, 1885
              );
  _cs.AddAlias(     -1, 'Azores_Oriental_1940',
                        6184
              );
  _cs.Add     (   6185, 'Madeira_1936',
                        7022, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6188, 'OSNI_1952',
                        7001, 1955
              );
end;

procedure prepare_6;
begin
  _cs.Add     (   6189, 'Red_Geodesica_Venezolana',
                        7019, 1768
              );
  _cs.AddAlias(     -1, 'REGVEN',
                        6189
              );
  _cs.AddAlias(     -1, 'SIRGAS_REGVEN',
                        6189
              );
  _cs.Add     (   6190, 'Posiciones_Geodesicas_Argentinas_1998',
                        7019, 1773
              );
  _cs.AddAlias(     -1, 'POSGAR_B',
                        6190
              );
  _cs.AddAlias(     -1, 'POSGAR_98',
                        6190
              );
  _cs.Add     (   6191, 'Albanian_1987',
                        7024, 6964
              );
  _cs.AddAlias(     -1, 'ALB86',
                        6191
              );
  _cs.Add     (   6192, 'Douala_1948',
                        7022, 15873
              );
  _cs.Add     (   6193, 'Manoca_1962',
                        7011, 1796
              );
  _cs.Add     (   6194, 'Qornoq_1927',
                        7022, 1798
              );
  _cs.Add     (   6195, 'Scoresbysund_1952',
                        7022, 1799
              );
  _cs.Add     (   6196, 'Ammassalik_1958',
                        7022, 1800
              );
  _cs.Add     (   6197, 'Garoua',
                        7012, 0
              );
  _cs.Add     (   6198, 'Kousseri',
                        7012, 0
              );
  _cs.Add     (   6199, 'Egypt_1930',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'New_Egyptian',
                        6199
              );
  _cs.Add     (   6200, 'Pulkovo_1995',
                        7024, 5043
              );
  _cs.Add     (   6201, 'Adindan',
                        7012, 1103
              );
  _cs.AddAlias(     -1, 'Blue_Nile_1958',
                        6201
              );
  _cs.Add     (   6202, 'Australian_Geodetic_Datum_1966',
                        7050, 1803, 9185
              );
  _cs.AddAlias(     -1, 'AGD66',
                        6202
              );
  _cs.Add     (   6203, 'Australian_Geodetic_Datum_1984',
                        7050, 1804, 1280
              );
  _cs.AddAlias(     -1, 'AGD84',
                        6203
              );
  _cs.Add     (   6204, 'Ain_el_Abd_1970',
                        7022, 1055
              );
  _cs.AddAlias(     -1, 'Ain_el_Abd',
                        6204
              );
  _cs.Add     (   6205, 'Afgooye',
                        7024, 1107
              );
  _cs.Add     (   6206, 'Agadez',
                        7011, 0
              );
  _cs.Add     (   6207, 'Lisbon_1937',
                        7022, 6188, 1988
              );
  _cs.Add     (   6208, 'Aratu',
                        7022, 5051
              );
  _cs.Add     (   6209, 'Arc_1950',
                        7013, 1114
              );
  _cs.Add     (   6210, 'Arc_1960',
                        7012, 1284
              );
  _cs.Add     (   6211, 'Batavia',
                        7004, 1813
              );
  _cs.AddAlias(     -1, 'Genuk',
                        6211
              );
  _cs.Add     (   6212, 'Barbados_1938',
                        7012, 15793
              );
  _cs.Add     (   6213, 'Beduaram',
                        7011, 15849
              );
  _cs.Add     (   6214, 'Beijing_1954',
                        7024, 15918
              );
  _cs.Add     (   6215, 'Reseau_National_Belge_1950',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'BD50',
                        6215
              );
  _cs.AddAlias(     -1, 'Belge_1950',
                        6215
              );
  _cs.Add     (   6216, 'Bermuda_1957',
                        7008, 15970
              );
  _cs.Add     (   6218, 'Bogota_1975',
                        7022, 1597
              );
  _cs.AddAlias(     -1, 'Bogota',
                        6218
              );
  _cs.Add     (   6219, 'Bukit_Rimpah',
                        7004, 1126
              );
  _cs.Add     (   6220, 'Camacupa_1948',
                        7012, 1327
              );
  _cs.AddAlias(     -1, 'Camacupa',
                        6220
              );
  _cs.Add     (   6221, 'Campo_Inchauspe',
                        7022, 1527
              );
  _cs.Add     (   6222, 'Cape',
                        7013, 1128
              );
  _cs.AddAlias(     -1, 'South_Africa',
                        6222
              );
  _cs.Add     (   6223, 'Carthage',
                        7011, 1538
              );
  _cs.Add     (   6224, 'Chua',
                        7022, 4834
              );
  _cs.Add     (   6225, 'Corrego_Alegre_1970_72',
                        7022, 5526, 6192
              );
  _cs.AddAlias(     -1, 'Corrego_Alegre',
                        6225
              );
  _cs.Add     (   6226, 'Cote_d_Ivoire',
                        7011, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6227, 'Deir_ez_Zor',
                        7011, 15743
              );
  _cs.AddAlias(     -1, 'Levant',
                        6227
              );
  _cs.Add     (   6228, 'Douala',
                        7011, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6229, 'Egypt_1907',
                        7020, 7697
              );
  _cs.AddAlias(     -1, 'Old_Egyptian',
                        6229
              );
(* ilker silme  _cs.Add     (   6230, 'European_Datum_1950',
                        7022, 5661, 1311
              ); *)
  _cs.Add     (   6230, 'European_Datum_1950',
                        7022, 1133
              ); // ilker ekleme			  
  _cs.AddAlias(     -1, 'ED50',
                        6230
              );
  _cs.Add     (   6231, 'European_Datum_1987',
                        7022, 1146
              );
  _cs.AddAlias(     -1, 'ED87',
                        6231
              );
  _cs.Add     (   6232, 'Fahud',
                        7012, 6908
              );
  _cs.Add     (   6233, 'Gandajika_1970',
                        7022, 1894
              )         .MarkDeprecated( True );
  _cs.Add     (   6234, 'Garoua_A',
                        7011, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6235, 'Guyane_Francaise',
                        7022, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6236, 'Hu_Tzu_Shan_1950',
                        7022, 1152
              );
  _cs.AddAlias(     -1, 'Hu_Tzu_Shan',
                        6236
              );
  _cs.Add     (   6237, 'Hungarian_Datum_1972',
                        7036, 1242
              );
  _cs.AddAlias(     -1, 'HD72',
                        6237
              );
  _cs.Add     (   6238, 'Indonesian_Datum_1974',
                        7021, 1833
              );
  _cs.AddAlias(     -1, 'ID74',
                        6238
              );
  _cs.Add     (   6239, 'Indian_1954',
                        7015, 1153
              );
  _cs.Add     (   6240, 'Indian_1975',
                        7015, 1537
              );
  _cs.Add     (   6241, 'Jamaica_1875',
                        7034, 0
              );
  _cs.Add     (   6242, 'Jamaica_1969',
                        7008, 15927
              );
  _cs.AddAlias(     -1, 'JAD69',
                        6242
              );
  _cs.Add     (   6243, 'Kalianpur_1880',
                        7042, 0
              );
  _cs.Add     (   6244, 'Kandawala',
                        7015, 1157
              );
  _cs.Add     (   6245, 'Kertau_1968',
                        7018, 1158
              );
  _cs.AddAlias(     -1, 'Malaysia_Revised_Triangulation_1968',
                        6245
              );
  _cs.AddAlias(     -1, 'MRT68',
                        6245
              );
  _cs.Add     (   6246, 'Kuwait_Oil_Company',
                        7012, 1059
              );
  _cs.AddAlias(     -1, 'KOC',
                        6246
              );
  _cs.Add     (   6247, 'La_Canoa',
                        7022, 1516
              );
  _cs.Add     (   6248, 'Provisional_South_American_Datum_1956',
                        7022, 1583
              );
  _cs.AddAlias(     -1, 'PSAD56',
                        6248
              );
  _cs.Add     (   6249, 'Lake',
                        7022, 0
              );
  _cs.Add     (   6250, 'Leigon',
                        7012, 1159
              );
  _cs.Add     (   6251, 'Liberia_1964',
                        7012, 1160
              );
  _cs.Add     (   6252, 'Lome',
                        7011, 0
              );
  _cs.Add     (   6253, 'Luzon_1911',
                        7008, 1161
              );
  _cs.Add     (   6254, 'Hito_XVIII_1963',
                        7022, 1529
              );
  _cs.Add     (   6255, 'Herat_North',
                        7022, 1246
              );
  _cs.Add     (   6256, 'Mahe_1971',
                        7012, 1164
              );
  _cs.Add     (   6257, 'Makassar',
                        7004, 1837
              );
  _cs.Add     (   6258, 'European_Terrestrial_Reference_System_1989_ensemble',
                        7019, 1149
              );
  _cs.AddAlias(     -1, 'ETRS89_A',
                        6258
              );
  _cs.AddAlias(     -1, 'European_Terrestrial_Reference_System_1989',
                        6258
              );
end;

procedure prepare_7;
begin
  _cs.Add     (   6259, 'Malongo_1987',
                        7022, 1557
              );
  _cs.Add     (   6260, 'Manoca',
                        7012, 1316
              )         .MarkDeprecated( True );
  _cs.Add     (   6261, 'Merchich',
                        7011, 1166
              );
  _cs.Add     (   6262, 'Massawa',
                        7004, 1165
              );
  _cs.Add     (   6263, 'Minna',
                        7012, 1754
              );
  _cs.Add     (   6264, 'Mhast_A',
                        7022, 1329
              )         .MarkDeprecated( True );
  _cs.Add     (   6265, 'Monte_Mario',
                        7022, 1660
              );
  _cs.AddAlias(     -1, 'Rome_1940',
                        6265
              );
  _cs.Add     (   6266, 'M_poraloko',
                        7011, 1532
              );
  _cs.Add     (   6267, 'North_American_Datum_1927',
                        7008, 1692, 7698
              );
  _cs.AddAlias(     -1, 'NAD27',
                        6267
              );
  _cs.Add     (   6268, 'NAD27_Michigan',
                        7009, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6269, 'North_American_Datum_1983',
                        7019, 1474, 1188
              );
  _cs.AddAlias(     -1, 'NAD83_A',
                        6269
              );
  _cs.AddAlias(     -1, 'NAD83_1986',
                        6269
              );
  _cs.AddAlias(     -1, 'NAD83_Original',
                        6269
              );
  _cs.Add     (   6270, 'Nahrwan_1967',
                        7012, 6999
              );
  _cs.Add     (   6271, 'Naparima_1972',
                        7022, 1307
              );
  _cs.Add     (   6272, 'New_Zealand_Geodetic_Datum_1949',
                        7022, 1568, 1564
              );
  _cs.AddAlias(     -1, 'GD49',
                        6272
              );
  _cs.AddAlias(     -1, 'NZGD49',
                        6272
              );
  _cs.Add     (   6273, 'NGO_1948',
                        7005, 1654
              );
  _cs.Add     (   6274, 'Datum_73',
                        7022, 6189, 1987
              );
  _cs.Add     (   6275, 'Nouvelle_Triangulation_Francaise',
                        7011, 1193
              );
  _cs.AddAlias(     -1, 'NTF',
                        6275
              );
  _cs.Add     (   6276, 'NSWC_9Z_2',
                        7025, 0
              );
  _cs.Add     (   6277, 'Ordnance_Survey_of_Great_Britain_1936',
                        7001, 7709, 1314
              );
  _cs.AddAlias(     -1, 'OSGB_1936',
                        6277
              );
  _cs.AddAlias(     -1, 'OSGB36',
                        6277
              );
  _cs.Add     (   6278, 'OSGB_1970_SN',
                        7001, 0
              );
  _cs.AddAlias(     -1, 'OSGB70',
                        6278
              );
  _cs.Add     (   6279, 'OS_SN_1980',
                        7001, 0
              );
  _cs.AddAlias(     -1, 'OS_SN_80',
                        6279
              );
  _cs.Add     (   6280, 'Padang_1884',
                        7004, 0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Padang',
                        6280
              );
  _cs.Add     (   6281, 'Palestine_1923',
                        7010, 1074
              );
  _cs.AddAlias(     -1, 'OID',
                        6281
              );
  _cs.AddAlias(     -1, 'Old_Israeli_Datum',
                        6281
              );
  _cs.Add     (   6282, 'Congo_1960_Pointe_Noire',
                        7011, 1802
              );
  _cs.AddAlias(     -1, 'Pointe_Noire',
                        6282
              );
  _cs.Add     (   6283, 'Geocentric_Datum_of_Australia_1994',
                        7019, 8444, 8048
              );
  _cs.AddAlias(     -1, 'GDA94',
                        6283
              );
  _cs.Add     (   6284, 'Pulkovo_1942',
                        7024, 1303
              );
  _cs.Add     (   6285, 'Qatar_1974',
                        7022, 1562
              );
  _cs.Add     (   6286, 'Qatar_1948',
                        7020, 0
              );
  _cs.Add     (   6287, 'Qornoq',
                        7022, 1211
              )         .MarkDeprecated( True );
  _cs.Add     (   6288, 'Loma_Quintana',
                        7022, 0
              );
  _cs.Add     (   6289, 'Amersfoort',
                        7004, 9282, 9281
              );
  _cs.Add     (   6291, 'South_American_Datum_1969',
                        7036, 1548
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'SAD69',
                        6291
              );
  _cs.Add     (   6292, 'Sapper_Hill_1943',
                        7022, 1225
              );
  _cs.Add     (   6293, 'Schwarzeck',
                        7046, 1226
              );
  _cs.Add     (   6294, 'Segora',
                        7004, 1836
              )         .MarkDeprecated( True );
  _cs.Add     (   6295, 'Serindung',
                        7004, 0
              );
  _cs.Add     (   6296, 'Sudan',
                        7011, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6297, 'Tananarive_1925',
                        7022, 6873
              );
  _cs.AddAlias(     -1, 'Tananarive',
                        6297
              );
  _cs.Add     (   6298, 'Timbalai_1948',
                        7016, 5249
              );
  _cs.AddAlias(     -1, 'Borneo_Triangulation_of_1968',
                        6298
              );
  _cs.AddAlias(     -1, 'BT68',
                        6298
              );
  _cs.AddAlias(     -1, 'Timbalai_1968',
                        6298
              );
  _cs.Add     (   6299, 'TM65',
                        7002, 1641
              );
  _cs.Add     (   6300, 'Geodetic_Datum_of_1965',
                        7002, 6946, 1953
              );
  _cs.AddAlias(     -1, '1975_Mapping_Adjustment',
                        6300
              );
  _cs.AddAlias(     -1, 'TM75',
                        6300
              );
  _cs.Add     (   6301, 'Tokyo',
                        7004, 6712, 1305
              );
  _cs.AddAlias(     -1, 'Tokyo_1918',
                        6301
              );
  _cs.Add     (   6302, 'Trinidad_1903',
                        7007, 1296
              );
  _cs.Add     (   6303, 'Trucial_Coast_1948',
                        7020, 0
              );
  _cs.AddAlias(     -1, 'TC_1948',
                        6303
              );
  _cs.Add     (   6304, 'Voirol_1875',
                        7011, 1294
              );
  _cs.Add     (   6306, 'Bern_1938',
                        7004, 0
              );
  _cs.Add     (   6307, 'Nord_Sahara_1959',
                        7012, 5660
              );
  _cs.Add     (   6308, 'Stockholm_1938',
                        7004, 0
              );
  _cs.AddAlias(     -1, 'Rikets_koordinatsystem_1938',
                        6308
              );
  _cs.AddAlias(     -1, 'RT38',
                        6308
              );
  _cs.Add     (   6309, 'Yacare',
                        7022, 5386
              );
  _cs.AddAlias(     -1, 'ROU_USAMS',
                        6309
              );
  _cs.Add     (   6310, 'Yoff',
                        7011, 6903
              );
  _cs.Add     (   6311, 'Zanderij',
                        7022, 1235
              );
  _cs.Add     (   6312, 'Militar_Geographische_Institut',
                        7004, 5891, 1194
              );
  _cs.AddAlias(     -1, 'MGI',
                        6312
              );
  _cs.Add     (   6313, 'Reseau_National_Belge_1972',
                        7022, 8369, 15928
              );
  _cs.AddAlias(     -1, 'BD72',
                        6313
              );
  _cs.AddAlias(     -1, 'Belge_1972',
                        6313
              );
  _cs.Add     (   6314, 'Deutsches_Hauptdreiecksnetz',
                        7004, 9310, 1778
              );
  _cs.AddAlias(     -1, 'DHDN',
                        6314
              );
  _cs.Add     (   6315, 'Conakry_1905',
                        7011, 1517
              );
  _cs.Add     (   6316, 'Dealul_Piscului_1930',
                        7022, 1995
              );
  _cs.AddAlias(     -1, 'Dealul_Piscului_1933',
                        6316
              );
  _cs.Add     (   6317, 'Dealul_Piscului_1970',
                        7024, 1996
              )         .MarkDeprecated( True );
  _cs.Add     (   6318, 'National_Geodetic_Network',
                        7030, 1060
              );
  _cs.AddAlias(     -1, 'NGN',
                        6318
              );
  _cs.Add     (   6319, 'Kuwait_Utility',
                        7019, 1061
              );
  _cs.AddAlias(     -1, 'KUDAMS',
                        6319
              );
  _cs.Add     (   6322, 'World_Geodetic_System_1972',
                        7043, 1237
              );
  _cs.AddAlias(     -1, 'WGS_72',
                        6322
              );
  _cs.Add     (   6324, 'WGS_72_Transit_Broadcast_Ephemeris',
                        7043, 1240
              );
  _cs.AddAlias(     -1, 'WGS_72BE',
                        6324
              );
  _cs.Add     (   6326, 'World_Geodetic_System_1984_ensemble',
                        7030, 0
              );
end;

procedure prepare_8;
begin
  _cs.AddAlias(     -1, 'WGS_84',
                        6326
              );
  _cs.AddAlias(     -1, 'World_Geodetic_System_1984',
                        6326
              );
  _cs.Add     (   6600, 'Anguilla_1957',
                        7012, 0
              );
  _cs.Add     (   6601, 'Antigua_1943',
                        7012, 1441
              );
  _cs.Add     (   6602, 'Dominica_1945',
                        7012, 1442
              );
  _cs.Add     (   6603, 'Grenada_1953',
                        7012, 1443
              );
  _cs.Add     (   6604, 'Montserrat_1958',
                        7012, 1444
              );
  _cs.Add     (   6605, 'St_Kitts_1955',
                        7012, 1445
              );
  _cs.Add     (   6606, 'St_Lucia_1955',
                        7012, 6897
              );
  _cs.Add     (   6607, 'St_Vincent_1945',
                        7012, 1959
              );
  _cs.Add     (   6608, 'North_American_Datum_1927_1976',
                        7008, 1690
              );
  _cs.AddAlias(     -1, 'NAD27_76',
                        6608
              );
  _cs.Add     (   6609, 'North_American_Datum_1927_CGQ77',
                        7008, 1691
              );
  _cs.AddAlias(     -1, 'CGQ77',
                        6609
              );
  _cs.Add     (   6610, 'Xian_1980',
                        7049, 0
              );
  _cs.Add     (   6611, 'Hong_Kong_1980',
                        7022, 1825
              );
  _cs.AddAlias(     -1, 'HK80',
                        6611
              );
  _cs.Add     (   6612, 'Japanese_Geodetic_Datum_2000',
                        7019, 6713, 1826
              );
  _cs.AddAlias(     -1, 'JGD2000',
                        6612
              );
  _cs.Add     (   6613, 'Gunung_Segara',
                        7004, 1838
              );
  _cs.AddAlias(     -1, 'P2_Exc',
                        6613
              );
  _cs.AddAlias(     -1, 'P2_Exc_T9',
                        6613
              );
  _cs.AddAlias(     -1, 'Samboja_A',
                        6613
              );
  _cs.AddAlias(     -1, 'Segara',
                        6613
              );
  _cs.Add     (   6614, 'Qatar_National_Datum_1995',
                        7022, 1840
              );
  _cs.AddAlias(     -1, 'QND95',
                        6614
              );
  _cs.Add     (   6615, 'Porto_Santo_1936',
                        7022, 1888
              );
  _cs.AddAlias(     -1, 'Madeira_SE_Base',
                        6615
              );
  _cs.AddAlias(     -1, 'Porto_Santo',
                        6615
              );
  _cs.Add     (   6616, 'Selvagem_Grande',
                        7022, 1965
              );
  _cs.AddAlias(     -1, 'Marco_Astro',
                        6616
              );
  _cs.AddAlias(     -1, 'Selvagem_Grande_1938',
                        6616
              );
  _cs.Add     (   6618, 'South_American_Datum_1969_A',
                        7050, 5528, 6974
              );
  _cs.Add     (   6619, 'SWEREF99',
                        7019, 1879
              );
  _cs.Add     (   6620, 'Point_58',
                        7012, 1880
              );
  _cs.AddAlias(     -1, '12th_Parallel_traverse',
                        6620
              );
  _cs.Add     (   6621, 'Fort_Marigot',
                        7022, 1903
              );
  _cs.Add     (   6622, 'Guadeloupe_1948',
                        7022, 1905
              );
  _cs.AddAlias(     -1, 'Sainte_Anne',
                        6622
              );
  _cs.Add     (   6623, 'Centre_Spatial_Guyanais_1967',
                        7022, 1906
              );
  _cs.AddAlias(     -1, 'CSG67',
                        6623
              );
  _cs.AddAlias(     -1, 'Guyane_Francaise_A',
                        6623
              );
  _cs.Add     (   6624, 'Reseau_Geodesique_Francais_Guyane_1995',
                        7019, 4840
              );
  _cs.AddAlias(     -1, 'RGFG95',
                        6624
              );
  _cs.Add     (   6625, 'Martinique_1938',
                        7022, 1910
              );
  _cs.AddAlias(     -1, 'Fort_Desaix',
                        6625
              );
  _cs.Add     (   6626, 'Reunion_1947',
                        7022, 15751
              );
  _cs.AddAlias(     -1, 'Piton_des_Neiges',
                        6626
              );
  _cs.Add     (   6627, 'Reseau_Geodesique_de_la_Reunion_1992',
                        7019, 1912
              );
  _cs.AddAlias(     -1, 'RGR92',
                        6627
              );
  _cs.Add     (   6628, 'Tahiti_52',
                        7022, 1924
              );
  _cs.AddAlias(     -1, 'IGN_1952',
                        6628
              );
  _cs.AddAlias(     -1, 'Tahiti',
                        6628
              );
  _cs.Add     (   6629, 'Tahaa_54',
                        7022, 8834
              );
  _cs.AddAlias(     -1, 'Tahaa',
                        6629
              );
  _cs.Add     (   6630, 'IGN72_Nuku_Hiva',
                        7022, 8848
              );
  _cs.Add     (   6631, 'K0_1949',
                        7022, 1915
              )         .MarkDeprecated( True );
  _cs.Add     (   6632, 'Combani_1950',
                        7022, 1916
              );
  _cs.Add     (   6633, 'IGN56_Lifou',
                        7022, 15902
              );
  _cs.Add     (   6634, 'IGN72_Grande_Terre',
                        7022, 15947, 15888
              );
  _cs.AddAlias(     -1, 'MHNC72',
                        6634
              );
  _cs.Add     (   6635, 'ST87_Ouvea',
                        7022, 1919
              )         .MarkDeprecated( True );
  _cs.Add     (   6636, 'Petrels_1972',
                        7022, 1921
              );
  _cs.Add     (   6637, 'Pointe_Geologie_Perroud_1950',
                        7022, 1922
              );
  _cs.AddAlias(     -1, 'Perroud_1950',
                        6637
              );
  _cs.Add     (   6638, 'Saint_Pierre_et_Miquelon_1950',
                        7008, 8270
              );
  _cs.AddAlias(     -1, 'St_Pierre_et_Miquelon_1950',
                        6638
              );
  _cs.Add     (   6639, 'MOP78',
                        7022, 15847
              );
  _cs.AddAlias(     -1, 'Uvea_SHOM_1978',
                        6639
              );
  _cs.Add     (   6640, 'Reseau_de_Reference_des_Antilles_Francaises_1991_A',
                        7030, 1934
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'RRAF91_A',
                        6640
              );
  _cs.Add     (   6641, 'IGN53_Mare',
                        7022, 15901
              );
  _cs.Add     (   6642, 'ST84_Ile_des_Pins',
                        7022, 1930
              );
  _cs.Add     (   6643, 'ST71_Belep',
                        7022, 1931
              );
  _cs.Add     (   6644, 'NEA74_Noumea',
                        7022, 1932
              );
  _cs.Add     (   6645, 'Reseau_Geodesique_Nouvelle_Caledonie_1991',
                        7022, 1920
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'RGNC91',
                        6645
              );
  _cs.Add     (   6646, 'Grand_Comoros',
                        7022, 5521
              );
  _cs.Add     (   6647, 'International_Terrestrial_Reference_Frame_1988',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1988',
                        6647
              );
  _cs.AddAlias(     -1, 'ITRF88',
                        6647
              );
  _cs.Add     (   6648, 'International_Terrestrial_Reference_Frame_1989',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1989',
                        6648
              );
  _cs.AddAlias(     -1, 'ITRF89',
                        6648
              );
  _cs.Add     (   6649, 'International_Terrestrial_Reference_Frame_1990',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1990',
                        6649
              );
  _cs.AddAlias(     -1, 'ITRF90',
                        6649
              );
  _cs.Add     (   6650, 'International_Terrestrial_Reference_Frame_1991',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1991',
                        6650
              );
  _cs.AddAlias(     -1, 'ITRF91',
                        6650
              );
  _cs.Add     (   6651, 'International_Terrestrial_Reference_Frame_1992',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1992',
                        6651
              );
  _cs.AddAlias(     -1, 'ITRF92',
                        6651
              );
  _cs.Add     (   6652, 'International_Terrestrial_Reference_Frame_1993',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1993',
                        6652
              );
  _cs.AddAlias(     -1, 'ITRF93',
                        6652
              );
  _cs.Add     (   6653, 'International_Terrestrial_Reference_Frame_1994',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'IERS_Terrestrial_Reference_Frame_1994',
                        6653
              );
  _cs.AddAlias(     -1, 'ITRF94',
                        6653
              );
  _cs.Add     (   6654, 'International_Terrestrial_Reference_Frame_1996',
                        7019, 0
              );
end;

procedure prepare_9;
begin
  _cs.AddAlias(     -1, 'ITRF96',
                        6654
              );
  _cs.Add     (   6655, 'International_Terrestrial_Reference_Frame_1997',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ITRF97',
                        6655
              );
  _cs.Add     (   6656, 'International_Terrestrial_Reference_Frame_2000',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ITRF2000',
                        6656
              );
  _cs.Add     (   6657, 'Reykjavik_1900',
                        7051, 1994
              );
  _cs.Add     (   6658, 'Hjorsey_1955',
                        7022, 6909
              );
  _cs.Add     (   6659, 'Islands_Net_1993',
                        7019, 1952
              );
  _cs.AddAlias(     -1, 'ISN93',
                        6659
              );
  _cs.Add     (   6660, 'Helle_1954',
                        7022, 1957
              );
  _cs.Add     (   6661, 'Latvia_1992',
                        7019, 1958
              );
  _cs.AddAlias(     -1, 'LKS92',
                        6661
              );
  _cs.Add     (   6663, 'Porto_Santo_1995',
                        7022, 1967
              );
  _cs.AddAlias(     -1, 'Base_SE_A',
                        6663
              );
  _cs.AddAlias(     -1, 'Madeira_SE_Base_1995',
                        6663
              );
  _cs.Add     (   6664, 'Azores_Oriental_Islands_1995',
                        7022, 1969
              );
  _cs.AddAlias(     -1, 'Azores_Oriental_1995',
                        6664
              );
  _cs.AddAlias(     -1, 'Sao_Bras_A',
                        6664
              );
  _cs.Add     (   6665, 'Azores_Central_Islands_1995',
                        7022, 1973
              );
  _cs.AddAlias(     -1, 'Base_SW_A',
                        6665
              );
  _cs.AddAlias(     -1, 'Azores_Central_1995',
                        6665
              );
  _cs.AddAlias(     -1, 'Graciosa_A',
                        6665
              );
  _cs.Add     (   6666, 'Lisbon_1890',
                        7004, 1990
              );
  _cs.Add     (   6667, 'Iraq_Kuwait_Boundary_Datum_1992',
                        7030, 1993
              );
  _cs.AddAlias(     -1, 'IKBD_92',
                        6667
              );
  _cs.Add     (   6668, 'European_Datum_1979',
                        7022, 15752
              );
  _cs.AddAlias(     -1, 'ED79',
                        6668
              );
  _cs.Add     (   6670, 'Istituto_Geografico_Militaire_1995',
                        7019, 1099
              );
  _cs.AddAlias(     -1, 'IGM95',
                        6670
              );
  _cs.Add     (   6671, 'Voirol_1879',
                        7011, 0
              );
  _cs.Add     (   6672, 'Chatham_Islands_Datum_1971',
                        7022, 1080
              );
  _cs.Add     (   6673, 'Chatham_Islands_Datum_1979',
                        7022, 1081
              );
  _cs.AddAlias(     -1, 'CI1979',
                        6673
              );
  _cs.Add     (   6674, 'Sistema_de_Referencia_Geocentrico_para_las_AmericaS_2000',
                        7019, 15894
              );
  _cs.AddAlias(     -1, 'SIRGAS_2000',
                        6674
              );
  _cs.AddAlias(     -1, 'SIRGAS2000',
                        6674
              );
  _cs.AddAlias(     -1, 'Sistema_de_Referencia_Geocentrico_para_America_del_Sur_2000',
                        6674
              );
  _cs.Add     (   6675, 'Guam_1963',
                        7008, 1068, 1070
              );
  _cs.Add     (   6676, 'Vientiane_1982',
                        7024, 0
              );
  _cs.Add     (   6677, 'Lao_1993',
                        7024, 0
              );
  _cs.Add     (   6678, 'Lao_National_Datum_1997',
                        7024, 1065
              );
  _cs.AddAlias(     -1, 'Lao_1997',
                        6678
              );
  _cs.Add     (   6679, 'Jouik_1961',
                        7012, 15870
              );
  _cs.Add     (   6680, 'Nouakchott_1965',
                        7012, 15709
              );
  _cs.Add     (   6681, 'Mauritania_1999',
                        7012, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6682, 'Gulshan_303',
                        7015, 9679
              );
  _cs.Add     (   6683, 'Philippine_Reference_System_1992',
                        7008, 15708
              );
  _cs.AddAlias(     -1, 'Modified_Luzon_Datum',
                        6683
              );
  _cs.AddAlias(     -1, 'PRS92',
                        6683
              );
  _cs.Add     (   6684, 'Gan_1970',
                        7022, 15713
              );
  _cs.Add     (   6685, 'Gandajika',
                        7022, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6686, 'Marco_Geocentrico_Nacional_de_Referencia',
                        7019, 15738
              );
  _cs.AddAlias(     -1, 'MAGNA_SIRGAS',
                        6686
              );
  _cs.Add     (   6687, 'Reseau_Geodesique_de_la_Polynesie_Francaise',
                        7019, 8828
              );
  _cs.AddAlias(     -1, 'RGPF',
                        6687
              );
  _cs.Add     (   6688, 'Fatu_Iva_72',
                        7022, 8842
              );
  _cs.AddAlias(     -1, 'MHEFO_55',
                        6688
              );
  _cs.Add     (   6689, 'IGN63_Hiva_Oa',
                        7022, 8844
              );
  _cs.Add     (   6690, 'Tahiti_79',
                        7022, 8830
              );
  _cs.AddAlias(     -1, 'IGN79_Tahiti',
                        6690
              );
  _cs.Add     (   6691, 'Moorea_87',
                        7022, 8832
              );
  _cs.Add     (   6692, 'Maupiti_83',
                        7022, 8853
              );
  _cs.AddAlias(     -1, 'MOP_1983',
                        6692
              );
  _cs.Add     (   6693, 'Nakhl_e_Ghanem',
                        7030, 15746
              );
  _cs.Add     (   6694, 'Posiciones_Geodesicas_Argentinas_1994',
                        7030, 1210
              );
  _cs.AddAlias(     -1, 'POSGAR_C',
                        6694
              );
  _cs.AddAlias(     -1, 'POSGAR_94',
                        6694
              );
  _cs.Add     (   6695, 'Katanga_1955',
                        7008, 4068
              );
  _cs.Add     (   6696, 'Kasai_1953',
                        7012, 0
              );
  _cs.Add     (   6697, 'IGC_1962_Arc_of_the_6th_Parallel_South',
                        7012, 0
              );
  _cs.AddAlias(     -1, 'IGC_1962_6th_Parallel_South',
                        6697
              );
  _cs.Add     (   6698, 'IGN_1962_Kerguelen',
                        7022, 15850
              );
  _cs.Add     (   6699, 'Le_Pouce_1934',
                        7012, 15784
              );
  _cs.AddAlias(     -1, 'Le_Pouce_Mauritius_94',
                        6699
              );
  _cs.AddAlias(     -1, 'Le_Pouce_Mauritius_PN_94',
                        6699
              );
  _cs.Add     (   6700, 'IGN_Astro_1960',
                        7012, 0
              );
  _cs.Add     (   6701, 'Institut_Geographique_du_Congo_Belge_1955',
                        7012, 15787
              );
  _cs.AddAlias(     -1, 'Bas_Congo_1955',
                        6701
              );
  _cs.AddAlias(     -1, 'IGCB_1955',
                        6701
              );
  _cs.Add     (   6702, 'Mauritania_1999_A',
                        7019, 15860
              );
  _cs.Add     (   6703, 'Missao_Hidrografico_Angola_y_Sao_Tome_1951',
                        7012, 0
              );
  _cs.AddAlias(     -1, 'Mhast_1951',
                        6703
              );
  _cs.Add     (   6704, 'Mhast_onshore',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'Mhast_B',
                        6704
              );
  _cs.Add     (   6705, 'Mhast_offshore',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'Mhast_C',
                        6705
              );
  _cs.Add     (   6706, 'Egypt_Gulf_of_Suez_S_650_TL',
                        7020, 15846
              );
  _cs.AddAlias(     -1, 'S_650_TL',
                        6706
              );
  _cs.Add     (   6707, 'Tern_Island_1961',
                        7022, 15795
              );
  _cs.Add     (   6708, 'Cocos_Islands_1965',
                        7050, 15794
              );
  _cs.Add     (   6709, 'Iwo_Jima_1945',
                        7022, 15796
              );
  _cs.Add     (   6710, 'Astro_DOS_71',
                        7022, 7894
              );
  _cs.AddAlias(     -1, 'ASTRO_DOS_71_4',
                        6710
              );
  _cs.AddAlias(     -1, 'St_Helena_1971',
                        6710
              );
  _cs.Add     (   6711, 'Marcus_Island_1952',
                        7022, 15799
              );
  _cs.Add     (   6712, 'Ascension_Island_1958',
                        7022, 15797
              );
  _cs.Add     (   6713, 'Ayabelle_Lighthouse',
                        7012, 6907
              );
  _cs.Add     (   6714, 'Bellevue',
                        7022, 15801
              );
  _cs.AddAlias(     -1, 'Bellevue_IGN',
                        6714
              );
  _cs.Add     (   6715, 'Camp_Area_Astro',
                        7022, 15802
              );
end;

procedure prepare_10;
begin
  _cs.Add     (   6716, 'Phoenix_Islands_1966',
                        7022, 15803
              );
  _cs.Add     (   6717, 'Cape_Canaveral',
                        7008, 15804
              );
  _cs.Add     (   6718, 'Solomon_1968',
                        7022, 15805
              );
  _cs.Add     (   6719, 'Easter_Island_1967',
                        7022, 15806
              );
  _cs.Add     (   6720, 'Fiji_Geodetic_Datum_1986',
                        7043, 15876
              );
  _cs.AddAlias(     -1, 'FGD_1986',
                        6720
              );
  _cs.AddAlias(     -1, 'Fiji_1986',
                        6720
              );
  _cs.Add     (   6721, 'Fiji_1956',
                        7022, 15875
              );
  _cs.Add     (   6722, 'South_Georgia_1968',
                        7022, 15813
              );
  _cs.Add     (   6723, 'Grand_Cayman_Geodetic_Datum_1959',
                        7008, 6142
              );
  _cs.AddAlias(     -1, 'GCGD59',
                        6723
              );
  _cs.AddAlias(     -1, 'Grand_Cayman_1959',
                        6723
              );
  _cs.Add     (   6724, 'Diego_Garcia_1969',
                        7022, 15808
              );
  _cs.Add     (   6725, 'Johnston_Island_1961',
                        7022, 15809
              );
  _cs.AddAlias(     -1, 'Johnston_Atoll_1961',
                        6725
              );
  _cs.Add     (   6726, 'Sister_Islands_Geodetic_Datum_1961',
                        7008, 6143
              );
  _cs.AddAlias(     -1, 'Little_Cayman_Geodetic_Datum_1961',
                        6726
              );
  _cs.AddAlias(     -1, 'SIGD61',
                        6726
              );
  _cs.Add     (   6727, 'Midway_1961',
                        7022, 15818
              );
  _cs.Add     (   6728, 'Pico_de_las_Nieves_1984',
                        7022, 15815
              );
  _cs.AddAlias(     -1, 'PN84',
                        6728
              );
  _cs.Add     (   6729, 'Pitcairn_1967',
                        7022, 15819
              );
  _cs.Add     (   6730, 'Santo_1965',
                        7022, 15820
              );
  _cs.AddAlias(     -1, 'Santo_DOS',
                        6730
              );
  _cs.Add     (   6731, 'Viti_Levu_1916',
                        7012, 15821
              )         .MarkDeprecated( True );
  _cs.Add     (   6732, 'Marshall_Islands_1960',
                        7053, 15822
              );
  _cs.Add     (   6733, 'Wake_Island_1952',
                        7022, 15823
              );
  _cs.Add     (   6734, 'Tristan_1968',
                        7022, 15816
              );
  _cs.Add     (   6735, 'Kusaie_1951',
                        7022, 15810
              );
  _cs.Add     (   6736, 'Deception_Island',
                        7012, 15812
              );
  _cs.Add     (   6737, 'Geocentric_datum_of_Korea',
                        7019, 15831
              );
  _cs.AddAlias(     -1, 'Korea_2000',
                        6737
              );
  _cs.Add     (   6738, 'Hong_Kong_1963',
                        7007, 0
              );
  _cs.AddAlias(     -1, 'HK63',
                        6738
              );
  _cs.Add     (   6739, 'Hong_Kong_1963_67',
                        7022, 15842
              );
  _cs.AddAlias(     -1, 'HK63_67',
                        6739
              );
  _cs.Add     (   6740, 'Parametry_Zemli_1990',
                        7054, 1244
              );
  _cs.AddAlias(     -1, 'PZ_90',
                        6740
              );
  _cs.Add     (   6741, 'Faroe_Datum_1954',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'FD54',
                        6741
              );
  _cs.Add     (   6742, 'Geodetic_Datum_of_Malaysia_2000',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'GDM2000',
                        6742
              );
  _cs.Add     (   6743, 'Karbala_1979',
                        7012, 5078
              );
  _cs.AddAlias(     -1, 'Karbala_1979_Polservice',
                        6743
              );
  _cs.Add     (   6744, 'Nahrwan_1934',
                        7012, 7033
              );
  _cs.Add     (   6745, 'Rauenberg_Datum_83',
                        7004, 6948, 15868
              );
  _cs.AddAlias(     -1, 'RD_83',
                        6745
              );
  _cs.Add     (   6746, 'Potsdam_Datum_83',
                        7004, 15955, 15867
              );
  _cs.AddAlias(     -1, 'PD_83',
                        6746
              );
  _cs.Add     (   6747, 'Greenland_1996',
                        7019, 15879
              );
  _cs.AddAlias(     -1, 'GR96',
                        6747
              );
  _cs.Add     (   6748, 'Vanua_Levu_1915',
                        7055, 15878
              );
  _cs.AddAlias(     -1, 'Vanua_Levu_1917',
                        6748
              );
  _cs.Add     (   6749, 'Reseau_Geodesique_de_Nouvelle_Caledonie_91_93',
                        7019, 15880
              );
  _cs.AddAlias(     -1, 'RGNC',
                        6749
              );
  _cs.AddAlias(     -1, 'RGNC91_93',
                        6749
              );
  _cs.Add     (   6750, 'ST87_Ouvea_A',
                        7030, 15881
              );
  _cs.Add     (   6751, 'Kertau_RSO',
                        7056, 0
              );
  _cs.Add     (   6752, 'Viti_Levu_1912',
                        7055, 6895
              );
  _cs.AddAlias(     -1, 'Viti_Levu_1916_A',
                        6752
              );
  _cs.Add     (   6753, 'fk89',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'FD54a',
                        6753
              );
  _cs.Add     (   6754, 'Libyan_Geodetic_Datum_2006',
                        7022, 15908
              );
  _cs.AddAlias(     -1, 'LGD2006',
                        6754
              );
  _cs.Add     (   6755, 'Datum_Geodesi_Nasional_1995',
                        7030, 15912
              );
  _cs.AddAlias(     -1, 'DGN95',
                        6755
              );
  _cs.AddAlias(     -1, 'Indonesian_Geodetic_Datum_1995',
                        6755
              );
  _cs.AddAlias(     -1, 'IGD95',
                        6755
              );
  _cs.Add     (   6756, 'Vietnam_2000',
                        7030, 5194
              );
  _cs.AddAlias(     -1, 'VN_2000',
                        6756
              );
  _cs.Add     (   6757, 'SVY21',
                        7030, 8886
              );
  _cs.Add     (   6758, 'Jamaica_2001',
                        7030, 15925
              );
  _cs.AddAlias(     -1, 'JAD2001',
                        6758
              );
  _cs.Add     (   6759, 'NAD83_National_Spatial_Reference_System_2007',
                        7019, 15931
              );
  _cs.Add     (   6760, 'World_Geodetic_System_1966',
                        7025, 0
              );
  _cs.AddAlias(     -1, 'WGS_66',
                        6760
              );
  _cs.Add     (   6761, 'Croatian_Terrestrial_Reference_System',
                        7019, 15967
              );
  _cs.AddAlias(     -1, 'HTRS96',
                        6761
              );
  _cs.Add     (   6762, 'Bermuda_2000',
                        7030, 15971
              );
  _cs.AddAlias(     -1, 'BDA2000',
                        6762
              );
  _cs.Add     (   6763, 'Pitcairn_2006',
                        7030, 15972
              );
  _cs.Add     (   6764, 'Ross_Sea_Region_Geodetic_Datum_2000',
                        7019, 15974
              );
  _cs.AddAlias(     -1, 'RSRGD2000',
                        6764
              );
  _cs.Add     (   6765, 'Slovenia_Geodetic_Datum_1996',
                        7019, 15976
              );
  _cs.AddAlias(     -1, 'D96',
                        6765
              );
  _cs.AddAlias(     -1, 'Slovenia_1996',
                        6765
              );
  _cs.Add     (   6801, 'CH1903_Bern',
                        7004, 7673, 1766
              );
  _cs.Add     (   6802, 'Bogota_1975_Bogota',
                        7022, 1597
              );
  _cs.Add     (   6803, 'Lisbon_1937_Lisbon',
                        7022, 6188, 1988
              );
  _cs.Add     (   6804, 'Makassar_Jakarta',
                        7004, 1837
              );
  _cs.Add     (   6805, 'Militar_Geographische_Institut_Ferro',
                        7004, 5891, 1194
              );
  _cs.AddAlias(     -1, 'MGI_Ferro',
                        6805
              );
  _cs.Add     (   6806, 'Monte_Mario_Rome',
                        7022, 1660
              );
  _cs.AddAlias(     -1, 'Rome_1940_Rome',
                        6806
              );
  _cs.Add     (   6807, 'Nouvelle_Triangulation_Francaise_Paris',
                        7011, 1193
              );
  _cs.Add     (   6808, 'Padang_1884_Jakarta',
                        7004, 0
              )         .MarkDeprecated( True );
  _cs.Add     (   6809, 'Reseau_National_Belge_1950_Brussels',
                        7022, 0
              );
  _cs.AddAlias(     -1, 'BD50_Brussels',
                        6809
              );
  _cs.Add     (   6810, 'Tananarive_1925_Paris',
                        7022, 6873
              );
  _cs.Add     (   6811, 'Voirol_1875_Paris',
                        7011, 1294
              );
end;

procedure prepare_11;
begin
  _cs.Add     (   6813, 'Batavia_Jakarta',
                        7004, 1813
              );
  _cs.Add     (   6814, 'Stockholm_1938_Stockholm',
                        7004, 0
              );
  _cs.Add     (   6815, 'Greek_Athens',
                        7004, 0
              );
  _cs.Add     (   6816, 'Carthage_Paris',
                        7011, 1538
              );
  _cs.Add     (   6817, 'NGO_1948_Oslo',
                        7005, 1654
              );
  _cs.Add     (   6818, 'System_of_the_Unified_Trigonometrical_Cadastral_Network_Ferro',
                        7004, 1623
              );
  _cs.AddAlias(     -1, 'S_JTSK_Ferro',
                        6818
              );
  _cs.Add     (   6819, 'Nord_Sahara_1959_Paris',
                        7012, 5660
              )         .MarkDeprecated( True );
  _cs.Add     (   6820, 'Gunung_Segara_Jakarta',
                        7004, 1838
              );
  _cs.AddAlias(     -1, 'Segara_Jakarta',
                        6820
              );
  _cs.Add     (   6821, 'Voirol_1879_Paris',
                        7011, 0
              );
  _cs.Add     (   6896, 'International_Terrestrial_Reference_Frame_2005',
                        7019, 0
              );
  _cs.AddAlias(     -1, 'ITRF2005',
                        6896
              );
  _cs.Add     (   6901, 'Ancienne_Triangulation_Francaise_Paris',
                        7027, 0
              );
  _cs.AddAlias(     -1, 'ATF_Paris',
                        6901
              );
  _cs.Add     (   6902, 'Nord_de_Guerre_Paris',
                        7027, 0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'NDG_Paris',
                        6902
              );
  _cs.Add     (   6903, 'Madrid_1870_Madrid',
                        7028, 0
              );
  _cs.AddAlias(     -1, 'Madrid',
                        6903
              );
  _cs.Add     (   6904, 'Lisbon_1890_Lisbon',
                        7004, 1990
              );
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
  prepare_10;
  prepare_11;
end;

//==================================== END ====================================

