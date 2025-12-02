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
  List of Coordinate System Datums.
  Generated automatically.
}


procedure Init_CSDatumListEx(
  const _cs : TGIS_CSDatumList
) ;

procedure prepare_1;
begin
  _cs.Add     ( 906002, 'Fischer_1960',
                        905002, 0
              );
  _cs.Add     ( 906003, 'Fischer_1968',
                        905003, 0
              );
  _cs.Add     ( 906004, 'Fischer_Modified',
                        905004, 0
              );
  _cs.Add     ( 906005, 'Hough_1960',
                        7053, 0
              );
  _cs.Add     ( 906006, 'Everest_Modified_1969',
                        7056, 0
              );
  _cs.Add     ( 906007, 'Walbeck',
                        905005, 0
              );
  _cs.Add     ( 906036, 'GRS_1967_Truncated',
                        7036, 0
              );
  _cs.Add     ( 906101, 'Estonia_1937',
                        7004, 901112
              );
  _cs.Add     ( 906102, 'Hermannskogel',
                        7004, 901121
              );
  _cs.Add     ( 906103, 'Sierra_Leone_1960',
                        7012, 0
              );
  _cs.Add     ( 906202, 'Everest_Bangladesh',
                        7015, 901125
              );
  _cs.Add     ( 906203, 'Everest_India_Nepal',
                        7044, 901126
              );
  _cs.Add     ( 906206, 'Oman',
                        7012, 901114
              );
  _cs.Add     ( 906207, 'South_Asia_Singapore',
                        905004, 901150
              );
  _cs.Add     ( 906212, 'Beacon_E_1945',
                        7022, 901103
              );
  _cs.Add     ( 906214, 'Astro_1952',
                        7022, 901102
              );
  _cs.Add     ( 906216, 'Canton_1966',
                        7022, 901105
              );
  _cs.Add     ( 906218, 'DOS_1968',
                        7022, 901118
              );
  _cs.Add     ( 906221, 'GUX_1',
                        7022, 901109
              );
  _cs.Add     ( 906229, 'Wake_Eniwetok_1960',
                        7053, 901111
              );
  _cs.Add     ( 906231, 'Anna_1_1965',
                        7003, 901100
              );
  _cs.Add     ( 906233, 'ISTS_073_1969',
                        7022, 901128
              );
  _cs.Add     ( 906234, 'Kerguelen_Island_1949',
                        7022, 901130
              );
  _cs.Add     ( 906235, 'Reunion',
                        7022, 901145
              );
  _cs.Add     ( 906238, 'DOS_71_4',
                        7022, 901149
              );
  _cs.Add     ( 906240, 'Fort_Thomas_1955',
                        7012, 901116
              );
  _cs.Add     ( 906241, 'Graciosa_Base_SW_1948',
                        7022, 901119
              );
  _cs.Add     ( 906242, 'ISTS_061_1968',
                        7022, 901127
              );
  _cs.Add     ( 906243, 'LC5_1961',
                        7008, 901132
              );
  _cs.Add     ( 906245, 'Observ_Meteorologico_1939',
                        7022, 901115
              );
  _cs.Add     ( 906249, 'Sao_Braz',
                        7022, 901147
              );
  _cs.Add     ( 906257, 'S42_Hungary',
                        7024, 901160
              );
  _cs.Add     ( 906260, 'Alaskan_Islands',
                        7008, 0
              );
  _cs.Add     ( 906261, 'Datum_Lisboa_Bessel',
                        7004, 0
              );
  _cs.Add     ( 906262, 'LISBONBESSEL',
                        7004, 0
              ) ;
  _cs.Add     ( 906263, 'Datum_Lisboa_Hayford',
                        7022, 901165
              );
end;

procedure fix_1;
begin
  _cs.Fix     (   1144, 'IG05_12_Intermediate_Datum',
                        7019, 901168
              ); // use better definition
  _cs.Fix     (   6152, 'NAD83_High_Accuracy_Reference_Network',
                        7019, 901169, 1580
              ); // better coordinate transformation
  _cs.Fix     (   6055, 'Popular_Visualisation_Datum',
                        7059, 901163
              ); // turn off ellipsoid->spheroid calculations
  _cs.Fix     (   6197, 'Garoua_B',
                        7012, 0
              ); // fix Garoua RGN vs IGN
  _cs.Fix     (   6267, 'North_American_Datum_1927',
                        7008, 901167
              ); // better coordinate transformation
  _cs.Fix     (   6277, 'OSGB_1936',
                        7001, 7709, 1314
              ); // better coordinate transformation
  _cs.Fix     (   6234, 'Garoua',
                        7011, 0
              ); // fix Garoua RGN vs IGN
  _cs.Fix     (   6317, 'Dealul_Piscului_1970',
                        7024, 15994
              ); // better coordinate transformation
end;

procedure alias_1;
begin
  _cs.AddAlias(     -1, 'Airy_1830',
                        6001
              );
  _cs.AddAlias(     -1, 'Airy_Modified',
                        6002
              );
  _cs.AddAlias(     -1, 'Australian',
                        6003
              );
  _cs.AddAlias(     -1, 'Bessel_1841',
                        6004
              );
  _cs.AddAlias(     -1, 'Bessel_Modified',
                        6005
              );
  _cs.AddAlias(     -1, 'Bessel_Namibia',
                        6006
              );
  _cs.AddAlias(     -1, 'Clarke_1858',
                        6007
              );
  _cs.AddAlias(     -1, 'Clarke_1866',
                        6008
              );
  _cs.AddAlias(     -1, 'CLRK1866',
                        6008
              );
  _cs.AddAlias(     -1, 'Clarke_1866_Michigan',
                        6009
              );
  _cs.AddAlias(     -1, 'Clarke_1880_Benoit',
                        6010
              );
  _cs.AddAlias(     -1, 'Clarke_1880_IGN',
                        6011
              );
  _cs.AddAlias(     -1, 'Clarke_1880_RGS',
                        6012
              );
  _cs.AddAlias(     -1, 'Clarke_1880_Arc',
                        6013
              );
  _cs.AddAlias(     -1, 'Clarke_1880_SGA',
                        6014
              );
  _cs.AddAlias(     -1, 'Everest_Adj_1937',
                        6015
              );
  _cs.AddAlias(     -1, 'Everest_Def_1967',
                        6016
              );
  _cs.AddAlias(     -1, 'Everest_Modified',
                        6018
              );
  _cs.AddAlias(     -1, 'GRS_1980',
                        6019
              );
  _cs.AddAlias(     -1, 'Helmert_1906',
                        6020
              );
  _cs.AddAlias(     -1, 'Indonesian',
                        6021
              );
  _cs.AddAlias(     -1, 'International_1924',
                        6022
              );
  _cs.AddAlias(     -1, 'Krasovsky_1940',
                        6024
              );
  _cs.AddAlias(     -1, 'NWL_9D',
                        6025
            );
  _cs.AddAlias(     -1, 'Plessis_1817',
                        6027
              );
  _cs.AddAlias(     -1, 'Struve_1860',
                        6028
              );
  _cs.AddAlias(     -1, 'War_Office',
                        6029
              );
  _cs.AddAlias(     -1, 'GEM_10C',
                        6031
              );
  _cs.AddAlias(     -1, 'OSU_86F',
                        6032
              );
  _cs.AddAlias(     -1, 'OSU_91A',
                        6033
              );
  _cs.AddAlias(     -1, 'Clarke_1880',
                        6034
              );
  _cs.AddAlias(     -1, 'Sphere',
                        6035
              );
  _cs.AddAlias(     -1, 'GRS_1967',
                        6036
              );
  _cs.AddAlias(     -1, 'Everest_1830',
                        6042
              );
  _cs.AddAlias(     -1, 'Everest_Def_1962',
                        6044
              );
  _cs.AddAlias(     -1, 'Everest_Def_1975',
                        6045
              );
  _cs.AddAlias(     -1, 'GRS_1980_Sphere',
                        6047
              );
  _cs.AddAlias(     -1, 'Clarke_1866_Authalic_Sphere',
                        6052
             );
  _cs.AddAlias(     -1, 'Sphere_ARC_INFO',
                        6052
             );
  _cs.AddAlias(     -1, 'GGRS_1987',
                        6121
             );
  _cs.AddAlias(     -1, 'ATS_1977',
                        6122
              );
  _cs.AddAlias(     -1, 'Kartastokoordinaattijarjestelma',
                        6123
              );
  _cs.AddAlias(     -1, 'RT_1990',
                        6124
              );
  _cs.AddAlias(     -1, 'Lithuania_1994',
                        6126
              );
  _cs.AddAlias(     -1, 'FD_1958',
                        6132
              );
  _cs.AddAlias(     -1, 'PDO_1993',
                        6134
              );
  _cs.AddAlias(     -1, 'North_American_1983_CSRS98',
                        6140
              );
  _cs.AddAlias(     -1, 'Kalianpur',
                        6146
              );
  _cs.AddAlias(     -1, 'Hartebeesthoek_1994',
                        6148
              );
  _cs.AddAlias(     -1, 'Swiss_TRF_1995',
                        6151
              );
  _cs.AddAlias(     -1, 'North_American_1983_HARN',
                        6152
              );
  _cs.AddAlias(     -1, 'European_1950_ED77',
                        6154
              );
  _cs.AddAlias(     -1, 'Dabola',
                        6155
              );
  _cs.AddAlias(     -1, 'European_Libyan_1979',
                        6159
              );
  _cs.AddAlias(     -1, 'Yemen_NGN_1996',
                        6163
              );
  _cs.AddAlias(     -1, 'NZGD_2000',
                        6167
              );
  _cs.AddAlias(     -1, 'Samoa_1962',
                        6169
              );
  _cs.AddAlias(     -1, 'SIRGAS',
                        6170
              );
  _cs.AddAlias(     -1, 'Sistema_de_Referencia_Geocentrico_para_America_del_Sur',
                        6170
              );
  _cs.AddAlias(     -1, 'RGF_1993',
                        6171
              );
  _cs.AddAlias(     -1, 'Sierra_Leone_1924',
                        6174
              );
  _cs.AddAlias(     -1, 'POSGAR_1998',
                        6190
              );
  _cs.AddAlias(     -1, 'Garoua_RGS',
                        6197
              );
  _cs.AddAlias(     -1, 'Australian_1966',
                        6202
              );
  _cs.AddAlias(     -1, 'Australian_1984',
                        6203
              );
  _cs.AddAlias(     -1, 'Lisbon',
                        6207
              );
  _cs.AddAlias(     -1, 'ARC1950',
                        6209
              );
  _cs.AddAlias(     -1, 'ARC1960',
                        6210
              );
  _cs.AddAlias(     -1, 'Barbados',
                        6212
              );
  _cs.AddAlias(     -1, 'BEIJING',
                        6214
              );
  _cs.AddAlias(     -1, 'BELG50',
                        6215
              );
  _cs.AddAlias(     -1, 'EGYPT07',
                        6229
              );
  _cs.AddAlias(     -1, 'European_1950',
                        6230
              );
  _cs.AddAlias(     -1, 'European_1987',
                        6231
              );
  _cs.AddAlias(     -1, 'Hungarian_1972',
                        6237
              );
  _cs.AddAlias(     -1, 'Indonesian_1974',
                        6238
              );
  _cs.AddAlias(     -1, 'JA1875',
                        6241
              );
  _cs.AddAlias(     -1, 'Provisional_S_American_1956',
                        6248
              );
  _cs.AddAlias(     -1, 'ETRF_1989',
                        6258
              );
  _cs.AddAlias(     -1, 'European_Reference_System_1989',
                        6258
              );
  _cs.AddAlias(     -1, 'Mporaloko',
                        6266
              );
  _cs.AddAlias(     -1, 'North_American_1927',
                        6267
              );
  _cs.AddAlias(     -1, 'North_American_Michigan',
                        6268
              );
  _cs.AddAlias(     -1, 'North_American_1983',
                        6269
              );
  _cs.AddAlias(     -1, 'New_Zealand_1949',
                        6272
              );
  _cs.AddAlias(     -1, 'PALEST23',
                        6281
              );
  _cs.AddAlias(     -1, 'PTNOIRE',
                        6282
              );
  _cs.AddAlias(     -1, 'GDA_1994',
                        6283
              );
  _cs.AddAlias(     -1, 'Qatar',
                        6285
              );
  _cs.AddAlias(     -1, 'Tokyo_Datum',
                        6301
              );
  _cs.AddAlias(     -1, 'WGS_1972',
                        6322
             );
  _cs.AddAlias(     -1, 'WGS_1972_BE',
                        6324
              );
  _cs.AddAlias(     -1, 'WGS72BE',
                        6324
              );
  _cs.AddAlias(     -1, 'WGS_1972_Transit_Broadcast_Ephemeris',
                        6324
              );
  _cs.AddAlias(     -1, 'WGS_1984',
                        6326
              );
  _cs.AddAlias(     -1, 'NAD_1927_Definition_1976',
                        6608
              );
  _cs.AddAlias(     -1, 'NAD_1927_CGQ77',
                        6609
              );
  _cs.AddAlias(     -1, 'Qatar_1995',
                        6614
              );
  _cs.AddAlias(     -1, 'CI1971',
                        6672
              );
  _cs.AddAlias(     -1, 'Bern_1898_Bern',
                        6801
              );
  _cs.AddAlias(     -1, 'Bogota_Bogota',
                        6802
              );
  _cs.AddAlias(     -1, 'Lisbon_Lisbon',
                        6803
              );
  _cs.AddAlias(     -1, 'NTF_Paris',
                        6807
              );
  _cs.AddAlias(     -1, 'Belge_1950_Brussels',
                        6809
              );
  _cs.AddAlias(     -1, 'Voirol_Unifie_1960_Paris',
                        6307
              );
  _cs.AddAlias(     -1, 'Voirol_Unifie_1960',
                        6307
              );
  _cs.AddAlias(     -1, 'RT38_Stockholm',
                        6814
              );
  _cs.AddAlias(     -1, 'ATF',
                        6901
              );
  _cs.AddAlias(     -1, 'Ancienne_Triangulation_Francaise',
                        6901
              );
  _cs.AddAlias(     -1, 'Nord_de_Guerre',
                        6902
              );
  _cs.AddAlias(     -1, 'Madrid_1870',
                        6903
              );
  _cs.AddAlias(     -1, 'WGS_1966',
                        6760
              );
  _cs.AddAlias(     -1, 'ETRS_1989',
                        6258
              );
  _cs.AddAlias(     -1, 'POTSDAM',
                        6746
              );
  _cs.AddAlias(     -1, 'CMPOINCH',
                        6221
              );
  _cs.AddAlias(     -1, 'BAHRAIN',
                        6204
              );
  _cs.AddAlias(     -1, 'CORRALEG',
                        6225
              );
  _cs.AddAlias(     -1, 'DEIR',
                        6227
              );
  _cs.AddAlias(     -1, 'EGSA87',
                        6121
              );
  _cs.AddAlias(     -1, 'FINKKJ',
                        6123
              );
  _cs.AddAlias(     -1, 'HGRS87',
                        6121
              );
  _cs.AddAlias(     -1, 'IND74',
                        6238
              );
  _cs.AddAlias(     -1, 'INDIAN54',
                        6239
              );
  _cs.AddAlias(     -1, 'INDIAN60',
                        6131
              );
  _cs.AddAlias(     -1, 'INDIAN75',
                        6240
              );
  _cs.AddAlias(     -1, 'KALIANPR',
                        6146
              );
  _cs.AddAlias(     -1, 'LACANOA',
                        6247
              );
  _cs.AddAlias(     -1, 'LUZON11',
                        6253
              );
  _cs.AddAlias(     -1, 'MAHE71',
                        6256
              );
  _cs.AddAlias(     -1, 'MANOKA',
                        6260
              );
  _cs.AddAlias(     -1, 'MINAA',
                        6263
              );
  _cs.AddAlias(     -1, 'MONTEMAR',
                        6265
              );
  _cs.AddAlias(     -1, 'MONTROME',
                        6806
              );
  _cs.AddAlias(     -1, 'MPORO',
                        6266
              );
  _cs.AddAlias(     -1, 'NAD27A76',
                        6608
              );
  _cs.AddAlias(     -1, 'NAHRWAN',
                        6270
              );
  _cs.AddAlias(     -1, 'OSSN80',
                        6279
              );
  _cs.AddAlias(     -1, 'PULKOVO',
                        6284
              );
  _cs.AddAlias(     -1, 'RD',
                        6745
              );
  _cs.AddAlias(     -1, 'SLOVENIA',
                        6765
              );
  _cs.AddAlias(     -1, 'TANANAR',
                        6297
              );
  _cs.AddAlias(     -1, 'TANANPAR',
                        6810
              );
  _cs.AddAlias(     -1, 'TIMBALAI',
                        6298
              );
  _cs.AddAlias(     -1, 'TRUCIAL',
                        6303
              );
  _cs.AddAlias(     -1, 'XIAN80',
                        6610
              );
  _cs.AddAlias(     -1, 'YOFF2000',
                        6310
              );
  _cs.AddAlias(     -1, 'ITRF_1988',
                        6647
              );
  _cs.AddAlias(     -1, 'ITRF_1989',
                        6648
              );
  _cs.AddAlias(     -1, 'ITRF_1990',
                        6649
              );
  _cs.AddAlias(     -1, 'ITRF_1991',
                        6650
              );
  _cs.AddAlias(     -1, 'ITRF_1992',
                        6651
              );
  _cs.AddAlias(     -1, 'ITRF_1993',
                        6652
              );
  _cs.AddAlias(     -1, 'ITRF_1994',
                        6653
              );
  _cs.AddAlias(     -1, 'ITRF_1996',
                        6654
              );
  _cs.AddAlias(     -1, 'Jednotne_Trigonometricke_Site_Katastralni',
                        6156
              );
  _cs.AddAlias(     -1, 'Ayabelle',
                        6713
              );
  _cs.AddAlias(     -1, 'Chatham_Island_1971',
                        6672
              );
  _cs.AddAlias(     -1, 'Chatham_Island_1979',
                        6673
              );
  _cs.AddAlias(     -1, 'European_1979',
                        6668
              );
  _cs.AddAlias(     -1, 'Camp_Area',
                        6715
              );
  _cs.AddAlias(     -1, 'Santo_DOS_1965',
                        6730
              );
end;

procedure alias_2;
begin
  _cs.AddAlias(     -1, 'International_1967',
                        1032
              );
  _cs.AddAlias(     -1, 'Sphere_Clarke_1866_Authalic',
                        6052
              );
  _cs.AddAlias(     -1, 'Australian_Antarctic_1998',
                        6176
              );
  _cs.AddAlias(     -1, 'Pulkovo_1942_Adj_1983',
                        6178
              );
  _cs.AddAlias(     -1, 'Pulkovo_1942_Adj_1958',
                        6179
              );
  _cs.AddAlias(     -1, 'Kertau',
                        6245
              );
  _cs.AddAlias(     -1, 'South_American_1969',
                        6618
              );
  _cs.AddAlias(     -1, 'JGD_2000',
                        6612
              );
  _cs.AddAlias(     -1, 'QND_1995',
                        6614
              );
  _cs.AddAlias(     -1, 'CSG_1967',
                        6623
              );
  _cs.AddAlias(     -1, 'RGFG_1995',
                        6624
              );
  _cs.AddAlias(     -1, 'RGR_1992',
                        6627
              );
  _cs.AddAlias(     -1, 'Tahiti_1952',
                        6628
              );
  _cs.AddAlias(     -1, 'Tahaa_1954',
                        6629
              );
  _cs.AddAlias(     -1, 'RRAF_1991',
                        6640
              );
  _cs.AddAlias(     -1, 'RGNC_1991',
                        6645
              );
  _cs.AddAlias(     -1, 'Islands_Network_1993',
                        6659
              );
  _cs.AddAlias(     -1, 'Iraq_Kuwait_Boundary_1992',
                        6667
              );
  _cs.AddAlias(     -1, 'IGM_1995',
                        6670
              );
  _cs.AddAlias(     -1, 'Chatham_Islands_1979',
                        6673
              );
  _cs.AddAlias(     -1, 'Phillipine_Reference_System_1992',
                        6683
              );
  _cs.AddAlias(     -1, 'MAGNA',
                        6686
              );
  _cs.AddAlias(     -1, 'Fatu_Iva_1972',
                        6688
              );
  _cs.AddAlias(     -1, 'Tahiti_1979',
                        6690
              );
  _cs.AddAlias(     -1, 'Moorea_1987',
                        6691
              );
  _cs.AddAlias(     -1, 'Maupiti_1983',
                        6692
              );
  _cs.AddAlias(     -1, 'POSGAR_1994',
                        6694
              );
  _cs.AddAlias(     -1, 'Kasai_1955',
                        6696
              );
  _cs.AddAlias(     -1, 'Little_Cayman_1961',
                        6726
              );
  _cs.AddAlias(     -1, 'GDM_2000',
                        6742
              );
  _cs.AddAlias(     -1, 'Bern_1898',
                        6801
              );
  _cs.AddAlias(     -1, 'Nakhl-e_Ghanem',
                        6693
              );
  _cs.AddAlias(     -1, 'Egypt_Gulf_of_Suez_S-650_TL',
                        6706
              );
end;

begin
  prepare_1;
  fix_1;
  alias_1;
  alias_2;
end;
{==================================== END =====================================}

