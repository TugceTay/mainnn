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
  List of Units.
  Generated from EPSG 9.1 dataset.
}


procedure Init_CSUnitsList(
  const _cs : TGIS_CSUnitsList
) ;

procedure prepare_1;
begin
  _cs.Add     (   1025, 'millimetre',
                        'mm',
                        TGIS_CSUnitsType.Linear,
                        1.0 / 1000.0
              );
  _cs.AddAlias(     -1, 'millimeter',
                        1025
              );
  _cs.AddAlias(     -1, 'mm',
                        1025
              );
  _cs.Add     (   1031, 'milliarc_second',
                        'msec',
                        TGIS_CSUnitsType.Angular,
                        Pi / 648000000.0
              );
  _cs.AddAlias(     -1, '0_001_seca',
                        1031
              );
  _cs.AddAlias(     -1, 'angular_millisecond',
                        1031
              );
  _cs.AddAlias(     -1, 'mas',
                        1031
              );
  _cs.AddAlias(     -1, 'msec',
                        1031
              );
  _cs.Add     (   1033, 'centimetre',
                        'cm',
                        TGIS_CSUnitsType.Linear,
                        1.0 / 100.0
              );
  _cs.AddAlias(     -1, 'centimeter',
                        1033
              );
  _cs.AddAlias(     -1, 'cm',
                        1033
              );
  _cs.Add     (   9001, 'metre',
                        'm',
                        TGIS_CSUnitsType.Linear,
                        1.0 / 1.0
              );
  _cs.AddAlias(     -1, 'International_metre',
                        9001
              );
  _cs.AddAlias(     -1, 'm',
                        9001
              );
  _cs.AddAlias(     -1, 'meter',
                        9001
              );
  _cs.Add     (   9002, 'foot',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.3048 / 1.0
              );
  _cs.AddAlias(     -1, 'ft',
                        9002
              );
  _cs.AddAlias(     -1, 'international_foot',
                        9002
              );
  _cs.Add     (   9003, 'US_survey_foot',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        12.0 / 39.37
              );
  _cs.AddAlias(     -1, 'American_foot',
                        9003
              );
  _cs.AddAlias(     -1, 'ft_US',
                        9003
              );
  _cs.AddAlias(     -1, 'ftUS',
                        9003
              );
  _cs.Add     (   9005, 'Clarke_s_foot',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.3047972654 / 1.0
              );
  _cs.AddAlias(     -1, 'Clarke_foot',
                        9005
              );
  _cs.AddAlias(     -1, 'ft_Cla',
                        9005
              );
  _cs.AddAlias(     -1, 'ftCla',
                        9005
              );
  _cs.AddAlias(     -1, 'South_African_geodetic_foot',
                        9005
              );
  _cs.Add     (   9014, 'fathom',
                        'ftm',
                        TGIS_CSUnitsType.Linear,
                        1.8288 / 1.0
              );
  _cs.AddAlias(     -1, 'f',
                        9014
              );
  _cs.AddAlias(     -1, 'fathom_A',
                        9014
              );
  _cs.AddAlias(     -1, 'international_fathom',
                        9014
              );
  _cs.Add     (   9030, 'nautical_mile',
                        'NM',
                        TGIS_CSUnitsType.Linear,
                        1852.0 / 1.0
              );
  _cs.AddAlias(     -1, 'International_nautical_mile',
                        9030
              );
  _cs.AddAlias(     -1, 'mi_naut',
                        9030
              );
  _cs.AddAlias(     -1, 'NM',
                        9030
              );
  _cs.Add     (   9031, 'German_legal_metre',
                        'glm',
                        TGIS_CSUnitsType.Linear,
                        1.0000135965 / 1.0
              );
  _cs.AddAlias(     -1, 'GLM',
                        9031
              );
  _cs.AddAlias(     -1, 'm_Ger',
                        9031
              );
  _cs.Add     (   9033, 'US_survey_chain',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        792.0 / 39.37
              );
  _cs.AddAlias(     -1, 'chain_US',
                        9033
              );
  _cs.AddAlias(     -1, 'chUS',
                        9033
              );
  _cs.Add     (   9034, 'US_survey_link',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        7.92 / 39.37
              );
  _cs.AddAlias(     -1, 'link_US',
                        9034
              );
  _cs.AddAlias(     -1, 'lkUS',
                        9034
              );
  _cs.Add     (   9035, 'US_survey_mile',
                        'mi',
                        TGIS_CSUnitsType.Linear,
                        63360.0 / 39.37
              );
  _cs.AddAlias(     -1, 'mi_US',
                        9035
              );
  _cs.AddAlias(     -1, 'miUS',
                        9035
              );
  _cs.Add     (   9036, 'kilometre',
                        'km',
                        TGIS_CSUnitsType.Linear,
                        1000.0 / 1.0
              );
  _cs.AddAlias(     -1, 'kilometer',
                        9036
              );
  _cs.AddAlias(     -1, 'km',
                        9036
              );
  _cs.Add     (   9037, 'Clarke_s_yard',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.9143917962 / 1.0
              );
  _cs.AddAlias(     -1, 'Clarke_yard',
                        9037
              );
  _cs.AddAlias(     -1, 'yd_Cla',
                        9037
              );
  _cs.AddAlias(     -1, 'ydCla',
                        9037
              );
  _cs.Add     (   9038, 'Clarke_s_chain',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        20.1166195164 / 1.0
              );
  _cs.AddAlias(     -1, 'chain_Cla',
                        9038
              );
  _cs.AddAlias(     -1, 'chCla',
                        9038
              );
  _cs.AddAlias(     -1, 'Clarke_chain',
                        9038
              );
  _cs.Add     (   9039, 'Clarke_s_link',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        0.201166195164 / 1.0
              );
  _cs.AddAlias(     -1, 'Clarke_link',
                        9039
              );
  _cs.AddAlias(     -1, 'link_Clarke_s_ratio',
                        9039
              );
  _cs.AddAlias(     -1, 'link_Cla',
                        9039
              );
  _cs.AddAlias(     -1, 'lkCla',
                        9039
              );
  _cs.Add     (   9040, 'British_yard_Sears_1922',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        36.0 / 39.370147
              );
  _cs.AddAlias(     -1, 'yard',
                        9040
              );
  _cs.AddAlias(     -1, 'yd_Se',
                        9040
              );
  _cs.AddAlias(     -1, 'ydSe',
                        9040
              );
  _cs.Add     (   9041, 'British_foot_Sears_1922',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        12.0 / 39.370147
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9041
              );
  _cs.AddAlias(     -1, 'ft_Se',
                        9041
              );
  _cs.AddAlias(     -1, 'ftSe',
                        9041
              );
  _cs.Add     (   9042, 'British_chain_Sears_1922',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        792.0 / 39.370147
              );
  _cs.AddAlias(     -1, 'chain',
                        9042
              );
  _cs.AddAlias(     -1, 'chain_Se',
                        9042
              );
  _cs.AddAlias(     -1, 'chSe',
                        9042
              );
  _cs.Add     (   9043, 'British_link_Sears_1922',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        7.92 / 39.370147
              );
  _cs.AddAlias(     -1, 'link',
                        9043
              );
  _cs.AddAlias(     -1, 'link_Se',
                        9043
              );
  _cs.AddAlias(     -1, 'lkSe',
                        9043
              );
  _cs.Add     (   9050, 'British_yard_Benoit_1895_A',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.9143992 / 1.0
              );
  _cs.AddAlias(     -1, 'yard',
                        9050
              );
  _cs.AddAlias(     -1, 'yd_BnA',
                        9050
              );
  _cs.AddAlias(     -1, 'ydBnA',
                        9050
              );
  _cs.Add     (   9051, 'British_foot_Benoit_1895_A',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.9143992 / 3.0
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9051
              );
  _cs.AddAlias(     -1, 'ft_BnA',
                        9051
              );
  _cs.AddAlias(     -1, 'ftBnA',
                        9051
              );
  _cs.Add     (   9052, 'British_chain_Benoit_1895_A',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        20.1167824 / 1.0
              );
  _cs.AddAlias(     -1, 'chain',
                        9052
              );
  _cs.AddAlias(     -1, 'chain_BnA',
                        9052
              );
  _cs.AddAlias(     -1, 'chBnA',
                        9052
              );
  _cs.Add     (   9053, 'British_link_Benoit_1895_A',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        0.201167824 / 1.0
              );
  _cs.AddAlias(     -1, 'link',
                        9053
              );
  _cs.AddAlias(     -1, 'link_BnA',
                        9053
              );
  _cs.AddAlias(     -1, 'lkBnA',
                        9053
              );
  _cs.Add     (   9060, 'British_yard_Benoit_1895_B',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        36.0 / 39.370113
              );
  _cs.AddAlias(     -1, 'yard',
                        9060
              );
  _cs.AddAlias(     -1, 'yd_BnB',
                        9060
              );
  _cs.AddAlias(     -1, 'ydBnB',
                        9060
              );
  _cs.Add     (   9061, 'British_foot_Benoit_1895_B',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        12.0 / 39.370113
              );
end;

procedure prepare_2;
begin
  _cs.AddAlias(     -1, 'foot_A',
                        9061
              );
  _cs.AddAlias(     -1, 'ft_BnB',
                        9061
              );
  _cs.AddAlias(     -1, 'ftBnB',
                        9061
              );
  _cs.Add     (   9062, 'British_chain_Benoit_1895_B',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        792.0 / 39.370113
              );
  _cs.AddAlias(     -1, 'chain',
                        9062
              );
  _cs.AddAlias(     -1, 'chain_BnB',
                        9062
              );
  _cs.AddAlias(     -1, 'chBnB',
                        9062
              );
  _cs.Add     (   9063, 'British_link_Benoit_1895_B',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        7.92 / 39.370113
              );
  _cs.AddAlias(     -1, 'link',
                        9063
              );
  _cs.AddAlias(     -1, 'link_BnB',
                        9063
              );
  _cs.AddAlias(     -1, 'lkBnB',
                        9063
              );
  _cs.Add     (   9070, 'British_foot_1865',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.9144025 / 3.0
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9070
              );
  _cs.AddAlias(     -1, 'ft_Br65',
                        9070
              );
  _cs.AddAlias(     -1, 'ftBr_65',
                        9070
              );
  _cs.Add     (   9080, 'Indian_foot',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        12.0 / 39.370142
              );
  _cs.AddAlias(     -1, 'ft_Ind',
                        9080
              );
  _cs.AddAlias(     -1, 'ftInd',
                        9080
              );
  _cs.AddAlias(     -1, 'Indian_geodetic_foot',
                        9080
              );
  _cs.Add     (   9081, 'Indian_foot_1937',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.30479841 / 1.0
              );
  _cs.AddAlias(     -1, 'ft_Ind37',
                        9081
              );
  _cs.AddAlias(     -1, 'ftInd_37',
                        9081
              );
  _cs.AddAlias(     -1, 'Indian_geodetic_foot',
                        9081
              );
  _cs.Add     (   9082, 'Indian_foot_1962',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.3047996 / 1.0
              );
  _cs.AddAlias(     -1, 'ft_Ind62',
                        9082
              );
  _cs.AddAlias(     -1, 'ftInd_62',
                        9082
              );
  _cs.Add     (   9083, 'Indian_foot_1975',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.3047995 / 1.0
              );
  _cs.AddAlias(     -1, 'ft_Ind75',
                        9083
              );
  _cs.AddAlias(     -1, 'ftInd_75',
                        9083
              );
  _cs.Add     (   9084, 'Indian_yard',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        36.0 / 39.370142
              );
  _cs.AddAlias(     -1, 'yard',
                        9084
              );
  _cs.AddAlias(     -1, 'yd_Ind',
                        9084
              );
  _cs.AddAlias(     -1, 'ydInd',
                        9084
              );
  _cs.Add     (   9085, 'Indian_yard_1937',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.91439523 / 1.0
              );
  _cs.AddAlias(     -1, 'yard',
                        9085
              );
  _cs.AddAlias(     -1, 'yd_Ind37',
                        9085
              );
  _cs.AddAlias(     -1, 'ydInd_37',
                        9085
              );
  _cs.Add     (   9086, 'Indian_yard_1962',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.9143988 / 1.0
              );
  _cs.AddAlias(     -1, 'yd_Ind62',
                        9086
              );
  _cs.AddAlias(     -1, 'ydInd_62',
                        9086
              );
  _cs.Add     (   9087, 'Indian_yard_1975',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.9143985 / 1.0
              );
  _cs.AddAlias(     -1, 'yd_Ind75',
                        9087
              );
  _cs.AddAlias(     -1, 'ydInd_75',
                        9087
              );
  _cs.Add     (   9093, 'Statute_mile',
                        'mi',
                        TGIS_CSUnitsType.Linear,
                        1609.344 / 1.0
              );
  _cs.AddAlias(     -1, 'mi',
                        9093
              );
  _cs.AddAlias(     -1, 'mile',
                        9093
              );
  _cs.Add     (   9094, 'Gold_Coast_foot',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        6378300.0 / 20926201.0
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9094
              );
  _cs.AddAlias(     -1, 'ft_GC',
                        9094
              );
  _cs.AddAlias(     -1, 'ftGC',
                        9094
              );
  _cs.Add     (   9095, 'British_foot_1936',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.3048007491 / 1.0
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9095
              );
  _cs.AddAlias(     -1, 'ft_Br36',
                        9095
              );
  _cs.AddAlias(     -1, 'ftBr_36',
                        9095
              );
  _cs.Add     (   9096, 'yard',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.9144 / 1.0
              );
  _cs.AddAlias(     -1, 'international_yard',
                        9096
              );
  _cs.AddAlias(     -1, 'yd',
                        9096
              );
  _cs.Add     (   9097, 'chain',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        20.1168 / 1.0
              );
  _cs.AddAlias(     -1, 'ch',
                        9097
              );
  _cs.AddAlias(     -1, 'chain_A',
                        9097
              );
  _cs.AddAlias(     -1, 'international_chain',
                        9097
              );
  _cs.Add     (   9098, 'link',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        20.1168 / 100.0
              );
  _cs.AddAlias(     -1, 'international_link',
                        9098
              );
  _cs.AddAlias(     -1, 'link_A',
                        9098
              );
  _cs.AddAlias(     -1, 'lk',
                        9098
              );
  _cs.Add     (   9099, 'British_yard_Sears_1922_truncated',
                        'yd',
                        TGIS_CSUnitsType.Linear,
                        0.914398 / 1.0
              );
  _cs.AddAlias(     -1, 'yard_A',
                        9099
              );
  _cs.AddAlias(     -1, 'yd_SeT',
                        9099
              );
  _cs.AddAlias(     -1, 'ydSe_T',
                        9099
              );
  _cs.Add     (   9101, 'radian',
                        'rad',
                        TGIS_CSUnitsType.Angular,
                        1.0 / 1.0
              );
  _cs.AddAlias(     -1, 'rad',
                        9101
              );
  _cs.Add     (   9102, 'degree',
                        'deg',
                        TGIS_CSUnitsType.Angular,
                        Pi / 180.0
              );
  _cs.AddAlias(     -1, 'angular_degree',
                        9102
              );
  _cs.AddAlias(     -1, 'deg',
                        9102
              );
  _cs.AddAlias(     -1, 'dega',
                        9102
              );
  _cs.Add     (   9103, 'arc_minute',
                        'min',
                        TGIS_CSUnitsType.Angular,
                        Pi / 10800.0
              );
  _cs.AddAlias(     -1, 'angular_minute',
                        9103
              );
  _cs.AddAlias(     -1, 'min',
                        9103
              );
  _cs.AddAlias(     -1, 'mina',
                        9103
              );
  _cs.Add     (   9104, 'arc_second',
                        'sec',
                        TGIS_CSUnitsType.Angular,
                        Pi / 648000.0
              );
  _cs.AddAlias(     -1, 'angular_second',
                        9104
              );
  _cs.AddAlias(     -1, 'sec',
                        9104
              );
  _cs.AddAlias(     -1, 'seca',
                        9104
              );
  _cs.Add     (   9105, 'grad',
                        'grad',
                        TGIS_CSUnitsType.Angular,
                        Pi / 200.0
              );
  _cs.AddAlias(     -1, 'gon',
                        9105
              );
  _cs.AddAlias(     -1, 'gon',
                        9105
              );
  _cs.AddAlias(     -1, 'gr',
                        9105
              );
  _cs.Add     (   9106, 'gon',
                        'gon',
                        TGIS_CSUnitsType.Angular,
                        Pi / 200.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'g',
                        9106
              );
  _cs.AddAlias(     -1, 'gon_A',
                        9106
              );
  _cs.Add     (   9109, 'microradian',
                        'urad',
                        TGIS_CSUnitsType.Angular,
                        1.0 / 1000000.0
              );
  _cs.AddAlias(     -1, 'urad',
                        9109
              );
  _cs.Add     (   9112, 'centesimal_minute',
                        'cgr',
                        TGIS_CSUnitsType.Angular,
                        Pi / 20000.0
              );
  _cs.AddAlias(     -1, 'centesimal_minute',
                        9112
              );
  _cs.AddAlias(     -1, 'cgr',
                        9112
              );
  _cs.AddAlias(     -1, 'c',
                        9112
              );
  _cs.Add     (   9113, 'centesimal_second',
                        'ccgr',
                        TGIS_CSUnitsType.Angular,
                        Pi / 2000000.0
              );
  _cs.AddAlias(     -1, 'centesimal_second',
                        9113
              );
  _cs.AddAlias(     -1, 'cc',
                        9113
              );
  _cs.AddAlias(     -1, 'ccgr',
                        9113
              );
end;

procedure prepare_3;
begin
  _cs.Add     (   9114, 'mil_6400',
                        'mil',
                        TGIS_CSUnitsType.Angular,
                        Pi / 3200.0
              );
  _cs.AddAlias(     -1, 'angular_mil',
                        9114
              );
  _cs.AddAlias(     -1, 'mil',
                        9114
              );
  _cs.AddAlias(     -1, 'mila',
                        9114
              );
  _cs.Add     (   9122, 'degree_supplier_to_define_representation',
                        'deg',
                        TGIS_CSUnitsType.Angular,
                        Pi / 180.0
              );
  _cs.AddAlias(     -1, 'deg',
                        9122
              );
  _cs.Add     (   9204, 'Bin_width_330_US_survey_feet',
                        'Bin330ftUS',
                        TGIS_CSUnitsType.Linear,
                        3960.0 / 39.37
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin330ftUS',
                        9204
              );
  _cs.Add     (   9205, 'Bin_width_165_US_survey_feet',
                        'Bin165ftUS',
                        TGIS_CSUnitsType.Linear,
                        1980.0 / 39.37
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin165ftUS',
                        9205
              );
  _cs.Add     (   9206, 'Bin_width_82_5_US_survey_feet',
                        'Bin82_5ftUS',
                        TGIS_CSUnitsType.Linear,
                        990.0 / 39.37
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin82_5ftUS',
                        9206
              );
  _cs.Add     (   9207, 'Bin_width_37_5_metres',
                        'Bin37_5m',
                        TGIS_CSUnitsType.Linear,
                        37.5 / 1.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin37_5m',
                        9207
              );
  _cs.Add     (   9208, 'Bin_width_25_metres',
                        'Bin25m',
                        TGIS_CSUnitsType.Linear,
                        25.0 / 1.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin25m',
                        9208
              );
  _cs.Add     (   9209, 'Bin_width_12_5_metres',
                        'Bin12_5m',
                        TGIS_CSUnitsType.Linear,
                        12.5 / 1.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin12_5m',
                        9209
              );
  _cs.Add     (   9210, 'Bin_width_6_25_metres',
                        'Bin6_25m',
                        TGIS_CSUnitsType.Linear,
                        6.25 / 1.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin6_25m',
                        9210
              );
  _cs.Add     (   9211, 'Bin_width_3_125_metres',
                        'Bin3_125m',
                        TGIS_CSUnitsType.Linear,
                        3.125 / 1.0
              )         .MarkDeprecated( True );
  _cs.AddAlias(     -1, 'Bin3_125m',
                        9211
              );
  _cs.Add     (   9300, 'British_foot_Sears_1922_truncated',
                        'ft',
                        TGIS_CSUnitsType.Linear,
                        0.914398 / 3.0
              );
  _cs.AddAlias(     -1, 'foot_A',
                        9300
              );
  _cs.AddAlias(     -1, 'ft_SeT',
                        9300
              );
  _cs.AddAlias(     -1, 'ftSe_T',
                        9300
              );
  _cs.Add     (   9301, 'British_chain_Sears_1922_truncated',
                        'ch',
                        TGIS_CSUnitsType.Linear,
                        20.116756 / 1.0
              );
  _cs.AddAlias(     -1, 'chain_A',
                        9301
              );
  _cs.AddAlias(     -1, 'chain_SeT',
                        9301
              );
  _cs.AddAlias(     -1, 'chSe_T',
                        9301
              );
  _cs.Add     (   9302, 'British_link_Sears_1922_truncated',
                        'li',
                        TGIS_CSUnitsType.Linear,
                        20.116756 / 100.0
              );
  _cs.AddAlias(     -1, 'link_A',
                        9302
              );
  _cs.AddAlias(     -1, 'link_SeT',
                        9302
              );
  _cs.AddAlias(     -1, 'lkSe_T',
                        9302
              );
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
end;

//==================================== END ====================================

