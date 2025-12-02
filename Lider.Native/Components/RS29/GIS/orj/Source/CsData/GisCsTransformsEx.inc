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
  List of Coordinate System Transformations.
  User defined list.
  New items should be defined as 901xxx.
}

procedure Init_CSTranformListEx(
  const _cs : TGIS_CSTransformList
) ;

procedure prepare_1;
const
  MR   = 1 / 1000000.0        ; // microradian
  AC   = (Pi/180.0) / 3600.0  ; // arc second
  CC  =  (Pi/200.0) / 10000.0 ; // centesimal second
  PM  =  1 / 1000000.0        ; // parts-per-milion
begin
  _cs.Add     ( 901100, 'Anna_1_1965_1',
                        9603, 7003, 1, 1069,
                        -491, -22, 435,
                        0, 0, 0,
                        0,
                        0, 0, 0
                   );
  _cs.Add     ( 901102, 'Astro_1952_1',
                        9603, 7022, 1, 907025,
                        124, -234, -25,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901103, 'Beacon_E_1945_1',
                        9603, 7022, 1, 907026,
                        145, 75, -272,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901105, 'Canton_1966_1',
                        9603, 7022, 1, 907028,
                        298, -304, -375,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901109, 'GUX_1_1',
                        9603, 7022, 1, 907032,
                        252, -209, -751,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901111, 'Wake_Eniwetok_1960_1',
                        9603, 7053, 1, 907034,
                        102, 52, -38,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901112, 'Estonia_1937_1',
                        9603, 7004, 1, 1090,
                        374, 150, 588,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901114, 'Oman_1',
                        9603, 7012, 1, 1183,
                        -346, -1, 224,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901115, 'Observ_Meteorologico_1939_1',
                        9603, 7022, 1, 1344,
                        -425, -169, 81,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901116, 'Fort_Thomas_1955_1',
                        9603, 7012, 1, 1200,
                        -7, 215, 225,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901119, 'Graciosa_Base_SW_1948_1',
                        9603, 7022, 1, 1301,
                        -104, 167, -38,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901121, 'Hermannskogel_1',
                        9603, 7004, 1, 907036,
                        682, -203, 480,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901125, 'Everest_Bangladesh_1',
                        9603, 7015, 1, 1041,
                        282, 726, 254,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901126, 'Everest_India_Nepal_1',
                        9603, 7044, 1, 2411,
                        295, 736, 257,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901127, 'ISTS_061_1968_1',
                        9603, 7022, 1, 1331,
                        -794, 119, -298,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901128, 'ISTS_073_1969_1',
                        9603, 7022, 1, 907038,
                        208, -435, -229,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901130, 'Kerguelen_Island_1949_1',
                        9603, 7022, 1, 2816,
                        145, -187, 103,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901132, 'LC5_1961_1',
                        9603, 7008, 1, 1063,
                        42, 124, 147,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901145, 'Reunion_1',
                        9603, 7022, 1, 907043,
                        94, -948, -1262,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901147, 'Sao_Braz_1',
                        9603, 7022, 1, 1345,
                        -203, 141, 53,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901149, 'DOS_71_4_1',
                        9603, 7022, 1, 1219,
                        -320, 550, -494,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901150, 'South_Asia_Singapore_1',
                        9603, 905004, 1, 1210,
                        7, -10, -26,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901156, 'Israel_1',
                        9606, 7019, 1, 1226,
                        -23.5, -18.19, -17.53,
                        -0.3*AC, -1.84*AC, 1.65*AC,
                        5.43*PM,
                        0, 0, 0
              )         .MarkDeprecated( True );
  _cs.Add     ( 901160, 'S42_Hungary_1',
                        9603, 7024, 1, 1119,
                        28, -121, -77,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901161, 'Jamaica_1969_1',
                        9603, 7008, 1, 1128,
                        -69, -207, -389,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );
  _cs.Add     ( 901163, 'Popular_Visualisation_Datum_1',
                        9619, 7059, 1, 3544,
                        0, 0, 0,
                        0, 0, 0,
                        0,
                        0, 0, 0
              );

  _cs.Add     ( 901164, 'Israel_to_WGS_84_7params',
                        9606, 7019, 1, 1126,
                        -23.50, -18.19, -17.53,
                        -0.3000*AC, -1.8400*AC, 1.6400*AC,
                        5.43000*PM,
                        0, 0, 0
              )         .MarkDeprecated( True );
  _cs.Add     ( 901165, 'Lisboa_Hayford_to_WGS_84',
                        9606, 7022, 1, 1193,
                        -283.088, -70.693, 117.445,
                        -1.157*AC, 0.059*AC, -0.652*AC,
                        -4.058*PM,
                        0, 0, 0
              );
  _cs.Add     ( 901166, 'Israeli_to_WGS_94_MAPI',
                        9606, 7019, 1, 1126,
                        -24.0024, -17.1032, -17.8444,
                        -0.33009*AC, -1.85269*AC, 1.66969*AC,
                        5.4248 *PM,
                        0, 0, 0
              )         .MarkDeprecated( True );
  _cs.Add     ( 901167, 'NAD27_To_NAD83',
                        9615, 7008, 1, 1325,
                        'conus,alaska,hawaii,prvi,stgeorge,stlrnc,stpaul,NTV2_0'
              );
  _cs.Add     ( 901168, 'IG05_12_to_WGS_94_1',
                        9606, 7019, 1, 1126,
                        24.0024, 17.1032, 17.8444,
                        -0.33009*AC, -1.85269*AC, 1.66969*AC,
                        -5.4248 *PM,
                        0, 0, 0
              );
  _cs.Add     ( 901169, 'NAD83_HARN_to_WGS_84',
                        9615, 7019, 1, 1337,
                        'alhpgn,arhpgn,azhpgn,cnhpgn,cohpgn,cshpgn,emhpgn,'+
                        'eshpgn,ethpgn,flhpgn,gahpgn,guhpgn,hihpgn,iahpgn,'+
                        'ilhpgn,inhpgn,kshpgn,kyhpgn,lahpgn,mdhpgn,mehpgn,'+
                        'mihpgn,mnhpgn,mohpgn,mshpgn,nbhpgn,nchpgn,ndhpgn,'+
                        'nehpgn,njhpgn,nmhpgn,nvhpgn,nyhpgn,ohhpgn,okhpgn,'+
                        'pahpgn,pvhpgn,schpgn,sdhpgn,tnhpgn,uthpgn,vahpgn,'+
                        'wihpgn,wmhpgn,wohpgn,wshpgn,wthpgn,wvhpgn,wyhpgn'
              );
//AT_GIS_GRID.gsb
end;

begin
  prepare_1;

end;
{==================================== END =====================================}

