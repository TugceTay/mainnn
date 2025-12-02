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
  List of Units.
  User defined list.
  New items should be defined as 904xxx.
}


procedure Init_CSUnitsListEx(
  const _cs : TGIS_CSUnitsList
) ;

procedure prepare_1;
begin
  _cs.Fix     (   9001, 'Meter',
                        'm',
                        TGIS_CSUnitsType.Linear,
                        1.0 / 1.0
              ) ; // just replace 'metre' with 'Meter' as default
  _cs.AddAlias(     -1, 'metre',
                        9001
              ) ; // and add 'metre' alias

  _cs.Fix     (   9122, 'Degree',
                        'deg',
                        TGIS_CSUnitsType.Angular,
                        Pi / 180.0
              ) ; // 9122 ha bad name

  _cs.AddAlias( 904001, 'Milimeter',
                        1025
              ) ;  // DK own definition has now an official EPSG version
  _cs.AddAlias( 904002, 'Centimeter',
                        1033
              ) ;  // DK own definition has now an official EPSG version
  _cs.Add     ( 904003, 'Inch',
                        'in',
                        TGIS_CSUnitsType.Linear,
                        2.54/100
              ) ;
end ;

procedure prepare_2;
begin
  _cs.Add     ( 904101, 'Hectar',
                        'h',
                         TGIS_CSUnitsType.Areal,
                        100.0*100.0
              ) ;
  _cs.Add     ( 904102, 'Acre_International',
                        'ac',
                        TGIS_CSUnitsType.Areal,
                        4046.8564224
              ) ; 
  _cs.Add     ( 904103, 'US_Survey_Acre',
                        'ac',
                        TGIS_CSUnitsType.Areal,
                        4046.87261
              ) ;
end ;

procedure prepare_3;
begin
  _cs.AddAuto ( 904201, 'Metric', 
                        [904001, 904002, 9001, 904101, 9036] 
              ) ;
  _cs.AddAuto ( 904202, 'US', 
                        [904003,   9003, 9096, 904103, 9035] 
              ) ;
end ;

procedure alias_1;
begin
  _cs.Alias   (   9102, 9122
              ) ;
end;

begin
  prepare_1;
  prepare_2;
  prepare_3;
  alias_1;
end;
{==================================== END =====================================}


