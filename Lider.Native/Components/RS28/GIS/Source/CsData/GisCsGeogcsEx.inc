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
  List of Geographic Coordinate System.
  User defined list.
  New items should be defined as 903xxx.
}


procedure Init_CSGeographicCoordinateSystemListEx(
  const _cs : TGIS_CSGeographicCoordinateSystemList
) ;

procedure alias_1;
begin
  _cs.AddAlias(     -1, 'WGS_1984',
                        4326
              );
  _cs.AddAlias(     -1, 'WGS84',
                        4326
              );
  _cs.AddAlias(     -1, 'CRS84',
                        4326
              );
  _cs.AddAlias(     -1, 'WGS72',
                        4322
              );
  _cs.AddAlias(     -1, 'CRS27',
                        4322
              );
  _cs.AddAlias(     -1, 'CRS83',
                        4269
              );
  _cs.AddAlias(     -1, 'South_American_Datum_1969',
                        4618
              );
  _cs.AddAlias(     -1, 'GGRS_1987',
                        4121
              );
  _cs.AddAlias(     -1, 'North_American_1983_HARN',
                        4152
              );
  _cs.AddAlias(     -1, 'European_1950_ED77',
                        4154
              );
  _cs.AddAlias(     -1, 'European_Libyan_Datum_1979',
                        4159
              );
  _cs.AddAlias(     -1, 'New_Zealand_1949',
                        4272
              );
  _cs.AddAlias(     -1, 'Australian_1966',
                        4202
              );
  _cs.AddAlias(     -1, 'Australian_1984',
                        4203
              );
  _cs.AddAlias(     -1, 'Ain_el_Abd_1970',
                        4204
              );
  _cs.AddAlias(     -1, 'Bogota',
                        4218
              );
  _cs.AddAlias(     -1, 'European_1950',
                        4230
              );
  _cs.AddAlias(     -1, 'Kertau',
                        4245
              );
  _cs.AddAlias(     -1, 'Provisional_S_American_1956',
                        4248
              );
  _cs.AddAlias(     -1, 'ETRS_1989',
                        4258
              );
  _cs.AddAlias(     -1, 'Mporaloko',
                        4266
              );
  _cs.AddAlias(     -1, 'North_American_1927',
                        4267
              );
  _cs.AddAlias(     -1, 'North_American_1983',
                        4269
              );
  _cs.AddAlias(     -1, 'New_Zealand_1949',
                        4272
              );
  _cs.AddAlias(     -1, 'GDA_1994',
                        4283
              );
  _cs.AddAlias(     -1, 'South_American_1969',
                        4291
              );
  _cs.AddAlias(     -1, 'Tananarive_1925',
                        4297
              );
  _cs.AddAlias(     -1, 'Deutsches_Hauptdreiecksnetz',
                        4314
              );
  _cs.AddAlias(     -1, 'WGS_1972',
                        4322
              );
  _cs.AddAlias(     -1, 'WGS_1972_BE',
                        4324
              );
  _cs.AddAlias(     -1, 'NAD_1927_Definition_1976',
                        4608
              );
  _cs.AddAlias(     -1, 'NAD_1927_CGQ77',
                        4609
              );
  _cs.AddAlias(     -1, 'JGD_2000',
                        4612
              );
  _cs.AddAlias(     -1, 'ISN_1993',
                        4659
              );
  _cs.AddAlias(     -1, 'LKS_1992',
                        4661
              );
  _cs.AddAlias(     -1, 'LKS_1994',
                        4669
              );
  _cs.AddAlias(     -1, 'PRS_1992',
                        4683
              );
  _cs.AddAlias(     -1, 'FD_1954',
                        4741
              );
  _cs.AddAlias(     -1, 'RD/83',
                        4745
              );
  _cs.AddAlias(     -1, 'PD/83',
                        4746
              );
  _cs.AddAlias(     -1, 'RGNC_1991_93',
                        4749
              );
  _cs.AddAlias(     -1, 'DGN_1995',
                        4755
              );
  _cs.AddAlias(     -1, 'JAD_2001',
                        4758
              );
  _cs.AddAlias(     -1, 'RGNC_1991-93',
                        4749
              );

end;

begin
  alias_1;
end;
{==================================== END =====================================}

