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
  List of Elliipsoid.
  User defined list.
  New items should be defined as 905xxx.
}


procedure Init_CSEllipsoidListEx(
  const _cs : TGIS_CSEllipsoidList
) ;

procedure prepare_1;
begin
  _cs.Add     ( 905002, 'Fischer_1960',
                        6378166.000 , 298.3
              ) ;
  _cs.Add     ( 905003, 'Fischer_1968',
                        6378150.000 , 298.3
              ) ;
  _cs.Add     ( 905004, 'Fischer_Modified',
                        6378155.000 , 298.3
              ) ;
  _cs.Add     ( 905005, 'Walbeck',
                        6376896.000 , 302.78
              ) ;
end;

procedure alias_1;
begin
  _cs.AddAlias(     -1, 'WGS_1984',
                        7030
              );
  _cs.Alias   (   1024, 7019
              ) ;
  _cs.Alias   (   7031, 7030
              ) ;
  _cs.Alias   (   7003, 7050
              ) ;
end;


begin
  prepare_1;
  alias_1;
end;
{==================================== END =====================================}


