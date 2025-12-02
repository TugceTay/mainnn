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
  List of Prime Meridians.
  Generated automatically.
}


procedure Init_CSPrimeMeridianList(
  const _cs : TGIS_CSPrimeMeridianList
) ;

procedure prepare_1;
begin
  _cs.Add     (   8901, 'Greenwich', DegToRad(   0               )
              ) ;
  _cs.Add     (   8902, 'Lisbon'   , DegToRad(-  9.1319061111111 )
              ) ;
  _cs.Add     (   8903, 'Paris'    , DegToRad(   2.33722917      )
              ) ;
  _cs.Add     (   8904, 'Bogota'   , DegToRad(- 74.0809166666667 )
              ) ;
  _cs.Add     (   8905, 'Madrid'   , DegToRad(-  3.6879388888888 )
              ) ;
  _cs.Add     (   8906, 'Rome'     , DegToRad(  12.4523333333333 )
              ) ;
  _cs.Add     (   8907, 'Bern'     , DegToRad(   7.4395833333333 )
              ) ;
  _cs.Add     (   8908, 'Jakarta'  , DegToRad( 106.8077194444444 )
              ) ;
  _cs.Add     (   8909, 'Ferro'    , DegToRad(- 17.6666666666666 )
              ) ;
  _cs.Add     (   8910, 'Brussels' , DegToRad(   4.367975        )
              ) ;
  _cs.Add     (   8911, 'Stockholm', DegToRad(  18.058277777777  )
              ) ;
  _cs.Add     (   8912, 'Athens'   , DegToRad(  23.7163375       )
              ) ;
  _cs.Add     (   8913, 'Oslo'     , DegToRad(  10.722916666666  )
              ) ;
  _cs.AddAlias(   8914, 'Paris_RGS', 8903
              ) ;
end;

begin
  prepare_1;

end;
{==================================== END =====================================}


