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
  List of Projected Coordinate System.
  User defined list.
  New items should be defined as 902xxx.
}


procedure Init_CSProjectedCoordinateSystemListEx(
  const _cs : TGIS_CSProjectedCoordinateSystemList ;
  const _pp : TGIS_CSProjParameters
) ;

function p(
  _central_meridian           : Double  ;
  _latitude_of_origin         : Double  ;
  _false_easting              : Double  ;
  _false_northing             : Double  ;
  _standard_parallel_1        : Double  ;
  _standard_parallel_2        : Double  ;
  _pseudo_standard_parallel_1 : Double  ;
  _zone                       : Integer ;
  _scale_factor               : Double  ;
  _longitude_of_center        : Double  ;
  _latitude_of_center         : Double  ;
  _azimuth                    : Double  ;
  _longitude_of_point_1       : Double  ;
  _latitude_of_point_1        : Double  ;
  _longitude_of_point_2       : Double  ;
  _latitude_of_point_2        : Double  ;
  _x_scale                    : Double  ;
  _y_scale                    : Double  ;
  _xy_plane_rotation          : Double
) : TGIS_CSProjParameters ;
begin
  Result := _pp ;
  Result.CentralMeridian          := DegToRad(  _central_meridian           );
  Result.LatitudeOfOrigin         := DegToRad(  _latitude_of_origin         );
  Result.FalseEasting             :=           _false_easting               ;
  Result.FalseNorthing            :=           _false_northing              ;
  Result.StandardParallel_1       := DegToRad( _standard_parallel_1        );
  Result.StandardParallel_2       := DegToRad( _standard_parallel_2        );
  Result.PseudoStandardParallel_1 := DegToRad( _pseudo_standard_parallel_1 );
  Result.Zone                     :=           _zone                        ;
  Result.ScaleFactor              :=           _scale_factor                ;
  Result.LongitudeOfCenter        := DegToRad( _longitude_of_center        );
  Result.LatitudeOfCenter         := DegToRad( _latitude_of_center         );
  Result.Azimuth                  := DegToRad( _azimuth                    );
  Result.LongitudeOfPoint_1       := DegToRad( _longitude_of_point_1       );
  Result.LatitudeOfPoint_1        := DegToRad( _latitude_of_point_1        );
  Result.LongitudeOfPoint_2       := DegToRad( _longitude_of_point_2       );
  Result.LatitudeOfPoint_2        := DegToRad( _latitude_of_point_2        );
  Result.XScale                   :=           _x_scale                     ;
  Result.YScale                   :=           _y_scale                     ;
  Result.XYPlaneRotation          := DegToRad( _xy_plane_rotation          );
end ;

procedure prepare_1;
begin
  _cs.AddAlias( 900013, 'Google_Maps_Global_Mercator',
                        3785
              );

  _cs.AddAlias( 900913, 'Spherical_Mercator',
                        3785
              );
  _cs.AddAlias( 102100, 'Spherical_Mercator_ESRI_1',
                        3785
              );
  _cs.AddAlias( 102113, 'Spherical_Mercator_ESRI_2',
                        3785
              );

end;

procedure fix_1;
begin
  _cs.Fix     (   3785, 'Popular_Visualisation_CRS_Mercator',
                        4055, 9001, 9804,
                        p(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
                        False, 3544
              );
end;

begin
  prepare_1;
  fix_1;
end;
{==================================== END =====================================}

