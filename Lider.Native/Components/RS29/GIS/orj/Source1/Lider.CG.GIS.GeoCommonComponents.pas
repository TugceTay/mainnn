//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
//    $USER:LICENSE$
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  TatukGIS Developer Kernel registration unit.
}
unit Lider.CG.GIS.GeoCommonComponents;

interface

uses
  System.Classes ;
  
procedure Register;
 
implementation

uses
  Lider.CG.GIS.GeoDataSet,
  Lider.CG.GIS.GeoControlPrintPreviewSimple;

procedure Register ;
begin
  RegisterComponents( 'TatukGIS', [ TGIS_DataSet ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlPrintPreviewSimple ] ) ;
end;

end.
