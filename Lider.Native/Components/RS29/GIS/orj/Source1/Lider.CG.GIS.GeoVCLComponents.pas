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
unit Lider.CG.GIS.GeoVCLComponents;

interface

uses
  System.Classes ;
  
procedure Register;
 
implementation

uses
  Lider.CG.GIS.VCL.GeoViewerWnd,
  Lider.CG.GIS.VCL.GeoControlLegend,
  Lider.CG.GIS.VCL.GeoControlAttributes,
  Lider.CG.GIS.VCL.GeoControlNorthArrow,
  Lider.CG.GIS.VCL.GeoControlScale,
  Lider.CG.GIS.VCL.GeoControlPrintPreview,
  Lider.CG.GIS.VCL.GeoControl3D,
  Lider.CG.GIS.VCL.GeoGps;

procedure Register ;
begin
  RegisterComponents( 'TatukGIS', [ TGIS_ViewerWnd ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlLegend ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlAttributes ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlScale ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlPrintPreview ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_ControlNorthArrow ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_Control3D ] ) ;
  RegisterComponents( 'TatukGIS', [ TGIS_GpsNmea ] );
end;

end.
