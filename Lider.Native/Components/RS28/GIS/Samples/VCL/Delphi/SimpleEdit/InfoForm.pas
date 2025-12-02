//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to provide editing functionality.
}
unit InfoForm;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,

  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Controls,
  Vcl.Grids,
  Vcl.ExtCtrls,

  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,

  Lider.CG.GIS.VCL.GeoControlAttributes;

type
  TfrmInfo = class(TForm)
    GISAttributes: TGIS_ControlAttributes;

  public
    { Public declarations }
    procedure ShowInfo( const _shp : TGIS_Shape ) ;
  end;

var
  frmInfo: TfrmInfo;

implementation

{$R *.DFM}


procedure TfrmInfo.ShowInfo( const _shp : TGIS_Shape ) ;
begin
  GISAttributes.ShowShape( _shp ) ;
end ;

end.




