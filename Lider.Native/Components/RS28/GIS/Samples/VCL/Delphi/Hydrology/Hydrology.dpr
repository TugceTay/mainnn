//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
program Hydrology;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {frmHydrology},
  Lider.CG.GIS.GeoHydrology;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHydrology, frmHydrology);
  Application.Run;
end.

