//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to provide WMTS Layer support.
}
program WMTSManager;

uses
  Vcl.Forms,
  formMain in 'formMain.pas' {frmMain},
  formWMTS in 'formWMTS.pas' {frmWMTS};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmWMTS, frmWMTS);
  Application.Run;
end.

