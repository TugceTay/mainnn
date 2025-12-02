//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to provide SQL Layer support.
}

program SQLWizard;

uses
  Vcl.Forms,
  formLayerSQL in 'formLayerSQL.pas' {frmLayerSQL},
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmLayerSQL, frmLayerSQL);
  Application.Run;
end.

