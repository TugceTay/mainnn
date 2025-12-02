//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to provide editing functionality.
}

program SimpleEdit;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  InfoForm in 'InfoForm.pas' {frmInfo} ;

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'SimpleEdit - Sopot';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.Run;
end.

