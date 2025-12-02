//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  Export to image window
}
program ExportToImage;

uses
  Vcl.Forms,
  mainform in 'mainform.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TatukGIS ExportToImage';
  Application.CreateForm(TfrmExportToImage, frmExportToImage);
  Application.Run;
end.

