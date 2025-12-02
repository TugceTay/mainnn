//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to prepare small coverage previewer.
  Main window.
}
program ttkLider.CG.GIS.GeoViewer;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  ExportForm in 'ExportForm.pas' {frmExportLayer},
  InfoForm in 'InfoForm.pas' {frmInfo},
  SearchForm in 'SearchForm.pas' {FSearch},
  EditForm in 'EditForm.pas' {fEdit};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TatukGIS CoverageViewer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExportLayer, frmExportLayer);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TFSearch, FSearch);
  Application.CreateForm(TfEdit, fEdit);
  Application.Run;
end.

