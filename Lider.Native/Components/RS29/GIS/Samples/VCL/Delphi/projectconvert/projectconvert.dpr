//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to create format converter
}
program projectconvert;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,

  
  Lider.CG.GIS.GeoAllLayers,

  Vcl.Lider.CG.GIS.GeoViewerBMP;

var
  vwr   : TGIS_ViewerBmp  ;
  path  : String          ;
begin
  if ParamCount <> 1 then
  begin
    Writeln('TatukGIS Samples - TTKGP -> TTKPROJECT converter');
    Writeln('Enter path of the TTKGP project. TTKPROJECT output will be placed in the same directory.');
    Writeln('TTKGP file will be kept in its place after conversion.');
    Writeln('Put directories with filenames and .TTKGP extension into parameters.');
    Readln;
    exit;
  end;

  vwr := TGIS_ViewerBmp.Create;
  path := ParamStr(1);
  vwr.Open(path);

  path := TPath.ChangeExtension( path, '.ttkproject' ) ;
  vwr.SaveProjectAs( path );
  vwr.Free
end.

