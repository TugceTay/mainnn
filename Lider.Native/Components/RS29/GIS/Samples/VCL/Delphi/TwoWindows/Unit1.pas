// This source code is a part of TatukGIS Developer Kernel.
// DK 11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
{
  How to support synchronized, two-windows display.
}
unit Unit1;

interface

uses
  System.SysUtils,
  System.Classes,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  Vcl.ToolWin,
  Vcl.ComCtrls,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoAllLayers,
  Lider.CG.GIS.GeoEditor,
  
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,
  
  Lider.CG.GIS.VCL.GeoViewerWnd;

type
  TForm1 = class(TForm)
    GIS_ViewerWnd1: TGIS_ViewerWnd;
    GIS_ViewerWnd2: TGIS_ViewerWnd;
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    Button1: TButton;
    CheckBox1: TCheckBox;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure GIS_ViewerWnd2AfterPaint(_sender, _canvas: TObject);
    procedure GIS_ViewerWnd1AfterPaint(_sender, _canvas: TObject);
  private
    zoom  : double ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.Button1Click(Sender: TObject);
begin
  // open the same project for two viewers
  GIS_ViewerWnd1.Open( TGIS_Utils.GisSamplesDataDir + '\World\Countries\Poland\DCW\poland.ttkproject' ) ;
  GIS_ViewerWnd1.Zoom := GIS_ViewerWnd1.Zoom * 3 ;
  GIS_ViewerWnd1.Mode := TGIS_ViewerMode.Zoom  ;
  GIS_ViewerWnd2.Open( TGIS_Utils.GisSamplesDataDir + '\World\Countries\Poland\DCW\poland.ttkproject' ) ;
  GIS_ViewerWnd2.Zoom := GIS_ViewerWnd2.Zoom * 4 ;
  zoom := GIS_ViewerWnd2.Zoom;
end;

procedure TForm1.GIS_ViewerWnd2AfterPaint(_sender, _canvas: TObject);
begin
 // synchronize two viewers
 if TGIS_Utils.GisIsSamePoint( GIS_ViewerWnd2.Center, GIS_ViewerWnd1.Center ) then exit ;
 GIS_ViewerWnd1.Center := GIS_ViewerWnd2.Center ;

  if checkBox1.Checked then
   GIS_ViewerWnd2.Zoom  := GIS_ViewerWnd1.Zoom
  else
   GIS_ViewerWnd2.Zoom := zoom;
end;

procedure TForm1.GIS_ViewerWnd1AfterPaint(_sender, _canvas: TObject);
begin
 // synchronize two viewers
  if TGIS_Utils.GisIsSamePoint( GIS_ViewerWnd2.Center, GIS_ViewerWnd1.Center ) then exit ;
  GIS_ViewerWnd2.Center := GIS_ViewerWnd1.Center ;

  if checkBox1.Checked then
   GIS_ViewerWnd2.Zoom  := GIS_ViewerWnd1.Zoom
  else
   GIS_ViewerWnd2.Zoom := zoom;
end;


end.

