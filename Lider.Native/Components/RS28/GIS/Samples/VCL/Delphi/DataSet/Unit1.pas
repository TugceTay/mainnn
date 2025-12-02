//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to use TGIS_DataSet.
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
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.FileCtrl,
  Vcl.ExtCtrls,
  Vcl.DBCtrls,

  Data.DB,

  Lider.CG.GIS.GeoAllLayers,
  Lider.CG.GIS.GeoDataSet,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerVector,
  
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer
  ,
  Lider.CG.GIS.VCL.GeoViewerWnd ;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    GIS: TGIS_ViewerWnd;
    Splitter1: TSplitter;
    GIS_DataSet1: TGIS_DataSet;
    DBGrid1: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure GIS_DataSet1AfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.FormCreate(Sender: TObject);
var
  ll : TGIS_LayerVector ;
begin
  GIS.Open( TGIS_Utils.GisSamplesDataDir + '\World\Countries\USA\States\California\tl_2008_06_county.shp' ) ;
  ll := TGIS_LayerVector(GIS.Items[0]);
  ll.Params.Labels.Field := 'GIS_UID';
  GIS_DataSet1.Open( TGIS_LayerVector( GIS.Items[0] ), GIS.Extent ) ;
end;

procedure TForm1.GIS_DataSet1AfterScroll(DataSet: TDataSet);
begin
  if GIS_DataSet1.ActiveShape <> nil then
  GIS.Lock ;
  GIS.VisibleExtent := GIS_DataSet1.ActiveShape.Extent ;
  GIS.Zoom := GIS.Zoom * 0.8 ;
  GIS.Unlock ;
end;

end.

