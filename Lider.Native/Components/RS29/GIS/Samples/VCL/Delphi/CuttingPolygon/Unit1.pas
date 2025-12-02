unit Unit1;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ToolWin,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoAllLayers,
  Lider.CG.GIS.GeoLayerSHP,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoViewer,
  
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoRegistredLayers,

  Lider.CG.GIS.VCL.GeoViewerWnd,
  Lider.CG.GIS.VCL.GeoControlLegend ;

type
  TForm1 = class(TForm)
    GIS: TGIS_ViewerWnd;
    GIS_ControlLegend1: TGIS_ControlLegend;
    ToolBar1: TToolBar;
    btnCutting: TButton;
    btnZoom: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCuttingClick(Sender: TObject);
    procedure btnZoomClick(Sender: TObject);
  private
  var
    ll  : TGIS_LayerVector ;
    lp  : TGIS_LayerPixel ;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnCuttingClick(Sender: TObject);
begin
  lp := TGIS_LayerPixel( GIS.Items[ 0 ] ) ;
  lp.CuttingPolygon := TGIS_ShapePolygon( ll.GetShape( 1 ).CreateCopyCS( lp.CS ) );
  ll.Active := False ;
  GIS.InvalidateWholeMap ;
end;

procedure TForm1.btnZoomClick(Sender: TObject);
begin
  GIS.Mode := TGIS_ViewerMode.Zoom ;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  shp : TGIS_Shape ;
begin
  GIS.Lock;

  GIS.Open( TGIS_Utils.GisSamplesDataDir + '\World\VisibleEarth\world_8km.jpg');
  ll := TGIS_LayerVector.Create ;
  ll.Name := 'shape' ;
  GIS.Add(ll) ;

  ll.Lock;
  shp := ll.CreateShape(TGIS_ShapeType.Polygon);
  shp.Lock(TGIS_Lock.Extent);
  shp.AddPart ;
  shp.AddPoint( TGIS_Utils.GisPoint( -5,8) );
  shp.AddPoint( TGIS_Utils.GisPoint( 40,2 ) );
  shp.AddPoint( TGIS_Utils.GisPoint( 20,-20) );
  shp.Unlock;
  ll.Unlock;

  GIS.Unlock;
end;

end.

