//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to provied a join to external database and how to provide charts.
}
unit Unit1;

interface

uses

  System.SysUtils,
  System.Classes,
  System.Win.ComObj,
  System.Variants,

  Winapi.Windows,
  Winapi.AdoInt,

  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ToolWin,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ImgList,
  Vcl.ExtCtrls,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoLayerSHP,
  
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTopology,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,
  
  Lider.CG.GIS.VCL.GeoViewerWnd;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    GIS: TGIS_ViewerWnd;
    StatusBar1: TStatusBar;
    cmbValues: TComboBox;
    cmbSize: TComboBox;
    btnFullExtent: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    cmbStyle: TComboBox;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure btnFullExtentClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure ButtonZoomOutClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbChange(Sender: TObject);
  private
    { Private declarations }
    sqlDC : Variant ;
    sqlRS : Variant ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.FormCreate(Sender: TObject);
var
  ll : TGIS_LayerSHP ;
begin
  cmbSize.ItemIndex   := 0 ;
  cmbValues.ItemIndex := 0 ;
  cmbStyle.ItemIndex  := 0 ;

  // create ADO objects - one for keeping connection, second for data
  sqlDC := CreateOleObject('ADODB.Connection') ;
  sqlRS := CreateOleObject('ADODB.Recordset') ;

  // connect to database
  sqlDC.Open( Format( 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%sStats.mdb',
                      [ TGIS_Utils.GisSamplesDataDir + '\World\Countries\USA\Statistical\']
                    )
            ) ;

  // use layer to display charts
  ll      := TGIS_LayerSHP.Create ;
  ll.Path := TGIS_Utils.GisSamplesDataDir + '\World\Countries\USA\States\California\tl_2008_06_county.shp' ;
  ll.Name := 'tl_2008_06_county';

  ll.UseConfig                  := False ;
  ll.Params.Labels.Field        :='name';
  ll.Params.Labels.Pattern      := TGIS_BrushStyle.Clear ;
  ll.Params.Labels.OutlineWidth := 0 ;
  ll.Params.Labels.Font.Color   := TGIS_Color.White ;
  ll.Params.Labels.Color        := TGIS_Color.Black ;
  ll.Params.Labels.Position     := [ TGIS_LabelPosition.MiddleCenter,  TGIS_LabelPosition.Flow] ;
  ll.Params.Chart.Size        := GIS_RENDER_SIZE ;
  ll.Params.Render.StartSize  := 350  ;
  ll.Params.Render.EndSize    := 1000 ;

  GIS.Add(ll) ;
  GIS.FullExtent ;

  cmbChange(Self) ;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sqlRS.Close ;
  sqlDC.Close ;
end;

procedure TForm1.btnFullExtentClick(Sender: TObject);
begin
  GIS.FullExtent ;
end;

procedure TForm1.btnZoomInClick(Sender: TObject);
begin
   GIS.Zoom := GIS.Zoom * 2 ;
end;

procedure TForm1.ButtonZoomOutClick(Sender: TObject);
begin
   GIS.Zoom := GIS.Zoom / 2 ;
end;


procedure TForm1.cmbChange(Sender: TObject);
var
  ll      : TGIS_LayerVector ;
  vsize   : String ;
  vvalues : String ;
  vstyle  : String ;
  vmin    : Double ;
  vmax    : Double ;
   q       : _Recordset ;
  _query  : OleVariant ;
begin
  ll := TGIS_LayerVector(GIS.Items[0]) ;
  if not Assigned( ll ) then exit ;

  // get params
  vsize   := cmbSize.Items  [ cmbSize.ItemIndex   ] ;
  vvalues := cmbValues.Items[ cmbValues.ItemIndex ] ;
  vstyle  := cmbStyle.Items [ cmbStyle.ItemIndex  ] ;

  if sqlRS.State <> 0 then
    sqlRS.Close ;

  // find min, max values for shapes
  sqlRS.Open( Format( 'SELECT min(%s) AS mini, max(%s) AS maxi FROM ce2000t',
                      [ vsize, vsize ]
                    ),
              sqlDC
            ) ;

  vmin := sqlRS.Fields[ 'mini' ].Value ;
  vmax := sqlRS.Fields[ 'maxi' ].Value ;
  sqlRS.Close ;

  // let's load data to recordset
  _query := 'select * FROM ce2000t ORDER BY fips';
  sqlRS.Open( _query, sqlDC ) ;

  // connect layer with query results
  if (IUnknown(sqlRS).QueryInterface(_Recordset, q) = S_OK) then
      ll.JoinADO := q;

  // set primary and foreign keys
  ll.JoinPrimary := 'cntyidfp' ;
  ll.JoinForeign := 'fips'   ;

  // render results
  ll.Params.Render.Expression := vsize ;
  ll.Params.Render.Chart      := '0:0:' + vvalues ;
  ll.Params.Chart.Style       := ParamChart( vstyle, TGIS_ChartStyle.Pie ) ;
  ll.Params.Render.Zones      := 10 ;
  ll.Params.Render.MinVal     := vmin ;
  ll.Params.Render.MaxVal     := vmax ;

  GIS.InvalidateWholeMap ;
end;

end.

