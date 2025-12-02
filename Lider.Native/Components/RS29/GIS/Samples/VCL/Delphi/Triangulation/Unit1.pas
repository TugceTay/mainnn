//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  Generates a Voronoi diagram/Delaunay triangulation.
}
unit Unit1;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Variants,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,

  
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerSHP,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,
  Lider.CG.GIS.GeoInterpolation,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoContour,
  Lider.CG.GIS.GeoOpenCL,
  
  Lider.CG.GIS.VCL.GeoControlAttributes,
  Lider.CG.GIS.VCL.GeoControlLegend,
  Lider.CG.GIS.VCL.GeoViewerWnd, System.ImageList;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    GIS: TGIS_ViewerWnd;
    StatusBar: TStatusBar;
    btnFullExtent: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    GIS_Attributes: TGIS_ControlAttributes;
    grpbxResult: TGroupBox;
    rdbtnVoronoi: TRadioButton;
    rdbtnDelaunay: TRadioButton;
    btnGenerate: TButton;
    lblName: TLabel;
    edtName: TEdit;
    GIS_ControlLegend1: TGIS_ControlLegend;
    procedure FormCreate(Sender: TObject);
    procedure GISMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnFullExtentClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure rdbtnVoronoiClick(Sender: TObject);
    procedure rdbtnDelaunayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Lider.CG.GIS.GeoTriangulation ;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // open a file
  GIS.Open( TGIS_Utils.GisSamplesDataDir + '\World\Countries\Poland\DCW\city.shp' ) ;
 // GIS.Open('C:\Users\Developer\Desktop\SELCUK_TM279-ucgen\SELCUK_TM279-ucgen\N_TAKEOMETRI_POINT.shp' ) ;


  // and add a new parametr
  with TGIS_LayerVector( GIS.Items[0] ) do
  begin
    Params.Marker.Color         := TGIS_Color.FromRGB($4080FF) ;
    Params.Marker.OutlineWidth  := 2 ;
    Params.Marker.Style         := TGIS_MarkerStyle.Circle ;

    ParamsList.Add ;
    Params.Style := 'selected' ;
    Params.Area.OutlineWidth := 1 ;
    Params.Area.color := TGIS_Color.Blue ;
  end ;
   GIS_ControlLegend1.Update ;
end;

procedure TForm1.GISMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ptg : TGIS_Point ;
  shp : TGIS_Shape ;
begin
  if GIS.IsEmpty then exit ;
  if GIS.InPaint then exit ;

  // let's locate a shape after click
  ptg := GIS.ScreenToMap( Point(x, y ) );
  shp := TGIS_Shape( GIS.Locate( ptg, 5/GIS.Zoom ) ) ; // 5 pixels precision
  if shp <> nil then
    GIS_Attributes.ShowShape(shp) ;
end;

procedure TForm1.rdbtnDelaunayClick(Sender: TObject);
begin
  edtName.Text := 'Delaunay' ;
end;

procedure TForm1.rdbtnVoronoiClick(Sender: TObject);
begin
  edtName.Text := 'Voronoi' ;
end;

procedure TForm1.btnFullExtentClick(Sender: TObject);
begin
  GIS.FullExtent ;
end;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  lVrn: TGIS_LayerVector ;
  Tin_To_Grid: TGIS_TinToGrid;
  grid_layer: TGIS_LayerPixel;
  contour_generator: TGIS_ContourGenerator;
  contour_layer: TGIS_LayerVector ;
  lPoints : TGIS_LayerVector;
begin
  if gis.Get( edtName.Text ) <> nil then
  begin
    ShowMessage( 'Result layer already exists. Use different name.') ;
    exit ;
  end ;

  if rdbtnVoronoi.Checked then
    lVrn := TGIS_LayerVoronoi.Create
  else
    lVrn := TGIS_LayerDelaunay.Create ;

  var ext := GIS.Extent;
  lPoints := TGIS_LayerVector( GIS.Items[0] ) ;
 (* var lPoints := TGIS_LayerVector.Create;
  lPoints.Name := 'points';
  lPoints.Open;
  lPoints.CS := GIS.CS;
  lPoints.DefaultShapeType := TGIS_ShapeType.Point;
  lPoints.DefaultDimension := TGIS_DimensionType.XYZ;

  for var i := 0 to 1000 do begin

    var shp := lPoints.CreateShape(TGIS_ShapeType.Point);
    shp.Lock(TGIS_Lock.Projection);
    try
      shp.AddPart;
      var x := ext.XMin + Random * (ext.XMax - ext.XMin);
      var y := ext.YMin + Random * (ext.YMax - ext.YMin);
      var z := Sqr(x) + Sqr(y) ;

      shp.AddPoint3D( TGIS_Utils.GisPoint3D( x, y, z ));
    finally
      shp.Unlock;
    end;
  end;  *)

  lVrn.Name := edtName.Text ;
  lVrn.ImportLayer( lPoints, GIS.Extent,
                    TGIS_ShapeType.Unknown, '', False
                   ) ;
  lVrn.Transparency := 60 ;

  lVrn.Params.Render.Expression := 'GIS_AREA' ;
  lVrn.Params.Render.MinVal     := 10000000 ;
  lVrn.Params.Render.MaxVal     := 1300000000 ;
  lVrn.Params.Render.StartColor := TGIS_Color.White ;

  if rdbtnVoronoi.Checked then
    lVrn.Params.Render.EndColor  := TGIS_Color.Red
  else
    lVrn.Params.Render.EndColor := TGIS_Color.Blue ;
  lVrn.Params.Render.Zones      := 10 ;
  lVrn.Params.Area.Color        := TGIS_Color.RenderColor ;
  lVrn.CS := GIS.CS ;

  GIS.Add( lVrn ) ;
  GIS.InvalidateWholeMap ;
  GIS_ControlLegend1.Update ;

  grid_layer := TGIS_LayerPixel.Create;
  grid_layer.Name := 'grid' ;
  grid_layer.CS := lVrn.CS ;
  grid_layer.Build(True, lVrn.CS, lVrn.Extent, 0, 0 ) ;
  grid_layer.Params.Pixel.GridShadow := False;

  Tin_To_Grid := TGIS_TinToGrid.Create;
  Tin_To_Grid.Generate(lVrn, grid_layer);

  GIS.Add(grid_layer) ;
  GIS.InvalidateWholeMap ;
  GIS_ControlLegend1.Update ;

  contour_layer := TGIS_LayerVector.Create;
  contour_layer.Name := 'contours';
  contour_layer.CS := grid_layer.CS;

  contour_generator := TGIS_ContourGenerator.Create;
  contour_generator.ContourInterval := 10;
  contour_generator.Smoothen := True;
  contour_generator.Mode := TGIS_ContourGeneratorMode.Polylines;
  contour_generator.Generate(grid_layer, contour_layer, '');

  GIS.Add(contour_layer) ;
  GIS.InvalidateWholeMap ;
  GIS_ControlLegend1.Update ;
  ShowMessage(contour_layer.GetLastUid.ToString); // Result 0;

end;

procedure TForm1.btnZoomInClick(Sender: TObject);
begin
  GIS.Zoom := GIS.Zoom * 2 ;
end;

procedure TForm1.btnZoomOutClick(Sender: TObject);
begin
  GIS.Zoom := GIS.Zoom / 2 ;
end;

end.

