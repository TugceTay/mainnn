//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to create 3D View
}

unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.Math,
  System.SysUtils,
  System.Types,
  System.Variants,
  
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.jpeg,

  
  Lider.CG.GIS.GeoAllLayers,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoLayerGrd,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoTypes3D,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,
  Lider.CG.GIS.GeoViewer3DBase,

  Lider.CG.GIS.VCL.GeoControl3D,
  Lider.CG.GIS.VCL.GeoControlLegend,
  Lider.CG.GIS.VCL.GeoViewerWnd;

type
  TForm1 = class(TForm)
    btnFullExtent: TButton;
    btn2D3D: TButton;
    lb3DMode: TLabel;
    cbx3DMode: TComboBox;
    GIS: TGIS_ViewerWnd;
    GIS_3D: TGIS_Control3D;
    btnNavigation: TButton;
    Image1: TImage;
    Image2: TImage;
    procedure btnFullExtentClick(Sender: TObject);
    procedure btn2D3DClick(Sender: TObject);
    procedure cbx3DModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GISMouseWheelDown(Sender: TObject;Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure GISMouseWheelUp(Sender: TObject;Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure btnNavigationClick(Sender: TObject);
    procedure GISMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn2D3DClick(Sender: TObject);
begin
  if GIS.IsEmpty then exit ;

  GIS.View3D := not GIS.View3D ;
  if GIS.View3D then
  begin
    btn2D3D.Caption := '2D View' ;
    GIS_3D.Enabled := true ;
  end else begin
    btn2D3D.Caption := '3D View' ;
    GIS_3D.Enabled := false;
  end;
  cbx3DMode.ItemIndex := 0 ;
end;

procedure TForm1.btnFullExtentClick(Sender: TObject);
begin
  if not GIS.View3D then
    GIS.FullExtent
  else
    GIS.Viewer3D.ResetView ;
end;

procedure TForm1.btnNavigationClick(Sender: TObject);
begin
  if not GIS.View3D then exit ;
  if not GIS.Viewer3D.AdvNavigation then begin
    GIS.Viewer3D.AdvNavigation := True ;
    btnNavigation.Caption := 'Std. Navigation' ;
    GIS.Viewer3D.FastMode := True ;
  end
  else begin
    GIS.Viewer3D.AdvNavigation := False ;
    btnNavigation.Caption := 'Adv. Navigation' ;
    GIS.Viewer3D.FastMode := False ;
  end;
end;

procedure TForm1.cbx3DModeChange(Sender: TObject);
begin
  if not GIS.View3D then exit ;

  case cbx3DMode.ItemIndex of
    0 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.CameraPosition;
    1 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.CameraXYZ ;
    2 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.CameraXY ;
    3 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.CameraRotation ;
    4 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.SunPosition ;
    5 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.Zoom ;
    6 : GIS.Viewer3D.Mode := TGIS_Viewer3DMode.Select ;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GIS.View3D := False ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //GIS_3D.GIS_Viewer := GIS;
  cbx3DMode.ItemIndex := 0 ;
  cbx3DModeChange(Self);
  GIS.Lock;
  try
    if GIS.View3D then
      btn2D3DClick( self ) ;
    GIS.Close ;
    //GIS.Open( 'C:\Lider.Native\Components\RS28\GIS\Data\Samples11\' + 'Samples\3D\Building3D.ttkproject');
    GIS.Open( 'C:\Lider.Native\Analysis\LiCity\ORNEK\30328093\M-104493826.gml');

    cbx3DMode.ItemIndex := 0 ;
  finally
    GIS.Unlock ;
  end;
end;

procedure TForm1.GISMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  shp  : TGIS_Shape ;
  prec : Integer ;
begin
  // if there is no layer or we are not in select mode, exit
  if GIS.IsEmpty then exit ;
  if GIS.InPaint then exit ;
  
  if GIS.View3D and ( GIS.Viewer3D.Mode = TGIS_Viewer3DMode.Select ) then begin

    prec := 20 ;
    shp := TGIS_Shape(GIS.Locate(Point(X, Y), prec));

    if Assigned(shp) then begin
      shp.IsSelected := not shp.IsSelected;
      GIS.Viewer3D.UpdateAllSelectedObjects;
    end;
  end ;
end;

procedure TForm1.GISMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt : TPoint ;
  cam: TGIS_Point3D ;
begin
  if GIS.IsEmpty then exit ;

  pt := GIS.ScreenToClient( MousePos ) ;

  if GIS.View3D then begin
    // allows MouseWheel works properly in ZoomMode
    GIS.Viewer3D.StoreMousePos( pt ) ;

    cam := GIS.Viewer3D.CameraPosition ;
    cam.Z := cam.Z  / ( 1 + 0.05 ) ;
    GIS.Viewer3D.CameraPosition := cam ;
  end
  else
    GIS.ZoomBy( 3/2, pt.X, pt.Y );

  Handled := True ;
end;

procedure TForm1.GISMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt : TPoint ;
  cam: TGIS_Point3D ;
begin
  if GIS.IsEmpty then exit ;

  pt := GIS.ScreenToClient( MousePos ) ;

  if GIS.View3D then begin
    // allows MouseWheel works properly in ZoomMode
    GIS.Viewer3D.StoreMousePos( pt ) ;

    cam := GIS.Viewer3D.CameraPosition ;
    cam.Z := cam.Z  * ( 1 + 0.05 ) ;
    GIS.Viewer3D.CameraPosition := cam ;
  end
  else
   GIS.ZoomBy( 2/3, pt.X, pt.Y );

  Handled := True ;
end;


end.

