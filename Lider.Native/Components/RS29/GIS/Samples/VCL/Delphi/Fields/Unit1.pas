//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to use field properties of a layer.
}

unit Unit1;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.DBGrids,

  Data.DB,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoDataSet,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerVector,
  
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoViewer,
  
  Lider.CG.GIS.VCL.GeoControlLegend,
  Lider.CG.GIS.VCL.GeoViewerWnd;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    Button1: TButton;
    GIS: TGIS_ViewerWnd;
    chkUseSymbols: TCheckBox;
    GIS_ControlLegend: TGIS_ControlLegend;
    ToolButton1: TToolButton;
    GIS_DataSet1: TGIS_DataSet;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Lider.CG.GIS.GeoParams ;

procedure TForm1.Button1Click(Sender: TObject);
var
  lv  : TGIS_LayerVector ;
  shp : TGIS_Shape ;
  i   : Integer ;
  an  : Double ;
begin
  GIS.Close ;

  lv := TGIS_LayerVector.Create ;
  lv.Name := 'Fields' ;
  lv.Open ;

  lv.AddField( 'rotateLabel', TGIS_FieldType.Float, 10, 4 ) ;
  lv.AddField( 'rotateSymbol', TGIS_FieldType.Float, 10, 4 ) ;
  lv.AddField( 'color', TGIS_FieldType.Number, 10, 0 ) ;
  lv.AddField( 'outlinecolor', TGIS_FieldType.Number, 10, 0 ) ;
  lv.AddField( 'size', TGIS_FieldType.Number, 10, 0 ) ;
  lv.AddField( 'label', TGIS_FieldType.String, 1, 0 ) ;
  lv.AddField( 'position', TGIS_FieldType.String, 1, 0 ) ;
  lv.AddField( 'scale', TGIS_FieldType.Float, 10, 4 ) ;

  for i := 0 to 10 do begin
    shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Projection ) ;
    shp.AddPart ;
    shp.AddPoint( TGIS_Utils.GisPoint( Random( 20 ), Random( 20 ) ) ) ;
    an := Random( 360 ) ;
    shp.SetField( 'rotateLabel',  an ) ;
    shp.SetField( 'rotateSymbol', an ) ;
    shp.SetField( 'color', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
    shp.SetField( 'outlinecolor', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
    shp.SetField( 'label', 'Point' + IntToStr( i ) ) ;
    shp.SetField( 'size', Random( 400 ) ) ;
    shp.SetField( 'position', ConstructParamPosition( [ TGIS_labelPosition( Random( 9 ) ) ] ) ) ;
    shp.SetField( 'scale', Pi/180 ) ;
    shp.Unlock ;
  end;

  shp := lv.CreateShape( TGIS_ShapeType.Arc );
  shp.Lock( TGIS_Lock.Extent ) ;
  shp.AddPart ;
  for i := 0 to 10 do begin
    shp.AddPoint( TGIS_Utils.GisPoint( Random( 20 ) - 10 , Random( 20 ) - 10 ) );
  end ;
  an := Random( 360 ) ;
  shp.SetField( 'rotateLabel',  an ) ;
  shp.SetField( 'rotateSymbol', an ) ;
  shp.SetField( 'color', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
  shp.SetField( 'label', 'Point' + IntToStr( 1 ) ) ;
  shp.SetField( 'outlinecolor', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
  shp.SetField( 'scale', Pi/180 ) ;
  shp.Unlock ;

  for i := 1 to 11 do begin
    shp := lv.CreateShape( TGIS_ShapeType.Arc );
    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;
    shp.AddPoint( TGIS_Utils.GisPoint( 20+ 2*i , 0 ) );
    shp.AddPoint( TGIS_Utils.GisPoint( 30+ 2*i , 10 ) );
    an := Random( 360 ) ;
    shp.SetField( 'rotateLabel',  an ) ;
    shp.SetField( 'rotateSymbol', an ) ;
    shp.SetField( 'size', 20*i ) ;
    shp.SetField( 'color', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
    shp.SetField( 'outlinecolor', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
    shp.SetField( 'scale', Pi/180 ) ;
    shp.Unlock ;
  end ;

  shp := lv.CreateShape( TGIS_ShapeType.Polygon );
  shp.Lock( TGIS_Lock.Extent ) ;
  shp.AddPart ;
  for i := 0 to 3 do
  begin
    shp.AddPoint( TGIS_Utils.GisPoint( Random( 20 ) - 10 , Random( 20 ) - 10 ) );
  end ;
  an := Random( 360 ) ;
  shp.SetField( 'rotateLabel',  an ) ;
  shp.SetField( 'rotateSymbol', an ) ;
  shp.SetField( 'color', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
  shp.SetField( 'outlinecolor', Random(256) shl 16 + Random( 256 ) shl 8 + Random ( 256 ) ) ;
  shp.SetField( 'label', 'Point' + IntToStr( 2 ) ) ;
  shp.Unlock ;

  lv.Params.Marker.ColorAsText           := 'FIELD:color' ;
  lv.Params.Marker.OutlineColorAsText    := 'FIELD:outlinecolor' ;
  lv.Params.Marker.OutlineWidth          := 1 ;
  lv.Params.Marker.Size                  := -20 ;

  if chkUseSymbols.Checked then
  begin
    lV.Params.Marker.Symbol              := SymbolList.Prepare( TGIS_Utils.GisSamplesDataDir + '\Symbols\2267.cgm' ) ;
    lv.Params.Marker.SizeAsText          := 'FIELD:size:1 px' ;
    lv.Params.Marker.SymbolRotateAsText  := 'FIELD:rotateSymbol' ;
  end;

  lv.Params.Labels.Field                 := 'label' ;
  lv.Params.Labels.Allocator             := False ;
  lv.Params.Labels.ColorAsText           := 'FIELD:color' ;
  lv.Params.Labels.OutlineColorAsText    := 'FIELD:outlinecolor' ;
  lv.Params.Labels.PositionAsText        := 'FIELD:position' ;
  lv.Params.Labels.FontColorAsText       := 'FIELD:color' ;
  lv.Params.Labels.RotateAsText          := 'FIELD:rotateLabel:1 deg';


  if chkUseSymbols.Checked then
  begin
    lV.Params.Line.Symbol                := SymbolList.Prepare( TGIS_Utils.GisSamplesDataDir + '\Symbols\1301.cgm' ) ;
    lv.Params.Line.SymbolRotateAsText    := 'FIELD:rotateSymbol:1 deg';
  end;
  lv.Params.Line.Width                   := -10 ;
  lv.Params.Line.ColorAsText             := 'FIELD:color' ;
  lv.Params.Line.OutlineColorAsText      := 'FIELD:outlinecolor' ;
  lv.Params.Line.WidthAsText             := 'FIELD:size:1 px';

  lv.Params.Area.SymbolRotateAsText    := 'FIELD:rotateSymbol' ;
  if chkUseSymbols.Checked then
    lV.Params.Area.Symbol                := SymbolList.Prepare( TGIS_Utils.GisSamplesDataDir + '\Symbols\1301.cgm' ) ;
  lv.Params.Area.ColorAsText           := 'FIELD:color' ;
  lv.Params.Area.OutlineColorAsText    := 'FIELD:outlinecolor' ;

  GIS.Add( lv ) ;
  GIS.FullExtent ;
  GIS_ControlLegend.GIS_Layer := lv ;
  GIS_ControlLegend.Update ;

  GIS_DataSet1.Open( lv, lv.Extent ) ;
end;

end.

