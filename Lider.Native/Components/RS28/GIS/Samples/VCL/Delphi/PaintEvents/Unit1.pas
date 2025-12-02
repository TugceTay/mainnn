//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.78.0.31836-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//=============================================================================
{
  How to manage Paint events.
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
  Vcl.Buttons,
  Vcl.ToolWin,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  Vcl.Direct2D,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoEditor,
  Lider.CG.GIS.GeoLayerSHP,
  
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoViewer,
  Lider.CG.GIS.GeoRendererAbstract,

  Lider.CG.GIS.VCL.GeoViewerWnd;

type
  TForm1 = class(TForm)
    GIS: TGIS_ViewerWnd;
    StatusBar1: TStatusBar;
    Toolbar1: TToolBar;
    btnFullExtent: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    ToolButton4: TToolButton;
    chkDrag: TCheckBox;
    ImageList1: TImageList;
    Panel1: TPanel;
    chkBeforePaintRendererEvent: TCheckBox;
    chkBeforePaintEvent: TCheckBox;
    chkAfterPaintEvent: TCheckBox;
    chkAfterPaintRendererEvent: TCheckBox;
    chkPaintExtraEvent: TCheckBox;
    btnTestPrintBmp: TButton;
    chkPrintBmpWithEvents: TCheckBox;
    SaveDialog1: TSaveDialog;
    grbRenderers: TRadioGroup;
    rbDirect2D: TRadioButton;
    rbGdiPlus: TRadioButton;
    rbGdi32: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure chkDragClick(Sender: TObject);
    procedure btnFullExtentClick(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure chkBeforePaintRendererEventClick(Sender: TObject);
    procedure chkBeforePaintEventClick(Sender: TObject);
    procedure chkPaintExtraEventClick(Sender: TObject);
    procedure chkAfterPaintEventClick(Sender: TObject);
    procedure chkAfterPaintRendererEventClick(Sender: TObject);
    procedure GISBeforePaintRendererEvent(_sender: TObject;
      _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
    procedure GISBeforePaintEvent(_sender, _canvas: TObject);
    procedure GISPaintExtraEvent(_sender: TObject;
      _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
    procedure GISAfterPaintEvent(_sender, _canvas: TObject);
    procedure GISAfterPaintRendererEvent(_sender: TObject;
      _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
    procedure btnTestPrintBmpClick(Sender: TObject);
    procedure rbDirect2DClick(Sender: TObject);
    procedure rbGdiPlusClick(Sender: TObject);
    procedure rbGdi32Click(Sender: TObject);
  private
    { Private declarations }
    center_ptg : TGIS_Point ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Lider.CG.GIS.GeoRegistredLayers,
  Lider.CG.GIS.VCL.GeoGdiPlus,
  Lider.CG.GIS.VCL.GeoRendererDirect2D,
  Lider.CG.GIS.VCL.GeoRendererGdiPlus,
  Lider.CG.GIS.VCL.GeoRendererGdi32 ;

procedure TForm1.FormCreate(Sender: TObject);
var
  ll : TGIS_LayerSHP ;
begin
  // add layer
  ll := TGIS_LayerSHP.Create ;
  ll.Path := TGIS_Utils.GisSamplesDataDir + '\World\Countries\USA\States\California\Counties.shp' ;
  GIS.Add(ll) ;
  GIS.FullExtent ;
  center_ptg := GIS.CenterPtg ;
end;

procedure TForm1.GISAfterPaintEvent(_sender, _canvas: TObject);
var
  bcl : TGIS_Color ;
  canvasD2D : TDirect2DCanvas ;
  canvasVcl : TCanvas ;
begin
  bcl := TGIS_Color.Blue ;
  if _canvas is TDirect2DCanvas then begin
    canvasD2D := TDirect2DCanvas(_canvas);
    canvasD2D.Pen.Color := bcl.ToBGR ;
    canvasD2D.Pen.Width := 1 ;
    canvasD2D.DrawRectangle(Rect(70, 70, GIS.Width-70, GIS.Height-70));
  end else begin
    canvasVcl := TCanvas(_canvas);
    canvasVcl.Pen.Color := bcl.ToBGR ;
    canvasVcl.Pen.Width := 1;
    canvasVcl.Brush.Style := bsClear;
    canvasVcl.Rectangle(Rect(70, 70, GIS.Width-70, GIS.Height-70));
  end;
end ;

procedure TForm1.GISAfterPaintRendererEvent(_sender: TObject;
  _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
var
  rdr : TGIS_RendererAbstract ;
begin
  rdr := _renderer;
  rdr.CanvasPen.Color := TGIS_Color.Blue;
  rdr.CanvasPen.Width := 1;
  rdr.CanvasBrush.Style := TGIS_BrushStyle.Clear;
  rdr.CanvasDrawRectangle(Rect(100, 100, GIS.Width-100, GIS.Height-100));
end ;

procedure TForm1.GISBeforePaintEvent(_sender, _canvas: TObject);
var
  bcl : TGIS_Color ;
  canvasD2D  : TDirect2DCanvas ;
  canvasVcl  : TCanvas ;
begin
  bcl := TGIS_Color.Blue ;
  if _canvas is TDirect2DCanvas then begin
    canvasD2D := TDirect2DCanvas(_canvas);
    canvasD2D.Pen.Color := bcl.ToBGR ;
    canvasD2D.Pen.Width := 1 ;
    canvasD2D.DrawRectangle(Rect(40, 40, GIS.Width-40, GIS.Height-40));
  end else begin
    canvasVcl := TCanvas(_canvas);
    canvasVcl.Pen.Color := bcl.ToBGR ;
    canvasVcl.Pen.Width := 1;
    canvasVcl.Brush.Style := bsClear;
    canvasVcl.Rectangle(Rect(40, 40, GIS.Width-40, GIS.Height-40));
  end;
end ;

procedure TForm1.GISBeforePaintRendererEvent(_sender: TObject;
  _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
var
  cn  : TObject ;
  rdr : TGIS_RendererAbstract ;
  bcl : TGIS_Color ;
  canvasD2D  : TDirect2DCanvas ;
  canvasPlus : TGIS_GdipGraphics ;
  brushPlus  : TGIS_GdipSolidBrush ;
  canvasVcl  : TCanvas ;
begin
  rdr := _renderer;
  cn  := rdr.CanvasNative;
  bcl := TGIS_Color.FromRGB($EE, $E8, $AA);
  if cn is TDirect2DCanvas then begin
    canvasD2D := TDirect2DCanvas(cn);
    canvasD2D.Brush.Color := bcl.ToBGR ;
    canvasD2D.FillRect( Rect(0, 0, GIS.Width, GIS.Height) );
  end else if cn is TGIS_GdipGraphics then begin
    canvasPlus := TGIS_GdipGraphics(cn);
    brushPlus := TGIS_GdipSolidBrush.Create( bcl ) ;
    canvasPlus.FillRectangle( brushPlus, Rect(0, 0, GIS.Width, GIS.Height ));
    brushPlus.Free;
  end else begin
    canvasVcl := TCanvas(cn);
    canvasVcl.Brush.Color := bcl.ToBGR ;
    canvasVcl.FillRect( Rect(0, 0, GIS.Width, GIS.Height));
  end;
  rdr.CanvasPen.Color := TGIS_Color.Blue;
  rdr.CanvasPen.Width := 1;
  rdr.CanvasBrush.Style := TGIS_BrushStyle.Clear;
  rdr.CanvasDrawRectangle(Rect(10, 10, GIS.Width-10, GIS.Height-10));
end ;

procedure TForm1.GISPaintExtraEvent(_sender: TObject;
  _renderer: TGIS_RendererAbstract; _mode: TGIS_DrawMode);
var
  txt : String ;
  pt  : TPoint ;
  ptc : TPoint ;
begin
  txt := 'PaintExtra' ;
  _renderer.CanvasFont.Name := 'Courier New' ;
  _renderer.CanvasFont.Size := 24 ;
  _renderer.CanvasFont.Color := TGIS_Color.Blue ;
  pt := _renderer.CanvasTextExtent( txt ) ;
  ptc := GIS.MapToScreen( center_ptg ) ;
  _renderer.CanvasDrawText( Rect( ptc.X - pt.X div 2,
                                  ptc.Y - pt.Y div 2,
                                  ptc.X + pt.X div 2,
                                  ptc.Y + pt.Y div 2 ),
                            txt ) ;
end;

procedure TForm1.chkAfterPaintEventClick(Sender: TObject);
begin
  if chkAfterPaintEvent.Checked then
    GIS.AfterPaintEvent := GISAfterPaintEvent
  else
    GIS.AfterPaintEvent := nil ;
  GIS.Invalidate ;
end;

procedure TForm1.chkAfterPaintRendererEventClick(Sender: TObject);
begin
  if chkAfterPaintRendererEvent.Checked then
    GIS.AfterPaintRendererEvent := GISAfterPaintRendererEvent
  else
    GIS.AfterPaintRendererEvent := nil ;
  GIS.Invalidate ;
end;

procedure TForm1.chkBeforePaintEventClick(Sender: TObject);
begin
  if chkBeforePaintEvent.Checked then
    GIS.BeforePaintEvent := GISBeforePaintEvent
  else
    GIS.BeforePaintEvent := nil ;
  GIS.Invalidate ;
end;

procedure TForm1.chkBeforePaintRendererEventClick(Sender: TObject);
begin
  if chkBeforePaintRendererEvent.Checked then
    GIS.BeforePaintRendererEvent := GISBeforePaintRendererEvent
  else
    GIS.BeforePaintRendererEvent := nil ;
  GIS.Invalidate ;
end;

procedure TForm1.chkPaintExtraEventClick(Sender: TObject);
begin
  if chkPaintExtraEvent.Checked then
    GIS.PaintExtraEvent := GISPaintExtraEvent
  else
    GIS.PaintExtraEvent := nil ;
  GIS.Invalidate ;
end;

procedure TForm1.btnTestPrintBmpClick(Sender: TObject);
var
  bitmap : TBitmap ;
begin
  if not SaveDialog1.Execute then exit ;
  bitmap := nil ;
  try
    GIS.PrintBmp( bitmap, chkPrintBmpWithEvents.Checked ) ;
    bitmap.SaveToFile( SaveDialog1.FileName ) ;
  finally
    bitmap.Free ;
  end;
end;

procedure TForm1.rbDirect2DClick(Sender: TObject);
begin
  GIS.Renderer := TGIS_RendererVclDirect2D.Create ;
  GIS.InvalidateWholeMap ;
end;

procedure TForm1.rbGdiPlusClick(Sender: TObject);
begin
  GIS.Renderer := TGIS_RendererVclGdiPlus.Create ;
  GIS.InvalidateWholeMap ;
end;

procedure TForm1.rbGdi32Click(Sender: TObject);
begin
  GIS.Renderer := TGIS_RendererVclGdi32.Create ;
  GIS.InvalidateWholeMap ;
end;

procedure TForm1.chkDragClick(Sender: TObject);
begin
  // change viewer mode
  if chkDrag.Checked then begin
     GIS.Mode := TGIS_ViewerMode.Drag ;
  end else begin
     GIS.Mode := TGIS_ViewerMode.Select ;
  end
end;

procedure TForm1.btnFullExtentClick(Sender: TObject);
begin
  GIS.FullExtent ;
end;

procedure TForm1.btnZoomInClick(Sender: TObject);
begin
   // change viewer zoom
   GIS.Zoom := GIS.Zoom * 2 ;
end;

procedure TForm1.btnZoomOutClick(Sender: TObject);
begin
   // change viewer zoom
   GIS.Zoom := GIS.Zoom / 2 ;
end;

end.

