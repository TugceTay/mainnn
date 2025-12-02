unit Lider.CG.LiWork.Ribbon;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Math,
  Lider.CG.Com.LicadInt,System.StrUtils,
  Lider.CG.Com.RibbonInt,   licadutil,


  Lider.CG.Com.Lib,
  cxControls, dxListView,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Skin,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.ModulesConsts,
  Lider.CG.Com.Types,
  Lider.CG.Com.ModulesTypes,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.scalebar,
  Lider.CG.Com.ReferenceInt,
  Lider.CG.Com.RTree,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.LiWork.Olcekle,
  Lider.CG.LiWork.ActionSetTextSeparate,
  hattacek,
  cokludogrudonustur;

function OnModuleEvents(AEvent: TlicgModuleEvents; AParam: Pointer): Boolean; stdcall;
procedure RibbonAdd; stdcall;

type
  TMyEvents = class
  public
    class procedure MyButtonClick(Sender: TObject);
  end;


implementation

uses
  Lider.CG.LiWork.DataModuleCom,
  Lider.CG.LiWork.FormDeneme, DenemeAction, Lider.CG.LiWork.ActionStairHatch,
  Lider.CG.LiWork.NoktaEkleme,
  Lider.CG.LiWork.Dortgen,
  Lider.CG.LiWork.YaziEkleme,
  Lider.CG.LiWork.Katman,
  Lider.CG.LiWork.ActionLine,
  Lider.CG.LiWork.ActionPoint,
  Lider.CG.LiWork.ActionPoligon,
  Lider.CG.LiWork.ActionWrite,
  Lider.CG.LiWork.ActionDelete,
  Lider.CG.LiWork.LauncherUpdate,
  Lider.CG.LiWork.LauncherDelete,
  Lider.CG.LiWork.ActionLinesFromPoints,
  Lider.CG.LiWork.ActionCircle,
  Lider.CG.LiWork.LauncherLayerChange,
  Lider.CG.LiWork.LauncherSetTextUpperOrLower,
  Lider.CG.LiWork.LauncherSetTextFirstWordUpper,
  Lider.CG.LiWork.LauncherSetTextSpacingBetweenWord,
  Lider.CG.LiWork.LauncherSetTextRoundingToDecimal,
  Lider.CG.LiWork.LauncherSetTextMathOperations,
  Lider.CG.LiWork.ActionSetTextQuickText,
  Lider.CG.LiWork.ActionPointAciklamali,
//Lider.CG.LiWork.ActionSetText,
  Lider.CG.LiWork.LauncherPointFromText,
  Lider.CG.LiWork.HizliYaziDüzenleme;

var
  FlicgTrackedEntityEvent: TlicgTrackedEntityEvent;
  ButonTiklanan: Integer;

function OnModuleEvents(AEvent: TlicgModuleEvents; AParam: Pointer): Boolean; stdcall;
begin
  case AEvent of
    AfterModuleLoad:;
    BeforeProjectLoad:;
    AfterProjectLoad:;
    BeforeProjectClose:;
    BeforeActiveProjectChange:;
    AfterActiveProjectChange:;
    BeforeModuleUnload:;
  end;
end;

function RibbonUpdate(CommandIndex: Integer = 1): Boolean; stdcall;
begin
  Result := True;
end;

function RibbonEnable(CommandIndex: integer = 1): Boolean; stdcall;
begin
  Result := True;
end;

procedure AddPoints;
var
  Point: ILicgEntity;
  Rc, I, J :Integer;
  IncX, IncY: Integer;
begin
  IncX := 10;
  IncY := 10;

  Point := Licad.CreateEntityFactory.MakeEntity(idPoint, 1, _3D);
  Point.Geometry.Points.X[0] := 0;
  Point.Geometry.Points.Y[0] := 0;
  for I := 4 downto 0 do
  begin
    Point.Geometry.Points.X[0] := 0;
    for J := 0 to I do
    begin
      Point.Geometry.Points.X[0] := Point.Geometry.Points.X[0] + IncX;
      Point.Name := '1';
      Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Point);
    end;
    Point.Geometry.Points.Y[0] := Point.Geometry.Points.Y[0] - IncY;
  end;
//  Point.Geometry.Points.X[0] := 10;
//  Point.Geometry.Points.Y[0] := 10;
//  Point.Geometry.Points.Z[0] := 10;
//  Point.Name := '1';

//  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaUndelete);
//  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Point);
//  if Rc > 0 then
//    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uaUndelete);
//  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;;

  CurrCmdLine.ActiveDrawBox.RepaintExtent(Point.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Point.Extent);

end;

procedure AddLines;
var
  Line: ILicgEntity;
    Rc, I, J :Integer;
  IncX, IncY: Integer;
begin
  IncX := 20;
  IncY := 20;

  Line := Licad.CreateEntityFactory.MakeEntity(idLine, 1, _3D);
  Line.Geometry.Points.X[0] := 0;
  Line.Geometry.Points.Y[0] := 0;
  Line.Geometry.Points.X[1] := 10;
  Line.Geometry.Points.Y[1] := 0;
  //Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Line);
  for I := 0 to 4 do
  begin
    Line.Geometry.Points.X[0] := 0;
    Line.Geometry.Points.X[1] := Line.Geometry.Points.X[1] + IncX;
    Line.Geometry.Points.Y[0] := Line.Geometry.Points.Y[0] - IncY;
    Line.Geometry.Points.Y[1] := Line.Geometry.Points.Y[1] - IncY;
    Line.Name := '1';
    //Line.DrawTools.PenTool.Color := I * 50;
    //Line.DrawTools.PenTool.Style := I;
    //Line.DrawTools.PenTool.Width := I;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Line, False);
  end;
  CurrCmdLine.ActiveDrawBox.RepaintExtent(Line.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Line.Extent);
end;

procedure AddPolyLines;
var
  PolyLine: ILicgEntity;
  Rc, I: Integer;
begin
  PolyLine := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _3D);
  for I := 0 to 4 do
  begin
    PolyLine.Geometry.Points.Add(0, 0, 0);
    PolyLine.Geometry.Points.Add(10, 20, 0);
    PolyLine.Geometry.Points.Add(0, 20, 0);
    PolyLine.Geometry.Points.Add(10, 0, 0);
    PolyLine.Geometry.Points.Add(0, 0, 0);
  end;

  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(PolyLine);
  CurrCmdLine.ActiveDrawBox.RepaintExtent(PolyLine.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(PolyLine.Extent);

end;

procedure AddPolygon;
var
  Polygon: ILicgEntity;
  Rc, I: Integer;
begin
  Polygon := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _3D);
  Polygon := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _3D);
  Polygon.Geometry.BeginUpdate;    //Hýzlý çalýþmasý
//  for I := 0 to 4 do
//  begin
    Polygon.Geometry.Points.Add(0, 0, 0);
    Polygon.Geometry.Points.Add(10, 20, 0);
    Polygon.Geometry.Points.Add(0, 20, 0);
    Polygon.Geometry.Points.Add(10, 0, 0);
    Polygon.Geometry.Points.Add(0, 0, 0);
//  end;
  Polygon.Geometry.EndUpdate;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Polygon);
  CurrCmdLine.ActiveDrawBox.RepaintExtent(Polygon.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Polygon.Extent);
end;

procedure AddText;
var
  TextEnt: ILicgEntity;
  Rc:Integer;
begin
  TextEnt := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  TextEnt.AsText.Text := 'Arial';
  TextEnt.Geometry.Points[0] := AsCoor(0, 0);
  //TextEnt.DrawTools.FontTool.Angle := 90;
  TextEnt.DrawTools.FontTool.Color := clgreen;
  //TextEnt.DrawTools.FontTool.Height := 12;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt, false);

  TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
  TextEnt.Geometry.Points[0] := AsCoor(100, 100);
  //TextEnt.DrawTools.FontTool.Angle := 100;
  TextEnt.AsVectorialText.Text := 'Vector';
  TextEnt.DrawTools.FontTool.Color := clgreen;
  //TextEnt.AsVectorialText.CharWidthFactor := 1;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt, false);

  {TextEnt := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  TextEnt.AsText.Text := 'Ayhan';
  TextEnt.Geometry.Points[0] := AsCoor(50, 50);
  //TextEnt.DrawTools.FontTool.Angle := 90;
  TextEnt.DrawTools.FontTool.Color := clred;
  //TextEnt.DrawTools.FontTool.Height := 12;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt, false);}

   TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
  TextEnt.Geometry.Points[0] := AsCoor(150, 150);
  //TextEnt.DrawTools.FontTool.Angle := 100;
  TextEnt.AsVectorialText.Text := 'Selcan';
  TextEnt.DrawTools.FontTool.Color := clpurple;
  //TextEnt.AsVectorialText.CharWidthFactor := 1;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt, false);

  TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
  TextEnt.Geometry.Points[0] := AsCoor(50, 50);
  //TextEnt.DrawTools.FontTool.Angle := 100;
  TextEnt.AsVectorialText.Text := 'Ayhan';
  TextEnt.DrawTools.FontTool.Color := clred;
  //TextEnt.AsVectorialText.CharWidthFactor := 1;
  Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt, false);
end;

{procedure AddArc;
var
  Arc: ILicgEntity;
begin
  Arc := Licad.CreateEntityFactory.MakeEntity(idArc, 0, _3D);

end;

procedure AddCircle;
var
  Circle: ILicgEntity;
begin
  Circle := Licad.CreateEntityFactory.MakeEntity(idCircle, 0, _3D);

end;

procedure AddRectangle;
var
  Rectangle: ILicgEntity;
begin
  Rectangle := Licad.CreateEntityFactory.MakeEntity(idRectangle, 0,_3D);

end;

procedure AddEllips;
var
  Ellips: ILicgEntity;
begin
  Ellips := Licad.CreateEntityFactory.MakeEntity(idEllipse, 0, _3D);

end;

procedure AddSymbol;
var
  Symbol: ILicgEntity;
begin
  Symbol := Licad.CreateEntityFactory.MakeEntity(idPlace, 0, _3D);

end;

procedure AddCizgiForm;
var
  Entity: ILicgEntity;
  X1, Y1, Z1, X2, Y2,  Z2 : Double;
  Rc: Integer;
begin
  Entity := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _3D);
  fmCizgiEkleme := TfmCizgiEkleme.Create(nil);
  fmCizgiEkleme.ShowModal;
  if fmCizgiEkleme.ModalResult = mrOk then
  begin
    X1:= StrToFloat(fmCizgiEkleme.teX1Coor.Text);
    Y1:= StrToFloat(fmCizgiEkleme.teY1Coor.Text);
    Z1:= StrToFloat(fmCizgiEkleme.teZ1Coor.Text);
    X2:= StrToFloat(fmCizgiEkleme.teX2Coor.Text);
    Y2:= StrToFloat(fmCizgiEkleme.teY2Coor.Text);
    Z2:= StrToFloat(fmCizgiEkleme.teZ2Coor.Text);
    Entity.Geometry.Points.X[0] := X1;
    Entity.Geometry.Points.Y[0] := Y1;
    Entity.Geometry.Points.Z[0] := Z1;
    Entity.Geometry.Points.X[1] := X2;
    Entity.Geometry.Points.Y[1] := Y2;
    Entity.Geometry.Points.Z[1] := Z2;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Entity);
  end;

  CurrCmdLine.ActiveDrawBox.RepaintExtent(Entity.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Entity.Extent);
  Entity := nil;
end;  }

procedure AddNoktaForm;
var
  nokta: ILicgEntity;
  X, Y, Z : Double;
  Rc: Integer;
begin
  nokta := Licad.CreateEntityFactory.MakeEntity(idPoint, 1, _3D);
  fmNoktaEkleme := TfmNoktaEkleme.Create(nil);
  fmNoktaEkleme.ShowModal;
  if fmNoktaEkleme.ModalResult = mrOk then
  begin
    X := fmNoktaEkleme.ceXCoor.Value;
    Y := fmNoktaEkleme.ceYCoor.Value;
    Z := fmNoktaEkleme.ceZCoor.Value;
    Nokta.Geometry.Points.X[0] := X;
    Nokta.Geometry.Points.Y[0] := Y;
    Nokta.Geometry.Points.Z[0] := Z;
    Nokta.Name:= fmNoktaEkleme.teNoktaAd.Text;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Nokta);
  end;

  CurrCmdLine.ActiveDrawBox.RepaintExtent(nokta.Extent);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(nokta.Extent);
  nokta := nil;
end;

Procedure AddDortgenForm;
var
Dortgen: ILicgEntity;
X,Y,Z,Du,Yu: Double;
Rc: Integer;
begin
 Dortgen := Licad.CreateEntityFactory.MakeEntity(idPolygon, 1, _3D);
 fmDortgenBilgisi := TfmDortgenBilgisi.Create(nil);
 fmDortgenBilgisi.ShowModal;
 if fmDortgenBilgisi.ModalResult = mrOk then
  begin
    X := fmDortgenBilgisi.ceXCoor.Value;
    Y := fmDortgenBilgisi.ceYCoor.Value;
    Z := fmDortgenBilgisi.ceZCoor.Value;
    Du := fmDortgenBilgisi.ceDikU.Value;
    Yu := fmDortgenBilgisi.ceYatayU.Value;
    {Dortgen.Geometry.Points.X[0] := X;
    Dortgen.Geometry.Points.Y[0] := Y;
    Dortgen.Geometry.Points.X[0] := Z;}
    Dortgen.Geometry.Points.Add(X, Y, Z);
    Dortgen.Geometry.Points.Add(X+Yu, Y, Z);
    Dortgen.Geometry.Points.Add(X+Yu, Y+Du, Z);
    Dortgen.Geometry.Points.Add(X, Y+Du, Z);
    Dortgen.Geometry.Points.Add(X, Y, Z);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Dortgen);

    CurrCmdLine.ActiveDrawBox.RepaintExtent(Dortgen.Extent);
    CurrCmdLine.ActiveDrawBox.ZoomWindow(Dortgen.Extent);

  end;
  Dortgen:=nil;
end;

procedure AddYaziForm;
var
  Yazi: ILicgEntity;
  Rc:Integer;
begin
  fmYaziFormu := TfmYaziFormu.Create(nil);
  fmYaziFormu.ShowModal;
if fmYaziFormu.ModalResult = mrOk then
  if fmYaziFormu.chkVektor.Checked then
    begin
      Yazi := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
      Yazi.Geometry.Points[0] := AsCoor(100, 100);
      Yazi.DrawTools.FontTool.Angle := StrToInt(fmYaziFormu.ceAci.Text);
      Yazi.AsVectorialText.Text := fmYaziFormu.ceMetin.Text;
      //Yazi.DrawTools.FontTool.Color := clgreen;
      //Yazi.AsVectorialText.CharWidthFactor := 1;
      Yazi.DrawTools.FontTool.Height := StrToInt(fmYaziFormu.ceYBoyu.Text);
      Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Yazi);
    end
  else
   begin
    Yazi := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
    Yazi.AsText.Text := fmYaziFormu.ceMetin.Text;
    Yazi.Geometry.Points[0] := AsCoor(0, 0);
    Yazi.DrawTools.FontTool.Angle := StrToInt(fmYaziFormu.ceAci.Text) ;
    //Yazi.DrawTools.FontTool.Color := clgreen;
    Yazi.DrawTools.FontTool.Height := StrToInt(fmYaziFormu.ceYBoyu.Text);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Yazi);
   end;
   CurrCmdLine.ActiveDrawBox.RepaintExtent(Yazi.Extent);
   CurrCmdLine.ActiveDrawBox.ZoomWindow(Yazi.Extent);
   Yazi:=nil;
end;

procedure DeleteLayer(LName: String);
var
  Layer: TLicgBaseLayer;
begin
  Layer := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(LName);
  if Layer <> nil then
    CurrCmdLine.ActiveDrawBox.GIS.DeleteLayer(LName);
end;

function AddLayer(LName: String; C: TColor; PenWidth: double; IsLock: Boolean = False; IsVisible: Boolean = True): TLicgBaseLayer;
begin
  Result := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(LName);
  if Result = nil then
    Result := CurrCmdLine.ActiveDrawBox.GIS.CreateLayer(LName, ltDesktop, C);
  Result.LayerInfo.Selectable := Not IsLock;
  Result.LayerInfo.Visible := IsVisible;
  Result.LayerInfo.DefPenTool.Width := PenWidth;
  Result.LayerInfo.isLayerBrush := False;
end;

procedure AddLayers;
var
  Layer: TLicgBaseLayer;

begin
  fmKatmanForm := TfmKatmanForm.Create(nil);
  fmKatmanForm.btnSil.OnClick := TMyEvents.MyButtonClick;
  fmKatmanForm.ShowModal;
  if fmKatmanForm.ModalResult = mrOk then
    Layer := AddLayer(
      fmKatmanForm.ceAd.Text,
      fmKatmanForm.cxColorComboBox1.ColorValue,
      StrToFloat(fmKatmanForm.cbPen.Text),
      fmKatmanForm.chkKilit.Checked,
      fmKatmanForm.chkVisible.Checked);
end;

Procedure Delete;
var
  Layer: TLicgBaselayer;
  Ent: ILicgEntity;
  Rc: Integer;
begin
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaUndelete);
  Layer := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer;    //mevcut katmaný çek
  Layer.First;     //Katmanýn ilk nesnesinden baþlýyor
  repeat
    Rc := Layer.RecNo;        //kayýt numarasý
    Ent := Layer.LoadEntityWithRecNo(Rc);  //nesneyi katmandan çek
    if Ent = nil then
      Continue;
    Layer.DeleteEntity(Rc);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUndelete);
    Layer.Next;
  until (Layer.Eof);
  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  Ent := nil;
end;

{procedure PointAction;
begin
   CurrCmdLine.DoCommandEx(TPointAction.CreateAction(CurrCmdLine), TFarProcedure1(@PointAction));
end;}

procedure LineAction;
begin
   CurrCmdLine.DoCommandEx(TLineAction.CreateAction(CurrCmdLine), TFarProcedure1(@LineAction));
end;

procedure PoligonAction;
begin
   CurrCmdLine.DoCommandEx(TPoligonAction.CreateAction(CurrCmdLine), TFarProcedure1(@PoligonAction));
end;

procedure WriteAction;
begin
   CurrCmdLine.DoCommandEx(TWriteAction.CreateAction(CurrCmdLine), TFarProcedure1(@WriteAction));
end;

procedure DeleteAction;
begin
   CurrCmdLine.DoCommandEx(TDeleteAction.CreateAction(CurrCmdLine), TFarProcedure1(@DeleteAction));
end;

procedure DeleteLauncher;
begin
  CurrCmdLine.DoLauncher(TDeleteLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@DeleteLauncher));
end;

procedure UpdateLauncher;
begin
  CurrCmdLine.DoLauncher(TUpdateLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@UpdateLauncher));
end;

procedure LayerChangeLauncher;
begin
  CurrCmdLine.DoLauncher(TLayerChangeLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@LayerChangeLauncher));
end;

procedure LinesFromPointsAction;
begin
   CurrCmdLine.DoCommandEx(TLinesFromPointsAction.CreateAction(CurrCmdLine), TFarProcedure1(@LinesFromPointsAction));
end;

procedure CircleAction;
begin
   CurrCmdLine.DoCommandEx(TCircleAction.CreateAction(CurrCmdLine), TFarProcedure1(@CircleAction));
end;
procedure PointAction;
begin
   CurrCmdLine.DoCommandEx(TPointAction.CreateAction(CurrCmdLine), TFarProcedure1(@PointAction));
end;

procedure SetTextUpper;
begin
  CurrCmdLine.DoLauncher(TSetTextUpperOrLowerLauncher.CreateLauncher(CurrCmdLine, True), TFarProcedure1(@SetTextUpper));
end;

procedure SetTextLower;
begin
  CurrCmdLine.DoLauncher(TSetTextUpperorLowerLauncher.CreateLauncher(CurrCmdLine, False), TFarProcedure1(@SetTextLower));
end;

procedure SetTextFirstWordUpper;
begin
  CurrCmdLine.DoLauncher(TSetTextFirstWordUpperLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextFirstWordUpper));
end;

procedure SetTextSpacingBetweenWord;
begin
  CurrCmdLine.DoLauncher(TSetTextSpacingBetweenWordLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextSpacingBetweenWord));
end;

procedure SetTextSeparate;
begin
  CurrCmdLine.DoCommandEx(TSetTextSeparateAction.CreateAction(CurrCmdLine), TFarProcedure1(@SetTextSeparate));
//  CurrCmdLine.DoLauncher(TSetTextSeparateLauncher.CreateLauncher(CurrCmdLine, False), TFarProcedure1(@SetTextSeparate));
end;

procedure SetTextRoundingToDecimal;
begin
  CurrCmdLine.DoLauncher(TSetTextRoundingToDecimalLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextRoundingToDecimal));
end;

procedure SetTextMathOperations;
begin
  CurrCmdLine.DoLauncher(TSetTextMathOperationsLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextMathOperations));
end;

procedure SetTextQuickText;
begin
  CurrCmdLine.DoCommandEx(TSetTextQuickTextAction.CreateAction(CurrCmdLine), TFarProcedure1(@SetTextQuickText));
end;

procedure PointFromText;
begin
  CurrCmdLine.DoLauncher(TPointFromTextLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@PointFromText));
end;


{procedure SetText;
begin
  CurrCmdLine.DoCommandEx(TSetTextAction.CreateAction(CurrCmdLine), TFarProcedure1(@SetText));
end;}

Procedure AddLineType;
var
  Count, I, Rc: integer;
  Uzunluk: Double;
  Satir: Double;
  BoslukYazi: Double;
  Text, Cizgi: IlicgEntity;
  FirstX, FirstY: Double;
  Ext: TlicgExtent;
begin
  Count := 117;
  Uzunluk := 100;
  FirstX := 0;
  FirstY := 0;
  Satir := 30;
  BoslukYazi := 20;
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
  Text := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  Cizgi := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _3D);
  for I := 0 to Count-1 do
  begin
    Cizgi.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Cizgi.Geometry.Points[1] := AsCoor(Uzunluk , FirstY);
    Cizgi.DrawTools.PenTool.Style := I;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Cizgi, False);
    if I = 0 then
      Ext := Cizgi.Extent;
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
    Text.Geometry.Points[0] := AsCoor(Uzunluk + BoslukYazi, FirstY);
    Text.AsText.Text := CurrCmdLine.ActiveDrawBox.GIS.LineTypes.Items[I].Name;
    Text.DrawTools.FontTool.TextPos := tpCenterLeftOut;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Text);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
    FirstY := FirstY - Satir;
  end;
  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  Text.Geometry.UpdateExtension;
  CalcMaxMinBounds(Ext, Text.Extent);
  CurrCmdLine.ActiveDrawBox.RepaintExtent(Ext);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Ext);
  Text := Nil;
  Cizgi := Nil;

end;

Procedure AddSymbols;
var
  I, Rc: integer;
  Symbol: ILicgEntity;
  ProjectScale: Double;
  FirstX, FirstY: DOuble;
  bosluk, Satir: Double;
  height :  Double;
begin
  FirstX := 0;
  FirstY := 0;
  Bosluk := 30;
  Satir := 30;
  ProjectScale := CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
  Symbol := Licad.CreateEntityFactory.MakeEntity(idPlace, 0, _2D);
  //Symbol.DrawTools.SymbolTool.Index := 1;
 //  with CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Items[1].Extension do    //Davulcu Deðiþtiröe
//   Height := ProjectScale * (UpperRight.Y - LowerLeft.Y); //Davulcu Deðiþtirme

  for I := 1 to 254 do
  begin
    Symbol.DrawTools.SymbolTool.Index := I;
    with CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Items[I].Extension do
      Height := ProjectScale * (UpperRight.Y - LowerLeft.Y);

    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Symbol.DrawTools.SymbolTool.Height := Height/2;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := FirstX + Bosluk;
    Symbol.DrawTools.SymbolTool.Height := height;
    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := FirstX + Bosluk;
    Symbol.DrawTools.SymbolTool.Height := height*2;
    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := FirstX + Bosluk;
    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Symbol.DrawTools.SymbolTool.Height := Height/2;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := FirstX + Bosluk;
    Symbol.DrawTools.SymbolTool.Height := height;
    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := FirstX + Bosluk;
    Symbol.DrawTools.SymbolTool.Height := height*2;
    Symbol.Geometry.Points[0] := AsCoor(FirstX , FirstY);
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Symbol, False);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);

    FirstX := 0;
    FirstY := FirstY - Satir;
  end;
  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  CurrCmdLine.ActiveDrawBox.Repaint;
//  CurrCmdLine.ActiveDrawBox.ZoomWindow(Symbol.Extent);
end;


procedure DrawThickness;
var
  Count, I, J, Rc: integer;
  Uzunluk: Double;
  Satir: Double;
  BoslukYazi: Double;
  Text, Cizgi: IlicgEntity;
  FirstX, Fy, Fx, FirstY: Double;
  Ext: TlicgExtent;
  Layer: TLicgBaseLayer;
begin
  Count := 117;
  Uzunluk := 100;
  FirstX := 0;
  FirstY := 0;
  Fy := 0;
  Fy := 0;
  Satir := 30;
  BoslukYazi := 20;
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
  Text := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  Cizgi := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _3D);
  for J := 0 to 5 do
  begin
    Text.Geometry.Points[0] := AsCoor(Fx, Fy+30);
    Fx := Fx + Uzunluk + BoslukYazi;
    Text.AsText.Text := IntToStr(J*3) + '. Kalem No';
    Text.DrawTools.FontTool.TextPos := tpCenterLeftOut;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Text);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
    for I := 0 to Count-1 do
    begin
      Cizgi.Geometry.Points[0] := AsCoor(FirstX , FirstY);
      Cizgi.Geometry.Points[1] := AsCoor(FirstX+Uzunluk , FirstY);
      Cizgi.DrawTools.PenTool.Style := I;
      {Cizgi.Geometry.Points[0] := AsCoor(FirstX , FirstY);
      Cizgi.Geometry.Points[1] := AsCoor(Uzunluk , FirstY); }
      Layer := AddLayer(IntToStr(J*3), clSkyBlue, 1);
      Rc := CurrCmdLine.ActiveDrawBox.GIS.Layers[J].AddEntity(Cizgi, False);
      CurrCmdLine.ActiveDrawBox.GIS.Layers[J].LayerInfo.PenNo := J*3;
      if J = 0 then
      begin
        Ext := Cizgi.Extent;
        Text.Geometry.Points[0] := AsCoor(-Uzunluk/2 + BoslukYazi, FirstY);
        Text.AsText.Text := CurrCmdLine.ActiveDrawBox.GIS.LineTypes.Items[I].Name;
        Text.DrawTools.FontTool.TextPos := tpCenterRightOut;
        Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Text);
      end;
    //CurrCmdLine.ActiveDrawBox.GIS.Layers[1].AddEntity(Cizgi, False);
    //CurrCmdLine.ActiveDrawBox.GIS.Layers[2].AddEntity(Cizgi, False);
      CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
      FirstY := FirstY - Satir;
    end;
    FirstY := 0;
    FirstX := FirstX + Uzunluk + BoslukYazi;
  end;
  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  Text.Geometry.UpdateExtension;
  CalcMaxMinBounds(Ext, Text.Extent);
  CurrCmdLine.ActiveDrawBox.RepaintExtent(Ext);
  CurrCmdLine.ActiveDrawBox.ZoomWindow(Ext);
  Text := Nil;
  Cizgi := Nil;
end;

Procedure QuickText;
begin
  fmHizliYaziDüzenleme := TfmHizliYaziDüzenleme.Create(nil);
  fmHizliYaziDüzenleme.ShowModal;
  if fmHizliYaziDüzenleme.ModalResult = mrOk then
  begin
    if fmHizliYaziDüzenleme.dxListViewControl1.Items[0].Selected then
      SetTextUpper
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[1].Selected then
      SetTextLower
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[2].Selected then
      SetTextFirstWordUpper
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[3].Selected then
      SetTextSpacingBetweenWord
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[4].Selected then
      SetTextSeparate
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[5].Selected then
      SetTextQuickText
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[6].Selected then
      SetTextRoundingToDecimal
    else if fmHizliYaziDüzenleme.dxListViewControl1.Items[7].Selected then
      SetTextMathOperations
  end
  else
  exit;
end;



procedure RibbonExecute(Command: Integer); stdcall;
begin
  case Command of
    1: PointAction;
    2: LineAction;
    3: PoligonAction;
    4: AddPolygon;
//  5: AddArc;
    6: CircleAction;
//  7: AddRectangle;
//  8: AddEllips;
    9: WriteAction;
// 10: AddSymbol;
   11: AddNoktaForm;
   12: AddDortgenForm;
   13: AddYaziForm;
   14: AddLayers;
   15: DeleteAction;     //nesne eklemediðimiz için baþýna add yapmýyoruz.
   16: DeleteLauncher;
   17: UpdateLauncher;
   18: LayerChangeLauncher;
   19: LinesFromPointsAction;
   70: SetTextLower;
   71: SetTextUpper;
   72: SetTextFirstWordUpper;
   73: SetTextSpacingBetweenWord;
   74: SetTextSeparate;
   75: SetTextRoundingToDecimal;
   76: SetTextMathOperations;
   77: SetTextQuickText;
   78: PointFromText;
   79: QuickText;
  end;
end;

procedure RibbonAdd; stdcall;
begin
  with LicadRibbon do
  begin
    Images := dmCom.SmallImages;
    LargeImages := dmCom.LargeImages;
      AddCommand('LiWork\Çizim Araçlarý\Ardýþýl Nokta ', '', 'Ardýþýl Nokta atma iþlemi',
      @RibbonExecute, 1, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Çizim Araçlarý\Çizgi', '', 'Çizgi Ekleme Bilgisi ',
      @RibbonExecute, 2, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Çizim Araçlarý\Çoklu Doðru', '', 'Çoklu Doðru Ekleme',
      @RibbonExecute, 3, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Çizim Araçlarý\Kapalý Alan ', '', 'Kapalý Alan Ekleme',
      @RibbonExecute, 4, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    {AddCommand('LiWork\Çizim Araçlarý\Yay', '', 'Yay Ekleme',
      @RibbonExecute, 5, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);}
    AddCommand('LiWork\Çizim Araçlarý\Daire ', '', 'Daire oluþturma Ýþlemi',
      @RibbonExecute, 6, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    {AddCommand('LiWork\Çizim Araçlarý\Kutu', '', 'Kutu Ekleme',
      @RibbonExecute, 7, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);}
    {AddCommand('LiWork\Çizim Araçlarý\Elips', '', 'Elips Ekleme',
      @RibbonExecute, 8, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0); }
    AddCommand('LiWork\Çizim Araçlarý\Yazý ', '', 'Yazý Ekleme',
      @RibbonExecute, 9, 0, False, @RibbonEnable, bisButton, nil, True, 0);
    AddCommand('LiWork\Çizim Araçlarý\Sembol ', '', 'Nokta Ekleme',
      @RibbonExecute, 10, 0, False, @RibbonEnable, bisButton, nil, True, 0);
    {AddCommand('LiWork\Formdan Nesne Ekleme \Çizgi', '', 'Çizgi Ekleme Bilgi',
      @RibbonExecute, 11, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0); }
    AddCommand('LiWork\Formdan Nesne Ekleme \Nokta', '', 'Nokta Ekleme Bilgi',
      @RibbonExecute, 11, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Formdan Nesne Ekleme \Dörtgen', '', 'Dörtgen Oluþturma Bilgi',
      @RibbonExecute, 12, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Formdan Nesne Ekleme \Yazý', '', 'Yazý Ekleme Bilgi',
      @RibbonExecute, 13, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Formdan Katman Ekleme \Katman', '', 'Katman Ekleme Bilgi',
      @RibbonExecute, 14, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Nesne Sil \Hýzlý Sil', '', 'Silme Ýþlemi',
      @RibbonExecute, 15, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Nesne  \Sil', '', 'Silme Ýþlemi',
      @RibbonExecute, 16, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Nesne  \Güncelleme', '', 'Güncelleme Ýþlemi',
      @RibbonExecute, 17, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Katman Deðiþtirme \Katman Deðiþtirme', '', 'Katman Deðiþtirme Ýþlemi',
      @RibbonExecute, 18, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Noktadan Nesne \Noktadan Nesne ', '', 'Noktadan Nesne Üretme Ýþlemi',
      @RibbonExecute, 19, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);

    AddCommand('LiWork\Yazý Edit\Yazý Edit Ýþlemleri', '', '',
      @RibbonExecute, 0, 0, True, @RibbonUpdate, bisLargeListStart, nil, True, 0);
    AddCommand('LiWork\Yazý edit\Hepsi Büyük Harf', '', '',
      @RibbonExecute, 70, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Hepsi Küçük Harf', '', '',
      @RibbonExecute, 71, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Baþ Harfler Büyük', '', '',
      @RibbonExecute, 72, 0, False, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Harfler Arasýna Boþluk', '', '',
      @RibbonExecute, 73, 0, False, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Yazýyý Ayýr', '', '',
      @RibbonExecute, 74, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Yuvarlama', '', '',
      @RibbonExecute, 75, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Matematik', '', '',
      @RibbonExecute, 76, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Hazýr Yazý', '', '',
      @RibbonExecute, 77, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý edit\Yazýdan Nokta', '', '',
      @RibbonExecute, 78, 0, True, @RibbonEnable, bisList, nil, True, -1);
    AddCommand('LiWork\Yazý \Hýzlý Yazý Düzenleme', '', 'Hýzlý Yazý Düzenleme Ýþlemi',
      @RibbonExecute, 79, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
  end;
end;

{ TMyEvents }

class procedure TMyEvents.MyButtonClick(Sender: TObject);
begin

end;

initialization

finalization

end.


