unit Lider.CG.ModulesCom.LauncherIfraz;

interface

uses
  Windows, SysUtils, Types, Graphics, Controls, Forms, Math, DB, IniFiles,
  Dialogs, IOUtils,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.Types,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Math,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Lib,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.System,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.ModulesCom.Ifraz,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.Rtree;

type
  TIfrazLauncher = class(TlicgLauncher)
  private
    UygulamaYolu: string;
    KalanObj: IlicgEntity;
    OncekiIfrazYontemi: Integer;
    HatObje: IlicgEntity;
    FTrackedEntity : IlicgEntity;
    FTrackedLineEntity : IlicgEntity;
    FBoolDestroy: Boolean;
    FBoolTrackedPoint: Boolean;
    FIsEndex: Boolean;

    ATop: IlicgTopology;
//    FBoolEscPress: Boolean;

    procedure IfrazFormCreate;
    procedure IfrazFormAyarlariniKaydet;
    procedure IfrazFormClose;
    procedure IfrazbtnTamamClick;
    procedure actTamamExecute(Sender: TObject);
    procedure actIptalExecute(Sender: TObject);
//    procedure IfrazActIptalExecute(Sender: TObject);
//    procedure IfrazFormShow(Sender: TObject);
//    function HatObjeSec(ASecimMesaj: String):IlicgEntity;

    function BolunecekPolygonSec: IlicgEntity;
    procedure LauncherTrackedEntityClickedPoly(const TrackID: string; var TrackedEntity: IlicgEntity);
    procedure LauncherFinishedPoly(Sender: TObject);
    function BolecekNoktaSec(const prompt: WideString): TlicgCoor;
    procedure MouseUpPoint(const TrackID: string; var TrackedEntity: IlicgEntity);
    procedure LauncherFinishedPoint(Sender: TObject);
    function SelectLine: IlicgEntity;
    procedure LauncherTrackedEntityLine(const TrackID: string; var TrackedEntity: IlicgEntity);
    procedure LauncherFinishedLine(Sender: TObject);

    function HatObjeSec2(ASecimMesaj: String):IlicgEntity;

    procedure NoktalariDuzenle(Nokta1, Nokta2: TlicgCoor; out YeniNokta1, YeniNokta2: TlicgCoor);
    function TabakaDurumu: Boolean;

    function YeniIfraz(BolunecekDegerSayisi:Double; AlanYonu, IfrazYontem:Integer; PolySecim:IlicgEntity; DogruP1,DogruP2:TlicgCoor; Cephe: Boolean):IlicgEntity;
    procedure ComplexPolyToSplitPoly(cpp: IlicgEntity; TabakaAdi: String);
    procedure ComplexPolyToSplitPolyShow(cp: IlicgEntity);
    function ComplexEndeksHesaplaComplexPoly(Acp: IlicgEntity; EndeksTabakAdi: string; out Alan, Endeks, DegerSayisi: Double): Boolean;

    procedure LauncherFinished(Sender: TObject);
    function PolyClip(AClipType: TlicgClipType; SubEnt, PolyEnt: IlicgEntity; var resultcp: IlicgEntity): Boolean;
  public
    FLauncherProc: TFarProcedure1;
//    procedure actTamamExecute(Sender: TObject);
    Launcher2: IlicgActionTracker;
    function GetSavingLayer(PLayerName: string; PColor: TColor = clBlue): TlicgBaseLayer;
    function SelectPoint(const prompt: WideString; var c: TlicgCoor; acursor: Integer): WordBool;

//    procedure EscPressed(Sender: TObject; var Key: Word; Shift: TShiftState);

    function DikdortgenOlustur(Poly:IlicgEntity;DogruP1,DogruP2:TlicgCoor): IlicgEntity;

    constructor CreateLauncher(ACmdLine: TlicgBaseCmdLine; AIsEndex: Boolean = False; ALayerTable: TDataset = nil);
    destructor Destroy; override;
  end;

var
  DecimalOfCoordinate: Integer;

  GenelParselNo: string = '1';
  TABAKA_Ada: string =  'ADA';
  TABAKA_AdaHat: string = 'ADA_HAT';
  TABAKA_AdaHat_Color: Integer= clYellow;
  TABAKA_Parsel: string= '#PARSEL';
  TABAKA_ParselAln: string= 'PARSEL_ALN';
  TABAKA_ParselNo: string= 'PARSEL_NO';
  TABAKA_Endeks: string= 'EN_1_00';

implementation

uses
  lxStrUtil, LicadUtil,
  Lider.CG.ModulesCom.ParalelIfrazParselAlaniGirisi,
  Lider.CG.ModulesCom.DikIfrazParselAlaniGirisi,
  Lider.CG.ModulesCom.CepheAciUzunluguGirisi,
  Lider.CG.ModulesCom.ParalelMesafeGirisi,
  Lider.CG.ModulesCom.ParselNoGirisi,
  Lider.CG.ModulesCom.SabitNoktaParselAlaniGirisi;

{ TIfrazLauncher }

procedure TIfrazLauncher.actTamamExecute(Sender: TObject);
begin
  fmIfraz.Hide;
  IfrazbtnTamamClick;
  fmIfraz.Show;
end;

procedure TIfrazLauncher.actIptalExecute(Sender: TObject);
begin
//  SendMessage(Self.Handle, WM_SYSCOMMAND, SC_CLOSE, 0 );
  //PostMessage(Self.Handle,wm_close,0,0);
  {FormAyarlariniKaydet;
  FormStyle := fsNormal;
  Hide;
  fmBYAna.Show;}
  CmdLine.Tag := 0;
  IfrazFormClose;
  FBoolDestroy := True;
  fmIfraz.Close;
//  Launcher.Finished := True;
//  Launcher.Finish;
//  LauncherFinished(nil);
end;

constructor TIfrazLauncher.CreateLauncher(ACmdLine: TlicgBaseCmdLine; AIsEndex: Boolean; ALayerTable: TDataset);
begin
  if ACmdLine.Tag = 1325 then
    Exit;

  inherited CreateLauncher(ACmdLine);
  FIsEndex := AIsEndex;
  CmdLine.Tag := 1325;
  UygulamaYolu := Licad.Path.GetModules + 'LiMap\';

  if (FIsEndex) and (ALayerTable <> nil) then //LiTop Tabaka Yapýsýna Göre
  begin
    UygulamaYolu := Licad.Path.GetModules + 'LiTop\';

    TABAKA_Ada := ALayerTable.FieldByName('BLOK').AsString; //'ADA';
    TABAKA_AdaHat := ALayerTable.FieldByName('BLOKHAT').AsString; // 'ADA_HAT';
    if CmdLine.ActiveDrawBox.GIS.Layers.IndexOfName(TABAKA_AdaHat) > -1 then
      TABAKA_AdaHat_Color := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).LayerInfo.Color // clYellow;
    else
      TABAKA_AdaHat_Color := clYellow;
    TABAKA_Parsel := ALayerTable.FieldByName('PARSEL').AsString; //'#PARSEL';
    TABAKA_ParselAln := ALayerTable.FieldByName('PARSELALN').AsString; //'PARSEL_ALN';
    TABAKA_ParselNo := ALayerTable.FieldByName('PARSELNO').AsString; //'PARSEL_NO';
    TABAKA_Endeks := ALayerTable.FieldByName('EN100').AsString; //'EN_1_00';

    if Pos('@', TABAKA_Parsel) = 1 then
      TABAKA_Parsel := AnsiReplaceStr(TABAKA_Parsel, '@', '#');
  end
  else if (not FIsEndex) and (ALayerTable <> nil) then //LiMar Tabaka Yapýsýna Göre
  begin
    UygulamaYolu := Licad.Path.GetModules + 'LiMar\';

    TABAKA_Ada := ALayerTable.FieldByName('ADA').AsString; //'ADA';
    TABAKA_AdaHat := ALayerTable.FieldByName('ADAHAT').AsString; // 'ADA_HAT';
    if CmdLine.ActiveDrawBox.GIS.Layers.IndexOfName(TABAKA_AdaHat) > -1 then
      TABAKA_AdaHat_Color := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).LayerInfo.Color // clYellow;
    else
      TABAKA_AdaHat_Color := clYellow;
    TABAKA_Parsel := ALayerTable.FieldByName('PARSEL').AsString; //'#PARSEL';
    TABAKA_ParselAln := ALayerTable.FieldByName('PARSELALN').AsString; //'PARSEL_ALN';
    TABAKA_ParselNo := ALayerTable.FieldByName('PARSELNO').AsString; //'PARSEL_NO';

    if Pos('@', TABAKA_Parsel) = 1 then
      TABAKA_Parsel := AnsiReplaceStr(TABAKA_Parsel, '@', '#');
  end;

  DecimalOfCoordinate := -8; //(-1*(Integer(Licad.Settings.Precision.Coordinate)+1))
  FBoolDestroy := False;
  FBoolTrackedPoint := False;

  Launcher.Caption:= 'Ýfraz Yöntemleri';
  Launcher.OnFinished := LauncherFinished;
  fmIfraz := TfmIfraz.Create(Application.MainForm);

  ATop := Licad.CreateTopology;
//  FBoolEscPress := False;
//  Launcher.OnKeyDown := EscPressed;

//  fmIfraz.OnCreate  := IfrazFormCreate;
//  fmIfraz.OnClose := IfrazFormClose;
//  fmIfraz.OnShow := IfrazFormShow;
//  fmIfraz.btnTamamClick := IfrazbtnTamamClick;
//  fmIfraz.actIptalExecute := IfrazActIptalExecute;

  with fmIfraz do
  begin
  //    LockWindowUpdate(fmIfraz.Handle);
  //    LockWindowUpdate(0);
  //    FAction := CurrCmdLine.CurrentAction;
    ActionList1[0].OnExecute := actIptalExecute;
    ActionList1[1].OnExecute := actTamamExecute;
    IfrazFormCreate;

    if (FIsEndex) and (ALayerTable <> nil) then //LiTop Tabaka Yapýsýna Göre
      fmIfraz.Caption := 'Endekslere Göre Ýfraz'
    else if (not FIsEndex) and (ALayerTable <> nil) then //LiMar Tabaka Yapýsýna Göre
      fmIfraz.Caption := 'Ýmar Ýfraz';

    Show;
  end;
    {
    repeat
      ShowModal;
      if ModalResult = MrOk then //IfrazbtnTamamClick; BolunecekPolygonSec
        IfrazbtnTamamClick;
      Close;
    until ModalResult <> MrOk;

    if ModalResult <> MrOk then
    begin
      IfrazFormClose;

//      FormHider1.Free;
      FBoolDestroy := True;

//      CurrCmdLine.LastCommand := SCmdLauncher;
//      Launcher.TrackQuickSelect(SCmdLauncher);
      Launcher.Finished := True;
      Launcher.Finish;
      LauncherFinished(nil);
//      Launcher.CurrentAction.Destroy;
//      Self.Free;
    end;
  end;
//  FreeAndNil(fmIfraz);
    }
end;

//procedure TIfrazLauncher.actTamamExecute(Sender: TObject);
//begin
//  IfrazbtnTamamClick;
//end;

destructor TIfrazLauncher.Destroy;
begin
  CmdLine.Tag := 0;
  if FBoolDestroy then
    inherited Destroy;
end;

procedure TIfrazLauncher.LauncherFinished(Sender: TObject);
begin
  CmdLine.Tag := 0;
  if Launcher.Finished then
    Self.Free;
end;

//Harici Func ve Proc BAÞLANGIÇ/////////////////////////////////////////////////
{$region 'Harici Func ve Proc'}

function LayerIsVisibleIsLock(LayerName: String; isNilControl: Boolean=True): Boolean;
var BLayer: TlicgBaseLayer;
begin
  Result := True;
  BLayer := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(LayerName);
  if (isNilControl) and (BLayer = nil) then
  begin
    Result := False;
    Exit;
  end;
//  if (BLayer.LayerInfo.Visible) and (BLayer.LayerInfo.Selectable) then
//    Result := True;

  if LayerName = TABAKA_Endeks then
    Exit;

  if not BLayer.LayerInfo.Visible then
    BLayer.LayerInfo.Visible := True;
  if not BLayer.LayerInfo.Selectable then
    BLayer.LayerInfo.Selectable := True;
end;

function Intersect(Vect1, Vect2, ResultIntersectVector: IlicgVector): Boolean;
var
  X1, Y1, X2, Y2, X11, Y11, X22, Y22: Double;
  Alfa1, alfa2, SinAlfa1, SinAlfa2, CosAlfa1, CosAlfa2: Double;
  IPoint: TlicgCoor;
  ParalelUcUca: Boolean;
  UstUste: Boolean;
begin
  Result := True;
  ParalelUcUca := False;
  ResultIntersectVector.Clear;
  X1 := Vect1[0].y;
  Y1 := Vect1[0].x;
  X11 := Vect1[1].y;
  Y11 := Vect1[1].x;

  X2 := Vect2[0].y;
  Y2 := Vect2[0].x;
  X22 := Vect2[1].y;
  Y22 := Vect2[1].x;

  Alfa1 := Angle2DS(AsCoor(Y1, X1), AsCoor(Y11, X11));
  Alfa2 := Angle2DS(AsCoor(Y2, X2), AsCoor(Y22, X22));
  Alfa1 := Roundto(Alfa1, -8);
  Alfa2 := Roundto(Alfa2, -8);
  ParalelUcUca := (((X11 = X2) and (Y11 = Y2)) or ((X1 = X2) and (Y1 = Y2)) or ((X1
    = X22) and (Y1 = Y22)) or ((X11 = X22) and (Y11 = Y22)));
  UstUste := (((X1 = X2) and (Y1 = Y2)) and ((X11 = X22) and (Y11 = Y22))) or (((X1
    = X22) and (Y1 = Y22)) and ((X11 = X2) and (Y11 = Y2)));
  if UstUste then
  begin
    Result := False;
    Exit; //Hatlar Üst üste
  end;
  if ((Alfa1 = Alfa2) or (Alfa1 = Alfa2 + (2 * pi)) or (Alfa1 = Alfa2 - (2 * pi)))
    and (not ParalelucUca) then
  begin

    Result := False;
    Exit; //Paralel Hatlar
  end;

  if ParalelUcUca and ((Alfa1 = Alfa2) or (Alfa1 = Alfa2 + (2 * pi)) or (Alfa1 =
    Alfa2 - (2 * pi))) then
  begin
    if ((X11 = X2) and (Y11 = Y2)) then
      ResultIntersectVector.Add(y11, X11)
    else if ((X1 = X2) and (Y1 = Y2)) then
      ResultIntersectVector.Add(Y1, X1)
    else if ((X1 = X22) and (Y1 = Y22)) then
      ResultIntersectVector.Add(Y1, X1)
    else if ((X11 = X22) and (Y11 = Y22)) then
      ResultIntersectVector.Add(Y11, X11);
    Result := True;
    Exit;
  end;

  SinAlfa1 := Sin(Alfa1);
  CosAlfa1 := Cos(Alfa1);
  SinAlfa2 := Sin(Alfa2);
  CosAlfa2 := Cos(Alfa2);

  IPoint.x := (((SinAlfa1 * SinAlfa2) * (X2 - X1)) + (Y1 * SinAlfa2 * CosAlfa1)
    - (Y2 * SinAlfa1 * CosAlfa2)) / ((CosAlfa1 * SinAlfa2) - (SinAlfa1 * CosAlfa2));
  if (Roundto(SinAlfa1, -10) <> 0) and (Roundto(SinAlfa2, -10) <> 0) then
    IPoint.y := ((IPoint.x * CosAlfa1) - (Y1 * CosAlfa1) + (X1 * SinAlfa1)) / SinAlfa1
  else
  begin
    if Roundto(SinAlfa2, -10) = 0 then
    begin
      IPoint.y := ((IPoint.x * CosAlfa1) - (Y1 * CosAlfa1) + (X1 * SinAlfa1)) / SinAlfa1;
    end;
    if Roundto(SinAlfa1, -10) = 0 then
    begin
      IPoint.y := ((IPoint.x * CosAlfa2) - (Y2 * CosAlfa2) + (X2 * SinAlfa2)) / SinAlfa2;
    end;
  end;
  ResultIntersectVector.Add(IPoint.x, IPoint.y);
end;

function intersection(var cX: TlicgCoor; const a1: TlicgCoor; const a2: TlicgCoor; const b1: TlicgCoor;
                      const b2: TlicgCoor): WordBool;
var
  _VectorR, _Vector1, _Vector2: IlicgVector;
begin
  _VectorR := Licad.CreateEntityFactory.MakeVector(3, 0);
  _Vector1 := Licad.CreateEntityFactory.MakeVector(3, 0);
  _Vector2 := Licad.CreateEntityFactory.MakeVector(3, 0);

  _Vector1.Add(a1.X,a1.Y);
  _Vector1.Add(a2.X,a2.Y);
  _Vector2.Add(b1.X,b1.Y);
  _Vector2.Add(b2.X,b2.Y);

  Intersect(_Vector1, _Vector2, _VectorR);
  cX := _VectorR[0];

//  _Intersect(a1,a2,b1,b2,cx);

  _VectorR := nil;
  _Vector1 := nil;
  _Vector2 := nil;

  Result := True;
end;

procedure JoinEnt(AEnt: IlicgEntity; ALayerName: string = '';hasUndo:Boolean = True);
var
  Rc: Integer;
  ALayer: TlicgBaseLayer;
begin
  if AEnt = nil then
    Exit;
  with CurrCmdLine.ActiveDrawBox do
  begin
    ALayer := GIS.Layers.LayerByName(ALayerName);
    if ALayer = nil then
    begin
      ALayer := GIS.CurrentLayer;
      ALayerName := ALayer.DisplayName;
    end;

    if (not LayerIsVisibleIsLock(ALayerName)) then
      Exit;

    if hasUndo then
      Undo.BeginUndo(uaDelete);

    Rc := ALayer.AddEntity(AEnt,True,True);
    RepaintExtent(AEnt.Geometry.Extent);

    if hasUndo then
    begin
      Undo.AddUndo(ALayer, Rc, uaDelete);
      Undo.EndUndo;
    end;
  end;
end;

function PolyOnMe(MiddlePoint: TlicgCoor; PPolyEnt: IlicgEntity): Boolean;
var
  FStackedSelList: IPickedList;
  tmpAlayer: TlicgBaseLayer;
  AEnt: IlicgEntity;
  PickedPoint, tmpARecno, I: Integer;
  resultBool, Picked: Boolean;
begin
  resultBool := False;
  Result := resultBool;
  FStackedSelList := TPickedList.Create;
  Picked := CurrCmdLine.ActiveDrawBox.PickEntity(MiddlePoint.x, MiddlePoint.y, 4, '', TmpALayer, TmpARecNo, PickedPoint, FStackedSelList);
  if Picked then
  begin
    if FStackedSelList.Count > 0 then
    begin
      for I := 0 to FStackedSelList.Count - 1 do
      begin
        TmpALayer := FStackedSelList.PickedItems[I].Pick.Layer;

        if (not LayerIsVisibleIsLock(TmpALayer.DisplayName)) then Continue;

        TmpARecno := Longint(FStackedSelList.PickedItems[I].Pick.RecNo);
        AEnt := TmpALayer.LoadEntityWithRecNo(TmpARecno);
        if (AEnt.EntityID in [idPolygon]) and
           (AEnt.Geometry.Points.IsEqual(PPolyEnt.Geometry.Points)) and
           (AEnt.Name = GenelParselNo) then
        begin
          resultBool := True;
          Break;
        end;
      end;
    end;
  end;
  Result := resultBool;
end;

function hasPointEntPart(Ent: IlicgEntity; Point: TlicgCoor): Boolean;
var I,J: Integer;
  TempEnt: IlicgEntity;
begin
  Result := False;
  TempEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
  for I := 0 to Ent.Geometry.Points.PartCount - 1 do
  begin
    TempEnt.Geometry.Points.Clear;
    for J := Ent.Geometry.Points.PartStart(I) to Ent.Geometry.Points.PartEnd(I) do
    begin
      TempEnt.Geometry.Points.AddPoints([Ent.Geometry.Points[J]]);
    end;
    // and (TempEnt.IsPointInsideMe(Point2.X,Point2.Y))
   if (Ent.InPoly(TempEnt.Geometry.Points[0])) then
//    if (Ent.IsInsideEntity(TempEnt,True)) then //üstünde (f isPointOnMe) ve içindeyse (kendin) 1 er arttýr
    begin
      if onPoly(TempEnt, Point) then
      begin
        Result := False;
        Exit;
      end;
    end;
    TempEnt.Geometry.Points.Clear;
//      Inc(Count);
//    if Count > 1 then
//    Begin
//      Result := False;
//      Exit;
//    End;
  end;
//  if Count = 0 then
//    Result := True;
  Result := True;
end;

procedure AddCoor(var PEnt: IlicgEntity; PCoor: TlicgCoor);
begin
  PEnt.Geometry.Points.AddPoints([PCoor]);
end;

function FazlaNoktalariSil2(APline: IlicgEntity; AllDelete: Boolean = False): IlicgEntity; //Ard arda kopyalarý siler.
var
  I: Integer;
  resultEnt: IlicgEntity;
  DeletedCount: Integer;
begin
  DeletedCount := 0;
  resultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
  resultEnt.Assign(APline);
  if APline.Geometry.Points.Count >= 2 then
  for I := 0 to APline.Geometry.Points.Count - 2 do
  begin
    if (EqualPoint2D(APline.Geometry.Points[I],APline.Geometry.Points[I+1],DecimalOfCoordinate)) then
    begin
      if AllDelete then
      begin
        resultEnt.Geometry.Points.Delete(I-DeletedCount);
        Inc(DeletedCount);
      end;
      resultEnt.Geometry.Points.Delete(I-DeletedCount);
      Inc(DeletedCount);
    end;
  end;
  Result := resultEnt;
end;

{$region 'FazlaNoktalariSil'}
{
function FazlaNoktalariSilDublicated(APline: IlicgEntity; isDublate: Boolean = False): IlicgEntity;
var
  I, J: Integer;
  resultEnt: IlicgEntity;
  DeletedCount: Integer;
begin
  DeletedCount := 0;

  resultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;

  resultEnt.Assign(APline);

  if APline.Geometry.Points.Count > 1 then
  begin
    for I := 1 to APline.Geometry.Points.Count - 1 do
    begin
      for J := I+ 1 to APline.Geometry.Points.Count do
      begin
        if (EqualPoint2D(APline.Geometry.Points[I],APline.Geometry.Points[J],DecimalOfCoordinate)) then
        begin
          if isDublate then
          begin
            Result := nil;
            Exit;
          end;
          resultEnt.Geometry.Points.Delete(I-DeletedCount);
        end;
      end;
    end;
    Result := resultEnt;
  end
  else
    Result := APline;
end;
}
{
function FazlaNoktalariSil4(APline: IlicgEntity; APoline: IlicgEntity): IlicgEntity;
type
  TKoordinat = record
      x : double;
      y : double;
  end;
  TPline = Array of TKoordinat;

var
  Dizi1, Dizi2: TPline;
  c: TlicgCoor;
  Say, I, J: Integer;
  resultEnt: IlicgEntity;
begin
  SetLength(Dizi1, APline.Geometry.Points.Count);
  SetLength(Dizi2, APline.Geometry.Points.Count);
  resultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
  for I := 0 to APline.Geometry.Points.Count - 1 do
  begin
    Dizi1[I].y := APline.Geometry.Points[I].y;
    Dizi1[I].x := APline.Geometry.Points[I].x;
    Dizi2[I].y := APline.Geometry.Points[I].y;
    Dizi2[I].x := APline.Geometry.Points[I].x;
  end;
  Say := 0;
  for I := 0 to APline.Geometry.Points.Count - 1 do
  begin
    for J := Low(Dizi2) to High(Dizi2) do
    begin
      if (EqualPoint2D(APline.Geometry.Points[I],APoline.Geometry.Points[J], DecimalOfCoordinate)) then
      begin
        Say := Say + 1;
        if Say > 2 then
        begin
          Dizi2[I].x := -9901333671325;
          Break;
        end;
      end;
    end;
  end;

  for i := 0 to High(Dizi2) do
  begin
    if (Dizi2[i].x <> -9901333671325) and (Dizi2[i].y <> -9901333671325) then
    begin
      c := AsCoor(Dizi2[i].X, Dizi2[i].Y);
      AddCoor(resultEnt,c);
    end;
  end;
  Result := resultEnt;
end;
}
{$endregion}

function MidPointOf(c1,c2: TlicgCoor):TlicgCoor;
  var
    ResultC: TlicgCoor;
begin
  ResultC.X := (c1.X + c2.X) / 2;
  ResultC.Y := (c1.Y + c2.Y) / 2;
  Result := ResultC;
end;

function FazlaNoktalariSil1(APPoly: IlicgEntity; ACPoly: IlicgEntity; EXPoly:IlicgEntity): IlicgEntity;
// Kalan alanýn Kesilen alan noktalarýný belirli noktalar hariç siler.
var
  I, J, K: Integer;
  resultEnt: IlicgEntity;
  Flag: Boolean;
  DeletedCount: Integer;
begin
  DeletedCount := 0;
  Flag := True;

  resultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
  resultEnt.Assign(APPoly);

  for I := 0 to APPoly.Geometry.Points.Count - 1 do
  begin
    Flag := True;
    for J := 0 to EXPoly.Geometry.Points.Count - 1 do
    begin
      if (EqualPoint2D(APPoly.Geometry.Points[I],EXPoly.Geometry.Points[J],DecimalOfCoordinate)) then
      begin
        Flag := False;
        Break;
      end;
    end;
    if Flag then
    begin //1.0E-8
      if (ACPoly.InPoly(APPoly.Geometry.Points[i]))
      or (ACPoly.Geometry.Points.PointOnMe
      (APPoly.Geometry.Points.X[i], APPoly.Geometry.Points.Y[i]) > -1) or
         PolyOnMe(APPoly.Geometry.Points[i],ACPoly) then
      begin
        resultEnt.Geometry.Points.Delete(I-DeletedCount);
        Inc(DeletedCount);
        Continue;
      end;
      for K := 0 to ACPoly.Geometry.Points.Count - 1 do
      begin
        if (EqualPoint2D(APPoly.Geometry.Points[I],ACPoly.Geometry.Points[K],DecimalOfCoordinate)) then
        begin
          resultEnt.Geometry.Points.Delete(I-DeletedCount);
          Inc(DeletedCount);
          Break;
        end;
      end;
    end;

//    if ACPoly.Geometry.Points.PointExists(APPoly.Geometry.Points[I].X, APPoly.Geometry.Points[I].Y,0,0,(-1*(Integer(Licad.Settings.Precision.Coordinate)))-2) <= 0 then
//    begin
//      if EXPoly.Geometry.Points.PointExists(APPoly.Geometry.Points[I].X, APPoly.Geometry.Points[I].Y,0,0,(-1*(Integer(Licad.Settings.Precision.Coordinate)))-2) <= 0 then
//        Dizi2[I].x := -9901333671325;
//    end;
  end;

  Result := resultEnt;
end;

procedure Rotate_Around(Origin: TlicgCoor; var C: TLicgcoor; Angle:Double);
var
  xx, yy, CosT, Sint: Double;
Begin
  yy := C.Y - Origin.Y;
  xx := C.X - Origin.X; (* first shift origin to Origin *)
  SinCos(Angle, Sint, Cost); { cost:=cos(Angle); sint:=sin(Angle);}
  C.X := yy * sint + xx * cost;(* rotates counterclockwise *)
  C.Y := yy * cost - xx * sint;
  C.X := C.X + Origin.X; (* take origin back to (0,0) *)
  C.Y := C.Y + Origin.Y;
end;

procedure Polar(Origin: TlicgCoor; Angle, Dist: Double; var C: TlicgCoor);
begin
  C := Origin;
  C.Y := c.Y + dist;
  Rotate_Around(Origin, C , Angle);
end;

function GetParalel(pDist: Double; PLineEnt: IlicgEntity): IlicgEntity;
var
  Coor1, Coor2: TlicgCoor;
  ResultEnt: IlicgEntity;
begin
//  factor := 1;
//  if pDist < 0 then
//    factor := -1;
//
  ResultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
//  AngleLine := Angle2DS(PLineEnt.Geometry.Points[0],PLineEnt.Geometry.Points[1]);
//  Polar(PLineEnt.Geometry.Points[0],AngleLine+((pi/2)*factor),pDist,Coor1);
//  Polar(PLineEnt.Geometry.Points[1],AngleLine+((pi/2)*factor),pDist,Coor2);
//  ResultEnt.Geometry.Points[0] := Coor1;
//  ResultEnt.Geometry.Points[1] := Coor2;
//  Result := ResultEnt;
  if pDist = 0 then
    ResultEnt := PLineEnt
  else if pDist > 0 then
  begin
    Coor1.x := PLineEnt.Geometry.Points[0].x + ((Sin(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) - (pi / 2))) * pDist);
    Coor1.y := PLineEnt.Geometry.Points[0].y + ((Cos(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) - (pi / 2))) * pDist);

    AddCoor(ResultEnt,Coor1);
  //  VParalel.Add(P.x, P.y);

    Coor2.x := PLineEnt.Geometry.Points[1].x + ((Sin(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) - (pi / 2))) * pDist);
    Coor2.y := PLineEnt.Geometry.Points[1].y + ((Cos(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) - (pi / 2))) * pDist);

    AddCoor(ResultEnt,Coor2);
  //  VParalel.Add(P.x, P.y);
  end
  else
  begin
  //küçükse
    Coor1.x := PLineEnt.Geometry.Points[0].x + ((Sin(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) + (pi / 2))) * pDist);
    Coor1.y := PLineEnt.Geometry.Points[0].y + ((Cos(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) + (pi / 2))) * pDist);

    AddCoor(ResultEnt,Coor1);
  //  VParalel.Add(P.x, P.y);

    Coor2.x := PLineEnt.Geometry.Points[1].x + ((Sin(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) + (pi / 2))) * pDist);
    Coor2.y := PLineEnt.Geometry.Points[1].y + ((Cos(Angle2DS(PLineEnt.Geometry.Points[0],
      PLineEnt.Geometry.Points[1]) + (pi / 2))) * pDist);

    AddCoor(ResultEnt,Coor2);
  //  VParalel.Add(P.x, P.y);
  end;
  Result := ResultEnt;
end;

function TIfrazLauncher.GetSavingLayer(PLayerName: string; PColor: TColor = clBlue): TlicgBaseLayer;
var ResulLayer: TlicgBaseLayer;
begin
  ResulLayer := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(PLayerName);

  if (ResulLayer = nil) then
  begin
    ResulLayer := CurrCmdLine.ActiveDrawBox.GIS.CreateLayer(PLayerName, ltDesktop, PColor);
  end
  else
  begin
    ResulLayer := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer;
  end;
  Result := ResulLayer;
end;

procedure ObjeBombala(APoly: IlicgEntity; Tabaka:TlicgBaseLAyer);
var
  I, J, Rc, HatCount: Integer;
  TempLineEnt: IlicgEntity;
  TempCoor: TlicgCoor;
  isCClockWise: Boolean;
begin
  HatCount := 1;
  isCClockWise := False;
  if _IsCounterClockWise(APoly.Geometry.Points) then
    isCClockWise := True;

  TempLineEnt := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
  //Çevirde count "0" sýfýr geliyor.
//  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(UaDelete);//UaDelete Hata Verdiriyor. uaUnTransform

  if APoly.Geometry.Points.PartCount > 1 then
  begin
    for I := 0 to APoly.Geometry.Points.PartCount - 1 do
    begin
      for J := APoly.Geometry.Points.PartStart(I) to APoly.Geometry.Points.PartEnd(I) - 1 do
      begin
        TempLineEnt.Geometry.Points.Clear;
        TempLineEnt.Geometry.Points[0]:= APoly.Geometry.Points[J];
        TempLineEnt.Geometry.Points[1]:= APoly.Geometry.Points[J+1];

        if isCClockWise then
        begin
          TempCoor := TempLineEnt.Geometry.Points[0];
          TempLineEnt.Geometry.Points[0] := TempLineEnt.Geometry.Points[1];
          TempLineEnt.Geometry.Points[1] := TempCoor;
        end;

        TempLineEnt.Name := 'HAT' + '/' +  IntToStr(HatCount);
        Inc(HatCount);
        Rc := Tabaka.AddEntity(TempLineEnt);//AddObject(Obje);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(TempLineEnt.Geometry.Extent);
//        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(Tabaka, Rc, UaDelete);
      end;
    end;
  end
  else
  begin
    for I := 0 to APoly.Geometry.Points.Count - 1 do
    begin
      if I = (APoly.Geometry.Points.Count - 1) then
      begin
        TempLineEnt.Geometry.Points.Clear;
        TempLineEnt.Geometry.Points[0] := APoly.Geometry.Points[I];
        TempLineEnt.Geometry.Points[1] := APoly.Geometry.Points[0];
//        Obje := Licad.CreateEntityFactory.MakeLine(APoly.Geometry.Points[I], APoly.Geometry.Points[0]);//,TabakaNo, 0,0);
      end
      else
      begin
        TempLineEnt.Geometry.Points.Clear;
        TempLineEnt.Geometry.Points[0] := APoly.Geometry.Points[I];
        TempLineEnt.Geometry.Points[1] := APoly.Geometry.Points[I+1];
//        Obje := Licad.CreateEntityFactory.MakeLine(APoly.Geometry.Points[I], APoly.Geometry.Points[I+1]);//,TabakaNo, 0,0);
      end;

      if isCClockWise then
      begin
        TempCoor := TempLineEnt.Geometry.Points[0];
        TempLineEnt.Geometry.Points[0] := TempLineEnt.Geometry.Points[1];
        TempLineEnt.Geometry.Points[1] := TempCoor;
      end;

      TempLineEnt.Name := 'HAT' + '/' +  IntToStr(I+1);
      Rc := Tabaka.AddEntity(TempLineEnt);//AddObject(Obje);
      CurrCmdLine.ActiveDrawBox.RepaintExtent(TempLineEnt.Geometry.Extent);
//      CurrCmdLine.ActiveDrawBox.Undo.AddUndo(Tabaka, Rc, UaDelete);
    end;
  end;

//  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
end;

procedure KesebilecekNoktalariniBul(PCEnt: IlicgVector; PPolyEnt: IlicgEntity;
                                    var REnt: IlicgEntity);
var
  I, J, ResultIndex: Integer;
  MinDist, Dist: Double;
begin
  for I := 0 to PCEnt.Count - 1 do
  begin
    ResultIndex := 0;
    MinDist := _Distance(PPolyEnt.Geometry.Points[0],PCEnt[I]);
    for J := 0 to PPolyEnt.Geometry.Points.Count - 1 do
    begin
      Dist := _Distance(PPolyEnt.Geometry.Points[J],PCEnt[I]);
      if CompareValue(Dist,MinDist) = LessThanValue then
      begin
        ResultIndex := J;
        MinDist := Dist;
      end;
    end;
    AddCoor(REnt, PPolyEnt.Geometry.Points[ResultIndex]);
  end;
end;

procedure SpliceEntity(SEnt: IlicgEntity; var DEnt: IlicgEntity; PSecondIndex: Integer=0; PFirstIndex: Integer=0);
var I, J, FCount, SCount: Integer;
begin
  J := 0;
  if (PFirstIndex < 0) or (PSecondIndex < 0) then
    Exit;
  FCount := PFirstIndex;
  SCount := PSecondIndex;
  if PSecondIndex = 0 then
    SCount := SEnt.Geometry.Points.Count;

  for I := FCount to SCount do
  begin
    DEnt.Geometry.Points[J] := SEnt.Geometry.Points[I];
    Inc(J);
  end;
end;

{$endregion}
//Harici Func ve Proc BÝTÝÞ/////////////////////////////////////////////////////


//Dahili Func ve Proc BAÞLANGIÇ/////////////////////////////////////////////////
{$region 'Dahili Func ve Proc'}
//----------------------------------------------------------------------------//

{$region 'EscPressed'}
{
procedure TIfrazLauncher.EscPressed(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    FBoolEscPress := True;
    Exit;
  end;
  FBoolEscPress := False;
end;
}
{$endregion}

procedure TIfrazLauncher.IfrazFormCreate;
var
  Ini: TIniFile;
begin
with fmIfraz do
begin
  MClbIfrazYontemi.ItemIndex := 0;
  try
    FAction := CurrCmdLine.CurrentAction;
    if not TDirectory.Exists(UygulamaYolu + 'Settings') then
      TDirectory.CreateDirectory(UygulamaYolu + 'Settings');
    Ini := TIniFile.Create(UygulamaYolu + 'Settings\Ifraz.ini') ;
    MClbIfrazYontemi.ItemIndex := Ini.ReadInteger('IfrazAyarlari','IfrazYontemi', 0);
    rgYeniAlanYonu.ItemIndex := Ini.ReadInteger('KonumAyarlari','AlanYonu', 0);
    chbGeriKalanAlaniTekrarBolunecek.Checked := Ini.ReadBool('KonumAyarlari','GeriKalanAlanTekrarBoluncekmi', True);
    chbSonSecilenHattiKullan.Checked := Ini.ReadBool('KonumAyarlari','SonSecilenHattiKullan', True);
    chkParselNoYaz.Checked := Ini.ReadBool('KonumAyarlari','ParselNoYaz', True);
    chkAdaNoYaz.Checked := Ini.ReadBool('KonumAyarlari','ParselNoIleAdaNoyudaYaz', False);
    seYaziBoyu.EditText := Ini.ReadString('KonumAyarlari','YaziBoyutu', '2');
    chkParselinOrtaNoktasinaYaz.Checked := Ini.ReadBool('KonumAyarlari','ParselinOrtaNoktasinaYaz', True);
    GenelParselNo := Ini.ReadString('KonumAyarlari','GenelParselNo', '0/0');
  finally
    Ini.Free;
  end;

  GetSavingLayer(TABAKA_Parsel,clBlue);
  GetSavingLayer(TABAKA_ParselAln,clBlue);
  GetSavingLayer(TABAKA_ParselNo,clBlue);

  if (LayerIsVisibleIsLock(TABAKA_AdaHat)) then
    TABAKA_AdaHat_Color := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).LayerInfo.Color;
  GetSavingLayer(TABAKA_AdaHat,TABAKA_AdaHat_Color);

  MClbIfrazYontemi.OnClick(nil);
end;
end;

procedure TIfrazLauncher.IfrazFormAyarlariniKaydet;
var
  Ini: TIniFile;
begin
with fmIfraz do
begin
  try
    Ini := TIniFile.Create(UygulamaYolu + 'Settings\Ifraz.ini') ;
    Ini.WriteInteger('IfrazAyarlari','IfrazYontemi', MClbIfrazYontemi.ItemIndex);
    Ini.WriteInteger('KonumAyarlari','AlanYonu', rgYeniAlanYonu.ItemIndex );
    Ini.WriteBool('KonumAyarlari','GeriKalanAlanTekrarBoluncekmi', chbGeriKalanAlaniTekrarBolunecek.Checked);
    Ini.WriteBool('KonumAyarlari','SonSecilenHattiKullan', chbSonSecilenHattiKullan.Checked);
    Ini.WriteBool('KonumAyarlari','ParselNoYaz', chkParselNoYaz.Checked);
    Ini.WriteBool('KonumAyarlari','ParselNoIleBlokNoyudaYaz', chkAdaNoYaz.Checked);
    Ini.WriteString('KonumAyarlari','YaziBoyutu', seYaziBoyu.EditText);
    Ini.WriteBool('KonumAyarlari','ParselinOrtaNoktasinaYaz', chkParselinOrtaNoktasinaYaz.Checked);
    Ini.WriteString('KonumAyarlari','GenelParselNo', GenelParselNo);
  finally
    Ini.Free;
  end;
end;
end;

procedure TIfrazLauncher.IfrazFormClose;
begin
  CmdLine.Tag := 0;
  IfrazFormAyarlariniKaydet;
//  if FAction <> nil then
//    FAction.Finished := True;
//  if CurrCmdLine.CurrentAction <> nil then
//  begin
//    CurrCmdLine.CurrentAction.Finished := True;
//    CurrCmdLine.LastCommand := SCmdExternal;
//  end;
end;

{$region 'IfrazAct'}
{
procedure TIfrazLauncher.IfrazFormShow(Sender: TObject);
begin
  fmIfraz.MClbIfrazYontemi.SetFocus;
end;
}
{
procedure TIfrazLauncher.IfrazActIptalExecute(Sender: TObject);
begin
  SendMessage(fmIfraz.Handle, WM_SYSCOMMAND, SC_CLOSE, 0 );
  //PostMessage(Self.Handle,wm_close,0,0);
//  FormAyarlariniKaydet;
//  FormStyle := fsNormal;
//  Hide;
//  fmBYAna.Show;
end;
}
{$endregion}

procedure TIfrazLauncher.IfrazbtnTamamClick;
var
  YeniNokta1, YeniNokta2: TlicgCoor;
  XNokta1, XNokta2: TlicgCoor;
  TempCoor: TlicgCoor;
  BolunecekAlan, Aci, Cephe, ParalelMesafe: Double;
  BolunecekPolygon, Paralel, Serbestcp: IlicgEntity;
  AdaParselNo: String;
  Kalancp: IlicgEntity;
  RotatingCoor: TlicgCoor;
  EndeksliDeger, Alan, Endeks: Double;
  TempDrawEnt: IlicgEntity;
begin
//with fmIfraz do
//  begin
  try
//    fmIfraz.FormHider1.Hide;

    if not TabakaDurumu then
      Exit;

    //EN_1_00
    if FIsEndex and (not LayerIsVisibleIsLock(TABAKA_Endeks)) then
    begin
      Application.MessageBox(PChar( TABAKA_Endeks + ' Tabakasý Bulunmamaktadýr' + #13 +
                             'Tabakayý oluþturup tekrar deneyiniz.'), 'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;

    if (fmIfraz.chbGeriKalanAlaniTekrarBolunecek.Checked) and (KalanObj <> nil) then
      BolunecekPolygon := KalanObj
    else
      BolunecekPolygon := BolunecekPolygonSec;

    if BolunecekPolygon = nil then
      Exit;

    CmdLine.ActiveDrawBox.ClearAlwaysDisplayList;
    TempDrawEnt := BolunecekPolygon.Clone;
    TempDrawEnt.DrawTools.PenTool.Color := Licad.Settings.SelectionPen.Color;//clWebRed;
    TempDrawEnt.DrawTools.PenTool.Width := CurrCmdLine.ActiveDrawBox.Grapher.PointsToDistX(Licad.Settings.ApertureWidth)/8;
    CmdLine.ActiveDrawBox.Add2EntityOnceDisplayList(TempDrawEnt); //Add2AlwaysDisplayList Add2EntityOnceDisplayList

    EndeksliDeger := -1;

    if FIsEndex then
      ComplexEndeksHesaplaComplexPoly(BolunecekPolygon,TABAKA_Endeks,Alan,Endeks,EndeksliDeger);

    //Yorum Satiri
    //NCLayermanager.Delete(FoundLayer(TABAKA_AdaHat), True);
    //CurrCmdLine.ActiveDrawBox.GIS.Layers.RemoveLayer(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat));
    //TabakaBulYoksaAc(TABAKA_AdaHat, clYellow);

//    CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(UaUnDelete);
    CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(UaDelete);

    if (LayerIsVisibleIsLock(TABAKA_AdaHat)) then
    begin
      TABAKA_AdaHat_Color := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).LayerInfo.Color;
      DeleteAllEntityInLayer(TABAKA_AdaHat);
//  	  CurrCmdLine.ActiveDrawBox.GIS.DeleteLayer(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).DisplayName);
    end;
    GetSavingLayer(TABAKA_AdaHat, TABAKA_AdaHat_Color);
    ObjeBombala(BolunecekPolygon, CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat));

    case fmIfraz.MClbIfrazYontemi.ItemIndex of
      0:begin //Paralel - OK
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            HatObje := Licad.CreateEntityFactory.MakeLine(AsCoor(0,0),AsCoor(0,0)); //newobject;
            HatObje := HatObjeSec2('Paraleli Alýnacak Hattý Göster.');
            //CurrCmdLine.Pop;
            if HatObje = nil then
              Exit;
          end;
          BolunecekAlan := TfmParalelIfrazParselAlaniGirisi.Execute('Paralel Ýfraz', BolunecekPolygon,HatObje.Geometry.Points[0],HatObje.Geometry.Points[1],EndeksliDeger);
          if BolunecekAlan <= 0 then
            Exit;

          NoktalariDuzenle(HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], YeniNokta1, YeniNokta2);
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, YeniNokta1, YeniNokta2, False);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      1:begin //Dik - OK
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            HatObje := Licad.CreateEntityFactory.MakeLine(AsCoor(0,0),AsCoor(0,0));// newobject;
            HatObje := HatObjeSec2('Dik Alýnacak Hattý Göster.');
            if HatObje = nil then
              Exit;
          end;
          BolunecekAlan := TfmDikIfrazParselAlaniGirisi.Execute('Dik Ýfraz', BolunecekPolygon, HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], Aci,EndeksliDeger);
          if BolunecekAlan <= 0 then
            Exit;

          NoktalariDuzenle(HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], YeniNokta1, YeniNokta2);
          HatObje.Geometry.Points[0] := YeniNokta1;
          HatObje.Geometry.Points[1] := YeniNokta2;
          //Seçilen Doðruyu Dik olarak Döndür
          RotatingCoor := HatObje.Geometry.Points[1];
          rotate_around(HatObje.Geometry.Points[0], RotatingCoor, DegToRad(Aci));
          NoktalariDuzenle(HatObje.Geometry.Points[0], RotatingCoor, YeniNokta1, YeniNokta2);
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, YeniNokta1, YeniNokta2, False);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      2:begin //Sabit Nokta
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            XNokta1 := AsCoor(0, 0);
            //Netcad.SetParam(PNC_SNAPENDOF, 1); //Yorum Satýrý
            //AppActivate('TNetcadForWindowsMainForm');
            //SendKeys('{F5}', True);
            if SelectPoint('Sabit Noktayý Seçiniz.', XNokta1, -1) then
            begin
              XNokta2 := XNokta1;
            end
            else
            begin
              Exit;
            end;
            // Doðru seçimi.
            //if not WacxLine('2. Noktayý Seçiniz', XNokta2,-1,XNokta1) then Exit;
            {HatObje := HatObjeSec('Hattý Göster.');
            if rgYeniAlanYonu.ItemIndex = 0 then
            begin
              XNokta1 := HatObje.p2;
              XNokta2 := HatObje.p1;
            end
            else
            begin
              XNokta1 := HatObje.p1;
              XNokta2 := HatObje.p2;
            end;
            if HatObje = nil then Exit; }
          end;
          //BolunecekPolygon.SetDirection(True); // Yorum Satýrý
          BolunecekAlan := TfmSabitNoktaParselAlaniGirisi.Execute('Sabit Noktadan Ýfraz', BolunecekPolygon, XNokta1, XNokta2, Cephe,EndeksliDeger);
          if BolunecekAlan <= 0 then
            Exit;
          //XNokta1 := NetcadMath.side_shoot(XNokta1, XNokta2, Cephe, 0);
          if BolunecekPolygon = nil then
            Exit;
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, XNokta1, XNokta2, True);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      3:begin //Serbest - OK
        { Yeni özellik için yapýldý o zaman kullan
          XNokta1 := newc(0, 0, 0);
          XNokta2 := newc(0, 0, 0);
          Serbestcp := newpoly;
          o := newobject;
        if SelectPoint('1. Noktayý Seçiniz.', XNokta1, -1) then
        begin
          Serbestcp.AddCoor(XNokta1);
          while WacxLine('Sonraki Noktayý seç', XNokta2, -1, XNokta1) do
          begin
            Serbestcp.addcoor(XNokta2);
            XNokta1.Assignx(XNokta2);
          o := MakePline('1',0,0,0,0,0,Serbestcp);
          AddObject(o);
          DelObject(Netcad.NumObject - 2, o);
          end;
          DelObject(Netcad.NumObject - 1, o);
        end;  }
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            XNokta1 := AsCoor(0, 0);
            XNokta2 := AsCoor(0, 0);
            FTrackedLineEntity := nil;
            Serbestcp := SelectLine;
            if Serbestcp = nil then
              Exit;
            XNokta1 := Serbestcp.Geometry.Points[0];
            XNokta2 := Serbestcp.Geometry.Points[1];
            //if not SelectLine('1. Noktayý Seçiniz.', XNokta1, -1) then  Exit;// Doðru seçimi.
            //if not WalkLine('2. Noktayý Seçiniz', XNokta2,-1,XNokta1) then Exit;
          end;
          BolunecekAlan := 0;
          NoktalariDuzenle(XNokta1, XNokta2, YeniNokta1, YeniNokta2);
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, YeniNokta1, YeniNokta2, False);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      4:begin //Cephe/Açý
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            HatObje := Licad.CreateEntityFactory.MakeLine(AsCoor(0,0),AsCoor(0,0));//newobject;
            HatObje := HatObjeSec2('Cephe Alýnacak Hattý Göster.');
            if HatObje = nil then
              Exit;
          end;

          Cephe := TfmCepheAciUzunluguGirisi.Execute('Cephe Uzunluðundan Ýfraz', BolunecekPolygon, HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], Aci, EndeksliDeger);
          if Cephe <= 0 then
          begin
            KalanObj := nil;
            HatObje := nil;
            Exit;
          end;
          NoktalariDuzenle(HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], XNokta1, XNokta2);
          if fmIfraz.rgYeniAlanYonu.ItemIndex = 0 then
          begin
            HatObje.Geometry.Points[0] := side_shoot(XNokta1, XNokta2, Cephe, 0);//NetcadMath.side_shoot(XNokta1, XNokta2, Cephe, 0);
            Rotate_Around(side_shoot(XNokta1, XNokta2, Cephe, 0), XNokta2, DegToRad(Aci));//NetcadMath.rotate_around(NetcadMath.side_shoot(XNokta1, XNokta2, Cephe, 0), XNokta2, DegToRad(Aci));
            HatObje.Geometry.Points[1] := XNokta2;
          end
          else
          begin
            HatObje.Geometry.Points[0] := side_shoot(XNokta2, XNokta1, Cephe, 0);//NetcadMath.side_shoot(XNokta2, XNokta1, Cephe, 0);
            Rotate_Around(side_shoot(XNokta2, XNokta1, Cephe, 0), XNokta2, DegToRad(Aci));//NetcadMath.rotate_around(NetcadMath.side_shoot(XNokta2, XNokta1, Cephe, 0), XNokta2, DegToRad(Aci));
            HatObje.Geometry.Points[1] := XNokta2;
          end;
          NoktalariDuzenle(HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], YeniNokta1, YeniNokta2);
          BolunecekAlan := 0;
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, YeniNokta1, YeniNokta2, False);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      5:begin //Paralel Mesafe
          if (not fmIfraz.chbSonSecilenHattiKullan.Checked) or (HatObje = nil) or (OncekiIfrazYontemi <> fmIfraz.MClbIfrazYontemi.ItemIndex) then
          begin
            HatObje := Licad.CreateEntityFactory.MakeLine(AsCoor(0,0),AsCoor(0,0));//newobject;
            HatObje := HatObjeSec2('Paraleli Alýnacak Hattý Göster.');
            if HatObje = nil then
            begin
//              fmIfraz.FormHider1.Show;
              Exit;
            end;
          end;
          ParalelMesafe := TfmParalelMesafeGirisi.Execute('Paralel Ýfraz', BolunecekPolygon, HatObje.Geometry.Points[0], HatObje.Geometry.Points[1],EndeksliDeger);
          if ParalelMesafe <= 0 then
            Exit;
          if HatObje.Geometry.Points[0].x < 0 then
            ParalelMesafe := ParalelMesafe * -1;
          Paralel := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
//          NoktalariDuzenle(HatObje.Geometry.Points[0], HatObje.Geometry.Points[1], YeniNokta1, YeniNokta2);
//          HatObje.Geometry.Points[0] := YeniNokta1;
//          HatObje.Geometry.Points[1] := YeniNokta2;

//          if _IsCounterClockWise(HatObje.Geometry.Points) then
//          begin
          TempCoor := HatObje.Geometry.Points[0];
          HatObje.Geometry.Points[0] := HatObje.Geometry.Points[1];
          HatObje.Geometry.Points[1] := TempCoor;
//          end;

          Paralel := GetParalel(ParalelMesafe,HatObje);

          //Paralel := HatObje.GetObjectAsPline.GetParalel(ParalelMesafe,False);
          BolunecekAlan := 0;
          NoktalariDuzenle(Paralel.Geometry.Points[0], Paralel.Geometry.Points[1], YeniNokta1, YeniNokta2);
          KalanObj := YeniIfraz(BolunecekAlan, fmIfraz.rgYeniAlanYonu.ItemIndex, fmIfraz.MClbIfrazYontemi.ItemIndex, BolunecekPolygon, YeniNokta1, YeniNokta2, False);
          OncekiIfrazYontemi := fmIfraz.MClbIfrazYontemi.ItemIndex;
        end;
      6:begin
          //Not Blokun Kalan Alaný Yoksa Otomatik Ekle
          Kalancp := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//NewComplexPoly;
          if (KalanObj = nil) and (BolunecekPolygon <> nil) then
            Kalancp.Assign(BolunecekPolygon)//PolyToComplexPoly(BolunecekPolygon)
          else
            Kalancp.Assign(KalanObj);//PolyToComplexPoly(KalanObj.GetObjectAsPline);
          ComplexPolyToSplitPolyShow(Kalancp);

          if (not LayerIsVisibleIsLock(TABAKA_ParselAln)) then
            Exit;
          
          AdaParselNo := TfmParselNoGirisi.Execute('Yeni Parsel No Giriþi,', GenelParselNo);
          if AdaParselNo = '' then
            Exit
          else
            GenelParselNo := AdaParselNo;

          Kalancp.Name := GenelParselNo;
          ComplexPolyToSplitPoly(Kalancp, TABAKA_ParselAln);
        end;
    end;

  finally
    Kalancp := nil;
    if LayerIsVisibleIsLock(TABAKA_AdaHat) then
    begin
      TABAKA_AdaHat_Color := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).LayerInfo.Color;
	    DeleteAllEntityInLayer(TABAKA_AdaHat);
//      CurrCmdLine.ActiveDrawBox.GIS.DeleteLayer(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_AdaHat).DisplayName);
      if BolunecekPolygon <> nil then
        CurrCmdLine.ActiveDrawBox.RepaintExtent(BolunecekPolygon.Geometry.Extent);
    end;
//    fmIfraz.FormHider1.Show;
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
    CmdLine.ActiveDrawBox.ClearAlwaysDisplayList;
//    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
end;

//----------------------------------------------------------------------------//

function TIfrazLauncher.SelectPoint(const prompt: WideString; var c: TlicgCoor;
                                    acursor: Integer): WordBool;
begin
//  CurrCmdline.Cursor := crDrawCross;
  CurrCmdline.CaptionCommand := prompt;
  c := BolecekNoktaSec(prompt);
//  CurrCmdline.Cursor := crDefault;
  if not FBoolTrackedPoint then
  begin
    Result := False;
    FBoolTrackedPoint := False;
    Exit;
  end;
  Result := True;
end;

function TIfrazLauncher.BolunecekPolygonSec: IlicgEntity;
begin
  FTrackedEntity := nil;
  Launcher2 := Licad.CreateActionTracker(CurrCmdLine);
  Launcher2.IsCmdLineClear := False;
  Launcher2.IsDefaultInitialization := True;
  Launcher2.OnTrackedEntityClicked := LauncherTrackedEntityClickedPoly;
  Launcher2.TrackGetPolygon(SCmdLauncher);  
  Launcher2.OnFinished := LauncherFinishedPoly;
  repeat
    Application.HandleMessage;
  until Launcher2.Finished;
  Launcher2.Finish;
  Result := FTrackedEntity;
end;

procedure TIfrazLauncher.LauncherTrackedEntityClickedPoly(const TrackID: string;
                                                          var TrackedEntity: IlicgEntity);
begin
  if (TrackedEntity <> nil) and (TrackedEntity.EntityID in [idPolygon]) then
  begin
    if FTrackedEntity = nil then
      FTrackedEntity := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
    FTrackedEntity.Geometry.Points.Clear;
    FTrackedEntity.Assign(TrackedEntity);
    Launcher2.Finished := True;
  end;
end;

procedure TIfrazLauncher.LauncherFinishedPoly(Sender: TObject);
begin
//  Launcher.CurrentAction.Finished := True;
end;

function TIfrazLauncher.BolecekNoktaSec(const prompt: WideString): TlicgCoor;
var
  CurrPoint: TlicgCoor;
begin
  Launcher2 := Licad.CreateActionTracker(CurrCmdLine);
  Launcher2.IsCmdLineClear := False;
  Launcher2.IsDefaultInitialization := True;
  Launcher2.TrackPoint(SCmdLauncher);
  Launcher2.OnTrackedEntity := MouseUpPoint;
  Launcher2.OnFinished := LauncherFinishedPoint;
  Launcher2.CurrentAction.Caption := prompt;

//  if not Launcher.CurrentAction.CmdLine.AccuSnap.Enabled then
//    Launcher.CurrentAction.CmdLine.AccuSnap.Enabled := True;

  if not Self.CmdLine.AccuSnap.Enabled then
    Self.CmdLine.AccuSnap.Enabled := True; //Invalid Floating Point Operation : HATA //Giderildi.

  FBoolTrackedPoint := False;

  repeat
    Application.HandleMessage; //Invalid Floating Point Operation : HATA //Giderildi.
  until Launcher2.Finished;

  if Self.CmdLine.AccuSnap.Enabled then
    Self.CmdLine.AccuSnap.Enabled := False;

  Launcher2.Finish;

  if CurrCmdLine.IsSnapped then
    CurrPoint := CurrCmdLine.GetSnappedPoint
  else
    CurrPoint := CurrCmdLine.CurrentPoint;

  Result := CurrPoint;
end;

procedure TIfrazLauncher.MouseUpPoint(const TrackID: string; var TrackedEntity: IlicgEntity);
begin
  FBoolTrackedPoint := True;
end;

procedure TIfrazLauncher.LauncherFinishedPoint(Sender: TObject);
begin
//  Launcher.CurrentAction.Finished := True;
end;

function TIfrazLauncher.SelectLine: IlicgEntity;
begin
  FTrackedLineEntity := nil;
  Launcher2 := Licad.CreateActionTracker(CurrCmdLine);
  Launcher2.IsCmdLineClear := False;
  Launcher2.IsDefaultInitialization := True;
  Launcher2.OnTrackedEntity := LauncherTrackedEntityLine;
  Launcher2.TrackLine(SCmdLine,False);  
  Launcher2.OnFinished := LauncherFinishedLine;
  repeat
    Application.HandleMessage;
  until Launcher2.Finished;
  Launcher2.TrackLine(SCmdLauncher);
  Launcher2.Finish;
  Result := FTrackedLineEntity;
end;

procedure TIfrazLauncher.LauncherTrackedEntityLine(const TrackID: string;
                                                  var TrackedEntity: IlicgEntity);
begin
  if (TrackedEntity <> nil) and (TrackedEntity.EntityID in [idLine]) then
  begin
    FTrackedLineEntity := TrackedEntity;
    Launcher2.Finished := True;
  end;
end;

procedure TIfrazLauncher.LauncherFinishedLine(Sender: TObject);
begin
//  Launcher.CurrentAction.Finished := True;
end;

{$region 'HatObjeSec'}
{
function TIfrazLauncher.HatObjeSec(ASecimMesaj: String):IlicgEntity;
var
  FAction: TlicgAction;
  TmpAlayer: TlicgBaseLayer;
  TmpARecNo: Integer;
  I, J, FirstI, SecondI: Integer;
  AEnt: IlicgEntity;
  CurrPoint: TlicgCoor;
  Distance: Double;
begin
  CurrCmdLine.DoCommand('QUICKSELECT', '');
  FAction := CurrCmdLine.CurrentAction;
  FAction.Caption := ASecimMesaj;
  FAction.NoPickFilter := AllEntityIDs - [idLine, idPolyline, idSpline, idPolygon, idCircle,
                          idEllipse, idPolyEllipse, idArc, idPolyArc, idRectangle, idPolyRectangle];

  if Assigned(CurrCmdLine.OnSetActionParameterProperties) then
    CurrCmdLine.OnSetActionParameterProperties(FAction);

  CurrCmdLine.ActiveDrawBox.Selection.Clear;

  repeat
    Application.HandleMessage;
    if CurrCmdLine.ActiveDrawBox.Selection.NumSelected > 0 then
      FAction.Finished := True;
  until FAction.Finished;

  if CurrCmdLine.ActiveDrawBox.Selection.Count <= 0 then
  begin
    Result := nil;
    Exit;
  end;

  if CurrCmdLine.IsSnapped then
    CurrPoint := CurrCmdLine.GetSnappedPoint
  else
    CurrPoint := CurrCmdLine.CurrentPoint;

  CurrCmdLine.Pop;

  with CurrCmdLine.ActiveDrawBox do
  begin
    for I := 0 to Selection.Count - 1 do
    begin
      for J := 0 to Selection[i].SelList.Count - 1 do
      begin
        TmpALayer := Selection[i].Layer;

        if (not LayerIsVisibleIsLock(TmpALayer.DisplayName)) then Continue;
        
        TmpARecno := Longint(Selection[i].SelList[j]);
        AEnt := nil;
        AEnt := TmpALayer.LoadEntityWithRecNo(TmpARecno);

        if not (AEnt.EntityID in [idLine, idPolyline, idSpline, idPolygon, idCircle, idEllipse,
          idPolyEllipse, idArc, idPolyArc, idRectangle, idPolyRectangle]) then
          Continue;

        BetweenWhichPoints(AEnt.Geometry.Points, CurrPoint, CurrCmdLine.ActiveDrawBox.Grapher,
          Licad.Settings.ApertureWidth, FirstI, SecondI, Distance);

        if (FirstI = -1) and (SecondI = -1) then
        begin
          FirstI := 0;
          SecondI := 1;
        end;

        Result := Licad.CreateEntityFactory.MakeLine(Aent.Geometry.Points[FirstI],Aent.Geometry.Points[SecondI]);
        Exit;
      end;
    end;
  end;
end;
}
{$endregion}

function TIfrazLauncher.HatObjeSec2(ASecimMesaj: String):IlicgEntity;
var
  TmpAlayer: TlicgBaseLayer;
  TmpARecNo: Integer;
  I: Integer;
  AEnt: IlicgEntity;
  CurrPoint: TlicgCoor;
  FStackedSelList: IPickedList;
  PickedPoint: Integer;
  Picked: Boolean;
  TempDrawEnt: IlicgEntity;
begin
  Launcher2 := Licad.CreateActionTracker(CurrCmdLine);
  Launcher2.IsCmdLineClear := False;
  Launcher2.IsDefaultInitialization := True;
  Launcher2.TrackPoint(SCmdLauncher);
  Launcher2.CurrentAction.Cursor := crSelectEntity;
  Launcher2.CurrentAction.Caption := ASecimMesaj;
  Launcher2.OnFinished := LauncherFinishedPoint;
  repeat
    Application.HandleMessage;
    //if CurrCmdLine.ActiveDrawBox.Selection.NumSelected > 0 then
    //  Launcher.Finished := True;
  until Launcher2.Finished;

  Launcher2.Finish;
  if CurrCmdLine.IsSnapped then
    CurrPoint := CurrCmdLine.GetSnappedPoint
  else
    CurrPoint := CurrCmdLine.CurrentPoint;

//  CurrCmdLine.Pop;

  FStackedSelList := TPickedList.Create;
  Picked := CurrCmdLine.ActiveDrawBox.PickEntity(CurrPoint.x, CurrPoint.y, Licad.Settings.ApertureWidth,
    '', TmpALayer, TmpARecNo, PickedPoint, FStackedSelList);

  for I := 0 to FStackedSelList.Count - 1 do
  begin
    TmpALayer := FStackedSelList.PickedItems[I].Pick.Layer;

    if (not LayerIsVisibleIsLock(TmpALayer.DisplayName)) then Continue;


    TmpARecno := Longint(FStackedSelList.PickedItems[I].Pick.RecNo);
    AEnt := TmpALayer.LoadEntityWithRecNo(TmpARecno);
    if not (AEnt.EntityID in [idLine]) then //, idPolyline, idSpline, idPolygon, idCircle, idEllipse,
//     idPolyEllipse, idArc, idPolyArc, idRectangle, idPolyRectangle]) then
      Continue;

//    BetweenWhichPoints(AEnt.Geometry.Points, CurrPoint, CurrCmdLine.ActiveDrawBox.Grapher,
//          Licad.Settings.ApertureWidth, FirstI, SecondI, Distance);

//    if (FirstI = -1) and (SecondI = -1) then
//    begin
//      FirstI := 0;
//      SecondI := 1;
//    end;

    Result := Aent;

//    CmdLine.ActiveDrawBox.ClearAlwaysDisplayList;
    TempDrawEnt := Aent.Clone;
    TempDrawEnt.DrawTools.PenTool.Color := Licad.Settings.SelectionPen.Color;//clWebRed; //clGreen
    TempDrawEnt.DrawTools.PenTool.Width := CurrCmdLine.ActiveDrawBox.Grapher.PointsToDistX(Licad.Settings.ApertureWidth)/8;
    CmdLine.ActiveDrawBox.Add2EntityOnceDisplayList(TempDrawEnt);
//    Result := Licad.CreateEntityFactory.MakeLine(Aent.Geometry.Points[FirstI],Aent.Geometry.Points[SecondI]);
    Exit;
  end;
end;

procedure TIfrazLauncher.NoktalariDuzenle(Nokta1, Nokta2: TlicgCoor;
                                        out YeniNokta1, YeniNokta2: TlicgCoor);
var
  X, X1, X2, Y, Y1, Y2: Double;
begin
  X1 := Nokta1.x;
  X2 := Nokta2.x;
  Y1 := Nokta1.y;
  Y2 := Nokta2.y;
  X  := ABS(X1 - X2);
  Y  := ABS(Y1 - Y2);
  YeniNokta1 := AsCoor(0,0);//Netcad.newc(0, 0, 0);
  YeniNokta2 := AsCoor(0,0);//Netcad.newc(0, 0, 0);
  //ShowMessage(FloatToStr(Nokta1.x) + '  --)' + FloatToStr(Nokta1.y));
  //ShowMessage(FloatToStr(Nokta2.x) + '  --)' + FloatToStr(Nokta2.y));

  if X >= Y then // X Kullan
  begin
    if X1 <= X2 then
    begin
       YeniNokta1.x := X1;
       YeniNokta1.y := Y1;
       YeniNokta2.x := X2;
       YeniNokta2.y := Y2;
    end
    else if X1 > X2  then
    begin
       YeniNokta1.x := X2;
       YeniNokta1.y := Y2;
       YeniNokta2.x := X1;
       YeniNokta2.y := Y1;
    end;
  end
  else if Y > X then // Y Kullan
  begin
    if Y1 <= Y2 then
    begin
       YeniNokta1.x := X1;
       YeniNokta1.y := Y1;
       YeniNokta2.x := X2;
       YeniNokta2.y := Y2;
    end
    else if Y1 > Y2  then
    begin
       YeniNokta1.x := X2;
       YeniNokta1.y := Y2;
       YeniNokta2.x := X1;
       YeniNokta2.y := Y1;
    end;
  end;
end;

function TIfrazLauncher.PolyClip(AClipType: TlicgClipType; SubEnt, PolyEnt: IlicgEntity; var resultcp: IlicgEntity): Boolean;
begin
  Result := True;
  if AClipType = ClipTypeIntersection then
    resultcp := ATop.Intersection(SubEnt,PolyEnt, False)
  else if AClipType = ClipTypeDifference then
    resultcp := ATop.Difference(SubEnt,PolyEnt, False);

  if resultcp = nil then
  begin
    Result := False;
    resultcp := PolyEnt;
  end;
end;

function TIfrazLauncher.TabakaDurumu: Boolean;
begin
  Result := True;
  //Yorum Satiri
//  if not TabakalariKontrolEt(['ADA','ADA_HAT','EN_1_00'],//dm.tbTabakaBLOK.AsString,
//                                 //dm.tbTabakaBLOKHAT.AsString,
//                                // dm.tbTabakaEN100.AsString],
//                                ['Blok Alan',
//                                 'Blok Hat',
//                                 'Derece(Endeks) Sýnýr'],
//                                TfmParselNoGirisi) then//TfmBYTabakaAyarlari) then
//  begin
//    Result := False;
//    Exit;
//  end;
end;

function TIfrazLauncher.DikdortgenOlustur(Poly:IlicgEntity;DogruP1,DogruP2:TlicgCoor): IlicgEntity;
var I: Integer;
    PolyMatris: Array of Array [0..8] of Double;
    BuyukH, KucukH, BuyukS1, BuyukS2: Double;
    _sideTail, _sideLength: Double;
    MinDikDortgen: IlicgEntity;
begin
  SetLength(PolyMatris,Poly.Geometry.Points.Count);
  for I := 0 to Poly.Geometry.Points.Count - 1 do
  begin
    PolyMatris[i,0] := i; // Nokta No
    PolyMatris[i,1] := 1; // Paralel Hat 1. Nokta
    PolyMatris[i,2] := 2; // Paralel Hat 2. Nokta
    PolyMatris[i,3] := Poly.Geometry.Points[i].y; // Nokta Y
    PolyMatris[i,4] := Poly.Geometry.Points[i].x; // Nokta X
    INVside_shoot(Poly.Geometry.Points[i],DogruP1,DogruP2,_sideLength,_sideTail);
    PolyMatris[i,5] := _sideTail;// DikBoy
    PolyMatris[i,6] := _sideLength;// DikAyak (1 - 2)
    INVside_shoot(Poly.Geometry.Points[i],DogruP2,DogruP1,_sideLength,_sideTail);
    PolyMatris[i,7] := _sideLength;// DikAyak (2 - 1)
    PolyMatris[i,8] := 0;
  end;
  // Seçilen paralel hatta göre enbüyük(h) ve enküçük(h) yüksekliklerinin tespit edilmesi
  // Enbüyük (SP1) ve (SP2) lerin bulunmasý
  BuyukH := -1e+300;
  KucukH := 1e+300;
  BuyukS1 := -1e+300;
  BuyukS2 := -1e+300;
  for i := 0 to Poly.Geometry.Points.Count - 1 do
  begin
    BuyukH := Max(BuyukH,PolyMatris[i,5]);
    KucukH := Min(KucukH,PolyMatris[i,5]);
  end;
  for i := 0 to Poly.Geometry.Points.Count - 1 do
  begin
    BuyukS1 := Max(BuyukS1,PolyMatris[i,6]);
    BuyukS2 := Max(BuyukS2,PolyMatris[i,7]);
  end;
  //Parseleasyon kesiþim Dikdörtgeninin bulunmasý
  MinDikDortgen := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly
  AddCoor(MinDikDortgen, side_shoot(DogruP1,DogruP2,BuyukS1,KucukH));//NetcadMath.
  AddCoor(MinDikDortgen, side_shoot(DogruP1,DogruP2,BuyukS1,BuyukH));
  AddCoor(MinDikDortgen, side_shoot(DogruP2,DogruP1,BuyukS2,-BuyukH));
  AddCoor(MinDikDortgen, side_shoot(DogruP2,DogruP1,BuyukS2,-KucukH));
  Result := MinDikDortgen;
end;

function TIfrazLauncher.YeniIfraz(BolunecekDegerSayisi:Double; AlanYonu, IfrazYontem:Integer;
                                  PolySecim:IlicgEntity; DogruP1,DogruP2:TlicgCoor;
                                  Cephe: Boolean):IlicgEntity;
var
  I,J,K,EntCount,WhileCount,InPartPointCount : Integer;
  PolyMatris: Array of Array [0..8] of Double;
  BuyukH, KucukH, BuyukS1, BuyukS2: Double;
  MinDikDortgen, MinDikDortgenParcaAlanPoly, TempMinDikDortgen: IlicgEntity;//NCPline;
  Katsayi: Double;
  StartPoint1,StartPoint2,FinishPoint1,FinishPoint2,MiddlePoint1,MiddlePoint2:TlicgCoor;

  GWhileCount: Integer;
  ExitCount: Integer;
  sameFlag: Boolean;
  cp, tempcp, tempcp2, tempcpkalan: IlicgEntity;//NCComplexPoly;
//  cpResultList, cpListSubject, cpListPoly,
  cpList: IlicgEntityList;
  KalanPoly: IlicgEntity;//NCPline;
//  KalanAlanCollection: IlicgEntity;//NCCollection;

  Alan, Endeks, DegerSayisi: Double;
  BolmeOrani: Double;
  Mesafe : Double;
  AdaParselNo: string;

  // Sabit Nokta Ýçin
  YeniDogru: IlicgEntity;//NetCadObj;
  IcxNokta, SonNokta, OrtaNokta: TlicgCoor;
  DBaslama, DBitis: Integer;
  PolySirali, PolyYeni, PolyYeniBuyuk: IlicgEntity;//NCPline;

  _sideTail, _sideLength: Double;
  Rc: Integer;
  _Vector1, _Vector2, _VectorR: IlicgVector;
  _PolyArea: Double;
  EntList: TlicgEntityList;
  coor1, coor2: TlicgCoor;
  iV, tempiV: IlicgVector;
  TempMidPoint1,TempMidPoint2: TlicgCoor;
  factor: Integer;
  FIndex1, FIndex2, SIndex1, SIndex2: Integer;
begin
  with fmIfraz do
  begin
  try
    // Seçilen Çokludoðru bilgilerinin (DikBoy,DikAyak vb) PolyMatrise atanmasý.
    _PolyArea := 0;
    GWhileCount := 0;
    InPartPointCount := 0;
    SetLength(PolyMatris,PolySecim.Geometry.Points.Count);
    for i := 0 to PolySecim.Geometry.Points.Count - 1 do
    begin
      PolyMatris[i,0] := i; // Nokta No
      PolyMatris[i,1] := 1; // Paralel Hat 1. Nokta
      PolyMatris[i,2] := 2; // Paralel Hat 2. Nokta
      PolyMatris[i,3] := PolySecim.Geometry.Points[i].y; // Nokta Y
      PolyMatris[i,4] := PolySecim.Geometry.Points[i].x; // Nokta X
      INVside_shoot(PolySecim.Geometry.Points[i],DogruP1,DogruP2,_sideLength,_sideTail);
      PolyMatris[i,5] := _sideTail;// DikBoy
      PolyMatris[i,6] := _sideLength;// DikAyak (1 - 2)
      INVside_shoot(PolySecim.Geometry.Points[i],DogruP2,DogruP1,_sideLength,_sideTail);
      PolyMatris[i,7] := _sideLength;// DikAyak (2 - 1)
      PolyMatris[i,8] := 0;
    end;
    // Seçilen paralel hatta göre enbüyük(h) ve enküçük(h) yüksekliklerinin tespit edilmesi
    // Enbüyük (SP1) ve (SP2) lerin bulunmasý
    BuyukH := -1e+300;
    KucukH := 1e+300;
    BuyukS1 := -1e+300;
    BuyukS2 := -1e+300;
    for i := 0 to PolySecim.Geometry.Points.Count - 1 do
    begin
      BuyukH := Max(BuyukH,PolyMatris[i,5]);
      KucukH := Min(KucukH,PolyMatris[i,5]);
    end;
    for i := 0 to PolySecim.Geometry.Points.Count - 1 do
    begin
      BuyukS1 := Max(BuyukS1,PolyMatris[i,6]);
      BuyukS2 := Max(BuyukS2,PolyMatris[i,7]);
    end;
    //Parseleasyon kesiþim Dikdörtgeninin bulunmasý
    MinDikDortgen := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly
    TempMinDikDortgen := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
    AddCoor(MinDikDortgen, side_shoot(DogruP1,DogruP2,BuyukS1,KucukH));//NetcadMath.
    AddCoor(MinDikDortgen, side_shoot(DogruP1,DogruP2,BuyukS1,BuyukH));
    AddCoor(MinDikDortgen, side_shoot(DogruP2,DogruP1,BuyukS2,-1*BuyukH));
    AddCoor(MinDikDortgen, side_shoot(DogruP2,DogruP1,BuyukS2,-1*KucukH));
    TempMinDikDortgen.Assign(MinDikDortgen);
    //AddObject(MakePline('DikDortgen', POLYCLOSED  + POLYNAME + POLYFILLED, 0, FoundLayer('@PARSEL'), 0, 0, MinDikDortgen));
    Katsayi := 0.0001;
    StartPoint1 := MinDikDortgen.Geometry.Points[0];
    StartPoint2 := MinDikDortgen.Geometry.Points[3];
    FinishPoint1 := MinDikDortgen.Geometry.Points[1];
    FinishPoint2 := MinDikDortgen.Geometry.Points[2];
    //AddObject(MakePline('MinDikDortgen', POLYCLOSED + POLYNAME + POLYFILLED, 0, 0, 0, 0, MinDikDortgen));
    cp := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//NewComplexPoly;

    if FIsEndex then
      ComplexEndeksHesaplaComplexPoly(PolySecim, TABAKA_Endeks, Alan, Endeks, DegerSayisi)//Yorum Satýrý
    else
      DegerSayisi := Abs(PolySecim.Geometry.Points.Area); //yapýlacak.

    if (IfrazYontem <> 3) and (BolunecekDegerSayisi > DegerSayisi) then  // Serbest Böl hariç ise
    begin
      Application.MessageBox(PChar('Girilen Deðer Sayýsý En Fazla ' + FloatToStr(RoundTo(DegerSayisi,(-1*(Integer(Licad.Settings.Precision.Area))))) + ' Olabilir.' + #13 +
                             'Girilen Deðer Sayýsý Ýçin Ýfraz Ýþlemi Yapýlamaz.'), 'Bilgi', MB_OK + MB_ICONINFORMATION);
      Exit;
    end;

    {if BolunecekDegerSayisi < 10000 then
      BolmeOrani := 2
    else
      BolmeOrani := (BolunecekDegerSayisi / DegerSayisi) * 50;
    }
    BolmeOrani := 2; // Yapýldý küçükleri bölmüyor. BolmeOrani := (BolunecekDegerSayisi / DegerSayisi) * 50;

    DegerSayisi := 0;

////////////////////////////////////////////////////////////////////////////////

    if (Cephe) then // Sabit Nokta
    begin
      IcxNokta := AsCoor(0, 0);
      SonNokta := AsCoor(0, 0);
      OrtaNokta := AsCoor(0, 0);
      //YeniDogru := newobject;
      //PolySecim.SetDirection(true); // Yorum Satýrý
      PolySirali := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
      if PolySecim.Geometry.Points.Count >= 2 then
      for I := 0 to PolySecim.Geometry.Points.Count - 1 do //-2 deðiþtirildi.
      begin
        if EqualPoint2D(DogruP1,PolySecim.Geometry.Points[I],DecimalOfCoordinate+3) then //-5
        begin
          DBaslama := I;
          Break;
        end;
      end;
      if AlanYonu = 0 then //saat yönü saðda
      begin
        if PolySecim.Geometry.Points.Count >= 2 then
        for I := DBaslama to PolySecim.Geometry.Points.Count - 1 do //-2 deðiþtirildi.
        begin
          AddCoor(PolySirali,PolySecim.Geometry.Points[I]);
        end;
        for I := 0 to DBaslama - 1 do
        begin
          AddCoor(PolySirali,PolySecim.Geometry.Points[I]);
        end;
      end
      else if AlanYonu = 1 then //solda saat yönü tersi
      begin
        for I := DBaslama downto 0 do
        begin
          AddCoor(PolySirali,PolySecim.Geometry.Points[I]);
        end;
        if PolySecim.Geometry.Points.Count >= 2 then
        for I := PolySecim.Geometry.Points.Count - 2 downto DBaslama + 1 do
        begin
          AddCoor(PolySirali,PolySecim.Geometry.Points[I]);
        end;
      end;
      //PolySirali.SetClosed(True); // Önemli // Yorum Satýrý

      //ObjeBombala(PolySirali, FoundLayer('1'));
      //AddObject(MakePline('Polyyeni', POLYCLOSED + POLYNAME + POLYFILLED, 0,FoundLayer('1'), 0, 0, PolySirali));
      //Exit;

      PolyYeni := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
      PolyYeniBuyuk := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
      //Ýstenilen deðerden büyük poly bul
      coor1 := AsCoor(0,0);
      coor1.X := PolySirali.Geometry.Points[0].X;
      coor1.Y := PolySirali.Geometry.Points[0].Y;

      for I := 0 to PolySirali.Geometry.Points.Count - 1 do
      begin
        AddCoor(PolyYeniBuyuk,PolySirali.Geometry.Points[I]);

        if FIsEndex then
          ComplexEndeksHesaplaComplexPoly(PolyYeniBuyuk, TABAKA_Endeks, Alan, Endeks, DegerSayisi)
        else
          DegerSayisi := Abs(PolyYeniBuyuk.Geometry.Points.Area);

        if Abs(DegerSayisi) > 0 then
        begin
        //Yorum Satýrý
        //GPCComplexPolyOperation(GPC_INT, PolyAddObje(PolyYeniBuyuk).GetObjectAsComplexPline, PolyAddObje(PolySecim).GetObjectAsComplexPline, cp);
        //ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi);

          tempcp := PolyYeniBuyuk;

          PolyClip(ClipTypeIntersection, PolySecim, PolyYeniBuyuk, cp);

          if FIsEndex then
            ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi)
          else
            DegerSayisi := Abs(cp.Geometry.Points.Area);

          //ShowMessage(FloatToStr(DegerSayisi));
          //AddObject(MakePline('Polyyeni', POLYCLOSED + POLYNAME + POLYFILLED, 0,FoundLayer('1'), 0, 0, PolyYeniBuyuk));

          if DegerSayisi >= BolunecekDegerSayisi then
          begin
            PolyYeni.Assign(PolyYeniBuyuk); //Assignx
            if PolyYeni.Geometry.Points.Count >= 2 then
            begin
              IcxNokta.X := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 2].X;
              IcxNokta.Y := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 2].Y;
            end;
            SonNokta.X := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 1].X;
            SonNokta.Y := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 1].Y;
            OrtaNokta.X := SonNokta.X;
            OrtaNokta.Y := SonNokta.Y;
            //YeniDogru := MakeLine(PolyYeni.Cor[PolyYeni.num - 2], PolyYeni.Cor[PolyYeni.num - 1], FoundLayer('1'), 0, 0);
            //AddObject(YeniDogru);
            Break;
          end;
        end
      end;
      //AddObject(MakePline('Polyyeni', POLYCLOSED + POLYNAME + POLYFILLED, 0,FoundLayer('1'), 0, 0, PolyYeni));
      // En son doðruyu bul böl ve alaný bul
      while not (Abs(DegerSayisi - BolunecekDegerSayisi) <= Katsayi) do //0.00001
      begin
        //YeniDogru := MakeLine(IcxNokta, OrtaNokta, 0, 0, 0);

        //Yapýlacak
        //PolyYeni.DeleteCoor(PolyYeni.Geometry.Points.Count - 1);
//        PolyYeni.Geometry.Points.Delete(PolyYeni.Geometry.Points.Count - 1);
        PolyYeni.Geometry.Points.Count := PolyYeni.Geometry.Points.Count-1;
        AddCoor(PolyYeni, OrtaNokta);
      //Yorum Satýrý
      //GPCComplexPolyOperation(GPC_INT, PolyAddObje(PolyYeni).GetObjectAsComplexPline, PolyAddObje(PolySecim).GetObjectAsComplexPline, cp);
      //ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi);
        tempcp := PolyYeni;
        PolyClip(ClipTypeIntersection, PolySecim,PolyYeni,cp);

        if FIsEndex then
          ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi)
        else
          DegerSayisi := Abs(cp.Geometry.Points.Area);

        if DegerSayisi = 0 then
        begin
//          PolyYeni.Name := 'deneme';
//          JoinEnt(PolyYeni);
        end;

        coor2 := IcxNokta;
        if DegerSayisi > BolunecekDegerSayisi then
        begin
          OrtaNokta := MidPointOf(IcxNokta, OrtaNokta);
        end
        else if DegerSayisi < BolunecekDegerSayisi then
        begin
          IcxNokta.X := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 1].X;
          IcxNokta.Y := PolyYeni.Geometry.Points[PolyYeni.Geometry.Points.Count - 1].Y;
          OrtaNokta := MidPointOf(IcxNokta, SonNokta);
        end;

        //Sonsuz Döngü var. While da kesmek lazým.
        if EqualPoint2D(IcxNokta,OrtaNokta,DecimalOfCoordinate+3) then//-5
        begin
        //OrtaNokta.X := SonNokta.X;
        //OrtaNokta.Y := SonNokta.Y;
          IcxNokta := coor2;
          Break;
        end;
        //AddObject(MakePline('Polyyeni', POLYCLOSED + POLYNAME + POLYFILLED, 0,FoundLayer('1'), 0, 0, PolyYeni));

//        if FBoolEscPress then Exit; //Yorum Satýrý
      end;
      //AddObject(MakePline('Polyyeni', POLYCLOSED + POLYNAME + POLYFILLED, 0,FoundLayer('1'), 0, 0, PolyYeni));
      //ShowMessage(FloatToStr(DegerSayisi));
    end;
    coor2 := FinishPoint1;
    if (AlanYonu = 0) and (not Cephe) then
    begin
      while not (Abs(DegerSayisi - BolunecekDegerSayisi)<=Katsayi) do
      begin
        Mesafe := _Distance(StartPoint1,FinishPoint1);

        if EqualPoint2D(StartPoint1,FinishPoint1,-8) then
        begin
          FinishPoint1 := coor2;
          Inc(GWhileCount);
          if GWhileCount = 10 then
          begin
            GWhileCount := 0;
            Break;
          end;
        end;

        MinDikDortgenParcaAlanPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[3]);
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[0]);
        MiddlePoint1 := side_shoot(StartPoint1,FinishPoint1, Mesafe/BolmeOrani, 0);
        MiddlePoint2 := side_shoot(StartPoint2,FinishPoint2, Mesafe/BolmeOrani, 0);

        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint1);
        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint2);

      //Yorum Satýrý
      //GPCComplexPolyOperation(GPC_INT,PolyAddObje(MinDikDortgenParcaAlanPoly).GetObjectAsComplexPline,PolyAddObje(PolySecim).GetObjectAsComplexPline,cp);
      //ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi);

        // Poligonu kestiði alan haline dönüþtürmemiz gerek.
        tempcp := MinDikDortgenParcaAlanPoly;
        PolyClip(ClipTypeIntersection,PolySecim,MinDikDortgenParcaAlanPoly,cp);

        if FIsEndex then
          ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi)
        else
          DegerSayisi := Abs(cp.Geometry.Points.Area);

        if BolunecekDegerSayisi > DegerSayisi then
        begin
          StartPoint1 := MiddlePoint1;
          StartPoint2 := MiddlePoint2;
        end;
        if BolunecekDegerSayisi < DegerSayisi then
        begin
          FinishPoint1 := MiddlePoint1;
          FinishPoint2 := MiddlePoint2;
        end;
//          if FBoolEscPress then Exit; //yapýlcak
      end;
    end
    else if (AlanYonu = 1) and (not Cephe) then
    begin
      while not (Abs(DegerSayisi - BolunecekDegerSayisi)<=Katsayi) do
      begin
        Mesafe := _Distance(StartPoint1,FinishPoint1);

        if EqualPoint2D(StartPoint1,FinishPoint1,-8) then
        begin
          FinishPoint1 := coor2;
          Inc(GWhileCount);
          if GWhileCount = 10 then
          begin
            GWhileCount := 0;
            Break;
          end;
        end;

        MinDikDortgenParcaAlanPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[1]);
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[2]);
        MiddlePoint1 := side_shoot(FinishPoint1, StartPoint1, Mesafe/BolmeOrani, 0);
        MiddlePoint2 := side_shoot(FinishPoint2, StartPoint2, Mesafe/BolmeOrani, 0);
        if Cephe then //Sabit Nokta Ýçin Ekle.
        begin
          MiddlePoint1 := DogruP1;
          MiddlePoint2 := DogruP1;
        end;

        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint2);
        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint1);

      //Yorum Satýrý
      //GPCComplexPolyOperation(GPC_INT, PolyAddObje(MinDikDortgenParcaAlanPoly).GetObjectAsComplexPline, PolyAddObje(PolySecim).GetObjectAsComplexPline, cp);
      //ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi);

        tempcp := MinDikDortgenParcaAlanPoly;
        PolyClip(ClipTypeIntersection,PolySecim,MinDikDortgenParcaAlanPoly,cp);

        if FIsEndex then
          ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, DegerSayisi)
        else
          DegerSayisi := Abs(cp.Geometry.Points.Area);

        if BolunecekDegerSayisi < DegerSayisi then
        begin
          StartPoint1 := MiddlePoint1;
          StartPoint2 := MiddlePoint2;
        end;
        if BolunecekDegerSayisi > DegerSayisi then
        begin
          FinishPoint1 := MiddlePoint1;
          FinishPoint2 := MiddlePoint2;
        end;
//        if FBoolEscPress then Exit; //yapýlcak
      end;
    end;

    // BolunecekDegerSayisi 0 ise yani deðer girmeden bölmek için
    if (BolunecekDegerSayisi = 0) and (not Cephe) then
    begin
      MiddlePoint1 := AsCoor(0,0);
      MiddlePoint2 := AsCoor(0,0);
      MinDikDortgenParcaAlanPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly
      //PolySecim.SetDirection(True);//Yorum Satýrý
      if AlanYonu = 0 then
      begin
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[3]);
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[0]);

        intersection(MiddlePoint1, StartPoint1, FinishPoint1, DogruP1, DogruP2);
        intersection(MiddlePoint2, StartPoint2, FinishPoint2, DogruP1, DogruP2);

        //NetcadMath.intersection(MiddlePoint1, StartPoint1, FinishPoint1, DogruP1, DogruP2);
        //NetcadMath.intersection(MiddlePoint2, StartPoint2, FinishPoint2, DogruP1, DogruP2);

        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint1);
        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint2);
      end
      else if AlanYonu = 1 then
      begin
        intersection(MiddlePoint1, StartPoint1, FinishPoint1, DogruP1, DogruP2);
        intersection(MiddlePoint2, StartPoint2, FinishPoint2, DogruP1, DogruP2);

        //NetcadMath.intersection(MiddlePoint1, StartPoint1, FinishPoint1, DogruP1, DogruP2);
        //NetcadMath.intersection(MiddlePoint2, StartPoint2, FinishPoint2, DogruP1, DogruP2);

        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint2);
        AddCoor(MinDikDortgenParcaAlanPoly,MiddlePoint1);
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[1]);
        AddCoor(MinDikDortgenParcaAlanPoly,MinDikDortgen.Geometry.Points[2]);
      end;
      //MinDikDortgenParcaAlanPoly.SetDirection(True);//Yorum Satýrý
    //Yorum Satýrý
    //GPCComplexPolyOperation(GPC_INT, PolyAddObje(MinDikDortgenParcaAlanPoly).GetObjectAsComplexPline, PolyAddObje(PolySecim).GetObjectAsComplexPline, cp);
      tempcp := MinDikDortgenParcaAlanPoly;
      PolyClip(ClipTypeIntersection,PolySecim,MinDikDortgenParcaAlanPoly,cp);
    end;

////////////////////////////////////////////////////////////////////////////////

    //Yorum Satýrý
    //if cp.outs.num < 1 then Exit;
    //  ComplexPolyToSplitPolyShow(cp, TABAKA_ParselAln);
    _PolyArea := Abs(cp.Geometry.Points.Area);
//    ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, _PolyArea);
//    _PolyArea := RoundTo(_PolyArea,(-1*(Integer(Licad.Settings.Precision.Area))));
    if (cp.Geometry.Points.Count < 1) or (_PolyArea <= 0) then
    begin
      Result := nil;
      Exit;
    end;

//    cp := FazlaNoktalariSil2(cp);//Fazla kopya noktalarý siler.

    ComplexPolyToSplitPolyShow(cp);



    //Kesen noktalar 2den fazla ise birden fazla alan oluþuyor demektir.
    KalanPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D); //newpoly// Yorum Satýrý
    KalanPoly.Assign(PolySecim);// Yorum Satýrý

    if MClbIfrazYontemi.ItemIndex = 2 then
    begin
      MiddlePoint2.X := IcxNokta.X;
      MiddlePoint2.Y := IcxNokta.Y;
      MiddlePoint1.X := coor1.X;
      MiddlePoint1.Y := coor1.Y;
    end;

    tempcp := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
    tempcp2 := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
    tempcp.Geometry.Points.Clear;
    tempcp.Geometry.Points[0] := MiddlePoint1;
    tempcp.Geometry.Points[1] := MiddlePoint2;

    iV := Licad.CreateEntityFactory.MakeVector(_2D, 0);

    _IntersectionPoints(PolySecim.Geometry.Points, tempcp.Geometry.Points,
                        PolySecim.Geometry.Points.Count, tempcp.Geometry.Points.Count,
                        iV, itCrossTouch,False,False);

    tempcp.Geometry.Points.Clear;
    WhileCount := 0;
    tempiV := iV;
    while (iV.Count < 2) and (WhileCount < 5) do
    begin
      //
      Inc(WhileCount);
      Polar(MiddlePoint1,Angle2DS(MiddlePoint1,MiddlePoint2),-1,MiddlePoint1);
      Polar(MiddlePoint2,Angle2DS(MiddlePoint1,MiddlePoint2),1,MiddlePoint2);

      tempcp.Geometry.Points.Clear;
      tempcp.Geometry.Points[0] := MiddlePoint1;
      tempcp.Geometry.Points[1] := MiddlePoint2;

      iV := Licad.CreateEntityFactory.MakeVector(_2D, 0);

      _IntersectionPoints(PolySecim.Geometry.Points, tempcp.Geometry.Points,
                          PolySecim.Geometry.Points.Count, tempcp.Geometry.Points.Count,
                          iV, itCrossTouch,False,False);

      tempcp.Geometry.Points.Clear;
      if iV.Count = 0 then
      begin
        iV := tempiV;
        Break;
      end;
    end;

    KesebilecekNoktalariniBul(iV, cp, tempcp);

//    if MClbIfrazYontemi.ItemIndex = 2 then
//    begin
//      polyClipOp(pcDiff,PolySecim,cp,tempcpkalan);
//      KalanPoly := FazlaNoktalariSil1(tempcpkalan, cp, tempcp);
//      KalanPoly := FazlaNoktalariSil2(KalanPoly);//Fazla kopya noktalarý siler.
//    end;

    //if (FazlaNoktalariSilDublicated(PolySecim, True) <> nil) then
    //  KalanPoly := FazlaNoktalariSilDublicated(KalanPoly);

    cpList := TlicgEntityList.Create;
    EntCount := 0;
    InPartPointCount := 0;

    if cp.Geometry.Points.PartCount > 1 then
    begin
      for I := 0 to tempcp.Geometry.Points.Count - 1 do
      begin

        if not hasPointEntPart(PolySecim,tempcp.Geometry.Points[I]) then
          Inc(InPartPointCount);
      end;
    end;

    if (tempcp.Geometry.Points.Count - InPartPointCount > 2) then
    begin

      sameFlag := False;
      coor1 := cp.Geometry.Points[0];
      for I := 1 to cp.Geometry.Points.Count - 1 do
      begin
        if sameFlag then
        begin
          sameFlag := False;
          Continue;
        end;
        EntCount := EntCount + 1;
        if EqualPoint2D(coor1,cp.Geometry.Points[I]) then
        begin
          if (I = (cp.Geometry.Points.Count - 1)) then
          begin

          //coor2 := cp.CenterOfMass;     
          //if not PolySecim.isPointInsideMe(coor2.X, coor2.Y) and (PolySecim.Geometry.Points.PartCount <= 1) then //and (PolySecim.Geometry.Points.Area > 0)
            if not cp.Intersect(PolySecim) then // ilker deðiþtirme
            //if not cp.IsInsideEntity(PolySecim, False) then // ilker silme ileride kaldýr.
            begin
              Application.MessageBox(PChar('Ýstenilen Deðerde Alan Oluþturulamadý. ' + #13 +
                                     'Daha Büyük Alan Giriniz.'), 'Bilgi',
                                     MB_OK + MB_ICONINFORMATION);
                Exit;
            end;

            if (not LayerIsVisibleIsLock(TABAKA_ParselAln)) then
              Exit;

            AdaParselNo := TfmParselNoGirisi.Execute('Yeni Parsel No Giriþi,', GenelParselNo);
            if AdaParselNo = '' then
              Exit
            else
              GenelParselNo := AdaParselNo;
            ComplexPolyToSplitPoly(cp, TABAKA_ParselAln);//Yorum Satýrý
            Break;
          end;
          tempcpkalan := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
          SpliceEntity(cp,tempcpkalan,I,I-EntCount);
          cpList.Add(tempcpkalan);
          EntCount := 0;
          if (Abs(((tempcp.Geometry.Points.Count - InPartPointCount) / 2) - cpList.Count) = 1) then //Partlýlarda girmiyor. //and (Abs((tempcp.Geometry.Points.Count / 4) - cpList.Count) = 1)
          //or ((cp.Geometry.Points.PartCount > 1)  )
          begin
            tempcpkalan := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
            SpliceEntity(cp,tempcpkalan,(cp.Geometry.Points.Count - 1),(I+1));
            cpList.Add(tempcpkalan);
            Break;
          end
          else
          begin
            coor1 := cp.Geometry.Points[I+1];
            sameFlag := True;
          end;
        end;
      end;
      ExitCount := 0;
      for I := 0 to cpList.Count - 1 do
      begin
        //ComplexPolyToSplitPolyShow(cpList[I]);
        //2 alan oluþuyorsa yönünde sýkýntý var.
        //2 den fazla alanda çalýþmýyor.

        if (not LayerIsVisibleIsLock(TABAKA_ParselAln)) then
          Exit;

        AdaParselNo := TfmParselNoGirisi.Execute(PChar(IntToStr(I+1)+'. Yeni Parsel No Giriþi'), GenelParselNo);

        if AdaParselNo = '' then
        begin
          Inc(ExitCount);
          if ExitCount = cpList.Count then
          begin
            ExitCount := 0;
            Exit;
          end;
          Continue;
        end
        else
          GenelParselNo := AdaParselNo;
        ComplexPolyToSplitPoly(cpList[I], TABAKA_ParselAln);//Yorum Satýrý
      end;
    end
    else
    begin
      //polysecimin içinde olmasý gerek alanýn.
      //coor2 := cp.CenterOfMass;
      //if not PolySecim.isPointInsideMe(coor2.X, coor2.Y) and (PolySecim.Geometry.Points.PartCount <= 1) then //and (PolySecim.Geometry.Points.Area > 0)
      if not cp.Intersect(PolySecim) then // ilker deðiþtirme
      //if not cp.IsInsideEntity(PolySecim, False) then // ilker silme ileride kaldýr.
      begin
        Application.MessageBox(PChar('Ýstenilen Deðerde Alan Oluþturulamadý. ' + #13 +
        //FloatToStr(RoundTo(cp.Geometry.Points.Area,(-1*(Integer(Licad.Settings.Precision.Area))))) + #13 +
                               'Daha Büyük Alan Giriniz.'), 'Bilgi',
                               MB_OK + MB_ICONINFORMATION);

        //if I = MrNo then
          Exit;
      end;

      if (not LayerIsVisibleIsLock(TABAKA_ParselAln)) then
        Exit;
      
      AdaParselNo := TfmParselNoGirisi.Execute('Yeni Parsel No Giriþi,', GenelParselNo);
      if AdaParselNo = '' then
        Exit
      else
        GenelParselNo := AdaParselNo;
      ComplexPolyToSplitPoly(cp, TABAKA_ParselAln);//Yorum Satýrý
    end;


  // Blok Kalan Alanýn Bulunmasý
  //Result := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D); //newobject;// Yorum Satýrý
  //PolySecim.SetDirection(True);// Yorum Satýrý

//    KalanPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D); //newpoly// Yorum Satýrý
//    KalanPoly.Assign(PolySecim);// Yorum Satýrý

//----------------------------------------------------------------------------//
//Düzgün Çalýþmýyor Yapýlcak.
//1. Yöntem
//    polyClipOp(pcDiff,PolySecim,cp,tempcpkalan);//11
//    tempcp := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);
//    PolyClip(ClipTypeIntersection,PolySecim,MinDikdortgen,tempcpkalan);//1
//    polyClipOp(pcDiff,tempcpkalan,cp,KalanPoly);//1
//    polyClipOp(pcDiff,tempcpkalan,cp,KalanPoly);//2
    //Yeniden Düzenlendi. Ard arda ayný olan koordinatlar fazladan nokta oluþturuyordu.
//    KalanPoly := FazlaNoktalariSil3(tempcpkalan,cp);
//    KalanPoly := FazlaNoktalariSil2(tempcp);
//----------------------------------------------------------------------------//
//2. Yöntem
//    KalanPoly.Assign(tempcpkalan);//11
//    tempcp := PolySecim.SymmetricDifference(cp);
//    PolyClip(ClipTypeIntersection,PolySecim,tempcp,KalanPoly);
//    AddCoor(KalanPoly,AsCoor(iv[0].X,iv[0].Y));
//    AddCoor(KalanPoly,AsCoor(iv[1].X,iv[1].Y));
//----------------------------------------------------------------------------//
//3. Yöntem
    {
    if MClbIfrazYontemi.ItemIndex = 2 then
    begin
      MiddlePoint2.X := IcxNokta.X;
      MiddlePoint2.Y := IcxNokta.Y;
      MiddlePoint1.X := coor1.X;
      MiddlePoint1.Y := coor1.Y;
    end;

    tempcp := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
    tempcp2 := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
    tempcp.Geometry.Points.Clear;
    tempcp.Geometry.Points[0] := MiddlePoint1;
    tempcp.Geometry.Points[1] := MiddlePoint2;

    iV := Licad.CreateEntityFactory.MakeVector(_2D, 0);

    _IntersectionPoints(PolySecim.Geometry.Points, tempcp.Geometry.Points,
                        PolySecim.Geometry.Points.Count, tempcp.Geometry.Points.Count,
                        iV, itCrossTouch,False,False);

    tempcp.Geometry.Points.Clear;
//    tempcp.Geometry.Points[0] := iV[0];
//    tempcp.Geometry.Points[1] := iV[1];
//    JoinEnt(tempcp);

    KesebilecekNoktalariniBul(iV, cp, tempcp);

//    KalanPoly.Assign(tempcpkalan);

//    tempcp.Geometry.Points[0] := TempMidPoint1;
//    tempcp.Geometry.Points[1] := TempMidPoint2;

    KalanPoly := FazlaNoktalariSil3(tempcpkalan, cp, tempcp);
    }
{
    if EntList = nil then
      EntList := TlicgEntityList.Create
    else
      EntList.Clear;

    EntList := CutPolyWithLine(PolySecim,tempcp);

    if EntList.Count <= 0 then
    begin
      if MClbIfrazYontemi.ItemIndex = 2 then
        Polar(MiddlePoint1,Angle2DS(MiddlePoint1,MiddlePoint2),_Distance(MiddlePoint1,MiddlePoint2),MiddlePoint2)
      else
      begin
        MiddlePoint1.X := RoundTo(MiddlePoint1.X,(-1*(Integer(Licad.Settings.Precision.Coordinate))));
        MiddlePoint1.Y := RoundTo(MiddlePoint1.Y,(-1*(Integer(Licad.Settings.Precision.Coordinate))));
        MiddlePoint2.X := RoundTo(MiddlePoint2.X,(-1*(Integer(Licad.Settings.Precision.Coordinate))));
        MiddlePoint2.Y := RoundTo(MiddlePoint2.Y,(-1*(Integer(Licad.Settings.Precision.Coordinate))));
      end;
      tempcp.Geometry.Points[0] := MiddlePoint1;
      tempcp.Geometry.Points[1] := MiddlePoint2;
      EntList := CutPolyWithLine(PolySecim,tempcp);

      if EntList.Count <= 0 then
        Exit;
    end;
    KalanPoly.Geometry.Points.Clear;
    KalanPoly.Assign(EntList[0]);
    coor2 := EntList[0].CenterOfMass;
    if cp.IsPointInsideMe(coor2.X,coor2.Y) then
    begin
      KalanPoly.Assign(EntList[1]);
//      cp := EntList[0];
    end;
//    else
//      cp := EntList[1];
}
//----------------------------------------------------------------------------//

//    if ((KalanPoly.Geometry.Points.Area + cp.Geometry.Points.Area + 0.00001) < PolySecim.Geometry.Points.Area) or
//      (KalanPoly.Geometry.Points.Area + cp.Geometry.Points.Area > (PolySecim.Geometry.Points.Area) + 0.00001) then
//    begin
//      polyClipOp(pcDiff,PolySecim,cp,tempcpkalan);//11
//      KalanPoly.Assign(tempcpkalan);//11
//      FazlaNoktalariSil4;
//    end;

    {
    FIndex1 := 0;
    FIndex2 := 0;
    SIndex1 := 0;
    SIndex2 := 0;
    if MClbIfrazYontemi.ItemIndex = 2 then
    begin
    //çýkarma yöntemi nokta dolaþýyor.
//      Tempcp2 := PolySirali;
//      for I := 0 to tempcp.Geometry.Points.Count - 1 do
//      begin
//        if not isPointExistEnt(Tempcp2,tempcp.Geometry.Points[I]) then
//        begin
//          Tempcp2.Geometry.Points.AddPoints([Tempcp2.Geometry.Points[0]]);
//          Tempcp2.Geometry.Points[0] := tempcp.Geometry.Points[I];
//        end;
//      end;
//
//      tempcp.Geometry.Points.Clear;
//      //partlý sistem
//      if Tempcp2.Geometry.Points.PartCount > 1 then
//      begin
//
//      end;
//      KalanPoly := FazlaNoktalariSil1(Tempcp2, cp, tempcp);

      TempMinDikDortgen.Geometry.Points.Clear;
      TempMinDikDortgen := DikdortgenOlustur(PolySecim,MiddlePoint1,MiddlePoint2);
      JoinEnt(TempMinDikDortgen);
      iv.Clear;
      iv[0] := TempMinDikDortgen.Geometry.Points[0];
      iv[1] := TempMinDikDortgen.Geometry.Points[3];
      if EqualPoint2D(GetNearestPoint(iv,MiddlePoint1),TempMinDikDortgen.Geometry.Points[3]) then
      begin
        MiddlePoint2 := Perpend(MiddlePoint1,TempMinDikDortgen.Geometry.Points[0],TempMinDikDortgen.Geometry.Points[1]);
        MiddlePoint1 := Perpend(MiddlePoint2,TempMinDikDortgen.Geometry.Points[2],TempMinDikDortgen.Geometry.Points[3]);
;        tersAlAlan yönünü Sabit Noktada
      end
      else
      begin
        MiddlePoint1 := Perpend(MiddlePoint1,TempMinDikDortgen.Geometry.Points[3],TempMinDikDortgen.Geometry.Points[2]);
        MiddlePoint2 := Perpend(MiddlePoint2,TempMinDikDortgen.Geometry.Points[0],TempMinDikDortgen.Geometry.Points[1]);
      end;
//
      MiddlePoint1 := Perpend(MiddlePoint1,TempMinDikDortgen.Geometry.Points[3],TempMinDikDortgen.Geometry.Points[2]);
      MiddlePoint2 := Perpend(MiddlePoint2,TempMinDikDortgen.Geometry.Points[0],TempMinDikDortgen.Geometry.Points[1]);
//
      FIndex1 := 0;
      FIndex2 := 3;
      SIndex1 := 1;
      SIndex2 := 2;
    end
    }
    {
    //Sabit Nokta haricindekiler çalýþýyor.(2 ye bölme yöntemi)
    if MClbIfrazYontemi.ItemIndex <> 2 then
    begin
      FIndex1 := 2;
      FIndex2 := 1;
      SIndex1 := 3;
      SIndex2 := 0;
      Tempcp2.Geometry.Points.AddPoints([MiddlePoint1]);
      Tempcp2.Geometry.Points.AddPoints([MiddlePoint2]);
      if AlanYonu = 0 then
      begin
        Tempcp2.Geometry.Points.AddPoints([TempMinDikDortgen.Geometry.Points[FIndex1]]);
        Tempcp2.Geometry.Points.AddPoints([TempMinDikDortgen.Geometry.Points[FIndex2]]);
        Tempcp2.Geometry.Points.AddPoints([MiddlePoint1]);
      end
      else
      begin
        Tempcp2.Geometry.Points.AddPoints([TempMinDikDortgen.Geometry.Points[SIndex1]]);
        Tempcp2.Geometry.Points.AddPoints([TempMinDikDortgen.Geometry.Points[SIndex2]]);
        Tempcp2.Geometry.Points.AddPoints([MiddlePoint1]);
      end;
      PolyClip(ClipTypeIntersection,PolySecim,Tempcp2,KalanPoly);
    end;
    }

    PolyClip(ClipTypeDifference,PolySecim,cp,KalanPoly);

    _PolyArea := Abs(KalanPoly.Geometry.Points.Area);
//    ComplexEndeksHesaplaComplexPoly(cp, TABAKA_Endeks, Alan, Endeks, _PolyArea);
    if _PolyArea > 0 then
    begin

      if (not LayerIsVisibleIsLock(TABAKA_Parsel)) then
        Exit;

//      CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
      KalanPoly.Name := 'GK';//+AdaParselNo;
      Rc := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_Parsel).AddEntity(KalanPoly,True,True);
      CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_Parsel).LayerInfo.PolygonNameOnOFF := False;
      CurrCmdLine.ActiveDrawBox.RepaintExtent(KalanPoly.Geometry.Extent);
      CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TABAKA_Parsel), Rc, uaDelete);
//      CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
    end
    else
      KalanPoly := nil;

//    for I := 0 to cp.outs.num - 1 do // Deliksiz
//    begin
//    //Yapýlacak
//      KalanAlanCollection := KalanPoly.Operation(subtract, cp.outs.get(I));
//      KalanAlanCollection.GetObject(0, Result, KalanPoly);
//    end;

    Result := KalanPoly;

    //MakePline('GK', POLYCLOSED + POLYNAME + POLYFILLED, 0, FoundLayer('@PARSEL'), 0, 0, KalanPoly);// Yorum Satýrý
    //AddComplexPoly(Result, PolyToComplexPoly(KalanPoly));// Yorum Satýrý

  finally
    MinDikDortgen := nil;
    MinDikDortgenParcaAlanPoly := nil;
    KalanPoly := nil;
    cp := nil;
//    KalanAlanCollection := nil;
  end;
  end;
end;

procedure TIfrazLauncher.ComplexPolyToSplitPoly(cpp: IlicgEntity; TabakaAdi: String);
var
  px: IlicgEntity;
  Obje: IlicgEntity;
  FontIndex, Rc: Integer;
  YaziParselNo: string;
  YaziNoktasi: TlicgCoor;
  AdaNo, ParselNo: String;
  FFont: TlicgFontStyle;
begin
  FontIndex := 1;//NetcadFontNameIndex(1);//dm.tbGenelAyarlarYAZITIPI.AsInteger);
  FFont.Name := 'Arial';
  if FontIndex < 0 then
  begin
    Application.MessageBox('Kullandýðýnýz CAD yazýlýmda tanýmlý Yazý Tipi yok. Lütfen Yazý Tipi ekleyiniz.', 'Hata', MB_OK + MB_ICONERROR);
    Exit;
  end;
  try
    px := Licad.CreateEntityFactory.MakeEntity(idPolygon, 2, _2D);//newpoly;
    Obje := Licad.CreateEntityFactory.MakeEntity(idText, 2, _2D);//newobject;

    AdaNo := KadastroAdaNoGetir(GenelParselNo);

    ParselNo := IntToStr(StrToInt64Def(KadastroParselNoGetir(GenelParselNo), -1));
    if StrToInt(ParselNo) > -1 then
      ParselNo := IntToStr(StrToInt(ParselNo) - 1);

    px.Assign(cpp);
//    px := FazlaNoktalariSil(cp);
    if (px.Geometry.Points.Count < 1) or (RoundTo(Abs(px.Geometry.Points.Area),(-1*(Integer(Licad.Settings.Precision.Area)))) = 0) then
      Exit;
    if StrToInt(ParselNo) > -1 then
    begin
      ParselNo := IntToStr(StrToInt(ParselNo) + 1);
      GenelParselNo := AdaNo+'/'+ParselNo;
    end;

    if (not LayerIsVisibleIsLock(TabakaAdi)) then
      Exit;

//    CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);

    px.Name := GenelParselNo;
    Rc := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi).AddEntity(px,True,True);
    CurrCmdLine.ActiveDrawBox.RepaintExtent(px.Geometry.Extent);
    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi), Rc, uaDelete);

    //Obje := MakePline(GenelParselNo, POLYCLOSED + POLYNAME + POLYFILLED, 0, FoundLayer(TabakaAdi), 0, 0, px);
    //AddObject(Obje);

    if fmIfraz.chkParselNoYaz.Checked then
    begin
      if fmIfraz.chkAdaNoYaz.Checked then
        YaziParselNo := GenelParselNo
      else
        YaziParselNo := KadastroParselNoGetir(GenelParselNo);
      if fmIfraz.chkParselinOrtaNoktasinaYaz.Checked then
      begin
        YaziNoktasi := px.CenterOfMass;
        //Polar(px.CenterOfMass,0,seYaziBoyu.Value,YaziNoktasi2);

        Obje := Licad.CreateEntityFactory.MakeText(YaziNoktasi, YaziParselNo);
        Obje.DrawTools.FontTool.Height := (fmIfraz.seYaziBoyu.Value * CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000);
        Obje.DrawTools.FontTool.Angle := 0;
        //CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);

        if (not LayerIsVisibleIsLock(TabakaAdi)) then
          Exit;

        Obje.Name := GenelParselNo;
        AslicgTextValue(Obje.Geometry).Text := YaziParselNo;
        Rc := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi).AddEntity(Obje,True,True);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(Obje.Geometry.Extent);
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi), Rc, uaDelete);
        //CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
      end
      else
      begin
        YaziNoktasi := AsCoor(0,0);
        SelectPoint('Parsel No için bir yer gösteriniz', YaziNoktasi, -1);
        //Polar(YaziNoktasi,0,seYaziBoyu.Value,YaziNoktasi2);

        //Obje := Licad.CreateEntityFactory.MakeText(YaziParselNo,lpCenter,0, YaziNoktasi,YaziNoktasi2,FFont,True);
        Obje := Licad.CreateEntityFactory.MakeText(YaziNoktasi, YaziParselNo);
        Obje.DrawTools.FontTool.Height := (fmIfraz.seYaziBoyu.Value * CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000);
        Obje.DrawTools.FontTool.Angle := 0;

        //CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);

        if (not LayerIsVisibleIsLock(TabakaAdi)) then
          Exit;

        Obje.Name := GenelParselNo;
        AslicgTextValue(Obje.Geometry).Text := YaziParselNo;
        Rc := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi).AddEntity(Obje,True,True);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(Obje.Geometry.Extent);
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TabakaAdi), Rc, uaDelete);
        //CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
      end;
    end;
//    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  finally
    Obje := nil;
    px := nil;
  end;
end;

procedure TIfrazLauncher.ComplexPolyToSplitPolyShow(cp: IlicgEntity);
var
  px: IlicgEntity;
begin
  try
    px := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);//newpoly;
    px.Geometry.Points.Assign(cp.Geometry.Points);//FazlaNoktalariSil(cp);
    if (px.Geometry.Points.Count < 1) or (Abs(px.Geometry.Points.Area) <= 0) then
      Exit;
    CurrCmdLine.All_DrawEntity2DRubberBand(px);
  finally
    px := nil;
  end;
end;

function TIfrazLauncher.ComplexEndeksHesaplaComplexPoly(Acp: IlicgEntity; EndeksTabakAdi: string; out Alan, Endeks, DegerSayisi: Double): Boolean;
var
  I: Integer;
//  EndeksObje: IlicgEntity;//NetCadObj;
  cp: IlicgEntity;//NCComplexPoly;
  E: IlicgEntity;
  EntName: Double;

  recnoList: IlicgIntegerList;
  box: TlicgExtent;
  _L: TlicgBaseLayer;
  ol: IlicgIntegerList;
//  px: IlicgEntity;//NCPline;
begin
//  with Netcad do
//  begin
  try
    Alan := 0;
    Endeks := 0;
    DegerSayisi := 0;
    Result := False;
    //EndeksObje := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);
    cp := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);

    {
    px := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);
    for I := 0 to Acp.outs.num - 1 do
    begin
      for J := 0 to Acp.outs.get(I).num - 1 do
      begin
        px.AddCoor(Acp.outs.get(I).Cor[J]);
      end;
    end;
    }

//      SetFilter(px.Limits, VarArrayOf([FoundLayer(EndeksTabakAdi)]), VarArrayOf([opline]));

//      while GetNextObject2(EndeksObje) do
//      begin

    {
    L := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(EndeksTabakAdi);
    L.First;
    while not L.Eof do
    begin
      if L.RecIsDeleted then
      begin
        L.Next;
        Continue;
      end;
      E := L.LoadEntityWithRecNo(L.Recno);
      if (E <> nil) and (E.EntityID = idPolygon) then
      begin
      //if GPCComplexPolyOperation(GPC_INT, Acp, EndeksObje.GetObjectAsComplexPline, cp) then
        if PolyClip(ClipTypeIntersection, E, Acp, cp) then
        begin
          if not TryStrToFloat(E.name, EntName) then
          begin
            Application.MessageBox(PChar('Endeks katmanýndaki kapalý alan ismi deðer olmalý.' + #13 +
                           'Obje ismi Deðer Sayýsý Ýçin Ýfraz Ýþlemi Yapýlamaz.'), 'Bilgi', MB_OK + MB_ICONINFORMATION);
            Exit;
          end;
          Alan := Alan + (Abs(cp.Geometry.Points.Area));//ComplexPolyArea(cp);
          DegerSayisi := DegerSayisi + (Abs(cp.Geometry.Points.Area) * StrToFloat(E.name));//(ComplexPolyArea(cp) * StrToFloat(EndeksObje.pname));
        end;
      end;
      L.Next;
      E := nil;
      cp := nil;
    end;
    }

    try
      ol := CreateIntegerList;
      recnoList := CreateIntegerList;
      box := Acp.Geometry.Extent;
      _L := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(EndeksTabakAdi);
      _L.StartBuffering;
        try
        _L.rt.StartSearch;
        _L.rt.Search(stoverlap,box,ol,10);
          if ol.Count > 0 then
          begin
            for I := 0 to ol.Count-1 do
            begin
            E := _L.LoadEntityWithRecNo(ol.Items[I]);
              if E.EntityID = idPolygon then
              begin
                if PolyClip(ClipTypeIntersection, E, Acp, cp) then
                begin
                  if not TryStrToFloat(E.name, EntName) then
                  begin
                    Application.MessageBox(PChar('Endeks katmanýndaki kapalý alan ismi deðer olmalý.' + #13 +
                                   'Obje ismi Deðer Sayýsý Ýçin Ýfraz Ýþlemi Yapýlamaz.'), 'Bilgi', MB_OK + MB_ICONINFORMATION);
                    Exit;
                  end;
                  Alan := Alan + (Abs(cp.Geometry.Points.Area));//ComplexPolyArea(cp);
                  DegerSayisi := DegerSayisi + (Abs(cp.Geometry.Points.Area) * StrToFloat(E.name));//(ComplexPolyArea(cp) * StrToFloat(EndeksObje.pname));
                end;
              end
            end;
          end;
        finally
        _L.rt.FinishSearch;
        end;
      _L.EndBuffering;
    finally
      recnoList.clear;
    end;

    //ResetFilter;
    //Endeks := DegerSayisi / Alan; // Benim Yaptýðým  Kesisen Alan Toplamýna Göre
    if (Abs(Acp.Geometry.Points.Area)) = 0 then
      Exit;

    Endeks := DegerSayisi / (Abs(Acp.Geometry.Points.Area));//ComplexPolyArea(Acp); // Netcad Yaptýðý Seçilen Alana Göre Buluyor.
    if Endeks > 0 then
      Result := True;
  finally
  //EndeksObje := nil;
    cp := nil;
  end;
//  end;
end;

{$endregion}
//Dahili Func ve Proc BÝTÝÞ/////////////////////////////////////////////////////
end.
