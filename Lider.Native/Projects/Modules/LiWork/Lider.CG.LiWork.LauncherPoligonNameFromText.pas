unit Lider.CG.LiWork.LauncherPoligonNameFromText;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  Graphics,
  Math,
  Forms,
  System.Generics.Collections,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type TPoligonNameFromTextLauncher = class(TLicgLauncher)
  private
  FMetinList,FParselList: IlicgEntityList;
  FOldEntIDS, FNewEntIDS: TlicgEntityIDs;
  FCount: Integer;
    {Eventler}
   procedure LauncherFinished(Sender: TObject);
   procedure LauncherFinished2(Sender: TObject);
   procedure LauncherSuspendOperation(Sender: TObject);
   procedure LauncherContinueOperation(Sender: TObject);
  public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
end;

implementation

{ TDeleteLauncher }

constructor TPoligonNameFromTextLauncher.CreateLauncher(CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  Launcher.TrackQuickSelect(SCmdLauncher);
  FOldEntIDS := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntIDS := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
  Launcher.CurrentAction.Caption:= 'Yazý nesnelerini seçiniz';
  Launcher.OnFinished := LauncherFinished;
  Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
end;

destructor TPoligonNameFromTextLauncher.Destroy;
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntIDS;
  CmdLine.ActiveDrawBox.Selection.Clear;
  FParselList := nil;
  FMetinList := nil;
  inherited;
end;

procedure TPoligonNameFromTextLauncher.LauncherContinueOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
end;

function Yakala(FParselList,FMetinList: IlicgEntityList): Boolean;                                //FONKSÝYON
var
  I,J,ParselNo,MetinNo: integer;
  Kontrol: Boolean;
  Fark, MinUzaklik: Double;
  SolAlt,SagUst,Ort: TlicgCoor;
begin
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  for I := 0 to FParselList.Count-1 do
  begin
    MinUzaklik := 0;
    Fark :=0;
    for J := 0 to FMetinList.Count-1 do
    begin
      Kontrol := FParselList[I].Geometry.InPoly(AsCoor(FMetinList[J].Geometry.Points.X[0], FMetinList[J].Geometry.Points.Y[0]));
      if Kontrol = True then
      begin
        SolAlt := FMetinList[J].Geometry.Extent.LowerLeft;
        SagUst := FMetinList[J].Geometry.Extent.UpperRight;
        Ort.X := (SolAlt.X + SagUst.X) / 2;
        Ort.Y := (SolAlt.Y + SagUst.Y) / 2;
        if (MinUzaklik = 0) and (Fark = 0) then
        begin
          MinUzaklik := _Distance(FParselList[I].Geometry.Centroid, Ort);
        end;
          Fark := _Distance(FParselList[I].Geometry.Centroid,Ort);
        if (MinUzaklik >= Fark)then
        begin
          MinUzaklik := Fark;
          ParselNo := I;
          MetinNo := J;
        end
        else
        Continue;
      end
      else
      Continue;
    end;
     FParselList[ParselNo].Name := FMetinList[MetinNo].AsTextValue.Text;
     CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TlicgBaseLayer(FParselList[ParselNo].Layer), FParselList[ParselNo].Geometry.ID, uauntransform);
     TlicgBaseLayer(FParselList[ParselNo].Layer).UpdateEntity(FParselList[ParselNo].Geometry.ID, FParselList[ParselNo]);
     CurrCmdLine.ActiveDrawBox.RepaintExtent(FParselList[ParselNo].Extent);
  end;
  CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
end;

procedure TPoligonNameFromTextLauncher.LauncherFinished(Sender: TObject);
var
  Rc, I, J: Integer;
  TempLayer: TLicgBaseLayer;
  Ent: ILicgEntity;
  Key: Char;
begin
  if CmdLine.ActiveDrawBox.Selection.Count > 0 then
  begin
    FMetinList := TlicgEntityList.Create;
    with CmdLine.ActiveDrawBox do
    for I := 0 to Selection.Count - 1 do
    begin
      for J := 0 to Selection[I].SelList.Count - 1 do
      begin
        TempLayer := TLicgBaseLayer(Selection[I].Layer);
        Rc := Selection[I].SelList[J];
        Ent := TempLayer.LoadEntityWithRecNo(Rc);
        if (Ent.EntityID in [idText, idVectorialText]) then
          FMetinList.add(Selection[I].Layer.LoadEntityWithRecNo(Selection[I].SelList[J]));
      end;
    end;
    if FMetinList = nil then
    begin
      Self.Free;
      exit;
    end;
    CmdLine.ActiveDrawBox.Selection.Clear;
    Ent := nil;
    inherited CreateLauncher(CmdLine);                        //yeni launcher
    Launcher.TrackQuickSelect(SCmdLauncher);
    FNewEntIDS :=  AllEntityIDs - [idPolygon];
    CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
    Launcher.CurrentAction.Caption:= 'Parselleri seçiniz';
    Launcher.OnFinished := LauncherFinished2;
    Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
    Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
  end;
end;

procedure TPoligonNameFromTextLauncher.LauncherFinished2(Sender: TObject);
var
  Rc, I, J: Integer;
  TempLayer: TLicgBaseLayer;
  Ent: ILicgEntity;
begin
  if FMetinList <> nil then
  begin
    FParselList := TlicgEntityList.Create;
    with CmdLine.ActiveDrawBox do
    for I := 0 to Selection.Count - 1 do
    begin
      for J := 0 to Selection[I].SelList.Count - 1 do
      begin
        TempLayer := TLicgBaseLayer(Selection[I].Layer);
        Rc := Selection[I].SelList[J];
        Ent := TempLayer.LoadEntityWithRecNo(Rc);
        if (Ent.EntityID in [idPolygon]) then
          FParselList.add(Selection[I].Layer.LoadEntityWithRecNo(Selection[I].SelList[J]));
      end;
    end;
    if (FMetinList <> nil) and (FParselList <> nil) then
      Yakala(FParselList, FMetinList);
    CmdLine.ActiveDrawBox.Selection.Clear;
    Self.Free;
    Ent := nil;
  end;
end;

procedure TPoligonNameFromTextLauncher.LauncherSuspendOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntIDS;
end;
end.
