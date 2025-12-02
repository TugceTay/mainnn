unit Lider.CG.LiWork.LauncherPointFromText;

interface

uses
  Classes,
  Controls,
  SysUtils,
  Windows,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes;

type
  TPointFromTextLauncher = class(TlicgLauncher)
  private
    FCount:Integer;
    FPointEnt: IlicgEntity;
    FValY, FValX: Double;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    {Launcher Events}
    procedure LauncherFinished(Sender: TObject);
    procedure LauncherKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LauncherTrackedEntityClick(const TrackID: string; Button:
      TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double; Layer:
      TlicgBaseLayer; Recno: Integer; var Accept: Boolean);
    procedure LauncherSuspendOperation(Sender: TObject);
    procedure LauncherContinueOperation(Sender: TObject);
    public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
  end;

implementation

uses
  Lider.CG.Com.Consts,
  Lider.CG.Com.Utilities,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.LicadInt;

{ TPointFromTextLauncher }

constructor TPointFromTextLauncher.CreateLauncher(CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
  Launcher.TrackSelectEntity(SCmdLauncher);
  Launcher.OnTrackedEntityClick := LauncherTrackedEntityClick;
  Launcher.OnFinished := LauncherFinished;
  Launcher.OnKeyDown := LauncherKeyDown;
  Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
  Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  FPointEnt := Licad.CreateEntityFactory.MakeEntity(idPoint, 0, _3D);
  Launcher.CurrentAction.Caption := 'Koordinatýný oluþturacaðýnýz nesneyi seçiniz.';
  FCount := 0;
end;

destructor TPointFromTextLauncher.Destroy;
begin
  inherited;
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  FPointEnt := nil;
end;

procedure TPointFromTextLauncher.LauncherContinueOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
end;

procedure TPointFromTextLauncher.LauncherSuspendOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
end;

procedure TPointFromTextLauncher.LauncherFinished(Sender: TObject);
begin
  Self.Free; //Eðer yazmazsan destroya düþmez.
end;

procedure TPointFromTextLauncher.LauncherKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
    Launcher.Finish;
end;

procedure TPointFromTextLauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
var
  Rc: Integer;
  ValX, ValY,TempName : IlicgEntity;
begin
  if (layer = nil) or (Recno = 0) then
    Exit;
 if FCount = 0 then
  begin
    TempName := Layer.LoadEntityWithRecNo(Recno);
    FPointEnt.Name := TempName.AsTextValue.Text;
    Launcher.CurrentAction.Caption := 'Noktanýn Y koordinatýný seçiniz.'; //*
    Inc(FCount);
  end
  else if FCount = 1 then
  begin
    ValY := Layer.LoadEntityWithRecNo(RecNo); //Seçtiðimiz Nesneyi Çekiyoruz.
    if TryStrToFloat(ValY.AsTextValue.Text, FValY) then
    begin
      FPointEnt.Geometry.Points.X[0] := FValY;
      Launcher.CurrentAction.Caption := 'Noktanýn X koordinatýný seçiniz.'; //iþlem yaptýktan hemen sonra
      Inc(FCount);
    end;
    ValY := nil;
  end
  else if FCount = 2 then
  begin
    CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
    ValX := Layer.LoadEntityWithRecNo(RecNo); //Seçtiðimiz Nesneyi Çekiyoruz.
    if TryStrToFloat(ValX.AsTextValue.Text, FValX) then
    begin
      FPointEnt.Geometry.Points.Y[0] := FValX;
      FPointEnt.Geometry.Points.Z[0] := CalculateZ(CurrCmdLine, FPointEnt.Geometry.Points[0]);
      Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(FPointEnt);
      CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
      CurrCmdLine.ActiveDrawBox.RepaintExtent(InflateExtent(FPointEnt.Extent, 5));
      Launcher.CurrentAction.Caption := 'Koordinatýný oluþturacaðýnýz nesneyi seçiniz.';
      FCount := 0;
    end;
    ValX := nil;
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
end;
end.


