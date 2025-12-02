unit Lider.CG.LiWork.LauncherUnionSameNamedPolys;

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
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TEntModel = class(TObject)
  private
    PolyName: String;
    EntList: ILicgEntityList;
    procedure AddItem(Ent: ILicgEntity);
  public
    Constructor Create(PName: String);
    function Copy: TEntModel;
  end;

type TUnionSameNamedPolysLauncher = class(TLicgLauncher)
  private
    FOldEntIDS, FNewEntIDS: TlicgEntityIDs;
    function IsContains(EmList: TList<TEntModel>; PName: String; Var Index: Integer): Boolean;
    procedure SaveAllEnts(EntList, SelectedEnts: ILicgEntityList);
    function UnoionAllEnts(EMList: TList<TEntModel>): ILicgEntitylist;
    function GetAllEnts(Sel: TLicgBaseSelection): ILicgEntityList;
    function ClassifyAllEnts(EntList: ILicgEntityList): TList<TEntModel>;
    {Eventler}
   procedure LauncherFinished(Sender: TObject);
   procedure LauncherSuspendOperation(Sender: TObject);
   procedure LauncherContinueOperation(Sender: TObject);
  public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
end;

implementation

uses
  Gosterge,
  Lider.CG.Com.System,
  Lider.CG.Details.AlanBirlestirSecenekleri,
  Lider.CG.Com.Lib,
  Lider.CG.Com.LicadInt;

{ TUnionSameNamedPolysLauncher }

function TUnionSameNamedPolysLauncher.ClassifyAllEnts(
  EntList: ILicgEntityList): TList<TEntModel>;
var
  I, Index: Integer;
begin
  Result := TList<TEntModel>.Create;
  for I := 0 to EntList.Count - 1 do
  begin
    if Result.Count = 0 then
    begin
      Result.Add(TEntModel.Create(EntList[I].Name));
      Result[0].AddItem(EntList[I]);
    end
    else
    begin
      if IsContains(Result, EntList[I].Name, Index) then
        Result[Index].AddItem(EntList[I])
      else
      begin
        Result.Add(TEntModel.Create(EntList[I].Name));
        Result[Result.Count - 1].AddItem(EntList[I]);
      end;
    end;
  end;
end;

constructor TUnionSameNamedPolysLauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  Launcher.TrackQuickSelect(SCmdLauncher);
  FOldEntIDS := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntIDS := AllEntityIDs - [idPolygon];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
  CmdLine.CurrentAction.Caption := 'Birleþtirilecek alanlarý seçiniz';
  Launcher.OnFinished := LauncherFinished;
  Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
end;

destructor TUnionSameNamedPolysLauncher.Destroy;
begin
  if Assigned(fmAlanBirlestirSecenekleri) then
    FreeAndNil(fmAlanBirlestirSecenekleri);
  inherited;
end;

function TUnionSameNamedPolysLauncher.GetAllEnts(Sel: TLicgBaseSelection): ILicgEntityList;
var
  I, J: Integer;
  TempEnt: ILicgEntity;
begin
  Result := TLicgEntityList.Create;
  for I := 0 to Sel.Count - 1 do
  begin
    for J := 0 to Sel[I].SelList.Count - 1 do
    begin
      TempEnt := Sel[I].Layer.LoadEntityWithRecNo(Sel[I].SelList[J]);
      if TempEnt.EntityID in FNewEntIDS then
        Continue;
      Result.Add(Sel[I].Layer.LoadEntityWithRecNo(Sel[I].SelList[J]));
    end;
  end;

  TempEnt := nil;
end;

function TUnionSameNamedPolysLauncher.IsContains(EmList: TList<TEntModel>;
  PName: String; var Index: Integer): Boolean;
var
  I: Integer;
begin
  Index := -1;
  Result := False;
  for I := 0 to EMList.Count - 1 do
  begin
    if PName.Equals(EMList[I].PolyName) then
    begin
      Index := I;
      Result := True;
      Break;
    end;
  end;
end;

procedure TUnionSameNamedPolysLauncher.LauncherContinueOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
end;

procedure TUnionSameNamedPolysLauncher.LauncherFinished(Sender: TObject);
var
  UnionList,  SelectedEntList: ILicgEntityList;
  EmList: TList<TEntModel>;
begin
  if CmdLine.ActiveDrawBox.Selection.Count > 0 then
  begin
    fmAlanBirlestirSecenekleri := TfmAlanBirlestirSecenekleri.Create(nil);
    fmAlanBirlestirSecenekleri.ShowModal;
    if fmAlanBirlestirSecenekleri.ModalResult = mrOk then
    begin
      try
        CmdLine.ActiveDrawBox.SetFocus;
        CmdLine.ActiveDrawBox.Cursor := crHourGlass;
        CmdLine.CurrentAction.Caption := 'Birleþtirme iþlemi uygulanýyor...';
        SelectedEntList := GetAllEnts(CmdLine.ActiveDrawBox.Selection);
        if SelectedEntList.Count > 0 then
        begin
          EMList := ClassifyAllEnts(SelectedEntList);
          UnionList := UnoionAllEnts(EMList);
          SaveAllEnts(UnionList, SelectedEntList);
        end;
        UnionList := nil;
        if Assigned(EMList) then
          FreeAndNil(EMList);
        CmdLine.ActiveDrawBox.Cursor := crDefault;
        finally
        begin
          UnionList := nil;
          SelectedEntList := nil;
        end;
       end;
    end;

  end;
  CmdLine.ActiveDrawBox.Selection.Clear;
  Self.Free;
end;

procedure TUnionSameNamedPolysLauncher.LauncherSuspendOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntIDS;
end;

procedure TUnionSameNamedPolysLauncher.SaveAllEnts(EntList, SelectedEnts: ILicgEntityList);
var
  I, Rc: Integer;
  Ext: TlicgExtent;
begin
  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  if Not fmAlanBirlestirSecenekleri.chkBirlestirilenleriKoru.Checked then
  begin
    for I := 0 to SelectedEnts.Count - 1 do
    begin
      CmdLine.ActiveDrawBox.Undo.AddUndo(
        TlicgBaseLayer(SelectedEnts[I].Layer), SelectedEnts[I].Geometry.ID, uaUndelete);
      TlicgBaseLayer(SelectedEnts[I].Layer).DeleteEntity(SelectedEnts[I].Geometry.ID);
    end;
  end;

  for I := 0 to EntList.Count - 1 do
  begin
    Rc := CmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(EntList[I]);
    if Rc > 0 then
      CmdLine.ActiveDrawBox.Undo.AddUndo(CmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uaDelete);
    if I = 0 then
      Ext := EntList[I].Extent
    else
      CalcMaxMinBounds(Ext, EntList[I].Extent);
  end;

  CmdLine.ActiveDrawBox.RepaintExtent(Ext);
  CmdLine.ActiveDrawBox.Undo.EndUndo;
end;

function TUnionSameNamedPolysLauncher.UnoionAllEnts(
  EMList: TList<TEntModel>): ILicgEntitylist;
var
  I, J: Integer;
  ATop: IlicgTopology;
  NewEnt: ILicgEntity;
  Str: String;
begin
  ATop := Licad.CreateTopology;
  ATop.Tolerance := fmAlanBirlestirSecenekleri.spTolerance.Value;
  Result := TLicgEntityList.Create;
  for I := 0 to EMList.Count - 1 do
  begin
    try
      NewEnt := ATop.UnionOnList(EMList[I].EntList);
      if NewEnt <> nil then
      begin
        NewEnt.Name := EMList[I].PolyName;
        NewEnt.DrawTools.Assign(EMList[I].EntList[0].DrawTools);
        Result.Add(NewEnt);
      end;
    except on E: Exception do
      begin
        for J := 0 to EMList[I].EntList.Count - 1 do
          Result.Add(EMList[I].EntList[J]);
        Str := E.ToString;
      end;
    end;
  end;
  NewEnt := nil;
end;

{ TEntModel }

procedure TEntModel.AddItem(Ent: ILicgEntity);
begin
  if Assigned(EntList) then
    EntList.Add(Ent);
end;

function TEntModel.Copy: TEntModel;
var
  I: Integer;
begin
  Result := TEntModel.Create(Self.PolyName);
  if Self.EntList.Count > 0 then
  begin
    for I := 0 to Self.EntList.Count - 1 do
      Result.EntList.Add(Self.EntList[I]);
  end;
end;

constructor TEntModel.Create(PName: String);
begin
  Self.EntList := TlicgEntityList.Create;
  Self.PolyName := PName;
end;

end.
