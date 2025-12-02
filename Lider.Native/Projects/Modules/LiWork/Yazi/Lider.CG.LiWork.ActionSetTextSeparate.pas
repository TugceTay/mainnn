unit Lider.CG.LiWork.ActionSetTextSeparate;

interface

uses
  Classes,
  Controls,
  SysUtils,
  Forms,
  System.Generics.Collections,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GeoTypes;

type
  TMyClass = class(TObject)
  private
    FEnt: ILicgEntity;
    FSeparatedEnts: ILicgEntityList;
    FRefP: TLicgCoor;
    FRowList: TList<Integer>;
  public
    constructor Create(Ent: ILicgEntity);
    destructor Destroy; override;
end;

type
 TSetTextSeparateAction = class(TlicgAction)
  private
    FIsAllSelect, FIsTextSeparated, FFinishFlag, FRefPSelected: Boolean;
    FIsCopyMood: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    FLineEnt: ILicgEntity;
    FAllRefP: TLicgCoor;
    FMyClassList: TList<TMyClass>;
    procedure DrawAllEnts(Ents: ILicgEntityList);
    function AssingingCoordinateToText(RefP: TLicgCoor; Ent: ILicgEntity;
      List: TList<Integer>; ListN:ILicgEntityList; S:Integer): IlicgEntityList;
    function SetTextSeparate(S: String): TList<Integer>;
    function CreateTextEntityList(List1: TList<Integer>;
      Text:IlicgEntity): ILicgEntityList;
    function StartQuickSelect(ACmdLine: TlicgBaseCmdLine;
      P: TLicgCoor; Sender: TObject; Shift: TShiftState): IlicgEntityList;
    procedure LauncherFinished(Sender: TObject);
    procedure StartTrackSelect(ACmdLine: TlicgBaseCmdLine);
    procedure LauncherTrackedEntityClick(const TrackID: string; Button:
      TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double; Layer:
      TlicgBaseLayer; Recno: Integer; var Accept: Boolean);
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MySuspendOperation(Sender: TObject);
    procedure MyContinueOperation(Sender: TObject);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    procedure MyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MyKeyPress(Sender: TObject; var Key: Char);
  public
      constructor CreateAction(CmdLine: TlicgBaseCmdLine);
      destructor Destroy; override;
  end;

implementation

uses
  lxStrUtil,
  Lider.CG.Com.Math,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.VectorInt,
  Lider.CG.LiWork.HizliYaziDüzenle,
  Lider.CG.LiWork.YaziParcalamaSecenekleri;

{ TSetTextSeparateAction }

constructor TSetTextSeparateAction.CreateAction(CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateAction(CmdLine);
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  if Not Assigned(fmYaziParcalamaSecenekleri) then  //form oluþturulmadýysa iþleme girer.
    fmYaziParcalamaSecenekleri := TfmYaziParcalamaSecenekleri.Create(nil);
  fmYaziParcalamaSecenekleri.ShowModal;
  if fmYaziParcalamaSecenekleri.ModalResult = mrOk then
  begin
    FIsAllSelect := fmHizliYaziDüzenle.chkCokluNesneSecim.Checked;
    FIsCopyMood := fmYaziParcalamaSecenekleri.chkKopyaModu.Checked;
    FMyClassList := TList<TMyClass>.Create;
    OnMouseDown := MyMouseDown;
    OnMouseMove := MyMouseMove;
    OnKeyPress := MyKeyPress;
    OnContinueOperation := MyContinueOperation;
    OnSuspendOperation := MySuspendOperation;
    Self.Cursor := crDrawCross;
    FFinishFlag := False;
    FLineEnt := Licad.CreateEntityFactory.MakeEntity(idLine, 0, _3D);
    StartTrackSelect(CmdLine);
  end
  else
    Self.Finished := True;
end;

destructor TSetTextSeparateAction.Destroy;
begin
  CmdLine.ActiveDrawBox.Selection.Clear;
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  if Assigned(FMyClassList) then
  begin
    Self.FLineEnt := nil;
    FMyClassList.Clear;
    FreeAndNil(FMyClassList);
  end;
  if Assigned(fmYaziParcalamaSecenekleri) then
    FreeAndNil(fmYaziParcalamaSecenekleri);
  inherited;
end;

procedure TSetTextSeparateAction.DrawAllEnts(Ents: ILicgEntityList);
var
 I: Integer;
begin
  For I := 0 to Ents.Count - 1 do
    CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(Ents[I]);  //** kýrmýzý yazý
end;

procedure TSetTextSeparateAction.LauncherFinished(Sender: TObject);
begin

end;

procedure TSetTextSeparateAction.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
var
  Key: Word;
  S: Integer;
begin
  S := fmYaziParcalamaSecenekleri.seSatirAraligi.Value;
  if (layer = nil) or (Recno = 0) then  //Geçersiz yere basýnca hata döndürmemesi için kullanýlýr.
    Exit;
  Accept := False; //kýrmýzý þekilde yazýnýn görünmemesi için
  CmdLine.ActiveDrawBox.Selection.Add(Layer, RecNO);
  CmdLine.ActiveDrawBox.RepaintExtent(Layer.LoadEntityWithRecNo(RecNO).Extent);
  FMyClassList.Add(TMyClass.Create(Layer.LoadEntityWithRecNo(RecNO)));
  FMyClassList[FMyClassList.Count - 1].FRowList :=
    SetTextSeparate(FMyClassList[FMyClassList.Count - 1].FEnt.AsTextValue.Text);
  FMyClassList[FMyClassList.Count - 1].FSeparatedEnts := CreateTextEntityList(
    FMyClassList[FMyClassList.Count - 1].FRowList,
    FMyClassList[FMyClassList.Count - 1].FEnt);
  if FMyClassList[FMyClassList.Count - 1].FRowList.Count > 0 then
    FIsTextSeparated := True
  else
    Exit;

  FMyClassList[FMyClassList.Count - 1].FRefP := ASCoor(Wx, Wy);
  AssingingCoordinateToText(
    FMyClassList[FMyClassList.Count - 1].FRefP,
    FMyClassList[FMyClassList.Count - 1].FEnt,
    FMyClassList[FMyClassList.Count - 1].FRowList,
    FMyClassList[FMyClassList.Count - 1].FSeparatedEnts,
    fmYaziParcalamaSecenekleri.seSatirAraligi.Value);
  if Not FIsAllSelect then
  begin
    Key := 27;
    MyKeyDown(nil, Key, Shift);
  end;
end;

procedure TSetTextSeparateAction.MySuspendOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
end;

procedure TSetTextSeparateAction.MyContinueOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
end;

procedure TSetTextSeparateAction.MyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    if FIsTextSeparated then
    begin
      Launcher.Finished := True;
      FFinishFlag := True;
      //Exit;
    end;
  end;
end;

procedure TSetTextSeparateAction.MyKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #27 then
  begin
    Self.Finished := True;
    Exit;
  end;
end;

procedure TSetTextSeparateAction.MyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; const WX,
  WY: Double);
var
  I, J, Rc: Integer;
begin
  if Button = mbLeft then
  begin
    CmdLine.ActiveDrawBox.Selection.Clear;
    if FIsAllSelect And (Not FRefPSelected) then
    begin
      Caption := 'Yazýlarýn Taþýnacaðý Yeri Gösteriniz.';
      FAllRefP := AsCoor(WX, WY);
      FRefPSelected := True;
      FLineEnt.Geometry.Points[0] := FAllRefP;
      Exit;
    end;

    for J := 0 to FMyClassList.Count - 1 do
    begin
      for I := 0 to FMyClassList[J].FSeparatedEnts.Count - 1 do
      begin
        CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
        Rc := TlicgBaseLayer(
          FMyClassList[J].FEnt.Layer).AddEntity(
          FMyClassList[J].FSeparatedEnts[I],
          FMyClassList[J].FEnt.ApplyLayerProps);
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(
          CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(
          FMyClassList[J].FSeparatedEnts[I].Extent);
        CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
      end;

      FMyClassList[J].FSeparatedEnts.Clear;
      FIsTextSeparated := False;

      if Not FIsCopyMood then
      begin
        CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaundelete);
        TlicgBaseLayer(FMyClassList[J].FEnt.Layer).DeleteEntity(
          FMyClassList[J].FEnt.Geometry.ID);  //katman bilgisini alýp nesneyi siler.
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(
          CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer,
          FMyClassList[J].FEnt.Geometry.ID, uaUndelete);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(
          FMyClassList[J].FEnt.Extent);
        CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
        //FEnt.Geometry.ID: Seçilen nesnenin kayýt numarasýna budan ulaþýrýz.
      end;
    end;
    StartTrackSelect(CmdLine);
    CmdLine.ActiveDrawBox.Selection.Clear;
  end
  else
  begin
    Self.Finished := True;
    Exit;
  end;
end;

procedure TSetTextSeparateAction.MyMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; const WX, WY: Double);
var
  I, FCount: Integer;
  P: TLicgCoor;
begin
  if FIsTextSeparated then
  begin
    for I := 0 to FMyClassList.Count - 1 do
    begin
      if FIsAllSelect then
      begin
        if NoT FRefPSelected then
          Exit
        else
        begin
          P.X := FMyClassList[I].FRefP.X - (FAllRefP.X - AsCoor(Wx, Wy).X);
          P.Y := FMyClassList[I].FRefP.Y - (FAllRefP.Y - AsCoor(Wx, Wy).Y);
          DrawAllEnts(FMyClassList[I].FSeparatedEnts);
         // CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(FLineEnt);
          CmdLine.ActiveDrawBox.RefreshExtent(Cmdline.ActiveDrawBox.Grapher.CurrentParams.VisualWindow);
          FLineEnt.Geometry.Points[1] := AsCoor(Wx, Wy);
          CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(FLineEnt);
        end;

      end
      else
      begin
        CmdLine.ActiveDrawBox.RefreshExtent(Cmdline.ActiveDrawBox.Grapher.CurrentParams.VisualWindow);
        P := AsCoor(Wx, Wy);
        DrawAllEnts(FMyClassList[I].FSeparatedEnts);
      end;
      AssingingCoordinateToText(
        P, FMyClassList[I].FEnt,
        FMyClassList[I].FRowList,
        FMyClassList[I].FSeparatedEnts,
        fmYaziParcalamaSecenekleri.seSatirAraligi.Value);
      DrawAllEnts(FMyClassList[I].FSeparatedEnts);
    end;
  end;
end;

function TSetTextSeparateAction.StartQuickSelect(ACmdLine: TlicgBaseCmdLine;
   P: TLicgCoor; Sender: TObject; Shift: TShiftState): ILicgEntityList;
var
  Layer: TLicgBaseLayer;
  Rc: Integer;
begin
  if not Assigned(Launcher) then
    Launcher := Licad.CreateActionTracker(ACmdLine);
  Launcher.IsCmdLineClear := False;
  Launcher.IsDefaultInitialization := True;
  Launcher.TrackQuickSelect(SCmdQuickSelect);
  Launcher.OnFinished := LauncherFinished;
  Launcher.CurrentAction.Caption := 'Ayýrmak Ýstediðiniz Yazý Nesnesini Seçiniz';

  repeat
    Launcher.CurrentAction.OnMouseDown(Sender, mbLeft, Shift, 0, 0, P.X, P.Y);
    if  CmdLine.ActiveDrawBox.Selection.Count > 0 then
      Break;
    Application.HandleMessage;
  until  Launcher.Finished;
  Launcher.Finish;
  if CmdLine.ActiveDrawBox.Selection.Count > 0 then
  begin
    Result := TlicgEntityList.Create;
    Layer := CmdLine.ActiveDrawBox.Selection[0].Layer;
    Rc := CmdLine.ActiveDrawBox.Selection[0].Sellist[0];
    Result.Add(Layer.LoadEntityWithRecNo(Rc));
    CmdLine.ActiveDrawBox.Selection.Clear;
  end;
end;

procedure TSetTextSeparateAction.StartTrackSelect(ACmdLine: TlicgBaseCmdLine);
begin
  if not Assigned(Launcher) then
    Launcher := Licad.CreateActionTracker(ACmdLine);   //launcher oluþturulmadýysa create edilir.
  Launcher.Finished := False;
  Launcher.IsCmdLineClear := False;
  Launcher.IsDefaultInitialization := True;
  Launcher.TrackEntityClick(SCmdQuickSelect);
  Launcher.OnFinished := LauncherFinished;
  LAuncher.OnTrackedEntityClick := LauncherTrackedEntityClick;
  Launcher.CurrentAction.OnKeyDown := MyKeyDown;
  Launcher.CurrentAction.Cursor := crSelectEntity;
  Launcher.CurrentAction.Caption := 'Ayýrmak Ýstediðiniz Yazý Nesnesini Seçiniz';
  repeat
    Application.HandleMessage;
  until  Launcher.Finished;
  if FFinishFlag then
    Launcher.Finish;
  FFinishFlag := False;
  if Not FIsTextSeparated then
    Self.Finished := True;
  Caption := 'Referans Noktasýný Seçiniz.';
end;

function TSetTextSeparateAction.SetTextSeparate(S: String): TList<Integer>;
var
  I, J, K: Integer;
  Text: String;
  SArray: TArray<string>;
  Val, Bolum, Kalan: Integer;
  Wx, Wy: Double;
begin
  Result := TList<Integer>.Create;
  SArray := S.Split([' ']); //[] Ýçine sözcüklerin ayrýlacaðý yerin belirtilmesi saðlanýr.
  Val := fmYaziParcalamaSecenekleri.seParcaAdet.Value;
  if Val = 0 then
    Val := 1
  Else if Val > Length(SArray) then
   Val := Length(SArray);
  Bolum := Length(SArray) div Val;
  Kalan := Length(SArray) mod Val;
  for I := 0 to Val - 1 do
  begin
    Result.Add(0);
    for J := 0 to Bolum - 1 do
    begin
        Result[I] := Result[I] + 1;
    end;
  end;
  J := 0;
  for I := 0 to Kalan - 1 do
  begin
    Result[J] := Result[J] + 1;
    Inc(J);
  end;
end;

function TSetTextSeparateAction.CreateTextEntityList(List1: TList<Integer>;
  Text: IlicgEntity ): ILicgEntityList;
var
  K, I, J: Integer;
  S,TextNew: String;
  SArray: TArray<string>;
  TextEnt: ILicgEntity;
begin
  K := 0;
  Result := TLicgEntityList.Create;   //**
  if  Text.EntityID = idVectorialText then
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D)
  else
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  S := Text.AsTextValue.Text;
  SArray := S.Split([' ']);  //S ye atanan metni parçalayarak listeler
  List1 := SetTextSeparate(S);
  for I := 0 to List1.Count-1 do
  begin
    for J := 0 to List1[I] - 1 do
    begin
      if J = 0 then
        TextNew := TextNew + SArray[K]
      else
        TextNew := TextNew + ' ' + SArray[K];
      inc(K);
    end;
    TextEnt.Geometry.Points[0] := AsCoor(0, 0);// Geçici Kordinat hata verdiði için oluþturuldu.
    TextEnt.AsTextValue.Text := TextNew;
    TextEnt.DrawTools.Assign(Text.DrawTools); //Assign metodu nesnenin özelliklerinin hepsini ayný alýr.
    TextEnt.DrawTools.FontTool.TextPos := tpCenter;
    Result.Add(TextEnt.Clone);
    TextNew := '';   //eski haline döndürmek için kullanýlýr.
  end;
end;

function TSetTextSeparateAction.AssingingCoordinateToText(RefP: TLicgCoor;
  Ent: ILicgEntity; List: TList<Integer>;
  ListN: ILicgEntityList; S: Integer): IlicgEntityList;
var
  I, R, HCount: Integer;
  P: TLicgCoor;
  H, Dist: Double;
begin
  S := fmYaziParcalamaSecenekleri.seSatirAraligi.Value;
  H := Ent.DrawTools.FontTool.Height;
  R := listN.Count div 2;
  if (listN.Count Mod 2) = 0 then
  begin
    Dist := (S/2 + H/2 + S*(R-1) + H*(R-1));
    P := Polar(RefP, Ent.DrawTools.FontTool.Angle , Dist);
    for I := 0 to ListN.Count - 1 do
    begin
      ListN[I].Geometry.Points[0] := P;
      P.Y := Polar(P, Ent.DrawTools.FontTool.Angle ,
        -(H + S)*cos(Ent.DrawTools.FontTool.Angle) ).Y;
    end;
  end
  else
  begin
    Dist := (R * S + H * R);
    P := Polar(RefP, Ent.DrawTools.FontTool.Angle , Dist);
    for I := 0 to ListN.Count - 1 do
    begin
      ListN[I].Geometry.Points[0] := P;
      P.Y := Polar(P, Ent.DrawTools.FontTool.Angle , -(H + S)).Y;
    end;
  end;
  Result := ListN;
end;

{ TMyClass }

constructor TMyClass.Create(Ent: ILicgEntity);
begin
  self.FEnt := Ent.Clone;
  Self.FRowList := TList<Integer>.Create;
  Self.FSeparatedEnts := TlicgEntityList.Create;
end;

destructor TMyClass.Destroy;
begin
  Self.FEnt := nil;
  Self.FSeparatedEnts := nil;
  if Assigned(Self.FRowList) then
    FreeAndNil(Self.FRowList);
  inherited;
end;

end.
