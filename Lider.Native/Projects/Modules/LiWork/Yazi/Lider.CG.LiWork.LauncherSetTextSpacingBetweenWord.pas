unit Lider.CG.LiWork.LauncherSetTextSpacingBetweenWord;

interface

uses
  Classes,
  Controls,
  SysUtils,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.GeoTypes;

type
  TSetTextSpacingBetweenWordLauncher = class(TlicgLauncher)
  private
    FIsAllSelect: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    procedure SetTextSpacingBetweenWord(Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
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
  lxStrUtil,
  Lider.CG.LiWork.HizliYaziDüzenle,
  Lider.CG.LiWork.HarflerArasiBoslukEklemeSecenekleri,
  Lider.CG.Com.GeoLibrary;

{ TSetTextSpacingBetweenWordlauncher }

constructor TSetTextSpacingBetweenWordlauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  //Launcher.TrackSelectEntity(SCmdLauncher);   TrackQuickSelect ve TrackSelectEntity kullandýðýnda burda yazma!
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;

  if Not Assigned(fmHarflerArasiBoslukEklemeSecenekleri) then  //form oluþturulmadýysa iþleme girer.
    fmHarflerArasiBoslukEklemeSecenekleri := TfmHarflerArasiBoslukEklemeSecenekleri.Create(nil);
  fmHarflerArasiBoslukEklemeSecenekleri.ShowModal;
  if fmHarflerArasiBoslukEklemeSecenekleri.ModalResult = mrOk then
  begin
    FIsAllSelect := fmHizliYaziDüzenle.chkCokluNesneSecim.Checked;
    if FIsAllSelect then
      Launcher.TrackQuickSelect(SCmdLauncher)
    else
    begin
      Launcher.TrackSelectEntity(SCmdLauncher);
      Launcher.OnTrackedEntityClick := LauncherTrackedEntityClick;
    end;
    Launcher.CurrentAction.Caption := 'Ýþlem Yapmak Ýstediðiniz Yazý Nesnesini Seçiniz.';
    Launcher.OnFinished := LauncherFinished;
    Launcher.OnKeyDown := LauncherKeyDown;
    Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
    Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  end
  else
  self.Free;
end;

destructor TSetTextSpacingBetweenWordlauncher.Destroy;
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  if Assigned(fmHarflerArasiBoslukEklemeSecenekleri) then       //Önemli bunu unutma!
    FreeAndNil(fmHarflerArasiBoslukEklemeSecenekleri);
  inherited;
end;

procedure TSetTextSpacingBetweenWordlauncher.LauncherContinueOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
end;

procedure TSetTextSpacingBetweenWordlauncher.LauncherSuspendOperation(
  Sender: TObject);
begin
   CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
end;

procedure TSetTextSpacingBetweenWordlauncher.LauncherFinished(Sender: TObject);
var
  I, J, TempRc: Integer;
  TempLayer: TLicgBaseLayer;
  Ext: TLicgExtent;
begin
  if FIsAllSelect then
  begin
    CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUndelete);
    with CmdLine.ActiveDrawBox do
    begin
      if Selection.Count > 0 then
      begin
        for I := 0 to Selection.Count - 1 do
        begin
          TempLayer := Selection[I].Layer;
          for J := 0 to Selection[I].SelList.Count - 1 do
          begin
            TempRc := Selection[I].SelList[J];
            SetTextSpacingBetweenWord(TempLayer, TempRc, FIsAllSelect);
            if (I = 0) And (J = 0) then
            begin
              Ext := Selection[I].GetRecExtension(TempRc);//Extent döndürür
            end
            else
              CalcMaxMinBounds(Ext, Selection[I].GetRecExtension(TempRc));
            CmdLine.ActiveDrawBox.Undo.AddUndo(TempLayer, TempRc, uaUndelete);
          end;
        end;
        CmdLine.ActiveDrawBox.RepaintExtent(Ext);
      end;
      Selection.clear;  //selection clear yap yoksa baþka iþlemlerde hafýzada kalýr ve iþleme alabilir.
    end;
    CmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
  Self.Free;
end;

procedure TSetTextSpacingBetweenWordlauncher.LauncherKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Launcher.Finish;
end;

procedure TSetTextSpacingBetweenWordlauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
begin
  Accept := False;   //kýrmýzý þekilde yazýnýn görünmemesi için kullanýlýr.
  if (layer = nil) or (Recno = 0) then  //Geçersiz yere basýnca hata döndürmemesi için kullanýlýr.
    Exit;

  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  SetTextSpacingBetweenWord(Layer, Recno, False);
  CmdLine.ActiveDrawBox.Undo.EndUndo;
end;

procedure TSetTextSpacingBetweenWordlauncher.SetTextSpacingBetweenWord(
  Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
var
  TextN: IlicgEntity;
  I: Integer;
  S, TempS: String;
  Bosluk: String;
begin
  TextN := layer.LoadEntityWithRecNo(Rc);
  Bosluk := ' ';
  TempS := '';
  S := TextN.AsTextValue.Text;
  for I := 0 to fmHarflerArasiBoslukEklemeSecenekleri.seBosluk.Value-1 do
    Bosluk := ' ' + Bosluk;
  for I := 1 to Length(S) do
    TempS := TempS + S[I] + Bosluk;
  TextN.AsTextValue.Text := BastakiveSondakiBosluklariSil(TempS);
  TextN.Geometry.UpdateExtension;
  CmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUnTransform); //güncellemede iþlemden önce ekliyoruz.
  Layer.UpdateEntity(Rc, TextN);
  CmdLine.ActiveDrawBox.RepaintExtent(InflateExtent(TextN.Extent, TextN.DrawTools.FontTool.Height * 5));
  TextN := nil;
end;
end.
