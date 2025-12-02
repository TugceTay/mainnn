unit Lider.CG.LiWork.LauncherSetTextUpperOrLower;

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
  TSetTextUpperorLowerLauncher = class(TlicgLauncher)
  private
    FIsUpper, FIsAllSelect: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;  //filtreleme iþlemini yapmak için oluþturduðumuz deðiþkenler.
    procedure SetTextUpperOrLower(Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
    {Launcher Events}
    procedure LauncherFinished(Sender: TObject);   //sola týkladýðýmýzda iþlemi bitirmek için self.finished yerine kullanýyoruz.
    procedure LauncherKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LauncherTrackedEntityClick(const TrackID: string; Button:  //Üstüste gelen nesnelerde seçim yapabilmek için kullanýlýr.
      TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double; Layer:
      TlicgBaseLayer; Recno: Integer; var Accept: Boolean);
    procedure LauncherSuspendOperation(Sender: TObject);   //mevcut action devam ederken baþka action çalýþtýrýlýrsa ilk olarak bu event tetiklenir ve mevcut action dondurulur
    procedure LauncherContinueOperation(Sender: TObject); //Mevcur action'a geri dönüldüðünde ilk olarak bu event tetiklenir ve mevcut actiona devam edilir.
  public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdLine; IsUpper: Boolean);
    destructor Destroy; override;
  end;

implementation

uses
  lxStrUtil,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.LiWork.HizliYaziDüzenle;

{ TSetTextUpperorLowerLauncher }

constructor TSetTextUpperOrLowerLauncher.CreateLauncher(CmdLine: TlicgBaseCmdLine;
  IsUpper: Boolean);
begin
  inherited CreateLauncher(CmdLine);
  FIsUpper := IsUpper;
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
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
end;

destructor TSetTextUpperorLowerLauncher.Destroy;
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  inherited;
end;

procedure TSetTextUpperorLowerLauncher.LauncherSuspendOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;    //Eski ayarlarýna döndürmek için kullanýlýr.
end;

procedure TSetTextUpperorLowerLauncher.LauncherContinueOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;   //Devam iþlemi için yeni filtreyi getirir.
end;

procedure TSetTextUpperorLowerLauncher.LauncherFinished(Sender: TObject);
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
            SetTextUpperOrLower(TempLayer, TempRc, FIsUpper);
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
      Selection.clear;
    end;
    CmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
  Self.Free;
end;

procedure TSetTextUpperorLowerLauncher.LauncherKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
    Launcher.Finish;
end;

procedure TSetTextUpperorLowerLauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
begin
  Accept := False;
  if (layer = nil) or (Recno = 0) then
    Exit;

  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  SetTextUpperOrLower(Layer, RecNo, FIsUpper);
  CmdLine.ActiveDrawBox.Undo.EndUndo;

end;

procedure TSetTextUpperorLowerLauncher.SetTextUpperOrLower(
  Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
var
  Text: ILicgEntity;
begin
  Text := layer.LoadEntityWithRecNo(Rc);
  if FIsUpper then
    Text.AsTextValue.Text := HarfleriBuyut(Text.AsTextValue.Text)
  else
    Text.AsTextValue.Text := HarfleriKucult(Text.AsTextValue.Text);
  Text.Geometry.UpdateExtension;
  CmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUnTransform); //güncellemede iþlemden önce ekliyoruz.
  Layer.UpdateEntity(Rc, Text);
  CmdLine.ActiveDrawBox.RepaintExtent(InflateExtent(Text.Extent, Text.DrawTools.FontTool.Height * 5 ));
  Text := nil;
end;

end.
