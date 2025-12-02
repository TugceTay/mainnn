unit Lider.CG.LiWork.LauncherSetTextFirstWordUpper;

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
  TSetTextFirstWordUpperLauncher = class(TlicgLauncher)
  private
    FIsAllSelect: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    procedure SetTextFirstWordUpper(Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
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
  Lider.CG.Com.GeoLibrary,
  Lider.CG.LiWork.HizliYaziDüzenle;

{ TSetTextFirstWordUpperLauncher }

constructor TSetTextFirstWordUpperLauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
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

destructor TSetTextFirstWordUpperLauncher.Destroy;
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  inherited;
end;

procedure TSetTextFirstWordUpperLauncher.LauncherSuspendOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
end;

procedure TSetTextFirstWordUpperLauncher.LauncherContinueOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
end;

procedure TSetTextFirstWordUpperLauncher.LauncherFinished(Sender: TObject);
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
            SetTextFirstWordUpper(TempLayer, TempRc, FIsAllSelect);
            if (I = 0) And (J = 0) then
            begin
              Ext := Selection[I].GetRecExtension(TempRc);//Extent döndürür
            end
            else
              CalcMaxMinBounds(Ext, Selection[I].GetRecExtension(TempRc));
            CmdLine.ActiveDrawBox.Undo.AddUndo(TempLayer, TempRc, uaUndelete);
          end;
        end; // for I
        CmdLine.ActiveDrawBox.RepaintExtent(Ext);
      end; // Selection.Count
      Selection.clear;
    end; //with
    CmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
  Self.Free;
end;

procedure TSetTextFirstWordUpperLauncher.LauncherKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Launcher.Finish;
end;

procedure TSetTextFirstWordUpperLauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
begin
  Accept := False;   //Kýrmýzý çizgi için kullanýlýr
  if (layer = nil) or (Recno = 0) then  //Geçersiz yere basýnca hata döndürmemesi için kullanýlýr.
    Exit;

  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  SetTextFirstWordUpper(Layer, RecNo, false );
  CmdLine.ActiveDrawBox.Undo.EndUndo;
end;

procedure TSetTextFirstWordUpperLauncher.SetTextFirstWordUpper(
  Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
var
  TextN: ILicgEntity;
begin
  TextN := layer.LoadEntityWithRecNo(Rc);
  TextN.AsTextValue.Text := IlkHarflerBuyuk(TextN.AsTextValue.Text);
  TextN.Geometry.UpdateExtension;
  CmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUnTransform); //güncellemede iþlemden önce ekliyoruz.
  Layer.UpdateEntity(Rc, TextN);
  CmdLine.ActiveDrawBox.RepaintExtent(InflateExtent(TextN.Extent, TextN.DrawTools.FontTool.Height * 5));
  TextN := nil;
end;
end.
