unit Lider.CG.LiWork.LauncherSetTextRoundingToDecimal;

interface

uses
  Math,
  Graphics,
  Classes,
  Controls,
  SysUtils,
  Forms,
  Windows,
  Inifiles,
  cxLookAndFeelPainters, cxCheckGroup, cxCheckComboBox,
  cxCheckBox, cxRadioGroup, cxGridCustomView, cxDropDownEdit, cxStyles, cxGridCustomTableView,
  System.RegularExpressions,
  System.Generics.Collections,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.GeoTypes;

type
  TSetTextRoundingToDecimalLauncher = class(TlicgLauncher)
  private
    FIsAllSelect: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    procedure SetTextRoundingToDecimal(Layer: TLicgBaseLayer; Rc: Integer; IsAllSelect: Boolean);
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
  Lider.CG.LiWork.YuvarlamaSecenekleri,
  Lider.CG.Com.GeoLibrary;

{ TRoundingToDecimalLauncher }

constructor TSetTextRoundingToDecimalLauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
 inherited CreateLauncher(CmdLine);
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;

   if Not Assigned(fmYuvarlamaSecenekleri) then  //form oluþturulmadýysa iþleme girer.
    fmYuvarlamaSecenekleri := TfmYuvarlamaSecenekleri.Create(nil);
  fmYuvarlamaSecenekleri.ShowModal;
  if fmYuvarlamaSecenekleri.ModalResult = mrOk then
  begin
    if FIsAllSelect then
      Launcher.TrackQuickSelect(SCmdLauncher)
    else
    begin
      Launcher.TrackSelectEntity(SCmdLauncher);
      Launcher.OnTrackedEntityClick := LauncherTrackedEntityClick;
    end;

    Launcher.OnFinished := LauncherFinished;
    Launcher.OnKeyDown := LauncherKeyDown;
    Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
    Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  end
  else
    Self.Free;
end;

destructor TSetTextRoundingToDecimalLauncher.Destroy;
begin
   CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  if Assigned(fmYuvarlamaSecenekleri) then       //Önemli bunu unutma!
    FreeAndNil(fmYuvarlamaSecenekleri);          //Önemli bunu unutma!
  inherited;
end;

procedure TSetTextRoundingToDecimalLauncher.LauncherContinueOperation(Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter :=  FNewEntsFilter
end;

procedure TSetTextRoundingToDecimalLauncher.LauncherFinished(Sender: TObject);
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
            SetTextRoundingToDecimal(TempLayer, TempRc, FIsAllSelect);   //**
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
      Selection.clear;    //selection clear yap yoksa baþka iþlemlerde hafýzada kalýr ve iþleme alabilir.
    end; //with
    CmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
  Self.Free;
end;

procedure TSetTextRoundingToDecimalLauncher.LauncherKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
  Launcher.Finish;
end;

procedure TSetTextRoundingToDecimalLauncher.LauncherSuspendOperation(Sender: TObject);
begin
   CmdLine.ActiveDrawBox.NoPickFilter  := FOldEntsFilter
end;

procedure TSetTextRoundingToDecimalLauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
begin
  Accept := False;   //Kýrmýzý çizgi için kullanýlýr
  if (layer = nil) or (Recno = 0) then  //Geçersiz yere basýnca hata döndürmemesi için kullanýlýr.
    Exit;

  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  SetTextRoundingToDecimal(Layer, RecNo, false );
  CmdLine.ActiveDrawBox.Undo.EndUndo;
end;

procedure TSetTextRoundingToDecimalLauncher.SetTextRoundingToDecimal(Layer: TLicgBaseLayer;
  Rc: Integer; IsAllSelect: Boolean);
var
  ExTextN, TextN: ILicgEntity;
  S, Fark, I, CountBeforeTrunc, PurposeCount: Integer;
  Text: String;
  Val, TempVal: double;
begin
  S := fmYuvarlamaSecenekleri.seOndalikBasamakSayisi.Value;
  TextN := layer.LoadEntityWithRecNo(Rc);
  Text := TextN.AsTextValue.Text;
   if text.IndexOf('.') = -1 then       //parantez içindeki ifade varsa indeksini verir yoksa -1 döner.
    exit;

  if TryStrToFloat(Text, Val) then    //string deðeri floata dönüþebiliyorsa iþlem yapar.
  begin
    Val := RoundTo(Val, S * -1);     //virgülden sonra s kadar sayýyý alýr, yuvarlatýr.
    CountBeforeTrunc := Length(floattoStr(Val));
    TempVal := Trunc(Val);     //Virgülden öncesini alýr.
    PurposeCount := S + Length(floattoStr(TempVal));
    if Not (Length(floattoStr(TempVal)) = CountBeforeTrunc) then
      Inc(PurposeCount);
    if Length(floattoStr(Val)) < PurposeCount then
    begin
      if (Length(floattoStr(TempVal)) = CountBeforeTrunc) And (S > 0) then
        Text := FloatToStr(Val) + '.'
      else
        Text := floattoStr(Val);
      for I := 0 to PurposeCount - Length(floattoStr(Val)) - 1 do
        Text := Text + '0';
    end
    else if (Length(floattoStr(TempVal)) = CountBeforeTrunc) And (S > 0) then
    begin
      Text := FloatToStr(TempVal) + '.';
      for I := 0 to S - 1 do
        Text := Text + '0';
    end
    else
      Text :=  FloatToStr(Val);
    TextN.AsTextValue.Text := Text;
    TextN.Geometry.UpdateExtension;
    CmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUnTransform); //güncellemede iþlemden önce ekliyoruz.
    Layer.UpdateEntity(Rc, TextN);
    CmdLine.ActiveDrawBox.RepaintExtent(InflateExtent(TextN.Extent, 5));
//    CmdLine.ActiveDrawBox.ZoomWindow(TextN.Extent);
  end;
end;
end.
