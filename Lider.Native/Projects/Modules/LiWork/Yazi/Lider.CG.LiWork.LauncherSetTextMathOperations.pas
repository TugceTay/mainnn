unit Lider.CG.LiWork.LauncherSetTextMathOperations;

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
  Lider.CG.Com.GeoTypes;

type
  TSetTextMathOperationsLauncher = class(TlicgLauncher)
  private
    FIsAllSelect: Boolean;
    FIsAlphaNumber: Boolean;
    FOldEntsFilter, FNewEntsFilter: TlicgEntityIDs;
    function MathOperation(Val: Double): Double;
    function IsCharNumber(C: Char): Boolean;
    procedure SetTextMathOperations(Layer: TLicgBaseLayer; Rc: Integer);
    function SetTextAlphaOperations(Layer: TlicgBaseLayer; Rc: Integer): String;
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
  Lider.CG.LiWork.MatematikIslemleri,
  Lider.CG.LiWork.HizliYaziDüzenle,
  Lider.CG.Com.GeoLibrary;

{ TSetTextMathOperationsLauncher }

function TSetTextMathOperationsLauncher.IsCharNumber(C: Char): Boolean;
const
  Chars = ['0'..'9'];
begin
  Result := C in Chars;
end;

constructor TSetTextMathOperationsLauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  FOldEntsFilter := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntsFilter := AllEntityIDs - [idText, idVectorialText];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;

  if Not Assigned(fmMatematikIslemleriYeni) then  //form oluþturulmadýysa iþleme girer.
    fmMatematikIslemleriYeni := TfmMatematikIslemleriYeni.Create(nil);
  fmMatematikIslemleriYeni.ShowModal;
  if fmMatematikIslemleriYeni.ModalResult = mrOk then
  begin
    FIsAllSelect := fmHizliYaziDüzenle.chkCokluNesneSecim.Checked;
    FIsAlphaNumber := fmMatematikIslemleriYeni.chkAlfasayisal.Checked;
    if FIsAllSelect then
      Launcher.TrackQuickSelect(SCmdLauncher)
    else
    begin
      Launcher.TrackSelectEntity(SCmdLauncher);
      Launcher.OnTrackedEntityClick := LauncherTrackedEntityClick;
    end;
    Launcher.Caption := 'Ýþlem Yapacaðýnýz Nesneyi Seçiniz.';
    Launcher.OnFinished := LauncherFinished;
    Launcher.OnKeyDown := LauncherKeyDown;
    Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
    Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  end
  else
    Self.Free;
end;

destructor TSetTextMathOperationsLauncher.Destroy;
begin
   CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
  if Assigned(fmMatematikIslemleriYeni) then       //Önemli bunu unutma!
    FreeAndNil(fmMatematikIslemleriYeni);
  inherited;
end;

procedure TSetTextMathOperationsLauncher.LauncherContinueOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntsFilter;
end;

procedure TSetTextMathOperationsLauncher.LauncherSuspendOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntsFilter;
end;

procedure TSetTextMathOperationsLauncher.LauncherKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Launcher.Finish;
end;

procedure TSetTextMathOperationsLauncher.LauncherFinished(Sender: TObject);
var
  I, J, TempRc: Integer;
  TempLayer: TLicgBaseLayer;
  S, Ext: TLicgExtent;
begin
  if FIsAllSelect  then
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
            SetTextMathOperations(TempLayer, TempRc);
            if (I = 0) And (J = 0) then
            begin
              Ext := Selection[I].GetRecExtension(TempRc);//Extent döndürür
            end
            else
              CalcMaxMinBounds(Ext, Selection[I].GetRecExtension(TempRc));
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

procedure TSetTextMathOperationsLauncher.LauncherTrackedEntityClick(
  const TrackID: string; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; const WX, WY: Double; Layer: TlicgBaseLayer; Recno: Integer;
  var Accept: Boolean);
begin
  Accept := False;   //Kýrmýzý çizgi için kullanýlýr
  if (layer = nil) or (Recno = 0) then  //Geçersiz yere basýnca hata döndürmemesi için kullanýlýr.
    Exit;

  CmdLine.ActiveDrawBox.Undo.BeginUndo(uaUnTransform);
  SetTextMathOperations(Layer, RecNo);
  CmdLine.ActiveDrawBox.Undo.EndUndo;
  //CmdLine.ActiveDrawBox.ClearAlwaysDisplayList;

end;

function TSetTextMathOperationsLauncher.MathOperation(Val: Double): Double;
var
  Deger: Double;
begin
  Result := 0;
  Deger := fmMatematikIslemleriYeni.ceDeger.Value;
  if fmMatematikIslemleriYeni.rgbIslemler.ItemIndex = 0  then
    Result := Val + Deger
  else if fmMatematikIslemleriYeni.rgbIslemler.ItemIndex = 1 then
    Result := Val - Deger
  else if fmMatematikIslemleriYeni.rgbIslemler.ItemIndex = 2 then
    Result := Val * Deger
  else if fmMatematikIslemleriYeni.rgbIslemler.ItemIndex = 3 then
    Result := Val / Deger;
end;

function TSetTextMathOperationsLauncher.SetTextAlphaOperations(
  Layer: TlicgBaseLayer; Rc: Integer): String;
var
  LeftC, MiddleC, RightC: String;
  I, J : Integer;
  TextN: ILicgEntity;
  //Value: Double;
  TextA: String;
  Flag, Flag1: Boolean;
begin
  LeftC := '';
  MiddleC := '';
  RightC := '';
  TextN := layer.LoadEntityWithRecNo(Rc);
  TextA := TextN.AsTextValue.Text;
  Flag := False;
  Flag1 := False;
  for I := 1 to Length(TextA) do
  begin
    if IsCharNumber(TextA[I]) then
    begin
      if Flag1 then
      begin
        Result := TextN.AsTextValue.Text;
        TextN := nil;
        Exit;
      end;
      Flag := True;
      MiddleC := MiddleC + TextA[I];
    end
    else
    begin
      if Flag  then
      begin
        if TextA[I] = '.' then
        begin
          MiddleC := MiddleC + TextA[I];
          continue;
        end;
        RightC := RightC + TextA[I];
        Flag1 := True;
      end
      else
      LeftC := LeftC + TextA[I];
    end;
  end;
  if (MiddleC='') and (RightC = '') then
   Result := LeftC
  else
   Result := LeftC + FloatToStr(MathOperation(StrToFloat(MiddleC))) + RightC;
  TextN := nil;
end;

procedure TSetTextMathOperationsLauncher.SetTextMathOperations(
  Layer: TLicgBaseLayer; Rc: Integer);
var
  TextN: ILicgEntity;
  Val: Double;
  TempRc: Integer;
  TempLayer: TLicgBaseLayer;
begin
  TextN := layer.LoadEntityWithRecNo(Rc);

  //Sonuc := 0;
 //Deger := fmMatematikIslemleriYeni.ceDeger.Value;
   if Not FIsAlphaNumber then
   begin
    if Not TryStrToFloat(TextN.AsTextValue.Text, Val) then
    begin
      TextN := nil;
      Exit;
    end;
    TextN.AsTextValue.Text := FloatToStr(MathOperation(StrToFloat(TextN.AsTextValue.Text))) ;
   end
   else
    TextN.AsTextValue.Text := SetTextAlphaOperations(Layer, Rc);
  TextN.Geometry.UpdateExtension;
  CmdLine.ActiveDrawBox.Undo.AddUndo(Layer, Rc, uaUnTransform); //güncellemede iþlemden önce ekliyoruz.
  Layer.UpdateEntity(Rc, TextN);
  CmdLine.ActiveDrawBox.RepaintExtent(TextN.Extent);
  TextN := nil;
end;

end.
