unit Lider.CG.LiWork.Ribbon;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.IOUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Math,
  Lider.CG.Com.LicadInt,System.StrUtils,
  Lider.CG.Com.RibbonInt,   licadutil,

  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,        // Topology sýnýfý
  Lider.CG.Com.Skin,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.ModulesConsts,
  Lider.CG.Com.Types,
  Lider.CG.Com.ModulesTypes,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.scalebar,
  Lider.CG.Com.ReferenceInt,
  Lider.CG.Com.RTree,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.LiWork.LauncherUnionSameNamedPolys,
  Lider.CG.Com.DrawToolsInt;

function OnModuleEvents(AEvent: TlicgModuleEvents; AParam: Pointer): Boolean; stdcall;
procedure RibbonAdd; stdcall;

implementation

uses
  Lider.CG.LiWork.DataModuleCom,
  Lider.CG.LiWork.HizliYaziDüzenle,
  Lider.CG.LiWork.LauncherSetTextUpperOrLower,
  Lider.CG.LiWork.LauncherSetTextFirstWordUpper,
  Lider.CG.LiWork.LauncherSetTextSpacingBetweenWord,
  Lider.CG.LiWork.LauncherSetTextRoundingToDecimal,
  Lider.CG.LiWork.LauncherSetTextMathOperations,
  Lider.CG.LiWork.ActionSetTextSeparate,
  Lider.CG.LiWork.ActionSetTextQuickText,
  Lider.CG.LiWork.LauncherPointFromText,
  Lider.CG.LiWork.LauncherPoligonNameFromText,
  Lider.CG.LiWork.LauncherAddingTextToTheParcel,
  Lider.CG.Com.KatmanOznitelikTablosu;

function OnModuleEvents(AEvent: TlicgModuleEvents; AParam: Pointer): Boolean; stdcall;
begin
  case AEvent of
    AfterModuleLoad:;
    BeforeProjectLoad:;
    AfterProjectLoad:;
    BeforeProjectClose:;
    BeforeActiveProjectChange:;
    AfterActiveProjectChange:;
    BeforeModuleUnload:;
  end;
end;

function RibbonEnable(CommandIndex: integer = 1): Boolean; stdcall;
begin
  Result := True;
end;

procedure SetTextUpper;
begin
  CurrCmdLine.DoLauncher(TSetTextUpperOrLowerLauncher.CreateLauncher(CurrCmdLine, True), TFarProcedure1(@SetTextUpper));
end;

procedure SetTextLower;
begin
  CurrCmdLine.DoLauncher(TSetTextUpperorLowerLauncher.CreateLauncher(CurrCmdLine, False), TFarProcedure1(@SetTextLower));
end;

procedure SetTextFirstWordUpper;
begin
  CurrCmdLine.DoLauncher(TSetTextFirstWordUpperLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextFirstWordUpper));
end;

procedure SetTextSpacingBetweenWord;
begin
  CurrCmdLine.DoLauncher(TSetTextSpacingBetweenWordLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextSpacingBetweenWord));
end;

procedure SetTextSeparate;
begin
  CurrCmdLine.DoCommandEx(TSetTextSeparateAction.CreateAction(CurrCmdLine), TFarProcedure1(@SetTextSeparate));
//  CurrCmdLine.DoLauncher(TSetTextSeparateLauncher.CreateLauncher(CurrCmdLine, False), TFarProcedure1(@SetTextSeparate));
end;

procedure SetTextRoundingToDecimal;
begin
  CurrCmdLine.DoLauncher(TSetTextRoundingToDecimalLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextRoundingToDecimal));
end;

procedure SetTextMathOperations;
begin
  CurrCmdLine.DoLauncher(TSetTextMathOperationsLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@SetTextMathOperations));
end;

procedure SetTextQuickText;
begin
  CurrCmdLine.DoCommandEx(TSetTextQuickTextAction.CreateAction(CurrCmdLine), TFarProcedure1(@SetTextQuickText));
end;

procedure PointFromText;
begin
  CurrCmdLine.DoLauncher(TPointFromTextLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@PointFromText));
end;

Procedure QuickText;
begin
  fmHizliYaziDüzenle := TfmHizliYaziDüzenle.Create(nil);
  fmHizliYaziDüzenle.ShowModal;
  if fmHizliYaziDüzenle.ModalResult = mrOk then
  begin
    if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[0].Selected then
      SetTextUpper
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[1].Selected then
      SetTextLower
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[2].Selected then
      SetTextFirstWordUpper
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[3].Selected then
      SetTextSpacingBetweenWord
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[4].Selected then
      SetTextSeparate
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[5].Selected then
      SetTextQuickText
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[6].Selected then
      SetTextRoundingToDecimal
    else if fmHizliYaziDüzenle.lvcHizliYaziDuzenleAraclari.Items[7].Selected then
      SetTextMathOperations
  end
  else
  exit;
end;

procedure PoligonNameFromText;
begin
   CurrCmdLine.DoLauncher(TPoligonNameFromTextLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@PoligonNameFromText));
end;

procedure AddingTextToTheParcel;
begin
  CurrCmdLine.DoLauncher(TAddingTextToTheParcelLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@AddingTextToTheParcel));
end;

procedure UnionSameNameAreas;
begin
  CurrCmdLine.DoLauncher(
    TUnionSameNamedPolysLauncher.CreateLauncher(CurrCmdLine), TFarProcedure1(@UnionSameNameAreas));
end;

 procedure ShowLayerAttributeTable;
var
  ActiveLayer: TlicgBaseLayer;
begin
  if (CurrCmdLine = nil) or (CurrCmdLine.ActiveDrawBox = nil) or
    (CurrCmdLine.ActiveDrawBox.GIS = nil) then
  begin
    Application.MessageBox('Aktif katman bulunamadý.', 'Bilgi', MB_OK or MB_ICONINFORMATION);
    Exit;
  end;

  ActiveLayer := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer;
  if ActiveLayer = nil then
  begin
    Application.MessageBox('Aktif katman bulunamadý.', 'Bilgi', MB_OK or MB_ICONINFORMATION);
    Exit;
  end;

   with TfmKatmanOznitelikTablosu.Create(nil) do
    Enter(CurrCmdLine.ActiveDrawBox, ActiveLayer);
end;


procedure RibbonExecute(Command: Integer); stdcall;
begin
  case Command of
    11: QuickText;
    12: PointFromText;
    13: PoligonNameFromText;
    14: AddingTextToTheParcel;
    15: UnionSameNameAreas;
    16: ShowLayerAttributeTable;
  end;
end;

procedure RibbonAdd; stdcall;
begin
  with LicadRibbon do
  try
    Images := dmCom.SmallImages;
    LargeImages := dmCom.LargeImages;
    {
    AddCommand('LiWork\Deneme\Deneme1', '', 'Deneme1',
      @RibbonExecute, 11, 0, False, @RibbonEnable, bisLargeButton, nil, True, 0);
    AddCommand('LiWork\Deneme\Deneme2', '', 'Deneme2',
      @RibbonExecute, 12, 0, False, @RibbonEnable, bisLargeButton, nil, True, 1); }
    //AddCommand('LiWork\LiWork\Hýzlý Yazý Düzenleme', '', 'Hýzlý Yazý Düzenleme Ýþlemi',
      //@RibbonExecute, 11, 0, False, @RibbonEnable, bisLargeButton, nil, True, -1);
    AddCommand('LiWork\LiWork\Yazýdan Nokta', '', '',
      @RibbonExecute, 12, 0, True, @RibbonEnable, bisLargeButton, nil, True, -1);
    AddCommand('LiWork\LiWork\Yazýyý Nesne Adýna Ata', '', '',
      @RibbonExecute, 13, 0, False, @RibbonEnable, bisLargeButton, nil, True, -1);
    AddCommand('LiWork\LiWork\Alana Yazý Ekle', '', '',
      @RibbonExecute, 14, 0, False, @RibbonEnable, bisLargeButton, nil, True, -1);
    AddCommand('LiWork\LiWork\Alanlarý Birleþtir', '', 'Ayný Ýsme Sahip Alanlarý Birleþtirir',
      @RibbonExecute, 15, 0, False, @RibbonEnable, bisLargeButton, nil, True, -1);
   AddCommand('LiWork\LiWork\Katman Öznitelik Tablosu', '', 'Katman özniteliklerini düzenler',
      @RibbonExecute, 16, 0, False, @RibbonEnable, bisLargeButton, nil, True, -1);
  finally
    Images := nil;
    LargeImages := nil;
    FreeAndNil(dmCom.SmallImages);
    FreeAndNil(dmCom.LargeImages);
  end;
end;

initialization

finalization

end.


