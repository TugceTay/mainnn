unit Lider.CG.LiWork.Module;


interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  Forms,
  SysUtils,
  IOUtils,
  Registry,
  ActiveX,
  Axctrls,
  dxCore,
  cxLookAndFeels,
  dxSkinsDefaultPainters,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Skin,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.RibbonInt,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.ModulesConsts,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Types,
  Lider.CG.Com.ModulesTypes,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

implementation

uses
  Lider.CG.LiWork.Ribbon;

var
  OldApp: TApplication;

procedure ModuleSkinChangeEvent(const ASkinStream: IStream; const ASkinName: widestring); stdcall;
var
  AStream: TStream;
begin
  AStream := TOleStream.Create(ASkinStream);
  AStream.Position := 0;
  try
    if dxSkinsUserSkinLoadFromStream(AStream, ASkinName) then
    begin
      LicadSkinController.NativeStyle := False;
      LicadSkinController.SkinName := 'UserSkin';
      LicadSkinController.UseSkins := True;
      //LicadSkinController.Refresh;
      RootLookAndFeel.RibbonSkinName := ASkinName;
    end;
  finally
    AStream.Free;
  end;
end;
(*
procedure ModuleSkinChangeEvent(ASkinName: widestring); stdcall;
begin
  if dxSkinsUserSkinLoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skins.skinres', ASkinName) then
  begin
    LicadSkinController.NativeStyle := False;
    LicadSkinController.SkinName := 'UserSkin';
    LicadSkinController.UseSkins := True;
    LicadSkinController.Refresh;
    RootLookAndFeel.RibbonSkinName := ASkinName;
  end;
end;
*)


procedure ModuleInit(App: TApplication; ALicad: ILicad; ARibbon: IlicgRibbon = nil; AParams: PModulePassParams = nil); stdcall;
begin
  TModulePassSkins(AParams[1]^)[SkinModuleLiWorkID] := @ModuleSkinChangeEvent;

  if (App <> nil) then
  begin
    OldApp := Application;
    Application.Icon.Handle := App.Icon.Handle;
    Application.Handle := App.MainFormHandle;
  end;

  SetupCursors;

  Licad := ALicad;

  LicadRibbon := ARibbon;
  ModulePassParams := AParams;

  LicadRibbon.AddCallBackEvent(@OnModuleEvents);
  RibbonAdd;
end;

procedure ModuleClose; stdcall;
begin
  DisposeCursors;

  if (OldApp <> nil) then
  begin
    Application := OldApp;
  end;
end;

procedure ModuleRun(var InitProc: TLicadInitProc; var CloseProc: TLicadCloseProc); stdcall;
begin
  InitProc := @ModuleInit;
  CloseProc := @ModuleClose;
end;

function GetModuleInterface: IUnknown; stdcall;
begin
  Result := nil;
end;

exports
  dxInitialize,
  dxFinalize,
  ModuleRun,
  GetModuleInterface;

initialization
  OldApp := nil;

end.


