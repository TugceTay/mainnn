unit Lider.CG.Com.ModulesTypes;

interface

uses
  Windows,
  Classes,
  Graphics,
  Forms,
  Controls,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Types,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.Lib,
  Lider.CG.Com.RibbonInt,
  Lider.CG.Com.ModulesConsts;

type
  PTlicgModuleInfo = ^TlicgModuleInfo;

  TlicgModuleInfo = packed record
    ModuleName: string;
    Version: string;
    Author: string;
    Caption: string;
    Visible: Boolean;
  end;

type
  TLicadInitProc = procedure(App: TApplication; _Licad: ILicad; _LicadMenuReg: IlicgRibbon = nil; _Params:
    PModulePassParams = nil); stdcall;

  TLicadCloseProc = procedure(); stdcall;

  TLicadApiInitProc = procedure(App: TApplication; var IsTerminate: boolean; _Params: PModulePassParams = nil); stdcall;

  TLicadModulRun = procedure(var InitProc: TLicadInitProc; var CloseProc:
    TLicadCloseProc); stdcall;

  TLicadModulApiRun = procedure(var InitProc: TLicadApiInitProc; var CloseProc:
    TLicadCloseProc); stdcall;

  TLicadGetModulIntProc = function(ModulName: string): IUnknown; stdcall;

  TGetDrawBoxPanelProc = function(): TObject; stdcall; // TPanel;

implementation

end.


