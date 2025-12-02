unit Lider.CG.Com.CmdListInt;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Forms,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.ProjectInt,
  Lider.CG.Com.Types;

type
  ICmdLineList = interface
    ['{F025D33A-4620-46C5-A6FD-480B76D71A2F}']
    function GetCmdLineCurrIndex: Integer; stdcall;
    procedure SetCmdLineCurrIndex(CmdIndex: Integer);
    procedure ClearCmdLines; stdcall;
    function GetScaleBarVisible: Boolean; stdcall;
    procedure SetScaleBarVisible(const Value: Boolean); stdcall;
    function GetCmdline: TlicgBaseCmdline; stdcall;
    function AddCmdlineByParams(AProject: IlicgProject): TlicgBaseCmdline; stdcall;
    procedure DeleteByFileName(AFileName: string; var IgnoreClose: Boolean); stdcall;
    procedure DeleteByIndex(Index: Integer; var IgnoreClose: Boolean); stdcall;
    procedure DeleteAll(var IgnoreClose: Boolean); stdcall;
    procedure DeleteLast; stdcall;
    function Count: Integer; stdcall;
    function SetCmdlineByFileName(AFileName: string): Boolean; stdcall;
    function SetCmdlineByIndex(Index: Integer; BringToFront: Boolean = False): Boolean; stdcall;
    function GetCmdLineIndexByFileName(AFileName: string): Integer; stdcall;
    function GetCmdLineByIndex(I: Integer): TlicgBaseCmdline; stdcall;
    function GetCmdLineByFileName(AFileName: string): TlicgBaseCmdline; stdcall;
    function CurrViewPort: TForm; stdcall;
    function ViewPort(I: Integer): TForm; stdcall;
    function DisplayName(I: Integer): string; stdcall;
    function CurrDBoxPanelRect: TRect; stdcall;
    procedure SetLeftPanelGroup(lpg: TLeftPanelGroups; PageIndex: Integer = 0;
      IsTabKey: Boolean = False; ForceShowWhenAutoHide: Boolean = False); stdcall;
    function LeftPanelShowing: Boolean; stdcall;
    procedure UpdateCurrViewPortCaption; stdcall;
    procedure SetOnMainFormCloseEventFire(Event: TOnMainFormCloseEventFire); stdcall;
    procedure ScaleControlSettings; stdcall;
    procedure CoordinateCalculator(ACoorCalc: TCoordinateCalculatorType); stdcall;

    property ScaleBarVisible: Boolean read GetScaleBarVisible write SetScaleBarVisible;
    property CurrCmdline: TlicgBaseCmdline read GetCmdline;
    property CurrIndex: Integer read GetCmdLineCurrIndex write SetCmdLineCurrIndex;
  end;

implementation

end.


