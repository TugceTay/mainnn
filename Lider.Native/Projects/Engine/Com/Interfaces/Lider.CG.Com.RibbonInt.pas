unit Lider.CG.Com.RibbonInt;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Graphics,
  Forms,
  Dialogs,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.Types,
  Lider.CG.Com.DrawToolsInt;

type
  TlicgModuleEvents = (AfterModuleLoad, BeforeProjectLoad, AfterProjectLoad,
    BeforeProjectClose, BeforeActiveProjectChange, AfterActiveProjectChange,
    BeforeModuleUnload);

  TlicgCallBackEvent = function(Event: TlicgModuleEvents; Param: Pointer): Boolean; stdcall;

  TlicgCommandProc = procedure(CommandID: LongInt); stdcall;

  TlicgBooleanFuncCHCK = function(): Boolean; stdcall;

  TlicgBooleanFunc = function(CommandIndex: integer = 1): Boolean; stdcall;

  TlicgBarItemStyle = (bisButton, bisLargeButton, bisCheck, bisListStart, bisLargeListStart, bisList,
    bisCommandListStart, bisCommandLargeListStart, bisListCheck);

  TlicgAddBarItemEvent = procedure(ACaption, AShortCut, AHint: string;
    ACommandIndex: LongInt; AHelpContext: Word; ABeginGroup: Boolean;
    ABarItemStyle: TlicgBarItemStyle = bisLargeButton;
    AIsGroupCheck: Boolean = True; AModuleImageIndex: Integer = -1) of object; stdcall;


  TChangeColorEvent = procedure(const Color: TColor); stdcall;

  TlicgBarItemCommand = class
    ClientCommandIndex: integer;
    Caption: string; // ModulName ve Caption
    CommandProc: TFarProc;
    CommandEnableProc: TFarProc;
    CommandCheckedProc: TFarProc;
    ModulCommandID: Word;
  end;

  IlicgRibbon = interface
    ['{B0C53D4A-7360-466F-9812-9CDB2827BB36}']
    //Call Back Event.. Uygulama -exe tarafýndan Set edilmeli..
    function GetAddBarItemEvent: TlicgAddBarItemEvent; stdcall;
    procedure SetAddBarItemEvent(AValue: TlicgAddBarItemEvent); stdcall;
    function AddCommand(const ACaption, AShortCut, AHint: string;
      ACommandProc: TFarProc; AModuleCommandID, AHelpContext: Word; ABeginGroup: Boolean;
      ACommandEnableProc: TFarProc; ABtnStyle: TlicgBarItemStyle = bisLargeButton;
      ACommandCheckedProc: TFarProc = nil; AIsGroupCheck: Boolean = True;
      AModuleImageIndex: Integer = -1): LongInt; stdcall;

    procedure AddCallBackEvent(AModuleCallBackEvent: TFarProc); stdcall;
    procedure CallBackModuleEvents(AEvent: TlicgModuleEvents; AParam: Pointer); stdcall;

     // Uygulama -exe tarafýndan tetiklenecek...
    function ExecuteCommand(ACommandIndex: LongInt): Boolean; stdcall;
     //for Update Action Enable property
    function GetActionEnable(ACommandIndex: LongInt): Boolean; stdcall;
     //for Update Action Check property
    function GetActionChecked(ACommandIndex: LongInt): Boolean; stdcall;

     // Tüm Modul Listesinde duran komut sayýsý.
    function BarItemCommandCount: LongInt; stdcall;
     // Tüm Modul Listesinden Getirir.
    function GetBarItemCommand(ACommandIndex: LongInt): TlicgBarItemCommand; stdcall;

    function GetBarItemCommandIndexes(const AModuleName: string): IlicgIntegerList; stdcall;
    function GetBarItemCommandClientIndexes(const AModuleName: string): IlicgIntegerList; stdcall;

      // ilker ekleme
    function GetImages: TObject; stdcall;
    function GetLargeImages: TObject; stdcall;
    procedure SetImages(const Value: TObject); stdcall;
    procedure SetLargeImages(const Value: TObject); stdcall;

    function GetGlyph: TObject; stdcall;
    function GetLargeGlyph: TObject; stdcall;
    procedure SetGlyph(const Value: TObject); stdcall;
    procedure SetLargeGlyph(const Value: TObject); stdcall;

    // ilker ekleme
    property Images: TObject read GetImages write SetImages;
    property LargeImages: TObject read GetLargeImages write SetLargeImages;
    property Glyph: TObject read GetGlyph write SetGlyph;
    property LargeGlyph: TObject read GetLargeGlyph write SetLargeGlyph;

     // Uygulama -exe tarafýndan ilk baþta Set edilecek CallBack Eventi
    property OnAddBarItemEvent: TlicgAddBarItemEvent read GetAddBarItemEvent write SetAddBarItemEvent;
  end;

{$J+}
const
  LicadRibbon: IlicgRibbon = nil;
{$J-}

function EnableByLayerVariable(CommandIndex: integer = -1): Boolean; stdcall;

function EnableByGISVariable(CommandIndex: integer = -1): Boolean; stdcall;

function EnableByDrawBoxVariable(CommandIndex: integer = -1): Boolean; stdcall;

function EnableByCmdLineVariable(CommandIndex: integer = -1): Boolean; stdcall;

implementation

uses
  ActnList,
  Lider.CG.Com.LicadInt;

function EnableByCmdLineVariable(CommandIndex: integer = -1): Boolean; stdcall;
begin
  Result := (CurrCmdLine <> nil);
end;

function EnableByDrawBoxVariable(CommandIndex: integer = -1): Boolean; stdcall;
begin
  Result := EnableByCmdLineVariable(CommandIndex);
  if Result then
    Result := (CurrCmdLine.ActiveDrawBox <> nil);
end;

function EnableByGISVariable(CommandIndex: integer = -1): Boolean; stdcall;
begin
  Result := EnableByDrawBoxVariable(CommandIndex);
  if Result then
    Result := (CurrCmdLine.ActiveDrawBox.GIS <> nil) and (not CurrCmdLine.ActiveDrawBox.GIS.InProcess);
end;

function EnableByLayerVariable(CommandIndex: integer = -1): Boolean; stdcall;
begin
  Result := EnableByGISVariable(CommandIndex);
  if Result then
    Result := (CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer <> nil);
end;

end.


