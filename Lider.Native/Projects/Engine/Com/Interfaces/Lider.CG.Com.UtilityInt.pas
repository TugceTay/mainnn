unit Lider.CG.Com.UtilityInt;

interface

uses
  Classes,
  Controls,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.ModulesInt;

type
  TDlgMode = (dmOK, dmOkCancel);

{*** Picture masks ***}
const
  {the following characters are meaningful in Picture masks}
  pmAnyChar = 'X';         {allows any character}
  pmForceUpper = '!';         {allows any character, forces upper case}
  pmForceLower = 'L';         {allows any character, forces lower case}
  pmForceMixed = 'x';         {allows any character, forces mixed case}
  pmAlpha = 'a';         {allows alphas only}
  pmUpperAlpha = 'A';         {allows alphas only, forces upper case}
  pmLowerAlpha = 'l';         {allows alphas only, forces lower case}
  pmPositive = '9';         {allows numbers and spaces only}
  pmWhole = 'i';         {allows numbers, spaces, minus}
  pmDecimal = '#';         {allows numbers, spaces, minus, period}
  pmScientific = 'E';         {allows numbers, spaces, minus, period, 'e'}
  pmHexadecimal = 'K';         {allows 0-9, A-F, and space forces upper case}
  pmOctal = 'O';         {allows 0-7, space}
  pmBinary = 'b';         {allows 0-1, space}
  pmTrueFalse = 'B';         {allows T, t, F, f}
  pmYesNo = 'Y';         {allows Y, y, N, n}

type
  IInputBox = interface
    ['{FB20FA15-17A7-4E78-991B-B3C347BB2AC3}']
    procedure setCombo(_Cap: string; Value: TStringList; itemIndex: Integer); stdcall;
    procedure setRadio(_Cap: string; Value: Boolean; editable: Boolean = True); stdcall;
    procedure setCheck(_Cap: string; Value: Boolean; editable: Boolean = True); stdcall;
    procedure setFloat(_Cap: string; Value: Double; editable: Boolean = True); stdcall;
    procedure setInt(_Cap: string; Value: integer; editable: Boolean = True); stdcall;
    procedure setString(_Cap: string; Value: string; editable: Boolean = True; maxLength: Integer = 0); stdcall;
    procedure SetLabel(_Cap: string; Value: string); stdcall;
    procedure setDate(_Cap: string; Value: TDateTime; showtime: boolean; editable: Boolean = True); stdcall;
    function ShowModal: TModalResult; stdcall;
    function getRadio(_Cap: string): Boolean; stdcall;
    function getCheck(_Cap: string): Boolean; stdcall;
    function getFloat(_Cap: string): Double; stdcall;
    function getInt(_Cap: string): integer; stdcall;
    function getString(_Cap: string): string; stdcall;
    function getComboItemIndex(_Cap: string): Integer; stdcall;
    function getComboItemText(_Cap: string): string; stdcall;
    function getDate(_Cap: string): TDateTime; stdcall;
  end;

function GetInputBox(cap: string; DlgMode: TDlgMode): IInputBox;

function GetText(fmCaption, LblCaption: string; var EditText: string; Mask:
  AnsiChar; MaxLenth: integer = 255): boolean;

function LayerSelectDialog(const ACmdLine: TlicgBaseCmdLine; ALayerList: TList; AMultiSelect: Boolean = False;
  AFormCaption: string = ''; ALayerTypes: TlicgLayerClassTypes = [lctLayer]): TModalResult; stdcall;

function GetStrings(cap: string; getStrs: TStrings): Boolean;

function GetStringFromComboBox(cap: string; Strs: TStrings): string; stdcall;

type
  IlicgUtility = interface
    ['{70624C18-C87A-4AA8-867D-8C4CA2952133}']
    function GetInputBox(cap: string; DlgMode: TDlgMode): IInputBox; stdcall;
    function GetText(fmCaption, LblCaption: string; var EditText: string; Mask:
      AnsiChar; MaxLenth: integer = 255): boolean; stdcall;
    function GetStrings(cap: string; getStrs: TStrings): Boolean; stdcall;
    function GetStringFromComboBox(cap: string; Strs: TStrings): string; stdcall;
    function LayerSelectDialog(const ACmdLine: TlicgBaseCmdLine; ALayerList: TList; AMultiSelect: Boolean = False;
      AFormCaption: string = ''; ALayerTypes: TlicgLayerClassTypes = [lctLayer]): TModalResult; stdcall;
  end;

implementation

uses
  Lider.CG.Com.LicadInt;

function GetInputBox(cap: string; DlgMode: TDlgMode): IInputBox;
var
  Utility_Int: IlicgUtility;
begin
  result := nil;
  Utility_Int := Licad.GetUtility;
  if Utility_Int <> nil then
  begin
    result := Utility_Int.GetInputBox(cap, DlgMode);
  end;
  Utility_Int := nil;
end;

function GetText(fmCaption, LblCaption: string; var EditText: string; Mask:
  AnsiChar; MaxLenth: integer = 255): boolean;
var
  Utility_Int: IlicgUtility;
begin
  result := false;
  Utility_Int := Licad.GetUtility;
  if Utility_Int <> nil then
  begin
    result := Utility_Int.GetText(fmCaption, LblCaption, EditText, Mask, MaxLenth)
  end;
  Utility_Int := nil;
end;

function LayerSelectDialog(const ACmdLine: TlicgBaseCmdLine; ALayerList: TList; AMultiSelect: Boolean = False;
  AFormCaption: string = ''; ALayerTypes: TlicgLayerClassTypes = [lctLayer]): TModalResult; stdcall;
var
  Utility_Int: IlicgUtility;
begin
  Utility_Int := Licad.GetUtility;
  if Utility_Int <> nil then
  begin
    Result := Utility_Int.LayerSelectDialog(ACmdLine, ALayerList, AMultiSelect, AFormCaption, ALayerTypes);
  end;
  Utility_Int := nil;
end;

function GetStrings(cap: string; getStrs: TStrings): Boolean;
var
  Utility_Int: IlicgUtility;
begin
  result := false;
  Utility_Int := Licad.GetUtility;
  if Utility_Int <> nil then
  begin
    result := Utility_Int.GetStrings(cap, getStrs);
  end;
  Utility_Int := nil;
end;

function GetStringFromComboBox(cap: string; Strs: TStrings): string; stdcall;
var
  Utility_Int: IlicgUtility;
begin
  result := '';
  Utility_Int := Licad.GetUtility;
  if Utility_Int <> nil then
  begin
    result := Utility_Int.GetStringFromComboBox(Cap, Strs);
  end;
  Utility_Int := nil;
end;

end.


