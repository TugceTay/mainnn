unit Lider.CG.ModulesCom.Table.BaseInterior;

interface

uses
    Lider.CG.ModulesCom.Table.InteriorInt,
    Lider.CG.ModulesCom.Table.PatternInt,
    SysUtils,
    StrUtils;

type
  BaseInterior = class(TInterfacedObject, InteriorInt)
    private
      _Color: String;
      function HasColor: Boolean;
      function HasPattern: Boolean;
      function HasPatternColor: Boolean;
    public
      function GetPattern: PatternInt; virtual; abstract;
      property Pattern: PatternInt read GetPattern;

      function GetElektronikTablo2003String: String;

      function GetHexColor: String;
      procedure SetHexColor(HexColor: String);
      property HexColor: String read GetHexColor write SetHexColor;

      function ToString: String; override;
  end;

implementation
{ BaseInterior }

function BaseInterior.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append('<Interior').ToString;

    if HasColor then
        strExcelXml.Append(' ss:Color="' + IfThen((not HexColor.Contains('#')),'#'+HexColor,HexColor) + '"');

    if HasPattern then
        strExcelXml.Append(' ss:Pattern="' + Pattern.Name + '"');

    if HasPatternColor then
        strExcelXml.Append(' ss:PatternColor="' + Pattern.HexPatternColor + '"');

    strExcelXml.Append('/>');
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function BaseInterior.GetHexColor: String;
begin
  result := Self._Color;
end;

//function BaseInterior.GetPattern: PatternInt;
//begin
//  result := Self.Pattern;
//end;

function BaseInterior.HasColor: Boolean;
begin
  result := False;
  if not (String.IsNullOrEmpty(Self.HexColor)) then
    if not (Self.HexColor = '#FFFFFF') then
      result := True;
end;

function BaseInterior.HasPattern: Boolean;
begin
  result := False;
  if not (Self.Pattern = nil) then
    if not (String.IsNullOrEmpty(Self.Pattern.Name)) then
      result := True;
end;

function BaseInterior.HasPatternColor: Boolean;
begin
  result := False;
  if not (Self.Pattern = nil) then
    if not (Self.Pattern.HexPatternColor = '#FFFFFF') then
      result := True;
end;

procedure BaseInterior.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

function BaseInterior.ToString: String;
begin
  result := Format('%s', [Self.Pattern.Name]);
end;

end.

