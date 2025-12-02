unit Lider.CG.ModulesCom.Table.BaseFont;

interface

uses
  Lider.CG.ModulesCom.Table.FontInt,
  Lider.CG.ModulesCom.Table.FontTypeInt,
  SysUtils,
  StrUtils;

type
  BaseFont = class(TInterfacedObject, FontInt)

  private
    _Bold: Boolean;
    _Charset: Integer;
    _Color: String;
    _Italic: Boolean;
    _Size: Single;
    _StrikeThrough: Boolean;

    function HasVerticalAlign: Boolean;
    function HasUnderline: Boolean;
    function HasCharSet: Boolean;
    function HasColor: Boolean;
    function IsBold: Boolean;
    function IsItalic: Boolean;
    function IsStrikeThrough: Boolean;
  public
    function GetFamily: FontTypeInt; virtual; abstract;
    property Family: FontTypeInt read GetFamily;

    function GetFontName: FontTypeInt; virtual; abstract;
    property FontName: FontTypeInt read GetFontName;

    function GetVerticalAlign: FontTypeInt; virtual; abstract;
    property VerticalAlign: FontTypeInt read GetVerticalAlign;

    function GetUnderline: FontTypeInt; virtual; abstract;
    property Underline: FontTypeInt read GetUnderline;

    function GetElektronikTablo2003String: String;

    function GetBold: Boolean;
    procedure SetBold(Bold: Boolean);
    property Bold: Boolean read GetBold write SetBold;

    function GetCharSet: Integer;
    procedure SetCharSet(CharSet: Integer);
    property CharSet: Integer read GetCharSet write SetCharSet;

    function GetHexColor: String;
    procedure SetHexColor(HexColor: String);
    property HexColor: String read GetHexColor write SetHexColor;

    function GetItalic: Boolean;
    procedure SetItalic(Italic: Boolean);
    property Italic: Boolean read GetItalic write SetItalic;

    function GetSize: Single;
    procedure SetSize(Size: Single);
    property Size: Single read GetSize write SetSize;

    function GetStrikeThrough: Boolean;
    procedure SetStrikeThrough(StrikeThrough: Boolean);
    property StrikeThrough: Boolean read GetStrikeThrough write SetStrikeThrough;
  end;

implementation
{ BaseFont }

function BaseFont.GetBold: Boolean;
begin
  result := Self._Bold;
end;

function BaseFont.GetCharSet: Integer;
begin
  result := Self._Charset;
end;

function BaseFont.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
  a: String;
begin
  strExcelXml := TStringBuilder.Create;
  a := '';
  try
    strExcelXml.Append('<Font').ToString;

    strExcelXml.Append(' ss:FontName="' + FontName.Name + '"');

    if HasCharSet then
        strExcelXml.Append(' x:CharSet="' + CharSet.ToString + '"');

    strExcelXml.Append(' x:Family="' + Family.Name + '"');

    strExcelXml.Append(' ss:Size="' + Size.ToString + '"');

    if HasColor then
        strExcelXml.Append(' ss:Color="' + IfThen((not HexColor.Contains('#')),'#'+HexColor,HexColor) + '"');

    if IsBold then
        strExcelXml.Append(' ss:Bold="' + '1' + '"');

    if IsItalic then
        strExcelXml.Append(' ss:Italic="' + '1' + '"');

    if IsStrikeThrough then
        strExcelXml.Append(' ss:StrikeThrough="' + '1' + '"');

    if HasVerticalAlign then
        strExcelXml.Append(' ss:VerticalAlign="' + VerticalAlign.Name + '"');

    if HasUnderline then
        strExcelXml.Append(' ss:Underline="' + Underline.Name + '"');

    strExcelXml.Append('/>');
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

//function BaseFont.GetFamily: FontTypeInt;
//begin
//  result := Self.Family;
//end;

//function BaseFont.GetFontName: FontTypeInt;
//begin
//  result := Self.FontName;
//end;

function BaseFont.GetHexColor: String;
begin
  result := Self._Color;
end;

function BaseFont.GetItalic: Boolean;
begin
  result := Self._Italic;
end;

function BaseFont.GetSize: Single;
begin
  result := Self._Size;
end;

function BaseFont.GetStrikeThrough: Boolean;
begin
  result := Self._StrikeThrough;
end;

//function BaseFont.GetUnderline: FontTypeInt;
//begin
//  result := Self.Underline;
//end;

//function BaseFont.GetVerticalAlign: FontTypeInt;
//begin
//  result := Self.VerticalAlign;
//end;

function BaseFont.HasCharSet: Boolean;
begin
  result := False;
  if (Self.CharSet <> 0) then
    result := True;
end;

function BaseFont.HasColor: Boolean;
begin
  result := False;
  if not (String.IsNullOrEmpty(Self.HexColor)) then
    if not (Self.HexColor = '#FFFFFF') then
      result := True;
end;

function BaseFont.HasUnderline: Boolean;
begin
  result := False;
  if not (Self.Underline = nil) then
    if not (String.IsNullOrEmpty(Self.Underline.Name)) then
      result := True;
end;

function BaseFont.HasVerticalAlign: Boolean;
begin
  result := False;
  if not (Self.VerticalAlign = nil) then
    if not (String.IsNullOrEmpty(Self.VerticalAlign.Name)) then
      result := True;
end;

function BaseFont.IsBold: Boolean;
begin
  result := Self.Bold;
end;

function BaseFont.IsItalic: Boolean;
begin
  result := Self.Italic;
end;

function BaseFont.IsStrikeThrough: Boolean;
begin
  result := Self.StrikeThrough;
end;

procedure BaseFont.SetBold(Bold: Boolean);
begin
  Self._Bold := Bold;
end;

procedure BaseFont.SetCharSet(CharSet: Integer);
begin
  Self._CharSet := CharSet;
end;

procedure BaseFont.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure BaseFont.SetItalic(Italic: Boolean);
begin
  Self._Italic := Italic;
end;

procedure BaseFont.SetSize(Size: Single);
begin
  Self._Size := Size;
end;

procedure BaseFont.SetStrikeThrough(StrikeThrough: Boolean);
begin
  Self._StrikeThrough := StrikeThrough;
end;

end.

