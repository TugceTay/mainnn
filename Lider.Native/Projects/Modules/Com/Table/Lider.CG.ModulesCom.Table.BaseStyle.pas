unit Lider.CG.ModulesCom.Table.BaseStyle;

interface

uses
  SysUtils,
  Lider.CG.ModulesCom.Table.StyleInt,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.Alignment,
  Lider.CG.ModulesCom.Table.Borders,
  Lider.CG.ModulesCom.Table.Fonts,
  Lider.CG.ModulesCom.Table.Interior,
  Lider.CG.ModulesCom.Table.NumberFormat,
  Lider.CG.ModulesCom.Table.Protection;

type
  BaseStyle = Class(TInterfacedObject, StyleInt)
  private
    _Id: String;
    _Name: String;

    function HasName: Boolean;
  public
    function GetName: String;
    procedure SetName(Value: String);
    property Name: String read GetName write SetName;

    function GetId: String;
    procedure SetId(Value: String);
    property Id: String read GetId write SetId;

    function GetAligment: Alignment; virtual; abstract;
    procedure SetAligment(Aligment: Alignment); virtual; abstract;
    property Aligment: Alignment read GetAligment write SetAligment;

    function GetBorders: Borders; virtual; abstract;
    procedure SetBorders(Borders: Borders); virtual; abstract;
    property Borders: Borders read GetBorders write SetBorders;

    function GetFont: Fonts; virtual; abstract;
    procedure SetFont(Font: Fonts); virtual; abstract;
    property Font: Fonts read GetFont write SetFont;

    function GetInterior: Interior; virtual; abstract;
    procedure SetInterior(Interior: Interior); virtual; abstract;
    property Interior: Interior read GetInterior write SetInterior;

    function GetNumberFormat: NumberFormat; virtual; abstract;
    procedure SetNumberFormat(NumberFormat: NumberFormat); virtual; abstract;
    property NumberFormat: NumberFormat read GetNumberFormat write SetNumberFormat;

    function GetProtection: Protection; virtual; abstract;
    procedure SetProtection(Protection: Protection); virtual; abstract;
    property Protection: Protection read GetProtection write SetProtection;

    function GetElektronikTablo2003String: String;
  end;

const
  vbLf = #$A;

implementation

{ BaseStyle }

//function BaseStyle.GetAligment: Alignment;
//begin
//  result := Self.Aligment;
//end;

//function BaseStyle.GetBorders: Borders;
//begin
//  result := Self.Borders;
//end;

function BaseStyle.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append('<Style').ToString;
    strExcelXml.Append(' ss:ID="' + Id + '"');
    if HasName then
        strExcelXml.Append(' ss:Name="' + Name + '"');
    strExcelXml.Append('>');

    strExcelXml.Append(vblf + ' ' + Self.Aligment.GetElektronikTablo2003String); //vbLf
    strExcelXml.Append(vblf + ' ' + Self.Borders.GetElektronikTablo2003String);
    strExcelXml.Append(vblf + ' ' + Self.Font.GetElektronikTablo2003String);
    strExcelXml.Append(vblf + ' ' + Self.Interior.GetElektronikTablo2003String);
    strExcelXml.Append(vblf + ' ' + Self.NumberFormat.GetElektronikTablo2003String);
    strExcelXml.Append(vblf + ' ' + Self.Protection.GetElektronikTablo2003String);

    strExcelXml.Append(vblf + '</Style>');
  except on e: exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

//function BaseStyle.GetFont: Fonts;
//begin
//  result := Self.Font;
//end;

function BaseStyle.GetId: String;
begin
  result := Self._Id;
end;

//function BaseStyle.GetInterior: Interior;
//begin
//  result := Self.Interior;
//end;

function BaseStyle.GetName: String;
begin
  result := Self._Name;
end;

//function BaseStyle.GetNumberFormat: NumberFormat;
//begin
//  result := Self.NumberFormat;
//end;

//function BaseStyle.GetProtection: Protection;
//begin
//  result := Self.Protection;
//end;

function BaseStyle.HasName: Boolean;
begin
  result := False;
  if not (String.IsNullOrEmpty(Self.Name)) then
    result := True;
end;

//procedure BaseStyle.SetAligment(Aligment: Alignment);
//begin
//  Self.Aligment := Aligment;
//end;

//procedure BaseStyle.SetBorders(Borders: Borders);
//begin
//  Self.Borders := Borders;
//end;

//procedure BaseStyle.SetFont(Font: Fonts);
//begin
//  Self.Font := Font;
//end;

procedure BaseStyle.SetId(Value: String);
begin
  Self._Id := Value;
end;

//procedure BaseStyle.SetInterior(Interior: Interior);
//begin
//  Self.Interior := Interior;
//end;

procedure BaseStyle.SetName(Value: String);
begin
  Self._Name := Value;
end;

//procedure BaseStyle.SetNumberFormat(NumberFormat: NumberFormat);
//begin
//  Self.NumberFormat := NumberFormat;
//end;

//procedure BaseStyle.SetProtection(Protection: Protection);
//begin
//  Self.Protection := Protection;
//end;

end.

