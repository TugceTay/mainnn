unit Lider.CG.ModulesCom.Table.BaseProtection;

interface

uses
	Lider.CG.ModulesCom.Table.ProtectionInt,
  SysUtils;

type
  BaseProtection = class(TInterfacedObject, ProtectionInt)
    private
      _HideFormula: Boolean;
      _Protectet: Boolean;

      function HasHideFormula: Boolean;
      function HasProtectet: Boolean;
    public
    function GetProtectet: Boolean;
    procedure SetProtectet(Protectet: Boolean);
    property Protectet: Boolean read GetProtectet write SetProtectet;

    function GetHideFormula: Boolean;
    procedure SetHideFormula(HideFormula: Boolean);
    property HideFormula: Boolean read GetHideFormula write SetHideFormula;

    function GetElektronikTablo2003String: String;
  end;

implementation
{ BaseProtection }

function BaseProtection.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append('<Protection').ToString;
    if HasHideFormula then
        strExcelXml.Append(' x:HideFormula="' + '1' + '"');
    if HasProtectet then
        strExcelXml.Append(' ss:Protected="' + '0' + '"');
    strExcelXml.Append('/>');
  except on e: exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function BaseProtection.GetHideFormula: Boolean;
begin
  result := Self._HideFormula;
end;

function BaseProtection.GetProtectet: Boolean;
begin
  result := Self._Protectet;
end;

function BaseProtection.HasHideFormula: Boolean;
begin
  result := Self.HideFormula;
end;

function BaseProtection.HasProtectet: Boolean;
begin
  result := Protectet or True;
end;

procedure BaseProtection.SetHideFormula(HideFormula: Boolean);
begin
  Self._HideFormula := HideFormula;
end;

procedure BaseProtection.SetProtectet(Protectet: Boolean);
begin
  Self._Protectet := Protectet;
end;

end.

