unit Lider.CG.ModulesCom.Table.BaseNumberFormat;

interface

uses
	Lider.CG.ModulesCom.Table.NumberFormatInt,
  SysUtils;

type
  BaseNumberFormat = class(TInterfacedObject, NumberFormatInt)
    private
      _format: String;
      function HasFormat: Boolean;
    public
      function GetElektronikTablo2003String: String;

      function GetFormat: String;
      procedure SetFormat(Format: String);
      property Format: String read GetFormat write SetFormat;
  end;

implementation
{ BaseNumberFormat }

function BaseNumberFormat.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;

  try
    strExcelXml.Append('<NumberFormat').ToString;
    if HasFormat then
        strExcelXml.Append(' ss:Format="' + Format + '"');

    strExcelXml.Append('/>');
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function BaseNumberFormat.GetFormat: String;
begin
  result := Self._format;
end;

function BaseNumberFormat.HasFormat: Boolean;
begin
  result := False;
  if (Format <> '') then
    result := True;
end;

procedure BaseNumberFormat.SetFormat(Format: String);
begin
  Self._format := Format;
end;

end.

