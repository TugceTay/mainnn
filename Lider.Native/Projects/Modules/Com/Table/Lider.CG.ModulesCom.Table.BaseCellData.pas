unit Lider.CG.ModulesCom.Table.BaseCellData;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.CellDataInt,
  SysUtils,
  System.Character,
  System.Variants,
  DateUtils;
//  Windows;

type
  BaseCellData = class(TInterfacedObject, CellDataInt)
  private
    _Values: String;
    function HasValue: Boolean;
  protected

  published
  public
    function GetElektronikTablo2003String: string; stdcall;

    function GetTypes: String; stdcall;
    property Types: String read GetTypes;

    function GetValue: String; stdcall;
    procedure SetValue(Value: String); stdcall;
    property Value: String read GetValue write SetValue;

    function GetIsEmpty: Boolean; stdcall;
    property IsEmpty: Boolean read GetIsEmpty;

    function FractionToDecimal(fraction: String): Boolean;
    function FractionToDecimal1(frac: String): String;

    function IsNumeric(s: string): Boolean;
    function isDate(const DateString: string ): Boolean;

    function ToString: string; override;
  end;

implementation
{ BaseCellData }

function BaseCellData.isDate(const DateString: string ): Boolean;
var
  fs: TFormatSettings;
  s: string;
  dt: TDateTime;
begin
  try
    s := DateString;
    fs := TFormatSettings.Create;
    fs.DateSeparator := '-';
    fs.ShortDateFormat := 'yyyy-MM-dd';
    fs.TimeSeparator := ':';
    fs.ShortTimeFormat := 'hh:mm';
    fs.LongTimeFormat := 'hh:mm:ss';
    dt := StrToDateTime(s, fs);
//    DateVal := StrToDateTime(Str);//StrToDateTime(DateString); //TryStrToDate
    result := true;
  except
    result := false;
  end;
end;

function BaseCellData.GetIsEmpty: Boolean;
begin
  result := True;
  if (not (Value = ''))
  and ((not String.IsNullOrEmpty(Value)) Or (not String.IsNullOrWhiteSpace(Value))) then
    result := False;
end;

function BaseCellData.HasValue: Boolean;
begin
  result := False;
  if (not (Value = ''))
  and ((not String.IsNullOrEmpty(Value)) Or (not String.IsNullOrWhiteSpace(Value))) then
    result := True;
end;

function IsStrANumber(const S: string): Boolean;
var
P: PChar;
begin
  P := PChar(S);
  Result := False;
  while P^ <> #0 do
  begin
  if not (P^ in ['0'..'9']) then Exit;
  Inc(P);
  end;
  Result := True;
end;

function BaseCellData.IsNumeric(s: string): Boolean;
var
  iValue, iCode: Integer;
  dValue: Double;
begin
  result := False;
  try
    if TryStrToFloat(s,dValue) then
      result := True;
  except
    result := False;
  end;
{
  result := False;
  val(s, iValue, iCode);
  if iCode = 0 then
    result := True//ShowMessage('s has a number')
}
end;

function BaseCellData.GetTypes: String;
begin
  if FractionToDecimal(Value) then
    result := 'String'
  else if IsNumeric(Value) and (Value.Length > 0) and (Value.Substring(0, 1).Length = 0) and (Value.Length > 1) then
    result := 'String'
  else if IsNumeric(Value) then
    result := 'Number'
  else if IsDate(Value) then
    result := 'DateTime'
  else
    result := 'String';
end;

function BaseCellData.GetValue: String;
begin
  result := _Values;
end;

procedure BaseCellData.SetValue(Value: String);
begin
  Self._Values := Value;
end;

function BaseCellData.ToString: string;
begin
  result := Value;
end;

function BaseCellData.FractionToDecimal(fraction: String): Boolean;
var
  remain, upper, lower: Double;
  spaceIndex, slashIndex: Integer;
begin
  try
    if fraction.Contains('/') then
    begin
//      remain := 13;//0D;
      spaceIndex := fraction.IndexOf(' ');//, StringComparison.Ordinal);
      if spaceIndex <> -1 then
      begin
//          remain := StrToFloat(fraction.Substring(0, spaceIndex));
          fraction := fraction.Substring(spaceIndex);
      end;
      slashIndex := fraction.IndexOf('/');//, StringComparison.Ordinal);
      upper := StrToFloat(fraction.Substring(0, slashIndex));
      lower := StrToFloat(fraction.Substring(slashIndex + 1));
      //'  Return remain + (upper / lower)
      result := True;
    end
    Else
      result := False;
      //'Return Decimal.Parse(fraction)
  except
    result := False;
  end;
end;

function BaseCellData.FractionToDecimal1(frac: String): String;
var
  decimalVal: String;
  upper, lower, remain: Double;
begin
  decimalVal := '0';
  upper := 0;
  lower := 0;
  remain := 0;
  If frac.IndexOf('/') <> -1 Then
  begin
    If frac.IndexOf(' ') <> -1 Then
    begin
      remain := StrToFloat(frac.Substring(0, frac.IndexOf(' ')));//CType , decimal
      frac := frac.Substring(frac.IndexOf(' '));
    End;
  End;
  upper := StrToFloat(frac.Substring(0, frac.IndexOf('/')));//CType , decimal
  lower := StrToFloat(frac.Substring(frac.IndexOf('/') + 1));//CType , decimal
  If upper > lower Then
      Result := 'Error Please Check Fraction'
  Else
      decimalVal := (remain + (upper / lower)).ToString;

  Result := decimalVal;
end;

function BaseCellData.GetElektronikTablo2003String: string;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    if HasValue then
    begin
      strExcelXml.Append('<Data').ToString;
      strExcelXml.Append(' ss:Type="' + Types + '"');
      strExcelXml.Append('>');
      strExcelXml.Append(Value);
      strExcelXml.Append('</Data>');
    end;

  except on e: exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

end.

