unit Lider.CG.ModulesCom.Table.BaseBorder;

interface

uses
  Lider.CG.ModulesCom.Table.BorderInt,
  Lider.CG.ModulesCom.Table.LineStyleInt,
  Lider.CG.ModulesCom.Table.PositionInt,
  SysUtils,
  StrUtils;

type
  BaseBorder = class(TInterfacedObject, BorderInt)

    private
      function HasBorder: Boolean;
      function HasColor: Boolean;
    public
      function GetLineStyle: LineStyleInt; virtual; abstract;
      property LineStyle: LineStyleInt read GetLineStyle;

      function GetPosition: PositionInt; virtual; abstract;
      property Position: PositionInt read GetPosition;

      function GetElektronikTablo2003String: String;

      function ToString: String; override;
  end;

implementation
{ BaseBorder }

function BaseBorder.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
      If HasBorder Then
          strExcelXml.Append('<Border ss:Position="' + Position.Name + '"'
                           + ' ss:LineStyle="' + LineStyle.Name + '"'
                           + ' ss:Weight="' + LineStyle.Weight.ToString + '"').ToString;

    if HasColor then
      strExcelXml.Append(' ss:Color="' + IfThen((not LineStyle.HexColor.Contains('#')),'#'+LineStyle.HexColor,LineStyle.HexColor) + '"' + '/>')
    else
      strExcelXml.Append('/>');

  except on e: Exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

//function BaseBorder.GetLineStyle: LineStyleInt;
//begin
//  result := Self.LineStyle;
//end;

//function BaseBorder.GetPosition: PositionInt;
//begin
//  result := Self.Position;
//end;

function BaseBorder.HasBorder: Boolean;
begin
  result := False;
  if not (Self.Position = nil) then
    if not (Self.LineStyle = nil) then
      if (Position.Name <> '') or (LineStyle.Name <> '') or (LineStyle.Weight <> 0) then
        result := True;
end;

function BaseBorder.HasColor: Boolean;
begin
  result := False;
  if not (Self.LineStyle = nil) then
    if not (Self.LineStyle.HexColor = '#FFFFFF') then
      result := True;
end;

function BaseBorder.ToString: String;
begin
  result := Format('%s, %s', [Self.Position.Name, Self.LineStyle.Name]);
end;

end.

