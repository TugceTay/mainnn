unit Lider.CG.ModulesCom.Table.BaseNamedCell;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.NamedCellInt,
  Lider.CG.ModulesCom.Table.NamedCellStyleInt,
  SysUtils;
//  Windows;

type
  BaseNamedCell = class(TInterfacedObject, NamedCellInt)
  private
    function HasNamedCell: Boolean;
  public
    function GetNamedCell: NamedCellStyleInt; virtual; abstract;
    property NamedCell: NamedCellStyleInt read GetNamedCell;

    function GetRefersTo: String; virtual; abstract;
    property RefersTo: String read GetRefersTo;

    function GetElektronikTablo2003String: String; virtual;
  end;

implementation
{ BaseNamedCell }

function BaseNamedCell.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    if HasNamedCell then
    begin
      strExcelXml.Append('<NamedCell').ToString;
      strExcelXml.Append(' ss:Name="' + NamedCell.Name + '"' + '/>');
    end;
  except
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

//function BaseNamedCell.GetNamedCell: NamedCellStyleInt;
//begin
//  result := Self.NamedCell;
//end;

function BaseNamedCell.HasNamedCell: Boolean;
begin
  if not (NamedCell = nil) then
  begin
      if (not String.IsNullOrEmpty(NamedCell.Name)) Or
      (not String.IsNullOrWhiteSpace(NamedCell.Name)) then
        result := True;
  end;
  result := False;
end;

end.

