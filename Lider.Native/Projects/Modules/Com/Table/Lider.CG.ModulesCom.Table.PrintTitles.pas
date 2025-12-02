unit Lider.CG.ModulesCom.Table.PrintTitles;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseNamedCellStyle;
//  Windows;

type
  PrintTitles = class(BaseNamedCellStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
{ PrintTitles }

function PrintTitles.GetName: String;
begin
  result := 'PrintTitles';
end;

end.

