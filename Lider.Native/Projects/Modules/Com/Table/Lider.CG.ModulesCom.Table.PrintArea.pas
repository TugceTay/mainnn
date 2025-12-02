unit Lider.CG.ModulesCom.Table.PrintArea;

//{$I Lider.CG.Com.Component.inc}

interface

uses
	Lider.CG.ModulesCom.Table.BaseNamedCellStyle;
//  Windows;

type
  PrintArea = class(BaseNamedCellStyle)
    function GetName: String; override;
    property Name: String read GetName;
  end;

implementation
{ PrintArea }

function PrintArea.GetName: String;
begin
  result := 'PrintArea';
end;

end.

