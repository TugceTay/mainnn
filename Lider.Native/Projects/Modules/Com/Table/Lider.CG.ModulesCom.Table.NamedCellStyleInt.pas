unit Lider.CG.ModulesCom.Table.NamedCellStyleInt;

//{$I Lider.CG.Com.Component.inc}

interface

//uses
//  Lider.CG.ModulesCom.Table.CellDataInt;
//  Windows;

type
  NamedCellStyleInt = interface
    ['{C9F30023-1631-4A09-91F7-0E9A6409C6DA}']
    function GetName: String;
    property Name: String read GetName;
  end;

implementation
end.

