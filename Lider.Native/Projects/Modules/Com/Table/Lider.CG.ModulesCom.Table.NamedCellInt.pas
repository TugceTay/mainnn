unit Lider.CG.ModulesCom.Table.NamedCellInt;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.NamedCellStyleInt;
//  Windows;

type
  NamedCellInt = interface
    ['{74043EF7-0D9C-433A-89DA-7922B71D99AE}']
    function GetNamedCell: NamedCellStyleInt;
    property NamedCell: NamedCellStyleInt read GetNamedCell;

    function GetRefersTo: String;
    property RefersTo: String read GetRefersTo;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

