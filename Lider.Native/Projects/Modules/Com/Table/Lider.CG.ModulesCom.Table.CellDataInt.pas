unit Lider.CG.ModulesCom.Table.CellDataInt;

//{$I Lider.CG.Com.Component.inc}

interface

//uses
//  Windows;

type
  CellDataInt = interface
    ['{C137F9E1-9813-4CC1-AAAD-10CA51D48FD6}']
    function GetTypes: String; stdcall;
    property Types: String read GetTypes;

    function GetValue: String; stdcall;
    procedure SetValue(Value: String); stdcall;
    property Value: String read GetValue write SetValue;

    function GetIsEmpty: Boolean; stdcall;
    property IsEmpty: Boolean read GetIsEmpty;

    function GetElektronikTablo2003String: String; stdcall;
  end;

implementation
end.

