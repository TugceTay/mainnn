unit Lider.CG.ModulesCom.Table.BorderInt;

interface

uses
  Lider.CG.ModulesCom.Table.PositionInt,
  Lider.CG.ModulesCom.Table.LineStyleInt,
  Classes;

type
  BorderInt = interface
    ['{33200203-0A2D-425E-AFA0-2776EC9718E7}']
    function GetPosition: PositionInt; //stdcall;
    function GetLineStyle: LineStyleInt; //stdcall;
    function GetElektronikTablo2003String: String; //stdcall;

    property Position: PositionInt read GetPosition;
    property LineStyle: LineStyleInt read GetLineStyle;
  end;

implementation
end.

