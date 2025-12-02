unit Lider.CG.ModulesCom.Table.BasePosition;

interface

uses
  Lider.CG.ModulesCom.Table.PositionInt;

type
  BasePosition = class(TInterfacedObject, PositionInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BasePosition }

//function BasePosition.GetName: String;
//begin
//  result := Self.Name;
//end;

function BasePosition.ToString: String;
begin
  result := Self.Name;
end;

end.

