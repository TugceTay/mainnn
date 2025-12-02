unit Lider.CG.ModulesCom.Table.BaseReadingOrder;

interface

uses
  Lider.CG.ModulesCom.Table.AlignmentStyleInt;

type
  BaseReadingOrder = class(TInterfacedObject, AlignmentStyleInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseReadingOrder }

//function BaseReadingOrder.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseReadingOrder.ToString: String;
begin
  result := Self.Name;
end;

end.

