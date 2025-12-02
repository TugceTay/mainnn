unit Lider.CG.ModulesCom.Table.BaseFamilyType;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt;

type
  BaseFamilyType = class(TInterfacedObject, FontTypeInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseFamilyType }

//function BaseFamilyType.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseFamilyType.ToString: String;
begin
  result := Self.Name;
end;

end.

