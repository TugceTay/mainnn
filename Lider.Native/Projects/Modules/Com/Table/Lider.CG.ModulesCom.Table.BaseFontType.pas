unit Lider.CG.ModulesCom.Table.BaseFontType;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt;

type
  BaseFontType = class(TInterfacedObject, FontTypeInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseFontType }

//function BaseFontType.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseFontType.ToString: String;
begin
  result := Self.Name;
end;

end.

