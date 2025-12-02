unit Lider.CG.ModulesCom.Table.BaseUnderlineStyle;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt;

type
  BaseUnderlineStyle = class(TInterfacedObject, FontTypeInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseUnderlineStyle }

//function BaseUnderlineStyle.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseUnderlineStyle.ToString: String;
begin
  result := Self.Name;
end;

end.

