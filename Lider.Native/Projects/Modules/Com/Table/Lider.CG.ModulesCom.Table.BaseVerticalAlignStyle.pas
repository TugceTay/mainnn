unit Lider.CG.ModulesCom.Table.BaseVerticalAlignStyle;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt;

type
  BaseVerticalAlignStyle = class(TInterfacedObject, FontTypeInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseVerticalAlignStyle }

//function BaseVerticalAlignStyle.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseVerticalAlignStyle.ToString: String;
begin
  result := Self.Name;
end;

end.

