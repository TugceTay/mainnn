unit Lider.CG.ModulesCom.Table.BaseAlignmentStyle;

interface

uses
  Lider.CG.ModulesCom.Table.AlignmentStyleInt;

type
  BaseAlignmentStyle = class(TInterfacedObject, AlignmentStyleInt)
    public
    function GetName: String;  virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseAlignmentStyle }

//function BaseAlignmentStyle.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseAlignmentStyle.ToString: String;
begin
  result := Self.Name;
end;

end.

