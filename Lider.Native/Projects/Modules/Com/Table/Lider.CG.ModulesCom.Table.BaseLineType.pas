unit Lider.CG.ModulesCom.Table.BaseLineType;

interface

uses
  Lider.CG.ModulesCom.Table.LineStyleInt;

type
  BaseLineType = class(TInterfacedObject, LineStyleInt)
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function GetWeight: Integer; virtual; abstract;
    procedure SetWeight(Weight: Integer); virtual; abstract;
    property Weight: Integer read GetWeight write SetWeight;

    function GetHexColor: String; virtual; abstract;
    procedure SetHexColor(HexColor: String); virtual; abstract;
    property HexColor: String read GetHexColor write SetHexColor;

    function ToString: String; override;
  end;

implementation
{ BaseLineType }

//function BaseLineType.GetHexColor: String;
//begin
//  result := Self.HexColor;
//end;

//function BaseLineType.GetName: String;
//begin
//  result := Self.Name;
//end;

//function BaseLineType.GetWeight: Integer;
//begin
//  result := Self.Weight;
//end;

//procedure BaseLineType.SetHexColor(HexColor: String);
//begin
//  Self.HexColor := HexColor;
//end;

//procedure BaseLineType.SetWeight(Weight: Integer);
//begin
//  Self.Weight := Weight;
//end;

function BaseLineType.ToString: String;
begin
  result := Self.Name;
end;

end.

