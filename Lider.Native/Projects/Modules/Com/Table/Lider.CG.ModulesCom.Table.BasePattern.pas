unit Lider.CG.ModulesCom.Table.BasePattern;

interface

uses
    Lider.CG.ModulesCom.Table.PatternInt;

type
  BasePattern = class(TInterfacedObject, PatternInt)
    public
      function GetName: String; virtual; abstract;
      property Name: String read GetName;

      function GetHexPatternColor: String; virtual; abstract;
      procedure SetHexPatternColor(HexPatternColor: String); virtual; abstract;
      property HexPatternColor: String read GetHexPatternColor write SetHexPatternColor;

      function ToString: String; override;
  end;

implementation
{ BasePattern }

//function BasePattern.GetHexPatternColor: String;
//begin
//  result := Self.HexPatternColor;
//end;

//function BasePattern.GetName: String;
//begin
//  result := Self.Name;
//end;

//procedure BasePattern.SetHexPatternColor(HexPatternColor: String);
//begin
//  Self.HexPatternColor := HexPatternColor;
//end;

function BasePattern.ToString: String;
begin
  result := Self.Name;
end;

end.

