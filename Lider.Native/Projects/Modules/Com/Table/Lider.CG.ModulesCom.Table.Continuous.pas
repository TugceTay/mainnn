unit Lider.CG.ModulesCom.Table.Continuous;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  Continuous = class(BaseLineType)

    private
      _Weight: Integer;
      _Color: String;
    public
      function GetName: String; override;
      property Name: String read GetName;

      function GetWeight: Integer; override;
      procedure SetWeight(Weight: Integer); override;
      property Weight: Integer read GetWeight write SetWeight;

      function GetHexColor: String; override;
      procedure SetHexColor(Value: String); override;
      property HexColor: String read GetHexColor write SetHexColor;
  end;

implementation
{ Continuous }

function Continuous.GetHexColor: String;
begin
  result := Self._Color;
end;

function Continuous.GetName: String;
begin
  result := 'Continuous';
end;

function Continuous.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure Continuous.SetHexColor(Value: String);
begin
  Self._Color := Value;
end;

procedure Continuous.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

