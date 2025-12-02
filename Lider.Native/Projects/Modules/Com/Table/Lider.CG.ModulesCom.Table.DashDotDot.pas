unit Lider.CG.ModulesCom.Table.DashDotDot;

interface

uses
  Lider.CG.ModulesCom.Table.BaseLineType;

type
  DashDotDot = class(BaseLineType)

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
      procedure SetHexColor(HexColor: String); override;
      property HexColor: String read GetHexColor write SetHexColor;
  end;

implementation
{ DashDotDot }

function DashDotDot.GetHexColor: String;
begin
  result := Self._Color;
end;

function DashDotDot.GetName: String;
begin
  result := 'DashDotDot';
end;

function DashDotDot.GetWeight: Integer;
begin
  result := Self._Weight;
end;

procedure DashDotDot.SetHexColor(HexColor: String);
begin
  Self._Color := HexColor;
end;

procedure DashDotDot.SetWeight(Weight: Integer);
begin
  Self._Weight := Weight;
end;

end.

