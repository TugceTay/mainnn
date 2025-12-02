unit Lider.CG.ModulesCom.Table.Column;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseCol;

type
  Col = class(BaseCol)
    private
      _Index: Integer;
    public
      function GetIndex: Integer; override;
      procedure SetIndex(Value: Integer); override;
      property Indexs: Integer read GetIndex write SetIndex;

      constructor Create; overload;
      constructor Create(Width: Single); overload;
  end;

implementation
{ Column }

constructor Col.Create;
begin
  inherited Create;
end;

constructor Col.Create(Width: Single);
begin
  inherited Create;
  inherited Width := Width;
end;

function Col.GetIndex: Integer;
begin
  result := _Index;
end;

procedure Col.SetIndex(Value: Integer);
begin
  inherited;
  Self._Index := Value;
end;

end.

