unit Lider.CG.ModulesCom.Table.Cell;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.BaseCell,
  Lider.CG.ModulesCom.Table.CellData,
  Lider.CG.ModulesCom.Table.CellDataInt;
//  Windows;

type
  Cell = class(BaseCell)
  private
    _Data: CellDataInt;
    _Index: Integer;
  public
    function GetData: CellDataInt; override;
    procedure SetData(Data: CellDataInt); override;
    property Data: CellDataInt read GetData write SetData;

    function GetIndex: Integer; override;
    procedure SetIndex(Value: Integer); override;
    property Indexs: Integer read GetIndex write SetIndex;

    constructor Create; overload;
    constructor Create(StyleId: String; MergeAcross: Integer; MergeDown: Integer; Data: CellData); overload;
    constructor Create(StyleId: String; MergeAcross: Integer; MergeDown: Integer); overload;
    constructor Create(StyleId: String; MergeAcross: Integer); overload;
    constructor Create(StyleId: String); overload;
    constructor Create(StyleId: String; Data: String); overload;
    constructor Create(StyleId: String; MergeAcross: Integer; Data: String); overload;
    constructor Create(StyleId: String; MergeAcross: Integer; MergeDown: Integer; Data: String); overload;
  end;

implementation
{ Cell }

constructor Cell.Create(StyleId: String; MergeAcross: Integer);
begin
  Self.Create(StyleId, MergeAcross, 0, CellData.Create);
end;

constructor Cell.Create(StyleId: String; MergeAcross, MergeDown: Integer);
begin
  Self.Create(StyleId, MergeAcross, MergeDown, CellData.Create);
end;

constructor Cell.Create(StyleId: String; MergeAcross, MergeDown: Integer;
  Data: CellData);
begin
  inherited Create;
  inherited MergeAcross := MergeAcross;
  inherited MergeDown := MergeDown;
  inherited StyleID := StyleId;
  _Data := Data;
  Self._Index := -1;
end;

constructor Cell.Create;
begin
  inherited Create;
  inherited MergeAcross := 0;
  inherited MergeDown := 0;
  inherited StyleID := '';
  _Data := CellData.Create;
  Self._Index := -1;
end;

constructor Cell.Create(StyleId: String; MergeAcross, MergeDown: Integer;
  Data: String);
begin
  Self.Create(StyleId, MergeAcross, MergeDown, CellData.Create(Data));
end;

constructor Cell.Create(StyleId: String; MergeAcross: Integer; Data: String);
begin
  Self.Create(StyleId, MergeAcross, 0, CellData.Create(Data));
end;

constructor Cell.Create(StyleId, Data: String);
begin
  Self.Create(StyleId, 0, 0, CellData.Create(Data));
end;

constructor Cell.Create(StyleId: String);
begin
  Self.Create(StyleId, 0, 0, CellData.Create);
end;

function Cell.GetData: CellDataInt;
begin
  result := Self._Data;
end;

function Cell.GetIndex: Integer;
begin
  result := Self._Index;
end;

procedure Cell.SetData(Data: CellDataInt);
begin
  Self._Data := Data;
end;

procedure Cell.SetIndex(Value: Integer);
begin
  inherited;
  Self._Index := Value;
end;

end.

