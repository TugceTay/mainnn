unit Lider.CG.ModulesCom.Table.BaseRow;

interface

uses
  Lider.CG.ModulesCom.Table.RowInt,
  Lider.CG.ModulesCom.Table.Cells,
  Sysutils;

type
  BaseRow = Class(TInterfacedObject, RowInt)
  private
    _AutoFitHeight: Boolean;
    _Height: Single;
    _Hidden: Boolean;
    _StyleId: String;

  public
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(Value: Integer); virtual; abstract;
    property Indexs: Integer read GetIndex write SetIndex;

    function GetAutoFitHeight: Boolean;
    procedure SetAutoFitHeight(AutoFitHeight: Boolean);
    property AutoFitHeight: Boolean read GetAutoFitHeight write SetAutoFitHeight;

    function GetHeight: Single;
    procedure SetHeight(Height: Single);
    property Height: Single read GetHeight write SetHeight;

    function GetHidden: Boolean;
    procedure SetHidden(Hidden: Boolean);
    property Hidden: Boolean read GetHidden write SetHidden;

    function GetCell: Cells; virtual; abstract;
    procedure SetCell(Cell: Cells); virtual; abstract;
    property Cell: Cells read GetCell write SetCell;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;

    function ToString: String; override;
    function FulType: String;
    constructor Create;
  end;

implementation
{ BaseRow }

constructor BaseRow.Create;
begin
  inherited Create;
end;

function BaseRow.FulType: String;
begin
  result := Format('%d, %d, %d',[Self.Indexs,Self.Height,Self.Cell.Count]);
end;

function BaseRow.GetAutoFitHeight: Boolean;
begin
  result := Self._AutoFitHeight;
end;

function BaseRow.GetHidden: Boolean;
begin
  result := Self._Hidden;
end;

//function BaseRow.GetIndex: Integer;
//begin
//  result := Self.Index;
//end;

//function BaseRow.GetCell: Cells;
//begin
//  result := Self._Cell;
//end;

function BaseRow.GetStyleID: String;
begin
  result := Self._StyleId;
end;

function BaseRow.GetHeight: Single;
begin
  result := Self._Height;
end;

procedure BaseRow.SetAutoFitHeight(AutoFitHeight: Boolean);
begin
  Self._AutoFitHeight := AutoFitHeight;
end;

procedure BaseRow.SetHidden(Hidden: Boolean);
begin
  _Hidden := Hidden;
end;

//procedure BaseRow.SetCell(Cell: Cells);
//begin
//  _Cell := Cell;
//end;

procedure BaseRow.SetStyleID(StyleID: String);
begin
  _StyleId := StyleID;
end;

procedure BaseRow.SetHeight(Height: Single);
begin
  _Height := Height;
end;

function BaseRow.ToString: String;
begin
  result := Format('%d, %d',[Self.Indexs,Self.Height]);
end;

end.

