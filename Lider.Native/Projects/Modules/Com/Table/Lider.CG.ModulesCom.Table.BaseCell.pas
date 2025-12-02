unit Lider.CG.ModulesCom.Table.BaseCell;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.CellDataInt,
  SysUtils;
//  Windows;

type
  BaseCell = class(TInterfacedObject, CellInt)
  private
    _MAcross: Integer;
    _MDown: Integer;
    _StyleId: String;
  protected
  published
    function FulType: string; //stdcall;
  //  [EditorBrowsable(EditorBrowsableState.Never)]
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(Value: Integer); virtual; abstract;
    property Indexs: Integer read GetIndex write SetIndex;

    function GetMergeAcross: Integer;
    procedure SetMergeAcross(MergeAcross: Integer);
    property MergeAcross: Integer read GetMergeAcross write SetMergeAcross;

    function GetMergeDown: Integer;
    procedure SetMergeDown(Value: Integer);
    property MergeDown: Integer read GetMergeDown write SetMergeDown;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;

    function GetData: CellDataInt; virtual; abstract;
    procedure SetData(Data: CellDataInt); virtual; abstract;
    property Data: CellDataInt read GetData write SetData;

    function GetIsEmpty: Boolean;
    property IsEmpty: Boolean read GetIsEmpty;

    function GetIsImaginary: Boolean;
    property IsImaginary: Boolean read GetIsImaginary;

    function ToString: String; override;
  end;

implementation
{ BaseCell }

function BaseCell.FulType: string;
begin
  result := Format('%d, %s, %s', [Self.Indexs, Self.Data.Types, Self.Data.Value])
end;

//function BaseCell.GetData: CellDataInt;
//begin
//  result := Self.Data;
//end;

//function BaseCell.GetIndex: Integer;
//begin
//  result := Self.Index;
//end;

function BaseCell.GetIsEmpty: Boolean;
begin
  result := Self.Data.IsEmpty;
end;

function BaseCell.GetIsImaginary: Boolean;
begin

  if (IsEmpty) And (Self.Indexs <= 0) then //<=
      result := True
  Else
      result := False;

//  result := Self.IsImaginary;
end;

function BaseCell.GetMergeAcross: Integer;
begin
  result := Self._MAcross;
end;

function BaseCell.GetMergeDown: Integer;
begin
  result := Self._MDown;
end;

function BaseCell.GetStyleID: String;
begin
  result := Self._StyleId;
end;

//procedure BaseCell.SetData(Data: CellDataInt);
//begin
//  Self.Data := Data;
//end;

procedure BaseCell.SetMergeAcross(MergeAcross: Integer);
begin
  Self._MAcross := MergeAcross;
end;

procedure BaseCell.SetMergeDown(Value: Integer);
begin
  Self._MDown := Value;
end;

procedure BaseCell.SetStyleID(StyleID: String);
begin
  Self._StyleId := StyleID;
end;

function BaseCell.ToString: String;
begin
  result := Format('%s, %s', [Self.Data.Types,Self.Data.Value]);
end;

end.

