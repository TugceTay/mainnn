unit Lider.CG.ModulesCom.Table.CellInt;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.CellDataInt;
//  Windows;

type
  CellInt = interface
    ['{BE31A825-C911-458E-9B7B-70168C106258}']
//  [EditorBrowsable(EditorBrowsableState.Never)]
    function FulType: string; //stdcall;
//  [EditorBrowsable(EditorBrowsableState.Never)]
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
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

    function GetData: CellDataInt;
    procedure SetData(Data: CellDataInt);
    property Data: CellDataInt read GetData write SetData;

    function GetIsEmpty: Boolean;
    property IsEmpty: Boolean read GetIsEmpty;

    function GetIsImaginary: Boolean;
    property IsImaginary: Boolean read GetIsImaginary;
  end;

implementation
end.

