unit Lider.CG.ModulesCom.Table.RowInt;

interface

uses
  Lider.CG.ModulesCom.Table.Cells;

type
  RowInt = interface
    ['{58EEC097-D04E-48DC-B7E2-CDCF1B7279E2}']
//    <EditorBrowsable(EditorBrowsableState.Never), Browsable(False)>
    function FulType: String;
//    <EditorBrowsable(EditorBrowsableState.Never), Browsable(False)>
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
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

    function GetCell: Cells;
    procedure SetCell(Cell: Cells);
    property Cell: Cells read GetCell write SetCell;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;
  end;

implementation
end.

