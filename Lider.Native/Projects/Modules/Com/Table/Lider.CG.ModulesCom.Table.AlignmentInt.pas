unit Lider.CG.ModulesCom.Table.AlignmentInt;

interface

uses
  Lider.CG.ModulesCom.Table.AlignmentStyleInt;

type
  AlignmentInt = interface
    ['{6314F3A5-3BB3-40FF-BB1C-BFFB65CE96F8}']
    function GetHorizontal: AlignmentStyleInt;
    property Horizontal: AlignmentStyleInt read GetHorizontal;

    function GetVertical: AlignmentStyleInt;
    property Vertical: AlignmentStyleInt read GetVertical;

    function GetReadingOrder: AlignmentStyleInt;
    property ReadingOrder: AlignmentStyleInt read GetReadingOrder;

    function GetIndent: Integer;
    procedure SetIndent(Indent: Integer);
    property Indent: Integer read GetIndent write SetIndent;

    function GetRotate: Integer;
    procedure SetRotate(Rotate: Integer);
    property Rotate: Integer read GetRotate write SetRotate;

    function GetShrinkToFit: Boolean;
    procedure SetShrinkToFit(ShrinkToFit: Boolean);
    property ShrinkToFit: Boolean read GetShrinkToFit write SetShrinkToFit;

    function GetWrapText: Boolean;
    procedure SetWrapText(WrapText: Boolean);
    property WrapText: Boolean read GetWrapText write SetWrapText;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

