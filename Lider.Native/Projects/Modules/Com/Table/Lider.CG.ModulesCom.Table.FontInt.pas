unit Lider.CG.ModulesCom.Table.FontInt;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt;

type
  FontInt = interface
    ['{9C132570-7B51-4B8C-896A-7E4739606204}']
    function GetFontName: FontTypeInt;
    property FontName: FontTypeInt read GetFontName;

    function GetCharSet: Integer;
    procedure SetCharSet(CharSet: Integer);
    property CharSet: Integer read GetCharSet write SetCharSet;

    function GetFamily: FontTypeInt;
    property Family: FontTypeInt read GetFamily;

    function GetSize: Single;
    procedure SetSize(Size: Single);
    property Size: Single read GetSize write SetSize;

    function GetHexColor: String;
    procedure SetHexColor(HexColor: String);
    property HexColor: String read GetHexColor write SetHexColor;

    function GetBold: Boolean;
    procedure SetBold(Bold: Boolean);
    property Bold: Boolean read GetBold write SetBold;

    function GetItalic: Boolean;
    procedure SetItalic(Italic: Boolean);
    property Italic: Boolean read GetItalic write SetItalic;

    function GetStrikeThrough: Boolean;
    procedure SetStrikeThrough(StrikeThrough: Boolean);
    property StrikeThrough: Boolean read GetStrikeThrough write SetStrikeThrough;

    function GetVerticalAlign: FontTypeInt;
    property VerticalAlign: FontTypeInt read GetVerticalAlign;

    function GetUnderline: FontTypeInt;
    property Underline: FontTypeInt read GetUnderline;

    function GetElektronikTablo2003String: String;
  end;

implementation
end.

