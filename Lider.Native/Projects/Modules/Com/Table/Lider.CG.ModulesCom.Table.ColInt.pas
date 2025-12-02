unit Lider.CG.ModulesCom.Table.ColInt;

interface

//uses

type
  ColInt = interface
    ['{848FAE05-7BE4-46EE-8CD0-12EF62747489}']
//    <EditorBrowsable(EditorBrowsableState.Never), Browsable(False)>
    function FulType: String;
//    <EditorBrowsable(EditorBrowsableState.Never), Browsable(False)>
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    property Indexs: Integer read GetIndex write SetIndex;

    function GetAutoFitWidth: Boolean;
    procedure SetAutoFitWidth(AutoFitWidth: Boolean);
    property AutoFitWidth: Boolean read GetAutoFitWidth write SetAutoFitWidth;

    function GetWidth: Single;
    procedure SetWidth(Width: Single);
    property Width: Single read GetWidth write SetWidth;

    function GetHidden: Boolean;
    procedure SetHidden(Hidden: Boolean);
    property Hidden: Boolean read GetHidden write SetHidden;

//    ''' <summary>
//    ''' Kendisi ile Ayný öznitelikte olan kendinden sonraki colon adedidir.
//    ''' </summary>
//    ''' <value></value>
//    ''' <returns></returns>
//    ''' <remarks></remarks>
    function GetSpan: Integer;
    procedure SetSpan(Span: Integer);
    property Span: Integer read GetSpan write SetSpan;

    function GetStyleID: String;
    procedure SetStyleID(StyleID: String);
    property StyleID: String read GetStyleID write SetStyleID;
  end;

implementation
end.

