unit Lider.CG.Com.QuickSelect;

interface

uses
  System.Classes,
  System.UITypes,
  Lider.CG.Com.GIS;

type
  IlicgQuickSelect = interface
    ['{E53696FE-6420-45E2-AC63-82191DA82FAF}']
    procedure MyKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MyKeyPress(Sender: TObject; var Key: Char);
    procedure MyMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyPaint(Sender: TObject);
    procedure MySuspendOperation(sender: TObject);
    procedure MyContinueOperation(sender: TObject);

    function IsDefaultAction: Boolean; stdcall;
    function CurrentActionQuickSelect: Boolean; stdcall;

    function GetLayer: TlicgBaseLayer; stdcall;
    procedure SetLayer(Value: TlicgBaseLayer); stdcall;
    function GetRecNo: Integer; stdcall;
    procedure SetRecNo(Value: Integer); stdcall;

    property Layer: TlicgBaseLayer read GetLayer write SetLayer;
    property RecNo: integer read GetRecNo write SetRecNo;
  end;


implementation

end.


