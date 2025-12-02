unit Lider.CG.Com.ListsInt;

interface

uses
  Classes,
  Lider.CG.Com.GeoTypes;

type
  IlicgIntegerList = interface
    ['{AAE09AEA-991E-4770-A3FD-B88428F5CD4E}']
    function Get(Index: Integer): Integer; stdcall;
    procedure Put(Index: Integer; Value: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    procedure SetCapacity(Value: Integer); stdcall;
    function GetCount: Integer; stdcall;
    procedure SetCount(Value: Integer); stdcall;
    procedure SetList(value: TList); stdcall;
    function GetList: TList; stdcall;
    procedure Assign(AList: IlicgIntegerList); stdcall;
    function Add(Item: Integer): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Insert(Index: Integer; Value: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function IndexofValue(Item: Integer): Integer; stdcall;
    procedure DeleteValue(Item: Integer); stdcall;
    procedure Sort; stdcall;
    function Find(Value: Integer; var Index: Integer): Boolean; stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure Reindex; stdcall;
    property Items[Index: Integer]: Integer read Get write Put; default;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property List: TList read getList write setList;
  end;

  IlicgDoubleList = interface
    ['{41141334-4B4A-4CAE-8FE4-258B522C0E91}']
    function Get(Index: Integer): Double; stdcall;
    procedure Grow; stdcall;
    procedure Put(Index: Integer; const Item: Double); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    function Add(const Item: Double): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure Insert(Index: Integer; const Item: Double); stdcall;
    procedure Sort; stdcall;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: Double read Get write Put; default;
  end;

  TBezierTangentType = (btTangent, btBroken);

  TBezierPoint = packed record
    Index: Integer; // the index of the start of the segment that is a bezier segment
    BezierControlPts: array[0..1] of TlicgCoor; // the point that is the first and second control point of the bezier segment
    TangentType: TBezierTangentType; // how the first point is accordingly to second point of previous segment
  end;

  IlicgBezierPointList = interface
    ['{96D29C59-A064-4173-9AB1-DBE93E856608}']
    procedure Grow; stdcall;
    function Get(Index: Integer): TBezierPoint; stdcall;
    procedure Put(Index: Integer; const Item: TBezierPoint); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    function Add(const Item: TBezierPoint): Integer; stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Insert(Index: Integer; const Value: TBezierPoint); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function IndexOfIndex(AIndex: Integer): Integer; stdcall;
    procedure Sort; stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TBezierPoint read Get write Put; default;
  end;

  IStringsPassIntegerData = interface
    ['{F1809080-3144-40FA-884E-3695A4A00AF4}']
    procedure Add(const StringData: string; const IntData: integer); stdcall;
    procedure Clear; stdcall;
    function Count: integer; stdcall;
    function integerData(const index: integer): integer; stdcall;
    function stringData(const index: integer): string; stdcall;
    function GetStrings: TStrings; stdcall;
    property Strings: TStrings read GetStrings;
  end;

function GetStringsPassIntegerData(const AStrings: TStrings): IStringsPassIntegerData;

function CreateIntegerList: IlicgIntegerList;

function CreateDoubleList: IlicgDoubleList;

function CreateBezierList: IlicgBezierPointList;

implementation

uses
  Lider.CG.Com.Lists;

function GetStringsPassIntegerData(const AStrings: TStrings): IStringsPassIntegerData;
begin
  Result := TStringsPassIntegerData.Create(AStrings);
end;

function CreateIntegerList: IlicgIntegerList;
begin
  Result := TlicgIntegerList.Create;
end;

function CreateDoubleList: IlicgDoubleList;
begin
  Result := TlicgDoubleList.Create;
end;

function CreateBezierList: IlicgBezierPointList;
begin
  Result := TlicgBezierPointList.Create;
end;

end.


