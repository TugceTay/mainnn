unit Lider.CG.Com.DB;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Classes,
  SysUtils,
  Windows,
  Forms,
  DB,
  Variants,
  Dialogs,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.System,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Base,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  {-------------------------------------------------------------------------------}
  //                  TGXBaseDataset
  {-------------------------------------------------------------------------------}

  TlicgBaseDataset = class(TDataset)
  private
    FisOpen: Boolean;
    FStartCalculated: Integer;
    FBufferMap: TStringList;
    FRecordSize: Integer; // full buffer size
    FDataSize: Integer; // from bull buffer, only the data size
    //FInternalBookmarkSize: Integer;
    procedure FillBufferMap;
    procedure AllocateBLOBPointers(Buffer: TRecordBuffer);
    procedure FreeBlobPointers(Buffer: TRecordBuffer);
    function GetisOpen: Boolean;
  protected {My simplified methods to override}
    function DoOpen: Boolean; virtual; abstract;
    procedure DoClose; virtual; abstract;
    procedure DoDeleteRecord; virtual;
    procedure DoCreateFieldDefs; virtual; abstract;
    function GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; const Value: Variant); virtual; abstract;
    procedure GetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TField; Stream: TStream); virtual; abstract;
    //Called before and after getting a set of field values
    procedure DoBeforeGetFieldValue; virtual;
    procedure DoAfterGetFieldValue; virtual;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); virtual;
    procedure DoAfterSetFieldValue(Inserting: Boolean); virtual;
    //Handle buffer ID
    function AllocateRecordID: Pointer; virtual; abstract;
    procedure DisposeRecordID(Value: Pointer); virtual; abstract;
    procedure GotoRecordID(Value: Pointer); virtual; abstract;
    //BookMark functions
    function GetBookMarkSize: Integer; virtual; //******
    procedure DoGotoBookmark(Bookmark: Pointer); virtual; abstract; //********
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); virtual; abstract;
    //Navigation methods
    procedure DoFirst; virtual; abstract;
    procedure DoLast; virtual; abstract;
    function Navigate(Buffer: TRecBuf; GetMode: TGetMode; doCheck: Boolean): TGetResult; virtual; abstract;
    //Internal isOpen property
    function FilterRecord(Buffer: TRecBuf): Boolean; virtual;
    function DoBookmarkValid(Bookmark: TBookmark): boolean; virtual; abstract;
    function DoCompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; virtual; abstract;

    property isOpen: Boolean read GetisOpen;
  protected {TlicgBaseDataset Internal functions that can be overriden if needed}
    procedure AllocateBLOBPointer(Field: TField; var P: Pointer); virtual;
    procedure FreeBLOBPointer(Field: TField; var P: Pointer); virtual;
    procedure FreeRecordPointers(Buffer: TRecordBuffer); virtual;
    function GetDataSize: Integer; virtual;
    procedure BufferToRecord(Buffer: TRecBuf); virtual;
    procedure RecordToBuffer(Buffer: TRecBuf); virtual;
  protected
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalInsert; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalEdit; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecBuf); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetCanModify: Boolean; override;
    procedure ClearCalcFields(Buffer: TRecBuf); override;
    function GetActiveRecordBuffer: TRecBuf; virtual;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;

    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision, Decimals: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function BookmarkValid(Bookmark: TBookmark): boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    //function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
{$IFDEF LEVEL4}
    procedure SetBlockReadSize(Value: Integer); override;
{$ENDIF}
  published
    { inherited events }
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  {-------------------------------------------------------------------------------}
  //                  TlicgTable
  {-------------------------------------------------------------------------------}

    { for browse one layer }
  TlicgGISField = class(TCollectionItem)
  private
    FExpression: string;
    FFieldName: string; // used as the TField.FieldName when IsExpression=true
    FIsExpression: Boolean;
    SourceField: Integer; // runtime only
    FResolver: TlicgBaseMainExpr; // runtime only
    procedure SetExpression(const Value: string);
    function GetExpression: string;
    function GetFieldName: string;
    function GetIsExpression: Boolean;
    function GetResolver: TlicgBaseMainExpr;
    procedure SetFieldName(const Value: string);
    procedure SetIsExpression(const Value: Boolean);
    procedure SetResolver(const Value: TlicgBaseMainExpr); // used only when FIsExpresion=true
  protected
    function GetDisplayName: string; override;
    function GetCaption: string;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Resolver: TlicgBaseMainExpr read GetResolver write SetResolver;
  published
    property Expression: string read GetExpression write SetExpression;
    property FieldName: string read GetFieldName write SetFieldName;
    property IsExpression: Boolean read GetIsExpression write SetIsExpression;
  end;

  TlicgTable = class;

  TlicgGISFields = class(TOwnedCollection)
  private
    FOwner: TlicgTable;
    function GetItem(Index: Integer): TlicgGISField;
    procedure SetItem(Index: Integer; Value: TlicgGISField);
  public
    constructor Create(AOwner: TlicgTable);
    function Add: TlicgGISField;
    procedure PopulateFromLayer(const Layer: TlicgBaseLayer);
{$IFDEF LEVEL5}
    procedure Move(FromIndex, ToIndex: Integer);
{$ENDIF}

    property Items[Index: Integer]: TlicgGISField read GetItem write SetItem; default;
  end;



  TlicgTable = class(TlicgBaseDataset)
  private
    FGIS: TlicgBaseGIS;
    FMapFields: TlicgGISFields;
    FLayer: TlicgBaseLayer;
    FRecords: IlicgIntegerList;
    FFindExpr: TlicgBaseMainExpr;
    FFilterExpr: TlicgBaseMainExpr;
    FUseDeleted: Boolean;
    FRecordCount: Integer;
    FMaxRecords: Integer;
    FCurRec: Integer;
    FReadOnly: Boolean;
    FFindRow: Integer;
    FLayerName: string;
    { for filtering graphically the database }
    FGraphicFilterList: IlicgIntegerList;
    FGraphicFiltered: Boolean;
    procedure AddFieldDesc(FieldNo: Word);
    procedure SetBaseLayer;
    procedure CreateFilterExpr(const Text: string);
    procedure RebuildRecordList;
    function GetSourceRecNo: Integer;
    procedure SetGIS(const Value: TlicgBaseGIS);
    procedure SetReadOnly(Value: Boolean);
    procedure SetFilterData(const Text: string);
    function DoFindFirst: Boolean;
    procedure SetLayerName(const Value: string);
    function GetGIS: TlicgBaseGIS;
    function GetLayer: TlicgBaseLayer;
    function GetLayerName: string;
    function GetMapFields: TlicgGISFields;
    function GetMaxRecords: Longint;
    function GetReadOnly: Boolean;
    function GetUseDeleted: Boolean;
    procedure SetLayer(const Value: TlicgBaseLayer);
    procedure SetMaxRecords(const Value: Longint);
    procedure SetUseDeleted(const Value: Boolean);



  protected
    function FilterRecord(Buffer: TRecBuf): Boolean; override;
    procedure InternalRefresh; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; const Value: Variant); override;
    //Handle buffer ID
    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    //BookMark functions
    function GetBookMarkSize: Integer; override;
    procedure DoGotoBookmark(Bookmark: Pointer); override;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); override;
    function DoBookmarkValid(Bookmark: TBookmark): boolean; override;
    function DoCompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    procedure DoBeforeGetFieldValue; override;
    //Navigation methods
    procedure DoFirst; override;
    procedure DoLast; override;
    function Navigate(Buffer: TRecBuf; GetMode: TGetMode; doCheck: Boolean): TGetResult; override;
    procedure AllocateBLOBPointer(Field: TField; var P: Pointer); override;
    procedure FreeBLOBPointer(Field: TField; var P: Pointer); override;
    //Called before and after getting a set of field values
    //procedure DoBeforeGetFieldValue; override;
    //procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    //procedure DoAfterSetFieldValue(Inserting: Boolean); override;
    // other
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
    procedure Notification(AComponent: TComponent; Operation: toperation); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetFiltered(Value: Boolean); override;
    function GetCanModify: Boolean; override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    procedure GotoRecordID(Value: Pointer); override;
    function GetRecords : IlicgIntegerList;
    Procedure SetRecords(Value:IlicgIntegerList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SearchRecNo(Value: Pointer):boolean;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string;
      const KeyValues: Variant; const ResultFields: string): Variant; override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function Find(const Expression: string; Direction: TlicgDirection; Origin: TlicgOrigin): Boolean;
    function FindNext: Boolean;
    procedure OrderBy(const Expression: string; Descending: Boolean = False);
    procedure UnSort;
    function IsDeleted: boolean;
    procedure Recall;
    procedure SelectionFilter(Selection: TlicgBaseSelection; ClearBefore: Boolean);
    procedure ScopeFilter(const Scope: string; ClearBefore: Boolean);
    procedure PolygonFilter(Polygon: IlicgEntity; operator: TlicgGraphicOperator;
      const QueryExpression: string; ClearBefore: Boolean);
    procedure RectangleFilter(const AxMin, AyMin, AxMax, AyMax: Double;
      Operator: TlicgGraphicOperator; const QueryExpression: string;
      ClearBefore: Boolean);
    procedure BufferFilter(Buffer: IlicgEntity; Operator: TlicgGraphicOperator;
      const QueryExpression: string; CurvePoints: Integer;
      const Distance: Double; ClearBefore: Boolean);
    procedure PolylineIntersects(Polyline: IlicgEntity;
      const QueryExpression: string; ClearBefore: Boolean);
    procedure FilterFromLayer(SourceLayer: TlicgBaseLayer;
      const QueryExpression: string; Operator: TlicgGraphicOperator; ClearBefore: Boolean);
    procedure DoSelect(Selection: TlicgBaseSelection);
    Procedure AddRecnoToFRecords(Const Recno:Integer);

    property SourceRecNo: Integer read GetSourceRecNo;
    property Layer: TlicgBaseLayer read GetLayer write SetLayer;
  published
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property MaxRecords: Longint read GetMaxRecords write SetMaxRecords;
    property GIS: TlicgBaseGIS read GetGIS write SetGIS;
    property UseDeleted: Boolean read GetUseDeleted write SetUseDeleted default True;
    property MapFields: TlicgGISFields read GetMapFields;
    property LayerName: string read GetLayerName write SetLayerName;

    { inherited properties }
    property Filter;
    property Filtered;

  end;

  { TDesignTable - a dataset used for editing fields when restructuring }
  TlicgDesignTable = class(TlicgBaseDataset)
  private
    FNameColumn: TStringList;
    FAliasColumn: TStringList;
    FTypeColumn: TStringList;
    FSizeColumn: IlicgIntegerList;
    FDecColumn: IlicgIntegerList;
    FOrigFieldNoColumn: IlicgIntegerList;
    FRecordCount: Integer;
    FCurRec: Integer;
    FModified: Boolean;
  protected
    procedure InternalRefresh; override;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoDeleteRecord; override;
    procedure DoCreateFieldDefs; override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; const Value: Variant); override;
    //Handle buffer ID
    function AllocateRecordID: Pointer; override;
    procedure DisposeRecordID(Value: Pointer); override;
    procedure GotoRecordID(Value: Pointer); override;
    //BookMark functions
    function GetBookMarkSize: Integer; override;
    procedure DoGotoBookmark(Bookmark: Pointer); override;
    procedure AllocateBookMark(RecordID: Pointer; Bookmark: Pointer); override;
    function DoBookmarkValid(Bookmark: TBookmark): boolean; override;
    function DoCompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    //Navigation methods
    procedure DoFirst; override;
    procedure DoLast; override;
    function Navigate(Buffer: TRecBuf; GetMode: TGetMode; doCheck: Boolean): TGetResult; override;
    //Called before and after getting a set of field values
    procedure DoBeforeGetFieldValue; override;
    procedure DoAfterGetFieldValue; override;
    procedure DoBeforeSetFieldValue(Inserting: Boolean); override;
    procedure DoAfterSetFieldValue(Inserting: Boolean); override;
    procedure GetBlobField(Field: TField; Stream: TStream); override;
    procedure SetBlobField(Field: TField; Stream: TStream); override;
    // other
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
    function GetCanModify: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsSequenced: Boolean; override;
  end;

implementation

uses
  Lider.CG.Com.Consts,
  Lider.CG.Com.LicadInt;

type

  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    RecordID: Pointer;
    Bookmark: Pointer;
    BookMarkFlag: TBookmarkFlag;
  end;

  {-------------------------------------------------------------------------------}
  //                  TlicgBaseDataset
  {-------------------------------------------------------------------------------}

constructor TlicgBaseDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBufferMap := TStringList.Create;
end;

destructor TlicgBaseDataset.Destroy;
begin
  if Active then
    Close;
  FBufferMap.Free;
  inherited Destroy;
end;

function TlicgBaseDataset.GetisOpen: Boolean;
begin
  Result:=FIsOpen;
end;

procedure TlicgBaseDataset.FillBufferMap;
var
  Index, Offset: Integer;
begin
  FBufferMap.Clear;
  Offset := 0;
  for Index := 0 to FieldCount - 1 do
  begin
    FBufferMap.AddObject(Fields[Index].FieldName, Pointer(Offset));
    case FieldbyName(FBufferMap[Index]).DataType of
      ftString: inc(Offset, FieldbyName(FBufferMap[Index]).Size + 1);
      ftInteger, ftSmallInt, ftDate, ftTime: inc(Offset, sizeof(Integer));
      ftDateTime, ftFloat, ftBCD, ftCurrency: inc(Offset, sizeof(Double));
      ftBoolean: inc(Offset, sizeof(WordBool));
      ftGraphic, ftMemo: inc(Offset, sizeof(Pointer));
    end;
  end;
end;

procedure TlicgBaseDataset.InternalOpen;
begin
  if DoOpen then
  begin
    BookmarkSize := GetBookMarkSize; //Bookmarks not supported
    InternalInitFieldDefs;
    if DefaultFields then
      CreateFields;
    BindFields(True);
    FisOpen := True;
    FillBufferMap;
    FRecordSize := GetRecordSize;
  end;
end;

function TlicgBaseDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecordSize);
  FillChar(Result^, FRecordSize, 0);
  AllocateBlobPointers(Result);
end;

procedure TlicgBaseDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, FRecordSize);
end;

procedure TlicgBaseDataset.FreeRecordPointers(Buffer: TRecordBuffer);
begin
  FreeBlobPointers(Buffer);
  DisposeRecordID(PRecordInfo(Buffer + FDataSize).RecordID);
  if PRecordInfo(Buffer + FDataSize)^.BookMark <> nil then
  begin
    FreeMem(PRecordInfo(Buffer + FDataSize)^.BookMark);
    PRecordInfo(Buffer + FDataSize)^.BookMark := nil;
  end;
end;

procedure TlicgBaseDataset.AllocateBLOBPointers(Buffer: TRecordBuffer);
var
  Index: Integer;
  Offset: Integer;
  //Stream: TMemoryStream;
  P: Pointer;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftMemo, ftGraphic] then
    begin
      Offset := Integer(FBufferMap.Objects[Index]);
      //Stream:=TMemoryStream.Create;
      //Move(Pointer(Stream),(Buffer+Offset)^,sizeof(Pointer));
      // get the pointer to the blob field
      AllocateBlobPointer(Fields[Index], P);
      Move(P, (Buffer + Offset)^, sizeof(Pointer));
    end;
end;

procedure TlicgBaseDataset.FreeBlobPointers(Buffer: TRecordBuffer);
var
  Index: Integer;
  Offset: Integer;
  P: Pointer;
begin
  for Index := 0 to FieldCount - 1 do
    if Fields[Index].DataType in [ftMemo, ftGraphic] then
    begin
      Offset := Integer(FBufferMap.Objects[Index]);
      Move((Buffer + Offset)^, Pointer(P), sizeof(Pointer));
      FreeBlobPointer(Fields[Index], P);
      P := nil;
      Move(P, (Buffer + Offset)^, sizeof(Pointer));
    end;
end;

procedure TlicgBaseDataset.AllocateBLOBPointer(Field: TField; var P: Pointer);
begin
  P := nil;
end;

procedure TlicgBaseDataset.FreeBLOBPointer(Field: TField; var P: Pointer);
begin
  P := nil;
end;

procedure TlicgBaseDataset.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TlicgBaseDataset.ClearCalcFields(Buffer: TRecBuf);
begin
  FillChar(TRecordBuffer(Buffer)^, CalcFieldsSize, 0);
end;

function TlicgBaseDataset.GetActiveRecordBuffer: TRecBuf;
begin
  case State of
    dsBrowse: if isEmpty then
        Result := 0
      else
        Result := ActiveBuffer;
    dsCalcFields: Result := CalcBuffer;
    dsFilter: Result := 0;
    dsEdit, dsInsert: Result := ActiveBuffer;
    dsNewValue, dsOldValue, dsCurValue: Result := ActiveBuffer;
{$IFDEF level5}
    dsBlockRead: Result := ActiveBuffer;
{$ENDIF}
  else
    Result := TRecBuf(nil);
  end;
end;

function TlicgBaseDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

function TlicgBaseDataset.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := Navigate(Buffer, GetMode, DoCheck);
  if (Result = grOk) then
  begin
    RecordToBuffer(Buffer);
    ClearCalcFields(Buffer);
    GetCalcFields(Buffer);
  end
  else if (Result = grError) and DoCheck then
    DatabaseError('No Records');
end;

function TlicgBaseDataset.GetRecordSize: Word;
begin
  FDataSize := GetDataSize;
  Result := FDataSize + sizeof(TRecordInfo) + CalcFieldsSize;
  FStartCalculated := FDataSize + sizeof(TRecordInfo);
end;

function TlicgBaseDataset.GetDataSize: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to FieldCount - 1 do
    case Fields[Index].DataType of
      ftString: Result := Result + Fields[Index].Size + 1; //Leave space for terminating null
      ftInteger, ftSmallInt, ftDate, ftTime: Result := Result + sizeof(Integer);
      ftFloat, ftCurrency, ftBCD, ftDateTime: Result := Result + sizeof(Double);
      ftBoolean: Result := Result + sizeof(WordBool);
      ftMemo, ftGraphic: Result := Result + sizeof(Pointer);
    end;
end;

procedure TlicgBaseDataset.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  DoClose;
  FisOpen := False;
end;

procedure TlicgBaseDataset.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TlicgBaseDataset.InternalEdit;
begin
  if GetActiveRecordBuffer <> TRecBuf(nil) then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TlicgBaseDataset.InternalFirst;
begin
  DoFirst;
end;

procedure TlicgBaseDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s since that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}

procedure TlicgBaseDataset.InternalInitRecord(Buffer: TRecBuf);
begin
  FreeRecordPointers(TRecordBuffer(Buffer));
  FillChar(TRecordBuffer(Buffer)^, FRecordSize, 0);
  AllocateBlobPointers(TRecordBuffer(Buffer));
end;

procedure TlicgBaseDataset.InternalInsert;
begin

end;

procedure TlicgBaseDataset.InternalLast;
begin
  DoLast;
end;

procedure TlicgBaseDataset.InternalPost;
begin
  if FisOpen then
  begin
    DoBeforeSetFieldValue(State = dsInsert);
    BufferToRecord(GetActiveRecordBuffer);
    DoAfterSetFieldValue(State = dsInsert);
  end;
end;

procedure TlicgBaseDataset.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
begin
  if Append then
    InternalLast;
  DoBeforeSetFieldValue(True);
  BufferToRecord(Buffer);
  DoAfterSetFieldValue(True);
end;

procedure TlicgBaseDataset.InternalSetToRecord(Buffer: TRecBuf);
begin
  GotoRecordID(PRecordInfo(Buffer + FDataSize).RecordID);
end;

function TlicgBaseDataset.IsCursorOpen: Boolean;
begin
  Result := FisOpen;
end;

procedure TlicgBaseDataset.BufferToRecord(Buffer: TRecBuf);
var
  TempStr: string;
  TempInt: Integer;
  TempDouble: Double;
  TempBool: WordBool;
  Offset: Integer;
  Index: Integer;
  //Stream: TStream;
begin
  for Index := 0 to FieldCount - 1 do
  begin
    Offset := Integer(FBufferMap.Objects[Fields[Index].FieldNo - 1]);
    case Fields[Index].DataType of
      ftString:
        begin
          TempStr := PChar(Buffer + Offset);
          SetFieldValue(Fields[Index], TempStr);
        end;
      ftInteger, ftSmallInt, ftDate, ftTime:
        begin
          Move(TRecordBuffer(Buffer + Offset)^, TempInt, sizeof(Integer));
          SetFieldValue(Fields[Index], TempInt);
        end;
      ftFloat, ftBCD, ftCurrency, ftDateTime:
        begin
          Move(TRecordBuffer(Buffer + Offset)^, TempDouble, sizeof(Double));
          SetFieldValue(Fields[Index], TempDouble);
        end;
      ftBoolean:
        begin
          Move(TRecordBuffer(Buffer + Offset)^, TempBool, sizeof(Boolean));
          SetFieldValue(Fields[Index], TempBool);
        end;
      ftGraphic, ftMemo:
        begin
          {Move((Buffer + Offset)^, Pointer(Stream), sizeof(Pointer));
          Stream.Position := 0;
          SetBlobField(Fields[Index], Stream);}
        end;
    end;
  end;
end;

procedure TlicgBaseDataset.RecordToBuffer(Buffer: TRecBuf);
var
  Value: Variant;
  TempStr: AnsiString;
  TempInt: Integer;
  TempDouble: Double;
  TempBool: WordBool;
  Offset: Integer;
  Index: Integer;
  //Stream: TStream;
begin
  with PRecordInfo(Buffer + FDataSize)^ do
  begin
    BookmarkFlag := bfCurrent;
    RecordID := AllocateRecordID;
    if BookmarkSize > 0 then
    begin
      if BookMark = nil then
        GetMem(BookMark, BookmarkSize);
      AllocateBookMark(RecordID, BookMark);
    end
    else
      BookMark := nil;
  end;
  DoBeforeGetFieldValue;
  for Index := 0 to FieldCount - 1 do
  begin
    if not (Fields[Index].DataType in [ftMemo, ftGraphic]) then
      Value := GetFieldValue(Fields[Index]);
    if Fields[Index].FieldNo < 0 then
      Continue;
    Offset := Integer(FBufferMap.Objects[Fields[Index].FieldNo - 1]);
    case Fields[Index].DataType of
      ftString:
        begin
          if VarIsNull(Value) then
            TempStr := ''
          else
            TempStr := Value;
          if length(TempStr) > Fields[Index].Size then
            System.Delete(TempStr, Fields[Index].Size, length(TempStr) - Fields[Index].Size);
          StrLCopy(PAnsiChar(TRecordBuffer(Buffer + Offset)), PAnsiChar(TempStr), length(TempStr)); // þüpheli
        end;
      ftInteger, ftSmallInt, ftDate, ftTime:
        begin
          if VarIsNull(Value) then
            TempInt := 0
          else
            TempInt := Value;
          Move(TempInt, TRecordBuffer(Buffer + Offset)^, sizeof(TempInt));
        end;
      ftFloat, ftBCD, ftCurrency, ftDateTime:
        begin
          if VarIsNull(Value) then
            TempDouble := 0
          else
            TempDouble := Value;
          Move(TempDouble, TRecordBuffer(Buffer + Offset)^, sizeof(TempDouble));
        end;
      ftBoolean:
        begin
          if VarIsNull(Value) then
            TempBool := false
          else
            TempBool := Value;
          Move(TempBool, TRecordBuffer(Buffer + Offset)^, sizeof(TempBool));
        end;
      ftMemo, ftGraphic:
        begin
          {
          Move((Buffer + Offset)^, Pointer(Stream), sizeof(Pointer));
          if Stream <> Nil then
          begin
            Stream.Size := 0;
            Stream.Position := 0;
            GetBlobField(Fields[Index], Stream);
          end; }
        end;
    end;
  end;
  DoAfterGetFieldValue;
end;

procedure TlicgBaseDataset.DoDeleteRecord;
begin
  //Nothing in base class
end;

function TlicgBaseDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuffer: TRecBuf;
  Offset: Integer;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
  TempInt: Integer;
begin
  Result := false;
  if not FisOpen then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = TRecBuf(nil) then
    exit;
  if Buffer = nil then
  begin
    //Dataset checks if field is null by passing a nil buffer
    //Tell it is not null by passing back a result of True
    Result := True;
    exit;
  end;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    inc(RecBuffer, FStartCalculated + Field.Offset);
    if (RecBuffer = TRecBuf(nil)) or (Buffer = nil) then
      exit
    else
      CopyMemory(Buffer, TRecordBuffer(RecBuffer), Field.DataSize); // þüpheli
  end
  else
  begin
    Offset := Integer(FBufferMap.Objects[Field.FieldNo - 1]);
    case Field.DataType of
      ftInteger:
        Move(TRecordBuffer(RecBuffer + Offset)^, Integer(Buffer^), sizeof(Integer));
      ftTime, ftDate:
        begin
          Move(TRecordBuffer(RecBuffer + Offset)^, TempInt, sizeof(Integer));
          if TempInt = 0 then
            TempInt := 693594;
          Move(TempInt, Integer(Buffer^), sizeof(Integer));
        end;
      ftBoolean:
        begin
          Move(TRecordBuffer(RecBuffer + Offset)^, TempBool, sizeof(WordBool));
          Move(TempBool, WordBool(Buffer^), sizeof(WordBool));
        end;
      ftString:
        begin
          StrLCopy(Buffer, PChar(RecBuffer + Offset), StrLen(PChar(RecBuffer + Offset)));
          StrPCopy(Buffer, TrimRight(StrPas(PAnsiChar(Buffer)))); // þüpheli
        end;
      ftCurrency, ftFloat: Move(TRecordBuffer(RecBuffer + Offset)^, Double(Buffer^), sizeof(Double));
      ftDateTime:
        begin
          Move(TRecordBuffer(RecBuffer + Offset)^, TempDouble, sizeof(Double));
          TimeStamp := DateTimeToTimeStamp(TempDouble);
          Data.DateTime := TimeStampToMSecs(TimeStamp);
          Move(Data, Buffer^, sizeof(TDateTimeRec));
        end;
    end;
  end;
  Result := True;
end;

procedure TlicgBaseDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  Offset: Integer;
  RecBuffer: TRecBuf;
  TempDouble: Double;
  Data: TDateTimeRec;
  TimeStamp: TTimeStamp;
  TempBool: WordBool;
begin
  if not Active then
    exit;
  RecBuffer := GetActiveRecordBuffer;
  if RecBuffer = TRecBuf(nil) then
    exit;
  if Buffer = nil then
    exit;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    Inc(RecBuffer, FStartCalculated + Field.Offset);
    if (RecBuffer = TRecBuf(nil)) or (Buffer = nil) then

    else
      CopyMemory(TRecordBuffer(RecBuffer), Buffer, Field.DataSize) // þüpheli
    { orinal þüpheli
    Boolean(RecBuffer[0]) := (Buffer <> nil);
    if Boolean(RecBuffer[0]) then
      CopyMemory(@RecBuffer[1], Buffer, Field.DataSize)}
  end



  else
  begin
    Offset := Integer(FBufferMap.Objects[Field.FieldNo - 1]);
    case Field.DataType of
      ftInteger, ftDate, ftTime: Move(Integer(Buffer^), TRecordBuffer(RecBuffer + Offset)^, sizeof(Integer));
      ftBoolean:
        begin
          Move(WordBool(Buffer^), TempBool, sizeof(WordBool));
          Move(TempBool, TRecordBuffer(RecBuffer + Offset)^, sizeof(WordBool));
        end;
      ftString: StrLCopy(PChar(RecBuffer + Offset), Buffer, StrLen(PChar(Buffer)));
      ftDateTime:
        begin
          Data := TDateTimeRec(Buffer^);
          TimeStamp := MSecsToTimeStamp(Data.DateTime);
          TempDouble := TimeStampToDateTime(TimeStamp);
          Move(TempDouble, TRecordBuffer(RecBuffer + Offset)^, sizeof(TempDouble));
        end;
      ftFloat, ftCurrency: Move(Double(Buffer^), TRecordBuffer(RecBuffer + Offset)^, sizeof(Double));
    end;
  end;
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

function TlicgBaseDataset.GetBookMarkSize: Integer;
begin
  Result := 0;
end;

procedure TlicgBaseDataset.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if BookMarkSize > 0 then
    AllocateBookMark(PRecordInfo(Buffer + FDataSize).RecordID, Data);
end;

function TlicgBaseDataset.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
  Result := PRecordInfo(Buffer + FDataSize).BookMarkFlag;
end;

procedure TlicgBaseDataset.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if PRecordInfo(Buffer + FDataSize)^.BookMark = nil then
    GetMem(PRecordInfo(Buffer + FDataSize)^.BookMark, BookmarkSize);
  Move(PRecordInfo(Buffer + FDataSize).BookMark^, Data, BookmarkSize);
end;

procedure TlicgBaseDataset.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + FDataSize).BookMarkFlag := Value;
end;

procedure TlicgBaseDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  DoGotoBookMark(BookMark);
end;

function TlicgBaseDataset.BookmarkValid(Bookmark: TBookmark): boolean;
begin
  result := DoBookmarkValid(Bookmark);
end;

function TlicgBaseDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  result := DoCompareBookmarks(Bookmark1, Bookmark2);
end;

{function TlicgBaseDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:=TlicgBlobStream.Create(Field as TBlobField, Mode);
end; }

procedure TlicgBaseDataset.DoAfterGetFieldValue;
begin

end;

procedure TlicgBaseDataset.DoBeforeGetFieldValue;
begin

end;

procedure TlicgBaseDataset.DoAfterSetFieldValue(Inserting: Boolean);
begin

end;

procedure TlicgBaseDataset.DoBeforeSetFieldValue(Inserting: Boolean);
begin

end;

function TlicgBaseDataset.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TlicgBaseDataset.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;

function TlicgBaseDataset.FilterRecord(Buffer: TRecBuf): Boolean;
begin
  result := True;
end;

{$IFDEF LEVEL4}

procedure TlicgBaseDataset.SetBlockReadSize(Value: Integer);
{$IFNDEF LEVEL5}
var
  DoNext: Boolean;
{$ENDIF}
begin
  if Value <> BlockReadSize then
  begin
    if (Value > 0) or (Value < -1) then
    begin
      inherited;
      BlockReadNext;
    end
    else
    begin
{$IFNDEF LEVEL5}
      doNext := Value = -1;
{$ENDIF}
      Value := 0;
      inherited;

{$IFNDEF LEVEL5}
      if doNext then
        Next
      else
      begin
{$ENDIF}
        CursorPosChanged;
        Resync([]);
{$IFNDEF LEVEL5}
      end;
{$ENDIF}
    end;
  end;
end;
{$ENDIF}

//************************** TlicgBlobStream ***************************************
{constructor TlicgBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
Begin
  inherited Create;
  FField:=Field;
  FMode:=Mode;
  FDataSet:=FField.DataSet as TlicgBaseDataset;
  if Mode<>bmWrite then LoadBlobData;
End;

destructor TlicgBlobStream.Destroy;
Begin
  if FModified then SaveBlobData;
  inherited Destroy;
End;

function TlicgBlobStream.Read(var Buffer; Count: Longint): Longint;
Begin
  Result:=inherited Read(Buffer,Count);
  FOpened:=True;
End;

function TlicgBlobStream.Write(const Buffer; Count: Longint): Longint;
Begin
  Result:=inherited Write(Buffer,Count);
  FModified:=True;
End;

procedure TlicgBlobStream.LoadBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: PChar;
Begin
  Self.Size:=0;
  RecBuffer:=FDataset.GetActiveRecordBuffer;
  if RecBuffer<>nil then
    Begin
    Offset:=Integer(FDataset.FBufferMap.Objects[FField.FieldNo-1]);
    Move((RecBuffer+Offset)^,Pointer(Stream),sizeof(Pointer));
    Self.CopyFrom(Stream,0);
    End;
  Position:=0;
End;

procedure TlicgBlobStream.SaveBlobData;
var Stream: TMemoryStream;
Offset: Integer;
RecBuffer: Pchar;
Begin
  RecBuffer:=FDataset.GetActiveRecordBuffer;
  if RecBuffer<>nil then
    Begin
    Offset:=Integer(FDataset.FBufferMap.Objects[FField.FieldNo-1]);
    Move((RecBuffer+Offset)^,Pointer(Stream),sizeof(Pointer));
    Stream.Size:=0;
    Stream.CopyFrom(Self,0);
    Stream.Position:=0;
    End;
  FModified:=False;
End; }

{-------------------------------------------------------------------------------}
//                  TlicgTable
{-------------------------------------------------------------------------------}

{ TlicgGISField }

destructor TlicgGISField.Destroy;
begin
  if Assigned(FResolver) then
    FResolver.Free;

  inherited Destroy;
end;

procedure TlicgGISField.Assign(Source: TPersistent);
begin
  if Source is TlicgGISField then
  begin
    FExpression := TlicgGISField(Source).Expression;
    FFieldName := TlicgGISField(Source).FieldName;
    FIsExpression := TlicgGISField(Source).IsExpression;
    FResolver.Free;
    FResolver := nil;
  end
  else
    inherited Assign(Source);
end;

function TlicgGISField.GetCaption: string;
begin
  result := FFieldName;
end;

function TlicgGISField.GetDisplayName: string;
begin
  result := GetCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TlicgGISField.SetExpression(const Value: string);
begin
  FExpression := Value;
  FFieldName := Value;
end;

function TlicgGISField.GetExpression: string;
begin
  Result:=FExpression
end;

function TlicgGISField.GetFieldName: string;
begin
  Result:=FFieldName
end;

function TlicgGISField.GetIsExpression: Boolean;
begin
  Result:=FIsExpression
end;

function TlicgGISField.GetResolver: TlicgBaseMainExpr;
begin
  Result:=FResolver
end;

procedure TlicgGISField.SetFieldName(const Value: string);
begin
  FFieldName:= Value
end;

procedure TlicgGISField.SetIsExpression(const Value: Boolean);
begin
  FIsExpression:= Value
end;

procedure TlicgGISField.SetResolver(const Value: TlicgBaseMainExpr);
begin
  FResolver:= Value
end;

{ TlicgGISFields }

constructor TlicgGISFields.Create(AOwner: TlicgTable);
begin
  FOwner := AOwner;
  inherited Create(AOwner, TlicgGISField);
end;

function TlicgGISFields.Add: TlicgGISField;
begin
  Result := TlicgGISField(inherited Add);
end;

function TlicgGISFields.GetItem(Index: Integer): TlicgGISField;
begin
  Result := TlicgGISField(inherited GetItem(Index));
end;

procedure TlicgGISFields.SetItem(Index: Integer; Value: TlicgGISField);
begin
  inherited SetItem(Index, Value);
end;

{$IFDEF LEVEL5}

procedure TlicgGISFields.Move(FromIndex, ToIndex: Integer);
var
  Moved: TlicgGISField;
begin
  Moved := TlicgGISField.Create(Self);
  Moved.Assign(GetItem(FromIndex));
  Delete(FromIndex);
  Insert(ToIndex);
  GetItem(ToIndex).Assign(Moved);
  Moved.Free;
end;
{$ENDIF}

procedure TlicgGISFields.PopulateFromLayer(const Layer: TlicgBaseLayer);
var
  I: Integer;
begin
  if (Layer = nil) or (Layer.DBTable = nil) then
    exit;

  Clear;
  with Add do
  begin
    Expression := 'TYPE(ENT)';
    IsExpression := TRUE;
  end;
  for I := 1 to Layer.DBTable.FieldCount do
    if (AnsiCompareText(Layer.DBTable.Field(I), 'Geometry') <> 0) and
      (AnsiCompareText(Layer.DBTable.Field(I), 'Shape') <> 0) then
      with Add do
      begin
        SourceField := I;
        Expression := Layer.DBTable.Field(I);
        FieldName := Expression;
        IsExpression := False;
      end;
end;

type

  TMapBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TlicgTable;
    FIndex: Integer;
    procedure ReadBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
  end;

constructor TMapBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FIndex := FField.Index;
  FDataSet := FField.DataSet as TlicgTable;
  if Mode = bmRead then
    ReadBlobData;
  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      DatabaseErrorFmt('Field ''%s'' cannot be modified', [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then
      DatabaseError(SBlobErrNotEditing);
  end;
  if Mode = bmWrite then
    Clear
  else
    ReadBlobData;
end;

procedure TMapBlobStream.ReadBlobData;
var
  MapField: TlicgGISField;
  n: Integer;
begin
  if FDataSet.RecNo < 1 then
    Exit;
  MapField := TlicgTable(FDataSet).FMapFields[FIndex];
  n := FDataSet.SourceRecNo;
  with TlicgTable(FDataSet).Layer do
  begin
    Recno := n;
    if DBTable <> nil then
      DBTable.Recno := n;
    if DBTable <> nil then
      DBTable.MemoLoadN(MapField.SourceField, Self);
    //(MapField.SourceField as TBlobField).SaveToStream(Self);
  end;
  Self.Position := 0;
end;

{ TlicgTable }

constructor TlicgTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMapFields := TlicgGISFields.Create(Self);

  FRecords := CreateIntegerList;
  FGraphicFilterList := CreateIntegerList;
  FUseDeleted := True;
end;

destructor TlicgTable.Destroy;
begin
  inherited Destroy;
  FMapFields.Free;
  FRecords := nil;

  if Assigned(FFilterExpr) then
    FFilterExpr.Free;
  if Assigned(FFindExpr) then
    FFindExpr.Free;
  FGraphicFilterList := nil;
end;

function TlicgTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TMapBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TlicgTable.SetBaseLayer;
begin
  if FGIS = nil then
    Exit;
  FLayer := nil;
  if Length(FLayerName) > 0 then
    FLayer := FGIS.Layers.LayerByName(FLayerName);
end;

function TlicgTable.DoOpen: Boolean;
var
  I: Integer;
begin
  if csDesigning in ComponentState then
    DatabaseError(SCannotOpenDesignMode);

  if FLayer = nil then
    SetBaseLayer;

  if (FLayer = nil) or not (Flayer.Active) then
    DatabaseError(SLayerNotAssigned);

  if FMapFields.Count = 0 then
    FMapFields.PopulateFromLayer(FLayer)
  else
    { check if FMapFields is defined correctly }
    for I := 0 to FMapFields.Count - 1 do
      with FMapFields[I] do
        if not IsExpression then
        begin
          if Layer.DBTable = nil then
            DatabaseError(SLayerEmpty);
          if Length(FieldName) = 0 then
            DatabaseError(SExpressionEmpty);
          if FLayer.DBTable.FieldNo(FieldName) = 0 then
            DatabaseError(Format(SWrongFieldnameCol, [I]));
        end
        else
        begin
          if Length(FieldName) = 0 then
            DatabaseError(SExpressionEmpty);
        end;

  { create the list of records}
  if Assigned(FFilterExpr) then begin
    FFilterExpr.Free;
    FFilterExpr := nil;
  end;

  RebuildRecordList;

  FCurRec := -1;

  Result := True;

end;

procedure TlicgTable.RebuildRecordList;
var
  I, N, Idx1, Idx2, K: Integer;
  DoFilter, Accepted: Boolean;
begin
  //C: 11.01.2006 GIS sorgusundan dönen sonuçlar listeye GISBeforePaintEntity de  eklendiginden.
  //asagidaki kosul eklendi.
  if FRecords.Count>0 then
  begin
    FRecordCount := FRecords.Count;
    Exit;
  End;
  DoFilter := false;
  if Filtered and (Length(Filter) > 0) then
  begin
    CreateFilterExpr(Filter);
    DoFilter := true;
  end;
  N := FLayer.RecordCount;
  if (FGraphicFilterList.Count > 0) or FGraphicFiltered then
  begin
    Idx1 := 0;
    Idx2 := FGraphicFilterList.Count - 1;
    FGraphicFiltered := true;
  end
  else
  begin
    Idx1 := 1;
    Idx2 := N;
    FGraphicFiltered := false;
  end;

  FRecords.Clear;
  FRecords.Capacity := N;
  for I := Idx1 to Idx2 do
  begin
    if FGraphicFiltered then
      K := FGraphicFilterList[I]
    else
      K := I;
    if not FUseDeleted then
    begin
      FLayer.RecNo := K;
      if FLayer.RecIsDeleted then
        Continue;
    end;
    Accepted := true;
    if DoFilter then
    begin
      FLayer.Recno := K;
      FLayer.Synchronize;
      Accepted := True;
      if FFilterExpr.Expression <> nil then
        Accepted := FFilterExpr.Expression.AsBoolean;
    end;
    if Accepted then
      FRecords.Add(K);
  end;
  if DoFilter then
    Filtered := False;
  FGraphicFilterList.Clear;
  FRecordCount := FRecords.Count;

end;

procedure TlicgTable.DoClose;
begin
  FRecords.Clear;
  if Assigned(FFilterExpr) then begin
    FFilterExpr.Free;
    FFilterExpr := nil;
  end;
  if Assigned(FFindExpr) then begin
    FFindExpr.Free;
    FFindExpr := nil;
  end;
  FGraphicFilterList.Clear;
  FGraphicFiltered := False;
end;

procedure TlicgTable.AllocateBookMark(RecordID, Bookmark: Pointer);
begin
  PInteger(Bookmark)^ := Integer(RecordID);
end;

function TlicgTable.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurRec);
end;

procedure TlicgTable.DisposeRecordID(Value: Pointer);
begin
  // Do nothing, no need to dispose since pointer is just an integer
end;

procedure TlicgTable.GotoRecordID(Value: Pointer);
var
  n: Integer;
begin
  FCurRec := Integer(Value);
  n := FRecords[FCurRec];
  FLayer.Recno := n;
  FLayer.Synchronize;
end;

procedure TlicgTable.AddFieldDesc(FieldNo: Word);
var
  iFldType: TFieldType;
  Size: Word;
  Name: string;
  typ: AnsiChar;
  dec : integer;
begin
  if FieldNo <= 0 then
    Exit;
  Name := FLayer.DBTable.Field(FieldNo);
  Size := 0;
  Typ := FLayer.DBTable.FieldType(FieldNo);
  dec := FLayer.DBTable.FieldDec(FieldNo);
  case Typ of
    'C':
      begin
        iFldType := ftString; { Char string }
        Size := FLayer.DBTable.FieldLen(FieldNo);
      end;
    'N':
      begin
         if dec = 0 then
           iFldType := ftInteger
         else
           iFldType := ftFloat;
      end;


    'F':
      begin
          iFldType := ftFloat; { Number }
      end;

    'M':
      begin
        iFldType := ftMemo;
      end;
    'G',
      'B':
      begin
        iFldType := ftBlob;
      end;
    'L':
      begin
        iFldType := ftBoolean; { Logical }
      end;
    'D':
      begin
        iFldType := ftDate; { Date }
      end;
    'I':
      begin
        iFldType := ftInteger; {VFP integer}
      end;
    'T':
      begin
        iFldType := ftDateTime; {VFP datetime}
      end;
  else
    iFldType := ftUnknown;
  end;

  if iFldType <> ftUnknown then
    FieldDefs.Add(Name, iFldType, Size, false);
end;

procedure TlicgTable.DoCreateFieldDefs;
var
  I, P: Integer;
  DataType: TFieldType;
  ASize: Word;

  function UniqueFieldName(const Value: string): string;
  var
    found: Boolean;
    NumTry, J: Integer;
  begin
    result := Value;
    Numtry := 0;
    repeat
      Found := False;
      for J := 0 to FieldDefs.Count - 1 do
        if AnsiCompareText(FieldDefs[J].Name, result) = 0 then
        begin
          Found := True;
          Break;
        end;
      if Found then
      begin
        Inc(Numtry);
        result := Value + '_' + IntToStr(Numtry);
      end;
    until not Found;
  end;

begin
  FieldDefs.Clear;
  for I := 0 to FMapFields.Count - 1 do
    with FMapFields[I] do
    begin
      if IsExpression then
      begin
        FMapFields[I].FResolver.Free;
        FMapFields[I].FResolver := nil;
        FMapFields[I].FResolver := Licad.CreateMainExpr(FGIS, FLayer);
        FMapFields[I].FResolver.ParseExpression(Expression);
        ASize := 0;
        case FMapFields[I].FResolver.Expression.ExprType of
          ttString:
            begin
              DataType := ftString;
              ASize := FMapFields[I].FResolver.Expression.MaxLen;
              if ASize = 0 then
                ASize := 10;
            end;
          ttFloat:
            DataType := ftFloat;
          ttInteger:
            DataType := ftInteger;
          ttBoolean:
            DataType := ftBoolean;
        else
          DataType := ftString;
        end;
        P := AnsiPos('.', FieldName);
        if P > 0 then
          FieldName := Copy(FieldName, P + 1, Length(FieldName));
        FieldName := UniqueFieldName(FieldName);
        FieldDefs.Add(FieldName, DataType, ASize, false);
        {FldDef:= FieldDefs.AddFieldDef;
        FldDef.Name:= FieldName;
        FldDef.DataType:= DataType;
        FldDef.Size:= ASize;
        FldDef.Required:= False;}
      end
      else if FLayer.DBTable <> nil then
      begin
        AddFieldDesc(FLayer.DBTable.FieldNo(FieldName));
        {FieldName:= FieldName;
        SourceField:= FLayer.DBTable.FieldNo(FieldName);
        FldDef:= FieldDefs.AddFieldDef;
        FldDef.Name:= FLayer.DBTable.Field(SourceField);
        FldDef.DataType:= SourceField.DataType;
        FldDef.Size:= SourceField.Size;
        FldDef.Required:= SourceField.Required;
        if SourceField.DataType = ftBCD then
        begin
          FldDef.Size:= TBCDField(SourceField).Size;
          FldDef.Precision:= TBCDField(SourceField).Precision;
        end; }
      end;
    end;
end;

procedure TlicgTable.DoDeleteRecord;
begin
  UpdateCursorPos;
  FLayer.DeleteEntity(SourceRecNo);
end;

procedure TlicgTable.DoFirst;
begin
  FCurRec := -1;
end;

procedure TlicgTable.DoGotoBookmark(Bookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(Bookmark)^));
end;

procedure TlicgTable.DoLast;
begin
  FCurRec := FRecordCount;
end;

function TlicgTable.GetBookMarkSize: Integer;
begin
  Result := sizeof(Integer);
end;

function TlicgTable.DoBookmarkValid(Bookmark: TBookmark): boolean;
begin
  result := (PInteger(Bookmark)^ > 0) and (PInteger(Bookmark)^ <= FRecordCount);
end;

function TlicgTable.DoCompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  b1, b2: integer;
begin
  b1 := PInteger(Bookmark1)^;
  b2 := PInteger(Bookmark2)^;
  if b1 = b2 then
    Result := 0
  else if b1 < b2 then
    Result := -1
  else
    Result := 1;
end;

function TlicgTable.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  result := false;
end;

function TlicgTable.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  result := '';
end;

function TlicgTable.GetFieldValue(Field: TField): Variant;
var
  MapField: TlicgGISField;
  s: Ansistring;
  Ft: AnsiChar;
begin
  if FLayer.RecIsDeleted then
  begin
    Result := Null;
    Exit;
  end;
  if Field.FieldNo < 0 then
  begin
    Result := Field.Value;
    exit;
  end;
  MapField := FMapFields[Field.FieldNo - 1];
  if MapField.IsExpression then
  begin
    with MapField.Resolver.Expression do
      case ExprType of
        ttString: result := AsString;
        ttFloat: result := AsFloat;
        ttInteger:
          try
            result := AsInteger;
          except
            result := 0;
          end;
        ttBoolean: result := AsBoolean;
      end;
  end
  else if FLayer.DBTable <> nil then
  begin
    // must be as variant
    Ft := FLayer.DBTable.FieldType(MapField.SourceField);
    case Ft of
      'C':
        begin
          S := FLayer.DBTable.StringGetN(MapField.SourceField);
          if Length(Trim(s)) = 0 then
            Result := Null
          else
            Result := s;
        end;
      'D', 'T':
        Result := FLayer.DBTable.DateGetN(MapField.SourceField);
      'L':
        Result := FLayer.DBTable.LogicGetN(MapField.SourceField);
      'N', 'F':
        Result := FLayer.DBTable.FloatGetN(MapField.SourceField);
      'I':
        Result := FLayer.DBTable.IntegerGetN(MapField.SourceField);
    end;
  end;
end;

procedure TlicgTable.SetFieldValue(Field: TField; const Value: Variant);
var
  MapField: TlicgGISField;
begin
  MapField := FMapFields[Field.FieldNo - 1];
  if MapField.IsExpression then
    Exit;
  if not (FLayer.DBTable.FieldType(MapField.SourceField) in ['M', 'B', 'G']) then
  begin
    FLayer.DBTable.Edit;
    FLayer.DBTable.FieldPutN(MapField.SourceField, Value);
    FLayer.DBTable.Post;
  end;
end;

function TlicgTable.Navigate(Buffer: TRecBuf; GetMode: TGetMode; doCheck: Boolean): TGetResult;
var
  Acceptable: Boolean;
begin
  if FRecordCount < 1 then
    Result := grEOF
  else
  begin
    Result := grOK;
    repeat
      case GetMode of
        gmNext:
          if FCurRec >= FRecordCount - 1 then
            Result := grEOF
          else
            Inc(FCurRec);
        gmPrior:
          if FCurRec <= 0 then
          begin
            Result := grBOF;
            FCurRec := -1;
          end
          else
            Dec(FCurRec);
        gmCurrent:
          if (FCurRec < 0) or (FCurRec >= FRecordCount) then
            Result := grError;
      end;
      Acceptable := FilterRecord(Buffer);
      if (GetMode = gmCurrent) and not Acceptable then
        Result := grError;
      if (Result = grError) and DoCheck then
        DatabaseError(SGetRecordInvalid);
    until (Result <> grOk) or Acceptable;
  end;
end;

function TlicgTable.Find(const Expression: string; Direction: TlicgDirection; Origin: TlicgOrigin): Boolean;
begin
  result := false;
  if (FFindExpr <> nil) then begin
    FFindExpr.Free;
    FFindExpr := nil;
  end;
  if FRecordCount = 1 then
    Exit;
  FFindExpr := Licad.CreateMainExpr(FGIS, FLayer);
  try
    FFindExpr.ParseExpression(Expression);
    if FFindExpr.Expression.ExprType <> ttBoolean then
    begin
      DatabaseErrorFmt('Expression [''%s''] is not of Boolean type', [Expression]);
      FFindExpr.Free;
      FFindExpr := nil;
      Exit;
    end;
    if Origin = orEntire then
      result := DoFindFirst
    else begin
    end;
  except
    FFindExpr.Free;
    FFindExpr := nil;
  end;
end;

function TlicgTable.DoFindFirst: Boolean;
var
  n: Integer;
begin
  result := false;
  if FFindExpr = nil then
    Exit;
  UpdateCursorPos;
  FFindRow := 1;
  while FFindRow <= FRecordCount do
  begin
    n := FRecords[FFindRow - 1];
    FLayer.Recno := n;
    if not Layer.RecIsDeleted then
      with FGIS do
      begin
        FLayer.Synchronize;
        if FFindExpr.Expression.AsBoolean then
        begin
          Self.Recno := FFindRow;
          result := true;
          break;
        end;
      end;
    Inc(FFindRow);
  end;
  //if not result then
  //  DatabaseError(SRecordNotFound);
end;

function TlicgTable.FindNext: Boolean;
var
  n: Longint;
begin
  result := False;
  if FFindExpr = nil then
    Exit;
  UpdateCursorPos;
  Inc(FFindRow);
  while FFindRow <= FRecordCount do
  begin
    n := FRecords[FFindRow - 1];
    FLayer.Recno := n;
    if not FLayer.RecIsDeleted then
      with FGIS do
      begin
        FLayer.Synchronize;
        if FFindExpr.Expression.AsBoolean then
        begin
          Self.Recno := FFindRow;
          result := true;
          Break;
        end;
      end;
    Inc(FFindRow);
  end;
  //if not bfound then
  //  DatabaseError(SRecordNotFound);
end;

function TlicgTable.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (FRecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
end;

function TlicgTable.GetSourceRecNo: Integer;
begin
  UpdateCursorPos;
  Result := 0;
  if (FCurRec < 0) or (FCurRec > FRecordCount - 1) then
    Exit;
  result := FRecords[FCurRec];
end;

function TlicgTable.IsDeleted: boolean;
begin
  UpdateCursorPos;
  result := FLayer.RecIsDeleted;
end;

procedure TlicgTable.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value > 0) and (Value <= FRecordCount) then
  begin
    FCurRec := Value - 1;
    FLayer.Recno := FRecords[FCurRec];
    FLayer.Synchronize;
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TlicgTable.SetGIS(const Value: TlicgBaseGIS);
begin
{$IFDEF LEVEL5}
  if Assigned(FGis) then
//    FGis.RemoveFreeNotification(Self);
{$ENDIF}
  if Value <> nil then
  begin
//    Value.FreeNotification(Self);
  end;
  FGIS := Value;
end;

procedure TlicgTable.DoBeforeSetFieldValue(Inserting: Boolean);
begin
  if Inserting then
    DatabaseError(SInsertNotAllowed);
end;

function TlicgTable.GetCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TlicgTable.Notification(AComponent: TComponent;
  Operation: toperation);
begin
  inherited Notification(AComponent, Operation);
//  if (Operation = opRemove) and (AComponent = FGIS) then
//    FGIS := nil;
end;

procedure TlicgTable.SetFiltered(Value: Boolean);
begin
  if IsOpen then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      if Value then
        SetFilterData(Filter);
    end;
    First;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TlicgTable.SetFilterText(const Value: string);
begin
  SetFilterData(Value);
end;

function TlicgTable.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

procedure TlicgTable.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    if Active then
      DatabaseError(SWrongReadOnly);
    FReadOnly := Value;
  end;
end;

procedure TlicgTable.CreateFilterExpr(const Text: string);
begin
  if Assigned(FFilterExpr) then begin
    FFilterExpr.Free;
    FFilterExpr := nil;
  end;
  if Length(Text) > 0 then
  begin
    FFilterExpr := Licad.CreateMainExpr(FGIS, FLayer);
    try
      FFilterExpr.ParseExpression(Text);
    except
      FFilterExpr.Free;
      FFilterExpr := nil;
      raise;
    end;
  end;
end;

procedure TlicgTable.SetFilterData(const Text: string);
begin
  if IsOpen and Filtered and (Length(Text) > 0) then
  begin
    CheckBrowseMode;
    CreateFilterExpr(Text);
    First;
  end;
  inherited SetFilterText(Text);
end;

function TlicgTable.FilterRecord(Buffer: TRecBuf): Boolean;
var
  SaveState: TDatasetState;
begin
  Result := True;
  if not Filtered then
    exit;
  if not (Assigned(FFilterExpr) or Assigned(OnFilterRecord)) then
    Exit;
  SaveState := SetTempState(dsFilter);
  if Assigned(OnFilterRecord) then
    OnFilterRecord(Self, Result);
  if Assigned(FFilterExpr) and Result then
  begin
    FLayer.Recno := FRecords[FCurRec];
    FLayer.Synchronize;
    Result := FFilterExpr.Expression.AsBoolean;
  end;
  RestoreState(SaveState);
end;

function TlicgTable.GetRecordCount: Integer;
var
  SaveState: TDataSetState;
  SavePosition: integer;
  TempBuffer: TRecordBuffer;
begin
  CheckActive;
  if not Filtered then
    Result := FRecordCount
  else
  begin
    Result := 0;
    SaveState := SetTempState(dsBrowse);
    SavePosition := FCurRec;
    try
      TempBuffer := AllocRecordBuffer;
      InternalFirst;
      while GetRecord(TempBuffer, gmNext, True) = grOk do
        Inc(Result);
    finally
      RestoreState(SaveState);
      FCurRec := SavePosition;
      FreeRecordBuffer(TempBuffer);
    end;
  end;
end;

procedure TlicgTable.Recall;
begin
  CheckActive;
  if State in [dsInsert, dsSetKey] then
    Cancel
  else
  begin
    if (FCurRec < 0) or (FCurRec >= FRecordCount) or
      (RecordCount = 0) then
      Exit;
    DataEvent(deCheckBrowseMode, 0);
    DoBeforeScroll;
    UpdateCursorPos;
    FLayer.RecNo := FRecords[FCurRec];
    FLayer.Recall;
    FreeFieldBuffers;
    SetState(dsBrowse);
    Resync([]);
    DoAfterScroll;
  end;
end;

procedure TlicgTable.OrderBy(const Expression: string;
  Descending: Boolean = False);
var
  IndexObj: TlicgBaseMainExpr;
  Recnum, n, I: Integer;
  SortList: TStringList;
begin
  if FRecordCount < 2 then
    Exit;
  IndexObj := Licad.CreateMainExpr(FGIS, Self.FLayer);
  SortList := TStringList.Create;
  DisableControls;
  try
    IndexObj.ParseExpression(Expression);
    for I := 0 to FRecords.Count - 1 do
    begin
      n := FRecords[I];
      FLayer.Recno := n;
      if not FUseDeleted and FLayer.RecIsDeleted then
        Continue;
      FLayer.Synchronize;
      SortList.AddObject(IndexObj.Expression.AsString, Pointer(n))
    end;
    SortList.Sort;
    { Now recreate the recno list }
    FRecords.Clear;
    FRecords.Capacity := FRecordCount;
    for I := 0 to SortList.Count - 1 do
    begin
      if Descending then
        Recnum := Integer(SortList.Objects[SortList.Count - I - 1])
      else
        Recnum := Integer(SortList.Objects[I]);
      FRecords.Add(Recnum);
    end;
  finally
    SortList.Free;
    IndexObj.Free;
    First;
    EnableControls;
  end;
end;

procedure TlicgTable.SetLayerName(const Value: string);
begin
  InternalClose;
  FLayerName := Value;
  FLayer := nil;
  FMapFields.Clear;
end;

procedure TlicgTable.InternalRefresh;
begin
  InternalClose;
  InternalOpen;
end;

procedure TlicgTable.UnSort;
begin
  RebuildRecordList;
  First;
end;

procedure TlicgTable.DoBeforeGetFieldValue;
var
  n: Integer;
begin
  n := FRecords[FCurRec];
  FLayer.Recno := n;
  FLayer.Synchronize;
end;

procedure TlicgTable.BufferFilter(Buffer: IlicgEntity;
  Operator: TlicgGraphicOperator; const QueryExpression: string; CurvePoints: Integer;
  const Distance: Double; ClearBefore: Boolean);
begin
  SetBaseLayer;
  if (FLayer = nil) or (FGIS = nil) then
    Exit;
  FGIS.QuerySelectTool.QueryBuffer(Buffer, FLayer.Name, QueryExpression, Operator,
    CurvePoints, Distance, FGraphicFilterList, ClearBefore);
end;

procedure TlicgTable.ScopeFilter(const Scope: string; ClearBefore: Boolean);
begin
  SetBaseLayer;
  if (FLayer = nil) or (FGIS = nil) then
    Exit;
  FGIS.QuerySelectTool.QueryExpression(FLayer.Name, Scope, FGraphicFilterList, ClearBefore);
  FGraphicFiltered := True;
end;

procedure TlicgTable.PolygonFilter(Polygon: IlicgEntity;
  Operator: TlicgGraphicOperator; const QueryExpression: string; ClearBefore: Boolean);
begin
  SetBaseLayer;
  if (FLayer = nil) or (FGIS = nil) then
    Exit;
  FGIS.QuerySelectTool.QueryPolygon(Polygon, FLayer.Name, QueryExpression, Operator,
    FGraphicFilterList, ClearBefore);
  FGraphicFiltered := True;
end;

procedure TlicgTable.RectangleFilter(const AxMin, AyMin, AxMax, AyMax: Double;
  Operator: TlicgGraphicOperator; const QueryExpression: string; ClearBefore: Boolean);
begin
  SetBaseLayer;
  if (FLayer = nil) or (FGIS = nil) then
    Exit;
  FGIS.QuerySelectTool.QueryRectangle(Axmin, Aymin, Axmax, Aymax, FLayer.Name, QueryExpression,
    Operator, FGraphicFilterList, ClearBefore);
  FGraphicFiltered := True;
end;

procedure TlicgTable.PolylineIntersects(Polyline: IlicgEntity;
  const QueryExpression: string; ClearBefore: Boolean);
begin
  SetBaseLayer;
  if Assigned(FLayer) and Assigned(FGIS) then begin
     FGIS.QuerySelectTool.QueryPolygon(Polyline, FLayer.Name, QueryExpression, goIntersects, FGraphicFilterList, ClearBefore);
     FGraphicFiltered := True;
  end;
end;

procedure TlicgTable.FilterFromLayer(SourceLayer: TlicgBaseLayer;
  const QueryExpression: string; Operator: TlicgGraphicOperator; ClearBefore: Boolean);
var
  E: IlicgEntity;
  WasInit: Boolean;
  DoClearBefore: Boolean;
begin
  if Sourcelayer = nil then
    Exit;
  WasInit := false;
  SourceLayer.First;
  while not SourceLayer.Eof do
  begin
    if SourceLayer.RecIsDeleted then
    begin
      SourceLayer.Next;
      Continue;
    end;
    E := SourceLayer.LoadEntityWithRecno(SourceLayer.Recno);
    if E = nil then
    begin
      Sourcelayer.Next;
      Continue;
    end;
    try
      try
        if not (E.EntityID in [idPolyline,
          idPolygon, idPolyArc, idPolyEllipse,
            idArc,
            idEllipse,
            idSpline]) then
        begin
          Sourcelayer.Next;
          Continue;
        end;
        if not WasInit then
        begin
          DoClearBefore := ClearBefore;
          WasInit := true;
        end
        else
          DoClearBefore := false;
        if E.IsPolygon then
          FGIS.QuerySelectTool.QueryPolygon(E,
            Sourcelayer.Name,
            QueryExpression,
            Operator,
            FGraphicFilterList,
            DoClearBefore)
        else
           FGIS.QuerySelectTool.QueryPolygon(E, Sourcelayer.Name, QueryExpression, goIntersects, FGraphicFilterList, DoClearBefore);
      finally
        E := nil;
      end;
    finally
      Sourcelayer.Next;
    end;
  end;
  FGraphicFiltered := True;
end;

procedure TlicgTable.AllocateBLOBPointer(Field: TField; var P: Pointer);
begin
  // save the source recno in the blob field
  P := Pointer(GetSourceRecNo);
end;

procedure TlicgTable.FreeBLOBPointer(Field: TField; var P: Pointer);
begin
  // nothing to do here
end;

procedure TlicgTable.SelectionFilter(Selection: TlicgBaseSelection; ClearBefore: Boolean);
var
  SelIndex: Integer;
  SelLayer: TlicgBaseSelectionLayer;
  Layer: TlicgBaseLayer;
begin
  if FGIS = nil then
    Exit;
  Layer := FGis.Layers.LayerByName(FLayerName);
  if Layer = nil then
    Exit;
  SelIndex := Selection.IndexOf(Layer);
  if SelIndex < 0 then
  begin
    FGraphicFiltered := true;
    FGraphicFilterList.Clear;
    Exit;
  end;
  SelLayer := Selection[SelIndex];
  if ClearBefore then
    FGraphicFilterList.Clear;
  FGraphicFilterList.Assign(SelLayer.SelList);
end;

procedure TlicgTable.DoSelect(Selection: TlicgBaseSelection);
var
  I: Integer;
begin
  if (FGIS = nil) or (FLayer = nil) then
    Exit;
  for I := 0 to FRecords.Count - 1 do
    Selection.Add(Layer, FRecords[I]);
end;

procedure TlicgTable.GetBlobField(Field: TField; Stream: TStream);
begin
end;

procedure TlicgTable.SetBlobField(Field: TField; Stream: TStream);
begin
end;

Procedure TlicgTable.AddRecnoToFRecords(Const Recno:Integer);
begin
  FRecords.Add(Recno);
end;

function TlicgTable.GetGIS: TlicgBaseGIS;
begin
  Result := FGis;
end;

function TlicgTable.GetLayer: TlicgBaseLayer;
begin
  Result := FLayer;
end;

function TlicgTable.GetLayerName: string;
begin
  Result := FLayerName;
end;

function TlicgTable.GetMapFields: TlicgGISFields;
begin
  Result := FMapFields;
end;

function TlicgTable.GetMaxRecords: Longint;
begin
  Result := FMaxRecords;
end;

function TlicgTable.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TlicgTable.GetUseDeleted: Boolean;
begin
  Result := FuseDeleted;
end;

procedure TlicgTable.SetLayer(const Value: TlicgBaseLayer);
begin
  FLayer := Value;
end;

procedure TlicgTable.SetMaxRecords(const Value: Longint);
begin
  FMaxRecords := Value;
end;

procedure TlicgTable.SetUseDeleted(const Value: Boolean);
begin
  FUseDeleted := Value;
end;

function TlicgTable.SearchRecNo(Value: Pointer):Boolean;

begin
  FCurRec := Integer(Value);
  result:=(FRecords.IndexofValue(FCurRec))>=0;

end;



function TlicgTable.GetRecords: IlicgIntegerList;
begin
  result := FRecords ;
  if result = Nil then begin
    if Licad<>Nil then
      FRecords := CreateIntegerList;
    result := FRecords ;  
  end;
end;

procedure TlicgTable.SetRecords(Value: IlicgIntegerList);
begin
  FRecords := Value;
end;

{ TlicgDesignTable }

constructor TlicgDesignTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameColumn := TStringList.Create;
  FAliasColumn := TStringList.Create;
  FTypeColumn := TStringList.Create;
  FSizeColumn := CreateIntegerList;
  FDecColumn := CreateIntegerList;
  FOrigFieldNoColumn := CreateIntegerList;
end;

destructor TlicgDesignTable.Destroy;
begin
  inherited Destroy;
  FNameColumn.Free;
  FAliasColumn.Free;
  FTypeColumn.Free;
  FSizeColumn := nil;
  FDecColumn := nil;
  FOrigFieldNoColumn := nil;
end;

procedure TlicgDesignTable.AllocateBookMark(RecordID, Bookmark: Pointer);
begin
  PInteger(Bookmark)^ := Integer(RecordID);
end;

function TlicgDesignTable.AllocateRecordID: Pointer;
begin
  Result := Pointer(FCurRec);
end;

function TlicgDesignTable.DoBookmarkValid(Bookmark: TBookmark): boolean;
begin
  result := (PInteger(Bookmark)^ > 0) and (PInteger(Bookmark)^ <= FRecordCount);
end;

function TlicgDesignTable.DoCompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  b1, b2: integer;
begin
  b1 := PInteger(Bookmark1)^;
  b2 := PInteger(Bookmark2)^;
  if b1 = b2 then
    Result := 0
  else if b1 < b2 then
    Result := -1
  else
    Result := 1;
end;

procedure TlicgDesignTable.DisposeRecordID(Value: Pointer);
begin
  // Do nothing, no need to dispose since pointer is just an integer
end;

procedure TlicgDesignTable.DoBeforeSetFieldValue(Inserting: Boolean);
begin
  if Inserting then
  begin
    FNameColumn.Add('FIELD1');
    FAliasColumn.Add('FIELD1');
    FTypeColumn.Add('C');
    FSizeColumn.Add(10);
    FDecColumn.Add(0);
    FOrigFieldNoColumn.Add(0);
    FModified := True;
    FRecordCount := FNameColumn.Count;
    FCurRec := FRecordCount - 1;
  end;
end;

procedure TlicgDesignTable.DoCreateFieldDefs;
var
  FldDef: TFieldDef;
begin
  FieldDefs.Clear;
  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'FIELDNAME';
  FldDef.DataType := ftString;
  FldDef.Size := 100;
  FldDef.Required := False;

  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'ALIAS';
  FldDef.DataType := ftString;
  FldDef.Size := 100;
  FldDef.Required := False;

  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'TYPE';
  FldDef.DataType := ftString;
  FldDef.Size := 20;
  FldDef.Required := False;

  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'SIZE';
  FldDef.DataType := ftInteger;
  FldDef.Size := 0;
  FldDef.Required := False;

  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'DEC';
  FldDef.DataType := ftInteger;
  FldDef.Size := 0;
  FldDef.Required := False;

  FldDef := FieldDefs.AddFieldDef;
  FldDef.Name := 'ORIG_FIELDNO';
  FldDef.DataType := ftInteger;
  FldDef.Size := 0;
  FldDef.Required := False;
end;

procedure TlicgDesignTable.DoDeleteRecord;
begin
  UpdateCursorPos;
  if FRecordCount = 0 then
    Exit;
  FNameColumn.Delete(FCurRec);
  FAliasColumn.Delete(FCurRec);
  FTypeColumn.Delete(FCurRec);
  FSizeColumn.Delete(FCurRec);
  FDecColumn.Delete(FCurRec);
  FOrigFieldNoColumn.Delete(FCurRec);
  FRecordCount := FNameColumn.Count;
  if FCurRec > FRecordCount - 1 then
    Dec(FCurRec);
  if (FCurRec < 0) and (FRecordCount > 0) then
    FCurRec := 0;
  FModified := true;
end;

procedure TlicgDesignTable.DoFirst;
begin
  FCurRec := -1;
end;

procedure TlicgDesignTable.DoGotoBookmark(Bookmark: Pointer);
begin
  GotoRecordID(Pointer(PInteger(Bookmark)^));
end;

procedure TlicgDesignTable.DoLast;
begin
  FCurRec := FRecordCount;
end;

function TlicgDesignTable.DoOpen: Boolean;
begin
  FRecordCount := 0;
  FCurRec := -1;
  Result := True;
end;

procedure TlicgDesignTable.DoClose;
begin
  FNameColumn.Clear;
  FAliasColumn.Clear;
  FTypeColumn.Clear;
  FSizeColumn.Clear;
  FDecColumn.Clear;
  FOrigFieldNoColumn.Clear;
end;

function TlicgDesignTable.GetBookMarkSize: Integer;
begin
  Result := sizeof(Integer);
end;

function TlicgDesignTable.GetFieldValue(Field: TField): Variant;
begin
  if AnsiCompareText(Field.FieldName, 'FIELDNAME') = 0 then
    result := FNameColumn[FCurRec]
  else if AnsiCompareText(Field.FieldName, 'ALIAS') = 0 then
    result := FAliasColumn[FCurRec]
  else if AnsiCompareText(Field.FieldName, 'TYPE') = 0 then
    result := FTypeColumn[FCurRec]
  else if AnsiCompareText(Field.FieldName, 'SIZE') = 0 then
    result := FSizeColumn[FCurRec]
  else if AnsiCompareText(Field.FieldName, 'DEC') = 0 then
    result := FDecColumn[FCurRec]
  else if AnsiCompareText(Field.FieldName, 'ORIG_FIELDNO') = 0 then
    result := Integer(FOrigFieldNoColumn[FCurRec]);
end;

function TlicgDesignTable.GetRecNo: Integer;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (FRecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
end;

function TlicgDesignTable.GetRecordCount: Integer;
begin
  CheckActive;
  Result := FRecordCount
end;

procedure TlicgDesignTable.GotoRecordID(Value: Pointer);
begin
  FCurRec := Integer(Value);
end;

procedure TlicgDesignTable.InternalRefresh;
begin
  InternalClose;
  InternalOpen;
end;

function TlicgDesignTable.IsSequenced: Boolean;
begin
  Result := True;
end;

function TlicgDesignTable.Navigate(Buffer: TRecBuf; GetMode: TGetMode;
  doCheck: Boolean): TGetResult;
begin
  if FRecordCount < 1 then
    Result := grEOF
  else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        begin
          if FCurRec >= FRecordCount - 1 then
            Result := grEOF
          else
            Inc(FCurRec);
        end;
      gmPrior:
        begin
          if FCurRec <= 0 then
          begin
            Result := grBOF;
            FCurRec := -1;
          end
          else
            Dec(FCurRec);
        end;
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= FRecordCount) then
          Result := grError;
    end;
  end;
end;

procedure TlicgDesignTable.SetFieldValue(Field: TField; const Value: Variant);
var
  n, v: Integer;
begin
  if FCurRec < 0 then
    n := 0
  else
    n := FCurRec;
  if AnsiCompareText(Field.FieldName, 'FIELDNAME') = 0 then
  begin
    FNameColumn[n] := Value;
    FAliasColumn[n] := Value;
  end
  else if AnsiCompareText(Field.FieldName, 'ALIAS') = 0 then
    FAliasColumn[n] := Value
  else if AnsiCompareText(Field.FieldName, 'TYPE') = 0 then
    FTypeColumn[n] := Value
  else if AnsiCompareText(Field.FieldName, 'SIZE') = 0 then
  begin
    v := Value;
    FSizeColumn[n] := v;
  end
  else if AnsiCompareText(Field.FieldName, 'DEC') = 0 then
  begin
    v := Value;
    FDecColumn[n] := v;
  end
  else if AnsiCompareText(Field.FieldName, 'ORIG_FIELDNO') = 0 then
  begin
    v := Value;
    FOrigFieldNoColumn[n] := v;
  end;
  FModified := True;
end;

procedure TlicgDesignTable.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecordCount) then
  begin
    FCurRec := Value - 1;
    Resync([]);
    doAfterScroll;
  end;
end;

function TlicgDesignTable.GetCanModify: Boolean;
begin
  result := true;
end;

procedure TlicgDesignTable.DoAfterGetFieldValue;
begin
end;

procedure TlicgDesignTable.DoAfterSetFieldValue(Inserting: Boolean);
begin
end;

procedure TlicgDesignTable.DoBeforeGetFieldValue;
begin
end;

procedure TlicgDesignTable.GetBlobField(Field: TField; Stream: TStream);
begin
end;

procedure TlicgDesignTable.SetBlobField(Field: TField; Stream: TStream);
begin
end;

end.
