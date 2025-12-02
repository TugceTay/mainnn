unit Lider.CG.Com.Rtree;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt;

const
  EXT_RTREE = '.LRX';

  TREE_VERSION = 1001;
  TREE_ERROR = -1;
  OK = 0;
  BUCKET_SIZE = 56; // before can be as much as 140, must be set to 100
  LOWER_BOUND = 20;
  HALF_BUCKET = BUCKET_SIZE div 2;
  NOT_FOUND = -1;
  ROOT_CHANGED = 1;
  MIN_VALUE = -999999999999.0;
  NULL_RECT: TlicgExtent = (
    LowerLeft: (
    X: MIN_VALUE;
    Y: MIN_VALUE
  );
    UpperRight: (
    X: MIN_VALUE;
    Y: MIN_VALUE
  )
  );

type
  // Structure of an object list element
  POLstElem = ^TOLstElem;

  TOLstElem = record
    obj: Integer;    // disk address of the spatial object
    r: TlicgExtent;      // MBR of the object
    lev: integer;    // level of the object in the tree
    Next: POLstElem; // Next element in the list
  end;

  // Object list class declaration
  TOList = class
  private
    Head, Tail, Curr: POLstElem;
  public
    constructor Create;
    destructor Destroy; override;
    function isEmpty: boolean;
    function Insert(o: Integer; const r: TlicgExtent): integer;
    function Insertl(o: Integer; const r: TlicgExtent; l: integer): integer;
    function FetchORL(var o: Integer; var r: TlicgExtent; var l: integer): integer;
    function FetchOR(var o: Integer; var r: TlicgExtent): integer;
    function FetchO: Integer;
    function FetchR: TlicgExtent;
    procedure Zap;
    procedure Rewind;
  end;

  TSearchType = (stOverlap, stDisjoint, stMeet, stCoveredBy, stCovers, stInside, stContains, stEqual, stSetFilter);

  TSearchList = class
  private
    b: TBits;
    MinRecno: Integer;
    MaxRecno: Integer;
    ReferenceCount: Integer;
  public
    constructor Create(RecCount: Integer);
    destructor Destroy; override;
    procedure Add(Recno: Integer);
  end;

  TTreeType = (ttRTree, ttRStar);

  { - xxx.rtx the table index (the node pages)
    - xxx.rtc the catalog for the index
      the catalog will have a header as follows, and
      followed by a list of free page number in file .rtx Resulting from
      deleted nodes
  }

  TRTCatalog = packed record
    RootNode: Integer; // Address of root node
    Depth: Integer; // depth of the tree
    PageCount: Integer; // number of occupied pages on disk
    Implementor: Integer; // Luis = #1, Garry = #2
    FileType: byte;
    PageSize: byte; // in k
    TreeType: TTreeType;
    Version: Integer;
    BucketSize: Integer; // number of entries per node
    LowerBound: Integer; // minimum no. of entries per node
    LastUpdate: TDateTime; // last updated
    NextAvail: Integer; // next available page address
  end;

  // Data structure of an R(*)tree entry
  TRTEntry = packed record
    R: TlicgExtent; // The MBR and ...
    Child: Integer; // ... disk address of the child object or record number in .EZX file
  end;

  // Structure of a node disk image
  TDiskPage = packed record
    Parent: Integer; // Parent node object disk address
    FullEntries: Integer; // # of entries in use
    Entries: array[0..BUCKET_SIZE - 1] of TRTEntry; // Entries
    PrevAvail: Integer; // previous available page address
    Leaf: Boolean; // The leaf flag
    Dummy: array[0..18] of byte; // every disk node page is of 2048 bytes
  end;

  TCompareOperator = (coBT, coEQ, coGT, coGE, coLT, coLE);

  TRTNode = class;

  TDiskPageList = class;

  TRTree = class
  private
    FOpenMode: Word; // fmOpenReadWrite or fmShareDenyNone or other
    levFlags: byte; // Used for rstar trees only.
    FCheckCancelSearch: Boolean;
    FSearchCanceled: Boolean;
  protected
    function GetLayer: TObject; virtual; abstract;
    procedure SetLayer(const Value: TObject); virtual; abstract;
    function GetList: TDiskPageList; virtual; abstract;
    function chooseLeaf(const r: TlicgExtent): TRTNode;
    function chooseNode(const r: TlicgExtent; lev: integer): TRTNode;
    function IntInsert(const r: TlicgExtent; o, lev: Integer): integer;
    //function  GetCheckCancelSearch: Boolean;
    function GetOpenMode: Word;
    function GetSearchCanceled: Boolean;
    //procedure SetCheckCancelSearch(const Value: Boolean);
    procedure SetOpenMode(const Value: Word);
    procedure SetSearchCanceled(const Value: Boolean);
  public
    TreeType: TTreeType; // The type of this tree
    RootNode: TRTNode; // The root object
    Depth: Word; // The depth of the rtree
    RootId: Integer; // Root object disk address
    FName: string;
    constructor Create(t: TTreeType; Mode: Word);
    constructor CreateName(t: TTreeType; const Name: string; Mode: Word);
    destructor Destroy; override;
    function CreateNewNode: TRTNode; virtual;
    function CreateIndex(const Name: string): integer; virtual;
    procedure DropIndex; virtual;
    function Open(const Name: string; Mode: Word): integer; virtual;
    procedure Close; virtual;
    procedure ReadCatalog(var IdxInfo: TRTCatalog); virtual;
    procedure WriteCatalog(const IdxInfo: TRTCatalog); virtual;
    function Insert(const r: TlicgExtent; o: Integer): integer;
    function Delete(const r: TlicgExtent; o: Integer): integer;
    function Update(const r: TlicgExtent; o: Integer; const newr: TlicgExtent): integer;
    function Search(s: TSearchType; const r: TlicgExtent; ol: IlicgIntegerList;
      RecordCount: Integer; AutoClear: Boolean = True): Boolean;
    procedure CheckSearchCanceled; virtual;
    procedure StartSearch; virtual;
    procedure FinishSearch; virtual;
    procedure GetAllNodes(ol: IlicgIntegerList);
    function RootExtent: TlicgExtent;
    function FillFactor(var NodeCount, Entries: Integer): Double;
    procedure FlushFiles; virtual;
    procedure FindArea(Compare: TCompareOperator; const Area1, Area2: Double; ol: TList);
    function IsEmpty: Boolean;
    property OpenMode: Word read GetOpenMode write SetOpenMode;
    property Layer: TObject read GetLayer write SetLayer;
    //property CheckCancelSearch: Boolean read GetCheckCancelSearch write SetCheckCancelSearch;
    property SearchCanceled: Boolean read GetSearchCanceled write SetSearchCanceled;
    property DiskPages: TDiskPageList read GetList;
  end;

  TDiskPageList = class
  private
    FList: array of TDiskPage;
    FAvailable: IlicgIntegerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): TDiskPage;
    procedure Grow;
    procedure Put(Index: Integer; const Item: TDiskPage);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetCapacity: Integer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Item: TDiskPage): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IsDeleted(Index: Integer): Boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: TDiskPage read Get write Put; default;
  end;

  // Class declaration of an R-tree node
  TRTNode = class
  private
  protected
    rt: TRTree; // the tree that this node belongs to
    oid: Integer; // disk address of the node object
    Data: TDiskPage; // Pointer into a copy of the info. Used in updates.

    function bestEntry(const r: TlicgExtent): Integer;
    function findLeaf(const r: TlicgExtent; o: Integer): integer;
    function equalSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function containsSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function insideSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function coversSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function coveredBySearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function disjointSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function meetSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
    function overlapSearch(const r: TlicgExtent; ol: TSearchList; _result: Boolean = true): Boolean;
    function SetFilterSearch(const r: TlicgExtent; var ol: TSearchList; _result: Boolean = True): Boolean;
  public
    constructor Create(rtree: TRTree);
    function isLeaf: boolean;
    function isRoot: boolean;
    procedure Read(NId: Integer); virtual;
    procedure Write; virtual;
    procedure AddNodeToFile; virtual;
    procedure DeleteNodeFromFile; virtual;
    function Delete(o: Integer; l: integer; rl: TOList): integer;
    function Insert(const r: TlicgExtent; o: Integer; var newo: Integer): integer;
    function Locate(o: Integer): integer;
    procedure Compact;
    procedure propagate(n: integer);
    procedure GetAllNodes(ol: IlicgIntegerList);
    procedure FindArea(Compare: TCompareOperator; const Area1, Area2: double; ol: TList);
    procedure FillFactor(var NodeCount, Entries: Integer);
  end;

function Contains_rect(const r1, r2: TlicgExtent): Boolean;

implementation

uses
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.LicadInt;

procedure SwapEntry(var Source, Dest: TRTEntry);
var
  tmp: TRTEntry;
begin
  tmp := Source;
  Source := Dest;
  Dest := tmp;
end;

function Intersect_rect(const r1, r2: TlicgExtent): TlicgExtent;
begin
  Result.LowerLeft.x := Max(r1.LowerLeft.x, r2.LowerLeft.x);
  Result.LowerLeft.y := Max(r1.LowerLeft.y, r2.LowerLeft.y);
  Result.UpperRight.x := Min(r1.UpperRight.x, r2.UpperRight.x);
  Result.UpperRight.y := Min(r1.UpperRight.y, r2.UpperRight.y);

  if (Result.LowerLeft.x > Result.UpperRight.x) or (Result.LowerLeft.y > Result.UpperRight.y)
    then
    Result.LowerLeft.x := MIN_VALUE; // no intersection
end;

// Return the mbr of r1 and r2
function Extent_rect(const r1, r2: TlicgExtent): TlicgExtent;
begin
  Result := r1;
  if r1.LowerLeft.x = MIN_VALUE then
    Result := r2
  else if not (r2.LowerLeft.x = MIN_VALUE) then
  begin
    Result.LowerLeft.x := Min(r1.LowerLeft.x, r2.LowerLeft.x);
    Result.LowerLeft.y := Min(r1.LowerLeft.y, r2.LowerLeft.y);
    Result.UpperRight.x := Max(r1.UpperRight.x, r2.UpperRight.x);
    Result.UpperRight.y := Max(r1.UpperRight.y, r2.UpperRight.y);
  end;
end;

// Return the area of this
function Area_rect(const r: TlicgExtent): double;
var
  dx, dy: double;
begin
  if r.LowerLeft.x = MIN_VALUE then
    Result := 0
  else
  begin
    dx := (r.UpperRight.x - r.LowerLeft.x);
    dy := (r.UpperRight.y - r.LowerLeft.y);
    Result := abs(dx * dy);
  end;
end;

// Return the margin of this
function Margin_rect(const r: TlicgExtent): Double;
begin
  if r.LowerLeft.x = MIN_VALUE then
    Result := 0
  else
    Result := 2 * (abs(r.UpperRight.x - r.LowerLeft.x) + abs(r.UpperRight.y - r.LowerLeft.y));
end;

// Return delta(Area) if this is extended with a rect r.
function Delta_rect(const r1, r2: TlicgExtent): double;
begin
  Result := Area_rect(Extent_rect(r1, r2)) - Area_rect(r1);
end;

function Contains_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := ((r2.LowerLeft.x >= r1.LowerLeft.x) and (r2.LowerLeft.x <= r1.UpperRight.x)
    and (r2.UpperRight.x >= r1.LowerLeft.x) and (r2.UpperRight.x <= r1.UpperRight.x)
    and (r2.LowerLeft.y >= r1.LowerLeft.y) and (r2.LowerLeft.y <= r1.UpperRight.y)
    and (r2.UpperRight.y >= r1.LowerLeft.y) and (r2.UpperRight.y <= r1.UpperRight.y));
end;

function Inside_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := (r1.LowerLeft.x > r2.LowerLeft.x) and (r1.UpperRight.x < r2.UpperRight.x)
    and (r1.LowerLeft.y > r2.LowerLeft.y) and (r1.UpperRight.y < r2.UpperRight.y);
end;

function Covers_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := (r1.LowerLeft.x <= r2.LowerLeft.x) and (r1.UpperRight.x >= r2.UpperRight.x)
    and (r1.LowerLeft.y <= r2.LowerLeft.y) and (r1.UpperRight.y >= r2.UpperRight.y);
end;

function CoveredBy_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := (r1.LowerLeft.x >= r2.LowerLeft.x) and (r1.UpperRight.x <= r2.UpperRight.x)
    and (r1.LowerLeft.y >= r2.LowerLeft.y) and (r1.UpperRight.y <= r2.UpperRight.y);
end;

function Disjoint_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := (r1.LowerLeft.x > r2.UpperRight.x) or (r2.LowerLeft.x > r1.UpperRight.x)
    or (r1.LowerLeft.y > r2.UpperRight.y) or (r2.LowerLeft.y > r1.UpperRight.y);
end;

function Meet_rect(const r1, r2: TlicgExtent): Boolean;
begin
  Result := (r1.LowerLeft.x = r2.UpperRight.x) or (r2.LowerLeft.x = r1.UpperRight.x)
    or (r1.LowerLeft.y = r2.UpperRight.y) or (r2.LowerLeft.y = r1.UpperRight.y);
end;

function SetFilter_rect(const r1, r2: TlicgExtent): boolean;
begin
  Result := (r1.LowerLeft.x <= r2.UpperRight.x) and (r2.LowerLeft.x <= r1.UpperRight.x)
    and (r1.LowerLeft.y <= r2.UpperRight.y) and (r2.LowerLeft.y <= r1.UpperRight.y);
end;

function Overlaps_rect(const r1, r2: TlicgExtent): boolean;
begin
  Result := (r1.LowerLeft.x < r2.UpperRight.x) and (r2.LowerLeft.x < r1.UpperRight.x)
    and (r1.LowerLeft.y < r2.UpperRight.y) and (r2.LowerLeft.y < r1.UpperRight.y);
end;

function Equal_rect(const r1, r2: TlicgExtent): boolean;
begin
  Result := CompareMem(@r1, @r2, sizeof(TlicgExtent));
end;


// Find the pair of entries in r which have the largest distance
procedure PickSeed(var r: array of TRTEntry; n: integer);
var
  i, j, im, jm: integer;
  admax, d: double;
begin
  im := 0;
  jm := 1;
  admax := Area_rect(Extent_rect(r[0].R, r[1].R));
  admax := admax - (Area_rect(r[0].R) + Area_rect(r[1].R));
  for i := 1 to n - 2 do
    for j := i + 1 to n - 1 do
    begin
      d := Area_rect(Extent_rect(r[i].R, r[j].R)) - (Area_rect(r[i].R) +
        Area_rect(r[j].R));
      if d > admax then
      begin
        im := i;
        jm := j;
        admax := d;
      end;
    end;

  SwapEntry(r[im], r[n - 2]);
  SwapEntry(r[jm], r[n - 1]);
end;

function PickNext(var r: array of TRTEntry; n: integer; const mbr0, mbr1:
  TlicgExtent): integer;
var
  i, im, mm: integer;
  d0, d1, admin: double;
begin
  im := 0;

  d0 := Delta_rect(r[0].R, mbr0);
  d1 := Delta_rect(r[0].R, mbr1);

  if d0 < d1 then
  begin
    admin := d0;
    mm := 0;
  end
  else
  begin
    admin := d1;
    mm := 1;
  end;

  for i := 1 to n - 1 do
  begin
    d0 := Delta_rect(r[i].R, mbr0);
    d1 := Delta_rect(r[i].R, mbr1);
    if Min(d0, d1) < admin then
    begin
      im := i;
      if d0 < d1 then
      begin
        admin := d0;
        mm := 0;
      end
      else
      begin
        admin := d1;
        mm := 1;
      end;
    end;
  end;
  SwapEntry(r[n - 1], r[im]);
  Result := mm;
end;




// TOList - class implementation
constructor TOList.Create;
begin
  inherited Create;
  Curr := nil;
  Head := nil;
  Tail := nil;
end;

destructor TOList.destroy;
begin
  Zap;
  inherited Destroy;
end;

procedure TOList.Zap;
var
  p: POLstElem;
begin
  while Head <> nil do
  begin
    p := Head;
    Head := Head.Next;
    dispose(p);
  end;
  Tail := nil;
  Curr := nil;
end;

// Reposition the current pointer at the Head;
procedure TOList.Rewind;
begin
  Curr := Head;
end;

// Insert to tail
function TOList.Insert(o: Integer; const r: TlicgExtent): integer;
var
  p: POLstElem;
begin
  New(p);

  p.obj := o;
  p.r := r;
  p.lev := 0;
  p.Next := nil;

  if Tail <> nil then
    Tail.Next := p;
  if Head = nil then
    Head := p;
  Tail := p;

  Rewind;
  //Dispose(p); // ilker ekleme
  Result := OK;
end;

function TOList.Insertl(o: Integer; const r: TlicgExtent; l: integer): integer;
var
  p: POLstElem;
begin
  new(p);

  p.obj := o;
  p.r := r;
  p.lev := l;
  p.Next := nil;

  if Tail <> nil then
    Tail.Next := p;
  if Head = nil then
    Head := p;
  Tail := p;
  Rewind;
  //Dispose(p); // ilker ekleme
  Result := OK;
end;

// Fetch from head.
function TOList.FetchORL(var o: Integer; var r: TlicgExtent; var l: integer): integer;
begin
  if Curr = nil then
  begin
    Result := TREE_ERROR;
    exit;
  end;

  o := Curr.obj;
  r := Curr.r;
  l := Curr.lev;
  Curr := Curr.Next;

  Result := OK;
end;

// Fetch from head.
function TOList.FetchOR(var o: Integer; var r: TlicgExtent): integer;
begin
  if Curr = nil then
  begin
    Result := TREE_ERROR;
    exit;
  end;
  o := Curr.obj;
  r := Curr.r;
  Curr := Curr.Next;
  Result := OK;
end;

// Delete from Head, return the OID
function TOList.FetchO: Integer;
var
  o: Integer;
begin
  if Curr = nil then
  begin
    Result := TREE_ERROR;
    exit;
  end;

  o := Curr.obj;
  Curr := Curr.Next;
  Result := o;
end;

// Delete from Head, return the rect
function TOList.FetchR: TlicgExtent;
begin
  Result := Curr.r;
  Curr := Curr.Next;
end;

function TOList.isEmpty: boolean;
begin
  Result := (Curr = nil);
end;


{ TSearchList }

constructor TSearchList.Create(RecCount: Integer);
begin
  inherited Create;
  b := TBits.Create;
  b.Size := RecCount + 1;
  MinRecno := RecCount;
  MaxRecno := 0;
end;

destructor TSearchList.Destroy;
begin
  b.free;
  inherited Destroy;
end;

procedure TSearchList.Add(Recno: Integer);
begin
  b[Recno] := true;
  Inc(ReferenceCount);
  if Recno < MinRecno then
    MinRecno := Recno;
  if Recno > MaxRecno then
    MaxRecno := Recno;
end;

{ TRTree }

constructor TRTree.Create(t: TTreeType; Mode: Word);
begin
  inherited Create;
  TreeType := t;
  RootNode := CreateNewNode;
  RootId := -1;
  FOpenMode := Mode;

  FCheckCancelSearch := true;
end;

constructor TRTree.CreateName(t: TTreeType; const Name: string; Mode: Word);
begin
  Create(t, Mode);
  Open(Name, Mode);
end;

destructor TRTree.Destroy;
begin
  Close;
  RootNode.Free;
  inherited Destroy;
end;

{
function TRTree.GetCheckCancelSearch: Boolean;
begin
  Result:=FCheckCancelSearch
end;
}

function TRTree.GetOpenMode: Word;
begin
  Result := FOpenMode
end;

function TRTree.GetSearchCanceled: Boolean;
begin
  Result := FSearchCanceled
end;

{
procedure TRTree.SetCheckCancelSearch(const Value: Boolean);
begin
  FCheckCancelSearch:= Value
end;
}

procedure TRTree.SetOpenMode(const Value: Word);
begin
  FOpenMode := Value
end;

procedure TRTree.SetSearchCanceled(const Value: Boolean);
begin
  FSearchCanceled := Value
end;

// Insert object o with mbr r into rtree

function TRTree.Insert(const r: TlicgExtent; o: Integer): integer;
var
  L: TRTNode;
  retCode: integer;
  IdxInfo: TRTCatalog;
  newo: Integer;
begin
  levFlags := 0;

  L := chooseLeaf(r);
  newo := -1;
  retCode := L.Insert(r, o, newo);
  if retcode = TREE_ERROR then
  begin
    L.free;
    Result := TREE_ERROR;
    exit;
  end;
  L.free;

  if retCode = ROOT_CHANGED then
  begin
    Inc(Depth); // Grow tree taller
    RootNode.Read(RootId); // Read the old root
    RootId := TRTNode(RootNode).Data.Parent; // Take its parent id

    // Update the catalog entry
    ReadCatalog(IdxInfo);
    IdxInfo.RootNode := RootId;
    IdxInfo.Depth := Depth;
    WriteCatalog(IdxInfo);
  end;

  Result := OK;
end;

// Insert internal node

function TRTRee.IntInsert(const r: TlicgExtent; o, lev: Integer): integer;
var
  L: TRTNode;
  newo, retCode: integer;
  IdxInfo: TRTCatalog;
begin
  L := chooseNode(r, lev);

  newo := -1;
  retCode := L.Insert(r, o, newo);

  L.free;

  if retCode = TREE_ERROR then
  begin
    Result := TREE_ERROR;
    exit;
  end;

  if retCode = ROOT_CHANGED then
  begin
    Inc(Depth);
    RootNode.Read(RootId); // Read the old root
    RootId := TRTNode(RootNode).Data.Parent; // Take its parent id
      // Read the catalog entry
    ReadCatalog(IdxInfo);

      // Assign the new values
    IdxInfo.RootNode := RootId;
    IdxInfo.Depth := Depth;

      // Update it
    WriteCatalog(IdxInfo);
  end;

  Result := OK;
end;

// Delete the object o from the rtree

function TRTree.Delete(const r: TlicgExtent; o: Integer): integer;
var
  L: TRTNode;
  rl: TOList;
  rc, il: integer;
  IdxInfo: TRTCatalog;
  rr: TlicgExtent;
  ro: Integer;
begin

  L := CreateNewNode;
  rl := TOList.Create;
  try
    L.Read(RootId);
    if TRTNode(L).findLeaf(r, o) = TREE_ERROR then
    begin
      Result := TREE_ERROR;
      exit;
    end;

    rc := L.Delete(o, 0, rl);

    if rc = TREE_ERROR then
    begin
      Result := TREE_ERROR;
      exit;
    end;

    if rc = ROOT_CHANGED then
    begin
      Dec(Depth);
      RootNode.Read(RootId); // Read the old root
      RootId := TRTNode(RootNode).Data.Entries[0].Child; // Take the new root id
      RootNode.DeleteNodeFromFile;

      RootNode.Read(RootId); // Read the new root

      // Update the catalog entry
      ReadCatalog(IdxInfo);

      IdxInfo.RootNode := RootId;
      IdxInfo.Depth := Depth;

      WriteCatalog(IdxInfo);
    end;

    while not rl.isEmpty do
    begin
      rl.FetchORL(ro, rr, il);
      if il = 0 then
        Insert(rr, ro) // leaf level
      else
        IntInsert(rr, ro, il); // internal
    end;

  finally
    L.free;
    rl.free;
  end;

  Result := OK;
end;

function TRTree.Update(const r: TlicgExtent; o: Integer; const newr: TlicgExtent): integer;
begin
  Result := Delete(r, o);
  if not (Result = OK) then
    Exit;
  Result := Insert(newr, o);
end;

// Choose the best place to create the new internal node
function TRTree.chooseNode(const r: TlicgExtent; lev: integer): TRTNode;
var
  best: Integer;
  l: integer;
begin
  Result := CreateNewNode;
  Result.Read(RootId); // Read the root node into work space

  l := Depth;
  while l > lev do
  begin
    best := TRTNode(Result).bestEntry(r);
    Result.Read(best);
    Dec(l);
  end;
end;

// Choose the best leaf node to insert the new rect r.
function TRTree.chooseLeaf(const r: TlicgExtent): TRTNode;
var
  best: Integer;
begin

  Result := CreateNewNode;

  Result.Read(RootId); // Read the root node into work space

  while not Result.isLeaf do
  begin
    best := TRTNode(Result).bestEntry(r);
    Result.Read(best);
  end;
end;

procedure TRTree.CheckSearchCanceled;
begin
  // Nothing to do here. Only for descendants.
end;

procedure TRTree.StartSearch;
begin
  // Nothing to do here. Only for descendants.
end;

// Look up rectangle r with search type s
function TRTree.Search(s: TSearchType; const r: TlicgExtent; ol: IlicgIntegerList;
  RecordCount: Integer; AutoClear: Boolean = True): Boolean;
var
  sl: TSearchList;
  I: Integer;
begin

  Result := True;
  StartSearch;
  if not Assigned(ol) then
  begin
    FinishSearch;
    Exit(True);
  end;

  sl := TSearchList.Create(RecordCount);
  try
    RootNode.Read(RootId);
    case s of
      stSetFilter: Result := TRTNode(RootNode).SetFilterSearch(r, sl);
      stOverlap: Result := TRTNode(RootNode).overlapSearch(r, sl);
      stDisjoint: Result := TRTNode(RootNode).disjointSearch(r, sl);
      stMeet: Result := TRTNode(RootNode).meetSearch(r, sl);
      stCoveredBy: Result := TRTNode(RootNode).coveredBySearch(r, sl);
      stCovers: Result := TRTNode(RootNode).coversSearch(r, sl);
      stInside: Result := TRTNode(RootNode).insideSearch(r, sl);
      stContains: Result := TRTNode(RootNode).containsSearch(r, sl);
      stEqual: Result := TRTNode(RootNode).equalSearch(r, sl);
    end;

    FinishSearch;

    if TSearchList(sl).ReferenceCount = 0 then
      Exit(True);

    if AutoClear then
      ol.Capacity := TSearchList(sl).ReferenceCount;
    for I := TSearchList(sl).MinRecno to TSearchList(sl).MaxRecno do
      if TSearchList(sl).b[I] then
        ol.Add(I);
  finally
    sl.Free;
  end;
end;

procedure TRTree.GetAllNodes(ol: IlicgIntegerList);
begin
  ol.clear;
  RootNode.Read(RootID);
  RootNode.GetAllNodes(ol);
end;

function TRTree.RootExtent: TlicgExtent;
var
  i: integer;
begin
  RootNode.Read(RootID);
  Result := TRTNode(RootNode).Data.Entries[0].R;
  for i := 1 to TRTNode(RootNode).Data.FullEntries - 1 do
    Result := Extent_rect(Result, TRTNode(RootNode).Data.Entries[i].R);
end;

function TRTree.IsEmpty: Boolean;
begin
  RootNode.Read(RootID);
  Result := (TRTNode(RootNode).Data.FullEntries = 0);
end;

procedure TRTree.FindArea(Compare: TCompareOperator; const Area1, Area2: Double;
  ol: TList);
begin
  RootNode.Read(RootID);
  RootNode.FindArea(Compare, Area1, Area2, ol);

end;

function TRTree.FillFactor(var NodeCount, Entries: Integer): Double;
begin
  RootNode.Read(RootID);
  NodeCount := 0;
  Entries := 0;
  RootNode.FillFactor(NodeCount, Entries);
  Result := Entries / NodeCount;
end;

procedure TRTree.Close;
begin
//  inherited;

end;

function TRTree.CreateIndex(const Name: string): integer;
begin

end;

function TRTree.CreateNewNode: TRTNode;
begin

end;

procedure TRTree.DropIndex;
begin
  // inherited;

end;

procedure TRTree.FlushFiles;
begin
  inherited;

end;

function TRTree.Open(const Name: string; Mode: Word): integer;
begin

end;

procedure TRTree.ReadCatalog(var IdxInfo: TRTCatalog);
begin
  inherited;

end;

procedure TRTree.WriteCatalog(const IdxInfo: TRTCatalog);
begin
  inherited;

end;

procedure TRTree.FinishSearch;
begin
  //
end;

{ TDiskPageList }

constructor TDiskPageList.Create;
begin
  inherited Create;
  FAvailable := CreateIntegerList;
end;

destructor TDiskPageList.Destroy;
begin
  Clear;
  Finalize(FList);
  FAvailable := nil;
  inherited;
end;

function TDiskPageList.Add(const Item: TDiskPage): Integer;
begin
  if FAvailable.Count = 0 then
  begin
    Result := FCount;
    if Result = FCapacity then
      Grow;
    FList[Result] := Item;
    Inc(FCount);
  end
  else
  begin
    Result := FAvailable[0];
    FList[Result] := Item;
    FAvailable.Delete(0);
  end;
end;

procedure TDiskPageList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
  FAvailable.Clear;
end;

procedure TDiskPageList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError('TDiskPageList.Delete: ' + SWordListError);
  FAvailable.Add(Index);
end;

function TDiskPageList.Get(Index: Integer): TDiskPage;
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError('TDiskPageList.Get: ' + SWordListError);
  Result := FList[Index];
end;

procedure TDiskPageList.Put(Index: Integer; const Item: TDiskPage);
begin
  if (Index < 0) or (Index >= FCount) then
    LicadGISError('TDiskPageList.Put: ' + SWordListError);
  FList[Index] := Item;
end;

procedure TDiskPageList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TDiskPageList.LoadFromStream(Stream: TStream);
var
  Value: TDiskPage;
  I, N: Integer;
begin
  Clear;
  with Stream do
  begin
    Read(N, Sizeof(N));
    Self.SetCapacity(N);
    for I := 1 to N do
    begin
      Read(Value, Sizeof(Value));
      Self.Add(Value);
    end;
  end;
  FAvailable.LoadFromStream(Stream);
end;

procedure TDiskPageList.SaveToStream(Stream: TStream);
var
  I, N: Integer;
  Value: TDiskPage;
begin
  N := FCount;
  with Stream do
  begin
    Write(N, Sizeof(N));
    for I := 0 to FCount - 1 do
    begin
      Value := FList[I];
      Write(Value, Sizeof(Value));
    end;
  end;
  FAvailable.SaveToStream(Stream);
end;

procedure TDiskPageList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    LicadGISError('TDiskPageList.SetCapacity: ' + SWordListError);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TDiskPageList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    LicadGISError('TDiskPageList.SetCount: ' + SWordListError);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList[FCount], (NewCount - FCount) * SizeOf(TDiskPage), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

function TDiskPageList.IsDeleted(Index: Integer): Boolean;
begin
  Result := FAvailable.IndexOfValue(Index) >= 0;
end;

function TDiskPageList.GetCapacity: Integer;
begin
  Result := FCapacity
end;

function TDiskPageList.GetCount: Integer;
begin
  Result := FCount
end;



{ TRTNode class implementation }

constructor TRTNode.Create(rtree: TRTree);
begin
  inherited Create;
  rt := rtree;
end;

procedure TRTNode.FillFactor(var NodeCount, Entries: Integer);
var
  i, me: Integer;
  TmpDiskPage: TDiskPage;
begin
  if not Data.Leaf then
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      TmpDiskPage := Data;
      me := oid;
      Read(Data.Entries[i].Child);
      FillFactor(NodeCount, Entries);
      oid := me;
      Data := TmpDiskPage;
    end;
  end;
  if Data.Leaf then
  begin
    Inc(Entries, Data.FullEntries);
    Inc(NodeCount);
  end;
end;

procedure TRTNode.FindArea(Compare: TCompareOperator; const Area1, Area2: double;
  ol: TList);
var
  i, me: Integer;
  tmpArea: double;
  rslt: Boolean;
  TmpDiskPage: TDiskPage;
begin
  if Data.Leaf then
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      tmpArea := Area_rect(Data.Entries[i].R);
      case Compare of
        coBT:
          rslt := (tmpArea >= Area1) and (tmpArea <= Area2);
        coEQ:
          rslt := tmpArea = Area1;
        coGT:
          rslt := tmpArea > Area1;
        coGE:
          rslt := tmpArea >= Area1;
        coLT:
          rslt := tmpArea < Area1;
        coLE:
          rslt := tmpArea <= Area1;
      else
        rslt := false;
      end;
      if rslt then
        ol.Add(Pointer(Data.Entries[i].Child));
    end
  end
  else
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      tmpArea := Area_rect(Data.Entries[i].R);
      if tmpArea >= Area1 then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        FindArea(Compare, Area1, Area2, ol);
        //Read(me);                        // Refresh
        oid := me;
        Data := TmpDiskPage;
      end;
    end;
  end;
end;

// Locate the smallest entry in which r fits

function TRTNode.bestEntry(const r: TlicgExtent): Integer;
var
  i, mini: integer;
  mina, mind, delta: double;
begin
  mini := 0;

  mina := area_rect(Data.Entries[0].R);
  mind := delta_rect(Data.Entries[0].R, r);

  for i := 1 to Data.FullEntries - 1 do
  begin
    delta := delta_rect(Data.Entries[i].R, r);
    if (delta < mind) or ((delta = mind) and (mina > area_rect(Data.Entries[i].R))) then
    begin
      mini := i;
      mina := area_rect(Data.Entries[i].R);
      mind := delta;
    end;
  end;

  Result := Data.Entries[mini].Child;
end;

procedure TRTNode.GetAllNodes(ol: IlicgIntegerList);
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  if Data.Leaf then
  begin
    for i := 0 to Data.FullEntries - 1 do
      ol.Add(Data.Entries[i].Child);
  end
  else
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      me := oid;
      TmpDiskPage := Data;
      Read(Data.Entries[i].Child);
      GetAllNodes(ol);
      Data := TmpDiskPage;
      oid := me;
    end;
  end;
end;

// Add all objects contained in r to ol, and return it.

function TRTNode.EqualSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Equal_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Equal_rect(Data.Entries[i].R, r) or Covers_rect(Data.Entries[i].R, r)
        or Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        EqualSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
        //Read(me);                        // Refresh
      end;
end;

function TRTNode.ContainsSearch(const r: TlicgExtent; ol: TSearchList): boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Contains_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        ContainsSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
        //Read(me);                        // Refresh
      end;
end;

function TRTNode.InsideSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Inside_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Overlaps_rect(Data.Entries[i].R, r) or CoveredBy_rect(Data.Entries[i].R,
        r) or Inside_rect(Data.Entries[i].R, r) or Equal_rect(Data.Entries[i].R,
        r) or Covers_rect(Data.Entries[i].R, r) or Contains_rect(Data.Entries[i].R,
        r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        InsideSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
        //Read(me);                        // Refresh
      end;
end;

function TRTNode.CoversSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Covers_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Covers_rect(Data.Entries[i].R, r) or Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        CoversSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
      end;
end;

function TRTNode.CoveredBySearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if CoveredBy_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Overlaps_rect(Data.Entries[i].R, r) or CoveredBy_rect(Data.Entries[i].R,
        r) or Equal_rect(Data.Entries[i].R, r) or Covers_rect(Data.Entries[i].R,
        r) or Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        CoveredBySearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
        //Read(me);                        // Refresh
      end;
end;

function TRTNode.DisjointSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Disjoint_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Disjoint_rect(Data.Entries[i].R, r) or Meet_rect(Data.Entries[i].R, r)
        or Overlaps_rect(Data.Entries[i].R, r) or Covers_rect(Data.Entries[i].R,
        r) or Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        DisjointSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
        //Read(me);                        // Refresh
      end;
end;

function TRTNode.MeetSearch(const r: TlicgExtent; ol: TSearchList): Boolean;
var
  I, me: integer;
  TmpDiskPage: TDiskPage;
begin
  result := true;
  if Data.Leaf then
    for i := 0 to Data.FullEntries - 1 do
    begin
      if Meet_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  else
    for i := 0 to Data.FullEntries - 1 do
      // intermediate nodes relations
      if Meet_rect(Data.Entries[i].R, r) or Overlaps_rect(Data.Entries[i].R, r)
        or Covers_rect(Data.Entries[i].R, r) or Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        MeetSearch(r, ol);
        Data := TmpDiskPage;
        oid := me;
      end;
end;

// Add all objects overlapping with r to ol, and return it.

function TRTNode.overlapSearch(const r: TlicgExtent; ol: TSearchList; _result:
  Boolean = true): Boolean;
var
  i: integer;
  me: Integer;
  TmpDiskPage: TDiskPage;
begin
  result := _result;
  if not _result then
    exit;

  if Data.Leaf then
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      if rt.FCheckCancelSearch then
      begin
        rt.CheckSearchCanceled;
        if rt.FSearchCanceled then
        begin
          result := false;
          Exit;
        end;
      end;

      if Overlaps_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  end
  else
  begin

    for i := 0 to Data.FullEntries - 1 do
    begin

      if rt.FCheckCancelSearch then
      begin
        rt.CheckSearchCanceled;
        if rt.FSearchCanceled then
        begin
          result := false;
          Exit;
        end;
      end;

      if Overlaps_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        result := overlapSearch(r, ol, result);
        if not _result then
          exit;
        Data := TmpDiskPage;
        oid := me;
      end;
    end;
  end;
end;

function TRTNode.SetFilterSearch(const r: TlicgExtent; var ol: TSearchList; _result: Boolean): Boolean;
var
  i: integer;
  me: Integer;
  TmpDiskPage: TDiskPage;
begin
  result := _result;
  if not _result then
    exit;

  if Data.Leaf then
  begin
    for i := 0 to Data.FullEntries - 1 do
    begin
      if rt.FCheckCancelSearch then
      begin
        rt.CheckSearchCanceled;
        if rt.FSearchCanceled then
        begin
          result := false;
          Exit;
        end;
      end;

      if SetFilter_rect(Data.Entries[i].R, r) then
        ol.Add(Data.Entries[i].Child);
    end
  end
  else
  begin

    for i := 0 to Data.FullEntries - 1 do
    begin

      if rt.FCheckCancelSearch then
      begin
        rt.CheckSearchCanceled;
        if rt.FSearchCanceled then
        begin
          result := false;
          Exit;
        end;
      end;

      if SetFilter_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        TmpDiskPage := Data;
        Read(Data.Entries[i].Child);
        result := SetFilterSearch(r, ol, result);
        if not _result then
          exit;
        Data := TmpDiskPage;
        oid := me;
      end;
    end;
  end;
end;

procedure TRTNode.propagate(n: integer);
var
  me: integer;
  MyOid, Par: Integer;
  r: TlicgExtent;
  //oldData: TDiskPage;
begin

  if isRoot then
    Exit;

  MyOid := oid;
  Par := Data.Parent;

  r := Data.Entries[n].R;

  Read(Par);

  me := Locate(MyOid);
  if me = NOT_FOUND then
    LicadGISError('propagate');

  //oldData := Data;

  Data.Entries[me].R := Extent_rect(Data.Entries[me].R, r);
  Write;
  //Data := oldData;

  propagate(me);
end;

// Insert r into this. Make all arrangements like splitting.
// Return ROOT_CHANGED if it changes.

function TRTNode.Insert(const r: TlicgExtent; o: Integer; var newo: Integer): integer;
var
  s1, s2, node, m, n, na, nb, me, ric, ret: integer;
  newNode, newRoot, parNode, childNode: TRTNode;
  temp: array[0..BUCKET_SIZE] of TRTEntry;
  MBRa, MBRb: TlicgExtent;
  MyOid, parNewo: Integer;
begin
  ric := OK;
  na := 0;
  nb := 0;

  if Data.FullEntries < BUCKET_SIZE then // Trivial case.
  begin
    Data.Entries[Data.FullEntries].R := r; // Put it at the end
    Data.Entries[Data.FullEntries].Child := o;
    Inc(Data.FullEntries);
    Write;

    if not Data.Leaf then
    begin
      childNode := rt.CreateNewNode;
      childNode.Read(Data.Entries[Data.FullEntries - 1].Child);
      TRTNode(childNode).Data.Parent := oid;
      childNode.Write;
      childNode.free;
    end;

    MyOid := oid;
    propagate(Data.FullEntries - 1);
    Read(MyOid);

    Result := OK;
    exit;
  end;

  // This node is full. It needs to be splitted.

  if isRoot then // If this is the root
  begin
    newRoot := rt.CreateNewNode;
    TRTNode(newRoot).Data.Parent := -1; // invalidate the parent
    TRTNode(newRoot).Data.Leaf := False;
    TRTNode(newRoot).Data.FullEntries := 1; // One entry.

    // First son of the new root is the old root
    TRTNode(newRoot).Data.Entries[0].Child := oid;

    TRTNode(newRoot).AddNodeToFile;

    Data.Parent := TRTNode(newRoot).oid; // The same parent
    ric := ROOT_CHANGED; // Mark root is changed
    newRoot.free;
  end;

  newNode := rt.CreateNewNode;
  parNode := rt.CreateNewNode;
  try

    TRTNode(newNode).Data.Leaf := Data.Leaf; // Initialize the new node
    TRTNode(newNode).Data.FullEntries := 0; // It's empty yet
    TRTNode(newNode).Data.Parent := Data.Parent; // The same parent
    TRTNode(newNode).AddNodeToFile; // Create it on disk
    newo := TRTNode(newNode).oid;

    parNode.Read(Data.Parent);

    me := parNode.Locate(oid); // Which entry is pointing to me?
    if me = NOT_FOUND then
      LicadGISError('Insert');

    TRTNode(parNode).Data.Entries[me].R := NULL_RECT; // Replace the old mbr with
    // Nil rect
    parNode.Write;

    // Insert the new node into the parent

    parNewo := -1;
    ret := parNode.Insert(NULL_RECT, TRTNode(newNode).oid, parNewo);
    if ret = TREE_ERROR then
    begin
      Result := TREE_ERROR;
      exit;
    end;
    if parNewo >= 0 then
    begin
      if parNode.Locate(oid) = NOT_FOUND then
      begin
        Data.Parent := parNewO;
        Write;
      end;
      if parNode.Locate(TRTNode(newNode).oid) = NOT_FOUND then
      begin
        TRTNode(newNode).Data.Parent := parNewO;
        newNode.Write;
      end;
    end;
    if ret = ROOT_CHANGED then
      ric := ret;

    Move(Data.Entries[0], temp[0], SizeOf(TRTEntry) * BUCKET_SIZE);

    temp[BUCKET_SIZE].R := r; // The entry to be inserted is placed
    temp[BUCKET_SIZE].Child := o; // ...at the last position in the temp

    Data.FullEntries := 0; // Empty the node

    // Select the first pair of entries and move to the last 2 positions
    PickSeed(temp, BUCKET_SIZE + 1);

    s1 := BUCKET_SIZE - 1;
    s2 := BUCKET_SIZE; // Initialize the MBRs
    MBRa := temp[s1].R;
    MBRb := temp[s2].R; // of the nodes

    // Insert the first into this, and the second into the new node
       {parNewo := -1;}
    ret := Insert(MBRa, temp[s1].Child, parNewo);
    if ret = ROOT_CHANGED then
      ric := ret;
    {parNewo := -1;}     ret := newNode.Insert(MBRb, temp[s2].Child, parNewo);
    if ret = ROOT_CHANGED then
      ric := ret;

    n := BUCKET_SIZE - 1;
    m := HALF_BUCKET;

    while n <> 0 do
    begin
      if na = m then
        node := 1 // node A is full, pick B
      else if nb = m then
        node := 0 // node B is full, pick A
      else
        node := PickNext(temp, n, MBRa, MBRb); // pick the next node

      if node <> 0 then
      begin
        {parNewo := -1;}         ret := newNode.Insert(temp[n - 1].R, temp[n - 1].Child,
          parNewo);
        if ret = ROOT_CHANGED then
          ric := ret;
        MBRb := Extent_rect(MBRb, temp[n - 1].R);
        inc(nb);
      end
      else
      begin
        {parNewo := -1;}         ret := Insert(temp[n - 1].R, temp[n - 1].Child, parNewo);
        if ret = ROOT_CHANGED then
          ric := ret;
        MBRa := Extent_rect(MBRa, temp[n - 1].R);
        Inc(na);
      end;
      Dec(n);
    end;

  finally
    newNode.free;
    parNode.free;
  end;

  Result := ric;

end;

function TRTNode.IsRoot: boolean;
begin
  Result := (Data.Parent = -1);
end;

procedure TRTNode.Compact;
var
  r: TlicgExtent;
  i, me: integer;
  MyOid, Par: Integer;
begin

  if isRoot then
    Exit; // cannot compact root...

  MyOid := oid;
  Par := Data.Parent;

  r := Data.Entries[0].R;
  for i := 1 to Data.FullEntries - 1 do
    r := Extent_rect(r, Data.Entries[i].R);

  Read(Par);

  me := Locate(MyOid);
  if me = NOT_FOUND then
    LicadGISError('Compact');

  Data.Entries[me].R := r;
  Write;

  Compact;
end;

// Delete r from this. Make all arrangements like condensing the tree
// Return Root if it changes.

function TRTNode.Delete(o: Integer; l: integer; rl: TOList): integer;
var
  n, i: integer;
  child, parent: TRTNode;
  choid: Integer;
  retCode: integer;
begin
  parent := rt.CreateNewNode;
  try

    retcode := OK;

    n := Locate(o); // Find the entry for this obj.

    if n = NOT_FOUND then
      LicadGISError('Unrecoverable error! Aborting.');

    if not Data.Leaf then
    begin
      child := rt.CreateNewNode;
      child.Read(Data.Entries[n].Child);
      child.DeleteNodeFromFile; // Destroy the deleted node
      child.free;
    end;

    // Replace the entry with the last entry and decrement Full Entries
    Dec(Data.FullEntries);
    Data.Entries[n] := Data.Entries[Data.FullEntries];
    Write;

    if Data.FullEntries < LOWER_BOUND then // Condense
    begin
      if isRoot then // if this is the root
      begin
        if Data.FullEntries <> 1 then
        begin
          Result := OK;
          exit;
        end;
        if not Data.Leaf then
        begin
          choid := Data.Entries[0].Child;
          child := rt.CreateNewNode;
          child.Read(choid);
          TRTNode(child).Data.Parent := -1;
          child.Write;
          child.free;
          Result := ROOT_CHANGED;
          exit;
        end;
        Result := OK;
        exit;
      end;

      for i := 0 to Data.FullEntries - 1 do // copy rest of the entries into temp
        rl.Insertl(Data.Entries[i].Child, Data.Entries[i].R, l);

      parent.Read(Data.Parent);
      retCode := parent.Delete(oid, l + 1, rl); // Delete this node from parent
    end
    else
      Compact;

    Result := retCode;
  finally
    parent.free;
  end;
end;

// Return the index of the entry in this TRTNode which Geometry.Points to o.

function TRTNode.Locate(o: Integer): integer;
var
  i: integer;
begin
  Result := NOT_FOUND;
  for i := 0 to Data.FullEntries - 1 do
    if Data.Entries[i].Child = o then
    begin
      Result := i;
      exit;
    end;
end;

function TRTNode.findLeaf(const r: TlicgExtent; o: Integer): integer;
var
  i: integer;
  me: Integer;
begin
  if Data.Leaf then
  begin
    if Locate(o) = NOT_FOUND then
    begin
      Result := TREE_ERROR;
      exit;
    end
    else
    begin
      Result := OK;
      exit;
    end; // it is here!
  end
  else
    for i := 0 to Data.FullEntries - 1 do
      if Contains_rect(Data.Entries[i].R, r) then
      begin
        me := oid;
        Read(Data.Entries[i].Child);
        if findLeaf(r, o) = OK then
        begin
          Result := OK;
          exit;
        end;
        Read(me);
      end;

  Result := TREE_ERROR;
end;

function TRTNode.isLeaf: boolean;
begin
  Result := Data.Leaf;
end;

procedure TRTNode.AddNodeToFile;
begin
//  inherited;

end;

procedure TRTNode.DeleteNodeFromFile;
begin
//  inherited;

end;

procedure TRTNode.Read(NId: Integer);
begin
//  inherited;

end;

procedure TRTNode.Write;
begin
//  inherited;

end;

end.


