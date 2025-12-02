unit Lider.CG.ModulesCom.Table.Cells;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.Cell,
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.CellDataInt,
  System.Generics.Collections,
  SysUtils,
  Generics.Defaults,
  Vcl.Forms,
  Windows;
//  Windows;

//var
//  _TotalMergeAccrossandListCount: Integer = 0; //=0

type
  Cells = class
  private
    _TotalMergeAccrossandListCount: Integer;
    _innerList: TList<CellInt>;
    function GetContainsListCollection: Boolean;
    property ContainsListCollection: Boolean read GetContainsListCollection;

    function GetList: TList<CellInt>;
  public
    function GetInnerList: TList<CellInt>;
    property InnerList: TList<CellInt> read GetInnerList;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetFullCount: Integer;
    property FullCount: Integer read GetFullCount;

    function GetIsReadOnly: Boolean;
    property IsReadOnly: Boolean read GetIsReadOnly;

    procedure Add(item: CellInt);
    procedure Clear;
    function Contains(item: CellInt): Boolean;
    procedure CopyTo(Arrays: CellInt; arrayIndex: Integer);
    function Remove(item: CellInt): Boolean;
    function GetEnumerator: TEnumerator<CellInt>;
    function GetEnumerator1: TEnumerator<CellInt>;
    function GetCultists: TList<CellInt>;
    constructor Create;
  end;

implementation
{ Cells }

procedure Cells.Clear;
begin
  Self.InnerList.Clear;
end;

function Cells.Contains(item: CellInt): Boolean;
var
  findIndex: Integer;
  Comparer: IComparer<CellInt>;
begin
  findIndex := -1;
  Comparer := TDelegatedComparer<CellInt>.Construct(
    function (const L, R: CellInt): Integer
    begin
      result := CompareText(L.FulType, R.FulType);
    end
  );
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
  result := Self.InnerList.BinarySearch(item, findIndex, Comparer);
end;

procedure Cells.CopyTo(Arrays: CellInt; arrayIndex: Integer);
var
  tempList: TList<CellInt>;
begin
  tempList := TList<CellInt>.create;
  tempList.AddRange(Self.InnerList);
  tempList.InsertRange(arrayIndex,arrays);// CopyTo(array, arrayIndex)
end;

constructor Cells.Create;
begin
  inherited Create;
  _innerList := TList<CellInt>.create;
end;

function Cells.GetContainsListCollection: Boolean;
begin
  result := True;
end;

function Cells.GetCount: Integer;
begin
  result := Self.InnerList.Count;
end;

function Cells.GetCultists: TList<CellInt>;
begin
  result := Self.InnerList.Create;
end;

function Cells.GetEnumerator: TEnumerator<CellInt>; //IEnumerator(Of CellInt)
begin
  result := Self.InnerList.Create.GetEnumerator;
end;

function Cells.GetEnumerator1: TEnumerator<CellInt>; //IEnumerator
begin
  result := Self.GetEnumerator;
end;

function Cells.GetFullCount: Integer;
begin
  if _innerList.Count > 0 then
    _TotalMergeAccrossandListCount := _innerList[_innerList.Count - 1].Indexs
                                    + _innerList[_innerList.Count - 1].MergeAcross;
  result := _TotalMergeAccrossandListCount;
end;

function Cells.GetInnerList: TList<CellInt>;
begin
  result := Self._innerList;
end;

function Cells.GetIsReadOnly: Boolean;
begin
  result := False;
end;

function Cells.GetList: TList<CellInt>;
begin
  result := Self.InnerList;
end;

function Cells.Remove(item: CellInt): Boolean;
begin
  try
    raise Exception.Create('Herhangibir Cel''i kaldýramazsýnýz.');
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
  result := False;
end;

procedure Cells.Add(item: CellInt);// Implements System.Collections.Generic.ICollection(Of CellInt).Add
var
  InnerCel: CellInt;
begin
  try
    if item = nil then raise Exception.Create('Item cannot be nothing.');
    _TotalMergeAccrossandListCount := 0;
    for InnerCel In _innerList do
    begin
        if InnerCel.Indexs <> 0 then
            _TotalMergeAccrossandListCount := InnerCel.Indexs + InnerCel.MergeAcross + 1
        else
            _TotalMergeAccrossandListCount := _TotalMergeAccrossandListCount + InnerCel.MergeAcross;
    end;
    if _TotalMergeAccrossandListCount = 0 then _TotalMergeAccrossandListCount := 1;

    if item.Indexs > 0 then
        _TotalMergeAccrossandListCount := item.Indexs;

    // _TotalMergeAccrossandListCount += _innerList.Count + 1
    item.Indexs := _TotalMergeAccrossandListCount;
  //  Dim p1 As System.Reflection.FieldInfo = item.GetType.GetField( _
  //          "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
  //  p1.SetValue(item, _TotalMergeAccrossandListCount)

    Self.InnerList.Add(item);

    _TotalMergeAccrossandListCount := _TotalMergeAccrossandListCount + item.MergeAcross;
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
End;

end.

