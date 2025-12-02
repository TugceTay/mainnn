unit Lider.CG.ModulesCom.Table.Columns;

interface

uses
  Lider.CG.ModulesCom.Table.Column,
  Lider.CG.ModulesCom.Table.ColInt,
  System.Generics.Collections,
  SysUtils,
  Generics.Defaults,
  Vcl.Forms,
  Windows,
  Math;

type
  Cols = class
    private
      _innerList: TList<ColInt>;
      function GetContainsListCollection: Boolean;
      property ContainsListCollection: Boolean read GetContainsListCollection;

      function GetList: TList<ColInt>;
    public
      function GetInnerList: TList<ColInt>;
      property InnerList: TList<ColInt> read GetInnerList;

      function GetCount: Integer;
      property Count: Integer read GetCount;

      function GetIsReadOnly: Boolean;
      property IsReadOnly: Boolean read GetIsReadOnly;

      procedure Add(item: ColInt);
      procedure Clear;
      function Contains(item: ColInt): Boolean;
      procedure CopyTo(Arrays: ColInt; arrayIndex: Integer);
      function Remove(item: ColInt): Boolean;
      function GetEnumerator: TEnumerator<ColInt>;
      function GetEnumerator1: TEnumerator<ColInt>;
      constructor Create;
  end;

implementation
{ Cols }

procedure Cols.Add(item: ColInt);
var
  count, i: Integer;
begin
  try
    if (item = nil) then raise Exception.Create('Item cannot be nothing.');
      //Throw New ArgumentNullException("Item cannot be nothing.")

    if item.Indexs < 1 then
    begin
      if _innerList.Count = 0 then
        i := 1
      else
        i := _innerList[(_innerList.Count - 1)].Indexs + 1 + _innerList[(_innerList.Count - 1)].Span;
//        i := IfThen(_innerList.Count = 0, 1, _innerList[(_innerList.Count - 1)].Indexs + 1 + _innerList[(_innerList.Count - 1)].Span);
        item.Indexs := i;
//        Dim p1 As System.Reflection.FieldInfo = item.GetType.GetField("_Indexs",
//         Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//        p1.SetValue(item, i)
    end;


  //  ' Add the new recruit to the cult!
    Self.InnerList.Add(item);

  //  ' Check how many members the cult currently has and
  //  ' set the new recruits rank accordingly.
    count := Self.InnerList.Count;
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
end;

procedure Cols.Clear;
begin
  Self.InnerList.Clear;
end;

function Cols.Contains(item: ColInt): Boolean;
var
  findIndex: Integer;
  Comparer: IComparer<ColInt>;
begin
  findIndex := -1;
  Comparer := TDelegatedComparer<ColInt>.Construct(
    function (const L, R: ColInt): Integer
    begin
      result := CompareText(L.FulType, R.FulType);
    end
  );
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
  result := Self.InnerList.BinarySearch(item, findIndex, Comparer);
end;

procedure Cols.CopyTo(Arrays: ColInt; arrayIndex: Integer);
var
  tempList: TList<ColInt>;
begin
  tempList := TList<ColInt>.create;
  tempList.AddRange(Self.InnerList);
  tempList.InsertRange(arrayIndex,arrays);// CopyTo(array, arrayIndex)
end;

constructor Cols.Create;
begin
  inherited Create;
  _innerList := TList<ColInt>.create;
end;

function Cols.GetContainsListCollection: Boolean;
begin
  result := True;
end;

function Cols.GetCount: Integer;
begin
  result := Self.InnerList.Count;
end;

function Cols.GetEnumerator: TEnumerator<ColInt>;
begin
  result := Self.InnerList.Create.GetEnumerator;
end;

function Cols.GetEnumerator1: TEnumerator<ColInt>;
begin
  result := Self.GetEnumerator;
end;

function Cols.GetInnerList: TList<ColInt>;
begin
  result := Self._innerList;
end;

function Cols.GetIsReadOnly: Boolean;
begin
  result := False;
end;

function Cols.GetList: TList<ColInt>;
begin
  result := Self.InnerList;
end;

function Cols.Remove(item: ColInt): Boolean;
begin
  try
    raise Exception.Create('Herhangibir Row''u kaldýramazsýnýz.');
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
  result := False;
end;



end.

