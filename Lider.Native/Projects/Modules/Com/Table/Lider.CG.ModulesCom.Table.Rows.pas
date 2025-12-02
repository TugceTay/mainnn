unit Lider.CG.ModulesCom.Table.Rows;

interface

uses
  Lider.CG.ModulesCom.Table.Row,
  Lider.CG.ModulesCom.Table.RowInt,
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.Cells,
  Lider.CG.ModulesCom.Table.Cell,
  System.Generics.Collections,
  SysUtils,
  Generics.Defaults,
  Vcl.Forms,
  Windows,
  Math;

type
  Rows = class
    private
      _innerList: TList<RowInt>;

      function GetContainsListCollection: Boolean;
      property ContainsListCollection: Boolean read GetContainsListCollection;

      function GetList: TList<RowInt>;
    public
      function GetInnerList: TList<RowInt>;
      property InnerList: TList<RowInt> read GetInnerList;

      function GetCount: Integer;
      property Count: Integer read GetCount;

      function GetIsReadOnly: Boolean;
      property IsReadOnly: Boolean read GetIsReadOnly;

      procedure Add(item: RowInt);
      procedure Clear;
      function Contains(item: RowInt): Boolean;
      procedure CopyTo(Arrays: RowInt; arrayIndex: Integer);
      function Remove(item: RowInt): Boolean;
      function GetEnumerator: TEnumerator<RowInt>;
      function GetEnumerator1: TEnumerator<RowInt>;
      function GetCells: Tlist<RowInt>;
      constructor Create;
  end;

implementation
{ Cols }

var
  InnerOneCells: Cells;

procedure Rows.Add(item: RowInt);
var
  count, i, f, Otele, sf, ii, ih: Integer;
//  InnerOneCells,
  TempInnerOneCells: Cells;
  Incel, c, CurrentCel, Upcel: CellInt;
begin
  try
    if (item = nil) then raise Exception.Create('Item cannot be nothing.');
      //Throw New ArgumentNullException("Item cannot be nothing.")
//    InnerOneCells := Cells.Create;
    if InnerOneCells.Count <= 0 then
    begin
      for Incel In item.Cell do
      begin
//          Dim c As New Cell.Cell
          c := Cell.Create;
//          Dim p1 As System.Reflection.FieldInfo = c.GetType.GetField( _
//                  "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//          p1.SetValue(c, Incel.Index)
          c.Indexs := Incel.Indexs;
          c.MergeAcross := Incel.MergeAcross;
          c.MergeDown := Incel.MergeDown;
          InnerOneCells.Add(c);
      end;

      i := IfThen(_innerList.Count = 0, 0, _innerList.Count + 1);
      item.Indexs := i;
//      Dim p2 As System.Reflection.FieldInfo = item.GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//      p2.SetValue(item, i)
      Self.InnerList.Add(item);
      Exit;
    end;


//  '' OTELEME ISLEMI
    f := 0;
    for CurrentCel In item.Cell do //As Cell.Cell
    begin
        for Upcel In InnerOneCells do //As Cell.Cell
        begin
            Otele := 0;
            if (CurrentCel.Indexs >= Upcel.Indexs) And
               (CurrentCel.Indexs <= Upcel.Indexs + Upcel.MergeAcross) then
            begin
                if Upcel.MergeDown > 0 then
                begin
                    Otele := Upcel.MergeAcross + 1;
                    for ii := f to item.Cell.Count - 1 do
                    begin
                      item.Cell.InnerList[ii].Indexs := item.Cell.InnerList[ii].Indexs + Otele;
//                        Dim p2 As System.Reflection.FieldInfo = item.Cell(ii).GetType.GetField( _
//                        "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                        p2.SetValue(item.Cell(ii), item.Cell(ii).Index + Otele)
                    end;
                end;
            end;
        end;
        f := f + 1;
    end;




//  ' inner cel hücreleri düzenleniyor
    TempInnerOneCells := Cells.Create;


    sf := 0;
    for Upcel In InnerOneCells do
    begin
        if Upcel.MergeDown <= 0 then
        begin
            for ii := sf To item.Cell.Count - 1 do
            begin
                if Upcel.Indexs = item.Cell.InnerList[ii].Indexs then //.Cell
                begin
                    c := Cell.Create;
//                    Dim p1 As System.Reflection.FieldInfo = c.GetType.GetField("_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                    p1.SetValue(c, item.Cell(ii).Index)
                    c.Indexs := item.Cell.InnerList[ii].Indexs;
                    c.MergeAcross := item.Cell.InnerList[ii].MergeAcross;
                    c.MergeDown := item.Cell.InnerList[ii].MergeDown;
                    TempInnerOneCells.Add(c);
                    sf := sf + 1;
                end
                else if item.Cell.InnerList[ii].Indexs < Upcel.Indexs + Upcel.MergeAcross then
                begin
                    c := Cell.Create;
//                    Dim p1 As System.Reflection.FieldInfo = c.GetType.GetField("_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                    p1.SetValue(c, item.Cell(ii).Index)
                    c.Indexs := item.Cell.InnerList[ii].Indexs;
                    c.MergeAcross := item.Cell.InnerList[ii].MergeAcross;
                    c.MergeDown := item.Cell.InnerList[ii].MergeDown;
                    TempInnerOneCells.Add(c);
                    sf := sf + 1;
                end
                Else
                begin
                    sf := ii;
                    Break;//Exit For
                end;
            end;
            if item.Cell.Count = 0 then
            begin
                If Upcel.MergeDown > 0 then Upcel.MergeDown := Upcel.MergeDown - 1;
                TempInnerOneCells.Add(Upcel);
            end;
        end
        else
        begin
            if Upcel.MergeDown > 0 then Upcel.MergeDown := Upcel.MergeDown - 1;
            TempInnerOneCells.Add(Upcel);
        end;
    end;



  //  'yeni hücrelerden index numaralarý büyük olanlar sona ekleniyor
    for CurrentCel In item.Cell do
    begin
        if CurrentCel.MergeDown > 0 then
        begin
            if CurrentCel.Indexs > InnerOneCells.FullCount then
            begin
                c := Cell.Create;
//                Dim p1 As System.Reflection.FieldInfo = c.GetType.GetField("_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                p1.SetValue(c, CurrentCel.Index)
                c.Indexs := CurrentCel.Indexs;
                c.MergeAcross := CurrentCel.MergeAcross;
                c.MergeDown := CurrentCel.MergeDown;
                TempInnerOneCells.Add(c);
            end;
        end;
    end;

    InnerOneCells := TempInnerOneCells;

    if(_innerList.Count = 0) then
      ih := 0
    else
      ih := _innerList.Count + 1;
//    ih := If(_innerList.Count = 0, 0, _innerList.Count + 1)
//    Dim p3 As System.Reflection.FieldInfo = item.GetType.GetField( _
//            "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//    p3.SetValue(item, ih)
    item.Indexs := ih;
    Self.InnerList.Add(item);
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
end;

procedure Rows.Clear;
begin
  Self.InnerList.Clear;
end;

function Rows.Contains(item: RowInt): Boolean;
var
  findIndex: Integer;
  Comparer: IComparer<RowInt>;
begin
  findIndex := -1;
  Comparer := TDelegatedComparer<RowInt>.Construct(
    function (const L, R: RowInt): Integer
    begin
      result := CompareText(L.FulType, R.FulType);
    end
  );
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
  result := Self.InnerList.BinarySearch(item, findIndex, Comparer);
end;

procedure Rows.CopyTo(Arrays: RowInt; arrayIndex: Integer);
var
  tempList: TList<RowInt>;
begin
  tempList := TList<RowInt>.create;
  tempList.AddRange(Self.InnerList);
  tempList.InsertRange(arrayIndex,arrays);// CopyTo(array, arrayIndex)
end;

constructor Rows.Create;
begin
  inherited Create;
  _innerList := TList<RowInt>.create;
end;

function Rows.GetCells: Tlist<RowInt>;
begin
  result := Self.InnerList.Create;
end;

function Rows.GetContainsListCollection: Boolean;
begin
  result := True;
end;

function Rows.GetCount: Integer;
begin
  result := Self.InnerList.Count;
end;

function Rows.GetEnumerator: TEnumerator<RowInt>;
begin
  result := Self.InnerList.Create.GetEnumerator;
end;

function Rows.GetEnumerator1: TEnumerator<RowInt>;
begin
  result := Self.GetEnumerator;
end;

function Rows.GetInnerList: TList<RowInt>;
begin
  result := Self._innerList;
end;

function Rows.GetIsReadOnly: Boolean;
begin
  result := False;
end;

function Rows.GetList: TList<RowInt>;
begin
  result := Self.InnerList;
end;

function Rows.Remove(item: RowInt): Boolean;
begin
  try
    raise Exception.Create('Herhangibir Row''u kaldýramazsýnýz.');
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
  result := False;
end;

initialization
  InnerOneCells := Cells.Create;
finalization
  FreeAndNil(InnerOneCells);

end.

