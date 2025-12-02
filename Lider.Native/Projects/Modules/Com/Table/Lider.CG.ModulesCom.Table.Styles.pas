unit Lider.CG.ModulesCom.Table.Styles;

interface

uses
  Lider.CG.ModulesCom.Table.StyleInt,
  Lider.CG.ModulesCom.Table.Style,
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.Cells,
  Lider.CG.ModulesCom.Table.Cell,
  System.Generics.Collections,
  SysUtils,
  Generics.Defaults,
  Vcl.Forms,
  Windows;

type
  Styles = class
    private
      _innerList: TList<StyleInt>;

      function GetContainsListCollection: Boolean;
      property ContainsListCollection: Boolean read GetContainsListCollection;

      function GetList: TList<StyleInt>;
    public
      function GetInnerList: TList<StyleInt>;
      property InnerList: TList<StyleInt> read GetInnerList;

      function GetCount: Integer;
      property Count: Integer read GetCount;

      function GetIsReadOnly: Boolean;
      property IsReadOnly: Boolean read GetIsReadOnly;

      procedure Add(item: StyleInt);
      procedure Clear;
      function Contains(item: StyleInt): Boolean;
      procedure CopyTo(Arrays: StyleInt; arrayIndex: Integer);
      function Remove(item: StyleInt): Boolean;
      function GetEnumerator: TEnumerator<StyleInt>;
      function GetEnumerator1: TEnumerator<StyleInt>;
      function GetCultists: TList<StyleInt>;

      function GetElektronikTablo2003String: String;

      function GetGetStyleByID(ID: String): Style;
      property GetStyleByID[ID: String]: Style read GetGetStyleByID;

      constructor Create;
  end;

const
  vbLf = #$A;

implementation
{ Styles }

procedure Styles.Add(item: StyleInt);
var
  varmi: Boolean;
  findIndex, count: Integer;
  Comparer: IComparer<StyleInt>;
begin
  try
    if (item = nil) then raise Exception.Create('Item cannot be nothing.');
      //Throw New ArgumentNullException("Item cannot be nothing.")

    findIndex := -1;
    Comparer := TDelegatedComparer<StyleInt>.Construct(
    function (const L, R: StyleInt): Integer
    begin
      result := CompareText(L.Id, R.Id);
    end
    );

    //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
//    varmi := (findIndex = 1);

    if Self.InnerList.BinarySearch(item, findIndex, Comparer) then
      Exit;

    Self.InnerList.Add(item);

    count := Self.InnerList.Count;
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
end;

procedure Styles.Clear;
begin
  Self.InnerList.Clear;
end;

function Styles.Contains(item: StyleInt): Boolean;
var
  findIndex: Integer;
  Comparer: IComparer<StyleInt>;
begin
  findIndex := -1;
  Comparer := TDelegatedComparer<StyleInt>.Construct(
    function (const L, R: StyleInt): Integer
    begin
      result := CompareText(L.GetElektronikTablo2003String, R.GetElektronikTablo2003String);
    end
  );
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
  result := Self.InnerList.BinarySearch(item, findIndex, Comparer);
end;

procedure Styles.CopyTo(Arrays: StyleInt; arrayIndex: Integer);
var
  tempList: TList<StyleInt>;
begin
  tempList := TList<StyleInt>.create;
  tempList.AddRange(Self.InnerList);
  tempList.InsertRange(arrayIndex,arrays);// CopyTo(array, arrayIndex)
end;

constructor Styles.Create;
begin
  inherited Create;
  _innerList := TList<StyleInt>.create;
  _innerList.Add(Style.create);
end;

function Styles.GetContainsListCollection: Boolean;
begin
  result := True;
end;

function Styles.GetCount: Integer;
begin
  result := Self.InnerList.Count;
end;

function Styles.GetCultists: TList<StyleInt>;
begin
  result := Self.InnerList.Create;
end;

function Styles.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
  Style1: StyleInt;
begin
  strExcelXml := TStringBuilder.Create;
  try
    if Self.Count > 0 then
    begin
        strExcelXml.Append('<Styles>').ToString;
        strExcelXml.Append(vblf); //vbLf
        For Style1 In Self do
        begin
            strExcelXml.Append(Style1.GetElektronikTablo2003String);
            strExcelXml.Append(vblf); //vbLf
        end;
        strExcelXml.Append('</Styles>').ToString;
    end
    Else
      strExcelXml.Clear;
  except on e: exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function Styles.GetEnumerator: TEnumerator<StyleInt>;
begin
  result := Self.InnerList.Create.GetEnumerator;
end;

function Styles.GetEnumerator1: TEnumerator<StyleInt>;
begin
  result := Self.GetEnumerator;
end;

function Styles.GetGetStyleByID(ID: String): Style;
var
  rv: Style;
  a: StyleInt;
begin
  rv := nil;
  For a In Self.InnerList do
  begin
    if a.Id = ID then
    begin
      rv := a as Style;//rv.Create(a.Id,a.Name,a.Aligment,a.Borders,a.Font,a.Interior,a.NumberFormat,a.Protection);
      Break;
    end;
  end;
  result := rv;
end;

function Styles.GetInnerList: TList<StyleInt>;
begin
  result := Self._innerList;
end;

function Styles.GetIsReadOnly: Boolean;
begin
  result := False;
end;

function Styles.GetList: TList<StyleInt>;
begin
  result := Self.InnerList;
end;

function Styles.Remove(item: StyleInt): Boolean;
var
  findIndex: Integer;
  Comparer: IComparer<StyleInt>;
  varmi: Boolean;
begin
  result := False;
  findIndex := -1;
  Comparer := TDelegatedComparer<StyleInt>.Construct(
  function (const L, R: StyleInt): Integer
  begin
    result := CompareText(L.GetElektronikTablo2003String, R.GetElektronikTablo2003String);
  end
  );
//  Self.InnerList.BinarySearch(item, findIndex, Comparer);
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
//  varmi := (findIndex = 1);

  if Self.InnerList.BinarySearch(item, findIndex, Comparer) then
  begin
    try
      Self.InnerList.Delete(findIndex);
      result := True;
    except

    end;
  end;
end;



end.

