unit Lider.CG.ModulesCom.Table.Borders;

//{$I Lider.CG.Com.Component.inc}

interface

uses
//  Lider.CG.ModulesCom.Table.Cell,
  Lider.CG.ModulesCom.Table.BaseBorder,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.BorderInt,
  System.Generics.Collections,
  Generics.Defaults,
  Lider.CG.Com.EntityInt,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, generics.collections, Vcl.StdCtrls;

type
  Borders = class
  private
    _innerList: TList<BorderInt>;
    function GetContainsListCollection: Boolean;
    property ContainsListCollection: Boolean read GetContainsListCollection;

    function GetList: TList<BorderInt>;
  public
    function Get(Index: Integer): BorderInt;

    function GetInnerList: TList<BorderInt>;
    property InnerList: TList<BorderInt> read GetInnerList;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetIsReadOnly: Boolean;
    property IsReadOnly: Boolean read GetIsReadOnly;

    procedure Add(item: BorderInt);
    procedure Clear;
    function Contains(item: BorderInt): Boolean;
    procedure CopyTo(Arrays: BorderInt; arrayIndex: Integer);
    function Remove(item: BorderInt): Boolean;
    function GetEnumerator: TEnumerator<BorderInt>;
    function GetEnumerator1: TEnumerator<BorderInt>;
    function GetCultists: TList<BorderInt>;

    function GetElektronikTablo2003String: String;

    constructor Create;
    destructor Destroy;
  end;

const
  vbLf = #$A;

implementation
{ Borders }

constructor Borders.Create;
begin
   inherited Create;
  _innerList := TList<BorderInt>.Create;
end;

function Borders.Get(Index: Integer): BorderInt;
begin
  Result := nil;
  if (Index < 0) or (Index > _innerList.Count - 1) then
    Exit;
  Result := _innerList[Index] as BorderInt;
end;

procedure Borders.Clear;
begin
  Self.InnerList.Clear;
end;

function Borders.Contains(item: BorderInt): Boolean;
//var
//  findIndex: Integer;
//  Comparer: IComparer<BorderInt>;
begin
//  findIndex := -1;
//  Comparer := TDelegatedComparer<BorderInt>.Construct(
//    function (const L, R: BorderInt): Integer
//    begin
//      result := CompareText(L.GetElektronikTablo2003String, R.GetElektronikTablo2003String);
//    end
//  );
  //Where(Function(cultist) cultist.FulType = item.FulType).FirstOrDefault = nil)
  result := Self.InnerList.IndexOf(item) > -1;//BinarySearch(item, findIndex, Comparer);
end;

procedure Borders.CopyTo(Arrays: BorderInt; arrayIndex: Integer);
var
  tempList: TList<BorderInt>;
begin
  tempList := TList<BorderInt>.create;
  tempList.AddRange(Self.InnerList);
  tempList.InsertRange(arrayIndex,arrays);// CopyTo(array, arrayIndex)
end;

destructor Borders.Destroy;
begin
  FreeAndNil(_innerList);
  inherited Destroy;
end;

function Borders.GetContainsListCollection: Boolean;
begin
  result := True;
end;

function Borders.GetCount: Integer;
begin
  result := Self.InnerList.Count;
end;

function Borders.GetCultists: TList<BorderInt>;
begin
  result := Self.InnerList.Create;
end;

function Borders.GetElektronikTablo2003String: String;
var
  strExcelXml: TStringBuilder;
  aa: BorderInt;
  I: Integer;
begin
  strExcelXml := TStringBuilder.Create;
  try
      if Self.Count > 0 then
          strExcelXml.Append('<Borders>' + vbLf) //vbLf
      else
          strExcelXml.Append('<Borders');

      for aa In Self do
      begin
          strExcelXml.Append(aa.GetElektronikTablo2003String);
          strExcelXml.Append(vbLf); //vbLf
      end;

      if Self.Count > 0 then
          strExcelXml.Append('</Borders>')
      else
          strExcelXml.Append('/>');

  except
      strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function Borders.GetEnumerator: TEnumerator<BorderInt>; //IEnumerator(Of BorderInt)
begin
  result := Self.InnerList.Create.GetEnumerator;
end;

function Borders.GetEnumerator1: TEnumerator<BorderInt>; //IEnumerator
begin
  result := Self.GetEnumerator;
end;

function Borders.GetInnerList: TList<BorderInt>;
begin
  result := Self._innerList;
end;

function Borders.GetIsReadOnly: Boolean;
begin
  result := False;
end;

function Borders.GetList: TList<BorderInt>;
begin
  result := Self.InnerList;
end;

function Borders.Remove(item: BorderInt): Boolean;
begin
  try
    raise Exception.Create('Herhangibir BorderInt''i kaldýramazsýnýz.');
  except on e: Exception do
    Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
  end;
  result := False;
end;

procedure Borders.Add(item: BorderInt);// Implements System.Collections.Generic.ICollection(Of BorderInt).Add
var
  InnerCel: BorderInt;
  count: Integer;
begin
  if item = nil then raise Exception.Create('Item cannot be nothing.');

  Self.InnerList.Add(item);
  count := Self.InnerList.Count;
End;

end.

