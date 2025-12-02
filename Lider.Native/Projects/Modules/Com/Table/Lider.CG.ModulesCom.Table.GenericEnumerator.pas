unit Lider.CG.ModulesCom.Table.GenericEnumerator;

interface

uses
  Lider.CG.ModulesCom.Table.Styles,
  Lider.CG.ModulesCom.Table.TableInt;

type
  GenericEnumerator<T> = class(TInterfacedObject, IEnumerable<T>, IEnumerable)
    public
    { IEnumerable<T> }
    function GetEnumeratorGeneric: IEnumerator<T>;
    function IEnumerable<T>.GetEnumerator = GetEnumeratorGeneric;

    { IEnumerable }
    function GetEnumerator: IEnumerator;
    function IEnumerable.GetEnumerator = GetEnumerator;
  end;

implementation
end.

