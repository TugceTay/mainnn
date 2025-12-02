unit Lider.CG.ModulesCom.Table.BaseNamedCellStyle;

//{$I Lider.CG.Com.Component.inc}

interface

uses
  Lider.CG.ModulesCom.Table.NamedCellStyleInt;
//  Windows;

type
  BaseNamedCellStyle = class(TInterfacedObject, NamedCellStyleInt)
  private
  public
    function GetName: String; virtual; abstract;
    property Name: String read GetName;

    function ToString: String; override;
  end;

implementation
{ BaseNamedCellStyle }

//function BaseNamedCellStyle.GetName: String;
//begin
//  result := Self.Name;
//end;

function BaseNamedCellStyle.ToString: String;
begin
  result := Self.Name;
end;

end.

