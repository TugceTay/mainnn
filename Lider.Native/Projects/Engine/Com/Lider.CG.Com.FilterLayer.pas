unit Lider.CG.Com.FilterLayer;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ComCtrls,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.Lists,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.GIS;

function FilterLayer(AGIS: TlicgBaseGis; s: string; OnlyWholeWords: boolean; ForceL: TList = nil): IlicgIntegerList;

implementation

uses lxStrUtil;

(*
function FilterLayer(s: string; OnlyWholeWords: boolean;
  forceL: TList = nil): IlicgIntegerList;
var
  i, j: integer;
  fL: TStrings;
  _s, str: string; //li2016 string
begin    (* ilker geçici silme layergroup dan dolayý
  if pos('+', s) <> 0 then
  begin
    fL := TStringList.Create;

    _s := s;

    str := parse(_s, '+');

    while (str <> '') do
    begin
      fL.Add(trim(str));
      str := parse(_s, '+');
    end;

    if trim(_s) <> '' then
      fL.Add(trim(_s));

    if fL.Count > 0 then
    begin
      Result := TlicgIntegerList.Create;
      for i := _G.Layers.Count - 1 downto 0 do
      begin
        if (Assigned(forceL) and (forceL.IndexOf(TlicgBaseLayer(_G.Layers.Items
          [i])) < 0)) then
          continue;

        for j := 0 to fL.Count - 1 do
        begin
          if (not OnlyWholeWords) and ((pos(HarfleriBuyut(fL[j]), HarfleriBuyut(TlicgBaseLayer(_G.Layers.Items
            [i]).DisplayName)) <> 0) or (pos(HarfleriKucult(fL[j]), HarfleriKucult(TlicgBaseLayer(_G.Layers.Items
            [i]).DisplayName)) <> 0)) then
          begin

            if (not Result.IndexofValue(i) >= 0) then
              Result.Add(i);
          end
          else if OnlyWholeWords and (HarfleriBuyut(fL[j]) = HarfleriBuyut(TlicgBaseLayer(_G.Layers.Items
            [i]).DisplayName)) then
            if (not Result.IndexofValue(i) >= 0) then
              Result.Add(i);

        end;

      end;

    end;
    fL.Free;

  end
  else
  begin
    Result := TlicgIntegerList.Create;
    for i := _G.Layers.Count - 1 downto 0 do
    begin
      if (Assigned(forceL) and (forceL.IndexOf(TlicgBaseLayer(_G.Layers.Items
        [i])) < 0)) then
        continue;

      if (s = '') or (trim(s) = '*') then
      begin
        if (not Result.IndexofValue(i) >= 0) then
          Result.Add(i);
      end
      else if (pos(HarfleriBuyut(s), HarfleriBuyut(TlicgBaseLayer(_G.Layers.Items[i]).DisplayName))
        <> 0) or (pos(HarfleriKucult(s), HarfleriKucult(TlicgBaseLayer(_G.Layers.Items[i]).DisplayName))
        <> 0) then
      begin
        if (not Result.IndexofValue(i) >= 0) then
          Result.Add(i);
      end;

    end;
  end;
end;  *)

function FilterLayer(AGIS: TlicgBaseGis; s: string; OnlyWholeWords: boolean; ForceL: TList = nil): IlicgIntegerList;
var
  I, J: Integer;
  FL: TStrings;
  _s, str: string;
begin
  if pos('+', s) <> 0 then
  begin
    FL := TStringList.Create;
    _s := s;
    str := parse(_s, '+');

    while (str <> '') do
    begin
      FL.Add(trim(str));
      str := Parse(_s, '+');
    end;

    if Trim(_s) <> '' then
      FL.Add(Trim(_s));

    if FL.Count > 0 then
    begin
      Result := TlicgIntegerList.Create;
      for I := AGIS.Layers.Count - 1 downto 0 do
      begin
        if (Assigned(ForceL) and (forceL.IndexOf(AGIS.Layers.Items[I]) < 0)) then
          Continue;

        for J := 0 to FL.Count - 1 do
        begin
          if (not OnlyWholeWords) and ((Pos(HarfleriBuyut(fL[J]), HarfleriBuyut(AGIS.Layers[I].DisplayName)) <> 0) or
            (Pos(HarfleriKucult(fL[j]), HarfleriKucult(AGIS.Layers[I].DisplayName)) <> 0)) then
          begin
            if (not Result.IndexofValue(I) >= 0) then
              Result.Add(I);
          end
          else if OnlyWholeWords and (HarfleriBuyut(fL[J]) = HarfleriBuyut(AGIS.Layers[I].DisplayName)) then
          begin
            if (not Result.IndexofValue(I) >= 0) then
              Result.Add(i);
          end;
        end;
      end;
    end;
    FL.Free;
  end
  else
  begin
    Result := TlicgIntegerList.Create;
    for I := AGIS.Layers.Count - 1 downto 0 do
    begin
      if (Assigned(ForceL) and (ForceL.IndexOf(AGIS.Layers.Items[I]) < 0)) then
        Continue;

      if (s = '') or (trim(s) = '*') then
      begin
        if (not Result.IndexofValue(I) >= 0) then
          Result.Add(i);
      end
      else if (Pos(HarfleriBuyut(s), HarfleriBuyut(AGIS.Layers[I].DisplayName)) <> 0) or
        (Pos(HarfleriKucult(s), HarfleriKucult(AGIS.Layers[I].DisplayName)) <> 0) then
      begin
        if (not Result.IndexofValue(I) >= 0) then
          Result.Add(I);
      end;
    end;
  end;
end;

end.


