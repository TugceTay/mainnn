unit Lider.CG.LiWork.StringOperations;

interface

uses
  Math,
  SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  Lider.CG.Com.Math,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.System,
  Lider.CG.Com.GeoLibrary,
  lxStrUtil,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.GeoTypes;

 function AssingingCoordinateToText(ARefP: TLicgCoor; AEnt: ILicgEntity;
  AList: TList<Integer>; AListN:ILicgEntityList; ARowSpace: Double): IlicgEntityList;

 function SetTextSeparate(S: String; AVal: Integer): TList<Integer>;

 function CreateTextEntityList(AList: TList<Integer>;
  AText:IlicgEntity; AVal: Integer): ILicgEntityList;

implementation

function SetTextSeparate(S: String; AVal: Integer): TList<Integer>;
var
  I, J, K: Integer;
  Text: String;
  SArray: TArray<string>;
  Bolum, Kalan: Integer;
  Wx, Wy: Double;
begin

  Result := TList<Integer>.Create;
  SArray := S.Split([' ']); //[] Ýçine sözcüklerin ayrýlacaðý yerin belirtilmesi saðlanýr.
//  AVal := fmYaziAyarlariSecenekleri.seParcaAdet.Value;
  if AVal = 0 then
    AVal := 1
  Else if AVal > Length(SArray) then
   AVal := Length(SArray);
  Bolum := Length(SArray) div AVal;
  Kalan := Length(SArray) mod AVal;
  for I := 0 to AVal - 1 do
  begin
    Result.Add(0);
    for J := 0 to Bolum - 1 do
    begin
        Result[I] := Result[I] + 1;
    end;
  end;
  J := 0;
  for I := 0 to Kalan - 1 do
  begin
    Result[J] := Result[J] + 1;
    Inc(J);
  end;
end;

function AssingingCoordinateToText(ARefP: TLicgCoor;
  AEnt: ILicgEntity; AList: TList<Integer>;
  AListN: ILicgEntityList; ARowSpace: Double): IlicgEntityList;
var
  I, R, HCount: Integer;
  P: TLicgCoor;
  H, Dist: Double;
begin
  H := AEnt.DrawTools.FontTool.Height;
  R := AlistN.Count div 2;
  if (AListN.Count Mod 2) = 0 then
  begin
    Dist := (ARowSpace / 2 + H/2 + ARowSpace*(R-1) + H*(R-1));
    P := Polar(ARefP, AEnt.DrawTools.FontTool.Angle , Dist);
    for I := 0 to AListN.Count - 1 do
    begin
      AListN[I].Geometry.Points[0] := P;
      P.Y := Polar(P, AEnt.DrawTools.FontTool.Angle ,
        -(H + ARowSpace)*cos(AEnt.DrawTools.FontTool.Angle) ).Y;
    end;
  end
  else
  begin
    Dist := (R * ARowSpace + H * R);
    P := Polar(ARefP, AEnt.DrawTools.FontTool.Angle , Dist);
    for I := 0 to AListN.Count - 1 do
    begin
      AListN[I].Geometry.Points[0] := P;
      P.Y := Polar(P, AEnt.DrawTools.FontTool.Angle , -(H + ARowSpace)).Y;
    end;
  end;
  Result := AListN;
end;

function CreateTextEntityList(AList: TList<Integer>;
  AText: IlicgEntity; AVal: Integer): ILicgEntityList;
var
  K, I, J: Integer;
  S,TextNew: String;
  SArray: TArray<string>;
  TextEnt: ILicgEntity;
begin
  K := 0;
  Result := TLicgEntityList.Create;   //**
  if  AText.EntityID = idVectorialText then
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D)
  else
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  S := AText.AsTextValue.Text;
  SArray := S.Split([' ']);  //S ye atanan metni parçalayarak listeler
  AList := SetTextSeparate(S, Aval);
  for I := 0 to AList.Count-1 do
  begin
    for J := 0 to AList[I] - 1 do
    begin
      if J = 0 then
        TextNew := TextNew + SArray[K]
      else
        TextNew := TextNew + ' ' + SArray[K];
      inc(K);
    end;
    TextEnt.Geometry.Points[0] := AsCoor(0, 0);// Geçici Kordinat hata verdiði için oluþturuldu.
    TextEnt.AsTextValue.Text := TextNew;
    TextEnt.DrawTools.Assign(AText.DrawTools); //Assign metodu nesnenin özelliklerinin hepsini ayný alýr.
    TextEnt.DrawTools.FontTool.TextPos := tpCenter;
    Result.Add(TextEnt.Clone);
    TextNew := '';   //eski haline döndürmek için kullanýlýr.
  end;
end;

end.
