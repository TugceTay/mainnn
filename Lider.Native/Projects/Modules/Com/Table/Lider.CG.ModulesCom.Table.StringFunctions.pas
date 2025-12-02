unit Lider.CG.ModulesCom.Table.StringFunctions;

interface

uses
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  StrUtils,
  SysUtils,
  Math,
  Vcl.Graphics,
  System.Classes,
  Lider.CG.Com.LicadInt,
  System.RegularExpressions,
  Lider.CG.Com.DrawToolsInt;

type
 TextJustify = (
            SolOrta = 49,
            SolUst = 50,
            OrtaUst = 51,
            SagUst = 52,
            SagOrta = 53,
            SolAltDis = 54,
            OrtaAltDis = 55,
            SagAltDis = 56,
            OrtaAlt = 67,
            Solalt = 76,
            OrtaOrta = 77,
            SolOrtaDis = 80,
            SagOrtaDis = 81,
            SagAlt = 82,
            UstOrtaDis = 85);

type
  TStringFunctions = Class
    public
      Scale: Single;
      function Get_CorrInSplitString(Refcorr: TLicgCoor3d; Yazidayama: TextJustify;
                                    TextSize: Double; TotalTextCount: Integer; index: Integer): TLicgCoor3d;

      function ToSMm(Value: Single): Single;
      function ToM(Value: Single): Single;

      function Split(SourceStr: string; Delimiter: Char; ListOfStrings:TStrings): string; overload;
      function StringListFromStrings(const Strings: array of string): TStringList;
      Function Split(SourceStr: String; ExplitCapture: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; ExplitCapture: Boolean): TStringList; overload;
      Function Split(SourceStr: String; ExplitCapture: Boolean; Pattern: String): TStringList; overload;
      Function Split(SourceStr: String; Count: Integer; IsNetcadText: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; MaxSplitCount: Integer): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; Widht: Double; AFontTool: IlicgFontTool; IsNetcadText: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; Widht: Double; FontName: String; FontSize: Single; IsNetcadText: Boolean): TStringList; overload;
      Function FindinRectanleJustifCoor(Limit: TlicgExtent; VerticalIndent: Double; HorizontalIndent: Double; Just: TextJustify): TlicgCoor;
      function GetPosCad(PPos: TextJustify): TlicgTextPos;
      function IsNumeric(s: string): Boolean;
  end;

  Const TO_M: Double = 72 / 25.4;
        SatirAralikKatsayisi: Double = 0.5;
        UstAltIndent: Double = 1;
        vbLf = #$A;

  var StringFunctions: TStringFunctions;

implementation
{ StringFunctions }

{$region 'Diðer Fonksiyonlar' }
function TStringFunctions.GetPosCad(PPos: TextJustify): TlicgTextPos;
var
  PosArray: TStringList;
begin
  PosArray := TStringList.Create;
  PosArray.AddObject(IntToStr(Integer(TextJustify.Solalt)), TObject(tpLowerLeft));//("L")> Solalt
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaAlt)), TObject(tpCenterDown));//("C")> OrtaAlt
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagAlt)), TObject(tpLowerRight));//("R")> SagAlt
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolOrta)), TObject(tpCenterLeft));//("1")> SolOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaOrta)), TObject(tpCenter));//("M")> OrtaOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagOrta)), TObject(tpCenterRight));//("5")> SagOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolUst)), TObject(tpUpperLeft));//("2")> SolUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaUst)), TObject(tpCenterUp));//("3")> OrtaUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagUst)), TObject(tpUpperRight));//("4")> SagUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolOrtaDis)), TObject(tpCenterLeftOut));//("P")> SolOrtaDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagOrtaDis)), TObject(tpCenterRightOut));//("Q")> SagOrtaDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolAltDis)), TObject(tpLowerLeftOutDown));//("6")> SolAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaAltDis)), TObject(tpCenterDownOut));//("7")> OrtaAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagAltDis)), TObject(tpLowerRightOutDown));//("8")> SagAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.UstOrtaDis)), TObject(tpCenterUpOut));//("U")> UstOrtaDis
  Result := TlicgTextPos(PosArray.Objects[PosArray.IndexOf(IntToStr(Integer(PPos)))]);
end;

function TStringFunctions.IsNumeric(s: string): Boolean;
var
  iValue, iCode: Integer;
  dValue: Double;
begin
  result := False;
  try
    if TryStrToFloat(s,dValue) then
      result := True;
  except
    result := False;
  end;
{
  result := False;
  val(s, iValue, iCode);
  if iCode = 0 then
    result := True//ShowMessage('s has a number')
}
end;
{$endregion}

{$region 'Ana Ýþlemler' }

function TStringFunctions.Get_CorrInSplitString(Refcorr: TLicgCoor3d;
  Yazidayama: TextJustify; TextSize: Double; TotalTextCount,
  index: Integer): TLicgCoor3d;
var
  NewCoor: TlicgCoor3d;
  i: Integer;
  deltax, itemX: Double;
begin
  NewCoor := AsCoor(Refcorr.Y, Refcorr.X);
//  NewCoor := AsCoor(Refcorr.X, Refcorr.Y);
  NewCoor.Z := Refcorr.z;
  Case Yazidayama of
//      ' x degerine eklenecek her satýr için
      TextJustify.OrtaAlt,
      TextJustify.SagAlt,
      TextJustify.Solalt,
      TextJustify.OrtaAltDis,
      TextJustify.SagAltDis,
      TextJustify.SolAltDis:
        For i := 0 To TotalTextCount - 1 do
        begin
            If i = index Then
              Break;//Exit For
            NewCoor.x := NewCoor.x + TextSize + (TextSize * SatirAralikKatsayisi);
        end;
//          'x deðerinden eksilecek her satýr için
      TextJustify.OrtaUst,
      TextJustify.SolUst,
      TextJustify.SagUst,
      TextJustify.UstOrtaDis:
          For i := 0 To TotalTextCount - 1 do
          begin
              If i = index Then
                Break;//Exit For
              NewCoor.x := NewCoor.x - (TextSize + (TextSize * SatirAralikKatsayisi));
          end;
//          ' orta kýsýmdan ortalama alýnarak yazýlar yerleþtirilecek
      TextJustify.OrtaOrta,
      TextJustify.SolOrta,
      TextJustify.SagOrta,
      TextJustify.SagOrtaDis,
      TextJustify.SolOrtaDis:
          If TotalTextCount = 1 Then
              TotalTextCount := 1//Exit Select
          Else If TotalTextCount > 1 Then
          begin
//              Dim deltax As Double
              deltax := Abs((TextSize + (TextSize * SatirAralikKatsayisi)) * (TotalTextCount - 1)) / 2;
//              Dim itemX As Double
              itemX := Abs((TextSize + (TextSize * SatirAralikKatsayisi)) * index);
              NewCoor.x := NewCoor.x - itemX;
              NewCoor.x := NewCoor.x + deltax;
//              Exit Select
          End;
  End;
  result := AsCoor(NewCoor.Y, NewCoor.X);
end;

function TStringFunctions.ToSMm(Value: Single): Single;
begin
  try
    if Value = 0 then
    begin
      result := Value;
      Exit;
    end;
    result := ((((Value / 72) * 25.4) * Scale) / 1.5);
  except
    result := 0;
  end;
end;

function TStringFunctions.ToM(Value: Single): Single;
begin
  try
    if Value = 0 then
    begin
      result := Value;
      Exit;
    end;
    result := ((Value / TO_M) * Scale);
  except
    result := 0;
  end;
end;
{$endregion}

{$region 'Split' }
function TStringFunctions.Split(SourceStr: string; Delimiter: Char; ListOfStrings:TStrings): string;
begin
  if ListOfStrings = nil then
    ListOfStrings:= TStringList.Create;

  ListOfStrings.Clear;
  ListOfStrings.Delimiter       := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText   := SourceStr;
  Result := ListOfStrings.Text;
end;

function TStringFunctions.StringListFromStrings(const Strings: array of string): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := low(Strings) to high(Strings) do
    Result.Add(Strings[i]);
end;

/// <summary>
/// String Bilgiyi SatýrBaþlarýndan öler (vbnevline)
/// </summary>
/// <param name="SourceStr">Bölünecek String</param>
/// <param name="ExplitCapture">Satýrbaþý Karakteri Gelmesin</param>
/// <returns></returns>
/// <remarks></remarks> //Private Shared
function TStringFunctions.Split(SourceStr: String; ExplitCapture: Boolean): TStringList; // As List(Of String)
var
  Mpattern: String;
  currStr: String;
  resultList: TStringList;
begin
    If String.IsNullOrEmpty(SourceStr) Or
       String.IsNullOrWhiteSpace(SourceStr) Then
       begin
          result := nil;
          Exit;
       end;
    Mpattern := '(\r?\n)';
    resultList := TStringList.Create;// As List(Of String)
//    '  "(\r?\n)" satýr \r=CR \n=LF için satýr baþlarýndan böler Her þartta bölecektir.
    If ExplitCapture Then
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roExplicitCapture, TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList
    end
    Else
    begin
      resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roMultiLine]));
      for currStr in resultList do
      begin
        if String.IsNullOrEmpty(currStr) then
          resultList.Delete(resultList.IndexOf(currStr));
      end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList;
    end;

    result := resultList;
End;

/// <summary>
/// String Bilgiyi Ýstenilen Karakterlerden Böler
/// </summary>
/// <param name="SourceStr">Bölünecek String</param>
/// <param name="Pattern">Bölecek Krakterler (örn: ",.-")</param>
/// <param name="ExplitCapture">Bölen Karakterler Gelmesin</param>
/// <returns></returns>
/// <remarks></remarks>
///Private Shared
Function TStringFunctions.Split(SourceStr: String; Pattern: String; ExplitCapture: Boolean): TStringList;
var
  Mpattern, currStr: String;
  resultList: TStringList;
begin
    If String.IsNullOrEmpty(SourceStr) Or
        String.IsNullOrWhiteSpace(SourceStr) Or
        String.IsNullOrEmpty(Pattern) Then
    begin
      result := nil;
      Exit;// Nothing
    end;
    Mpattern := String.Empty;
    resultList := TStringList.Create;// List(Of String)
//    '  "(\r?\n)" satýr \r=CR \n=LF için satýr baþlarýndan böler Her þartta bölecektir.
    Mpattern := '([' + Pattern + '])';
    If ExplitCapture Then
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roExplicitCapture, TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList
    end
    Else
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList;
    end;
    result := resultList;
End;

/// <summary>
/// String Bilgiyi Ýstenilen Karakterlerden Böler
/// Satýrbaþý Varsa Otomatik Bölünür
/// </summary>
/// <param name="SourceStr">Bölünecek String</param>
/// <param name="Pattern">Bölecek Krakterler (örn: ",.-")</param>
/// <param name="ExplitCapture">Bölen Karakterler Gelmesin</param>
/// <returns></returns>
/// <remarks></remarks>
///Private Shared
Function TStringFunctions.Split(SourceStr: String; ExplitCapture: Boolean; Pattern: String): TStringList;
var
  Mpattern, currStr: String;
  resultList: TStringList;
begin
    If String.IsNullOrEmpty(SourceStr) Or
        String.IsNullOrWhiteSpace(SourceStr) Or
        String.IsNullOrEmpty(Pattern) Then
        begin //Return Nothing
          result := nil;
          Exit;
        end;
    Mpattern := String.Empty;
    resultList := TStringList.Create;// List(Of String)
//    '  "(\r?\n)" satýr \r=CR \n=LF için satýr baþlarýndan böler Her þartta bölecektir.
    Mpattern := IfThen(Mpattern = String.Empty, Mpattern + '(\r?\n)' + '|([' + Pattern + '])', Mpattern + '|([' + Pattern + '])');
    If ExplitCapture Then
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roExplicitCapture, TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList
    end
    Else
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList;
    end;
    result :=  resultList;
End;

/// <summary>
/// String Bilgiyi istenilen Karakter uzunluðunda parçalar.
/// </summary>
/// <param name="SourceStr">Bölünecek Text</param>
/// <param name="Count">Bölüm sonrasýndaki Karakter Adedi</param>
/// <param name="IsNetcadText">Netcad ise Cont>50 ise  50 olarak alýnýr</param>
/// <returns></returns>
/// <remarks></remarks>
///Public Shared
Function TStringFunctions.Split(SourceStr: String; Count: Integer; IsNetcadText: Boolean): TStringList;
begin
    If String.IsNullOrEmpty(SourceStr) Or
       String.IsNullOrWhiteSpace(SourceStr) Then
       begin //Return Nothing
          result := nil;
          Exit;
       end;
//    If IsNetcadText And (Count > 50) Then
//      Count := 50;
//    'Return Regex.Replace(SourceStr, "(.{1," & Count & "})", """$1""" & vbLf).Split(vbLf).ToList
    result := StringListFromStrings(TRegex.Replace(SourceStr, '(.{' + IntToStr(Count) + '})', '$&' + vbLf).Split([vbLf]));
//    'regex = New Regex(".{50}")
//    'result = regex.Replace(SourceStr, "$&" + Environment.NewLine).Split(vbLf).ToList
//    'Console.WriteLine(result)
End;

/// <summary>
/// String bilgi istenilen miktarda Böldürülür
/// </summary>
/// <param name="SourceStr">String bilgi</param>
/// <param name="Pattern">Patern</param>
/// <param name="MaxSplitCount">Parca adedi</param>
/// <returns></returns>
/// <remarks></remarks>
///Public Shared
Function TStringFunctions.Split(SourceStr: String; Pattern: String; MaxSplitCount: Integer): TStringList;
var
  Atla, Atla1: Boolean;
  Adet, Say, Topla, i: Integer;
  resultList, S, SS: TStringList;
  Str: String;
  A, B: Char;
  AA: String;
begin
    If String.IsNullOrEmpty(SourceStr) Or
       String.IsNullOrWhiteSpace(SourceStr) Or
       String.IsNullOrEmpty(Pattern) Then 
    begin //Return Nothing
      result := nil;
      Exit;
    end;

    S := Split(SourceStr, Pattern, True);

    Adet := S.Count;

    If Adet <= MaxSplitCount Then 
    begin //Return S
      result := S;
      Exit;
    end;

    SS := Split(SourceStr, Pattern, False);

    Adet := Math.Floor((SS.Count) / MaxSplitCount);

    Say := 0;

    resultList := TStringList.Create;// New List(Of String);

    Str := String.Empty;

    For A In Pattern.ToCharArray do
    begin
        while SS[0] <> A do
            SS.Delete(0);
    end;

    for i := 0 to SS.Count - 1 do begin
      SS.Exchange(i,SS.Count-i-1);
    end;
//    SS.Reverse
    For A In Pattern.ToCharArray do
    begin
        while SS[0] <> A do
            SS.Delete(0);
    end;
    for i := 0 to SS.Count - 1 do begin
      SS.Exchange(i,SS.Count-i-1);
    end;
//    SS.Reverse

    Atla := False;
    Atla1 := False;
    Topla := 1;

    For AA In SS do
    begin
        Topla := 1;
        If Say < Adet Then
        begin
            For B In Pattern.ToCharArray do
            begin
                If b = AA Then
                begin
                  Topla := 0;
                  Break;//Exit For
                end;
            end;
            If Topla = 0 Then
            begin
                If Say <> 0 Then
                  Str := Str +AA;
            end
            Else
            begin
                Str := Str + AA;
                Say := Say + Topla;
            End;
        end
        Else If Say = Adet Then
        begin
            resultList.Add(Str.Trim);
            For b In Pattern.ToCharArray do
            begin
                If b = AA Then
                begin
                  Topla := 0;
                  Break;// : Exit For
                end;
            end;
            If Topla = 0 Then
            begin
                Str := String.Empty;
                Say := 0;
                Continue;// For
            end
            Else
            begin
                Str := AA;
                Say := 1;
                Continue;// For
            End;
        End;
    end;
    If Str <> String.Empty Then
      resultList.Add(Str.Trim);
    result := resultList;
End;

/// <summary>
/// String Bilgiyi Uzunluk Olarak Böler
/// </summary>
/// <param name="SourceStr">Bölünecek String</param>
/// <param name="Pattern">Bölecek Krakterler (örn: ",.-")</param>
/// <param name="Widht">Ýstenilen Uzunluk (m)</param>
/// <param name="Font">Font</param>
/// <param name="IsNetcadText">Netcad de kullanýlacaksa true Netcad 50 karakter den fazla string bilgiyi tutmaz</param>
/// <returns></returns>
/// <remarks></remarks>
///Public Shared
Function TStringFunctions.Split(SourceStr: String; Pattern: String; Widht: Double; AFontTool: IlicgFontTool; IsNetcadText: Boolean): TStringList;
var
  CString, Str, txt: String;
  ResultList, Satirlar, TempStringSplit: TStringList;
  uzunluk: Single;
  LBmp: TBitmap;
  TempEnt: IlicgEntity;
begin
    If String.IsNullOrEmpty(SourceStr) Or
        String.IsNullOrWhiteSpace(SourceStr) Or
        String.IsNullOrEmpty(Pattern) Or
        (Widht <= 0) Then
        begin //Return Nothing
          result := nil;
          Exit;
        end;
    CString := String.Empty;
    ResultList := TStringList.Create;//As New List(Of String);
    Satirlar := Split(SourceStr, True);
//    uzunluk As Single;
    TempEnt := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);
    TempEnt.Geometry.Points.Add(AsCoor(0,0));
    For Str In Satirlar do
    begin
        TempStringSplit := Split(Str, Pattern, False);
        For txt In TempStringSplit do
        begin
//            cnv := TCanvas.Create;
//            cnv.Font := Font;
//            uzunluk := cnv.TextExtent(Format('%s%s', [CString, txt])).cx; //Width
          if (txt = '') and (CString = '') then
            continue;
//          TempEnt := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);
          TempEnt.AsTextValue.Text := Format('%s%s', [CString, txt]);
          TempEnt.DrawTools.FontTool.Height := AFontTool.Height;
//          TempEnt.Geometry.Points.Add(AsCoor(0,0));
//          TempEnt := Licad.CreateEntityFactory.MakeText(AsCoor(0,0),,Font.Height,0);
          uzunluk := Abs(TempEnt.Geometry.Extent.LowerLeft.X - TempEnt.Geometry.Extent.UpperRight.X);
          //CadGeometri.TextWidthCanvas(Format('%s%s', [CString, txt]),(Font.Height));
            {LBmp := TBitmap.Create;
            try
//                      System.Drawing.Font('Arial', (_Yaziboyu / Olcek))
              LBmp.Canvas.Font.name := Font.Name;// 'Arial';
              LBmp.Canvas.Font.Height := Round(Font.Height);//Round(_YaziBoyu * Olcek);
              uzunluk := LBmp.Canvas.TextWidth(Format('%s%s', [CString, txt])); //TextExtent(Format('%s%s', [CString, txt])).cx;//
            finally
             LBmp.Free;
            end;
            }
            {
            If IsNetcadText Then
            begin

                If Length(Format('%s%s', [CString, txt]).ToCharArray) > 50 Then
                begin
                    CString := CString.Trim;
                    if CString <> '' then
                      ResultList.Add(CString);
                    CString := txt;
                    Continue;// For;
                End;

            End;
            }
            If uzunluk < Widht Then
                CString := Format('%s%s', [CString, txt])
            Else
            begin
                If Not String.IsNullOrEmpty(CString) Then
                begin
                    CString := CString.Trim;
                    if CString <> '' then
                      ResultList.Add(CString);
                End;
                CString := txt;
            End;
        end;
        CString := CString.Trim;
        if CString <> '' then
          ResultList.Add(CString);
        CString := String.Empty;
    end;
    result := ResultList;
End;

/// <summary>
/// String Bilgiyi Uzunluk Olarak Böler
/// Kelime kelime
/// </summary>
/// <param name="SourceStr">Bölünecek String</param>
/// <param name="Pattern">Bölecek Krakterler (örn: ",.-")</param>
/// <param name="Widht">Ýstenilen Uzunluk (m)</param>
/// <param name="FontName">Font Adý</param>
/// <param name="FontSize">Font Boyu</param>
/// <param name="IsNetcadText">Netcad de kullanýlacaksa true Netcad 50 karakter den fazla string bilgiyi tutmaz</param>
/// <returns></returns>
/// <remarks></remarks>
///Public Shared

Function TStringFunctions.Split(SourceStr: String; Pattern: String; Widht: Double; FontName: String; FontSize: Single; IsNetcadText: Boolean): TStringList;
var
  AFontTool: IlicgFontTool;
begin
  AFontTool := Licad.Settings.FontTool;
  AFontTool.Name := FontName; // As New Font(FontName, FontSize)
  AFontTool.Height := Round(FontSize);
  Result := Split(SourceStr, Pattern, Widht, AFontTool, IsNetcadText);
End;
{$endregion}

{$region 'Yazý Dayama Noktasýna Göre Kapalý dikdörtgen Objenin içerisinde text için koordinat döndürür' }
/// <summary>
/// Yazý Dayama Noktasýna Göre Kapalý dikdörtgen Objenin içerisinde text için koordinat döndürür
/// </summary>
/// <param name="Limit">Kutu</param>
/// <param name="VerticalIndent">Dikey Girinti</param>
/// <param name="HorizontalIndent">Yatay Girinti</param>
/// <param name="Just">Yazý Dayama Noktasý</param>
/// <returns></returns>
/// <remarks></remarks>
///Public Shared
Function TStringFunctions.FindinRectanleJustifCoor(Limit: TlicgExtent; VerticalIndent: Double; HorizontalIndent: Double; Just: TextJustify): TlicgCoor;
var
  resultCoor: TLicgCoor;
begin
    resultCoor := AsCoor(0,0);//New NCCoor;

    With Limit do
    begin
        Case Just of
            TextJustify.OrtaAlt:
            begin
                resultCoor.x := lowerLeft.y + HorizontalIndent;
                resultCoor.y := (Abs(lowerLeft.x - upperRight.x) / 2) + lowerLeft.x;
            end;
            TextJustify.OrtaOrta:
            begin
                resultCoor.x := (Abs(lowerLeft.y - upperRight.y) / 2) + lowerLeft.y;
                resultCoor.y := (Abs(lowerLeft.x - upperRight.x) / 2) + lowerLeft.x;
            end;
            TextJustify.OrtaUst:
            begin
                resultCoor.x := upperRight.y - HorizontalIndent;
                resultCoor.y := (Abs(upperRight.x + lowerLeft.x)) / 2;
            end;
            TextJustify.SolUst:
            begin
                resultCoor.x := upperRight.y - HorizontalIndent;
                resultCoor.y := lowerLeft.x + VerticalIndent;
            end;
            TextJustify.Solalt:
            begin
                resultCoor.x := lowerLeft.y + HorizontalIndent;
                resultCoor.y := lowerLeft.x + VerticalIndent;
            end;
            TextJustify.SolOrta:
            begin
                resultCoor.x := (Abs(lowerLeft.y - upperRight.y) / 2) + lowerLeft.y;
                resultCoor.y := lowerLeft.x + VerticalIndent;
            end;
            TextJustify.SagUst:
            begin
                resultCoor.x := upperRight.y - HorizontalIndent;
                resultCoor.y := upperRight.x - VerticalIndent;
            end;
            TextJustify.SagAlt:
            begin
                resultCoor.x := lowerLeft.y + HorizontalIndent;
                resultCoor.y := upperRight.x - VerticalIndent;
            end;
            TextJustify.SagOrta:
            begin
                resultCoor.x := (Abs(lowerLeft.y - upperRight.y) / 2) + lowerLeft.y;
                resultCoor.y := upperRight.x - VerticalIndent;
            end;
            TextJustify.OrtaAltDis:
            begin
                resultCoor.x := lowerLeft.y;// '- TextSize
                resultCoor.y := (Abs(lowerLeft.x - upperRight.x) / 2) + lowerLeft.x;
            end;
            TextJustify.SagAltDis:
            begin
                resultCoor.x := lowerLeft.y;// '- TextSize
                resultCoor.y := upperRight.x;
            end;
            TextJustify.SagOrtaDis:
            begin
                resultCoor.x := (Abs(lowerLeft.y - upperRight.y) / 2) + lowerLeft.y;
                resultCoor.y := upperRight.x;// '+ TextSize
            end;
            TextJustify.SolAltDis:
            begin
                resultCoor.x := lowerLeft.y;// ' - TextSize
                resultCoor.y := lowerLeft.x;
            end;
            TextJustify.SolOrtaDis:
            begin
                resultCoor.x := (Abs(lowerLeft.y - upperRight.y) / 2) + lowerLeft.y;
                resultCoor.y := lowerLeft.x;// ' - TextSize
            end;
            TextJustify.UstOrtaDis:
            begin
                resultCoor.x := upperRight.y;// '+ TextSize
                resultCoor.y := (Abs(upperRight.x + lowerLeft.x)) / 2;
            end;
        End;
    End;
    result := AsCoor(resultCoor.Y,resultCoor.X);
End;
{$endregion}

end.

