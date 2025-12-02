unit Lider.CG.ModulesCom.Table.XmlReader;

interface

uses
  System.IOUtils,
  Vcl.Dialogs,
  Lider.CG.ModulesCom.Table.Fonts,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.NumberFormat,
  Lider.CG.ModulesCom.Table.WorksBook,
  Lider.CG.ModulesCom.Table.Worksheet,
  Lider.CG.ModulesCom.Table.Style,
  Lider.CG.ModulesCom.Table.Alignment,
  Lider.CG.ModulesCom.Table.Column,
  Lider.CG.ModulesCom.Table.Row,
  Lider.CG.ModulesCom.Table.Cell,
  Lider.CG.ModulesCom.Table.EnumsAlignment,
  Lider.CG.ModulesCom.Table.EnumsFont,
  System.Classes,
  Winapi.Windows,
  Vcl.Controls,
  System.Generics.Collections,
  SysUtils,
  StrUtils,
  XMLIntf, XMLDoc,
  Vcl.Graphics,
  Lider.CG.Com.GIS,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.Consts,
  Lider.CG.Com.EntityInt,
  Vcl.Forms,
  Math,
  System.RegularExpressions;

type
  XmlReader = class
    private
      Tablo: WorksBook;
      Launcher: IlicgActionTracker;
      FBoolTrackedPoint: Boolean;
      FTrackedLineEntity: IlicgEntity;

      procedure SetListbox;

      procedure AddNetcad(Objectlist: ILicgEntityList; yref: Double; xref: Double);

    function MakeLine(C1, C2: TlicgCoor; Tabaka: string; CizgiTuru: Integer;
      Kalinlik: Double): IlicgEntity;
    function MakeRect(p1, pa: TlicgCoor; name, Tabaka: String): IlicgEntity;
    function MakeText(C: TlicgCoor; Text: String; Flag: TFontStyles;
      Font: Integer; Size, Angle: Double; Just: TlicgTextPos;
      Tabaka: String): IlicgEntity;
    function SelectLine(PCaption: string=''): IlicgEntity;
    procedure LauncherFinishedLine(Sender: TObject);
    procedure LauncherTrackedEntityLine(const TrackID: string;
      var TrackedEntity: IlicgEntity);
    function GetPrintTitlesCount(SouceStr: String): Integer;
    public
      SayfaYukseklik: Double;
      YinelenecekSatir: Integer;
      YukaridanAsagiSoldanSaga: Boolean;

      procedure DosyaSec;
      procedure Yukseklik;
      procedure Ekle;

      function TabakaOlustur(PLayerName: string; PColor: TColor = -1; FDrawBox:TlicgBaseDrawBox=nil): TlicgBaseLayer;
      function TabakaVarmi(Name: String): Boolean;
      function NoktaBelirle(PCaption: String; var PCoor: TlicgCoor; PCursor: Integer): Boolean;
      procedure LauncherFinishedPoint(Sender: TObject);
      procedure MouseUpPoint(const TrackID: string; var TrackedEntity: IlicgEntity);

      Constructor Create; overload;
  end;

implementation
{ XmlReader }

constructor XmlReader.Create;
begin
  SayfaYukseklik := 0;
  YinelenecekSatir := 0;
  YukaridanAsagiSoldanSaga := False;
end;

procedure XmlReader.DosyaSec;
begin
    Tablo := WorksBook.Create;
    Tablo.Read('');
    SetListbox;
End;

procedure XmlReader.SetListbox;
var
  a: String;
begin
//    Self.ListView1.Items.Clear
//    For a In Tablo.GetSheetNames do
//        Self.ListView1.Items.Add(a);
End;

procedure XmlReader.Yukseklik;
var
  Ent: IlicgEntity;
begin
//    Dim Netcad As New NCMacro.NCSObj
//    Dim c1 As New NCMacro.NCCoor
//    Dim c2 As New NCMacro.NCCoor
//    Dim Mathma As New NCMacro.NCMath
//    If Netcad.SelectPoint('Mesafe için ilk nokta yý seçiniz', c1, -1) Then
//        While Netcad.WalkLine('Mesafe için ikinci nokta yý seçiniz', c2, -1, c1)
//            TextBox1.Text = Mathma.distance(c1, c2, False)
//            Exit While
//        End While
//    End If
//    Mathma = Nothing
//    c2 = Nothing
//    c1 = Nothing
//    Netcad = Nothing
  Ent := SelectLine('Sayfa Yüksekliðini Belirleyiniz.');
  SayfaYukseklik := _Distance(Ent.Geometry.Points[0],Ent.Geometry.Points[1]);
End;

function XmlReader.GetPrintTitlesCount(SouceStr: String): Integer;
var
  MatchedList: TMatch;
  resultInt: Integer;
  TempStr, PatternStr: String;
  IndexRow: Integer;
//  gr: TGroup;
begin
  resultInt := 0;
  IndexRow := 0;
  PatternStr := '(\=.*\!R(?<r1>[0-9]+)\:R(?<r2>[0-9]+))';
  if TRegEx.IsMatch(SouceStr,PatternStr) then
  begin
    MatchedList := TRegEx.Match(SouceStr,PatternStr);
    if MatchedList.Success then
    begin
      try              //StrToInt(MatchedList.Groups['r1'].Value)-
        IndexRow := Abs(StrToInt(MatchedList.Groups['r2'].Value));
      except

      end;

      resultInt := IndexRow;
    end;
  end;
  result := resultInt;
end;

procedure XmlReader.Ekle;
var
  c1: TLicgCoor;
  I: Integer;
//  SayfaYukseklik: Double;
//  YinelenecekSatir: Integer; //üstte yinelecek Satýr
//  SoldanSagaYukaridanAsagi: Boolean; //Soldan saða diz = True, Yukarýdan Asagý Diz = False
//  i: Integer;
begin
//  SayfaYukseklik := 0;
//  Yineleme := 4;
//  SoldanSaga := True;
//    Dim Netcad As New NCMacro.NCSObj
//    i := 0;

//    Self.Hide;
//    For Each xx As ListViewItem In Me.ListView1.Items
//        Dim c1 As NCMacro.NCCoor = Netcad.newc(0, 0, 0)
//        If xx.Checked = True Then
//  Tablo.Sheet[0].
  for I := 0 to Tablo.Sheet.Count - 1 do
  begin
    YinelenecekSatir := 0;
    If (Tablo.Sheet[I].NamedRange <> nil) and (Tablo.Sheet[I].NamedRange[0].NamedCell.Name = 'PrintTitles') then
      YinelenecekSatir := GetPrintTitlesCount(Tablo.Sheet[I].NamedRange[0].RefersTo);
    If NoktaBelirle('Baþlangýç Noktasý seç', c1, -1) Then
      AddNetcad(Tablo.GetNcObjectList(CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000, I,
                SayfaYukseklik, YinelenecekSatir, YukaridanAsagiSoldanSaga), c1.y, c1.x);
  end;
//            End If
//        End If
//        i += 1
//    Next

//    Self.Show;
//    Netcad = Nothing
End;

function GetSavingLayer(PLayerName: string; PColor: TColor = -1; FDrawBox:TlicgBaseDrawBox=nil): TlicgBaseLayer;
var ResulLayer: TlicgBaseLayer;
begin
  if PColor = -1 then
    PColor := clBlack;

  if FDrawBox = nil then
    FDrawBox := CurrCmdLine.ActiveDrawBox;
  ResulLayer := FDrawBox.GIS.Layers.LayerByName(PLayerName);

  if (PLayerName <> '') then
  begin
    if (ResulLayer = nil) then
      ResulLayer := FDrawBox.GIS.CreateLayer(PLayerName, ltDesktop, PColor)
    else
      ResulLayer := FDrawBox.GIS.Layers.LayerByName(PLayerName)
  end
  else
    ResulLayer := FDrawBox.GIS.CurrentLayer;

  Result := ResulLayer;
end;

procedure XmlReader.AddNetcad(Objectlist: ILicgEntityList; yref: Double; xref: Double);
var
  TabakaCizgi, TabakaYazi, TabakaKutu: String;
  aaa: IlicgEntity;
  Ent: IlicgEntity;
  RecNo, i: Integer;
begin
//    Dim asa As New NCMacro.NCSObj

//    asa.Undo(1)
    CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);

    TabakaCizgi := 'TABLE_TABLO_CIZGI';
    TabakaOlustur(TabakaCizgi);
    TabakaYazi := 'TABLE_TABLO_YAZI';
    TabakaOlustur(TabakaYazi);
    TabakaKutu := 'TABLE_TABLO_KUTU';
    TabakaOlustur(TabakaKutu);


    For i := 0 to Objectlist.Count - 1 do
    begin
      aaa := Objectlist[i];
        If aaa.EntityID = idLine then//aaa.GetType = GetType(Tag2_Line) Then
            Ent := MakeLine(AsCoor(aaa.Geometry.Points[0].x + xref,aaa.Geometry.Points[0].y + yref),
            AsCoor(aaa.Geometry.Points[1].x + xref, aaa.Geometry.Points[1].y + yref), TabakaCizgi, aaa.DrawTools.PenTool.Style, 0)
        Else If aaa.EntityID = idRectangle then//aaa.GetType = GetType(Tag10_Rectangle) Then
            Ent := MakeRect(AsCoor(aaa.Geometry.Points[0].x + xref,aaa.Geometry.Points[0].y + yref),
            AsCoor(aaa.Geometry.Points[1].x + xref, aaa.Geometry.Points[1].y + yref), aaa.name, TabakaKutu)
//            Ent := MakeRect(AsCoor(aaa.Geometry.Points[0].x + xref,aaa.Geometry.Points[0].y + yref),
//            AsCoor(aaa.Geometry.Points[1].x + xref, aaa.Geometry.Points[1].y + yref), aaa.name, TabakaKutu)
        Else If aaa.EntityID in TextEntityIDs then//aaa.GetType = GetType(Tag5_Text) Then
            Ent := MakeText(AsCoor(aaa.Geometry.Points[0].x + xref,aaa.Geometry.Points[0].y + yref),
            aaa.AsTextValue.Text, aaa.DrawTools.FontTool.Style, 0, aaa.DrawTools.FontTool.Height, aaa.DrawTools.FontTool.Angle, aaa.DrawTools.FontTool.TextPos, TabakaYazi);

        if Ent.Layer = nil then
          Ent.Layer := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer;
        RecNo := GetSavingLayer(TLicgBaseLayer(Ent.Layer).DisplayName).AddEntity(Ent);
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TLicgBaseLayer(Ent.Layer),RecNo,uaDelete);
    end;
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
//    asa.Undo(2)
//    asa = Nothing
End;

procedure XmlReader.LauncherFinishedPoint(Sender: TObject);
begin
//  Launcher.CurrentAction.Finished := True;
end;

procedure XmlReader.MouseUpPoint(const TrackID: string; var TrackedEntity: IlicgEntity);
begin
  FBoolTrackedPoint := True;
end;

function XmlReader.NoktaBelirle(PCaption: String; var PCoor: TlicgCoor; PCursor: Integer): Boolean;
var
  ResultBool: Boolean;
  TempCoor: TLicgCoor;
begin
  Result := False;
  PCoor := AsCoor(0,0);
  try
    Launcher := Licad.CreateActionTracker(CurrCmdLine);
    Launcher.IsCmdLineClear := False;
    Launcher.IsDefaultInitialization := True;
    Launcher.TrackPoint(SCmdLauncher); //SCmdPoint
    Launcher.OnTrackedEntity := MouseUpPoint;
    Launcher.OnFinished := LauncherFinishedPoint;
    Launcher.CurrentAction.Caption := PCaption;

  //  if not Launcher.CurrentAction.CmdLine.AccuSnap.Enabled then
  //    Launcher.CurrentAction.CmdLine.AccuSnap.Enabled := True;

  //  if not CurrCmdline.AccuSnap.Enabled then
  //    CurrCmdline.AccuSnap.Enabled := True; //Invalid Floating Point Operation : HATA //Giderildi.

    FBoolTrackedPoint := False;

    repeat
      Application.HandleMessage; //Invalid Floating Point Operation : HATA //Giderildi.
    until Launcher.Finished;

  //  if CurrCmdline.AccuSnap.Enabled then
  //    CurrCmdline.AccuSnap.Enabled := False;

    Launcher.Finish;

    if CurrCmdLine.IsSnapped then
      PCoor := CurrCmdLine.GetSnappedPoint
    else
      PCoor := CurrCmdLine.CurrentPoint;

    Result := FBoolTrackedPoint;
  finally

  end;
  {
  Result := False;
  try
    MyLauncher := Licad.CreateActionTracker(CurrCmdLine);
    MyLauncher.Caption := PCaption;
    MyLauncher.Cursor := PCursor;
    MyLauncher.GetPointFromScreen(CB_getFromScreen);
    PCoor := CurrCmdLine.CurrentPoint;
    Result := True;
  finally

  end;
  }
end;

function XmlReader.SelectLine(PCaption: string = ''): IlicgEntity;
begin
  FTrackedLineEntity := nil;
  Launcher := Licad.CreateActionTracker(CurrCmdLine);
  Launcher.IsCmdLineClear := False;
  Launcher.IsDefaultInitialization := True;
  Launcher.OnTrackedEntity := LauncherTrackedEntityLine;
  Launcher.TrackLine(SCmdLine,False);
  Launcher.OnFinished := LauncherFinishedLine;

  if (PCaption <> '') then
    Launcher.CurrentAction.Caption := PCaption;
  repeat
    Application.HandleMessage;
  until Launcher.Finished;
  Launcher.TrackLine(SCmdLauncher);
  Launcher.Finish;
  Result := FTrackedLineEntity;
end;

procedure XmlReader.LauncherTrackedEntityLine(const TrackID: string;
                                                  var TrackedEntity: IlicgEntity);
begin
  if (TrackedEntity <> nil) and (TrackedEntity.EntityID in [idLine]) then
  begin
    FTrackedLineEntity := TrackedEntity.Clone;
    Launcher.Finished := True;
  end;
end;

procedure XmlReader.LauncherFinishedLine(Sender: TObject);
begin
//  Launcher.CurrentAction.Finished := True;
end;

function XmlReader.TabakaOlustur(PLayerName: string; PColor: TColor = -1; FDrawBox:TlicgBaseDrawBox=nil): TlicgBaseLayer;
var ResulLayer: TlicgBaseLayer;
begin
  if PColor = -1 then
    PColor := clBlack;

  if FDrawBox = nil then
    FDrawBox := CurrCmdLine.ActiveDrawBox;

  ResulLayer := FDrawBox.GIS.Layers.LayerByName(PLayerName);

  if (PLayerName <> '') then
  begin
    if (ResulLayer = nil) then
      ResulLayer := FDrawBox.GIS.CreateLayer(PLayerName, ltDesktop, PColor)
    else
      ResulLayer := FDrawBox.GIS.Layers.LayerByName(PLayerName)
  end
  else
    ResulLayer := FDrawBox.GIS.CurrentLayer;

  Result := ResulLayer;
end;

Function XmlReader.TabakaVarmi(Name: String): Boolean;
begin
  result := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(Name) <> nil;
end;

function XmlReader.MakeLine(C1: TlicgCoor;C2: TlicgCoor;Tabaka: string;CizgiTuru:Integer;Kalinlik: Double): IlicgEntity;
var
  LineObj: IlicgEntity;
begin
  LineObj := Licad.CreateEntityFactory.MakeLine(C1,C2);
  LineObj.DrawTools.PenTool.Style := CizgiTuru;
  LineObj.DrawTools.PenTool.Width := Kalinlik;
  LineObj.Layer := TabakaOlustur(Tabaka);
  Result := LineObj;
end;

function XmlReader.MakeText(C:TlicgCoor;Text:String;Flag:TFontStyles;Font:Integer;Size:Double;Angle:Double;Just:TlicgTextPos;Tabaka: String): IlicgEntity;
var
  TextObj: IlicgEntity;
begin
  TextObj := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);
  TextObj.AsTextValue.Text := Text;
  TextObj.DrawTools.FontTool.Height := Size;
  TextObj.DrawTools.FontTool.Angle := Angle;
  TextObj.Geometry.Points.Add(C);
//  TextObj := Licad.CreateEntityFactory.MakeText(C,Text,Size,Angle);

  TextObj.DrawTools.FontTool.TextPos := Just;
//  TextObj.DrawTools.FontTool.Style := Flag;
//  TextObj.DrawTools.FontTool.Name := 'Arial';
  TextObj.Layer := TabakaOlustur(Tabaka);
  Result := TextObj;
end;

function XmlReader.MakeRect(p1: TlicgCoor; pa: TlicgCoor; name: String; Tabaka: String): IlicgEntity;
var
  RectObj: IlicgEntity;
begin
  RectObj := Licad.CreateEntityFactory.MakeRectangle(p1,pa);
  RectObj.Name := name;
  RectObj.Layer := TabakaOlustur(Tabaka);
  Result := RectObj;
end;

{
Public Overloads Function TabakaOlustur(ByVal Name As String, Optional ByVal Color As Byte = 0) As Boolean
begin
    Dim LayerManager As New NCMacro.NetcadLayerManagerMC
    Try
        If Color = 0 Then
            Randomize
            Color = Int(Rnd * 255)
        End If
        LayerManager.add(Name, Color)
        releaseObject(LayerManager)
        Return True
    Catch ex As Exception
        releaseObject(LayerManager)
        Return False
    End Try
End;


Function XmlReader.TabakaVarmi(Name: String): Boolean;
begin
    Dim LayerManager As New NCMacro.NetcadLayerManagerMC
    Dim Sonuc As Boolean
    Select Case LayerManager.Find(Name)
        Case 1
            Sonuc = True
        Case -1
            Sonuc = False
    End Select
    releaseObject(LayerManager)
    Return Sonuc
End Function

#Region " Objeler Sonlandýrýlýyor"
Private Sub releaseObject(ByVal obj As Object)
    Try
        System.Runtime.InteropServices.Marshal.ReleaseComObject(obj)
        obj = Nothing
    Catch ex As Exception
        obj = Nothing
    Finally
        GC.Collect
        GC.WaitForPendingFinalizers
    End Try
End Sub
#End Region
}

end.

