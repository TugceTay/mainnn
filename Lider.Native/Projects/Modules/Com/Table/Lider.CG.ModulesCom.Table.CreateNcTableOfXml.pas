unit Lider.CG.ModulesCom.Table.CreateNcTableOfXml;

interface

uses
  Lider.CG.ModulesCom.Table.WorksBook,
  Lider.CG.Com.GIS,
  Vcl.Forms,
  Winapi.Windows,
  Lider.CG.ModulesCom.Table.Alignment,
  Lider.CG.Com.EntityInt,
  Lider.CG.ModulesCom.Table.CellInt,
  Lider.CG.ModulesCom.Table.Column,
  Lider.CG.ModulesCom.Table.Cell,
  Lider.CG.ModulesCom.Table.ColInt,
  Lider.CG.ModulesCom.Table.RowInt,
  Lider.CG.ModulesCom.Table.Row,
  Lider.CG.ModulesCom.Table.Style,
  Lider.CG.ModulesCom.Table,
  Lider.CG.ModulesCom.Table.Border,
  Lider.CG.ModulesCom.Table.BorderInt,
  Lider.CG.ModulesCom.Table.Border.Top,
  Lider.CG.ModulesCom.Table.Border.Left,
  Lider.CG.ModulesCom.Table.Border.Right,
  Lider.CG.ModulesCom.Table.DiagonalLeft,
  Lider.CG.ModulesCom.Table.DiagonalRight,
  Lider.CG.ModulesCom.Table.Border.Bottom,
  Lider.CG.ModulesCom.Table.Continuous,
  Lider.CG.Com.GeoTypes,
  System.UITypes,
  StrUtils,
  SysUtils,
  Math,
  Vcl.Graphics,
  System.Classes,
  Lider.CG.Com.LicadInt,
  System.Generics.Collections,
  System.RegularExpressions,
  Lider.CG.Com.GeometryInt,
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
  CreateNcTableOfXml = Class
    private
      Scale: Single;
      NcTable: Table;
      RealWorksBook: WorksBook;
      NcObjelist: IlicgEntityList;

      function AddCaption(CaptionRowCount: Integer; var ref_y: Double; var ref_x: Double): Double;
      function AddObjeInNetcadObjList(var NetcadObjList: IlicgEntityList; PCell: Cell;
                    VerticalLeng: Double; HorizontalLeng: Double; ref_y: Double; ref_x: Double): Integer;
      function Get_CorrInSplitString(Refcorr: TLicgCoor3d; Yazidayama: TextJustify;
                                    TextSize: Double; TotalTextCount: Integer; index: Integer): TLicgCoor3d;

      procedure CreateNcTabloColums(ShetIndex: Integer);
      procedure CreateNcTabloRows(ShetIndex: Double);
      function GetFormattedValue(Cell: Cell; Style: Style): String;
      function ToSMm(Value: Single): Single;
      function ToM(Value: Single): Single;
    public
      function Split(SourceStr: string; Delimiter: Char; ListOfStrings:TStrings): string; overload;
      function GetTableTablo(SheetIndex: Integer): IlicgEntityList; overload;
      function GetTableTablo(SheetIndex: Integer; SheetHeight: Double; CaptionRowCount: Integer;
                               PutRightorDown: Boolean): IlicgEntityList; overload;

      function StringListFromStrings(const Strings: array of string): TStringList;
      Function Split(SourceStr: String; ExplitCapture: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; ExplitCapture: Boolean): TStringList; overload;
      Function Split(SourceStr: String; ExplitCapture: Boolean; Pattern: String): TStringList; overload;
      Function Split(SourceStr: String; Count: Integer; IsNetcadText: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; MaxSplitCount: Integer): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; Widht: Single; AFontTool: IlicgFontTool; IsNetcadText: Boolean): TStringList; overload;
      Function Split(SourceStr: String; Pattern: String; Widht: Single; FontName: String; FontSize: Single; IsNetcadText: Boolean): TStringList; overload;
      Function FindinRectanleJustifCoor(Limit: TlicgExtent; VerticalIndent: Double; HorizontalIndent: Double; Just: TextJustify): TlicgCoor;
      constructor Create(Olcek: Single; v: WorksBook);
  end;

  Const TO_M: Double = 72 / 25.4;
        SatirAralikKatsayisi: Double = 0.5;
        UstAltIndent: Double = 1;
        vbLf = #$A;

implementation
{ CreateNcTableOfXml }

var
  ref_y, ref_x: Double;

{$region 'Diðer Fonksiyonlar' }
function GetPosCad(PPos: TextJustify): TlicgTextPos;
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

function IsNumeric(s: string): Boolean;
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
constructor CreateNcTableOfXml.Create(Olcek: Single; v: WorksBook);
begin
  Scale := Olcek;
  RealWorksBook := v;
end;

///        ''' <summary>
///        ''' Kullanmýyorum çünki sayfalara bölen ile ayný iþi yapýyor
///        ''' </summary>
///        ''' <param name="SheetIndex"></param>
///        ''' <returns></returns>
///        ''' <remarks></remarks>
function CreateNcTableOfXml.GetTableTablo(
  SheetIndex: Integer): IlicgEntityList;
var
//  NcObjelist: IlicgEntityList;
//  NcTable: Table;
//  ref_x, ref_y,
  VerticalLeng, HorizontalLeng: Double;
  CurrentRow, i, Coli, Rowi: Integer;
  ShetStyle, CurrentColStyle: Style;
  a: RowInt;
begin
  NcObjelist := TLicgEntityList.Create;
  NcTable := Table.Create(RealWorksBook.Sheet[(SheetIndex)].Table.DefaultRowHeight, RealWorksBook.Sheet[(SheetIndex)].Table.DefaultColumnWidth);
  CreateNcTabloColums(SheetIndex);
  CreateNcTabloRows(SheetIndex);

  ref_x := 0; //Static
  ref_y := 0; //Static

//  'Satýr Gecerli Satýr Numarasý
//  'Ýþlem Tamamlandýðýnda ðeçerli
//  'satýrýn index numarasý ile ayný olur çünkilistler 0 ile baþlar
  CurrentRow := 0;
//  'Yatay Uzunluk Hücre Bazlý
//  VerticalLeng := nil;
//  ' Dikey Uzunluk Hucre Bazlý
//  HorizontalLeng := nil;
  VerticalLeng := 0;
  HorizontalLeng := 0;
//  ' Sayfa Stili
  ShetStyle := nil;
//  'Gecerli Colonun Stli
  CurrentColStyle := nil;
//  ' Satýr adedi kadar dön
  For a In NcTable.Row do
  begin
//      ' Hücre Adedi Kadar Dön
      i := -1;
      while i < a.Cell.Count - 1 do
      begin
        Inc(i);
          VerticalLeng := 0;//nil
          HorizontalLeng := 0;//nil


//          ' Hücre Kullanýlan Hücremi
          If Not a.Cell.InnerList[i].IsImaginary Then
          begin
//              'Hücrenin saga birleþmesi yoksa
              If a.Cell.InnerList[i].MergeAcross = 0 Then
                  VerticalLeng := IfThen(NcTable.Column.InnerList[i].Width > 0, NcTable.Column.InnerList[i].Width, NcTable.DefaultColumnWidth)
//                  'Hücrenin saga birleþmesi varsa
              Else If a.Cell.InnerList[i].MergeAcross > 0 Then
              begin
//                  ' Saga Birleþme kadar colon da dön
                  For Coli := i To i + a.Cell.InnerList[i].MergeAcross do
                  begin
                      VerticalLeng := VerticalLeng + IfThen(NcTable.Column.InnerList[Coli].Width > 0, NcTable.Column.InnerList[Coli].Width, NcTable.DefaultColumnWidth);
                  end;
              End;

              If a.Cell.InnerList[i].MergeDown = 0 Then
              begin
//                  ' 15 degeri standart yuksekliktir
                  HorizontalLeng := IfThen(a.Height > 0, a.Height, NcTable.DefaultRowHeight)
              end
              Else If a.Cell.InnerList[i].MergeDown > 0 Then
              begin
                  For Rowi := CurrentRow To CurrentRow + a.Cell.InnerList[i].MergeDown do
                  begin
                      HorizontalLeng := HorizontalLeng + IfThen(NcTable.Row.InnerList[Rowi].Height > 0, NcTable.Row.InnerList[Rowi].Height, NcTable.DefaultRowHeight);
                  end;
              End;
//              ' Hücre Kullanýlmayan Hücremi
          end
          Else
          begin
              For Coli := i To a.Cell.Count - 1 do
              begin
                  If (Not a.Cell.InnerList[Coli].IsImaginary) Or (Coli = a.Cell.Count - 1) Then
                  begin
                      If Coli = a.Cell.Count - 1 Then
                      begin
                          i := Coli;
                          Break;// : Exit For
                      end
                      Else
                      begin
                          i := Coli - 1;
                          Break;// : Exit For
                      End;

                  End;
                  ref_y := ref_y + IfThen(NcTable.Column.InnerList[Coli].Width > 0, ToM(NcTable.Column.InnerList[Coli].Width), ToM(NcTable.DefaultColumnWidth));
              end;
              If (HorizontalLeng = 0) And (i = a.Cell.Count - 1) Then
                  HorizontalLeng := IfThen(a.Height > 0, a.Height, NcTable.DefaultRowHeight);
              If (VerticalLeng = 0) And (i = a.Cell.Count - 1) Then
                  VerticalLeng := IfThen(NcTable.Column.InnerList[i].Width > 0, NcTable.Column.InnerList[i].Width, NcTable.DefaultColumnWidth);
          End;

//          'boyutlar metreye çevriliyor
          VerticalLeng := ToM(VerticalLeng);
          HorizontalLeng := ToM(HorizontalLeng);

          AddObjeInNetcadObjList(NcObjelist, a.Cell.InnerList[i] as Cell, VerticalLeng, HorizontalLeng, ref_y, ref_x);


          If (a.Cell.InnerList[i].MergeAcross) > 0 Then
              i := i + a.Cell.InnerList[i].MergeAcross;


          ref_y := ref_y + VerticalLeng;
      end;
      CurrentRow := CurrentRow + 1;
      ref_y := 0;
      ref_x := ref_x - IfThen(a.Height > 0, ToM(a.Height), ToM(NcTable.DefaultRowHeight));
  end;
  result := NcObjelist;
end;

function CreateNcTableOfXml.GetTableTablo(SheetIndex: Integer;
  SheetHeight: Double; CaptionRowCount: Integer;
  PutRightorDown: Boolean): IlicgEntityList;
var
//  NcObjelist: IlicgEntityList;
//  NcTable: Table;
  objeadded: Boolean;
  AddingSheetCount, i, CurrentRowIndex, CurrentCellIndex, CRow, Suankisatir, yenidown: Integer;
  Y_Kaydir, X_Kaydir, CellVerticalLeng, CellHorizontalLeng,
  BaslikYuksekligi, ToplamSayfaYuksekligi, TempCurrentHeight: Double;
  CurrentRow: RowInt;
  CurrentCell, BolunenCell: CellInt;
begin
  If SheetHeight = 0 Then
    SheetHeight := 10000000000;
//  ' Netcad Objesini Tutacak olan Object Listesi
  NcObjelist := TlicgEntityList.Create;//New List(Of CoveringCadObject)
//  ' Yeni Bir Tablo oluþturuluyor Gelen Tablodan Farklý Olarak Tüm satur ve sutunlar dolu olacak
  NcTable := Table.Create(RealWorksBook.Sheet[SheetIndex].Table.DefaultRowHeight, RealWorksBook.Sheet[SheetIndex].Table.DefaultColumnWidth);
//  ' Oluþturululan Tablonun Colonlarýný Düzenler Eksikler olan Colonlar tamamlanýr
  CreateNcTabloColums(SheetIndex);
//  ' Oluþturululan Tablonun Satýrlarýný Düzenler satýr içinde eksik olan hücreler tamamlanýr
  CreateNcTabloRows(SheetIndex);


  Try
      If CaptionRowCount > NcTable.Row.Count Then
        raise Exception.Create('Tabloda Bulunan Satýr Adedi Baþlýk Satýr Adedinden Az veya Eþit.');
//          Throw New Exception("Tabloda Bulunan Satýr Adedi Baþlýk Satýr Adedinden Az veya Eþit.");
  except on e: Exception do
      CaptionRowCount := 0;
//      ' MsgBox(ex.Message)
//      ' Return NcObjelist
  End;

  objeadded := False;
//  ' Dim AddNewSheetHeader As Boolean = False
  AddingSheetCount := 0;

//  ' Bir sayfanýn Geniþliðini bulur ve yanyana diziliþler de nekadar kayýlacaðýný hesaplar
//  Y_Kaydir As Double;
  Y_Kaydir := 0;
  For i := 0 To NcTable.Column.Count - 1 do
      Y_Kaydir := Y_Kaydir + IfThen(NcTable.Column.InnerList[i].Width > 0, NcTable.Column.InnerList[i].Width, NcTable.DefaultColumnWidth);

  Y_Kaydir := Y_Kaydir + 10;
//  ' X yönünde kaydýrýlacak mesafe
  X_Kaydir := 10;

//  ' Referans Y deðeri
  ref_x := 0; //Static
//  ' Referans X deðeri
  ref_y := 0; //Static



//  '' '' '' Baþlýk Yüksekliðini Bulur
//  ' '' ''Dim BaslikYuksekligi = 0
//  ' '' ''For i = 0 To CaptionRowCount - 1
//  ' '' ''    BaslikYuksekligi += If(NcTable.Row(i).Height > 0, NcTable.Row(i).Height, NcTable.DefaultRowHeight)
//  ' '' ''Next

  BaslikYuksekligi := 0;
  BaslikYuksekligi := AddCaption(CaptionRowCount, ref_y, ref_x);
  If ToM(BaslikYuksekligi) > SheetHeight Then
    raise Exception.Create('Baslik yüksekliði Sayfa Yüksekliðinden Büyük.');

  ToplamSayfaYuksekligi := BaslikYuksekligi;

//  ' Satýr adedi kadar dön
  CurrentRowIndex := CaptionRowCount;
  while CurrentRowIndex <= NcTable.Row.Count - 1 do
  begin
      CurrentRow := NcTable.Row.InnerList[CurrentRowIndex];

      If ToM(ToplamSayfaYuksekligi + IfThen(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight)) < SheetHeight Then
      begin
//          ' Hücre Adedi Kadar Dön
          CurrentCellIndex := 0;
          while CurrentCellIndex <= CurrentRow.Cell.Count - 1 do
          begin
              CurrentCell := CurrentRow.Cell.InnerList[CurrentCellIndex];
              CellVerticalLeng := 0;
              CellHorizontalLeng := 0;

//              ' Hücre Kullanýlan Hücremi
              If Not CurrentCell.IsImaginary Then
              begin

//                  'Hücrenin saga birleþmesi yoksa
                  If CurrentCell.MergeAcross = 0 Then
                  begin
                      CellVerticalLeng := IfThen(NcTable.Column.InnerList[CurrentCellIndex].Width > 0, NcTable.Column.InnerList[CurrentCellIndex].Width, NcTable.DefaultColumnWidth)
//                      'Hücrenin saga birleþmesi varsa
                  end
                  Else If CurrentCell.MergeAcross > 0 Then
                  begin
//                      ' Saga Birleþme kadar colon da dön
                      For i := CurrentCellIndex To (CurrentCellIndex + CurrentCell.MergeAcross) do
                          CellVerticalLeng := CellVerticalLeng + IfThen(NcTable.Column.InnerList[i].Width > 0, NcTable.Column.InnerList[i].Width, NcTable.DefaultColumnWidth);
                  End;


//                  ' Hücrenin Aþaðý Birleþmesi yok sa
                  If CurrentCell.MergeDown = 0 Then
                  begin
//                      ' Hücrenin yüksekliði var sa hücre yüksekliði yok ise satýr için default deðeri yükseklik kabul eder
                      CellHorizontalLeng := IfThen(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight)
//                      ' Hücrenin Aþaðý birleþmesi varsa
                  end
                  Else If CurrentCell.MergeDown > 0 Then
                  begin
//                      ' Geçerli satýr indexsinden geçerli hücre aþaðý birleþme adedi kadar dön
//                      ' a nýn index numarasý birden baþladýðý ve listlerin index numarasý 0 dan baþladýðý
//                      ' için +1 ile indexleri eþitliyoruz

                      For CRow := 0 To CurrentCell.MergeDown do
                      begin
                          Suankisatir := CRow + CurrentRowIndex;
                          TempCurrentHeight := IfThen(NcTable.Row.InnerList[Suankisatir].Height > 0, NcTable.Row.InnerList[Suankisatir].Height, NcTable.DefaultRowHeight);
//                          ' Sayfa Yüksekliði geçiliyormu kontrol et
                          If ToM(ToplamSayfaYuksekligi + CellHorizontalLeng + TempCurrentHeight) >= SheetHeight Then
                          begin
                              BolunenCell := NcTable.Row.InnerList[Suankisatir].Cell.InnerList[CurrentCellIndex];
//                              ' Geildiði için hücre yi burada bitir ve hücrenin devamý olan hücreye yeni deðerleri ekle
//                              ' Aþaðý birleþme yi düzenle ve döngüden çýk
//                              p1 As System.Reflection.FieldInfo := BolunenCell.GetType.GetField( _
//                              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                              p1.SetValue(BolunenCell, CurrentCell.Index)
                              BolunenCell.Indexs := CurrentCell.Indexs;
                              BolunenCell.MergeAcross := CurrentCell.MergeAcross;
                              BolunenCell.StyleID := CurrentCell.StyleID;
                              BolunenCell.Data := CurrentCell.Data;
                              yenidown := Abs(CRow - CurrentCell.MergeDown);
                              BolunenCell.MergeDown := yenidown;
                              CurrentCell.MergeDown := Abs(CRow);

//                              'boyutlar metreye çevriliyor
                              CellVerticalLeng := ToM(CellVerticalLeng);
                              CellHorizontalLeng := ToM(CellHorizontalLeng);
                              objeadded := True;
                              AddObjeInNetcadObjList(NcObjelist, CurrentCell as Cell, CellVerticalLeng, CellHorizontalLeng, ref_y, ref_x);

                              If (CurrentCell.MergeAcross) > 0 Then
                                  CurrentCellIndex := CurrentCellIndex + CurrentCell.MergeAcross;
                              ref_y := ref_y + CellVerticalLeng;
//                              ' AddNewSheetHeader = True/
                              Break;//Exit For;
                          End;
//                          ' Dikey yüksekliði kendisi ile topla
                          CellHorizontalLeng := CellHorizontalLeng + IfThen(NcTable.Row.InnerList[Suankisatir].Height > 0, NcTable.Row.InnerList[Suankisatir].Height, NcTable.DefaultRowHeight);
                      end;
                      If objeadded Then
                      begin
                          objeadded := False;
                          Inc(CurrentCellIndex);
                          Continue;// For;
                      End;
                  End;
//                  ' Hücre Kullanýlmayan Hücremi
              end
              Else
              begin
//                  'For CCol = CurrentCellIndex To CurrentRow.Cell.Count - 1
//                  '    If Not CurrentRow.Cell(CCol).IsImaginary OrElse CCol = CurrentRow.Cell.Count - 1 Then
//                  '        If CCol = CurrentRow.Cell.Count - 1 Then
//                  '            CurrentCellIndex = CCol : Exit For
//                  '        Else
//                  '            CurrentCellIndex = CCol - 1 : Exit For
//                  '        End If
//
//                  '    End If
                  ref_y := ref_y + IfThen(NcTable.Column.InnerList[CurrentCellIndex].Width > 0, ToM(NcTable.Column.InnerList[CurrentCellIndex].Width), ToM(NcTable.DefaultColumnWidth));
//                  'Next

                  If (CellHorizontalLeng = 0) And (CurrentCellIndex = CurrentRow.Cell.Count - 1) Then
                      CellHorizontalLeng := IfThen(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight);
                  If (CellVerticalLeng = 0) And (CurrentCellIndex = CurrentRow.Cell.Count - 1) Then
                      CellVerticalLeng := IfThen(NcTable.Column.InnerList[CurrentCellIndex].Width > 0, NcTable.Column.InnerList[CurrentCellIndex].Width, NcTable.DefaultColumnWidth);

//                  CurrentCell.Indexs := 1;
              End;

//              'boyutlar metreye çevriliyor
              CellVerticalLeng := ToM(CellVerticalLeng);
              CellHorizontalLeng := ToM(CellHorizontalLeng);


//              ' Oluþturulan hücreyi listeye eklenmek üzere gönder
              AddObjeInNetcadObjList(NcObjelist, CurrentCell as Cell, CellVerticalLeng, CellHorizontalLeng, ref_y, ref_x);


              If (CurrentCell.MergeAcross) > 0 Then
                  CurrentCellIndex := CurrentCellIndex + CurrentCell.MergeAcross;


              ref_y := ref_y + CellVerticalLeng;
              Inc(CurrentCellIndex);
          end;

          ToplamSayfaYuksekligi := ToplamSayfaYuksekligi + IfThen(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight);

          If Not PutRightorDown Then
              ref_y := ToM(Y_Kaydir) * AddingSheetCount
          Else
              ref_y := 0;

          ref_x := ref_x - IfThen(CurrentRow.Height > 0, ToM(CurrentRow.Height), ToM(NcTable.DefaultRowHeight));
      end
      Else
      begin
//          'If AddNewSheetHeader Then
//          'AddNewSheetHeader = False
          AddingSheetCount := AddingSheetCount + 1;

          If Not PutRightorDown Then
          begin
              ref_y := ToM(Y_Kaydir) * AddingSheetCount;
              ref_x := 0;
              AddCaption(CaptionRowCount, ref_y, ref_x);
          end
          Else
          begin
              ref_y := 0;
              ref_x := ref_x - ToM(X_Kaydir);
              AddCaption(CaptionRowCount, ref_y, ref_x);
          End;

//          ' End If


          ToplamSayfaYuksekligi := BaslikYuksekligi;

          CurrentRowIndex := CurrentRowIndex - 1;

      End;
      Inc(CurrentRowIndex);
  end;


  result := NcObjelist;
end;

function CreateNcTableOfXml.AddCaption(CaptionRowCount: Integer; var ref_y,
  ref_x: Double): Double;
var
  objeadded: Boolean;
  i, CRow, Suankisatir, yenidown,
  AddingSheetCount, CurrentRowIndex, CurrentCellIndex: Integer;
  CurY, BaslikYuksekligi: Double;
  CurrentRow: RowInt;
  CurrentCell: CellInt;
  BolunenCell: CellInt;
  CellVerticalLeng, CellHorizontalLeng, TempCurrentHeight: Double;
begin
  objeadded := False;
  AddingSheetCount := 0;
  CurY := ref_y;
//  ' Satýr adedi kadar dön
  For CurrentRowIndex := 0 To CaptionRowCount - 1 do
  begin
      CurrentRow := NcTable.Row.InnerList[CurrentRowIndex];
//      ' Hücre Adedi Kadar Dön
      CurrentCellIndex := 0;
      while CurrentCellIndex <= CurrentRow.Cell.Count - 1 do
      begin
          CurrentCell := CurrentRow.Cell.InnerList[CurrentCellIndex];
          CellVerticalLeng := 0;
          CellHorizontalLeng := 0;
//          ' Hücre Kullanýlan Hücremi
          If Not CurrentCell.IsImaginary Then
          begin
//              'Hücrenin saga birleþmesi yoksa
              If CurrentCell.MergeAcross = 0 Then
              begin
                If(NcTable.Column.InnerList[CurrentCellIndex].Width > 0) then
                  CellVerticalLeng := NcTable.Column.InnerList[CurrentCellIndex].Width
                else
                  CellVerticalLeng := NcTable.DefaultColumnWidth;
//                  CellVerticalLeng = If(NcTable.Column(CurrentCellIndex).Width > 0, NcTable.Column(CurrentCellIndex).Width, NcTable.DefaultColumnWidth)
//                  'Hücrenin saga birleþmesi varsa'
              end
              Else If CurrentCell.MergeAcross > 0 Then
              begin
//                  ' Saga Birleþme kadar colon da dön
                  For i := CurrentCellIndex To CurrentCellIndex + CurrentCell.MergeAcross do
                  begin
                    If(NcTable.Column.InnerList[i].Width > 0) then
                      CellVerticalLeng := CellVerticalLeng + NcTable.Column.InnerList[i].Width
                    else
                      CellVerticalLeng := CellVerticalLeng + NcTable.DefaultColumnWidth;
//                      CellVerticalLeng += If(NcTable.Column(i).Width > 0, NcTable.Column(i).Width, NcTable.DefaultColumnWidth);
                  end;
              End;

//              ' Hücrenin Aþaðý Birleþmesi yok sa
              If CurrentCell.MergeDown = 0 Then
              begin
                If(CurrentRow.Height > 0) then
                  CellHorizontalLeng := CurrentRow.Height
                else
                  CellHorizontalLeng := NcTable.DefaultRowHeight;
//                  ' Hücrenin yüksekliði var sa hücre yüksekliði yok ise satýr için default deðeri yükseklik kabul eder
//                  CellHorizontalLeng = If(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight);
//                  ' Hücrenin Aþaðý birleþmesi varsa
              end
              Else If CurrentCell.MergeDown > 0 Then
              begin
//                  ' Geçerli satýr indexsinden geçerli hücre aþaðý birleþme adedi kadar dön
//                  ' a nýn index numarasý birden baþladýðý ve listlerin index numarasý 0 dan baþladýðý
//                  ' için +1 ile indexleri eþitliyoruz

                  For CRow := 0 To CurrentCell.MergeDown do
                  begin
                      Suankisatir := CRow + CurrentRowIndex;
                      If(NcTable.Row.InnerList[Suankisatir].Height > 0) then
                        TempCurrentHeight := NcTable.Row.InnerList[Suankisatir].Height
                      else
                        TempCurrentHeight := NcTable.DefaultRowHeight;
//                      TempCurrentHeight := If(NcTable.Row(Suankisatir).Height > 0, NcTable.Row(Suankisatir).Height, NcTable.DefaultRowHeight);
//                      ' Sayfa Yüksekliði geçiliyormu kontrol et
                      If CurrentRowIndex = CaptionRowCount - 1 Then
                      begin
                          BolunenCell := NcTable.Row.InnerList[Suankisatir].Cell.InnerList[CurrentCellIndex];
//                          ' Geildiði için hücre yi burada bitir ve hücrenin devamý olan hücreye yeni deðerleri ekle
//                          ' Aþaðý birleþme yi düzenle ve döngüden çýk
//                          Dim p1 As System.Reflection.FieldInfo := BolunenCell.GetType.GetField( _
//                          "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                          p1.SetValue(BolunenCell, CurrentCell.Index);
                          BolunenCell.Indexs := CurrentCell.Indexs;
                          BolunenCell.MergeAcross := CurrentCell.MergeAcross;
                          BolunenCell.StyleID := CurrentCell.StyleID;
                          BolunenCell.Data := CurrentCell.Data;
                          yenidown := Abs(CRow - CurrentCell.MergeDown);
                          BolunenCell.MergeDown := yenidown;
                          CurrentCell.MergeDown := Abs(CRow);
//                          'boyutlar metreye çevriliyor
                          CellVerticalLeng := ToM(CellVerticalLeng);
                          CellHorizontalLeng := ToM(CellHorizontalLeng);
                          objeadded := True;
                          AddObjeInNetcadObjList(NcObjelist, CurrentCell as Cell, CellVerticalLeng, CellHorizontalLeng, ref_y, ref_x);

                          If (CurrentCell.MergeAcross) > 0 Then
                              CurrentCellIndex := CurrentCellIndex + CurrentCell.MergeAcross;

                          ref_y := ref_y + CellVerticalLeng;
                          Break;//Exit For
                      End;
//                      ' Dikey yüksekliði kendisi ile topla
                      If(NcTable.Row.InnerList[Suankisatir].Height > 0) then
                        CellHorizontalLeng := CellHorizontalLeng + NcTable.Row.InnerList[Suankisatir].Height
                      else
                        CellHorizontalLeng := CellHorizontalLeng + NcTable.DefaultRowHeight;
//                      CellHorizontalLeng += If(NcTable.Row[Suankisatir].Height > 0, NcTable.Row[Suankisatir].Height, NcTable.DefaultRowHeight);
                  end;
                  If objeadded Then
                  begin
                      objeadded := False;
                      Inc(CurrentCellIndex);
                      Continue; //For;
                  End;
              End;
//              ' Hücre Kullanýlmayan Hücremi
          end
          Else
          begin
            If(NcTable.Column.InnerList[CurrentCellIndex].Width > 0) then
              ref_y := ref_y + ToM(NcTable.Column.InnerList[CurrentCellIndex].Width)
            else
              ref_y := ref_y + ToM(NcTable.DefaultColumnWidth);
//              ref_y += If(NcTable.Column[CurrentCellIndex].Width > 0, ToM(NcTable.Column[CurrentCellIndex].Width), ToM(NcTable.DefaultColumnWidth));

              If (CellHorizontalLeng = 0) And (CurrentCellIndex = CurrentRow.Cell.Count - 1) Then
                  CellHorizontalLeng := IfThen(CurrentRow.Height > 0, CurrentRow.Height, NcTable.DefaultRowHeight);
              If (CellVerticalLeng = 0) And (CurrentCellIndex = CurrentRow.Cell.Count - 1) Then
                  CellVerticalLeng := IfThen(NcTable.Column.InnerList[CurrentCellIndex].Width > 0, NcTable.Column.InnerList[CurrentCellIndex].Width, NcTable.DefaultColumnWidth);
          End;

//          'boyutlar metreye çevriliyor
          CellVerticalLeng := ToM(CellVerticalLeng);
          CellHorizontalLeng := ToM(CellHorizontalLeng);


//          ' Oluþturulan hücreyi listeye eklenmek üzere gönder
          AddObjeInNetcadObjList(NcObjelist, CurrentCell as Cell, CellVerticalLeng, CellHorizontalLeng, ref_y, ref_x);


          If (CurrentCell.MergeAcross) > 0 Then
              CurrentCellIndex := CurrentCellIndex + CurrentCell.MergeAcross;

          ref_y := ref_y + CellVerticalLeng;

          Inc(CurrentCellIndex);
      end;
      ref_y := CurY;
      ref_x := ref_x - IfThen(CurrentRow.Height > 0, ToM(CurrentRow.Height), ToM(NcTable.DefaultRowHeight));
  end;

//  ' Baþlýk Yüksekliðini Bulur
  BaslikYuksekligi := 0;
  For i := 0 To CaptionRowCount - 1 do
  begin
      BaslikYuksekligi := BaslikYuksekligi + IfThen(NcTable.Row.InnerList[i].Height > 0, NcTable.Row.InnerList[i].Height, NcTable.DefaultRowHeight);
  end;

  result := BaslikYuksekligi;
end;

function CreateNcTableOfXml.AddObjeInNetcadObjList(
  var NetcadObjList: IlicgEntityList; PCell: Cell; VerticalLeng, HorizontalLeng,
  ref_y, ref_x: Double): Integer;
var
  Styl, StylDefault: Style;
  border: BorderInt;//Lider.CG.ModulesCom.Table.Border.Border;
  VerticalUp, VerticalDown, HorizontalLeft, HorizontalRight, DiagonalLeft,
  Rectangnle, DiagonalRight, Text: IlicgEntity;
  YaziDayama: TextJustify;
  Coor, TempCoor: TlicgCoor3d;
  Strng: TStringList;//TStrings;
  i: Integer;
  CurFontStyles: TFontStyles;
  Coor2: TlicgCoor;
  TempD: Double;
  IndentHorizontalVarible: Double;
  IndentVerticalVarible: Double;
  //  border: Border;
begin
//  Tempd := ref_x;
//  ref_x := ref_y;
//  ref_y := Tempd;
//  Tempd := VerticalLeng;
//  VerticalLeng := HorizontalLeng;
//  HorizontalLeng := Tempd;

  If Not PCell.IsImaginary Then
  begin
    Styl := RealWorksBook.Styles.GetStyleByID[(PCell.StyleID)];
    StylDefault := RealWorksBook.Styles.GetStyleByID[('Default')];
    If (Styl = nil) Then
      Styl := StylDefault;

    //' ***********************Cerceve****************************
    For border In Styl.Borders do
    begin
        If border.Position.Name = (Top.Create).Name Then
        begin
            VerticalUp := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            VerticalUp.Geometry.Points.Add(ref_y,ref_x);
//            VerticalUp.Geometry.Points.Add(ref_x,ref_y);
//            VerticalUp.Geometry.Points.Y[0] := ref_y;
//            VerticalUp.Geometry.Points.X[0] := ref_x;
            VerticalUp.Geometry.Points.Add(ref_y + VerticalLeng,ref_x);
//            VerticalUp.Geometry.Points.Add(ref_x,ref_y + VerticalLeng);
//            VerticalUp.Geometry.Points.Y[1] := ref_y + VerticalLeng;
//            VerticalUp.Geometry.Points.X[1] := ref_x;
            If border.LineStyle.Name = ((Continuous.Create).Name) Then
                VerticalUp.DrawTools.PenTool.Style := 0
            Else
                VerticalUp.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(VerticalUp);
//            VerticalUp := nil;
        end
        Else If border.Position.Name = (Bottom.Create).Name Then
        begin
            //'alt
            VerticalDown := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            VerticalDown.Geometry.Points.Add(ref_y,ref_x - HorizontalLeng);
//            VerticalDown.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y);
//            VerticalDown.Geometry.Points.Y[0] := ref_y;
//            VerticalDown.Geometry.Points.X[0] := ref_x - HorizontalLeng;
            VerticalDown.Geometry.Points.Add(ref_y + VerticalLeng,ref_x - HorizontalLeng);
//            VerticalDown.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y + VerticalLeng);
//            VerticalDown.Geometry.Points.Y[1] := ref_y + VerticalLeng;
//            VerticalDown.Geometry.Points.X[1] := ref_x - HorizontalLeng;
            If border.LineStyle.Name = (Continuous.Create).Name Then
                VerticalDown.DrawTools.PenTool.Style := 0
            Else
                VerticalDown.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(VerticalDown);
//            VerticalDown := nil;
        end
        Else If border.Position.Name = (Left.Create).Name Then
        begin
            //'sol
            HorizontalLeft := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            HorizontalLeft.Geometry.Points.Add(ref_y,ref_x);
//            HorizontalLeft.Geometry.Points.Add(ref_x,ref_y);
//            HorizontalLeft.Geometry.Points.Y[0] := ref_y;
//            HorizontalLeft.Geometry.Points.X[0] := ref_x;
            HorizontalLeft.Geometry.Points.Add(ref_y,ref_x - HorizontalLeng);
//            HorizontalLeft.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y);
//            HorizontalLeft.Geometry.Points.Y[1] := ref_y;
//            HorizontalLeft.Geometry.Points.X[1] := ref_x - HorizontalLeng;
            If border.LineStyle.Name = (Continuous.Create).Name Then
                HorizontalLeft.DrawTools.PenTool.Style := 0
            Else
                HorizontalLeft.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(HorizontalLeft);
//            HorizontalLeft := nil;
        end
        Else If border.Position.Name = (Right.Create).Name Then
        begin
            //'sað
            HorizontalRight := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            HorizontalRight.Geometry.Points.Add(ref_y + VerticalLeng,ref_x);
//            HorizontalRight.Geometry.Points.Add(ref_x,ref_y + VerticalLeng);
//            HorizontalRight.Geometry.Points.Y[0] := ref_y + VerticalLeng;
//            HorizontalRight.Geometry.Points.X[0] := ref_x;
            HorizontalRight.Geometry.Points.Add(ref_y + VerticalLeng,ref_x - HorizontalLeng);
//            HorizontalRight.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y + VerticalLeng);
//            HorizontalRight.Geometry.Points.Y[1] := ref_y + VerticalLeng;
//            HorizontalRight.Geometry.Points.X[1] := ref_x - HorizontalLeng;
            If border.LineStyle.Name = (Continuous.Create).Name Then
                HorizontalRight.DrawTools.PenTool.Style := 0
            Else
                HorizontalRight.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(HorizontalRight);
//            HorizontalRight := nil;
        end
        Else If border.Position.Name = (Lider.CG.ModulesCom.Table.DiagonalLeft.DiagonalLeft.Create).Name Then
        begin
            //'Capraz Sol
            DiagonalLeft := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            DiagonalLeft.Geometry.Points.Add(ref_y,ref_x);
//            DiagonalLeft.Geometry.Points.Add(ref_x,ref_y);
//            DiagonalLeft.Geometry.Points.Y[0] := ref_y;
//            DiagonalLeft.Geometry.Points.X[0] := ref_x;
            DiagonalLeft.Geometry.Points.Add(ref_y + VerticalLeng,ref_x - HorizontalLeng);
//            DiagonalLeft.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y + VerticalLeng);
//            DiagonalLeft.Geometry.Points.Y[1] := ref_y + VerticalLeng;
//            DiagonalLeft.Geometry.Points.X[1] := ref_x - HorizontalLeng;
            If border.LineStyle.Name = (Continuous.Create).Name Then
                DiagonalLeft.DrawTools.PenTool.Style := 0
            Else
                DiagonalLeft.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(DiagonalLeft);
//            DiagonalLeft := nil;
        end
        Else If border.Position.Name = (Lider.CG.ModulesCom.Table.DiagonalRight.DiagonalRight.Create).Name Then
        begin
//            'Capraz sað
            DiagonalRight := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//' çizgi
            DiagonalRight.Geometry.Points.Add(ref_y + VerticalLeng,ref_x);
//            DiagonalRight.Geometry.Points.Add(ref_x,ref_y + VerticalLeng);
//            DiagonalRight.Geometry.Points.Y[0] := ref_y + VerticalLeng;
//            DiagonalRight.Geometry.Points.X[0] := ref_x;
            DiagonalRight.Geometry.Points.Add(ref_y,ref_x - HorizontalLeng);
//            DiagonalRight.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y);
//            DiagonalRight.Geometry.Points.Y[1] := ref_y;
//            DiagonalRight.Geometry.Points.X[1] := ref_x - HorizontalLeng;
            If border.LineStyle.Name = (Continuous.Create).Name Then
                DiagonalRight.DrawTools.PenTool.Style := 0
            Else
                DiagonalRight.DrawTools.PenTool.Style := 1;
            NcObjelist.Add(DiagonalRight);
//            DiagonalRight := nil;
        End;
    end;
//    'Kutu
    Rectangnle := Licad.CreateEntityFactory.MakeEntity(idRectangle,0,_2D);//' kutu
    Rectangnle := Licad.CreateEntityFactory.MakeRectangle(AsCoor(ref_y,ref_x - HorizontalLeng),AsCoor(ref_y + VerticalLeng,ref_x));
//    Tempd := HorizontalLeng;
//    if Tempd = 0 then
//      Tempd := NcTable.DefaultRowHeight;
//    Rectangnle.Geometry.Points.Add(ref_y,ref_x - HorizontalLeng); //yleri ayný gelince kutu oluþturmaz.
//    Rectangnle.Geometry.Points.Add(ref_x,ref_y + VerticalLeng);
//    Rectangnle.Geometry.Points.Y[1] := ref_y + VerticalLeng;
//    Rectangnle.Geometry.Points.X[1] := ref_x;
//    Rectangnle.Geometry.Points.Add(ref_y + VerticalLeng,ref_x);
//    Rectangnle.Geometry.Points.Add(ref_x - HorizontalLeng,ref_y);
//    Rectangnle.Geometry.Points.Y[0] := ref_y;
//    Rectangnle.Geometry.Points.X[0] := ref_x - HorizontalLeng;
    Rectangnle.Name := '';
    NcObjelist.Add(Rectangnle);
//    CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(Rectangnle);

//    '****************Yazi Dayama************************
//    YaziDayama As New Asistan.CadObj.Enums.TextJustify;
    If (Styl.Aligment.Horizontal.Name = 'Center') And (Styl.Aligment.Vertical.Name = 'Center') Then
        YaziDayama := TextJustify.OrtaOrta
    Else If (Styl.Aligment.Horizontal.Name = 'Center') And (Styl.Aligment.Vertical.Name = 'Bottom') Then
        YaziDayama := TextJustify.OrtaAlt
    Else If (Styl.Aligment.Horizontal.Name = 'Center') And (Styl.Aligment.Vertical.Name = 'Top') Then
        YaziDayama := TextJustify.OrtaUst
    Else If (Styl.Aligment.Horizontal.Name = 'Right') And (Styl.Aligment.Vertical.Name = 'Center') Then
        YaziDayama := TextJustify.SagOrta
    Else If (Styl.Aligment.Horizontal.Name = 'Right') And (Styl.Aligment.Vertical.Name = 'Bottom') Then
        YaziDayama := TextJustify.SagAlt
    Else If (Styl.Aligment.Horizontal.Name = 'Right') And (Styl.Aligment.Vertical.Name = 'Top') Then
        YaziDayama := TextJustify.SagUst
    Else If (Styl.Aligment.Horizontal.Name = 'Left') And (Styl.Aligment.Vertical.Name = 'Center') Then
        YaziDayama := TextJustify.SolOrta
    Else If (Styl.Aligment.Horizontal.Name = 'Left') And (Styl.Aligment.Vertical.Name = 'Bottom') Then
        YaziDayama := TextJustify.Solalt
    Else If (Styl.Aligment.Horizontal.Name = 'Left') And (Styl.Aligment.Vertical.Name = 'Top') Then
        YaziDayama := TextJustify.SolUst
    Else
        YaziDayama := TextJustify.OrtaOrta;
//    '**************** yazý dayama koordinatý***************
//    Coor := AsCoor3d(Rectangnle.Geometry.Extent.X1,Rectangnle.Geometry.Extent.Y1,0);
    IndentVerticalVarible := (Styl.Font.Size / 10 * 0.5) * Scale;
    IndentHorizontalVarible := (IndentVerticalVarible+(Styl.Aligment.Indent / 10))*Scale;
    Coor2 := FindinRectanleJustifCoor(Rectangnle.Geometry.Extent, IndentHorizontalVarible, IndentVerticalVarible, YaziDayama); //Styl.Aligment.Indent 0
    Coor := AsCoor3d(Coor2.X, Coor2.Y, 0);
//    Coor := Asistan.CadObj.StringFunctions.Text.FindinRectanleJustifCoor(Rectangnle.Limits, Styl.Aligment.Indent, 0, YaziDayama);
    Strng := nil;
    If Styl.Aligment.WrapText Then  //(VerticalLeng * 0.8)
    begin
      Strng := Split(GetFormattedValue(PCell, Styl), ' ', (VerticalLeng - (IndentHorizontalVarible*2)), StylDefault.Font.FontName.Name, IfThen(Styl.Font.Size = 0, ToSMm(StylDefault.Font.Size), ToSMm(Styl.Font.Size)), True)
//      Split(GetFormattedValue(Cell, Styl),' ',Strng)
//        Strng := Asistan.CadObj.StringFunctions.Text.Split(GetFormattedValue(Cell, Styl), ' ', VerticalLeng * 0.8, StylDefault.Font.FontName.Name, IfThen(Styl.Font.Size = 0, ToSMm(StylDefault.Font.Size), ToSMm(Styl.Font.Size)), True)
    end
    Else
    begin
        Strng := TStringList.Create;//TStrings.Create;//New List(Of String);
        if Styl = nil then
          Styl := StylDefault
        else
        begin
          Styl := Styl;
//        IfThen((Styl=nil), StylDefault, Styl);
          Strng.Add(GetFormattedValue(PCell, Styl));
        end;
    End;
    Rectangnle := nil;
    If Not (Strng = nil) Then
    begin
//        ' ************** yazý************************
        For i := 0 To Strng.Count - 1 do
        begin
            Text := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);//; //' yazý idText
            TempCoor := Get_CorrInSplitString(Coor, YaziDayama, IfThen(Styl.Font.Size = 0, ToSMm(StylDefault.Font.Size), ToSMm(Styl.Font.Size)), Strng.Count, i);
            Text.Geometry.Points.Add(TempCoor.X,TempCoor.Y,TempCoor.Z);
//            Text.Geometry.Points.X[0] := TempCoor.X;
//            Text.Geometry.Points.Y[0] := TempCoor.Y;
//            Text.Geometry.Points.Z[0] := TempCoor.Z;
             //            ' Text.s = GetFormattedValue(Cell, If(Isnil(Styl), StylDefault, Styl))
            Text.AsTextValue.Text := (Strng[i].TrimEnd([' '])).TrimStart([' ']);
            Text.DrawTools.FontTool.Height := IfThen(Styl.Font.Size = 0, ToSMm(StylDefault.Font.Size), ToSMm(Styl.Font.Size));
            Text.DrawTools.FontTool.TextPos := GetPosCad(YaziDayama);//Asc(getEnumDescription(YaziDayama));
            CurFontStyles := [];
            if Styl.Font.Italic then
              CurFontStyles := [TFontStyle.fsItalic];
            if (not (Styl.Font.Underline=nil)) and (Not String.IsNullOrEmpty(Styl.Font.Underline.Name)) then
              CurFontStyles := CurFontStyles + [TFontStyle.fsUnderline];

            Text.DrawTools.FontTool.Style := Text.DrawTools.FontTool.Style + CurFontStyles;
            //[Math.IfThen((Styl.Font.Italic=True), 1, 0), Math.IfThen(((Not (Styl.Font.Underline=nil)) And (Not String.IsNullOrEmpty(Styl.Font.Underline.Name))), fsUnderline, 0)];
            Text.DrawTools.FontTool.angle := IfThen(Styl.Aligment.Rotate > 0, (Styl.Aligment.Rotate * (PI * 2)) / 360, 0);// ' dereceden radyana
//            'If Styl.Font.Italic Then
//            '    Text.flags += 1
//            'End If
//            'If Not Isnil(Styl.Font.Underline) Then
//            '    Text.flags += 2
//            'End If
            If Not String.IsNullOrEmpty(Text.AsTextValue.Text) Then
                NcObjelist.Add(Text);
        end;
    End;
  End;
  result := NetcadObjList.Count;
end;

function CreateNcTableOfXml.Get_CorrInSplitString(Refcorr: TLicgCoor3d;
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

procedure CreateNcTableOfXml.CreateNcTabloColums(ShetIndex: Integer);
var
  i, ii, iii: Integer;
  aa: ColInt;
begin
  For i := 0 To RealWorksBook.Sheet[(ShetIndex)].Table.ExpandedColumnCount - 1 do
  begin
      NcTable.Column.Add(Col.Create);
  end;

  ii := 0;
  For aa In RealWorksBook.Sheet[(ShetIndex)].Table.Column.InnerList do
  begin
      If aa.Indexs = 0 Then
      begin
          NcTable.Column.InnerList[ii].AutoFitWidth := aa.AutoFitWidth;
//          Dim p1 As System.Reflection.FieldInfo := NcTable.Column(ii).GetType.GetField( _
//                   "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//          p1.SetValue(NcTable.Column(ii), aa.Index)
          NcTable.Column.InnerList[ii].Indexs := aa.Indexs;
          NcTable.Column.InnerList[ii].Span := aa.Span;
          NcTable.Column.InnerList[ii].StyleID := aa.StyleID;
          NcTable.Column.InnerList[ii].Width := aa.Width;
          ii := ii + 1;
          If aa.Span > 0 Then
          begin
              For iii := ii To ii + Round(aa.Span) - 1 do //CInt
              begin
                  NcTable.Column.InnerList[iii].AutoFitWidth := aa.AutoFitWidth;
//                  Dim p2 As System.Reflection.FieldInfo = NcTable.Column(iii).GetType.GetField( _
//                  "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                  p2.SetValue(NcTable.Column(iii), -1)
                  NcTable.Column.InnerList[iii].Indexs := -1;
                  NcTable.Column.InnerList[iii].Span := 0;
                  NcTable.Column.InnerList[iii].StyleID := aa.StyleID;
                  NcTable.Column.InnerList[iii].Width := aa.Width;
              end;
              ii := ii + Round(aa.Span);//CInt
          End;
      end
      Else If aa.Indexs > 0 Then
      begin
          Try
              NcTable.Column.InnerList[(aa.Indexs - 1)].AutoFitWidth := aa.AutoFitWidth;
//              Dim p1 As System.Reflection.FieldInfo := NcTable.Column(aa.Index - 1).GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//              p1.SetValue(NcTable.Column(aa.Index - 1), aa.Index)
              NcTable.Column.InnerList[(aa.Indexs - 1)].Indexs := aa.Indexs;
              NcTable.Column.InnerList[(aa.Indexs - 1)].Span := aa.Span;
              NcTable.Column.InnerList[(aa.Indexs - 1)].StyleID := aa.StyleID;
              NcTable.Column.InnerList[(aa.Indexs - 1)].Width := aa.Width;
              ii := (aa.Indexs - 1) + 1;
              If aa.Span > 0 Then
              begin
                  For iii := ii To ii + aa.Span - 1 do
                  begin
                      NcTable.Column.InnerList[iii].AutoFitWidth := aa.AutoFitWidth;
//                      Dim p2 As System.Reflection.FieldInfo := NcTable.Column(iii).GetType.GetField( _
//                      "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//                      p2.SetValue(NcTable.Column(iii), -1)
                      NcTable.Column.InnerList[iii].Indexs := -1;
                      NcTable.Column.InnerList[iii].Span := 0;
                      NcTable.Column.InnerList[iii].StyleID := aa.StyleID;
                      NcTable.Column.InnerList[iii].Width := aa.Width;
                  end;
                  ii := ii + Round(aa.Span);//CInt
              End;
          except on e: Exception do
              Application.MessageBox(PWideChar('- Hata: ' + e.Message),'Hata',Mb_ok);
          End;
      End;
  end;
end;

procedure CreateNcTableOfXml.CreateNcTabloRows(ShetIndex: Double);
var
  i, ii, iii, iiii, CurrentTrow: Integer;
  aa: RowInt;
  bb: CellInt;
  Rows: Row;
begin
  For i := 0 To RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedRowCount - 1 do
  begin
      Rows := Row.Create;
      For ii := 0 To RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedColumnCount - 1 do
      begin
          Rows.Cell.Add(Cell.Create);
//          Dim p1 As System.Reflection.FieldInfo = Row.Cell(ii).GetType.GetField( _
//          "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//          p1.SetValue(Row.Cell(ii), -1)
        Rows.Cell.InnerList[ii].Indexs := -1;
      end;
      NcTable.Row.Add(Rows);
  end;

  iii := 0;
  CurrentTrow := 0;
  For aa In RealWorksBook.Sheet[Trunc(ShetIndex)].Table.Row.InnerList do
  begin
      If aa.Indexs <= 0 Then
      begin
          NcTable.Row.InnerList[(CurrentTrow)].AutoFitHeight := aa.AutoFitHeight;
          NcTable.Row.InnerList[(CurrentTrow)].Height := aa.Height;
          NcTable.Row.InnerList[(CurrentTrow)].Indexs := aa.Indexs;
//          Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).GetType.GetField( _
//          "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//          p1.SetValue(NcTable.Row(CurrentTrow), aa.Index)
      end
      Else If aa.Indexs > 0 Then
      begin
          CurrentTrow := Round(aa.Indexs) - 1; //CInt
          NcTable.Row.InnerList[(CurrentTrow)].AutoFitHeight := aa.AutoFitHeight;
          NcTable.Row.InnerList[(CurrentTrow)].Height := aa.Height;
          NcTable.Row.InnerList[(CurrentTrow)].Indexs := aa.Indexs;
//          Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).GetType.GetField( _
//          "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//          p1.SetValue(NcTable.Row(CurrentTrow), aa.Index)
      End;


      iiii := 0;
      For bb In RealWorksBook.Sheet[Trunc(ShetIndex)].Table.Row.InnerList[(iii)].Cell.InnerList do
      begin
          If (bb.MergeAcross = 0) And (bb.Indexs <= 0) Then
          begin
//              Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).Cell(iiii).GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//              p1.SetValue(NcTable.Row(CurrentTrow).Cell(iiii), bb.Index)
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Indexs := bb.Indexs;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Data.Value := bb.Data.Value;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeAcross := bb.MergeAcross;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeDown := bb.MergeDown;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].StyleID := bb.StyleID;
              iiii := iiii + 1;
              If iiii > RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedColumnCount - 1 Then
                Break;//Exit For
          end
          Else If (bb.MergeAcross > 0) And (bb.Indexs > 0) Then
          begin
              iiii := Round(bb.Indexs) - 1; //CInt
//              Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).Cell(iiii).GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//              p1.SetValue(NcTable.Row(CurrentTrow).Cell(iiii), bb.Index)
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Indexs := bb.Indexs;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Data.Value := bb.Data.Value;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeAcross := bb.MergeAcross;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeDown := bb.MergeDown;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].StyleID := bb.StyleID;
              iiii := iiii + bb.MergeAcross + 1;
              If iiii > RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedColumnCount - 1 Then
                Break;//Exit For;
          end
          Else If bb.Indexs > 0 Then
          begin
              iiii := Round(bb.Indexs) - 1;//CInt
//              Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).Cell(iiii).GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//              p1.SetValue(NcTable.Row(CurrentTrow).Cell(iiii), bb.Index)
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Indexs := bb.Indexs;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Data.Value := bb.Data.Value;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeAcross := bb.MergeAcross;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeDown := bb.MergeDown;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].StyleID := bb.StyleID;
              iiii := bb.Indexs;
              If iiii > RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedColumnCount - 1 Then
                Break;//Exit For
          end
          Else If bb.MergeAcross > 0 Then
          begin
//              Dim p1 As System.Reflection.FieldInfo = NcTable.Row(CurrentTrow).Cell(iiii).GetType.GetField( _
//              "_Index", Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic)
//              p1.SetValue(NcTable.Row(CurrentTrow).Cell(iiii), bb.Index)
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Indexs := bb.Indexs;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].Data.Value := bb.Data.Value;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeAcross := bb.MergeAcross;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].MergeDown := bb.MergeDown;
              NcTable.Row.InnerList[(CurrentTrow)].Cell.InnerList[(iiii)].StyleID := bb.StyleID;
              iiii := iiii + Round(bb.MergeAcross) + 1; //CInt
              If iiii > (RealWorksBook.Sheet[Trunc(ShetIndex)].Table.ExpandedColumnCount - 1) Then
                Break;//Exit For
          End;
      end;
      iii := iii + 1;
      CurrentTrow := CurrentTrow + 1;
  end;
end;

function CreateNcTableOfXml.GetFormattedValue(Cell: Cell; Style: Style): String;
var
  value: Double;
  value1: TDateTime;
  resultGetFormattedValue: String;
  fs: TFormatSettings;
begin
  resultGetFormattedValue := String.Empty;
  If (String.IsNullOrEmpty(Cell.Data.Value)) Then
    result := '';

  If Cell.Data.Types = 'Number' Then //yyyy-mm-ddThh:nn:ss.msmsms
  begin
      If (Not (Style.NumberFormat = nil)) And Not String.IsNullOrEmpty(Style.NumberFormat.Format) Then
      begin
          If IsNumeric(Cell.Data.Value) Then
          begin
//            value.ToString(System.Globalization.CultureInfo)
              value := StrToFloat(Cell.Data.Value);
              If (Style.NumberFormat.Format = 'Fixed') Or
                 (Style.NumberFormat.Format = 'Standard') Then
                  resultGetFormattedValue := Format('%f',[(value)]) //value.ToString('f', CultureInfo.InvariantCulture).ToString
              Else If Style.NumberFormat.Format = 'General' Then
                  resultGetFormattedValue := Cell.Data.Value
              Else If Style.NumberFormat.Format = '@' Then
                  resultGetFormattedValue := Cell.Data.Value
              Else      //value.ToString(Style.NumberFormat.Format);//
                  resultGetFormattedValue := formatfloat(Style.NumberFormat.Format,(value));//value.ToString(Style.NumberFormat.Format, CultureInfo.InvariantCulture).ToString;

          end
          Else
              resultGetFormattedValue := Cell.Data.Value;
      end
      Else
          resultGetFormattedValue := Cell.Data.Value;
  end
  Else If Cell.Data.Types = 'DateTime' Then
  begin
    fs := TFormatSettings.Create;
    fs.DateSeparator := '-';
    fs.ShortDateFormat := 'yyyy-MM-dd';
    fs.TimeSeparator := ':';
    fs.ShortTimeFormat := 'hh:mm';
    fs.LongTimeFormat := 'hh:mm:ss';
      value1 := StrToDateTime(Cell.Data.Value,fs);
      resultGetFormattedValue := DateTimeToStr(value1);//Format('%d',[Trunc(value1)]);//value1.ToString('d', CultureInfo.InvariantCulture).ToString;
//    DateTimeToString(resultGetFormattedValue,Style.NumberFormat.Format,value1);
  end
  Else If Cell.Data.Types = 'String' Then
  begin
      If (Not (Style.NumberFormat=nil)) And (Not String.IsNullOrEmpty(Style.NumberFormat.Format)) Then
      begin
          If IsNumeric(Cell.Data.Value) Then
          begin
              value := StrToFloat(Cell.Data.Value);
              If (Style.NumberFormat.Format = 'Fixed') Or
                 (Style.NumberFormat.Format = 'Standard') Then
                  resultGetFormattedValue := Format('%f',[(value)])//value.ToString('f', CultureInfo.InvariantCulture).ToString;
              Else If Style.NumberFormat.Format = 'General' Then
                  resultGetFormattedValue := Cell.Data.Value
              Else If Style.NumberFormat.Format = '@' Then
                  resultGetFormattedValue := Cell.Data.Value
              Else
                  resultGetFormattedValue := Format('%'+Style.NumberFormat.Format+'s',[(value)]);//value.ToString(Style.NumberFormat.Format, CultureInfo.InvariantCulture).ToString;
          end
          Else
              resultGetFormattedValue := Cell.Data.Value;
      end
      Else
          resultGetFormattedValue := Cell.Data.Value;

      resultGetFormattedValue := Cell.Data.Value;
  end
  Else
      resultGetFormattedValue := Cell.Data.Value;

  result := resultGetFormattedValue;
end;

function CreateNcTableOfXml.ToSMm(Value: Single): Single;
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

function CreateNcTableOfXml.ToM(Value: Single): Single;
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
function CreateNcTableOfXml.Split(SourceStr: string; Delimiter: Char; ListOfStrings:TStrings): string;
begin
  if ListOfStrings = nil then
    ListOfStrings:= TStringList.Create;

  ListOfStrings.Clear;
  ListOfStrings.Delimiter       := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText   := SourceStr;
  Result := ListOfStrings.Text;
end;

function CreateNcTableOfXml.StringListFromStrings(const Strings: array of string): TStringList;
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
function CreateNcTableOfXml.Split(SourceStr: String; ExplitCapture: Boolean): TStringList;// As List(Of String)
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
Function CreateNcTableOfXml.Split(SourceStr: String; Pattern: String; ExplitCapture: Boolean): TStringList;
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
Function CreateNcTableOfXml.Split(SourceStr: String; ExplitCapture: Boolean; Pattern: String): TStringList;
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
Function CreateNcTableOfXml.Split(SourceStr: String; Count: Integer; IsNetcadText: Boolean): TStringList;
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
Function CreateNcTableOfXml.Split(SourceStr: String; Pattern: String; MaxSplitCount: Integer): TStringList;
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
Function CreateNcTableOfXml.Split(SourceStr: String; Pattern: String; Widht: Single; AFontTool: IlicgFontTool; IsNetcadText: Boolean): TStringList;
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
          TempEnt := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);
          TempEnt.AsTextValue.Text := Format('%s%s', [CString, txt]);
          TempEnt.DrawTools.FontTool.Height := AFontTool.Height;
          TempEnt.Geometry.Points.Add(AsCoor(0,0));
//          TempEnt := Licad.CreateEntityFactory.MakeText(AsCoor(0,0),,Font.Height,0);
          uzunluk := Abs(TempEnt.Geometry.Extent.LowerLeft.X - TempEnt.Geometry.Extent.UpperRight.X);
            {
            LBmp := TBitmap.Create;
            try
//                      System.Drawing.Font('Arial', (_Yaziboyu / Olcek))
              LBmp.Canvas.Font.name := Font.Name;// 'Arial';
//              LBmp.Canvas.Font.Height := Round(_YaziBoyu * Olcek);
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
Function CreateNcTableOfXml.Split(SourceStr: String; Pattern: String; Widht: Single; FontName: String; FontSize: Single; IsNetcadText: Boolean): TStringList;
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
Function CreateNcTableOfXml.FindinRectanleJustifCoor(Limit: TlicgExtent; VerticalIndent: Double; HorizontalIndent: Double; Just: TextJustify): TlicgCoor;
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

initialization
  ref_y := 0;
  ref_x := 0;


end.

