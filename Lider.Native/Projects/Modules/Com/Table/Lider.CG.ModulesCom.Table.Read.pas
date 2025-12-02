unit Lider.CG.ModulesCom.Table.Read;

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
  Vcl.Forms,
  System.Generics.Collections,
  SysUtils,
  StrUtils,
  XMLIntf, XMLDoc;

type
  Read = class
    private
      _FileName: String;
      Book: WorksBook;

      procedure Read;
      function ToSMm(Value: String): String;
      function ToM(Value: String): String;
    public
      constructor Create(wb: WorksBook); overload;
      constructor Create(wb: WorksBook; FileName: String); overload;
  end;

implementation
{ Read }

constructor Read.Create(wb: WorksBook);
var
  fil: TOpenDialog;
begin
  fil := TOpenDialog.Create(nil);
  try
    fil.Filter := 'Xml Elektronik Tablo 2003/*.xml';
    fil.Title := 'Okunacak Xml Elektronik Tablosu 2003';
    fil.FileName := '';
    if fil.Execute then //Handle
    begin
      if fil.FileName = '' then
        Exit
      else
      begin
        _FileName := fil.FileName;
        Book := wb;
        Read;
      end;
    end;
  finally
    FreeAndNil(fil);
  end;

//  Dim fil As New Asistan.FileOperation.IO
//  fil.OpenDialog("", 1, New List(Of String)({"Xml Elektronik Tablo 2003/*.xml"}), "Okunacak Xml Elektronik Tablosu 2003")
//
//  If fil.DosyaAdi = String.Empty Then
//      Exit Sub
//  Else
//      _FileName = fil.TamAdi
//      fil = Nothing
//      Book = wb
//      Read
//  End If
end;

constructor Read.Create(wb: WorksBook; FileName: String);
begin
  _FileName := FileName;
  Book := wb;
  Read;
end;

procedure Read.Read;
var
  xmldoc: IXmlDocument;
  xmlnode: IXmlNodeList;
  i, ShettCount: Integer;
  str: String;
  fs: TFileStream;
  AraNode, AnaNode, AtritubesNode, ChildNode, AtritubesNode1, ChildNodes, ChildNode1, Attributes, TableNode, CellAtritubesNode: IXmlNode;
  sty: Style;
  VerticalAligment, HorizontalAligment: AligmentStyle;
  Rotate: Integer;
  Indent: Integer;
  WrapText: Boolean;
  Font: Lider.CG.ModulesCom.Table.EnumsFont.Font;
  FontFamily: Lider.CG.ModulesCom.Table.EnumsFont.FontFamily;
  FontUnderline: Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline;
  FontVerticalAligment: Lider.CG.ModulesCom.Table.EnumsFont.FontVerticalAlign;
  Size, Weight: Single;
  Color, LineStyle, Name, Refersto: String;
  Italic, Bold: Boolean;
  sht: Worksheet;
  CountLoop, CountLoop1, CountLoop2, CountLoop3, CountLoop4, CountLoop5: Integer;
  Clm: Col;
  Rw: Row;
  cll: Cell;
  OuterRoot: IXmlNode;
  CountOuter: Integer;
  ReadingOrderType: ReadingOrderStyle;
begin
  CountOuter := 0;
  CountLoop := 0;
  CountLoop1 := 0;
  CountLoop2 := 0;
  CountLoop3 := 0;
  CountLoop4 := 0;
  CountLoop5 := 0;

  xmldoc := TXmlDocument.Create('');
  xmlnode := TXmlNodeList.Create(nil,'',nil);
  fs := TFileStream.Create(_FileName,fmOpenRead);
  //TFileMode.fmOpen, TFileAccess.faRead, TFileShare.fsReadWrite]);
  xmldoc.LoadFromStream(fs);

  OuterRoot := xmldoc.DocumentElement; //.DOMDocument.getElementsByTagName('Style') as IXmlNodeList;
  for CountOuter := 0 to OuterRoot.ChildNodes.Count - 1 do
  begin
    if OuterRoot.ChildNodes[CountOuter].NodeName='Styles' then
    begin
      AraNode := OuterRoot.ChildNodes[CountOuter];
      for CountLoop := 0 to AraNode.ChildNodes.Count - 1 do
      begin
        AnaNode := AraNode.ChildNodes[CountLoop];
        sty := Style.Create;
        sty.Name := '';
        sty.Id := '';
        For CountLoop1 := 0 to AnaNode.AttributeNodes.Count - 1 do
        begin
          AtritubesNode := AnaNode.AttributeNodes[CountLoop1];
            If AtritubesNode.LocalName = 'ID' Then
                sty.Id := AtritubesNode.Text; //InnerText
            If AtritubesNode.LocalName = 'Name' Then
                sty.Name := AtritubesNode.Text; //InnerText
        end;

        For CountLoop1 := 0 to AnaNode.ChildNodes.Count - 1 do
        begin
          ChildNode := AnaNode.ChildNodes[CountLoop1];
            If ChildNode.LocalName = 'Alignment' Then
            begin
                VerticalAligment := AligmentStyle.Bottom;
                HorizontalAligment := AligmentStyle.Left;
                Rotate := 0;
                WrapText := False;
                For CountLoop2 := 0 to ChildNode.AttributeNodes.Count - 1 do
                begin
                    AtritubesNode := ChildNode.AttributeNodes[CountLoop2];
                    If AtritubesNode.LocalName = 'Vertical' Then
                    begin
                        Case ansiindexstr(AtritubesNode.Text, ['Bottom', 'Top', 'Left', 'Right', 'Center', 'Justify', 'Distributed', 'Fill', 'CenterAcrossSelection']) of// InnerText
                            0:
                                VerticalAligment := AligmentStyle.Bottom;
                            1:
                                VerticalAligment := AligmentStyle.Top;
                            2:
                                VerticalAligment := AligmentStyle.Bottom;
                            3:
                                VerticalAligment := AligmentStyle.Bottom;
                            4:
                                VerticalAligment := AligmentStyle.Center;
                            5:
                                VerticalAligment := AligmentStyle.Justify;
                            6:
                                VerticalAligment := AligmentStyle.Distributed;
                            7:
                                VerticalAligment := AligmentStyle.Fill;
                            8:
                                VerticalAligment := AligmentStyle.CenterAcrossSelection;
                            Else
                                VerticalAligment := AligmentStyle.Bottom;
                        End;
                    End;
                    If AtritubesNode.LocalName = 'Horizontal' Then
                    begin
                        Case ansiindexstr(AtritubesNode.Text, ['Bottom', 'Top', 'Left', 'Right', 'Center', 'Justify', 'Distributed', 'Fill', 'CenterAcrossSelection']) of// InnerText
                            0:
                                HorizontalAligment := AligmentStyle.Left;
                            1:
                                HorizontalAligment := AligmentStyle.Left;
                            2:
                                HorizontalAligment := AligmentStyle.Left;
                            3:
                                HorizontalAligment := AligmentStyle.Right;
                            4:
                                HorizontalAligment := AligmentStyle.Center;
                            5:
                                HorizontalAligment := AligmentStyle.Justify;
                            6:
                                HorizontalAligment := AligmentStyle.Distributed;
                            7:
                                HorizontalAligment := AligmentStyle.Fill;
                            8:
                                HorizontalAligment := AligmentStyle.CenterAcrossSelection;
                            Else
                                HorizontalAligment := AligmentStyle.Left;
                        End;
                    End;
                    If AtritubesNode.LocalName = 'Indent' Then
                        Indent := StrToInt(AtritubesNode.Text);//InnerText
                    If AtritubesNode.LocalName = 'ReadingOrder' Then
                    begin
                      case ansiindexstr(AtritubesNode.Text, ['LeftToRight', 'RightToLeft']) of
                        0:  ReadingOrderType := ReadingOrderStyle.LeftToRight;
                        1:  ReadingOrderType := ReadingOrderStyle.RightToLeft;
                        else
                          ReadingOrderType := ReadingOrderStyle.NoneReadingOrderStyle;
                      end;
                    end;
                    If AtritubesNode.LocalName = 'Rotate' Then
                        Rotate := StrToInt(AtritubesNode.Text);//InnerText
                    If AtritubesNode.LocalName = 'WrapText' Then
                        WrapText := StrToBool(AtritubesNode.Text);//InnerText
                end;
                sty.Aligment := Alignment.Create(HorizontalAligment, VerticalAligment, Indent, ReadingOrderType, Rotate, False, WrapText);
            End;
            If ChildNode.LocalName = 'Font' Then
            begin
                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.Arial;
                FontFamily := Lider.CG.ModulesCom.Table.EnumsFont.FontFamily.Roman;
                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.NoneFontUnderline;
                FontVerticalAligment := Lider.CG.ModulesCom.Table.EnumsFont.FontVerticalAlign.NoneFontVerticalAlign;
                Size := 0;
                Color := String.Empty;
                Italic := False;
                Bold := False;

                For CountLoop2 := 0 to ChildNode.AttributeNodes.Count - 1 do
                begin
                  AtritubesNode := ChildNode.AttributeNodes[CountLoop2];
                    If AtritubesNode.LocalName = 'FontName' Then
                    begin
                        Case ansiindexstr(AtritubesNode.Text, ['Arial', 'Arial Black', 'Calibri', 'Times New Roman']) of// InnerText
                            0:
                                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.Arial;
                            1:
                                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.ArialBlack;
                            2:
                                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.Calibri;
                            3:
                                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.TimesNewRoman;
                            Else
                                Font := Lider.CG.ModulesCom.Table.EnumsFont.Font.Calibri;
                        End;
                    End;
                    If AtritubesNode.LocalName = 'Size' Then
                        Size := StrToFloat(ToSMm(AtritubesNode.Text));//InnerText
                    If AtritubesNode.LocalName = 'Color' Then
                        Color := AtritubesNode.Text;//InnerText
                    If AtritubesNode.LocalName = 'Italic' Then
                        Italic := StrToBool(AtritubesNode.Text);//InnerText
                    If AtritubesNode.LocalName = 'Bold' Then
                        Bold := StrToBool(AtritubesNode.Text);//InnerText
                    If AtritubesNode.LocalName = 'Underline' Then
                    begin
                        Case ansiindexstr(AtritubesNode.Text, ['Double', 'DoubleAccounting', 'SingleAccounting', 'Single']) of// InnerText
                            0:
                                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.Doubles;
                            1:
                                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.DoubleAccounting;
                            2:
                                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.SingleAccounting;
                            3:
                                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.Singles;
                            Else
                                FontUnderline := Lider.CG.ModulesCom.Table.EnumsFont.FontUnderline.NoneFontUnderline;
                        End;
                    End;
                end;
    //            ' eðer font için deðer verilmemiþse varsayýlan olarak 10
    //            ' If String.Empty = sty.Fonts.Size Then sty.Fonts.Size = ToSMm(FountSize)
                sty.Font := Fonts.Create(Font, 162, FontFamily, Size, Color, Bold, Italic, False, FontVerticalAligment, FontUnderline)

            End;
            If ChildNode.LocalName = 'NumberFormat' Then
            begin
                For CountLoop2 := 0 to ChildNode.AttributeNodes.Count - 1 do
                begin
                  AtritubesNode := ChildNode.AttributeNodes[CountLoop2];
                    If AtritubesNode.LocalName = 'Format' Then
                    begin
                        sty.NumberFormat := NumberFormat.Create(AtritubesNode.Text.Replace('\+','+'));//InnerText

                    end;
                end;
            end;

            If ChildNode.LocalName = 'Borders' Then
            begin
                For CountLoop2 := 0 to ChildNode.ChildNodes.Count - 1 do
                begin
                  ChildNodes := ChildNode.ChildNodes[CountLoop2];
                    For CountLoop3 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                    begin
                       AtritubesNode := ChildNodes.AttributeNodes[CountLoop3];
                        If AtritubesNode.LocalName = 'Position' Then
                        begin
                            If AtritubesNode.Text = 'DiagonalLeft' Then //innerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('DiagonalLeft', LineStyle, Trunc(Weight), Color));

                            End;

                            If AtritubesNode.Text = 'DiagonalRight' Then //InnerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('DiagonalRight', LineStyle, Trunc(Weight), Color));
                            End;

                            If AtritubesNode.Text = 'Right' Then //InnerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('Right', LineStyle, Trunc(Weight), Color));
                            End;

                            If AtritubesNode.Text = 'Left' Then //InnerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('Left', LineStyle, Trunc(Weight), Color));
                            End;

                            If AtritubesNode.Text = 'Top' Then //InnerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('Top', LineStyle, Trunc(Weight), Color));
                            End;

                            If AtritubesNode.Text = 'Bottom' Then //InnerText
                            begin
                                Weight := 0;
                                LineStyle := String.Empty;
                                Color := String.Empty;
                                For CountLoop4 := 0 to ChildNodes.AttributeNodes.Count - 1 do
                                begin
                                  AtritubesNode1 := ChildNodes.AttributeNodes[CountLoop4];
                                    If AtritubesNode1.LocalName = 'LineStyle' Then
                                        LineStyle := AtritubesNode1.Text; //InnerText
                                    If AtritubesNode1.LocalName = 'Weight' Then
                                        Weight := StrToFloat(AtritubesNode1.Text); //InnerText
                                    If AtritubesNode1.LocalName = 'Color' Then
                                        Color := AtritubesNode1.Text; //InnerText
                                end;
                                sty.AddBorder(Border.Create('Bottom', LineStyle, Trunc(Weight), Color));
                            End;
                        End;

                    end;
                end;
            End;
        end;
        Book.Styles.Add(sty);
        sty := nil;
      end;
    end
//  xmlnode := xmldoc.DOMDocument.getElementsByTagName('Worksheet') as IXmlNodeList;
//  xmlnode := xmldoc.GetElementsByTagName('Worksheet');
    else if OuterRoot.ChildNodes[CountOuter].NodeName='Worksheet' then
    begin
      AnaNode := OuterRoot.ChildNodes[CountOuter];
  //    AnaNode := xmlnode[CountLoop];
      sht := Worksheet.Create;
      For CountLoop2 := 0 to AnaNode.AttributeNodes.Count - 1 do
      begin
        Attributes := AnaNode.AttributeNodes[CountLoop2] ;
          If Attributes.LocalName = 'Name' Then
              sht.Name := Attributes.Text; //innerText
      end;
      For CountLoop2 := 0 to AnaNode.ChildNodes.Count - 1 do
      begin
        ChildNode := AnaNode.ChildNodes[CountLoop2];
          If ChildNode.LocalName = 'Names' Then
          begin
              For CountLoop3 := 0 to ChildNode.ChildNodes.Count - 1 do
              begin
                ChildNode1 := ChildNode.ChildNodes[CountLoop3];
                  If ChildNode1.LocalName = 'NamedRange' Then
                  begin
                      Name := String.Empty;
                      Refersto := String.Empty;
                      For CountLoop4 := 0 to ChildNode1.AttributeNodes.Count - 1 do
                      begin
                        AtritubesNode := ChildNode1.AttributeNodes[CountLoop4];
                          If AtritubesNode.LocalName = 'Name' Then
                              Name := AtritubesNode.Text; //innerText
                          If AtritubesNode.LocalName = 'RefersTo' Then
                              Refersto := AtritubesNode.Text; //innerText
                      end;
                      if sht.NamedRange = nil then
                        sht.NamedRange := TList<NamedRange>.create;
                      sht.NamedRange.Add(NamedRange.Create(Refersto, Name));
                  End;
              end;
          End;

          If ChildNode.LocalName = 'Table' Then
          begin
          
              For CountLoop3 := 0 to ChildNode.AttributeNodes.Count - 1 do
              begin
                AtritubesNode := ChildNode.AttributeNodes[CountLoop3];
  //                'If AtritubesNode.LocalName := 'ExpandedColumnCount' Then
  //                '    sht.Table.ExpandedColumnCount := CInt(AtritubesNode.InnerText)
  //                'End If
                  If AtritubesNode.LocalName = 'StyleID' Then
                      sht.Table.StyleID := AtritubesNode.Text; //innerText
  //                'If AtritubesNode.LocalName := 'ExpandedRowCount' Then
  //                '    sht.Table.ExpandedRowCount := AtritubesNode.InnerText
  //                'End If
                  If AtritubesNode.LocalName = 'FullColumns' Then
                      sht.Table.FullColumns := StrToBool(AtritubesNode.Text); //innerText
                  If AtritubesNode.LocalName = 'FullRows' Then
                      sht.Table.FullRows := StrToBool(AtritubesNode.Text); //innerText
                  If AtritubesNode.LocalName = 'DefaultColumnWidth' Then
                      sht.Table.DefaultColumnWidth := StrToFloat(ToM(AtritubesNode.Text)); //innerText
                  If AtritubesNode.LocalName = 'DefaultRowHeight' Then
                      sht.Table.DefaultRowHeight := StrToFloat(ToM(AtritubesNode.Text)); //innerText
              end;



              For CountLoop3 := 0 to ChildNode.ChildNodes.Count - 1 do
              begin
                TableNode := ChildNode.ChildNodes[CountLoop3];
                  If TableNode.LocalName = 'Column' Then
                  begin
                      Clm := Col.Create;
                    
                      For CountLoop4 := 0 to TableNode.AttributeNodes.Count - 1 do
                      begin
                        AtritubesNode := TableNode.AttributeNodes[CountLoop4];
                          If AtritubesNode.LocalName = 'AutoFitWidth' Then
                              Clm.AutoFitWidth := StrToBool(ToM(AtritubesNode.Text)); //innerText
                          If AtritubesNode.LocalName = 'Width' Then
                              Clm.Width := StrToFloat(ToM(AtritubesNode.Text)); //innerText
                          If AtritubesNode.LocalName = 'StyleID' Then
                              Clm.StyleID := AtritubesNode.Text; //innerText
                          If AtritubesNode.LocalName = 'Index' Then
                          begin
                            Clm.Indexs := Round(StrToInt(AtritubesNode.Text));
  //                            Dim p1 As System.Reflection.FieldInfo := Clm.GetType.GetField('_Index',;
  //                            Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic);

  //                            p1.SetValue(Clm, CInt(AtritubesNode.InnerText.ToString));

  //                            '  Clm.Index := AtritubesNode.InnerText
                          End;
                          If AtritubesNode.LocalName = 'Span' Then
                              Clm.Span := StrToInt(AtritubesNode.Text); //innerText
                          If AtritubesNode.LocalName = 'Hidden' Then
                              Clm.Hidden := StrToBool(AtritubesNode.Text); //innerText
                      end;
                      sht.Table.Column.Add(Clm);
                      Clm := nil;
                  End;
                  If TableNode.LocalName = 'Row' Then
                  begin
                      Rw := Row.Create;

                      For CountLoop4 := 0 to TableNode.AttributeNodes.Count - 1 do
                      begin
                          AtritubesNode := TableNode.AttributeNodes[CountLoop4];
                          If AtritubesNode.LocalName = 'AutoFitHeight' Then
                              Rw.AutoFitHeight := StrToBool(ToM(AtritubesNode.Text)); //innerText
                          If AtritubesNode.LocalName = 'Height' Then
                              Rw.Height := StrToFloat(ToM(AtritubesNode.Text)); //innerText
                          If AtritubesNode.LocalName = 'Index' Then
                          begin
                            Rw.Indexs := Round(StrToInt(AtritubesNode.Text));
  //                            Dim p2 As System.Reflection.FieldInfo := Rw.GetType.GetField( _
  //                           '_Index', Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic);
  //                            p2.SetValue(Rw, CInt(AtritubesNode.Text));  //innerText
                          End;
                      end;

                      For CountLoop4 := 0 to TableNode.ChildNodes.Count - 1 do
                      begin
                         ChildNode1 := TableNode.ChildNodes[CountLoop4];
                          If ChildNode1.LocalName = 'Cell' Then
                          begin
                              cll := Cell.Create;
                              For CountLoop5 := 0 to ChildNode1.AttributeNodes.Count - 1 do
                              begin
                                AtritubesNode := ChildNode1.AttributeNodes[CountLoop5];
                                  If AtritubesNode.LocalName = 'MergeAcross' Then
                                      cll.MergeAcross := StrToInt(AtritubesNode.Text); //innerText
                                  If AtritubesNode.LocalName = 'MergeDown' Then
                                      cll.MergeDown := StrToInt(AtritubesNode.Text); //innerText
                                  If AtritubesNode.LocalName = 'StyleID' Then
                                      cll.StyleID := AtritubesNode.Text; //innerText
                                  If AtritubesNode.LocalName = 'Index' Then
                                  begin
                                    cll.Indexs := StrToInt(AtritubesNode.Text);
  //                                    Dim p2 As System.Reflection.FieldInfo := cll.GetType.GetField( _
  //                                    '_Index', Reflection.BindingFlags.Instance Or Reflection.BindingFlags.NonPublic);
  //                                    p2.SetValue(cll, CInt(AtritubesNode.Text)); //innerText
                                  End;
                              end;
                            
                              For CountLoop5 := 0 to ChildNode1.ChildNodes.Count - 1 do
                              begin
                                CellAtritubesNode := ChildNode1.ChildNodes[CountLoop5];
                                  If CellAtritubesNode.LocalName = 'Data' Then
                                  begin
                                      cll.Data.Value := CellAtritubesNode.Text;// XML; //innerText
                                      { //ismet yotum satýrý
                                      For Each AtritubesNode1 As XmlNode In CellAtritubesNode.Attributes
                                      begin
                                          If AtritubesNode1.LocalName = 'Type' Then
  //                                            'cll.Datas.Type := AtritubesNode1.InnerText
                                      end;
  //                                    ' cll.Datas.Value := SetValue(cll.Datas.Value, cll.Datas.Type)
                                      }
                                  End;

                                  If CellAtritubesNode.LocalName = 'NamedCell' Then
                                  begin
                                    { //ismet yotum satýrý
                                      For AtritubesNode1 As XmlNode In CellAtritubesNode.Attributes
                                      begin
                                          If AtritubesNode1.LocalName = 'Name' Then
  //                                            'cll.NamedCel.Name := CellAtritubesNode.InnerText
                                      end;
                                    }
                                  End;
                              end;
                              Rw.Cell.Add(cll);
                              cll := nil;
                          End;
                      end;
                      sht.Table.Row.Add(Rw);
                      Rw := nil;
                  End;
              end;
          End;
      end;
      Book.Sheet.Add(sht);
      sht := nil;
  //    'If Shett(ShettCount).ExpandedColumnCount := String.Empty Then Shett(ShettCount).ExpandedColumnCount := Shett(ShettCount).Col.Count
  //    'If Shett(ShettCount).ExpandedRowCount := String.Empty Then Shett(ShettCount).ExpandedRowCount := Shett(ShettCount).Row.Count
  //    'If Shett(ShettCount).DefaultColumnWidth := String.Empty Then Shett(ShettCount).DefaultColumnWidth := ToM('48')
  //    'If Shett(ShettCount).DefaultRowHeight := String.Empty Then Shett(ShettCount).DefaultRowHeight := ToM('15')
  //    'Shett(ShettCount).SetColAndRow
      ShettCount := ShettCount + 1;
    end;
  end;
//  fs.Close;
//  fs.Dispose;
//  fs := nil;
  FlushFileBuffers(fs.Handle); 
  FreeAndNil(fs);
end;

function Read.ToM(Value: String): String;
begin
  result := Value;
end;

function Read.ToSMm(Value: String): String;
begin
  result := Value;
end;

end.

