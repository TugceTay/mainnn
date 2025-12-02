unit Lider.CG.ModulesCom.Table.WorksBook;

interface

uses
  Winapi.Windows,
  Vcl.Forms,
  Winapi.Messages,
  Lider.CG.ModulesCom.Table.Styles,
  Lider.CG.ModulesCom.Table.Worksheet,
  System.Generics.Collections,
  Lider.CG.Com.EntityInt,
  SysUtils,
  System.IOUtils,
  Vcl.Dialogs,
  System.Classes;

type
  WorksBook = class
    private
      _Styles: Styles;
      _Sheet: TList<Worksheet>;
    public
      function GetSheet: TList<Worksheet>;
      procedure SetSheet(Sheet: TList<Worksheet>);
      property Sheet: TList<Worksheet> read GetSheet write SetSheet;

      function GetStyles: Styles;
      procedure SetStyles(Styles: Styles);
      property Styles: Styles read GetStyles write SetStyles;

      function GetSheetNames: TList<String>;
      function Write(FileName: String): Boolean;
      procedure Read(FileName: String);
      function GetNcObjectList(Scale: Double; SheetIndex: Integer; ShetHeight: Double;
                               CaptionRowsCount: Integer; PutRightorDown: Boolean): IlicgEntityList;//(Of CoveringCadObject;

      constructor Create;
  end;

const
  vbLf = #$A;

implementation
{ WorksBook }

uses
  Lider.CG.ModulesCom.Table.Read,
  Lider.CG.ModulesCom.Table.CreateNcTableOfXml;

constructor WorksBook.Create;
begin
  _Styles := Lider.CG.ModulesCom.Table.Styles.Styles.Create;
  _Sheet := TList<Worksheet>.Create;
end;

function WorksBook.GetSheet: TList<Worksheet>;
begin
  result := Self._Sheet;
end;

function WorksBook.GetSheetNames: TList<String>;
var
  GetSheetNames: TList<String>;
  a: Worksheet;
  resultList: TList<String>;
begin
  resultList := TList<String>.Create;
  GetSheetNames := TList<String>.Create;
  For a In Self.Sheet do
    resultList.Add(a.Name);

  result := resultList;
end;

function WorksBook.GetStyles: Styles;
begin
  result := Self._Styles;
end;

procedure WorksBook.Read(FileName: String);
var
  a: Lider.CG.ModulesCom.Table.Read.Read;
begin
  if String.IsNullOrEmpty(FileName) then
  begin
    a := Lider.CG.ModulesCom.Table.Read.Read.Create(Self);
    a := nil;
  end
  Else
  begin
    a := Lider.CG.ModulesCom.Table.Read.Read.Create(Self, FileName);
    a := nil;
  end;
end;

procedure WorksBook.SetSheet(Sheet: TList<Worksheet>);
begin
  Self._Sheet := Sheet;
end;

procedure WorksBook.SetStyles(Styles: Styles);
begin
  Self._Styles := Styles;
end;

function WorksBook.Write(FileName: String): Boolean;
var
  _fileName: String;
  strExcelXml: TStringBuilder;
  sht: Worksheet;
  outfile: TStreamWriter;
begin
  _fileName := '';

  if String.IsNullOrEmpty(FileName) then
  begin
    with TSaveDialog.Create(nil) do
    begin
      try
        Title := 'Okunacak Xml Elektronik Tablosu 2003';
        Filter := 'Xml Elektronik Tablo 2003/*.xml';
        DefaultExt := 'xml';
        FilterIndex := 1;
        FileName := '';
        if Execute then
        begin
          if String.IsNullOrEmpty(FileName) then
          begin
            result := False;
            Exit;
          end;
          _fileName := FileName;
        end;
      finally
        Free;
      end
    end;

//      Dim fil As New Asistan.FileOperation.IO;
//      fil.SaveDialog('', 1, New List(Of String)({'Xml Elektronik Tablo 2003/*.xml'}), 'Okunacak Xml Elektronik Tablosu 2003');
//      if String.IsNullOrEmpty(fil.TamAdi) then
//      begin
//        result := False;
//        Exit;
//      end;
//      _fileName := fil.TamAdi;
//      fil := nil;
  end
  else
    _fileName := FileName;

  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append('<?xml version="' + '1.0' + '"?>');
    strExcelXml.Append(vblf); //vbLf
    strExcelXml.Append('<?mso-application progid="' + 'Excel.Sheet"' + '?>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('<Workbook xmlns="' + 'urn:schemas-microsoft-com:office:spreadsheet"');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' xmlns:o="' + 'urn:schemas-microsoft-com:office:office"');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' xmlns:x="' + 'urn:schemas-microsoft-com:office:excel"');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' xmlns:ss="' + 'urn:schemas-microsoft-com:office:spreadsheet"');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' xmlns:html="' + 'http://www.w3.org/TR/REC-html40"' + '>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' <DocumentProperties xmlns="' + 'urn:schemas-microsoft-com:office:office"' + '>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <Author>Licad</Author>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <LastAuthor>Licad</LastAuthor>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <Created>2015-04-23T10:35:24Z</Created>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <Version>15.00</Version>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' </DocumentProperties>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' <OfficeDocumentSettings xmlns="' + 'urn:schemas-microsoft-com:office:office"' + '>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <AllowPNG/>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' </OfficeDocumentSettings>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' <ExcelWorkbook xmlns="' + 'urn:schemas-microsoft-com:office:excel"' + '>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <WindowHeight>9750</WindowHeight>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <WindowWidth>24000</WindowWidth>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <WindowTopX>0</WindowTopX>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <WindowTopY>0</WindowTopY>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <ProtectStructure>False</ProtectStructure>');
    strExcelXml.Append(vblf);
    strExcelXml.Append('  <ProtectWindows>False</ProtectWindows>');
    strExcelXml.Append(vblf);
    strExcelXml.Append(' </ExcelWorkbook>');
    strExcelXml.Append(vblf);

    strExcelXml.Append(Self.Styles.GetElektronikTablo2003String);
    strExcelXml.Append(vblf);

    For sht In Sheet do
    begin
      strExcelXml.Append('<Worksheet');
      strExcelXml.Append(' ss:Name="' + sht.Name + '"');
      strExcelXml.Append('>');
      strExcelXml.Append(vblf);

      strExcelXml.Append(sht.GetElektronikTablo2003String);
      strExcelXml.Append(vblf);

      strExcelXml.Append('<WorksheetOptions xmlns="' + 'urn:schemas-microsoft-com:office:excel"' + '>');
      strExcelXml.Append(vblf);
      strExcelXml.Append(' <PageSetup>');
      strExcelXml.Append(vblf);
      strExcelXml.Append('  <Header x:Margin="' + '0.3"' + '/>');
      strExcelXml.Append(vblf);
      strExcelXml.Append('  <Footer x:Margin="' + '0.3"' + '/>');
      strExcelXml.Append(vblf);
      strExcelXml.Append('  <PageMargins x:Bottom="' + '0.75"' + ' x:Left="' + '0.7"' + ' x:Right="' + '0.7"' + ' x:Top="' + '0.75"' + '/>');
      strExcelXml.Append(vblf);
      strExcelXml.Append(' </PageSetup>');
      strExcelXml.Append(vblf);
      strExcelXml.Append(' <Selected/>');
      strExcelXml.Append(vblf);
      strExcelXml.Append(' <ProtectObjects>False</ProtectObjects>');
      strExcelXml.Append(vblf);
      strExcelXml.Append(' <ProtectScenarios>False</ProtectScenarios>');
      strExcelXml.Append(vblf);
      strExcelXml.Append('</WorksheetOptions>');
      strExcelXml.Append(vblf);
      strExcelXml.Append('</Worksheet>');
    end;
    strExcelXml.Append(vblf);
    strExcelXml.Append('</Workbook>');

    except on e: Exception do
    begin
      strExcelXml.Clear;
      result := False;
      Exit;
    end;

  end;

  try
    //TStreamWriter
    outfile := TStreamWriter.Create(_fileName,False);
    outfile.Write(strExcelXml.ToString);
    outfile.Close;
    FreeAndNil(outfile);
//      Using outfile As StreamWriter = New StreamWriter(_fileName, False)
//          outfile.Write(strExcelXml.ToString);
//      End Using
    except on e: Exception do
    begin
      Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
      result := False;
      Exit;
    end;
  end;
  result := True;
end;

function WorksBook.GetNcObjectList(Scale: Double; SheetIndex: Integer;
  ShetHeight: Double; CaptionRowsCount: Integer;
  PutRightorDown: Boolean): IlicgEntityList;
var
  ncadd: CreateNcTableOfXml;
begin
  ncadd := Lider.CG.ModulesCom.Table.CreateNcTableOfXml.CreateNcTableOfXml.Create(Scale, Self);
  result := ncadd.GetTableTablo(SheetIndex, ShetHeight, CaptionRowsCount, PutRightorDown);
  ncadd := nil;
end;

end.

