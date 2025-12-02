unit Lider.CG.ModulesCom.Table;

interface

uses
  Lider.CG.ModulesCom.Table.BaseTable,
  Lider.CG.ModulesCom.Table.Columns,
  Lider.CG.ModulesCom.Table.Rows,
  Lider.CG.ModulesCom.Table.Cells,
  SysUtils,
  Lider.CG.ModulesCom.Table.ColInt,
  Lider.CG.ModulesCom.Table.RowInt,
  Lider.CG.ModulesCom.Table.CellInt,
  Vcl.Forms,
  Winapi.Windows;

type
  Table = class(BaseTable)
    private
      _Column: Cols;
      _Row: Rows;

      function GetElektronikTablo2003StringCol(DefaultColumnWidth: Single): String;
      function GetElektronikTablo2003StringRow(DefaultRowHeight: Single): String;
      function GetElektronikTablo2003StringCells(Cels: Cells): String;
    public
      function GetColumn: Cols; override;
      procedure SetColumn(Column: Cols); override;
      property Column: Cols read GetColumn write SetColumn;

      function GetRow: Rows; override;
      procedure SetRow(Row: Rows); override;
      property Row: Rows read GetRow write SetRow;

      function GetElektronikTablo2003StringTable: String; override;

      constructor Create(DefaultRowHeight: Single; DefaultColumnWidth: Single);
  end;

const
  vbLf = #$A;

implementation
{ Table }

constructor Table.Create(DefaultRowHeight: Single; DefaultColumnWidth: Single);
begin
  inherited Create;
  inherited DefaultColumnWidth := DefaultColumnWidth;
  inherited DefaultRowHeight := DefaultRowHeight;

  _Column := Lider.CG.ModulesCom.Table.Columns.Cols.Create;
  _Row := Lider.CG.ModulesCom.Table.Rows.Rows.Create;
end;

function Table.GetElektronikTablo2003StringTable: String;
var
  strExcelXml: TStringBuilder;
begin
  strExcelXml := TStringBuilder.Create;
  try
    strExcelXml.Append('<Table');

    if ExpandedColumnCount > 0 then
        strExcelXml.Append(' ss:ExpandedColumnCount="' + ExpandedColumnCount.ToString + '"');

    if ExpandedRowCount > 0 then
        strExcelXml.Append(' ss:ExpandedRowCount="' + ExpandedRowCount.ToString + '"');

    if FullColumns then
        strExcelXml.Append(' x:FullColumns="' + '1' + '"');

    if FullRows then
        strExcelXml.Append(' x:FullRows="' + '1' + '"');

    if Not String.IsNullOrEmpty(StyleID) then
        strExcelXml.Append(' ss:StyleID="' + StyleID + '"');

    if DefaultColumnWidth > 0 then
        strExcelXml.Append(' ss:DefaultColumnWidth="' + DefaultColumnWidth.ToString + '"');

    if DefaultRowHeight > 0 then
        strExcelXml.Append(' ss:DefaultRowHeight="' + DefaultRowHeight.ToString + '"');

    strExcelXml.Append('>');

    if ExpandedRowCount > 0 then
    begin
        strExcelXml.Append(vblf); //vbLf
        strExcelXml.Append(GetElektronikTablo2003StringCol(DefaultColumnWidth));
        strExcelXml.Append(GetElektronikTablo2003StringRow(DefaultRowHeight));
        strExcelXml.Append('</Table>');
    End;
  except on e: exception do
    strExcelXml.Clear;
  end;
  result := strExcelXml.ToString;
end;

function Table.GetElektronikTablo2003StringCol(
  DefaultColumnWidth: Single): String;
var
  strExcelXml: TStringBuilder;
  Col: ColInt;
begin
  strExcelXml := TStringBuilder.Create;
  if Self.Column.Count > 0 Then
  begin
    For Col In Self.Column do
    begin
      try
        strExcelXml.Append('<Column');

        if Col.Indexs > 0 then
            strExcelXml.Append(' ss:Index="' + Col.Indexs.ToString + '"');

        if Not String.IsNullOrEmpty(Col.StyleID) Then
            strExcelXml.Append(' ss:StyleId="' + Col.StyleID + '"');

        if Col.AutoFitWidth then
            strExcelXml.Append(' ss:AutoFitWidtht="' + '1' + '"');

        if Col.Width > 0 then
            strExcelXml.Append(' ss:Width="' + Col.Width.ToString + '"')
        else
            strExcelXml.Append(' ss:Width="' + DefaultColumnWidth.ToString + '"');

        if Col.Hidden then
            strExcelXml.Append(' ss:Hidden="' + '1' + '"');

        if Col.Span > 0 then
            strExcelXml.Append(' ss:Span="' + Col.Span.ToString + '"');

        strExcelXml.Append('/>');
        strExcelXml.Append(vblf); //vbLf
      except on e: exception do
        strExcelXml.Clear;
      end;
    end;
  end;
  result := strExcelXml.ToString;
end;

function Table.GetElektronikTablo2003StringRow(
  DefaultRowHeight: Single): String;
var
  strExcelXml: TStringBuilder;
  Row: RowInt;
begin
  strExcelXml := TStringBuilder.Create;
  If Self.Row.Count > 0 Then
  begin
    For Row In Self.Row do
    begin
      try
        strExcelXml.Append('<Row');

        if Row.Indexs > 0 then
            strExcelXml.Append(' ss:Index="' + Row.Indexs.ToString + '"');

        if Row.AutoFitHeight then
            strExcelXml.Append(' ss:AutoFitHeight="' + '1' + '"');

        if Row.Height > 0 then
            strExcelXml.Append(' ss:Height="' + Row.Height.ToString + '"')
        else
            strExcelXml.Append(' ss:Height="' + DefaultRowHeight.ToString + '"');

        if Row.Hidden then
            strExcelXml.Append(' ss:Hidden="' + '1' + '"');

        if Not String.IsNullOrEmpty(Row.StyleID) then
            strExcelXml.Append(' ss:StyleId="' + Row.StyleID + '"');

        if Row.Cell.Count > 0 then
        begin
            strExcelXml.Append('>');
            strExcelXml.Append(vblf); //vbLf
            strExcelXml.Append(GetElektronikTablo2003StringCells(Row.Cell));
            strExcelXml.Append('</Row>');
            strExcelXml.Append(vblf);
        end
        else
        begin
            strExcelXml.Append('/>');
            strExcelXml.Append(vblf);
        end;
      except on e: exception do
        strExcelXml.Clear;
      end;
    end;
    end;
  result := strExcelXml.ToString;
end;

function Table.GetElektronikTablo2003StringCells(Cels: Cells): String;
var
  strExcelXml: TStringBuilder;
  Cel: CellInt;
  celdata, styl: String;
begin
  strExcelXml := TStringBuilder.Create;
  try
    if Cels.Count > 0 Then
    begin
      for Cel In Cels do
      begin
        celdata := Cel.Data.GetElektronikTablo2003String;
        styl := Cel.StyleID;

//        ' Celde deðer yok ve stili default ise cel yazýlmaz
        if String.IsNullOrEmpty(celdata) And (styl = 'Default') And String.IsNullOrEmpty(styl) Then
//            'Cel de deðer yok ve stili tanýmlanmýþ ve default deðil ise sadece stili yazýlýr
            Continue
        else if String.IsNullOrEmpty(celdata) And (Not String.IsNullOrEmpty(styl)) And (styl <> 'Default') Then
        begin
            strExcelXml.Append('<Cell').ToString;
            if Cel.Indexs > 1 then
                strExcelXml.Append(' ss:Index="' + Cel.Indexs.ToString + '"');

            strExcelXml.Append(' ss:StyleID="' + styl + '"');
            strExcelXml.Append('/>');
        end
        else
        begin
            strExcelXml.Append('<Cell').ToString;

            if Cel.Indexs > 1 Then
                strExcelXml.Append(' ss:Index="' + Cel.Indexs.ToString + '"');

            if Cel.MergeAcross <> 0 Then
                strExcelXml.Append(' ss:MergeAcross="' + Cel.MergeAcross.ToString + '"');

            if Cel.MergeDown <> 0 Then
                strExcelXml.Append(' ss:MergeDown="' + Cel.MergeDown.ToString + '"');

            if (Not String.IsNullOrEmpty(styl)) And (styl <> 'Default') Then
                strExcelXml.Append(' ss:StyleID="' + styl + '"');

            strExcelXml.Append('>');

            if Not String.IsNullOrEmpty(celdata) Then
            begin
                strExcelXml.Append(celdata);
                strExcelXml.Append('</Cell>');
            End;
        End;
        strExcelXml.Append(vblf); //vbLf
      end;
    end
    Else
      strExcelXml.Clear;

    except on e: exception do
    begin
      strExcelXml.Clear;
      Application.MessageBox(PWideChar(e.Message), 'Bilgi',MB_OK + MB_ICONINFORMATION);
    end;
  end;
  result := strExcelXml.ToString;
end;

function Table.GetColumn: Cols;
begin
  result := Self._Column;
end;

function Table.GetRow: Rows;
begin
  result := Self._Row;
end;

procedure Table.SetColumn(Column: Cols);
begin
  Self._Column := Column;
end;

procedure Table.SetRow(Row: Rows);
begin
  Self._Row := Row;
end;

end.

