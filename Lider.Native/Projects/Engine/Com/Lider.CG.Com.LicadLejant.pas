unit Lider.CG.Com.LicadLejant;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Dialogs,
  Math,
  Forms,
  StdCtrls,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Base,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.Lib,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TCellType = (ljtText, ljtFillHatch, ljtFillColor, ljtLineType, ljtSymbol,
    ljtLine, ljtNone);

  TCellRecord = record
    ljCellType: TCellType;
    ljCellKey: string; //Key = yazý için Text, vektör tarama ve hat türü için isim, sembol için indis
    ljCellEntHeight: Double; //yazý ve sembol  nesneleri için yükseklik
    ljCellColor: TColor;
    ljCellFontName: string;
    ljCellAlign: TlicgTextHorzAlignment;
    ljDrawRowLine: Boolean;
    ljRowLineY: Double;
  end;

  TCellMeasureRecord = record
    ljMCellWidth: Double;
    ljMCellHeight: Double;
  end;

  TBELejand = class(TObject)
  private
    FCmdLine: TlicgBaseCmdLine;
    FLejandArray: array of array of TCellRecord;
    FCellMArray: array of array of TCellMeasureRecord;
    FLejandFontName: string; // Tüm lejand yazýlarý için genel font adý
    FPATFileName: string; // vektörel tarama türleri dosya adý (path+file name)
    FRowSpace: Double;
    FColSpace: Double;
    FRowCount, FColCount: Integer;
    FDrawBorder: Boolean;
    FBorderColor: TColor;
    FBorderWidth: Double;
    FTextEntType: TlicgEntityID;
  public
    constructor Create(cmd: TlicgBAseCmdLine; rowCount, colCount: Integer;
      RowHeight: Double = 0; ColHeight: Double = 0);
    destructor Destroy; override;
    procedure SetColWidth(col: Integer; Width: Double);
    procedure SetRowHeight(row: Integer; height: Double);
    procedure SetCellColRowMeasure(row, col: Integer; Width, Height: Double);
    procedure InsertLejandCell(cellRow, cellCol: Integer; CellType: TCellType;
      CellKey: string; CellColor: TColor; CellAlign: TlicgTextHorzAlignment = thaLeft;
      CellEntHeight: DOuble = 0);
    procedure CreateLejand(gEnt: IlicgEntity);
    procedure DrawLine(row: Integer);
    procedure SetCellFontName(row, col: Integer; textFontName: string);
    property PATFileName: string read FPATFileName write FPATFileName;
    property RowSpace: Double read FRowSpace write FRowSpace;
    property ColSpace: Double read FColSpace write FColSpace;
    property LejandFontName: string read FLejandFontName write FLejandFontName;
    property DrawBorder: Boolean read FDrawBorder write FDrawBorder;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderWidth: Double read FBorderWidth write FBorderWidth;
    property TextEntType: TlicgEntityID read FTextEntType write FTextEntType;
  end;

implementation

{ TBELejand }

constructor TBELejand.Create(cmd: TlicgBaseCmdLine; rowCount, colCount: Integer;
  RowHeight: Double = 0; ColHeight: Double = 0);
var
  r, c: Integer;
begin
  inherited Create;
  SetLength(FLejandArray, RowCount, colcount);
  SetLength(FCellMArray, RowCount, colcount);
  FTextEntType := idVectorialText;
  for r := 0 to RowCount - 1 do
  begin
    for c := 0 to colcount - 1 do
    begin
      FCellMArray[r, c].ljMCellHeight := RowHeight;
      FCellMArray[r, c].ljMCellWidth := ColHeight;
    end;
  end;
  FRowCount := rowCount;
  FColCount := colCount;
  FCmdLine := cmd;
end;

destructor TBELejand.Destroy;
begin
  inherited;
end;

procedure TBELejand.InsertLejandCell(cellRow, cellCol: Integer; CellType:
  TCellType; CellKey: string; CellColor: TColor; CellAlign: TlicgTextHorzAlignment =
  thaLeft; CellEntHeight: Double = 0);
begin
  if (CellRow > FRowCount) or (cellCol > FColCount) then
    exit;

  with FLejandArray[CellRow, cellCol] do
  begin
    ljCellType := CellType;
    ljCellKey := CellKey;
    ljCellEntHeight := CellEntHeight;
    ljCellColor := CellColor;
    ljCellAlign := CellAlign;
  end;
end;

procedure TBELejand.CreateLejand(gEnt: IlicgEntity);
var
  r, c, i, ii, n, cc: Integer;
  y, x: Double;
  fY, fX: Double; //ilk x ve y deðerleri
  tmpEnt, pline: IlicgEntity;
  textHeightRatio: Double; //yazý nesnesinin boyunun satýr yüksekliðine oraný (örn: yazý boyu satýrýn 0.80)
  p: TlicgCoor;
  hatchEntList, resultEntList: IlicgEntityList;
  maxRowHeight: Double;
  maxWidth: Double;
  maxTWidth: Double;
  textBox: TlicgExtent;
  _cellHeight: Double;
  _cellWidth: Double;
  _textHeight: Double;
  borderMAxX: Double;
begin
  hatchEntList := Nil;
  resultEntList := Nil;
  hatchEntList := TlicgEntityList.Create;

  try

    fx := 0;
    fy := 0;
    x := fx;
    y := fy;
    maxTWidth := 0;
    textHeightRatio := 0.80;

    for r := 0 to FRowCount - 1 do
    begin

      x := fx;
      maxRowHeight := MinDouble;
      maxWidth := MinDouble;
      for c := 0 to FColCount - 1 do
      begin
        with FLejandArray[r, c] do
        begin

          _CellHeight := FCellMArray[r, c].ljMCellHeight;
          _CellWidth := FCellMArray[r, c].ljMCellWidth;

          p := AsCoor(x, y);

          case ljCellType of
            ljtText:
              begin
                tmpEnt := Nil;
                textBox.LowerLeft := p;
                textBox.UpperRight.x := p.x + _CellWidth;

                if ljCellEntHeight <> 0 then
                  _textHeight := ljCellEntHeight
                else
                  _textHeight := (_cellHeight * textHeightRatio);
                textBox.LowerLeft.y := textBox.LowerLeft.y - ((_cellHeight / 2)
                  - (_textHeight / 2));
                textBox.UpperRight.y := textBox.LowerLeft.y - _textHeight;
                if FTextEntType = idVectorialText then
                begin
                  tmpEnt := Licad.CreateEntityFactory.MakeVectorialText(textBox.LowerLeft, ljCellKey);
                  tmpEnt.DrawTools.FontTool.Height := _textHeight;
                end
                else
                begin
                  tmpEnt := Licad.CreateEntityFactory.MakeVectorialFittedText(p, ljCellKey);
                  tmpEnt.DrawTools.FontTool.Height := _textHeight;
                  tmpEnt.AsVectorialFittedText.TextWidth := _CellWidth;
                end;
                try
                  if ljCellFontName <> '' then
                    AslicgVectorialText(tmpEnt.Geometry).VectorFont :=
                      Licad.VectorFonts.FontByName(ljCellFontName)
                  else
                    AslicgVectorialText(tmpEnt.Geometry).VectorFont :=
                      Licad.VectorFonts.FontByName(FLejandFontName);

                  tmpEnt.DrawTools.FontTool.Color := ljCellColor;

                  tmpEnt.Geometry.UpdateExtension;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
                  if (tmpEnt.Geometry.Extent.UpperRight.x - tmpEnt.Geometry.Extent.LowerLeft.x)
                    > _CellWidth then
                    x := x + ((tmpEnt.Geometry.Extent.UpperRight.x - tmpEnt.Geometry.Extent.LowerLeft.x)
                      + FColSpace)
                  else
                    x := x + (_CellWidth + FColSpace);
                  maxTWidth := maxTWidth + _CellWidth;

                finally
                  if Assigned(tmpEnt) then
                    tmpEnt := nil;
                end;
              end; //ljtText

            ljtFillHatch:
              begin
                tmpEnt := Nil;
                tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _2D);
                try
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.DrawTools.PenTool.Color := ljCellColor;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);

                  x := x + (_CellWidth + FColSpace);

                  hatchEntList.Clear;
                  hatchEntList.Add(tmpEnt.Clone);

                  if ljCellEntHeight = 0 then
                    ljCellEntHeight := 1;
                  resultEntList := FCmdline.VectorFill(FPATFileName, ljCellKey,
                    hatchEntList, nil, ljCellColor, 0, ljCellEntHeight);

                  if resultEntList = Nil then
                    Continue;
                  for i := 0 to resultEntList.Count - 1 do
                  begin
                    AslicgGroupGeometry(gEnt.Geometry).AddEntity(resultEntList[i].Clone);
                  end;
                  resultEntList := nil;
                  maxTWidth := maxTWidth + _CellWidth;
                finally
                  if Assigned(tmpEnt) then
                    tmpEnt := nil;
                end; //try
              end; //ljtFillHatch

            ljtFillColor:
              begin
                tmpEnt := Nil;
                tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);
                try
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.DrawTools.PenTool.Color := ljCellColor;
                  tmpEnt.DrawTools.BrushTool.ForeColor := ljCellColor;
                  tmpEnt.DrawTools.BrushTool.BackColor := ljCellColor;
                  tmpEnt.DrawTools.BrushTool.Pattern := 1;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
                  x := x + (_CellWidth + FColSpace);
                  maxTWidth := maxTWidth + _CellWidth;
                finally
                  if Assigned(tmpEnt) then
                    tmpEnt := nil;
                end;  //try
              end; //ljtFillColor

            ljtLineType:
              begin
                try
                  tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _2D);
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y - _CellHeight);
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.DrawTools.PenTool.Color := ljCellColor;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);

                  tmpEnt := nil;

                  tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _2D);
                  tmpEnt.Geometry.Points.Add(p.x, p.y - (_CellHeight / 2));
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y - (_CellHeight / 2));
                  tmpEnt.ApplyLayerProps := False;
                  if strToInt(ljCellKey) = -1 then
                    tmpEnt.DrawTools.PenTool.Style := 0
                  else
                    tmpEnt.DrawTools.PenTool.Style := strToInt(ljCellKey);
                  tmpEnt.DrawTools.PenTool.Color := ljCellColor;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
                  tmpEnt := nil;
                  x := x + (_CellWidth + FColSpace);
                  maxTWidth := maxTWidth + _CellWidth;

                finally
                end;
              end; //ljtLineType

            ljtSymbol:
              begin
                try
                  tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPlace, 0, _2D);
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.DrawTools.SymbolTool.Index := StrToInt(ljCellKey);
                  if ljCellEntHeight <> 0 then
                    tmpEnt.DrawTools.SymbolTool.Height := ljCellEntHeight
                  else
                    tmpEnt.DrawTools.SymbolTool.Height := _CellHeight;
                  tmpEnt.Geometry.UpdateExtension;
                  case ljCellAlign of
                    thaLeft:
                      begin
                        tmpEnt.Geometry.Points.x[0] := tmpEnt.Geometry.Points.x[0]
                          + (tmpEnt.Geometry.Extent.UpperRight.x - tmpEnt.Geometry.Extent.LowerLeft.x)
                          / 2;
                      end;
                    thaCenter:
                      begin
                        tmpEnt.Geometry.Points.x[0] := tmpEnt.Geometry.Points.x[0]
                          + ((_CellWidth / 2) - ((tmpEnt.Geometry.Extent.UpperRight.x
                          - tmpEnt.Geometry.Extent.LowerLeft.x) / 2));
                      end;
                    thaRight:
                      begin
                        tmpEnt.Geometry.Points.x[0] := tmpEnt.Geometry.Points.x[0]
                          + ((_CellWidth) - ((tmpEnt.Geometry.Extent.UpperRight.x
                          - tmpEnt.Geometry.Extent.LowerLeft.x)));
                      end;
                  end;

                  tmpEnt.Geometry.Points.y[0] := tmpEnt.Geometry.Points.y[0] - (_Cellheight
                    / 2);
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);

                  x := x + (_CellWidth + FColSpace);
                  maxTWidth := maxTWidth + _CellWidth;
                finally
                  tmpEnt := nil;
                end
              end; //ljtSymbol

            ljtLine:
              begin
                tmpEnt := Licad.CreateEntityFactory.MakeEntity(idLine, 0, _2D);
                try
                  tmpEnt.Geometry.Points.Add(p.x, p.y);
                  tmpEnt.Geometry.Points.Add(p.x + _CellWidth, p.y);
                  tmpEnt.ApplyLayerProps := False;
                  tmpEnt.DrawTools.PenTool.Style := 0;
                  tmpent.DrawTools.PenTool.Color := ljCellColor;
                  AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
                  x := x + (_CellWidth + FColSpace);
                  maxTWidth := maxTWidth + _CellWidth;
                finally
                  tmpEnt := nil;
                end;
              end; //ljtLine
            ljtNone:
              begin
                x := x + (_CellWidth + FColSpace);
                maxTWidth := maxTWidth + _CellWidth;
              end;

          end; //case FLejandArray[r,c].ljCellType
          if _cellHeight > maxRowHeight then
            maxRowHeight := _cellHeight;
        end; //With FLejandArray[r,c]


      end; //for c array columns
      if maxTWidth > maxWidth then
        maxWidth := maxTWidth;
      maxTWidth := 0;
      y := y - (maxRowHeight + FRowSpace);
      for c := 0 to FColCount - 1 do
      begin
        FLejandArray[r, c].ljRowLineY := y;
      end;
    end; //for r array rows

    if FDrawBorder then
    begin
      gEnt.Geometry.UpdateExtension;
      tmpEnt := Licad.CreateEntityFactory.MakeRectangle(AsCoor(0 - FColSpace, 0
        + FRowSpace), AsCoor(gEnt.Geometry.Extent.LowerLeft.x + maxWidth +
        FColSpace * 4, y - FRowSpace));
      tmpEnt.DrawTools.PenTool.Color := FBorderColor;
      tmpEnt.DrawTools.PenTool.Width := FBorderWidth;
      tmpEnt.DrawTools.BrushTool.Pattern := 0;
      AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
      borderMAxX := tmpEnt.Geometry.Extent.UpperRight.x;
      tmpEnt := nil;

      for r := 0 to FRowCount - 1 do
      begin
        for c := 0 to FColCount - 1 do
        begin
          if FLejandArray[r, c].ljDrawRowLine then
          begin
            tmpEnt := Licad.CreateEntityFactory.MakeEntity(idPolyline, 0, _2D);
            tmpEnt.DrawTools.PenTool.Color := FBorderColor;
            tmpEnt.DrawTools.PenTool.Width := FBorderWidth;
            y := FLejandArray[r, c].ljRowLineY;
            y := y + (FRowSpace / 2);
            tmpent.Geometry.Points.Add(0 - FColSpace, y);
            tmpent.Geometry.Points.Add(borderMAxX, y);
            AslicgGroupGeometry(gEnt.Geometry).AddEntity(tmpEnt.Clone);
            tmpEnt := nil;
          end;
        end;
      end;
      gEnt.Geometry.UpdateExtension;
    end;
  finally
    resultEntList := nil;
    hatchEntList := nil;
  end; //try

end;

procedure TBELejand.SetColWidth(col: Integer; Width: Double);
var
  r: Integer;
begin
  if (col > FColCount) then
    exit;

  for r := 0 to FRowCount - 1 do
  begin
    FCellMArray[r, col].ljMCellWidth := Width;
  end;
end;

procedure TBELejand.SetRowHeight(row: Integer; height: Double);
var
  c: Integer;
begin
  if (row > FRowCount) then
    exit;

  for c := 0 to FColCount - 1 do
  begin
    FCellMArray[row, c].ljMCellHeight := height;
  end;
end;

procedure TBELejand.SetCellColRowMeasure(row, col: Integer; Width, Height: Double);
begin
  if (row > FRowCount) or (col > FColCount) then
    exit;
  FCellMArray[row, col].ljMCellWidth := Width;
  FCellMArray[row, col].ljMCellHeight := Height;
end;

procedure TBELejand.SetCellFontName(row, col: Integer; textFontName: string);
begin
  if (row > FRowCount) or (col > FColCount) then
    exit;
  FLejandArray[row, col].ljCellFontName := textFontName;
end;

procedure TBELejand.DrawLine(row: Integer);
var
  r, c: Integer;
begin
  if (row > FRowCount) then
    exit;

  for c := 0 to FColCount - 1 do
  begin
    FLejandArray[row, c].ljDrawRowLine := true;
  end;

end;

end.


