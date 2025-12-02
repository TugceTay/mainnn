unit Lider.CG.Com.System;

{$I Lider.CG.Com.Component.inc}

{$WARNINGS OFF}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, Forms, SysUtils,
  IniFiles, Dialogs, IOUtils, Types, StrUtils, RegularExpressions,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Base,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Lib,
  Lider.CG.Com.LineDraw;

type
  TListImageKind = (liBitmaps, liBlocks, liAllImages, liOleObject);

const
  { mouse cursors used in the system }
  crZoomIn = 10000 + 1;
  crZoomOut = 10000 + 2;
  crScrollingUp = 10000 + 3;
  crScrollingDn = 10000 + 4;
  crRealTimeZoom = 10000 + 5;
  crHidden = 10000 + 6;
  crDrawCross = 10000 + 7;
  crRotate = 10000 + 8;
  crSelectEntity = 10000 + 9;
  crMirror = 10000 + 11;
  crBusy: integer = -1;
  { buffered read }
  SIZE_LONGBUFFER = 100 * 1024;

function IsEscapePressed: Boolean;
function CtrlDown: Boolean;
function AltDown: Boolean;
function ShiftDown: Boolean;
procedure LicadGISError(const ErrStr: string);
procedure SortList(ol: TList);
function GetTemporaryLayerName(const Path, Prefix: string): string;
function GetTemporaryEZFFileNameinEXE(const _fn: string): string;
function GetTemporaryFilePath: string;

function GetTempLicadGISPath: string; // ilker ekleme
function GetTempLicadGISProjectsPath: string; // ilker ekleme
function GetTempLicadGISTempPath: string; // ilker ekleme
function GetTempFileNameLicadGISTemp(const Prefix: string): string; // ilker ekleme
function GetTempProjectNameLicadGISTemp: string;

function isNotSavedGIS(filename: string): Boolean;
function AddSlash(const Path: string): string; // ilker Test Ok.
function RemoveSlash(const Path: string): string;
function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
function MessageToUser(const Msg, Caption: string; AType: Word): Word;
procedure AddMarker(DrawBox: TlicgBaseDrawBox; const X, Y: Double; SetInView:
  Boolean; UseThread: Boolean = False; margin: double = 0);
function DeleteFileChecking(const FileName: string): Boolean;
procedure CombineSelection(DrawBox: TlicgBaseDrawBox; DeleteOriginals: Boolean);
procedure ReadFile(const Path: string; FileList: TStrings);
procedure FreeList(var alist: TList);
function GetListOfVectors(const Entity: IlicgEntity): IInterfaceList;
function CreateIfNotExists(const FileName: string): TFileStream;
function CreateDllList(FieldList: TStringList): string;
function HasAttr(const FileName: string; Attr: Word): Boolean;
function DegMinSec2Extended(const DegMinSec: TDegMinSec): Double;
function Extended2DegMinSec(const RealDeg: Double): TDegMinSec;
procedure DoShowGuideLines(DrawBox: TlicgBaseDrawBox; ACanvas: TCanvas;
  HGuideLines, VGuideLines: IlicgDoubleList);
procedure MoveDisplayOrder(DrawBox: TlicgBaseDrawBox; Code: Integer; UseThread:
  Boolean = False);
procedure PaintDrawBoxFSGrid(DrawBox: TlicgBaseDrawBox; const WCRect: TlicgExtent);
function Dark(Col: TColor; Percent: Byte): TColor;
procedure BlinkEntity(DrawBox: TlicgBaseDrawBox; Layer: TlicgBaseLayer; RecNo: Integer);
procedure BlinkEntityIndirect(DrawBox: TlicgBaseDrawBox; Entity: IlicgEntity);
procedure BlinkEntities(DrawBox: TlicgBaseDrawBox);
procedure HiliteEntity(Entity: IlicgEntity; DrawBox: TlicgBaseDrawBox);
procedure UnHiliteEntity(Entity: IlicgEntity; DrawBox: TlicgBaseDrawBox);
function Field1Exprtype(Layer: TlicgBaseLayer; FieldName: string): TExprtype;
function Field2Exprtype(Layer: TlicgBaseLayer; FieldNo: Integer): TExprtype;

function Dpi2Units(iUnits: TlicgScaleUnits; Dpis, Value: integer): Double;
function Units2Dpi(iUnits: TlicgScaleUnits; Dpis: Integer; Value: Double): integer;
procedure licgWriteStrToStreamForNCZ(const TextToWrite: string; stream: TStream);
procedure licgWriteStrToStream(const TextToWrite: string; stream: TStream);
function licgReadStrFromStream(stream: TStream): string;
function StringIndex(const SearchString: string; const StrList: array of string): Integer;
function GetCurrentColorDepth: Integer;
function RemoveStrDelim(const S: string): string;
function GetSameFilesSameName(const Filename: string; SL: TStrings): boolean;
function DeleteFilesSameName(const Filename: string; DeleteAllFile: Boolean = False): Boolean;
function DeleteLayerFiles(const ALayerName: string): Boolean;
function MoveFilesSameName(const Filename, NewFileName: string; ChangeFn:
  Boolean; DeleteFiles: Boolean): Boolean;
function CopyFilesSameName(const GIS: TlicgBaseGIS; const NewDir: string;
  NewName: string = ''): Boolean; overload;
function CopyFilesSameName(const Layer: TlicgBaseLayer; const NewDir: string;
  NewName: string = ''): Boolean; overload;
function CopyFilesSameName(const Layer: TlicgBaseLayer; const layname: string;
  const NewDir: string; NewName: string): Boolean; overload;
function _CopyFilesSameName(const Layer: TlicgBaseLayer; const NewDir: string;
  var NewName: string): Boolean;
function AddBrackets(const Value: string): string;
function GetValidLayerName(const OrigLayerName: string): string;
function TrimCrLf(const s: widestring): widestring;
function ComplColor(Clr: TColor): TColor;
//Procedure GetMessageBoxFont( afont: TFont );

function DefaultFontName: string;
function DefaultFont: TFont;
function DefaultFontHandle: HFont;

function GetParentFormHWND(Control: TWinControl): HWND;
//procedure HideFormTitleBar(Form: TForm);
//procedure ShowFormTitleBar(Form: TForm);
function FindContrastingColor(Color: TColor): TColor;
function StrSplitToList(const sToSplit, sSeparator: string; tsStrList: TStrings; bAllowEmpty: Boolean = True): Integer;
function Sub2D(const A, B: TlicgCoor): TlicgCoor; // A-B
procedure WriteFontToIniFile(AIniFile: TIniFile; const ASection: string; AFont: TFont); // ilker ekleme
procedure ReadFontFromIniFile(AIniFile: TIniFile; const ASection: string; AFont: TFont); // ilker ekleme
function ReadFloatFromIniFile(AIniFile: TIniFile; const ASection, AIdent: string; const ADefault: Double): Double;
function ReadIntFromIniFile(AIniFile: TIniFile; const ASection, AIdent: string; const ADefault: Integer): Integer;
procedure WriteFloatToIniFile(AIniFile: TIniFile; const ASection, AIdent: string; AValue: Double);

function GetLastPolygonName(drawBox: TlicgBaseDrawBox; sPrefix: string; layer: TlicgBaselayer = nil): string;
function GetLastPointName(drawBox: TlicgBaseDrawBox; sPrefix: string; layer: TlicgBaselayer = nil): string;
function IncreasePointName(FLastPointName: string): string;
function DecreasePointName(FLastPointName: string): string;
function GetPointNamesByAperture(p1n, p2n: string; SL: TStrings; isAllLower: boolean = false): boolean;
function GetPointEntityList(const aGis: TlicgBAseGis; const SL: TStrings): IlicgEntityList;
function StrIsNumeric(tmpStr: string): Boolean;
procedure SaveStringsAsUnicode(const thefilename: string; theStringlist: TStrings);
procedure CalculateAnchorPoints(const LineFrom, LineTo: TlicgCoor; const Distance:
  Double; var ap1s, ap2s, ap1s_cw, ap1s_ccw, ap2s_cw, ap2s_ccw: TlicgCoor);
procedure CreateOffsetEntitysInPolyArea(Entity: IlicgEntity; Distance: Double);
function LayerExists(const FileName: string; LayerType: TlicgLayerType): Boolean;
function SelectCommonElement(const Subdir, IniFilter: string; ListImageKind: TListImageKind): string;
procedure AddToRecentProjects(ASectionName: string; const AFileName: string; AExt: string);
procedure DeleteToRecentProjects(ASectionName: string; const AFileName: string; AExt: string);
function GetRecentProjects(ASectionName: string): TArray<string>;
procedure SetupCursors;
procedure DisposeCursors;
function DetectCancelPaint(DrawBox: TlicgBaseDrawBox): Boolean;
function DetectCancelPaintHandle(AHandle: THandle): Boolean;
function DetectCancelFarPaint: Boolean; stdcall;
// ilker silme procedure CopyFilesInDirToAnotherDir(SourceDir, DestDir: string);
function AngleToRad(A: double): double;
function RadToAngle(A: double): double;
procedure PopulateFieldList(DBTable: TlicgBaseTable; FieldList: TStrings);
function GetActiveRecordCount(L: TlicgBaseLayer): Integer;
procedure VectorDrawCross(ADrawBox: TlicgBaseDrawBox; AVector: IlicgVector); // ilker ekleme

function MakeExpression(AField: string; AValue: string): string; // ilker ekleme
function MakeExpression2(FilterValue: string;var Kontrol : string; AField: string = 'UPPER(TEXT(ENT))'): string;
function SelectionKontrol(FilterValue: string): string;


function CompareNatural(aList: TStringlist; index1, index2: Integer): Integer; // ilker ekleme
function NaturalSortCompare(const S1, S2: string): Integer; // ilker ekleme
function StrCmpLogicalW(psz1, psz2: UnicodeString{PWideChar}): Integer; Stdcall;//ilker ekleme
function CustomNaturalSort(List: TStringList; Index1, Index2: Integer): Integer; // ilker ekleme
procedure TellWindowsWeArentFrozen; // ilker ekleme
function GetDLLName: string; // ilker ekleme
function GetAppVersionInfo(sAppNamePath: string):string; // ilker ekleme
/// <summary>
///   Katmandaki tüm nesneleri siler.
/// </summary>
procedure DeleteAllEntityInLayer(ALayerName: string); overload;
/// <summary>
///   Katmandaki tüm nesneleri siler.
/// </summary>
procedure DeleteAllEntityInLayer(ALayer: TlicgBaseLayer); overload;

procedure ShrinkMemory;
function FindLastName(drawBox: TlicgBaseDrawbox; sPrefix: string; layer:
  TlicgBaselayer = nil; FindObjectType: TlicgEntityID = idPoint): string;//Uğur Ekleme

var
  GlobalTempEntity: IlicgEntity; { this temporary entity is used in several situations }
  ScreenList: TList;
  LastPeekMessageTime: Cardinal = 0;

implementation

uses
  ShellApi,
  Printers,
  Math,
  ZLib,
  Lider.CG.Com.Math,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.YaccLib,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.Rtree,
  Lider.CG.Com.LicadInt;

procedure SortList(ol: TList);

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P, T: TObject;
  begin
    repeat
      I := L;
      J := R;
      P := ol[(L + R) shr 1];
      repeat
        while Longint(ol[I]) < Longint(P) do
          Inc(I);
        while Longint(ol[J]) > Longint(P) do
          Dec(J);
        if I <= J then
        begin
          T := ol[I];
          ol[I] := ol[J];
          ol[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ol.Count > 0 then
    QuickSort(0, ol.Count - 1);
end;

procedure SaveStringsAsUnicode(const thefilename: string; theStringlist: TStrings);
var
  WS: WideString;
  fs: TFileStream;
  byteorder_marker: Word;
begin
  WS := theStringList.Text;
  fs := Tfilestream.create(thefilename, fmCreate);
  try
    byteorder_marker := $FEFF;
    fs.WriteBUffer(byteorder_marker, sizeof(byteorder_marker));
    fs.WriteBuffer(WS[1], Length(WS) * Sizeof(WS[1]));

      {
      You should check whether WS <> '', otherwise your little
      source will throw out an exception if WS = ''. Or use
      PWideChar(WS)^ instead of WS[1].
      }
  finally
    fs.free
  end;
end;

{This function will return the number of bits per pixel
 (8, 16, 24...) for the current desktop resolution }
function GetCurrentColorDepth: Integer;
var
  topDC: HDC;
begin
  topDC := GetDC(0);
  try
    Result := GetDeviceCaps(topDC, BITSPIXEL) * GetDeviceCaps(topDC, PLANES);
  finally
    ReleaseDC(0, topDC);
  end;
end;

function StringIndex(const SearchString: string; const StrList: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(StrList) do
    if CompareText(SearchString, StrList[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function Dpi2Units(iUnits: TlicgScaleUnits; Dpis, Value: integer): Double;
begin
  Result := Value / Dpis;
  if iUnits = suMms then
    Result := Result * 25.4
  else if iUnits = suCms then
    Result := Result * 2.54;
end;

function Units2Dpi(iUnits: TlicgScaleUnits; Dpis: Integer; Value: Double): integer;
var
  u: Double;
begin
  u := Value;
  if iUnits = suMms then
    u := Value / 25.4
  else if iUnits = suCms then
    u := Value / 2.54;
  Result := trunc(u * Dpis);
end;

{ Field2Exprtype }

function Field2Exprtype(Layer: TlicgBaseLayer; FieldNo: Integer): TExprtype;
begin
  Result := ttString;
  if Layer.DBTable = nil then
    Exit;

  case Layer.DBTable.FieldType(FieldNo) of
    'C':
      Result := ttString;
    'N', 'F', 'T':
      Result := ttFloat;
    'D', 'I':
      Result := ttInteger;
    'L':
      Result := ttBoolean;
  end;
end;

function Field1Exprtype(Layer: TlicgBaseLayer; FieldName: string): TExprtype;
begin
  Result := ttString;
  if Layer.DBTable = nil then
    Exit;

  case Layer.DBTable.FieldType(Layer.DBTable.FieldNo(FieldName)) of
    'C':
      Result := ttString;
    'N', 'F', 'T':
      Result := ttFloat;
    'D', 'I':
      Result := ttInteger;
    'L':
      Result := ttBoolean;
  end;
end;

procedure CalculateAnchorPoints(const LineFrom, LineTo: TlicgCoor; const Distance:
  Double; var ap1s, ap2s, ap1s_cw, ap1s_ccw, ap2s_cw, ap2s_ccw: TlicgCoor);
var
  pc: TlicgCoor;
  RefLength, Scale: Double;
  Matrix: TlicgMatrix;
  bolen: Integer;
begin
  { calculate the center of line segment }
  bolen := 2;

  pc := AsCoor((LineFrom.x + LineTo.x) / bolen, (LineFrom.y + LineTo.y) / bolen);

  { calculate a reference length for scaling }
  RefLength := _Distance(pc, LineFrom);
  if RefLength = 0 then
    RefLength := Distance;
  { now scale the line segment }
  Scale := 1 + Distance / RefLength;
  Matrix := Scale2D(Scale, Scale, pc);
  ap1s := TransformPoint2D(LineFrom, Matrix);
  ap2s := TransformPoint2D(LineTo, Matrix);

  { rotate p1s to the right  }
  Matrix := Rotate2D(System.Pi / bolen, LineFrom);
  ap1s_cw := TransformPoint2D(ap1s, Matrix);
  { rotate p1s to the left  }
  Matrix := Rotate2D(-System.Pi / bolen, LineFrom);
  ap1s_ccw := TransformPoint2D(ap1s, Matrix);

  { now the opposite point }
  { rotate p2s to the right  }
  Matrix := Rotate2D(System.Pi / bolen, LineTo);
  ap2s_cw := TransformPoint2D(ap2s, Matrix);
  { rotate p1s to the left  }
  Matrix := Rotate2D(-System.Pi / bolen, LineTo);
  ap2s_ccw := TransformPoint2D(ap2s, Matrix);
end;

procedure CreateOffsetEntitysInPolyArea(Entity: IlicgEntity; Distance: Double);
type
  _TMatrix = packed record
    Matrix: array[0..2, 0..2] of Double;
  end;

  function _Scale2D(Sx, Sy: Double; refPt: TlicgCoor): _TMatrix;
  begin
    Result.Matrix[0, 0] := Sx;
    Result.Matrix[0, 2] := (1 - Sx) * refPt.X;
    Result.Matrix[1, 1] := Sy;
    Result.Matrix[1, 2] := (1 - Sy) * refPt.Y;
  end;

  function _TransformCoor2D(P: TlicgCoor; T: _TMatrix): TlicgCoor;
  begin
    Result.X := T.Matrix[0, 0] * P.X + T.Matrix[0, 1] * P.Y + T.Matrix[0, 2];
    Result.Y := T.Matrix[1, 0] * P.X + T.Matrix[1, 1] * P.Y + T.Matrix[1, 2];
  end;

var
  SourceVect, DestVect: IlicgVector;
  i: integer;
  E: IlicgEntity;
  S: _TMatrix;
  CP, p1, p2: TlicgCoor;
  MinArea, Area: double;
begin
  if Assigned(Entity) and (Entity.EntityID = idPolygon) then
  begin

    SourceVect := Entity.Geometry.Points;

    DestVect := Licad.CreateEntityFactory.MakeVector(_2D, SourceVect.Count);

    MinArea := Distance * Distance;
    Area := Entity.Geometry.points.Area;
    CP := Entity.Centroid;
    S := _Scale2D(0.9, 0.9, cp);
    while Area > MinArea do
    begin
      DestVect.Clear;
      for i := 0 to SourceVect.Count - 1 do
      begin
        p1 := SourceVect[i];
        p2 := _TransformCoor2D(p1, S);
        DestVect.Add(p2.X, p2.Y);
      end;

      E := Licad.CreateEntityFactory.MakePolygon([]);
      E.Geometry.Points.Assign(DestVect);

      E.DrawTools.BrushTool.Assign(Entity.DrawTools.BrushTool);
      E.DrawTools.PenTool.Assign(Entity.DrawTools.PenTool);
      CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(E);

      S := _Scale2D(0.9, 0.9, cp);
      Area := E.Geometry.points.Area;
      SourceVect := E.Geometry.Points;

    end;

    DestVect := nil;

  end;

end;

procedure MoveDisplayOrder(DrawBox: TlicgBaseDrawBox; Code: Integer; UseThread:
  Boolean = False);
var
  SelLayer: TlicgBaseSelectionLayer;
  Layer: TlicgBaseLayer;
  ARecno: Integer;
begin
  with DrawBox do
  begin
    if Selection.NumSelected <> 1 then
      Exit;
    selLayer := Selection.Items[0];
    Layer := selLayer.Layer;
    if Layer.LayerInfo.Locked then
      Exit;

    case Code of
      1:
        ARecno := Layer.SendEntityToBack(SelLayer.SelList[0]);
      2:
        ARecno := Layer.BringEntityToFront(SelLayer.SelList[0]);
      3:
        ARecno := Layer.MoveEntityForward(SelLayer.SelList[0]);
      4:
        ARecno := Layer.MoveEntityBackwards(SelLayer.SelList[0]);
    end;

    // select it
    Selection.Clear;
    Selection.Add(Layer, ARecno);
    if UseThread then
      RepaintInThread
    else
      Repaint;
  end;
end;

{ DoShowGuideLines }

procedure DoShowGuideLines(DrawBox: TlicgBaseDrawBox; ACanvas: TCanvas;
  HGuideLines, VGuideLines: IlicgDoubleList);
var
  I, orientation: Integer;
  X, Y, Coor: Double;
  TmpPt1, TmpPt2: TPoint;

  procedure DrawGuideLine;
  begin
    with DrawBox.Grapher do
    begin
      case Orientation of
        0:
          begin
            Y := Coor;
            with CurrentParams.VisualWindow do
              if (Y >= LowerLeft.y) and (Y <= UpperRight.y) then
              begin
                { dibuja la linea guia }
                TmpPt1 := RealToPoint(AsCoor(LowerLeft.x, Y));
                TmpPt2 := RealToPoint(AsCoor(UpperRight.x, Y));
                with ACanvas do
                begin
                  MoveTo(TmpPt1.X, TmpPt1.Y);
                  LineTo(TmpPt2.X, TmpPt2.Y);
                end;
              end;
          end;
        1:
          begin
            X := Coor;
            with CurrentParams.VisualWindow do
              if (X >= LowerLeft.x) and (X <= UpperRight.x) then
              begin
                RealToPoint(AsCoor(LowerLeft.x, Y));
                TmpPt1 := RealToPoint(AsCoor(X, LowerLeft.y));
                TmpPt2 := RealToPoint(AsCoor(X, UpperRight.y));
                with ACanvas do
                begin
                  MoveTo(TmpPt2.X, TmpPt2.Y);
                  LineTo(TmpPt1.X, TmpPt1.Y);
                end;
              end;
          end;
      end;
    end;
  end;

begin
  if DrawBox.IsAerial or ((HGuideLines.Count = 0) and (VGuideLines.Count = 0)) then
  begin
    Exit;
  end;
  //if (odCanvas in DrawBox.OutputDevices) then
  begin
    with DrawBox {.ScreenBitmap} do
    begin
      DrawBox.Grapher.SaveCanvas(ACanvas);
      with ACanvas do
      begin
        Brush.Style := bsClear;
        Pen.Mode := pmCopy;
        Pen.Color := clBlue;
        Pen.Width := 1;
        Pen.Style := psDot;
      end;
      orientation := 0;
      for I := 0 to HGuideLines.Count - 1 do
      begin
        Coor := HGuideLines[I];
        DrawGuideLine;
      end;
      orientation := 1;
      for I := 0 to VGuideLines.Count - 1 do
      begin
        Coor := VGuideLines[I];
        DrawGuideLine;
      end;
      DrawBox.Grapher.RestoreCanvas(ACanvas);
    end;
  end;
end;

{ PaintDrawBoxFSGrid }

procedure PaintDrawBoxFSGrid(DrawBox: TlicgBaseDrawBox; const WCRect: TlicgExtent);
var
  X, Y, AX1, AY1, AX2, AY2: Double;
  DeltaX, DeltaY: Integer;
  p: TPoint;
begin
  with DrawBox, DrawBox.ScreenBitmap.Canvas, WCRect do
  begin
    if (ScreenGrid.Step.X <= 0) or (ScreenGrid.Step.Y <= 0) then
      Exit;

    Pen.Color := ScreenGrid.Color;
    Pen.Mode := pmCopy;
    Pen.Width := 1;
    DeltaX := Abs(Grapher.RealToDistX(ScreenGrid.Step.X));
    DeltaY := Abs(Grapher.RealToDistY(ScreenGrid.Step.Y));
    if (DeltaX < 8) or (DeltaY < 8) then
      Exit;
    AX1 := Trunc(LowerLeft.x / ScreenGrid.Step.X) * ScreenGrid.Step.X;
    AY1 := Trunc(LowerLeft.y / ScreenGrid.Step.Y) * ScreenGrid.Step.Y;
    AX2 := UpperRight.x;
    AY2 := UpperRight.y;
    X := AX1;
    while X < AX2 do
    begin
      Y := AY1;
      while Y < AY2 do
      begin
        p := Grapher.RealToPoint(AsCoor(X, Y));
        // the horz line
        MoveTo(0, p.Y);
        LineTo(Width, p.Y);
        // the vert line
        MoveTo(p.X, 0);
        LineTo(p.x, Height);
        Y := Y + ScreenGrid.Step.Y;
      end;
      X := X + ScreenGrid.Step.X;
    end;
  end;
end;

{ BlinkEntityIndirect }

procedure BlinkEntityIndirect(DrawBox: TlicgBaseDrawBox; Entity: IlicgEntity);
var
  I: Integer;
begin
  if (Entity.EntityID in NoneEntityIDs) or not Entity.IsVisible(DrawBox.Grapher.CurrentParams.VisualWindow)
    then
    Exit;
  for I := 1 to DrawBox.BlinkCount do
  begin
    HiliteEntity(Entity, DrawBox);
    DrawBox.Refresh;
    Sleep(DrawBox.BlinkRate);
    DrawBox.Refresh;
    Sleep(DrawBox.BlinkRate);
  end;
end;

{ BlinkEntity }

procedure BlinkEntity(DrawBox: TlicgBaseDrawBox; Layer: TlicgBaseLayer; RecNo: Integer);
var
  Entity: IlicgEntity;
begin
  Entity := Layer.LoadEntityWithRecNo(RecNo);
  if Entity = nil then
    Exit;
  try
    BlinkEntityIndirect(DrawBox, Entity);
  finally
    Entity := nil;
  end;
end;

procedure HiliteEntity(Entity: IlicgEntity; DrawBox: TlicgBaseDrawBox);
var
//  TempEntity: IlicgEntity;
  er: TlicgExtent;
begin
////  TempEntity := nil;
//  try
//    if (Entity.EntityID in [idPlace, idBlockInsert, idPersistBitmap, idVectorialFittedText,
//      idDimHorizontal, idDimVertical, idDimParallel]) then
//    begin
//
//      if Entity.EntityID = idPlace then
//        er := AslicgPlace(Entity.Geometry).CalcRealSizeExtension(DrawBox.Grapher)
//      else if Entity.EntityID = idBlockInsert then
//      begin
//        er := AslicgBlockInsert(Entity.Geometry).CalcRealSizeExtension(DrawBox.Grapher);
//
//      end
//      else
//        er := Entity.Geometry.Extent;
//
//      if Entity.EntityID = idVectorialFittedText then
//      begin
//        TempEntity := Licad.CreateEntityFactory.MakePolygon([]);
//        TempEntity.Geometry.Points.Assign(Entity.Geometry.Points);
//      end
//      else
//        with er do
//          TempEntity := Licad.CreateEntityFactory.MakePolygon([LowerLeft, AsCoor
//            (UpperRight.x, LowerLeft.y), UpperRight, AsCoor(LowerLeft.x,
//            UpperRight.y), LowerLeft]);
//
////      TempEntity.DrawTools.PenTool.Style := 0;
////      TempEntity.DrawTools.PenTool.Color := Licad.Settings.HilitePen.Color;
////      TempEntity.DrawTools.PenTool.Width := Licad.Settings.HilitePen.Width;
////      if Licad.Settings.HiliteBrush.ForeColor = Graphics.clNone then
////        TempEntity.DrawTools.BrushTool.Pattern := 0
////      else
////      begin
////        TempEntity.DrawTools.BrushTool.Pattern := 0;
////        TempEntity.DrawTools.BrushTool.ForeColor := Licad.Settings.HiliteBrush.ForeColor;
////      end;
//
////      if Entity.EntityID = idBlockInsert then
////      begin
////        if (TempEntity.DrawTools.BrushTool <> nil) then
////        begin
////          TempEntity.DrawTools.BrushTool.Pattern := 7;
////          TempEntity.DrawTools.BrushTool.BackColor := Graphics.ClNone;
////        end;
////      end;
//
//    end
//    else
//    begin
//      TempEntity := Entity.Clone;
//      if TempEntity.DrawTools.PenTool <> nil then
//      begin
//        if Entity.DrawTools.PenTool.Style = 0 then
//          if Entity.Layer <> nil then
//            TempEntity.DrawTools.PenTool.Style := TlicgBaseLayer(Entity.Layer).LayerInfo.DefPenTool.Style;
////           else
////            TempEntity.DrawTools.PenTool.Style := 0
////        else
////          TempEntity.DrawTools.PenTool.Style := 0;
//        TempEntity.DrawTools.PenTool.Color := Licad.Settings.HilitePen.Color;
//        TempEntity.DrawTools.PenTool.Width := Licad.Settings.HilitePen.Width;
//      end;
//      if (TempEntity.DrawTools.BrushTool <> nil) and (not (TempEntity.EntityID in TextEntityIDs)) then //and (Licad.Settings.HiliteBrush.ForeColor <> Graphics.clNone) then
//      begin
//        if Entity.DrawTools.BrushTool.Pattern = 0 then
//          if Entity.Layer <> nil then
//            TempEntity.DrawTools.BrushTool.Pattern := TlicgBaseLayer(Entity.Layer).LayerInfo.DefBrushTool.Pattern;
////           else
////            TempEntity.DrawTools.BrushTool.Pattern := 0
////        else
////          TempEntity.DrawTools.BrushTool.Pattern := 0;
//        TempEntity.DrawTools.BrushTool.ForeColor := Licad.Settings.HiliteBrush.ForeColor;
//      end;
////      else if TempEntity.DrawTools.BrushTool <> nil then
////        if Entity.ApplyLayerProps then
////          if Entity.Layer <> nil then
////            TempEntity.DrawTools.BrushTool.Pattern := TlicgBaseLayer(Entity.Layer).LayerInfo.DefBrushTool.Pattern;
////           else
////            TempEntity.DrawTools.BrushTool.Pattern := 0
////        else
////          TempEntity.DrawTools.BrushTool.Pattern := 0;
//
////      if (TempEntity.DrawTools.BrushTool <> nil) then
////        TempEntity.DrawTools.BrushTool.BackColor := Graphics.ClNone;
//
//    end;
//    //if TempEntity.IsPolygon then
//      //TempEntity.DrawTools.BrushTool.BackColor := Graphics.ClNone;
//
//    if Entity.EntityID in [idPlace, idBlockInsert] + TextEntityIDs then

      DrawBox.Add2EntityOnceDisplayList(Entity);
//    else
//      DrawBox.Add2EntityOnceDisplayList(TempEntity);
//
    DrawBox.Refresh;
//  except
//    DrawBox.Refresh;
//  end;
end;

procedure UnHiliteEntity(Entity: IlicgEntity; DrawBox: TlicgBaseDrawBox);
var
  TmpR: TlicgExtent;
begin
  DrawBox.ClearOnceDisplayList;
  TmpR := Entity.Geometry.Extent;
  InflateExtent(TmpR, 0.001, 0.001);

  DrawBox.RefreshExtent(TmpR);
end;

{ BlinkEntities }

procedure BlinkEntities(DrawBox: TlicgBaseDrawBox);
var
  Entity: IlicgEntity;
  I, J, L, S: Integer;
  Found: Boolean;
  Layer: TlicgBaseLayer;
begin

  Found := False;
  { for every blink }
  for I := 1 to DrawBox.BlinkCount do
  begin
    { for every type of painting }
    for S := 1 to 2 do
    begin

      { for every layer }
      for L := 0 to DrawBox.GIS.Layers.Count - 1 do
      begin
        Layer := DrawBox.GIS.Layers[L];
        if not Layer.Active or not Layer.HasBlinkers then
          Continue;

        { for every entity on the layer marked for blinking }
        for J := 0 to Layer.Blinkers.Count - 1 do
        begin
          Entity := Layer.LoadEntityWithRecNo(Layer.Blinkers[J]);

          if Entity = nil then
            Continue;

          if (Entity.EntityID in NoneEntityIDs) or not Entity.IsVisible(DrawBox.Grapher.CurrentParams.VisualWindow)
            then
          begin
            Entity := nil;
            Continue;
          end;

          if S = 1 then
          begin
            HiliteEntity(Entity, DrawBox);
          end
          else if S = 2 then
          begin
            UnHiliteEntity(Entity, DrawBox);
          end;
          Found := True;

          Entity := nil;
        end;
      end;

      if not Found then
        Break;

      Sleep(DrawBox.BlinkRate);

    end;
    if not Found then
      Break;
  end;
end;


procedure WriteFontToIniFile(AIniFile: TIniFile; const ASection: string; AFont: TFont); // ilker ekleme
begin
  AIniFile.WriteString(aSection, 'Font', AFont.Name + ',' +
                                     IntToStr(AFont.CharSet) + ',' +
                                     IntToStr(AFont.Color) + ',' +
                                     IntToStr(AFont.Size) + ',' +
                                     IntToStr(Byte(AFont.Style)));
end;

procedure ReadFontFromIniFile(AIniFile: TIniFile; const ASection: string; AFont: TFont); // ilker ekleme
var
  s, Data: string;
  i: Integer;
begin
  s := AIniFile.ReadString(ASection, 'Font', ',,,,');
  try
    i := Pos(',', s);
    if i > 0 then
    begin
      {Name}
      Data := Trim(Copy(s, 1, i - 1));
      if Data <> '' then
        AFont.Name := Data;
      Delete(s, 1, i);
      i := Pos(',', s);
      if i > 0 then
      begin
        {CharSet}
        Data := Trim(Copy(s, 1, i - 1));
        if Data <> '' then
          AFont.Charset := TFontCharSet(StrToIntDef(Data, AFont.Charset));
        Delete(s, 1, i);
        i := Pos(',', s);
        if i > 0 then
        begin
          { Color }
          Data := Trim(Copy(s, 1, i - 1));
          if Data <> '' then
            AFont.Color := TColor(StrToIntDef(Data, AFont.Color));
          Delete(s, 1, i);
          i := Pos(',', s);
          if i > 0 then
          begin
            {Size}
            Data := Trim(Copy(s, 1, i - 1));
            if Data <> '' then
              AFont.Size := StrToIntDef(Data, AFont.Size);
            Delete(s, 1, i);
            {Style}
            Data := Trim(s);
            if Data <> '' then
              AFont.Style := TFontStyles(Byte(StrToIntDef(Data, Byte(AFont.Style))));
          end
        end
      end
    end;
  except
  end;
end;

function ReadFloatFromIniFile(AIniFile: TIniFile; const ASection, AIdent: string; const ADefault: Double): Double;
var
  Temp: string;
  Code: Integer;
begin
  System.Str(ADefault: 32: 16, Temp);
  Temp := AInifile.ReadString(ASection, AIdent, Trim(Temp));
  System.Val(Temp, Result, Code);
  if Code <> 0 then
    Result := 0;
  if Abs(Result) < 1E-10 then
    Result := 0;
end;

function ReadIntFromIniFile(AIniFile: TIniFile; const ASection, AIdent: string; const ADefault: Integer): Integer;
var
  Temp: string;
  Code: Integer;
begin
  Temp := AIniFile.ReadString(ASection, AIdent, IntToStr(ADefault));
  Val(Temp, Result, Code);
  if Code <> 0 then
    Result := 0;
end;

procedure WriteFloatToIniFile(AIniFile: TIniFile; const ASection, AIdent: string; AValue: Double);
var
  Temp: string;
begin
  if Abs(AValue) < 1E-10 then
    AValue := 0;
  System.Str(AValue: 32: 16, temp);
  AInifile.WriteString(ASection, AIdent, Trim(Temp));
end;

{ DeleteDuplicatedVertexes }

function DegMinSec2Extended(const DegMinSec: TDegMinSec): Double;
begin
  with DegMinSec do
  begin
    Result := (Minutes * 60 + Seconds) / 3600.0 + Abs(Degrees);
    if Degrees < 0 then
      Result := -Result;
  end;
end;

function Extended2DegMinSec(const RealDeg: Double): TDegMinSec;
var
  Seconds, Working: Double;
begin
  Working := Abs(RealDeg);
  Result.Degrees := Trunc(Working);
  Seconds := Frac(Working) * 3600;
  Result.Minutes := Trunc(Seconds / 60);
  Result.Seconds := Seconds - Result.Minutes * 60;
  if RealDeg < 0 then
    Result.Degrees := -Result.Degrees;
end;

function DeleteFileChecking(const FileName: string): Boolean;
begin
  Result := false;
  if FileExists(FileName) then
  begin
    try
      TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive).Free;
    except
      Exit;
    end;
    Result := SysUtils.DeleteFile(FileName);
  end;
end;

{ -- Create Tlicgitmap object from TIcon -- }

function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
var
  IWidth, IHeight: Integer;
begin
  IWidth := Icon.Width;
  IHeight := Icon.Height;
  Result := TBitmap.Create;
  try
    Result.Width := IWidth;
    Result.Height := IHeight;
    with Result.Canvas do
    begin
      Brush.Color := BackColor;
      FillRect(Rect(0, 0, IWidth, IHeight));
      Draw(0, 0, Icon);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function AddSlash(const Path: string): string; // ilker Test Ok.
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[length(Result)] <> '\') then
    Result := Result + '\'
end;

function RemoveSlash(const Path: string): string;
var
  rlen: integer;
begin
  Result := Path;
  rlen := length(Result);
  if (rlen > 0) and (Result[rlen] = '\') then
    Delete(Result, rlen, 1);
end;

//const
 //TempPrefix : string = 'Licad';

function GetTemporaryLayerName(const Path, Prefix: string): string;
(*var
  FileName: array[0..1023] of Ansichar; //ilker değiştirme
  _Prefix : string;
begin
  _Prefix := Prefix;
  GetTempFileNameA(PAnsiChar(Path), PAnsiChar(_Prefix), 0, FileName);
  Result := FileName;
//  Result := Result   + '~' + TempPrefix;
  windows.DeleteFileA(FileName);
end;*)
var
  FileName: array[0..MAX_PATH] of Char;
  _Prefix: string;
begin
  _Prefix := Prefix;
  GetTempFileName(PChar(Path), PChar(_Prefix), 0, FileName);
  Result := FileName;
  Windows.DeleteFile(FileName);
end;

function GetTemporaryEZFFileNameinEXE(const _fn: string): string;
var
  TempDir: string;
  fn: string;
begin
  Result := '';
  if not Assigned(Licad) then
    Exit;


  TempDir := GetTempLicadGISProjectsPath;

  if not TDirectory.Exists(TempDir) then
    TDirectory.CreateDirectory(TempDir);

  if TDirectory.Exists(TempDir) then
  begin
    TempDir := AddSlash(TempDir);
    //TempDir := GetTemporaryProjePath + '\' + TPath.GetGUIDFileName(True);// + '\' + ExtractFileNameOnly(AProjectFile);
    TempDir := Lider.CG.Com.StringLi.GetNewDirName(TempDir, ChangeFileExt(ExtractFileName(_fn), ''));

    CreateDir(TempDir);

    if TDirectory.Exists(TempDir) then
    begin

      fn := AddSlash(TempDir) + ExtractFileName(TempDir);
      fn := ChangeFileExt(fn, EXT_MAP);
      Result := ChangeFileExt(fn, '.TMP');
    end;

  end;

end;

function isNotSavedGIS(filename: string): Boolean;
begin
  Result := AnsiCompareText(ExtractFilePath(FileName), GetTemporaryFilePath) = 0;
end;

function GetTemporaryFilePath: string;
(*var
  TempPath: array[0..1023] of Ansichar; // ilker değiştirme
  FileName: array[0..1023] of Ansichar; // ilker değiştirme
begin
  GetTempPathA(1023, TempPath);

  GetTempFileNameA(TempPath, '', 0, FileName);

  Result := ExtractFilePath(FileName);

  sysutils.DeleteFile(FileName);

end; *)
var
  TempPath: array[0..MAX_PATH] of Char;
  FileName: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, TempPath);
  GetTempFileName(TempPath, '', 0, FileName);
  Result := ExtractFilePath(FileName);
  Windows.DeleteFile(FileName);
end;

function GetTempLicadGISPath: string;
begin
  Result := TPath.GetTempPath + 'LicadGIS\';
  if not TDirectory.Exists(Result) then
  begin
    TDirectory.CreateDirectory(Result);
  end;
end;

function GetTempLicadGISProjectsPath: string;
begin
  Result := TPath.GetTempPath + 'LicadGIS\Projects\';
  if not TDirectory.Exists(Result) then
  begin
    TDirectory.CreateDirectory(Result);
  end;
end;

function GetTempLicadGISTempPath: string;
begin
  Result := TPath.GetTempPath + 'LicadGIS\Temp\';
  if not TDirectory.Exists(Result) then
  begin
    TDirectory.CreateDirectory(Result);
  end;
end;

function GetTempFileNameLicadGISTemp(const Prefix: string): string;
var
  TempPath: array[0..MAX_PATH] of Char;
  FileName: array[0..MAX_PATH] of Char;
  _Prefix: string;
begin
  GetTempPath(MAX_PATH, TempPath);
  _Prefix := Prefix;
  if not TDirectory.Exists(TempPath + 'LicadGIS\Temp\') then
  begin
    TDirectory.CreateDirectory(TempPath + 'LicadGIS\Temp\');
  end;

  GetTempFileName(PChar(TempPath + 'LicadGIS\Temp\'), PChar(_Prefix), 0, FileName);
  Result := FileName;
  Windows.DeleteFile(FileName);
end;

function GetTempProjectNameLicadGISTemp: string;
var
  ProjePath, ProjeName: string;
begin
  ProjeName := TPath.GetGUIDFileName(True);
  ProjePath := TPath.GetTempPath + 'LicadGIS\Temp\' + ProjeName + '\';
  if not TDirectory.Exists(ProjePath) then
  begin
    TDirectory.CreateDirectory(ProjePath);
  end;
  Result := ProjePath + ProjeName;
end;

function MessageToUser(const Msg, Caption: string; AType: Word): Word;
begin
  if Msg <> sUnrecognizedCommand then
    Result := Application.Messagebox(PChar(Msg), PChar(Caption), MB_OK or AType);
end;

procedure AddMarker(DrawBox: TlicgBaseDrawBox; const X, Y: Double; SetInView:
  Boolean; UseThread: Boolean = False; margin: double = 0);
var
  TmpPlace: IlicgEntity;
  Extents: TlicgExtent;
  CX, CY, TmpWidth, TmpHeight, TmpMargin: Double;
begin
  with DrawBox, Licad.Settings do
  begin
    if CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Count = 0 then
      Exit;
    if CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count = 0 then
      Exit;

    TmpPlace := Licad.CreateEntityFactory.MakePlace(AsCoor(X, Y));
    with TmpPlace.DrawTools.Symboltool do
    begin
      if SymbolMarker > CurrCmdLine.ActiveDrawBox.GIS.SymbolsList.Items[0].Count - 1 then
        SymbolMarker := 0;
      Index := SymbolMarker;
      Height := Grapher.getrealsize(DefSymbolStyle.height);
      TmpPlace.Geometry.UpdateExtension;
      TempEntities.Add(TmpPlace);
    end;

    if not SetInView then
      Exit;
    TmpPlace.Geometry.UpdateExtension;
    Extents := TmpPlace.Geometry.Extent;
    with Extents do
    begin
      CX := (LowerLeft.x + UpperRight.x) / 2;
      CY := (LowerLeft.y + UpperRight.y) / 2;
    end;


    {
    with Grapher.CurrentParams.VisualWindow do
    begin
      TmpWidth := (UpperRight.x - LowerLeft.x) / 2;
      TmpHeight := (UpperRight.y - LowerLeft.y) / 2;
    end;
    }

    if Drawbox.IsGeographicCS then
      TmpMargin := metertoDeg(margin)
    else
      TmpMargin := margin;

    {
    with Extents do
    begin
      LowerLeft.x  := CX - TmpWidth  - margin;
      UpperRight.x := CX + TmpWidth  + margin;
      LowerLeft.y  := CY - TmpHeight - margin;
      UpperRight.y := CY + TmpHeight + margin;
    end;
    }

    with Extents do
    begin
      LowerLeft.x := CX - margin;
      UpperRight.x := CX + margin;
      LowerLeft.y := CY - margin;
      UpperRight.y := CY + margin;
    end;

    Grapher.SetViewTo(Extents);

    if UseThread then
      RepaintInThread
    else
      Repaint;
  end;
end;

{ freelist }

procedure FreeList(var alist: TList);
var
  i: Integer;
begin
  if alist = nil then
    Exit;
  for i := 0 to alist.count - 1 do
    TObject(alist[i]).free;

  FreeAndNil(alist);
end;

{ GetListOfVectors }

function GetListOfVectors(const Entity: IlicgEntity): IInterfaceList;
var
  I, n, Idx1, Idx2: Integer;
  V, SrcV: IlicgVector;
begin
  if Entity.EntityID in [idLine, idPolyline, idPolygon, idTriangle, idPafta] then
    SrcV := Entity.Geometry.Points
  else
    SrcV := Entity.Geometry.DrawPoints;
  Result := TInterfaceList.Create;
  n := 0;
  if SrcV.PartCount < 2 then
  begin
    Idx1 := 0;
    Idx2 := SrcV.Count - 1;
  end
  else
  begin
    Idx1 := SrcV.Parts[n];
    Idx2 := SrcV.Parts[n + 1] - 1;
  end;
  repeat
    V := Licad.CreateEntityFactory.MakeVector(3, succ(Idx2 - Idx1));
    for I := Idx1 to Idx2 do
      V.Add(SrcV[I].x, SrcV[I].y);
    Result.Add(V);
    if SrcV.PartCount < 2 then
      Break;
    Inc(n);
    if n >= SrcV.PartCount then
      Break;
    Idx1 := SrcV.Parts[n];
    if n < SrcV.PartCount - 1 then
      Idx2 := SrcV.Parts[n + 1] - 1
    else
      Idx2 := SrcV.Count - 1;
  until false;
end;

{ CombineSelection }

procedure CombineSelection(DrawBox: TlicgBaseDrawBox; DeleteOriginals: Boolean);
var
  Combined, TmpEnt: IlicgEntity;
  I, K, n, Idx, cnt, NewRecno, SourceRecnoClosed, SourceRecnoOpened: Integer;
  TempL, ListClosed, ListOpened: IInterfaceList;
  LayerListClosed, LayerListOpened: TList;
  SourceLayerOpened, SourceLayerClosed: TlicgBaseLayer;
  IsSame: Boolean;
  V: IlicgVector;
  AvoidList: IlicgIntegerList;
begin
  { Combine all open entities
     (TlicgLine, TlicgPolyline, TlicgArc, TSpline2D) into one single entity }
  { Count all open entities }
  with DrawBox do
  begin
    if Selection.Count = 0 then
      Exit;
    ListClosed := TInterfaceList.Create;
    ListOpened := TInterfaceList.Create;
    LayerListClosed := TList.Create;
    LayerListOpened := TList.Create;
    SourceLayerClosed := nil;
    SourceLayerOpened := nil;
    SourceRecNoClosed := 0;
    SourceRecNoOpened := 0;
    AvoidList := CreateIntegerList;
    try
      for cnt := 0 to Selection.Count - 1 do
      begin
        with Selection.Items[cnt] do
        begin { TlicgSelectionLayer }
          AvoidList.Clear;
          for K := 0 to SelList.Count - 1 do
          begin
            TmpEnt := Layer.LoadEntityWithRecno(SelList[K]);
            if TmpEnt <> nil then
            begin
              if TmpEnt.IsPolygon then
              begin
                TempL := GetListOfVectors(TmpEnt);
                for I := 0 to TempL.Count - 1 do
                  ListClosed.Add(IlicgVector(TempL[I]));
                TempL := nil;
                LayerListClosed.Add(Layer);
                if SourceLayerClosed = nil then
                begin
                  SourceLayerClosed := Layer;
                  SourceRecnoClosed := K;
                end;
              end
              else
              begin
                TempL := GetListOfVectors(TmpEnt);
                for I := 0 to TempL.Count - 1 do
                  ListOpened.Add(IlicgVector(TempL[I]));
                TempL := nil;
                LayerListOpened.Add(Layer);
                if (SourceLayerOpened = nil) then
                begin
                  SourceLayerOpened := Layer;
                  SourceRecnoOpened := K;
                end;
              end;
              TmpEnt := nil;
            end
            else
              AvoidList.add(SelList[K]);
          end;
          for K := 0 to AvoidList.Count - 1 do
            Delete(AvoidList[K]);
        end;
      end;
      if ListOpened.Count > 1 then
      begin
        (* combine them *)
        Combined := Licad.CreateEntityFactory.MakePolyline([_NULL_COOR]);
        try
          Idx := 0;
          Combined.Geometry.Points.Clear;
          for cnt := 0 to ListOpened.Count - 1 do
          begin
            V := IlicgVector(ListOpened[cnt]);
            n := V.Count;
            Combined.Geometry.Points.AddPart(Idx);
            for K := 0 to n - 1 do
              Combined.Geometry.Points.Add(V[K].x, V[K].y);
            Inc(Idx, n);
          end;
          if Combined.Geometry.Points.PartCount = 1 then
            Combined.Geometry.Points.ClearParts;
          (* Now save this entity to the original layer or to the current layer
             if not possible *)
          IsSame := True;
          for K := 1 to LayerListOpened.Count - 1 do
            if TlicgBaseLayer(LayerListOpened[K]) <> TlicgBaseLayer(LayerListOpened
              [0]) then
            begin
              IsSame := False;
              Break;
            end;
          if IsSame then
          begin
            NewRecno := TlicgBaseLayer(LayerListOpened[0]).AddEntity(Combined, false);
            if NewRecno > 0 then
              SourceLayerOpened.CopyRecord(SourceRecNoOpened, NewRecno);
          end
          else
            DrawBox.GIS.CurrentLayer.AddEntity(Combined);
        finally
          Combined := nil;
        end;
      end;

      if ListClosed.Count > 1 then
      begin
        // combine them
        Combined := Licad.CreateEntityFactory.MakePolygon([_NULL_COOR]);
        try
          Idx := 0;
          Combined.Geometry.Points.Clear;
          for cnt := 0 to ListClosed.Count - 1 do
          begin
            V := IlicgVector(ListClosed[cnt]);
            n := V.Count;
            Combined.Geometry.Points.AddPart(Idx);
            for K := 0 to n - 1 do
              Combined.Geometry.Points.Add(V[K].x, V[K].y);
            Inc(Idx, n);
          end;
          if Combined.Geometry.Points.PartCount = 1 then
            Combined.Geometry.Points.ClearParts;
          (* Now save this entity to the original layer or to the current layer
             if not possible *)
          IsSame := True;
          for k := 1 to LayerListClosed.Count - 1 do
            if TlicgBaseLayer(LayerListClosed[k]) <> TlicgBaseLayer(LayerListClosed
              [0]) then
            begin
              IsSame := False;
              Break;
            end;
          if IsSame then
          begin
            NewRecno := TlicgBaseLayer(LayerListClosed[0]).AddEntity(Combined, false);
            if NewRecno > 0 then
              SourceLayerClosed.CopyRecord(SourceRecNoClosed, NewRecno);
          end
          else
            DrawBox.GIS.CurrentLayer.AddEntity(Combined);
        finally
          Combined := nil;
        end;
      end;
      if ((ListOpened.Count > 1) or (ListClosed.Count > 1)) and DeleteOriginals then
        DeleteSelection;
    finally
      ListClosed := nil;
      ListOpened := nil;
      LayerListClosed.Free;
      LayerListOpened.Free;
      AvoidList := nil;
    end;
  end;

end;

function CreateDllList(FieldList: TStringList): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to pred(FieldList.Count) do
    Result := Result + FieldList[i] + '\';
end;

{---------------------------------------------------------------------}

procedure ReadFile(const Path: string; FileList: TStrings);
var
  SearchRec: TSearchRec;
  FindResult: integer;
begin
  FileList.Clear;
  try
    FindResult := FindFirst(Path, faAnyFile, SearchRec);
    while FindResult = 0 do
    begin
      FileList.Add(SearchRec.Name);
      FindResult := FindNext(SearchRec);
    end;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

function CreateIfNotExists(const FileName: string): TFileStream;
var
  Mode: Word;
begin
  if FileExists(FileName) then
    Mode := fmOpenReadWrite or fmShareDenyNone
  else
    Mode := fmCreate;
  Result := TFileStream.Create(FileName, Mode);
end;

procedure LicadGISError(const ErrStr: string);
begin
  raise Exception.Create(ErrStr);
end;

function HasAttr(const FileName: string; Attr: Word): Boolean;
var
  FileAttr: Integer;
begin
  FileAttr := FileGetAttr(FileName);
  if FileAttr = -1 then
    FileAttr := 0;
  Result := (FileAttr and Attr) = Attr;
end;

procedure licgWriteStrToStreamForNCZ(const TextToWrite: string; stream: TStream);
var
  n: byte;
  ATextToWrite: AnsiString;
begin
  ATextToWrite := AnsiString(TextToWrite);
  n := Length(ATextToWrite);
  stream.write(n, sizeof(byte));
  if n > 0 then
    // ilker sileme stream.write(TextToWrite[1], n);
    stream.write(ATextToWrite[1], n * SizeOf(AnsiChar)); // ilker ekleme
end;

procedure licgWriteStrToStream(const TextToWrite: string; stream: TStream);
var
  n: Integer;
  ATextToWrite: AnsiString;
begin
  ATextToWrite := AnsiString(TextToWrite);
  n := Length(ATextToWrite);
  stream.write(n, sizeof(Integer));
  if n > 0 then
    // ilker silme stream.write(TextToWrite[1], n);
    Stream.Write(ATextToWrite[1], n * SizeOf(AnsiChar)); // ilker ekleme
end;

function licgReadStrFromStream(stream: TStream): string;
var
  n: Integer;
  AResult: AnsiString;
begin
  stream.Read(n, sizeof(Integer));
  if n > 0 then
  begin
    SetLength(AResult, n);
    // ilker silme stream.Read(Result[1], n);
    stream.Read(AResult[1], n * SizeOf(AnsiChar)); // ilker ekleme
    Result := string(AResult);
  end
  else
    Result := '';
end;

function WritePrivateProfileInt(const Section, Entry: string; iToWrite: word;
  const FileName: string): BOOL;
var
  Work: string;
begin
  Work := format('%.6d', [iToWrite]);
  WritePrivateProfileInt := WritePrivateProfileString(PChar(Section), PChar(Entry),
    PChar(Work), PChar(FileName));
end;

function RemoveStrDelim(const S: string): string;
const
  SQUOTE =['''', '"'];
begin
  if (Length(S) >= 2) and (S[1] in SQUOTE) and (S[Length(S)] in SQUOTE) then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

function AddBrackets(const Value: string): string;
begin
  if AnsiPos(#32, Value) > 0 then
    Result := '[' + Value + ']'
  else
    Result := Value;
end;

function DeleteFilesSameName(const Filename: string; DeleteAllFile: Boolean = False): Boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  source: string;
begin
  Result := True;
  layname := ChangeFileExt(FileName, '');
  source := ExtractFilepath(FileName);
  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  try
    while Result and (Found = 0) do
    begin
      //katman adı ile proje adı aynı ise uzantılara sahip proje dosyası silme.
      if (SR.Name <> '.') and (SR.Name <> '..') and (DeleteAllFile or (Pos(EXT_MAP, uppercase(SR.Name)) = 0) and
        (Pos('.ERL', uppercase(SR.Name)) = 0) and (Pos(EXT_REFERANS, uppercase(SR.Name)) = 0) and
        (Pos('.BMP', uppercase(SR.Name)) = 0) and (Pos(EXT_TRANSFORM, uppercase(SR.Name)) = 0)) then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001 or $00000002));
        // Delete file
        if not SysUtils.DeleteFile(source + SR.Name) then
          Result := False;
      end;
      Found := FindNext(SR);
    end;
  finally
    Sysutils.FindClose(SR);
  end;
  Result := True;
end;

function DeleteLayerFiles(const ALayerName: string): Boolean;
var
  ALayerNameNoneExt: string;
  function LayerFileDelete(AEXT: string): Boolean;
  var
    AFileName: string;
  begin
    Result := False;
    AFileName := ALayerNameNoneExt + AEXT;
    if FileExists(AFileName) then
    begin
      Result := SysUtils.DeleteFile(AFileName);
    end;
  end;
begin
  try
    Result := True;
    ALayerNameNoneExt := ChangeFileExt(ALayerName, '');
    LayerFileDelete(EXT_LAYERDB);
    LayerFileDelete(EXT_LAYERDRAW);
    LayerFileDelete(EXT_LAYERINDEX);
    LayerFileDelete(EXT_LAYERRTREE);
    LayerFileDelete(EXT_LAYERCAD);
  except
    Result := False;
  end;
end;

function GetSameFilesSameName(const Filename: string; SL: TStrings): boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  source: string;
begin

  Result := True;
  layname := ChangeFileExt(FileName, '');
  source := ExtractFilepath(FileName);
  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  try
    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001
          or $00000002));
        // Delete file
        SL.Add(source + SR.Name);
      end;
      Found := FindNext(SR);
    end;
  finally
    Sysutils.FindClose(SR);
  end;

  Result := SL.Count > 0;
end;

function CopyFilesSameName(const GIS: TlicgBaseGIS; const NewDir: string;
  NewName: string): Boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  TargetFn, SourceFn, Target, source: string;
begin
  Result := True;
  layname := ChangeFileExt(GIS.FileName, '');
  source := ExtractFilepath(GIS.FileName);
  Target := ExtractFilepath(NewDir);

  //GIS.Close;

  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  Result := (Found = 0);
  try
    while Result and (Found = 0) do
    begin

      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  //  Sr.Attr<>faDirectory) then
      begin
        SourceFn := source + SR.Name;

        if NewName = '' then
          TargetFn := target + SR.Name
        else
          TargetFn := target + NewName;

        TargetFn := ChangeFileExt(TargetFn, ExtractFileExt(SourceFn));

        SourceFn := SourceFn + #0;
        TargetFn := TargetFn + #0;
        if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
          Result := False;
      end;
      Found := FindNext(SR);
    end;
    SourceFn := Source + 'DBConnections.xml';
    TargetFn := target + 'DBConnections.xml';

    SourceFn := SourceFn + #0;
    TargetFn := TargetFn + #0;

    TFile.Copy(SourceFn[1], TargetFn[1], false); //CopyFile(@SourceFn[1], @TargetFn[1], false);

    SourceFn := Source + 'LayerDatasets.xml';
    TargetFn := target + 'LayerDatasets.xml';
    SourceFn := SourceFn + #0;
    TargetFn := TargetFn + #0;

    TFile.Copy(SourceFn[1], TargetFn[1], false); //CopyFile(@SourceFn[1], @TargetFn[1], false);

    sysutils.CreateDir(Target + 'GISQuery');
    Sysutils.FindClose(SR);

    Found := FindFirst(source + 'GISQuery\' + '*.*', faAnyFile, SR);

    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  // Sr.Attr<>faDirectory) then
      begin
        SourceFn := source + 'GISQuery\' + SR.Name;
        TargetFn := target + 'GISQuery\' + SR.Name;

        SourceFn := SourceFn + #0;
        TargetFn := TargetFn + #0;
        if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
          Result := False;
      end;
      Found := FindNext(SR);
    end;

//<-


    sysutils.CreateDir(Target + 'Sorgular');
    Sysutils.FindClose(SR);
//->
    Found := FindFirst(source + 'Sorgular\' + '*.*', faAnyFile, SR);

    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  // Sr.Attr<>faDirectory) then
      begin
        SourceFn := source + 'Sorgular\' + SR.Name;
        TargetFn := target + 'Sorgular\' + SR.Name;

        SourceFn := SourceFn + #0;
        TargetFn := TargetFn + #0;
        if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
          Result := False;
      end;
      Found := FindNext(SR);
    end;

//<-


  finally
    Sysutils.FindClose(SR);
  end;

end;

function CopyFilesSameName(const Layer: TlicgBaseLayer; const NewDir: string;
  NewName: string = ''): Boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  TargetFn, SourceFn, Target, source: string;
begin
  Result := True;
  layname := ChangeFileExt(lAYER.FileName, '');
  source := ExtractFilepath(lAYER.FileName);
  Target := ExtractFilepath(NewDir);

  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  Result := (Found = 0);
  try
    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  // Sr.Attr<>faDirectory) then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001
          or $00000002));
        // Delete file

        SourceFn := source + SR.Name;
        if NewName = '' then
          TargetFn := target + SR.Name
        else
          TargetFn := target + NewName;

        TargetFn := getNewFileName(TargetFn);

        try
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := false;
          if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
            Result := False;
        finally
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := true;
        end;

      end;
      Found := FindNext(SR);
    end;

  finally
    Sysutils.FindClose(SR);
  end;

end;

function CopyFilesSameName(const Layer: TlicgBaseLayer; const layname: string;
  const NewDir: string; NewName: string): Boolean;
var
  SR: TSearchRec;
  Found: Integer;
  TargetFn, SourceFn, Target, source: string;
begin
  Result := True;
  //layname := ChangeFileExt  (lAYER.FileName, '');
  source := ExtractFilepath(layname);
  Target := ExtractFilepath(NewDir);

  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  Result := (Found = 0);
  try
    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  // Sr.Attr<>faDirectory) then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001
          or $00000002));
        // Delete file

        SourceFn := source + SR.Name;
        TargetFn := target + NewName;

        TargetFn := ChangeFileExt(TargetFn, ExtractFileExt(SourceFn));

        try
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := false;
          if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
            Result := False;
        finally
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := true;
        end;

      end;
      Found := FindNext(SR);
    end;

  finally
    Sysutils.FindClose(SR);
  end;

end;

function _CopyFilesSameName(const Layer: TlicgBaseLayer; const NewDir: string;
  var NewName: string): Boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  TargetFn, SourceFn, Target, source: string;
begin
  Result := True;
  layname := ChangeFileExt(lAYER.FileName, '');
  source := ExtractFilepath(lAYER.FileName);
  Target := ExtractFilepath(NewDir);
  NewName := '';

  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  Result := (Found = 0);
  try
    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') and (Sr.Attr <> faDirectory)
        then  // Sr.Attr<>faDirectory) then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001
          or $00000002));
        // Delete file

        SourceFn := source + SR.Name;
        TargetFn := target + SR.Name;

        TargetFn := getNewFileName(TargetFn);

        if UpperCase(ExtractFileExt(TargetFn)) = EXT_LAYERDRAW then
        begin
          NewName := TargetFn;
        end;

        try
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := false;
          if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
            Result := False;
        finally
          if UpperCase(ExtractFileExt(SourceFn)) = '.DBF' then
            Layer.Active := true;
        end;

      end;
      Found := FindNext(SR);
    end;

  finally
    Sysutils.FindClose(SR);
  end;

end;

function MoveFilesSameName(const Filename, NewFileName: string; ChangeFn:
  Boolean; DeleteFiles: Boolean): Boolean;
var
  layname: string;
  SR: TSearchRec;
  Found: Integer;
  TargetFn, SourceFn, Target, source: string;
begin
  Result := True;
  layname := ChangeFileExt(FileName, '');
  source := ExtractFilepath(FileName);
  Target := ExtractFilepath(NewFileName);

  // Remove the files in the directory
  Found := FindFirst(layname + '.*', faAnyFile, SR);
  Result := (Found = 0);
  try
    while Result and (Found = 0) do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
        // Remove attributes that could prevent us from deleting the file
        FileSetAttr(source + SR.Name, FileGetAttr(source + SR.Name) and not ($00000001
          or $00000002));
        // Delete file

        if not ChangeFn then
        begin
          SourceFn := source + SR.Name;
          TargetFn := target + SR.Name;
        end
        else
        begin
          SourceFn := source + SR.Name;
          TargetFn := target + ExtractFileName(NewFileName);
        end;

        if not CopyFile(@SourceFn[1], @TargetFn[1], false) then
          Result := False;

      end;
      Found := FindNext(SR);
    end;

  finally
    Sysutils.FindClose(SR);
  end;

  if (Result) and (DeleteFiles) then
    DeleteFilesSameName(Filename);

end;

procedure GetFileList(filePath: string; fileList: TStringList);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin

  FileAttrs := faAnyFile;

  if FindFirst(filePath, FileAttrs, sr) = 0 then
  begin
    repeat
      if (sr.Attr and FileAttrs) = sr.Attr then
      begin
        fileList.Add(sr.Name);
      end;
    until FindNext(sr) <> 0;
    SysUtils.FindClose(sr);
  end;

end;

function GetValidLayerName(const OrigLayerName: string): string;
var
  I: Integer;
  Found: Boolean;
begin
  Result := OrigLayerName;
  for I := 0 to Length(Result) do
  begin
    if not CharInSet(Result[I], ['A'..'Z', 'a'..'z', '0'..'9', '_', #127..#255]) then
    begin
      if CharInSet(Result[I], ['Ş', 'ş', 'G', 'g', 'İ', 'ı', 'Ö', 'ö', 'Ç', 'ç', 'Ü', 'ü']) then // ilker ekleme Türkçe Karakterleri Silmemeyi sağlıyor.
      begin
        Result[I] := '_';
      end;
    end;
  end;
end;

function TrimCrLf(const s: widestring): widestring;
begin
  Result := s;

  while (Length(Result) > 0) and (Result[Length(Result)] in [Widechar(#13),
    wideChar(#10)]) do
    System.Delete(Result, Length(Result), 1);
end;

function ComplColor(Clr: TColor): TColor;
var
  r, g, b: Byte;
begin
  r := GetRValue(clr);
  g := GetGValue(clr);
  b := GetBValue(clr);
  Result := RGB(255 - r, 255 - g, 255 - b);
end;

function Dark(Col: TColor; Percent: Byte): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(Col);
  G := GetGValue(Col);
  B := GetBValue(Col);
  R := Round(R * Percent / 100);
  G := Round(G * Percent / 100);
  B := Round(B * Percent / 100);
  Result := RGB(R, G, B);
end;

function Light(Col: TColor; Percent: Byte): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(Col);
  G := GetGValue(Col);
  B := GetBValue(Col);
  R := Round(R * Percent / 100) + Round(255 - Percent / 100 * 255);
  G := Round(G * Percent / 100) + Round(255 - Percent / 100 * 255);
  B := Round(B * Percent / 100) + Round(255 - Percent / 100 * 255);
  Result := RGB(R, G, B);
end;

function DefaultFontHandle: HFont;
var
  ncMetrics: TNonClientMetrics;
begin
  ncMetrics.cbSize := sizeof(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(TNonClientMetrics), @ncMetrics, 0);
  Result := CreateFontIndirect(ncMetrics.lfMessageFont);
end;

procedure GetMessageBoxFont(afont: TFont);
begin
  assert(assigned(afont));
  afont.Handle := DefaultFontHandle;
end;

function DefaultFont: TFont;
begin
  Result := TFont.Create;
  GetMessageBoxFont(Result);
end;

function DefaultFontName: string;
var
  aFont: TFont;
begin
  aFont := DefaultFont;
  try
    Result := aFont.Name;
  finally
    aFont.free;
  end;
end;

function GetParentFormHWND(Control: TWinControl): HWND;
begin
  Result := GetParentForm(Control).Handle;
end;
(*
procedure ShowFormTitlebar(Form: TForm);
var
  Save: LongInt;
begin
  with Form do
  begin
    if BorderStyle = bsNone then
      Exit;
    Save := GetWindowLong(Handle, gwl_Style);
    if (Save and ws_Caption) <> ws_Caption then
    begin
      case BorderStyle of
        bsSingle, bsSizeable:
          SetWindowLong(Handle, gwl_Style, Save or ws_Caption or ws_border);
        bsDialog:
          SetWindowLong(Handle, gwl_Style, Save or ws_Caption or ds_modalframe
            or ws_dlgframe);
      end;
      Height := Height + getSystemMetrics(sm_cyCaption);
      Refresh;
    end;
  end;
end;

procedure HideFormTitlebar(Form: TForm);
var
  Save: LongInt;
begin
  with Form do
  begin
    if BorderStyle = bsNone then
      Exit;
    Save := GetWindowLong(Handle, gwl_Style);
    if (Save and ws_Caption) = ws_Caption then
    begin
      case BorderStyle of
        bsSingle, bsSizeable:
          SetWindowLong(Handle, gwl_Style, Save and (not (ws_Caption)) or ws_border);
        bsDialog:
          SetWindowLong(Handle, gwl_Style, Save and (not (ws_Caption)) or
            ds_modalframe or ws_dlgframe);
      end;
      Height := Height - getSystemMetrics(sm_cyCaption);
      Refresh;
    end;
  end;
end;
*)
function ArrayOfByteToHexString(const A: array of Byte): string;
var
  i: Integer;
  Temp: string;
begin
  Result := '';
  for i := Low(A) to High(A) do
  begin
    Temp := ' ' + IntToHex(A[i], 2);
    // following is optional
    if i mod 16 = 0 then
      Temp := Temp + #13#10
    else if (i mod 8) <> 0 then
      Temp := Temp + '-';
  end;
  Result := Trim(Temp);
end;

function FindContrastingColor(Color: TColor): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  if R < 128 then
    R := 255
  else
    R := 0;
  if G < 128 then
    G := 255
  else
    G := 0;
  if B < 128 then
    B := 255
  else
    B := 0;
  Result := RGB(R, G, B);
end;

function StrSplitToList(const sToSplit, sSeparator: string; tsStrList: TStrings;
  bAllowEmpty: Boolean = True): Integer;
var
  iCurPos, iNoStrings: Integer;
  sTmpRet, sTmpStr: string;
begin
  sTmpRet := sToSplit;
  iCurPos := AnsiPos(sSeparator, sToSplit);
  tsStrList.Clear;
  iNoStrings := 0;
  if iCurPos > 0 then
  begin
    while iCurPos > 0 do
    begin
      sTmpStr := Copy(sTmpRet, 0, iCurPos - 1);
      if (Length(sTmpStr) > 0) or bAllowEmpty then { let user choose to get empty strings}
      begin
        tsStrList.Add(sTmpStr);
        Inc(iNoStrings, 1);
      end;
      sTmpRet := Copy(sTmpRet, iCurPos + Length(sSeparator), Length(sTmpRet));
      iCurPos := AnsiPos(sSeparator, sTmpRet);
    end;
    if Length(sTmpRet) > 0 then
    begin
      tsStrList.Add(sTmpRet);
      Inc(iNoStrings, 1);
    end;
  end
  else
  begin
    if (Length(sTmpRet) > 0) or bAllowEmpty then
    begin
      tsStrList.Add(sTmpRet);
      Inc(iNoStrings, 1);
    end;
  end;
  Result := iNoStrings;
end;

function GetPointEntityList(const aGis: TlicgBAseGis; const SL: TStrings):
  IlicgEntityList;
var
  i: integer;
  tmpLayer: TlicgBaseLayer;
  Ent: IlicgEntity;
begin
  Result := TlicgEntityList.Create;
  for i := 0 to aGIS.Layers.Count - 1 do
  begin
    tmpLayer := aGIS.Layers[i];
    tmpLayer.First;
    tmpLayer.StartBuffering;
    while not tmpLayer.Eof do
    begin
      if tmpLayer.RecIsDeleted then
      begin
        tmpLayer.Next;
        Continue;
      end;

      Ent := tmpLayer.RecLoadEntity;

      if (Ent.EntityID in [idPoint]) then
      begin
        if SL.IndexOf(Ent.Name) >= 0 then
          Result.Add(Ent)

      end;
      tmpLayer.Next;
      if Result.Count = SL.Count then
        BREAK;
    end;

    tmpLayer.EndBuffering;

    if Result.Count = SL.Count then
      Break;
  end;
end;

function FindLastName(drawBox: TlicgBaseDrawbox; sPrefix: string; layer:
  TlicgBaselayer = nil; FindObjectType: TlicgEntityID = idPoint): string;
var
  PNamePref: ansiString;
  i, LayerCount, L: Integer;
  tmpLayer: TlicgBaseLayer;
  Ent: IlicgEntity;
  LastNo: Integer;
  tmpPName: ansiString;
  ReadPName: ansiString;
  sc: TCursor;
begin
  //Uğur Ekleme (Orginalden değişti, Orjinali aşağıda)
  sc := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    PNamePref := AnsiString(Trim(sPrefix)); //ön ek
    L := Length(PNamePref); //ön ek uzunluğu
    LastNo := 0;

    if layer = nil then
      i := 0
    else
      for LayerCount := 0 to DrawBox.GIS.Layers.Count - 1 do
      begin
        if (Layer = DrawBox.GIS.Layers[LayerCount]) then
        begin
          i := LayerCount;
          BREAK;
        end;
      end;

    for LayerCount := i to DrawBox.GIS.Layers.Count - 1 do
    begin
      tmpLayer := DrawBox.GIS.Layers[LayerCount];
      tmpLayer.First;
      tmpLayer.StartBuffering;
      while not tmpLayer.Eof do
      begin
        if tmpLayer.RecIsDeleted then
        begin
          tmpLayer.Next;
          Continue;
        end;

        if (tmpLayer.RecEntityID in [FindObjectType]) then
        begin
          Ent := tmpLayer.RecLoadEntity;
          tmpPName := AnsiString(Ent.Name);
          if (Result < string(tmpPName)) then
            Result := String(tmpPName)
          else if (Length(Result) < Length(tmpPName)) then// ZA6 ile ZA10 Karşılaştırmasında Kullanılıyor
          begin
            for I := 1 to Length(Result) do
            begin
              if copy(Result, i, 1) = string(tmpPName[i]) then//Aynı Olmayan Yeri Buluyor
                Continue
              else       //basamağı büyükse atanıyor
if (copy(Result, i, 1) > string(tmpPName[i])) and StrISNumeric(string(tmpPName[i])) and
  StrISNumeric(copy(Result, i, 1)) then
              begin
                Result := String(tmpPName);
                break;
              end;
              break;
            end;
          end;
        end;
        tmpLayer.Next;
      end; //While not tmpLayer.Eof do
      tmpLayer.EndBuffering;
    end; //For LayerCount:=i to DrawBox.GIS.Layers.Count -1 do

  //  Result:=Trim(sPrefix)+inttostr(LastNo); //Bulunan son no ön ek'e ekleniyor.

  finally
    Screen.Cursor := sc;
    if Result = '' then
      Result := '1'
    else
    begin
      for I := 0 to Length(Result) - 1 do
        if not StrISNumeric(Copy(Result, Length(Result) - i, 1)) then
          break;
      if ((I = Length(Result)) or (I = 0)) and not StrISNumeric(Result) then
        Result := Result + '1'
      else
        Result := Copy(Result, 1, Length(Result) - i) + inttostr(strtoint(Copy(Result,
          Length(Result) - i + 1, i)) + 1);
    end;
  end;
end;

function GetLastPolygonName(drawBox: TlicgBaseDrawbox; sPrefix: string; layer:
  TlicgBaselayer = nil): string;
begin
  Result := FindLastName(drawBox, sPrefix, layer, idPolygon);
end;

function GetLastPointName(drawBox: TlicgBaseDrawbox; sPrefix: string; layer:
  TlicgBaselayer = nil): string;
begin
  Result := FindLastName(drawBox, sPrefix, layer, idPoint);
  //Uğur Silme Buradan Aşağısı Orjinali
  exit;
   (*
 sc:=  Screen.Cursor;

 Screen.Cursor := crHourGlass;

 try

   PNamePref:= Trim(sPrefix) ; //ön ek
   L:=Length(PNamePref);//ön ek uzunluğu
   LastNo:=0;


   if layer=nil then
     i:=0
   else
   for LayerCount:=0 to DrawBox.GIS.Layers.Count -1 do
   begin
     if (Layer = DrawBox.GIS.Layers[LayerCount]) then
     begin
       i := LayerCount;
       BREAK;
     end;
   end;


   for LayerCount:=i to DrawBox.GIS.Layers.Count -1 do
    begin

        tmpLayer:=DrawBox.GIS.Layers[LayerCount];
        tmpLayer.First ;
        tmpLayer.StartBuffering;
        While not tmpLayer.Eof do
        begin
           if tmpLayer.RecIsDeleted then
           begin
             tmpLayer.Next ;
             Continue;
           end;

           if (tmpLayer.RecEntityID in [idPoint]) then
           begin
              Ent:=tmpLayer.RecLoadEntity ;
              tmpPName:= Ent.Name;
              if L>0 then
                ReadPName:=  Copy(tmpPName,1,L)
              else
                ReadPName:='';
              if UpperCase(ReadPName)= UpperCase(PNamePref)   then
              begin
                try
                  if tmpPName='' then tmpPName:='0';
                  if StrISNumeric(Copy(tmpPName,L+1,Length(tmpPName))) then
                  begin
                     if Copy(tmpPName,L+1,Length(tmpPName))<>'' then
                       if StrtoInt(Copy(tmpPName,L+1,Length(tmpPName))) >LastNo then
                          LastNo:=StrtoInt(Copy(tmpPName,L+1,Length(tmpPName))) ;
                  end;
                Except
                end;
              end;
              Ent := nil;
           end;

           tmpLayer.Next ;
        end;//While
        LastNo:=LastNo+1; //Bulunan son numaraya 1 ekleniyor. Yeni nokta no bu olacak.
        tmpLayer.EndBuffering ;
   end; //For

   Result:=Trim(sPrefix)+inttostr(LastNo); //Bulunan son no ön ek'e ekleniyor.

 finally
    Screen.Cursor := sc;
 end;            *)
end; //GetLastPointName

function _decPointName(var pntName: string): Boolean;
begin
  pntName := DecreasePointName(pntName);
  Result := True;
end;

function GetPointNamesByAperture(p1n, p2n: string; SL: TStrings; isAllLower: Boolean = False): boolean;

  function _incPointName(var pntName: string): Boolean;
  begin
    pntName := IncreasePointName(pntName);
    Result := True;
  end;

var
  _p1n: string; // li2016
  N: integer;
  sayiok: boolean;
  kucuktenbuyuge: boolean;
begin
  Result := false;

  sayiok := StrISNumeric(p1n) and StrISNumeric(p2n);

  if sayiok then
    kucuktenbuyuge := strtoint(p2n) > StrToInt(p1n)
  else
    kucuktenbuyuge := p2n > p1n;

  if kucuktenbuyuge then
  begin

    N := 0;
    _p1n := p1n;
    if isAllLower then
      SL.Add(Lower(_p1n))
    else
      SL.Add((_p1n));

    while _incPointName(_p1n) do
    begin
      inc(N);
      if isAllLower then
        SL.Add(Lower(_p1n))
      else
        SL.Add((_p1n));

      if (_p1n = p2n) or (N > 1000) then
        BREAK;
    end;

    Result := (_p1n = p2n);

  end
  else
  begin
    N := 0;
    _p1n := p1n;
    SL.Add(Lower(_p1n));

    while _decPointName(_p1n) do
    begin
      inc(N);

      if isAllLower then
        SL.Add(lower(_p1n))
      else
        SL.Add((_p1n));

      if (_p1n = p2n) or (N > 1000) then
        BREAK;
    end;

    Result := (_p1n = p2n);

  end;

end;


//Son Nokta ismi bulmak için kullanılır.
//FLastPointName string+sayısal dan oluşabilir. Bu durumda
//her bir byte okunur ve bulunan en son sayısal değerin başlangıç ve
//bitiş pozisyonları arasındaki sayısal değer son nokta numarası olarak
//geri döner.
//Örneğin FLastPointName 'A2B2ZZ23' içersin . Bu durumda FLastPointName'in yeni
// değeri 'A2B2ZZ24' olacaktır.  Eğer değer sadece string ise yada sonu sayısal
//bir değer ile bitmiyorsa Örnek: 123ABC bu durumda dönen değer 123ABC1 olacaktır.
function DecreasePointName(FLastPointName: string): string;
var
  tmpName: AnsiString;
  tmpChar: AnsiChar; // ilker değiştirme 13.07.2016
  lengthStr: Integer;
  I, lastNumPos, firstNumPos: Integer;
begin
  Result := FLastPointName; //ilker ekleme
  tmpName := AnsiString(Trim(FLastPointName));
  lengthStr := Length(tmpName);
  firstNumPos := 0;
  lastNumPos := 0;
  for I := 1 to lengthStr do
  begin
    tmpChar := tmpName[I];
    case ord(tmpChar) of
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57:  //0..9  isNumeric
        begin
          if firstNumPos = 0 then
            firstNumPos := I;
          lastNumPos := I;
        end; //48,49,50,51,52,53,54,55,56,57
    else
      begin
        firstNumPos := 0;
        lastNumPos := 0;
      end; //else
    end; //Case
  end; //For

  if (firstNumPos + lastNumPos) > 0 then
  begin
    FLastPointName := Copy(string(tmpname), 1, firstNumPos - 1);
    if StrToInt64(Copy(string(tmpname), firstNumPos, (lastNumPos - firstNumPos) + 1)) >
      -9e+16 then
      FLastPointName := FLastPointName + InttoStr(StrToInt64(Copy(string(tmpname),
        firstNumPos, (lastNumPos - firstNumPos) + 1)) - 1)
    else
      Application.MessageBox('En büyük değer 9999999999 olabilir.', 'Bilgi',
        MB_OK + MB_ICONINFORMATION);
  end
  else
  begin
    FLastPointName := string(tmpname) + '1';
  end; //if (firstNumPos+lastNumPos)>0

  Result := FLastPointName; //ilker ekleme
end;

function IncreasePointName(FLastPointName: string): string;
var
  tmpName: AnsiString; // ilker değiştirme 13.07.2016
  tmpChar: AnsiChar; // ilker değiştirme 13.07.2016
  lengthStr: Integer;
  I, lastNumPos, firstNumPos: Integer;
  NewName: string;
  NewNumber: UInt64;
begin
  Result := FLastPointName; // ilker ekleme
  tmpName := AnsiString((FLastPointName));
  lengthStr := Length(tmpName);
  firstNumPos := 0;
  lastNumPos := 0;
  for I := 1 to lengthStr do
  begin
    tmpChar := tmpName[I];
    case ord(tmpChar) of
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57:  //0..9  isNumeric
        begin
          if firstNumPos = 0 then
            firstNumPos := I;
          lastNumPos := I;
        end; //48,49,50,51,52,53,54,55,56,57
    else
      begin
        firstNumPos := 0;
        lastNumPos := 0;
      end; //else
    end; //Case
  end; //For

  if (firstNumPos + lastNumPos) > 0 then
  begin
    FLastPointName := Copy(string(tmpname), 1, firstNumPos - 1);
    if StrToFloat(Copy(string(tmpname), firstNumPos, (lastNumPos - firstNumPos) + 1)) < 9e+30 then
    begin
      NewNumber :=(StrToUInt64(Copy(string(tmpname), firstNumPos, (lastNumPos - firstNumPos) + 1)) + 1);
      Str(NewNumber, NewName);
      FLastPointName := FLastPointName + NewName;
    end
    else
      Application.MessageBox('En büyük değer 9999999999 olabilir.', 'Bilgi',
        MB_OK + MB_ICONINFORMATION);

  end
  else
  begin
    FLastPointName := string(tmpname) + '1';
  end; //if (firstNumPos+lastNumPos)>0

  Result := FLastPointName; // ilker ekleme
end;

function StrIsNumeric(tmpStr: string): Boolean;
var
  cC, lengthStr: Integer;
begin
  Result := False;
  tmpStr := Trim(tmpStr);
  lengthStr := Length(tmpStr);
  for cC := 1 to lengthStr do
  begin
    case ord(tmpStr[cC]) of
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57:
        begin
          Result := True;
        end;
    else
      begin
        Result := False;
        Break;
      end;
    end; //case
  end;
end;

function Sub2D(const A, B: TlicgCoor): TlicgCoor; // A-B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function IsEscapePressed: Boolean;
begin
  Result := ((GetAsyncKeyState(VK_ESCAPE) shr 1) <> 0);
end;

function CtrlDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Control] and 128) <> 0);
end;

function ShiftDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function AltDown: Boolean;
var
  State: TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Menu] and 128) <> 0);
end;

function LayerExists(const FileName: string; LayerType: TlicgLayerType): Boolean;
var
  layname: string;
begin
  layname := ChangeFileExt(FileName, '');
  if LayerType = ltMemory then
    Result := FileExists(ChangeFileExt(layname, EXT_LAYERCAD))
  else
    Result := FileExists(ChangeFileExt(layname, EXT_LAYERDRAW)) and FileExists(ChangeFileExt
      (layname, EXT_LAYERINDEX));
end;

function SelectCommonElement(const Subdir, IniFilter: string; ListImageKind:
  TListImageKind): string;
var
  OpenDialog: TOpenDialog;
begin
  Result := '';
  OpenDialog := TOpenDialog.Create(nil);
  try
    if Length(IniFilter) > 0 then
    begin
      //OpenDialog.DefaultExt := DefaultExt;
      OpenDialog.Filter := IniFilter;
    end
    else
    begin
      case ListImageKind of
        liBitmaps:
          OpenDialog.Filter := SBandedImagesFilter;
        liAllImages:
          begin
            OpenDialog.Filter := SBitmapFilter
              + '|' + SJPGFilter
              + '|' + SMetafileFilter + '|' + SICOFilter;
          end;
        liBlocks:
          OpenDialog.Filter := SEDBFilter;
        liOleObject:
          OpenDialog.Filter := SOLEFilter;
      end;
    end;
    //OpenDialog.InitialDir := SubDir;
    OpenDialog.Options := [ofPathMustExist, ofFileMustExist, ofNoChangeDir];
    //repeat
    if not OpenDialog.Execute then
      Exit;
     { if AnsiCompareText(AddSlash(ExtractFilePath(OpenDialog.FileName)), SubDir) = 0 then
        Break;
      MessageToUser(SChangeDirNotAllowed, smsgerror, MB_ICONERROR);
      }
    //until False;
    Result := OpenDialog.FileName;
  finally
    OpenDialog.Free;
  end;
end;

procedure AddToRecentProjects(ASectionName: string; const AFileName: string; AExt: string);
var
  AIniFile: TIniFile;
  I, ANumProjects: integer;
  AProjectName: string;
  AProjectList, AControlProjectList: TStrings;
begin
  AIniFile := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'Settings\RecentProjects.ini');
  AProjectList := TStringList.Create;
  AProjectList.BeginUpdate;

  AControlProjectList := TStringList.Create;
  AControlProjectList.BeginUpdate;
  try
    ANumProjects := AIniFile.ReadInteger(ASectionName, 'NumProjects', 0);
    if ANumProjects > 19  then // En Falza 20 son açılan dosya
      ANumProjects := 19;

    for I := 1 to ANumProjects do
      AProjectList.Add(AIniFile.ReadString(ASectionName, 'RecentProject' + Inttostr(I), ''));

    AProjectList.Insert(0, ChangeFileExt(AFileName, AExt)); // Son Dosyayı Hep Listenin başına ekle
    AProjectList.EndUpdate;
    AIniFile.EraseSection('RecentProjects'); // Ini Temizle
    ANumProjects := 0;
    for I := 0 to AProjectList.Count - 1 do
    begin
      AProjectName := Trim(AProjectList[I]);
      if (Length(AProjectName) > 0) and (AControlProjectList.IndexOf(AProjectName) < 0) then
      begin
        Inc(ANumProjects);
        AIniFile.WriteString(ASectionName, 'RecentProject' + ANumProjects.ToString, AProjectName);
        AControlProjectList.Add(AProjectName);
      end;
    end;
    AIniFile.WriteInteger(ASectionName, 'NumProjects', ANumProjects);
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
    AProjectList.Free;
    AControlProjectList.Free;
  end;
end;

procedure DeleteToRecentProjects(ASectionName: string; const AFileName: string; AExt: string);
var
  AIniFile: TIniFile;
  I, ANumProjects: integer;
  AProjectName: string;
  AProjectList, AControlProjectList: TStrings;
begin
  AIniFile := TIniFile.Create(ExtractFilePath(paramstr(0)) + 'Settings\RecentProjects.ini');
  AProjectList := TStringList.Create;
  AProjectList.BeginUpdate;

  AControlProjectList := TStringList.Create;
  AControlProjectList.BeginUpdate;
  try
    ANumProjects := AIniFile.ReadInteger(ASectionName, 'NumProjects', 0);
    if ANumProjects > 19  then // En Falza 20 son açılan dosya
      ANumProjects := 19;

    for I := 1 to ANumProjects do
      AProjectList.Add(AIniFile.ReadString(ASectionName, 'RecentProject' + Inttostr(I), ''));

    AProjectList.Delete(AProjectList.IndexOf(ChangeFileExt(AFileName, AExt))); // Son Dosyayı sil
    AProjectList.EndUpdate;
    AIniFile.EraseSection('RecentProjects'); // Ini Temizle
    ANumProjects := 0;
    for I := 0 to AProjectList.Count - 1 do
    begin
      AProjectName := Trim(AProjectList[I]);
      if (Length(AProjectName) > 0) and (AControlProjectList.IndexOf(AProjectName) < 0) then
      begin
        Inc(ANumProjects);
        AIniFile.WriteString(ASectionName, 'RecentProject' + ANumProjects.ToString, AProjectName);
        AControlProjectList.Add(AProjectName);
      end;
    end;
    AIniFile.WriteInteger(ASectionName, 'NumProjects', ANumProjects);
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
    AProjectList.Free;
    AControlProjectList.Free;
  end;
end;

function GetRecentProjects(ASectionName: string): TArray<string>;
var
  AIniFile: TInifile;
  I, ANumProjects: Integer;
  AProjectName: string;
  AProjectList: TStrings;
begin
  AProjectList := TStringList.Create;
  AProjectList.BeginUpdate;
  AIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Settings\RecentProjects.ini');
  try
    ANumProjects := AIniFile.ReadInteger(ASectionName, 'NumProjects', 0);
    for I := 1 to ANumProjects do
    begin
      AProjectName := AIniFile.ReadString(ASectionName, 'RecentProject' + Inttostr(I), '');
      AProjectList.Add(AProjectName);
    end;
    AProjectList.EndUpdate;
    Result := AProjectList.ToStringArray;
  finally
    AIniFile.Free;
    AProjectList.Free;
  end;
end;

procedure SetupCursors;
var
  fn: string;
begin
  with Screen do
  begin
    Cursors[crZoomIn] := LoadCursor(HInstance, 'LICG_ZOOMIN');
    Cursors[crZoomOut] := LoadCursor(HInstance, 'LICG_ZOOMOUT');
    //Cursors[crScrollingUp]  := LoadCursor(HInstance, 'LICG_SCROLLING_UP');
    //Cursors[crScrollingDn]  := LoadCursor(HInstance, 'LICG_SCROLLING_DN');
    Cursors[crScrollingDn] := LoadCursor(HInstance, 'LICG_SCROLLING_UP');
    Cursors[crScrollingUp] := LoadCursor(HInstance, 'LICG_SCROLLING_DN');
    Cursors[crRealTimeZoom] := LoadCursor(HInstance, 'LICG_REALTIMEZ');
    Cursors[crHidden] := LoadCursor(HInstance, 'LICG_CURSOR_HIDDEN');
    Cursors[crDrawCross] := LoadCursor(HInstance, 'LICG_DRAW_CROSS');
    Cursors[crRotate] := LoadCursor(HInstance, 'LICG_ROTATE');
    Cursors[crSelectEntity] := LoadCursor(HInstance, 'LICG_SELECT');
    Cursors[crMirror] := LoadCursor(HInstance, 'LICG_MIRROR');

    //fn := ExtractFilePath(paramstr(0)) + 'Data\aero_busy_l.ani';
    {if FileExists(fn) then
    begin
      crBusy := 10;
      Cursors[crBusy] := LoadCursorFromFile(PChar(fn));
    end;}
  end;
end;

procedure DisposeCursors;
begin
  with Screen do
  begin
    Cursors[crZoomIn] := 0;
    Cursors[crZoomOut] := 0;
    Cursors[crScrollingUp] := 0;
    Cursors[crScrollingDn] := 0;
    Cursors[crRealTimeZoom] := 0;
    Cursors[crHidden] := 0;
    Cursors[crDrawCross] := 0;
    Cursors[crRotate] := 0;
    Cursors[crSelectEntity] := 0;
    Cursors[crMirror] := 0;
    if crBusy <> -1 then
      Cursors[crBusy] := 0;
  end;
end;

function DetectCancelPaint(DrawBox: TlicgBaseDrawBox): Boolean;
var
  Msg: TMsg;
begin
  Result := false;
  try
    with DrawBox do
    begin
      //Refresh;
      RefreshExtent(Grapher.CurrentParams.VisualWindow); // ilker bu daha hızlı
      if PeekMessage(Msg, Handle, WM_MOUSEWHEEL, WM_MOUSEWHEEL, PM_NOREMOVE) then //and
      begin
        Result := True;
      end  // is a message waiting on the message pool ?
      else if PeekMessage(Msg, Handle, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE) and
      ((Msg.WParam = VK_ESCAPE) or (Msg.WParam = VK_SUBTRACT) or (Msg.WParam = VK_ADD) or
      (Msg.WParam = VK_UP) or (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_LEFT) or
      (Msg.WParam = VK_RIGHT)) then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_NOREMOVE) then
      //Msg.Message >= WM_LBUTTONDOWN then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_LBUTTONDBLCLK, WM_LBUTTONDBLCLK, PM_NOREMOVE) then
        //Msg.Message >= WM_LBUTTONDBLCLK then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_NOREMOVE) then
        //Msg.Message >= WM_RBUTTONDOWN then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_HSCROLL, WM_VSCROLL, PM_NOREMOVE) and
        (Msg.Message = WM_HSCROLL) or (Msg.Message = WM_VSCROLL) then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_MBUTTONDOWN, WM_MBUTTONDOWN, PM_NOREMOVE) then
      begin
        Result := True;
      end
      else if PeekMessage(Msg, Handle, WM_MBUTTONDBLCLK, WM_MBUTTONDBLCLK, PM_NOREMOVE) then
      begin
        Result := True;
      end;
    end;
  except
  end;
end;

function DetectCancelPaintHandle(AHandle: THandle): Boolean;
var
  Msg: TMsg;
begin
  Result := false;
  try
    if PeekMessage(Msg, AHandle, WM_MOUSEWHEEL, WM_MOUSEWHEEL, PM_NOREMOVE) then //and
    begin
      Result := True;
    end  // is a message waiting on the message pool ?
    else if PeekMessage(Msg, AHandle, WM_KEYDOWN, WM_KEYDOWN, PM_NOREMOVE) and
    ((Msg.WParam = VK_ESCAPE) or (Msg.WParam = VK_SUBTRACT) or (Msg.WParam = VK_ADD) or
    (Msg.WParam = VK_UP) or (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_LEFT) or
    (Msg.WParam = VK_RIGHT)) then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_NOREMOVE) then
    //Msg.Message >= WM_LBUTTONDOWN then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_LBUTTONDBLCLK, WM_LBUTTONDBLCLK, PM_NOREMOVE) then
      //Msg.Message >= WM_LBUTTONDBLCLK then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_RBUTTONDOWN, WM_RBUTTONDOWN, PM_NOREMOVE) then
      //Msg.Message >= WM_RBUTTONDOWN then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_HSCROLL, WM_VSCROLL, PM_NOREMOVE) and
      (Msg.Message = WM_HSCROLL) or (Msg.Message = WM_VSCROLL) then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_MBUTTONDOWN, WM_MBUTTONDOWN, PM_NOREMOVE) then
    begin
      Result := True;
    end
    else if PeekMessage(Msg, AHandle, WM_MBUTTONDBLCLK, WM_MBUTTONDBLCLK, PM_NOREMOVE) then
    begin
      Result := True;
    end;
  except
  end;
end;

function DetectCancelFarPaint: Boolean; stdcall;
begin
  Result := False;
  if CurrCmdLine <> nil then
    Result := DetectCancelPaint(CurrCmdLine.ActiveDrawBox);
end;

{
procedure CopyFilesInDirToAnotherDir(SourceDir, DestDir: string);
var
  i: integer;
  SL: TStrings;
begin
  SL := TStringList.Create;

  try
    BuildFileList(AddSlash(SourceDir) + '*', faAnyFile, SL, False);

    for i := 0 to SL.Count - 1 do
    begin

      FileCopy(AddSlash(SourceDir) + SL[i], AddSlash(DestDir) + ExtractFileName(SL
        [i]), true);
    end;

  except
  end;

  SL.free;
end;    }

function AngleToRad(A: double): double;
begin
  if Licad.Settings.Units.Angle = 'grad' then
    Result := GradToRad(A)
  else if Licad.Settings.Units.Angle = 'deg' then
    Result := DegToRad(A)
  else
    Result := A;
end;

function RadToAngle(A: double): double;
begin
  if Licad.Settings.Units.Angle = 'grad' then
    Result := RadToGrad(A)
  else if Licad.Settings.Units.Angle = 'deg' then
    Result := RadToDeg(A)
  else
    Result := A;
end;

procedure PopulateFieldList(DBTable: TlicgBaseTable; FieldList: TStrings);
var
  I: Integer;
  s: string;
begin
  try
    for i := 1 to DBTable.FieldCount do
    begin

      FieldList.Add(Format('%s;%s;%d;%d', [DBTable.Field(I), DBTable.FieldType(I),
        DBTable.FieldLen(I), DBTable.FieldDec(I)]));

    end;
  except
  end;
end;

function GetActiveRecordCount(L: TlicgBaseLayer): Integer;
begin
  Result := 0;
  L.First;
  L.StartBuffering;
  while not L.Eof do
  begin
    if not L.RecIsDeleted then
      inc(Result);
    L.Next;
  end;
  L.EndBuffering;
end;

procedure VectorDrawCross(ADrawBox: TlicgBaseDrawBox; AVector: IlicgVector); // ilker ekleme
var
  I, ACount: Integer;
begin
  with ADrawBox do
  begin
    { "mark" the clicked point }
    ACount := AVector.Count - 1;
    if ACount > 1 then
      if AVector[0] = AVector[ACount] then // Kapalı Alan ise
        ACount := ACount - 1;

    for I := 0 to ACount do
      DrawCross(GetIlicgCanvas(Canvas), Grapher.RealToPoint(AVector[I]));
  end;
end;

function SelectionKontrol(FilterValue: string): string;
var
  Selection : TlicgBaseSelection;
  Ex : TlicgExtent;
  Fent : IlicgEntity;
  KontrolList : TArray<String>;
  I, J, K, sayi : Integer;
  Bulundu, FNot : Boolean;
  Fstr : string;
begin
  try
    KontrolList := FilterValue.Split([',']);
    Selection := CurrCmdLine.ActiveDrawBox.Selection;
    Ex := CurrCmdLine.ActiveDrawBox.Selection.GetExtension;
    I := 0;
    if Length(KontrolList) > 1 then
    begin
      while I < Selection.Count do
  //    for I := 0 to Selection.Count-1 do
      begin
        j := 0;
        while j < Selection[I].SelList.Count do
        begin
          Fent := Selection[I].Layer.LoadEntityWithRecNo(Selection[I].SelList[J]);
          bulundu := False;
          for K := 1 to Length(KontrolList)-1 do
          begin
            FNot := False;
            if AnsiContainsText(KontrolList[K], '#') then
            begin
              sayi := StrToInt(KontrolList[K].Split(['#'])[0]);
              Fstr := KontrolList[K].Split(['#'])[1];
              if AnsiPos(Fstr, Fent.Name) = sayi then
              begin
                bulundu := True;
                break;
              end;
            end
            else
            begin
              if KontrolList[K][1] = '-' then
              begin
                FNot := true;
                KontrolList[K] := Copy(KontrolList[K], 2, Length(KontrolList[K])-1);
              end;
              if AnsiPos(KontrolList[K], Fent.Name)-1 + Length(KontrolList[k]) = Length(Fent.Name) then
              begin
                bulundu := True;
                if FNot then
                  KontrolList[K] := '-'+ KontrolList[K];
                break;
              end;
            end;
            if FNot then
              KontrolList[K] := '-'+ KontrolList[K];
          end;
          Fent := nil;
          if FNot then
          begin
            if Bulundu then
            begin
              selection.Delete(Selection[I].Layer,Selection[I].SelList[j]);
              if Selection[I] = nil then
                break;
            end
            else
              inc(j);
          end
          else
          begin
            if not Bulundu then
            begin
              selection.Delete(Selection[I].Layer,Selection[I].SelList[j]);
              if Selection[I] = nil then
                break;
            end
            else
              inc(j);
          end;
        end;
        if Selection[I] <> nil then
          inc(I);
      end;
    end;
  finally
    CurrCmdLine.ActiveDrawBox.RepaintExtent(Ex);
    Fent := nil;
    Selection := nil;
  end;
end;

function MakeExpression2(FilterValue: string ;var Kontrol : string ;AField: string = 'UPPER(TEXT(ENT))'): string;
var
  I, J, IsaretValue, DegisgenI, StringLen : integer;
  Filterlist, Parca : TArray<String>;
  son : Boolean;
  IslemString, AralikBaslangic, Aralikson, IslemLikeString, IsaretDegeri, DegisgenS : string;
  function Yildiz(s: string): String;
  begin
    Result := Replace(S, '*', '%');
  end;
  function ArtiBoolean(s: string): Boolean;
  begin
    Result := False;
    if AnsiContainsText(S, '+') then
      Result := True;
    if AnsiContainsText(S, '&') then
      Result := True;
  end;

  function IsNumeric(s: string): Boolean;
  var
    Sayi: Double;
    hata: Integer;
  begin
    Val(S,Sayi,hata);
    Result := hata = 0;
  end;

  function sayiyibul(girdeger: string): Integer;
  var
    yazibas: string;
    I, karaktersayisi: Integer;
  begin
    karaktersayisi:=length(girdeger);
    for I := 1 to karaktersayisi do
    begin
      if isnumeric(copy(girdeger,i,1)) then
      begin
        result:=strtoint(copy(girdeger,i,karaktersayisi-i+1));
        if isnumeric(copy(girdeger,i,karaktersayisi-i+1)) then break;
      end;
    end;
  end;

  function AralikDeger(baslama, bitis: string): String;
  var
     yazibas : string;
    sybas, sybitis, Z  : integer;
    begin
    if not (isnumeric(baslama) and isnumeric(bitis)) then
    begin
      sybas := sayiyibul(baslama);
      sybitis := sayiyibul(bitis);
      yazibas := copy(baslama,1,(pos(sybas.ToString,baslama))-1);
      for z := sybas+1 to sybitis-1 do
      begin
        baslama:=baslama+'"'+'*+'+'"'+yazibas+inttostr(z);
      end;
      Result := '"'+baslama+'"*+"'+bitis+'*"';
    end;
    if Isnumeric(baslama) and Isnumeric(bitis) then
    begin
      for z := baslama.ToInteger+1 to bitis.ToInteger-1 do
      begin
        baslama:=baslama+'*+'+inttostr(z);
      end;
      Result := baslama+'*+'+bitis+'*';
    end;
  end;

  function Tire(Deger : string): String;
  begin
    if Deger[1] = '-' then
    begin
      if Deger[length(Deger)] = '*' then
      begin
        Deger :=copy(Deger,2, length(Deger)-1);
        Result := AField + ' NOT LIKE ' + '"' + Deger+'%"';
      end
      else if Deger[2] = '*' then
      begin
        deger := Replace(Deger,'*','%');
        Result := AField + ' NOT LIKE ' + '"' + copy(Deger, 2, length(Deger)-1)+'"' ;
        Kontrol := Kontrol+ ',-'+ copy(Deger, 3, length(Deger)-1);
      end;
    end
    else
      Result := '';
  end;
  function AndOr(S, kapi : string): String;
  begin
    if kapi = '+' then
      Result := s + ' or '
    else if kapi = '&' then
      Result := s + ' and '
    else if kapi = '%' then
      Result := s + ' and '
    else
      result := s;
  end;
begin
  try

    FilterValue := FilterValue.Trim;
    FilterValue := Replace(FilterValue, ' ', '');
    Filterlist := FilterValue.Split(['..']);
    if Length(Filterlist) > 1 then
    begin
      son := True;
      for I := 0 to Length(Filterlist)-1 do
      begin
        if ArtiBoolean(Filterlist[I]) then
        begin
          Parca := Filterlist[I].Split(['+','&']);
          if son then
          begin
            IslemString := Copy(Filterlist[I],1 , length(Filterlist[I]) - length(parca[length(parca)-1]));
            son := False;
            AralikBaslangic := Parca[Length(Parca)-1];
          end
          else
          begin
            IslemString := Copy(Filterlist[I], length(parca[0])+1, length(Filterlist[I]) - length(parca[0]));
            son := True;
            Aralikson := Parca[0];
            IslemString := AralikDeger(AralikBaslangic, Aralikson) + IslemString;
          end;
        end
        else
        begin
          if son then
          begin
            son := False;
            AralikBaslangic := Filterlist[I];
          end
          else
          begin
            son := True;
            Aralikson := Filterlist[I];
          end;
          if (((I+1) mod 2) = 0) and (I <> 0)  then
            IslemString := IslemString + AralikDeger(AralikBaslangic, Aralikson);
        end;

      end;
      FilterValue := IslemString;
      IslemString := '';
      Filterlist := nil;
    end;
    IsaretValue := 0;
    Filterlist := FilterValue.Split(['+','&','%']);
    I := 0;
  //  for I := 0 to Length(Filterlist)-1 do
    while I < Length(Filterlist) do
    begin
      IsaretValue := IsaretValue + Length(Filterlist[I])+1;
      if '+' = FilterValue[IsaretValue] then
        IsaretDegeri := ' or '
      else if '%' = FilterValue[IsaretValue] then
        IsaretDegeri := '%'
      else
        IsaretDegeri := ' and ';

      if IsaretDegeri = '%' then
      begin
        if Filterlist[I][1] = '*' then
        begin
          Filterlist[I] := replace(Filterlist[I], '*', '%');
          IslemString := AndOr(AField + ' NOT LIKE "' + Filterlist[I] +'"', FilterValue[IsaretValue]);
          Kontrol := Kontrol+ ',-'+ copy(Filterlist[I], 2, length(Filterlist[I])-1);
        end
        else if Filterlist[I][Length(Filterlist[I])] = '*' then
        begin
          Filterlist[I] := replace(Filterlist[I],'*','%');
          IslemString := AndOr(AField + ' NOT LIKE "' + Filterlist[I] +'"', FilterValue[IsaretValue]);
        end;
      end
      else if AnsiContainsText(Filterlist[I], '-') then
      begin
        IslemString := Tire(Filterlist[I]);
      end
      else if AnsiContainsText(Filterlist[I], '>') then
      begin
        if Filterlist[I][1] = '>' then
        begin
          Filterlist[I]:=copy(Filterlist[I],2, Length(Filterlist[I]));
          if isnumeric(Filterlist[I]) then
          begin
            IslemString := AndOr(AField + '>'+Filterlist[I], FilterValue[IsaretValue]) ;
          end;
        end
        else if Filterlist[I][Length(Filterlist[I])] = '>' then
        begin
          Filterlist[I]:=copy(Filterlist[I],1, Length(Filterlist[I])-1);
          if isnumeric(Filterlist[I]) then
          begin
            IslemString := AndOr(AField + '<'+Filterlist[I], FilterValue[IsaretValue]);
          end;
        end
      end
      else if AnsiContainsText(Filterlist[I], '<') then
      begin
        if Filterlist[I][1] = '<' then
        begin
          Filterlist[I] := copy(Filterlist[I],2, Length(Filterlist[I]));
          if isnumeric(Filterlist[I]) then
          begin
            IslemString := AndOr(AField + '<'+Filterlist[I], FilterValue[IsaretValue]) ;
          end;
        end
        else if Filterlist[I][Length(Filterlist[I])] = '<' then
        begin
          Filterlist[I] := copy(Filterlist[I],1, Length(Filterlist[I])-1);
          if isnumeric(Filterlist[I]) then
          begin
            IslemString := AndOr(AField + '>'+Filterlist[I], FilterValue[IsaretValue]) ;
          end;
        end
      end
      else if AnsiContainsText(Filterlist[I], '#') then
      begin
        DegisgenI := StrToInt(copy(Filterlist[I], 2, AnsiPos( ',', Filterlist[I])-2));
        Filterlist[I] := copy(Filterlist[I], AnsiPos(',', Filterlist[I])+1, Length(Filterlist[I]));
        IslemString := AField + ' LIKE '+'"%' + Filterlist[I] + '%"';
        Kontrol := Kontrol+ ','+ IntToStr(DegisgenI)+ '#' + Filterlist[I];
      end
      else if AnsiContainsText(Filterlist[I], '~') then
      begin
        StringLen := Length(Filterlist[I]) - Length(Replace(Filterlist[I], '~', ''));
        if (Filterlist[I][1] = '~') and (StringLen = 1) then
        begin
          Filterlist[I] := copy(Filterlist[I], 2, Length(Filterlist[I]));
          IslemString := AndOr(AField + ' LIKE "%' + Filterlist[I] +'%"', FilterValue[IsaretValue]);
          Kontrol := Kontrol+ ','+ Filterlist[I];
        end
        else if (Filterlist[I][Length(Filterlist[I])] = '~') and (StringLen = 1) then
        begin
          Filterlist[I] := copy(Filterlist[I],1, Length(Filterlist[I])-1);
          IslemString := AndOr(AField + ' LIKE "' + Filterlist[I] +'%"', FilterValue[IsaretValue]);
        end
        else if (Filterlist[I][1] <> '~') and (Filterlist[I][Length(Filterlist[I])] <> '~') and (StringLen = 1) then
        begin
          Kontrol := Kontrol+ ','+ Copy(Filterlist[I], AnsiPos('~',Filterlist[I])+1, Length(Filterlist[I]) - AnsiPos('~',Filterlist[I]));
          Filterlist[I] := copy(Filterlist[I], 1, AnsiPos('~',Filterlist[I])-1);
          IslemString := AndOr(AField + ' LIKE "' + Filterlist[I] +'%"', FilterValue[IsaretValue]);
        end;
      end
      else if AnsiContainsText(Filterlist[I], '?') then
      begin
        Filterlist[I] := replacec(Filterlist[I],'?','_');
        StringLen := Length(Filterlist[I]) - Length(Replace(Filterlist[I], '_', ''));
        if (StringLen = 1) and (Filterlist[I][1] = '_') then
        begin
          IslemString := AndOr(AField + ' LIKE "' + Filterlist[I] +'%"', FilterValue[IsaretValue]);
          Kontrol := Kontrol+ ','+ Copy(Filterlist[I], 2, length(Filterlist[I])-1);
        end
        else if (StringLen = 1) and (AnsiPos('_',Filterlist[I]) <> 1) and (AnsiPos('_',Filterlist[I]) <> Length(Filterlist[I]) ) then  //çalışmıyor
        begin
          StringLen := Length(Filterlist[I]) - AnsiPos('_',Filterlist[I]);
          IslemString := AndOr(AField + ' LIKE "' + Copy(Filterlist[I], 0, AnsiPos('_',Filterlist[I]))+ DupeString('_',StringLen)+'"', FilterValue[IsaretValue]);
        end
        else if (StringLen = 1) and (Filterlist[I][length(Filterlist[I])] = '_') then
          IslemString := AndOr(AField + ' LIKE "' + Filterlist[I] +'"', FilterValue[IsaretValue]);
      end
      else if AnsiContainsText(Filterlist[I], '*') then
      begin
        Filterlist[I] := Yildiz(Filterlist[I]);
        IslemLikeString := AndOr((AField + ' LIKE "'+Filterlist[I]+'"'),FilterValue[IsaretValue]);
        StringLen := Length(Filterlist[I]) - Length(Replace(Filterlist[I], '%', ''));
        if (Filterlist[I][1] = '%') and (StringLen = 1) and (Length(Filterlist[I]) <> 1) then
        begin
          IslemLikeString := AndOr((AField + ' LIKE "'+Filterlist[I]+'%"'),FilterValue[IsaretValue]);
          Kontrol := Kontrol+ ','+ Replace(Filterlist[I], '%', '');
        end
        else if (Filterlist[I][1] <> '%') and ((Filterlist[I][Length(Filterlist[I])] <> '%')) and (StringLen = 1) then
        begin
          IslemLikeString := AndOr((AField + ' LIKE "'+copy(Filterlist[I],0,AnsiPos('%',Filterlist[I]))+'"'),FilterValue[IsaretValue]);
          Kontrol := Kontrol+ ','+ copy(Filterlist[I], AnsiPos('%',Filterlist[I])+1, Length(Filterlist[I])- AnsiPos('%',Filterlist[I]));
        end
      end
      else
      begin
        if IslemString = '' then
          IslemString := AndOr((AField + ' = "'+Filterlist[I]+'"'),FilterValue[IsaretValue]);
      end;
      if IslemLikeString <> '' then
        Result := Result + IslemLikeString
      else
        Result := Result + IslemString;
      IslemString := '';
      IslemLikeString := '';
      inc(I);
    end;
  except on E: Exception do
      Application.MessageBox(PChar('Seçilen Filitrede Hata Oluştu.'), PChar(Application.Title), + MB_OK + MB_ICONWARNING);
  end;
end;

function MakeExpression(AField: string; AValue: string): string;

  function IsNumeric(s: string): Boolean;
  var
    Sayi: Double;
    hata: Integer;
  begin
    Val(S,Sayi,hata);
    Result := hata = 0;
  end;

  function sayiyibul(girdeger: string): Integer;
  var
    yazibas: string;
    I, karaktersayisi: Integer;
  begin
    karaktersayisi:=length(girdeger);
    for I := 1 to karaktersayisi do
    begin
      if isnumeric(copy(girdeger,i,1)) then
      begin
        result:=strtoint(copy(girdeger,i,karaktersayisi-i+1));
        if isnumeric(copy(girdeger,i,karaktersayisi-i+1)) then break;
      end;
    end;
  end;

var
  girdi,Sonuc,baslama,bitis,bas2,bitis2,sonucaa: string;
  buldum, buldum2,karaktersayisi,buldumnn,buldumtire : Integer;
  buldumsoru,bulbuyuk,bulkucuk,bularti,bulicinde: Integer;
  bulve,bulyuzde,bulsonsuz,bulyilnokta,bulvirgul,listno: Integer;
  bulkare,bulparac,bulparkapat: Integer;
  z,i,ii,k,m: Integer;
  basint,bitint,sybas,sybitis: Integer;
  yazibas,yazibitis,baslamas,yyazison,kacrakam: string;
  bulartii,ardisilsayi,bulbaslamayildiz,bulbitisyildiz: Integer;
  baslamaa,bitiss,adim,altcizgi,parantezici,bitisvedensonra,ysonsuz: string;
  bas,son : Array[0..20] of string;
begin
  Result := '';
  if Trim(AValue) = '' then
    Exit;

  //AField := 'UPPER(TEXT(ENT))';
  //AField := 'POINTCODE(ENT)';

  i := 0;
  girdi:= Trim(AValue);
  karaktersayisi:=Length(girdi);

  buldum:=pos('*',girdi);
  buldumnn:=pos('..',girdi);
  buldumtire:=pos('-',girdi);
  buldumsoru:=pos('?',girdi);
  bulbuyuk:=pos('>',girdi);
  bulkucuk:=pos('<',girdi);
  bularti:=pos('+',girdi);
  bulicinde:=pos('~',girdi);   //alt+126 =~/b  /// ~ işareti * ile aynı görevi görüyor
  bulve:=pos('&',girdi);
  bulyuzde:=pos('%',girdi);
  bulyilnokta:=pos('*..',girdi);
  bulvirgul:=pos(',',girdi);
  bulkare:=pos('#',girdi);
  bulparac:=pos('(',girdi);//parantez aç
  bulparkapat:=pos(')',girdi);//parantez kapat
 // bulsonsuz:=pos('~',girdi);// ~     ///bulicinde:=pos('~',girdi);   //alt+126 =~/b  /// ~ işareti * ile aynı görevi görüyor

  baslama:=copy(girdi,1,buldum-1);//* karakterinden öncekiler alındı
  bitis:=copy(girdi,buldum+1, karaktersayisi-1);//* karaktereinden sonrakiler alındı
  buldum2:= pos('*',bitis);
  bas2:=copy(bitis,1,buldum2-1);
  bitis2:=copy(bitis,buldum2+1,(length(bitis))-1);
  // showmessage(inttostr(buldum2));
  //Memo1.text:=(inttostr(buldum))+'---'+baslama+'---'+bitis;



  if (buldum=1) AND (bulyilnokta<1)  then   //*ddd --->%ddd yapıldı ....~/b--->%/b yapıldı
  begin
    Sonuc := AField + ' LIKE ' + '"%'+bitis+'"' ; //*ddd --->%ddd yapıldı ....~/b--->%/b yapıldı
  end;


  if (buldum>1) AND (bulyilnokta<1) then
    Sonuc := AField + ' LIKE ' + '"' + baslama + '%"'; //ddd*-->ddd% yapıldı


  if(buldum=1) and (buldum2>1) then
    Sonuc := AField + ' LIKE ' + '"%' + bas2+'%'+bitis2+'"'; //ddd*-->ddd% yapıldı





  if (buldumnn>1) AND (bulyilnokta<1) and (bulvirgul<1) then    //5..25 ---> yapılıyor
  begin
    baslama:=copy(girdi,1,buldumnn-1);
    bitis:=copy(girdi,buldumnn+2,karaktersayisi-1);


    if not (isnumeric(baslama) and isnumeric(bitis)) then //girilen değer alfa nümerik olunca...A1..A101 gibi
    begin
      sybas:=sayiyibul(baslama);
      sybitis:=sayiyibul(bitis);
      yazibas:=copy(baslama,1,(pos(sybas.ToString,baslama))-1);
      //yazibas:=DELETE(baslama,(pos(sybas.ToString,baslama))-1,1);

      for II := sybas+1 to sybitis-1 do
      begin
        baslama:=baslama+'"'+','+'"'+yazibas+inttostr(ii);
      end;
      Sonuc := AField + ' IN ('+'"'+baslama+'","'+bitis+'"'+')';//A1..A5 -->in (A1,A2,A3,A4,A5) yapıldı
    end;

    if Isnumeric(baslama) and Isnumeric(bitis) then//girilen değer numerik olunca
    begin
      for z := baslama.ToInteger+1 to bitis.ToInteger-1 do
      begin
        baslama:=baslama+','+inttostr(z);
      end;
      Sonuc := AField + ' IN (' +  baslama+','+bitis+')';//1..5 -->in (1,2,3,4,5) yapıldı
    end;
    // Memo1.Text := inttostr(sayiyibul(bitis));
  end;



  if (buldumtire=1) and (bulparac<1) then  //başta tire VE sonda * olunca  : -A* gibi   : A ile başlamayanlar
  begin
    bitis:=copy(girdi,buldumtire+1, karaktersayisi-1);//- karaktereinden sonrakiler alındı
    buldum2:= pos('*',bitis);
    bas2:=copy(bitis,1,buldum2-1);
    Sonuc := AField + ' NOT LIKE ' + '"' + bas2+'%"' ; //-A*--->NOT LIKE A% YAPILDI
  end;



  if buldumsoru=1 then //  ? : herhangi bir karakter = _ olacak // ist? --->ist_ olaca
  begin
    bitis:=copy(girdi,buldumsoru+1, karaktersayisi-1);// ? karaktereinden sonrakiler alındı
    Sonuc := AField + ' LIKE ' + '"_' + bitis+'"' ; //?5555--->_5555 yapıldı
  end ;



  if buldumsoru>1 then //  ? : herhangi bir karakter = _ olacak // ist? --->ist_ olaca
  begin
    baslama:=copy(girdi,1,buldumsoru-1);// ? karakterinden öncekiler alındı
    Sonuc := AField + ' LIKE ' + '"' + baslama+'_"' ; //5555?---->5555_ yapıldı
  end;



  if (bulbuyuk=1) and (bulparac<1) then
  begin
    bitis:=copy(girdi,bulbuyuk+1, karaktersayisi-1);// > karakterinden sonrakiler alındı
    if isnumeric(bitis) then
    begin
      Sonuc := AField + '>'+bitis ; //
    end;
     //Sonuc := AField + '>' + '"' + bitis+'"' ; //
  end;



  if (bulkucuk=1) and (bulparac<1) then
  begin
    bitis:=copy(girdi,bulkucuk+1, karaktersayisi-1);// > karaktereinden sonrakiler alındı
    Sonuc := AField + '<' + '"' + bitis+'"' ; //
  end;





  if (bularti>1) and (bulyilnokta<1) and (bulparac<1) then // 101/1+102/2+155+25+19 --->101/1 ve 102/2 içerenleri seçiyor..TAMAMDIR
  begin

    baslama:=copy(girdi,1,bularti-1);// + karakterinden öncekiler alındı
    bitis:=copy(girdi,bularti+1, karaktersayisi-1);// + karakterinden sonrakiler alındı
    //yyazison:=son[0];

    if pos('+',bitis)<1 then
    begin
      sonuc := AField + '= "' + baslama+'" OR '+ AField + '= "' +bitis+'"'; //100 + 105 ---->100 or 105 yapıldı
    end
    else
    begin
      sonuc := AField + '= "' + baslama+'"'; //100 + 105 ---->100 or 105 yapıldı
    end;

    if pos('+',bitis)>=1 then
      for m := 1 to length(bitis) do
      begin
        if pos('+',bitis)>=1 then
        begin
          bulartii:=pos('+',bitis);
          baslamaa:= copy(bitis,1,bulartii-1);
          bitiss:=copy(bitis,bulartii+1, length(bitis)-1);
          sonuc:=sonuc+ ' OR ' + AField + '= "' +baslamaa+'"';
          if pos('+',bitiss)<1 then
          begin
            sonuc:=sonuc+ ' OR ' + AField + '= "' +bitiss+'"';
            break;
          end;
          //m:=bulartii;
          bitis:=bitiss;
          baslamaa:='';
        end;

      end;
  end;





  if bulicinde>=1 then    // ~555--->%555 yapıldı
  begin
    baslama:=copy(girdi,1,bulicinde-1);// + karakterinden öncekiler alındı
    bitis:=copy(girdi,bulicinde+1, karaktersayisi-1);// + karaktereinden sonrakiler alındı

    if bulicinde=1 then
    begin
      Sonuc := AField + ' LIKE "%' +  bitis+'"' ; // ~555--->%555 yapıldı
    end else
    begin
      Sonuc := AField + ' LIKE "' +  baslama+'%"' ; // 555~--->555% yapıldı
    end;

  end;




  if bulve>1 then  //A*&2*--->A%2 yapıldı --->kontrol edemedim....
  begin
    baslama:=copy(girdi,1,bulve-1);// & karakterinden öncekiler alındı
    bitis:=copy(girdi,bulve+1, karaktersayisi-1);// & karaktereinden sonrakiler alındı

    if (copy(baslama,length(baslama),1)='*') and (copy(bitis,1,1)='*')  then
    begin
      baslama:=copy(baslama,1,length(baslama)-1);
      bitis:=copy(bitis,2,length(bitis));
      sonuc:= AField + ' LIKE ' + '"'+baslama+'%' + Bitis+'"';//'buldum****'+baslama+'///'+inttostr(bulve);
    end;
  end;




  if bulyuzde>1 then  //A*%*2--->başı ve sonu "not like a%2" olmayanlar seçiliyor.hepsini seçti.testi yapılamadı
  begin
    baslama:=copy(girdi,1,bulyuzde-1);// % karakterinden öncekiler alındı
    bitis:=copy(girdi,bulyuzde+1, karaktersayisi-1);// % karaktereinden sonrakiler alındı

    if  (copy(baslama,length(baslama),1)='*') and (copy(bitis,1,1)='*')  then
    begin
      baslama:=copy(baslama,1,length(baslama)-1);
      bitis:=copy(bitis,2,length(bitis));
      sonuc:=AField + ' NOT LIKE ' + '"'+baslama+'%'+Bitis+'"';//'buldum****'+baslama+'///'+inttostr(bulve);
    end;
  end;




  if bulyilnokta>1 then  //101*..102* veya A1*..A5*-->101 ve 102 ile başlayanlar seçiliyor....TAMAMDIR.
  begin
    baslama:=copy(girdi,1,bulyilnokta-1);
    bitis:=copy(girdi,bulyilnokta+2,karaktersayisi-bulyilnokta-1);
    sonuc:= AField + ' LIKE ' + '"'+baslama+'%" OR ' + AField + ' LIKE "'+bitis+'%"';// bitiş e * geliyor....SORRRR
  end;





  if (buldumnn>1) AND  (bulvirgul>1) then    //1..10,2 --->1 den 10 kadar 2 şer atlayarak seç...yapıyorum
  begin
    baslama:=copy(girdi,1,buldumnn-1);
    bitis:=copy(girdi,buldumnn+2,bulvirgul-buldumnn-2);
    adim:=copy(girdi,bulvirgul+1,karaktersayisi-bulvirgul);

    if not (isnumeric(baslama) and isnumeric(bitis)) then //girilen değer alfa nümerik olunca...A1..A101 gibi
    begin
      sybas:=sayiyibul(baslama);
      sybitis:=sayiyibul(bitis);
      yazibas:=copy(baslama,1,(pos(sybas.ToString,baslama))-1);
      //yazibas:=DELETE(baslama,(pos(sybas.ToString,baslama))-1,1);
      ii := sybas+adim.ToInteger;
      while ii <= sybitis do // for ii := sybas to sybitis do
      begin
        baslama:=baslama+'"'+','+'"'+yazibas+inttostr(ii);
        ii:=ii+adim.ToInteger;
      end;

      if adim.ToInteger<0 then    //adim - (eksi) ise....geriye doğru sayıyorsak 27.08.2018 eklendi.
      while ii >= sybitis do // for ii := sybas to sybitis do
      begin
        baslama:=baslama+'"'+','+'"'+yazibas+inttostr(ii);
        ii:=ii+adim.ToInteger;
      end; //27.08.2018 eklendi.


      Sonuc := AField + ' IN ('+'"'+baslama+'"'+')';//A1..A5,2 -->in (A1,A3,A5) yapıldı
    end;

    if Isnumeric(baslama) and Isnumeric(bitis) then//girilen değer numerik olunca
    begin
      z:=baslama.Tointeger+adim.ToInteger;
      while z<=bitis.ToInteger do // for z := baslama.ToInteger+1 to bitis.ToInteger-1 do
      begin
        baslama:=baslama+','+inttostr(z);
        z:=z+adim.ToInteger;
      end;

      if adim.ToInteger<0  then //adim - (eksi) ise  .... geriye doğru sayıyorsak 27.08.2018 eklendi
      while z>=bitis.ToInteger do
      begin
         baslama:=baslama+','+inttostr(z);
         z:=z+adim.ToInteger;
      end; // 27.08.2018 eklendi

      Sonuc := AField + ' IN (' +baslama+')';//1..5,2 -->in (1,3,5) yapıldı
    end;
    // Memo1.Text := inttostr(sayiyibul(bitis));
  end;






  if (bulkare=1) and (bulvirgul>1) then //#3,569--->___569...3. rakamından itibaren 569 olanlar..testi yapılamadı
  begin
    kacrakam:=copy(girdi,bulkare+1,bulvirgul-2);//#3 den 3 sayısı alındı
    bitis:=copy(girdi,bulvirgul+1,karaktersayisi); //virgülden sonrası alındı
    altcizgi:='_';
    for I := 1 to strtoint(kacrakam)-1 do
    begin
      altcizgi:=altcizgi+'_';
    end;
    Sonuc := AField + ' LIKE '+'"'+altcizgi+bitis+'"';//#2,12--->__12 yapıldı
  end;






  if (bulparac>=1) and (bulparkapat>1) and (bulve>1) AND (bularti>1) and (bulicinde>1) then    //(ATA*+*K) & ~OKULU  ----aç-kapa parantez var ise--->
  begin
    parantezici:=copy(girdi,bulparac+1,bulparkapat-2);
    bitisvedensonra:=copy(girdi,bulve+1, karaktersayisi-1);// & karaktereinden sonrakiler alındı
    if bularti>=1 then   //  + var ise
    begin
      baslama:=copy(parantezici,1,bularti-2);
      bulbaslamayildiz:=pos('*',baslama);
      bitis:=copy(parantezici,bulbaslamayildiz+2,length(parantezici));
      bulbitisyildiz:=pos('*',bitis);
      if bulbaslamayildiz=1 then
      begin
        baslama:='%'+copy(baslama,2,length(baslama)-1);//*ata--->%ata
      end
      else
      begin
        baslama:=copy(baslama,1,length(baslama)-1)+'%';//Ata*---->ata%
      end;

      if bulbitisyildiz=1 then
      begin
        bitis:='%'+copy(bitis,2,length(bitis));//*ata--->%ata
      end
      else
      begin
        bitis:=copy(bitis,1,length(bitis)-1)+'%';//Ata*---->ata%
      end;
    end;
    if bulicinde>=1 then
    begin
      ysonsuz:=copy(girdi,bulicinde+1,karaktersayisi-bulicinde);
    end;

    // sonuc:=parantezici+'////'+baslama+'//////'+bitis+'//'+bitisvedensonra+'///'+ysonsuz;
    sonuc:= AField + ' LIKE '+'"'+baslama+ '" OR ' + AField + ' LIKE '+'"'+bitis+'"'+' AND ' + AField + ' LIKE '+'"%'+ysonsuz+'%"';
  end;


  if (bulparac>1) and (bulparkapat>1) AND (buldumtire=1) and (bularti>1) then    //-(100/1+100/8)--->Adı 100/1 ve 100/8 olan objelerin dışında kalanlar
  begin
    parantezici:=copy(girdi,bulparac+1,bulparkapat-3);
    baslama:=copy(girdi,bulparac+1,bularti-3);
    bitis:=copy(girdi,bularti+1,karaktersayisi-1-bularti);
    sonuc:= AField + ' NOT LIKE '+'"'+baslama+'" AND ' + AField + ' NOT LIKE "'+bitis+'"';
  end;



  if  (bulparac=1) and (bulparkapat>1) and (bularti>1) and (bulicinde<1) then    //(100/1+100/8)--->Adı 100/1 ve 100/8 olan objeler
  begin
    parantezici:=copy(girdi,bulparac+1,bulparkapat-2);
    baslama:=copy(girdi,bulparac+1,bularti-2);
    bitis:=copy(girdi,bularti+1,karaktersayisi-1-bularti);
    sonuc:= AField + ' LIKE '+'"'+baslama+'" OR ' + AField + ' LIKE "'+bitis+'"';
  end;

  if Sonuc = '' then // ilker ekleme Joker karakter yok ise eşit olanı seçmesi lazım idi. Boş döndürüyordu.
    Sonuc := AField + ' = ' + '"' + AValue + '"'; //

  Result := Sonuc;
end;

function CompareNatural(aList: TStringlist; index1, index2: Integer): Integer; // ilker ekleme
var
  S1, S2: string;
begin
  S1:= aList[index1];
  S2:= aList[index2];
  // Assuming string = unicodestring here!
  //Result := StrCmpLogicalW(PChar(S1), PChar(S2));
end;

function NaturalSortCompare(const S1, S2: string): Integer; // ilker ekleme
const
  RE_CHUNK: string = '([\D]+|[\d]+)';
  RE_LETTERS: string = '\D+';
  RE_NUMBERS: string = '\d+';
var
  L, R: string;
  LChunk, RChunk: string;
  ReChunk, ReLetter, ReNumber: TRegEx;

  /// <summary>
  /// 文字列の先頭から、連続する数字または文字列を取得する
  /// </summary>
  function GetChunk(var S: string): string;
  var
    Match: TMatch;
  begin
    Match := ReChunk.Match(S);
    if Match.Success then
    begin
      Result := Match.Groups[0].Value;
      S := S.Substring(Result.Length);
    end
    else
    begin
      Result := '';
    end;
  end;

/// <summary>
/// 文字列を数値として比較して、S1が小さいときは-1、S2が小さいときは1、等しいときは0を返す
/// </summary>
  function CompareNum(const S1, S2: string): Integer;
  var
    Num1, Num2: Integer;
  begin
    Num1 := StrToInt(S1);
    Num2 := StrToInt(S2);
    if Num1 < Num2 then
      Result := -1
    else if Num1 > Num2 then
      Result := 1
    else
      Result := 0;
  end;

begin
  //Result := StrCmpLogicalW(PChar(S1), PChar(S2));
  //Exit;

  Result := 0;
  L := S1;
  R := S2;
  ReChunk := TRegEx.Create(RE_CHUNK);
  ReLetter := TRegEx.Create(RE_LETTERS);
  ReNumber := TRegEx.Create(RE_NUMBERS);

  while Result = 0 do
  begin
    if L.IsEmpty and R.IsEmpty then
      Exit(CompareStr(S1, S2));

    LChunk := GetChunk(L);
    RChunk := GetChunk(R);

    if ReLetter.Match(LChunk).Success and ReLetter.Match(RChunk).Success then
    begin
      Result := CompareStr(LChunk, RChunk);
    end
    else
    begin
      if ReNumber.Match(LChunk).Success and ReNumber.Match(RChunk).Success then
      begin
        Result := CompareNum(LChunk, RChunk);
      end
      else
      begin
        Result := CompareStr(LChunk, RChunk);
        if Result = 0 then
          Result := 1;
      end;
    end;
  end;
end;


function StrCmpLogicalW; external 'shlwapi.dll' name 'StrCmpLogicalW';

function StrCmpLogical(const s1, s2: string): Integer;
begin
  Result := StrCmpLogicalW(PChar(s1), PChar(s2));
end;

function CustomNaturalSort(List: TStringList; Index1, Index2: Integer): Integer; // ilker ekleme
begin
  //Result := NaturalSortCompare(List[Index1], List[Index2]);
   Result := StrCmpLogicalW(List[Index1], List[Index2]);
end;

procedure TellWindowsWeArentFrozen;
var
  Msg: TMsg;
begin
  if GetTickCount <> LastPeekMessageTime then
  begin
    PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
    LastPeekMessageTime := GetTickCount;
  end;
end;

function GetDLLName: string; // ilker ekleme
var
  aName: array[0..MAX_PATH] of Char;
begin
  FillChar(aName, SizeOf(aName), #0);
  GetModuleFileName(HInstance, aName, MAX_PATH);
  Result := aName;
end;

function GetAppVersionInfo(sAppNamePath: string):string; // ilker ekleme
var
  VerSize: integer;
  VerBuf: PChar;
  VerBufValue: pointer;
  {$IFDEF Delphi3Below}
  VerHandle: integer;
  VerBufLen: integer;
  {$ELSE}
  VerHandle: cardinal;
  VerBufLen: cardinal;
  {$ENDIF}
  VerKey: string;

  function GetInfo(ThisKey: string): string;
  begin
    Result := '';
    VerKey := '\StringFileInfo\' + IntToHex(loword(integer(VerBufValue^)), 4) +
    IntToHex(hiword(integer(VerBufValue^)), 4) + '\' + ThisKey;
    if VerQueryValue(VerBuf, PWideChar(VerKey), VerBufValue, VerBufLen) then
      Result := StrPas(PChar(VerBufValue));
  end;

  function QueryValue(ThisValue: string): string;
  begin
    Result := '';
    if GetFileVersionInfo(PChar(sAppNamePath), VerHandle, VerSize, VerBuf) and
      VerQueryValue(VerBuf, '\VarFileInfo\Translation', VerBufValue, VerBufLen) then
      Result := GetInfo(ThisValue);
  end;

begin
  if sAppNamePath = '' then
    sAppNamePath := Application.ExeName;
  VerSize := GetFileVersionInfoSize(PChar(sAppNamePath), VerHandle);
  VerBuf := AllocMem(VerSize);
  try
    //FileVersionInfo.fCompanyName      := QueryValue('CompanyName');
    //FileVersionInfo.fFileDescription  := QueryValue('FileDescription');
    //FileVersionInfo.fFileVersion      := QueryValue('FileVersion');
    Result := QueryValue('FileVersion');
    //FileVersionInfo.fInternalName     := QueryValue('InternalName');
    //FileVersionInfo.fLegalCopyRight   := QueryValue('LegalCopyRight');
    //FileVersionInfo.fLegalTradeMark   := QueryValue('LegalTradeMark');
    //FileVersionInfo.fOriginalFileName := QueryValue('OriginalFileName');
    //FileVersionInfo.fProductName      := QueryValue('ProductName');
    //FileVersionInfo.fProductVersion   := QueryValue('ProductVersion');
    //FileVersionInfo.fComments         := QueryValue('Comments');
  finally
    FreeMem(VerBuf, VerSize);
  end;
end;

procedure DeleteAllEntityInLayer(ALayerName: string);
var
  ALayer: TlicgBaseLayer;
begin
  try
    ALayer := CurrCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(ALayerName);
    DeleteAllEntityInLayer(ALayer);
  finally
    if ALayer <> nil then
      ALayer := nil;
  end;
end;

procedure DeleteAllEntityInLayer(ALayer: TlicgBaseLayer);
var
  E: IlicgEntity;
begin
  if ALayer = nil then
    Exit;
  try
    ALayer.First;
    ALayer.StartBuffering;
    while not ALayer.Eof do
    begin
      if ALayer.RecIsDeleted then
      begin
        ALayer.Next;
        Continue;
      end;
      E := ALayer.LoadEntityWithRecNo(ALayer.Recno);
      if (E <> nil) then
      begin
        ALayer.DeleteEntity(ALayer.Recno);
      end;
      ALayer.Next;
      E := nil;
    end;
  finally
    if E <> nil then
      E := nil;
    ALayer.EndBuffering;
  end;
end;

procedure ShrinkMemory;
var
  myProcess : THandle;
  dwMin,
  dwMax : DWord;
begin Exit;
  dwMin := High(DWord); // Yada $FFFFFFFF
  dwMax:= High(DWord); // Yada $FFFFFFFF
  myProcess := GetCurrentProcess;
  SetProcessWorkingSetSize(myProcess, dwMin, dwMax);
  //ilker gerek yok gibi Application.ProcessMessages;
end;

initialization

finalization


end.


