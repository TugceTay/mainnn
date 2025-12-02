unit Lider.CG.ModulesCom.Table.Entity;

//{$I Lider.CG.Com.Component.inc}
 
interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Lider.CG.Com.EntityInt,
  TypInfo,
  Vcl.Graphics,
  Math,
//  Gosterge,
//  Lider.CG.ModulesCom.Table.CreateNcTableOfXml,
  Lider.CG.ModulesCom.Table.CellRectangle,
//  Lider.CG.ModulesCom.Table.Cells,
//  Lider.CG.ModulesCom.Table.RowInt,
//  Lider.CG.ModulesCom.Table.Rows,
  Lider.CG.Com.LicadInt,
  System.Generics.Collections,
  Lider.CG.Com.GeoTypes,
  System.RegularExpressions,
  Lider.CG.Com.GIS,
  System.Generics.Defaults,
  Lider.CG.Com.CmdLine,
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.Forms,
  System.IOUtils,
  Winapi.Windows,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.UtilityCommands,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Lib;

type
  CellUpdate = (Yok, Varr, Degistirildi, Degismis);

  Cells = class//(TList<CellRectangle>)
    private
      _TextCount: Integer;
    function GetMaxHeight2: Double;//0

    public
      CellRectangleList: TList<CellRectangle>;

      _LastEditIndex: Integer; //-1

      function GetGetCloneLast(TextStartIndex: Integer; EditOriginal: Boolean): Cells;
      property GetCloneLast[TextStartIndex: Integer; EditOriginal: Boolean]: Cells read GetGetCloneLast;

      function GetGetCloneFirst(TextStartIndex: Integer): Cells;
      property GetCloneFirst[TextStartIndex: Integer]: Cells read GetGetCloneFirst;

      function GetCellByNme(Name: String): CellRectangle;
      function GetCellsByNme(Name: String): Cells;

      procedure SortLefttoRight;
      procedure DoSameHeights;

      function GetWidth: Double;
      property Width: Double read GetWidth;

      function GetMaxHeight: Double;
      property MaxHeight: Double read GetMaxHeight;

      procedure SetStil(_BlokDosyasi: IlicgEntityList);//Asistan.CanvasObj.NczReader

      function SetvalueCell(IsRow: Boolean; ValueName: String; Value: String): CellUpdate;

      function GetGet_MaxTextCount_in_One_Cel: Integer;
      property Get_MaxTextCount_in_One_Cel: Integer read GetGet_MaxTextCount_in_One_Cel;

      procedure Add(item: CellRectangle); //override;

      constructor Create;
      destructor Destroy; override;
  end;

  Rows = class//(TList<Cells>)
  private
    _TotalRowsHeigh: Double;
    _CurrentCelHeigt: Double;

    _Endedits: Boolean;//False
  public

    CellsList: TList<Cells>;

    function GetGetRowsHight: Double;
    property GetRowsHight: Double read GetGetRowsHight;

    procedure EndEdit;
    procedure Reset;
    constructor Create;
    destructor Destroy; override;
  end;

  Tbl = class
    private
      _LineCount: Integer; //0
      _Stil: Integer; //0
      TableTabloTempRow: Cells;
      TableTabloRows: Rows;
      TableTabloTopCell: Cells;
      TableTabloDownCell: Cells;
      TableTabloTopCell_FirstCount: Integer;

      procedure FindTabloRow(_BlokDosyasi: IlicgEntityList);
      procedure FindTabloUp(_BlokDosyasi: IlicgEntityList);
      procedure FindTabloDown(_BlokDosyasi: IlicgEntityList);
      procedure Setpage;

    public
      GOlcek: Double;
      Name: String; //''
      TabloRec: ILicgEntity;
      TabloWidth: Double;
      GTemplateWidth: Double;
      GFirstSpaceWidth: Double;

      function GetGet_TableTabloTopCell_FirstCount: Integer;
      property Get_TableTabloTopCell_FirstCount: Integer read GetGet_TableTabloTopCell_FirstCount;

      procedure FindAllRow(_BlokDosyasi: IlicgEntityList);
      procedure reset;
      procedure AddNewRow;
      procedure EndEdit;

      function GetGet_TopCells: Cells;
      property Get_TopCells: Cells read GetGet_TopCells;

      function GetGet_DownCells: Cells;
      property Get_DownCells: Cells read GetGet_DownCells;

      function GetGet_RowsCells: Rows;
      property Get_RowsCells: Rows read GetGet_RowsCells;

      function GetGet_Tempow: Cells;
      property Get_Tempow: Cells read GetGet_Tempow;

      function GetLineCount: Double;
      procedure SetLineCount(Value: Double);
      property LineCount: Double read GetLineCount write SetLineCount;

      function GetStil: Integer;
      procedure SetStil(Value: Integer);
      property Stil: Integer read GetStil write SetStil;

      function Deltax(x1, x2: Double): Double;
      function Deltay(y1, y2: Double): Double;
      function Mesafe(y1, x1, y2, x2: Double): Double;

      Constructor Create;
      destructor Destroy; override;
//      function GetProjectEntityList(PFileName: String = ''): IlicgEntityList;
  end;

  Tablo = class//(TList<Tbl>)
  private
    pattern: String;
    Myrgx: TRegex;
    matches: TMatch;

    Cell: Cells;
    BlokDisSiniri: ILicgEntity;

    procedure FindKrokiPen(_BlokDosyasi: IlicgEntityList);
    procedure FindTablo(_BlokDosyasi: IlicgEntityList);
    procedure FindCell(_BlokDosyasi: IlicgEntityList);

  public
    FGIS: TlicgBaseGIS;
    ResultList: IlicgEntityList;
    GOlcek: Double;
//    GTemplateWidth: Double;

    TblList: TList<Tbl>;
    function Get_BlokDisSiniri: ILicgEntity;
    procedure Set_BlokDisSiniri(BlokDisSiniri: ILicgEntity);
    procedure Setvalue(ValueName: String; Value: String);
    function Get_Tables: Cells;
    function Get_Last_Row_By_TabloName(TabloName: String): Rows;
    procedure AddInFile(Ofset_X: Double = 0; Ofset_Y: Double = 0; POran: Double = 1); //var Ncz: IlicgEntityList;
    procedure Reset;

    function GetGet_Celss: Cells;
    property Get_Celss: Cells read GetGet_Celss;

    constructor Create(POlcek: Double = 1); overload;
    constructor Create(_BlokDosyasi: IlicgEntityList; BlokDisSiniri: ILicgEntity = nil; POlcek: Double = 1; PWidthType: Integer = 0; PWidth: Double = 0); overload;
    destructor Destroy; override;
    function GetSavingLayer(PLayerName: string; PColor: TColor = -1; PGIS:TLicgBaseGIS=nil): TlicgBaseLayer;

//    function GetProjectEntityList(PFileName: String = ''): IlicgEntityList;

    procedure CellRectangleToEntity(item: CellRectangle);//: IlicgEntity;

//    function SwitchEntityXY(PEnt: IlicgEntity): IlicgEntity;
  end;

//var
//  pattern : String = '\bTBL/\b';

var Currentindex: Integer;
    GRecNo: Integer;
//    DislamaString: String = 'dýþlanan';

const
  GRepaitDownTable: string = 'repait';

  GDisCerceveName: string = 'DIS_CERCEVE';
  GKrokiName: string = 'KROKI';
  GKuzeyName: string = 'KUZEY';
  GTbl: string = 'TBL/';
  GPattern: string = '\#\#';//'\?\?';
  GUST: string = 'UST';
  GALT: string = 'ALT';
  GSOL: string = 'SOL';
  GSAG: string = 'SAG';
  GHizaStil: string = '0/0/1';
  GKUTU: string = 'KUTU';
  GGeri: string = '\b';
  GSlash: string = '/';
  GYazi: string = 'yazi';
  GDefFontName: string = 'Arial';
  GDefFontHeight: Double = 12;
  GALAN: string = 'ALAN';

  LayerCell: string = 'TABLE_CELL';
  LayerTabloTop: string = 'TABLE_TABLO_TOP';
  LayerTabloDown: string = 'TABLE_TABLO_DOWN';
  LayerTabloRow: string = 'TABLE_TABLO_ROW';
  LayerStilFont: string = 'TABLE_STIL_FONT';
  LayerLine: string = 'TABLE_LAYER_LINE';
  LayerText: string = 'TABLE_LAYER_TEXT';
  LayerStilAlign: string = 'TABLE_STIL_ALIGN';
  LayerCellBorder: string = 'TABLE_CELL_BORDER';


implementation

function RectangleToPoly(PEnt: IlicgEntity): IlicgEntity;
var
  resultEnt: IlicgEntity;
begin
  //
  resultEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);
  resultEnt.Geometry.DrawPoints.Assign(PEnt.Geometry.DrawPoints);
  //  resultEnt.Assign(PEnt);

//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(resultEnt);

//  resultEnt.Geometry.Points.X[0] := PEnt.Geometry.Points.X[0];
//  resultEnt.Geometry.Points.Y[0] := PEnt.Geometry.Points.Y[0];
//
//  resultEnt.Geometry.Points.X[1] := PEnt.Geometry.Points.X[1];
//  resultEnt.Geometry.Points.Y[1] := PEnt.Geometry.Points.Y[0];
//
//  resultEnt.Geometry.Points.X[2] := PEnt.Geometry.Points.X[1];
//  resultEnt.Geometry.Points.Y[2] := PEnt.Geometry.Points.Y[1];
//
//  resultEnt.Geometry.Points.X[3] := PEnt.Geometry.Points.X[0];
//  resultEnt.Geometry.Points.Y[3] := PEnt.Geometry.Points.Y[1];

  // // // // // //
  result := resultEnt;
end;

constructor Tablo.Create(POlcek: Double = 1);
begin
//  GTemplateWidth := 0;
  FGIS := CurrCmdLine.ActiveDrawBox.GIS;
  GOlcek := POlcek;
  ResultList := TlicgEntityList.Create;
  Cell := Cells.Create;
  TblList := TList<Tbl>.Create;
  pattern := GGeri + GTbl + GGeri;
  Myrgx := TRegex.Create(pattern, [roSingleLine]);
end;

function Tablo.GetSavingLayer(PLayerName: string; PColor: TColor = -1; PGIS:TlicgBaseGis=nil): TlicgBaseLayer;
var ResulLayer: TlicgBaseLayer;
begin
  if PColor = -1 then
    PColor := clBlack;

  if PGIS = nil then
    PGIS := CurrCmdLine.ActiveDrawBox.GIS;

  ResulLayer := PGIS.Layers.LayerByName(PLayerName);

  if (PLayerName <> '') then
  begin
    if (ResulLayer = nil) then
      ResulLayer := PGIS.CreateLayer(PLayerName, ltDesktop, PColor)
    else
      ResulLayer := PGIS.Layers.LayerByName(PLayerName)
  end
  else
    ResulLayer := PGIS.CurrentLayer;

  Result := ResulLayer;
end;
{
function Tablo.SwitchEntityXY(PEnt: IlicgEntity): IlicgEntity;
var
  I: Integer;
  TempCoor: TlicgCoor;
  resultEnt: IlicgEntity;
begin
  result := Pent;
  Exit;
  resultEnt := PEnt.Clone;
  for I := 0 to PEnt.Geometry.Points.Count - 1 do
  begin
    TempCoor.X := PEnt.Geometry.Points.X[I];
    TempCoor.Y := PEnt.Geometry.Points.Y[I];
    resultEnt.Geometry.Points.X[I] := TempCoor.Y;
    resultEnt.Geometry.Points.Y[I] := TempCoor.X;
  end;
  result := resultEnt;
end;
}
procedure Tablo.CellRectangleToEntity(item: CellRectangle);//: IlicgEntity;
var
  obje, YaziObjesi, Alan, AlanObje, CizgiObjesiUst,
  CizgiObjesiAlt, CizgiObjesiSol, CizgiObjesiSag: IlicgEntity;
  Yaziindex: Integer;
  yazipoint: Point;
  Ext: TlicgExtent;
  matches: TMatch;
  pattern1: String;
  Myrgx: TRegex;
  TempCmdLine: TLicgBaseCmdLine;
//  TempEntity: IlicgEntity;
begin
  TempCmdLine := CurrCmdLine;

  obje := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);//As New Asistan.CadObj.CoveringCadObject
//  'If GroupCode <> "" Then
//  '    obje.objname = "@ASIS" & GroupCode
//  'End If
//  tempEntity := Licad.CreateEntityFactory.MakeEntity(idPolygon,5,_2D);
//  tempEntity.Geometry.Points.X[0] := item.SolAlt.X;
//  tempEntity.Geometry.Points.Y[0] := item.SolAlt.Y;
//
//  tempEntity.Geometry.Points.X[1] := item.SagAlt.X;
//  tempEntity.Geometry.Points.Y[1] := item.SagAlt.Y;
//
//  tempEntity.Geometry.Points.X[2] := item.SagUst.X;
//  tempEntity.Geometry.Points.Y[2] := item.SagUst.Y;
//
//  tempEntity.Geometry.Points.X[3] := item.SolUst.X;
//  tempEntity.Geometry.Points.Y[3] := item.SolUst.Y;
//
//  tempEntity.Geometry.Points.X[4] := item.SolAlt.X;
//  tempEntity.Geometry.Points.Y[4] := item.SolAlt.Y;
//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(tempEntity);
  If item.Fill Then
  begin
//      ' Hücre Taramalarý
//      ' item.FillColor := 1
    Alan := item.GetCorners; //As Nc_CollectionsOf_PolyPoint
    AlanObje := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);//As New Asistan.CadObj.Tag07_PlineBase;
    obje := Alan; //AlanObje.Obje
//    obje.Tag := 7;
    {
    obje.Geometry.DisableEvents := true;
    Ext.LowerLeft.X := Alan.Geometry.Points[0].X;// Item[(0)].x; //Limits.cll.
    Ext.LowerLeft.y := Alan.Geometry.Points[0].y;
    Ext.UpperRight.x := Alan.Geometry.Points[1].x;
    Ext.UpperRight.y := Alan.Geometry.Points[1].y;
    obje.Geometry.Points.Extent := ext;
    }
    obje.DrawTools.PenTool.Style := 0; //lt
    obje.DrawTools.BrushTool.Pattern := 1; //flags
//    obje.Flagsone := 1;
    obje.DrawTools.PenTool.Color := item.FillColor; //renk
    obje.DrawTools.BrushTool.ForeColor := item.FillColor; //renk
    obje.DrawTools.BrushTool.BackColor := item.FillColor; //renk
    obje.Layer := GetSavingLayer(item.TabakaAdi,item.FillColor,FGIS);// CreateLayer(item.TabakaAdi, item.FillColor);
    obje.Name := GALAN;
//    obje := SwitchEntityXY(obje);

    ResultList.Add(obje);
//    GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//    TempCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
//    TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Alan, item.TabakaAdi);
  End;
//  ' Hücre dogrularý
  If item.UstCerceve.Draw Then
  begin
//      ' item.FillColor := 2
    CizgiObjesiUst := Licad.CreateEntityFactory.MakeEntity(idline,2,_2D);//As New Asistan.CadObj.Tag02_LineBase;
//    obje := New Asistan.CadObj.CoveringCadObject;
    obje := CizgiObjesiUst;
//    obje.Tag := 2;
    {
    obje.Geometry.DisableEvents := true;
    Ext.LowerLeft.X := item.SolUst.X;// Item[(0)].x; //Limits.cll.
    Ext.LowerLeft.y := item.SolUst.Y;
    Ext.UpperRight.x := item.SagUst.X;
    Ext.UpperRight.y := item.SagUst.Y;
    obje.Geometry.Points.Extent := ext;
    }

//    obje.Limits.cll.x := item.SolUst.X;
//    obje.Limits.cll.y := item.SolUst.Y;
//    obje.Limits.cur.x := item.SagUst.X;
//    obje.Limits.cur.y := item.SagUst.Y;
//    obje.Geometry.Points.Add(AsCoor(0,0));
//    obje.Geometry.Points.Add(AsCoor(0,0));
    obje.Geometry.Points.X[0] := item.SolUst.X;
    obje.Geometry.Points.Y[0] := item.SolUst.Y;
    obje.Geometry.Points.Z[0] := 0;
    obje.Geometry.Points.X[1] := item.SagUst.X;
    obje.Geometry.Points.Y[1] := item.SagUst.Y ;
    obje.Geometry.Points.Z[1] := 0;
    obje.DrawTools.PenTool.Style := item.UstCerceve.LineIndex; //lt
//    obje.renk := item.FillColor;
//    obje.Tabaka := CreateLayer(item.CizgiTabakaAdi, item.FillColor);
    obje.DrawTools.PenTool.Color := item.FillColor;
    obje.DrawTools.BrushTool.ForeColor := item.FillColor;
    obje.DrawTools.BrushTool.BackColor := item.FillColor;
    obje.Layer := GetSavingLayer(item.CizgiTabakaAdi,item.FillColor,FGIS);
//    obje := SwitchEntityXY(obje);

    ResultList.Add(obje);
//    GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
//    TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Nothing, item.CizgiTabakaAdi);
  End;
  If item.AltCerceve.Draw Then
  begin
//      ' item.FillColor := 2
    CizgiObjesiAlt := Licad.CreateEntityFactory.MakeEntity(idline,2,_2D);//As New Asistan.CadObj.Tag02_LineBase;
//    obje := New Asistan.CadObj.CoveringCadObject;
    obje := CizgiObjesiAlt; //obje.Obje
//    obje.Tag := 2;
    {
    obje.Geometry.DisableEvents := true;
    Ext.LowerLeft.X := item.SagAlt.X;
    Ext.LowerLeft.y := item.SagAlt.Y;
    Ext.UpperRight.x := item.SolAlt.X;
    Ext.UpperRight.y := item.SolAlt.Y;
    obje.Geometry.Points.Extent := ext;
    }
//    obje.Limits.cll.x := item.SagAlt.X;
//    obje.Limits.cll.y := item.SagAlt.Y;
//    obje.Limits.cur.x := item.SolAlt.X;
//    obje.Limits.cur.y := item.SolAlt.Y;
//    obje.Geometry.Points.Add(AsCoor(0,0));
//    obje.Geometry.Points.Add(AsCoor(0,0));
    obje.Geometry.Points.X[0] := item.SagAlt.X;
    obje.Geometry.Points.Y[0] := item.SagAlt.Y;
    obje.Geometry.Points.Z[0] := 0;
    obje.Geometry.Points.X[1] := item.SolAlt.X;
    obje.Geometry.Points.Y[1] := item.SolAlt.Y;
    obje.Geometry.Points.Z[1] := 0;
    obje.DrawTools.PenTool.Style := item.AltCerceve.LineIndex; //lt
//    obje.renk := item.FillColor;
//    obje.Tabaka := CreateLayer(item.CizgiTabakaAdi, item.FillColor);
//    AddObject(obje, Nothing, item.CizgiTabakaAdi);
    obje.DrawTools.PenTool.Color := item.FillColor;
    obje.DrawTools.BrushTool.ForeColor := item.FillColor;
    obje.DrawTools.BrushTool.BackColor := item.FillColor;
    obje.Layer := GetSavingLayer(item.CizgiTabakaAdi,item.FillColor,FGIS);
//    obje := SwitchEntityXY(obje);

    ResultList.Add(obje);
//    GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
//    TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Nothing, item.CizgiTabakaAdi);
  End;
  If item.SolCerceve.Draw Then
  begin
//      '  item.FillColor := 2
    CizgiObjesiSol := Licad.CreateEntityFactory.MakeEntity(idline,2,_2D);//As New Asistan.CadObj.Tag02_LineBase;
//    obje := New Asistan.CadObj.CoveringCadObject;
    obje := CizgiObjesiSol;
//    obje.Tag := 2;

    {
    obje.Geometry.DisableEvents := true;
    Ext.LowerLeft.X := item.SolAlt.X;
    Ext.LowerLeft.y := item.SolAlt.Y;
    Ext.UpperRight.x := item.SolUst.X;
    Ext.UpperRight.y := item.SolUst.Y;
    obje.Geometry.Points.Extent := ext;
    }
//    obje.Limits.cll.x := item.SolAlt.X;
//    obje.Limits.cll.y := item.SolAlt.Y;
//    obje.Limits.cur.x := item.SolUst.X;
//    obje.Limits.cur.y := item.SolUst.Y;
//    obje.Geometry.Points.Add(AsCoor(0,0));
//    obje.Geometry.Points.Add(AsCoor(0,0));
    obje.Geometry.Points.X[0] := item.SolAlt.X;
    obje.Geometry.Points.Y[0] := item.SolAlt.Y;
    obje.Geometry.Points.Z[0] := 0;
    obje.Geometry.Points.X[1] := item.SolUst.X;
    obje.Geometry.Points.Y[1] := item.SolUst.Y;
    obje.Geometry.Points.Z[1] := 0;
    obje.DrawTools.PenTool.Style := item.SolCerceve.LineIndex;
//    obje.renk := item.FillColor;
//    obje.Tabaka := CreateLayer(item.CizgiTabakaAdi, item.FillColor);
//    AddObject(obje, Nothing, item.CizgiTabakaAdi);
    obje.DrawTools.PenTool.Color := item.FillColor;
    obje.DrawTools.BrushTool.ForeColor := item.FillColor;
    obje.DrawTools.BrushTool.BackColor := item.FillColor;
    obje.Layer := GetSavingLayer(item.CizgiTabakaAdi,item.FillColor,FGIS);
//    obje := SwitchEntityXY(obje);

    ResultList.Add(obje);
//    GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
//    TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Nothing, item.CizgiTabakaAdi);
  End;
  If item.SagCerceve.Draw Then
  begin
//      ' item.FillColor := 2
    CizgiObjesiSag := Licad.CreateEntityFactory.MakeEntity(idline,2,_2D);//As New Asistan.CadObj.Tag02_LineBase;
//    obje := New Asistan.CadObj.CoveringCadObject;
    obje := CizgiObjesiSag;
//    obje.Tag := 2;
    {
    obje.Geometry.DisableEvents := true;
    Ext.LowerLeft.X := item.SagUst.X;
    Ext.LowerLeft.y := item.SagUst.Y;
    Ext.UpperRight.x := item.SagAlt.X;
    Ext.UpperRight.y := item.SagAlt.Y;
    obje.Geometry.Points.Extent := ext;
    }
//    obje.Limits.cll.x := item.SagUst.X;
//    obje.Limits.cll.y := item.SagUst.Y;
//    obje.Limits.cur.x := item.SagAlt.X;
//    obje.Limits.cur.y := item.SagAlt.Y;
//    obje.Geometry.Points.Add(AsCoor(0,0));
//    obje.Geometry.Points.Add(AsCoor(0,0));
    obje.Geometry.Points.X[0] := item.SagUst.X;
    obje.Geometry.Points.Y[0] := item.SagUst.Y;
    obje.Geometry.Points.Z[0] := 0;
    obje.Geometry.Points.X[1] := item.SagAlt.X;
    obje.Geometry.Points.Y[1] := item.SagAlt.Y;
    obje.Geometry.Points.Z[1] := 0;

    obje.DrawTools.PenTool.Style := item.SagCerceve.LineIndex;

//    obje.renk := item.FillColor;
//    obje.Tabaka := CreateLayer(item.CizgiTabakaAdi, item.FillColor);
//    AddObject(obje, Nothing, item.CizgiTabakaAdi);
    obje.DrawTools.PenTool.Color := item.FillColor;
    obje.DrawTools.BrushTool.ForeColor := item.FillColor;
    obje.DrawTools.BrushTool.BackColor := item.FillColor;
    obje.Layer := GetSavingLayer(item.CizgiTabakaAdi,item.FillColor,FGIS);
//    obje := SwitchEntityXY(obje);

    ResultList.Add(obje);
//    GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//    CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
//    TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Nothing, item.CizgiTabakaAdi);
  End;

  Yaziindex := 0;
  yazipoint := Point.Create;
  For yazipoint In item.CellYaziCor do //As Asistan.CadObj.Tablo.Point
  begin
      If item.CellYazi[Yaziindex] = '' Then
      begin
        Yaziindex := Yaziindex + 1;
        Continue;// For
      end;
//      '  item.FillColor := 3
      YaziObjesi := Licad.CreateEntityFactory.MakeEntity(idText,1,_2D);
      //Dim YaziObjesi As New Asistan.CadObj.Tag05_TextBase
//      obje := New Asistan.CadObj.CoveringCadObject;
      obje := YaziObjesi;
//      obje.Tag := 5;

//      obje.Geometry.Points.Add(AsCoor(0,0));
      obje.Geometry.Points.X[0] := yazipoint.X;
      obje.Geometry.Points.Y[0] := yazipoint.Y;
      obje.Geometry.Points.Z[0] := 0;
      obje.AsTextValue.Text := item.CellYazi[Yaziindex];
      obje.DrawTools.FontTool.Angle := item.Aci;
      obje.DrawTools.FontTool.TextPos := item.GetNcJustNum;
      obje.DrawTools.FontTool.Height := item.Font.Height;

//      obje.renk := item.FillColor;
      obje.DrawTools.PenTool.Color := item.FillColor;
      obje.DrawTools.BrushTool.ForeColor := item.FillColor;
      obje.DrawTools.BrushTool.BackColor := item.FillColor;

//      Dim CultureTextInfo As TextInfo := New CultureInfo("en-US", False).TextInfo;
//      obje.ref := GetFontIndex(CultureTextInfo.ToUpper(item.Font.Name) & ".NCF");

//      obje.Tabaka := CreateLayer(item.YaziTabakaAdi, item.FillColor);
      obje.Layer := GetSavingLayer(item.YaziTabakaAdi,item.FillColor,FGIS);

//      Dim matches As Match;
      pattern1 := GPattern;
      Myrgx := TRegex.Create(pattern1, [roSingleline]);

//      repeat
          matches := TRegex.Match(obje.AsTextValue.Text, pattern1, [roSingleline]);
          If matches.Success Then
            obje.AsTextValue.Text := Myrgx.Replace(obje.AsTextValue.Text, '');//, 1, matches.Index);
//      until(Not matches.Success);

//      Loop Until Not matches.Success;
//      ' If matches.Success Then obje.s := Myrgx.Replace(obje.s, "", 1, matches.Index)

      If obje.AsTextValue.Text = '' Then
      Else If obje.AsTextValue.Text = GSlash Then
      Else
      begin
//        obje := SwitchEntityXY(obje);
        obje.Geometry.UpdateExtension;

        ResultList.Add(obje);
//        GRecNo := TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName).AddEntity(obje);
//        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(TempCmdLine.ActiveDrawBox.GIS.Layers.LayerByName(TlicgBaseLayer(obje.Layer).DisplayName),GRecNo,uaDelete);
      end;
//          TlicgBaseLayer(obje.Layer).AddEntity(obje); //AddObject(obje, Nothing, item.YaziTabakaAdi);

      Yaziindex := Yaziindex + 1;
  end;

//  'Dim KutuObjesi As New Asistan.CadObj.Tag10_RectangleBase;
//  'item.FillColor := 4;
//  'obje := New Asistan.CadObj.CoveringCadObject;
//  'obje.Obje := KutuObjesi;
//  'obje.Tag := 10;
//  'obje.Limits.cll.x := item.Limit.cll.X;
//  'obje.Limits.cll.y := item.Limit.cll.Y;
//  'obje.Limits.cur.x := item.Limit.cur.X;
//  'obje.Limits.cur.y := item.Limit.cur.Y;
//  'obje.p1.x := item.SolAlt.X;
//  'obje.p1.y := item.SolAlt.Y;
//  'obje.p1.z := 0;
//  'obje.p2.x := item.SolAlt.X + item.Yukseklik;
//  'obje.p2.y := item.SolAlt.Y + item.Genislik;
//  'obje.angle := item.Aci;
//  'obje.renk := item.FillColor;
//  'obje.Tabaka := CreateLayer(item.TabakaAdi, item.FillColor);
//  'obje.pname := "KUTU";
//  'AddObject(obje, Nothing, item.TabakaAdi);
end;

procedure Tablo.AddInFile(Ofset_X: Double = 0; Ofset_Y: Double = 0; POran: Double = 1); //var Ncz: IlicgEntityList;
var
  Atg: Tbl;
  TopCells, cll, Dcel, Clls: CellRectangle;
  RowsCel: Cells;
  I: Integer;
begin
  I := 0;
//  if fmGosterge <> nil then
//  begin
//    fmGosterge.Close;
//    fmGosterge.Free;
//  end;
//  fmGosterge := TfmGosterge.Create(Application);
//  fmGosterge.Execute(TblList.Count,'Nesneler Yazdýrýlýyor...',False);
  For Atg In TblList do
  begin
    inc(i);
//    fmGosterge.Gosterge(i);
      Atg.EndEdit;
      For TopCells In Atg.Get_TopCells.CellRectangleList do
      begin
        TopCells.Olcek := POran;
          TopCells.ScrollLeftReight(Ofset_X);
          TopCells.ScrollUpDown(Ofset_Y);
          TopCells.Fill := False;
          CellRectangleToEntity(TopCells);
//          GetSavingLayer(TopCells.TabakaAdi).AddEntity(TopCells as IlicgEntity);// Ncz.AddObject(TopCells);
      end;
      For RowsCel In Atg.Get_RowsCells.CellsList do //200mb yer
      begin
          For cll In RowsCel.CellRectangleList do
          begin
            cll.Olcek := POran;
              cll.ScrollLeftReight(Ofset_X);
              cll.ScrollUpDown(Ofset_Y);
              cll.Fill := False;
              CellRectangleToEntity(cll);
//              GetSavingLayer(cll.TabakaAdi).AddEntity(cll as IlicgEntity);//Ncz.AddObject(cll);
          end;
      end;
      For Dcel In Atg.Get_DownCells.CellRectangleList do
      begin
        Dcel.Olcek := POran;
          Dcel.ScrollLeftReight(Ofset_X);
          Dcel.ScrollUpDown(Ofset_Y);
          Dcel.Fill := False;
          CellRectangleToEntity(Dcel);
//          GetSavingLayer(Dcel.TabakaAdi).AddEntity(Dcel as IlicgEntity);//Ncz.AddObject(Dcel);
      end;
  end;
  For Clls In Self.Get_Celss.CellRectangleList do
  begin
    Clls.Olcek := POran;
      Clls.ScrollLeftReight(Ofset_X);
      Clls.ScrollUpDown(Ofset_Y);
      Clls.Fill := False;
      CellRectangleToEntity(Clls);
//      GetSavingLayer(Clls.TabakaAdi).AddEntity(Clls as IlicgEntity);//Ncz.AddObject(Clls);
  end;
//  fmGosterge.Close;
//  fmGosterge.Free;
//  FreeAndNil(TopCells);
//  FreeAndNil(RowsCel);
//  FreeAndNil(Dcel);
//  FreeAndNil(Clls);
//  FreeAndNil(Atg);
//  TblList.Clear;
//  FreeAndNil(TblList);
end;

function RealMod(x, y : extended) : extended;
begin
   Result := x - y * Trunc(x/y);
end;

constructor Tablo.Create(_BlokDosyasi: IlicgEntityList; BlokDisSiniri: ILicgEntity = nil; POlcek: Double = 1; PWidthType: Integer = 0; PWidth: Double = 0);
var
  NewRow: Cells;
  a: Tbl;
  ss: CellRectangle;
  I: Integer;
  TempEnt: IlicgEntity;
  TempExtent: TlicgExtent;
  TempDouble: Double;
  SCount: Integer;
begin
  TempDouble := 0;
  TempExtent := _NULL_EXTENT;
  for I := 0 to _BlokDosyasi.Count - 1 do
  begin
    TempEnt := _BlokDosyasi[I];
    if (TempEnt <> nil) and (TempEnt.EntityID in [idPolygon,idPolyLine,idLine,idRectangle]) then
    begin
      if (TempExtent.LowerLeft = _NULL_EXTENT.LowerLeft)
      and (TempExtent.UpperRight = _NULL_EXTENT.UpperRight) then
        TempExtent := TempEnt.Geometry.Points.Extent;
      CalcMaxMinBounds(TempExtent,TempEnt.Geometry.Points.Extent);
      TempDouble := Abs(TempExtent.LowerLeft.X-TempExtent.UpperRight.X);
    end;
  end;

  FGIS := CurrCmdLine.ActiveDrawBox.GIS;
  GOlcek := POlcek;
  ResultList := TlicgEntityList.Create;
  Cell := Cells.Create;
  TblList := TList<Tbl>.Create;
  pattern := GGeri + GTbl + GGeri;
  Myrgx := TRegex.Create(pattern, [roSingleLine]);

  If (BlokDisSiniri = nil) Then
      FindKrokiPen(_BlokDosyasi)
  Else
      Self.BlokDisSiniri := BlokDisSiniri;

  FindTablo(_BlokDosyasi);

  I := 0;
//  fmGosterge := TfmGosterge.Create(Application);
//  fmGosterge.Execute(TblList.Count,'Nesneler Ýþleniyor...',False);

  For a In TblList do
  begin
    inc(i);
//    fmGosterge.Gosterge(i);
    a.GOlcek := Golcek;
    if PWidthType = 0 then //Kenan BATKI (Raporlar) (Þablon - Tablo kadar boþluk koyar her birine)
    begin
      a.GTemplateWidth := Abs(TempDouble-a.TabloWidth);
      a.GFirstSpaceWidth := 0;
    end
    else if PWidthType = 1 then //Hasan ABALI (Raporlar) (Ýlk Þablon - Tablo kadar boþluk koyar sonra her birine þablona sýðacak kadar sýðdýrýp boþluk ayarlar)
    begin
      a.GTemplateWidth := PWidth;
      if (TempDouble <> 0) and (PWidth = 0) and (TempDouble > a.TabloWidth) then
      begin
        SCount := Trunc(TempDouble/a.TabloWidth);
        if SCount > 1 then
          a.GTemplateWidth := RealMod(TempDouble, a.TabloWidth)/(SCount-1);
//        else if True then
//          a.GTemplateWidth := Abs(TempDouble - a.TabloWidth);
      end;
      a.GFirstSpaceWidth := Abs(TempDouble-a.TabloWidth);
    end
    else if PWidthType = 2 then //Klasik Koordine vb. için. (Verilen boþluk kadar koyar her birine)
    begin
      a.GTemplateWidth := PWidth;
      a.GFirstSpaceWidth := 0;//Abs(TempDouble-a.TabloWidth);
    end;

      a.FindAllRow(_BlokDosyasi);
      a.Get_Tempow.SetStil(_BlokDosyasi);
      a.Get_DownCells.SetStil(_BlokDosyasi);
      a.Get_TopCells.SetStil(_BlokDosyasi);
      a.Get_Tempow.SortLefttoRight; // Tablodaki Verileri Soldan Saða basmasýný saðlýyor.

      NewRow := Cells.Create;// New Tbl.Cells;
      For ss In a.Get_Tempow.CellRectangleList do
      begin
          NewRow.CellRectangleList.Add(ss.GetCopy);
//          NewRow.CellRectangleList[NewRow.CellRectangleList.Count - 1].Yazi := '??';
      end;

      a.Get_RowsCells.CellsList.Add(NewRow);

  end;
//  fmGosterge.Close;
//  fmGosterge.Free;
  FindCell(_BlokDosyasi);
  Cell.SetStil(_BlokDosyasi);
end;

destructor Tablo.Destroy;
var
  _tbl: Tbl;
begin
  for _tbl in TblList do
    _tbl.Free;
  inherited;
end;

procedure Tablo.FindCell(_BlokDosyasi: IlicgEntityList);
var
  I: Integer;
  TabloCel: CellRectangle;
  ft: IlicgFontTool;
  Olcek: Double;
  Coor1, Coor2: TlicgCoor;
begin
  Olcek := GOlcek;//CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
//  ft := IlicgFontTool.Create;
  ft := Licad.Settings.FontTool;
  ft.Name := GDefFontName;
  ft.Height := GDefFontHeight;
  If (BlokDisSiniri = nil) Then
  begin
      For i := 0 To _BlokDosyasi.Count - 1 do
      begin
        //GetLayerName(_BlokDosyasi.GetObject(i).Tabaka)
          If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCell) And
              (_BlokDosyasi[i].EntityID = idRectangle) And //.GetObject(i).Tag = 10 And
              (_BlokDosyasi[i].Name <> GKrokiName) And // GetObject(i).pname
              (_BlokDosyasi[i].Name <> GKuzeyName) Then //GetObject(i).pname
          begin
              Coor1 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[0];
              Coor2 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[2];
              TabloCel := CellRectangle.Create(_BlokDosyasi[i].Name, //.GetObject(i).pname,
                                              '',
                                              ft,//New Asistan.CadObj.RFont('Arial', 12),
                                              DikeyHizalama.DikeyOrta,
                                              YatayHizalama.YatayOrta,
                                              LayerCell,
                                              False,
                                              0,
                                              Coor1.Y ,//GetObject(i).p1.y,
                                              Coor1.X ,//GetObject(i).p1.x,
                                              Coor2.Y ,//GetObject(i).p2.y,
                                              Coor2.X ,//GetObject(i).p2.x,
                                              Olcek,//_BlokDosyasi[i].,//GetScale,
                                              _BlokDosyasi[i].AsRectangle.Rotangle,
                                              RectangletoPoly(_BlokDosyasi[i]));//GetObject(i).angle)
//              _BlokDosyasi[i].EntityID := idNone;// .GetObject(i).Tag = 0;
              TabloCel.FillColor := TlicgBaseLayer(_BlokDosyasi[i].Layer).LayerInfo.Color;// GetLayer(_BlokDosyasi.GetObject(i).Tabaka).LayerAndProperty.Color
              Cell.CellRectangleList.Add(TabloCel);
          End;
      end;
  end
  Else
  begin
      For i := 0 To _BlokDosyasi.Count - 1 do
      begin
          If (BlokDisSiniri.InPoly(_BlokDosyasi[i].Geometry.Points[0])
          or BlokDisSiniri.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) And
              //_BlokDosyasi.GetPolyline(BlokDisSiniri).PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 And
             (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCell) And
             (_BlokDosyasi[i].EntityID = idRectangle) And//GetObject(i).Tag = 10 And
             (_BlokDosyasi[i].Name <> GKrokiName) And // GetObject(i).pname
             (_BlokDosyasi[i].Name <> GKuzeyName) Then
          begin
            Coor1 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[0];
            Coor2 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[2];
//            if _BlokDosyasi[i].AsRectangle.Rotangle <> 0 then
//              _BlokDosyasi[i].Name := _BlokDosyasi[i].Name;
            TabloCel := CellRectangle.Create(_BlokDosyasi[i].Name,
                                            '',
                                            ft,//New Asistan.CadObj.RFont('Arial', 12),
                                            DikeyHizalama.DikeyOrta,
                                            YatayHizalama.YatayOrta,
                                            LayerCell,
                                            False,
                                            0,
                                            Coor1.Y ,//_BlokDosyasi.GetObject(i).p1.y,
                                            Coor1.X ,//_BlokDosyasi.GetObject(i).p1.x,
                                            Coor2.Y ,//_BlokDosyasi.GetObject(i).p2.y,
                                            Coor2.X ,//_BlokDosyasi.GetObject(i).p2.x,
                                            Olcek,//_BlokDosyasi.GetScale,
                                            _BlokDosyasi[i].AsRectangle.Rotangle,
                                            RectangletoPoly(_BlokDosyasi[i]));//_BlokDosyasi.GetObject(i).angle)
//              _BlokDosyasi.GetObject(i).Tag = 0
              TabloCel.FillColor := TlicgBaseLayer(_BlokDosyasi[i].Layer).LayerInfo.Color;//_BlokDosyasi.GetLayer(_BlokDosyasi.GetObject(i).Tabaka).LayerAndProperty.Color
              Cell.CellRectangleList.Add(TabloCel);
           End;
      end;
  End;
end;

procedure Tablo.FindKrokiPen(_BlokDosyasi: IlicgEntityList);
var
  i: Integer;
begin
  For i := 0 To _BlokDosyasi.Count - 1 do
  begin
      If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCell) And
         (_BlokDosyasi[i].EntityID = idRectangle) And (_BlokDosyasi[i].Name = GDisCerceveName) Then
      begin
          BlokDisSiniri := _BlokDosyasi[i].Clone;// GetCopy;
//          _BlokDosyasi.GetObject(i).Tag = 0;
          Break;
      End;
  end;
end;

procedure Tablo.FindTablo(_BlokDosyasi: IlicgEntityList);
var
  I: Integer;
  Tablom: Tbl;
  ev: TMatchEvaluator;
begin
  If (BlokDisSiniri = nil) Then
  begin
      For i := 0 To _BlokDosyasi.Count - 1 do
      begin
          If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCell) And
              (_BlokDosyasi[i].EntityID = idRectangle) And
              (Not String.IsNullOrEmpty(_BlokDosyasi[i].Name)) And
              (TRegex.Match(_BlokDosyasi[i].Name, pattern, [roSingleLine]).Success) Then //System.Text.RegularExpressions.RegexOptions.Singleline
          begin
              Tablom := Tbl.Create;
              matches := TRegex.Match(_BlokDosyasi[i].Name, pattern, [roSingleLine]);
              Tablom.TabloRec := _BlokDosyasi[i].Clone;//.GetObject(i).GetCopy;
              Tablom.TabloWidth := Abs(_BlokDosyasi[i].Geometry.Points[0].X - _BlokDosyasi[i].Geometry.Points[1].X);
//              Tablom.TabloRec.ParamString := DislamaString;
//              _BlokDosyasi[i].Name := Myrgx.Replace(_BlokDosyasi[i].Name, '', 1);//, ev(matches));//1, matches.Index);
              Tablom.Name := _BlokDosyasi[i].Name.Split([GSlash])[1];
              Tablom.LineCount := StrToFloat(_BlokDosyasi[i].Name.Split([GSlash])[2]);
              Tablom.Stil := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[3]);
//              _BlokDosyasi[i] := Licad.CreateEntityFactory.MakeEntity(idNone,0,_2D);
//              _BlokDosyasi.GetObject(i).Tag = 0;
//            Tablom.GOlcek := GOlcek;
              TblList.Add(Tablom);
          End;
      end;
  end
  Else
  begin
      For i := 0 To _BlokDosyasi.Count - 1 do
      begin
          If //(_BlokDosyasi.GetPolyline(BlokDisSiniri).PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1)
              (BlokDisSiniri.InPoly(_BlokDosyasi[i].Geometry.Points[0])
              or BlokDisSiniri.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) And
              (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCell) And
             (_BlokDosyasi[i].EntityID = idRectangle) And
              (Not String.IsNullOrEmpty(_BlokDosyasi[i].Name)) And
              (TRegex.Match(_BlokDosyasi[i].Name, pattern, [roSingleLine]).Success) Then
          begin
              Tablom := Tbl.Create;// New Tbl
              matches := TRegex.Match(_BlokDosyasi[i].Name, pattern, [roSingleLine]);
              Tablom.TabloRec := _BlokDosyasi[i].Clone;//_BlokDosyasi.GetObject(i).GetCopy;
              Tablom.TabloWidth := Abs(_BlokDosyasi[i].Geometry.Points[0].X - _BlokDosyasi[i].Geometry.Points[1].X);
//              Tablom.TabloRec.ParamString := DislamaString;
//              _BlokDosyasi[i].Name := Myrgx.Replace(_BlokDosyasi[i].Name, '', 1);//, ev(matches));//1, matches.Index);
              Tablom.Name := _BlokDosyasi[i].Name.Split([GSlash])[1];
              Tablom.LineCount := StrToFloat(_BlokDosyasi[i].Name.Split([GSlash])[2]);
              Tablom.Stil := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[3]);
//              _BlokDosyasi[i] := Licad.CreateEntityFactory.MakeEntity(idNone,0,_2D);
//              _BlokDosyasi.GetObject(i).Tag = 0
//              Tablom.GOlcek := GOlcek;
              TblList.Add(Tablom);
          End;
      end;
  End;
end;

function Tablo.Get_BlokDisSiniri: ILicgEntity;
begin
  Result := Self.BlokDisSiniri;
end;

function Tablo.Get_Last_Row_By_TabloName(TabloName: String): Rows;
var
  rslt: Rows;
  Atg: Tbl;
begin
  rslt := Rows.Create;
  For Atg In Self.TblList do
  begin
      If Atg.Name = TabloName Then
          rslt.CellsList.Add(Atg.Get_RowsCells.CellsList[Atg.Get_RowsCells.CellsList.Count - 1]);//Item(Atg.Get_RowsCells.Count - 1));
  end;
  result := rslt;
end;

function Tablo.GetGet_Celss: Cells;
begin
  result := Cell;
end;

function Tablo.Get_Tables: Cells;
var
  rslt, RowsCel: Cells;
  Atg: Tbl;
  TopCells, cll, Clls, Dcel: CellRectangle;
begin
  rslt := Cells.Create;
  For Atg In TblList do
  begin
      Atg.EndEdit;
      For TopCells In Atg.Get_TopCells.CellRectangleList do
          rslt.CellRectangleList.Add(TopCells);

      For RowsCel In Atg.Get_RowsCells.CellsList do
      begin
          For cll In RowsCel.CellRectangleList do
              rslt.CellRectangleList.Add(cll);
      end;
      For Dcel In Atg.Get_DownCells.CellRectangleList do
          rslt.CellRectangleList.Add(Dcel);
  end;
  For Clls In Self.Get_Celss.CellRectangleList do
      rslt.CellRectangleList.Add(Clls);
  result := rslt;
end;

procedure Tablo.Reset;
var
  atg: Tbl;
  ath: CellRectangle;
begin
  For atg In Self.TblList do
      atg.reset;

  For ath In Self.Cell.CellRectangleList do
      ath.GoFirstCorr;
end;

procedure Tablo.Setvalue(ValueName, Value: String);
var
  a: Tbl;
  NewRow: Cells;
  ss: CellRectangle;
begin
  Cell.SetvalueCell(False, ValueName, Value);
  For a In TblList do
  begin
      a.Get_DownCells.SetvalueCell(False, ValueName, Value);
      a.Get_TopCells.SetvalueCell(False, ValueName, Value);

      If (a.Get_RowsCells.CellsList.Count = 0)
      Or (a.Get_RowsCells.CellsList[a.Get_RowsCells.CellsList.Count - 1].SetvalueCell(True, ValueName, Value) = CellUpdate.Degismis) Then
      begin
          NewRow := Cells.Create;// As New Tbl.Cells;
          For ss In a.Get_Tempow.CellRectangleList do
          begin
//            ssc := CellRectangle.Create('??',ss.SolAlt.Y,ss.SolAlt.X,ss.SagUst.Y,ss.SagUst.X,ss.Olcek,ss.Aci);
            NewRow.CellRectangleList.Add(ss.GetCopy(GYazi));//.CellRectangleList .GetCopy
//            NewRow.CellRectangleList[NewRow.CellRectangleList.Count - 1].Yazi := '??';
          end;

//          a.Get_RowsCells.CellsList[a.Get_RowsCells.CellsList.Count - 1].SetvalueCell(True, ValueName, Value);
//          if a.Get_RowsCells.CellsList.Count > 0 then
//            Continue;

          a.Get_RowsCells.CellsList.Add(NewRow);
          a.Get_RowsCells.CellsList[a.Get_RowsCells.CellsList.Count - 1].SetvalueCell(True, ValueName, Value);
      End;
  end;
end;

procedure Tablo.Set_BlokDisSiniri(BlokDisSiniri: ILicgEntity);
begin
  Self.BlokDisSiniri := BlokDisSiniri;
end;

{ Tbl }

procedure Tbl.AddNewRow;
begin
  TableTabloRows.CellsList.Add(TableTabloTempRow);
end;

procedure Tbl.EndEdit;
var
  cls: CellRectangle;
  TempStr: string;
begin
  TableTabloRows.EndEdit;
  TableTabloTopCell_FirstCount := TableTabloTopCell.CellRectangleList.Count;
  For cls In TableTabloDownCell.CellRectangleList do
  begin
    TempStr := cls.Adi;
//    if TempStr.Equals(GDownTableRepait) then
//    begin
//      cls.ScrollUpDown(-TableTabloTopCell.GetMaxHeight-TableTabloTempRow.MaxHeight);
      cls.ScrollUpDown(-TableTabloTopCell.GetMaxHeight-TableTabloTempRow.MaxHeight);
//    end
//    else
//      cls.ScrollUpDown(TableTabloRows.GetRowsHight - TableTabloTempRow.CellRectangleList[0].Yukseklik);
  end;
//      cls.ScrollUpDown(TableTabloRows.GetRowsHight - TableTabloTempRow.CellRectangleList[0].Yukseklik);
  Setpage;
end;

procedure Tbl.FindAllRow(_BlokDosyasi: IlicgEntityList);
begin
  FindTabloDown(_BlokDosyasi);
  FindTabloRow(_BlokDosyasi);
  FindTabloUp(_BlokDosyasi);
end;

procedure Tbl.FindTabloDown(_BlokDosyasi: IlicgEntityList);
var
  TabloCel: CellRectangle;
  I: Integer;
  ft: IlicgFontTool;
  Olcek: Double;
  Coor1, Coor2: TlicgCoor;
begin
  Olcek := GOlcek;//CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
//  ft := IlicgFontTool.Create;
  ft := Licad.Settings.FontTool;
  ft.Name := GDefFontName;
  ft.Height := GDefFontHeight;
  For i := 0 To _BlokDosyasi.Count - 1 do
  begin
      If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerTabloDown) And
         (_BlokDosyasi[i].EntityID = idRectangle) And
         (TabloRec.InPoly(_BlokDosyasi[i].Geometry.Points[0])
         or TabloRec.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) then// _BlokDosyasi.GetPolyline(TabloRec).PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 Then
      begin
        Coor1 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[0];
        Coor2 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[2];
          TabloCel := CellRectangle.Create(_BlokDosyasi[i].Name,
                                           '',
                                           ft,//New Asistan.CadObj.RFont('Arial', 12),
                                           DikeyHizalama.DikeyOrta,
                                           YatayHizalama.YatayOrta,
                                           LayerTabloDown,
                                           False,
                                           0,
                                           Coor1.Y,//.GetObject(i).p1.y,
                                           Coor1.X,//GetObject(i).p1.x,
                                           Coor2.Y,//GetObject(i).p2.y,
                                           Coor2.X,//GetObject(i).p2.x,
                                           Olcek,//_BlokDosyasi.GetScale,
                                           RectangletoPoly(_BlokDosyasi[i]).Geometry.Points.Z[2],
                                           RectangletoPoly(_BlokDosyasi[i]));//.GetObject(i).p2.z);
//          _BlokDosyasi.GetObject(i).Tag = 0
          TabloCel.FillColor := TlicgBaseLayer(_BlokDosyasi[i].Layer).layerInfo.Color;
//          _BlokDosyasi.GetLayer(_BlokDosyasi.GetObject(i).Tabaka).LayerAndProperty.Color;
          TableTabloDownCell.CellRectangleList.Add(TabloCel);
      End;
  end;
end;

procedure Tbl.FindTabloRow(_BlokDosyasi: IlicgEntityList);
var
  TabloCel: CellRectangle;
  ft: IlicgFontTool;
  I: Integer;
  Olcek: Double;
  Coor1, Coor2: TlicgCoor;
begin
  Olcek := GOlcek;//CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
//  ft := IlicgFontTool.Create;
  ft := Licad.Settings.FontTool;
  ft.Name := GDefFontName;
  ft.Height := GDefFontHeight;
  For i := 0 To _BlokDosyasi.Count - 1 do
  begin
      If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerTabloRow) And
         (_BlokDosyasi[i].EntityID = idRectangle) And
         (TabloRec.InPoly(_BlokDosyasi[i].Geometry.Points[0])
         or TabloRec.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) Then //_BlokDosyasi.GetPolyline(TabloRec).PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1
      begin
        Coor1 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[0];
        Coor2 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[2];

          TabloCel := CellRectangle.Create(_BlokDosyasi[i].Name,
                                           '',
                                           ft,//New Asistan.CadObj.RFont('Arial', 12),
                                           DikeyHizalama.DikeyOrta,
                                           YatayHizalama.YatayOrta,
                                           LayerTabloRow,
                                           False,
                                           0,
                                           Coor1.Y,//.GetObject(i).p1.y,
                                           Coor1.X,//.GetObject(i).p1.x,
                                           Coor2.Y,//.GetObject(i).p2.y,
                                           Coor2.X,//.GetObject(i).p2.x,
                                           Olcek,//_BlokDosyasi.GetScale,
                                           RectangletoPoly(_BlokDosyasi[i]).Geometry.Points.Z[2],
                                           RectangletoPoly(_BlokDosyasi[i]));//.GetObject(i).p2.z)
//          _BlokDosyasi.GetObject(i).Tag = 0
          TabloCel.FillColor := TlicgBaseLayer(_BlokDosyasi[i].Layer).layerinfo.color;
          TableTabloTempRow.CellRectangleList.Add(TabloCel);
      End;
  end;
end;

procedure Tbl.FindTabloUp(_BlokDosyasi: IlicgEntityList);
var
  TabloCel: CellRectangle;
  ft: IlicgFontTool;
  I: Integer;
  Olcek: Double;
  Coor1, Coor2: TlicgCoor;
begin
  Olcek := GOlcek;//CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
//  ft := IlicgFontTool.Create;
  ft := Licad.Settings.FontTool;
  ft.Name := GDefFontName;
  ft.Height := GDefFontHeight;
  For i := 0 To _BlokDosyasi.Count - 1 do
  begin
      If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerTabloTop) And
         (_BlokDosyasi[i].EntityID = idRectangle) And
         (TabloRec.InPoly(_BlokDosyasi[i].Geometry.Points[0])
         or TabloRec.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) then//_BlokDosyasi.GetPolyline(TabloRec).PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 Then
      begin
        Coor1 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[0];
        Coor2 := RectangletoPoly(_BlokDosyasi[i]).Geometry.Points[2];
          TabloCel := CellRectangle.Create(_BlokDosyasi[i].Name,
                                           '',
                                           ft,//New Asistan.CadObj.RFont('Arial', 12),
                                           DikeyHizalama.DikeyOrta,
                                           YatayHizalama.YatayOrta,
                                           LayerTabloTop,
                                           False,
                                           0,
                                           Coor1.Y,//.GetObject(i).p1.y,
                                           Coor1.X,//.GetObject(i).p1.x,
                                           Coor2.Y,//.GetObject(i).p2.y,
                                           Coor2.X,//.GetObject(i).p2.x,
                                           Olcek,//_BlokDosyasi.GetScale,
                                           RectangletoPoly(_BlokDosyasi[i]).Geometry.Points.Z[2],
                                           RectangletoPoly(_BlokDosyasi[i]));//.GetObject(i).p2.z)
//          _BlokDosyasi.GetObject(i).Tag = 0
          TabloCel.FillColor := TlicgBaseLayer(_BlokDosyasi[i].Layer).layerinfo.color;;
          TableTabloTopCell.CellRectangleList.Add(TabloCel);
      End;
  end;
end;

function Tbl.GetGet_TableTabloTopCell_FirstCount: Integer;
begin
  result := TableTabloTopCell_FirstCount;
end;

function Tbl.GetGet_DownCells: Cells;
begin
  result := TableTabloDownCell;
end;

function Tbl.GetGet_RowsCells: Rows;
begin
  result := TableTabloRows;
end;

function Tbl.GetGet_Tempow: Cells;
begin
  result := TableTabloTempRow;
end;

function Tbl.GetGet_TopCells: Cells;
begin
  result := TableTabloTopCell;
end;

function Tbl.GetLineCount: Double;
begin
  result := _LineCount;
end;

function Tbl.GetStil: Integer;
begin
  result := _Stil;
end;

procedure Tbl.reset;
var
  I: Integer;
  ath, ss: CellRectangle;
  NewRow: Cells;
begin
  For i := TableTabloTopCell.CellRectangleList.Count - 1 downTo TableTabloTopCell_FirstCount do
      TableTabloTopCell.CellRectangleList.Delete(i);// RemoveAt(i);

  For ath In TableTabloTopCell.CellRectangleList do
      ath.GoFirstCorr;

  For ath In TableTabloDownCell.CellRectangleList do
      ath.GoFirstCorr;

  TableTabloRows.CellsList.Clear;
  NewRow := Cells.Create;// New Tbl.Cells;
  For ss In TableTabloTempRow.CellRectangleList do
      NewRow.CellRectangleList.Add(ss.GetCopy);

  TableTabloRows.CellsList.Add(NewRow);
  TableTabloRows.Reset;
end;

procedure Tbl.SetLineCount(Value: Double);
begin
  _LineCount := Round(value);
end;

procedure Tbl.Setpage;
var
  TabloCel: CellRectangle;
  ft: IlicgFontTool;
  rowcount, Linecount, indexRow, i, ii, OuterPageIndex: Integer;
  OneRowHeight, pagebreak, TopCellHeight, CurrentTotalCellHeight, FirstTotalCellHeight, Rovheight, FirstRowHeigt, DownCellHeight: Double;
  DowncelClon, TopcelClon, RowsCel, nrow, nrow1: Cells;
  RowsCel1, cll, b, c: CellRectangle;
  scrol, Mes, pageheight, BolunenHucreYuksekligi, a: Double;
  Coor1, Coor2: TlicgCoor;
  TempStr: string;
  FlagFirst: Boolean;
//  DirectFactor: Integer;
  TempHeight: Double;
  SumWidth: Double;
  FirstSpace: Double;
  TempEnt: IlicgEntity;
  FirstTabCount, TempCount: Integer;
begin
//  SumWidth := 0;
  SumWidth := GTemplateWidth;
  FirstSpace := GFirstSpaceWidth;
//  DirectFactor := 1;

  FlagFirst := True;
//  ft := IlicgFontTool.Create;
  ft := Licad.Settings.FontTool;
  ft.Name := GDefFontName;
  ft.Height := GDefFontHeight;
//  'Tablolar kutusunu akýllý kutuya cevýrýyoruz
  Coor1 := RectangletoPoly(Self.TabloRec).Geometry.Points[0];
  Coor2 := RectangletoPoly(Self.TabloRec).Geometry.Points[2];
  TabloCel := CellRectangle.Create(GKUTU,
                                   '',
                                   //New Asistan.CadObj.RFont('Arial', 12),
                                   ft,
                                   DikeyHizalama.DikeyOrta,
                                   YatayHizalama.YatayOrta,
                                   LayerTabloRow,
                                   False,
                                   0,
                                   Coor1.Y,//Self.TabloRec.Geometry.Points[0].y, //p1
                                   Coor1.X,//Self.TabloRec.Geometry.Points[0].x,
                                   Coor2.Y,//Self.TabloRec.Geometry.Points[1].y, //p2
                                   Coor2.X,//Self.TabloRec.Geometry.Points[1].x,
                                   1,
                                   RectangletoPoly( Self.TabloRec).Geometry.Points.Z[2],
                                   RectangletoPoly(Self.TabloRec));

//  SumWidth := Abs(GTemplateWidth-TabloCel.Genislik);

  rowcount := 0;//As Integer;
  Linecount := _LineCount;
  indexRow := 0;// As Integer;
  OneRowHeight := 0;// As Double;
  pagebreak := 0;// As Double;
  TopCellHeight := 0;// As Double;
  TopcelClon := Cells.Create;// New Cells;
  DowncelClon := Cells.Create;// New Cells;
  CurrentTotalCellHeight := 0;// As Double;
  FirstTotalCellHeight := 0;// As Double;
  Rovheight := 0;// As Double;
  FirstRowHeigt := 0;//As Double;
  DownCellHeight := 0;//As Double;

//  ' üstbaþlýk kopyasý olusturuluyor
  For RowsCel1 In Self.Get_TopCells.CellRectangleList do
      TopcelClon.CellRectangleList.Add(RowsCel1);
  For RowsCel1 In Self.Get_DownCells.CellRectangleList do
  begin
    if RowsCel1.Adi = GRepaitDownTable then
      DowncelClon.CellRectangleList.Add(RowsCel1);
  end;

  TopCellHeight := Self.Get_TopCells.MaxHeight;
  OneRowHeight := Self.Get_Tempow.MaxHeight;
  FirstRowHeigt := Self.Get_RowsCells.CellsList[0].MaxHeight;
  DownCellHeight := Self.Get_DownCells.MaxHeight;

  if (Self.Stil = 5)
  and (Self.Get_RowsCells.CellsList.Count > Linecount) then
  begin
    Self.Get_RowsCells.CellsList.DeleteRange(Linecount, (Self.Get_RowsCells.CellsList.Count-Linecount));
  end;

  if (Self.Stil = 6)
  and ((Self.Get_RowsCells.CellsList.Count * OneRowHeight) > (TabloCel.Yukseklik)) then
  begin
    pageheight := 0;
    indexRow := 0;
    For RowsCel In Self.Get_RowsCells.CellsList do
    begin
      if (RowsCel.CellRectangleList <> nil) and (RowsCel.CellRectangleList.Count > 0) then
      begin
        pageheight := pageheight + RowsCel.CellRectangleList[(0)].Yukseklik;
        If ((TopCellHeight + pageheight) > TabloCel.Yukseklik) Then
        begin
          Self.Get_RowsCells.CellsList.DeleteRange(indexRow, (Self.Get_RowsCells.CellsList.Count-indexRow));
          Break;
        end;
      end;
      indexRow := indexRow + 1;
    end;
  end;

  For RowsCel In Self.Get_RowsCells.CellsList do
  begin
    if RowsCel.CellRectangleList.Count > 0 then
      FirstTotalCellHeight := FirstTotalCellHeight + RowsCel.CellRectangleList[0].Yukseklik;
  end;

//  '' Alt Satýrlarý alt alta kaydýrarak dizer
//  FirstrowNewHeight := Linecount * Self.Get_RowsCells.CellsList[0].MaxHeight;
//  for cll in Self.Get_DownCells.CellRectangleList do
//    cll.ScrollUpDown((OneRowHeight - FirstRowHeigt) - (OneRowHeight - FirstrowNewHeight));
//  'For cll As Asistan.CadObj.Tablo.CellRectangle In Me.Get_DownCells
//  '    cll.ScrollUpDown((OneRowHeight - FirstRowHeigt) - (OneRowHeight - FirstrowNewHeight))
//  'Next

//  ' Herþekilde tablonun içinden baþla (0) veya
//  ' tablonun dýsýndan baþla (1)
  If (Self.Stil = 0) or (Self.Stil = 5) or (Self.Stil = 6) then
  begin
    For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If _LineCount > 0 Then
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                  begin
                      If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;
                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                  end
                  Else
                  begin
                      If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;
//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          Rovheight := Rovheight + RowsCel.CellRectangleList[0].Yukseklik;
           For cll In RowsCel.CellRectangleList do
            cll.ScrollUpDown(Rovheight - OneRowHeight); //OneRowHeight
      end;

      For cll In Self.Get_DownCells.CellRectangleList do
        cll.ScrollUpDown((TopCellHeight + FirstTotalCellHeight));
  end
  else if (Self.Stil = 1) then
  begin
//      ' Tüm Hücereler Sýfýr Noktasýn da Ve Ýçerisinde Birden Fazla
//      ' Satýr Olan Hucreleri Boler. Bölünmüþ olarak Sýfýrnoktasýnda ust uste ekler
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If _LineCount > 0 Then
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                  begin
                      If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;
                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                  end
                  Else
                  begin
                      If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;
//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          Rovheight := Rovheight + RowsCel.CellRectangleList[0].Yukseklik;
          For cll In RowsCel.CellRectangleList do
              cll.ScrollUpDown(Rovheight - OneRowHeight);
      end;
      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if RowsCel.Get_MaxTextCount_in_One_Cel = 0 then
          RowsCel._TextCount := 1;
          If _LineCount > 0 Then
          begin
              rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
              If (Linecount Mod rowcount) = Linecount Then //Girmiyor1
              begin
                pagebreak := pagebreak + TopCellHeight + OneRowHeight;

//                For c In Self.Get_DownCells.CellRectangleList do
                for I := 0 to Self.Get_DownCells.CellRectangleList.Count - 1 do
                begin
                  c := Self.Get_DownCells.CellRectangleList[I];
                  TempStr := c.Adi;
                  if TempStr.Equals(GRepaitDownTable) then
                  begin
                    if FlagFirst then
                    begin
                      c.ScrollUpDown((TopCellHeight + CurrentTotalCellHeight));
                      if I = Self.Get_DownCells.CellRectangleList.Count - 1 then
                        FlagFirst := False;
                    end;
                    b := c.GetCopy;

                    if (Self.Get_RowsCells.CellsList.Count - LineCount) <= _LineCount then
                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight
                      - ((_LineCount-(Self.Get_RowsCells.CellsList.Count - LineCount))*OneRowHeight)))
                    else
                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight)); //-(TopCellHeight)
                    Self.Get_TopCells.CellRectangleList.Add(b);
//                    Self.Get_DownCells.CellRectangleList.Add(b);
                  end;
                end;

//                  ' üst bilgi kaydýrýlýyor
                  For cll In TopcelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight));
                      Self.Get_TopCells.CellRectangleList.Add(b);
                  end;
//                  ' ilgili satýrlarýn tumu aþaðý kaydýrýlýyor
                  For i := indexRow To Self.Get_RowsCells.CellsList.Count - 1 do
                  begin
                      For ii := 0 To Self.Get_RowsCells.CellsList[i].CellRectangleList.Count - 1 do
                          Self.Get_RowsCells.CellsList[i].CellRectangleList[ii].
						              ScrollUpDown((TopCellHeight + OneRowHeight));
                  end;
                  Linecount := Linecount + _LineCount;
              End;
          End;
//          'Toplam Satýr Yüksekliði
          CurrentTotalCellHeight := CurrentTotalCellHeight + RowsCel.CellRectangleList[0].Yukseklik;
          indexRow := indexRow + 1;
      end;

      scrol := 0;
//      If (Self.Stil = 1) Then
//          scrol := TabloCel.Yukseklik * 2;

      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          For cll In RowsCel.CellRectangleList do
              cll.ScrollUpDown(scrol);
      end;
      For cll In Self.Get_TopCells.CellRectangleList do
          cll.ScrollUpDown(scrol);

    Mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
                  Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.Y,
                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.X);

      {For cll In Self.Get_DownCells.CellRectangleList do
      begin
        TempStr := cll.Adi;
        if FlagFirst then //not TempStr.Equals(GDownTableRepait) or
        begin
          cll.ScrollUpDown((TopCellHeight + FirstTotalCellHeight));
        end;
//            cll.ScrollUpDown(scrol + -(Mes - FirstTotalCellHeight));
//          ' cll.ScrollUpDown(TabloCel.Yukseklik + TabloCel.Yukseklik + (Mes - FirstTotalCellHeight))
      end;}

      For cll In Self.Get_DownCells.CellRectangleList do
      begin
        TempStr := cll.Adi;
        if (not TempStr.Equals(GRepaitDownTable))
        or ((TempStr.Equals(GRepaitDownTable)) and (Self.Get_RowsCells.CellsList.Count <= _LineCount)) then
        begin
          cll.ScrollUpDown(Mes+OneRowHeight+TopCellHeight);
        end;
      end;

  end
  Else If Self.Stil = 2 Then //' Önce Tablonun Ýçine Sonra Dýþýna
  begin
//      ' Tüm Hücereler Sýfýr Noktasýn da Ve Ýçerisinde Birden Fazla
//      ' Satýr Olan Hucreleri Boler. Bölünmüþ olarak Sýfýrnoktasýnda ust uste ekler
      pageheight := 0;
      OuterPageIndex := -1;
      BolunenHucreYuksekligi := 0;
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if (RowsCel.CellRectangleList <> nil) and (RowsCel.CellRectangleList.Count > 0) then
          pageheight := pageheight + RowsCel.CellRectangleList[(0)].Yukseklik;
          If ((TopCellHeight + pageheight) > TabloCel.Yukseklik) And (OuterPageIndex = -1) Then
          begin
              pageheight := pageheight - RowsCel.CellRectangleList[(0)].Yukseklik;
              If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
              begin
                  a := TopCellHeight + pageheight + RowsCel.GetCloneFirst[(0)].CellRectangleList[(0)].Yukseklik;
                  If a > TabloCel.Yukseklik Then
                  begin
                      OuterPageIndex := indexRow - 1;
                      rowcount := 0;
                      Linecount := _LineCount;
                  end
                  Else
                  begin
                      ii := 1;
                      For i := (RowsCel.Get_MaxTextCount_in_One_Cel - 1) downTo 0 do
                      begin
                          a := TopCellHeight + pageheight + RowsCel.GetCloneFirst[(i)].CellRectangleList[(0)].Yukseklik; //'* ii;
                          If a < TabloCel.Yukseklik Then
                          begin
                              nrow1 := RowsCel.GetCloneLast[i, True];
                              pageheight := pageheight + RowsCel.CellRectangleList[(0)].Yukseklik;
                              Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow1);
                              OuterPageIndex := indexRow;
                              rowcount := 0;
                              Linecount := _LineCount;
                              Break;//Exit For;
                          End;
                          ii := ii + 1;
                      end;
                  End;
              end
              Else If RowsCel.Get_MaxTextCount_in_One_Cel = 1 Then
              begin
                  OuterPageIndex := indexRow - 1;
                  rowcount := 0;
                  Linecount := _LineCount;
              End;
          end
          Else
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                  begin

                      If (RowsCel.Get_MaxTextCount_in_One_Cel > 1) And (OuterPageIndex > -1) Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                      If (OuterPageIndex = -1) And (TopCellHeight + pageheight > TabloCel.Yukseklik) Then
                        OuterPageIndex := indexRow;
                  end
                  Else
                  begin
                      If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;

//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if (RowsCel.CellRectangleList <> nil) and (RowsCel.CellRectangleList.Count > 0) then
        begin
          Rovheight := Rovheight + RowsCel.CellRectangleList[(0)].Yukseklik;
          For cll In RowsCel.CellRectangleList do
          begin
              cll.ScrollUpDown(Rovheight - OneRowHeight);
//              CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(cll.GetCorners);
          end;
        end;
      end;
//      ' sayfa içerisinde kalacak olan hucrelerýn toplam yuksekliði
//      If OuterPageIndex > -1 Then
//      begin
//          BolunenHucreYuksekligi := Mesafe(Self.Get_RowsCells.CellsList[(OuterPageIndex)].CellRectangleList[(0)].SolAlt.Y,
//                                           Self.Get_RowsCells.CellsList[(OuterPageIndex)].CellRectangleList[(0)].SolAlt.X,
//                                           Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.Y,
//                                           Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.X);
//      End;

      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;

//      ' üst bilgiden bir adet daha ekleniyor
      If OuterPageIndex > -1 Then
      begin
          For cll In TopcelClon.CellRectangleList do
          begin
              b := cll.GetCopy;
              Self.Get_TopCells.CellRectangleList.Add(b);
          end;
//      ' alt bilgiden bir adet daha ekleniyor
          For cll In DowncelClon.CellRectangleList do
          begin
              b := cll.GetCopy;
              Self.Get_DownCells.CellRectangleList.Add(b);
          end;
      End;
      //Tablodan sonra Satýr Boþluklarý Ekler.
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If (_LineCount > 0) And (indexRow > OuterPageIndex) Then
          begin
              rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;

              If (rowcount <> 0) and ((Linecount Mod rowcount) = Linecount) Then
              begin
                  pagebreak := pagebreak + TopCellHeight + OneRowHeight;
//                  ' üst bilgi kaydýrýlýyor
                  For cll In TopcelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight));
                      Self.Get_TopCells.CellRectangleList.Add(b);
                  end;
//                  ' alt bilgi kaydýrýlýyor
                  For cll In DowncelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight));
                      Self.Get_DownCells.CellRectangleList.Add(b);
                  end;
//                  ' ilgili satýrlarýn tumu aþaðý kaydýrýlýyor
                  For i := indexRow To Self.Get_RowsCells.CellsList.Count - 1 do
                  begin
                      For ii := 0 To Self.Get_RowsCells.CellsList[i].CellRectangleList.Count - 1 do
                      begin
                          Self.Get_RowsCells.CellsList[(i)].CellRectangleList[(ii)]
                          .ScrollUpDown((TopCellHeight + OneRowHeight));
                      end;
                  end;
                  Linecount := Linecount + _LineCount;
              End;
          End;
//          'Toplam Satýr Yüksekliði
          CurrentTotalCellHeight := CurrentTotalCellHeight + RowsCel.CellRectangleList[(0)].Yukseklik;
          indexRow := indexRow + 1;
      end;
      //Tablo ile Arasýna Boþluk Ekler.
      indexRow := 0;
      TempHeight := (_LineCount*OneRowHeight)-((OuterPageIndex+1)*OneRowHeight);//+(TopCellHeight); //TabloCel.Yukseklik
      If (OuterPageIndex > -1) Then
      begin
          For RowsCel In Self.Get_RowsCells.CellsList do
          begin
              If indexRow > OuterPageIndex Then
              begin
                  For cll In RowsCel.CellRectangleList do
                      cll.ScrollUpDown(TempHeight+TopCellHeight);
//                      Abs(TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi));
              End;
              indexRow := indexRow + 1;
          end;
      End;

      indexRow := 0;
      If OuterPageIndex > -1 Then
      begin
          For cll In Self.Get_TopCells.CellRectangleList do
          begin
              If (indexRow >= (TopcelClon.CellRectangleList.Count * 2)) Then
                  cll.ScrollUpDown(TempHeight+TopCellHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi))
              Else If (indexRow >= TopcelClon.CellRectangleList.Count) Then
                  cll.ScrollUpDown((_LineCount*OneRowHeight)+TopCellHeight); //((OuterPageIndex+1)*OneRowHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik));

              indexRow := indexRow + 1;
          end;
      End;

      indexRow := 0;
      If OuterPageIndex > -1 Then
      begin
          For cll In Self.Get_DownCells.CellRectangleList do
          begin
            //(indexRow >= (Self.Get_DownCells.CellRectangleList.Count-DowncelClon.CellRectangleList.Count))
            if cll.Adi = GRepaitDownTable then
            begin
			        If (indexRow >= (DowncelClon.CellRectangleList.Count * 2)) Then
              begin
                if (indexRow >= (Self.Get_DownCells.CellRectangleList.Count-DowncelClon.CellRectangleList.Count)) then
                  cll.ScrollUpDown((TempHeight+TopCellHeight)-((OuterPageIndex+1)*OneRowHeight)
                  +(((Self.Get_RowsCells.CellsList.Count-OuterPageIndex-1) mod _LineCount)*OneRowHeight))
                else
                  cll.ScrollUpDown((TempHeight+TopCellHeight)-((OuterPageIndex+1)*OneRowHeight)+(_LineCount*OneRowHeight));
              end
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi))
              Else If (indexRow >= DowncelClon.CellRectangleList.Count) Then
                  cll.ScrollUpDown((TempHeight+TopCellHeight)+(_LineCount*OneRowHeight)); //((OuterPageIndex+1)*OneRowHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik));

              indexRow := indexRow + 1;
            end;
          end;
      End;

//      mes := 0;
//      If OuterPageIndex > -1 Then
//      begin
//          mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
//                        Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
//                        Self.Get_RowsCells.CellsList[(OuterPageIndex + 1)].CellRectangleList[(0)].SolUst.Y,
//                        Self.Get_RowsCells.CellsList[(OuterPageIndex + 1)].CellRectangleList[(0)].SolUst.X);

          mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
                        Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
                        Self.Get_RowsCells.CellsList[0].CellRectangleList[(0)].SolAlt.Y,
                        Self.Get_RowsCells.CellsList[0].CellRectangleList[(0)].SolAlt.X);
//      End;

      If OuterPageIndex > -1 Then
      begin
          For cll In Self.Get_DownCells.CellRectangleList do
          begin
            if cll.Adi = GRepaitDownTable then
              cll.ScrollUpDown(((OuterPageIndex+1)*OneRowHeight)+TopCellHeight);
              //(OuterPageIndex+1)*OneRowHeight+TopCellHeight);//Abs((_LineCount*OneRowHeight)+TopCellHeight));
//              ' cll.ScrollUpDown(TabloCel.Yukseklik + TabloCel.Yukseklik + (Mes - FirstTotalCellHeight))
          end;
      End;

      For cll In Self.Get_DownCells.CellRectangleList do
      begin
        if (cll.Adi <> GRepaitDownTable)
        or ((cll.Adi.Equals(GRepaitDownTable)) and (Self.Get_RowsCells.CellsList.Count <= _LineCount)) then
          cll.ScrollUpDown(Mes+OneRowHeight+TopCellHeight);
      end;
  end
  else if (Self.Stil = 3) Then //Saða
  begin
//      ' Tüm Hücereler Sýfýr Noktasýn da Ve Ýçerisinde Birden Fazla
//      ' Satýr Olan Hucreleri Boler. Bölünmüþ olarak Sýfýrnoktasýnda ust uste ekler
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If _LineCount > 0 Then
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                  begin
                      If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;
                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                  end
                  Else
                  begin
                      If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;
//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          Rovheight := Rovheight + RowsCel.CellRectangleList[0].Yukseklik;
          For cll In RowsCel.CellRectangleList do
              cll.ScrollUpDown(Rovheight - OneRowHeight);
      end;

      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;

      FirstTabCount := 0;
      if (Self.Get_RowsCells.CellsList.Count >= _LineCount) then
        FirstTabCount := _LineCount
      else
        FirstTabCount := Self.Get_RowsCells.CellsList.Count;

      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;
      TempCount := 0;
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if RowsCel.Get_MaxTextCount_in_One_Cel = 0 then
          RowsCel._TextCount := 1;
          If _LineCount > 0 Then
          begin
              if ((indexRow+1) = (FirstTabCount)) then
              begin
                  for I := 0 to Self.Get_DownCells.CellRectangleList.Count - 1 do
                  begin
                    c := Self.Get_DownCells.CellRectangleList[I];
                    TempStr := c.Adi;
                    if TempStr.Equals(GRepaitDownTable) then
                    begin
                      if FlagFirst then
                      begin
  //                      c.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth));
                        c.ScrollUpDown((TopCellHeight + CurrentTotalCellHeight + RowsCel.CellRectangleList[0].Yukseklik));
                        TempCount := TempCount + 1;
                        if TempCount = DowncelClon.CellRectangleList.Count then
                          FlagFirst := False;
                      end
                      else
                        Break;
                    end;
                  end;
              end;
              rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
              If (Linecount Mod rowcount) = Linecount Then //Girmiyor1
              begin
                pagebreak := pagebreak + TopCellHeight + OneRowHeight;
//                For c In Self.Get_DownCells.CellRectangleList do
                for I := 0 to Self.Get_DownCells.CellRectangleList.Count - 1 do
                begin
                  c := Self.Get_DownCells.CellRectangleList[I];
                  TempStr := c.Adi;
                  if TempStr.Equals(GRepaitDownTable) then
                  begin
                    {if FlagFirst then
                    begin
//                      c.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth));
                      c.ScrollUpDown((TopCellHeight + CurrentTotalCellHeight));
                      if I = Self.Get_DownCells.CellRectangleList.Count - 1 then
                        FlagFirst := False;
                    end;}
                    b := c.GetCopy;

                    if (Self.Get_RowsCells.CellsList.Count - LineCount) <= _LineCount then
                    begin
                      b.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth)); //-(TopCellHeight)
                      b.ScrollUpDown((-1*((_LineCount-(Self.Get_RowsCells.CellsList.Count - LineCount))*RowsCel.CellRectangleList[0].Yukseklik)));
//                      - ((_LineCount-(Self.Get_RowsCells.CellsList.Count - LineCount))*OneRowHeight)))
                    end
                    else
                      b.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth)); //-(TopCellHeight)
                    Self.Get_TopCells.CellRectangleList.Add(b);
//                    Self.Get_DownCells.CellRectangleList.Add(b);
                  end;
                end;

//                  ' üst bilgi kaydýrýlýyor
                  For cll In TopcelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
                      b.GoFirstCorr;
                      b.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth));//(TopcelClon.Width));

//                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight));
                      Self.Get_TopCells.CellRectangleList.Add(b);
                  end;

//                  ' ilgili satýrlarýn tumu aþaðý kaydýrýlýyor
                Rovheight := 0;
                  For i := indexRow To Self.Get_RowsCells.CellsList.Count - 1 do
                  begin
                      For ii := 0 To Self.Get_RowsCells.CellsList[i].CellRectangleList.Count - 1 do
                      begin
                        //+(GDefFontHeight/2)
                        Self.Get_RowsCells.CellsList[i].CellRectangleList[ii].GoFirstCorr;
                        Self.Get_RowsCells.CellsList[i].CellRectangleList[ii]
                        .ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik+SumWidth));
                        //(Self.Get_RowsCells.CellsList[i].Width)) //Self.Get_RowsCells.CellsList[0].Width 65
                        //(LineCount/_LineCount)*
//                          .ScrollUpDown(((-Self.Get_RowsCells.CellsList[(i-Linecount)].CellRectangleList[ii].SolAlt.Y+
//                                          Self.Get_RowsCells.CellsList[i].CellRectangleList[ii].SolAlt.Y))); //pagebreak + CurrentTotalCellHeight
//                          (pagebreak + CurrentTotalCellHeight - ((_LineCount-(Self.Get_RowsCells.CellsList.Count - LineCount))*OneRowHeight))*-1);
//                          .ScrollLeftReight((TopCellHeight + OneRowHeight));
                      end;
                      Rovheight := Rovheight + Self.Get_RowsCells.CellsList[i].CellRectangleList[0].Yukseklik;
                      For cll In Self.Get_RowsCells.CellsList[i].CellRectangleList do
                        cll.ScrollUpDown(Rovheight - OneRowHeight);
//						              ScrollUpDown((TopCellHeight + OneRowHeight));
                  end;
                  Linecount := Linecount + _LineCount;
              End;
          End;
//          'Toplam Satýr Yüksekliði
          CurrentTotalCellHeight := CurrentTotalCellHeight + RowsCel.CellRectangleList[0].Yukseklik;
          indexRow := indexRow + 1;
      end;

      scrol := 0;
//      If (Self.Stil = 1) Then
//          scrol := TabloCel.Yukseklik * 2;

      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          For cll In RowsCel.CellRectangleList do
              cll.ScrollUpDown(scrol);
      end;
      For cll In Self.Get_TopCells.CellRectangleList do
          cll.ScrollUpDown(scrol);

//    Mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
//                  Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
//                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.Y,
//                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.X);

      {For cll In Self.Get_DownCells.CellRectangleList do
      begin
        if cll.Adi = GRepaitDownTable then //not TempStr.Equals(GDownTableRepait) or
        begin
          cll.ScrollUpDown((TopCellHeight + FirstTotalCellHeight));
        end;
//            cll.ScrollUpDown(scrol + -(Mes - FirstTotalCellHeight));
//          ' cll.ScrollUpDown(TabloCel.Yukseklik + TabloCel.Yukseklik + (Mes - FirstTotalCellHeight))
      end;}

      Mes := Abs(Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.Y-Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y);
      Scrol := Abs(Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.X-Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X);

      For cll In Self.Get_DownCells.CellRectangleList do
      begin
        if cll.Adi <> GRepaitDownTable then
        begin
          cll.ScrollLeftReight(Scrol);
          cll.ScrollUpDown(Mes+OneRowHeight+TopCellHeight);
        end;
      end;

      //Top Row Down Ötele FirstSpace Kadar
      if FirstSpace <> 0 then
      begin
        For I := TopcelClon.CellRectangleList.Count to Self.Get_TopCells.CellRectangleList.Count - 1 do
        begin
          cll := Self.Get_TopCells.CellRectangleList[I];
          cll.ScrollLeftReight(FirstSpace);
        end;
        For I := _LineCount to Self.Get_RowsCells.CellsList.Count - 1 do
        begin
          RowsCel := Self.Get_RowsCells.CellsList[I];
          For cll In RowsCel.CellRectangleList do
            cll.ScrollLeftReight(FirstSpace);
        end;
        For I := DowncelClon.CellRectangleList.Count to Self.Get_DownCells.CellRectangleList.Count - 1 do
        begin
          cll := Self.Get_DownCells.CellRectangleList[I];
          cll.ScrollLeftReight(FirstSpace);
        end;
      end;
  end
  Else If Self.Stil = 4 Then //' Önce Tablonun Ýçine Sonra Dýþýna
  begin
//      ' Tüm Hücereler Sýfýr Noktasýn da Ve Ýçerisinde Birden Fazla
//      ' Satýr Olan Hucreleri Boler. Bölünmüþ olarak Sýfýrnoktasýnda ust uste ekler
      pageheight := 0;
      OuterPageIndex := -1;
      BolunenHucreYuksekligi := 0;
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if (RowsCel.CellRectangleList <> nil) and (RowsCel.CellRectangleList.Count > 0) then
          pageheight := pageheight + RowsCel.CellRectangleList[(0)].Yukseklik;
          If ((TopCellHeight + pageheight) > TabloCel.Yukseklik) And (OuterPageIndex = -1) Then
          begin
              pageheight := pageheight - RowsCel.CellRectangleList[(0)].Yukseklik;
              If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
              begin
                  a := TopCellHeight + pageheight + RowsCel.GetCloneFirst[(0)].CellRectangleList[(0)].Yukseklik;
                  If a > TabloCel.Yukseklik Then
                  begin
                      OuterPageIndex := indexRow - 1;
                      rowcount := 0;
                      Linecount := _LineCount;
                  end
                  Else
                  begin
                      ii := 1;
                      For i := (RowsCel.Get_MaxTextCount_in_One_Cel - 1) downTo 0 do
                      begin
                          a := TopCellHeight + pageheight + RowsCel.GetCloneFirst[(i)].CellRectangleList[(0)].Yukseklik; //'* ii;
                          If a < TabloCel.Yukseklik Then
                          begin
                              nrow1 := RowsCel.GetCloneLast[i, True];
                              pageheight := pageheight + RowsCel.CellRectangleList[(0)].Yukseklik;
                              Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow1);
                              OuterPageIndex := indexRow;
                              rowcount := 0;
                              Linecount := _LineCount;
                              Break;//Exit For;
                          End;
                          ii := ii + 1;
                      end;
                  End;
              end
              Else If RowsCel.Get_MaxTextCount_in_One_Cel = 1 Then
              begin
                  OuterPageIndex := indexRow - 1;
                  rowcount := 0;
                  Linecount := _LineCount;
              End;
          end
          Else
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                  begin

                      If (RowsCel.Get_MaxTextCount_in_One_Cel > 1) And (OuterPageIndex > -1) Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                      If (OuterPageIndex = -1) And (TopCellHeight + pageheight > TabloCel.Yukseklik) Then
                        OuterPageIndex := indexRow;
                  end
                  Else
                  begin
                      If (Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel)) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;

      indexRow := 0;
//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
        if (RowsCel.CellRectangleList <> nil) and (RowsCel.CellRectangleList.Count > 0) then
        begin
//          if (indexRow = (OuterPageIndex+1)) then
//            Rovheight := 0
          if (((indexRow-(OuterPageIndex+1)) mod (_LineCount)) = 0) then //OuterPageIndex
            Rovheight := 0;
          Rovheight := Rovheight + RowsCel.CellRectangleList[(0)].Yukseklik;
          For cll In RowsCel.CellRectangleList do
          begin
              cll.ScrollUpDown(Rovheight - OneRowHeight);
//              CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(cll.GetCorners);
          end;
        end;
        indexRow := indexRow + 1;
      end;
//      ' sayfa içerisinde kalacak olan hucrelerýn toplam yuksekliði
//      If OuterPageIndex > -1 Then
//      begin
//          BolunenHucreYuksekligi := Mesafe(Self.Get_RowsCells.CellsList[(OuterPageIndex)].CellRectangleList[(0)].SolAlt.Y,
//                                           Self.Get_RowsCells.CellsList[(OuterPageIndex)].CellRectangleList[(0)].SolAlt.X,
//                                           Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.Y,
//                                           Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.X);
//      End;

      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;

//      ' üst bilgiden bir adet daha ekleniyor
      If OuterPageIndex > -1 Then
      begin
          For cll In TopcelClon.CellRectangleList do
          begin
              b := cll.GetCopy;
              Self.Get_TopCells.CellRectangleList.Add(b);
          end;

//      ' alt bilgiden bir adet daha ekleniyor
        For cll In DowncelClon.CellRectangleList do
        begin
            b := cll.GetCopy;
            Self.Get_DownCells.CellRectangleList.Add(b);
        end;
      end;
      //Tablodan sonra Satýr Boþluklarý Ekler.
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If (_LineCount > 0) And (indexRow > OuterPageIndex) Then
          begin
              rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;

              If (rowcount <> 0) and ((Linecount Mod rowcount) = Linecount) Then
              begin
                  pagebreak := pagebreak + TopCellHeight + OneRowHeight;
//                  ' üst bilgi kaydýrýlýyor
                  For cll In TopcelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
                      b.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik)+SumWidth);
//                      b.ScrollUpDown((pagebreak + CurrentTotalCellHeight));
                      Self.Get_TopCells.CellRectangleList.Add(b);
                  end;
//                  ' alt bilgi kaydýrýlýyor
                  For cll In DowncelClon.CellRectangleList do
                  begin
                      b := cll.GetCopy;
//                      b.ScrollLeftReight((LineCount/_LineCount)*(TabloCel.Genislik)+SumWidth);
                      Self.Get_DownCells.CellRectangleList.Add(b);
                  end;
//                  ' ilgili satýrlarýn tumu aþaðý kaydýrýlýyor
                  For i := indexRow To Self.Get_RowsCells.CellsList.Count - 1 do
                  begin
                      For ii := 0 To Self.Get_RowsCells.CellsList[i].CellRectangleList.Count - 1 do
                      begin
                          Self.Get_RowsCells.CellsList[(i)].CellRectangleList[(ii)]
                          .ScrollLeftReight((TabloCel.Genislik)+SumWidth);
//                          .ScrollUpDown((TopCellHeight + OneRowHeight));
                      end;
                  end;
                  Linecount := Linecount + _LineCount;
              End;
          End;
//          'Toplam Satýr Yüksekliði
          CurrentTotalCellHeight := CurrentTotalCellHeight + RowsCel.CellRectangleList[(0)].Yukseklik;
          indexRow := indexRow + 1;
      end;
      //Tablo ile Arasýna Boþluk Ekler.
      indexRow := 0;
//      TempHeight := 0;
//      TempHeight := (_LineCount*OneRowHeight)-((OuterPageIndex+1)*OneRowHeight);//+(TopCellHeight); //TabloCel.Yukseklik
      If (OuterPageIndex > -1) Then
      begin
          For RowsCel In Self.Get_RowsCells.CellsList do
          begin
              If indexRow > OuterPageIndex Then
              begin
//                TempHeight := -( Trunc((((indexRow-OuterPageIndex)+1)/_LineCount)+1) * ((OuterPageIndex+1)*OneRowHeight));
                  For cll In RowsCel.CellRectangleList do
                  begin
                    cll.ScrollLeftReight((TabloCel.Genislik)+SumWidth);
//                    cll.ScrollUpDown(TempHeight);
                  end;
//                      cll.ScrollUpDown(TempHeight+TopCellHeight);
//                      Abs(TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi));
              End;
              indexRow := indexRow + 1;
          end;
      End;

      indexRow := 0;
      If OuterPageIndex > -1 Then
      begin
          For cll In Self.Get_TopCells.CellRectangleList do
          begin
              If (indexRow >= (TopcelClon.CellRectangleList.Count * 2)) Then
              begin
//                cll.GoFirstCorr;
                cll.ScrollLeftReight((TabloCel.Genislik)+SumWidth+((SumWidth)*(Trunc(indexRow/TopcelClon.CellRectangleList.Count)-2)));
              end
//                  cll.ScrollUpDown(TempHeight+TopCellHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi))
              Else If (indexRow >= TopcelClon.CellRectangleList.Count) Then
              begin
//                cll.GoFirstCorr;
                cll.ScrollLeftReight((TabloCel.Genislik)+SumWidth);
              end;
//                  cll.ScrollUpDown((_LineCount*OneRowHeight)+TopCellHeight); //((OuterPageIndex+1)*OneRowHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik));

              indexRow := indexRow + 1;
          end;
      End;

      indexRow := 0;
      If OuterPageIndex > -1 Then
      begin
          For cll In Self.Get_DownCells.CellRectangleList do
          begin
            //(indexRow >= (Self.Get_DownCells.CellRectangleList.Count-DowncelClon.CellRectangleList.Count))
            if cll.Adi = GRepaitDownTable then
            begin
			        If (indexRow >= (DowncelClon.CellRectangleList.Count)) Then // * 2
              begin
                if (indexRow >= (Self.Get_DownCells.CellRectangleList.Count-DowncelClon.CellRectangleList.Count)) then
                begin
                  cll.ScrollUpDown(-((_LineCount)*OneRowHeight)
//                  -((OuterPageIndex+1)*OneRowHeight)
                  +(((Self.Get_RowsCells.CellsList.Count-OuterPageIndex-1) mod _LineCount)*OneRowHeight));
                  cll.ScrollLeftReight(Trunc(indexRow/DowncelClon.CellRectangleList.Count)*(TabloCel.Genislik+SumWidth));
                end
                else
                  cll.ScrollLeftReight(Trunc(indexRow/DowncelClon.CellRectangleList.Count)*(TabloCel.Genislik+SumWidth));
//                  cll.ScrollUpDown((TempHeight+TopCellHeight)-((OuterPageIndex+1)*OneRowHeight)+(_LineCount*OneRowHeight));
              end
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik - BolunenHucreYuksekligi))
              Else If (indexRow >= DowncelClon.CellRectangleList.Count) Then
                  cll.ScrollUpDown(TabloCel.Yukseklik);
//                    ScrollLeftReight(Trunc(indexRow/DowncelClon.CellRectangleList.Count)*(TabloCel.Genislik+SumWidth));
                  //(TempHeight+TopCellHeight)+(_LineCount*OneRowHeight)); //((OuterPageIndex+1)*OneRowHeight)
//                  (TabloCel.Yukseklik + TabloCel.Yukseklik));

              indexRow := indexRow + 1;
            end;
          end;
      End;
//      mes := 0;
//      If OuterPageIndex > -1 Then
//      begin
//          mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
//                        Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
//                        Self.Get_RowsCells.CellsList[(OuterPageIndex + 1)].CellRectangleList[(0)].SolUst.Y,
//                        Self.Get_RowsCells.CellsList[(OuterPageIndex + 1)].CellRectangleList[(0)].SolUst.X);

            mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
                          Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
                          Self.Get_RowsCells.CellsList[0].CellRectangleList[(0)].SolAlt.Y,
                          Self.Get_RowsCells.CellsList[0].CellRectangleList[(0)].SolAlt.X);

//      End;
      indexRow := 0;
//      If OuterPageIndex > -1 Then
//      begin
          For cll In Self.Get_DownCells.CellRectangleList do
          begin
            if cll.Adi = GRepaitDownTable then
            begin
              If (indexRow >= (DowncelClon.CellRectangleList.Count)) Then
              begin
                cll.ScrollUpDown((_LineCount*OneRowHeight)+TopCellHeight);//((OuterPageIndex+1)*OneRowHeight)+TopCellHeight);
//                cll.ScrollLeftReight((TabloCel.Genislik+SumWidth));
                //(OuterPageIndex+1)*OneRowHeight+TopCellHeight);//Abs((_LineCount*OneRowHeight)+TopCellHeight));
  //              ' cll.ScrollUpDown(TabloCel.Yukseklik + TabloCel.Yukseklik + (Mes - FirstTotalCellHeight))
  //              cll.ScrollUpDown((TabloCel.Yukseklik + TabloCel.Yukseklik + (mes - FirstTotalCellHeight)));
              end
              else If (OuterPageIndex > -1) Then
                cll.ScrollUpDown(TopCellHeight+((OuterPageIndex+1)*OneRowHeight))//TabloCel.Yukseklik)
              else
                cll.ScrollUpDown(TopCellHeight+(CurrentTotalCellHeight));

              indexRow := indexRow + 1;
            end;
          end;
//      End;

      Mes := Abs(Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.Y-Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y);
      Scrol := Abs(Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolAlt.X-Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X);

      For cll In Self.Get_DownCells.CellRectangleList do
      begin
        if cll.Adi <> GRepaitDownTable then
        begin
          cll.ScrollLeftReight(Scrol);
          cll.ScrollUpDown(Mes+OneRowHeight+TopCellHeight);
        end;
      end;

      //Top Row Down Ötele FirstSpace Kadar
      if FirstSpace <> 0 then
      begin
        For I := TopcelClon.CellRectangleList.Count to Self.Get_TopCells.CellRectangleList.Count - 1 do
        begin
          cll := Self.Get_TopCells.CellRectangleList[I];
          cll.ScrollLeftReight(FirstSpace);
        end;
        if (OuterPageIndex > -1) then
        For I := (OuterPageIndex+1) to Self.Get_RowsCells.CellsList.Count - 1 do
        begin
          RowsCel := Self.Get_RowsCells.CellsList[I];
          For cll In RowsCel.CellRectangleList do
            cll.ScrollLeftReight(FirstSpace);
        end;
        For I := DowncelClon.CellRectangleList.Count to Self.Get_DownCells.CellRectangleList.Count - 1 do
        begin
          cll := Self.Get_DownCells.CellRectangleList[I];
          cll.ScrollLeftReight(FirstSpace);
        end;
      end;
  end;
//        TempEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon,0,_2D);

      {TempEnt.Geometry.Points.Add(
        AsCoor(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SagUst.X,
               Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SagUst.Y));

      TempEnt.Geometry.Points.Add(
        AsCoor(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SagAlt.X,
               Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SagAlt.Y));

      TempEnt.Geometry.Points.Add(
        AsCoor(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList
             [(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList.Count - 1)].SolAlt.X,
               Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList
             [(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList.Count - 1)].SolAlt.Y));

      TempEnt.Geometry.Points.Add(
        AsCoor(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList
             [(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList.Count - 1)].SolUst.X,
               Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList
             [(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList.Count - 1)].SolUst.Y));}

      {for I := 0 to Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList.Count - 1 do
        TempEnt.Geometry.Points.Append(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(I)].GetCorners.Geometry.Points);
      CurrCmdline.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TempEnt);
      TempEnt.Geometry.Points.Clear;}

//      for I := 0 to Self.Get_RowsCells.CellsList[0].CellRectangleList.Count - 1 do
//        TempEnt.Geometry.Points.Append(Self.Get_RowsCells.CellsList[0].CellRectangleList[(I)].GetCorners.Geometry.Points);
//      CurrCmdline.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TempEnt);
  {
  // Ýçine Sýðdýðý kadar kaydýrmadan.
  Else if Self.Stil = 3 then
  begin
//    for I := Linecount to Self.Get_RowsCells.CellsList.Count - 1 do
//    begin
//      Self.Get_RowsCells.CellsList.Delete(I);
//    end;

    //      ' Tüm Hücereler Sýfýr Noktasýn da Ve Ýçerisinde Birden Fazla
//      ' Satýr Olan Hucreleri Boler. Bölünmüþ olarak Sýfýrnoktasýnda ust uste ekler
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          If _LineCount > 0 Then
          begin
              If RowsCel.Get_MaxTextCount_in_One_Cel >= 1 Then
              begin

                  If Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel) = Linecount Then
                  begin
                      If RowsCel.Get_MaxTextCount_in_One_Cel > 1 Then
                      begin
                          nrow := RowsCel.GetCloneLast[Linecount - (rowcount), True];
                          Self.Get_RowsCells.CellsList.Insert(indexRow + 1, nrow);
                      End;
                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                      Linecount := Linecount + _LineCount;
                  end
                  Else
                  begin
                      If Linecount Mod (rowcount + RowsCel.Get_MaxTextCount_in_One_Cel) = Linecount Then
                          Linecount := Linecount + _LineCount;

                      rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
                  End;
              End;
          End;
          indexRow := indexRow + 1;
      end;
//      ' Satýrlarý alt alta kaydýrarak dizer
      For RowsCel In Self.Get_RowsCells.CellsList do
      begin
          Rovheight := Rovheight + RowsCel.CellRectangleList[0].Yukseklik;
          For cll In RowsCel.CellRectangleList do
              cll.ScrollUpDown(Rovheight - OneRowHeight);
      end;
      Linecount := _LineCount;
      rowcount := 0;
      indexRow := 0;

     For RowsCel In Self.Get_RowsCells.CellsList do
     begin
      if (_LineCount > 0) and (indexRow >= _LineCount) then
        Self.Get_RowsCells.CellsList.Remove(RowsCel);
      indexRow := indexRow + 1;
     end;

//      For RowsCel In Self.Get_RowsCells.CellsList do
//      begin
//          If _LineCount > 0 Then
//          begin
//              rowcount := rowcount + RowsCel.Get_MaxTextCount_in_One_Cel;
//              If Linecount Mod rowcount = Linecount Then
//              begin
//                  pagebreak := pagebreak + TopCellHeight + OneRowHeight;
////                  ' üst bilgi kaydýrýlýyor
//                  For cll In TopcelClon.CellRectangleList do
//                  begin
//                      b := cll.GetCopy(GYazi);
//                      b.ScrollUpDown(pagebreak + CurrentTotalCellHeight);
//                      Self.Get_TopCells.CellRectangleList.Add(b);
//                  end;
////                  ' ilgili satýrlarýn tumu aþaðý kaydýrýlýyor
//                  For i := indexRow To Self.Get_RowsCells.CellsList.Count - 1 do
//                  begin
//                      For ii := 0 To Self.Get_RowsCells.CellsList[i].CellRectangleList.Count - 1 do
//                          Self.Get_RowsCells.CellsList[i].CellRectangleList[ii].ScrollUpDown(TopCellHeight + OneRowHeight);
//                  end;
//                  Linecount := Linecount + _LineCount;
//              End;
//          End;
////          'Toplam Satýr Yüksekliði
//          CurrentTotalCellHeight := CurrentTotalCellHeight + RowsCel.CellRectangleList[0].Yukseklik;
//          indexRow := indexRow + 1;
//      end;
//      scrol := 0;
//      If Self.Stil = 1 Then
//          scrol := TabloCel.Yukseklik * 2;

//      For RowsCel In Self.Get_RowsCells.CellsList do
//      begin
//          For cll In RowsCel.CellRectangleList do
//              cll.ScrollUpDown(scrol);
//      end;
//      For cll In Self.Get_TopCells.CellRectangleList do
//          cll.ScrollUpDown(scrol);

//    Mes := Mesafe(Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.Y,
//                  Self.Get_RowsCells.CellsList[(Self.Get_RowsCells.CellsList.Count - 1)].CellRectangleList[(0)].SolAlt.X,
//                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.Y,
//                                                       Self.Get_RowsCells.CellsList[(0)].CellRectangleList[(0)].SolUst.X);
//
//      For cll In Self.Get_DownCells.CellRectangleList do
////          ' cll.ScrollUpDown(TabloCel.Yukseklik + TabloCel.Yukseklik + (Mes - FirstTotalCellHeight))
//          cll.ScrollUpDown(scrol + (Mes - FirstTotalCellHeight));
  End;
  }
//  FreeAndNil(b);
//  FreeAndNil(c);
//  FreeAndNil(cll);
end;

procedure Tbl.SetStil(Value: Integer);
begin
  _Stil := value;
end;

{$region 'Delta Y' }
///''' <summary>
///''' Delta Y Ýkinci Koordinattan Birinci Koordinatý Çýkarýr
///''' </summary>
///''' <returns></returns>
///''' <remarks></remarks>
///Private
Function Tbl.Deltay(y1: Double; y2: Double): Double;
begin
    result := y2 - y1;
End;
destructor Tbl.Destroy;
begin
  TableTabloTempRow.Free;
  TableTabloRows.Free;
  TableTabloTopCell.Free;
  TableTabloDownCell.Free;
  inherited;
end;

{$endregion}
{$region 'Delta X' }
///''' <summary>
///''' Delta X Ýkinci Koordinattan Birinci Koordinatý Çýkarýr
///''' </summary>
///''' <returns></returns>
///''' <remarks></remarks>
///Private
constructor Tbl.Create;
begin
  _LineCount := 0;
  _Stil := 0; //'0/0/0';
  Name := '';

  TableTabloTempRow := Cells.Create;
  TableTabloRows := Rows.Create;
  TableTabloTopCell := Cells.Create;
  TableTabloDownCell := Cells.Create;
end;

Function Tbl.Deltax(x1: Double; x2: Double): Double;
begin
    result := x2 - x1;
End;
{$endregion}
{$region 'Mesafe' }

///''' <summary>
///''' Bir Noktadan Diðerine Olan Mesafe 'm'
///''' </summary>
///''' <returns></returns>
///''' <remarks></remarks>
///Private
Function Tbl.Mesafe(y1: Double; x1: Double; y2: Double; x2: Double): Double;
var
  rslt: Double;
begin
    rslt := NaN;
    rslt := Sqrt(Power(Deltay(y1, y2), 2) + Power(Deltax(x1, x2), 2));
    result := rslt;
End;
{$endregion}

{ Rows }

constructor Rows.Create;
begin
  inherited Create;
  _Endedits := False;
  CellsList := TList<Cells>.Create;
end;

destructor Rows.Destroy;
var
  _cels: Cells;
begin
  for _cels in CellsList do
    _cels.Free;
  inherited;
end;

procedure Rows.EndEdit;
var
  CurrentRow, NextRow: Integer;
begin
  If Self.CellsList.Count > 0 Then
  begin
      _TotalRowsHeigh := 0;
      For CurrentRow := 0 To Self.CellsList.Count - 1 do
      begin
          NextRow := (CurrentRow + 1) Mod Self.CellsList.Count;
          Self.CellsList[CurrentRow].DoSameHeights;
          _CurrentCelHeigt := Self.CellsList[CurrentRow].MaxHeight;
          _TotalRowsHeigh := _TotalRowsHeigh + _CurrentCelHeigt;
          If NextRow = 0 Then
            Break;
//          'For Ni As Integer = 0 To Me(CurrentRow).Count - 1
//          '    Me(NextRow)(Ni).ScrollUpDown(_CurrentCelHeigt) ' Aktif hücre büyüme miktarý kadar aþaðý kaydýrýlýyor
//          'Next
      end;
  End;
  _Endedits := True;
end;

function Rows.GetGetRowsHight: Double;
begin
  If _Endedits Then
      result := _TotalRowsHeigh
  Else
  begin
      raise Exception.Create('Yükseklik Hesaplanmamýþ');
      result := 0;
  End;
end;

procedure Rows.Reset;
begin
  _TotalRowsHeigh := 0;
  _Endedits := False;
end;

{ Cells }

procedure Cells.Add(item: CellRectangle);
begin
  Self.CellRectangleList.Add(item);
end;

constructor Cells.Create;
begin
  inherited Create;
  _TextCount := 0;
  _LastEditIndex := -1;
  CellRectangleList := TList<CellRectangle>.Create;
end;

function Cells.GetCellByNme(Name: String): CellRectangle;
var
  I: Integer;
begin
//  Static
  Currentindex := 0;
  For i := Currentindex To Self.CellRectangleList.Count - 1 do
  begin
      If Self.CellRectangleList[i].Adi = Name Then
      begin
          Currentindex := i + 1;
          result := Self.CellRectangleList[i];
          Exit;
      End;
  end;
  Currentindex := 0;
  result := nil;
end;

function Cells.GetCellsByNme(Name: String): Cells;
var
  a: Cells;
  I: Integer;
begin
  a := Cells.Create;// New Cells
  For i := 0 To Self.CellRectangleList.Count - 1 do
  begin
      If Self.CellRectangleList[i].Adi = Name Then
          a.CellRectangleList.Add(Self.CellRectangleList[i]);
  End;
  result := a;
end;

function Cells.GetGetCloneFirst(TextStartIndex: Integer): Cells;
var
  a: Cells;
  b, cel: CellRectangle;
  txt1, txt2: String;
  I: Integer;
begin
  a := Cells.Create;
//  ' Diðer elemanlar içinde aramaya devam edelim
  For cel In Self.CellRectangleList do
  begin
      b := cel.GetCopy;
      txt1 := '';
      txt2 := '';
      For i := 0 To cel.CellYazi.Count - 1 do
      begin
          If (i >= TextStartIndex) Then
            txt2 := txt2 + cel.CellYazi[i]
          Else
            txt1 := txt1 + cel.CellYazi[i];
      end;

      b.Yazi := txt1;
      b.SetFirstYukseklik;
      a.CellRectangleList.Add(b);
  end;
  a.DoSameHeights;
  result := a;
end;

function Cells.GetGetCloneLast(TextStartIndex: Integer;
  EditOriginal: Boolean): Cells;
var
//  a: Cells;
  cel, b: CellRectangle;
  txt1, txt2: String;
  I: Integer;
begin
  result := Cells.Create;
//  ' Diðer elemanlar içinde aramaya devam edelim
  For cel In Self.CellRectangleList do
  begin
      b := cel.GetCopy;//('yazi');
      txt1 := '';
      txt2 := '';
      For i := 0 To cel.CellYazi.Count - 1 do
      begin
          If (i >= TextStartIndex) Then
            txt2 := txt2 + cel.CellYazi[i]
          Else
            txt1 := txt1 + cel.CellYazi[i];
      end;

      b.Yazi := txt2;
      b.SetFirstYukseklik;
      result.CellRectangleList.Add(b);
      If EditOriginal Then
      begin
          cel.Yazi := txt1;
          cel.SetFirstYukseklik;
      End;

  end;

  If EditOriginal Then
    Self.DoSameHeights;
  result.DoSameHeights;
//  result := a;
//  FreeAndNil(cel);
//  FreeAndNil(b);
end;

procedure Cells.SortLefttoRight;
var
  s: CellRectangle;
  a: TList<CellRectangle>;
  Comparison: TComparison<CellRectangle>;
  I: Integer;
begin
//  Dim a = Self.AsEnumerable.OrderBy(Function(c) c.SolAlt.Y).ToList
  Comparison :=
  function(const Left, Right: CellRectangle): Integer
  begin
    Result := Round(Left.SolAlt.X-Right.SolAlt.X); //Y
  end;
  a := TList<CellRectangle>.Create;
  for s in Self.CellRectangleList do
    a.Add(s);

//  a := Self.CellRectangleList;
  a.Sort(TComparer<CellRectangle>.Construct(Comparison));

  Self.CellRectangleList.Clear;
  For s In a do
      Add(s);// Self.CellRectangleList.Add(s);
end;

destructor Cells.Destroy;
var
  _cellRec: CellRectangle;
begin
  for _cellRec in CellRectangleList do
    _cellRec.Free;
  inherited;
end;

procedure Cells.DoSameHeights;
var
  s: CellRectangle;
  BigHeigh: Double;
  Comparison: TComparison<CellRectangle>;
begin
  _TextCount := 0;
  if Self.CellRectangleList.Count > 0 then
  begin
  //  BigHeigh := Self.AsEnumerable.OrderByDescending(Function(c) c.Yukseklik).First.Yukseklik;
    Comparison :=
    function(const Left, Right: CellRectangle): Integer
    begin
      Result := Round(Right.Yukseklik-Left.Yukseklik);
    end;
    Self.CellRectangleList.Sort(TComparer<CellRectangle>.Construct(Comparison));
    BigHeigh := Self.CellRectangleList[0].Yukseklik;
    For s In Self.CellRectangleList do
    begin
  //      ' s.ScrollUpDown(BigHeigh - s.Yukseklik) ' Aktif hücre büyüme miktarý kadar aþaðý kaydýrýlýyor
        s.Yukseklik := BigHeigh; //' Yüksekliði Yeni Yüksekliðine ayarlanýyor
        _TextCount := Max(s.CellYazi.Count, _TextCount);
    end;
  end;
end;

function Cells.GetMaxHeight: Double;
var
  a, b: IlicgEntity;
  pt: TlicgCoor;
  cel: CellRectangle;
  I: Integer;
begin
  If Self.CellRectangleList.Count > 0 Then
  begin
      a := Licad.CreateEntityFactory.MakeEntity(idpolygon,0,_2D);// As New Asistan.CadCollection.Nc_CollectionsOf_PolyPoint
//      ' Diðer elemanlar içinde aramaya devam edelim
      For cel In Self.CellRectangleList do
      begin
        I := 0;
          For I := 0 to cel.GetCorners.Geometry.Points.Count - 1 do //As Asistan.CadObj.NCCoor_Only_Pline
              a.Geometry.Points.Add(cel.GetCorners.Geometry.Points[I]);
      end;
      b := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//As Asistan.CadCollection.Nc_CollectionsOf_PolyPoint = a.FindSmallestBoundingRectangle
      b.Geometry.Points.Add(AsCoor(A.Geometry.Extent.LowerLeft.X,A.Geometry.Extent.LowerLeft.Y));
      b.Geometry.Points.Add(AsCoor(A.Geometry.Extent.UpperRight.X,A.Geometry.Extent.UpperRight.Y));
//      result := Sqrt(Power((b.Geometry.Points[0].y - b.Geometry.Points[1].y),2) + Power((b.Geometry.Points[0].x - b.Geometry.Points[1].x), 2));
      result := (Abs(b.Geometry.Points.Y[0] - b.Geometry.Points.Y[1]));//,-2); //Roundto
  end
  Else
      result := 0;
end;

function Cells.GetMaxHeight2: Double;
var
  a, b: IlicgEntity;
  pt: TlicgCoor;
  cel: CellRectangle;
  I: Integer;
begin
  If Self.CellRectangleList.Count > 0 Then
  begin
    a := Licad.CreateEntityFactory.MakeEntity(idpolygon,0,_2D);
      For cel In Self.CellRectangleList do
      begin
        b := cel.GetCorners;
          For I := 0 to b.Geometry.Points.Count - 1 do
          begin
            pt := b.Geometry.Points[I];
              a.Geometry.Points.Add(pt);
          end;
      end;
      //SolÜst SaðAlt
//      b := Licad.CreateEntityFactory.MakeRectangle(AsCoor(a.Geometry.Extent.LowerLeft.X,a.Geometry.Extent.UpperRight.Y),
//                                                   AsCoor(a.Geometry.Extent.UpperRight.X,a.Geometry.Extent.LowerLeft.Y));
      // a.FindSmallestBoundingRectangle;

      Result := Sqrt(((a.Geometry.Extent.LowerLeft.y - a.Geometry.Extent.UpperRight.y)*(a.Geometry.Extent.LowerLeft.y - a.Geometry.Extent.UpperRight.y))
       + ((a.Geometry.Extent.LowerLeft.x - a.Geometry.Extent.UpperRight.x)*(a.Geometry.Extent.LowerLeft.x - a.Geometry.Extent.UpperRight.x)));
  end
  Else
    Result := 0;
end;

function Cells.GetWidth: Double;
var
  a, b: IlicgEntity;
  pt: TlicgCoor;
  cel, cel2: CellRectangle;
  I: Integer;
begin
  If Self.CellRectangleList.Count > 0 Then
  begin
      a := Licad.CreateEntityFactory.MakeEntity(idpolygon,0,_2D);// As New Asistan.CadCollection.Nc_CollectionsOf_PolyPoint
//      ' Diðer elemanlar içinde aramaya devam edelim
      For cel In Self.CellRectangleList do
      begin
        cel2 := cel.GetCopy;
        cel2.GoFirstCorr;
        a.Geometry.Points.Append(cel2.GetCorners.Geometry.Points);
//        I := 0;
//          For I := 0 to cel.GetCorners.Geometry.Points.Count - 1 do //As Asistan.CadObj.NCCoor_Only_Pline
//              a.Geometry.Points.Add(cel.GetCorners.Geometry.Points[I]);
      end;
      b := Licad.CreateEntityFactory.MakeEntity(idLine,0,_2D);//As Asistan.CadCollection.Nc_CollectionsOf_PolyPoint = a.FindSmallestBoundingRectangle
      b.Geometry.Points.Add(AsCoor(A.Geometry.Extent.LowerLeft.X,A.Geometry.Extent.LowerLeft.Y));
      b.Geometry.Points.Add(AsCoor(A.Geometry.Extent.UpperRight.X,A.Geometry.Extent.UpperRight.Y));
//      result := Sqrt(Power((b.Geometry.Points[0].y - b.Geometry.Points[1].y),2) + Power((b.Geometry.Points[0].x - b.Geometry.Points[1].x), 2));
      result := (Abs(b.Geometry.Points.X[0] - b.Geometry.Points.X[1]));//,-2); //Roundto
  end
  Else
      result := 0;
end;

procedure Cells.SetStil(_BlokDosyasi: IlicgEntityList);
var
  O: CellRectangle;
  I, c: Integer;
//  fstyle: TFontStyles;
  ft: IlicgFontTool;
  TempInt: Integer;
begin
  ft := Licad.Settings.FontTool;
  For O In Self.CellRectangleList do
  begin
    if (O.Adi.Contains(GTbl) or (O.Adi = GDisCerceveName)) then
      Continue;
    For i := 0 To _BlokDosyasi.Count - 1 do
    begin
//      ' Kutunun Yazý Bilgileri Düzenleniyor (TABLOUSTBASLIK)
//      ' Altý Çizili
//      ' Ýtalik
//      ' Yazý Büyüklüðü
//      ' Font adý
      If (_BlokDosyasi[i].EntityID in TextEntityIDs)
          And (O.GetCorners.InPoly(_BlokDosyasi[i].Geometry.Points[0])
          Or O.GetCorners.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) then// PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 Then
      begin
          If TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerStilFont Then
          begin
              Try
//                [IlicgFontToolStyle.fsItalic]
                  O.Yazi := _BlokDosyasi[i].AsTextValue.Text;//.GetObject(i).s;
//                  fstyle := _BlokDosyasi[i].DrawTools.FontTool.Style;
//                  Dim bitvise As New Asistan.CadObj.Helper.IntAsBits(_BlokDosyasi.GetObject(i).flags);
//                  fstyle := TLicgFontStyle.Create;// New FontStyle;
//                  If bitvise(1) Then
//                      fstyle := fstyle + FontStyle.Regular;
//
//                  If bitvise(1) Then
//                      fstyle += FontStyle.Italic;
//
//                  If bitvise(2) Then
//                      fstyle += FontStyle.Underline;

//                  ft := IlicgFontTool.Create;
//                ft := Licad.Settings.DefFontStyle;
//                ft.Style := _BlokDosyasi[i].DrawTools.FontTool.Style;
//                ft.Name := _BlokDosyasi[i].DrawTools.FontTool.Name;
//                ft.Height := _BlokDosyasi[i].DrawTools.FontTool.Height;
//                O.Font := ft;
//                o.Font.Assign(ft);
                O.Font := _BlokDosyasi[i].DrawTools.FontTool;
                O.Font.Assign(_BlokDosyasi[i].DrawTools.FontTool);

                  //New Asistan.CadObj.RFont(_BlokDosyasi.GetFontName(_BlokDosyasi.GetObject(i).ref), _BlokDosyasi.GetObject(i).sc, fstyle);
//                  bitvise := nil;
//                  fstyle := nil;
//                  ' Continue For
              except //Catch ex As Exception

              End;
//              _BlokDosyasi.GetObject(i).Tag := 0
          end
          Else If TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerLine Then
          begin
              Try
                  O.CizgiTabakaAdi := _BlokDosyasi[i].AsTextValue.Text;
//                  ' Continue For
              except //Catch ex As Exception

              End;
//              _BlokDosyasi.GetObject(i).Tag := 0
          end
          Else If TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerText Then
          begin
              Try
                  O.YaziTabakaAdi := _BlokDosyasi[i].AsTextValue.Text;
//                  '  Continue For
              except //Catch ex As Exception

              End;
//              _BlokDosyasi.GetObject(i).Tag = 0
          End;
      End;
    end;
  end;
  For O In Self.CellRectangleList do
  begin
    if (O.Adi.Contains(GTbl) or (O.Adi = GDisCerceveName)) then
      Continue;
    For i := 0 To _BlokDosyasi.Count - 1 do
    begin
  //      ' Kutunun Yazý Dayama (TABLO ÜST BAÞLIKLAR)
  //      ' Dikey
  //      ' Yatay
  //      ' YazýParçala
        If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerStilAlign) And
             (_BlokDosyasi[i].EntityID = idPoint) And
            (O.GetCorners.InPoly(_BlokDosyasi[i].Geometry.Points[0])
            or O.GetCorners.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) then//PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 Then
        begin
            Try
                If _BlokDosyasi[i].Name = GHizaStil Then
                    c := i;
               TempInt := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[0]);
                O.DikeyHizalama := DikeyHizalama(TempInt);
               TempInt := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[1]);
                O.YatayHizalama := YatayHizalama(TempInt);
                O.YaziWrap := StrToBool(_BlokDosyasi[i].Name.Split([GSlash])[2]);
  //              'Continue For
            except//Catch ex As Exception
            End;
//            _BlokDosyasi.GetObject(i).Tag := 0;
        End;
    end;
  end;

  For O In Self.CellRectangleList do
  begin
    if (O.Adi.Contains(GTbl) Or (O.Adi = GDisCerceveName)) then
      Continue;
    For i := 0 To _BlokDosyasi.Count - 1 do
    begin
  //      ' Kutunun Cercevesi (tablo ustbaslýk)
  //      ' sol,sag,ust,alt
        If (TLicgBaseLayer(_BlokDosyasi[i].Layer).DisplayName = LayerCellBorder) And
           (_BlokDosyasi[i].EntityID = idPoint) And
           (O.GetCorners.InPoly(_BlokDosyasi[i].Geometry.Points[0])
           or O.GetCorners.OnPoly(_BlokDosyasi[i].Geometry.Points[0])) then//O.GetCorners.PointInPolygon(_BlokDosyasi.GetObject(i).p1) = 1 Then
        begin
            If _BlokDosyasi[i].Name.Split([GSlash])[0] = GUST Then
            begin
                Try
                    O.UstCerceve.Draw := StrToBool(_BlokDosyasi[i].Name.Split([GSlash])[(1)]);
                    O.UstCerceve.LineIndex := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[(2)]);
  //                  'Continue For
                except//Catch ex As Exception

                End;
            end
            Else If _BlokDosyasi[i].Name.Split([GSlash])[(0)] = GALT Then
            begin
                Try
                    O.AltCerceve.Draw := StrToBool(_BlokDosyasi[i].Name.Split([GSlash])[(1)]);
                    O.AltCerceve.LineIndex := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[(2)]);
  //                  'Continue For
                except//Catch ex As Exception

                End;
            end
            Else If _BlokDosyasi[i].Name.Split([GSlash])[(0)] = GSOL Then
            begin
                Try
                    O.SolCerceve.Draw := StrToBool(_BlokDosyasi[i].Name.Split([GSlash])[(1)]);
                    O.SolCerceve.LineIndex := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[(2)]);
  //                  'Continue For
                except//Catch ex As Exception

                End;
            end
            Else If _BlokDosyasi[i].Name.Split([GSlash])[(0)] = GSAG Then
            begin
                Try
                    O.SagCerceve.Draw := StrToBool(_BlokDosyasi[i].Name.Split([GSlash])[(1)]);
                    O.SagCerceve.LineIndex := StrToInt(_BlokDosyasi[i].Name.Split([GSlash])[(2)]);
  //                  'Continue For
                except//Catch ex As Exception

                End;
            End;
//            _BlokDosyasi.GetObject(i).Tag = 0
        End;
    End;
  End;
end;

function Cells.SetvalueCell(IsRow: Boolean; ValueName: String; Value: String): CellUpdate;
var
  matches: TMatch;
  pattern, pattern1: String;
  Myrgx: TRegex;
  digit, i:Integer;
  SUCCESS: CellUpdate;
  cell: CellRectangle;
  ev: TMatchEvaluator;
begin
//  Dim matches As Match
  pattern := GGeri + ValueName + GGeri;
  pattern1 := GPattern;
  Myrgx := TRegex.Create(pattern1, [roSingleLine]);//RegexOptions.Singleline)
  digit := 2;
  SUCCESS := CellUpdate.Yok;
  i := -1;

  For cell In Self.CellRectangleList do
  begin
      i := i + 1;
      matches := TRegex.Match(cell.Adi, pattern, [roSingleLine]);//System.Text.RegularExpressions.RegexOptions.Singleline)
      If matches.Success Then
      begin
          SUCCESS := CellUpdate.Varr;
          matches := TRegex.Match(cell.Yazi, pattern1, [roSingleLine]);//System.Text.RegularExpressions.RegexOptions.Singleline);
          If matches.Success Then
          begin
//              'If i > _LastEditIndex Then
              cell.Yazi := Myrgx.Replace(cell.Yazi, Value, 1);//, ev(matches)); //1, matches.Index
              SUCCESS := CellUpdate.Degistirildi;
              _LastEditIndex := i;
              If IsRow Then
                  Break//Exit For
              Else
                  Continue;// For
//              'End If
          end
          Else
              SUCCESS := CellUpdate.Degismis;
      End;
  end;
  result := SUCCESS;
end;

function Cells.GetGet_MaxTextCount_in_One_Cel: Integer;
begin
  result := _TextCount;
end;

initialization
  Currentindex := 0;

end.

