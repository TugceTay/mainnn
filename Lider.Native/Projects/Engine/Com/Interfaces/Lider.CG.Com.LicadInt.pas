unit Lider.CG.Com.LicadInt;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  Graphics,
  SysUtils,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.SettingsInt,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.RTRee,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Yacclib,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.LineDraw,
  Lider.CG.Com.CmdListInt,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.UtilityInt,
  Lider.CG.Com.Types,
  Lider.CG.Com.TransformInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.ProjectInt,
  Lider.CG.Com.VectorInt;

type
  IlicgProgress = interface
    ['{FEE0EDE5-E8C9-49F2-A15E-743D665FA20D}']
      {function  LoopCount : integer; stdcall;
      procedure UpdateLoopProgress(Position: Integer); stdcall;
      procedure StartProgress (const ACaption: string; AMin, AMax: Integer ; _ProgressHandle: THandle); stdcall;
      procedure EndProgress; stdcall;
      procedure UpdateCaption (const ACaption: string); stdcall;
      procedure UpdateProgress(Position: Integer); stdcall;
      function  ProcessStopWithEsc:Boolean;stdcall;}
    procedure ProgressStart(const AMax: Integer = 0; const AMessage: string = '';
      const AProgressHandle: THandle = 0); stdcall;
    procedure ProgressPosition(const APosition: Integer = -1); stdcall;
    procedure ProgressEnd; stdcall;
  end;

  ILicadReport = interface
    ['{F7D459C9-FD2A-4B90-87B8-1844E95291B0}']
    procedure ReportDesing; stdcall;
    procedure ReportShow; stdcall;
    procedure ReportPrint; stdcall;
    procedure DataSetsAdd(ADataSet: TObject); stdcall;
    procedure DataSetsBeginUpdate; stdcall;
    procedure DataSetsEndUpdate; stdcall;
    procedure LoadFromFile(AFileName: string); stdcall;
    function FindObject(AName: string): TObject; stdcall;
    procedure VariablesAdd(AVariable: string; AValue: string); stdcall;
  end;

  TLicadPath = record
  public
    class function GetLicad: string; static;
    class function GetData: string; static;
    class function GetFonts: string; static;
    class function GetLineTypes: string; static;
    class function GetModules: string; static;
    class function GetPatterns: string; static;
    class function GetSettings: string; static;
    class function GetSymbols: string; static;
  end;

  ILicadLib = interface
    ['{312084FA-0D82-4C82-AFED-5B562F15D88A}']
    function PolygonIntersection(const AOperationType: TlicgOperationType; const AEntityList, BEntityList: IlicgEntityList; out ASolutionList: IlicgEntityList): Boolean; stdcall;
    function PolygonClip(const AClipType: TlicgClipType; const AEntityList, BEntityList: IlicgEntityList; out ASolutionList: IlicgEntityList): Boolean; overload; stdcall;
    function PolygonClip(const AClipType: TlicgClipType; const AEntity, BEntity: IlicgEntity; out ASolutionEntity: IlicgEntity): Boolean; overload; stdcall;
    function PolygonPartsStatus(const AVector: IlicgVector; var AParts, AWinding: TlicgIntegerArray): Boolean; overload; stdcall;
  end;

  ILicad = interface
    ['{EAEB6EFB-6B8E-41BB-8977-FC720F2BA430}']
    // Project
    function CreateProject: IlicgProject; stdcall;
    function CreateProjectTools: IProjectTools; stdcall;
    function CreateGIS: TlicgBaseGIS; stdcall;
    function CreateCAD: TlicgBaseGis; stdcall;
    function CreateGrapher(Device: TlicgActiveDevice; DrawBox: TObject = nil):
      TlicgBaseGrapher; stdcall;
    function GetCmdLineList: ICmdLineList; stdcall;
    function GetBitmapInstance(ResName: string): HBitmap; stdcall;
    function CreateActionTracker(CmdLine: TlicgBaseCmdline): IlicgActionTracker; stdcall;
    function CreatePainterObject(ThreadData: PThreadData): IlicgPainterObject; stdcall;
    function LayerSelectDialog(const ACmdLine: TlicgBaseCmdLine; ALayerList: TList; AMultiSelect: Boolean = False;
      AFormCaption: string = ''; ALayerTypes: TlicgLayerClassTypes = [lctLayer]): TModalResult; stdcall;
    function GetPortInterface(DB_Width, DB_Height: integer; ParentHandle:
      THandle; _idPort: widestring; PassDrawBoxEvents: TPassDrawBoxEvents;
      isSymbolBox: Boolean): ILicadPort; stdcall;
    function CreateSelection(const DrawBox: TlicgBaseDrawBox): TlicgBaseSelection; stdcall;
    procedure CallBackSelectProcInSelectAction(const cmdline: TlicgBaseCmdline;
      Filter: TlicgEntityIDs; SelectProc: TlicgSelectProc); stdcall;
    function SelectEntityInstant(const ACmdline: TlicgBaseCmdLine; AMesaj: string; AEntityCount: Integer;
      AEntityFilter: TlicgEntityIDs; var ASelection: TlicgBaseSelection): Boolean; stdcall;
    function SelectEntityWithGivenPolygon(const Polygon: IlicgEntity; cmdLine:
      TlicgBaseCmdline; GraphicOp: TlicgGraphicOperator = goWithin): Boolean; stdcall;
    procedure EntCopyToAnotherLayer(CmdLine: TlicgBaseCmdLine; isMove: Boolean); stdcall;
    procedure SaveSelectedEntities(ADrawBox: TlicgBaseDrawBox; ANewProjectFile: string = ''); stdcall;
    function GetDistanceFromScreen(cmdline: TlicgBaseCmdLine): double; stdcall;
    procedure SaveClippedAreaTo(DrawBox: TlicgBaseDrawBox; saveTo: string); stdcall;
    function GetPreviewGIS(Ent: IlicgEntity): TlicgBaseGis; stdcall;
    procedure SetPreviewEntDrawingUnits(pEnt: IlicgEntity; _DrawingUnits: Double); stdcall;
    function GetPaperUnits(DrawBox: TlicgBaseDrawBox): TlicgScaleUnits; stdcall;
    function IsPreviewBox(Drawbox: TlicgBaseDrawBox): Boolean; stdcall;
    procedure SetAccuDrawPosition(P1, P2: TlicgCoor); stdcall;
    procedure SetGlobalTempEntity(Ent: IlicgEntity); stdcall;
    procedure SetAccuDrawEnable(Value: Boolean); stdcall;
      // procedure SetCmdLineIsMouseDown(Value:Boolean);stdcall;
    function ViewPortDisplayName(i: integer): string; stdcall;
    function GetViewPortCurrIndex: integer; stdcall;
    procedure SetViewportCaption(GIS: TlicgBaseGIS); stdcall;
    procedure ShowViewportByDrawBoxCount(_DBoxWinCount: integer);
    procedure PrintPreview(const CmdLine: TlicgBaseCmdLine); stdcall;

    function CreateCmdLine(const AOwner: TComponent): TlicgBaseCmdLine; stdcall;
    function CreateDrawBox(const AOwner: TComponent; DrawBoxType : TDrawBoxType): TlicgBaseDrawBox; stdcall;
    procedure JoinDrawBoxToCmdLine(const ADrawBox: TlicgBaseDrawBox; const ACmdLine: TlicgBaseCmdLine); stdcall;

    procedure DrawPattern(Canvas: IlicgCanvas; Index: Integer; ForeColor,
      BackColor, FillColor: TColor; Rect: TRect; ShowIndex: Boolean; IsSelected:
      Boolean; ShowHexa: Boolean; Clear: Boolean = true; Edged: Boolean = False); stdcall;
    procedure DrawSymbol(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; FillColor, ForeColor: Tcolor;
      ShowIndex, ShowHexa, ShowName, Clear, Edged: Boolean; categoryName: string); stdcall;
    procedure DrawBlock(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; FillColor, ForeColor: Tcolor;
      ShowIndex, ShowHexa, ShowName, Clear, Edged: Boolean); stdcall;
    procedure DrawLinetype(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; LineColor, FillColor: TColor;
      const Scale: Double; Repit: Integer; ShowIndex, ShowHexa, ShowName:
      Boolean; Clear: Boolean = true; Edged: Boolean = False); stdcall;

    procedure DrawLinetypeBitmap(Index, Height, Width: Integer;  LineColor,
      FillColor: TColor; bmp: TBitmap; Scale: Integer = 10; Edge: Boolean = False); stdcall;
    procedure DrawSymbolBitmap(FileIndex: Integer; Index, Height, Width: Integer;
      LineColor, FillColor: TColor; bmp: TBitmap; Edge: Boolean = False); stdcall;

    function GetDrawSymbolInt(_S: IlicgSymbol): IDrawSymbol; stdcall;
    function GetDrawLinetypeInt(_S: IlicgSymbol): IDrawLineType; stdcall;
    procedure DrawClosed_Vector(const Points: IlicgVector; Canvas: IlicgCanvas;
      const Clip, Extent: TlicgExtent; Grapher: TlicgBaseGrapher; const ADrawTools: IlicgDrawTools;
      const M: TlicgMatrix;
      DrawMode: TlicgDrawMode; Bitmap: Graphics.TBitmap; ClipRgn: HRgn;
      aLineWidthDirection: TlicgLineWidthDirection; aProjectScale: double;
      SymBitmap: TSymBitmap); stdcall;
    procedure DrawOpened_Vector(const Points: IlicgVector; Canvas: IlicgCanvas;
      const Clip, Extent: TlicgExtent; Grapher: TlicgBaseGrapher; const ADrawTools: IlicgDrawTools;
      const M: TlicgMatrix; DrawMode: TlicgDrawMode; ClipRgn:
      HRgn; aLineWidthDirection: TlicgLineWidthDirection; aProjectScale:
      double); stdcall;
    function DrawEntity: IDrawEntity; stdcall;
    function GetLocationFromAdres(Adres: string; var lat, lon: double; var
      formatted_address: widestring): boolean; stdcall;
    function ShowGooglePref: boolean; stdcall;
    function LayerSettings(const ACmdLine: TlicgBaseCmdLine; const ALayer: TlicgBaseLayer;
      AActivePage: TlicgLayerSettingsActivePage = lsapGeneral ): TModalResult;  stdcall;
    procedure PaftaSettings(cmdL: TlicgBasecmdline); stdcall;
    function RasterTransform(const TempPath, OrijinalPath: string;
      ATransformGCP: IlicgTransformGCP; PixelW, PixelH: integer): boolean; stdcall;

       //BEGIN< Modul (Core) Interface
    function Hatches: IlicgHatchList; stdcall;
    function RasterLineTypes: TlicgBaseRasterLineTypes; stdcall;
    function VectorFonts: IlicgVectorFonts; stdcall;
    function CreateLayerInfo(Layer: TlicgBaseLayer): TlicgBaseLayerInfo; stdcall;
    function CreateMemTree(Layer: TlicgBaseLayer; t: TTreeType; Mode: Word): TRTree; stdcall;
    function CreateRTree(Layer: TlicgBaseLayer; t: TTreeType; Mode: Word): TRTree; stdcall;
    function CreateNativeDBF(Gis: TlicgBaseGIS; const FName: string; ReadWrite,
      Shared: boolean): TlicgBaseTable; stdcall;
    function CreateLayers: TlicgBaseLayers; stdcall;
    function CreateLayer(GIS: TlicgBaseGis; const LayerFullPath: string; LayerType: TlicgLayerType; AColor: TColor = clBlue;
      UseDBFFile: Boolean = True; IsAddToGis: Boolean = True; FieldList: TStrings = nil): TlicgBaseLayer; stdcall;

    //procedure CreateSpline(Order, PointsInCurve: Integer; ControlPoints, CurvePoints: IlicgVector);stdcall;
    //function  CreateSymbol(symbols:IlicgSymbols):IlicgSymbol;stdcall;

    function CreateRasterLineTypes: TlicgBaseRasterLineTypes; stdcall;
    function CreateEntityFactory: IlicgEntityFactory; stdcall;
    function CreateCSFactory: IlicgCSFactory; stdcall;
    function CreateTransform: IlicgTransformGCP; stdcall;
    function CreateEntityStream: IEntityStream; stdcall;
    function CreateTopology: IlicgTopology; stdcall;

    //function  MakeSymbol (Symbols: IlicgSymbols): IlicgSymbol; stdcall;
    //function  MakeLineType (Lines: IlicgSymbols): IlicgSymbol; stdcall;


      // BEGIN< Modul (Expr) Interface
    function CreateMainExpr(const Gis: TlicgBaseGis; const Layer: TlicgBaseLayer):
      TlicgBaseMainExpr; stdcall;
    function CreateEntityExpr(ParameterList: TParameterList; GIS: TlicgBaseGIS;
      Layer: TlicgBaseLayer): TlicgaseEntExpr; stdcall;
    function CreateVectorExpr(ParameterList: TParameterList; Vector: IlicgVector;
      VectorType: TlicgVectorType; const BufferWidth: Double):
      TlicgBaseVectorExpr; stdcall;
    function CreateGraphicOperatorExpr(ParameterList: TParameterList;
      GraphicOperator: TlicgGraphicOperator): TlicgBaseGraphicOperatorExpr; stdcall;
    function CreateQueryVectorExpr(ParameterList: TParameterList; MainExpr:
      TlicgBaseMainExpr): TlicgBaseQueryVectorExpr; stdcall;
    function CreateQueryScopeExpr(ParameterList: TParameterList; MainExpr:
      TlicgBaseMainExpr): TlicgBaseQueryScopeExpr; stdcall;
    function CreateQueryLayerExpr(ParameterList: TParameterList; MainExpr:
      TlicgBaseMainExpr): TlicgBaseQueryLayerExpr; stdcall;
    function CreateExprParser(MainExpr: TlicgBaseMainExpr): TBaseExprParser; stdcall;
    function CreateScrLexer: TCustomLexer; stdcall;
    function CreateScrParser: TCustomParser; stdcall;
    function CreateExprLex: TCustomLexer; stdcall;
      //>END

    function GetModuleInterface(ModulName: string): IUnknown; stdcall;
    function LoadLicadEditorModul(PConnParams: PConnectionParams): IUnknown; stdcall;
    function GetUtility: IlicgUtility; stdcall;

      //procedure SymbolEditor; stdcall;
      //procedure LineEditor; stdcall;
    procedure ProgressStart(const AMax: Integer = 0; const AMessage: string = '';
      const AProgressHandle: THandle = 0); stdcall;
    procedure ProgressPosition(const APosition: Integer = -1); stdcall;
    procedure ProgressEnd; stdcall;

      {procedure StartLoopProgress(LMessage : string; LoopCount : integer); stdcall;
      procedure UpdateLoopProgress(Position: Integer); stdcall;
      procedure EndLoopProgress; stdcall;

      procedure StartProgress (const ACaption: string; AMin, AMax: Integer ; _ProgressHandle: THandle); stdcall;
      procedure EndProgress; stdcall;
      procedure UpdateCaption (const ACaption: string); stdcall;
      procedure UpdateProgress(Position: Integer); stdcall;
      function  ProcessStopWithEsc:Boolean;stdcall;}

    function LoadPostgisUserTable(const aGis, RefGis: TlicgBasegis;
      isDesktopLayer: Boolean): TlicgBaseLayer; stdcall;
    procedure InteraktifLayerRefresh(const aLayer: TlicgBaseLayer); stdcall;
    procedure PostgisGeometriAnaliz(const aCmdline: TlicgBasecmdline); stdcall;
    function GetLayer_SQLExecuteGeomSQLTable(const ConnParams: TConnectionParams;
      const aGis: TlicgBasegis; var SqlRec: TTableSQLRecord; layerName: string;
      phandle: THandle; isPersistBitmap: boolean = false; iconFieldName: string
      = ''; iconHeight: integer = 5): TlicgBaseLayer; stdcall;
    procedure LayerJoinAndPostgisTablosunaAktar(const aGis: TlicgBaseGis; const
      aLayer: TlicgBaseLayer); stdcall;
    procedure DeleteOrAddDBFFields(const aLayer: TlicgBaseLayer); stdcall;
    function GetPostGisLayer(const aGis: TlicgBasegis; const LCASI:
      ILayerConnectorAndSqlInfo; isStandart: Boolean): TlicgBaseLayer; stdcall;
    function CreateStandartPostGisLayerAndLoad(const aGis: TlicgBasegis; const
      LCASI: ILayerConnectorAndSqlInfo): TlicgBaseLayer; stdcall;
    procedure EzLayerToPostgis(const Cmdline: TObject; const Layer:
      TlicgBaseLayer; UseSelection: boolean); stdcall;
    function AddPostGisLayersFromIniFile(const aGis: TlicgBasegis; fn: string;
      isReload: boolean = false; LayerList: TStrings = nil): boolean; stdcall;
    function TestPostgresConnection(fn: string): boolean; stdcall;
    procedure PostgresSqlBuilder(const aCmdLine: TlicgBaseCmdLine); stdcall;
    procedure PostGisToSHP; stdcall;
    procedure ShowResimler(path: string; filestring: IStringsPassIntegerData); stdcall;
    procedure EntityPropertiesShow(const ACmdLine: TlicgBaseCmdline); stdcall;
    procedure StartRasterClickAction(const CmdLine: TlicgBaseCmdline); stdcall;
    procedure Save_AllLayerListAndProperties(const aGis: TlicgBaseGis); stdcall;
    procedure Load_AllLayerListAndProperties(const aGis: TlicgBaseGis); stdcall;
      //property ScreenCursor    : TCursor read GetScreenCursor write SetScreenCursor ;
      //property ClientRectProc  : TlicgGetClientRectProc read getClientRectProc write setClientRectProc;
    function GetPath: TLicadPath; stdcall;
    function GetLib: ILicadLib; stdcall;
    function GetReport: ILicadReport; stdcall;
    function GetTextEditor: IlicgTextEditor; stdcall;
    function GetSettings: IlicgSettings; stdcall;
    function GetTopology: IlicgTopology; stdcall;
    function GetLicadFonts: TStrings; stdcall;

    function GetModuleMain: IModuleMain; stdcall;
    procedure SetModuleMain(const Value: IModuleMain); stdcall;

    property Path: TLicadPath read GetPath;
    property Lib: ILicadLib read GetLib;
    property Report: ILicadReport read GetReport;
    property TextEditor: IlicgTextEditor read GetTextEditor;
    property Settings: IlicgSettings read GetSettings;
    property Topology: IlicgTopology read GetTopology;
    property ModuleMain: IModuleMain read GetModuleMain write SetModuleMain;
    property Fonts: TStrings read GetLicadFonts;
  end;

{$J+}
const
  Licad: ILicad = nil;
{$J-}

function CurrCmdLine: TlicgBaseCmdLine;

function CreateGrapher(Device: TlicgActiveDevice; DrawBox: TObject = nil):
  TlicgBaseGrapher; stdcall;

function CreateGradient: TlicgBaseGradient; stdcall;

function CreateNodeAssocList: TlicgBaseNodeAssocList; stdcall;

const
  Auth_Param_Read: integer = 1;
  Auth_Param_ReadWrite: integer = 2;

implementation

uses
  Lider.CG.Com.Grapher,
  Lider.CG.Com.Lists,
  Lider.CG.Com.ModulesConsts;

function CurrCmdLine: TlicgBaseCmdLine;
begin
  Result := nil;
  if Assigned(Licad) and Assigned(Licad.GetCmdLineList) then
    Result := Licad.GetCmdLineList.CurrCmdline;
end;

function CreateGrapher(Device: TlicgActiveDevice; DrawBox: TObject = nil):
  TlicgBaseGrapher;
begin
  Result := TlicgGrapher.Create(Device, DrawBox)
end;

function CreateGradient: TlicgBaseGradient;
begin
  Result := TlicgGradient.Create;
end;

function CreateNodeAssocList: TlicgBaseNodeAssocList;
begin
  Result := TlicgNodeAssocList.Create;
end;

{ TLicadPath }
class function TLicadPath.GetLicad: string;
begin
  Result := ExtractFilePath(ParamStr(0)); //ExtractFilePath(Application.ExeName);
end;

class function TLicadPath.GetData: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Data\';
end;

class function TLicadPath.GetFonts: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Fonts\';
end;

class function TLicadPath.GetLineTypes: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'LineTypes\';
end;

class function TLicadPath.GetModules: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Modules\';
end;

class function TLicadPath.GetPatterns: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Patterns\';
end;

class function TLicadPath.GetSettings: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Settings\';
end;

class function TLicadPath.GetSymbols: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'Symbols\';
end;


end.


