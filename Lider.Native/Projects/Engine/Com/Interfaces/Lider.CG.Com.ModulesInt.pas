unit Lider.CG.Com.ModulesInt;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Forms,
  Controls,
  DB,
  AdoDB,
  Lider.CG.Com.TransformInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.CmdListInt,
  Lider.CG.Com.RTree,
  Lider.CG.Com.SettingsInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.Yacclib,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.LineDraw,
  Lider.CG.Com.Types,
  Lider.CG.Com.ProjectInt,
  Lider.CG.Com.ListsInt;

{$I Lider.CG.Com.Component.inc}

type
  TModulActionID = Integer;

const
  ModActID_DBGISLayerEdit: TModulActionID = 1;

type
  TlicgLayerSettingsActivePage = (lsapGeneral, lsapPen, lsapBrush, lsapSym, lsapText, lsapLabel, lsapThematic);

  //LicadPort, ile birlikte yaratýlan drawbox bileþenini, cmdline bileþenine baðlamadan bu eventlerin
  //set edilmesi gerekiyor.
  //Bu yüzden  aþaðýdaki class getLicadPort a parametre olarak geçirilmeli...
  TPassDrawBoxEvents = class
    OnMouseDown2D: TlicgMouseEvent;
    OnMouseMove2D: TlicgMouseMoveEvent;
    OnMouseUp2D: TlicgMouseEvent;
    OnClick: TNotifyEvent;
    OnDblClick: TNotifyEvent;
      {
      OnKeyPress    :
      OnKeyDown     :
      OnKeyUp       :
      OnPaint       :
      }
    constructor Create;
  end;

  ILicadPort = interface
    ['{095A2FFF-A72B-4D99-ADEE-FCF533906C34}']
    function getDrawBox: TlicgBaseDrawBox; stdcall;
    function getCmdline: TlicgBaseCmdline; stdcall;
    function getForm: TForm; stdcall;
    function getIdPort: widestring; stdcall;
    procedure SetGisPaintEvents(_GIS: TlicgBaseGIS); stdcall;
    procedure SetGisOnTimer(_GIS: TlicgBaseGIS); stdcall;
    property DrawBox: TlicgBaseDrawBox read getDrawBox;
    property Cmdline: TlicgBaseCmdline read getCmdline;
    property Form: TForm read getForm;
    property IdPort: widestring read getIdPort;
  end;

  IProjectTools = interface
    ['{80E735F4-B22E-4D47-8320-33D3F14A029C}']
    function NewProject(_CreateInfo: IlicgProject; GisFileName: string = ''; Progress: TObject = nil): boolean; stdcall; // li2016
    procedure OpenProject(_CreateInfo: IlicgProject; GisFileName: string = ''; AShowProgress: Boolean = True; AAddRecentProjects: Boolean = True); stdcall;
    procedure SaveProject(cmdLine: TlicgBaseCmdLine = nil); stdcall;
    function SaveAsProject(AGis: TlicgBaseGis; var ANewFN: string; AAddRecentProjects: Boolean = True): Boolean; stdcall;
    procedure SaveAllProject; stdcall;
    procedure SaveSelectedEntities(ADrawBox: TlicgBaseDrawBox; ANewProjectFile: string = ''); stdcall;
    procedure SaveAsPicture; stdcall;
    procedure AddProjectFiles; stdcall;
    procedure CloseAllProject(var ignoreClose: Boolean); stdcall;
    procedure CloseProject; stdcall;
    procedure PrintPreview(const CmdLine: TlicgBasecmdline); stdcall;
    procedure Exit; stdcall;
  end;

  IlicgTextEditor = interface
  ['{53FDD216-46CC-4C8D-A27A-35027845D1ED}']
    procedure Execute(const AEditorText: WideString = ''); stdcall;
  end;

  IModuleMain = interface
  ['{20A2FEA5-8E69-4E4E-9DD5-0E3B568F9FFD}']
    procedure OpenProject(AFilename: string; ARibbonHomeTabShow: Boolean = True; AShowProgress: Boolean = True; AAddRecentProjects: Boolean = True); stdcall;
    procedure SaveAsProject(AFilename: string; AAddRecentProjects: Boolean = True); stdcall;
    procedure EntityPropertiesShow(const ACmdline: TlicgBaseCmdline); stdcall;
    function FormShow(AFormClass: TFormClass): TForm; stdcall;
    procedure DockingNesneOzellikleriExecute(const ACmdLine: TlicgBaseCmdLine; ALayer: TlicgBaseLayer; ARecNo: Integer); stdcall;
    function MainFormHandle: HWND; stdcall;
  end;

  IModuleCore = interface
    ['{52B8B033-2A06-4BE7-B76A-4BD43679F279}']
    function Hatches: IlicgHatchList; stdcall;
    function RasterLineTypes: TlicgBaseRasterLineTypes; stdcall;
    function VectorFonts: IlicgVectorFonts; stdcall;
    function CreateLayerInfo(Layer: TlicgBaseLayer): TlicgBaseLayerInfo; stdcall;
    function CreateMemTree(Layer: TlicgBaseLayer; t: TTreeType; Mode: Word): TRTree; stdcall;
    function CreateRTree(Layer: TlicgBaseLayer; t: TTreeType; Mode: Word): TRTree; stdcall;
    function CreateNativeDBF(Gis: TlicgBaseGIS; const FName: string; ReadWrite,
      Shared: boolean): TlicgBaseTable; stdcall;
    function CreateLayers: TlicgBaseLayers; stdcall;
    function CreateLayer(GIS: TlicgBaseGis; const LayerFullPath: string;
      LayerType: TlicgLayerType; UseDBFFile: boolean = true; isAddToGis: boolean = true;
      FieldList: TStrings = nil): TlicgBaseLayer; stdcall;
    function CreateRasterLineTypes: TlicgBaseRasterLineTypes; stdcall;
    function Core_CreateLayer(const AGis: TlicgBaseGis; const LayerName: string;
      LayerType: TlicgLayerType): TlicgBaseLayer; stdcall;
    function Core_CADCreateLayer(const AGis: TlicgBaseGis; const LayerName: string;
      LayerType: TlicgLayerType): TlicgBaseLayer; stdcall;
  end;

  IModuleGIS = interface
    ['{4AE38C80-B19A-4F14-BCBB-EB2A6D77231C}']
    function GetTextEditor: IlicgTextEditor; stdcall;
    function CreateGIS: TlicgBaseGIS; stdcall;
    function CreateCAD: TlicgBaseGis; stdcall;
    function CreateGrapher(Device: TlicgActiveDevice; DrawBox: TObject = nil): TlicgBaseGrapher; stdcall;
    function getCmdLineList: ICmdLineList; stdcall;
    function getBitmapInstance(ResName: string): HBitmap; stdcall;
    function CreateActionTracker(CmdLine: TlicgBaseCmdline): IlicgActionTracker; stdcall;
    function createPainterObject(ThreadData: PThreadData): IlicgPainterObject; stdcall;
    function LayerSelectDialog(const ACmdLine: TlicgBaseCmdLine; ALayerList: TList; AMultiSelect: Boolean = False;
      AFormCaption: string = ''; ALayerTypes: TlicgLayerClassTypes = [lctLayer]): TModalResult; stdcall;
    function getPortInterface(DB_Width, DB_Height: integer; ParentHandle:
      THandle; _idPort: widestring; PassDrawBoxEvents: TPassDrawBoxEvents;
      isSymbolBox: Boolean): ILicadPort; stdcall;
    function CreateSelection(const DrawBox: TlicgBaseDrawBox): TlicgBaseSelection; stdcall;
    procedure CallBackSelectProcInSelectAction(const cmdline: TlicgBaseCmdline;
      Filter: TlicgEntityIDs; SelectProc: TlicgSelectProc); stdcall;
    function SelectEntityInstant(const ACmdLine: TlicgBaseCmdline; AMesaj: string; AEntityCount: Integer;
      AEntityFilter: TlicgEntityIDs; var ASelection: TlicgBaseSelection): Boolean; stdcall;
    function SelectEntityWithGivenPolygon(const Polygon: IlicgEntity; cmdLine:
      TlicgBaseCmdline; GraphicOp: TlicgGraphicOperator = goWithin): Boolean; stdcall;
    procedure EntCopyToAnotherLayer(CmdLine: TlicgBaseCmdLine; isMove: Boolean); stdcall;
    function getDistanceFromScreen(cmdline: TlicgBaseCmdLine): double; stdcall;
    procedure SaveClippedAreaTo(DrawBox: TlicgBaseDrawBox; saveTo: string); stdcall;
    function GetPreviewGIS(Ent: IlicgEntity): TlicgBaseGis; stdcall;
    procedure SetPreviewEntDrawingUnits(pEnt: IlicgEntity; _DrawingUnits: Double); stdcall;
    function getPaperUnits(DrawBox: TlicgBaseDrawBox): TlicgScaleUnits; stdcall;
    function isPreviewBox(Drawbox: TlicgBaseDrawBox): Boolean; stdcall;
    procedure SetAccuDrawPosition(P1, P2: TlicgCoor); stdcall;
    procedure SetGlobalTempEntity(Ent: IlicgEntity); stdcall;
    procedure SetAccuDrawEnable(Value: Boolean); stdcall;
      // procedure SetCmdLineIsMouseDown(Value:Boolean);stdcall;
    function ViewPortDisplayName(i: integer): string; stdcall;
    function getViewPortCurrIndex: integer; stdcall;
    procedure SetViewportCaption(GIS: TlicgBaseGIS); stdcall;
    procedure ShowViewportByDrawBoxCount(_DBoxWinCount: integer);
    procedure PrintPreview(const CmdLine: TlicgBaseCmdLine); stdcall;

    function CreateCmdLine(const AOwner: TComponent): TlicgBaseCmdLine; stdcall;
    function CreateDrawBox(const AOwner: TComponent; DrawBoxType : TDrawBoxType): TlicgBaseDrawBox; stdcall;

    procedure DrawPattern(Canvas: IlicgCanvas; Index: Integer; ForeColor,
      BackColor, FillColor: TColor; Rect: TRect; ShowIndex: Boolean; IsSelected:
      Boolean; ShowHexa: Boolean; Clear: Boolean = true; Edged: Boolean = False); stdcall;
    procedure DrawSymbol(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; FillColor, ForeColor: Tcolor;
      ShowIndex, ShowHexa, ShowName, Clear, Edged: Boolean; categoryName: string); stdcall;
    procedure DrawBlock(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; FillColor, ForeColor: Tcolor;
      ShowIndex, ShowHexa, ShowName, Clear, Edged: Boolean); stdcall;
    procedure DrawLineType(Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas; Index:
      Integer; Rect: TRect; IsSelected: Boolean; LineColor, FillColor: TColor;
      const Scale: Double; Repit: Integer; ShowIndex, ShowHexa, ShowName: Boolean;
      Clear: Boolean = True; Edged: Boolean = False); stdcall;

    procedure DrawLinetypeBitmap(Index, Height, Width: Integer;  LineColor,
      FillColor: TColor; bmp: TBitmap; Scale: Integer = 10; Edge: Boolean = False); stdcall;
    procedure DrawSymbolBitmap(FileIndex: Integer; Index, Height, Width: Integer;
      LineColor, FillColor: TColor; bmp: TBitmap; Edge: Boolean = False); stdcall;

    function getDrawSymbolInt(_S: IlicgSymbol): IDrawSymbol; stdcall;
    function getDrawLinetypeInt(_S: IlicgSymbol): IDrawLineType; stdcall;
    procedure DrawClosed_Vector(const Points: IlicgVector; Canvas: IlicgCanvas;
      const Clip, Extent: TlicgExtent; Grapher: TlicgBaseGrapher;
      const ADrawTools: IlicgDrawTools; const M: TlicgMatrix;
      DrawMode: TlicgDrawMode; Bitmap: Graphics.TBitmap; ClipRgn: HRgn;
      aLineWidthDirection: TlicgLineWidthDirection; aProjectScale: double;
      SymBitmap: TSymBitmap); stdcall;
    procedure DrawOpened_Vector(const Points: IlicgVector; Canvas: IlicgCanvas;
      const Clip, Extent: TlicgExtent; Grapher: TlicgBaseGrapher; const ADrawTools: IlicgDrawTools;
      const M: TlicgMatrix; DrawMode: TlicgDrawMode; ClipRgn:
      HRgn; aLineWidthDirection: TlicgLineWidthDirection; aProjectScale:
      double); stdcall;
    function DrawEntity: IDrawEntity; stdcall;
    //procedure SymbolEditor; stdcall;
    //procedure LineEditor; stdcall;
    function GetLocationFromAdres(Adres: string; var lat, lon: double; var
      formatted_address: widestring): boolean; stdcall;
    function ShowGooglePref: boolean; stdcall;
    function LayerSettings(const ACmdLine: TlicgBaseCmdLine; const ALayer: TlicgBaseLayer;
      AActivePage: TlicgLayerSettingsActivePage = lsapGeneral ): TModalResult;  stdcall;
    procedure EntityPropertiesShow(const ACmdLine: TlicgBaseCmdline); stdcall;
    procedure StartRasterClickAction(const CmdLine: TlicgBaseCmdline); stdcall;
    procedure Save_AllLayerListAndProperties(const aGis: TlicgBaseGis); stdcall;
    procedure Load_AllLayerListAndProperties(const aGis: TlicgBaseGis); stdcall;
    procedure PaftaSettings(cmdL: TlicgBasecmdline); stdcall;
    function RasterTransform(const TempPath, OrijinalPath: string;
      ATransformGCP: IlicgTransformGCP; PixelW, PixelH: integer): boolean; stdcall;
    //function GetLayerManager: ILayerManager; stdcall;
    //property LayerManager: ILayerManager read GetLayerManager;

    // Core Modülünden Gelenler
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
    function CreateRasterLineTypes: TlicgBaseRasterLineTypes; stdcall;
  end;

  IModuleExpression = interface
    ['{E971F232-3DBA-4E38-8346-FF6083CFDE59}']
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
  end;

  IModuleTopology = interface
    ['{D479E243-DA7B-4FB6-B740-E5CE40FC0F03}']
    procedure AddTopologyMenus; stdcall;
    function CloseClickedArea(const P: TlicgCoor;{týklanan nokta} var ResultVector: IlicgVector;
      var AMessage: string): Boolean; stdcall;
    procedure CloseAreaWithLines_; stdcall;
    function LineToPolygonProc_: boolean; stdcall;
  end;

  IModuleIE = interface
    ['{79BDD4B4-1EF6-4974-9AF6-DCDF684C3372}']
    // ilker eklemeler yeni 28.05.2022
    function ImportSymbolNCZ(const SymbolFilename: string; FSymbols: IlicgSymbols): Boolean; stdcall;
    function ImportVectorelFontNCZ(const SymbolFilename: string; FSymbols: IlicgVectorFonts): Boolean; stdcall;
    procedure ImportNCZ(const Filename: string; Filter: TlicgEntityIDs; isOnlyLineTypes: Boolean = False; isShowMessages: Boolean = True); stdcall;

    procedure ImportPointFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportKMLFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportSHPFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportLASFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportLicadGISFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportDWGDXFFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportGEOJSONFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportGMLFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportGPXFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportMIFFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportTABFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportGDBTableFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ImportLandXMLFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;

    procedure ExportNCZ(const CmdLine: TlicgBaseCmdLine; const Filename: string; GIS: TlicgBaseGis = nil); stdcall;
    procedure ExportSHPFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportGMLFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportGPXFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportMIFFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportTABFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportGEOJSONFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportKMLFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportLCNFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportXLSFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;

  end;

  IIE = interface
    ['{B521833D-A627-4140-AA5B-5F44F968268A}']
    function GetIE(CmdLine: TlicgBaseCmdLine): IModuleIE; stdcall;
  end;

  IModuleIE2 = interface
    ['{79208C2F-1E74-422D-9098-E5B4596E13D2}']
    // ilker eklemeler yeni 28.05.202
    procedure ImportDWGDXFFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
    procedure ExportDWGDXFFile(ACmdLine: TlicgBaseCmdLine; AFileName: string); stdcall;
  end;

  IIE2 = interface
    ['{C3E1C0E9-2F33-4C09-958D-3DA912F64AD0}']
    function GetIE(CmdLine: TlicgBaseCmdLine): IModuleIE2; stdcall;
  end;

  IModuleLiSurf = interface
    ['{D2C657BA-B00E-4E97-B09A-5F9964B552F1}']
    function GetPredictedZValue(X, Y: Double; const OmitTriangleModel: Boolean = False): Double; stdcall;
    procedure MakeTriangulation(const GetObjectParams: TTriangleObjectParams;
      const TriangleLayer: string; const MaximumLength: double; const
      SetConstraints: boolean; const MinZ, MaxZ: double); stdcall;
  end;

  IDatabaseModulProperty = interface
    ['{89B61B01-5BC3-42E0-9F5D-BD4E57098B39}']
    function GetCaption: string; stdcall;
    procedure SetCaption(value: string); stdcall;
    function GetValue: variant; stdcall;
    procedure SetValue(value: variant); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(value: Boolean); stdcall;
    function GetDataType: TFieldType; stdcall;
    procedure SetDataType(value: TFieldType); stdcall;
    function GetHint: string; stdcall;
    procedure SetHint(value: string); stdcall;
    property Caption: string read GetCaption write SetCaption;
    property Value: variant read GetValue write SetValue;
    property ReadOnly: Boolean read GetReadOnly;
    property DataType: TFieldType read GetDataType;
    property Hint: string read GetHint write SetHint;
  end;

  TDatabaseModulProperty = class(TInterfacedObject, IDatabaseModulProperty)
  private
    FCaption, FHint: string;
    FValue: Variant;
    FReadOnly: Boolean;
    FDataType: TFieldType;
  public
    function GetCaption: string; stdcall;
    procedure SetCaption(value: string); stdcall;
    function GetValue: variant; stdcall;
    procedure SetValue(value: variant); stdcall;
    function GetReadOnly: Boolean; stdcall;
    procedure SetReadOnly(value: Boolean); stdcall;
    function GetDataType: TFieldType; stdcall;
    procedure SetDataType(value: TFieldType); stdcall;
    function GetHint: string; stdcall;
    procedure SetHint(value: string); stdcall;
  end;

  IDatabaseModulLayerProp = interface
    ['{2FEF3063-AA73-4803-B8CD-CE0F5AF45A74}']
    function GetLayerName: string; stdcall;
    function GetLayerDisplayName: string; stdcall;
    function GetLayerType: integer; stdcall;
    procedure SetLayerName(value: string); stdcall;
    procedure SetLayerDisplayName(value: string); stdcall;
    procedure SetLayerGroupName(value: string); stdcall;
    procedure SetLayerType(value: integer); stdcall;
    property LayerName: string read GetLayerName;
    property LayerDisplayName: string read GetLayerDisplayName;
    property LayerType: integer read GetLayerType;
  end;

  ILayerBatch = interface
    ['{E8AC5921-740E-40BF-B212-FA5E64710AB7}']
    function GetTable(TableName: string; OrderFields: string = ''): TDataset; stdcall;
    //function GetSQLStrings(TableName : string) : TStrings; stdcall;
    //procedure AddEntity(const Entity : IlicgEntity); stdcall;
  end;

  IDatabaseModul_Interface = interface
    ['{783A8517-67AE-4816-BF8E-089EE1A43965}']
    function GetSpatialType: integer; stdcall;
    function GetDbUser: string; stdcall;
    function GetDbPass: string; stdcall;
    function GetDbDatabase: string; stdcall;
    function GetDbHost: string; stdcall;
    function GetDbPort: integer; stdcall;
    procedure SetSpatialType(value: integer); stdcall;
    procedure SetDbUser(value: string); stdcall;
    procedure SetDbPass(value: string); stdcall;
    procedure SetDbDatabase(value: string); stdcall;
    procedure SetDbHost(value: string); stdcall;
    procedure SetDbPort(value: integer); stdcall;
    function GetGis: TObject; stdcall;
    procedure SetGis(const Value: TObject); stdcall;
    function DeleteEntity(const Layer: TlicgBaseLayer; id: Integer; var
      IgnoreAskDelete, CancelDelete: boolean; LookRelation: boolean): boolean; stdcall;
    function UnDeleteEntity(const Layer: TlicgBaseLayer; id: Integer): boolean; stdcall;
    function UpdateGeometryEntity(const Layer: TlicgBaseLayer; const Entity:
      IlicgEntity): boolean; stdcall;
    function SupportEntitiyIDs(const LayerName: string): TlicgEntityIDs; stdcall;
    procedure OznitelikTurleriGetir(EntID: TlicgEntityID; SL: TStrings); stdcall;
    function EditInfoRecord(const Layer: TlicgBaseLayer; const Entity:
      IlicgEntity; _param: string = ''): boolean; stdcall;
    function InitValuesBeforeAddEntity(const Layer: TlicgBaseLayer; const Entity:
      IlicgEntity; const Gis: TObject; FieldList: TStrings; _PropVList: IUnknown):
      boolean; stdcall;
    function GetRelationGeomInfoId(const Layer: TlicgBaseLayer; const Entity:
      IlicgEntity; const Gis: TObject; _Param: string; entityid: integer; Batch:
      ILayerBatch = nil): integer; stdcall;
    function SetupGeometryRelation(const Layer: TlicgBaseLayer; RecNo: integer;
      var infoid: integer; var NewLayer: TlicgBaseLayer; var NewRecNo: integer):
      boolean; stdcall;
    function ResimArchivePath: string; stdcall;
    procedure CompareAndSave_DBAndLayerEntites(const aGis: TlicgBaseGis;
      LayerList: TStrings = nil); stdcall;
    function GetStandartTabakalarI: IInterfaceList; stdcall;
    function GetStandartTabakalar(const aGis: TlicgBaseGis; GroupCap: string; SL:
      IInterfaceList; DontlayerControl: boolean = false): IlicgIntegerList; stdcall;
    procedure AddStandartLayers(const aGis: TlicgBaseGis; const
      _CurrentLayerName: string); stdcall;
    function isAdminUser: boolean; stdcall;
    function Adi: string; stdcall;
    function Soyadi: string; stdcall;
    function User: string; stdcall;
    function Pass: string; stdcall;
    function ModulCommandVisible(const commandId: integer): boolean; stdcall;
    procedure SetConnecctionToPgDataset(const fm: TForm; const Conn: TObject); stdcall;
    procedure ExecSQL(SQLText: string); stdcall;
    procedure ExecSQLWithConn(const Conn: TObject; SQLText: string); stdcall;
    function ModulActionYetkiOK(const aGis: TlicgBaseGis; ModulActionID:
      TModulActionID): boolean; stdcall;
    function TableEditYetkiOK(TableName: string): boolean; stdcall;
    function Create_SQLMenuForm(const aCmdline: TObject; ParentHandle: THandle):
      Pointer; stdcall;
    procedure Destroy_SQLMenuForm(Form: Pointer); stdcall;
    procedure Show_SQLMenuForm(Form: Pointer); stdcall;
    function Showing_SQLMenuForm(Form: Pointer): boolean; stdcall;
    procedure SetActiveNavigationIndex_SQLMenuForm(Form: Pointer; index: integer);
      stdcall;
    procedure SetSize_SQLMenuForm(Form: Pointer; W, H: integer); stdcall;
    property SpatialType: integer read GetSpatialType write SetSpatialType;
    property DbUser: string read getDbUser write setDbUser;
    property DbPass: string read GetDbPass write SetDbPass;
    property DbDatabase: string read GetDbDatabase write SetDbDatabase;
    property DbHost: string read GetDbHost write SetDbHost;
    property DBPort: integer read GetDbPort write SetDbPort;
    property Gis: TObject read GetGis write SetGis;
  end;

  IDatabaseModul_MESInterface = interface(IDatabaseModul_Interface)
    ['{0229FE23-2E64-4A07-87ED-BCD68B6B3AB8}']
    function GetConnectionParams: TConnectionParams; stdcall;
    function GetBinaInfos(binaid: integer; var _il, _ilce, _mah, _adano,
      _parselno, _site, _apt: string; var _ilid, _ilceid, _mahid, _parselid:
      integer): boolean; stdcall;
    function GetParselInfos(parselid: integer; var _il, _ilce, _mah, _adano,
      _parselno: string; var _ilid, _ilceid, _mahid: integer): boolean; stdcall;
    function GetMahalleInfos(mahid: integer; var _il, _ilce, _mah: string; var
      _ilid, _ilceid: integer): boolean; stdcall;
    function GetIlceInfos(ilceid: integer; var _il, _ilce: string; var _ilid:
      integer): boolean; stdcall;
    function GetIlInfos(ilid: integer; var _il: string): boolean; stdcall;

    //function  GetSpatialAdapter: ISpatialAdapter; stdcall;

    //function  AddRecordForEntity(const CmdLine : TlicgBaseCmdline; const Layer: TlicgBaseLayer;
      //   const Entity: IlicgEntity; EntityUid : integer; var InfoId : integer): boolean; stdcall;

    //procedure SetConnectionParams (Value: TConnectionParams); stdcall;
    //function  DatabaseModulConnected : Boolean; stdcall;
    //function  DatabaseModulConnect : Boolean; stdcall;
    //function  SetupGeometryRelation (const Layer: TlicgBaseLayer; RecNo : integer; var infoid : integer;
      // var NewLayer : TlicgBaseLayer; var NewRecNo : integer) : boolean; stdcall;
    //function  GetProperties(const Layer: TlicgBaseLayer; RecNo,InfoId : integer) : IDatabaseModulProperties; stdcall;
    //function  LayerProperites : IDatabaseModulLayerProperites; stdcall;
    //function  GetGis : TObject; stdcall;
    //procedure SetGis (value: TObject); stdcall;

    //procedure OznitelikTurleriGetir(EntID: TlicgEntityID; SL: TStrings);stdcall;
    //function GoInfoRecord(const Cmdline :TlicgBaseCmdLine; EntityUid: integer; tablename : string; _paramstr: string; var infoid: integer) : boolean; stdcall;
    //function  InitValuesBeforeAddEntity(const Layer : TlicgBaseLayer; const Entity: IlicgEntity; const Gis : TObject;
      //var isUndo : boolean): boolean;stdcall;

    //function  GetRelationGeomInfoId(const Layer : TlicgBaseLayer; const Entity: IlicgEntity; const Gis : TObject; _Param : string;
      //entityid : integer; Batch : ILayerBatch = nil) : integer; stdcall;

    function GetRegionWork(var Decsription: string): TlicgExtent; stdcall;
    procedure TestAKTARMA(const Gis: TlicgBaseGis); stdcall;
    function GetIlStrings(const StringsPassData: IStringsPassIntegerData):
      Boolean; stdcall;
    function GetIlceStrings(ilid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GetBeldeStrings(ilceid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GetMahalleStringsByIlce(ilceid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GetMahalleStringsByBelde(beldeid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GoIlByMalik(const Gis: TlicgBaseGis; ilid, malikid: integer; var
      layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIlceByMalik(const Gis: TlicgBaseGis; ilceid, malikid: integer;
      var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoBeldeByMalik(const Gis: TlicgBaseGis; beldeid, malikid: integer;
      var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoMahalleByMalik(const Gis: TlicgBaseGis; mahid, malikid: integer;
      var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIlByMusteri(const Gis: TlicgBaseGis; ilid, Musteriid: integer;
      var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIlceByMusteri(const Gis: TlicgBaseGis; ilceid, Musteriid: integer;
      var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoBeldeByMusteri(const Gis: TlicgBaseGis; beldeid, Musteriid:
      integer; var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoMahalleByMusteri(const Gis: TlicgBaseGis; mahid, Musteriid:
      integer; var layer, ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIlAll(const Gis: TlicgBaseGis; ilid: integer; var layer,
      ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIlceAll(const Gis: TlicgBaseGis; ilceid: integer; var layer,
      ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoBeldeAll(const Gis: TlicgBaseGis; beldeid: integer; var layer,
      ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoMahalleAll(const Gis: TlicgBaseGis; mahid: integer; var layer,
      ParselLayer: TlicgBaselayer; const StringsPassData:
      IStringsPassIntegerData): integer; stdcall;
    function GoIl(const Gis: TlicgBaseGis; ilid: integer; var layer:
      TlicgBaselayer): integer; stdcall;
    function GoIlce(const Gis: TlicgBaseGis; ilceid: integer; var layer:
      TlicgBaselayer): integer; stdcall;
    function GoBelde(const Gis: TlicgBaseGis; beldeid: integer; var layer:
      TlicgBaselayer): integer; stdcall;
    function GoMahalle(const Gis: TlicgBaseGis; mahid: integer; var layer:
      TlicgBaselayer): integer; stdcall;
    function GetAdaParselList(const Gis: TlicgBaseGis; ada, parsel: string; var
      layer: TlicgBaselayer; const StringsPassData: IStringsPassIntegerData):
      boolean; stdcall;
    function GetLabelFields(const Layer: TlicgBaselayer; var LabelFieldNames:
      string): boolean; stdcall;
    procedure EditDokumanPath; stdcall;
    function SaveParselGoogleGoruntuByMalik(infoid: integer; sorgutipi: integer;
      MusteriId, MalikId: integer; SL: TStrings): boolean; stdcall; // sorgutipi -> 0  : il; 1: ilce,2:belde, 3:mah, 4: parsel
    function SaveParselGoogleGoruntuByMusteri(infoid: integer; sorgutipi:
      integer; MusteriId: integer; SL: TStrings): boolean; stdcall; // sorgutipi -> 0  : il; 1: ilce,2:belde, 3:mah, 4: parsel
    function GetMalikId(id: integer = -1; param: integer = 0): integer; stdcall;
    function GetMusteriId: integer; stdcall;
    function GetMalikIdByMusteri(var musteriId: integer; id: integer = -1; param:
      integer = 0): integer; stdcall;
    procedure TableEdit(TableName: string); stdcall;
    procedure RunDefinedSql(dsqlname: string); stdcall;
    procedure GetlengthArea(const AEnt: IlicgEntity; var Area, length: double); stdcall;
    procedure GetArea(const AEnt: IlicgEntity; var Area: double); stdcall;
    procedure GetLength(const AEnt: IlicgEntity; var Length: double); stdcall;
    procedure Ucretlendirme; stdcall;
    function ilceSorByMahEntity(const mahE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function MahSorByMahEntity(const mahE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function ParselSorByParselEntity(const parE: IlicgEntity; const
      StringsPassData: IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function MahSorByParselEntity(const parE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function ilceSorByilceEntity(const ilceE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function ilSorByilEntity(const ilE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function binaSorBybinaEntity(const binaE: IlicgEntity; const StringsPassData:
      IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function beldeSorBybeldeEntity(const beldeE: IlicgEntity; const
      StringsPassData: IStringsPassIntegerData; var ecount: integer): boolean; stdcall;
    function EditAlternatifAdaNo(const CmdL: TlicgBaseCmdLine): boolean; stdcall;
    procedure PasifParseller; stdcall;
    procedure PackEntityTables; stdcall;
    procedure TempLayerRelationWizard(const AGis: TlicgBaseGis); stdcall;
    function _GetIDCounter(tablename: string): integer; stdcall;
    function Login(user, pass: string; var mess: string): boolean; stdcall;
    procedure AddActionListToDB(Action_name: string); stdcall;
    // Kullanýcý yönetimi ve yetkileri
    procedure UserManagement; stdcall;
    function GetCadUser: string; stdcall;
    procedure ChangeUserPassword; stdcall;
    function Auth_DigerCadDosyalarindanVeriAktar(auth_param: integer): boolean; stdcall;
    function Auth_IlSiniriCiz(auth_param: integer): boolean; stdcall;
    function Auth_IlceSiniriCiz(auth_param: integer): boolean; stdcall;
    function Auth_LicaddeSiniriCiz(auth_param: integer): boolean; stdcall;
    function Auth_MahalleSiniriCiz(auth_param: integer): boolean; stdcall;
    function Auth_ParselCiz(auth_param: integer): boolean; stdcall;
    function Auth_BinaCiz(auth_param: integer): boolean; stdcall;
    function Auth_KapiCiz(auth_param: integer): boolean; stdcall;
    function Auth_YolCiz(auth_param: integer): boolean; stdcall;
    function Auth_ilan_emlak_Ciz(auth_param: integer): boolean; stdcall;
    function Auth_DokumanYolu(auth_param: integer): boolean; stdcall;
    function Auth_Malikler(auth_param: integer): boolean; stdcall;
    function Auth_Musteriler(auth_param: integer): boolean; stdcall;
    function Auth_UcretlendirmeIslemleri(auth_param: integer): boolean; stdcall;
    function Auth_Takyidat(auth_param: integer): boolean; stdcall;
    function Auth_Gorusler(auth_param: integer): boolean; stdcall;
    function Auth_EmlakVergisi(auth_param: integer): boolean; stdcall;
    function Auth_AdaNosuOlmayanParsellericinAlternatifAdaNoBelirle(auth_param:
      integer): boolean; stdcall;
    function Auth_PasifParseller(auth_param: integer): boolean; stdcall;
    function Auth_NesneGeometriTablosuBakim(auth_param: integer): boolean; stdcall;
    function Auth_Tapu(auth_param: integer): boolean; stdcall;
    function Auth_Plan(auth_param: integer): boolean; stdcall;
    function Auth_Dokuman(auth_param: integer): boolean; stdcall;
    function Auth_Raporlar(auth_param: integer): boolean; stdcall;
  end;

  IDatabaseModul_LicadInterface = interface(IDatabaseModul_Interface)
    ['{D8352D7A-A567-4931-AD4E-4C8F335C9A09}']
    //  User
    // parsel
    function SQLExecuteAdaParsel(const ada, parsel: string; StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function SQLExecuteAdaParselByRuhsatNo(const ruhsat_No: string;
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function SQLExecuteAdaParselByIskanBelge(const belge_No: string;
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function SQLExecuteAdaParselById(const id: integer; StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetAdaParselProperties(id: integer; Properties: IInterfaceList):
      string; stdcall;
    function GetParselEntityById(id: integer): IlicgEntity; stdcall;

  //bina
    function SQLExecuteBinaById(const id: integer; Properties: IInterfaceList;
      var Soyut_id: integer): string; stdcall;
    function SQLExecuteBinaByBinaKodu(const binakodu: integer; Properties:
      IInterfaceList): string; stdcall;
    function SQLExecuteBagimsizByYapiId(const id: integer; Properties:
      IInterfaceList): string; stdcall;
    function GetSonBinaResim(const id: integer): string; stdcall;
    function GetBinaResimlerString(const id: integer): IStringsPassIntegerData; stdcall;
    function GetBinaResimlerStringByKapiId(const id: integer):
      IStringsPassIntegerData; stdcall;
    function GetBinaSonResimStringByKapiId(const id: integer): string; stdcall;
    function GetKapiResimlerString(const id: integer): IStringsPassIntegerData; stdcall;

    //mahalle
    function GetMahalleStrings(const StringsPassData: IStringsPassIntegerData):
      string; stdcall;
    function GetYolStringsByMahalleId(const MahId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetYolStringsByMahalleAdi(const MahAdi: string; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetYolStringsByMahalleId_LikeYolAdi(const MahId: integer; _likeYol:
      string; const StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetYolStringsByYolID(const StringsPassData: IStringsPassIntegerData):
      string; stdcall;
    function GetYolStringsByYolOrtaHatID(const StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetYolStringsByYolOrtaHatYonID(const StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetMahalleYolStringsByYolAdi(const YolAdi: string; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetYolStringsByLikeYolAdi(_likeYol: string; const StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetMahalleByYolOrtaHatID(const yohId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetMahalleByNumaratajID(const nId: integer; const StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetKapiStringsByYolOrtaYonId(const YolId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetKapiStringsByYolId(const YolId: integer; const StringsPassData:
      IStringsPassIntegerData): string; stdcall;
    function GetKapiStringsByYolOrtaHatId(const YolId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetKapiStringsByMahalleId(const MahId, YolId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;
    function GetKapiNosStringsBySoyutYapiId(const soyutId: integer; const
      StringsPassData: IStringsPassIntegerData): string; stdcall;

    //Genel Sorgular
    function ShowSqlFormByMalikBina: string; stdcall;
    function ShowSqlFormByMalikTapu: string; stdcall;
    function ShowSqlFormByMalikTum: string; stdcall;
    function GetParselIdNULLInTapu: string; stdcall;
    function GetYolRectByYolOrtaHatYonId(id: integer): TlicgExtent; stdcall;
    function GetKapiRectByNumaratajID(id: integer): TlicgExtent; stdcall;
    function GetYolEntityByYolOrtaHatYonId(id: integer): IlicgEntity; stdcall;
    function GetYolEntityByYolId(id: integer): IlicgEntity; stdcall;
    function GetKapiEntityByNumaratajID(id: integer): IlicgEntity; stdcall;
    function GetYapiEntityByid(id: integer): IlicgEntity; stdcall;
    function GetMahalleId(const Entity: IlicgEntity): integer; stdcall;
    function GetMahalles_YolIntersects(const Entity: IlicgEntity):
      IStringsPassIntegerData; stdcall;
    function GetParselId(const Entity: IlicgEntity; var ada_no, parsel_no:
      string): integer; stdcall;
    function GetSoyutYapiId(const Entity: IlicgEntity): integer; stdcall;
    function BagimsizBolumEditByYapiId(const yapi_id: integer): boolean;
    //

    procedure ShowListMahYolKapiMalik(binakodu: integer); stdcall;
    function SelectParselFromMap: integer; stdcall;
    function SelectBinaFromMap: integer; stdcall;
  end;

  IDatabaseModul_EMLAKInterface = interface(IDatabaseModul_Interface)
    ['{4233408A-34E2-4FD8-AE23-2A51FD4BFD9D}']
    function GetIlStrings(const StringsPassData: IStringsPassIntegerData):
      Boolean; stdcall;
    function GetIlceStrings(ilid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GetMahalleStringsByIlce(ilceid: integer; const StringsPassData:
      IStringsPassIntegerData): Boolean; stdcall;
    function GoIlAll(const Gis: TlicgBaseGis; ilid: integer; const
      StringsPassData: IStringsPassIntegerData): boolean; stdcall;
    function GoIlceAll(const Gis: TlicgBaseGis; ilceid: integer; const
      StringsPassData: IStringsPassIntegerData): boolean; stdcall;
    function GoMahalleAll(const Gis: TlicgBaseGis; mahid: integer; const
      StringsPassData: IStringsPassIntegerData): boolean; stdcall;
    function GoIl(const Gis: TlicgBaseGis; ilid: integer): TlicgExtent; stdcall;
    function GoIlce(const Gis: TlicgBaseGis; ilceid: integer): TlicgExtent; stdcall;
    function GoMahalle(const Gis: TlicgBaseGis; mahid: integer): TlicgExtent; stdcall;
    function GoParsel(const Gis: TlicgBaseGis; parselid: integer): TlicgExtent; stdcall;
    function GetAdaParselList(const Gis: TlicgBaseGis; ada, parsel: string;
      const StringsPassData: IStringsPassIntegerData): boolean; stdcall;
    procedure TableEdit(TableName: string); stdcall;
    function GetPortfoyProperties(const id: integer; Properties: IInterfaceList;
      var R: TlicgExtent): string; stdcall;
    function FindPortfoyId(const portfoyNo: integer; var R: TlicgExtent): integer; stdcall;
  end;

implementation

uses
  Variants;

{ TPassDrawBoxEvents }

constructor TPassDrawBoxEvents.Create;
begin
  OnMouseDown2D := nil;
  OnMouseMove2D := nil;
  OnMouseUp2D := nil;
  OnClick := nil;
  OnDblClick := nil;
end;

{ TDatabaseModulProperty }

function TDatabaseModulProperty.GetCaption: string;
begin
  Result := FCaption;
end;

function TDatabaseModulProperty.GetDataType: TFieldType;
begin
  Result := FDataType;
end;

function TDatabaseModulProperty.GetHint: string;
begin
  Result := FHint;
end;

function TDatabaseModulProperty.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TDatabaseModulProperty.GetValue: variant;
begin
  Result := FValue;
end;

procedure TDatabaseModulProperty.SetCaption(value: string);
begin
  FCaption := value;
end;

procedure TDatabaseModulProperty.SetDataType(value: TFieldType);
begin
  FDataType := value;
end;

procedure TDatabaseModulProperty.SetHint(value: string);
begin
  FHint := Value;
end;

procedure TDatabaseModulProperty.SetReadOnly(value: Boolean);
begin
  FReadOnly := value;
end;

procedure TDatabaseModulProperty.SetValue(value: variant);
begin
  FValue := value;
end;

end.


