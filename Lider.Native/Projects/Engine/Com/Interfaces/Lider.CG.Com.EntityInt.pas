unit Lider.CG.Com.EntityInt;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Graphics,
  Classes,
  Lider.CG.Com.stMatrix,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.CSInt;

type
  TIntersectionType = (itCross, itTouch, itCrossTouch);
  // idNone entity sadece veritabaný bilgisine sahip olan nesnedir. not ilker
  TlicgEntityID = (idNone = 0, idPoint = 1, idPlace = 2, idPolyline = 3, idPolygon = 4,
    idRectangle = 5, idArc = 6, idEllipse = 7, idPolyEllipse = 8, idPersistBitmap = 9,
    idPolyRectangle = 10, idSpline = 11, idPolyArc = 12, idGroup = 13, idText = 14,
    idTextM = 15, idTextFX = 16, idVectorialText = 17, idVectorialFittedText = 18,
    idDimHorizontal = 19, idDimVertical = 20, idDimParallel = 21, idLine = 22, idTriangle = 23,
    idPafta = 24, idCircle = 25, idOle = 26, idPreview = 27, idBlockInsert= 28);

  TlicgEntityIDs = set of TlicgEntityID;

const
  NoneEntityIDs: TlicgEntityIDs = [idNone];

  AllEntityIDs: TlicgEntityIDs = [idNone, idPoint, idPlace,
    idLine, idPolyline, idPolygon, idRectangle,
    idPolyRectangle, idArc, idPolyArc, idEllipse, idPolyEllipse, idPersistBitmap,
    idSpline, idGroup, idText, idTextM, idTextFX,
    idVectorialText, idVectorialFittedText, idDimHorizontal, idDimVertical,
    idDimParallel, idTriangle, idPafta, idCircle, idOle, idPreview, idBlockInsert];

  AllEntityPickIDs: TlicgEntityIDs = [idPoint, idPlace,
    idLine, idPolyline, idPolygon, idRectangle,
    idPolyRectangle, idArc, idPolyArc, idEllipse, idPolyEllipse, idPersistBitmap,
    idSpline, idGroup, idText, idVectorialText, idVectorialFittedText, idTriangle,
    idPafta, idCircle, idOle, idBlockInsert];

  LineEntityIDs: TlicgEntityIDs = [idLine, idPolyline, idArc, idPolyArc, idSpline];

  PolygonEntityIDs: TlicgEntityIDs = [idPolygon, idRectangle, idPolyRectangle, idEllipse, idPolyEllipse,
  idTriangle, idPafta, idCircle];

  NonAerialEntities = [idNone, idPersistBitmap, idVectorialText,
    idVectorialFittedText, idDimHorizontal, idDimVertical, idDimParallel, idPreview];

  TextEntityIDs = [idTextFX, idText, idTextM, idVectorialText, idVectorialFittedText];

  VectTextEntityIDs =[idVectorialText, idVectorialFittedText];

  TrueTextEntityIDs =[idTextFX, idText, idTextM];

  ImageEntityIDs =[idPersistBitmap];

  PrintEntityIDs =[idPoint, idPlace, idLine, idPolyline,
    idPolygon, idRectangle, idPolyRectangle,
    idArc, idPolyArc, idEllipse, idPolyEllipse, idPersistBitmap, idSpline,
    idGroup, idText, idVectorialText, idVectorialFittedText, idTriangle,
    idPafta, idCircle, idOle, idBlockInsert];

  CanGrowEntityIDs: TlicgEntityIDs = [idLine, idPolyline, idPolygon,
    idTriangle, idPafta, idSpline];

type

 { IlicgVectorFonts the list of fonts }
  IlicgVectorFonts = interface
    ['{DFCFED2D-9B5E-42BF-B09A-F6A632EAFA0D}']
    function isExistsByFilename(Filename: string): Boolean; stdcall;
    function Count: Integer; stdcall;
    { add a font file (Arial.fnt) to the list}
    procedure AddFontFile(const FileName: string); stdcall;
    procedure AddFont(const VectorFont: IlicgVectorFont); stdcall;
    { Clear the list of fonts}
    procedure Clear; stdcall;
    { delete a font by ordinal position }
    procedure Delete(Index: Integer); stdcall;
    { delete a font given a font name }
    procedure DeleteByName(const FontName: string); stdcall;
    { Given a font name ("Arial") this function returns the
      IlicgVectorFont associated with that name.
      If the given name is not found, the default font is returned }
    function FontByName(const FontName: string): IlicgVectorFont; stdcall;
    { this function return the vector font given an index on the
      list of fonts }
    function FontByIndex(Index: Integer): IlicgVectorFont; stdcall;
    { given a vector font, this function return the position on the list
      of fonts }
    function IndexOfFont(const VectorFont: IlicgVectorFont): Integer; stdcall;
    { given a font name, this function return the position on the
      list of fonts }
    function IndexOfFontByName(const FontName: string): Integer; stdcall;
    { This procedure defines the default font by name
      is the font name is not found, the 0 position font is used }
    procedure SetDefaultFontByName(const FontName: string); stdcall;
    { set the default font by ordinal position }
    procedure SetDefaultFontByIndex(Index: Integer); stdcall;
    { populates a TStrings with the list of font names }
    procedure PopulateTo(Strings: TStrings); stdcall;
    function Get(Index: Integer): IlicgVectorFont; stdcall;
    property Items[Index: Integer]: IlicgVectorFont read Get; default;
    function GetNullChar: IlicgVectorChar; stdcall;
    property NullChar: IlicgVectorChar read GetNullChar;
    function GetDefaultFont: IlicgVectorFont; stdcall;
    property DefaultFont: IlicgVectorFont read GetDefaultFont;
  end;

  // Hatch patterns
  PlicgHatchData = ^TlicgHatchData;

  TDushValues = array[0..10] of Double;

  TlicgHatchData = packed record
    Angle: Double;
    Origin: TlicgCoor;
    Delta: TlicgCoor;
    NumDashes: Integer;
    Dashes: TDushValues;
  end;

  IlicgEntity = interface;
  IlicgOleEntity = interface;
  IlicgPersistBitmap = interface;
  IlicgPreviewEntity = interface;

  TBaseHatch = class
  public
    procedure Clear; virtual; abstract;
    procedure Add(const Hatch: TlicgHatchData); virtual; abstract;
{    procedure DrawHatchTo( const Vector: IlicgVector; const Clip, Extent: TlicgExtent;
                           Grapher: TlicgBaseGrapher; Canvas: IlicgCanvas;
                           Color: TColor; const Scale, Angle: Double;
                           const Matrix: TBMatrix);                            stdcall;
 }
    procedure getLineVector(cmdline: Tobject; HatchEnt: IlicgEntity; Scale,
      _Angle: Double; LineVector, PointVector: IlicgVector; rotMatrix:
      TlicgMatrix; LineColor: TColor = clGray; isShowProgress: Boolean = false);
      virtual; abstract;
    function Count: Integer; virtual; abstract;
    function getName: string; virtual; abstract;
    procedure setName(value: string); virtual; abstract;
    property Name: string read getName write setName;
    function getDescription: string; virtual; abstract;
    procedure setDescription(value: string); virtual; abstract;
    property Description: string read getDescription write setDescription;
    function Get(Index: Integer): TlicgHatchData; virtual; abstract;
    procedure Put(Index: Integer; const Value: TlicgHatchData); virtual; abstract;
    property Items[Index: Integer]: TlicgHatchData read Get write Put; default;
  end;

  IlicgHatchList = interface
    ['{BC07904B-C9D8-4524-822D-1F60F418E807}']
    function Add: TBaseHatch; stdcall;
    function Count: Integer; stdcall;
    procedure Clear; stdcall;
    procedure AddPATFile(const FileName: string); stdcall;
    function Get(Index: Integer): TBaseHatch; stdcall;
    procedure Put(Index: Integer; const Value: TBaseHatch); stdcall;
    property Items[Index: Integer]: TBaseHatch read Get write Put; default;
  end;

  IlicgEntityList = interface
    ['{02000EDF-2852-4371-B215-620842FDE7A6}']
    function Get(Index: Integer): IlicgEntity;
    procedure _Set(Index: Integer; value: IlicgEntity);
    function GetList: IInterfaceList;
    function GetRecNoItems(Index: Integer): Integer;
    procedure SetRecNoItems(Index: Integer; Value: Integer);
    function Add(Ent: IlicgEntity): Integer; stdcall;
    function AddIfNotFound(Ent: IlicgEntity): Integer; stdcall;
    function AddEntity(AEntity: IlicgEntity; ARecNo: Integer): Integer; stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure Extract(Index: Integer); stdcall;
    procedure ExtractAll; stdcall;
    procedure Insert(Index: Integer; Ent: IlicgEntity); stdcall;
    procedure Replace(Index: Integer; Ent: IlicgEntity); stdcall;
    function IndexOf(Item: IlicgEntity): Integer; stdcall;
    property Items[Index: Integer]: IlicgEntity read Get write _Set; default;
    property List: IInterfaceList read GetList;
    property RecNoItems[Index: Integer]: Integer read GetRecNoItems write SetRecNoItems;
  end;

  IlicgEntity = interface
    ['{F348493F-EEF9-4AB2-80A2-62641FD556FA}']
    // IlicgEntity Interface
    function _ClassName: string; stdcall;
    function GetGISOwner: TObject; stdcall;
    function Clone: IlicgEntity; stdcall;
    procedure Assign(Source: IlicgEntity); stdcall;
    function IsVisible(const Clip: TlicgExtent): Boolean; stdcall;
    function IsClosed: Boolean; stdcall;
    function Extent: TlicgExtent; stdcall;
    function Extent4D: TlicgExtent4D; stdcall;

    { <supheliler> }
    function CompareAgainst(Entity: IlicgEntity; operator: TlicgGraphicOperator): Boolean; stdcall;
    function IntersectEntity(OtherEntity: IlicgEntity; ConsidereFullyInside:
      Boolean = True; IntType: TIntersectionType = itCrossTouch): Boolean; stdcall;
    function Centroid: TlicgCoor; stdcall; // ilker deðiþtirme
    function CenterOfMass: TlicgCoor; stdcall;
    function IsInsideEntity(OtherEntity: IlicgEntity; FullInside: Boolean): Boolean; stdcall;
    { </supheliler> }
    function NeedReposition: Boolean; stdcall; {Belki filebased sonrasý kalkar}
    function StorageSize: Integer; stdcall; {Belki filebased sonrasý kalkar}
    function IsPolygon: Boolean; stdcall;
    function IsPolyline: Boolean; stdcall;
    procedure ApplyProject(const _GisOwner: TObject; const InPrj, OutPrj:
      IlicgCS; MatrixElems: PAffineMatrixElements; isAfter: Boolean = True;
      isOnlyPoints: Boolean = False); stdcall;

    { IlicgGeometry geometrik iþlemlerinin wrapperleri }
    // Ýliþkiler, Baðlantýlar Relate Baþlama
    function InPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesnenin içinde mi? (Kenarlar Hariç).
    function OnPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesene Üstünde mi? (Kenarlarýn üstündemi, Kapalý alanýn içi Hariç)

    function Equality(const BEntity: IlicgEntity): Boolean; stdcall;
    function Disjoint(const BEntity: IlicgEntity): Boolean; stdcall;
    function Intersect(const BEntity: IlicgEntity): Boolean; stdcall;
    function Touch(const BEntity: IlicgEntity): Boolean; stdcall;
    function Cross(const BEntity: IlicgEntity): Boolean; stdcall;
    function Within(const BEntity: IlicgEntity): Boolean; stdcall;
    function Contains(const BEntity: IlicgEntity): Boolean; stdcall;
    function Overlap(const BEntity: IlicgEntity): Boolean; stdcall;
    function Relate(const BEntity: IlicgEntity; const A_de9im: string): Boolean; stdcall;
    // Ýliþkiler, Baðlantýlar Relate Bitiþ

    function MakeBuffer(const ADist: Double; const Api2_Points: Integer = 9): IlicgEntity; stdcall;
    function MakeOffset(const ADist: Double; AJoinType: TlicgJoinType; AEndType: TlicgEndType; AMiterLimit: Double = -1; AArcTolerance: Double = -1): IlicgEntity; stdcall;

    function ConvexHull(const BEntity: IlicgEntity = nil; const AFix: Boolean = True): IlicgEntity; stdcall;
    function ConcaveHull(const BEntity: IlicgEntity = nil; const AAlpha: Double = 0; const AFix: Boolean = True): IlicgEntity; stdcall;

    function Intersection(const BEntity: IlicgEntity): IlicgEntity; stdcall;
    function Difference(const BEntity: IlicgEntity): IlicgEntity; stdcall;
    //function Distance(const BEntity: IlicgEntity): Double; stdcall; ilker silme
    function SymmetricalDifference(const BEntity: IlicgEntity): IlicgEntity; stdcall;
    function Union(const BEntity: IlicgEntity): IlicgEntity; stdcall;

    function AsPoint: IlicgPoint; stdcall;
    function AsPolyline: IlicgPolyline; stdcall;
    function AsSpline: IlicgSpline; stdcall;
    function AsArc: IlicgArc; stdcall;
    function AsCircle: IlicgCircle; stdcall;
    function AsEllipse: IlicgEllipse; stdcall;
    function AsRectangle: IlicgRectangle; stdcall;
    function AsPlace: IlicgPlace; stdcall;
    function AsPolygon: IlicgPolygon; stdcall;
    function AsGroupGeometry: IlicgGroupGeometry; stdcall;
    function AsPersistBitmap: IlicgPersistBitmap; stdcall;
    function AsOleEntity: IlicgOleEntity; stdcall;
    function AsLine: IlicgLine; stdcall;
    function AsTriangle: IlicgTriangle; stdcall;
    function AsPafta: IlicgPafta; stdcall;
    function AsBlockInsert: IlicgBlockInsert; stdcall;
    function AsPreviewEntity: IlicgPreviewEntity; stdcall;
    function AsTextValue: IlicgTextValue; stdcall;
    function AsText: IlicgText; stdcall;
    function AsTextM: IlicgTextM; stdcall;
    function AsTextFX: IlicgTextFX; stdcall;
    function AsVectorialText: IlicgVectorialText; stdcall;
    function AsVectorialFittedText: IlicgVectorialFittedText; stdcall;

    // Property function and procedure
    function GetControlPointsToShow: TlicgControlPointTypes; stdcall;
    procedure SetControlPointsToShow(const Value: TlicgControlPointTypes); stdcall;
    function GetEntityID: TlicgEntityID; stdcall;
    function GetPainterObject: IInterface; stdcall;
    procedure SetPainterObject(const Value: IInterface); stdcall;
    function GetGeometry: IlicgGeometry; stdcall;
    procedure SetGeometry(const Value: IlicgGeometry); stdcall;
    function GetSelectedVertices: IlicgIntegerList; stdcall;
    procedure SetSelectedVertices(const Value: IlicgIntegerList); stdcall;
    function GetDrawTools: IlicgDrawTools; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GetVersion: Byte; stdcall;
    procedure SetVersion(const Value: Byte); stdcall;
    function GetLayer: TObject; stdcall;
    procedure SetLayer(const Value: TObject); stdcall;
    function GetApplyLayerProps: Boolean; stdcall;
    procedure SetApplyLayerProps(Value: Boolean); stdcall;
    procedure SetOriginalSize(const Value: Integer); stdcall;
    function GetParamString: string; stdcall;
    procedure SetParamString(Value: string); stdcall;
    function GetLabelString: string; stdcall;
    procedure SetLabelString(Value: string); stdcall;
    function GetParamIntegerList: IlicgIntegerList; stdcall;
    procedure SetParamIntegerList(Value: IlicgIntegerList); stdcall;

    property ControlPointsToShow: TlicgControlPointTypes read GetControlPointsToShow write SetControlPointsToShow;
    property EntityID: TlicgEntityID read GetEntityID;
    property PainterObject: IInterface read GetPainterObject write SetPainterObject;
    property Geometry: IlicgGeometry read GetGeometry write SetGeometry;
    property SelectedVertices: IlicgIntegerList read GetSelectedVertices write SetSelectedVertices;
    property DrawTools: IlicgDrawTools read GetDrawTools;
    property Name: string read GetName write SetName;
    property GroupName: string read GetGroupName write SetGroupName;
    property Version: Byte read GetVersion write SetVersion;
    property Layer: TObject read GetLayer write SetLayer;
    property ApplyLayerProps: Boolean read GetApplyLayerProps write SetApplyLayerProps;
    property OriginalSize: Integer write SetOriginalSize;
    property ParamString: string read GetParamString write SetParamString;
    property LabelString: string read GetLabelString write SetLabelString;
    property ParamIntegerList: IlicgIntegerList read GetParamIntegerList write SetParamIntegerList;
  end;

  IlicgOleEntity = interface(IlicgEntity)
    ['{08AA1A8F-0953-49CE-8254-F5AA024CCC04}']
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    property FileName: string read GetFileName write SetFileName;
  end;

  IlicgPersistBitmap = interface(IlicgEntity)
    ['{0B5992AB-425A-4C23-A253-C57CFB382C87}']
    procedure ReadGeneric(EditFileName: string); overload; stdcall;
    procedure ReadGeneric(const Stream: TStream); overload; stdcall;
    function GetTransparent: Boolean; stdcall;
    procedure SetTransparent(const Value: Boolean); stdcall;
    function GetPersistBitmap: TBitmap; stdcall;
    function getGraphicLink: TObject; {TlicgGraphicLink} stdcall;
    procedure setGraphicLink(const Value: TObject); stdcall;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property PersistBitmap: TBitmap read GetPersistBitmap;
    property GraphicLink: TObject read getGraphicLink write setGraphicLink;
  end;

  TlicgPreviewPresentation = (ppQuality, ppDraft);

  TlicgPrintMode = (pmAll, pmSelection, pmExcludeSelection);

  IlicgPreviewEntity = interface(IlicgEntity)
    ['{B13482B8-BBBE-4C20-B710-69D88F11801F}']
    function GetFileNo: Integer; stdcall;
    procedure SetFileNo(const Value: Integer); stdcall;
    function GetCalculatedPrintArea: TlicgExtent; stdcall;
    function GetDrawingUnits: Double; stdcall;
    function GetPaperUnits: TlicgScaleUnits; stdcall;
    function GetPlottedUnits: Double; stdcall;
    function GetPresentation: TlicgPreviewPresentation; stdcall;
    function GetPrintFrame: Boolean; stdcall;
    function GetPrintMode: TlicgPrintMode; stdcall;
    function GetProposedPrintArea: TlicgExtent; stdcall;
    procedure SetCalculatedPrintArea(const Value: TlicgExtent); stdcall;
    procedure SetDrawingUnits(const Value: Double); stdcall;
    procedure SetPaperUnits(const Value: TlicgScaleUnits); stdcall;
    procedure SetPlottedUnits(const Value: Double); stdcall;
    procedure SetPresentation(const Value: TlicgPreviewPresentation); stdcall;
    procedure SetPrintFrame(const Value: Boolean); stdcall;
    procedure SetPrintMode(const Value: TlicgPrintMode); stdcall;
    procedure SetProposedPrintArea(const Value: TlicgExtent); stdcall;
    function getPrintMatrix: TlicgMatrix; stdcall;
    procedure setPrintMatrix(value: TlicgMatrix); stdcall;
    function GetPreviewGIS: TObject; stdcall;
    procedure CalculateScales(const WindowToFit: TlicgExtent); stdcall;
    function CreateGrapherFrom(Grapher: TObject): TObject; stdcall;
    property GIS: TObject read GetPreviewGIS;
    property FileNo: Integer read GetFileNo write SetFileNo;
    property PlottedUnits: Double read GetPlottedUnits write SetPlottedUnits;
    property DrawingUnits: Double read GetDrawingUnits write SetDrawingUnits;
    property PaperUnits: TlicgScaleUnits read GetPaperUnits write SetPaperUnits;
    property PrintMode: TlicgPrintMode read GetPrintMode write SetPrintMode;
    property PrintFrame: Boolean read GetPrintFrame write SetPrintFrame;
    property Presentation: TlicgPreviewPresentation read GetPresentation write
      SetPresentation;
    property ProposedPrintArea: TlicgExtent read GetProposedPrintArea write
      SetProposedPrintArea;
    property CalculatedPrintArea: TlicgExtent read GetCalculatedPrintArea write
      SetCalculatedPrintArea;
    property PrintMatrix: TlicgMatrix read getPrintMatrix write setPrintMatrix;
  end;

  { for autolabeling position in event OnLabelEntity }
  TlicgLabelPos = (lpCenter, lpCenterUp, lpUpperLeft, lpUpperRight, lpCenterLeft,
    lpCenterRight, lpLowerLeft, lpCenterDown, lpLowerRight);

  (*uður ekleme baþlama
  TlicgTextPos = (tpCenter, tpCenterUp, tpUpperLeft, tpUpperRight, tpCenterLeft,
    tpCenterRight, tpLowerLeft, tpCenterDown, tpLowerRight, tpCenterUpOut, tpUpperLeftOutUp,
    tpUpperRightOutUp, tpCenterLeftOut, tpCenterRightOut, tpLowerLeftOutDown, tpCenterDownOut,
    tpLowerRightOutDown, tpUpperLeftOutLeft, tpUpperRightOutRigth, tpLowerLeftOutLeft,
    tpLowerRightOutRigth, tpUpperLeftCorner, tpUpperRightCorner, tpLowerLeftCorner,
    tpLowerRightCorner);
  uður ekleme bitiþ*)

  IlicgSymbols = interface;


  { The symbols used in the system }
  IlicgSymbol = interface
    ['{CEFC9258-6496-44CE-BCF7-A0EC82B129D3}']
    function GetSymbols: IlicgSymbols; stdcall;
    procedure SetSymbols(value: IlicgSymbols); stdcall;
    function GetEntityList: IlicgEntityList; stdcall;
    function GetInsertionPoint: TlicgCoor; stdcall;
    procedure SetInsertionPoint(value: TlicgCoor); stdcall;
    function GetEntities(Index: Integer): IlicgEntity; stdcall;
    procedure ConfigureSpecialItems(Item: IlicgEntity); stdcall;
    function GetCentroid: TlicgCoor; stdcall;
    function GetExtension: TlicgExtent; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure Assign(Symbol: IlicgSymbol); stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    function Add(Item: IlicgEntity): Integer; stdcall;
    procedure Insert(Index: Integer; Item: IlicgEntity); stdcall;
    procedure Delete(Index: Integer); stdcall;
    function IndexOf(Item: IlicgEntity): Integer; stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure UpdateExtension; stdcall;
    property Name: string read GetName write SetName;
    property Extension: TlicgExtent read GetExtension;
    property Centroid: TlicgCoor read GetCentroid;
    property InsertionPoint: TlicgCoor read GetInsertionPoint write SetInsertionPoint;
    property Entities[Index: Integer]: IlicgEntity read GetEntities;
    property EntityList: IlicgEntityList read GetEntityList;
    property Symbols: IlicgSymbols read GetSymbols write SetSymbols;
  end;

  IlicgLineType = interface(IlicgSymbol)
    ['{2C860EC4-7CD6-4040-91BA-7AF0AC8D4F26}']
    function CalculateWorkScale(_Scale: Double): TStMatrix; stdcall;
    procedure InverseCalculateWorkScale(const stM: TStMatrix); stdcall;
  end;

  IlicgSymbolsList = interface
    ['{7A07C2AB-F7AF-4FC5-90B6-06A7F7E55903}']
    function GetSymbols(Index: Integer): IlicgSymbols;
    procedure SetSymbols(Index: Integer; value: IlicgSymbols);
    function GetList: IInterfaceList;
    function Add(ASymbols: IlicgSymbols): Integer; stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    function IndexOf(Item: IlicgSymbols): Integer; stdcall;
    property Items[Index: Integer]: IlicgSymbols read GetSymbols write SetSymbols; default;
    property List: IInterfaceList read GetList;
  end;

  { IlicgSymbols }
  IlicgSymbols = interface
    ['{1C6EB3C8-4B6D-4B62-8D1B-DA7D51812E23}']
    function GetList: IInterfaceList; stdcall;
    function GetSymbol(Index: Integer): IlicgSymbol; stdcall;
    procedure SetSymbol(Index: Integer; Value: IlicgSymbol); stdcall;
    procedure SetActive(Value: Boolean); stdcall;
    function GetActive: Boolean; stdcall;
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    function getOnChange: TNotifyEvent; stdcall;
    procedure setOnChange(value: TNotifyEvent); stdcall;
    procedure Assign(Symbols: IlicgSymbols); stdcall;
    procedure OpenLineType(AGIS: TObject); stdcall;
    procedure OpenSymbol; stdcall;
    procedure Close; stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure Save; stdcall;
    procedure SaveAs(const FileName: string); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure Clear; stdcall;
    function Add(Item: IlicgSymbol; categoryName: string = ''; symbolIndex:
      Integer = 0): Integer; stdcall;
    procedure Delete(Index: Integer); stdcall;
    function Count: Integer; stdcall;
    procedure CurrExChange(Index1, Index2: Integer); stdcall;
    procedure CurrUp(Index: Integer); stdcall;
    procedure CurrDown(Index: Integer); stdcall;
    procedure CurrBringToTop(Index: Integer); stdcall;
    procedure CurrSendToBack(Index: Integer); stdcall;
    function IndexOfName(const Name: string): Integer; stdcall;
    procedure AtLeastOne; stdcall;
    procedure _SetSymbol(Index: Integer; value: IlicgSymbol); stdcall;

//    property _OnChange: TNotifyEvent read getOnChange write setOnChange;
    procedure SetName(Item: IlicgSymbol; _symbolName: string); stdcall;
    function GetSymbolIndexes(Sname: string): IStringsPassIntegerData; stdcall;
    function GetGIS: TObject; stdcall;
    procedure SetGIS(const Value: TObject); stdcall;

    property GIS: TObject read GetGIS write SetGIS;
    property Items[Index: Integer]: IlicgSymbol read GetSymbol write SetSymbol; default;
    property FileName: string read GetFileName write SetFileName;
    property Active: Boolean read GetActive write SetActive;

  end;


  { IlicgSymbols }
  IlicgLineTypes = interface
    ['{0B60C912-C716-4790-A138-DA956500BB9A}']
    function GetList: IInterfaceList; stdcall;
    function GetSymbol(Index: Integer): IlicgSymbol; stdcall;
    procedure SetSymbol(Index: Integer; Value: IlicgSymbol); stdcall;
    procedure SetActive(Value: Boolean); stdcall;
    function GetActive: Boolean; stdcall;
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    function getOnChange: TNotifyEvent; stdcall;
    procedure setOnChange(value: TNotifyEvent); stdcall;
    procedure Assign(Symbols: IlicgSymbols); stdcall;
    procedure OpenLineType(AGIS: TObject); stdcall;
    procedure OpenSymbol; stdcall;
    procedure Close; stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure Save; stdcall;
    procedure SaveAs(const FileName: string); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure Clear; stdcall;
    function Add(Item: IlicgSymbol; categoryName: string = ''; symbolIndex:
      Integer = 0): Integer; stdcall;
    procedure Delete(Index: Integer); stdcall;
    function Count: Integer; stdcall;
    procedure CurrExChange(Index1, Index2: Integer); stdcall;
    procedure CurrUp(Index: Integer); stdcall;
    procedure CurrDown(Index: Integer); stdcall;
    procedure CurrBringToTop(Index: Integer); stdcall;
    procedure CurrSendToBack(Index: Integer); stdcall;
    function IndexOfName(const Name: string): Integer; stdcall;
    procedure AtLeastOne; stdcall;
    procedure _SetSymbol(Index: Integer; value: IlicgSymbol); stdcall;
    property Items[Index: Integer]: IlicgSymbol read GetSymbol write SetSymbol; default;
    property FileName: string read GetFileName write SetFileName;
    property Active: Boolean read GetActive write SetActive;

//    property _OnChange: TNotifyEvent read getOnChange write setOnChange;
    procedure SetName(Item: IlicgSymbol; _symbolName: string); stdcall;
    function GetSymbolIndexes(Sname: string): IStringsPassIntegerData; stdcall;
  end;

  // Bloks ilker sembolle benzetildi
  IlicgBlocks = interface;

  { The Blocks used in the system }
  IlicgBlock = interface
    ['{B81ECACA-F923-4AC8-A42F-80E88C02D1B2}']
    function GetBlocks: IlicgBlocks; stdcall;
    procedure SetBlocks(value: IlicgBlocks); stdcall;
    function GetEntityList: IlicgEntityList; stdcall;
    function GetInsertionPoint: TlicgCoor; stdcall;
    procedure SetInsertionPoint(value: TlicgCoor); stdcall;
    function GetEntities(Index: Integer): IlicgEntity; stdcall;
    procedure ConfigureSpecialItems(Item: IlicgEntity); stdcall;
    function GetCentroid: TlicgCoor; stdcall;
    function GetExtension: TlicgExtent; stdcall;
    function GetName: string; stdcall;
    procedure SetName(const Value: string); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure Assign(Block: IlicgBlock); stdcall;
    procedure Clear; stdcall;
    function Count: Integer; stdcall;
    function Add(Item: IlicgEntity): Integer; stdcall;
    procedure Insert(Index: Integer; Item: IlicgEntity); stdcall;
    procedure Delete(Index: Integer); stdcall;
    function IndexOf(Item: IlicgEntity): Integer; stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    procedure UpdateExtension; stdcall;

    property Name: string read GetName write SetName;
    property Extension: TlicgExtent read GetExtension;
    property Centroid: TlicgCoor read GetCentroid;
    property InsertionPoint: TlicgCoor read GetInsertionPoint write SetInsertionPoint;
    property Entities[Index: Integer]: IlicgEntity read GetEntities;
    property EntityList: IlicgEntityList read GetEntityList;
    property Blocks: IlicgBlocks read GetBlocks write SetBlocks;
  end;

  { IlicgBlocks }
  IlicgBlocks = interface
    ['{F7F6C259-1F27-427E-BD1C-D29B1112E842}']
    function GetList: IInterfaceList; stdcall;
    function GetBlock(Index: Integer): IlicgBlock; stdcall;
    procedure SetBlock(Index: Integer; Value: IlicgBlock); stdcall;
    procedure SetActive(Value: Boolean); stdcall;
    function GetActive: Boolean; stdcall;
    function GetFileName: string; stdcall;
    procedure SetFileName(const Value: string); stdcall;
    function GetOnChange: TNotifyEvent; stdcall;
    procedure SetOnChange(value: TNotifyEvent); stdcall;
    procedure Assign(Blocks: IlicgBlocks); stdcall;
    procedure Open; stdcall;
    procedure Close; stdcall;
    procedure LoadFromFile(const FileName: string); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure Save; stdcall;
    procedure SaveAs(const FileName: string); stdcall;
    procedure LoadFromStream(Stream: TStream); stdcall;
    procedure SaveToStream(Stream: TStream); stdcall;
    procedure Clear; stdcall;
    function Add(Item: IlicgBlock; categoryName: string = ''; BlockIndex:
      Integer = 0): Integer; stdcall;
    procedure Delete(Index: Integer); stdcall;
    function Count: Integer; stdcall;
    function IndexOfName(const Name: string): Integer; stdcall;
    procedure AtLeastOne; stdcall;
    procedure _SetBlock(Index: Integer; value: IlicgBlock); stdcall;
    procedure SetName(Item: IlicgBlock; _BlockName: string); stdcall;
    function GetBlockIndexes(Sname: string): IStringsPassIntegerData; stdcall;
    property Items[Index: Integer]: IlicgBlock read GetBlock write SetBlock; default;
    property FileName: string read GetFileName write SetFileName;
    property Active: Boolean read GetActive write SetActive;
  end;
  // Bloks ilker sembole benzetildi.

  IEntityStream = interface;

  IlicgEntityFactory = interface
    ['{7F8B0D25-A1E9-4287-9AA4-58E1E1A421F6}']
    function CreateEntityStream: IEntityStream; stdcall;
    function CreateSymbols: IlicgSymbols; stdcall;
    function CreateLineTypes: IlicgLineTypes; stdcall;
    function CreateSymbolsList: IlicgSymbolsList; stdcall;
    function CreateBlocks: IlicgBlocks; stdcall;
    function MakeSymbol(Symbols: IlicgSymbols): IlicgSymbol; stdcall;
    function MakeLineType(Lines: IlicgSymbols): IlicgSymbol; stdcall;
    function GLB_LineTypes: IlicgSymbols; stdcall;
    function GLB_Symbols: IlicgSymbols; stdcall;

    // about DrawTools
    function CreateDrawTools: IlicgDrawTools; stdcall;
    function CreateBrushTool: IlicgBrushTool; stdcall;
    function CreateFontTool: IlicgFontTool; stdcall;
    function CreatePenTool: IlicgPenTool; stdcall;
    function CreateArrowTool: IlicgArrowTool; stdcall;
    function CreateSymbolTool: IlicgSymbolTool; stdcall;
    function CreateBlockTool: IlicgBlockTool; stdcall;

    // about geometry
    function MakeVector(const Dimension, Size: integer; CanGrow: Boolean = True): IlicgVector; stdcall;
    function CreateGeometry(GeomType: integer; Size: integer = 0; CanGrow: Boolean = True): IlicgGeometry; stdcall;
    function GetEntityDisplayText(const EntityID: TlicgEntityID): string; stdcall;
    function GetEntityClassName(const EntityID: TlicgEntityID): string; stdcall;

    // Make Entitties
    function MakeEntity(EntityID: TlicgEntityID; NPts: integer; dimension: integer): IlicgEntity; stdcall;
    function MakeLine(const P1, P2: TlicgCoor): IlicgEntity; stdcall;
    function MakePolygon(const Pts: array of TlicgCoor; CanGrow: Boolean = True): IlicgEntity; stdcall;
    function MakePolyline(const Pts: array of TlicgCoor; CanGrow: Boolean = True): IlicgEntity; stdcall;
    function MakeRectangle(P1, P2: TlicgCoor; _Roundness: Double = 0): IlicgEntity; stdcall;
    function MakePolyRectangle(P1, P2: TlicgCoor; _Roundness: Double = 0): IlicgEntity; stdcall;
    function MakeEllipse(P1, P2: TlicgCoor): IlicgEntity; stdcall;
    function MakePolyEllipse(P1, P2: TlicgCoor): IlicgEntity; stdcall;
    function MakeArc(P1, P2, P3: TlicgCoor; _arcStyle: TlicgArcStyle): IlicgEntity; stdcall;
    function MakeCircle(Center: TlicgCoor; Radius: Double): IlicgEntity; stdcall;
    function MakePolyArc(P1, P2, P3: TlicgCoor; _arcStyle: TlicgArcStyle): IlicgEntity; stdcall;
    function MakePlace(P: TlicgCoor; Color: TColor = clNone): IlicgEntity; stdcall;
    function MakeBlockInsert(P: TlicgCoor; Color: TColor = clNone): IlicgEntity; stdcall;
    function MakePoint(P: TlicgCoor3D; Name: string; Color: TColor = clNone): IlicgEntity; stdcall;
    function MakeSpline(const Pts: array of TlicgCoor; CurveType: TlicgCurveType = ctNone): IlicgEntity; stdcall;

    function MakeText(const ACoor: TlicgCoor; const AText: string;
      const AFontTool: IlicgFontTool = nil): IlicgEntity; stdcall;
    function MakeTextM(const ACoor: TlicgCoor; const AText: string; const AWidth, AHeight: Double;
      const AFontTool: IlicgFontTool = nil): IlicgEntity; stdcall;
    function MakeTextFX(const ACoor: TlicgCoor; const AText: string; const ABorderStyle: TlicgTextBorderStyle;
      const AFontTool: IlicgFontTool = nil): IlicgEntity; stdcall;
    function MakeVectorialText(const ACoor: TlicgCoor; const AText: AnsiString;
      const AFontTool: IlicgFontTool = nil): IlicgEntity; stdcall;
    function MakeVectorialFittedText(const ACoor: TlicgCoor; const AText: AnsiString;
      const AFontTool: IlicgFontTool = nil): IlicgEntity; stdcall;

    function MakePersistBitmap(const p1, p2: TlicgCoor; const FileName: string): IlicgEntity; overload; stdcall;
    function MakePersistBitmap(const p1, p2: TlicgCoor; const Stream: TStream): IlicgEntity; overload; stdcall;
    function MakeGroupEntity: IlicgEntity; stdcall;
    function MakeDimHorizantal(const BaseLineFrom, BaseLineTo: TlicgCoor; const TextLineY: Double): IlicgEntity; stdcall;
    function MakeDimParallel(const BaseLineFrom, BaseLineTo: TlicgCoor; const TextLineY: Double): IlicgEntity; stdcall;
    function MakeDimVertical(const BaseLineFrom, BaseLineTo: TlicgCoor; const TextLineY: Double): IlicgEntity; stdcall;
    function MakePreview(const p1, p2: TlicgCoor; PrintMode: TlicgPrintMode; FileNo: Integer): IlicgEntity; stdcall;
    function MakeOle(const P1, P2: TlicgCoor; const FileName: string): IlicgEntity; stdcall;
    function MakeVectorFonts: IlicgVectorFonts; stdcall;
    function MakeHatchList: IlicgHatchList; stdcall;
    function MakeTriangle(const P1, P2, P3: TlicgCoor): IlicgEntity; stdcall;
    function MakePafta(var AName: string): IlicgEntity; overload; stdcall;
    function MakePafta(LowerLeftPoint: TlicgCoor; PaftaScale: TlicgPaftaScale): IlicgEntity; overload; stdcall;
  end;

  IEntityStream = interface
    ['{9314F7AA-EE47-4E3E-83EB-D1EB58F220F7}']
    function GetInfoID(const stream: TStream): integer; stdcall;
    procedure LoadEntity(const stream: TStream; var entity: IlicgEntity); stdcall;
    procedure LoadEntityHeader(const stream: TStream; var entity: IlicgEntity); stdcall;
    procedure LoadDrawTools(const stream: TStream; const entity: IlicgEntity); stdcall;
    procedure LoadGeometry(const stream: TStream; const geometry: IlicgGeometry); stdcall;
    procedure SaveEntity(const stream: TStream; const entity: IlicgEntity); stdcall;
    procedure SaveEntityHeader(const stream: TStream; const entity: IlicgEntity); stdcall;
    procedure SaveDrawTools(const stream: TStream; const entity: IlicgEntity); stdcall;
    procedure SaveGeometry(const stream: TStream; const entity: IlicgGeometry); stdcall;

     // Load & Save of DrawTools...
    procedure Load_BrushTool(const stream: TStream; BrushTool: IlicgBrushTool); stdcall;
    procedure Save_BrushTool(const BrushTool: IlicgBrushTool; stream: TStream); stdcall;
    procedure Load_FontTool(const stream: TStream; AEntity: IlicgEntity); stdcall;
    procedure Save_FontTool(const FontTool: IlicgFontTool; stream: TStream); stdcall;
    procedure Load_PenTool(const stream: TStream; PenTool: IlicgPenTool); stdcall;
    procedure Save_PenTool(const PenTool: IlicgPenTool; stream: TStream); stdcall;
    procedure Load_SymbolTool(const stream: TStream; SymbolTool: IlicgSymbolTool); stdcall;
    procedure Save_SymbolTool(const SymbolTool: IlicgSymbolTool; stream: TStream); stdcall;
  end;

function AslicgEntity(const int: IInterface): IlicgEntity;
function AslicgPersistBitmap(const int: IInterface): IlicgPersistBitmap;
function AslicgOleEntity(const int: IInterface): IlicgOleEntity;
function AslicgPreviewEntity(const int: IInterface): IlicgPreviewEntity;
function isNeedPointTool(const Eid: TlicgEntityId): Boolean;
function isNeedPenTool(const Eid: TlicgEntityId): Boolean;
function isNeedBrushTool(const Eid: TlicgEntityId): Boolean;
function isNeedSymbolTool(const Eid: TlicgEntityId): Boolean;
function isNeedBlockInsertTool(const Eid: TlicgEntityId): Boolean;
function isNeedFontTTool(const Eid: TlicgEntityId): Boolean;
function isNeedFontVTool(const Eid: TlicgEntityId): Boolean;

implementation

function isNeedPointTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idPoint];
end;

function isNeedPenTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idLine, idPolyline, idPolygon, idRectangle, idArc, idEllipse,
    idPolyRectangle, idSpline, idPolyArc, idTriangle, idPafta];
end;

function isNeedBrushTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idPolygon, idRectangle, idEllipse, idPolyRectangle, idTriangle, idPafta];
end;

function isNeedSymbolTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idPlace];
end;

function isNeedBlockInsertTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idBlockInsert];
end;

function isNeedFontTTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idText, idTextM, idTextFX];
end;

function isNeedFontVTool(const Eid: TlicgEntityId): Boolean;
begin
  Result := Eid in [idVectorialText, idVectorialFittedText];
end;

function AslicgEntity(const int: IInterface): IlicgEntity;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgEntity, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPersistBitmap(const int: IInterface): IlicgPersistBitmap;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPersistBitmap, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgOleEntity(const int: IInterface): IlicgOleEntity;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgOleEntity, Result) <> 0) then
      Result := nil;
  end;
end;

function AslicgPreviewEntity(const int: IInterface): IlicgPreviewEntity;
begin
  Result := nil;
  if Assigned(Int) then
  begin
    if (Int.QueryInterface(IlicgPreviewEntity, Result) <> 0) then
      Result := nil;
  end;
end;

end.


