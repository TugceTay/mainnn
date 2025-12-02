unit Lider.CG.Com.CmdLine;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  SysUtils,
  Forms,
  StdCtrls,
  Graphics,
  Controls,
  Math,
  cxGraphics,
  Lider.CG.Com.TransformInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.YaccLib,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Base,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Types,
  Lider.CG.Com.RTree,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.Consts;

type
  IActionCommandPropertyList = interface;

  TlicgAction = class;

  TCBTransformSelectActionProc = procedure(Action: TlicgAction);

  TActionParameterType = (aptNone, aptAddEntity, aptSelect, aptFormLink, aptDefault);

  TActionParameterEvent = procedure(Action: TlicgAction) of object; stdcall;

  TActionParameterGetEntityProc = function: IlicgEntity of object; stdcall;

  (*
  IActionCommandProperty = interface
    ['{0D8E0B65-61A5-4B56-8AA2-02AC2D5D5CFB}']
     function  GetCaption : string; stdcall;
     procedure SetCaption(value : string);stdcall;
     function  GetValue : variant; stdcall;
     procedure SetValue(value : variant);stdcall;
     function  GetReadOnly : Boolean; stdcall;
     procedure SetReadOnly (value : Boolean); stdcall;
     function  GetActionDataType : TActionParamaterDataType; stdcall;
     procedure SetActionDataType (value : TActionParamaterDataType); stdcall;
     function  GetHint : string; stdcall;
     procedure SetHint(value : string);stdcall;

     function  GetEntityDrawTools  : IlicgEntity ; stdcall;
     procedure SetEntityDrawTools  (value: IlicgEntity) ; stdcall;

     function  GetPenTool  : IlicgPenTool ; stdcall;
     procedure SetPenTool  (value: IlicgPenTool) ; stdcall;

     function  GetBrushTool  : IlicgBrushTool ; stdcall;
     procedure SetBrushTool  (value: IlicgBrushTool) ; stdcall;

     function  GetSymbolTool  : IlicgSymbolTool ; stdcall;
     procedure SetSymbolTool  (value: IlicgSymbolTool) ; stdcall;

     function  GetTTFontTool  : IlicgFontTool ; stdcall;
     procedure SetTTFontTool  (value: IlicgFontTool) ; stdcall;

     function  GetVFontTool  : IlicgFontTool ; stdcall;
     procedure SetVFontTool  (value: IlicgFontTool) ; stdcall;

     property  Caption : string read GetCaption write SetCaption;
     property  Value : variant read GetValue write SetValue;
     property  ReadOnly : Boolean read GetReadOnly write SetReadOnly;
     property  ActionDataType : TActionParamaterDataType read GetActionDataType write SetActionDataType;
     property  Hint : string read GetHint write SetHint;
     property  PenTool : IlicgPenTool read GetPenTool write SetPenTool;
     property  BrushTool : IlicgBrushTool read GetBrushTool write SetBrushTool;
     property  SymbolTool : IlicgSymbolTool read GetSymbolTool write SetSymbolTool;
     property  TTFontTool : IlicgFontTool read GetTTFontTool write SetTTFontTool;
     property  VFontTool : IlicgFontTool read GetVFontTool write SetVFontTool;

     property Entity : IlicgEntity read GetEntityDrawTools write SetEntityDrawTools;

  end;


  TActionCommandProperty = class(TInterfacedObject, IActionCommandProperty)
   private
     FCaption,FHint : string;
     FValue : Variant;
     FReadOnly : Boolean;
     FActionDataType: TActionParamaterDataType;
     FPenTool : IlicgPenTool;
     FBrushTool : IlicgBrushTool;
     FSymbolTool : IlicgSymbolTool;
     FTTFontTool : IlicgFontTool;
     FVFontTool : IlicgFontTool;
     FEntity : IlicgEntity;
   public
     function  GetCaption : string; stdcall;
     procedure SetCaption(value : string);stdcall;
     function  GetValue : variant; stdcall;
     procedure SetValue(value : variant);stdcall;
     function  GetReadOnly : Boolean; stdcall;
     procedure SetReadOnly (value : Boolean); stdcall;
     function  GetActionDataType : TActionParamaterDataType; stdcall;
     procedure SetActionDataType (value : TActionParamaterDataType); stdcall;
     function  GetHint : string; stdcall;
     procedure SetHint(value : string);stdcall;

     function  GetEntityDrawTools  : IlicgEntity ; stdcall;
     procedure SetEntityDrawTools  (value: IlicgEntity) ; stdcall;

     function  GetPenTool  : IlicgPenTool ; stdcall;
     procedure SetPenTool  (value: IlicgPenTool) ; stdcall;

     function  GetBrushTool  : IlicgBrushTool ; stdcall;
     procedure SetBrushTool  (value: IlicgBrushTool) ; stdcall;

     function  GetSymbolTool  : IlicgSymbolTool ; stdcall;
     procedure SetSymbolTool  (value: IlicgSymbolTool) ; stdcall;

     function  GetTTFontTool  : IlicgFontTool ; stdcall;
     procedure SetTTFontTool  (value: IlicgFontTool) ; stdcall;

     function  GetVFontTool  : IlicgFontTool ; stdcall;
     procedure SetVFontTool  (value: IlicgFontTool) ; stdcall;

  end;
  *)

  IActionCommandPropertyList = interface
    ['{B0677B23-0CB1-42FF-8093-EDB2240F69E5}']
    function Get(Index: Integer): IInterface; stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    procedure Put(Index: Integer; const Item: IInterface); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function First: IInterface; stdcall;
    function IndexOf(const Item: IInterface): Integer; stdcall;
    function Add(const Item: IInterface): Integer; stdcall;
    procedure Insert(Index: Integer; const Item: IInterface); stdcall;
    function Last: IInterface; stdcall;
    function Remove(const Item: IInterface): Integer; stdcall;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: IInterface read Get write Put; default;
  end;

  TActionCommandPropertyList = class(TInterfacedObject, IActionCommandPropertyList)
  private
    FIList: IInterfaceList;
  public
    constructor create;
    destructor destroy; override;

    // IInterfaceList implemantation
    function Get(Index: Integer): IInterface; stdcall;
    function GetCapacity: Integer; stdcall;
    function GetCount: Integer; stdcall;
    procedure Put(Index: Integer; const Item: IInterface); stdcall;
    procedure SetCapacity(NewCapacity: Integer); stdcall;
    procedure SetCount(NewCount: Integer); stdcall;
    procedure Clear; stdcall;
    procedure Delete(Index: Integer); stdcall;
    procedure Exchange(Index1, Index2: Integer); stdcall;
    function First: IInterface; stdcall;
    function IndexOf(const Item: IInterface): Integer; stdcall;
    function Add(const Item: IInterface): Integer; stdcall;
    procedure Insert(Index: Integer; const Item: IInterface); stdcall;
    function Last: IInterface; stdcall;
    function Remove(const Item: IInterface): Integer; stdcall;
    procedure Lock; stdcall;
    procedure Unlock; stdcall;

    //   IActionCommandPropertyList implementaion

  end;

  TlicgPolyCurrentAction = (pcaLine, pcaArc);

  TlicgPolyArcAction = (paaEndPoint, paaSecond, paaCenter, paaDirection);

  TlicgActionObjID = (idNoneAction, idEntityClickAction, idQuickSelectTypeAction, idRasterClickAction);

  IlicgActionTracker = interface;

  IlicgAction = interface;

  IlicgAccuDraw = interface;

  IlicgAccuSnap = interface;

  TlicgBaseCmdLine = class;

  TlicgActionNeed = (anFilter, anTranslate);

  TlicgActionNeeds = set of TlicgActionNeed;

  TPolySelectType = (pstNone, pstAreaClose, pstDrawPolygon, pstSelectPolygon, pstDrawLine, pstDrawPolyline, pstSelectPolyline); // ilker ekleme

  TQuickSelectType = (qstDefault, qstWindow, qstPoly, qstCircle, qstLine, qstPoint);

  TCoordinateCalculatorType = (cctCoordinate, cctdYdX, cctDistanceAngle, cctLayer, cctLastCoordinate, cctPointName);

  TlicgAfterCommandEvent = procedure(Sender: TObject; const Command, ActionID: string) of object;

  TlicgStatusMessageEvent = procedure(Sender: TObject; const AMessage: string) of object;

  TlicgMeasureInfoEvent = procedure(Sender: TObject; const Area, Perimeter, Angle, NumPoints: Double) of object;

  TlicgVertexRefList = class;

  TlicgVertexRef = class
  private
    FLayer: TlicgBaseLayer;
    FRecno: Integer;
    FVertexNo: Integer;
    FVertex: TlicgCoor;
    function GetLayer: TlicgBaseLayer;
    function GetRecno: Integer;
    function GetVertex: TlicgCoor;
    function GetVertexNo: Integer;
    procedure SetLayer(const Value: TlicgBaseLayer);
    procedure SetRecno(const Value: Integer);
    procedure SetVertex(const Value: TlicgCoor);
    procedure SetVertexNo(const Value: Integer);
  public
    procedure Draw(CmdLine: TlicgBaseCmdLine);
    property Layer: TlicgBaseLayer read GetLayer write SetLayer;
    property Recno: Integer read GetRecno write SetRecno;
    property VertexNo: Integer read GetVertexNo write SetVertexNo;
    property Vertex: TlicgCoor read GetVertex write SetVertex;
  end;

  TlicgVertexRefList = class
  private
    FOwner: IlicgAccuSnap;
    FList: TList;
    FSorted: Boolean;
    function GetSorteableElement(Index: Integer): string;
    function GetVertexRef(Index: Integer): TlicgVertexRef;
    function GetSorted: Boolean;
    procedure SetSorted(const Value: Boolean);
  public
    constructor Create(AOwner: IlicgAccuSnap);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Layer: TlicgBaseLayer; Recno, VertexNo: Integer; const Vertex:
      TlicgCoor);
    procedure Delete(Index: Integer);
    function IndexOf(Layer: TlicgBaseLayer; Recno, VertexNo: Integer): Integer;
    procedure Sort;
    function Count: Integer;
    procedure ShowHide(Ele: Integer = -1);
    property Sorted: Boolean read GetSorted write SetSorted;
    property Items[Index: Integer]: TlicgVertexRef read GetVertexRef; default;
  end;

  TCommandStr = class
    SCmd: string;
    SCap: string;
    SOverrideCmd: string;
    SCustomCmd: string;
  end;

  TCommandStrList = class(TList)
    destructor destroy; override;
    function GetCommandString(var cmd: string; var Exists: boolean): string;
    function GetCommandCaption(var cmd: string): string;
  end;

  TSnapPickEntityOrder = (speoAll, speoOnlyPoint, speoOnlyPolyline, speoOnlyPolygon);

  TlicgLauncher = class(TObject)  // ilker ekleme
  private
    FCmdLine: TlicgBaseCmdline;
    FLauncher: IlicgActionTracker;
  protected
    function GetCmdLine: TlicgBaseCmdline;
    procedure SetCmdLine(const Value: TlicgBaseCmdline);
    procedure SetLauncher(const Value: IlicgActionTracker);
    function GetLauncher: IlicgActionTracker;
  public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdline);
    destructor Destroy; override;
  published
    property CmdLine: TlicgBaseCmdline read GetCmdLine write SetCmdLine;
    property Launcher: IlicgActionTracker read GetLauncher write SetLauncher;
  end;

  TlicgBaseCmdline = class(TWinControl)
  private
    FGroupNameSelection: Boolean;
    function GetGroupNameSelection: Boolean;
    procedure SetGroupNameSelection(const Value: Boolean);
     //FCreateActionFormEvent  : TCreateActionFormEvent;
     //FDestroyActionFormEvent : TDestroyActionFormEvent;
  public
    procedure SystemKeydown(Sender: TObject; Key: Word); virtual; abstract;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double); virtual; abstract;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double); virtual; abstract;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double); virtual; abstract;
    procedure DoKeyPress(Sender: TObject; var Key: Char); virtual; abstract;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual; abstract;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual; abstract;
    procedure DoPaint(Sender: TObject); virtual; abstract;
    procedure DoDblClick(Sender: TObject); virtual; abstract;
  protected
    function GetSnapPickEntityOrder: TSnapPickEntityOrder; virtual; abstract;
    procedure SetSnapPickEntityOrder(value: TSnapPickEntityOrder); virtual; abstract;
    procedure SetOnMeasureInfo(value: TlicgMeasureInfoEvent); virtual; abstract;
    function GetOnMeasureInfo: TlicgMeasureInfoEvent; virtual; abstract;
    function GetActiveDrawBox: TlicgBaseDrawBox; virtual; abstract;
    function GetText: string; virtual; abstract;
    procedure SetText(const Value: string); virtual; abstract;
    function GetAccuDraw: IlicgAccuDraw; virtual; abstract;
    function GetAccuSnap: IlicgAccuSnap; virtual; abstract;
    function GetCurrentPoint: TlicgCoor; virtual; abstract;
    function GetDynamicUpdate: Boolean; virtual; abstract;
    function GetLastActionID: string; virtual; abstract;
    function GetLastCommand: string; virtual; abstract;
    function GetShowMeasureInfoWindow: Boolean; virtual; abstract;
    function GetTheDefaultAction: TlicgAction; virtual; abstract;
    function GetUseFullViewCursor: Boolean; virtual; abstract;
    function GetUseOrto: Boolean; virtual; abstract;
    procedure SetCurrentPoint(const Value: TlicgCoor); virtual; abstract;
    procedure SetDefaultCanTransform(const Value: Boolean); virtual; abstract;
    procedure SetDynamicUpdate(const Value: Boolean); virtual; abstract;
    procedure SetLastActionID(const Value: string); virtual; abstract;
    procedure SetLastCommand(const Value: string); virtual; abstract;
    procedure SetShowMeasureInfoWindow(const Value: Boolean); virtual; abstract;
    procedure SetUseFullViewCursor(const Value: Boolean); virtual; abstract;
    procedure SetUseOrto(const Value: Boolean); virtual; abstract;
    procedure SetActiveDrawBox(Value: TlicgBaseDrawBox); virtual; abstract;
    function GetOnAfterCommand: TlicgAfterCommandEvent; virtual; abstract;
    procedure SetOnAfterCommand(value: TlicgAfterCommandEvent); virtual; abstract;
    function GetExternalAction: TlicgAction; virtual; abstract;
    function GetDefaultCanTransform: Boolean; virtual; abstract;
    procedure SetDrawBoxList(const Value: TObject); virtual; abstract;
    function GetDrawBoxList: TObject; virtual; abstract;
    procedure SetSelectQueryExp(const Value: string); virtual; abstract;
    function GetSelectQueryExp: string; virtual; abstract;
    function GetCommandLineStrings: TCommandStrList; virtual; abstract;
    function GetActionList: TList; virtual; abstract;
    function GetPolySelectType: TPolySelectType; virtual; abstract;
    procedure SetPolySelectType(const Value: TPolySelectType); virtual; abstract;

    function GetAddLastPoint: TlicgCoor; virtual; abstract;
    procedure SetAddLastPoint(const Value: TlicgCoor); virtual; abstract;

    function  GetIsMouseDown : Boolean; virtual; abstract;
    procedure SetIsMouseDown(Value:Boolean); virtual; abstract;

    function GetonPushActionParameterProperties: TActionParameterEvent; virtual; abstract;
    procedure SetonPushActionParameterProperties(value: TActionParameterEvent);
      virtual; abstract;
    function GetonPopActionParameterProperties: TActionParameterEvent; virtual; abstract;
    procedure SetonPopActionParameterProperties(value: TActionParameterEvent);
      virtual; abstract;
    function GetonSetActionParameterProperties: TActionParameterEvent; virtual; abstract;
    procedure SetonSetActionParameterProperties(value: TActionParameterEvent);
      virtual; abstract;
    function GetOnAccuDrawActivate: TNotifyEvent; virtual; abstract;
    procedure SetOnAccuDrawActivate(value: TNotifyEvent); virtual; abstract;
    function GetOnAccuDrawChange: TNotifyEvent; virtual; abstract;
    procedure SetOnAccuDrawChange(value: TNotifyEvent); virtual; abstract;
    function GetOnAccuUtilityChange: TNotifyEvent; virtual; abstract;
    procedure SetOnAccuUtilityChange(value: TNotifyEvent); virtual; abstract;
    function GetParameterFormLeft: TForm; virtual; abstract;
    procedure SetParameterFormLeft(value: TForm); virtual; abstract;
  protected
   {
    procedure SetCreateActionFormEvent (value : TCreateActionFormEvent);
    function  GetCreateActionFormEvent : TCreateActionFormEvent;

    procedure SetDestroyActionFormEvent (value : TDestroyActionFormEvent);
    function  GetDestroyActionFormEvent : TDestroyActionFormEvent;
}
  public
    InverseSelection: Boolean;
    GraphicOperator: TlicgGraphicOperator;
    function ActionAddNewEntity(Entity: IlicgEntity; isUndo: Boolean = True;
      ApplyLayerProps: Boolean = true): Integer; virtual; abstract;
    procedure SetStatusMessage(const Value: string); virtual; abstract;
    procedure SetTheDefaultAction(Value: TlicgAction); virtual; abstract;
    procedure InternalDoCommand(const Cmd, ActionID: string; IsParam: Boolean); virtual; abstract;
    procedure DoCommandEx(Action: TlicgAction; CB: TFarProcedure1); virtual; abstract;
    procedure DoLauncher(Launcher: TlicgLauncher; CB: TFarProcedure1); virtual; abstract;

    procedure SetCaption(const Value: string); virtual;
    function GetCaptionCommand: string; virtual; abstract;
    procedure SetCaptionCommand(const Value: string); virtual; abstract;
    function GetHintCommand: string; virtual; abstract;
    procedure SetHintCommand(value: string); virtual; abstract;
    function GetHintCommandEvent: THintCommandEvent; virtual; abstract;
    procedure SetHintCommandEvent(Value: THintCommandEvent); virtual; abstract;
    function GetCommandLabelCommandEvent: THintCommandEvent; virtual; abstract;
    procedure SetCommandLabelCommandEvent(Value: THintCommandEvent); virtual; abstract;
    function CurrentAction: TlicgAction; virtual; abstract;
    function PreviousAction: TlicgAction; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Push(Action: TlicgAction; ClearBefore: Boolean; const Cmd,
      ActionID: string); virtual; abstract;
    procedure Pop; virtual; abstract;
    // command line processing
    procedure DoCommand(const Cmd: string; const ActionID: string = ''); virtual;
      abstract;
    procedure CurrentActionDoCommand(const Cmd: string); virtual; abstract;
    function IsBusy: Boolean; virtual; abstract;
    procedure HintCommandChange(const Value: string); virtual; abstract;
    function IsSnapped: Boolean; virtual; abstract;
    function CurrentActionID: string; virtual; abstract;
    function GetSnappedPoint: TlicgCoor; virtual; abstract;
    procedure AddPointToCurrentAction(const P: TlicgCoor; isPntZoom: Boolean = True); virtual; abstract;
    procedure AddRelativePointToCurrentAction(const P: TlicgCoor); virtual; abstract;
    procedure AddRelativeAngleToCurrentAction(const Dist, Angle: Double); virtual; abstract;
    procedure AddPointListToCurrentAction(const P: IlicgVector); virtual; abstract;
    function SnapPointToGridOrGuideLines(const Value: TlicgCoor; var IsSnapped: Boolean): TlicgCoor; virtual; abstract;
    function VectorFill(patFileName, hatchName: string; hatchEntList,
      nohatchList: IlicgEntityList; hatchColor: tcolor; hatchAngle, hatchScale:
      Double; IsShowProgress: Boolean = false): IlicgEntityList; virtual; abstract;
    { follows the methods and properties that will affect to all drawboxes }
    procedure All_StopRepaintingThread; virtual; abstract;
    procedure All_Repaint(UseThread: Boolean = False); virtual; abstract;
    procedure All_Cursor(Value: TCursor); virtual; abstract;
    procedure All_DrawEntity2DRubberBand(Entity: IlicgEntity; CtrlPts: Boolean =
      False; TransfPts: Boolean = False); virtual; abstract;
    procedure All_Invalidate; virtual; abstract;
    procedure All_Refresh; virtual; abstract;
    procedure All_RepaintExtent(const Value: TlicgExtent; UseThread: Boolean = False);
      virtual; abstract;
    procedure All_SetViewTo(const AXMin, AYMin, AXMax, AYMax: Double; UseThread:
      Boolean = False); virtual; abstract;
    procedure All_ClearOnceDisplayList; virtual; abstract;
    procedure All_ClearRefOnceDisplayList; virtual; abstract;
    procedure All_ClearAlwaysDisplayList(tag: integer = System.MaxInt); virtual; abstract;
     // cmdline nesnesine ait drawboxlist.count>1 olmasý halinde menuden tetiklenen komutlarýn bu drawbox a yönlendirilebilmesi için
     // komutlar tetiklenirken ActiveDrawbox = CommandDrawbox yapýlýr ....
    procedure SetCommandDrawBoxToActive; virtual; abstract;

    property AddLastPoint: TlicgCoor read GetAddLastPoint write SetAddLastPoint;
    property PolySelectType: TPolySelectType read GetPolySelectType write SetPolySelectType default pstNone;
    property OnAfterCommand: TlicgAfterCommandEvent read GetOnAfterCommand write SetOnAfterCommand;
    property LastCommand: string read GetLastCommand write SetLastCommand;
    property LastActionID: string read GetLastActionID write SetLastActionID;
    property ActiveDrawBox: TlicgBaseDrawBox read GetActiveDrawBox write SetActiveDrawBox;
    property UseFullViewCursor: Boolean read GetUseFullViewCursor write
      SetUseFullViewCursor default False;
    property UseOrto: Boolean read GetUseOrto write SetUseOrto;
    property AccuDraw: IlicgAccuDraw read GetAccuDraw;
    property AccuSnap: IlicgAccuSnap read GetAccuSnap;
    property CurrentPoint: TlicgCoor read GetCurrentPoint write SetCurrentPoint;
    property ExternalAction: TlicgAction read GetExternalAction;
    property DefaultCanTransform: Boolean read GetDefaultCanTransform write
      SetDefaultCanTransform default false;
    property DrawBoxList: TObject read GetDrawBoxList write SetDrawBoxList;
    property ShowMeasureInfoWindow: Boolean read GetShowMeasureInfoWindow write
      SetShowMeasureInfoWindow default True;
    property OnMeasureInfo: TlicgMeasureInfoEvent read GetOnMeasureInfo write
      SetOnMeasureInfo;
    property DynamicUpdate: Boolean read GetDynamicUpdate write SetDynamicUpdate;
    property TheDefaultAction: TlicgAction read GetTheDefaultAction write
      SetTheDefaultAction;
    property SelectQueryExp: string read GetSelectQueryExp write
      SetSelectQueryExp;
    procedure SetOnStatusMessage(value: TlicgStatusMessageEvent); virtual; abstract;
    function GetOnStatusMessage: TlicgStatusMessageEvent; virtual; abstract;
    function isQuickSelectAction(Action: TlicgAction; SelMode: TQuickSelectType):
      Boolean; virtual; abstract;
    procedure ChangeSelectActionType(Action: TlicgAction; SelMode: TQuickSelectType); virtual; abstract;
    function CurrentActionQuickSelection: Boolean; virtual; abstract;
    function CurrentActionNeedCoorCalc: boolean; virtual; abstract;
    function isCurrentActionAddPolyEntityAction: Boolean; virtual; abstract;
    property OnStatusMessage: TlicgStatusMessageEvent read GetOnStatusMessage
      write SetOnStatusMessage;

    property IsMouseDown: Boolean read GetIsMouseDown write SetIsMouseDown;

    property OnPushActionParameterProperties: TActionParameterEvent read
      GetonPushActionParameterProperties write SetonPushActionParameterProperties;
    property OnPopActionParameterProperties: TActionParameterEvent read
      GetonPopActionParameterProperties write SetonPopActionParameterProperties;
    property OnSetActionParameterProperties: TActionParameterEvent read
      GetonSetActionParameterProperties write SetonSetActionParameterProperties;
    property CaptionCommand: string read GetCaptionCommand write SetCaptionCommand;
    property HintCommand: string read GetHintCommand write SetHintCommand;
    property OnHintCommadEvent: THintCommandEvent read GetHintCommandEvent write
      SetHintCommandEvent;
    property OnCommandLabelCommadEvent: THintCommandEvent read
      GetCommandLabelCommandEvent write SetCommandLabelCommandEvent;
    property CommandLineStrings: TCommandStrList read GetCommandLineStrings;
    property OnAccuDrawActivate: TNotifyEvent read GetOnAccuDrawActivate write
      SetOnAccuDrawActivate;
    property OnAccuDrawChange: TNotifyEvent read GetOnAccuDrawChange write
      SetOnAccuDrawChange;
    property OnAccuUtilityChange: TNotifyEvent read GetOnAccuUtilityChange write
      SetOnAccuUtilityChange;
    property ActionList: TList read GetActionList;
    property ParameterFormLeft: TForm read GetParameterFormLeft write
      SetParameterFormLeft;
    property SnapPickEntityOrder: TSnapPickEntityOrder read
      GetSnapPickEntityOrder write SetSnapPickEntityOrder;
    property GroupNameSelection: Boolean read GetGroupNameSelection write SetGroupNameSelection;

  {  property CreateActionFormEvent : TCreateActionFormEvent read GetCreateActionFormEvent write SetCreateActionFormEvent;
    property DestroyActionFormEvent : TDestroyActionFormEvent read GetDestroyActionFormEvent write SetDestroyActionFormEvent;
  }

  published
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property Ctl3D;
  end;

  TlicgCircleDrawType = (ct2P, ct3P, ctCR, ctRadius); // ilker radius ekleme


  { If you return TrackedEntity = Nil, then you must dispose the entity passed
    on that parameter }

  TlicgTrackedEntityEvent = procedure(const TrackID: string; var TrackedEntity: IlicgEntity) of object;  // ilker ekleme

  { this is for event OnTrackedEntityClick.
    Important ! If Layer = Nil and Recno = 0, it means that end-user clicked on the map
    but no entity was found, so you must always check and proceed accordingly }
  TlicgTrackEntityClickEvent = procedure(const TrackID: string; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double; Layer:
    TlicgBaseLayer; Recno: Integer; var Accept: Boolean) of object;

  TlicgTrackSelectPickedListEvent = procedure(const TrackID: string; Button:  // ilker ekleme
    TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double;
    var SelectPickedList: IPickedList; Accept: Boolean) of object;

  // Raster
  TlicgTrackRasterClickEvent = procedure(const TrackID: string; Button:
    TMouseButton; Shift: TShiftState; X, Y: Integer; const WX, WY: Double; Path:
    widestring; var Accept: Boolean) of object;

  { Tracks when mouse is moved (not clicked) over and entity. This is checked every
    TlicgBaseDrawbox.DelayShowHint elapsed time (in milliseconds)
    Important! If Layer = Nil and Recno = 0 then that means that no entity was
    found under mouse when the time elapsed (We must check in a time delay
    because the search can be time consuming)}
  TlicgTrackEntityMouseMoveEvent = procedure(const TrackID: string; Layer:
    TlicgBaseLayer; Recno: Integer; var Accept: Boolean) of object;

  TlicgTrackEntityDragDropEvent = procedure(const TrackID: string; Layer:
    TlicgBaseLayer; Recno: Integer; var TrackedEntity: IlicgEntity) of object;

  TCB_GetPointFromScreen = procedure(IsMouseDown: Boolean; wPnt: TlicgCoor); stdcall;

  IlicgActionTracker = interface

    procedure GetPointFromScreen(CB_Proc: TCB_GetPointFromScreen); stdcall;
    procedure Finish; stdcall;
    procedure TrackQuickSelect(const TrackID: string); stdcall;
    procedure TrackSelectEntity(const TrackID: string; const LayerName: string = '';
      SelClear: Boolean = True; FormLink: TForm = nil); stdcall;
    procedure TrackAreaClose(const TrackID: string); stdcall;
    procedure TrackGetPolygon(const TrackID: string); stdcall;
    procedure TrackGetPolyline(const TrackID: string); stdcall;

    { track entity given in TrackedEntity parameter. You must prebuilt the
      entity with your choice of attributes.
      This method and TrackXXX (XXX=polygon,polyline,etc) will fire event
      OnTrackedEntity.
      You can define the TrackedEntity variable instance to nil in that event
      in order to avoid this method to dispose internally }
    procedure TrackEntity(const TrackID: string; TrackedEntity: IlicgEntity); stdcall;
    { track entity defined by name of method. Entity is built
      internally with default attributes.
      For other entities you must use previous method: TrackEntity. In event
      OnTrackedEntity you can define variable instance to nil if you will
      use the entity internally and the instance will not be disposed
      internally }
    procedure TrackLine(const TrackID: string; MultiLineDraw: Boolean = True); stdcall;
    procedure TrackPolygon(const TrackID: string); stdcall;
    procedure TrackPolyline(const TrackID: string); stdcall;
    procedure TrackPolygonFigure(const TrackID: string); stdcall;
    procedure TrackSketch(const TrackID: string); stdcall;
    procedure TrackRectangle(const TrackID: string; ScaledWidth: Integer = 0;
      ScaledHeight: Integer = 0); stdcall;
    procedure TrackPlace(const TrackID: string; SymTool: TlicgSymbolStyle); stdcall;
    procedure TrackBlockInsert(const TrackID: string; ABlockTool: TlicgBlockStyle); stdcall;
    procedure TrackArc3P(const TrackID: string); stdcall;
    procedure TrackArcSE(const TrackID: string); stdcall;
    procedure TrackEllipse(const TrackID: string); stdcall;
    procedure TrackPolyEllipse(const TrackID: string); stdcall;
    procedure TrackSpline(const TrackID: string); stdcall;
    procedure TrackBezier(const TrackID: string); stdcall;
    procedure TrackPoint(const TrackID: string); stdcall;
    { track a circle that can be draw with three methods:
      2 Geometry.Points; 3 Geometry.Points; or Center and Radius. Circle is
      created and disposed internally. This method also will fire event
       OnTrackedEntity }
    procedure TrackCircle(const TrackID: string; DrawType: TlicgCircleDrawType); stdcall;

    { track a buffer. This fire event OnTrackedEntity }
    procedure TrackBuffer(const TrackID: string; Dist: Double = 0); stdcall;

    { This method will fire event OnTrackedEntityClick when an entity on the
      map is clicked. You also can use event TlicgDrawBox.OnEntityClick }
    procedure TrackEntityClick(const TrackID: string; const LayerName: string =
      ''; HighlightClicked: Boolean = True; SelClear: Boolean = true; FormLink:
      TForm = nil); stdcall;
    procedure TrackRasterClick(const TrackID: string; transTpye:
      TlicgTransformType; const Path: widestring = ''; FormLink: TForm = nil); stdcall;

    { this method will detect which entity is under mouse when you move the
      mouse over entities on the map/drawing. Can be used for showing your own
      hints or other kind of actions. This method will fire method
      OnTrackedEntityMouseMove }
    procedure TrackEntityMouseMove(const TrackID: string; const LayerName:
      string = ''; HighlightDetected: Boolean = True); stdcall;

    { this method will track the moving of an entity on the map/drawing. If the method is
     succesful, then event OnTrackedEntityDragDrop is fired }
    procedure TrackEntityDragDrop(const TrackID: string); stdcall;
    procedure TrackGenericAction(const TrackCmd, TrackID: string; FormLink:
      TForm = nil); stdcall;
    function IsRunning: Boolean; stdcall;
    function GetCurrentAction: TlicgAction; stdcall;
    procedure SetCurrentAction(const Value: TlicgAction); stdcall;
    function GetOnTrackedEntityClick: TlicgTrackEntityClickEvent; stdcall;
    procedure SetOnTrackedEntityClick(value: TlicgTrackEntityClickEvent); stdcall;
    function GetOnTrackedRasterClick: TlicgTrackRasterClickEvent; stdcall;
    procedure SetOnTrackedRasterClick(value: TlicgTrackRasterClickEvent); stdcall;
    function GetCanDoOsnap: Boolean; stdcall;
    procedure SetCanDoOsnap(const Value: Boolean); stdcall;
    function GetCanDOAccuDraw: Boolean; stdcall;
    procedure SetCanDOAccuDraw(const Value: Boolean); stdcall;
    function GetCursor: TCursor; stdcall;
    procedure SetCursor(const Value: TCursor); stdcall;
    function GetMouseDrawElements: TlicgMouseDrawElements; stdcall;
    procedure SetMouseDrawElements(const Value: TlicgMouseDrawElements); stdcall;
    function GetDefaultAction: Boolean; stdcall;
    procedure SetDefaultAction(const Value: Boolean); stdcall;
    function GetCurrentTrackID: string; stdcall;
    procedure SetCaption(const Value: string); stdcall;
    function GetCaptionCommand: string; stdcall;
    procedure SetCaptionCommand(const Value: string); stdcall;
    function GetHintCommand: string; stdcall;
    procedure SetHintCommand(const Value: string); stdcall;
    function GetStatusMessage: string; stdcall;
    procedure SetStatusMessage(const Value: string); stdcall;
    function GetFinished: Boolean; stdcall;
    procedure SetFinished(const Value: Boolean); stdcall;
    function GetFinishing: Boolean; stdcall;
    function GetCmdLine: TlicgBaseCmdline; stdcall;
    procedure SetCmdLine(const Value: TlicgBaseCmdline); stdcall;
    function GetIsDrawBoxMouseDown: Boolean; stdcall;
    procedure SetIsDrawBoxMouseDown(value: Boolean); stdcall;

    { events }

    function GetOnTrackedEntity: TlicgTrackedEntityEvent; stdcall;
    procedure SetOnTrackedEntity(value: TlicgTrackedEntityEvent); stdcall;
    function GetOnTrackedEntityClickedEvent: TlicgTrackedEntityEvent; stdcall; // ilker ekleme
    procedure SetOnTrackedEntityClickedEvent(const Value: TlicgTrackedEntityEvent); stdcall; // ilker ekleme
    function GetOnTrackedEntityMouseMove: TlicgTrackEntityMouseMoveEvent; stdcall;
    procedure SetOnTrackedEntityMouseMove(value: TlicgTrackEntityMouseMoveEvent); stdcall;
    function GetOnTrackedEntityDragDrop: TlicgTrackEntityDragDropEvent; stdcall;
    procedure SetOnTrackedEntityDragDrop(value: TlicgTrackEntityDragDropEvent); stdcall;

    function GetOnTrackedSelectPickedListEvent: TlicgTrackSelectPickedListEvent; stdcall; // ilker ekleme
    procedure SetOnOnTrackedSelectPickedListEvent(const Value: TlicgTrackSelectPickedListEvent); stdcall; // ilker ekleme

    function GetOnMouseDown: TlicgMouseEvent; stdcall;
    procedure SetOnMouseDown(value: TlicgMouseEvent); stdcall;
    function GetOnMouseMove: TlicgMouseMoveEvent; stdcall;
    procedure SetOnMouseMove(value: TlicgMouseMoveEvent); stdcall;
    function GetOnMouseUp: TlicgMouseEvent; stdcall;
    procedure SetOnMouseUp(value: TlicgMouseEvent); stdcall;
    function GetOnPaint: TNotifyEvent; stdcall;
    procedure SetOnPaint(value: TNotifyEvent); stdcall;
    function GetOnDblClick: TNotifyEvent; stdcall;
    procedure SetOnDblClick(value: TNotifyEvent); stdcall;
    function GetOnKeyDown: TKeyEvent; stdcall;
    procedure SetOnKeyDown(value: TKeyEvent); stdcall;
    function GetOnKeyPress: TKeyPressEvent; stdcall;
    procedure SetOnKeyPress(value: TKeyPressEvent); stdcall;
    function GetOnKeyUp: TKeyEvent; stdcall;
    procedure SetOnKeyUp(value: TKeyEvent); stdcall;
    function GetOnActionDoCommand: TNotifyEvent; stdcall;
    procedure SetOnActionDoCommand(value: TNotifyEvent); stdcall;
    function GetOnSuspendOperation: TNotifyEvent; stdcall;
    procedure SetOnSuspendOperation(value: TNotifyEvent); stdcall;
    function GetOnContinueOperation: TNotifyEvent; stdcall;
    procedure SetOnContinueOperation(value: TNotifyEvent); stdcall;
    function GetOnUndo: TNotifyEvent; stdcall;
    procedure SetOnUndo(value: TNotifyEvent); stdcall;
    function GetOnInitialize: TNotifyEvent; stdcall;
    procedure SetOnInitialize(value: TNotifyEvent); stdcall;
    function GetOnFinished: TNotifyEvent; stdcall;
    procedure SetOnFinished(value: TNotifyEvent); stdcall;
    function GetIsCmdLineClear: Boolean; stdcall;
    procedure SetIsCmdLineClear(Value: Boolean); stdcall;
    function GetIsDefaultInitialization: Boolean; stdcall;
    procedure SetIsDefaultInitialization(const Value: Boolean); stdcall;
    property CurrentAction: TlicgAction read GetCurrentAction write SetCurrentAction;
    property CanDoOsnap: Boolean read GetCanDoOsnap write SetCanDoOsnap;
    property CanDoAccuDraw: Boolean read GetCanDOAccuDraw write SetCanDOAccuDraw;
    property Cursor: TCursor read GetCursor write SetCursor;
    property MouseDrawElements: TlicgMouseDrawElements read GetMouseDrawElements
      write SetMouseDrawElements;
    property DefaultAction: Boolean read GetDefaultAction write SetDefaultAction;
    property CurrentTrackID: string read GetCurrentTrackID;
    property TrackID: string read GetCurrentTrackID;
    property CaptionCommand: string read GetCaptionCommand; // write SetCaptionCommand;
    property HintCommand: string read GetHintCommand write SetHintCommand;
    property Caption: string read GetHintCommand write SetHintCommand;
    property StatusMessage: string read GetStatusMessage write SetStatusMessage;
    property Finished: Boolean read GetFinished write SetFinished;
    property Finishing: Boolean read GetFinishing;
    property CmdLine: TlicgBaseCmdline read GetCmdLine write SetCmdLine;
    property IsDrawBoxMouseDown: Boolean read GetIsDrawBoxMouseDown write
      SetIsDrawBoxMouseDown;
    property IsCmdLineClear: Boolean read GetIsCmdLineClear write SetIsCmdLineClear;
    property IsDefaultInitialization: Boolean read GetIsDefaultInitialization write SetIsDefaultInitialization;

    { events }
    property OnTrackedEntityClick: TlicgTrackEntityClickEvent read
      GetOnTrackedEntityClick write SetOnTrackedEntityClick;
    property OnTrackedEntityClicked: TlicgTrackedEntityEvent read
      GetOnTrackedEntityClickedEvent write SetOnTrackedEntityClickedEvent; // ilker ekleme

    property OnTrackedRasterClick: TlicgTrackRasterClickEvent read
      GetOnTrackedRasterClick write SetOnTrackedRasterClick;

    property OnTrackedEntity: TlicgTrackedEntityEvent read GetOnTrackedEntity
      write SetOnTrackedEntity;
    property OnTrackedEntityMouseMove: TlicgTrackEntityMouseMoveEvent read
      GetOnTrackedEntityMouseMove write SetOnTrackedEntityMouseMove;
    property OnTrackedEntityDragDrop: TlicgTrackEntityDragDropEvent read
      GetOnTrackedEntityDragDrop write SetOnTrackedEntityDragDrop;

    property OnTrackedSelectPickedList: TlicgTrackSelectPickedListEvent read
      GetOnTrackedSelectPickedListEvent write SetOnOnTrackedSelectPickedListEvent; // ilker ekleme

    property OnMouseDown: TlicgMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TlicgMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TlicgMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
    property OnActionDoCommand: TNotifyEvent read GetOnActionDoCommand write
      SetOnActionDoCommand;
    property OnSuspendOperation: TNotifyEvent read GetOnSuspendOperation write
      SetOnSuspendOperation;
    property OnContinueOperation: TNotifyEvent read GetOnContinueOperation write
      SetOnContinueOperation;
    property OnUndo: TNotifyEvent read GetOnUndo write SetOnUndo;
    property OnInitialize: TNotifyEvent read GetOnInitialize write SetOnInitialize;
    property OnFinished: TNotifyEvent read GetOnFinished write SetOnFinished;
  end;

  IlicgAction = interface
    ['{7140A8D0-01E8-4BF2-8A64-22D1DED5C843}']
    function GetGlobalTempEntity: IlicgEntity; stdcall;
    function GetActionObjID: TlicgActionObjID; stdcall;
    procedure SetLauncher(const Value: IlicgActionTracker); stdcall;
    function GetLauncher: IlicgActionTracker; stdcall;
    procedure SetCaption(const Value: string); stdcall;
    procedure SetCaptionCommand(const Value: string); stdcall;
    procedure SetHintCommand(const Value: string); stdcall;
    { This is used in order to cancel a command if a DrawBox is not
      of the expected type }
    procedure SetDefaultKeyboardInput(const Value: string); stdcall;
    function GetActionID: string; stdcall;
    function GetCanBeSuspended: Boolean; stdcall;
    function GetCanDoAccuDraw: Boolean; stdcall;
    function GetCanDoOSNAP: Boolean; stdcall;
    function GetCaptionCommand: string; stdcall;
    function GetHintCommand: string; stdcall;
    function GetChainedTo: TlicgAction; stdcall;
    function GetCmdLine: TlicgBaseCmdline; stdcall;
    function GetCursor: TCursor; stdcall;
    function GetDefaultKeyboardInput: string; stdcall;
    function GetFinished: Boolean; stdcall;
    function GetInfoForPrevious: string; stdcall;
    function GetLastClicked: TlicgCoor; stdcall;
    function GetMouseDrawElements: TlicgMouseDrawElements; stdcall;
    function GetUserCommand: TlicgInputType; stdcall;
    function GetUserString: string; stdcall;
    function GetUserValue: Double; stdcall;
    function GetWaitingMouseClick: Boolean; stdcall;
    procedure SetActionID(const Value: string); stdcall;
    procedure SetCanBeSuspended(const Value: Boolean); stdcall;
    procedure SetCanDoAccuDraw(const Value: Boolean); stdcall;
    procedure SetCanDoOSNAP(const Value: Boolean); stdcall;
    procedure SetChainedTo(const Value: TlicgAction); stdcall;
    procedure SetCmdLine(const Value: TlicgBaseCmdline); stdcall;
    procedure SetCursor(const Value: TCursor); stdcall;
    procedure SetFinished(const Value: Boolean); stdcall;
    procedure SetInfoForPrevious(const Value: string); stdcall;
    procedure SetLastClicked(const Value: TlicgCoor); stdcall;
    procedure SetMouseDrawElements(const Value: TlicgMouseDrawElements); stdcall;
    procedure SetUserCommand(const Value: TlicgInputType); stdcall;
    procedure SetUserString(const Value: string); stdcall;
    procedure SetUserValue(const Value: Double); stdcall;
    procedure SetWaitingMouseClick(const Value: Boolean); stdcall;
    function GetFinishing: Boolean; stdcall;
    procedure SetFinishing(const Value: Boolean); stdcall;
    function GetActionNeeds: TlicgActionNeeds; stdcall;
    procedure SetActionNeeds(value: TlicgActionNeeds); stdcall;
    function GetOnMouseDown: TlicgMouseEvent; stdcall;
    procedure SetOnMouseDown(value: TlicgMouseEvent); stdcall;
    function GetOnMouseMove: TlicgMouseMoveEvent; stdcall;
    procedure SetOnMouseMove(value: TlicgMouseMoveEvent); stdcall;
    function GetOnMouseUp: TlicgMouseEvent; stdcall;
    procedure SetOnMouseUp(value: TlicgMouseEvent); stdcall;
    function GetOnPaint: TNotifyEvent; stdcall;
    procedure SetOnPaint(value: TNotifyEvent); stdcall;
    function GetOnDblClick: TNotifyEvent; stdcall;
    procedure SetOnDblClick(value: TNotifyEvent); stdcall;
    function GetOnKeyDown: TKeyEvent; stdcall;
    procedure SetOnKeyDown(value: TKeyEvent); stdcall;
    function GetOnKeyPress: TlicgKeyPressEvent; stdcall;
    procedure SetOnKeyPress(value: TlicgKeyPressEvent); stdcall;
    function GetOnKeyUp: TKeyEvent; stdcall;
    procedure SetOnKeyUp(value: TKeyEvent); stdcall;
    function GetOnActionDoCommand: TNotifyEvent; stdcall;
    procedure SetOnActionDoCommand(value: TNotifyEvent); stdcall;
    function GetOnSuspendOperation: TNotifyEvent; stdcall;
    procedure SetOnSuspendOperation(value: TNotifyEvent); stdcall;
    function GetOnContinueOperation: TNotifyEvent; stdcall;
    procedure SetOnContinueOperation(value: TNotifyEvent); stdcall;
    function GetOnUndo: TNotifyEvent; stdcall;
    procedure SetOnUndo(value: TNotifyEvent); stdcall;
    function GetOnInitialize: TNotifyEvent; stdcall;
    procedure SetOnInitialize(value: TNotifyEvent); stdcall;
    function GetNeedCopyMode: boolean; stdcall;
    procedure ParseUserCommand(const Cmd: AnsiString); stdcall;
    procedure EraseFullViewCursor; stdcall;
    procedure SetFullViewCursorPos(const Pt: TlicgCoor); stdcall;
    procedure DrawFullViewCursor(Sender: TObject = nil); stdcall;
    function AcceptDrawBox: Boolean; stdcall;
    procedure UndoOperation; stdcall;
    procedure SuspendOperation; stdcall;
    procedure ContinueOperation; stdcall;
    function CurrentActionNeedCoorCalc: boolean; stdcall;
    function CurrentActionQuickSelect: boolean; stdcall;
    property ActionObjID: TlicgActionObjID read GetActionObjID;

    { When this property is assigned, it will be linked to the Action that is
      pointing to }
    property Finished: Boolean read GetFinished write SetFinished;
    property WaitingMouseClick: Boolean read GetWaitingMouseClick write
      SetWaitingMouseClick;
    property InfoForPrevious: string read GetInfoForPrevious write SetInfoForPrevious;
    property UserCommand: TlicgInputType read GetUserCommand write SetUserCommand;
    property UserString: string read GetUserString write SetUserString;
    property UserValue: Double read GetUserValue write SetUserValue;
    property MouseDrawElements: TlicgMouseDrawElements read GetMouseDrawElements
      write SetMouseDrawElements;
    property LastClicked: TlicgCoor read GetLastClicked write SetLastClicked;

    { previous version these properties was in the published section }
    property ChainedTo: TlicgAction read GetChainedTo write SetChainedTo;
    property CanDoOSNAP: Boolean read GetCanDoOSNAP write SetCanDoOSNAP;
    property CanDoAccuDraw: Boolean read GetCanDoAccuDraw write SetCanDoAccuDraw;
    property ActionID: string read GetActionID write SetActionID;
    property CanBeSuspended: Boolean read GetCanBeSuspended write SetCanBeSuspended;
    property CaptionCommand: string read GetCaptionCommand; //write SetCaptionCommand;
    property HintCommand: string read GetHintCommand write SetHintCommand;
    property Cursor: TCursor read GetCursor write SetCursor;
    property CmdLine: TlicgBaseCmdline read GetCmdLine write SetCmdLine;
    property DefaultKeyboardInput: string read GetDefaultKeyboardInput write
      SetDefaultKeyboardInput;
    property Finishing: Boolean read GetFinishing write SetFinishing;
    property Launcher: IlicgActionTracker read GetLauncher write SetLauncher;
    property ActionNeeds: TlicgActionNeeds read GetActionNeeds write SetActionNeeds;

    { events }
    property OnMouseDown: TlicgMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TlicgMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TlicgMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TlicgKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
    property OnActionDoCommand: TNotifyEvent read GetOnActionDoCommand write
      SetOnActionDoCommand;
    property OnSuspendOperation: TNotifyEvent read GetOnSuspendOperation write
      SetOnSuspendOperation;
    property OnContinueOperation: TNotifyEvent read GetOnContinueOperation write
      SetOnContinueOperation;
    property OnUndo: TNotifyEvent read GetOnUndo write SetOnUndo;
    property OnInitialize: TNotifyEvent read GetOnInitialize write SetOnInitialize;
  end;

  IlicgEntityClickAction = interface(IlicgAction)
    ['{9A39AFB7-0513-4218-B2BB-5DF064ED5795}']
    function GetLayer: TlicgBaseLayer; stdcall;
    procedure SetLayer(Value: TlicgBaseLayer); stdcall;
    function GetRecno: Integer; stdcall;
    procedure SetRecno(Value: Integer); stdcall;
    property Layer: TlicgBaseLayer read GetLayer write SetLayer;
    property RecNo: integer read GetRecno write SetRecno;
  end;

  IlicgRasterClickAction = interface(IlicgAction)
    ['{453CECE5-9050-42A7-99EF-697692D795C1}']
    function GetPath: widestring; stdcall;
    function GetTransformGCP: IlicgTransformGCP; stdcall;
    function Xmin_Native: double; stdcall;
    function Ymin_Native: double; stdcall;
    function Xmax_Native: double; stdcall;
    function Ymax_Native: double; stdcall;
    function Xmin_Projected: double; stdcall;
    function Ymin_Projected: double; stdcall;
    function Xmax_Projected: double; stdcall;
    function Ymax_Projected: double; stdcall;
  end;

  TlicgAction = class(TInterfacedPersistent, IlicgAction)
  private
    FCmdLine: TlicgBaseCmdline;
    FLauncher: IlicgActionTracker; { defined when launched from TlicgActionTracker }
    //FOldCursor : TCursor;
    FCursor: TCursor; // the cursor used in the DrawBox by the Action
    FFinished: Boolean;
   // FCaptionCommand: string;
    FHintCommand: string;
    //FOldHintCommand: string;
    FActionID: string;
    FCanBeSuspended: Boolean;
    FWaitingMouseClick: Boolean;
    FInfoForPrevious: string;
    FCanDoOSNAP: Boolean;
    FCanDoAccuDraw: Boolean;
    { The following data is updated when the command line is parsed }
    FUserCommand: TlicgInputType;
    FUserString: string;
    FUserValue: Double;
    { entity used for drawing the cursor }
    FFullViewCursor: IlicgEntity;
    FCursorFrame: IlicgEntity;
    FMouseDrawElements: TlicgMouseDrawElements;
    { After finish executing this one, start executing this }
    FChainedTo: TlicgAction;
    FLastClicked: TlicgCoor;
    FDefaultKeyboardInput: string;
    FFinishing: Boolean;
    FOnMouseDown: TlicgMouseEvent;
    FOnMouseMove: TlicgMouseMoveEvent;
    FOnMouseUp: TlicgMouseEvent;
    FOnPaint: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TlicgKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnActionDoCommand: TNotifyEvent;
    FOnSuspendOperation: TNotifyEvent;
    FOnContinueOperation: TNotifyEvent;
    FOnUndo: TNotifyEvent;
    FOnInitialize: TNotifyEvent;
    FActionNeeds: TlicgActionNeeds;
    FActionParameterType: TActionParameterType;
    FActionParameterPointerData: pointer;
    FActionParameterProperties: IActionCommandPropertyList;
    FActionParameterGetEntityProc: TActionParameterGetEntityProc;
    FActionParameterForm: TForm;
    FActionNeedActiveLayerShow: Boolean;
    FNoPickFilter: TlicgEntityIDs;
    function GetActionParameterGetEntity: IlicgEntity;
  protected
    function GetGlobalTempEntity: IlicgEntity; virtual; stdcall;
    function GetActionObjID: TlicgActionObjID; virtual; stdcall;
    procedure SetLauncher(const Value: IlicgActionTracker); stdcall;
    function GetLauncher: IlicgActionTracker; stdcall;
    procedure SetCaption(const Value: string); stdcall;
    procedure SetCaptionCommand(const Value: string); stdcall;
    procedure SetHintCommand(const Value: string); stdcall;
    { This is used in order to cancel a command if a DrawBox is not
      of the expected type }
    procedure SetDefaultKeyboardInput(const Value: string); stdcall;
    function GetActionID: string; stdcall;
    function GetCanBeSuspended: Boolean; stdcall;
    function GetCanDoAccuDraw: Boolean; stdcall;
    function GetCanDoOSNAP: Boolean; stdcall;
    function GetCaptionCommand: string; stdcall;
    function GetHintCommand: string; stdcall;
    function GetChainedTo: TlicgAction; stdcall;
    function GetCmdLine: TlicgBaseCmdline; stdcall;
    function GetCursor: TCursor; stdcall;
    function GetDefaultKeyboardInput: string; stdcall;
    function GetFinished: Boolean; stdcall;
    function GetInfoForPrevious: string; stdcall;
    function GetLastClicked: TlicgCoor; stdcall;
    function GetMouseDrawElements: TlicgMouseDrawElements; stdcall;
    function GetUserCommand: TlicgInputType; stdcall;
    function GetUserString: string; stdcall;
    function GetUserValue: Double; stdcall;
    function GetWaitingMouseClick: Boolean; stdcall;
    procedure SetActionID(const Value: string); stdcall;
    procedure SetCanBeSuspended(const Value: Boolean); stdcall;
    procedure SetCanDoAccuDraw(const Value: Boolean); stdcall;
    procedure SetCanDoOSNAP(const Value: Boolean); stdcall;
    procedure SetChainedTo(const Value: TlicgAction); stdcall;
    procedure SetCmdLine(const Value: TlicgBaseCmdline); stdcall;
    procedure SetCursor(const Value: TCursor); stdcall;
    procedure SetFinished(const Value: Boolean); stdcall;
    procedure SetInfoForPrevious(const Value: string); stdcall;
    procedure SetLastClicked(const Value: TlicgCoor); stdcall;
    procedure SetMouseDrawElements(const Value: TlicgMouseDrawElements); stdcall;
    procedure SetUserCommand(const Value: TlicgInputType); stdcall;
    procedure SetUserString(const Value: string); stdcall;
    procedure SetUserValue(const Value: Double); stdcall;
    procedure SetWaitingMouseClick(const Value: Boolean); stdcall;
    function GetFinishing: Boolean; stdcall;
    procedure SetFinishing(const Value: Boolean); stdcall;
    function GetActionNeeds: TlicgActionNeeds; stdcall;
    procedure SetActionNeeds(value: TlicgActionNeeds); stdcall;
    function GetOnMouseDown: TlicgMouseEvent; stdcall;
    procedure SetOnMouseDown(value: TlicgMouseEvent); stdcall;
    function GetOnMouseMove: TlicgMouseMoveEvent; stdcall;
    procedure SetOnMouseMove(value: TlicgMouseMoveEvent); stdcall;
    function GetOnMouseUp: TlicgMouseEvent; stdcall;
    procedure SetOnMouseUp(value: TlicgMouseEvent); stdcall;
    function GetOnPaint: TNotifyEvent; stdcall;
    procedure SetOnPaint(value: TNotifyEvent); stdcall;
    function GetOnDblClick: TNotifyEvent; stdcall;
    procedure SetOnDblClick(value: TNotifyEvent); stdcall;
    function GetOnKeyDown: TKeyEvent; stdcall;
    procedure SetOnKeyDown(value: TKeyEvent); stdcall;
    function GetOnKeyPress: TlicgKeyPressEvent; stdcall;
    procedure SetOnKeyPress(value: TlicgKeyPressEvent); stdcall;
    function GetOnKeyUp: TKeyEvent; stdcall;
    procedure SetOnKeyUp(value: TKeyEvent); stdcall;
    function GetOnActionDoCommand: TNotifyEvent; stdcall;
    procedure SetOnActionDoCommand(value: TNotifyEvent); stdcall;
    function GetOnSuspendOperation: TNotifyEvent; stdcall;
    procedure SetOnSuspendOperation(value: TNotifyEvent); stdcall;
    function GetOnContinueOperation: TNotifyEvent; stdcall;
    procedure SetOnContinueOperation(value: TNotifyEvent); stdcall;
    function GetOnUndo: TNotifyEvent; stdcall;
    procedure SetOnUndo(value: TNotifyEvent); stdcall;
    function GetOnInitialize: TNotifyEvent; stdcall;
    procedure SetOnInitialize(value: TNotifyEvent); stdcall;
    function GetNeedCopyMode: boolean; virtual; stdcall;
  public
    procedure ParseUserCommand(const Cmd: AnsiString); stdcall;
    procedure EraseFullViewCursor; stdcall;
    procedure SetFullViewCursorPos(const Pt: TlicgCoor); stdcall;
    procedure DrawFullViewCursor(Sender: TObject = nil); stdcall;
    function AcceptDrawBox: Boolean; stdcall;
    constructor CreateAction(CmdLine: TlicgBaseCmdline);
    destructor Destroy; override;
    procedure UndoOperation; dynamic; stdcall;
    procedure SuspendOperation; stdcall;
    procedure ContinueOperation; stdcall;
    function CurrentActionNeedCoorCalc: boolean; virtual; stdcall;
    function CurrentActionQuickSelect: boolean; virtual; stdcall;

    property ActionObjID: TlicgActionObjID read GetActionObjID;
    { When this property is assigned, it will be linked to the Action that is pointing to }
    property Finished: Boolean read GetFinished write SetFinished;
    property WaitingMouseClick: Boolean read GetWaitingMouseClick write
      SetWaitingMouseClick;
    property InfoForPrevious: string read GetInfoForPrevious write SetInfoForPrevious;
    property UserCommand: TlicgInputType read GetUserCommand write SetUserCommand;
    property UserString: string read GetUserString write SetUserString;
    property UserValue: Double read GetUserValue write SetUserValue;
    property MouseDrawElements: TlicgMouseDrawElements read GetMouseDrawElements
      write SetMouseDrawElements;
    property LastClicked: TlicgCoor read GetLastClicked write SetLastClicked;
    { previous version these properties was in the published section }
    property ChainedTo: TlicgAction read GetChainedTo write SetChainedTo;
    property CanDoOSNAP: Boolean read GetCanDoOSNAP write SetCanDoOSNAP;
    property CanDoAccuDraw: Boolean read GetCanDoAccuDraw write SetCanDoAccuDraw;
    property ActionID: string read GetActionID write SetActionID;
    property CanBeSuspended: Boolean read GetCanBeSuspended write SetCanBeSuspended;
    property Caption: string read GetHintCommand write SetHintCommand;
    property CaptionCommand: string read GetCaptionCommand; // write SetCaptionCommand;
    property HintCommand: string read GetHintCommand write SetHintCommand;
    property Cursor: TCursor read GetCursor write SetCursor;
    property CmdLine: TlicgBaseCmdline read GetCmdLine write SetCmdLine;
    property DefaultKeyboardInput: string read GetDefaultKeyboardInput write
      SetDefaultKeyboardInput;
    property Finishing: Boolean read GetFinishing write SetFinishing;
    property Launcher: IlicgActionTracker read GetLauncher write SetLauncher;
    property ActionNeeds: TlicgActionNeeds read GetActionNeeds write SetActionNeeds;
    property NeedCopyMode: Boolean read GetNeedCopyMode;
    { events }
    property OnMouseDown: TlicgMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TlicgMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TlicgMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnPaint: TNotifyEvent read GetOnPaint write SetOnPaint;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TlicgKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
    property OnActionDoCommand: TNotifyEvent read GetOnActionDoCommand write SetOnActionDoCommand;
    property OnSuspendOperation: TNotifyEvent read GetOnSuspendOperation write SetOnSuspendOperation;
    property OnContinueOperation: TNotifyEvent read GetOnContinueOperation write SetOnContinueOperation;
    property OnUndo: TNotifyEvent read GetOnUndo write SetOnUndo;
    property OnInitialize: TNotifyEvent read GetOnInitialize write SetOnInitialize;
    property _GlobalTempEntity: IlicgEntity read GetGlobalTempEntity;
    property ActionParameterType: TActionParameterType read FActionParameterType write FActionParameterType;
    property ActionParameterPointerData: pointer read fActionParameterPointerData write fActionParameterPointerData;
    property ActionParameterProperties: IActionCommandPropertyList read FActionParameterProperties write FActionParameterProperties;
    property ActionParameterGetEntityProc: TActionParameterGetEntityProc read fActionParameterGetEntityProc write fActionParameterGetEntityProc;
    property ActionParameterGetEntity: IlicgEntity read GetActionParameterGetEntity;
    property ActionParameterForm: TForm read FActionParameterForm write FActionParameterForm;
    property ActionNeedActiveLayerShow: boolean read FActionNeedActiveLayerShow write FActionNeedActiveLayerShow;
    property NoPickFilter: TlicgEntityIDs read FNoPickFilter write FNoPickFilter;
  end;

  { The action for selecting entities }
  TlicgScaleAxis = (saScaleBoth, saScaleHorizontal, saScaleVertical);

  IlicgAccuDraw = interface
    ['{43CD0413-BCC1-4D4B-A44A-7479B15F15F4}']
    procedure ClearDeltaLocked; stdcall;
    function GetInUpdate: boolean; stdcall;
    procedure SetInUpdate(value: boolean); stdcall;
    function GetPerpendFound: Boolean; stdcall;
    procedure SetPerpendFound(value: Boolean); stdcall;
    function GetPerpendFrom: TlicgCoor; stdcall;
    procedure SetPerpendFrom(value: TlicgCoor); stdcall;
    function GetRubberList: IlicgEntityList; stdcall;
    procedure SetDeltaXLocked(value: Boolean); stdcall;
    procedure SetDeltaYLocked(value: Boolean); stdcall;
    procedure SetFrameColor(const Value: TColor); stdcall;
    procedure SetHiliteColor(const Value: TColor); stdcall;
    procedure SetWidth(Value: Integer); stdcall;
    procedure SetXAxisColor(const Value: TColor); stdcall;
    procedure SetYAxisColor(const Value: TColor); stdcall;
    procedure SetDeltaX(const Value: Double); stdcall;
    procedure SetDeltaY(const Value: Double); stdcall;
    procedure SetShowing(const Value: Boolean); stdcall;
    procedure SetFrameStyle(const Value: TlicgFrameStyle); stdcall;
    procedure SetTolerance(const Value: Byte); stdcall;
    procedure SetEnabled(const Value: Boolean); stdcall;
    { semi-public procedures }
    procedure ShowHideRubberList(Reversed: Boolean = False); stdcall;
    procedure DrawAuxLines; stdcall;
    procedure EraseAuxLines; stdcall;
    function GetSnappedPoint: TlicgCoor; stdcall;
    procedure Draw; stdcall;
    procedure SetRotangle(const Value: Double); stdcall;
    procedure ShowRubberList(ACanvas: IlicgCanvas); stdcall;
    function GetAccuOrigin: TlicgCoor; stdcall;
    function GetAuxLinesColor: TColor; stdcall;
    function GetCtrlAltSuspend: Boolean; stdcall;
    function GetDeltaX: Double; stdcall;
    function GetDeltaXLocked: Boolean; stdcall;
    function GetDeltaY: Double; stdcall;
    function GetDeltaYLocked: Boolean; stdcall;
    function GetEnabled: Boolean; stdcall;
    function GetFrameColor: TColor; stdcall;
    function GetFrameStyle: TlicgFrameStyle; stdcall;
    function GetHiliteColor: TColor; stdcall;
    function GetPenWidth: Integer; stdcall;
    function GetReshapeAdvance: Boolean; stdcall;
    function GetRotangle: Double; stdcall;
    function GetRotateToSegments: Boolean; stdcall;
    function GetShowing: Boolean; stdcall;
    function GetSnapColor: TColor; stdcall;
    function GetSnapSameDistance: Boolean; stdcall;
    function GetSnapToAxis: Boolean; stdcall;
    function GetSnapUnRotated: Boolean; stdcall;
    function GetTolerance: Byte; stdcall;
    function GetWidth: Integer; stdcall;
    function GetXAxisColor: TColor; stdcall;
    function GetYAxisColor: TColor; stdcall;
    procedure SetAuxLinesColor(const Value: TColor); stdcall;
    procedure SetCtrlAltSuspend(const Value: Boolean); stdcall;
    procedure SetPenWidth(const Value: Integer); stdcall;
    procedure SetReshapeAdvance(const Value: Boolean); stdcall;
    procedure SetRotateToSegments(const Value: Boolean); stdcall;
    procedure SetSnapColor(const Value: TColor); stdcall;
    procedure SetSnapSameDistance(const Value: Boolean); stdcall;
    procedure SetSnapToAxis(const Value: Boolean); stdcall;
    procedure SetSnapUnRotated(const Value: Boolean); stdcall;
    procedure Reset; stdcall;
    procedure UpdatePosition(const FromPt, ToPt: TlicgCoor; Reversed: Boolean =
      False); stdcall;
    procedure ChangeOrigin(const Origin: TlicgCoor; const Angle: Double = 0); stdcall;
    procedure ShowUnRotated; stdcall;
    procedure CurrentDimensions(var DX, DY: Double); stdcall;
    procedure Change; stdcall;
    property Showing: Boolean read GetShowing write SetShowing;
    { displacement of current point (FAccuOrigin) }
    property DeltaX: Double read GetDeltaX write SetDeltaX;
    property DeltaY: Double read GetDeltaY write SetDeltaY;
    { same as above but for polar }
    property Dist: Double read GetDeltaX write SetDeltaX;
    property Angle: Double read GetDeltaY write SetDeltaY;

    { current origin of AccuDraw object on screen (FAccuOrigin) }
    property AccuOrigin: TlicgCoor read GetAccuOrigin;
    property Rotangle: Double read GetRotangle write SetRotangle;
    property DeltaXLocked: Boolean read GetDeltaXLocked write SetDeltaXLocked;
    property DeltaYLocked: Boolean read GetDeltaYLocked write SetDeltaYLocked;
    { same as above but for polar }
    property DistLocked: Boolean read GetDeltaXLocked write SetDeltaXLocked;
    property AngleLocked: Boolean read GetDeltaYLocked write SetDeltaYLocked;
    property FrameStyle: TlicgFrameStyle read GetFrameStyle write SetFrameStyle;

    { published properties }
    property PenWidth: Integer read GetPenWidth write SetPenWidth;
    property ReshapeAdvance: Boolean read GetReshapeAdvance write SetReshapeAdvance;
    property SnapUnRotated: Boolean read GetSnapUnRotated write SetSnapUnRotated;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Width: Integer read GetWidth write SetWidth;
    property XAxisColor: TColor read GetXAxisColor write SetXAxisColor;
    property YAxisColor: TColor read GetYAxisColor write SetYAxisColor;
    property HiliteColor: TColor read GetHiliteColor write SetHiliteColor;
    property FrameColor: TColor read GetFrameColor write SetFrameColor;
    property SnapColor: TColor read GetSnapColor write SetSnapColor;
    property AuxLinesColor: TColor read GetAuxLinesColor write SetAuxLinesColor;
    property SnapToAxis: Boolean read GetSnapToAxis write SetSnapToAxis;
    property Tolerance: Byte read GetTolerance write SetTolerance;
    property RotateToSegments: Boolean read GetRotateToSegments write SetRotateToSegments;
    property SnapSameDistance: Boolean read GetSnapSameDistance write SetSnapSameDistance;
    property CtrlAltSuspend: Boolean read GetCtrlAltSuspend write SetCtrlAltSuspend;
    property InUpdate: boolean read GetInUpdate write SetInUpdate;
  end;



  { the info for the current snapped point }
  TlicgAccuSnapInfo = record
    { position on the viewport in pixels }
    Pos: TPoint;
    { the current bitmap Visible on screen }
    Picture: TcxAlphaBitmap;
    { the current snap setting }
    SnapSetting: TlicgOSNAPSetting;
    { the current snapped point }
    SnapPoint: TlicgCoor;
    { the current vertex snapped point }
    VertexSnapPoint: TlicgCoor;
    { the entity that was accusnapped }
    Layer: TlicgBaseLayer;
    Recno: Integer;
    { is currently showing on screen ? }
    Showing: Boolean;
    { used when SnapSetting = osPerpend, osParallel }
    RefFrom: TlicgCoor;
    RefTo: TlicgCoor;
    isSnappedGrid: Boolean;
  end;

  IlicgAccuSnap = interface
    ['{86F2E946-BA32-4EFB-8D1C-ABD1B36A3EE6}']
    function GetOwner: TlicgBaseCmdline; stdcall;
    procedure EraseFromScreen; stdcall;
    procedure UpdateAccuSnapEntity; stdcall;
    procedure DrawAccuSnap(Draw: Boolean); stdcall;
    procedure SetEnabled(const Value: Boolean); stdcall;
    procedure SetOverrideOsnapSetting(const Value: TlicgOSNAPSetting); stdcall;
    //procedure SetOsnapSetting(const Value: TlicgOSNAPSetting);stdcall;
    //procedure SetOverrideOsnap(const Value: Boolean);stdcall;
    procedure ShowHideRubberList(Reversed: Boolean = False); stdcall;
    function GetCSDisabled: Boolean; stdcall;
    function GetCtrlShiftSuspend: Boolean; stdcall;
    function GetEnabled: Boolean; stdcall;
    function GetHiliteSnapped: Boolean; stdcall;
    //function  GetOsnapSetting: TlicgOSNAPSetting;stdcall;
    //function  GetOverrideOsnap: Boolean;stdcall;
    function GetOverrideOsnapSetting: TlicgOSNAPSetting; stdcall;
    function GetSensitivity: Byte; stdcall;
    function GetSnapDivisor: Byte; stdcall;
    function GetSnapLayerName: string; stdcall;
    function GetVertexSnapEnabled: Boolean; stdcall;
    function GetVertexSnapList: TlicgVertexRefList; stdcall;
    procedure SetCSDisabled(const Value: Boolean); stdcall;
    procedure SetCtrlShiftSuspend(const Value: Boolean); stdcall;
    procedure SetHiliteSnapped(const Value: Boolean); stdcall;
    procedure SetSensitivity(const Value: Byte); stdcall;
    procedure SetSnapDivisor(const Value: Byte); stdcall;
    procedure SetSnapLayerName(const Value: string); stdcall;
    procedure SetVertexSnapEnabled(const Value: Boolean); stdcall;

    //function GetOnlyDrawingWithSnap: Boolean;stdcall;
    //procedure SetOnlyDrawingWithSnap(value: Boolean);stdcall;

    function GetCurrentSnapInfo: TlicgAccuSnapInfo; stdcall;
    procedure SetCurrentSnapInfo(value: TlicgAccuSnapInfo); stdcall;
    function GetCurrentOsnapSetting: TlicgOsnapSetting; stdcall;
    procedure Change; stdcall;
    function IsSnappedNow: Boolean; stdcall;
    procedure DrawAuxLines; stdcall;
    procedure EraseAuxLines; stdcall;
    procedure EraseVertexSnapList; stdcall;
    property VertexSnapList: TlicgVertexRefList read GetVertexSnapList;
    property SnapLayerName: string read GetSnapLayerName write SetSnapLayerName;
    property OverrideOsnapSetting: TlicgOSNAPSetting read
      GetOverrideOsnapSetting write SetOverrideOsnapSetting;
    //property OverrideOsnap: Boolean read GetOverrideOsnap write SetOverrideOsnap;
    property HiliteSnapped: Boolean read GetHiliteSnapped write SetHiliteSnapped;
    property Sensitivity: Byte read GetSensitivity write SetSensitivity;
    //property OsnapSetting: TlicgOSNAPSetting read GetOsnapSetting write SetOsnapSetting;
    property SnapDivisor: Byte read GetSnapDivisor write SetSnapDivisor;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property CtrlShiftSuspend: Boolean read GetCtrlShiftSuspend write SetCtrlShiftSuspend;
    property VertexSnapEnabled: Boolean read GetVertexSnapEnabled write
      SetVertexSnapEnabled;
    property CSDisabled: Boolean read GetCSDisabled write SetCSDisabled;
    property CurrentSnapInfo: TlicgAccuSnapInfo read GetCurrentSnapInfo write
      SetCurrentSnapInfo;

    //?property OnlyDrawingWithSnap: Boolean read GetOnlyDrawingWithSnap write SetOnlyDrawingWithSnap;

    property Owner: TlicgBaseCmdline read GetOwner;
  end;

{Type
  PlicgViewPort = ^TlicgViewport;
  TlicgViewPort = record
    vp_form: TForm;
    vp_Drawbox: TlicgBaseDrawbox;
    vp_CmdLine: TlicgBaseCmdline;
  end; }

var
  FAccuSnapPicFocused: TcxAlphaBitmap;

const
  fmKeySpace: Tform = nil;

function _ActionAddNewEntity(CmdLine: TlicgBaseCmdline; Entity: IlicgEntity;
  isUndo: Boolean = True; ApplyLayerProps: Boolean = True): Integer;

type
  { Follows the list of draw box that can be connected to the same TlicgCmdLine }
  { Define TlicgDrawBoxItem }

  TlicgDrawBoxItem = class(TCollectionItem)
  private
    FDrawBox: TlicgBaseDrawBox;
    FCurrent: Boolean;
    FSavedMouseDown: TlicgMouseEvent;
    FSavedMouseMove: TlicgMouseMoveEvent;
    FSavedMouseUp: TlicgMouseEvent;
    FSavedClick: TNotifyEvent;
    FSavedDblClick: TNotifyEvent;
    FSavedKeyPress: TKeyPressEvent;
    FSavedKeyDown: TKeyEvent;
    FSavedKeyUp: TKeyEvent;
    FSavedPaint: TNotifyEvent;
    procedure SetDrawBox(Value: TlicgBaseDrawBox);
    function GetDrawBox: TlicgBaseDrawBox;
    function GetCurrent: Boolean;
    procedure SetCurrent(Value: Boolean);
    procedure InternalMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
      Integer; const WX, WY: Double);
  protected
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property DrawBox: TlicgBaseDrawbox read GetDrawBox write SetDrawBox;
    property Current: Boolean read GetCurrent write SetCurrent;
    property SavedMouseDown: TlicgMouseEvent read FSavedMouseDown write FSavedMouseDown;
    property SavedMouseMove: TlicgMouseMoveEvent read FSavedMouseMove write
      FSavedMouseMove;
    property SavedMouseUp: TlicgMouseEvent read FSavedMouseUp write FSavedMouseUp;
    property SavedClick: TNotifyEvent read FSavedClick write FSavedClick;
    property SavedDblClick: TNotifyEvent read FSavedDblClick write FSavedDblClick;
    property SavedKeyPress: TKeyPressEvent read FSavedKeyPress write FSavedKeyPress;
    property SavedKeyDown: TKeyEvent read FSavedKeyDown write FSavedKeyDown;
    property SavedKeyUp: TKeyEvent read FSavedKeyUp write FSavedKeyUp;
    property SavedPaint: TNotifyEvent read FSavedPaint write FSavedPaint;
  end;

  { Define TlicgDrawBoxCollection }

  TlicgDrawBoxCollection = class(TOwnedCollection)
  private
    FCmdLine: TlicgBaseCmdLine;
    function GetItem(Index: Integer): TlicgDrawBoxItem;
    procedure SetItem(Index: Integer; Value: TlicgDrawBoxItem);
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    function Add: TlicgDrawBoxItem;
    function Find(DrawBox: TlicgBaseDrawBox): Boolean;
    function FindCurrent: TlicgDrawBoxItem;
    function FindItem(Sender: TObject): TlicgDrawBoxItem;
    procedure SetCurrent(Value: TObject);
    property Items[Index: Integer]: TlicgDrawBoxItem read GetItem write SetItem; default;
  end;

implementation

uses
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.Math,
  Lider.CG.Com.ModulesConsts;


{ Define TlicgAction }

constructor TlicgAction.CreateAction(CmdLine: TlicgBaseCmdline);
begin
  inherited Create;
  FActionNeedActiveLayerShow := True;
  FNoPickFilter := [];

  fActionParameterPointerData := nil;
  fActionParameterGetEntityProc := nil;

  FActionParameterType := aptNone;
  FActionParameterProperties := TActionCommandPropertyList.Create;

  FLastClicked := _INVALID_COOR;

  FCmdLine := CmdLine;
  FCmdLine.CaptionCommand := '';

  FCanBeSuspended := True;

  // for drawing a full view cursor
  FFullViewCursor := Licad.CreateEntityFactory.MakePolyline([_NULL_COOR,
    _NULL_COOR, _NULL_COOR, _NULL_COOR]);
  with FFullViewCursor.Geometry.Points do
  begin
    AddPart(2);
  end;
  FCursorFrame := Licad.CreateEntityFactory.MakeRectangle(_NULL_COOR, _NULL_COOR);
  FCursorFrame.Geometry.ID := -2; // flag for not drawing interior axis when rubber banding

  FMouseDrawElements := [mdCursor];
  FCursor := crDefault;

  Launcher := nil;
end;

destructor TlicgAction.Destroy;
begin
  FActionParameterProperties := nil;
  with FCmdLine do
  begin
    if Assigned(FCmdLine) and (FCmdLine.GetTheDefaultAction <> Self) and Assigned(OnAfterCommand) then
      if FCmdLine.ActiveDrawBox.GIS.Active then
        TlicgAfterCommandEvent(OnAfterCommand)(FCmdLine, LastCommand, LastActionID);
  end;
  if Assigned(FLauncher) then // ilker ez son sürümüne göre ekledim.
    IlicgActionTracker(FLauncher).CurrentAction := nil;
  FFullViewCursor := nil;
  FCursorFrame := nil;
  inherited;
end;

function TlicgAction.GetFinishing: Boolean;
begin
  Result := FFinishing;
end;

procedure TlicgAction.SetFinishing(const Value: Boolean);
begin
  FFinishing := Value;
end;

function TlicgAction.GetActionID: string;
begin
  Result := FActionID
end;

function TlicgAction.GetCanBeSuspended: Boolean;
begin
  Result := FCanBeSuspended
end;

function TlicgAction.GetCanDoAccuDraw: Boolean;
begin
  Result := FCanDoAccuDraw
end;

function TlicgAction.GetCanDoOSNAP: Boolean;
begin
  Result := FCanDoOSNAP
end;

function TlicgAction.GetCaptionCommand: string;
begin
  if Assigned(FCmdLine) then
    Result := FCmdLine.CaptionCommand;
end;

function TlicgAction.GetChainedTo: TlicgAction;
begin
  Result := FChainedTo
end;

function TlicgAction.GetCmdLine: TlicgBaseCmdline;
begin
  Result := FCmdLine
end;

function TlicgAction.GetCursor: TCursor;
begin
  Result := FCursor
end;

function TlicgAction.GetDefaultKeyboardInput: string;
begin
  Result := FDefaultKeyboardInput
end;

function TlicgAction.GetFinished: Boolean;
begin
  Result := FFinished
end;

function TlicgAction.GetInfoForPrevious: string;
begin
  Result := FInfoForPrevious
end;

function TlicgAction.GetLastClicked: TlicgCoor;
begin
  Result := FLastClicked
end;

function TlicgAction.GetMouseDrawElements: TlicgMouseDrawElements;
begin
  Result := FMouseDrawElements
end;

function TlicgAction.GetUserCommand: TlicgInputType;
begin
  Result := FUserCommand
end;

function TlicgAction.GetUserString: string;
begin
  Result := FUserString
end;

function TlicgAction.GetUserValue: Double;
begin
  Result := FUserValue
end;

function TlicgAction.GetWaitingMouseClick: Boolean;
begin
  Result := FWaitingMouseClick
end;

procedure TlicgAction.SetActionID(const Value: string);
begin
  FActionID := Value
end;

procedure TlicgAction.SetCanBeSuspended(const Value: Boolean);
begin
  FCanBeSuspended := Value
end;

procedure TlicgAction.SetCanDoAccuDraw(const Value: Boolean);
begin
  FCanDoAccuDraw := Value
end;

procedure TlicgAction.SetCanDoOSNAP(const Value: Boolean);
begin
  FCanDoOSNAP := Value
end;

procedure TlicgAction.SetChainedTo(const Value: TlicgAction);
begin
  FChainedTo := Value
end;

procedure TlicgAction.SetCmdLine(const Value: TlicgBaseCmdline);
begin
  FCmdLine := Value
end;

procedure TlicgAction.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
end;

procedure TlicgAction.SetFinished(const Value: Boolean);
begin
  FFinished := Value
end;

procedure TlicgAction.SetInfoForPrevious(const Value: string);
begin
  FInfoForPrevious := Value
end;

procedure TlicgAction.SetLastClicked(const Value: TlicgCoor);
begin
  FLastClicked := Value
end;

procedure TlicgAction.SetMouseDrawElements(const Value: TlicgMouseDrawElements);
begin
  if not (mdCursor in Value) then
    FMouseDrawElements := [mdCursor] + value
  else
    FMouseDrawElements := Value
end;

procedure TlicgAction.SetUserCommand(const Value: TlicgInputType);
begin
  FUserCommand := Value
end;

procedure TlicgAction.SetUserString(const Value: string);
begin
  FUserString := Value
end;

procedure TlicgAction.SetUserValue(const Value: Double);
begin
  FUserValue := Value
end;

procedure TlicgAction.SetWaitingMouseClick(const Value: Boolean);
begin
  FWaitingMouseClick := Value
end;

procedure TlicgAction.DrawFullViewCursor(Sender: TObject = nil);
var
  DrawBox: TlicgBaseDrawBox;
begin
  if Sender = nil then
    DrawBox := FCmdLine.ActiveDrawBox
  else
    DrawBox := Sender as TlicgBaseDrawbox;
  with DrawBox do
  begin
    RubberPenStyle := psSolid;
    if FCmdLine.UseFullViewCursor and (mdFullViewCursor in FMouseDrawElements) then
      DrawEntityRubberBand(FFullViewCursor);
    if mdCursorFrame in FMouseDrawElements then // Or (Self.CanDoOsnap And FCmdLine.FAccuSnap.FEnabled) Then
      DrawEntityRubberBand(Self.FCursorFrame);
  end;
end;

{ draw in reverse order in order to erase }

procedure TlicgAction.EraseFullViewCursor;
begin
  with FCmdLine.ActiveDrawBox do
  begin
    RubberPenStyle := psSolid;
    if mdCursorFrame in FMouseDrawElements then
      DrawEntityRubberBand(Self.FCursorFrame);
    if FCmdLine.UseFullViewCursor and (mdFullViewCursor in FMouseDrawElements) then
      DrawEntityRubberBand(FFullViewCursor);
  end;
end;

function TlicgAction.GetLauncher: IlicgActionTracker;
begin
  Result := FLauncher;
end;

procedure TlicgAction.SetLauncher(const Value: IlicgActionTracker);
begin
  FLauncher := Value
end;

procedure TlicgAction.SetFullViewCursorPos(const Pt: TlicgCoor);
var
  DX, DY: Double;
begin
  { draw old position }
  with FCmdLine.ActiveDrawBox.Grapher do
  begin
    if FCmdLine.UseFullViewCursor and (mdFullViewCursor in FMouseDrawElements) then
      with FFullViewCursor, CurrentParams.VisualWindow do
      begin
        Geometry.Points[0] := AsCoor(Pt.X, LowerLeft.Y);
        Geometry.Points[1] := AsCoor(Pt.X, UpperRight.Y);
        Geometry.Points[2] := AsCoor(LowerLeft.X, Pt.Y);
        Geometry.Points[3] := AsCoor(UpperRight.X, Pt.Y);
      end;
    if mdCursorFrame in FMouseDrawElements then
    begin
      DX := DistToRealX(Licad.Settings.ApertureWidth);
      DY := DistToRealY(Licad.Settings.ApertureWidth);
      FCursorFrame.Geometry.DisableEvents := True;
      FCursorFrame.Geometry.Points[0] := AsCoor(Pt.X - DX / 2, Pt.Y - DY / 2);
      FCursorFrame.Geometry.Points[1] := AsCoor(Pt.X + DX / 2, Pt.Y + DY / 2);
      FCursorFrame.Geometry.DisableEvents := False;
    end;
  end;
end;

procedure TlicgAction.SetCaptionCommand(const Value: string);
begin
  if Assigned(FCmdLine) then
    FCmdLine.CaptionCommand := value; //HintCommandChange(Value);
end;

procedure TlicgAction.SetDefaultKeyboardInput(const Value: string);
begin
  FDefaultKeyboardInput := Value;
  FCmdLine.SetText(value);
end;

procedure TlicgAction.SuspendOperation;
begin
  { erase full view cursor }
  EraseFullViewCursor;
  if Assigned(FOnSuspendOperation) then
    FOnSuspendOperation(Self);
end;

procedure TlicgAction.ContinueOperation;
begin
  FCmdLine.HintCommand := FHintCommand;
  CmdLine.SetStatusMessage('');
  if Assigned(FOnContinueOperation) then
    FOnContinueOperation(Self);
  { re-draw full view cursor }
  DrawFullViewCursor;
end;

procedure TlicgAction.UndoOperation;
begin
  { Clear the caption by default }
  if Assigned(FCmdLine) then
    FCmdLine.CaptionCommand := '';
  if Assigned(FOnUndo) then
    FOnUndo(Self);
end;

function TlicgAction.AcceptDrawBox: Boolean;
begin
  Result := True;
end;

procedure TlicgAction.ParseUserCommand(const Cmd: AnsiString);
var
  lexer: TCustomLexer;
  parser: TCustomParser;
  outputStream: TMemoryStream;
  errorStream: TMemoryStream;
  Stream: TStream;
begin
  if Length(Cmd) = 0 then
    Exit;
  outputStream := TMemoryStream.Create;
  errorStream := TMemoryStream.Create;
  Stream := TMemoryStream.Create;
  Stream.Write(Cmd[1], Length(Cmd) * SizeOf(AnsiChar)); // ilker deðiþtirme
  Stream.Seek(0, 0);

  lexer := Licad.CreateScrLexer;
  lexer.yyinput := Stream;
  lexer.yyoutput := outputStream;
  lexer.yyerrorfile := errorStream;

  parser := Licad.CreateScrParser;
  parser.DrawBox := CmdLine.ActiveDrawBox;
  parser.CmdLine := CmdLine;
  parser.checksyntax := False;
  parser.yyLexer := lexer; // lexer and parser linked
  try
    FUserCommand := itNone;
    if parser.yyparse = 1 then
    begin
      // if it is a syntax error, we will consider it a simple string
      FUserCommand := itString;
      FUserString := Cmd;
    end;
  finally
    parser.free;
    lexer.free;
    outputStream.free;
    errorStream.free;
    Stream.Free;
  end;
end;

function TlicgAction.GetActionNeeds: TlicgActionNeeds;
begin
  Result := FActionNeeds;
end;

function TlicgAction.GetOnActionDoCommand: TNotifyEvent;
begin
  Result := FOnActionDoCommand;
end;

function TlicgAction.GetOnContinueOperation: TNotifyEvent;
begin
  Result := FOnContinueOperation;
end;

function TlicgAction.GetOnDblClick: TNotifyEvent;
begin
  Result := FOnDblClick;
end;

function TlicgAction.GetOnInitialize: TNotifyEvent;
begin
  Result := FOnInitialize;
end;

function TlicgAction.GetOnKeyDown: TKeyEvent;
begin
  Result := FOnKeyDown;
end;

function TlicgAction.GetOnKeyPress: TlicgKeyPressEvent;
begin
  Result := FOnKeyPress;
end;

function TlicgAction.GetOnKeyUp: TKeyEvent;
begin
  Result := FOnKeyUp;
end;

function TlicgAction.GetOnMouseDown: TlicgMouseEvent;
begin
  Result := FOnMouseDown;
end;

function TlicgAction.GetOnMouseMove: TlicgMouseMoveEvent;
begin
  Result := FOnMouseMove;
end;

function TlicgAction.GetOnMouseUp: TlicgMouseEvent;
begin
  Result := FOnMouseUp;
end;

function TlicgAction.GetOnPaint: TNotifyEvent;
begin
  Result := FOnPaint;
end;

function TlicgAction.GetOnSuspendOperation: TNotifyEvent;
begin
  Result := FOnSuspendOperation;
end;

function TlicgAction.GetOnUndo: TNotifyEvent;
begin
  Result := FOnUndo;
end;

procedure TlicgAction.SetActionNeeds(value: TlicgActionNeeds);
begin
  FActionNeeds := value;
end;

procedure TlicgAction.SetOnActionDoCommand(value: TNotifyEvent);
begin
  FOnActionDoCommand := value;
end;

procedure TlicgAction.SetOnContinueOperation(value: TNotifyEvent);
begin
  FOnContinueOperation := value;
end;

procedure TlicgAction.SetOnDblClick(value: TNotifyEvent);
begin
  FOnDblClick := value;
end;

procedure TlicgAction.SetOnInitialize(value: TNotifyEvent);
begin
  FOnInitialize := value;
end;

procedure TlicgAction.SetOnKeyDown(value: TKeyEvent);
begin
  FOnKeyDown := value;
end;

procedure TlicgAction.SetOnKeyPress(value: TlicgKeyPressEvent);
begin
  FOnKeyPress := value;
end;

procedure TlicgAction.SetOnKeyUp(value: TKeyEvent);
begin
  FOnKeyUp := value;
end;

procedure TlicgAction.SetOnMouseDown(value: TlicgMouseEvent);
begin
  FOnMouseDown := value;
end;

procedure TlicgAction.SetOnMouseMove(value: TlicgMouseMoveEvent);
begin
  FOnMouseMove := value;
end;

procedure TlicgAction.SetOnMouseUp(value: TlicgMouseEvent);
begin
  FOnMouseUp := value;
end;

procedure TlicgAction.SetOnPaint(value: TNotifyEvent);
begin
  FOnPaint := value;
end;

procedure TlicgAction.SetOnSuspendOperation(value: TNotifyEvent);
begin
  FOnSuspendOperation := value;
end;

procedure TlicgAction.SetOnUndo(value: TNotifyEvent);
begin
  FOnUndo := value;
end;

function TlicgAction.GetActionObjID: TlicgActionObjID;
begin
  result := idNoneAction;
end;

{ TlicgVertexRefList }

constructor TlicgVertexRefList.Create;
begin
  inherited Create;
  FOwner := AOwner;
  FList := TList.Create;
end;

destructor TlicgVertexRefList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TlicgVertexRefList.GetSorted: Boolean;
begin
  Result := FSorted;
end;

procedure TlicgVertexRefList.SetSorted(const Value: Boolean);
begin
  FSorted := Value
end;

procedure TlicgVertexRefList.ShowHide(Ele: Integer = -1);
var
  I: Integer;
  SavePenStyle: TPenStyle;
  SavePenWidth: Integer;
  SavePenColor: TColor;
  AStart, AEnd: Integer;
begin
  if FList.Count = 0 then
    Exit;

  with FOwner.Owner do
  begin
    with ActiveDrawBox do
    begin
      SavePenStyle := RubberPenStyle;
      SavePenWidth := RubberPenWidth;
      SavePenColor := RubberPenColor;
      RubberPenStyle := psSolid; // all other rubber banding entities drawn on psDot
      RubberPenWidth := 1;
      RubberPenColor := AccuDraw.SnapColor;
    end;

    if Ele = -1 then
    begin
      AStart := 0;
      AEnd := FList.Count - 1;
    end
    else
    begin
      AStart := Ele;
      AEnd := Ele;
    end;
    for I := AStart to AEnd do
      TlicgVertexRef(FList[I]).Draw(FOwner.Owner);

    with ActiveDrawBox do
    begin
      RubberPenStyle := SavePenStyle;
      RubberPenWidth := SavePenWidth;
      RubberPenColor := SavePenColor;
    end;
  end;
end;

procedure TlicgVertexRefList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TlicgVertexRef(FList[I]).Free;
  FList.Clear;
  FSorted := False;
end;

function TlicgVertexRefList.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TlicgVertexRefList.Add(Layer: TlicgBaseLayer; Recno, VertexNo: Integer;
  const Vertex: TlicgCoor);
var
  Ref: TlicgVertexRef;
begin
  Ref := TlicgVertexRef.Create;
  Ref.Layer := Layer;
  Ref.Recno := Recno;
  Ref.VertexNo := VertexNo;
  Ref.Vertex := Vertex;
  FList.Add(Ref);
  FSorted := False;
end;

procedure TlicgVertexRefList.Delete(Index: Integer);
begin
  TlicgVertexRef(FList[Index]).Free;
  FList.Delete(Index);
  FSorted := False;
end;

function TlicgVertexRefList.GetSorteableElement(Index: Integer): string;
var
  LayerName: string;
  Ref: TlicgVertexRef;
begin
  Ref := FList[Index];
  if Ref.Layer = nil then
    LayerName := #32
  else
    LayerName := Ref.Layer.Name;
  Result := Format('%-60s%.12d%.12d', [LayerName, Ref.Recno, Ref.VertexNo]);
end;

procedure TlicgVertexRefList.Sort;

  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P: string;
    TL: TlicgVertexRef;
  begin
    repeat
      I := L;
      J := R;
      P := GetSorteableElement((L + R) shr 1);
      repeat
        while GetSorteableElement(I) < P do
          Inc(I);
        while GetSorteableElement(J) > P do
          Dec(J);
        if I <= J then
        begin
          TL := FList[I];
          FList[I] := FList[J];
          FList[J] := TL;
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
  if FSorted then
    Exit;
  if FList.Count > 1 then
    QuickSort(0, FList.Count - 1);
  FSorted := True;
end;

function TlicgVertexRefList.IndexOf(Layer: TlicgBaseLayer; Recno, VertexNo:
  Integer): Integer;
var
  I, First, Last, Mid: Integer;
  Target, Compare, LayerName: string;
  Ref: TlicgVertexRef;
begin
  Result := -1;
  if not FSorted then
  begin
    for I := 0 to FList.Count - 1 do
    begin
      Ref := FList[I];
      if (Layer = Ref.Layer) and (Recno = Ref.Recno) and (VertexNo = Ref.VertexNo) then
      begin
        Result := I;
        Exit;
      end;
    end;
  end
  else
  begin
    if Layer = nil then
      LayerName := #32
    else
      LayerName := Layer.Name;
    Target := Format('%-60s%.12d%.12d', [LayerName, Recno, VertexNo]);
    First := 0;
    Last := FList.Count - 1;
    while First <= Last do
    begin
      Mid := (First + Last) div 2;
      Compare := GetSorteableElement(Mid);
      if Target > Compare then
        First := Mid + 1
      else
      begin
        if Target < Compare then
          Last := Mid - 1
        else
        begin
          Result := Mid;
          Exit;
        end;
      end;
    end;
  end;
end;

function TlicgVertexRefList.GetVertexRef(Index: Integer): TlicgVertexRef;
begin
  Result := nil;
  if (Index < 0) or (Index > FList.Count - 1) then
    exit;
  Result := FList[Index];
end;


{ TlicgVertexRef }

procedure TlicgVertexRef.Draw(CmdLine: TlicgBaseCmdLine);
begin
  with cmdLine.ActiveDrawBox do
  begin
    if not IsPointInExtent(FVertex, Grapher.CurrentParams.VisualWindow) then
      Exit;
    DrawCross(GetIlicgCanvas(Canvas), Grapher.RealToPoint(FVertex),
      FAccuSnapPicFocused.Width div 4);
  end;
end;

function TlicgVertexRef.GetLayer: TlicgBaseLayer;
begin
  Result := FLayer
end;

function TlicgVertexRef.GetRecno: Integer;
begin
  Result := FRecno
end;

function TlicgVertexRef.GetVertex: TlicgCoor;
begin
  Result := FVertex
end;

function TlicgVertexRef.GetVertexNo: Integer;
begin
  Result := FVertexNo
end;

procedure TlicgVertexRef.SetLayer(const Value: TlicgBaseLayer);
begin
  FLayer := Value
end;

procedure TlicgVertexRef.SetRecno(const Value: Integer);
begin
  FRecno := Value
end;

procedure TlicgVertexRef.SetVertex(const Value: TlicgCoor);
begin
  FVertex := Value
end;

procedure TlicgVertexRef.SetVertexNo(const Value: Integer);
begin
  FVertexNo := Value
end;

function _ActionAddNewEntity(CmdLine: TlicgBaseCmdline; Entity: IlicgEntity;
  isUndo: Boolean = True; ApplyLayerProps: Boolean = True): Integer;
var
  Extents: TlicgExtent;
  MinDim: Double;
  Accept: Boolean;
  pointEntity: IlicgEntity;
  I, pointRecno: Integer;
  lines: TStrings;
  _lastPointName: string; // li2016
  TempEntity: IlicgEntity;
  pnt: TlicgCoor;
  Tempheight: double;
  TempAngle: double;
  _isUndo: boolean;
  PropVList: IPropertyValueList;
  melems: TAffineMatrixElements;
  InPrj, OutPrj: IlicgCS;
begin
  try
    _isUndo := isUndo;
    Entity.DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
    Extents := Entity.Geometry.Extent;
    Result := 0;
    if not Entity.Geometry.IsValid then
      Exit;

    PropVList := TPropertyValueList.create;

    if not CmdLine.ActiveDrawBox.GIS.CurrentLayer.InitValuesBeforeAddEntity(Entity,
      CmdLine.ActiveDrawBox.GIS, _isUndo, PropVList) then
      Exit;

    if Entity.EntityID = idPreview then
    begin
      with (Entity as IlicgPreviewEntity) do
      begin
        CalculateScales(GetProposedPrintArea);
      end;
    end;

    with CmdLine.ActiveDrawBox do
    begin
      if ApplyLayerProps then
      begin
        //Uður Silme
//        Entity.DrawTools.PenTool.Assign(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.LayerInfo.DefPenTool);
       Entity.DrawTools.PenTool.Color := clNone;
        var // Uður ekleme yazý fonu için
          PatIndex: Integer;
        PatIndex := Entity.DrawTools.BrushTool.Pattern;
        //Uður Silme
//        Entity.DrawTools.BrushTool.Assign(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.LayerInfo.DefBrushTool);
        if Entity.EntityID in TextEntityIDs then // Uður ekleme yazý fonu için
        begin
          Entity.DrawTools.BrushTool.BackColor := clWhite;
          Entity.DrawTools.BrushTool.ForeColor := clWhite;
          Entity.DrawTools.BrushTool.Pattern := PatIndex;
        end;
      end;

      if Entity.EntityID <> idTextM then
      begin
        if _isUndo then
          CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);

        Result := GIS.CurrentLayer.AddEntity(Entity, Entity.ApplyLayerProps,
          ApplyLayerProps, PropVList);

        if Entity <> nil then
          Extents := Entity.Geometry.Extent;

        if (Entity.EntityID in [idPolyline, idPolygon]) and (CmdLine.ActiveDrawBox.InsertPointWhenLineDraw)
          then
        begin

            //GIS.LastPointColor:= Entity.DrawTools.PenTool.color;
          _lastPointName := GIS.LastPointName;
          for I := 0 to Entity.Geometry.Points.count - 1 do
          begin
            if (I = Entity.Geometry.Points.count - 1) and (_Distance(Entity.Geometry.Points
              [0], Entity.Geometry.Points[I]) = 0) then
              continue;
            _lastPointName := IncreasePointName(_lastPointName);

            pointEntity := Licad.CreateEntityFactory.MakePoint(Point3D(Entity.Geometry.Points
              [I].x, Entity.Geometry.Points[I].y, 0), '', Entity.DrawTools.PenTool.Color);
            try
              pointEntity.Name := _lastPointName;
              pointRecno := GIS.CurrentLayer.AddEntity(pointEntity, True,
                ApplyLayerProps, PropVList);
              GIS.LastPointName := _lastPointName;
              CmdLine.ActiveDrawBox.Undo.AddUndo(GIS.CurrentLayer, pointRecno, uaDelete);
            finally
              pointEntity := nil;
            end;
          end;

        end;

        if _isUndo then
        begin
          CmdLine.ActiveDrawBox.Undo.AddUndo(GIS.CurrentLayer, Result, uaDelete);
          CmdLine.ActiveDrawBox.Undo.EndUndo;
        end;

      end
      else
      begin

        if _isUndo then
          CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        Extents := Entity.Geometry.Extent;
        lines := TStringList.Create;
        try
          Lines.Text := AslicgTextM(Entity.Geometry).Text;

          Tempheight := Entity.DrawTools.FontTool.Height;
          for I := 0 to lines.Count - 1 do
          begin

            try

              if i = 0 then
                pnt := AslicgTextM(Entity.Geometry).BasePoint
              else
                PNt := GetCoor(AslicgTextM(Entity.Geometry).BasePoint.x,
                  AslicgTextM(Entity.Geometry).BasePoint.y, TempAngle +
                  pi / 2, (Tempheight + Tempheight * 0.80) * i, atRadian); // yazý yüksekliðinin %80'ý oranýnda ara veriliyor

              TempEntity := Licad.CreateEntityFactory.MakeText(pnt, lines.Strings
                [I], Entity.DrawTools.FontTool);

              TempEntity.DrawTools.FontTool.Assign(Entity.DrawTools.FontTool);
              TempEntity.DrawTools.FontTool.Height := Tempheight;
               // TempEntity.DrawTools.PenTool.Copy(Licad.Settings.TTextPenStyle);
               // TempEntity.DrawTools.BrushTool.Copy(Licad.Settings.TTextBrushStyle);
              TempEntity.DrawTools.BrushTool.Assign(Entity.DrawTools.BrushTool);
              TempEntity.DrawTools.PenTool.Assign(Entity.DrawTools.PenTool);

              Result := GIS.CurrentLayer.AddEntity(TempEntity, True, False, PropVList);

              MaxBound(Extents.UpperRight, TempEntity.Geometry.Extent.UpperRight);
              MinBound(Extents.LowerLeft, TempEntity.Geometry.Extent.LowerLeft);

              if i = 0 then
                TempAngle := Angle2DS(TempEntity.Geometry.Points[0], TempEntity.Geometry.Points
                  [3]);

            finally
              if _isUndo then
                CmdLine.ActiveDrawBox.Undo.AddUndo(GIS.CurrentLayer, Result, uaDelete);

              TempEntity := nil;
            end;
          end;

        finally
          Lines.Free;
          if _isUndo then
            CmdLine.ActiveDrawBox.Undo.EndUndo;
        end;
      end;

      // ilker deðiþtirme önceden hakan kapatmýþ.
     // if (Entity.IsPolygon) and (not GIS.CurrentLayer.LayerInfo.isLayerBrush) then
       // Entity.DrawTools.BrushTool.Pattern := 0;

      if CmdLine.ActiveDrawBox.GIS.MapInfo.IsOntheFly then
      begin
        InPrj := CmdLine.ActiveDrawBox.GIS.MapInfo.CS;
        OutPrj := CmdLine.ActiveDrawBox.GIS.MapInfo.OntheFlyCS;
        melems := CmdLine.ActiveDrawBox.GIS.OntheFlyMatrixElements;
        Entity.ApplyProject(CmdLine.ActiveDrawBox.GIS, inPrj, OutPrj, @melems, true);
      end;

      Extents := Entity.Geometry.Extent;
      CmdLine.ActiveDrawBox.DrawEntity(Entity, dmNormal);
      {Repaint only the affected area}
      MinDim := CmdLine.ActiveDrawBox.Grapher.DistToRealY(5);
      Lider.CG.Com.Lib.InflateExtent(Extents, MinDim, MinDim);
      CmdLine.ActiveDrawBox.RefreshExtent(Extents);
    end;
  finally
    if Entity <> nil then
      Entity := nil;
  end;
end;


(*
function _ActionAddNewEntityByLayerParam(CmdLine: TlicgBaseCmdline; Layer : Tlicgbaselayer; Entity: IlicgEntity; isUndo:Boolean = True;
   ApplyLayerProps: Boolean=True): Integer;
var
  Extents           : TlicgExtent;
  MinDim            : Double;
  Accept            : Boolean;
  pointEntity       : IlicgEntity;
  I,pointRecno      : Integer;
  lines             : TStrings;
  _lastPointName    : string;
  TempEntity        : IlicgEntity;
  pnt               : TlicgCoor;
  Tempheight        : double;
  TempAngle         : double;
  _isUndo : boolean;
  PropVList : IPropertyValueList;
  melems: TAffineMatrixElements;
  InPrj, OutPrj: IlicgCoordinatSystem;

begin
  try
    _isUndo := isUndo;
    Extents := Entity.Geometry.Extent;
    Result := 0;
    if not Entity.Geometry.IsValid then
      Exit;

    PropVList := TPropertyValueList.create;

    if not Layer.InitValuesBeforeAddEntity(Entity,
         CmdLine.ActiveDrawBox.GIS, _isUndo, PropVList) then
       Exit;

    if Entity.EntityID = idPreview then
    begin
      with (Entity as IlicgPreviewEntity) do
      begin
        CalculateScales(GetProposedPrintArea);
      end;
    end;

    with CmdLine.ActiveDrawBox do
    begin

      if Entity.EntityID <> idTextM then begin
        if _isUndo Then
          CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete) ;

         Result := Layer.AddEntity(Entity, ApplyLayerProps, True, PropVList);

        if Entity<>nil then
           Extents := Entity.Geometry.Extent;

        if (Entity.EntityID in [idPolyline, idPolygon])  and (CmdLine.ActiveDrawBox.InsertPointWhenLineDraw) then
        begin

            GIS.LastPointColor:= Entity.DrawTools.PenTool.color;
            _lastPointName:=GIS.LastPointName;
            For I:=0 to Entity.Geometry.Points.count-1 do begin
              if (I=Entity.Geometry.Points.count-1) and
                 (_Distance(Entity.Geometry.Points[0],Entity.Geometry.Points[I])=0) then continue;
              _lastPointName := IncreasePointName(_lastPointName);

              pointEntity:= Licad.CreateEntityFactory.MakePoint(Point3D(Entity.Geometry.Points[I].x,Entity.Geometry.Points[I].y,0),'',GIS.LastPointColor);
              try
                pointEntity.Name:=_lastPointName ;
                pointRecno := Layer.AddEntity(pointEntity,ApplyLayerProps,True,PropVList);
                GIS.LastPointName:=_lastPointName;
                CmdLine.ActiveDrawBox.Undo.AddUndo(Layer , pointRecno, uaDelete);
              finally
                pointEntity := nil;
              end;
            end;

        end;

        if _isUndo then
        begin
          CmdLine.ActiveDrawBox.Undo.AddUndo(Layer , Result, uaDelete);
          CmdLine.ActiveDrawBox.Undo.EndUndo;
        end;

      end else begin

        if _isUndo Then
          CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete) ;
          Extents := Entity.Geometry.Extent;
          lines := TStringList.Create;
          try
            Lines.Text := AsBTextM(Entity.Geometry).Text;

            Tempheight := Entity.DrawTools.FontTool.Height;
            for I:=0 to lines.Count -1 do begin

              try

                if i=0 then
                  pnt := AsBTextM(Entity.Geometry).BasePoint
                else
                  PNt := GetCoor( AsBTextM(Entity.Geometry).BasePoint.x,
                                  AsBTextM(Entity.Geometry).BasePoint.y, TempAngle + pi/2,(Tempheight+Tempheight*0.80)*i, atRadian);// yazý yüksekliðinin %80'ý oranýnda ara veriliyor

                TempEntity := Licad.CreateEntityFactory.MakeText ( pnt, lines.Strings[I] ,
                    Entity.DrawTools.FontTool.Height, Entity.DrawTools.FontTool.Angle );

                TempEntity.DrawTools.FontTool.Copy(Entity.DrawTools.FontTool);
                TempEntity.DrawTools.FontTool.Height := Tempheight;
                TempEntity.DrawTools.PenTool.Copy(Licad.Settings.TTextPenStyle);
                TempEntity.DrawTools.BrushTool.Copy(Licad.Settings.TTextBrushStyle);

                Result := Layer.AddEntity(TempEntity,False,False,PropVList);

                MaxBound (Extents.UpperRight, TempEntity.Geometry.Extent.UpperRight);
                MinBound (Extents.LowerLeft, TempEntity.Geometry.Extent.LowerLeft);

                if i=0 then
                  TempAngle := Angle2DS(TempEntity.Geometry.Points[0], TempEntity.Geometry.Points[3]);

              finally
               if _isUndo Then
                 CmdLine.ActiveDrawBox.Undo.AddUndo(Layer , Result, uaDelete);

                TempEntity := nil;
              end;
            end;

          finally
            Lines.Free;
            if _isUndo Then
              CmdLine.ActiveDrawBox.Undo.EndUndo;
          end;
      end;


      if (Entity.IsPolygon) and (Not Layer.LayerInfo.isLayerBrush) then
        Entity.DrawTools.BrushTool.Pattern := 0;



      if CmdLine.ActiveDrawBox.GIS.MapInfo.IsOntheFly then
      begin

        InPrj  := CmdLine.ActiveDrawBox.GIS.Projection;
        OutPrj := CmdLine.ActiveDrawBox.GIS.OntheflyProjection;
        melems := CmdLine.ActiveDrawBox.GIS.OntheFlyMatrixElements;

        Entity.ApplyProject(CmdLine.ActiveDrawBox.GIS, inPrj, OutPrj, @melems, true );

      end;

      Extents := Entity.Geometry.Extent;
      cmdLine.ActiveDrawBox.DrawEntity(Entity, dmNormal);
      {Repaint only the affected area}
      MinDim := CmdLine.ActiveDrawBox.Grapher.DistToRealY(5);
      InflateExtent(Extents, MinDim, MinDim);


      cmdLine.ActiveDrawBox.RefreshExtent(Extents);

    end;

    finally
      if Entity<>nil then
        Entity := nil;
    end;
end;
*)

function TlicgAction.GetGlobalTempEntity: IlicgEntity;
begin
  Result := nil;
end;


(*
{ TActionCommandProperty }

function TActionCommandProperty.GetCaption: string;
begin
  Result := FCaption;
end;

function TActionCommandProperty.GetActionDataType: TActionParamaterDataType;
begin
  Result := FActionDataType;
end;

function TActionCommandProperty.GetHint: string;
begin
  Result := FHint;
end;

function TActionCommandProperty.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TActionCommandProperty.GetValue: variant;
begin
  Result := FValue;
end;


procedure TActionCommandProperty.SetCaption(value: string);
begin
  FCaption := value;
end;

procedure TActionCommandProperty.SetActionDataType(value: TActionParamaterDataType);
begin
  FActionDataType := value;
end;

procedure TActionCommandProperty.SetHint(value: string);
begin
  FHint := Value;
end;

procedure TActionCommandProperty.SetReadOnly(value: Boolean);
begin
  FReadOnly := value;
end;

procedure TActionCommandProperty.SetValue(value: variant);
begin
  FValue := value;
end;

function TActionCommandProperty.GetBrushTool: IlicgBrushTool;
begin
  Result := FBrushTool;
end;

function TActionCommandProperty.GetTTFontTool: IlicgFontTool;
begin
  Result := FTTFontTool;
end;

function TActionCommandProperty.GetPenTool: IlicgPenTool;
begin
  Result := FPenTool;
end;

function TActionCommandProperty.GetSymbolTool: IlicgSymbolTool;
begin
  Result := FSymbolTool;
end;

procedure TActionCommandProperty.SetBrushTool(value: IlicgBrushTool);
begin
  FBrushTool := value;
end;

procedure TActionCommandProperty.SetTTFontTool(value: IlicgFontTool);
begin
  FTTFontTool := value;
end;

procedure TActionCommandProperty.SetPenTool(value: IlicgPenTool);
begin
  FPenTool := value;
end;

procedure TActionCommandProperty.SetSymbolTool(value: IlicgSymbolTool);
begin
  FSymbolTool := value;
end;

function TActionCommandProperty.GetVFontTool: IlicgFontTool;
begin
   Result := FVFontTool;
end;

procedure TActionCommandProperty.SetVFontTool(value: IlicgFontTool);
begin
  FVFontTool := value;
end;

function TActionCommandProperty.GetEntityDrawTools: IlicgEntity;
begin
  Result := FEntity;
end;

procedure TActionCommandProperty.SetEntityDrawTools(value: IlicgEntity);
begin
  FEntity := value;

end;
*)

{ TActionCommandPropertyList }

function TActionCommandPropertyList.Add(const Item: IInterface): Integer;
begin
  Result := FIList.Add(Item);
end;

procedure TActionCommandPropertyList.Clear;
begin
  FIList.Clear;
end;

constructor TActionCommandPropertyList.create;
begin
  inherited;
  FIList := TInterfaceList.Create;
end;

procedure TActionCommandPropertyList.Delete(Index: Integer);
begin
  FIList.Delete(Index);
end;

destructor TActionCommandPropertyList.destroy;
begin
  FIList.Clear;
  FIList := nil;
  inherited;
end;

procedure TActionCommandPropertyList.Exchange(Index1, Index2: Integer);
begin
  FIList.Exchange(Index1, Index2);
end;

function TActionCommandPropertyList.First: IInterface;
begin
  Result := FIList.First;
end;

function TActionCommandPropertyList.Get(Index: Integer): IInterface;
begin
  Result := FIList.Get(index);
end;

function TActionCommandPropertyList.GetCapacity: Integer;
begin
  Result := FIList.GetCapacity;
end;

function TActionCommandPropertyList.GetCount: Integer;
begin
  Result := FIList.GetCount;
end;

function TActionCommandPropertyList.IndexOf(const Item: IInterface): Integer;
begin
  Result := FIList.IndexOf(Item);
end;

procedure TActionCommandPropertyList.Insert(Index: Integer; const Item: IInterface);
begin
  FIList.Insert(Index, Item);
end;

function TActionCommandPropertyList.Last: IInterface;
begin
  Result := FIList.Last;
end;

procedure TActionCommandPropertyList.Lock;
begin
  FIList.Lock;
end;

procedure TActionCommandPropertyList.Put(Index: Integer; const Item: IInterface);
begin
  FIList.Put(Index, Item);
end;

function TActionCommandPropertyList.Remove(const Item: IInterface): Integer;
begin
  Result := FIList.Remove(Item);
end;

procedure TActionCommandPropertyList.SetCapacity(NewCapacity: Integer);
begin
  FIList.SetCapacity(NewCapacity);
end;

procedure TActionCommandPropertyList.SetCount(NewCount: Integer);
begin
  FIList.SetCount(NewCount);
end;

procedure TActionCommandPropertyList.Unlock;
begin
  FIList.Unlock;
end;

function TlicgAction.GetHintCommand: string;
begin
  Result := FHintCommand;
end;

procedure TlicgAction.SetHintCommand(const Value: string);
begin
  //FOldHintCommand := FHintCommand;
  FHintCommand := Value;
  FCmdLine.HintCommand := Value;
end;

procedure TlicgAction.SetCaption(const Value: string);
begin
  FHintCommand := Value;
  FCmdLine.HintCommand := Value;

end;

{ TCommandStrList }

destructor TCommandStrList.destroy;
var
  i: integer;
begin
  for i := count - 1 downto 0 do
    TCommandStr(Items[i]).Free;
  Clear;
  inherited;
end;

function TCommandStrList.GetCommandString(var cmd: string; var Exists: boolean): string;
var
  i: integer;
begin
  Result := Cmd;
  Exists := false;

  for i := 0 to count - 1 do
  begin
    if TCommandStr(Items[i]).SCustomCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SCustomCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCmd;
        Exists := true;
        BREAK;
      end;
    end
    else if TCommandStr(Items[i]).SOverrideCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SOverrideCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCmd;
        Exists := true;
        BREAK;
      end;
    end
    else if TCommandStr(Items[i]).SCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCmd;
        Exists := true;
        BREAK;
      end;
    end;

  end;

end;

function TCommandStrList.GetCommandCaption(var cmd: string): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to count - 1 do
  begin
    if TCommandStr(Items[i]).SCustomCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SCustomCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCap;
        BREAK;
      end;
    end
    else if TCommandStr(Items[i]).SOverrideCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SOverrideCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCap;
        BREAK;
      end;
    end
    else if TCommandStr(Items[i]).SCmd <> '' then
    begin
      if AnsiUpperCase(TCommandStr(Items[i]).SCmd) = AnsiUpperCase(Cmd) then
      begin
        Result := TCommandStr(Items[i]).SCap;
        BREAK;
      end;
    end;

  end;

end;

function TlicgAction.GetActionParameterGetEntity: IlicgEntity;
begin
  if fActionParameterPointerData <> nil then
    Result := IlicgEntity(fActionParameterPointerData)
  else if fActionParameterGetEntityProc <> nil then
    Result := fActionParameterGetEntityProc;
end;

{ TlicgBaseCmdline }

{
function TlicgBaseCmdline.GetCreateActionFormEvent: TCreateActionFormEvent;
begin
  Result := FCreateActionFormEvent;
end;

function TlicgBaseCmdline.GetDestroyActionFormEvent: TDestroyActionFormEvent;
begin
  Result := fDestroyActionFormEvent;
end;     }

function TlicgBaseCmdline.GetGroupNameSelection: Boolean;
begin
  Result := FGroupNameSelection;
end;

procedure TlicgBaseCmdline.SetCaption(const Value: string);
begin
  SetHintCommand(Value);
end;

{-------------------------------------------------------------------------------}
//                  Implements TlicgDrawBoxItem
{-------------------------------------------------------------------------------}

destructor TlicgDrawBoxItem.Destroy;
begin
  SetDrawBox(nil);
  inherited Destroy;
end;

function TlicgDrawBoxItem.GetDrawBox: TlicgBaseDrawBox;
begin
  Result := FDrawBox
end;

function TlicgDrawBoxItem.GetCurrent: Boolean;
begin
  Result := FCurrent;
end;

procedure TlicgDrawBoxItem.Assign(Source: TPersistent);
begin
  if Source is TlicgDrawBoxItem then
    SetDrawBox(TlicgDrawBoxItem(Source).FDrawBox)
  else
    inherited Assign(Source);
end;

function TlicgDrawBoxItem.GetDisplayName: string;
begin
  if FDrawBox <> nil then
    result := FDrawBox.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TlicgDrawBoxItem.SetDrawBox(Value: TlicgBaseDrawBox);
var
  cmdLine: TlicgBaseCmdLine;
begin
  if FDrawBox = Value then
    Exit;

  { check if this is pointing to the same TlicgBaseGIS component }
  with Collection do
    if (Value <> nil) and (Count > 0) and (TlicgDrawBoxItem(Items[0]).FDrawBox
      <> nil) and (TlicgDrawBoxItem(Items[0]).FDrawBox.GIS <> Value.GIS) then
      raise Exception.Create(sDrawBoxCollectionNotSame);

  cmdLine := (Collection as TlicgDrawBoxCollection).FCmdLine;

  { Remove free notification }
{$IFDEF LEVEL5}
  //if Assigned(Self.FDrawBox) then
    //Self.FDrawBox.RemoveFreeNotification(CmdLine);
{$ENDIF}

  { define free notification}
  //if Assigned(Value) then
    //Value.FreeNotification(CmdLine);

  { remove old hooked event handlers }
  if not (csDesigning in cmdLine.ComponentState) and Assigned(Self.FDrawBox) then
  begin
    with Self.FDrawBox do
    begin
      OnMouseDown2D := FSavedMouseDown;
      OnMouseMove2D := FSavedMouseMove;
      OnMouseUp2D := FSavedMouseUp;
      OnClick := FSavedClick;
      OnDblClick := FSavedDblClick;
      OnKeyPress := FSavedKeyPress;
      OnKeyDown := FSavedKeyDown;
      OnKeyUp := FSavedKeyUp;
      OnPaint := FSavedPaint;
      OnSystemKeyDown := cmdLine.SystemKeydown;
    end;
  end;

  { hook the event handlers }
  if not (csDesigning in cmdLine.ComponentState) and Assigned(Value) then
  begin
    with Value do
    begin
      { save the pointers to event handlers }
      FSavedMouseDown := OnMouseDown2D;
      FSavedMouseMove := OnMouseMove2D;
      FSavedMouseUp := OnMouseUp2D;
      FSavedClick := OnClick;
      FSavedDblClick := OnDblClick;
      FSavedKeyPress := OnKeyPress;
      FSavedKeyDown := OnKeyDown;
      FSavedKeyUp := OnKeyUp;
      FSavedPaint := OnPaint;

      { set the event handlers pointing to methods on this class }
      OnMouseDown2D := cmdLine.DoMouseDown;
      OnMouseMove2D := cmdLine.DoMouseMove;
      OnMouseUp2D := cmdLine.DoMouseUp;
      OnDblClick := cmdLine.DoDblClick;
      OnKeyPress := cmdLine.DoKeyPress;
      OnKeyDown := cmdLine.DoKeyDown;
      OnKeyUp := cmdLine.DoKeyUp;
      OnPaint := cmdLine.DoPaint;
      OnSystemKeyDown := cmdLine.SystemKeydown;
    end;
  end;

  if not (csDesigning in cmdLine.ComponentState) and Assigned(Self.FDrawBox) then
    Self.FDrawBox.OnInternalUseMouseMove := nil;

  if not (csDesigning in cmdLine.ComponentState) and Assigned(Value) then
    Value.OnInternalUseMouseMove := Self.InternalMouseMove;

  FDrawBox := Value;
end;

procedure TlicgDrawBoxItem.InternalMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; const WX, WY: Double);
var
  CurrentBoxItem: TlicgDrawBoxItem;
  Pt: TPoint;
begin
  if FCurrent then
    Exit;
  CurrentBoxItem := (Collection as TlicgDrawBoxCollection).FindCurrent;
  with Collection as TlicgDrawBoxCollection do
  begin
    if CurrentBoxItem <> nil then
    begin
      // we must turn off the accudraw and the accusnap
      FCmdLine.AccuSnap.EraseFromScreen;
      FCmdLine.AccuDraw.Showing := False;
      FCmdLine.CurrentAction.EraseFullViewCursor;
    end;
    // set the current drawbox
    SetCurrent(FDrawBox);
    Windows.GetCursorPos(Pt);
    Pt := FDrawBox.ScreenToClient(Pt);
    FCmdLine.CurrentAction.SetFullViewCursorPos(FDrawBox.Grapher.PointToReal(Pt));
    FCmdLine.CurrentAction.DrawFullViewCursor;

    // We must turn on the accudraw and the accusnap
    FCmdLine.AccuDraw.Showing := FCmdLine.CurrentAction.CanDoAccuDraw;
  end;
end;

procedure TlicgDrawBoxItem.SetCurrent(Value: Boolean);
var
  OldCurrent: Boolean;
  I: Integer;
begin
  OldCurrent := FCurrent;
  // set all to False
  for I := 0 to Collection.Count - 1 do
    TlicgDrawBoxItem(Collection.Items[I]).FCurrent := False;
  if OldCurrent and not Value then
  begin
    { if set to False, check that at least one is active }
    TlicgDrawBoxItem(Collection.Items[0]).FCurrent := True;
    if Index <> 0 then
      FCurrent := Value;
  end
  else
    FCurrent := Value;
end;

{-------------------------------------------------------------------------------}
//                  Implements TlicgDrawBoxCollection
{-------------------------------------------------------------------------------}

constructor TlicgDrawBoxCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TlicgDrawBoxItem);
  FCmdLine := AOwner as TlicgBaseCmdLine;
end;

function TlicgDrawBoxCollection.GetItem(Index: Integer): TlicgDrawBoxItem;
begin
  Result := TlicgDrawBoxItem(inherited GetItem(Index));
end;

procedure TlicgDrawBoxCollection.SetItem(Index: Integer; Value: TlicgDrawBoxItem);
begin
  inherited SetItem(Index, Value);
end;

function TlicgDrawBoxCollection.Add: TlicgDrawBoxItem;
begin
  Result := TlicgDrawBoxItem(inherited Add);
end;

function TlicgDrawBoxCollection.Find(DrawBox: TlicgBaseDrawBox): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if TlicgDrawBoxItem(Items[I]).FDrawBox = DrawBox then
    begin
      Result := True;
      Exit;
    end;
end;

function TlicgDrawBoxCollection.FindCurrent: TlicgDrawBoxItem;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
    if TlicgDrawBoxItem(Items[I]).FCurrent then
    begin
      Result := Items[I];
      Break;
    end;
  if (Result = nil) and (Count > 0) then
  begin
    TlicgDrawBoxItem(Items[0]).FCurrent := True;
    Result := Items[0];
  end;

end;

function TlicgDrawBoxCollection.FindItem(Sender: TObject): TlicgDrawBoxItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if TlicgDrawBoxItem(Items[I]).FDrawBox = Sender then
    begin
      Result := Items[I];
      Break;
    end;
  if (Result = nil) and (Count > 0) then
  begin
    Result := Items[0];
  end;
end;

procedure TlicgDrawBoxCollection.SetCurrent(Value: TObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if TlicgDrawBoxItem(Items[I]).FDrawBox = Value then
    begin
      TlicgDrawBoxItem(Items[I]).Current := True;
      Exit;
    end;
end;

destructor TlicgDrawBoxCollection.Destroy;
begin
  inherited;
end;

function TlicgAction.GetNeedCopyMode: boolean;
begin
  Result := False;
end;

function TlicgAction.CurrentActionNeedCoorCalc: boolean;
begin
  Result := False;
end;

function TlicgAction.CurrentActionQuickSelect: boolean;
begin
  Result := False;
end;

{
procedure TlicgBaseCmdline.SetCreateActionFormEvent(
  value: TCreateActionFormEvent);
begin
  FCreateActionFormEvent := value;
end;

procedure TlicgBaseCmdline.SetDestroyActionFormEvent(
  value: TDestroyActionFormEvent);
begin
  fDestroyActionFormEvent := value;
end;
}


procedure TlicgBaseCmdline.SetGroupNameSelection(const Value: Boolean);
begin
  FGroupNameSelection := Value;
end;

{ TlicgLauncher }

constructor TlicgLauncher.CreateLauncher(CmdLine: TlicgBaseCmdline);
begin
  inherited Create;
  FCmdLine := CmdLine;
  Launcher := Licad.CreateActionTracker(CmdLine);
  Launcher.IsCmdLineClear := False;
  FCmdLine.LastActionID := SCmdLauncher;
  Launcher.IsDefaultInitialization := True;
end;

destructor TlicgLauncher.Destroy;
begin
  with FCmdLine do
  begin
    if Assigned(FCmdLine) (*and (FCmdLine.GetTheDefaultAction <> Self)*) and Assigned(OnAfterCommand) then
      if FCmdLine.ActiveDrawBox.GIS.Active then
        TlicgAfterCommandEvent(OnAfterCommand)(FCmdLine, LastCommand, LastActionID);
  end;

  if Assigned(FLauncher) then
  begin
    IlicgActionTracker(FLauncher).CurrentAction := nil;
    FLauncher := nil;
  end;
  inherited;
end;

function TlicgLauncher.GetLauncher: IlicgActionTracker;
begin
  Result := FLauncher;
end;

procedure TlicgLauncher.SetLauncher(const Value: IlicgActionTracker);
begin
  FLauncher := Value
end;

function TlicgLauncher.GetCmdLine: TlicgBaseCmdline;
begin
  Result := FCmdLine
end;

procedure TlicgLauncher.SetCmdLine(const Value: TlicgBaseCmdline);
begin
  FCmdLine := Value
end;

end.


