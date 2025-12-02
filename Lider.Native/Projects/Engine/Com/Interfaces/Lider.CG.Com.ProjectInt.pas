unit Lider.CG.Com.ProjectInt;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Forms,
  Controls,
  Lider.CG.Com.Types;

type
  PProject = ^TProject;

  TProject = packed record
    isFirstTemporaryProject: boolean;
    ParentHandle: THandle;
    GisFileFullName: string;
    DataPath: string;
    ReferansGis: TObject;
    isReferansGis: boolean;
    isBlankGis: boolean;
    OnZoomChange: TlicgZoomChangeEvent;
    OnSelectionChange: TNotifyEvent;
    OnHintCommadEvent: THintCommandEvent;
    OnMouseMove2D: TlicgMouseMoveEvent;
    OnShowHint: TlicgShowHintEvent;

      //
    CreateLeftFormProc: TCreateLeftFormProc;
    SetFillLayersProc: TSetFillLayersProc;
    SetLeftPanelGroupProc: TSetLeftPanelGroupProc;

      //

      //CreateActionFormEvent : TCreateActionFormEvent;
      //DestroyActionFormEvent : TDestroyActionFormEvent;
  end;

  IlicgProject = interface
    ['{C87AA1C0-2DA4-4542-AED1-0A057FDC2FF8}']
    function GetProject: TProject; stdcall;
    procedure SetProject(value: TProject); stdcall;
    function GetLicadProjectFile: string; stdcall;
    procedure SetLicadProjectFile(const Value: string); stdcall;
    function GetLeftPanelForm: TForm; stdcall;
    procedure SetLeftPanelForm(value: TForm); stdcall;
    procedure SetGis(const aGis: TObject); stdcall;

    function GetisFirstTemporaryProject: boolean; stdcall;
    procedure SetisFirstTemporaryProject(value: boolean); stdcall;
    function GetParentHandle: THandle; stdcall;
    procedure SetParentHandle(value: THandle); stdcall;
    function GetGisFileFullName: string; stdcall;
    procedure SetGisFileFullName(value: string); stdcall;
    function GetDataPath: string; stdcall;
    procedure SetDataPath(value: string); stdcall;
    function GetReferansGis: TObject; stdcall;
    procedure SetReferansGis(value: TObject); stdcall;
    function GetisReferansGis: boolean; stdcall;
    procedure SetisReferansGis(value: boolean); stdcall;

    function GetDatabaseModul_Interface: IUnknown; stdcall;
    procedure SetDatabaseModul_Interface(value: IUnknown); stdcall;
    function GetProjectCaption: string; stdcall;
    procedure SetProjectCaption(value: string); stdcall;
    function GetBlankGIS: boolean; stdcall;
    procedure SetBlankGIS(value: boolean); stdcall;

    // Function or Procedure
    function NewProject(AProject: IlicgProject; AProjectFileName: string = ''; Progress: TObject = nil): Boolean; stdcall;
    procedure OpenProject(AProject: IlicgProject; AProjectFileName: string = ''); stdcall;
    procedure SaveProject; stdcall;
    function SaveAsProject(var AProjectFileName: string; AAddRecentProjects: Boolean = True): Boolean; stdcall;
    procedure SaveAllProject; stdcall;
    procedure SaveSelectedEntities(AProjectFileName: string = ''); stdcall;
    procedure SaveAsPicture; stdcall;
    procedure AddProjectFiles; stdcall;
    procedure CloseAllProject(var IgnoreClose: Boolean); stdcall;
    procedure CloseProject; stdcall;
    procedure PrintPreview; stdcall;
    procedure Exit; stdcall;

    // Events
    function GetOnMouseMove2D: TlicgMouseMoveEvent; stdcall;
    procedure SetOnMouseMove2D(value: TlicgMouseMoveEvent); stdcall;
    function GetOnZoomChange: TlicgZoomChangeEvent; stdcall;
    procedure SetOnZoomChange(value: TlicgZoomChangeEvent); stdcall;
    function GetOnSelectionChange: TNotifyEvent; stdcall;
    procedure SetOnSelectionChange(value: TNotifyEvent); stdcall;
    function GetOnHintCommadEvent: THintCommandEvent; stdcall;
    procedure SetOnHintCommadEvent(value: THintCommandEvent); stdcall;
    function GetOnShowHint: TlicgShowHintEvent; stdcall;
    procedure SetOnShowHint(value: TlicgShowHintEvent); stdcall;

    function GetCreateLeftFormProc: TCreateLeftFormProc; stdcall;
    procedure SetCreateLeftFormProc(value: TCreateLeftFormProc); stdcall;
    function GetFillLayersProc: TSetFillLayersProc; stdcall;
    procedure SetFillLayersProc(value: TSetFillLayersProc); stdcall;
    function GetLeftPanelGroupProc: TSetLeftPanelGroupProc; stdcall;
    procedure SetLeftPanelGroupProc(value: TSetLeftPanelGroupProc); stdcall;

    {function  GetCreateActionFormEvent  : TCreateActionFormEvent; stdcall;
    procedure SetCreateActionFormEvent (value : TCreateActionFormEvent); stdcall;
    function  GetDestroyActionFormEvent  : TDestroyActionFormEvent; stdcall;
    procedure SetDestroyActionFormEvent (value : TDestroyActionFormEvent); stdcall;}

    // Properties
    property Project: TProject read GetProject;
    property LicadProjectFile: string read GetLicadProjectFile write SetLicadProjectFile;
    property isFirstTemporaryProject: boolean read GetisFirstTemporaryProject write SetisFirstTemporaryProject;
    property ParentHandle: THandle read GetParentHandle write SetParentHandle;
    property GisFileFullName: string read GetGisFileFullName write SetGisFileFullName;
    property DataPath: string read GetDataPath write SetDataPath;
    property ReferansGis: TObject read GetReferansGis write SetReferansGis;
    property isReferansGis: boolean read GetisReferansGis write SetisReferansGis;
    property DBModul: IUnknown read GetDatabaseModul_Interface write SetDatabaseModul_Interface;
    property ProjectCaption: string read GetProjectCaption write SetProjectCaption;

    property CreateLeftFormProc: TCreateLeftFormProc read GetCreateLeftFormProc write SetCreateLeftFormProc;
    property FillLayersProc: TSetFillLayersProc read GetFillLayersProc write SetFillLayersProc;
    property LeftPanelGroupProc: TSetLeftPanelGroupProc read GetLeftPanelGroupProc write SetLeftPanelGroupProc;
    property LeftPanelForm: TForm read GetLeftPanelForm write SetLeftPanelForm;
    //property CreateActionFormEvent  : TCreateActionFormEvent read getCreateActionFormEvent write setCreateActionFormEvent;
    //property DestroyActionFormEvent : TDestroyActionFormEvent read getDestroyActionFormEvent write setDestroyActionFormEvent;

    // Events
    property OnMouseMove2D: TlicgMouseMoveEvent read GetOnMouseMove2D write SetOnMouseMove2D;
    property OnZoomChange: TlicgZoomChangeEvent read GetOnZoomChange write SetOnZoomChange;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property OnHintCommadEvent: THintCommandEvent read GetOnHintCommadEvent write SetOnHintCommadEvent;
    property OnShowHint: TlicgShowHintEvent read GetOnShowHint write SetOnShowHint;
  end;
  
implementation
 

end.


