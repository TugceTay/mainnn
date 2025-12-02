unit Lider.CG.Com.Types;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  Variants,
  Lider.CG.Com.ListsInt;

type
  TlicgIntegerArray = Array of Integer;
  (* ilker GIS events baþlama *)
  THintCommandEvent = procedure(const h: string) of object;

  TlicgShowHintEvent = procedure(Sender: TObject; Layer: TObject; Recno: Integer;
    PickCount: integer; var Hint: string; var ShowHint: Boolean) of object;

  TlicgKeyPressEvent = procedure(Sender: TObject; var Key: Char) of object;

  TlicgMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift:
    TShiftState; X, Y: Integer; const WX, WY: Double) of object;

  TlicgMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y:
    Integer; const WX, WY: Double) of object;

  TlicgZoomChangeEvent = procedure(Sender: TObject; const Scale: Double) of object;
  (* ilker GIS events bitiþ*)

  TlicgProgressEvent = procedure(Sender: TObject; APosition: Integer) of object;


  TlicgImageType = (itECW, itBMP, itJPEG, itPNG, itGIF, itEMF, itWMF, itPCX, itTIF, itTIFG4, itTGA);

  TLeftPanelGroups = (lpdLayer, lpdQuery, lpdInspector, lpdTextSearch, lpdImport,
    lpdExport, lpdReferansMan, lpdCommandList, lpdCommandParameter, lpdNone);

  TCallBackRenameEvent = procedure(AName: String) of object;

  TXMLParameter = class
    ParamType: string;
    ParamName: string;
    ParamValue: variant;
    constructor Create(Name: string; _Type: string);
  end;

  TXMLParameterList = class(TList)
    destructor destroy; override;
  end;

  TAddType = (etAdd, etGet, etData);

type
  TXMLTableData = class;

  TXMLColumnData = class
    TableData: TXMLTableData;
    ColumnName: string;
    AddType: TAddType;
    EditMod: boolean;
    RefTableName: string;
    RefColumnName: string;
    RefColumnDisplayName: string;
    ParameterList: TXMLParameterList;
    typex: string;
    constructor create(_TableData: TXMLTableData);
    destructor destroy; override;
  end;

  TXMLColumnDataList = class(TList)
    function isColumnExists(ColumnName: string): boolean;
    function GetColumnData(ColumnName: string): TXMLColumnData;
    destructor destroy; override;
  end;

  TXMLTableData = class
    TableName: string;
    ColumnDataList: TXMLColumnDataList;
    constructor create;
    destructor destroy; override;
  end;

  TXMLTableDataList = class(TList)
    function isTableExists(TableName: string): boolean;
    function isColumnExists(TableName, ColumnName: string): boolean;
    destructor destroy; override;
  end;

type
  TConnectionParams = packed record
    SpatialType: integer;
    HostName: string;
    Database: string;
    User: string;
    Password: string;
    Port: Integer;
    Encoding: string;
  end;

  PConnectionParams = ^TConnectionParams;

  TTableSQLRecord = record
    LayerFullPathName, TableName, DisplayName, GroupCap, PrimaryIdField,
      InfoFiedId, GeometryField: string;
    SQLOpenFileName: string;
    SQLGetFeatureFileName: string;
    DBFFields: string;
  end;

  ITableSQLClass = interface
    ['{B101CBAA-A035-48BB-86FD-5DB67CEE8ACA}']
    function GetTableName: string; stdcall;
    function GetDisplayName: string; stdcall;
    function GetGroupCap: string; stdcall;
    function GetPrimaryIdField: string; stdcall;
    function GetInfoIdField: string; stdcall;
    function GetGeometryField: string; stdcall;
    function GetSQLOpenFileName: string; stdcall;
    function GetSQLGetFeatureFileName: string; stdcall;
    function GetDBFFields: string; stdcall;
    procedure SetTableName(value: string); stdcall;
    procedure SetDisplayName(value: string); stdcall;
    procedure SetGroupCap(value: string); stdcall;
    procedure SetPrimaryIdField(value: string); stdcall;
    procedure SetInfoIdField(value: string); stdcall;
    procedure SetGeometryField(value: string); stdcall;
    procedure SetSQLOpenFileName(value: string); stdcall;
    procedure SetSQLGetFeatureFileName(value: string); stdcall;
    procedure SetDBFFields(value: string); stdcall;
    function GetSqlRecord: TTableSQLRecord; stdcall;
    property TableName: string read GetTableName write SetTableName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property GroupCap: string read GetGroupCap write SetGroupCap;
    property PrimaryIdField: string read GetPrimaryIdField write SetPrimaryIdField;
    property InfoIdField: string read GetInfoIdField write SetInfoIdField;
    property GeometryField: string read GetGeometryField write SetGeometryField;
    property SQLOpenFileName: string read GetSQLOpenFileName write SetSQLOpenFileName;
    property SQLGetFeatureFileName: string read GetSQLGetFeatureFileName write
      SetSQLGetFeatureFileName;
    property DBFFields: string read GetDBFFields write SetDBFFields;
    property SQLRecord: TTableSQLRecord read GetSqlRecord;
  end;

  TTableSQLClass = class(TInterfacedObject, ITableSQLClass)
  private
    Rec: TTableSQLRecord;
    function GetTableName: string; stdcall;
    function GetDisplayName: string; stdcall;
    function GetGroupCap: string; stdcall;
    function GetPrimaryIdField: string; stdcall;
    function GetInfoIdField: string; stdcall;
    function GetGeometryField: string; stdcall;
    function GetSQLOpenFileName: string; stdcall;
    function GetSQLGetFeatureFileName: string; stdcall;
    function GetDBFFields: string; stdcall;
    procedure SetTableName(value: string); stdcall;
    procedure SetDisplayName(value: string); stdcall;
    procedure SetGroupCap(value: string); stdcall;
    procedure SetPrimaryIdField(value: string); stdcall;
    procedure SetInfoIdField(value: string); stdcall;
    procedure SetGeometryField(value: string); stdcall;
    procedure SetSQLOpenFileName(value: string); stdcall;
    procedure SetSQLGetFeatureFileName(value: string); stdcall;
    procedure SetDBFFields(value: string); stdcall;
    function GetSqlRecord: TTableSQLRecord; stdcall;
  public
    property TableName: string read GetTableName write SetTableName;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property GroupCap: string read GetGroupCap write SetGroupCap;
    property PrimaryIdField: string read GetPrimaryIdField write SetPrimaryIdField;
    property GeometryField: string read GetGeometryField write SetGeometryField;
    property SQLOpenFileName: string read GetSQLOpenFileName write SetSQLOpenFileName;
    property SQLGetFeatureFileName: string read GetSQLGetFeatureFileName write
      SetSQLGetFeatureFileName;
    property SQLRecord: TTableSQLRecord read GetSqlRecord;
    property DBFFields: string read GetDBFFields write SetDBFFields;
  end;

  TFarProcedure1 = procedure(FarProc: TFarProc = nil); stdcall;
  TFarProcedure2 = procedure(CmdID: Word); stdcall;
  TShowSelectFilterDlg = procedure(const CurrAct: TObject; SelectActionButtonsVisible: boolean) of object; stdcall;
  TCloseSelectFilterDlg = procedure() of object; stdcall;
  TShowSelectionDlg = procedure(Gis: TObject; isGoogleMapProject: boolean) of object; stdcall;
  TCloseSelectionDlg = procedure of object; stdcall;
  TEventBehaviourChange = procedure(); stdcall;
  TGetSpatProviderDialogProc = function(const Gis: TObject; ConnParams: TConnectionParams): TObject; stdcall;
  //TCreateActionFormEvent  = function (ActFormClss : TActionFormClass) : TActionForm; stdcall;
  //TDestroyActionFormEvent = procedure (fm : TActionForm); stdcall;
  TOnMainFormCloseEventFire = TNotifyEvent;
  TSetFillLayersProc = procedure(const fmLeft: TForm; const Gis: TObject); stdcall;
  TSetLeftPanelGroupProc = procedure(const fmLeft: TForm; lpg: TLeftPanelGroups;
    pageindex: integer = 0; isTabKey: boolean = false; ForceShowWhenAutoHide:
    boolean = false); stdcall;
  TCreateLeftFormProc = function(const cmdLine: TObject): TForm; stdcall;


function __GetConnectionParams(fn: string): TConnectionParams;
procedure __SetConnectionParams(fn: string; ConnParams: TConnectionParams);

implementation

uses
  IniFiles;

function __GetConnectionParams(fn: string): TConnectionParams;
var
  Inifile: TIniFile;
begin
  Inifile := TiniFile.Create(fn);
  try
    Result.SpatialType := (Inifile.Readinteger('General', 'SpatialType', 1));
    Result.User := Inifile.ReadString('General', 'user', '');
    Result.Password := Inifile.ReadString('General', 'pass', '');
    Result.Database := Inifile.ReadString('General', 'database', '');
    Result.HostName := Inifile.ReadString('General', 'host', '');
    Result.Port := Inifile.ReadInteger('General', 'port', 5432);
    Result.Encoding := 'UTF8';
  finally
    Inifile.Free;
  end;
end;

procedure __SetConnectionParams(fn: string; ConnParams: TConnectionParams);
var
  Inifile: TIniFile;
begin
  Inifile := TiniFile.Create(fn);
  try
    Inifile.writeinteger('General', 'SpatialType', ConnParams.SpatialType);
    Inifile.writeString('General', 'user', ConnParams.User);
    Inifile.writeString('General', 'pass', ConnParams.Password);
    Inifile.writeString('General', 'database', ConnParams.Database);
    Inifile.writeString('General', 'host', ConnParams.HostName);
    Inifile.writeInteger('General', 'port', ConnParams.Port);

  finally
    Inifile.Free;
  end;
end;




{ TTableSQLClass }

function TTableSQLClass.GetDisplayName: string;
begin
  Result := Rec.DisplayName;
end;

function TTableSQLClass.GetGeometryField: string;
begin
  Result := Rec.GeometryField;
end;

function TTableSQLClass.GetGroupCap: string;
begin
  Result := Rec.GroupCap;
end;

function TTableSQLClass.GetDBFFields: string;
begin
  Result := Rec.DBFFields;
end;

function TTableSQLClass.GetPrimaryIdField: string;
begin
  Result := Rec.PrimaryIdField;
end;

function TTableSQLClass.GetSQLGetFeatureFileName: string;
begin
  Result := Rec.SQLGetFeatureFileName;
end;

function TTableSQLClass.GetSQLOpenFileName: string;
begin
  Result := Rec.SQLOpenFileName;
end;

function TTableSQLClass.GetTableName: string;
begin
  Result := Rec.TableName;
end;

procedure TTableSQLClass.SetDisplayName(value: string);
begin
  Rec.DisplayName := value;
end;

procedure TTableSQLClass.SetGeometryField(value: string);
begin
  Rec.GeometryField := value;
end;

procedure TTableSQLClass.SetGroupCap(value: string);
begin
  Rec.GroupCap := value;
end;

procedure TTableSQLClass.SetDBFFields(value: string);
begin
  Rec.DBFFields := value;
end;

procedure TTableSQLClass.SetPrimaryIdField(value: string);
begin
  Rec.PrimaryIdField := value;
end;

procedure TTableSQLClass.SetSQLGetFeatureFileName(value: string);
begin
  Rec.SQLGetFeatureFileName := value;
end;

procedure TTableSQLClass.SetSQLOpenFileName(value: string);
begin
  Rec.SQLOpenFileName := value;
end;

procedure TTableSQLClass.SetTableName(value: string);
begin
  Rec.TableName := value;
end;

function TTableSQLClass.GetSqlRecord: TTableSQLRecord;
begin
  Result := Rec;
end;

function TTableSQLClass.GetInfoIdField: string;
begin
  Result := Rec.InfoFiedId;
end;

procedure TTableSQLClass.SetInfoIdField(value: string);
begin
  Rec.InfoFiedId := value;
end;

{ TXMLParameterList }

destructor TXMLParameterList.destroy;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    TXMLParameter(Items[i]).Free;
  inherited;
end;

{ TXMLColumnData }

constructor TXMLColumnData.create(_TableData: TXMLTableData);
begin
  ParameterList := TXMLParameterList.Create;
  TableData := _TableData;
  AddType := etData;
end;

destructor TXMLColumnData.destroy;
begin
  ParameterList.FRee;
  inherited;
end;


{ TXMLTableData }

constructor TXMLTableData.create;
begin
  ColumnDataList := TXMLColumnDataList.Create;
end;

destructor TXMLTableData.destroy;
begin
  ColumnDataList.Free;
  inherited;
end;

{ TXMLColumnDataList }

destructor TXMLColumnDataList.destroy;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    TXMLColumnData(Items[i]).Free;
  inherited;
end;

function TXMLColumnDataList.GetColumnData(ColumnName: string): TXMLColumnData;
var
  i: integer;
begin
  Result := Nil;
  for i := 0 to Count - 1 do
  begin
    if TXMLColumnData(Items[i]).ColumnName = ColumnName then
    begin
      Result := TXMLColumnData(Items[i]);
      BREAK;
    end;
  end;

end;

function TXMLColumnDataList.isColumnExists(ColumnName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if TXMLColumnData(Items[i]).ColumnName = ColumnName then
    begin
      Result := true;
      BREAK;
    end;
  end;
end;

{ TXMLTableDataList }

destructor TXMLTableDataList.destroy;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    TXMLTableData(Items[i]).Free;
  inherited;
end;

function TXMLTableDataList.isColumnExists(TableName, ColumnName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if TXMLTableData(Items[i]).TableName = TableName then
    begin
      Result := TXMLTableData(Items[i]).ColumnDataList.isColumnExists(ColumnName);
      BREAK;
    end;
  end;

end;

function TXMLTableDataList.isTableExists(TableName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if TXMLTableData(Items[i]).TableName = TableName then
    begin
      Result := True;
      BREAK;
    end;
  end;

end;

{ TXMLParameter }

constructor TXMLParameter.Create(Name: string; _Type: string);
begin
  ParamName := Name;
  ParamType := _Type;
end;

end.


