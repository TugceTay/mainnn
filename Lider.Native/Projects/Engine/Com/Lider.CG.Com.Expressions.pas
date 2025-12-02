unit Lider.CG.Com.Expressions;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  IniFiles,
  SysUtils,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Lib,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TlicgBaseMainExpr = class
  protected
    procedure IDFunc(Sender: TObject; const Group, Identifier: string;
      ParameterList: TParameterList; var ReturnExpr: TExpression); virtual; abstract;
    procedure ClearOrderBy; virtual; abstract;
    function GetOrderBy(Index: Integer): TExpression; virtual; abstract;
    function GetDescending(Index: Integer): Boolean; virtual; abstract;
    function GetClosestMax: Integer; virtual; abstract;
    function GetDefaultLayer: TlicgBaseLayer; virtual; abstract;
    function GetGis: TlicgBaseGis; virtual; abstract;
    function GetHasUDFs: Boolean; virtual; abstract;
    procedure SetClosestMax(const Value: Integer); virtual; abstract;
    function getExpression: TExpression; virtual; abstract;
    procedure setExpression(value: TExpression); virtual; abstract;
  public
    constructor Create(GIS: TlicgBaseGIS; Layer: TlicgBaseLayer);
    destructor Destroy; override;
    procedure ParseExpression(const s: string); virtual; abstract;
    function CheckExpression(const s, CheckforThis: string): Boolean; virtual; abstract;
    function OrderByCount: Integer; virtual; abstract;
    property HasUDFs: Boolean read GetHasUDFs;
    property Gis: TlicgBaseGis read GetGis;
    property DefaultLayer: TlicgBaseLayer read GetDefaultLayer;
    property OrderByList[Index: Integer]: TExpression read GetOrderBy;
    property OrderDescending[Index: Integer]: Boolean read GetDescending;
    property ClosestMax: Integer read GetClosestMax write SetClosestMax;
    property Expression: TExpression read GetExpression write setExpression;
  end;

  (*
  TlicgExprList = class
  private
    FItems: TList;
    function GetItem(Index: Integer): TlicgMainExpr;
  public
    constructor Create;
    destructor destroy; override;
    procedure Add(Value: TlicgMainExpr);
    function Count: Integer;

    { properties }
    property Items[Index: Integer]: TlicgMainExpr read GetItem; default;
  end; *)

  TlicgaseEntExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
    function GetMaxString: string; override;
  public
    constructor Create(ParameterList: TParameterList; GIS: TlicgBaseGIS; Layer:
      TlicgBaseLayer);
  end;

  { This is for the following syntax;
    VECTOR( [ (10,10),(20,20),(30,30),(40,40),(50,50),(10,10) ] )
    and is used mainly to be passed as parameters to other special syntax
    this function will be of type ttBoolean and will return
    true if the vector consist of 1 or more Geometry.Points and false otherwise }

  TlicgVectorType = (vtUndefined, vtPolyline, vtPolygon, vtBuffer);

  TlicgBaseVectorExpr = class(TFunction)
  protected
    function GetVector: IlicgVector; virtual; abstract;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; Vector: IlicgVector;
      VectorType: TlicgVectorType; const BufferWidth: Double);
    destructor Destroy; override;
    property Vector: IlicgVector read GetVector;
  end;

  { this is used as a parameter for other expressions and return the following
    an integer that can be typecasted the following way

    case TlicgGraphicOperator(xx.AsInteger) }

  TlicgBaseGraphicOperatorExpr = class(Tfunction)
  protected
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; GraphicOperator:
      TlicgGraphicOperator);
  end;

  { This is required for supporting the following syntax :
    CITIES_.ENT graphic_operator VECTOR([(10,10),(20,20),(30,30),(40,40),(50,50),(10,10)])
  }
  TlicgBaseQueryVectorExpr = class(TFunction)
  protected
    function GetPrimaryLayer: TlicgBaseLayer; virtual; abstract;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; MainExpr: TlicgBaseMainExpr);
    destructor destroy; override;
    property PrimaryLayer: TlicgBaseLayer read GetPrimaryLayer;
  end;

  { This is for the following syntax :
    CITIES_.ENT graphic_operator STATES_.ENT SCOPE ("STATES_.NAME LIKE 'A%'") }

  TlicgBaseQueryScopeExpr = class(TFunction)
  protected
    function GetPrimaryLayer: TlicgBaseLayer; virtual; abstract;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; MainExpr: TlicgBaseMainExpr);
    destructor Destroy; override;
    property PrimaryLayer: TlicgBaseLayer read GetPrimaryLayer;
  end;

  { This is required for supporting the following syntax :
    CITIES_.ENT ENTIRELY WITHIN STATES_.ENT SCOPE (STATES_.STATE_NAME IN ("Oklahoma", "Washington") ) AND CITIES_.CITY_NAME > 'C'"
    STATES_.ENT ENTIRELY WITHIN VECTOR ( [
      (-122.55, 49.56), (-125.27, 49.22), (-125.32, 46.86), (-125.09, 45.23), (-124.20, 44.12), (-122.49, 44.48),
      (-122.11, 45.30), (-120.41, 45.70), (-118.88, 45.99), (-120.70, 47.46), (-119.34, 48.00), (-120.35, 49.07),
      (-121.89, 49.15) , (-122.55, 49.56) ] )

    and the list of integers are the list of records against to compare
    if no record number listed, then all the records in the layer are used
  }

  TlicgBaseQueryLayerExpr = class(TFunction)
  protected
    function GetPrimaryLayer: TlicgBaseLayer; virtual; abstract;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; MainExpr: TlicgBaseMainExpr);
    destructor Destroy; override;
    property PrimaryLayer: TlicgBaseLayer read GetPrimaryLayer;
  end;

implementation

{ TlicgBaseMainExpr }

constructor TlicgBaseMainExpr.Create(GIS: TlicgBaseGIS; Layer: TlicgBaseLayer);
begin
  inherited Create;
end;

destructor TlicgBaseMainExpr.Destroy;
begin

  inherited;
end;

{ TlicgaseEntExpr }

constructor TlicgaseEntExpr.Create(ParameterList: TParameterList; GIS:
  TlicgBaseGIS; Layer: TlicgBaseLayer);
begin
  inherited Create(ParameterList);
end;

function TlicgaseEntExpr.GetAsString: string;
begin
  Result := inherited GetAsString;
end;

function TlicgaseEntExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

function TlicgaseEntExpr.GetMaxString: string;
begin
  Result := inherited GetMaxString;
end;

{ TlicgBaseVectorExpr }

constructor TlicgBaseVectorExpr.Create(ParameterList: TParameterList; Vector:
  IlicgVector; VectorType: TlicgVectorType; const BufferWidth: Double);
begin
  inherited Create(ParameterList);
end;

destructor TlicgBaseVectorExpr.Destroy;
begin

  inherited;
end;

function TlicgBaseVectorExpr.GetAsBoolean: Boolean;
begin
  Result := inherited GetAsBoolean;
end;

function TlicgBaseVectorExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

{ TlicgBaseGraphicOperatorExpr }

constructor TlicgBaseGraphicOperatorExpr.Create(ParameterList: TParameterList;
  GraphicOperator: TlicgGraphicOperator);
begin
  inherited Create(ParameterList);
end;

function TlicgBaseGraphicOperatorExpr.GetAsInteger: Integer;
begin
  Result := inherited GetAsInteger;
end;

function TlicgBaseGraphicOperatorExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

{ TlicgBaseQueryVectorExpr }

constructor TlicgBaseQueryVectorExpr.Create(ParameterList: TParameterList;
  MainExpr: TlicgBaseMainExpr);
begin
  inherited Create(ParameterList);
end;

destructor TlicgBaseQueryVectorExpr.destroy;
begin

  inherited;
end;

function TlicgBaseQueryVectorExpr.GetAsBoolean: Boolean;
begin
  Result := inherited GetAsBoolean;
end;

function TlicgBaseQueryVectorExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

{ TlicgBaseQueryScopeExpr }

constructor TlicgBaseQueryScopeExpr.Create(ParameterList: TParameterList;
  MainExpr: TlicgBaseMainExpr);
begin
  inherited Create(ParameterList);
end;

destructor TlicgBaseQueryScopeExpr.Destroy;
begin

  inherited;
end;

function TlicgBaseQueryScopeExpr.GetAsBoolean: Boolean;
begin
  Result := inherited GetAsBoolean;
end;

function TlicgBaseQueryScopeExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

{ TlicgBaseQueryLayerExpr }

constructor TlicgBaseQueryLayerExpr.Create(ParameterList: TParameterList;
  MainExpr: TlicgBaseMainExpr);
begin
  inherited Create(ParameterList);
end;

destructor TlicgBaseQueryLayerExpr.Destroy;
begin
  inherited;
end;

function TlicgBaseQueryLayerExpr.GetAsBoolean: Boolean;
begin
  Result := inherited GetAsBoolean;
end;

function TlicgBaseQueryLayerExpr.GetExprType: TExprType;
begin
  Result := inherited GetExprType;
end;

end.


