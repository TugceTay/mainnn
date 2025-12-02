unit Lider.CG.Com.ReferenceInt;

interface

uses
  Windows,
  Forms,
  Classes,
  Sysutils,
  Graphics,
  Dialogs,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.ModulesConsts,
  Lider.CG.Com.GeoTypes;

//function passPoint2D(X, Y: double): TPassPoint2D;

(*type
  IlicgTransformGCP = interface
    ['{0537CF08-A693-4F23-AEB5-3107F53078DB}']
    function GetPolynomialDegree: integer; stdcall;
    procedure SetPolynomialDegree(value: integer); stdcall;
    property PolynomialDegree: integer read GetPolynomialDegree write
      SetPolynomialDegree;

      { if polynomialDegree is 1 then 1.Derece polynomial, en az 3 nokta
       if polynomialDegree is 2 then 2.Derece polynomial, en az 6 nokta
       if polynomialDegree is 3 then 3.Derece polynomial, en az 10 nokta
       }
    function Active(polynomialDegree: integer): boolean; stdcall;
    function Prepare(polynomialDegree: integer): boolean; stdcall;
    function Transform(const _ptg: TPassPoint2D): TPassPoint2D; stdcall;
    function Untransform(const _ptg: TPassPoint2D): TPassPoint2D; stdcall;
    procedure Transform3D_Ref(var _ptg: TPassPoint3D); stdcall;
    procedure Untransform3D_Ref(var _ptg: TPassPoint3D); stdcall;
    procedure LoadFromFile(const _path: wideString); stdcall;
    procedure SaveToFile(const _path: wideString); stdcall;
    function MustSave: Boolean; stdcall;
    function GetCuttingPolygon: widestring; stdcall;
    procedure SetCuttingPolygon(value: widestring); stdcall;
    property CuttingPolygon: WideString read GetCuttingPolygon write SetCuttingPolygon;
    function GetRMS: double; stdcall;
    function getPointsCount: Integer; stdcall;
    function getPointsSrc(const _index: Integer): TPassPoint2D; stdcall;
    procedure setPointsSrc(const _index: Integer; const _value: TPassPoint2D); stdcall;
    function getPointsDst(const _index: Integer): TPassPoint2D; stdcall;
    procedure setPointsDst(const _index: Integer; const _value: TPassPoint2D); stdcall;
    function getPointsActive(const _index: Integer): Boolean; stdcall;
    procedure setPointsActive(const _index: Integer; const _value: Boolean); stdcall;
    function getPointsUid(const _index: Integer): Integer; stdcall;
    procedure setPointsUid(const _index: Integer; const _value: Integer); stdcall;
    procedure Clear; stdcall;
    procedure AddPoint(const _src: TPassPoint2D; const _dst: TPassPoint2D; const
      _uid: Integer); overload; stdcall;
    procedure AddPoint(const _src: TPassPoint2D; const _dst: TPassPoint2D; const
      _uid: Integer; const _active: Boolean); overload; stdcall;
    property RMS: Double read GetRMS;
    property PointsCount: Integer read getPointsCount;
    property PointsSrc[const _index: Integer]: TPassPoint2D read getPointsSrc
      write setPointsSrc;
    property PointsDst[const _index: Integer]: TPassPoint2D read getPointsDst
      write setPointsDst;
    property PointsActive[const _index: Integer]: Boolean read getPointsActive
      write setPointsActive;
    property PointsUid[const _index: Integer]: Integer read getPointsUid write
      setPointsUid;
  end;  *)

  (*
  IlicgSpatialReference = interface
    ['{210A6709-E4A7-454E-A7C8-905BA8C7C3F1}']
    function ByEPSG(epsg: integer): IlicgCoordinatSystem; stdcall;
    function ByWKT(AWKT: string): IlicgCoordinatSystem; stdcall;
    function isLatlongPrj(epsg: integer): boolean; stdcall;
    //function ExecuteSelect(inPrj, OutPrj: IlicgCoordinatSystem): Boolean; overload; stdcall;
    //function ExecuteSelect(epsg: integer): integer; overload; stdcall;
    function GetPrjDescription(const inPrj: IlicgCoordinatSystem): WideString; stdcall;
    function isUnknownPrj(Prj: IlicgCoordinatSystem): boolean; stdcall; // is Kartezyen

    procedure saveToFile(IniFileName: WideString); stdcall;
    procedure loadFromFile(IniFileName: WideString); stdcall;
    function isLatlon: boolean; stdcall;
    function GetIsOnTheFly: boolean; stdcall;
    procedure SetIsOnTheFly(value: boolean); stdcall;
    property IsOntheFly: boolean read GetIsOnTheFly write SetIsOnTheFly;
    function GetOntheFlyProjection: IlicgCoordinatSystem; stdcall;
    procedure SetOntheFlyProjection(value: IlicgCoordinatSystem); stdcall;
    property OntheflyProjection: IlicgCoordinatSystem read GetOntheFlyProjection write
      SetOntheFlyProjection;
    function GetProjection: IlicgCoordinatSystem; stdcall;
    procedure SetProjection(value: IlicgCoordinatSystem); stdcall;
    property Projection: IlicgCoordinatSystem read GetProjection write SetProjection;
    function CurrentProjection: IlicgCoordinatSystem; stdcall;
    function getOntheFlyMatrixElements: TAffineMatrixElements; stdcall;
    procedure setOntheFlyMatrixElements(value: TAffineMatrixElements); stdcall;
    property OntheFlyMatrixElements: TAffineMatrixElements read
      getOntheFlyMatrixElements write setOntheFlyMatrixElements;
    function ProjectPoint(aXIn: double; aYIn: double; var aXOut: double; var
      aYOut: double; aProjIn, aProjOut: IlicgCoordinatSystem): boolean;
  end; *)

function EqualRect_ttkExtent(R1, R2: TReferenceGISExtent): boolean;

implementation

function EqualRect_ttkExtent(R1, R2: TReferenceGISExtent): boolean;
begin
  Result := (R1.Xmin = R2.Xmin) and (R1.XMax = R2.XMax) and (R1.Ymin = R2.Ymin)
    and (R1.YMax = R2.YMax);
end;

end.


