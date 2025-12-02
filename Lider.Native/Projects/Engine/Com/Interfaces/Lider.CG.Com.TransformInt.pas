unit Lider.CG.Com.TransformInt;

interface

uses
  Windows,
  Forms,
  Classes,
  Sysutils,
  Graphics,
  Dialogs,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.ModulesConsts;

type
  IlicgTransformGCP = interface
    ['{0537CF08-A693-4F23-AEB5-3107F53078DB}']
    function GetPolynomialDegree: integer; stdcall;
    procedure SetPolynomialDegree(value: integer); stdcall;
    property PolynomialDegree: integer read GetPolynomialDegree write SetPolynomialDegree;
    { if polynomialDegree is 1 then 1.Derece polynomial, en az 3 nokta
       if polynomialDegree is 2 then 2.Derece polynomial, en az 6 nokta
       if polynomialDegree is 3 then 3.Derece polynomial, en az 10 nokta}
    function Active(polynomialDegree: integer): boolean; stdcall;
    function Prepare(polynomialDegree: integer): boolean; stdcall;
    function Transform(const _ptg: TlicgCoor): TlicgCoor; stdcall;
    function Untransform(const _ptg: TlicgCoor): TlicgCoor; stdcall;
    procedure Transform3D_Ref(var _ptg: TlicgCoor3D); stdcall;
    procedure Untransform3D_Ref(var _ptg: TlicgCoor3D); stdcall;
    procedure LoadFromFile(const _path: wideString); stdcall;
    procedure SaveToFile(const _path: wideString); stdcall;
    function MustSave: Boolean; stdcall;
    function GetCuttingPolygon: widestring; stdcall;
    procedure SetCuttingPolygon(value: widestring); stdcall;
    property CuttingPolygon: WideString read GetCuttingPolygon write SetCuttingPolygon;
    function GetRMS: double; stdcall;
    function getPointsCount: Integer; stdcall;
    function getPointsSrc(const _index: Integer): TlicgCoor; stdcall;
    procedure setPointsSrc(const _index: Integer; const _value: TlicgCoor); stdcall;
    function getPointsDst(const _index: Integer): TlicgCoor; stdcall;
    procedure setPointsDst(const _index: Integer; const _value: TlicgCoor); stdcall;
    function getPointsActive(const _index: Integer): Boolean; stdcall;
    procedure setPointsActive(const _index: Integer; const _value: Boolean); stdcall;
    function getPointsUid(const _index: Integer): Integer; stdcall;
    procedure setPointsUid(const _index: Integer; const _value: Integer); stdcall;
    procedure Clear; stdcall;
    procedure AddPoint(const _src: TlicgCoor; const _dst: TlicgCoor; const _uid: Integer); overload; stdcall;
    procedure AddPoint(const _src: TlicgCoor; const _dst: TlicgCoor; const _uid: Integer; const _active: Boolean); overload; stdcall;
    property RMS: Double read GetRMS;
    property PointsCount: Integer read getPointsCount;
    property PointsSrc[const _index: Integer]: TlicgCoor read getPointsSrc write setPointsSrc;
    property PointsDst[const _index: Integer]: TlicgCoor read getPointsDst write setPointsDst;
    property PointsActive[const _index: Integer]: Boolean read getPointsActive write setPointsActive;
    property PointsUid[const _index: Integer]: Integer read getPointsUid write setPointsUid;
  end;

implementation

end.


