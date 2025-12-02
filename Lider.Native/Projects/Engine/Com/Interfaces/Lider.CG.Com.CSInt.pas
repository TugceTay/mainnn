unit Lider.CG.Com.CSInt;

interface

uses
  Lider.CG.Com.GeoTypes;

type
  IlicgCS = interface
    ['{FAA2F2DC-B32F-4BA7-896E-32905BE74F13}']
    function GetError: Integer; stdcall;
    function GetReversedCoordinates: Boolean; stdcall;
    procedure SetReversedCoordinates(const Value: Boolean); stdcall;
    function GetDescription: WideString; stdcall;
    function GetEPSG: Integer; stdcall;
    function GetMasterEPSG: Integer; stdcall;
    function GetWKT: WideString; stdcall;
    function GetCSCoordinateSystem: TObject; stdcall;
    procedure SetCSCoordinateSystem(const Value: TObject); stdcall;
    //function GetIsOnTheFly: Boolean; stdcall;
    //procedure SetIsOnTheFly(const Value: Boolean); stdcall;
    //function GetOnTheFlyCS: IlicgCS; stdcall;
    //procedure SetOnTheFlyCS(const Value: IlicgCS); stdcall;

    function  GetFullWKT: WideString; stdcall;
    function  GetPrettyWKT: WideString; stdcall;
    function  GetValidityExtentWGS: TlicgExtent; stdcall;
    function  GetValidityExtent: TlicgExtent; stdcall;

    procedure Assign(const ASource : TObject); stdcall;
    function  ToGeocs(const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  ToGeocs3D(const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    function  FromGeocs(const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  FromGeocs3D(const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    procedure ToWGS3D_Ref(var ACoords: TlicgCoor3D); stdcall;
    procedure FromWGS3D_Ref(var ACoords: TlicgCoor3D); stdcall;
    function  ExtentFromWGS(const AExtent: TlicgExtent): TlicgExtent; stdcall;
    function  ExtentToWGS(const AExtent: TlicgExtent): TlicgExtent; stdcall;

    function  IsSame(const ACS: IlicgCS): Boolean; stdcall;
    function  CanConvert(const ACS: IlicgCS): Boolean; stdcall;
    function  CanConvertEx(const ACS: IlicgCS): Boolean; stdcall;
    function  FromCS(const ACS: IlicgCS; const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  FromCS3D(const ACS: IlicgCS; const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    function  ToCS(const ACS: IlicgCS; const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  ToCS3D(const ACS: IlicgCS; const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    function  ToWGS(const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  ToWGS3D(const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    function  FromWGS(const ACoords: TlicgCoor): TlicgCoor; stdcall;
    function  FromWGS3D(const ACoords: TlicgCoor3D): TlicgCoor3D; stdcall;
    function  ExtentFromCS(const ACS: IlicgCS; const AExtent: TlicgExtent): TlicgExtent; stdcall;
    function  ExtentToCS(const ACS: IlicgCS; const AExtent : TlicgExtent): TlicgExtent; stdcall;
    function  Distance(const AFrom: TlicgCoor; const ATo: TlicgCoor): Double; stdcall;
    function  DistanceFromCS(const AFromCS: IlicgCS; const AFrom: TlicgCoor;
                             const AToCS: IlicgCS; const ATo: TlicgCoor): Double; stdcall;

    procedure SaveAsWKTFile(const APath: WideString); stdcall;
    procedure SaveAsPrettyWKTFile(const APath: WideString); stdcall;
    // ilker eklenenler
    //procedure ByWKT(const Awkt: WideString); stdcall;
    // ihtiyaþ deðil function Authority( const Awkt: string) : string ; stdcall;
    //procedure ByEPSG   ( const Aepsg: Integer); stdcall;
    //procedure ByWKTFile( const Apath: WideString); stdcall;
    procedure Execute; stdcall;

    function IsGeographicCS: Boolean; stdcall;
    function IsProjectedCS: Boolean; stdcall;
    function IsUnknownCS: Boolean; stdcall;
    function CSCoorToCSCoor(AXIn, AYIn: Double; var AXOut, AYOut: Double; ACSIn, ACSOut: IlicgCS): Boolean; stdcall;
    //function CurrentCS: IlicgCS; stdcall;
    function ExecuteSelect(AInCS, AOutCS: IlicgCS): Boolean; overload; stdcall;
    function ExecuteSelect(epsg: Integer): Integer; overload; stdcall;

    property EPSG: Integer read GetEPSG;
    property MasterEPSG: Integer read GetMasterEPSG;
    property WKT: WideString read GetWKT;
    property Description: WideString read GetDescription;

    property FullWKT: WideString  read GetFullWKT;
    property PrettyWKT: WideString  read GetPrettyWKT;
    property Error: Integer read GetError;
    property ReversedCoordinates: Boolean read  GetReversedCoordinates write SetReversedCoordinates;
    property ValidityExtentWGS: TlicgExtent read GetValidityExtentWGS ;
    property ValidityExtent: TlicgExtent read GetValidityExtent;
    property CSCoordinateSystem: TObject read GetCSCoordinateSystem write SetCSCoordinateSystem;

    //property IsOnTheFly: Boolean read GetIsOnTheFly write SetIsOnTheFly;
    //property OnTheFlyCS: IlicgCS read GetOnTheFlyCS write SetOnTheFlyCS;
  end;

  IlicgCSFactory = interface
    ['{B0235318-7444-47EF-BBD2-EA48DD909CF9}']
    function ByWKT(const Awkt: WideString) : IlicgCS; stdcall;
    function ByEPSG   ( const Aepsg: Integer) : IlicgCS; stdcall;
    function ByWKTFile( const Apath: WideString) : IlicgCS; stdcall;
    procedure EPSGToCS(var ACS: IlicgCS; const Aepsg: Integer) stdcall;
    procedure WKTToCS(var ACS: IlicgCS; const Awkt: WideString) stdcall;
  end;

  TOntheFlyCSInfo = record
    CS: IlicgCS;
    Enable: Boolean;
  end;

implementation

end.


