unit Lider.CG.Com.VectorInt;

interface

uses
  Classes,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.GeoTypes;

type
  IlicgVector = interface
    ['{F06A97AA-B99F-4559-905F-A49CBAF79F78}']
    procedure Put2D(Index: Integer; const item: TlicgCoor); stdcall;
    function Get2D(Index: Integer): TlicgCoor; stdcall;
    procedure Put3D(Index: Integer; const item: TlicgCoor3D); stdcall;
    function Get3D(Index: Integer): TlicgCoor3D; stdcall;
    procedure Put4D(Index: Integer; const item: TlicgCoor4D); stdcall;
    function Get4D(Index: Integer): TlicgCoor4D; stdcall;
    function GetX(Index: Integer): Double; stdcall;
    procedure PutX(Index: Integer; const Value: Double); stdcall;
    function GetY(Index: Integer): Double; stdcall;
    procedure PutY(Index: Integer; const Value: Double); stdcall;
    function GetZ(Index: Integer): Double; stdcall;
    procedure PutZ(Index: Integer; const Value: Double); stdcall;
    function GetN(Index: Integer): Double; stdcall;
    procedure PutN(Index: Integer; const Value: Double); stdcall;
    procedure SetCanGrow(CanGrow: boolean); stdcall;
    function GetCanGrow: boolean; stdcall;
    function GetDisableEvents: Boolean; stdcall;
    procedure SetDisableEvents(const Value: Boolean); stdcall;
    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall;
    procedure SetSize(Size: integer); stdcall;
    function GetSize: integer; stdcall;
    procedure SetCount(Count: integer); stdcall;
    function GetCount: integer; stdcall;
    function GetOnChange: TlicgCoorsChangedEvent; stdcall;
    procedure SetOnChange(value: TlicgCoorsChangedEvent); stdcall;
    function GetCS: IlicgCS; stdcall;
    procedure SetCS(const value: IlicgCS); stdcall;
    function GetAsString2D: string; stdcall;
    function GetAsString3D: string; stdcall;
    procedure SetAsString(const Value: string); stdcall;
    procedure GetIndexes(PartIndex: Integer; var Start_Index, End_Index: Integer); stdcall;
    function Dimension: integer; stdcall;
    function CalcExtension2D: TlicgExtent; stdcall;
    function CalcExtension4D: TlicgExtent4D; stdcall;
    function GetExtension2D: TlicgExtent; stdcall;
    function GetExtension4D: TlicgExtent4D; stdcall;
    procedure SetExtension2D(value: TlicgExtent); stdcall;
    procedure SetExtension4D(value: TlicgExtent4D); stdcall;
    function Add(X, Y: Double; Z: Double = 0; N: Double = 0): boolean; overload; stdcall;
    function Add(ACoor: TlicgCoor4D): boolean; overload; stdcall;
    function Delete(Index: Integer): boolean; stdcall;
    function Insert(Index: Integer; X, Y: Double; Z: Double = 0; N: Double = 0): boolean; stdcall;
    { sonuc baþarýsýz ise = -1, baþarýlý ise, noktayý kaydettiði indeksi döndürür. SADECE_2D }
    function AddOrdered(X, Y: Double; Z: Double = 0; N: Double = 0): integer; stdcall;
    procedure AddPoints(const Items: array of TlicgCoor); stdcall;
    procedure Assign(const Source: IlicgVector); stdcall;
      // 'source'u listenin sonuna ekler.
    procedure Append(const Source: IlicgVector); stdcall;
    procedure Clear; stdcall;
    function IsClosed: boolean; stdcall;
    function Rotation(PartIndex: integer = 0): integer; stdcall;
    function IsEqual(const Other: IlicgVector; const ReverseAlso: boolean =
      false; const Epsilon: Double = _EPSILON): boolean; stdcall;
    function PointExists(X, Y: Double; Z: Double = 0; N: Double = 0;
      Epsilon: Double = _EPSILON): integer; stdcall;
    function SignedArea: Double; stdcall;
    function Area: Double; stdcall;
    function Length: Double; stdcall;
    procedure DeleteDuplicates; stdcall;

    function GetCentroid: TlicgCoor4D; stdcall;
    procedure SetCentroid(Value: TlicgCoor4D) stdcall;

    function GetCenterOfMass: TlicgCoor4D; stdcall;
    procedure SetCenterOfMass(Value: TlicgCoor4D) stdcall;

      { su anda sadece iki boyutlu noktalar üzerinde iþlem yapýyor. }
    function PointOnMe(X, Y: Double; Epsilon: Double = _EPSILON): integer; stdcall;

    function InPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesnenin içinde mi? (Kenarlar Hariç).
    function OnPoly(const ACoor: TlicgCoor): Boolean; stdcall; //Nokta Nesene Üstünde mi? (Kenarlarýn üstündemi, Kapalý alanýn içi Hariç)

      { Koordinat array, kendisini kesmiyorsa "basit"tir. }
    function IsSelfIntersecting: boolean; stdcall;
      { PartIndex: yönü deðiþtirilmek istenen parçanýn indeksi. Indeks 0 dan baþladýðý için
                   bu parametrenin de anlamlý deðerleri, 0 ile PartCount-1 arasi.
                   -1 verildiðinde, tüm nesne ters çevriliyor. }
    function RevertRotation(PartIndex: integer = -1): boolean; stdcall;
    procedure LoadFromStream(Stream: TStream; Version: Byte; _SpatRef: IlicgCS = nil); stdcall;
    procedure SaveToStream(Stream: TStream; _SpatRef: IlicgCS = nil); stdcall;
    procedure LoadFromStreamWithoutCount(UsePoint3D: Boolean; pCount: Integer;
      Stream: TStream; Version: Byte; _SpatRef: IlicgCS = Nil); stdcall;
    procedure SaveToStreamWithoutCount(UsePoint3D: Boolean; pCount: Integer;
      Stream: TStream; _SpatRef: IlicgCS = Nil); stdcall;
      { Partlarla ilgili fonksiyonlar. }
    function GetParts(Index: Integer): integer; stdcall;
    procedure SetParts(Index: Integer; const Value: integer); stdcall;
    function GetPartList: TList; stdcall;
    function GetPartCount: integer; stdcall;
    procedure SetPartCount(const Value: integer); stdcall;
    function AddPart(PointstartIndex: integer = -1): boolean; stdcall;
    function DeletePart(const Index: integer): boolean; stdcall;
    function InsertPart(const PartIndex, PointIndex: integer): boolean; stdcall;
    function PartStart(index: integer): integer; stdcall;
      // index. part'ýn son noktasýnýn indisi döner
    function PartEnd(index: integer): integer; stdcall;
    procedure ClearParts; stdcall;
    function GetPartCapacity: integer; stdcall;
    procedure SetPartCapacity(const Value: integer); stdcall;
    function GetPartSize(index: integer): integer; stdcall;
    function GetPoint(PartIndex, RelativeIndex: integer): TlicgCoor; stdcall;
    function MakeClosed: boolean; stdcall;
      { Bu fonksiyonlar, count degerinden küçük olan noktalar için atama/okuma iþlemleri yapýyor.
        Yeni nokta eklemek için Add ya da insert fonksiyonlarý kullanýlmalý. }
    property Coors2D[Index: Integer]: TlicgCoor read Get2D write Put2D; default;
    property Coors3D[Index: Integer]: TlicgCoor3D read Get3D write Put3D;
    property Coors4D[Index: Integer]: TlicgCoor4D read Get4D write Put4D;
    property CanGrow: boolean read GetCanGrow write SetCanGrow;
    property Size: integer read GetSize write SetSize;
    property Count: integer read GetCount write SetCount;
    property Parts[Index: Integer]: integer read GetParts write SetParts;
    property PartList: TList read GetPartList;
    property X[Index: Integer]: Double read GetX write PutX;
    property Y[Index: Integer]: Double read GetY write PutY;
    property Z[Index: Integer]: Double read GetZ write PutZ;
    property N[Index: Integer]: Double read GetN write PutN;
    property PartCapacity: integer read GetPartCapacity write SetPartCapacity; // Load ve save de buraya geldiðinde, silinecek!!!
    property PartCount: integer read GetPartCount write SetPartCount;
    property DisableEvents: Boolean read GetDisableEvents write SetDisableEvents;
    property OnChange: TlicgCoorsChangedEvent read GetOnChange write SetOnChange;
    property CS: IlicgCS read GetCS write SetCS;
    property Extent: TlicgExtent read GetExtension2D write SetExtension2D;
    property Extent4D: TlicgExtent4D read GetExtension4D write SetExtension4D;
    property AsString: string read GetAsString3D write SetAsString;
    property AsString2D: string read GetAsString2D;
    property Centroid: TlicgCoor4D read GetCentroid write SetCentroid;
    property CenterOfMass: TlicgCoor4D read GetCenterOfMass write SetCenterOfMass;
  end;

implementation

end.


