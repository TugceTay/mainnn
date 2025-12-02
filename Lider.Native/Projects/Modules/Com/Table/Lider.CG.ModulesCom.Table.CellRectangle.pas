unit Lider.CG.ModulesCom.Table.CellRectangle;

interface

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  Lider.CG.Com.EntityInt,
  TypInfo,
  Vcl.Graphics,
  Math,
  Lider.CG.Com.Math,
  Lider.CG.Com.LicadInt,
  System.Generics.Collections,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.GeoLibrary,
  System.RegularExpressions,
  Lider.CG.ModulesCom.Table.StringFunctions;

type
  DikeyHizalama = (Ust = 0, DikeyOrta = 1, Alt = 2);

  YatayHizalama = (Sol = 0, YatayOrta = 1, Sag = 2);

  TextJustify = (
                  /// <summary>
                  ///   1
                  /// </summary>
                  SolOrta = 49, //1
                  /// <summary>
                  ///   2
                  /// </summary>
                  SolUst = 50, //2
                  /// <summary>
                  ///   3
                  /// </summary>
                  OrtaUst = 51, //3
                  /// <summary>
                  ///   4
                  /// </summary>
                  SagUst = 52, //4
                  /// <summary>
                  ///   5
                  /// </summary>
                  SagOrta = 53, //5
                  /// <summary>
                  ///   6
                  /// </summary>
                  SolAltDis = 54, //6
                  /// <summary>
                  ///   7
                  /// </summary>
                  OrtaAltDis = 55, //7
                  /// <summary>
                  ///   8
                  /// </summary>
                  SagAltDis = 56, //8
                  /// <summary>
                  ///   C
                  /// </summary>
                  OrtaAlt = 67, //C
                  /// <summary>
                  ///   L
                  /// </summary>
                  Solalt = 76, //L
                  /// <summary>
                  ///   M
                  /// </summary>
                  OrtaOrta = 77, //M
                  /// <summary>
                  ///   P
                  /// </summary>
                  SolOrtaDis = 80, //P
                  /// <summary>
                  ///   Q
                  /// </summary>
                  SagOrtaDis = 81, //Q
                  /// <summary>
                  ///   R
                  /// </summary>
                  SagAlt = 82, //R
                  /// <summary>
                  ///   U
                  /// </summary>
                  UstOrtaDis = 85 //U
                );
                            //'0000 0001 //'0000 0010 //'0000 0100
    TextFlag = (Normal = 0, Italik = 1, AltiCizili = 2, Fon = 4);

  Point = class
    public
      Y: Double;
      X: Double;

      function isEqual(Param1, Param2: Point): Boolean;

      Constructor Create;
//      Destructor destroy; override;
//    class operator Equal(Param1, Param2: Point): Boolean;
//    class operator NotEqual(Param1, Param2: Point): Boolean;
  end;

//  PointList = array of Point;

  Limit = class
    cll: Point;
    cur: Point;
      constructor Create;
//      Destructor destroy; override;
  end;

  Border = class
    Draw: Boolean;
    LineIndex: Integer;

    function isEqual(Param1, Param2: Border): Boolean;
//    Destructor destroy; override;
//    class operator Equal(Param1, Param2: Border): Boolean;
//    class operator NotEqual(Param1, Param2: Border): Boolean;
  end;

  TEXTMETRIC = record
    tmHeight: Integer;
    tmAscent: Integer;
    tmDescent: Integer;
    tmInternalLeading: Integer;
    tmExternalLeading: Integer;
    tmAveCharWidth: Integer;
    tmMaxCharWidth: Integer;
    tmWeight: Integer;
    tmOverhang: Integer;
    tmDigitizedAspectX: Integer;
    tmDigitizedAspectY: Integer;
    tmFirstChar: Char;
    tmLastChar: Char;
    tmDefaultChar: Char;
    tmBreakChar: Char;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;

  ICellRectangle = interface//(TInterfaceBase, INotifyPropertyChanged)
    ['{42232050-7D3D-4F89-A0A6-409A53121B87}']

//Inherits INotifyPropertyChanged
//Inherits INotifyPropertyChanging
//    function GetValue: Double;
//    procedure SetMyEvent(const Value: TNotifyEvent);
//    function GetMyEvent: TNotifyEvent;
//    property MyEvent: TNotifyEvent read GetMyEvent write SetMyEvent;

    procedure SetTabakaAdi(Value: String);
    function GetTabakaAdi: String;
    Property TabakaAdi: String read GetTabakaAdi write SetTabakaAdi;

    procedure SetYaziTabakaAdi(Value: String);
    function GetYaziTabakaAdi: String;
    Property YaziTabakaAdi: String read GetYaziTabakaAdi write SetYaziTabakaAdi;

    procedure SetCizgiTabakaAdi(Value: String);
    function GetCizgiTabakaAdi: String;
    Property CizgiTabakaAdi: String read GetCizgiTabakaAdi write SetCizgiTabakaAdi;

    procedure SetAdi(Value: String);
    function GetAdi: String;
    Property Adi: String read GetAdi write SetAdi;

    procedure SetGenislik(Value: Double);
    function GetGenislik: Double;
    Property Genislik: Double read GetGenislik write SetGenislik;

    procedure SetYukseklik(Value: Double);
    function GetYukseklik: Double;
    Property Yukseklik: Double read GetYukseklik write SetYukseklik;

    procedure SetSolAlt(Point: Point);
    function GetSolAlt: Point;
    Property SolAlt: Point read GetSolAlt write SetSolAlt;

    function GetLimit: Limit;
    Property Limits: Limit read GetLimit;

    procedure SetOlcek(Value: Double);
    function GetOlcek: Double;
    Property Olcek: Double read GetOlcek write SetOlcek;

    procedure SetAci(Value: Double);
    function GetAci: Double;
    Property Aci: Double read GetAci write SetAci;

    procedure SetYazi(Value: String);
    function GetYazi: String;
    Property Yazi: String read GetYazi write SetYazi;

    procedure SetIceGirinti(Value: Single);
    function GetIceGirinti: Single;
    Property IceGirinti: Single read GetIceGirinti write SetIceGirinti;

    function GetYaziUzunlugu: Single;
    Property YaziUzunlugu: Single read GetYaziUzunlugu;

    procedure SetFont(Value: IlicgFontTool);
    function GetFont: IlicgFontTool;
    Property Font: IlicgFontTool read GetFont write SetFont;

    procedure SetDikeyHizalama(Value: DikeyHizalama);
    function GetDikeyHizalama: DikeyHizalama;
    Property DikeyHizalama: DikeyHizalama read GetDikeyHizalama write SetDikeyHizalama;

    procedure SetYatayHizalama(Value: YatayHizalama);
    function GetYatayHizalama: YatayHizalama;
    Property YatayHizalama: YatayHizalama read GetYatayHizalama write SetYatayHizalama;

    procedure SetYaziWrap(Value: Boolean);
    function GetYaziWrap: Boolean;
    Property YaziWrap: Boolean read GetYaziWrap write SetYaziWrap;
  end;
  {
  TDevice = class(TInterfacedObject, ICellRectangle)
  private
    FMyEvent: TNotifyEvent;
    procedure SetMyEvent(const Value: TNotifyEvent);
    function GetMyEvent: TNotifyEvent;
  public
    function GetValue: Double;
    procedure EmulChar;
  end;
  }

  CellRectangle = class(TInterfacedObject, ICellRectangle)
    TempPoint: Point; TempPoint2, TempPoint3: Point;
//    FOnChangedProperty:TNotifyEvent;
//    property OnChangedProperty:TNotifyEvent read FOnChangedProperty write FOnChangedProperty;

    procedure SetTabakaAdi(Value: String);
    function GetTabakaAdi: String;
    Property TabakaAdi: String read GetTabakaAdi write SetTabakaAdi;

    procedure SetYaziTabakaAdi(Value: String);
    function GetYaziTabakaAdi: String;
    Property YaziTabakaAdi: String read GetYaziTabakaAdi write SetYaziTabakaAdi;

    procedure SetCizgiTabakaAdi(Value: String);
    function GetCizgiTabakaAdi: String;
    Property CizgiTabakaAdi: String read GetCizgiTabakaAdi write SetCizgiTabakaAdi;

    procedure SetAdi(Value: String);
    function GetAdi: String;
    Property Adi: String read GetAdi write SetAdi;

    procedure SetGenislik(Value: Double);
    function GetGenislik: Double;
    Property Genislik: Double read GetGenislik write SetGenislik;

    procedure SetYukseklik(Value: Double);
    function GetYukseklik: Double;
    Property Yukseklik: Double read GetYukseklik write SetYukseklik;

    procedure SetSolAlt(Value: Point);
    function GetSolAlt: Point;
    Property SolAlt: Point read GetSolAlt write SetSolAlt;

    function GetLimit: Limit;
    Property Limits: Limit read GetLimit;

    procedure SetOlcek(Value: Double);
    function GetOlcek: Double;
    Property Olcek: Double read GetOlcek write SetOlcek;

    procedure SetAci(Value: Double);
    function GetAci: Double;
    Property Aci: Double read GetAci write SetAci;

    procedure SetYazi(Value: String);
    function GetYazi: String;
    Property Yazi: String read GetYazi write SetYazi;

    procedure SetIceGirinti(Value: Single);
    function GetIceGirinti: Single;
    Property IceGirinti: Single read GetIceGirinti write SetIceGirinti;

    function GetYaziUzunlugu: Single;
    Property YaziUzunlugu: Single read GetYaziUzunlugu;

    procedure SetFont(Value: IlicgFontTool);
    function GetFont: IlicgFontTool;
    Property Font: IlicgFontTool read GetFont write SetFont;

    procedure SetDikeyHizalama(Value: DikeyHizalama);
    function GetDikeyHizalama: DikeyHizalama;
    Property DikeyHizalama: DikeyHizalama read GetDikeyHizalama write SetDikeyHizalama;

    procedure SetYatayHizalama(Value: YatayHizalama);
    function GetYatayHizalama: YatayHizalama;
    Property YatayHizalama: YatayHizalama read GetYatayHizalama write SetYatayHizalama;

    procedure SetYaziWrap(Value: Boolean);
    function GetYaziWrap: Boolean;
    Property YaziWrap: Boolean read GetYaziWrap write SetYaziWrap;

    //

    function GetSolUst: Point;
    Property SolUst: Point read GetSolUst;

    function GetSolOrta: Point;
    Property SolOrta: Point read GetSolOrta;

    function GetSagUst: Point;
    Property SagUst: Point read GetSagUst;

    function GetSagOrta: Point;
    Property SagOrta: Point read GetSagOrta;

    function GetSagAlt: Point;
    Property SagAlt: Point read GetSagAlt;

    function GetUstOrta: Point;
    Property UstOrta: Point read GetUstOrta;

    function GetAltOrta: Point;
    Property AltOrta: Point read GetAltOrta;

    function GetOrtaOrta: Point;
    Property OrtaOrta: Point read GetOrtaOrta;

    procedure SetJust(Value: TlicgTextPos);
    function GetJust: TlicgTextPos;
    Property Just: TlicgTextPos read GetJust write SetJust;

    function GetGetNcJust: TlicgTextPos;
    Property GetNcJust: TlicgTextPos read GetGetNcJust;

    function GetGetNcJustNum: TlicgTextPos;
    Property GetNcJustNum: TlicgTextPos read GetGetNcJustNum;

    function GetCellYazi: TStringList;
    Property CellYazi: TStringList read GetCellYazi;

    function GetCellYaziCor: TList<Point>;
    Property CellYaziCor: TList<Point> read GetCellYaziCor;

//    function GetGetTextRecF: TList<RectangleF>;
//    Property GetTextRecF: TList<RectangleF> read GetGetTextRecF;

    procedure SetFill(Value: Boolean);
    function GetFill: Boolean;
    Property Fill: Boolean read GetFill write SetFill;

    procedure SetFillColor(Value: TColor);
    function GetFillColor: TColor;
    Property FillColor: TColor read GetFillColor write SetFillColor;

    procedure SetUstCerceve(Value: Border);
    function GetUstCerceve: Border;
    Property UstCerceve: Border read GetUstCerceve write SetUstCerceve;

    procedure SetAltCerceve(Value: Border);
    function GetAltCerceve: Border;
    Property AltCerceve: Border read GetAltCerceve write SetAltCerceve;

    procedure SetSolCerceve(Value: Border);
    function GetSolCerceve: Border;
    Property SolCerceve: Border read GetSolCerceve write SetSolCerceve;

    procedure SetSagCerceve(Value: Border);
    function GetSagCerceve: Border;
    Property SagCerceve: Border read GetSagCerceve write SetSagCerceve;

    procedure SetStag(Value: String);
    function GetStag: String;
    Property Stag: String read GetStag write SetStag;
    //

//    Const Dikeybosluk: Double = 0.5;
//    Const Yataybosluk: Double = 0.5;

    function GetCorners: ILicgEntity;//Nc_CollectionsOf_PolyPoint
    procedure MoveRectangleDy(Dy: Single);
    procedure MoveRectangleDx(Dx: Single);
//    function GetEnumDescription<TEnum>(enumObj: TEnum): String;
    function GetPosCad(PPos: TextJustify): TlicgTextPos;
    procedure ScrollUpDown(m: Double);
    procedure ScrollLeftReight(m: Double);
    procedure DownToBottomRow;
    procedure GoFirstCorr;
    procedure SetFirstYukseklik;
    function GetCopy(Mode: string = ''): CellRectangle;
    //

    procedure InitVariable;
    constructor Create; overload;
    constructor Create(Adi: String;
                       Yazi: String;
                       Font: IlicgFontTool;
                       YatayHizalama: YatayHizalama;
                       DikeyHizalama: DikeyHizalama;
                       TabakaName: String;
                       YaziWrap: Boolean;
                       IceGirinti: Double;
                       Yukseklik: Double;
                       Genislik: Double;
                       SolAltY: Double;
                       SolAltX: Double;
                       Olcek: Double;
                       Aci: Double; YG: Boolean); overload;
    constructor Create(Adi: String;
                       Yazi: String;
                       Font: IlicgFontTool;
                       DikeyHizalama: DikeyHizalama;
                       YatayHizalama: YatayHizalama;
                       TabakaName: String;
                       YaziWrap: Boolean;
                       IceGirinti: Double;
                       SolAltY: Double;
                       SolAltX: Double;
                       SagUstY: Double;
                       SagUstX: Double;
                       Olcek: Double;
                       Aci: Double; PEnt: IlicgEntity); overload;

    constructor Create(Adi: String;
                       SolAltY: Double;
                       SolAltX: Double;
                       SagUstY: Double;
                       SagUstX: Double;
                       Olcek: Double;
                       Aci: Double); overload;

//     Destructor destroy; override;
    private
      _TabakaAdi: String;
      _YaziTabakaAdi: String;
      _CizgiTabakaAdi: String;
      _Adi: String;
      _Yazi: String;
      _YaziFirst: String;
      _YatayHizalama: YatayHizalama;// = YatayHizalama.YatayOrta;
      _DikeyHizalama: DikeyHizalama;// = DikeyHizalama.YatayOrta;
      _YaziWrap: Boolean;
      _YaziUzunlugu: Double;
      _Genislik: Double;
      _Yukseklik: Double;
      _YukseklikFirst: Double;
      _Olcek: Double;
      _Aci: Double;
      _SolAlt: Point;
      _SolAltFirst: Point;
      _SolUst: Point;
      _SolOrta: Point;
      _SagAlt: Point;
      _SagUst: Point;
      _SagOrta: Point;
      _AltOrta: Point;
      _UstOrta: Point;
      _OrtaOrta: Point;
      _ListOfYaziCor: TList<Point>;
      _Limit: Limit;
      _Font: IlicgFontTool;
      _ListOfYazi: TStringList;
      _Just: TlicgTextPos;//String;
      _IceGirinti: Single;
      _Ust: Border;
      _Alt: Border;
      _Sol: Border;
      _Sag: Border;
      _Fill: Boolean;
      _FillColor: TColor;
      _JustNum: TlicgTextPos;
      _Stag: String;
      _StandartDikeyBosluk: Double;
      _StandartYatayBosluk: Double;

//      _GetTextRecf: TList<RectangleF>;

     procedure Set_Limits(PAci: Double = 0);
     procedure Set_Limits2(PEnt: IlicgEntity);
     procedure Set_Text_new1;
     procedure Set_Text_Cor;
//     procedure SetTextRefPoint;

    function DegreesToRadians(degrees: Double): Double;
    function RadiansToDegrees(radians: Double): Double;
    function Deltay(Cor1, Cor2: Point): Double;
    function Deltax(Cor1, Cor2: Point): Double;
    function Mesafe(Cor1, Cor2: Point): Double;
    function DikAyakDikBoy(DkCor, BkCor: Point; DikAyak, DikBoy: Double): Point;
    function DogrununAcisi(Cor1, Cor2: Point): Double;
//    function IkiNoktadanKesisim(p0: Point; radius0: Double;
//                                p1: Point; radius1: Double;
//                                var intersection1: Point; var intersection2: Point): Integer;
//    function Width(SourceStr: String; PFont: IlicgFontTool): Double;

    procedure _PropertyChanged(info: String); virtual;
    procedure _PropertyChanging(info: String);
    function DikAyakDikBoy2(DkCor, BkCor: Point; DikAyak,
      DikBoy: Double): Point;
//    function PolarPoint(P: Point; PAngle, PDist: Double): Point;
//    procedure Set_Limits3(PAci: Double = 0);
  end;

var // ilker deðiþtirme Const dan Var yaptým.
  Dikeybosluk: Double = 0.5;
  Yataybosluk: Double = 0.5;

implementation

{function StringListFromStrings(const Strings: array of string): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := low(Strings) to high(Strings) do
    Result.Add(Strings[i]);
end;

Function Split(SourceStr: String; Pattern: String; ExplitCapture: Boolean): TStringList;
var
  Mpattern, currStr: String;
  resultList: TStringList;
begin
    If String.IsNullOrEmpty(SourceStr) Or
        String.IsNullOrWhiteSpace(SourceStr) Or
        String.IsNullOrEmpty(Pattern) Then
    begin
      result := nil;
      Exit;// Nothing
    end;
    Mpattern := String.Empty;
    resultList := TStringList.Create;// List(Of String)
//    '  "(\r?\n)" satýr \r=CR \n=LF için satýr baþlarýndan böler Her þartta bölecektir.
    Mpattern := '([' + Pattern + '])';
    If ExplitCapture Then
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roExplicitCapture, TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList
    end
    Else
    begin
        resultList := StringListFromStrings(TRegex.Split(SourceStr, Mpattern, [TRegExOption.roMultiLine]));
        for currStr in resultList do
        begin
          if String.IsNullOrEmpty(currStr) then
            resultList.Delete(resultList.IndexOf(currStr));
        end;
//        .Where(Function(s) Not String.IsNullOrEmpty(s)).ToList;
    end;
    result := resultList;
End;}
{
Function CellRectangle.Width(SourceStr: String; PFont: IlicgFontTool): Double;
var
  TempEnt: IlicgEntity;
begin
  TempEnt := Licad.CreateEntityFactory.MakeEntity(idText,0,_2D);
  TempEnt.AsText.Text := SourceStr;
  TempEnt.DrawTools.FontTool.Height := PFont.Height;
  TempEnt.Geometry.Points.Add(AsCoor(0,0));

//  TempEnt := Licad.CreateEntityFactory.MakeText(AsCoor(0,0),SourceStr,PFont.Height,0);
  result := Abs(TempEnt.Geometry.Extent.LowerLeft.X - TempEnt.Geometry.Extent.UpperRight.X);
//    Using string_format As New StringFormat(StringFormat.GenericTypographic)
//        string_format.Alignment = StringAlignment.Near
//        string_format.LineAlignment = StringAlignment.Near
//        string_format.Trimming = StringTrimming.None
//        string_format.FormatFlags = StringFormatFlags.MeasureTrailingSpaces
//        Using g As Graphics = Graphics.FromImage(New Bitmap(1, 1))
//            Return g.MeasureString("|" & SourceStr & "|", Font, 10000, string_format).Width - _
//                   g.MeasureString("|", Font, PointF.Empty, string_format).Width
//        End Using
//    End Using

End;
}
//Public Event OnPropertyChanging(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangingEventArgs) Implements System.ComponentModel.INotifyPropertyChanging.PropertyChanging
//        <System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>
procedure CellRectangle._PropertyChanging(info: String);
begin
//  FOnChangedProperty
//  RaiseEvent OnPropertyChanging(Me, New PropertyChangingEventArgs(info))
end;

//Public Event OnPropertyChanged(ByVal sender As Object, ByVal e As System.ComponentModel.PropertyChangedEventArgs) Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged
//        <System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>
procedure CellRectangle._PropertyChanged(info: String);
var
  ffont: IlicgFontTool;
begin
  case AnsiIndexStr(info, ['Aci', 'Genislik', 'Yukseklik', 'SolAlt',
                           'Olcek',
                           'Limit', 'Yazi', 'Font', 'IceGirinti', 'YaziWrap',
                           'DikeyHizalama', 'YatayHizalama']) of
    0, 1, 2, 3:
    begin
      Set_Limits;
      Set_Text_new1;
      Set_Text_Cor;
    end;
    4:
    begin
      Self._Genislik := Self._Genislik * Olcek;
      Self._Yukseklik := Self._Yukseklik * Olcek;
      Self._IceGirinti := Self._IceGirinti * Olcek;
      Self._StandartYatayBosluk := Self._StandartYatayBosluk * Olcek;
      Self._StandartDikeyBosluk := Self._StandartDikeyBosluk * Olcek;
//      ffont := IlicgFontTool.Create;
      ffont := Licad.Settings.FontTool;
      ffont.Name := Self._Font.Name;
      ffont.Height := (Self._Font.Height * Olcek); //Trunc
      ffont.Style := Self._Font.Style;
      Self._Font := ffont;
      Self._Font.Assign(ffont);//New RFont(Me._Font.Name, CSng(Me._Font.Size * Olcek), Me._Font.Style);
      Set_Limits;
    end;
    5, 6, 7, 8, 9:
    begin
      Set_Text_new1;
      Set_Limits;
      Set_Text_Cor;
    end;
    10, 11:
    begin
      Set_Text_Cor;
    end;
  end;

//  if Assigned(FOnChangedProperty) then
//    FOnChangedProperty(Self);
//  RaiseEvent OnPropertyChanged(Me, New PropertyChangedEventArgs(info))
end;

function RealMod (x, y : extended) : extended;
begin
   Result := x - y * Trunc(x/y);
end;

Function GetTextMetrics(hdc: IntPtr; var lptm: TEXTMETRIC): Boolean;
external 'gdi32.dll';

Function SelectObject(hdc: IntPtr; hObj: IntPtr): IntPtr;
external 'gdi32.dll';

Function DeleteObject(hObject: IntPtr): Boolean;
external 'gdi32.dll';

{ CellRectangle }

constructor CellRectangle.Create(Adi: String; SolAltY, SolAltX, SagUstY,
  SagUstX, Olcek, Aci: Double);
var
  Radyanacim, mesafem: Double;
  P1, P2: Point;
  F: IlicgFontTool;
begin
  InitVariable;
  Self._Adi := Adi;//'XX';
  Self._Yazi := Adi;
//  F := TFont.Create;
  f := Licad.Settings.FontTool;
  F.Name := 'Arial';
  F.Height := 1;
  Self._Font := F;
  Self._Font.Assign(F);//New Asistan.CadObj.RFont('Arial', 1);
  Self._DikeyHizalama := Lider.CG.ModulesCom.Table.CellRectangle.DikeyHizalama.DikeyOrta;
  Self._YatayHizalama := Lider.CG.ModulesCom.Table.CellRectangle.YatayHizalama.YatayOrta;
  Self._YaziWrap := False;
  Self._IceGirinti := 0;
  Self._TabakaAdi := '0';
  Self._YaziTabakaAdi := '0';
  Self._CizgiTabakaAdi := '0';

  p1 := Point.Create;
  p2 := Point.Create;
  p1.Y := SolAltY;
  p1.X := SolAltX;
  p2.Y := SagUstY;
  p2.X := SagUstX;
  Radyanacim := DogrununAcisi(p1,p2);//New Point With {.Y := SolAltY, .X := SolAltX}, New Point With {.Y := SagUstY, .X := SagUstX});
  mesafem := Mesafe(p1,p2);//New Point With {.Y := SolAltY, .X := SolAltX}, New Point With {.Y := SagUstY, .X := SagUstX});
  Self._Yukseklik := Sin(Radyanacim) * mesafem;
  Self._Genislik := Cos(Radyanacim) * mesafem;

  Self._SolAlt.Y := SolAltY;
  Self._SolAlt.X := SolAltX;

  Self._YaziFirst := Yazi;
  Self._SolAltFirst.Y := SolAltY;
  Self._SolAltFirst.X := SolAltX;
  Self._YukseklikFirst := _Yukseklik;

  If Olcek = 0.0 Then Olcek := 1;
  Self._Olcek := Olcek; //_olcek
  Self._Aci := Aci;
  Self._Sol.Draw := False;
  Self._Sol.LineIndex := 0;
  Self._Sag.Draw := False;
  Self._Sag.LineIndex := 0;
  Self._Ust.Draw := False;
  Self._Ust.LineIndex := 0;
  Self._Alt.Draw := False;
  Self._Alt.LineIndex := 0;
  Self._Fill := True;
  Self._FillColor := clBlue;
  Self._StandartDikeyBosluk := ((Dikeybosluk / 12) * Olcek) * Self._Font.Height;
  Self._StandartYatayBosluk := ((Yataybosluk / 12) * Olcek) * Self._Font.Height;
  Set_Limits;

  FreeAndNil(p1);
  FreeAndNil(p2);
end;

constructor CellRectangle.Create(Adi, Yazi: String; Font: IlicgFontTool;
  DikeyHizalama: DikeyHizalama; YatayHizalama: YatayHizalama;
  TabakaName: String; YaziWrap: Boolean; IceGirinti, SolAltY, SolAltX, SagUstY,
  SagUstX, Olcek, Aci: Double; PEnt: IlicgEntity);
  var
    Radyanacim, mesafem: Double;
//    P1, P2: Point;
    tempent: IlicgEntity;
    TempCoor: Point;
begin
//  Aci := 0;
  InitVariable;
  Self._Adi := Adi;
  Self._Yazi := Yazi;
  Self._Font := Font;
  Self._Font.Assign(Font);
  Self._DikeyHizalama := DikeyHizalama;
  Self._YatayHizalama := YatayHizalama;
  Self._YaziWrap := YaziWrap;
  Self._IceGirinti := IceGirinti;
  Self._TabakaAdi := TabakaName;
  Self._YaziTabakaAdi := TabakaName;
  Self._CizgiTabakaAdi := TabakaName;

  {
  if (Aci <> 0) and (SolAltX > SagUstX) then
  begin
    SolAltY := PEnt.Geometry.Points[1].Y;
    SolAltX := PEnt.Geometry.Points[1].X;
    SagUstY := PEnt.Geometry.Points[3].Y;
    SagUstX := PEnt.Geometry.Points[3].X;
    if (SolAltX > SagUstX) then
    begin
      SolAltY := PEnt.Geometry.Points[3].Y;
      SolAltX := PEnt.Geometry.Points[3].X;
      SagUstY := PEnt.Geometry.Points[1].Y;
      SagUstX := PEnt.Geometry.Points[1].X;
    end;
  end;
  }
//  p1 := Point.Create;
//  p2 := Point.Create;
//  p1.Y := SolAltY;
//  p1.X := SolAltX;
//  p2.Y := SagUstY;
//  p2.X := SagUstX;

  {
  if Aci <> 0 then
  begin
    tempent := licad.CreateEntityFactory.MakeEntity(idline,2,_2d);
    tempent.Geometry.Points.Add(p1.x,p1.y);
    tempent.Geometry.Points.Add(p2.x,p2.y);
    Currcmdline.ActiveDrawBox.GIS.CurrentLayer.AddEntity(tempent);
  end;
  }
  {
  if (p1.X > p2.X) then
  begin
    TempCoor := Point.Create;
    TempCoor := p1;
    p1 := p2;
    p2 := TempCoor;
  end;
  }
//  Radyanacim := DogrununAcisi(p1,p2);//New Point With {.Y := SolAltY, .X := SolAltX}, New Point With {.Y := SagUstY, .X := SagUstX});
//  mesafem := Mesafe(p1,p2);//New Point With {.Y := SolAltY, .X := SolAltX}, New Point With {.Y := SagUstY, .X := SagUstX});
//  Self._Yukseklik := Abs(Cos(Radyanacim) * mesafem); //Sin
//  Self._Genislik := Abs(Sin(Radyanacim) * mesafem); //Cos

//    Dim Radyanacim As Double = DogrununAcisi(New Point With {.Y = SolAltY, .X = SolAltX}, New Point With {.Y = SagUstY, .X = SagUstX})
//    Dim mesafem = Mesafe(New Point With {.Y = SolAltY, .X = SolAltX}, New Point With {.Y = SagUstY, .X = SagUstX})
//    Me._Yukseklik = Math.Sin(Radyanacim) * mesafem
//    Me._Genislik = Math.Cos(Radyanacim) * mesafem
  Self._Yukseklik := _Distance(PEnt.Geometry.DrawPoints[1],PEnt.Geometry.DrawPoints[2]);
  Self._Genislik := _Distance(PEnt.Geometry.DrawPoints[0],PEnt.Geometry.DrawPoints[1]);

  Self._SolAlt.Y := SolAltY;
  Self._SolAlt.X := SolAltX;

  Self._YaziFirst := Yazi;
  Self._SolAltFirst.Y := SolAltY;
  Self._SolAltFirst.X := SolAltX;
  Self._YukseklikFirst := _Yukseklik;

  If Olcek = 0.0 Then Olcek := 1;
  Self._Olcek := Olcek; //_olcek
  Self._Aci := Aci;
  Self._Sol.Draw := False;
  Self._Sol.LineIndex := 0;
  Self._Sag.Draw := False;
  Self._Sag.LineIndex := 0;
  Self._Ust.Draw := False;
  Self._Ust.LineIndex := 0;
  Self._Alt.Draw := False;
  Self._Alt.LineIndex := 0;
  Self._Fill := True;
  Self._FillColor := clBlue;
  Self._StandartDikeyBosluk := ((Dikeybosluk / 12) * Olcek) * Self._Font.Height;
  Self._StandartYatayBosluk := ((Yataybosluk / 12) * Olcek) * Self._Font.Height;

//  Set_Limits2(Pent);
  Set_Limits;
end;

constructor CellRectangle.Create(Adi, Yazi: String; Font: IlicgFontTool;
  YatayHizalama: YatayHizalama; DikeyHizalama: DikeyHizalama;
  TabakaName: String; YaziWrap: Boolean; IceGirinti, Yukseklik, Genislik,
  SolAltY, SolAltX, Olcek, Aci: Double; YG: Boolean);
begin
  InitVariable;
  Self._Adi := Adi;
  Self._Yazi := Yazi;
  Self._Font := Font;
  Self._Font.Assign(Font);
  Self._DikeyHizalama := DikeyHizalama;
  Self._YatayHizalama := YatayHizalama;
  Self._YaziWrap := YaziWrap;
  Self._IceGirinti := IceGirinti;
  Self._TabakaAdi := TabakaName;
  Self._YaziTabakaAdi := TabakaName;
  Self._CizgiTabakaAdi := TabakaName;

  Self._Yukseklik := Yukseklik;
  Self._Genislik := Genislik;
  Self._SolAlt.Y := SolAltY;
  Self._SolAlt.X := SolAltX;

  Self._YaziFirst := Yazi;
  Self._SolAltFirst.Y := SolAltY;
  Self._SolAltFirst.X := SolAltX;
  Self._YukseklikFirst := _Yukseklik;


  If Olcek = 0.0 Then Olcek := 1;
  Self._Olcek := Olcek; //_olcek
  Self._Aci := Aci;
  Self._Sol.Draw := False;
  Self._Sol.LineIndex := 0;
  Self._Sag.Draw := False;
  Self._Sag.LineIndex := 0;
  Self._Ust.Draw := False;
  Self._Ust.LineIndex := 0;
  Self._Alt.Draw := False;
  Self._Alt.LineIndex := 0;
  Self._Fill := True;
  Self._FillColor := clBlue;
  Self._StandartDikeyBosluk := ((Dikeybosluk / 12) * Olcek) * Self._Font.Height;
  Self._StandartYatayBosluk := ((Yataybosluk / 12) * Olcek) * Self._Font.Height;
  Set_Limits;
end;

constructor CellRectangle.Create;
begin
  InitVariable;
  //Boþ
end;

function CellRectangle.DegreesToRadians(degrees: Double): Double;
begin
  result := degrees * PI / 180;
end;

function CellRectangle.Deltax(Cor1, Cor2: Point): Double;
begin
  result := Cor2.X - Cor1.X;
end;

function CellRectangle.Deltay(Cor1, Cor2: Point): Double;
begin
  result := Cor2.Y - Cor1.Y;
end;
{
destructor CellRectangle.destroy;
begin
  _SolAlt.Free;
  _SolAltFirst.Free;
  _SolUst.Free;
  _SolOrta.Free;
  _SagAlt.Free;
  _SagUst.Free;
  _SagOrta.Free;
  _AltOrta.Free;
  _UstOrta.Free;
  _OrtaOrta.Free;
//  _ListOfYaziCor.Clear;
//  _ListOfYaziCor.Free;
//  for i := 0 to _ListOfYaziCor.Count - 1 do
//    TObject(_ListOfYaziCor[i]).Free;
  _Limit.Free;
  _ListOfYazi.Free;
  inherited;
end;
}
function CellRectangle.DikAyakDikBoy(DkCor, BkCor: Point; DikAyak,
  DikBoy: Double): Point;
var
  Xx, Yy, H, I, Deltayy, Deltaxx: Double;
//  TempDikAyakDikBoy: Point;
  tempEnt: IlicgEntity;
begin
//  Dim Xx, Yy, H, I, Deltayy, Deltaxx
//  Result := Point.Create;
//  Result.X := dkCor.X;
//  Result.Y := dkCor.Y;
//  exit;
//  tempent := Licad.CreateEntityFactory.MakeEntity(idline,2,_2D);
//  tempent.Geometry.Points.Add(dkcor.X,dkcor.Y);
//  tempent.Geometry.Points.Add(BkCor.X,BkCor.Y);
//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(tempent);
  result := TempPoint;//Point.Create;

//  H := Deltax(DkCor, BkCor) / Mesafe(DkCor, BkCor);
//  I := Deltay(DkCor, BkCor) / Mesafe(DkCor, BkCor);
//  Yy := DkCor.y + ((I * DikAyak) - (H * DikBoy));
//  Xx := DkCor.x + ((H * DikAyak) + (I * DikBoy));

  H := 0;
  I := 0;
  if Mesafe(DkCor, BkCor) <> 0 then
  begin
    H := Deltay(DkCor, BkCor) / Mesafe(DkCor, BkCor);
    I := Deltax(DkCor, BkCor) / Mesafe(DkCor, BkCor);
  end;

//  Yy := DkCor.Y + ((H * DikAyak) + (I * DikBoy));
//  Xx := DkCor.X + ((I * DikAyak) - (H * DikBoy));
  Yy := DkCor.Y;// + ((H * DikAyak) - (I * DikBoy));
  Xx := DkCor.X;// + ((I * DikAyak) + (H * DikBoy));

  Deltayy := Yy - DkCor.Y;
  Deltaxx := Xx - DkCor.X;

  result.Y := Yy; //y
  result.X := Xx; //x

//  result := TempDikAyakDikBoy;
end;
{
function CellRectangle.PolarPoint(P: Point; PAngle: Double; PDist: Double): Point;
var
  resultCoor: TlicgCoor;
begin
  resultCoor := Polar(AsCoor(P.X,P.Y),PAngle,PDist);
  Result := Point.Create;
  Result.X := resultCoor.X;
  Result.Y := resultCoor.Y;
end;
}
function CellRectangle.DikAyakDikBoy2(DkCor, BkCor: Point; DikAyak,
  DikBoy: Double): Point;
var
  Xx, Yy, H, I, Deltayy, Deltaxx: Double;
//  TempDikAyakDikBoy: Point;
  tempEnt: IlicgEntity;
begin
//  Result := DikAyakDikBoy(DkCor,BkCor,DikAyak,DikBoy);
//  Exit;
  result := TempPoint;//.Create;

  H := 0;
  I := 0;
  if Mesafe(DkCor, BkCor) <> 0 then
  begin
    H := Deltax(DkCor, BkCor) / Mesafe(DkCor, BkCor);
    I := Deltay(DkCor, BkCor) / Mesafe(DkCor, BkCor);
  end;

//  Yy := DkCor.Y + ((H * DikAyak) + (I * DikBoy));
//  Xx := DkCor.X + ((I * DikAyak) - (H * DikBoy));

  Yy := DkCor.Y + ((I * DikAyak) - (H * DikBoy));
  Xx := DkCor.X + ((H * DikAyak) + (I * DikBoy));

//  Yy := DkCor.Y + ((I * DikBoy) - (H * DikAyak));
//  Xx := DkCor.X + ((I * DikBoy) + (H * DikAyak));

  Deltayy := Yy - DkCor.Y;
  Deltaxx := Xx - DkCor.X;

  result.Y := Yy; //y
  result.X := Xx; //x

//  result := TempDikAyakDikBoy;
end;

function CellRectangle.DogrununAcisi(Cor1, Cor2: Point): Double;
var
  rslt: Double;
begin
  rslt := Double.Nan;
  if ((Cor1.Y - Cor2.Y) <> 0) and ((Cor1.X - Cor2.X) <> 0) then
    rslt := DegToRad(Sqrt(Power(Cor2.X-Cor1.X,2)+Power(Cor2.Y-Cor1.Y,2)));
//    rslt := Arctan(Deltax(Cor1, Cor2) / Deltay(Cor1, Cor2));
  result := rslt;
end;

procedure CellRectangle.DownToBottomRow;
var
  X, Y: Double;
  A: Point;
begin
  Yukseklik := _YukseklikFirst;
  X := Yukseklik * Cos(PI * (RadiansToDegrees(Aci) + 180) / 180.0);
  Y := Yukseklik * -Sin(PI * (RadiansToDegrees(Aci) + 180) / 180.0);
//  A := Point.create;
//  A.X := X + _SolAlt.X;
//  A.Y := Y + _SolAlt.Y;
//  SolAlt := A;
  TempPoint.X := X + _SolAlt.X;
  TempPoint.Y := Y + _SolAlt.Y;
  if TempPoint <> SolAlt then
  begin
    _SolAlt.X := TempPoint.X;
    _SolAlt.Y := TempPoint.Y;
    Self._PropertyChanged('SolAlt');
  end;
end;

function CellRectangle.GetAci: Double;
begin
  result := _Aci;
end;

function CellRectangle.GetAdi: String;
begin
  result := _Adi;
end;

function CellRectangle.GetAltCerceve: Border;
begin
  result := _Alt;
end;

function CellRectangle.GetAltOrta: Point;
begin
  result := _AltOrta;
end;

function CellRectangle.GetCellYazi: TStringList;
begin
  result := _ListOfYazi;
end;

function CellRectangle.GetCellYaziCor: TList<Point>;
begin
  result := _ListOfYaziCor;
end;

function CellRectangle.GetCizgiTabakaAdi: String;
begin
  If Not String.IsNullOrEmpty(_CizgiTabakaAdi) Then
      result := _CizgiTabakaAdi
  Else
      result := _TabakaAdi;
end;

function CellRectangle.GetCopy(Mode: string = ''): CellRectangle;
begin
  result := CellRectangle.Create;
//  rslt := Self;//CellRectangle(Self.ClassType).Create;//Assign(Self);// := HCloner.DeepCopy(Self);
  result._TabakaAdi := Self.TabakaAdi;
  result._YaziTabakaAdi := Self.YaziTabakaAdi;
  result._CizgiTabakaAdi := Self.CizgiTabakaAdi;
  result._Adi := Self.Adi;
  result._Genislik := Self.Genislik;
  result._Yukseklik := Self.Yukseklik;

  result._Olcek := Self.Olcek;
  result._Aci := Self.Aci;

  if mode = 'yazi' then
    result._Yazi := '##'//'??'
  else
    result._Yazi := Self.Yazi;

  result._IceGirinti := Self.IceGirinti;
  result._Font := Self.Font;
  result._DikeyHizalama := Self.DikeyHizalama;
  result._YatayHizalama := Self.YatayHizalama;
  result._YaziWrap := Self.YaziWrap;
  result._Just := Self.Just;
  result._JustNum := Self._JustNum;
  result._Fill := Self.Fill;
  result._FillColor := Self.FillColor;
  result._Ust := Self.UstCerceve;
  result._Alt := Self.AltCerceve;
  result._Sol := Self.SolCerceve;
  result._Sag := Self.SagCerceve;
  result._Stag := Self.Stag;
  result._YaziFirst := Self._YaziFirst;
  result._YaziUzunlugu := Self.YaziUzunlugu;
  result._YukseklikFirst := Self._YukseklikFirst;

  result._SolAlt.Y := Self._SolAlt.Y;
  result._SolAlt.X := Self._SolAlt.X;
  result._SolAltFirst.Y := Self._SolAltFirst.Y;
  result._SolAltFirst.X := Self._SolAltFirst.X;
  result._SolUst.Y := Self._SolUst.Y;
  result._SolUst.X := Self._SolUst.X;
  result._SolOrta.Y := Self._SolOrta.Y;
  result._SolOrta.X := Self._SolOrta.X;
  result._SagAlt.Y := Self._SagAlt.Y;
  result._SagAlt.X := Self._SagAlt.X;
  result._SagUst.Y := Self._SagUst.Y;
  result._SagUst.X := Self._SagUst.X;
  result._SagOrta.Y := Self._SagOrta.Y;
  result._SagOrta.X := Self._SagOrta.X;
  result._AltOrta.Y := Self._AltOrta.Y;
  result._AltOrta.X := Self._AltOrta.X;
  result._UstOrta.Y := Self._UstOrta.Y;
  result._UstOrta.X := Self._UstOrta.X;
  result._OrtaOrta.Y := Self._OrtaOrta.Y;
  result._OrtaOrta.X := Self._OrtaOrta.X;

  result._ListOfYaziCor := Self._ListOfYaziCor;
  result._ListOfYazi := Self._ListOfYazi;
  result._Limit.cll.Y := Self._Limit.cll.Y;
  result._Limit.cll.X := Self._Limit.cll.X;
  result._Limit.cur.Y := Self._Limit.cur.Y;
  result._Limit.cur.X := Self._Limit.cur.X;
  result._StandartDikeyBosluk := Self._StandartDikeyBosluk;
  result._StandartYatayBosluk := Self._StandartYatayBosluk;
//  result := rslt;
end;

function CellRectangle.GetCorners: ILicgEntity;
begin
  result := Licad.createEntityFactory.makeEntity(idpolygon,0,_2D);
//  Dim a As New Asistan.CadCollection.Nc_CollectionsOf_PolyPoint
//  a.Geometry.Points.Add(SolAlt.X, SolAlt.Y);//New NCCoor_Only_Pline(SolAlt.X, SolAlt.Y, 0));
//  a.Geometry.Points.Add(_SolUst.X, _SolUst.Y);//New NCCoor_Only_Pline(_SolUst.X, _SolUst.Y, 0));
//  a.Geometry.Points.Add(_SagUst.X, _SagUst.Y);//New NCCoor_Only_Pline(_SagUst.X, _SagUst.Y, 0));
//  a.Geometry.Points.Add(_SagAlt.X, _SagAlt.Y);//New NCCoor_Only_Pline(_SagAlt.X, _SagAlt.Y, 0));
//  a.Geometry.Points.Add(SolAlt.X, SolAlt.Y);//New NCCoor_Only_Pline(SolAlt.X, SolAlt.Y, 0));
  result.Geometry.Points.Add(SolAlt.X, SolAlt.Y);
  result.Geometry.Points.Add(_SagAlt.X, _SagAlt.Y);
  result.Geometry.Points.Add(_SagUst.X, _SagUst.Y);
  result.Geometry.Points.Add(_SolUst.X, _SolUst.Y);
  result.Geometry.Points.Add(SolAlt.X, SolAlt.Y);
//  result := a;
end;

function CellRectangle.GetDikeyHizalama: DikeyHizalama;
begin
  result := _DikeyHizalama;
end;

{
function CellRectangle.GetEnumDescription<TEnum>(enumObj: TEnum): String;
begin
  fi As FieldInfo = enumObj.GetType.GetField(enumObj.ToString);

  attributes As DescriptionAttribute = fi.GetCustomAttributes(GetType(DescriptionAttribute), False);

  If (attributes <> nil) And (attributes.Length > 0) Then
      result := attributes(0).Description
  Else
      result := enumObj.ToString;
end;
}
function CellRectangle.GetPosCad(PPos: TextJustify): TlicgTextPos;
var
  PosArray: TStringList;
begin
  PosArray := TStringList.Create;
  PosArray.AddObject(IntToStr(Integer(TextJustify.Solalt)), TObject(tpLowerLeft));//("L")> Solalt
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaAlt)), TObject(tpCenterDown));//("C")> OrtaAlt
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagAlt)), TObject(tpLowerRight));//("R")> SagAlt
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolOrta)), TObject(tpCenterLeft));//("1")> SolOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaOrta)), TObject(tpCenter));//("M")> OrtaOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagOrta)), TObject(tpCenterRight));//("5")> SagOrta
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolUst)), TObject(tpUpperLeft));//("2")> SolUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaUst)), TObject(tpCenterUp));//("3")> OrtaUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagUst)), TObject(tpUpperRight));//("4")> SagUst
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolOrtaDis)), TObject(tpCenterLeftOut));//("P")> SolOrtaDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagOrtaDis)), TObject(tpCenterRightOut));//("Q")> SagOrtaDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SolAltDis)), TObject(tpLowerLeftOutDown));//("6")> SolAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.OrtaAltDis)), TObject(tpCenterDownOut));//("7")> OrtaAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.SagAltDis)), TObject(tpLowerRightOutDown));//("8")> SagAltDis
  PosArray.AddObject(IntToStr(Integer(TextJustify.UstOrtaDis)), TObject(tpCenterUpOut));//("U")> UstOrtaDis
  Result := TlicgTextPos(PosArray.Objects[PosArray.IndexOf(IntToStr(Integer(PPos)))]);
  FreeAndNil(PosArray);
end;

function CellRectangle.GetFill: Boolean;
begin
  result := _Fill;
end;

function CellRectangle.GetFillColor: TColor;
begin
  result := _FillColor;
end;

function CellRectangle.GetFont: IlicgFontTool;
begin
  result := _Font;
end;

function CellRectangle.GetGenislik: Double;
begin
  result := _Genislik;
end;

function CellRectangle.GetGetNcJust: TlicgTextPos;
begin
  result := _Just;
end;

function CellRectangle.GetGetNcJustNum: TlicgTextPos;
begin
  result := _JustNum;
end;

function CellRectangle.GetIceGirinti: Single;
begin
  result := _IceGirinti;
end;

function CellRectangle.GetJust: TlicgTextPos;
begin
  result := _Just;
end;

function CellRectangle.GetLimit: Limit;
begin
  result := _Limit;
end;

function CellRectangle.GetOlcek: Double;
begin
  result := _Olcek;
end;

function CellRectangle.GetOrtaOrta: Point;
begin
  result := _OrtaOrta;
end;

function CellRectangle.GetSagAlt: Point;
begin
  result := _SagAlt;
end;

function CellRectangle.GetSagCerceve: Border;
begin
  result := _Sag;
end;

function CellRectangle.GetSagOrta: Point;
begin
  result := _SagOrta;
end;

function CellRectangle.GetSagUst: Point;
begin
  result := _SagUst;
end;

function CellRectangle.GetSolAlt: Point;
begin
  result := _SolAlt;
end;

function CellRectangle.GetSolCerceve: Border;
begin
  result := _Sol;
end;

function CellRectangle.GetSolOrta: Point;
begin
  result := _SolOrta;
end;

function CellRectangle.GetSolUst: Point;
begin
  result := _SolUst;
end;

function CellRectangle.GetStag: String;
begin
  result := _Stag;
end;

function CellRectangle.GetTabakaAdi: String;
begin
  result := _TabakaAdi;
end;

function CellRectangle.GetUstCerceve: Border;
begin
  result := _Ust;
end;

function CellRectangle.GetUstOrta: Point;
begin
  result := _UstOrta;
end;

function CellRectangle.GetYatayHizalama: YatayHizalama;
begin
  result := _YatayHizalama;
end;

function CellRectangle.GetYazi: String;
begin
  result := _Yazi;
end;

function CellRectangle.GetYaziTabakaAdi: String;
begin
  If Not String.IsNullOrEmpty(_YaziTabakaAdi) Then
      result := _YaziTabakaAdi
  Else
      result := _TabakaAdi;
end;

function CellRectangle.GetYaziUzunlugu: Single;
begin
  result := _YaziUzunlugu;
end;

function CellRectangle.GetYaziWrap: Boolean;
begin
  result := _YaziWrap;
end;

function CellRectangle.GetYukseklik: Double;
begin
  result := _Yukseklik;
end;

procedure CellRectangle.GoFirstCorr;
var
  A: Point;
begin
  Yukseklik := _YukseklikFirst;
//  A := Point.create;
//  A.X := _SolAltFirst.X;
//  A.Y := _SolAltFirst.Y;
//  SolAlt := A;
  TempPoint.X := _SolAltFirst.X;
  TempPoint.Y := _SolAltFirst.Y;
  if TempPoint <> SolAlt then
  begin
    _SolAlt.X := TempPoint.X;
    _SolAlt.Y := TempPoint.Y;
    Self._PropertyChanged('SolAlt');
  end;
//  Yazi := _YaziFirst;
end;

{function CellRectangle.IkiNoktadanKesisim(p0: Point; radius0: Double; p1: Point;
  radius1: Double; var intersection1, intersection2: Point): Integer;
var
  dx, dy, dist, a, h, cx2, cy2: Double;
//  intersection1, intersection2: Point;
begin
  dx := p0.X - p1.X;
  dy := p0.Y - p1.Y;
  dist := Sqrt(dx * dx + dy * dy);

//  ' See how many solutions there are.
  intersection1 := Point.Create;
  intersection2 := Point.Create;
  If (dist > (radius0 + radius1)) Then
  begin
//      ' No solutions, the circles are too far apart.
      intersection1.X := nan;// = New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection1.Y := nan;
      intersection2.X := nan;//= New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection2.Y := nan;
      result := 0;
      Exit;
  end
  Else If (dist < Abs(radius0 - radius1)) Then
  begin
//      ' No solutions, one circle contains the other.
//      intersection1 = New Point With (.X = Double.NaN, .Y = Double.NaN)
//      intersection2 = New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection1.X := nan;// = New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection1.Y := nan;
      intersection2.X := nan;//= New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection2.Y := nan;
      result := 0;
      Exit;
  end
  Else If ((dist = 0) And (radius0 = radius1)) Then
  begin
//      ' No solutions, the circles coincide.
//      intersection1 = New Point With (.X = Double.NaN, .Y = Double.NaN)
//      intersection2 = New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection1.X := nan;// = New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection1.Y := nan;
      intersection2.X := nan;//= New Point With (.X = Double.NaN, .Y = Double.NaN)
      intersection2.Y := nan;
      result := 0;
      Exit;
  end
  Else
  begin
//      ' Find a and h.
      a := (radius0 * radius0 - radius1 * radius1 + dist * dist) / (2 * dist);
      h := Sqrt(radius0 * radius0 - a * a);

//      ' Find P2.
      cx2 := p0.X + a * (p1.X - p0.X) / dist;
      cy2 := p0.Y + a * (p1.Y - p0.Y) / dist;

//      ' Get the points P3.
      intersection1.X := (cx2 + h * (p1.Y - p0.Y) / dist);//= New Point With (.X = (cx2 + h * (p1.Y - p0.Y) / dist), .Y = (cy2 - h * (p1.X - p0.X) / dist))
      intersection1.Y := (cy2 - h * (p1.X - p0.X) / dist);
      intersection2.X := (cx2 - h * (p1.Y - p0.Y) / dist);// = New Point With (.X = (cx2 - h * (p1.Y - p0.Y) / dist), .Y = (cy2 + h * (p1.X - p0.X) / dist))
      intersection2.Y := (cy2 + h * (p1.X - p0.X) / dist);
//      ' See if we have 1 or 2 solutions.
      If (dist = (radius0 + radius1)) Then
      begin
        result := 1;
        Exit;
      end;
      result := 2;
      Exit;
  End;
end;
}
procedure CellRectangle.InitVariable;
begin
  TempPoint := Point.Create;
  TempPoint2 := Point.Create;
  TempPoint3 := Point.Create;
  _SolAlt := Point.Create;
  _SolAltFirst := Point.Create;
  _SolUst := Point.Create;
  _SolOrta := Point.Create;
  _SagAlt := Point.Create;
  _SagUst := Point.Create;
  _SagOrta := Point.Create;
  _AltOrta := Point.Create;
  _UstOrta := Point.Create;
  _OrtaOrta := Point.Create;
  _ListOfYaziCor := TList<Point>.Create;

  _Limit := Limit.Create;
  _Font := Licad.Settings.FontTool;
  _ListOfYazi := TStringList.Create;
//  _Just := TlicgTextPos.tpLowerLeft;//String;
  _Ust := Border.Create;
  _Alt := Border.Create;
  _Sol := Border.Create;
  _Sag := Border.Create;
end;

function CellRectangle.Mesafe(Cor1, Cor2: Point): Double;
var
  rslt: Double;
begin
  rslt := nan;
  rslt := Sqrt(Power(Deltay(Cor1, Cor2), 2) + Power(Deltax(Cor1, Cor2), 2));
  result := rslt;
end;

procedure CellRectangle.MoveRectangleDx(Dx: Single);
begin
  Self._PropertyChanging('SolAlt');
  Self.SolAlt.X := Self.SolAlt.X + Dx;
  Self._PropertyChanged('SolAlt');
end;

procedure CellRectangle.MoveRectangleDy(Dy: Single);
begin
  Self._PropertyChanging('SolAlt');
  Self.SolAlt.Y := Self.SolAlt.Y + Dy;
  Self._PropertyChanged('SolAlt');
end;

function CellRectangle.RadiansToDegrees(radians: Double): Double;
begin
  result := radians * 180 / PI;
end;

procedure CellRectangle.ScrollLeftReight(m: Double);
var
  X, Y: Double;
  A: Point;
begin
  if m<>0 then
    x := 0;
  Y := m * Cos(PI * (RadiansToDegrees(Aci) + 270) / 180.0);
  X := m * -Sin(PI * (RadiansToDegrees(Aci) + 270) / 180.0);
//  A := Point.Create;
//  A.X := _SolAlt.X + X; //ismet deðiþtirildi.
//  A.Y := _SolAlt.Y + Y; //ismet deðiþtirildi.
//  SolAlt := A;
  TempPoint.X := _SolAlt.X + X;
  TempPoint.Y := _SolAlt.Y + Y;
  if TempPoint <> SolAlt then
  begin
    _SolAlt.X := TempPoint.X;
    _SolAlt.Y := TempPoint.Y;
    Self._PropertyChanged('SolAlt');
  end;
end;

procedure CellRectangle.ScrollUpDown(m: Double);
var
  X, Y: Double;
  a: Point;
begin
  if m<>0 then
    x := 0;
  Y := m * Cos(PI * (RadiansToDegrees(Aci) + 180) / 180.0);
  X := m * -Sin(PI * (RadiansToDegrees(Aci) + 180) / 180.0);
//  A := Point.create;
//  A.X := _SolAlt.X + X; //ismet deðiþtirildi.
//  A.Y := _SolAlt.Y + Y; //ismet deðiþtirildi.
//  SolAlt := A; //Hatalý aktarým aþaðýya
  TempPoint.X := _SolAlt.X + X;
  TempPoint.Y := _SolAlt.Y + Y;
  if TempPoint <> SolAlt then
  begin
    _SolAlt.X := TempPoint.X;
    _SolAlt.Y := TempPoint.Y;
    Self._PropertyChanged('SolAlt');
  end;
end;

procedure CellRectangle.SetAci(Value: Double);
begin
  Self._PropertyChanging('Aci');
  If value <> _Aci Then
  begin
      _Aci := value;
      Self._PropertyChanged('Aci');
  End;
end;

procedure CellRectangle.SetAdi(Value: String);
begin
  Self._PropertyChanging('Adi');
  If value <> _Adi Then
  begin
      _Adi := value;
      Self._PropertyChanged('Adi');
  End;
end;

procedure CellRectangle.SetAltCerceve(Value: Border);
begin
  Self._PropertyChanging('AltCerceve');
  If value <> _Alt Then
  begin
      _Alt := value;
      Self._PropertyChanged('AltCerceve');
  End;
end;

procedure CellRectangle.SetCizgiTabakaAdi(Value: String);
begin
  Self._PropertyChanging('CizgiTabakaAdi');
  If value <> _CizgiTabakaAdi Then
  begin
      _CizgiTabakaAdi := value;
      Self._PropertyChanged('CizgiTabakaAdi');
  End;
end;

procedure CellRectangle.SetDikeyHizalama(Value: DikeyHizalama);
begin
  Self._PropertyChanging('DikeyHizalama');
  If value <> _DikeyHizalama Then
  begin
      _DikeyHizalama := value;
      Self._PropertyChanged('DikeyHizalama');
  End;
end;

procedure CellRectangle.SetFill(Value: Boolean);
begin
  Self._PropertyChanging('Fill');
  If value <> _Fill Then
  begin
      _Fill := value;
      Self._PropertyChanged('Fill');
  End;
end;

procedure CellRectangle.SetFillColor(Value: TColor);
begin
  Self._PropertyChanging('FillColor');
  If value <> _FillColor Then
  begin
      _FillColor := value;
      Self._PropertyChanged('FillColor');
  End;
end;

procedure CellRectangle.SetFirstYukseklik;
begin
  Yukseklik := _YukseklikFirst;
end;

procedure CellRectangle.SetFont(Value: IlicgFontTool);
begin
  Self._PropertyChanging('Font');
  _Font := value;
  _Font.Assign(value);
  Self._PropertyChanged('Font');
end;

procedure CellRectangle.SetGenislik(Value: Double);
begin
  Self._PropertyChanging('Genislik');
  If value <> _Genislik Then
  begin
      _Genislik := value;
      Self._PropertyChanged('Genislik');
  End;
end;

procedure CellRectangle.SetIceGirinti(Value: Single);
begin
  Self._PropertyChanging('IceGirinti');
  If value <> _IceGirinti Then
  begin
      _IceGirinti := value;
      Self._PropertyChanged('IceGirinti');
  End;
end;

procedure CellRectangle.SetJust(Value: TlicgTextPos);
begin
  Self._PropertyChanging('Just');
  If value <> _Just Then
  begin
      _Just := value;
      Self._PropertyChanged('Just');
  End;
end;

procedure CellRectangle.SetOlcek(Value: Double);
begin
  Self._PropertyChanging('Olcek');
  If value = 0 Then value := 1;
  If _Olcek <> value Then
  begin
      _Olcek := value;
      Self._PropertyChanged('Olcek');
  End;
end;

procedure CellRectangle.SetSagCerceve(Value: Border);
begin
  Self._PropertyChanging('SagCerceve');
  If value <> _Sag Then
  begin
      _Sag := value;
      Self._PropertyChanged('SagCerceve');
  End;
end;

procedure CellRectangle.SetSolAlt(value: Point);
begin
  Self._PropertyChanging('SolAlt');
  If value <> _SolAlt Then
  begin
    _SolAlt := value;
//      _SolAlt.X := value.X;
//      _SolAlt.Y := value.Y;
      Self._PropertyChanged('SolAlt');
  End;
end;

procedure CellRectangle.SetSolCerceve(Value: Border);
begin
  Self._PropertyChanging('SolCerceve');
  If value <> _Sol Then
  begin
      _Sol := value;
      Self._PropertyChanged('SolCerceve');
  End;
end;

procedure CellRectangle.SetStag(Value: String);
begin
  _Stag := value;
end;

procedure CellRectangle.SetTabakaAdi(Value: String);
begin
  Self._PropertyChanging('TabakaAdi');
  If value <> _TabakaAdi Then
  begin
      _TabakaAdi := value;
      Self._PropertyChanged('TabakaAdi');
  End;
end;
{
procedure CellRectangle.SetTextRefPoint;
var
  a: Integer;
  p: Point;
  TempDeg1, TempDeg2, TempWidth: Double;
  TempEnt: IlicgEntity;
begin
  Try
    p := Point.Create;
    If Aci > (PI * 2) Then
      Aci := RealMod(Aci,(PI * 2));
    a := 0;

    For p In _ListOfYaziCor do
    begin
      TempEnt := Licad.CreateEntityFactory.MakeText(AsCoor(p.X,p.Y),_ListOfYazi[a],1,Aci);
      TempEnt.DrawTools.FontTool.TextPos := _Just;
      P.X := TempEnt.Geometry.Extent.LowerLeft.X;
      P.Y := TempEnt.Geometry.Extent.LowerLeft.Y;
      a := a + 1;
    end;
    Exit; // Gerek Yok.


      For p In _ListOfYaziCor do
      begin
        TempWidth := (Width(_ListOfYazi[a], Licad.Settings.DefFontStyle));
        TempDeg1 := Cos(PI * (RadiansToDegrees(Aci) + 90) / 180.0); //(RadiansToDegrees(Aci) + 90)
        TempDeg2 := -Sin(PI * (RadiansToDegrees(Aci) + 90) / 180.0);
          Case _Just of//['3', 'C', 'M', '4', '5', 'R']) of
              tpCenterUp, tpCenterDown, tpCenter:
              begin
                  p.X := (TempWidth/2 * TempDeg1) + p.X;
                  p.Y := (TempWidth/2 * TempDeg2) + p.Y;
              end;
              tpUpperRight, tpCenterRight, tpLowerRight:
              begin
                  p.X := (TempWidth * TempDeg1) + p.X;
                  p.Y := (TempWidth * TempDeg2) + p.Y;
              end;
          End;
          a := a + 1;
      end;
  except on e: Exception do
    raise Exception.Create('SetTextRefPoint');
//      Throw New Exception("SetTextRefPoint")
  End;
end;
}
procedure CellRectangle.SetUstCerceve(Value: Border);
begin
  Self._PropertyChanging('UstCerceve');
  If value <> _Ust Then
  begin
      _Ust := value;
      Self._PropertyChanged('UstCerceve');
  End;
end;

procedure CellRectangle.SetYatayHizalama(Value: YatayHizalama);
begin
  Self._PropertyChanging('YatayHizalama');
  If value <> _YatayHizalama Then
  begin
      _YatayHizalama := value;
      Self._PropertyChanged('YatayHizalama');
  End;
end;

procedure CellRectangle.SetYazi(Value: String);
begin
  Self._PropertyChanging('Yazi');
  If _Yazi <> value Then
  begin
      If _Yazi = '' Then _YaziFirst := value;
      _Yazi := value;
      Self._PropertyChanged('Yazi');
  End;
end;

procedure CellRectangle.SetYaziTabakaAdi(Value: String);
begin
  Self._PropertyChanging('YaziTabakaAdi');
  If value <> _YaziTabakaAdi Then
  begin
      _YaziTabakaAdi := value;
      Self._PropertyChanged('YaziTabakaAdi');
  End;
end;

procedure CellRectangle.SetYaziWrap(Value: Boolean);
begin
  Self._PropertyChanging('YaziWrap');
  If value <> _YaziWrap Then
  begin
      _YaziWrap := value;
      Self._PropertyChanged('YaziWrap');
  End;
end;

procedure CellRectangle.SetYukseklik(Value: Double);
begin
  Self._PropertyChanging('Yukseklik');
  If value <> _Yukseklik Then
  begin
      _Yukseklik := value;
      Self._PropertyChanged('Yukseklik');
  End;
end;
{
procedure CellRectangle.Set_Limits3(PAci: Double = 0);
var
  NYUK: Double;
  TempEnt: IlicgEntity;
begin
  //SolÜst Sað Alt
  PAci := Aci;
  If PAci > (PI * 2) Then
      PAci := RealMod(PAci,(PI * 2));
  NYUK := ((Font.Height * _ListOfYazi.Count) + ((Font.Height / 2) * (_ListOfYazi.Count - 1))) + (_StandartDikeyBosluk * 2);
  If YaziWrap And (_ListOfYazi.Count > 1) And (Yukseklik < NYUK) Then
    Self._Yukseklik := NYUK;
//  if Self._YukseklikFirst <> Self._Yukseklik then
//    TempEnt := Licad.CreateEntityFactory.MakeRectangle(AsCoor(_SolAlt.X,_SolAlt.Y+_Yukseklik),AsCoor(_SolAlt.X+_Genislik,_SolAlt.Y));
//  else
    TempEnt := Licad.CreateEntityFactory.MakeRectangle(
    AsCoor(_SolAlt.X,_SolAlt.Y+_YukseklikFirst),
    AsCoor(_SolAlt.X+_Genislik,_SolAlt.Y));
//    AsCoor(_SolAlt.X,_SolAlt.Y-_Yukseklik),
//    AsCoor(_SolAlt.X+_Genislik,_SolAlt.Y+(_YukseklikFirst-_Yukseklik)));

//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TempEnt);

  _SolAlt.Y := TempEnt.Geometry.DrawPoints.Y[0];
  _SolAlt.X := TempEnt.Geometry.DrawPoints.X[0];

  _SagAlt.Y := TempEnt.Geometry.DrawPoints.Y[1];
  _SagAlt.X := TempEnt.Geometry.DrawPoints.X[1];

  _SagUst.Y := TempEnt.Geometry.DrawPoints.Y[2];
  _SagUst.X := TempEnt.Geometry.DrawPoints.X[2];

  _SolUst.Y := TempEnt.Geometry.DrawPoints.Y[3];
  _SolUst.X := TempEnt.Geometry.DrawPoints.X[3];

  _SolOrta.Y := (_SolUst.Y + _SolAlt.Y)/2;
  _SolOrta.X := (_SolUst.X + _SolAlt.X)/2;

  _SagOrta.Y := (_SagUst.Y + _SagAlt.Y)/2;
  _SagOrta.X := (_SagUst.X + _SagAlt.X)/2;

  _AltOrta.Y := (_SolAlt.Y + _SagAlt.Y)/2;
  _AltOrta.X := (_SolAlt.X + _SagAlt.X)/2;

  _UstOrta.Y := (_SolUst.Y + _SagUst.Y)/2;
  _UstOrta.X := (_SolUst.X + _SagUst.X)/2;

  _OrtaOrta.Y := (_SolAlt.Y + _SagUst.Y)/2;
  _OrtaOrta.X := (_SolAlt.X + _SagUst.X)/2;
end;
}
procedure CellRectangle.Set_Limits(PAci: Double = 0);
var
  NYUK: Double;
  TempEnt: IlicgEntity;
//  p: Point;
begin
//  Set_Limits3(PAci);
//  Exit;

//  PAci := 0;
  PAci := Aci;
  If PAci > (PI * 2) Then
      PAci := RealMod(PAci,(PI * 2));

  NYUK := ((Font.Height * _ListOfYazi.Count) + ((Font.Height / 2) * (_ListOfYazi.Count - 1))) + (_StandartDikeyBosluk * 2);
//  'Dim NYUK As Double = (Font.Size * _ListOfYazi.Count) + (Font.Size / 2) * _ListOfYazi.Count - 1

  If YaziWrap And (_ListOfYazi.Count > 1) And (Yukseklik < NYUK) Then
  begin
//      '_SolAlt.X = ((NYUK - Yukseklik) * Math.Cos(Math.PI * (RadiansToDegrees(PAci) + 180) / 180)) + _SolAlt.X
//      '_SolAlt.Y = ((NYUK - Yukseklik) * -Math.Sin(Math.PI * (RadiansToDegrees(PAci) + 180) / 180)) + _SolAlt.Y
    Self._Yukseklik := NYUK;

    //  _SolAlt.Y := ((-NYUK) * Cos(PI * (RadiansToDegrees(PAci)) / 180)) + _SolAlt.Y;
    //  _SolAlt.X := ((-NYUK) * -Sin(PI * (RadiansToDegrees(PAci)) / 180)) + _SolAlt.X;
  end;

//  _SolAlt.Y := ((_Yukseklik - _YukseklikFirst) * Cos(PI * (RadiansToDegrees(PAci) + 180) / 180)) + _SolAlt.Y;
//  _SolAlt.X := ((_Yukseklik - _YukseklikFirst) * -Sin(PI * (RadiansToDegrees(PAci) + 180) / 180)) + _SolAlt.X;

//  ' Yükseklik = Dairenin Yariçapý
//  ' Açi derece cinsinden
//  '///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  _SolUst.Y := Yukseklik * Cos(PI * RadiansToDegrees(Aci) / 180.0) + _SolAlt.Y;
  _SolUst.X := Yukseklik * -Sin(PI * RadiansToDegrees(Aci) / 180.0) + _SolAlt.X;

  _SolOrta.Y := (Yukseklik / 2) * Cos(PI * RadiansToDegrees(Aci) / 180.0) + _SolAlt.Y;
  _SolOrta.X := (Yukseklik / 2) * -Sin(PI * RadiansToDegrees(Aci) / 180.0) + _SolAlt.X;

  _SagAlt.Y := Genislik * Cos(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolAlt.Y;
  _SagAlt.X := Genislik * -Sin(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolAlt.X;

  _SagUst.Y := Yukseklik * Cos(PI * RadiansToDegrees(Aci) / 180.0) + _SagAlt.Y;
  _SagUst.X := Yukseklik * -Sin(PI * RadiansToDegrees(Aci) / 180.0) + _SagAlt.X;

  _SagOrta.Y := (Yukseklik / 2) * Cos(PI * RadiansToDegrees(Aci) / 180.0) + _SagAlt.Y;
  _SagOrta.X := (Yukseklik / 2) * -Sin(PI * RadiansToDegrees(Aci) / 180.0) + _SagAlt.X;

  _AltOrta.Y := (Genislik / 2) * Cos(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolAlt.Y;
  _AltOrta.X := (Genislik / 2) * -Sin(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolAlt.X;

  _UstOrta.Y := (Genislik / 2) * Cos(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolUst.Y;
  _UstOrta.X := (Genislik / 2) * -Sin(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolUst.X;

  _OrtaOrta.Y := (Genislik / 2) * Cos(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolOrta.Y;
  _OrtaOrta.X := (Genislik / 2) * -Sin(PI * (RadiansToDegrees(Aci) + 270) / 180.0) + _SolOrta.X;

//  TempEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 5, _2D);
//  TempEnt.Geometry.Points.X[0] := _SolAlt.X;
//  TempEnt.Geometry.Points.Y[0] := _SolAlt.Y;
//  TempEnt.Geometry.Points.X[1] := _SagAlt.X;
//  TempEnt.Geometry.Points.Y[1] := _SagAlt.Y;
//  TempEnt.Geometry.Points.X[2] := _SagUst.X;
//  TempEnt.Geometry.Points.Y[2] := _SagUst.Y;
//  TempEnt.Geometry.Points.X[3] := _SolUst.X;
//  TempEnt.Geometry.Points.Y[3] := _SolUst.Y;
//  TempEnt.Geometry.Points.X[4] := _SolAlt.X;
//  TempEnt.Geometry.Points.Y[4] := _SolAlt.Y;
//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TempEnt);
//  _Limit.cll.X := _SagAlt.X;
//  _Limit.cll.Y := _SolAlt.Y;
//  _Limit.cur.X := _SolUst.X;
//  _Limit.cur.Y := _SagUst.Y;
//  _Limit.cll.X := _SolAlt.X;
//  _Limit.cll.Y := _SolAlt.Y;
//  _Limit.cur.X := _SagUst.X;
//  _Limit.cur.Y := _SagUst.Y;
end;

procedure CellRectangle.Set_Limits2(PEnt: IlicgEntity);
var
  TempEnt: IlicgEntity;
begin
  _SolAlt.Y := PEnt.Geometry.DrawPoints.Y[0];
  _SolAlt.X := PEnt.Geometry.DrawPoints.X[0];

  _SagAlt.Y := PEnt.Geometry.DrawPoints.Y[1];
  _SagAlt.X := PEnt.Geometry.DrawPoints.X[1];

  _SagUst.Y := PEnt.Geometry.DrawPoints.Y[2];
  _SagUst.X := PEnt.Geometry.DrawPoints.X[2];

  _SolUst.Y := PEnt.Geometry.DrawPoints.Y[3];
  _SolUst.X := PEnt.Geometry.DrawPoints.X[3];


  _SolOrta.Y := (_SolUst.Y + _SolAlt.Y)/2;
  _SolOrta.X := (_SolUst.X + _SolAlt.X)/2;

  _SagOrta.Y := (_SagUst.Y + _SagAlt.Y)/2;
  _SagOrta.X := (_SagUst.X + _SagAlt.X)/2;

  _AltOrta.Y := (_SolAlt.Y + _SagAlt.Y)/2;
  _AltOrta.X := (_SolAlt.X + _SagAlt.X)/2;

  _UstOrta.Y := (_SolUst.Y + _SagUst.Y)/2;
  _UstOrta.X := (_SolUst.X + _SagUst.X)/2;

  _OrtaOrta.Y := (_SolAlt.Y + _SagUst.Y)/2;
  _OrtaOrta.X := (_SolAlt.X + _SagUst.X)/2;

//  TempEnt := Licad.CreateEntityFactory.MakeEntity(idPolygon, 5, _2D);
//  TempEnt.Geometry.Points.X[0] := _SolAlt.X;
//  TempEnt.Geometry.Points.Y[0] := _SolAlt.Y;
//  TempEnt.Geometry.Points.X[1] := _SagAlt.X;
//  TempEnt.Geometry.Points.Y[1] := _SagAlt.Y;
//  TempEnt.Geometry.Points.X[2] := _SagUst.X;
//  TempEnt.Geometry.Points.Y[2] := _SagUst.Y;
//  TempEnt.Geometry.Points.X[3] := _SolUst.X;
//  TempEnt.Geometry.Points.Y[3] := _SolUst.Y;
//  TempEnt.Geometry.Points.X[4] := _SolAlt.X;
//  TempEnt.Geometry.Points.Y[4] := _SolAlt.Y;
//  CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TempEnt);
end;

procedure SetPointWith(Target: Point; Source: Point);
begin
  Target.X := Source.X;
  Target.Y := Source.Y;
end;

procedure CellRectangle.Set_Text_Cor;
var
  Satir_Araligi, TyaziYuk, Tsatiraraligi, TYukseklikYarisi: Double;
  StartPoint, EndPoint: Point;
  Negatif: Boolean;
  I: Integer;
begin
  Satir_Araligi := Self._Font.Height * 0.5;
  StartPoint := TempPoint2;//nil;
  EndPoint := TempPoint3;//nil;
  Negatif := False;
  Case DikeyHizalama of
    Alt:
    begin
        Case YatayHizalama of
            YatayOrta:
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_AltOrta, _SagAlt, 0, -_StandartDikeyBosluk));// ' _AltOrta
                SetPointWith(EndPoint, DikAyakDikBoy2(_SagAlt, _AltOrta, 0, _StandartDikeyBosluk));// ' _SagAlt
                Negatif := False;
                _Just := GetPosCad(TextJustify.OrtaAlt);
                _JustNum := GetPosCad(TextJustify.OrtaAlt); //CInt
            end;
            Sol:// 'Sol
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SolAlt, _AltOrta, _StandartYatayBosluk, -_StandartDikeyBosluk));// '  _SolAlt
                SetPointWith(EndPoint, DikAyakDikBoy2(_AltOrta, _SolAlt, 0, _StandartDikeyBosluk));// ' _AltOrta
                Negatif := False;
                _Just := GetPosCad(TextJustify.Solalt);
                _JustNum := GetPosCad(TextJustify.Solalt); //CInt
            end;
            Sag:// 'Sag
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SagAlt, _AltOrta, _StandartYatayBosluk, _StandartDikeyBosluk));// ' _SagAlt
                SetPointWith(EndPoint, DikAyakDikBoy2(_AltOrta, _SagAlt, 0, -_StandartDikeyBosluk));// '_AltOrta
                Negatif := True;
                _Just := GetPosCad(TextJustify.SagAlt);
                _JustNum := GetPosCad(TextJustify.SagAlt);//CInt
            end;
        End;
    End;
    Ust:
    begin
        Case YatayHizalama of
            YatayOrta:
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_UstOrta, _SagUst, 0, _StandartDikeyBosluk));// ' _UstOrta
                SetPointWith(EndPoint, DikAyakDikBoy2(_SagUst, _UstOrta, 0, -_StandartDikeyBosluk));// '_SagUst
                Negatif := False;
                _Just := GetPosCad(TextJustify.OrtaUst);
                _JustNum := GetPosCad(TextJustify.OrtaUst); //CInt
            end;
            Sol: //'Sol
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SolUst, _UstOrta, _StandartYatayBosluk, _StandartDikeyBosluk));// ' _SolUst
                SetPointWith(EndPoint, DikAyakDikBoy2(_UstOrta, _SolUst, 0, -_StandartDikeyBosluk));// '_UstOrta
                Negatif := False;
                _Just := GetPosCad(TextJustify.SolUst);
                _JustNum := GetPosCad(TextJustify.SolUst);
            end;
            Sag: //'Sag
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SagUst, _UstOrta, _StandartYatayBosluk, -_StandartDikeyBosluk));// ' _SagUst
                SetPointWith(EndPoint, DikAyakDikBoy2(_UstOrta, _SagUst, 0, _StandartDikeyBosluk));// '_UstOrta
                Negatif := True;
                _Just := GetPosCad(TextJustify.SagUst);
                _JustNum := GetPosCad(TextJustify.SagUst);
            end;
        End;
    End;
    DikeyOrta:
    begin
        Case YatayHizalama of
            YatayOrta:
            begin
                SetPointWith(StartPoint, _OrtaOrta);
                SetPointWith(EndPoint, _SagOrta);
                Negatif := False;
                _Just := GetPosCad(TextJustify.OrtaOrta);
                _JustNum := GetPosCad(TextJustify.OrtaOrta);
            end;
            Sol: //'Sol
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SolOrta, _OrtaOrta, _StandartYatayBosluk, 0));// '  _SolOrta
                SetPointWith(EndPoint, _OrtaOrta);
                Negatif := False;
                _Just := GetPosCad(TextJustify.SolOrta);
                _JustNum := GetPosCad(TextJustify.SolOrta);
            end;
            Sag: //'Sag
            begin
                SetPointWith(StartPoint, DikAyakDikBoy2(_SagOrta, _OrtaOrta, _StandartYatayBosluk, 0));// '  _SagOrta
                SetPointWith(EndPoint, _OrtaOrta);
                Negatif := True;
                _Just := GetPosCad(TextJustify.SagOrta);
                _JustNum := GetPosCad(TextJustify.SagOrta);
            end;
        End;
    End;

  end;

  If _ListOfYaziCor.Count > 1 Then
  begin
      For i := 0 To _ListOfYaziCor.Count - 1 do
      begin
          Case DikeyHizalama of
              Alt:
              begin
                  If Negatif Then
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, -(Self._Font.Height + Satir_Araligi) * (i - (_ListOfYaziCor.Count - 1))))
                  Else
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, (Self._Font.Height + Satir_Araligi) * (i - (_ListOfYaziCor.Count - 1))));
              End;
              Ust:
              begin
                  If Negatif Then
                  begin
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, -(Self._Font.Height + Satir_Araligi) * i));
//                      _ListOfYaziCor[i].Y := _ListOfYaziCor[i].Y - Self._Font.Height; //Y
                  end
                  Else
                  begin
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, (Self._Font.Height + Satir_Araligi) * i));
//                      _ListOfYaziCor[i].Y := _ListOfYaziCor[i].Y - Self._Font.Height; //Y
                  end;
              End;
              DikeyOrta:
              begin
//                  ' Toplam Yazý Yüksekliði
                  TyaziYuk := Abs((Self._Font.Height) * (_ListOfYaziCor.Count)) / 2;
//                  ' Satýraralýgý Yüksekliði
                  Tsatiraraligi := Abs((Satir_Araligi) * (_ListOfYaziCor.Count - 1)) / 2;
                  TYukseklikYarisi := (TyaziYuk + Tsatiraraligi)-(Self._Font.Height/2);// ' / 2; ismet ekleme
                  If Negatif Then
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, -(Self._Font.Height + Satir_Araligi) * (i - (_ListOfYaziCor.Count - 1)) - (TYukseklikYarisi)))
                  Else
                      SetPointWith(_ListOfYaziCor[i], DikAyakDikBoy2(StartPoint, EndPoint, 0, (Self._Font.Height + Satir_Araligi) * (i - (_ListOfYaziCor.Count - 1)) + (TYukseklikYarisi)));
              End;
          End;
      end;
  end
  Else
  begin
    if _ListOfYaziCor.Count > 0 then
      Case DikeyHizalama of
          Alt:
          begin
              If Negatif Then
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, 0))
              Else
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, 0));
          End;
          Ust:
          begin
              If Negatif Then
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, -(Self._Font.Height)))
              Else
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, (Self._Font.Height)));
          End;
          DikeyOrta:
          begin
              If Negatif Then
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, -(Self._Font.Height / 2)))
              Else
                  SetPointWith(_ListOfYaziCor[0], DikAyakDikBoy(StartPoint, EndPoint, 0, (Self._Font.Height / 2)));
          End;
      End;
  End;
//  SetTextRefPoint;
end;

procedure CellRectangle.Set_Text_new1;
var
  i: Integer;
//  P: Point;
begin
//  p := Point.Create;
  Self._ListOfYazi.Clear;
  Self._ListOfYaziCor.Clear;

  If Yazi = '' Then
      Self._ListOfYazi.Add('')
  Else
  begin
    if Self.YaziWrap then
      Self._ListOfYazi := StringFunctions.Split(Yazi, ';,', Self._Genislik, Self.Font, True) //Licad.Settings.DefFontStyle
    else
      Self._ListOfYazi := StringFunctions.Split(Yazi, True);
//      StringFunctions.Split(Yazi, ';', True);
//      Lider.CG.ModulesCom.Table.CreateNcTableOfXml.CreateNcTableOfXml.Create(0,nil)
//      .Split(Yazi, ';', True);
//Asistan.CadObj.StringFunctions.Text //Font.GetSystemFont ';, -'
  end;

  For i := 0 To Self._ListOfYazi.Count - 1 do
  begin
      Self._ListOfYaziCor.Add(Point.Create);
  end;
end;

{ Point }

constructor Point.Create;
begin
  Y := 0;
  X := 0;
  inherited Create;
end;

function Point.isEqual(Param1, Param2: Point): Boolean;
begin
  result := _IsEqual(Param1.X, Param2.X, _EPSILON) and _IsEqual(Param1.Y, Param2.Y, _EPSILON);
end;

{ Border }

function Border.isEqual(Param1, Param2: Border): Boolean;
begin
  result := (Param1.Draw = Param2.Draw) and (Param1.LineIndex = Param2.LineIndex);
end;

{ Limit }

constructor Limit.Create;
begin
//  inherited Create;
  Self.cll := Point.Create;
  Self.cur := Point.Create;
end;

end.

