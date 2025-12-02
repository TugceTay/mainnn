unit Lider.CG.Com.GISQSimple;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Messages,
  Menus,
  Dialogs,
  ComCtrls,
  ShlObj,
  CheckLst,
  Grids,
  DBGrids,
  cxMemo,
  DB,
  cxStyles,
  cxCustomData,
  cxGraphics,
  cxFilter,
  cxData,
  cxDataStorage,
  cxEdit,
  cxDropDownEdit,
  cxMRUEdit,
  cxGridLevel,
  cxGridCustomTableView,
  cxGridTableView,
  cxClasses,
  cxControls,
  cxGridCustomView,
  cxGrid,
  cxLookAndFeelPainters,
  cxButtons,
  cxShellCommon,
  cxShellComboBox,
  cxContainer,
  cxTextEdit,
  cxMaskEdit,
  cxCheckBox,
  cxLookAndFeels,
  dxSkinsCore,
  dxSkinsDefaultPainters,
  dxSkinscxPCPainter,
  cxNavigator,
  cxGroupBox,
  dxSkinsdxStatusBarPainter,
  dxStatusBar,
  cxLabel,
  cxDBData,
  cxGridDBTableView,
  cxCheckListBox,
  Lider.CG.Com.DB,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GIS,
  Lider.CG.Com.System,
  Lider.CG.Com.Base,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.UtilityInt,
  cxCustomListBox,
  dxDateRanges, dxScrollbarAnnotations;

type
  TfmGISQSimple = class(TForm)
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    cxGroupBox4: TcxGroupBox;
    cxGroupBox1: TcxGroupBox;
    cxComboBox_f1: TcxComboBox;
    cxComboBox_o1: TcxComboBox;
    cxComboBox_d1: TcxComboBox;
    cxGroupBox2: TcxGroupBox;
    cxComboBox_f2: TcxComboBox;
    cxComboBox_o2: TcxComboBox;
    cxComboBox_d2: TcxComboBox;
    cxComboBox_k2: TcxComboBox;
    cxGroupBox3: TcxGroupBox;
    cxComboBox_f3: TcxComboBox;
    cxComboBox_o3: TcxComboBox;
    cxComboBox_d3: TcxComboBox;
    cxComboBox_k3: TcxComboBox;
    Memo1: TcxMemo;
    Table1: TlicgTable;
    DataSource1: TDataSource;
    dxStatusBar1: TdxStatusBar;
    Panel2: TPanel;
    Button4: TSpeedButton;
    Button5: TSpeedButton;
    Button3: TSpeedButton;
    Button2: TSpeedButton;
    Panel7: TPanel;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    cxLabel3: TcxLabel;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    CheckList1: TcxCheckListBox;
    procedure BtnVerifyClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxComboBox_f1PropertiesChange(Sender: TObject);
    procedure cxComboBox_f1PropertiesEditValueChanged(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cxComboBox_k2PropertiesChange(Sender: TObject);
    procedure cxComboBox_k3PropertiesChange(Sender: TObject);
  private

    { Private declarations }
    FLayer: TlicgBaseLayer;
    FGis: TlicgBaseGis;
    FFunctions: TStringList;
    FFunctionsDescr: TStringList;
    FDrawBox: TlicgBaseDrawBox;
    function VerifyExpression(ShwMsg: boolean): boolean;
    function FindFunction(const func: string; var Syntax, Description, ftype:
      string): boolean;
    function GetOperator(Value: string; q: string): string;
  public
    { Public declarations }
    procedure Enter(Gis: TlicgBaseGis; Layer: TlicgBaseLayer; isPanelBottom: boolean);
    procedure GenerateQuery;
    procedure ExecuteQuery(const queryString: string);
    procedure Clear;
  end;

implementation

{$R *.DFM}

resourcestring


{$IFDEF LANG_TRK}

  SOpEqual = 'Eþit';
  SOpNotEqual = 'Eþit Deðil';
  SOpGreaterThan = 'Büyük';
  SOpGreaterOrEqual = 'Büyük Eþit';
  SOpLessThan = 'Küçük';
  SOpLessOrEqual = 'Küçük Eþit';
  SOpStartWith = 'Ýle Baþlayan';
  SOpNotStartWith = 'Ýle Baþlamayan';
  SOpEndWith = 'Ýle Biten';
  SOpNotEndWith = 'Ýle Bitmeyen';
  SOpIn = 'Ýçinde';
  SOpNotIn = 'Ýçinde Deðil';
  SOpIsBlank = 'Boþ';
  SOpIsNotBlank = 'Boþ Deðil';


  //SExpressionOK= 'Expression is right !';
  SCustomGlobal = 'A global User Defined Function';
  SExprNative = 'Return value of field %s of native database file of layer';
  //SCustomFunctionAdded= 'Return the value of User Defined Function %s';

  { suported functions }
  SAbs = 'Abs(X);Mutlak deðeri döndürür. (X = deðer)';
  SArcTan = 'ArcTan(X);Arc Tanjant deðerini döndürür. ';
//  SArea = 'Area(Ent);Returns the AREA of the entity';
//  SArea = 'AREA(ENT);Alan';
//  SLineType = 'LINETYPE(ENT);Hat Türü';
  //SPoints = 'Points( Ent );Returns the list of points for the entity';
  SCopy =
    'Copy(<string>, <Index>, <Count>);Bir metnin bir kýsmýný yada tamamýný pozisyon (Index) ve uzunluk (Count) verilerek  döndüren fonksiyondur.' +
    CrLf + 'e.g.: Copy(''LicadGIS'', 1, 5) = Licad ';
  SCos =
    'Cos(X);Radyan cinsinden verilen X deðerinin kosinüsünü geri döndüren fonksiyondur.';
  SExp = 'Exp(X);X deðerinin logaritmasýný geri döndürür.';
  //SFormat = 'Format(<S:string>, <X1>, <X2>, ...);Format returns a formatted string assembled from a Format string S and a series of array arguments  (X1, X2,...)' + crlf +
//    'e.g.: FORMAT( ''%.2n %10.2n CCAT=%s'', AREA, PERIMETER, CCAT)';
  SFrac =
    'Frac(X);X deðerinin ondalýk kýsmýný geri döndüren fonsiyondur. Frac(123.456) =  0.456 ';
//  SIf = 'If(<Condition:boolean>, TrueResult, FalseResult);Koþul.' + crlf +
  //  'Örnek: IF(ALAN > 3000, ALAN-1000, ALAN) ALAN deðeri 3000 den büyük ise ALAN deðerinden 1000 çýkar, deðilse ALAN deðerini döndür.';
  SInt = 'Int(X);X deðerinin (kayan noktalý) tamsayý kýsmýný döndüren fonksiyondur.';
  //SLength = 'Length(Metin);Metnin uzunluk deðerini döndüren fonksiyondur. Örnek: Length(''Licad'') = 5 deðerini geri döndürür. ';
  SLn =
    'Ln(X);Ln returns the natural log of a real expression. The Ln function returns the natural logarithm (Ln(e) = 1) of the real-type expression X';
  SLower =
    'Lower(<S:string>);Lower returns a string with the same text as the string passed in S, but with all letters converted to lowercase';
//  SMaxExtentX = 'MaxExtentX(Ent);Nesnenin en büyük X koordinat deðerini döndürür.';
//  SMaxExtentY = 'MaxExtentY(Ent);Nesnenin en büyük Y koordinat deðerini döndürür.';
//  SMinExtentX = 'MinExtentX(Ent);Nesnenin en küçük X koordinat deðerini döndürür.';
//  SMinExtentY = 'MinExtentY(Ent);Nesnenin en küçük Y koordinat deðerini döndürür.';
//  SPerimeter = 'PERIMETER(ENT);Çevresi';
  SPI = 'PI;Pi deðeri (3.1415926535897932385)';
  //SPos = 'Pos(<Aranan Karakter>, <Metin>);Bir metin içinde aranan karakter yada karakterlerin pozisyonunu geri döndürür.' + crlf +
    //'Örnrk: Pos(''xdy'', ''y'')  4 deðerini döndürür.';
  SPower =
    'Power(<Base:double>, <Exponent:double>);Power raises Base to any power.\r\ne.g.: Power(Perimeter, 2.5)';
  SRound =
    'Round(X);X kayan noktalý deðerini yuvarlayarak tamsayý olarak geri döndürür. ';
  SSin = 'Sin(X);Radyan cinsinden X deðerinin sinüs deðerini geri döndürür.';
  SSqr = 'Sqr(X);X deðerinin karesini geri döndürür.';
  SSqrt = 'Sqrt(X);X deðerinin karekök deðerini geri döndürür.';
  STrunc =
    'Trunc(<X>);The Trunc function truncates a real-type value to an integer-type value.' +
    crlf + 'e.g.: TRUNC(PERIMETER)';
  //SUpper = 'Upper(<S:string>);The Upper function returns a string containing the same text as S, but with all 7-bit ASCII characters between ''a'' and ''z'' converted to uppercase.' + crlf +
    //'e.g.: UPPER(NATIVE(''NOMBRE''))';
  SCurDate = 'CurDate;Returns the current date';
  //SFormateDateTime = 'FormatDateTime(<S:string>, <Date>);Formats the date-and-time value given by DATE using the format given by S.' + crlf +
   // 'e.g.1: FormatDateTime(''dd/mmm/yyyy'', Routes.DueDate)' + crlf +
   // 'e.g.2: FormatDateTime(''dd mmm yyyy'', Xbase1(''Born_Date'')';
  //SFormatFloat = 'FormatFloat(<Format>, <X:Kayan noktalý deðer>);Kayan noktalý bir deðerinin istenen formatta metin olarak geri döndürür.' + crlf +
    //'Örnek: FORMATFLOAT(''$###, ###. ##'', 123456.45) $123,456.45 döndürür.';
//  SCentroidX = 'CentroidX(Ent);Nesnenin merkez X koordinat deðerini döndürür.';
//  SCentroidY = 'CentroidY(Ent);Nesnenin merkez Y koordinat deðerini döndürür.';
  //SType = 'NesneTürü(Ent);Nesnenin türünü geri döndürür. Örnek: (Polygon), (Polyline)...';
  SCrLf = 'CrLf;Insert a new line in the result.';
  //SColor = 'Renk(Ent);Nesne renk deðerini geri döndürür.(Yazý, hat nesneleri)';
  //SFillColor = 'DolguRengi(Ent);Kapalý alan nesnesinin dolgu rengini geri döndürür. ';
  //SBlack = 'Black;Returns the numeric value of color';
  //SMaroon = 'Maroon;Returns the numeric value of color';
  //SGreen = 'Green;Returns the numeric value of color';
  //SOlive = 'Olive;Returns the numeric value of color';
  //SNavy = 'Navy;Returns the numeric value of color';
  //SPurple = 'Purple;Returns the numeric value of color';
  //STeal = 'Teal;Returns the numeric value of color';
  //SGray = 'Gray;Returns the numeric value of color';
  //SSilver = 'Silver;Returns the numeric value of color';
  //SRed = 'Red;Returns the numeric value of color';
  //SLime = 'Lime;Returns the numeric value of color';
  //SYellow = 'Yellow;Returns the numeric value of color';
  //SBlue = 'Blue;Mavi';
  //SFuchsia = 'Fuchsia;Returns the numeric value of color';
  //SAqua = 'Aqua;Returns the numeric value of color';
  //SWhite = 'White;Returns the numeric value of color';
  //SRGB = 'RGB(<Kýrmýzý>,<Yeþil>,<Mavi>);Verilen kýrmýzý, yeþil ve mavi renk deðerlerinin kombinasyonundan renk deðerini geri döndürür.';
//  SText = 'Text(Ent);Yazý nesnesinin metin bilgisi';
  //SLayerName = 'Tabaka(Ent);Nesnenin tabaka adýný geri döndürür.';
  //SDistance = 'Mesafe(X1, Y1, X2, Y2);X1,Y1 ve X2,Y2 koordinatlarý arasýndaki uzaklýk deðerini geri döndürür. ';
  //SLeft = 'Left( <Metin>, <Uzunluk> );Metnin solundan uzunluk kadar kýsmýný geri döndürür.';
  //SRight = 'Right( <Metin>, <Uzunluk> );Metnin saðýndan uzunluk kadar kýsmýný geri döndürür.';
  //SIsSelected = 'IsSelected(Ent);Nesnenin seçili olup olmadýðýný (True/False) döndürür.';
  //SYear = 'Year( <Date> );Return the year part of the date parameter';
  //SMonth = 'Month( <Date> );Return the month part of the date parameter';
  //SDay = 'Day( <Date> );Return the day part of the date parameter';
  //SHour = 'Hour( <Time> );Return the hour part of the time parameter';
  //SMin = 'Min( <Time> );Return the minutes part of the time parameter';
  //SSec = 'Sec( <Time> );Return the seconds part of the time parameter';
  //SMSec = 'MSec( <Time> );Return the milliseconds part of the time parameter';
  //STo_Char = 'TO_CHAR( expression );Converts the expression to a string of chars';
  //STo_Date = 'TO_DATE( string_expression );Converts the string_expression to a date by using Windows Short Date Format';
  //STo_Num = 'TO_NUM( string_expression );Converts the string_expression to a number';
  //SDecode = 'DECODE(base_expr, compare1, value1, compare2, value2...,default);Similar to a nested IF-THEN-ELSE, base_expr compare against compare1,compare2,etc.';
  SExpressionOkay = 'Ýfade OK.';
{$ENDIF}

procedure _PopulateFieldList(L: TlicgBaselayer; Strings: TStrings);
var
  I, p: Integer;
  temp, FieldName: string;
  TmpSL: TStringList;
begin
  Strings.Clear;
 /// Strings.AddObject(AddBrackets(DisplayName) + SEntityField, nil);
 /// Strings.AddObject(AddBrackets(DisplayName) + SRecNofield, nil);
  if L.DBTable <> nil then
    with L.DBTable do
      for I := 1 to FieldCount do
      begin
        FieldName := Field(I);
        Strings.AddObject(AddBrackets(L.DisplayName) + '.' + AddBrackets(FieldName), nil);
      end;
end;

procedure TfmGISQSimple.Enter(Gis: TlicgBaseGis; Layer: TlicgBaseLayer;
  isPanelBottom: boolean);
var
  I: integer;
  LayerName, FilePAth: string;
  Identifier: string;
  Accept: Boolean;

  procedure AddFunc(const NewFunct: string);
  var
    func, descr: string;
    P: integer;
  begin

    p := AnsiPos(';', NewFunct);
    if p = 0 then
    begin
      MessageToUser('Hatalý Ýfade !', smsgerror, MB_ICONERROR);
      Exit;
    end;
    func := Copy(NewFunct, 1, p - 1);
    descr := Copy(NewFunct, p + 1, Length(NewFunct));
    FFunctions.Add(func);
    FFunctionsDescr.Add(descr);
  end;

var
  SL: TStrings;
begin
  Panel2.Visible := isPanelBottom;
  FGis := Gis;
  FLayer := Layer;

  Table1.Gis := FGis;
  Table1.LayerName := FLayer.DisplayName;

  FDrawBox := CurrCmdline.ActiveDrawBox;

  FFunctions := TStringList.create;
  FFunctionsDescr := TStringList.create;

  AddFunc(SAbs);
  //AddFunc(SArcTan);
  //AddFunc(SArea);
  //AddFunc(SLineType);
 // AddFunc(SPoints);
  AddFunc(SCopy);
  AddFunc(SCos);
  AddFunc(SExp);
 // AddFunc(SFormat);
  AddFunc(SFrac);
  //AddFunc(SIf);
  AddFunc(SInt);
 // AddFunc(SLength);
 // AddFunc(SLn);
 // AddFunc(SLower);
  //AddFunc(SMaxExtentX);
  //AddFunc(SMaxExtentY);
  //AddFunc(SMinExtentX);
  //AddFunc(SMinExtentY);
  //AddFunc(SPerimeter);
  AddFunc(SPI);
//  AddFunc(SPos);
//  AddFunc(SPos);
//  AddFunc(SPower);
  AddFunc(SRound);
  AddFunc(SSin);
  AddFunc(SSqr);
  AddFunc(SSqrt);
//  AddFunc(STrunc);
 // AddFunc(SUpper);
 // AddFunc(SCurDate);
//  AddFunc(SFormateDateTime);
  //AddFunc(SFormatFloat);
  //AddFunc(SCentroidX);
 // AddFunc(SCentroidY);
//  AddFunc(SType);
//  AddFunc(SCrLf);
//  AddFunc(SColor);
 // AddFunc(SFillColor);
 { AddFunc(SBlack);
  AddFunc(SMaroon);
  AddFunc(SGreen);
  AddFunc(SOlive);
  AddFunc(SNavy);
  AddFunc(SPurple);
  AddFunc(STeal);
  AddFunc(SGray);
  AddFunc(SSilver);
  AddFunc(SRed);
  AddFunc(SLime);
  AddFunc(SYellow);
  AddFunc(SBlue);
  AddFunc(SFuchsia);
  AddFunc(SAqua);
  AddFunc(SWhite); }
//  AddFunc(SRGB);
  //AddFunc(SText);
 // AddFunc(SLayerName);
//  AddFunc(SDistance);
//  AddFunc(SLeft);
 // AddFunc(SRight);
//  AddFunc(SIsSelected);
 // AddFunc(SYear);
 // AddFunc(SMonth);
 // AddFunc(SDay);
 // AddFunc(SHour);
 // AddFunc(SMin);
 // AddFunc(SSec);
 // AddFunc(SMSec);
 // AddFunc(STo_Char);
 // AddFunc(STo_Date);
 // AddFunc(STo_Num);
//  AddFunc(SDecode);

  {populate listbox of functions}


  _PopulateFieldList(FLayer, cxComboBox_f1.Properties.Items);
  _PopulateFieldList(FLayer, cxComboBox_d1.Properties.Items);

  SL := TStringList.Create;

  _PopulateFieldList(FLayer, SL);

  for i := 0 to SL.Count - 1 do
    CheckList1.AddItem(SL[i]);

  for I := 0 to CheckList1.Items.Count - 1 do
    CheckList1.Items[I].Checked := True;

  SL.Free;

  CheckList1.AddItem(AnsiUpperCase('TYPE(ENT)'));

  CheckList1.AddItem(AnsiUpperCase('AREA(ENT)'));
  CheckList1.AddItem(AnsiUpperCase('PERIMETER(ENT)'));
  CheckList1.AddItem(AnsiUpperCase('MaxExtentX(Ent)'));
  CheckList1.AddItem(AnsiUpperCase('MaxExtentY(Ent)'));
  CheckList1.AddItem(AnsiUpperCase('MinExtentX(Ent)'));
  CheckList1.AddItem(AnsiUpperCase('MinExtentY(Ent)'));
  CheckList1.AddItem(AnsiUpperCase('CentroidX(Ent)'));
  CheckList1.AddItem(AnsiUpperCase('CentroidY(Ent)'));
  //CheckList1.AddItem (AnsiUpperCase('IsSelected(ENT)'));


  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"Area(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"Perimeter(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"MaxExtentX(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"MaxExtentY(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"MinExtentX(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"MinExtentY(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"CentroidX(ENT)"'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"CentroidY(ENT)"'), Nil);
  //cxComboBox_f1.Properties.Items.AddObject (AnsiUpperCase('IsSelected(ENT)'), Nil);
  cxComboBox_f1.Properties.Items.AddObject(AnsiUpperCase('"Text(ENT)"'), Nil);

  for I := 0 to FFunctions.Count - 1 do
  begin
    //cxComboBox_f1.Properties.Items.Add(FFunctions[I]);
    cxComboBox_d1.Properties.Items.Add(FFunctions[I]);
  end;

  cxComboBox_o1.Properties.Items.Add(SOpEqual);
  cxComboBox_o1.Properties.Items.Add(SOpNotEqual);
  cxComboBox_o1.Properties.Items.Add(SOpGreaterThan);
  cxComboBox_o1.Properties.Items.Add(SOpGreaterOrEqual);
  cxComboBox_o1.Properties.Items.Add(SOpLessThan);
  cxComboBox_o1.Properties.Items.Add(SOpLessOrEqual);
  cxComboBox_o1.Properties.Items.Add(SOpStartWith);
  cxComboBox_o1.Properties.Items.Add(SOpNotStartWith);
  cxComboBox_o1.Properties.Items.Add(SOpEndWith);
  cxComboBox_o1.Properties.Items.Add(SOpNotEndWith);
  cxComboBox_o1.Properties.Items.Add(SOpIn);
  cxComboBox_o1.Properties.Items.Add(SOpNotIn);
  cxComboBox_o1.Properties.Items.Add(SOpIsBlank);
  cxComboBox_o1.Properties.Items.Add(SOpIsNotBlank);

  cxComboBox_o2.Properties.Items.AddStrings(cxComboBox_o1.Properties.Items);
  cxComboBox_o3.Properties.Items.AddStrings(cxComboBox_o1.Properties.Items);

  cxComboBox_f2.Properties.Items.AddStrings(cxComboBox_f1.Properties.Items);
  cxComboBox_f3.Properties.Items.AddStrings(cxComboBox_f1.Properties.Items);

  cxComboBox_d2.Properties.Items.AddStrings(cxComboBox_d1.Properties.Items);
  cxComboBox_d3.Properties.Items.AddStrings(cxComboBox_d1.Properties.Items);

  if Assigned(FGis.OnStartExternalPopulate) and Assigned(FGis.OnExternalPopulate) then
  begin
    Accept := True;
    FGis.OnStartExternalPopulate(FGis, FLayer.DisplayName, Accept);
    if Accept then
    begin
      Identifier := '';
      FGis.OnExternalPopulate(FGis, FLayer.DisplayName, Identifier);
      LayerName := FLayer.DisplayName;
      if AnsiPos(#32, LayerName) > 0 then
        Identifier := '[' + LayerName + ']';
      while Length(Identifier) > 0 do
      begin
        if AnsiPos(#32, Identifier) > 0 then
          Identifier := '[' + Identifier + ']';

        Identifier := '';
        FGis.OnExternalPopulate(FGis, FLayer.DisplayName, Identifier);
      end;
      if Assigned(FGis.OnEndExternalPopulate) then
        FGis.OnEndExternalPopulate(FGis, FLayer.DisplayName);
    end;
  end;

  Show;
end;

function TfmGISQSimple.VerifyExpression(ShwMsg: boolean): boolean;
var
  Expr: string;
  MainExpr: TlicgBaseMainExpr;
begin

  if Length(Expr) = 0 then
  begin
    result := true;
    exit;
  end;
  MainExpr := Licad.CreateMainExpr(FGis, FLayer); // TlicgMainExpr.Create(FGis, FLayer);
  try
    MainExpr.ParseExpression(Expr);
    if ShwMsg then
      MessageToUser(SExpressionOkay, smsgwarning, MB_ICONINFORMATION);
  finally
    MainExpr.Free;
  end;
  result := true;
end;

procedure TfmGISQSimple.BtnVerifyClick(Sender: TObject);
begin
  VerifyExpression(true);
end;

procedure TfmGISQSimple.OKBtnClick(Sender: TObject);
begin
  if not VerifyExpression(false) then
    ModalResult := mrNone;
end;

function TfmGISQSimple.FindFunction(const func: string; var syntax, description,
  ftype: string): boolean;
var
  Index: integer;
begin

  result := false;
  //Index := FFunctions.IndexOf(func);
  //if Index >= 0 then
  begin
    syntax := func; // FFunctions[Index];

    begin
      if (pos('(ENT)', syntax) <> 0) and (pos('TEXT', syntax) = 0) then
      begin
        fType := 'N';
        result := true;
      end
      else if syntax = 'AREA(ENT)' then
      begin
        syntax := 'AREA(ENT)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'LINETYPE(ENT)' then
      begin
        syntax := 'LINETYPE(ENT)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'PERIMETER(ENT)' then
      begin
        syntax := 'PERIMETER(ENT)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'TEXT(Ent)' then
      begin
        syntax := 'Upper(TEXT(ENT))';
        fType := 'C';
        result := true;
      end
      else if syntax = 'Mesafe(X1,Y1,X2,Y2)' then
      begin
        syntax := 'Distance(X1,Y1,X2,Y2)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'DolguRengi(Ent)' then
      begin
        syntax := 'FillColor(Ent)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'Renk(Ent)' then
      begin
        syntax := 'Color(Ent)';
        fType := 'N';
        result := true;
      end
      else
      //if syntax='NesneTürü(Ent)' then begin
       // syntax:='Type(Ent)';
      //  fType := 'C';
      //end;
if syntax = 'Tabaka(Ent)' then
      begin
        syntax := 'LayerName(Ent)';
        fType := 'N';
        result := true;
      end
      else if syntax = 'IsSelected(Ent)' then
        syntax := 'isSelected(Ent)';
    end;
    //description := FFunctionsDescr[Index];
    //result := true;
  end;

  if not Result then
    if pos('.', func) > 0 then
    begin
      if FLayer.DBTable.FieldNo(copy(func, pos('.', func) + 1, length(func))) > 0 then
        fType := FLayer.DBTable.FieldType(FLayer.DBTable.FieldNo(copy(func, pos('.',
          func) + 1, length(func))));
    end;

end;

procedure TfmGISQSimple.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

procedure TfmGISQSimple.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFunctions.free;
  FFunctionsDescr.free;
end;

function TfmGISQSimple.GetOperator(value: string; q: string): string;
begin
  if value = SOpEqual then
    result := '= "%s"'
  else if value = SOpNotEqual then
    result := '<> "%s"'
  else if value = SOpGreaterThan then
    result := '> "%s"'
  else if value = SOpGreaterOrEqual then
    result := '>= "%s"'
  else if value = SOpLessThan then
    result := '< "%s"'
  else if value = SOpLessOrEqual then
    result := '<= "%s"'
  else if value = SOpStartWith then
    result := 'LIKE "%s[_]"'
  else if value = SOpNotStartWith then
    result := 'NOT LIKE "%s[_]"'
  else if value = SOpEndWith then
    result := 'LIKE "[_]%s"'
  else if value = SOpNotEndWith then
    result := 'NOT LIKE "[_]%s"'
  else if value = SOpIn then
    result := 'IN'
  else if value = SOpNotIn then
    result := 'NOT IN'
  else if value = SOpIsBlank then
    result := 'BLANK'
  else if value = SOpIsNotBlank then
    result := 'NOT BLANK';

  result := StringReplace(result, '"', q, [rfReplaceAll]);

end;

procedure TfmGISQSimple.GenerateQuery;

  function AddSorguText(cbox_f, cbox_o, cbox_d, cbox_k: TcxComboBox): boolean;
  var
    p, c, j: Integer;
    AOperator: string;
    formatString: string;
    syntax, descr: string;
    manualInput: Boolean;
    fType: string;
    q: string;
    fieldName: string;
  begin
    q := '';
    j := 0;
    Result := False;
    try

      if ((cbox_f.ItemIndex >= 0) and (cbox_f.Text <> '')) and ((cbox_o.ItemIndex
        >= 0) and (cbox_o.Text <> '')) and ((cbox_d.ItemIndex >= 0) or (cbox_d.Text
        <> '')) then
      begin

        if cbox_k <> nil then
        begin
          if cbox_k.Text <> '' then      //and / or
            if cbox_k.Text = 'VE' then
              Memo1.Lines.Strings[Memo1.Lines.Count - 1] := Memo1.Lines.Strings[Memo1.Lines.Count
                - 1] + ' AND '
            else if cbox_k.Text = 'VEYA' then
              Memo1.Lines.Strings[Memo1.Lines.Count - 1] := Memo1.Lines.Strings[Memo1.Lines.Count
                - 1] + ' OR ';
        end;


           //if (GetDisplayText(i,0) = '(') or (GetDisplayText(i,0) = ')') then begin
             // Memo1.Lines.Add ( GetDisplayText(i,0) );
              //continue;
           //end;

        if not FindFunction(cbox_f.Text, syntax, descr, fType) then
          syntax := cbox_f.Text;

        if fType = 'C' then
          syntax := 'Upper(' + syntax + ')';
        q := '';
        if cbox_o.Text = SOpStartWith then
          fType := 'C'
        else if cbox_o.Text = SOpNotStartWith then
          fType := 'C'
        else if cbox_o.Text = SOpEndWith then
          fType := 'C'
        else if cbox_o.Text = SOpNotEndWith then
          fType := 'C'
        else if cbox_o.Text = SOpIn then
          fType := 'C'
        else if cbox_o.Text = SOpNotIn then
          fType := 'C';
          //<-

        if (ftype = 'C') or (ftype = '') then
          q := '"';

        AOperator := GetOperator(cbox_o.Text, q);

        if ((Pos('BLANK', AOperator) = 0) and (Pos('NOT BLANK', AOperator) = 0))
          and (not ((cbox_o.Text = SOpNotEndWith) or (cbox_o.Text = SOpEndWith))) then
          Memo1.Lines.Strings[Memo1.Lines.Count - 1] := Memo1.Lines.Strings[Memo1.Lines.Count
            - 1] + ' ' + syntax + ' ';

        if ((Pos('BLANK', AOperator) = 0) and (Pos('NOT BLANK', AOperator) = 0))
          and (not FindFunction(cbox_d.Text, syntax, descr, fType)) then
          syntax := cbox_d.Text;

        manualInput := ((Pos('BLANK', AOperator) = 0) and (Pos('NOT BLANK',
          AOperator) = 0)) and (not (cbox_f.Properties.Items.IndexOf(cbox_d.Text) >= 0));

        if (manualInput) and (ftype = 'C') then
          syntax := UpperCase(syntax);

        if Pos('BLANK', AOperator) = 0 then
        begin
          if (AOperator = 'IN') or (AOperator = 'NOT IN') then
          begin
            c := 0;
            AOperator := AOperator + '(';
            while True do
            begin
              p := Pos(',', syntax);
              if (p = 0) and (c > 0) then
              begin
    //               AOperator := AOperator + ',"' + Copy(syntax,1,length(syntax)) + '"';

                if j > 0 then
                  AOperator := AOperator + ',' + q + Copy(syntax, 1, length(syntax)) + q
                else
                  AOperator := AOperator + q + Copy(syntax, 1, length(syntax)) + q;
                break;
              end;
              Inc(c);
              if c > 1 then
                AOperator := AOperator + ',';

              if Copy(syntax, 1, p - 1) <> '' then
              begin
                Inc(j);
                AOperator := AOperator + q + Copy(syntax, 1, p - 1) + q;
              end;
              syntax := Copy(syntax, p + 1, length(syntax));
            end;
            AOperator := AOperator + ')';
            formatString := AOperator;
          end
          else if syntax <> '' then
            formatString := format(AOperator, [syntax])
          else
            formatString := '';

          formatString := StringReplace(formatString, '[_]', '%', [rfReplaceAll]);
        end
        else
        begin
          if AOperator = 'BLANK' then
            formatString := syntax + ' = ""'
          else
            formatString := syntax + ' <> ""';
        end;

        if ((AOperator <> 'BLANK') and (AOperator <> 'NOT BLANK')) and (not
          manualInput) then
          formatString := StringReplace(formatString, '"', '', [rfReplaceAll]);

        if (cbox_o.Text = SOpEndWith) then
        begin
          fieldName := cbox_f.Text;
          if pos('.', cbox_f.Text) > 0 then
          begin
            fieldName := copy(cbox_f.Text, pos('.', cbox_f.Text) + 1, length(cbox_f.Text));
          end;
          formatString := 'Right(' + fieldName + ',' + inttostr(length(cbox_d.Text))
            + ')=' + q + cbox_d.Text + q;
        end
        else if (cbox_o.Text = SOpNotEndWith) then
        begin
          fieldName := cbox_f.Text;
          if pos('.', cbox_f.Text) > 0 then
          begin
            fieldName := copy(cbox_f.Text, pos('.', cbox_f.Text) + 1, length(cbox_f.Text));
          end;
          formatString := 'Right(' + fieldName + ',' + inttostr(length(cbox_d.Text))
            + ')<>' + q + cbox_d.Text + q;
        end;
           //<-

        Memo1.Lines.Strings[Memo1.Lines.Count - 1] := Memo1.Lines.Strings[Memo1.Lines.Count
          - 1] + ' ' + formatString + ' ';
        Memo1.Lines.Add('');

        Result := True;
      end;

    except
    end;

  end;

begin
  Memo1.Lines.BeginUpdate;

  Memo1.Lines.Clear;
  Memo1.Lines.Add('');
  if AddSorguText(cxComboBox_f1, cxComboBox_o1, cxComboBox_d1, nil) then
    if AddSorguText(cxComboBox_f2, cxComboBox_o2, cxComboBox_d2, cxComboBox_k2) then
      AddSorguText(cxComboBox_f3, cxComboBox_o3, cxComboBox_d3, cxComboBox_k3);
  Memo1.Lines.EndUpdate;

end;



(*
Var
  i,p,c,j       : Integer;
  AOperator      : string;
  formatString  : string;
  syntax,
  descr         : string;
  manualInput   : Boolean;
  fType         : string;
  q             : string;
  fieldName     : string;
begin
  q := '';
  j:=0;
  For i:=0 to cxGrid1TableView1.DataController.RecordCount-1 do begin

     With  cxGrid1TableView1.DataController do begin
       //
       if (GetDisplayText(i,0) = '(') or (GetDisplayText(i,0) = ')') then begin
          Memo1.Lines.Add ( GetDisplayText(i,0) );
          continue;
       end;

       if Not FindFunction(GetDisplayText(i,0), syntax, descr, fType) then
         syntax := GetDisplayText(i,0);
      if fType = 'C' then
        syntax := 'Upper('+syntax+')';
      q := '';
      if GetDisplayText(i,1) = SOpStartWith  then
        fType := 'C'
      Else
      if GetDisplayText(i,1) = SOpNotStartWith  then
        fType := 'C'
      Else
      if GetDisplayText(i,1) = SOpEndWith then
        fType := 'C'
      Else
      if GetDisplayText(i,1) = SOpNotEndWith then
        fType := 'C'
      Else
      if GetDisplayText(i,1) = SOpIn then
        fType := 'C'
      Else
      if GetDisplayText(i,1) = SOpNotIn then
        fType := 'C';
      //<-

       if (ftype = 'C') or (ftype='') then
         q := '"';

       AOperator := GetOperator(GetDisplayText(i,1),q);

       if ( (Pos('BLANK',AOperator) = 0) and (Pos('NOT BLANK',AOperator) = 0) ) and ( Not ((GetDisplayText(i,1) = SOpNotEndWith) or  (GetDisplayText(i,1) = SOpEndWith)) ) then
          Memo1.Lines.Add ( syntax );

       if  ( (Pos('BLANK',AOperator) = 0) and (Pos('NOT BLANK',AOperator) = 0) ) and  ( Not FindFunction(GetDisplayText(i,2), syntax, descr, fType) ) then
         syntax := GetDisplayText(i,2);

       manualInput :=  ( (Pos('BLANK',AOperator) = 0) and (Pos('NOT BLANK',AOperator) = 0) )   and (Not (TcxComboBoxProperties ( cxGrid1ColumnValue.Properties ).Items.IndexOf ( GetDisplayText(i,2) ) >=0) );

       if (manualInput) and (ftype = 'C') then
         syntax := UpperCase(syntax);

       if Pos('BLANK',AOperator) = 0 then begin
         if (AOperator='IN') or (AOperator='NOT IN') then begin
           c:=0;
           AOperator := AOperator + '(';
           While True do begin
             p := Pos(',',syntax);
             if (p = 0) and (c>0) then begin
//               AOperator := AOperator + ',"' + Copy(syntax,1,length(syntax)) + '"';

                 if j>0 then
                   AOperator := AOperator + ','+q + Copy(syntax,1,length(syntax)) + q
                 else
                   AOperator := AOperator + q + Copy(syntax,1,length(syntax)) + q;
                 break;
             end;
             Inc(c);
             if c > 1 then
               AOperator := AOperator + ',';

             if Copy(syntax,1,p-1)<>'' then begin
               Inc(j);
               AOperator := AOperator + q + Copy(syntax,1,p-1) + q;
             end;
             syntax := Copy(syntax,p+1,length(syntax));
           end;
           AOperator := AOperator + ')';
           formatString := AOperator;
         end
         Else
           if syntax <> '' then
              formatString := format( AOperator , [syntax] )
           Else
              formatString := '';

         formatString := StringReplace(formatString,'[_]','%',[rfReplaceAll]);
       end
       Else
       begin
         if AOperator = 'BLANK' then
           formatString := syntax + ' = ""'
         else
           formatString := syntax + ' <> ""';
       end;

       if  ((AOperator <> 'BLANK') and (AOperator <> 'NOT BLANK') ) and (Not manualInput) then
         formatString := StringReplace(formatString, '"', '', [rfReplaceAll]);

       if (GetDisplayText(i,1) = SOpEndWith) then begin
         fieldName := GetDisplayText(i,0);
         if pos('.', GetDisplayText(i,0))>0 then begin
            fieldName := copy( GetDisplayText(i,0),pos('.', GetDisplayText(i,0))+1,length( GetDisplayText(i,0)) );
         end;
         formatString := 'Right(' + fieldName + ',' + inttostr(length(GetDisplayText(i,2)))+')='+q+GetDisplayText(i,2)+q;
       end
       else
       if (GetDisplayText(i,1) = SOpNotEndWith) then begin
         fieldName := GetDisplayText(i,0);
         if pos('.', GetDisplayText(i,0))>0 then begin
            fieldName := copy( GetDisplayText(i,0),pos('.', GetDisplayText(i,0))+1,length( GetDisplayText(i,0)) );
         end;
         formatString := 'Right(' + fieldName + ',' + inttostr(length(GetDisplayText(i,2)))+')<>'+q+GetDisplayText(i,2)+q;
       end;
       //<-

       Memo1.Lines.Add ( formatString );
       if i <  cxGrid1TableView1.DataController.RecordCount-1 then begin
         if GetDisplayText(i,3)<>'' then      //and / or
           if GetDisplayText(i,3) = 'Ve' then
             Memo1.Lines.Add ( 'AND' )
           Else
           if GetDisplayText(i,3) = 'Veya' then
             Memo1.Lines.Add ( 'OR' ) ;
       end;

     end; //with
  end; //For
  *)

procedure TfmGISQSimple.ExecuteQuery(const queryString: string);
var
  i: integer;
begin
  try
    Table1.DisableControls;
    try
      if Table1.Active then
        Table1.Close;

      Table1.MapFields.Clear;
      for I := 0 to CheckList1.Items.Count - 1 do
      begin
        if CheckList1.Items[I].Checked then
          with Table1.MapFields.Add do
          begin
            Expression := CheckList1.Items[I].Text;
            FieldName := Expression;
            IsExpression := true;
          end;
      end;

      Table1.ScopeFilter(queryString, True);
      Table1.Open;
      dxStatusBar1.Panels[2].Text := Format('%d kayýt', [Table1.RecordCount]);
      //Table1.DoSelect(FDrawBox.Selection) ;
      //FDrawBox.Repaint;
    except
    end;
  except
    Application.MessageBox('Çalýþtýrýlan sorgu hatalý. Lütfen sorgu kriterlerinizi kontrol ediniz.',
      'Bilgi', MB_OK + MB_ICONINFORMATION);
  end;
  Table1.EnableControls;
end;

procedure TfmGISQSimple.cxComboBox_f1PropertiesChange(Sender: TObject);
var
  Syntax, fType, Description: string;
begin

  dxStatusBar1.Panels[0].Text := '';
  dxStatusBar1.Panels[1].Text := '';
  Syntax := '';
  FindFunction(TcxComboBox(Sender).Text, Syntax, Description, fType);
  dxStatusBar1.Panels[0].Text := Syntax;
  dxStatusBar1.Panels[1].Text := ''; //Description;
  GenerateQuery;

end;

procedure TfmGISQSimple.cxComboBox_f1PropertiesEditValueChanged(Sender: TObject);
begin
  GenerateQuery;
end;

procedure TfmGISQSimple.Clear;
begin
  cxComboBox_f1.ItemIndex := -1;
  cxComboBox_f2.ItemIndex := -1;
  cxComboBox_f3.ItemIndex := -1;

  cxComboBox_d1.ItemIndex := -1;
  cxComboBox_d2.ItemIndex := -1;
  cxComboBox_d3.ItemIndex := -1;

  cxComboBox_o1.ItemIndex := -1;
  cxComboBox_o2.ItemIndex := -1;
  cxComboBox_o3.ItemIndex := -1;

  Memo1.Lines.Clear;

end;

procedure TfmGISQSimple.Button3Click(Sender: TObject);
begin
  if not Table1.Active then
  begin
    Exit;
  end;
  FDrawBox.SetEntityInViewEx(Table1.LayerName, Table1.SourceRecno, True);
end;

procedure TfmGISQSimple.Button2Click(Sender: TObject);
begin
  if not Table1.Active then
  begin
    Exit;
  end;
  FDrawBox.BlinkEntityEx(Table1.LayerName, Table1.SourceRecno);
end;

procedure TfmGISQSimple.Button5Click(Sender: TObject);
begin
  FDrawBox.ZoomToSelection;
end;

procedure TfmGISQSimple.Button4Click(Sender: TObject);
var
  SavedRecNo: Integer;
begin
  // select the result set
  if not Table1.Active then
  begin
    Exit;
  end;
  Table1.DisableControls;
  SavedRecNo := Table1.RecNo;
  try
    FDrawBox.Selection.Clear;
    Table1.First;
    while not Table1.Eof do
    begin
      FDrawBox.Selection.Add(FLayer, Table1.SourceRecNo);

      Table1.Next;
    end;
  finally
    Table1.RecNo := SavedRecNo;
    Table1.EnableControls;
  end;
  FDrawBox.Repaint;

end;

procedure TfmGISQSimple.cxComboBox_k2PropertiesChange(Sender: TObject);
begin
  GenerateQuery;
end;

procedure TfmGISQSimple.cxComboBox_k3PropertiesChange(Sender: TObject);
begin
  GenerateQuery;
end;

end.


