unit Lider.CG.Com.Express;

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
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt;

type
  TfmExprDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    BtnVerify: TButton;
    GB1: TGroupBox;
    GB2: TGroupBox;
    BtnAddField: TButton;
    BtnAddFunct: TButton;
    BtnClear: TButton;
    BtnUndo: TButton;
    lbFuncs: TListBox;
    lbColumns: TListBox;
    Group1: TGroupBox;
    BtnAdd: TSpeedButton;
    BtnSubct: TSpeedButton;
    BtnMult: TSpeedButton;
    BtnDiv: TSpeedButton;
    BtnIDiv: TSpeedButton;
    BtnMod: TSpeedButton;
    BtnEq: TSpeedButton;
    BtnDiff: TSpeedButton;
    BtnLT: TSpeedButton;
    BtnGT: TSpeedButton;
    BtnLE: TSpeedButton;
    BtnGE: TSpeedButton;
    BtnExp: TSpeedButton;
    BtnNOT: TSpeedButton;
    BtnAND: TSpeedButton;
    BtnOR: TSpeedButton;
    BtnOpen: TSpeedButton;
    Btn0: TSpeedButton;
    Btn1: TSpeedButton;
    Btn3: TSpeedButton;
    Btn2: TSpeedButton;
    Btn5: TSpeedButton;
    Btn4: TSpeedButton;
    Btn7: TSpeedButton;
    Btn6: TSpeedButton;
    Btn9: TSpeedButton;
    Btn8: TSpeedButton;
    BtnPeriod: TSpeedButton;
    Panel1: TPanel;
    LblSyntax: TLabel;
    LblDesc: TLabel;
    btnBetween: TSpeedButton;
    btnLike: TSpeedButton;
    btnIN: TSpeedButton;
    btnWithin: TSpeedButton;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    PopupMenu1: TPopupMenu;
    Within1: TMenuItem;
    EntirelyWithin1: TMenuItem;
    Contains1: TMenuItem;
    ContainsEntire1: TMenuItem;
    ContainsEntireNotEdgeTouch1: TMenuItem;
    EntirelyWithinNotEdgeTouch1: TMenuItem;
    Intersects1: TMenuItem;
    ExtentOverlaps1: TMenuItem;
    ShareCommonPoint1: TMenuItem;
    ShareCommonLine1: TMenuItem;
    LineCross1: TMenuItem;
    CommonPointOrLineCross1: TMenuItem;
    EdgeTouch1: TMenuItem;
    EdgeTouchOrIntersects1: TMenuItem;
    PointInPolygon1: TMenuItem;
    CentroidInPolygon1: TMenuItem;
    Identical1: TMenuItem;
    procedure BtnVerifyClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnAddFieldClick(Sender: TObject);
    procedure BtnAddFunctClick(Sender: TObject);
    procedure lbFuncsClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnUndoClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lbColumnsDrawItem(Control: TWinControl; Index: Integer; Rect:
      TRect; State: TOwnerDrawState);
    procedure BtnOpenClick(Sender: TObject);
    procedure lbColumnsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnWithinClick(Sender: TObject);
    procedure Within1Click(Sender: TObject);
  private
    { Private declarations }
    FLayer: TlicgBaseLayer;
    FGis: TlicgBaseGis;
    FFunctions: TStringList;
    FFunctionsDescr: TStringList;
    function VerifyExpression(ShwMsg: boolean): boolean;
    function FindFunction(const func: string; var Syntax, Description: string): boolean;
  public
    { Public declarations }
    function Enter(const SourceExpr: string; Gis: TlicgBaseGis; Layer:
      TlicgBaseLayer): Word;
  end;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.Consts,
  Lider.CG.Com.Expressions;

resourcestring
{.$IFDEF LANG_ENG}
  //SExpressionOK= 'Expression is right !';
  SCustomGlobal = 'A global User Defined Function';
  SExprNative = 'Return value of field %s of native database file of layer';
  //SCustomFunctionAdded= 'Return the value of User Defined Function %s';

  { suported functions }
  SAbs = 'Abs(<X>);The ABS function returns the absolute value of argument X';
  SArcTan = 'ArcTan(<X>);ArcTan calculates the arctangent of the given number X';
  SArea = 'Area( <Ent> );Returns the AREA of the entity';
  SPoints = 'Points( <Ent> );Returns the list of points for the entity';
  SCopy =
    'Copy(<String>, <Index>, <Count>);The Copy function returns a substring of a String containing Count characters starting at Index.' +
    CrLf + 'e.g.: Copy(NAME, 1, 10) + Copy(USERS.LAST_NAME, 1, 15)';
  SCos = 'Cos(<X>);Cos calculates the cosine of an angle X (in radians)';
  SExp =
    'Exp(<X>);Exp returns the value of e raised to the power of X, where e is the base of the natural logarithms';
  SFormat =
    'Format(<S:string>, <X1>, <X2>, ...);Format returns a formatted string assembled from a Format string S and a series of array arguments  (X1, X2,...)' +
    crlf + 'e.g.: FORMAT( ''%.2n %10.2n CCAT=%s'', AREA, PERIMETER, CCAT)';
  SFrac = 'Frac(<X>);Frac returns the fractional part of a real number X';
  SIf =
    'If(<Condition:boolean>, TrueResult, FalseResult);Returns TrueResult if Condition is True, or FalseResult otherwise' +
    crlf + 'e.g.: IF(AREA > 3000, AREA-1000, AREA)';
  SInt =
    'Int(<X>);Int returns the integer part of a real number. X is a real-type expression. The result is the integer part of X; that is, X rounded toward zero';
  SLength =
    'Length(<S:string>);Length returns the number of characters used in string S.';
  SLn =
    'Ln(<X>);Ln returns the natural log of a real expression. The Ln function returns the natural logarithm (Ln(e) = 1) of the real-type expression X';
  SLower =
    'Lower(<S:string>);Lower returns a string with the same text as the string passed in S, but with all letters converted to lowercase';
  SMaxExtentX = 'MaxExtentX(<Ent>);Returns maximum X Coor of entity';
  SMaxExtentY = 'MaxExtentY(<Ent>);Returns maximum Y Coor of entity';
  SMinExtentX = 'MinExtentX(<Ent>);Returns minimum X Coor of entity';
  SMinExtentY = 'MinExtentY(<Ent>);Returns minimum Y Coor of entity';
  SPerimeter = 'Perimeter(<Ent>);Returns the perimeter or length of the entity';
  SPI = 'PI;Pi the value of PI, 3.1415926535897932385';
  SPos =
    'Pos(<Substr:String>, <S:String>);Pos returns the index value of the first character in a specified substring that occurs in a given string' +
    crlf + 'e.g.: Pos(''SMITH'', ''JOHN SMITH'') returns 6.';
  SPower =
    'Power(<Base:double>, <Exponent:double>);Power raises Base to any power.\r\ne.g.: Power(Perimeter, 2.5)';
  SRound = 'Round(<X>);Round returns the value of X rounded to the nearest whole number';
  SSin = 'Sin(<X>);Sin returns the sine of the angle X in radians';
  SSqr = 'Sqr(<X>);The Sqr function returns the square of the argument X.';
  SSqrt =
    'Sqrt(<X>);Sqrt returns the square root of X. X is a floating-point expression.';
  STrunc =
    'Trunc(<X>);The Trunc function truncates a real-type value to an integer-type value.' +
    crlf + 'e.g.: TRUNC(PERIMETER)';
  SUpper =
    'Upper(<S:string>);The Upper function returns a string containing the same text as S, but with all 7-bit ASCII characters between ''a'' and ''z'' converted to uppercase.' +
    crlf + 'e.g.: UPPER(NATIVE(''NOMBRE''))';
  SCurDate = 'CurDate;Returns the current date';
  SFormateDateTime =
    'FormatDateTime(<S:string>, <Date>);Formats the date-and-time value given by DATE using the format given by S.' +
    crlf + 'e.g.1: FormatDateTime(''dd/mmm/yyyy'', Routes.DueDate)' + crlf +
    'e.g.2: FormatDateTime(''dd mmm yyyy'', Xbase1(''Born_Date'')';
  SFormatFloat =
    'FormatFloat(<S:string>, <X:double>);Formats the floating-point value given by X using the format string given by S.' +
    crlf + 'e.g.1: FORMATFLOAT(''$###, ###. ##'', IMPORTE)' + crlf +
    'e.g.2: FORMATFLOAT(''###, ##0.0000 '', NATIVE('' AREA ''))';
  SCentroidX = 'CentroidX(<Ent>);Returns the X Coor of the entity''s centroid.';
  SCentroidY = 'CentroidY(<Ent>);Returns the Y Coor of the entity''s centroid.';
  SType =
    'Type(<Ent>);Returns a string describing the entity type. \r\ne.g.: (Polygon), (Polyline), etc.';
  SCrLf = 'CrLf;Insert a new line in the result.';
  SColor = 'Color(<Ent>);Retrieve the entity''s color';
  SFillColor = 'FillColor(<Ent>);Retrieve the entity''s fill color';
  SBlack = 'Black;Returns the numeric value of color';
  SMaroon = 'Maroon;Returns the numeric value of color';
  SGreen = 'Green;Returns the numeric value of color';
  SOlive = 'Olive;Returns the numeric value of color';
  SNavy = 'Navy;Returns the numeric value of color';
  SPurple = 'Purple;Returns the numeric value of color';
  STeal = 'Teal;Returns the numeric value of color';
  SGray = 'Gray;Returns the numeric value of color';
  SSilver = 'Silver;Returns the numeric value of color';
  SRed = 'Red;Returns the numeric value of color';
  SLime = 'Lime;Returns the numeric value of color';
  SYellow = 'Yellow;Returns the numeric value of color';
  SBlue = 'Blue;Returns the numeric value of color';
  SFuchsia = 'Fuchsia;Returns the numeric value of color';
  SAqua = 'Aqua;Returns the numeric value of color';
  SWhite = 'White;Returns the numeric value of color';
  SRGB =
    'RGB(<Red>,<Green>,<Blue>);Returns a color that is a combination of red, green and blue (ranging from 0 to 255)';
  SText = 'Text(<Ent>);Returns the text of entity (if entity is of TEXT type)';
  SLayerName = 'LayerName(<Ent>);Returns layer name for entity';
  SDistance =
    'Distance(X1, Y1, X2, Y2);Returns distance from point (X1, Y1) to point (X2, Y2)';
  SLeft =
    'Left( <String>, <Integer> );Returns left most <Integer> characters from <String>';
  SRight =
    'Right( <String>, <Integer> );Returns right most <Integer> characters from <String>';
  SIsSelected =
    'IsSelected( <Ent> );Return if the entity is currently selected on the map';
  SYear = 'Year( <Date> );Return the year part of the date parameter';
  SMonth = 'Month( <Date> );Return the month part of the date parameter';
  SDay = 'Day( <Date> );Return the day part of the date parameter';
  SHour = 'Hour( <Time> );Return the hour part of the time parameter';
  SMin = 'Min( <Time> );Return the minutes part of the time parameter';
  SSec = 'Sec( <Time> );Return the seconds part of the time parameter';
  SMSec = 'MSec( <Time> );Return the milliseconds part of the time parameter';
  STo_Char = 'TO_CHAR( expression );Converts the expression to a string of chars';
  STo_Date =
    'TO_DATE( string_expression );Converts the string_expression to a date by using Windows Short Date Format';
  STo_Num = 'TO_NUM( string_expression );Converts the string_expression to a number';
  SDecode =
    'DECODE(base_expr, compare1, value1, compare2, value2...,default);Similar to a nested IF-THEN-ELSE, base_expr compare against compare1,compare2,etc.';
  SExpressionOkay = 'Expression is Okay';
{.$ENDIF}
(*

{$IFDEF LANG_SPA}
//SExpressionOK= 'La expresion es correcta !';
SCustomGlobal = 'Una UDF global';
SExprNative = 'Valor de retorno del campo %s de la tabla adjunta a la capa';
//SCustomFunctionAdded= 'Valor de retorno de UDF %s';

{ suported functions }
SAbs = 'Abs(<X>);La funcion ABS regresa el valor absoluto del argumento X';
SArcTan = 'ArcTan(<X>);ArcTan calcula el arco tangente del numbero dado X';
SArea = 'Area( <Ent> );Regrea el AREA de la entidad';
SPoints = 'Points( <Ent> );Regresa la lista de puntos de la entidad';
SCopy = 'Copy(<String>, <Index>, <Count>);La funcion COPY regresa una subcadena de una cadena conteniendo COUNT caracteres iniciando en Index.' + CrLf +
'e.g.: Copy(NAME, 1, 10) + Copy(USERS.LAST_NAME, 1, 15)';
SCos = 'Cos(<X>);Cos calcula el coseno del angulo X (en radianes)';
SExp = 'Exp(<X>);Exp regresa el valor de e elevado a la X potencia, donde e es la base de los logaritmos naturales';
SFormat = 'Format(<S:string>, <X1>, <X2>, ...);Format regresa una cadena formateada basado en una cadena de formato S y una serie variable de argumentos (X1, X2,...)' + crlf +
'e.g.: FORMAT( ''%.2n %10.2n CCAT=%s'', AREA, PERIMETER, CCAT)';
SFrac = 'Frac(<X>);Frac regresa la parte fraccional de un numero real X';
SIf = 'If(<Condition:boolean>, TrueResult, FalseResult);Regresa TrueResult si Condition es True, o FalseResult de lo contrario' + crlf +
'e.g.: IF(AREA > 3000, AREA-1000, AREA)';
SInt = 'Int(<X>);Int regresa la parte entera de un numero real. X es una expresion de tipo real. El resultado es la parte entera de X; esto es, X redondeado mas cerca al cero';
SLength = 'Length(<S:string>);Length returns the number of characters used in string S.';
SLn = 'Ln(<X>);Ln regresa el logaritmo natural de una expresion real. La funcion Ln regresa el ogaritmo natural (Ln(e) = 1) de la expresion de tipo real X';
SLower = 'Lower(<S:string>);Lower regresa una cadena con el mismo texto que la cadena pasada en S, pero con todas las letras convertidas a minusculas';
SMaxExtentX = 'MaxExtentX(<Ent>);Regresa maxima Coorenada en X de la entidad';
SMaxExtentY = 'MaxExtentY(<Ent>);Regresa maxima Coorenada en Y de la entidad';
SMinExtentX = 'MinExtentX(<Ent>);Regresa minima Coorenada en X de la entidad';
SMinExtentY = 'MinExtentY(<Ent>);Regresa minima Coorenada en Y de la entidad';
SPerimeter = 'Perimeter(<Ent>);Regresa el perimetro o la longitud de la entidad';
SPI = 'PI;Pi the value of PI, 3.1415926535897932385';
SPos = 'Pos(<Substr:String>, <S:String>);Pos regresa el valor indice de el primer caracter en una especificada subcadena que ocurre en una dada cadena' + crlf +
'e.g.: Pos(''SMITH'', ''JOHN SMITH'') returns 6.';
SPower = 'Power(<Base:double>, <Exponent:double>);Power eleva Base a cualquier potencia.\r\ne.g.: Power(Perimeter, 2.5)';
SRound = 'Round(<X>);Round regresa el valor de X redondeado al mas cercano numero entero';
SSin = 'Sin(<X>);Sin regresa el seno del angulo X (dado en radianes)';
SSqr = 'Sqr(<X>);La funcion Sqr regresa el cuadrado del argumento X';
SSqrt = 'Sqrt(<X>);Sqrt regresa la raiz cuadrada de X. X es una expreson de punto flotante';
STrunc = 'Trunc(<X>);La funcion Trunc trunca un valor de tipo real a un valor de tipo entero' + crlf +
'e.g.: TRUNC(PERIMETER)';
SUpper = 'Upper(<S:string>);La funcion Upper regresa una cadena conteniendo el mismo texto que S,pero con todos los caracters de 7-bit entre ''a'' y ''z'' convertidos a mayusculas' + crlf +
'e.g.: UPPER(NATIVE(''NOMBRE''))';
SCurDate = 'CurDate;Regresa la fecha actual';
SFormateDateTime = 'FormatDateTime(<S:string>, <Date>);Formatea el valor de fecha-hora dado por DATE usando el formato dado en S' + crlf +
'e.g.1: FormatDateTime(''dd/mmm/yyyy'', Routes.DueDate)' + crlf +
'e.g.2: FormatDateTime(''dd mmm yyyy'', Xbase1(''Born_Date'')';
SFormatFloat = 'FormatFloat(<S:string>, <X:double>);Formatea el valor de punto flotante dado por X usando la cadena de formateo dado por S' + crlf +
'e.g.1: FORMATFLOAT(''$###, ###. ##'', IMPORTE)'+crlf+
'e.g.2: FORMATFLOAT(''###, ##0.0000 '', NATIVE('' AREA ''))';
SCentroidX = 'CentroidX(<Ent>);Regresa la Coorenada en X del centroide de la entidad';
SCentroidY = 'CentroidY(<Ent>);Regresa la Coorenada en Y del centroide de la entidad';
SType = 'Type(<Ent>);Regresa una cadena describiendo el tipo de entidad. \r\ne.g.: (Polygon), (Polyline), etc.';
SCrLf = 'CrLf;Agrega un cambio de linea al resultado';
SColor = 'Color(<Ent>);Regresa el color de la entidad';
SFillColor = 'FillColor(<Ent>);Regresa el color de relleno de la entidad';
SBlack = 'Black;Regresa el valor numerico de este color';
SMaroon = 'Maroon;Regresa el valor numerico de este color';
SGreen = 'Green;Regresa el valor numerico de este color';
SOlive = 'Olive;Regresa el valor numerico de este color';
SNavy = 'Navy;Regresa el valor numerico de este color';
SPurple = 'Purple;Regresa el valor numerico de este color';
STeal = 'Teal;Regresa el valor numerico de este color';
SGray = 'Gray;Regresa el valor numerico de este color';
SSilver = 'Silver;Regresa el valor numerico de este color';
SRed = 'Red;Regresa el valor numerico de este color';
SLime = 'Lime;Regresa el valor numerico de este color';
SYellow = 'Yellow;Regresa el valor numerico de este color';
SBlue = 'Blue;Regresa el valor numerico de este color';
SFuchsia = 'Fuchsia;Regresa el valor numerico de este color';
SAqua = 'Aqua;Regresa el valor numerico de este color';
SWhite = 'White;Regresa el valor numerico de este color';
SRGB = 'RGB(<Red>,<Green>,<Blue>);Regresa un color que es la combinacion de rojo, verde y azul (en el rango de 0 a 255)';
SText = 'Text(<Ent>);Regresa el texto de la entidad, si la entidad es de tipo TEXTO';
SLayerName = 'LayerName(<Ent>);Regresa el nombre de la capa para la entidad';
SDistance = 'Distance(X1, Y1, X2, Y2);Regresa la distancia desde el punto (X1, Y1) hasta el punto (X2, Y2)';
SLeft = 'Left( <String>, <Integer> );Regresa los n caracteres mas a la izquierda de una cadena';
SRight = 'Right( <String>, <Integer> );Regresa los n caracteres mas a la derecha de una cadena';
SIsSelected = 'IsSelected( <Ent> );Regresa si la entidad esta actualmente seleccionada';
SYear = 'Year( <Date> );Regresa la parte del año de un parametro de fecha';
SMonth = 'Month( <Date> );Regresa la parte del mes de un parametro de fecha';
SDay = 'Day( <Date> );Regresa la parte del dia de un parametro de fecha';
SHour = 'Hour( <Time> );Regresa la parte de la hora de un parametro de fecha';
SMin = 'Min( <Time> );Regresa la parte del minuto de un parametro de fecha';
SSec = 'Sec( <Time> );Regresa la parte del segundo de un parametro de fecha';
SMSec = 'MSec( <Time> );Regresa la parte del milisegundo de un parametro de fecha';
STo_Char = 'TO_CHAR( expression );Convierte la expresion a una cadena de caracteres';
STo_Date = 'TO_DATE( string_expression );Convierte la expresion "string_expression" a una fecha usando la fecha corta de Windows';
STo_Num = 'TO_NUM( string_expression );Convierte la expresion alfanumerica a numerica';
SDecode = 'DECODE(base_expr, compare1, value1, compare2, value2...,default);Similar a un IF-THEN-ELSE anidado, base_expr compara contra compare1,compare2,etc.';
SExpressionOkay = 'La expresion es correcta';
{.$ENDIF}

{$IFDEF LANG_SWE}
  //SExpressionOK= 'Expression is right !';
  SCustomGlobal= 'A global User Defined Function';
  SExprNative= 'Return value of field %s of native database file of layer';
  //SCustomFunctionAdded= 'Return the value of User Defined Function %s';

  { suported functions }
  SAbs= 'Abs(<X>);The ABS function returns the absolute value of argument X';
  SArcTan = 'ArcTan(<X>);ArcTan calculates the arctangent of the given number X';
  SArea= 'Area( <Ent> );Returns the AREA of the entity';
  SPoints='Points( <Ent> );Returns the list of points for the entity';
  SCopy= 'Copy(<String>, <Index>, <Count>);The Copy function returns a substring of a String containing Count characters starting at Index.'+CrLf+
           'e.g.: Copy(NAME, 1, 10) + Copy(USERS.LAST_NAME, 1, 15)';
  SCos= 'Cos(<X>);Cos calculates the cosine of an angle X (in radians)';
  SExp= 'Exp(<X>);Exp returns the value of e raised to the power of X, where e is the base of the natural logarithms';
  SFormat= 'Format(<S:string>, <X1>, <X2>, ...);Format returns a formatted string assembled from a Format string S and a series of array arguments  (X1, X2,...)'+crlf+
             'e.g.: FORMAT( ''%.2n %10.2n CCAT=%s'', AREA, PERIMETER, CCAT)';
  SFrac= 'Frac(<X>);Frac returns the fractional part of a real number X';
  SIf = 'If(<Condition:boolean>, TrueResult, FalseResult);Returns TrueResult if Condition is True, or FalseResult otherwise'+crlf+
          'e.g.: IF(AREA > 3000, AREA-1000, AREA)';
  SInt= 'Int(<X>);Int returns the integer part of a real number. X is a real-type expression. The result is the integer part of X; that is, X rounded toward zero';
  SLength= 'Length(<S:string>);Length returns the number of characters used in string S.';
  SLn= 'Ln(<X>);Ln returns the natural log of a real expression. The Ln function returns the natural logarithm (Ln(e) = 1) of the real-type expression X';
  SLower= 'Lower(<S:string>);Lower returns a string with the same text as the string passed in S, but with all letters converted to lowercase';
  SMaxExtentX= 'MaxExtentX(<Ent>);Returns maximum X Coor of entity';
  SMaxExtentY= 'MaxExtentY(<Ent>);Returns maximum Y Coor of entity';
  SMinExtentX= 'MinExtentX(<Ent>);Returns minimum X Coor of entity';
  SMinExtentY= 'MinExtentY(<Ent>);Returns minimum Y Coor of entity';
  SPerimeter= 'Perimeter(<Ent>);Returns the perimeter or length of the entity';
  SPI= 'PI;Pi the value of PI, 3.1415926535897932385';
  SPos= 'Pos(<Substr:String>, <S:String>);Pos returns the index value of the first character in a specified substring that occurs in a given string'+crlf+
          'e.g.: Pos(''SMITH'', ''JOHN SMITH'') returns 6.';
  SPower='Power(<Base:double>, <Exponent:double>);Power raises Base to any power.\r\ne.g.: Power(Perimeter, 2.5)';
  SRound='Round(<X>);Round returns the value of X rounded to the nearest whole number';
  SSin='Sin(<X>);Sin returns the sine of the angle X in radians';
  SSqr='Sqr(<X>);The Sqr function returns the square of the argument X.';
  SSqrt='Sqrt(<X>);Sqrt returns the square root of X. X is a floating-point expression.';
  STrunc='Trunc(<X>);The Trunc function truncates a real-type value to an integer-type value.'+crlf+
          'e.g.: TRUNC(PERIMETER)';
  SUpper='Upper(<S:string>);The Upper function returns a string containing the same text as S, but with all 7-bit ASCII characters between ''a'' and ''z'' converted to uppercase.'+crlf+
           'e.g.: UPPER(NATIVE(''NOMBRE''))';
  SCurDate='CurDate;Returns the current date';
  SFormateDateTime='FormatDateTime(<S:string>, <Date>);Formats the date-and-time value given by DATE using the format given by S.'+crlf+
                     'e.g.1: FormatDateTime(''dd/mmm/yyyy'', Routes.DueDate)'+crlf+
                     'e.g.2: FormatDateTime(''dd mmm yyyy'', Xbase1(''Born_Date'')';
  SFormatFloat='FormatFloat(<S:string>, <X:double>);Formats the floating-point value given by X using the format string given by S.'+crlf+
                 'e.g.1: FORMATFLOAT(''$###,###.##'', IMPORTE)'+crlf+
                 'e.g.2: FORMATFLOAT(''###,##0.0000'', NATIVE(''AREA''))';
  SCentroidX='CentroidX(<Ent>);Returns the X Coor of the entity''s centroid.';
  SCentroidY='CentroidY(<Ent>);Returns the Y Coor of the entity''s centroid.';
  SType='Type(<Ent>);Returns a string describing the entity type. \r\ne.g.: (Polygon), (Polyline), etc.';
  SCrLf='CrLf;Insert a new line in the result.';
  SColor='Color(<Ent>);Retrieve the entity''s color';
  SFillColor='FillColor(<Ent>);Retrieve the entity''s fill color';
  SBlack='Black;Returns the numeric value of color';
  SMaroon='Maroon;Returns the numeric value of color';
  SGreen='Green;Returns the numeric value of color';
  SOlive='Olive;Returns the numeric value of color';
  SNavy='Navy;Returns the numeric value of color';
  SPurple='Purple;Returns the numeric value of color';
  STeal='Teal;Returns the numeric value of color';
  SGray='Gray;Returns the numeric value of color';
  SSilver='Silver;Returns the numeric value of color';
  SRed='Red;Returns the numeric value of color';
  SLime='Lime;Returns the numeric value of color';
  SYellow='Yellow;Returns the numeric value of color';
  SBlue='Blue;Returns the numeric value of color';
  SFuchsia='Fuchsia;Returns the numeric value of color';
  SAqua='Aqua;Returns the numeric value of color';
  SWhite='White;Returns the numeric value of color';
  SRGB='RGB(<Red>,<Green>,<Blue>);Returns a color that is a combination of red, green and blue (ranging from 0 to 255)';
  SText='Text(<Ent>);Returns the text of entity (if entity is of TEXT type)';
  SLayerName='LayerName(<Ent>);Returns layer name for entity';
  SDistance='Distance(X1, Y1, X2, Y2);Returns distance from point (X1, Y1) to point (X2, Y2)';
  SLeft='Left( <String>, <Integer> );Returns left most <Integer> characters from <String>';
  SRight='Right( <String>, <Integer> );Returns right most <Integer> characters from <String>';
  SIsSelected='IsSelected( <Ent> );Return if the entity is currently selected on the map';
  SYear='Year( <Date> );Return the year part of the date parameter';
  SMonth='Month( <Date> );Return the month part of the date parameter';
  SDay='Day( <Date> );Return the day part of the date parameter';
  SHour='Hour( <Time> );Return the hour part of the time parameter';
  SMin='Min( <Time> );Return the minutes part of the time parameter';
  SSec='Sec( <Time> );Return the seconds part of the time parameter';
  SMSec='MSec( <Time> );Return the milliseconds part of the time parameter';
  STo_Char='TO_CHAR( expression );Converts the expression to a string of chars';
  STo_Date='TO_DATE( string_expression );Converts the string_expression to a date by using Windows Short Date Format';
  STo_Num='TO_NUM( string_expression );Converts the string_expression to a number';
  SDecode='DECODE(base_expr, compare1, value1, compare2, value2...,default);Similar to a nested IF-THEN-ELSE, base_expr compare against compare1,compare2,etc.';

  SExpressionOkay= 'Expression is Okay';
{$ENDIF}
*)

function TfmExprDlg.Enter(const SourceExpr: string; Gis: TlicgBaseGis; Layer:
  TlicgBaseLayer): Word;
var
  I: integer;
  Identifier, LayerName: string;
  Accept: Boolean;

  procedure AddFunc(const NewFunct: string);
  var
    func, descr: string;
    P: integer;
  begin
    p := AnsiPos(';', NewFunct);
    if p = 0 then
    begin
      MessageToUser('Error in resource string !', smsgerror, MB_ICONERROR);
      Exit;
    end;
    func := Copy(NewFunct, 1, p - 1);
    descr := Copy(NewFunct, p + 1, Length(NewFunct));
    FFunctions.Add(func);
    FFunctionsDescr.Add(descr);
  end;

begin
  FGis := Gis;
  FLayer := Layer;

  Memo1.Text := SourceExpr;

  FFunctions := TStringList.create;
  FFunctionsDescr := TStringList.create;
  AddFunc(SAbs);
  AddFunc(SArcTan);
  AddFunc(SArea);
  AddFunc(SPoints);
  AddFunc(SCopy);
  AddFunc(SCos);
  AddFunc(SExp);
  AddFunc(SFormat);
  AddFunc(SFrac);
  AddFunc(SIf);
  AddFunc(SInt);
  AddFunc(SLength);
  AddFunc(SLn);
  AddFunc(SLower);
  AddFunc(SMaxExtentX);
  AddFunc(SMaxExtentY);
  AddFunc(SMinExtentX);
  AddFunc(SMinExtentY);
  AddFunc(SPerimeter);
  AddFunc(SPI);
  AddFunc(SPos);
  AddFunc(SPos);
  AddFunc(SPower);
  AddFunc(SRound);
  AddFunc(SSin);
  AddFunc(SSqr);
  AddFunc(SSqrt);
  AddFunc(STrunc);
  AddFunc(SUpper);
  AddFunc(SCurDate);
  AddFunc(SFormateDateTime);
  AddFunc(SFormatFloat);
  AddFunc(SCentroidX);
  AddFunc(SCentroidY);
  AddFunc(SType);
  AddFunc(SCrLf);
  AddFunc(SColor);
  AddFunc(SFillColor);
  AddFunc(SBlack);
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
  AddFunc(SWhite);
  AddFunc(SRGB);
  AddFunc(SText);
  AddFunc(SLayerName);
  AddFunc(SDistance);
  AddFunc(SLeft);
  AddFunc(SRight);
  AddFunc(SIsSelected);
  AddFunc(SYear);
  AddFunc(SMonth);
  AddFunc(SDay);
  AddFunc(SHour);
  AddFunc(SMin);
  AddFunc(SSec);
  AddFunc(SMSec);
  AddFunc(STo_Char);
  AddFunc(STo_Date);
  AddFunc(STo_Num);
  AddFunc(SDecode);

  {populate listbox of functions}
  lbFuncs.Clear;
  for I := 0 to FFunctions.Count - 1 do
    lbFuncs.Items.AddObject(FFunctions[I], nil);
  lbFuncs.ItemIndex := 0;
  lbFuncs.OnClick(lbFuncs);

  FLayer.PopulateFieldList(lbColumns.Items, True);

  if Assigned(FGis.OnStartExternalPopulate) and Assigned(FGis.OnExternalPopulate) then
  begin
    Accept := True;
    FGis.OnStartExternalPopulate(FGis, FLayer.Name, Accept);
    if Accept then
    begin
      Identifier := '';
      FGis.OnExternalPopulate(FGis, FLayer.Name, Identifier);
      LayerName := FLayer.Name;
      if AnsiPos(#32, LayerName) > 0 then
        Identifier := '[' + LayerName + ']';
      while Length(Identifier) > 0 do
      begin
        if AnsiPos(#32, Identifier) > 0 then
          Identifier := '[' + Identifier + ']';
        lbColumns.Items.AddObject(LayerName + '.' + Identifier, Pointer(1));
        Identifier := '';
        FGis.OnExternalPopulate(FGis, FLayer.Name, Identifier);
      end;
      if Assigned(FGis.OnEndExternalPopulate) then
        FGis.OnEndExternalPopulate(FGis, FLayer.Name);
    end;
  end;

{$IFDEF LANG_SPA}
  Caption := 'Asistente de Expresiones';
  gb2.Caption := '&Funciones';
  BtnAddFunct.Caption := '&Agrega';
  BtnAddField.Caption := 'A&grega';
  GB1.Caption := 'Tabla &Nativa y UDFs (User Defined Functions)';
  Group1.caption := '&Operadores';
  Label1.Caption := 'Expresion';
  BtnVerify.Caption := '&Verifica';
  BtnClear.caption := '&Clear';
  BtnUndo.Caption := '&Deshacer';
  OKBtn.caption := 'Aceptar';
  CancelBtn.caption := 'Cancelar';
{$ENDIF}

  Result := ShowModal;
end;

function TfmExprDlg.VerifyExpression(ShwMsg: boolean): boolean;
var
  Expr: string;
  MainExpr: TlicgBaseMainExpr;
begin
  //result:=false;
  Expr := Memo1.Text;
  if Length(Expr) = 0 then
  begin
    result := true;
    exit;
  end;
  MainExpr := Licad.CreateMainExpr(FGis, FLayer);
  try
    MainExpr.ParseExpression(Expr);
    if ShwMsg then
      MessageToUser(SExpressionOkay, smsgwarning, MB_ICONINFORMATION);
  finally
    MainExpr.Free;
  end;
  result := true;
end;

procedure TfmExprDlg.BtnVerifyClick(Sender: TObject);
begin
  VerifyExpression(true);
end;

procedure TfmExprDlg.OKBtnClick(Sender: TObject);
begin
  if not VerifyExpression(false) then
    ModalResult := mrNone;
end;

procedure TfmExprDlg.BtnAddClick(Sender: TObject);
begin
  with TSpeedButton(Sender) do
  begin
    Memo1.SelText := Caption;
    ActiveControl := Memo1;
  end;
end;

procedure TfmExprDlg.BtnAddFieldClick(Sender: TObject);
begin
  if lbColumns.ItemIndex < 0 then
    exit;
  with lbColumns do
  begin
    Memo1.SelText := Items[ItemIndex];
    ActiveControl := Memo1;
  end;
end;

procedure TfmExprDlg.BtnAddFunctClick(Sender: TObject);
var
  syntax, descr: string;
begin
  if lbFuncs.ItemIndex < 0 then
    Exit;
  with lbFUncs do
    if (Items.Objects[ItemIndex] = nil) and FindFunction(Items[ItemIndex],
      syntax, descr) then
    begin
      Memo1.SelText := syntax;
      ActiveControl := Memo1;
    end
    else
    begin
      Memo1.SelText := Items[ItemIndex];
      ActiveControl := Memo1;
    end;
end;

procedure TfmExprDlg.lbFuncsClick(Sender: TObject);
var
  Syntax, Description: string;
begin
  LbColumns.ItemIndex := -1;
  with lbFuncs do
    if (Items.Objects[ItemIndex] = nil) and FindFunction(Items[ItemIndex],
      Syntax, Description) then
    begin
      LblSyntax.Caption := Syntax;
      LblDesc.Caption := Description;
    end
    else
    begin
      LblSyntax.Caption := Items[ItemIndex];
      LblDesc.Caption := SCustomGlobal;
    end;
end;

function TfmExprDlg.FindFunction(const func: string; var syntax, description:
  string): boolean;
var
  Index: integer;
begin
  {the expressions}
  result := false;
  Index := FFunctions.IndexOf(func);
  if Index >= 0 then
  begin
    syntax := FFunctions[Index];
    description := FFunctionsDescr[Index];
    result := true;
  end;
end;

procedure TfmExprDlg.BtnClearClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TfmExprDlg.BtnUndoClick(Sender: TObject);
begin
  Memo1.Perform(EM_UNDO, 0, LPARAM(0));
end;

procedure TfmExprDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

procedure TfmExprDlg.lbColumnsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with lbColumns.Canvas do
  begin
    FillRect(Rect);
    Font := lbColumns.Font;
    if odSelected in State then
      Font.Color := clHighlightText
    else
    begin
      if Index in [0, 1] then
        Font.Color := clMaroon
      else if Longint(lbColumns.Items.Objects[Index]) = 0 then
        Font.Color := clWindowText
      else
        Font.Color := clBlue;
    end;
    TextOut(Rect.Left, Rect.Top, lbColumns.Items[Index]);
  end;
end;

procedure TfmExprDlg.BtnOpenClick(Sender: TObject);
begin
  Memo1.SelText := '(' + Memo1.SelText + ')';
  ActiveControl := Memo1;
end;

procedure TfmExprDlg.lbColumnsClick(Sender: TObject);
var
  FieldName, Syntax: string;
  Index: Integer;
begin
  LbFuncs.ItemIndex := -1;
  Index := lbColumns.ItemIndex;
  if Index < 0 then
    exit;
  Syntax := lbColumns.Items[Index];
  Index := AnsiPos('.', Syntax);
  FieldName := Copy(Syntax, Index + 1, Length(Syntax));
  LblSyntax.Caption := Syntax;
  LblDesc.Caption := Format(SExprNative, [FieldName]);
end;

procedure TfmExprDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFunctions.free;
  FFunctionsDescr.free;
end;

procedure TfmExprDlg.btnWithinClick(Sender: TObject);
var
  TmpPt: TPoint;
begin
  TmpPt := Self.ClientToScreen(Point(btnWithin.Left, btnWithin.Top + btnWithin.Height));
  Popupmenu1.Popup(TmpPt.x, TmpPt.y);
end;

procedure TfmExprDlg.Within1Click(Sender: TObject);
begin
  Memo1.SelText := #32 + StringReplace(UpperCase((Sender as TMenuItem).Caption),
    '&', '', [rfReplaceAll]) + #32;
  ActiveControl := Memo1;
end;

end.


