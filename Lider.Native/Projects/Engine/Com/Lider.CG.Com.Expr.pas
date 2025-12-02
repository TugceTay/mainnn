unit Lider.CG.Com.Expr;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Math,
  SysUtils,
  Classes;

type
  TExprType = (ttString, ttFloat, ttInteger, ttBoolean);

  ecExpressionClass = (ecEntExpr, ecQueryLayerExpr, ecQueryScopeExpr,
    ecQueryVectorExpr, ecGraphicOperatorExpr, ecVectorExpr);

  TExpression = class
  private
    function GetMaxLen: Integer;
  protected
    function GetAsString: string; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetExprType: TExprType; virtual; abstract;
    function GetMaxString: string; virtual;
  public
    function ExpressionClass: ecExpressionClass; virtual; abstract;
    function CanReadAs(aExprType: TExprType): Boolean;
    {means 'can be interpreted as'. Sort of}
    property MaxString: string read GetMaxString;
    property AsString: string read GetAsString;
    property AsFloat: Double read GetAsFloat;
    property AsInteger: Integer read GetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean;
    property ExprType: TExprType read GetExprType;
    property MaxLen: Integer read GetMaxLen;
  end;

  TStringLiteral = class(TExpression)
  private
    FAsString: string;
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(const aAsString: string);
  end;

  TFloatLiteral = class(TExpression)
  private
    FAsFloat: Double;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsFloat: Double);
  end;

  TIntegerLiteral = class(TExpression)
  private
    FAsInteger: Integer;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsInteger: Integer);
  end;

  TBooleanLiteral = class(TExpression)
  private
    FAsBoolean: Boolean;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aAsBoolean: Boolean);
  end;

  TParameterList = class(TList)
  private
    function GetAsString(i: Integer): string;
    function GetAsFloat(i: Integer): Double;
    function GetAsInteger(i: Integer): Integer;
    function GetAsBoolean(i: Integer): Boolean;
    function GetExprType(i: Integer): TExprType;
    function GetParam(i: Integer): TExpression;
  public
    destructor Destroy; override;
    property Param[i: Integer]: TExpression read GetParam;
    property ExprType[i: Integer]: TExprType read GetExprType;
    property AsString[i: Integer]: string read GetAsString;
    property AsFloat[i: Integer]: Double read GetAsFloat;
    property AsInteger[i: Integer]: Integer read GetAsInteger;
    property AsBoolean[i: Integer]: Boolean read GetAsBoolean;
  end;

  TFunction = class(TExpression)
  private
    FParameterList: TParameterList;
    function GetParam(n: Integer): TExpression;
    function GetParameterList: TParameterList;
  public
    constructor Create(aParameterList: TParameterList);
    destructor Destroy; override;
    function ParameterCount: Integer;
    property Param[n: Integer]: TExpression read GetParam;
    property ParameterList: TParameterList read GetParameterList;
  end;

  TTypeCast = class(TFunction)
  private
    operator: TExprType;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aParameterList: TParameterList; aOperator: TExprType);
  end;

  TMF = (mfTrunc, mfRound, mfAbs, mfArcTan, mfCos, mfExp, mfFrac, mfInt, mfLn,
    mfPi, mfSin, mfSqr, mfSqrt, mfPower);

  TMathExpression = class(TFunction)
  private
    operator: TMF;
    procedure CheckParameters;
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aParameterList: TParameterList; aOperator: TMF);
  end;

  TSF = (sfUpper, sfLower, sfCopy, sfPos, sfLength, sfLTrim, sfRTrim, sfTrim);

  TStringExpression = class(TFunction)
  private
    operator: TSF;
    procedure CheckParameters;
  protected
    function GetMaxString: string; override;
    function GetAsString: string; override;
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aParameterList: TParameterList; aOperator: TSF);
  end;

  TConditional = class(TFunction)
  private
    procedure CheckParameters;
    function Rex: TExpression;
  protected
    function GetMaxString: string; override;
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  end;

type
  TOperator = (opNot, opExp, opMult, opDivide, opDiv, opMod, opAnd, opShl, opShr,
    opPlus, opMinus, opOr, opXor, opEq, opNEQ, opLT, opGT, opLTE, opGTE);

  TOperators = set of TOperator;

  TUnaryOp = class(TExpression)
  private
    Operand: TExpression;
    OperandType: TExprType;
    operator: TOperator;
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand: TExpression);
    destructor Destroy; override;
  end;

  TBinaryOp = class(TExpression)
  private
    Operand1, Operand2: TExpression;
    operator: TOperator;
    OperandType: TExprType;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
    destructor Destroy; override;
  end;

  TRelationalOp = class(TExpression)
  private
    Operand1, Operand2: TExpression;
    operator: TOperator;
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
    destructor Destroy; override;
  end;

  EExpression = class(Exception);

  { additional functions }
  { additional functions }
  TASCIIExpr = class(TFunction)
  protected
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList);
  end;

  TLeftExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  end;

  TRightExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  end;

  { This function is used exclusively for the LIKE predicate in SQL }
  TLikePos = (lpNone, lpLeft, lpMiddle, lpRight);

  TLikeCode = (lcSingle, lcMultiple);

  TLikeItem = class(TObject)
  public
    LikeText: string; { text to find }
    LikePos: TLikePos; { text must go at left, middle, right or on a column }
    LikeCode: TLikeCode;
  end;

  TLikeList = class(TObject)
  private
    fItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TLikeItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TLikeItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLikeItem read GetItem; default;
  end;

  TSQLLikeExpr = class(Tfunction)
  private
    LikeList: TLIKEList;
    FIsNotLike: Boolean;
    function SQLPos(var Start: Integer; const Substr, Str: string): Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetExprtype: TExprtype; override;
  public
    constructor Create(ParameterList: TParameterList; IsNotLike: Boolean);
    destructor Destroy; override;
    procedure AddToList(Like: TLikeItem);
  end;

  { TSQLInPredicateExpr }
  { This function is used exclusively for the IN predicate in SQL SELECT
     something like this : SELECT * FROM customer WHERE CustNo IN (1,10,8) }
  TSQLInPredicateExpr = class(Tfunction)
  private
    FIsNotIn: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetExprtype: TExprtype; override;
  public
    constructor Create(ParameterList: TParameterList; IsNotIn: Boolean);
  end;

  TBetweenExpr = class(Tfunction)
  private
    FIsNotBetween: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetExprtype: TExprtype; override;
  public
    constructor Create(ParameterList: TParameterList; IsNotBetween: Boolean);
  end;

  TCaseWhenElseExpr = class(TFunction)
  private
    FElseExpr: TExpression;
    FThenParamList: TParameterList;
    procedure CheckParameters;
  protected
    function GetMaxString: string; override;
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprtype: TExprtype; override;
  public
    constructor Create(WhenParamList: TParameterList; ThenParamList:
      TParameterList; ElseExpr: TExpression);
    destructor Destroy; override;
  end;

  TDecodeExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsBoolean: Boolean; override;
    function GetExprtype: TExprtype; override;
    function GetMaxString: string; override;
  public
    constructor Create(ParameterList: TParameterList);
  end;

  {Evaluate FormatDateTime('dd/mmm/yyyy', 32767)}
  TFormatDateTimeExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  end;

  {Evaluate FormatFloat('###,###,##0.00', 12345.567)}
  TFormatFloatExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  end;

  {Evaluate Format(Format,Args)}
  TFormatExpr = class(TFunction)
  protected
    function GetAsString: string; override;
    function GetExprType: TExprType; override;
  end;

  TDecodeKind = (dkYear, dkMonth, dkDay, dkHour, dkMin, dkSec, dkMSec);

  { supports syntax: YEAR(expr), MONTH(expr), DAY(expr), HOUR(expr), MIN(expr), SEC(expr), MSEC(expr)}
  TDecodeDateTimeExpr = class(TFunction)
  private
    FDecodeKind: TDecodeKind;
  protected
    function GetAsInteger: Integer; override;
    function GetExprType: TExprType; override;
  public
    constructor Create(ParameterList: TParameterList; DecodeKind: TDecodeKind);
  end;

  {  MINOF(arg1,arg2, ..., argn), MAXOF(ARG1,ARG2, ... ,argn)
      hint by: Fady Geagea
  }
  TMinMaxOfExpr = class(Tfunction)
  private
    FIsMin: Boolean;
  protected
    function GetAsFloat: Double; override;
    function GetExprtype: TExprtype; override;
  public
    constructor Create(ParameterList: TParameterList; IsMin: Boolean);
  end;

const
  NBoolean: array[Boolean] of string[5] = ('False', 'True');

implementation

uses
  Lider.CG.Com.Lib;

const
  { to get a string representation of TExprType use NExprType[ExprType] }
  NExprType: array[TExprType] of string = ('String', 'Float', 'Integer', 'Boolean');

resourcestring
  SEXPR_WRONGWHENEXPR = 'Expression in Case must be boolean';
  SEXPR_WRONGTHENEXPR = 'Expressions in THEN section must be all of same type';
  SEXPR_UNKNOWNID = 'Unknown Identifier %s';
  SEXPR_OPERATORINCOMPAT = 'Operator %s incompatible with %s';
  SEXPR_CANNOTCASTTOSTRING = 'Cannot read %s as String';
  SEXPR_CANNOTCASTTOFLOAT = 'Cannot read %s as Float';
  SEXPR_CANNOTCASTTOINTEGER = 'Cannot read %s as Integer';
  SEXPR_CANNOTCASTTOBOOLEAN = 'Cannot read %s as boolean';
  SEXPR_WRONGUNARYOP = '%s is not simple unary operator';
  SEXPR_WRONGBINARYOP = '%s is not a simple binary operator';
  SEXPR_WRONGBOOLEANOP = 'cannot apply %s to boolean operands';
  SEXPR_WRONGRELATIONALOP = '%s is not relational operator';
  SEXPR_WRONGPARAMETER = 'Invalid parameter to %s';
  SEXPR_INVALIDPARAMETERTO = 'Invalid parameter to %s';

const
  NOperator: array[TOperator] of string = ('opNot', 'opExp', 'opMult',
    'opDivide', 'opDiv', 'opMod', 'opAnd', 'opShl', 'opShr', 'opPlus', 'opMinus',
    'opOr', 'opXor', 'opEq', 'opNEQ', 'opLT', 'opGT', 'opLTE', 'opGTE');
  RelationalOperators =[opEQ, opNEQ, opLT, opGT, opLTE, opGTE];

function ResultType(operator: TOperator; OperandType: TExprType): TExprType;

  procedure NotAppropriate;
  begin
    Result := ttString;
    raise EExpression.CreateFmt(SEXPR_OPERATORINCOMPAT, [NOperator[operator],
      NExprType[OperandType]])
  end;

begin
  case OperandType of
    ttString:
      case operator of
        opPlus:
          Result := ttString;
        opEq..opGTE:
          Result := ttBoolean;
      else
        NotAppropriate;
      end;
    ttFloat:
      case operator of
        opExp, opMult, opDivide, opPlus, opMinus:
          Result := ttFloat;
        opEq..opGTE:
          Result := ttBoolean;
      else
        NotAppropriate;
      end;
    ttInteger:
      case operator of
        opNot, opMult, opDiv, opMod, opAnd, opShl, opShr, opPlus, opMinus, opOr, opXor:
          Result := ttInteger;
        opExp, opDivide:
          Result := ttFloat;
        opEq..opGTE:
          Result := ttBoolean;
      else
        NotAppropriate;
      end;
    ttBoolean:
      case operator of
        opNot, opAnd, opOr, opXor, opEq, opNEQ:
          Result := ttBoolean;
      else
        NotAppropriate;
      end;
  end
end;

function CommonType(Op1Type, Op2Type: TExprType): TExprType;
begin
  if Op1Type < Op2Type then
    Result := Op1Type
  else
    Result := Op2Type
end;

procedure Internal(Code: Integer);
begin
  raise EExpression.CreateFmt('Internal parser error. Code %d', [Code])
end;

function TExpression.GetMaxLen: Integer;
begin
  Result := 0;
  if ExprType = ttString then
    Result := Length(GetMaxString);
end;

function TExpression.GetMaxString: string;
begin
  Result := AsString;
end;

function TExpression.GetAsString: string;
begin
  case ExprType of
    ttString:
      raise EExpression.CreateFmt(SEXPR_CANNOTCASTTOSTRING, [NExprType[ExprType]]);
    ttFloat:
      Result := FloatToStr(AsFloat);
    ttInteger:
      Result := IntToStr(AsInteger);
    ttBoolean:
      Result := NBoolean[AsBoolean];
  end;
end;

function TExpression.GetAsFloat: Double;
begin
  Result := 0;
  case ExprType of
    // Allow cast expression to string... why not? (actually I need this)
    ttString:
      try
        if AsString <> '' then
          Result := StrToFloat(AsString);
      except
        //on EConvertError do
        //  Result := StrToDateTime(AsString);
      end;
    ttFloat:
      raise EExpression.CreateFmt(SEXPR_CANNOTCASTTOFLOAT, [NExprType[ExprType]]);
    ttInteger, ttBoolean:
      Result := AsInteger;
  end;
end;

function TExpression.GetAsInteger: Integer;
begin
  Result := 0;
  case ExprType of
    // Allow cast expression to string
    ttString:
      Result := StrToInt(AsString);
    //ttFloat : Result := FloatToStr (AsFloat);
    ttFloat, ttInteger:
      raise EExpression.CreateFmt(SEXPR_CANNOTCASTTOINTEGER, [NExprType[ExprType]]);
    ttBoolean:
      Result := Integer(AsBoolean);
  end;
end;

function TExpression.GetAsBoolean: Boolean;
begin
  raise EExpression.CreateFmt(SEXPR_CANNOTCASTTOBOOLEAN, [NExprType[ExprType]])
end;

function TExpression.CanReadAs(aExprType: TExprType): Boolean;
begin
  Result := Ord(ExprType) >= Ord(aExprType)
end;

function TStringLiteral.GetAsString: string;
begin
  Result := FAsString
end;

function TStringLiteral.GetExprType: TExprType;
begin
  Result := ttString
end;

constructor TStringLiteral.Create(const aAsString: string);
begin
  inherited Create;
  FAsString := aAsString
end;

function TFloatLiteral.GetAsString: string;
begin
  Result := FloatToStr(FAsFloat)
end;

function TFloatLiteral.GetAsFloat: Double;
begin
  Result := FAsFloat
end;

function TFloatLiteral.GetExprType: TExprType;
begin
  Result := ttFloat
end;

constructor TFloatLiteral.Create(aAsFloat: Double);
begin
  inherited Create;
  FAsFloat := aAsFloat
end;

function TIntegerLiteral.GetAsString: string;
begin
  Result := FloatToStr(FAsInteger)
end;

function TIntegerLiteral.GetAsFloat: Double;
begin
  Result := FAsInteger
end;

function TIntegerLiteral.GetAsInteger: Integer;
begin
  Result := FAsInteger
end;

function TIntegerLiteral.GetExprType: TExprType;
begin
  Result := ttInteger
end;

constructor TIntegerLiteral.Create(aAsInteger: Integer);
begin
  inherited Create;
  FAsInteger := aAsInteger
end;

function TBooleanLiteral.GetAsString: string;
begin
  Result := NBoolean[FAsBoolean]
end;

function TBooleanLiteral.GetAsFloat: Double;
begin
  Result := GetAsInteger
end;

function TBooleanLiteral.GetAsInteger: Integer;
begin
  Result := Integer(FAsBoolean)
end;

function TBooleanLiteral.GetAsBoolean: Boolean;
begin
  Result := FAsBoolean
end;

function TBooleanLiteral.GetExprType: TExprType;
begin
  Result := ttBoolean
end;

constructor TBooleanLiteral.Create(aAsBoolean: Boolean);
begin
  inherited Create;
  FAsBoolean := aAsBoolean
end;

function TUnaryOp.GetAsFloat: Double;
begin
  case operator of
    opMinus:
      Result := -Operand.AsFloat;
    opPlus:
      Result := Operand.AsFloat;
  else
    Result := inherited GetAsFloat;
  end
end;

function TUnaryOp.GetAsInteger: Integer;
begin
  Result := 0;
  case operator of
    opMinus:
      Result := -Operand.AsInteger;
    opPlus:
      Result := Operand.AsInteger;
    opNot:
      case OperandType of
        ttInteger:
          Result := not Operand.AsInteger;
        ttBoolean:
          Result := Integer(AsBoolean);
      else
        Internal(6);
      end;
  else
    Result := inherited GetAsInteger;
  end
end;

function TUnaryOp.GetAsBoolean: Boolean;
begin
  case operator of
    opNot:
      Result := not (Operand.AsBoolean)  else
    Result := inherited GetAsBoolean;
  end
end;

function TUnaryOp.GetExprType: TExprType;
begin
  Result := ResultType(operator, OperandType)
end;

constructor TUnaryOp.Create(aOperator: TOperator; aOperand: TExpression);
begin
  inherited Create;
  Operand := aOperand;
  Operator := aOperator;
  OperandType := Operand.ExprType;
  if not (operator in [opNot, opPlus, opMinus]) then
    raise EExpression.CreateFmt(SEXPR_WRONGUNARYOP, [NOperator[operator]])
end;

destructor TUnaryOp.Destroy;
begin
  Operand.Free;
  inherited Destroy
end;

function TBinaryOp.GetAsString: string;
begin
  Result := '';
  case ExprType of
    ttString:
      case operator of
        opPlus:
          Result := Operand1.AsString + Operand2.AsString;
      else
        Internal(10);
      end;
    ttFloat:
      Result := FloatToStr(AsFloat);
    ttInteger:
      Result := IntToStr(AsInteger);
    ttBoolean:
      Result := NBoolean[AsBoolean];
  end
end;

function TBinaryOp.GetAsFloat: Double;
begin
  Result := 0;
  case ExprType of
    ttFloat:
      case operator of
        opExp:
          Result := Exp(Operand2.AsFloat * Ln(Operand1.AsFloat));
        opPlus:
          Result := Operand1.AsFloat + Operand2.AsFloat;
        opMinus:
          Result := Operand1.AsFloat - Operand2.AsFloat;
        opMult:
          Result := Operand1.AsFloat * Operand2.AsFloat;
        opDivide:
          Result := Operand1.AsFloat / Operand2.AsFloat;
      else
        Internal(11);
      end;
    ttInteger:
      Result := AsInteger;
    ttBoolean:
      Result := Integer(AsBoolean);
  end
end;

function TBinaryOp.GetAsInteger: Integer;
begin
  Result := 0;
  case ExprType of
    ttInteger:
      case operator of
        opPlus:
          Result := Operand1.AsInteger + Operand2.AsInteger;
        opMinus:
          Result := Operand1.AsInteger - Operand2.AsInteger;
        opMult:
          Result := Operand1.AsInteger * Operand2.AsInteger;
        opDiv:
          Result := Operand1.AsInteger div Operand2.AsInteger;
        opMod:
          Result := Operand1.AsInteger mod Operand2.AsInteger;
        opShl:
          Result := Operand1.AsInteger shl Operand2.AsInteger;
        opShr:
          Result := Operand1.AsInteger shr Operand2.AsInteger;
        opAnd:
          Result := Operand1.AsInteger and Operand2.AsInteger;
        opOr:
          Result := Operand1.AsInteger or Operand2.AsInteger;
        opXor:
          Result := Operand1.AsInteger xor Operand2.AsInteger;
      else
        Internal(12);
      end;
    ttBoolean:
      Result := Integer(GetAsBoolean);
  end
end;

function TBinaryOp.GetAsBoolean: Boolean;
begin
  Result := false;
  case operator of
    opAnd:
      Result := Operand1.AsBoolean and Operand2.AsBoolean;
    opOr:
      Result := Operand1.AsBoolean or Operand2.AsBoolean;
    opXor:
      Result := Operand1.AsBoolean xor Operand2.AsBoolean;
  else
    Internal(13);
  end
end;

function TBinaryOp.GetExprType: TExprType;
begin
  GetExprType := ResultType(operator, OperandType)
end;

constructor TBinaryOp.Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
begin
  inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  OperandType := CommonType(Operand1.ExprType, Operand2.ExprType);
  if not (operator in [opExp, opMult..opXor]) then
    raise EExpression.CreateFmt(SEXPR_WRONGBINARYOP, [NOperator[operator]]);
end;

destructor TBinaryOp.Destroy;
begin
  Operand1.Free;
  Operand2.Free;
  inherited Destroy
end;

function TRelationalOp.GetAsString: string;
begin
  Result := NBoolean[AsBoolean]
end;

function TRelationalOp.GetAsFloat: Double;
begin
  Result := Integer(AsBoolean)
end;

function TRelationalOp.GetAsInteger: Integer;
begin
  Result := Integer(AsBoolean)
end;

function TRelationalOp.GetAsBoolean: Boolean;
begin
  Result := false;
  case CommonType(Operand1.ExprType, Operand2.ExprType) of
    ttBoolean:
      case operator of
        opEQ:
          Result := Operand1.AsBoolean = Operand2.AsBoolean;
        opNEQ:
          Result := Operand1.AsBoolean <> Operand2.AsBoolean;
      else
        raise EExpression.CreateFmt(SEXPR_WRONGBOOLEANOP, [NOperator[operator]]);
      end;

    ttInteger:
      case operator of
        opLT:
          Result := Operand1.AsInteger < Operand2.AsInteger;
        opLTE:
          Result := Operand1.AsInteger <= Operand2.AsInteger;
        opGT:
          Result := Operand1.AsInteger > Operand2.AsInteger;
        opGTE:
          Result := Operand1.AsInteger >= Operand2.AsInteger;
        opEQ:
          Result := Operand1.AsInteger = Operand2.AsInteger;
        opNEQ:
          Result := Operand1.AsInteger <> Operand2.AsInteger;
      end;

    ttFloat:
      case operator of
        opLT:
          Result := Operand1.AsFloat < Operand2.AsFloat;
        opLTE:
          Result := Operand1.AsFloat <= Operand2.AsFloat;
        opGT:
          Result := Operand1.AsFloat > Operand2.AsFloat;
        opGTE:
          Result := Operand1.AsFloat >= Operand2.AsFloat;
        opEQ:
          Result := Operand1.AsFloat = Operand2.AsFloat;
        opNEQ:
          Result := Operand1.AsFloat <> Operand2.AsFloat;
      end;

    ttString:
      case operator of
        opLT:
          Result := Operand1.AsString < Operand2.AsString;
        opLTE:
          Result := Operand1.AsString <= Operand2.AsString;
        opGT:
          Result := Operand1.AsString > Operand2.AsString;
        opGTE:
          Result := Operand1.AsString >= Operand2.AsString;
        opEQ:
          Result := Operand1.AsString = Operand2.AsString;
        opNEQ:
          Result := Operand1.AsString <> Operand2.AsString;
      end;
  end
end;

function TRelationalOp.GetExprType: TExprType;
begin
  Result := ttBoolean
end;

constructor TRelationalOp.Create(aOperator: TOperator; aOperand1, aOperand2: TExpression);
begin
  inherited Create;
  Operator := aOperator;
  Operand1 := aOperand1;
  Operand2 := aOperand2;
  if not (operator in RelationalOperators) then
    raise EExpression.CreateFmt(SEXPR_WRONGRELATIONALOP, [NOperator[operator]])
end;

destructor TRelationalOp.Destroy;
begin
  Operand1.Free;
  Operand2.Free;
  inherited Destroy
end;

function TParameterList.GetAsString(i: Integer): string;
begin
  Result := Param[i].AsString
end;

function TParameterList.GetAsFloat(i: Integer): Double;
begin
  Result := Param[i].AsFloat
end;

function TParameterList.GetAsInteger(i: Integer): Integer;
begin
  Result := Param[i].AsInteger
end;

function TParameterList.GetAsBoolean(i: Integer): Boolean;
begin
  Result := Param[i].AsBoolean
end;

function TParameterList.GetExprType(i: Integer): TExprType;
begin
  Result := Param[i].ExprType
end;

function TParameterList.GetParam(i: Integer): TExpression;
begin
  Result := TExpression(Items[i])
end;

destructor TParameterList.Destroy;
var
  i: Integer;
begin
  for i := 0 to (Count - 1) do
    TObject(Items[i]).Free;
  inherited Destroy
end;

{ TFunction }

function TFunction.GetParam(n: Integer): TExpression;
begin
  Result := FParameterList.Param[n]
end;

function TFunction.ParameterCount: Integer;
begin
  if (FParameterList <> nil) then
    ParameterCount := FParameterList.Count
  else
    ParameterCount := 0
end;

constructor TFunction.Create(aParameterList: TParameterList);
begin
  inherited Create;
  FParameterList := aParameterList
end;

destructor TFunction.Destroy;
begin
  FParameterList.Free;
  inherited Destroy
end;

function TFunction.GetParameterList: TParameterList;
begin
  Result := FParameterList;
end;

const
  NTypeCast: array[TExprType] of PChar = ('STRING', 'FLOAT', 'Integer', 'BOOLEAN');
  NMF: array[TMF] of PChar = ('TRUNC', 'ROUND', 'ABS', 'ARCTAN', 'COS', 'EXP',
    'FRAC', 'INT', 'LN', 'PI', 'SIN', 'SQR', 'SQRT', 'POWER');
  NSF: array[TSF] of PChar = ('UPPER', 'LOWER', 'COPY', 'POS', 'LENGTH', 'LTRIM',
    'RTRIM', 'TRIM');

function TStringExpression.GetMaxString: string;
begin
  CheckParameters;
  case operator of
    sfUpper, sfLower, sfLTrim, sfRTrim, sfTrim:
      Result := Param[0].MaxString;
    sfCopy:
      Result := Copy(Param[0].MaxString, Param[1].AsInteger, Param[2].AsInteger);
  else
    Result := inherited GetAsString;
  end
end;

function TStringExpression.GetAsString: string;
begin
  CheckParameters;
  case operator of
    sfUpper:
      Result := AnsiUpperCase(Param[0].AsString);
    sfLower:
      Result := AnsiLowerCase(Param[0].AsString);
    sfCopy:
      Result := Copy(Param[0].AsString, Param[1].AsInteger, Param[2].AsInteger);
    sfLTrim:
      Result := TrimLeft(Param[0].AsString);
    sfRTrim:
      Result := TrimRight(Param[0].AsString);
    sfTrim:
      Result := Trim(Param[0].AsString);
  else
    Result := inherited GetAsString;
  end
end;

function TStringExpression.GetAsInteger: Integer;
begin
  CheckParameters;
  case operator of
    sfPos:
      Result := AnsiPos(Param[0].AsString, Param[1].AsString);
    sfLength:
      Result := Length(Param[0].AsString);
  else
    Result := inherited GetAsInteger
  end
end;

function TStringExpression.GetExprType: TExprType;
begin
  case operator of
    sfUpper, sfLower, sfCopy, sfLTrim, sfRTrim, sfTrim:
      Result := ttString;
  else
    Result := ttInteger;
  end
end;

procedure TStringExpression.CheckParameters;
var
  OK: Boolean;
begin
  OK := false;
  case operator of
    sfUpper, sfLower, sfLength, sfLTrim, sfRTrim, sfTrim:
      OK := (ParameterCount = 1) and (Param[0].ExprType >= ttString);
    sfCopy:
      OK := (ParameterCount = 3) and (Param[0].ExprType >= ttString) and (Param[1].ExprType
        >= ttInteger) and (Param[2].ExprType >= ttInteger);
    sfPos:
      OK := (ParameterCount = 2) and (Param[0].ExprType >= ttString) and (Param[1].ExprType
        >= ttString);
  end;
  if not OK then
    raise EExpression.CreateFmt(SEXPR_WRONGPARAMETER, [NSF[operator]])
end;

constructor TStringExpression.Create(aParameterList: TParameterList; aOperator: TSF);
begin
  inherited Create(aParameterList);
  Operator := aOperator
end;

function TMathExpression.GetAsFloat: Double;
begin
  CheckParameters;
  case operator of
    mfAbs:
      Result := Abs(Param[0].AsFloat);
    mfArcTan:
      Result := ArcTan(Param[0].AsFloat);
    mfCos:
      Result := Cos(Param[0].AsFloat);
    mfExp:
      Result := Exp(Param[0].AsFloat);
    mfFrac:
      Result := Frac(Param[0].AsFloat);
    mfInt:
      Result := Int(Param[0].AsFloat);
    mfLn:
      Result := Ln(Param[0].AsFloat);
    mfPi:
      Result := System.Pi;
    mfSin:
      Result := Sin(Param[0].AsFloat);
    mfSqr:
      Result := Sqr(Param[0].AsFloat);
    mfSqrt:
      Result := Sqrt(Param[0].AsFloat);
    mfPower:
      Result := Exp(Param[1].AsFloat * Ln(Param[0].AsFloat))  else
    Result := inherited GetAsFloat;
  end
end;

function TMathExpression.GetAsInteger: Integer;
begin
  CheckParameters;
  case operator of
    mfTrunc:
      Result := Trunc(Param[0].AsFloat);
    mfRound:
      Result := Round(Param[0].AsFloat);
    mfAbs:
      Result := Abs(Param[0].AsInteger);
  else
    Result := inherited GetAsInteger;
  end
end;

procedure TMathExpression.CheckParameters;
var
  OK: Boolean;
begin
  OK := True;
  case operator of
    mfTrunc, mfRound, mfArcTan, mfCos, mfExp, mfFrac, mfInt, mfLn, mfSin, mfSqr,
      mfSqrt, mfAbs:
      begin
        OK := (ParameterCount = 1) and (Param[0].ExprType >= ttFloat);
      end;
    mfPower:
      begin
        OK := (ParameterCount = 2) and (Param[0].ExprType >= ttFloat) and (Param
          [1].ExprType >= ttFloat);
      end;
  end;
  if not OK then
    raise EExpression.CreateFmt(SEXPR_INVALIDPARAMETERTO, [NMF[operator]])
end;

function TMathExpression.GetExprType: TExprType;
begin
  case operator of
    mfTrunc, mfRound:
      Result := ttInteger;
  else
    Result := ttFloat;
  end
end;

constructor TMathExpression.Create(aParameterList: TParameterList; aOperator: TMF);
begin
  inherited Create(aParameterList);
  Operator := aOperator
end;

function TTypeCast.GetAsString: string;
begin
  Result := Param[0].AsString
end;

function TTypeCast.GetAsFloat: Double;
begin
  if Param[0].ExprType = ttString then
    Result := StrToFloat(Param[0].AsString)
  else
    Result := Param[0].AsFloat
end;

function TTypeCast.GetAsInteger: Integer;
begin
  if Param[0].ExprType = ttString then
    Result := StrToInt(Param[0].AsString)
  else if Param[0].ExprType = ttFloat then
    Result := Trunc(Param[0].AsFloat)
  else
    Result := Param[0].AsInteger
end;

function TTypeCast.GetAsBoolean: Boolean;
begin
  Result := Param[0].AsBoolean
end;

function TTypeCast.GetExprType: TExprType;
begin
  Result := operator
end;

constructor TTypeCast.Create(aParameterList: TParameterList; aOperator: TExprType);
begin
  inherited Create(aParameterList);
  Operator := aOperator
end;

function TConditional.Rex: TExpression;
begin
  CheckParameters;
  if Param[0].AsBoolean then
    Result := Param[1]
  else
    Result := Param[2]
end;

procedure TConditional.CheckParameters;
begin
  if not ((ParameterCount = 3) and (Param[0].ExprType = ttBoolean)) then
    raise EExpression.Create('Invalid parameters to If')
end;

function TConditional.GetMaxString: string;
begin
  if Length(Param[1].AsString) > Length(Param[2].AsString) then
    Result := Param[1].AsString
  else
    Result := Param[2].AsString;
end;

function TConditional.GetAsString: string;
begin
  Result := Rex.AsString;
end;

function TConditional.GetAsFloat: Double;
begin
  Result := Rex.AsFloat
end;

function TConditional.GetAsInteger: Integer;
begin
  Result := Rex.AsInteger
end;

function TConditional.GetAsBoolean: Boolean;
begin
  Result := Rex.AsBoolean
end;

function TConditional.GetExprType: TExprType;
begin
  Result := Rex.ExprType
end;

{TASCIIExpr}

constructor TASCIIExpr.Create(ParameterList: TParameterList);
begin
  inherited Create(ParameterList);
  if (ParameterList.Count <> 1) or (ParameterList.ExprType[0] <> ttString) then
    raise EExpression.Create('ASCII: Incorrect argument');
end;

function TASCIIExpr.GetAsInteger: Integer;
begin
  if Length(Param[0].AsString) = 0 then
    Result := 0
  else
    Result := Ord(Param[0].AsString[1]);
end;

function TASCIIExpr.GetExprType: TExprType;
begin
  result := ttInteger;
end;

{ TLeftExpr }

function TLeftExpr.GetAsString: string;
begin
  Result := Copy(Param[0].AsString, 1, Param[1].AsInteger);
end;

function TLeftExpr.GetExprType: TExprType;
begin
  Result := ttString;
end;

{ TRightExpr }

function TRightExpr.GetAsString: string;
var
  p: Integer;
begin
  p := Max(1, Length(Param[0].AsString) - Param[1].AsInteger + 1);
  Result := Copy(Param[0].AsString, p, Param[1].AsInteger);
end;

function TRightExpr.GetExprType: TExprType;
begin
  Result := ttString;
end;

{ TLikeList implementaton}

constructor TLikeList.Create;
begin
  inherited Create;
  fItems := TList.Create;
end;

destructor TLikeList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

function TLikeList.GetCount;
begin
  Result := fItems.Count;
end;

function TLikeList.GetItem(Index: Integer): TLikeItem;
begin
  Result := fItems[Index];
end;

function TLikeList.Add: TLikeItem;
begin
  Result := TLikeItem.Create;
  fItems.Add(Result);
end;

procedure TLikeList.Clear;
var
  I: Integer;
begin
  for I := 0 to fItems.Count - 1 do
    TLikeItem(fItems[I]).Free;
  fItems.Clear;
end;

procedure TLikeList.Delete(Index: Integer);
begin
  TLikeItem(fItems[Index]).Free;
  fItems.Delete(Index);
end;

{ TSQLLikeExpr implementation }

constructor TSQLLikeExpr.Create(ParameterList: TParameterList; IsNotLike: Boolean);
var
  s, Work: string;
  p, n: Integer;
  Previous: Char;
  EscapeChar: Char;
  Accept: Boolean;
begin
  inherited Create(ParameterList);
  LIKEList := TLikeList.Create;
  FIsNotLike := IsNotLike;
  if (ParameterCount > 2) and (Length(Param[2].AsString) > 0) then
    EscapeChar := Char(Param[2].AsString[1]) //li2016
  else
    EscapeChar := #0;

  s := Param[1].AsString;
  if (Length(s) = 0) or ((Pos('%', s) = 0) and (Pos('_', s) = 0)) then
  begin
    with LikeList.Add do
    begin
      LikeText := s;
      LikePos := lpNone;
    end;
  end
  else
  begin
    work := '';
    p := 1;
    n := 0;
    Previous := #0;
    while p <= Length(s) do
    begin
      Accept := ((s[p] = '%') and (EscapeChar = #0)) or ((s[p] = '%') and (Previous
        <> EscapeChar)) or ((s[p] = '_') and (Previous <> EscapeChar));
      if Accept then
      begin
        if (Length(Work) > 0) then
        begin
          if n = 0 then
          begin
            // text must start with Work
            with LikeList.Add do
            begin
              LikeText := Work;
              LikePos := lpLeft;
              if s[p] = '_' then
                LikeCode := lcSingle
              else
                LikeCode := lcMultiple
            end;
          end
          else
          begin
            // el texto debe tener en medio work
            with LikeList.Add do
            begin
              LikeText := Work;
              LikePos := lpMiddle;
              if s[p] = '_' then
                LikeCode := lcSingle
              else
                LikeCode := lcMultiple
            end;
          end;
        end;
        work := '';
        inc(n);
      end
      else
      begin
        if (EscapeChar = #0) or not (s[p] = EscapeChar) then //li2016
          work := work + s[p];
      end;
      Previous := s[p]; //li2016

      Inc(p);
    end;
    if Length(work) > 0 then
    begin
      { texto deber terminar en Work }
      with LikeList.Add do
      begin
        LikePos := lpRight;
        LikeText := Work;
      end;
    end;
  end;
end;

destructor TSQLLikeExpr.Destroy;
begin
  LIKEList.Free;
  inherited Destroy;
end;

function TSQLLikeExpr.SQLPos(var Start: Integer; const Substr, Str: string): Integer;
var
  I, Pivot, NumValid, L1, L2: Integer;
  Accept: Boolean;
begin
  Result := Low(Integer);
  L1 := Length(Str);
  L2 := Length(Substr);
  if (L1 = 0) or (L2 = 0) or (L2 > L1) then
    Exit;
  if (Start = 1) and (Pos('_', Substr) = 0) then
  begin
    Result := Pos(Substr, Str); // speed up result
    if Result > 0 then
      Inc(Start, Length(Substr));
  end
  else
  begin
    for I := Start to L1 do
    begin
      NumValid := 0;
      Pivot := 1;
      Accept := true;
      while Accept and (I + Pivot - 1 <= L1) and (Pivot <= L2) and ((Substr[Pivot]
        = '_') or (Str[I + Pivot - 1] = Substr[Pivot])) do
      begin
        Inc(NumValid);
        Inc(Pivot);
      end;
      if NumValid = L2 then
      begin
        Inc(Start, Length(Substr));
        Result := I;
        Exit;
      end;
    end;
  end;
  if Result = 0 then
    Result := Low(Integer);
end;

procedure TSQLLikeExpr.AddToList(Like: TLikeItem);
begin
  with LikeList.Add do
  begin
    LikePos := Like.LikePos;
    LikeCode := Like.LikeCode;
    LikeText := Like.LikeText;
  end;
end;

function TSQLLikeExpr.GetAsBoolean: Boolean;
var
  I, n, Start, p: Integer;
  Like: TLikeItem;
  s0, s1: string;
  Accept: Boolean;
begin
  n := 0;
  s0 := Param[0].AsString;
  Start := 1;
  Accept := False; //Basri
  for I := 0 to LIKEList.Count - 1 do
  begin
    Like := LIKEList[I];
    s1 := Like.LikeText;
    case Like.LikePos of
      lpNone:
        Accept := (s0 = s1);
      lpLeft:
        begin
          Start := 1;
          if Like.LikeCode = lcSingle then
            s1 := s1 + '_';
          Accept := (SQLPos(Start, s1, s0) = 1);
          if Accept and (Like.LikeCode = lcSingle) and (Length(s1) <> Length(s0)) then
            Accept := false;
        end;
      lpMiddle:
        Accept := (SQLPos(Start, s1, s0) > 0);
      lpRight:
        begin
          p := Length(s0) - Length(s1) + 1;
          if Start <= p then
          begin
            Start := p;
            if Like.LikeCode = lcSingle then
              s1 := '_' + s1;
            Accept := (SQLPos(Start, s1, s0) = p);
            if Accept and (Like.LikeCode = lcSingle) and (Length(s1) <> Length(s0)) then
              Accept := false;
          end
          else
            Accept := False;
        end;
    end;
    if Accept then
      Inc(n);
  end;
  Result := (n = LIKEList.Count);
  if FIsNotLike then
    Result := not Result;
end;

function TSQLLikeExpr.GetExprtype: TExprtype;
begin
  Result := ttBoolean;
end;

{ TBetweenExpr }

constructor TBetweenExpr.Create(ParameterList: TParameterList; IsNotBetween: Boolean);
begin
  inherited Create(ParameterList);
  FIsNotBetween := IsNotBetween;
end;

function TBetweenExpr.GetAsBoolean: Boolean;
var
  s: string;
  f: Double;
  i: Integer;
  b: Boolean;
begin
  Result := False;
  { We'll compare expressions like
      CustNo BETWEEN 10 AND 30
  }
  case Param[0].Exprtype of
    ttString:
      begin
        s := Param[0].AsString;
        result := (s >= Param[1].AsString) and (s <= Param[2].AsString);
      end;
    ttFloat:
      begin
        f := Param[0].AsFloat;
        result := (f >= Param[1].AsFloat) and (f <= Param[2].AsFloat);
      end;
    ttInteger:
      begin
        i := Param[0].AsInteger;
        result := (i >= Param[1].AsInteger) and (i <= Param[2].AsInteger);
      end;
    ttBoolean:
      begin
        b := Param[0].AsBoolean;
        result := (b >= Param[1].AsBoolean) and (b <= Param[2].AsBoolean);
      end;
  end;
  if FIsNotBetween then
    Result := not Result;
end;

function TBetweenExpr.GetExprtype: TExprtype;
begin
  Result := ttBoolean;
end;

{ TSQLInPredicateExpr - class implementation}

constructor TSQLInPredicateExpr.Create(ParameterList: TParameterList; IsNotIn: Boolean);
begin
  inherited Create(ParameterList);
  FIsNotIn := IsNotIn;
end;

function TSQLInPredicateExpr.GetAsBoolean: Boolean;
var
  t: Integer;
  s: string;
  f: Double;
  i: Integer;
  b: Boolean;
begin
  Result := False;
  { We'll compare expressions like
      COUNTRY IN ('USA','SPAIN','MEXICO','ENGLAND')
      CUSTID not IN (1,10,25)
      ISMARRIED IN (TRUE)
      Combination of parameters like:
      CUSTID IN ('USA', 2, 'MEXICO', 2.54)
      where CUSTID is integer, is invalid
  }
  case Param[0].Exprtype of
    ttString:
      begin
        s := Param[0].AsString;
        for t := 1 to ParameterCount - 1 do
          if s = Param[t].AsString then
          begin
            Result := True;
            Break;
          end;
      end;
    ttFloat:
      begin
        f := Param[0].AsFloat;
        for t := 1 to ParameterCount - 1 do
          if f = Param[t].AsFloat then
          begin
            Result := True;
            Break;
          end;
      end;
    ttInteger:
      begin
        i := Param[0].AsInteger;
        for t := 1 to ParameterCount - 1 do
          if i = Param[t].AsInteger then
          begin
            Result := True;
            Break;
          end;
      end;
    ttBoolean:
      begin
        b := Param[0].AsBoolean;
        for t := 1 to ParameterCount - 1 do
          if b = Param[t].AsBoolean then
          begin
            Result := True;
            Break;
          end;
      end;
  end;
  if FIsNotIn then
    Result := not Result;
end;

function TSQLInPredicateExpr.GetExprtype: TExprtype;
begin
  Result := ttBoolean;
end;

{ TCaseWhenElseExpr }

constructor TCaseWhenElseExpr.Create(WhenParamList: TParameterList;
  ThenParamList: TParameterList; ElseExpr: TExpression);
begin
  inherited Create(WhenParamList);
  FThenParamList := ThenParamList;
  FElseExpr := ElseExpr;
end;

procedure TCaseWhenElseExpr.CheckParameters;
var
  I: Integer;
begin
  { check that WHEN expression be of type boolean}
  for I := 0 to ParameterCount - 1 do
  begin
    if Param[I].ExprType <> ttBoolean then
      raise EExpression.Create(SEXPR_WRONGWHENEXPR);
  end;
  { check that all expression in THEN be of same type }
  for I := 1 to FThenParamList.Count - 1 do
  begin
    if not FThenParamList.Param[I].CanReadAs(FThenParamList.Param[0].ExprType) then
      raise EExpression.Create(SEXPR_WRONGTHENEXPR);
  end;
  if (FElseExpr <> nil) and not FElseExpr.CanReadAs(FThenParamList.Param[0].ExprType) then
    raise EExpression.Create(SEXPR_WRONGTHENEXPR);
end;

destructor TCaseWhenElseExpr.Destroy;
begin
  FThenParamList.Free;
  if FElseExpr <> nil then
    FElseExpr.Free;
  inherited Destroy;
end;

function TCaseWhenElseExpr.GetAsBoolean: Boolean;
var
  I: Integer;
begin
  CheckParameters;
  Result := FALSE;
  for I := 0 to ParameterCount - 1 do
    if Param[I].AsBoolean then
    begin
      Result := FThenParamList.AsBoolean[I];
      Exit;
    end;
  if FElseExpr <> nil then
    Result := FElseExpr.AsBoolean;
end;

function TCaseWhenElseExpr.GetAsFloat: Double;
var
  I: Integer;
begin
  CheckParameters;
  Result := 0;
  for I := 0 to ParameterCount - 1 do
    if Param[I].AsBoolean then
    begin
      Result := FThenParamList.AsFloat[I];
      Exit;
    end;
  if FElseExpr <> nil then
    Result := FElseExpr.AsFloat;
end;

function TCaseWhenElseExpr.GetAsInteger: Integer;
var
  I: Integer;
begin
  CheckParameters;
  Result := 0;
  for I := 0 to ParameterCount - 1 do
    if Param[I].AsBoolean then
    begin
      Result := FThenParamList.AsInteger[I];
      Exit;
    end;
  if FElseExpr <> nil then
    Result := FElseExpr.AsInteger;
end;

function TCaseWhenElseExpr.GetMaxString: string;
var
  I: Integer;
begin
  Result := '';
  if not (GetExprtype = ttString) then
    Exit;
  for I := 0 to ParameterCount - 1 do
    if Length(FThenParamList.AsString[I]) > Length(Result) then
      Result := FThenParamList.AsString[I];
  if (FElseExpr <> nil) and (Length(FElseExpr.AsString) > Length(Result)) then
    Result := FElseExpr.AsString;
end;

function TCaseWhenElseExpr.GetAsString: string;
var
  I: Integer;
begin
  CheckParameters;
  Result := '';
  for I := 0 to ParameterCount - 1 do
    if Param[I].AsBoolean then
    begin
      Result := FThenParamList.AsString[I];
      Exit;
    end;
  if FElseExpr <> nil then
    Result := FElseExpr.AsString;
end;

function TCaseWhenElseExpr.GetExprtype: TExprtype;
begin
  { the expression type is the type of the first expression }
  Result := FThenParamList.ExprType[0];
end;

{TFormatDateTimeExpr - class implementation}

function TFormatDateTimeExpr.GetAsString: string;
begin
  Result := FormatDateTime(Param[0].AsString, Param[1].AsFloat);
end;

function TFormatDateTimeExpr.GetExprType: TExprType;
begin
  Result := ttString;
end;

{TFormatFloatExpr - class implementation
 FORMATFLOAT('###,###,##0.00', 32767)}

function TFormatFloatExpr.GetAsString: string;
begin
  Result := FormatFloat(Param[0].AsString, Param[1].AsFloat);
end;

function TFormatFloatExpr.GetExprType: TExprType;
begin
  Result := ttString;
end;

{ TFormatExpr - class implementation
  Format('%d %s ...', 1234, 'ABC', ..., etc) }

function TFormatExpr.GetAsString: string;
const
  MAXARGS = 20; {maximum number of arguments allowed (increase if needed)}
var
  cnt, n: integer;
  ss: array[0..MAXARGS] of string;
  ea: array[0..MAXARGS] of Extended;
  Vars: array[0..MAXARGS] of TVarRec;
begin
  n := Min(ParameterCount - 1, MAXARGS);
  { first parameter is the format string and the rest are the args}
  for cnt := 1 to n do
  begin
    case Param[cnt].ExprType of
      ttString:
        begin
          Vars[cnt - 1].VType := vtString;
          ss[cnt - 1] := Param[cnt].AsString;
          Vars[cnt - 1].VString := @ss[cnt - 1];
        end;
      ttFloat:
        begin
          Vars[cnt - 1].VType := vtExtended;
          ea[cnt - 1] := Param[cnt].AsFloat;
          Vars[cnt - 1].VExtended := @ea[cnt - 1];
        end;
      ttInteger:
        begin
          Vars[cnt - 1].VType := vtInteger;
          Vars[cnt - 1].VInteger := Param[cnt].AsInteger;
        end;
      ttBoolean:
        begin
          Vars[cnt - 1].VType := vtBoolean;
          Vars[cnt - 1].VBoolean := Param[cnt].AsBoolean;
        end;
    end;
  end;
  result := Format(Param[0].AsString, Vars);
end;

function TFormatExpr.GetExprType: TExprType;
begin
  result := ttString;
end;

// TDecodeDateTimeExpr implementation

constructor TDecodeDateTimeExpr.Create(ParameterList: TParameterList; DecodeKind:
  TDecodeKind);
begin
  inherited Create(ParameterList);
  FDecodeKind := DecodeKind;
end;

function TDecodeDateTimeExpr.GetExprType: TExprType;
begin
  Result := ttInteger;
end;

function TDecodeDateTimeExpr.GetAsInteger: Integer;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  case FDecodeKind of
    dkYear, dkMonth, dkDay:
      DecodeDate(Param[0].AsFloat, Year, Month, Day);
    dkHour, dkMin, dkSec, dkMSec:
      DecodeTime(Param[0].AsFloat, Hour, Min, Sec, MSec);
  else
    DecodeDate(Param[0].AsFloat, Year, Month, Day);
  end;
  case FDecodeKind of
    dkYear:
      Result := Year;
    dkMonth:
      Result := Month;
    dkDay:
      Result := Day;
    dkHour:
      Result := Hour;
    dkMin:
      Result := Min;
    dkSec:
      Result := Sec;
    dkMSec:
      Result := MSec;
  else
    Result := Year;
  end;
end;

{ TDecodeExpr
  DECODE('abc', 'a', 1,
                'b', 2,
                'abc', 3,
                'd', 4,
                -1 )
 }

constructor TDecodeExpr.Create(ParameterList: TParameterList);
begin
  inherited Create(ParameterList);
  { check for valid expressions }
  if (ParameterList = nil) or ((ParameterList.Count mod 2) <> 0) then
    raise EExpression.Create('Incorrect number of arguments');
end;

function TDecodeExpr.GetAsBoolean: Boolean;
var
  I: Integer;
  Found: Boolean;
begin
  Result := false;
  Found := false;
  I := 1;
  while I < ParameterCount - 1 do
  begin
    case Param[0].ExprType of
      ttString:
        if Param[0].AsString = Param[I].AsString then
          Found := true;
      ttFloat:
        if Param[0].AsFloat = Param[I].AsFloat then
          Found := true;
      ttInteger:
        if Param[0].AsInteger = Param[I].AsInteger then
          Found := true;
      ttBoolean:
        if Param[0].AsBoolean = Param[I].AsBoolean then
          Found := true;
    end;
    if found then
    begin
      Result := Param[I + 1].AsBoolean;
      break;
    end;
    Inc(I, 2);
  end;
  if not found then
    Result := Param[ParameterCount - 1].AsBoolean;
end;

function TDecodeExpr.GetAsFloat: Double;
var
  I: Integer;
  Found: Boolean;
begin
  Result := 0;
  Found := false;
  I := 1;
  while I < ParameterCount - 1 do
  begin
    case Param[0].ExprType of
      ttString:
        if Param[0].AsString = Param[I].AsString then
          Found := true;
      ttFloat:
        if Param[0].AsFloat = Param[I].AsFloat then
          Found := true;
      ttInteger:
        if Param[0].AsInteger = Param[I].AsInteger then
          Found := true;
      ttBoolean:
        if Param[0].AsBoolean = Param[I].AsBoolean then
          Found := true;
    end;
    if found then
    begin
      Result := Param[I + 1].AsFloat;
      break;
    end;
    Inc(I, 2);
  end;
  if not found then
    Result := Param[ParameterCount - 1].AsFloat;
end;

function TDecodeExpr.GetAsInteger: Integer;
var
  I: Integer;
  Found: Boolean;
begin
  Result := 0;
  Found := false;
  I := 1;
  while I < ParameterCount - 1 do
  begin
    case Param[0].ExprType of
      ttString:
        if Param[0].AsString = Param[I].AsString then
          Found := true;
      ttFloat:
        if Param[0].AsFloat = Param[I].AsFloat then
          Found := true;
      ttInteger:
        if Param[0].AsInteger = Param[I].AsInteger then
          Found := true;
      ttBoolean:
        if Param[0].AsBoolean = Param[I].AsBoolean then
          Found := true;
    end;
    if found then
    begin
      Result := Param[I + 1].AsInteger;
      break;
    end;
    Inc(I, 2);
  end;
  if not found then
    Result := Param[ParameterCount - 1].AsInteger;
end;

function TDecodeExpr.GetMaxString: string;
var
  I, L, MaxL: Integer;
begin
  L := 2;
  MaxL := Length(Param[L].AsString);
  I := 2;
  while I <= ParameterCount - 1 do
  begin
    if Length(Param[I].AsString) > MaxL then
    begin
      L := I;
      MaxL := Length(Param[I].AsString);
    end;
    Inc(I, 2);
  end;
  Result := Param[L].AsString;
end;

function TDecodeExpr.GetAsString: string;
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  I := 1;
  while I < ParameterCount - 1 do
  begin
    case Param[0].ExprType of
      ttString:
        if Param[0].AsString = Param[I].AsString then
          Found := true;
      ttFloat:
        if Param[0].AsFloat = Param[I].AsFloat then
          Found := true;
      ttInteger:
        if Param[0].AsInteger = Param[I].AsInteger then
          Found := true;
      ttBoolean:
        if Param[0].AsBoolean = Param[I].AsBoolean then
          Found := true;
    end;
    if found then
    begin
      Result := Param[I + 1].AsString;
      break;
    end;
    Inc(I, 2);
  end;
  if not found then
    Result := Param[ParameterCount - 1].AsString;
end;

function TDecodeExpr.GetExprtype: TExprtype;
begin
  Result := Param[2].ExprType;
end;

{ MINOF, MAXOF functions support}

constructor TMinMaxOfExpr.Create(ParameterList: TParameterList; IsMin: Boolean);
begin
  inherited Create(ParameterList);
  { check for valid expressions }
  if (ParameterList = nil) or (ParameterList.Count < 1) then
    raise EExpression.Create('Incorrect number of arguments');
  FIsMin := IsMin;
end;

function TMinMaxOfExpr.GetAsFloat: Double;
var
  i: Integer;
begin
  Result := Param[0].AsFloat;
  for i := 1 to ParameterCount - 1 do
  begin
    if FIsMin then
      Result := Min(Result, Param[i].AsFloat)
    else
      Result := Max(Result, Param[i].AsFloat);
  end;
end;

function TMinMaxOfExpr.GetExprtype: TExprtype;
begin
  Result := ttFloat;
end;

end.


