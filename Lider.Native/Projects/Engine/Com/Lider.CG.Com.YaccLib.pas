unit Lider.CG.Com.YaccLib;

{$I Lider.CG.Com.Component.inc}

(* Yacc Library unit for TP Yacc Version 3.0, 6-17-91 AG *)
(* adapted to Delphi 3,4,5 4/15/2001 *)

interface

uses
  Classes,
  Lider.CG.Com.LexLib,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

const
  yymaxdepth = 2048;
  (* default stack size of parser *)

type
  TYYFlag = (yyfnone, yyfaccept, yyfabort, yyferror);

  (* default value type, may be redefined in Yacc output file *)
  TCustomParser = class(TObject)
  protected
    yyerrflag: Integer;
    yyflag: TYYFlag;
    //yylval: YYSType;  {modified by fduenas: make TP Yacc/Lex thread safe)}
    function getDrawBox: TlicgBaseDrawBox; virtual; abstract;
    procedure setDrawBox(value: TlicgBaseDrawBox); virtual; abstract;
    function getJustCreateEntity: Boolean; virtual; abstract;
    procedure setJustCreateEntity(value: Boolean); virtual; abstract;
    function getCheckSyntax: Boolean; virtual; abstract;
    procedure SetCheckSyntax(value: Boolean); virtual; abstract;
    function getVector: IlicgVector; virtual; abstract;
    function getCmdLine: TObject; virtual; abstract;
    procedure SetCmdLine(value: TObject); virtual; abstract;
    function getMustRepaint: Boolean; virtual; abstract;
    procedure SetMustRepaint(value: Boolean); virtual; abstract;
    function getEntityCreated: IlicgEntity; virtual; abstract;
    procedure SetEntityCreated(value: IlicgEntity); virtual; abstract;
  public
    yychar: Integer; (* current lookahead character *)
    yynerrs: Integer; (* current number of syntax errors reported by the
    parser *)
    yydebug: Boolean; (* set to true to enable debugging output of parser *)

    yyLexer: TCustomLexer; (* Lexer used to lex input *)

    yyerrormsg: string; (* Last error message in string format *)

    procedure yyerror(msg: string);
    (* error message printing routine used by the parser *)

    procedure yyclearin;
    (* delete the current lookahead token *)

    procedure yyaccept;
    (* trigger accept action of the parser; yyparse accepts returning 0, as if
       it reached end of input *)

    procedure yyabort;
    (* like yyaccept, but causes parser to return with value 1, as if an
       unrecoverable syntax error had been encountered *)

    procedure yyerrlab;
    (* causes error recovery to be started, as if a syntax error had been
       encountered *)

    procedure yyerrok;
    (* when in error mode, resets the parser to its normal mode of
       operation *)

    function yyparse: integer; virtual; abstract;

    (* Flags used internally by the parser routine: *)

    property DrawBox: TlicgBaseDrawBox read getDrawBox write setDrawBox;
    property JustCreateEntity: Boolean read getJustCreateEntity write setJustCreateEntity;
    property CheckSyntax: Boolean read getCheckSyntax write SetCheckSyntax;
    property Vector: IlicgVector read getVector;
    property CmdLine: TObject read getCmdLine write SetCmdLine;
    property MustRepaint: Boolean read getMustRepaint write SetMustRepaint;
    property EntityCreated: IlicgEntity read getEntityCreated write SetEntityCreated;
  end; (* TCustomParser *)

  TIdentifierFunctionEvent = procedure(Sender: TObject; const Group, Identifier:
    string; ParameterList: TParameterList; var Expression: TExpression) of object;

  TBaseExprParser = class(TCustomParser)
  protected
    function AddExpression(Expression: TExpression): TExpression; virtual; abstract;
    function GetParamList: TParameterList; virtual; abstract;
    function ForceParamList(Count: Integer): TParameterList; virtual; abstract;
    procedure GetTwoOperators; virtual; abstract;
    procedure GetOneOperator; virtual; abstract;
    procedure AddParam; virtual; abstract;
    function GetString(const s: string): string; virtual; abstract;
    function getVector: IlicgVector; virtual; abstract;
    function getOrderBy: TStrings; virtual; abstract;
    function getIsComplex: Boolean; virtual; abstract;
    procedure SetIsComplex(value: Boolean); virtual; abstract;
    function getOnIdentifierFunction: TIdentifierFunctionEvent; virtual; abstract;
    procedure setOnIdentifierFunction(value: TIdentifierFunctionEvent); virtual; abstract;
    function getClosestMax: integer; virtual; abstract;
  public
    constructor Create(MainExpr: TlicgBaseMainExpr);
    destructor Destroy; override;
    function yyparse: integer; override;
    function GetExpression: TExpression; virtual; abstract;
    function ecExpression: ecExpressionClass; virtual; abstract;
    property IsComplex: Boolean read getIsComplex write setIsComplex;
    property Vector: IlicgVector read getVector;
    property OrderBy: TStrings read getOrderBy;
    property ClosestMax: Integer read getClosestMax;
    property OnIdentifierFunction: TIdentifierFunctionEvent read
      getOnIdentifierFunction write setOnIdentifierFunction;
  end;

implementation

procedure TCustomParser.yyerror(msg: string);
begin
  yyerrormsg := msg;
  //writeln(yyLexer.yyerrorfile, msg);
end (*yyerrmsg*);

procedure TCustomParser.yyclearin;
begin
  yychar := -1;
end (*yyclearin*);

procedure TCustomParser.yyaccept;
begin
  yyflag := yyfaccept;
end (*yyaccept*);

procedure TCustomParser.yyabort;
begin
  yyflag := yyfabort;
end (*yyabort*);

procedure TCustomParser.yyerrlab;
begin
  yyflag := yyferror;
end (*yyerrlab*);

procedure TCustomParser.yyerrok;
begin
  yyerrflag := 0;
end (*yyerrork*);


{ TBaseExprParser }

constructor TBaseExprParser.Create(MainExpr: TlicgBaseMainExpr);
begin
  inherited Create;
end;

destructor TBaseExprParser.Destroy;
begin

  inherited;
end;

function TBaseExprParser.yyparse: integer;
begin
//empty
end;

end (*YaccLib*).


