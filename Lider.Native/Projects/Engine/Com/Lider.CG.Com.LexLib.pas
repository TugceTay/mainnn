unit Lider.CG.Com.LexLib;
{*****************************************************************************}
{   TxQuery DataSet                                                           }
{                                                                             }
{   The contents of this file are subject to the Mozilla Public License       }
{   Version 1.1 (the "License"); you may not use this file except in          }
{   compliance with the License. You may obtain a copy of the License at      }
{   http://www.mozilla.org/MPL/                                               }
{                                                                             }
{   Software distributed under the License is distributed on an "AS IS"       }
{   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   }
{   License for the specific language governing rights and limitations        }
{   under the License.                                                        }
{                                                                             }
{   The Original Code is: QLEXLIB.pas                                         }
{                                                                             }
{                                                                             }
{   The Initial Developer of the Original Code is Alfonso Moreno.             }
{   Portions created by Alfonso Moreno are Copyright (C) <1999-2003> of       }
{   Alfonso Moreno. All Rights Reserved.                                      }
{   Open Source patch reviews (2009-2012) with permission from Alfonso Moreno }
{                                                                             }
{   Alfonso Moreno (Hermosillo, Sonora, Mexico)                               }
{   email: luisarvayo@yahoo.com                                               }
{     url: http://www.ezsoft.com                                              }
{          http://www.sigmap.com/txquery.htm                                  }
{                                                                             }
{   Contributor(s): Chee-Yang, CHAU (Malaysia) <cychau@gmail.com>             }
{                   Sherlyn CHEW (Malaysia)                                   }
{                   Francisco Dueñas Rodriguez (Mexico) <fduenas@gmail.com>   }
{                                                                             }
{              url: http://code.google.com/p/txquery/                         }
{                   http://groups.google.com/group/txquery                    }
{                                                                             }
{*****************************************************************************}

{$I Lider.CG.Com.Component.inc}

interface

uses
  Classes,
  SysUtils;

const
  nl = #10; (* newline character *)
  max_chars = maxint div SizeOf(Char);
  intial_bufsize = 16384;

type
  PCharArray = ^TCharArray;

  TCharArray = array[1..max_chars] of char;

  {modified by fduenas: move YYSType here to make TP Yacc/Lex thread safe)}
  YYSType = record
    yystring: string
  end
  (*YYSType*);

  TCustomLexer = class
  private
    function GetBuf(Index: Integer): char;
    procedure SetBuf(Index: Integer; Value: char);
  public
    //yyinput, yyoutput : Text;      (* input and output file            *)
    //yyerrorfile       : Text;      (* standard error file              *)
    yyinput, yyoutput: TStream; (* input and output file            *)
    yyerrorfile: TStream; (* standard error file              *)
    yyline: string; (* current input line               *)
    yylineno, yycolno: Integer; (* current input position           *)
    yyTextBuf: PCharArray;
    yyTextLen: Integer;
    yyTextBufSize: Integer;

    (*   (should be considered r/o)     *)
{yyleng            : Byte         (* length of matched text *)
absolute yytext;                incompatible with Delphi 2.0       }

(* I/O routines:

The following routines get_char, unget_char and put_char are used to
implement access to the input and output files. Since \n (newline) for
Lex means line end, the I/O routines have to translate MS-DOS line ends
(carriage-return/line-feed) into newline characters and vice versa. Input
is buffered to allow rescanning text (via unput_char).

The input buffer holds the text of the line to be scanned. When the input
buffer empties, a new line is obtained from the input stream. Characters
can be returned to the input buffer by calls to unget_char. At end-of-
file a null character is returned.

The input routines also keep track of the input position and set the
yyline, yylineno, yycolno variables accordingly.

Since the rest of the Lex library only depends on these three routines
(there are no direct references to the yyinput and yyoutput files or
to the input buffer), you can easily replace get_char, unget_char and
put_char by another suitable set of routines, e.g. if you want to read
from/write to memory, etc. *)

    function get_char: char;
    (* obtain one character from the input file (null character at end-of-
       file) *)

    procedure unget_char(c: char);
    (* return one character to the input file to be reread in subsequent
       calls to get_char *)

    procedure put_char(c: char);
    (* write one character to the output file *)

  (* Utility routines: *)

    procedure echo;
    (* echoes the current match to the output stream *)

    procedure yymore;
    (* append the next match to the current one *)

    procedure yyless(n: Integer);
    (* truncate yytext to size n and return the remaining characters to the
       input stream *)

    procedure reject;
    (* reject the current match and execute the next one *)

    (* reject does not actually cause the input to be rescanned; instead,
       internal state information is used to find the next match. Hence
       you should not try to modify the input stream or the yytext variable
       when rejecting a match. *)

    procedure returni(n: Integer);
    procedure returnc(c: char);
    (* sets the return value of yylex *)

    procedure start(state: Integer);
    (* puts the lexical analyzer in the given start state; state=0 denotes
       the default start state, other values are user-defined *)

  (* yywrap:

     The yywrap function is called by yylex at end-of-file (unless you have
     specified a rule matching end-of-file). You may redefine this routine
     in your Lex program to do application-dependent processing at end of
     file. In particular, yywrap may arrange for more input and return false
     in which case the yylex routine resumes lexical analysis. *)

    function yywrap: Boolean;
    (* The default yywrap routine supplied here closes input and output
       files and returns true (causing yylex to terminate). *)

  (* The following are the internal data structures and routines used by the
     lexical analyzer routine yylex; they should not be used directly. *)

    function yylex: Integer; virtual; abstract;
    (* this function must be overriden by the Lexer descendent in order
       to provide the lexing service *)
    constructor Create;
    destructor Destroy; override;
    procedure CheckBuffer(Index: integer);
    procedure CheckyyTextBuf(Size: integer);
    procedure GetyyText(var s: string);
    property Buf[Index: Integer]: char read GetBuf write SetBuf;
  protected
    yystate: Integer; (* current state of lexical analyzer *)
    yyactchar: char; (* current character *)
    yylastchar: char; (* last matched character (#0 if none) *)
    yyrule: Integer; (* matched rule *)
    yyreject: Boolean; (* current match rejected? *)
    yydone: Boolean; (* yylex return value set? *)
    yyretval: Integer; (* yylex return value *)
    bufptr: Integer;
    //buf: Array[1..max_chars] Of Char;
    bufSize: Integer;
    FBuf: PCharArray;
    procedure yynew;
    (* starts next match; initializes state information of the lexical
       analyzer *)

    procedure yyscan;
    (* gets next character from the input stream and updates yytext and
       yyactchar accordingly *)

    procedure yymark(n: Integer);
    (* marks position for rule no. n *)

    procedure yymatch(n: Integer);
    (* declares a match for rule number n *)

    function yyfind(var n: Integer): Boolean;
    (* finds the last match and the corresponding marked position and
       adjusts the matched string accordingly; returns:
       - true if a rule has been matched, false otherwise
       - n: the number of the matched rule *)

    function yydefault: Boolean;
    (* executes the default action (copy character); returns true unless
       at end-of-file *)

    procedure yyclear;
    (* reinitializes state information after lexical analysis has been
       finished *)

    procedure fatal(msg: string);
    (* writes a fatal error message and halts program *)

  end; (* TCustomLexeer *)

//function eof(aStream: Tstream): boolean;
//procedure readln(aStream: TStream; Var aLine: string);

procedure writeln(aStream: TStream; const aline: string);

procedure write(aStream: TStream; const aLine: string);

implementation

uses
  Math;

(* utility procedures *)

function eof(aStream: Tstream): Boolean;
begin
  result := aStream.position >= aStream.size;
end;

procedure readln(aStream: TStream; var aLine: string);
var
  //aBuffer: String;
  CRBuf: string;
  trouve: Boolean;
  unCar: Char;
  Buf: PChar;
  BufSize: Integer;
  i: Integer;

  procedure CheckBuffer;
  begin
    repeat
      //we need to take into account size of char - we are increasing
      //position in stream by SizeOf(char) and not by a byte
      if (i * SizeOf(Char)) >= (BufSize - SizeOf(Char)) then
      //(- SizeOf(Char) is needed if BufSize is odd number and
      //GetMem works in chunks of 1 byte
      begin
        BufSize := Max(BufSize * SizeOf(Char), BufSize + 256);
        ReallocMem(Buf, BufSize);
      end;
    until (i * SizeOf(Char)) < (BufSize - SizeOf(Char));

  end;

begin
  // ??????
  //aBuffer := '';
  BufSize := 256;
  i := 0;
  GetMem(Buf, BufSize * SizeOf(Char)); { patched by ccy }
  FillChar(Buf^, BufSize * SizeOf(Char), #0); {added by fduenas}
  try
    trouve := false;
    repeat
      aStream.read(unCar, 1 * SizeOf(Char)); { patched by ccy }
      if aStream.Position >= aStream.Size then
      begin
        trouve := true;
        if not CharInSet(unCar, [#10, #13]) then
        begin
          //aLine := aBuffer+unCar
          Inc(i);
          CheckBuffer;
          Move(uncar, Buf[i - 1], 1 * SizeOf(Char)); { patched by ccy }
          SetLength(aLine, i);
          Move(Buf^, aLine[1], i * SizeOf(Char)); { patched by ccy }
        end
        else
        begin
          if i > 0 then
          begin
            SetLength(aLine, i);
            Move(Buf^, aLine[1], i * SizeOf(Char)); { patched by ccy }
          end
          else
            aLine := '';
        end;
      end
      else
        case unCar of
          #10, #11: {added by fduenas: char(11) some times is assigned when assigning TRichEdit.Lines.Text to SQL | SQL Script property}
            begin
              //aLine := aBuffer;
              if i > 0 then
              begin
                SetLength(aLine, i);
                Move(Buf^, aLine[1], i * SizeOf(Char)); { patched by ccy }
                trouve := true;
              end
              else
                aLine := '';
            end;
          #13:
            begin
              aStream.read(unCar, 1 * SizeOf(Char)); { patched by ccy }
              if CharInSet(unCar, [#10, #11]) then {patched by fduenas: some times a char(11) is added to end of each line
                                                    when assigning TRichEdit.Lines.Text to SQL | SQL Script property}
              begin
                //aLine := aBuffer;
                if i > 0 then
                begin
                  SetLength(aLine, i);
                  Move(Buf^, aLine[1], i * SizeOf(Char)); { patched by ccy }
                  trouve := true;
                end
                else
                  aLine := '';
              end
              else
              begin
                Inc(i, 2);
                CheckBuffer;
                CRBuf := #13 + unCar;
                Move(CRBuf[1], Buf[i - 2], 2 * SizeOf(Char)); { patched by ccy }
                //aBuffer := aBuffer + #13 + unCar;
              end;
            end;
        else
          //aBuffer := aBuffer + unCar;
          begin
            Inc(i);
            CheckBuffer;
            Move(unCar, Buf[i - 1], 1 * SizeOf(Char)); { patched by ccy }
          //aBuffer := aBuffer+unCar;
          end;

        end;
    until trouve;
  finally
    FreeMem(Buf, BufSize * SizeOf(Char));  { patched by ccy }
  end;
end;

procedure writeln(aStream: TStream; const aline: string);
const
  FINLIGNE: array[1..2] of char = (#13, #10);
begin
  // ??????
  write(aStream, aLine);
  aStream.write(FINLIGNE, 2);
end;

procedure write(aStream: TStream; const aLine: string);
const
  WRITEBUFSIZE = 8192;
var
  aBuffer: array[1..WRITEBUFSIZE] of char;
  j, nbuf: integer;
  k, nreste: integer;
begin
  nbuf := length(aLine) div WRITEBUFSIZE;
  nreste := length(aLine) - (nbuf * WRITEBUFSIZE);
  for j := 0 to nbuf - 1 do
  begin
    for k := 1 to WRITEBUFSIZE do
      aBuffer[k] := aLine[j * WRITEBUFSIZE + k];
    aStream.write(aBuffer, WRITEBUFSIZE);
  end;
  for k := 1 to nreste do
    aBuffer[k] := aLine[nbuf * WRITEBUFSIZE + k];
  aStream.write(aBuffer, nreste);
end;

procedure TCustomLexer.fatal(msg: string);
(* writes a fatal error message and halts program *)
begin
  writeln(yyerrorfile, 'LexLib: ' + msg);
  halt(1);
end (*fatal*);

(* I/O routines: *)

function TCustomLexer.get_char: char;
var
  i: Integer;
begin
  if (bufptr = 0) and not eof(yyinput) then
  begin
    readln(yyinput, yyline);
    inc(yylineno);
    yycolno := 1;
    buf[1] := nl;
    for i := 1 to length(yyline) do
    begin
      buf[i + 1] := yyline[length(yyline) - i + 1];
    end;
    inc(bufptr, length(yyline) + 1);
  end;
  if bufptr > 0 then
  begin
    get_char := buf[bufptr];
    dec(bufptr);
    inc(yycolno);
  end
  else
    get_char := #0;

end;

procedure TCustomLexer.unget_char(c: Char);
begin
  if bufptr = max_chars then
    fatal('input buffer overflow');
  inc(bufptr);
  dec(yycolno);
  buf[bufptr] := c;
end;

procedure TCustomLexer.put_char(c: Char);
begin
  if c = #0 then
    { ignore }

  else if c = nl then
    writeln(yyoutput, '')
  else
    write(yyoutput, c)
end;

(* Variables:

   Some state information is maintained to keep track with calls to yymore,
   yyless, reject, start and yymatch/yymark, and to initialize state
   information used by the lexical analyzer.
   - yystext: contains the initial contents of the yytext variable; this
     will be the empty string, unless yymore is called which sets yystext
     to the current yytext
   - yysstate: start state of lexical analyzer (set to 0 during
     initialization, and modified in calls to the start routine)
   - yylstate: line state information (1 if at beginning of line, 0
     otherwise)
   - yystack: stack containing matched rules; yymatches contains the number of
     matches
   - yypos: for each rule the last marked position (yymark); zeroed when rule
     has already been considered
   - yysleng: copy of the original yyleng used to restore state information
     when reject is used *)

const
  max_matches = 1024;
  max_rules = 256;

var
  yystext: string;
  yysstate, yylstate: Integer;
  yymatches: Integer;
  yystack: array[1..max_matches] of Integer;
  yypos: array[1..max_rules] of Integer;
  yysleng: Byte;

(* Utilities: *)

procedure TCustomLexer.echo;
var
  i: Integer;
begin
  for i := 1 to yyTextLen do
    put_char(yyTextBuf^[i])
end;

procedure TCustomLexer.yymore;
begin
  //yystext := yytext;
  if yyTextBuf <> nil then
  begin
    if yyTextLen > 0 then
    begin
      SetLength(yystext, yyTextLen);
      Move(yyTextBuf^, yystext[1], yyTextLen);
    end
    else
      yystext := '';
  end
  else
    yystext := '';
end;

procedure TCustomLexer.yyless(n: Integer);
var
  i: Integer;
begin
  for i := yytextlen downto n + 1 do
    unget_char(yytextbuf^[i]);
  CheckyyTextBuf(n);
  yyTextLen := n;
end (*yyless*);

procedure TCustomLexer.reject;
var
  i: Integer;
begin
  yyreject := true;
  for i := yyTextLen + 1 to yysleng do
  begin
    Inc(yyTextLen);
    yyTextBuf^[yyTextLen] := get_char;
    //yytext := yytext + get_char;
  end;
  dec(yymatches);
end;

procedure TCustomLexer.returni(n: Integer);
begin
  yyretval := n;
  yydone := true;
end;

procedure TCustomLexer.returnc(c: Char);
begin
  yyretval := ord(c);
  yydone := true;
end;

procedure TCustomLexer.start(state: Integer);
begin
  yysstate := state;
end;

(* yywrap: *)

function TCustomLexer.yywrap: Boolean;
begin
  // ????? close(yyinput); close(yyoutput);
  yywrap := true;
end;

(* Internal routines: *)

procedure TCustomLexer.yynew;
begin
  if yylastchar <> #0 then
    if yylastchar = nl then
      yylstate := 1
    else
      yylstate := 0;
  yystate := yysstate + yylstate;
  //yytext := yystext;
  CheckyyTextBuf(Length(yystext));
  yyTextLen := Length(yystext);
  if yyTextLen > 0 then
    Move(yystext[1], yytextbuf^, yyTextLen);

  yystext := '';
  yymatches := 0;
  yydone := false;
end;

procedure TCustomLexer.yyscan;
begin
  //if Length(yytext)=255 then fatal('yytext overflow');
  yyactchar := get_char;
  //yytext := yytext + yyactchar;
  CheckyyTextBuf(yyTextLen + 1);
  Inc(yyTextLen);
  yyTextBuf^[yyTextLen] := yyactchar;
end;

procedure TCustomLexer.yymark(n: Integer);
begin
  if n > max_rules then
    fatal('too many rules');
  //yypos[n] := Length(yytext);
  yypos[n] := yyTextLen;
end;

procedure TCustomLexer.yymatch(n: Integer);
begin
  inc(yymatches);
  if yymatches > max_matches then
    fatal('match stack overflow');
  yystack[yymatches] := n;
end;

function TCustomLexer.yyfind(var n: Integer): Boolean;
begin
  yyreject := false;
  while (yymatches > 0) and (yypos[yystack[yymatches]] = 0) do
    dec(yymatches);
  if yymatches > 0 then
  begin
    yysleng := yyTextLen;
    n := yystack[yymatches];
    yyless(yypos[n]);
    yypos[n] := 0;
    if yyTextLen > 0 then
      yylastchar := yytextbuf^[yytextlen]
    else
      yylastchar := #0;
    yyfind := true;
  end
  else
  begin
    yyless(0);
    yylastchar := #0;
    yyfind := false;
  end
end;

function TCustomLexer.yydefault: Boolean;
begin
  yyreject := false;
  yyactchar := get_char;
  if yyactchar <> #0 then
  begin
    put_char(yyactchar);
    yydefault := true;
  end
  else
  begin
    yylstate := 1;
    yydefault := false;
  end;
  yylastchar := yyactchar;
end;

procedure TCustomLexer.yyclear;
begin
  bufptr := 0;
  yysstate := 0;
  yylstate := 1;
  yylastchar := #0;
  yyTextLen := 0;
  yystext := '';
end;

constructor TCustomLexer.Create;
begin
  inherited Create;
  CheckyyTextBuf(intial_bufsize);
  CheckBuffer(intial_bufsize);
end;

destructor TCustomLexer.Destroy;
begin
  FreeMem(FBuf);
  FreeMem(yyTextBuf);
  inherited;
end;

procedure TCustomLexer.CheckBuffer(Index: Integer);
begin
  repeat
    if Index > BufSize then
    begin
      bufSize := max(bufSize * SizeOf(Char), intial_bufsize); {changed bye fduenas, 2 to SizeOf(Char)}
      ReallocMem(FBuf, bufSize);
    end;
  until Index <= bufSize;
end;

function TCustomLexer.GetBuf(Index: Integer): Char;
begin
  CheckBuffer(Index);
  Result := FBuf^[Index];
end;

procedure TCustomLexer.SetBuf(Index: Integer; Value: Char);
begin
  CheckBuffer(Index);
  FBuf^[Index] := Value;
end;

procedure TCustomLexer.CheckyyTextBuf(Size: Integer);
begin
  repeat
    if Size > yyTextBufSize then
    begin
      yyTextBufSize := max(yyTextBufSize * SizeOf(Char), intial_bufsize); {changed bye fduenas, 2 to SizeOf(Char)}
      ReallocMem(yyTextBuf, yyTextBufSize);
    end;
  until Size <= yyTextBufSize;
end;

procedure TCustomLexer.GetyyText(var s: string);
begin
  if yyTextLen > 0 then
  begin
    SetLength(s, yyTextLen);
    Move(yytextbuf^, s[1], yyTextLen * SizeOf(Char)); { patched by ccy }
  end
  else
    s := '';
end;

end (*LexLib*).


