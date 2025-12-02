unit Lider.CG.Com.StringLi;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Graphics, DateUtils, Variants, Math;

const
  _KILOMETRESTR = 0;

// formatlama katsayilari
const
  _DPC: integer = 3;  // KOORDINAT
  _DPZ: integer = 3;  // KOT
  _DPA: integer = 3;  // ALAN
  _DPK: integer = 3;  // KILOMETRE-UZUNLUK

type
  float = double;

  CharSet = set of AnsiChar; // ilker dei˛tirme

  Chr2 = string[2];

  st255 = string[255];

  st128 = string[128];

  st40 = string[40];

  st30 = string[30];

  st20 = string[20];

  st15 = string[15];

  st4 = string[4];

  st8 = string[8];

  st2 = string[2];

  st12 = string[12];

  st14 = string[14];

  TarihRecord = record
    Gun: Byte;
    Ay: Byte;
    Yil: Byte; { offset to 1900 }
  end;

  FileNameType = string[79];

const
  whole_set: charset = [#0..#255];
  alpha_set: charset = ['A'..'Z', 'a'..'z'];
  big_alpha_set: string[26] = 'ABCDEFGHIJKLMNOPRSTUVYZXWQ';
  special_set: charset = [' '..'/', '['..'`', ':'..'@', '~'];
  Control_set: charset = [#0..#31];
  Numerics: charset = ['-', '0'..'9'];
  turkce_set: charset = ['ò', 'ç', '›', 'Å', 'ô', 'î', 'Ä', 'á', '–', '', '¶', 'ß'];
  noktavirgulSet: charset = ['.', ','];

   (*
   corresponding codes  152,141,154,129,153,148,128,135,158,159,166,167
   description           @I, @i,                       , @S, @s, @G, @g
   above description is given if no turkish chars installed on disp.HW.
*)
  bigABC: string[32] = 'ABC«DEFG–HI›JKLMNO÷PRSﬁTU‹VYZXWQ';
  smallABC: string[32] = 'abcÁdefgh˝ijklmnoˆprs˛tu¸vyzxwq';

const
(* ANSI_set : array[1..12] of byte = (156,157,158,159,154,155,214,246,199,231,220,252); *)
  ANSI_set: array[1..12] of byte = (222, 254, 208, 240, 221, 253, 214, 246, 199, 231, 220, 252);
  //                                  ﬁ   ˛     –   g    ›     ˝   ÷     ˆ   «    Á    ‹    ¸
  TURK_set: array[1..12] of byte = (158, 159, 166, 167, 152, 141, 153, 148, 128,
    135, 154, 129);

  function atof(s: string): float;

  function strcmp(const n1, n2: string): Integer;

  function Equals(s1, s2: string): boolean;

  procedure DivideString(s: string; var s1, s2: string; ch: string);

  function Parse(var s: string; const ch: string): string; overload
  //Function  Parse(Var s:string;const ch:string):string;overload

  function ParseVAL(s: string; const ch: string): string; overload;
  //Function  ParseVAL(s:string;const ch:string):string; overload;

  function ParseToEnd(var s: string; ch: string): string; overload;
  //Function  ParseToEnd(Var s:string;ch:string):string;overload;

  function strtok(var s: string; ps: string): string;

  function strtoke(var s: string; ps: string): string;

  function tokenize(var s1, s2: string; ps: string): string;

  function cut(s: string; maxlen: integer): string;

  procedure deleteAll(sd: string; var s: string);

  procedure deleteallleading(sd: string; var s: string);

  procedure deletealltrailing(sd: string; var s: string);

  function LowCase(ch: char): char;

  function PosNOT(ch: char; s: string): integer;

  function Count(ch: char; s: string): integer;

  function Contain(what: charset; s: string): boolean;

  procedure DeleteSet(uset: charset; var s: string; var ExistsChar: boolean);

  function Replace(s, old, new: string): string;

  function replacec(s: string; cho, chn: char): string;

  function replacenc(s: string; cho, chn: char): string;

  function Spc(numofspc: integer): string;

  function Stc(s: integer; st: string): string;

  function ToControl(ch: char): char;

  function ToNormal(ch: char): char;

  function IsControl(ch: char): boolean;

  function isdigit(ch: char): boolean;

  function IsUpper(ch: char): boolean;

  function XUpCase(ch: char): char;

  function Lower(s: string): string;

  function Xupper(s: string): string;

  function Upper(s: string): string; { turkish }

  function DeleteF(s: string; start, len: integer): string;

  function Capitalize(s: string): string;

  function StrCase(s: string; mode: integer): string;

  function putcomma(s: string): string;

  function numtostr(r: float): string;

  function ToTurkish(s3: string): string;

  function detab(line: string): string;

  procedure chipher(var si, so1, so2: string);

  function dechipher(so1, so2: string): string;

  function reverse(s: string): string;

  function noblank(s: string): string;

  function RandomName(l: integer): string;

  function _longkey(l1, l2: longint): string;

  function _longrat(l1, l2: longint): string;

  function validpname(var pname: string): boolean;

  procedure correctstringforplotting(var prompt: string);

  function erased(s: string): boolean;

  procedure addextention(var name: string; ext: string);

  function allsame(s: string; st, en: integer): boolean;

  function translateby(s, tchs: string): string; { used to determine order of Geometry.Points }

var
  _A2NERROR: boolean;

{convert}
const
  _noblank = 1;
  _nob = 1;
  _withsign = 2;
  _pw0c = 3;
  _pw0ch = 4;
  _center = 5;
  _pw0chv = 6;
  _ljust = 7;
  _0fill = 8;
  _nodp = 9;
  _nobw0c = 10;

const
  _TICKDIGIT: integer = 2;

function atoi(s: string): integer;

function atol(s: string): longint;

function htoi(s: string): LongInt;

function Hexb(b: byte): string;

function Hexw(w: word): string;

function Hexl(li: LongInt): string;

// ilker silme kullan˝lm˝yor function HexAdr(p: pointer): string;

function Bin(li: LongInt; nbit: byte): string;

function HiWord(a: longint): word;

function LoWord(a: longint): word;

function rtos(r: float; fo1, fo2: integer): string;

function rtosb(r: float; dp: integer): string;

function rtosn(r: float): string;

function itos(i: integer; fo1: integer): string;

function i2s(i: integer): string;

function li2s(i: longint): string;
//li2016 function  li2a(key:string):longint;

function ltos(l: LongInt; fo1: integer): string;

function ltosn(l: longint): string;

function Ratio(s: string): float;

function stodate(t: string; var D: TarihRecord): boolean;

procedure dateinvalidate(var D: TarihRecord);

function MonthsBetween(var DN, DP: TarihRecord): integer;

function NullDate(Date: TarihRecord): boolean;

function reversedate(s: string): string;

function KMKEY(km: float; dp: integer): string;

{convertx 'den alindi }
function evaluate(Formula: string; var errpos: integer): float;

{bitlib}

const
  bmask: array[0..31] of longint = ($00000001, $00000002, $00000004, $00000008,
    $00000010, $00000020, $00000040, $00000080, $00000100, $00000200, $00000400,
    $00000800, $00001000, $00002000, $00004000, $00008000, $00010000, $00020000,
    $00040000, $00080000, $00100000, $00200000, $00400000, $00800000, $01000000,
    $02000000, $04000000, $08000000, $10000000, $20000000, $40000000, $80000000);

type
  bitrange = 0..31;

  longintset = set of bitrange;

function bitval(bitnum: byte): longint;

function bittest(var x: longint; bitnum: bitrange): boolean;

procedure bitset(var x: longint; bitnum: byte);

procedure bitclr(var x: longint; bitnum: byte);

procedure bitcpl(var x: longint; bitnum: byte);

function bitcnt(x: longint): byte;

procedure bitsetl(var x; bitnum: word);

procedure bitclrl(var x; bitnum: word);

function PrepareLine(s: string): string;

procedure ANSI_TO_Turkish(var s: string);

procedure Turkish_To_ANSI(var s: string);

function A2T(s: string): string;

function T2A(s: string): string;

procedure ConvertANSIToTurkish(SL: TStrings);

procedure ConvertTurkishToANSI(SL: TStrings);

function DeleteTrailingBackSlash(const Windir: string): string;

function floatToStrPoint(const v: double): string;

function AsFloat(const opt: string; alphacontrol: boolean = false): double;

function ToEnglish(s3: string): string;

function getNewFileName(_path: string; var N: integer; Ext: string): string; overload;

function getNewFileName(_fn: string; d: string = ''): string; overload;

function getNewDirName(_path, DirName: string): string;

function putcommaX(s: string): string;

function FormatCommaX(const _Format: string; const Args: array of const; Comma:
  string = '.'; V: double = 0): string;

function AsFloatQuotedSt(const Value: Double): string;

/// <summary>
///   Parametre olarak gelen <b><u>RootGrName</u></b> deerine rastgele olu˛turulan
///   5 haneli say˝y˝ ekleyerek geriye dˆnd¸r¸r.
/// </summary>
function CreateRandomGroupName(RootGrName: String): String;  {Davulcu Ekleme}

{Grup Root isimleri}
const  {Davulcu Ekleme}
  GRNAlanTaramasi = '#AT';{D¸z tarama iÁinde kullan˝labilir}
  GRNBinaTaramasi = '#BT';{Bina kenar tarama iÁinde kullan˝labilir}
  GRNMerdivenTaramasi = '#MT';
  GRNSevTaramasi = '#ST';
  GRNYatayOlculendir = '#YO';
  GRNDikeyOlculendir = '#DO';
  GRNParalelOlculendir = '#PO';
  GRNAciOlculendir = '#AO';
  GRNYariCapOlculendir = '#YCO';
  GRNUzublukYazdir = '#UY';
  GRNParalelDikHat = '#PDH';
  GRNPoligonHatti = '#RPH';
  GRNKuturBagla = '#RKB';
  GRNDikDus = '#RDD';
  GRNAraMesafeYaz = '#AMY';
  GRNAliymanaCepheYaz = '#RACY';
  GRNKoordinatYaz = '#YXZ';
  GRNGrid = '#GRID';
  GRNEnKesit = '#ENK';

implementation

uses
  ComObj,
  ActiveX,
  Lider.CG.Com.System;

{$R- }
function atof(s: string): float;
var
  p: float;
  e: integer;
begin
  deleteall(' ', s);
  if pos('E', s) = 1 then
    s[1] := 'X';
  val(s, p, e);
  _A2NERROR := e <> 0;
  atof := p;
end;

function strcmp(const n1, n2: string): Integer;
begin
  strcmp := Ord(n1 = n2) + 2 * ord(n1 > n2) - 1;
end;

function Equals(s1, s2: string): boolean;
begin
  Deletealltrailing(' ', s1);
  Equals := s1 = s2;
end;

procedure DivideString(s: string; var s1, s2: string; ch: string);
var
  p: integer;
begin
  p := pos(ch, s);
  if p = 0 then
    inc(p, succ(Length(s)));
  delete(s, p, length(ch));
  s1 := copy(s, 1, p - 1);
  s2 := copy(s, p, Length(s) - p + 1);
end;

function Parse(var s: string; const ch: string): string;
var
  s1, s2: string;
begin
  DivideString(s, s1, s2, ch);
  s := s2;
  Parse := s1;
end;



{
Function Parse(Var s:string;const ch:String):string;
Var s1,s2 : String;
begin
    DivideString(s,s1,s2,ch);
    s:=s2;
    Parse:=s1;
end;
}

function ParseVAL(s: string; const ch: string): string;
begin
  ParseVAL := parse(s, ch);
end;

function ParseToEnd(var s: string; ch: string): string;
var
  s1, s2: string;
begin
  DivideString(s, s1, s2, ch);
  s := s1;
  ParseToEnd := s2;
end;
{
Function ParseVAL(s:string;const ch:string):string;
begin
   ParseVAL:=parse(s,ch);
end;

Function ParseToEnd(Var s:string;ch:string):string;
Var s1,s2 : String;
begin
    DivideString(s,s1,s2,ch);
    s:=s1;
    ParseToEnd:=s2;
end;
}

function strtok(var s: string; ps: string): string;
var
  k, i, p: integer;
  ch: char;
begin
  ch := #0;
  p := 256;
  for i := 1 to length(ps) do
  begin
    k := pos(ps[i], s);
    if (k <> 0) and (k < p) then
    begin
      p := k;
      ch := ps[i];
    end;
  end;
  strtok := parse(s, ch)
end;

function strtoke(var s: string; ps: string): string;
var
  t: string;
begin
  t := strtok(s, ps);
  strtoke := s;
  s := t;
end;

(*
   seperates the string s1 by chars in ps and returns the contiguous
   blocks of those in ps, up to that in s2 and the left in s1
*)
function tokenize(var s1, s2: string; ps: string): string;
var
  k, i, p: integer;
  ch: char;
  s: string;
begin
  ch := #0;
  p := 256;
  for i := 1 to length(ps) do
  begin
    k := pos(ps[i], s1);
    if (k <> 0) and (k < p) then
    begin
      p := k;
      ch := ps[i];
    end;
  end;
  s2 := parse(s1, ch);
  if p <> 256 then
    s := ch
  else
    s := '';
  i := 1;
  while (length(s1) > 0) and (pos(s1[i], ps) <> 0) do
  begin
    s := s + s1[i];
    delete(s1, 1, 1);
  end;
  tokenize := s;
end;

function Cut(s: string; maxlen: integer): string;
begin
  if (maxlen <> 0) and (length(s) > maxlen) then
    SetLength(s, maxlen);
  cut := s;
end;

procedure DeleteAll(sd: string; var s: string);
begin
  repeat
    delete(s, pos(sd, s), length(sd));
  until pos(sd, s) = 0;
end;

procedure deleteallleading(sd: string; var s: string);
begin
  while (length(s) > 0) and (pos(sd, s) = 1) do
    delete(s, 1, length(sd));
end;

procedure deletealltrailing(sd: string; var s: string);
begin
  while (length(s) > 0) and (copy(s, length(s) - length(sd) + 1, length(s)) = sd) do
    delete(s, length(s) - length(sd) + 1, length(sd));
end;

function LowCase(ch: char): char;
var
  p: integer;
begin
  p := pos(ch, BigABC);
  if p <> 0 then
    ch := Char(smallABC[p]); //li2016
  LowCase := ch;
end;

function XUpCase(ch: char): char;
var
  p: integer;
begin
  p := pos(ch, SmallABC);
  if p <> 0 then
    ch := Char(BigABC[p]); //li2016
  XUpCase := ch;
end;

{ Gets the position Of first char<>ch }
function PosNOT(ch: char; s: string): integer;
var
  P: integer;
begin
  PosNOT := 0;
  if length(s) = 0 then
    exit;
  P := 1;
  while (P <= Length(s)) and (s[P] = ch) do
    P := succ(P);
  if P > Length(s) then
    P := 0;
  posNOT := P;
end;

function Count(ch: char; s: string): integer;
var
  i, j: integer;
begin
  i := 0;
  ch := Upcase(ch);
  for j := 1 to Length(s) do
    if Upcase(s[j]) = ch then
      inc(i);
  Count := i;
end;

function Contain(what: charset; s: string): boolean;
var
  i: integer;
  OK: boolean;
begin
  OK := false;
  for i := 1 to Length(s) do
    OK := OK or (s[i] in what);
  Contain := OK;
end;

procedure DeleteSet(uset: charset; var s: string; var ExistsChar: boolean);
var
  i: integer;
  sd: string;
begin
  ExistsChar := false;
  sd := '';
  for i := 1 to length(s) do
  begin
    if not (s[i] in uset) then
    begin
      sd := sd + s[i];
    end
    else
    begin
      ExistsChar := true;
      BREAK;
    end;

  end;
  s := sd;
end;

function Replace(s, old, new: string): string;
var
  p: integer;
begin
  p := pos(old, s);
  if old = new then
    p := 0;
  while p <> 0 do
  begin
    delete(s, p, Length(old));
    insert(new, s, p);
    p := pos(old, s);
  end;
  replace := s;
end;

(*
function Replace(s,old,new: widestring):widestring;
var p : integer;
begin
   p:=pos(old,s);
   if old=new then p:=0;
   while p<>0 Do begin
     delete(s,p,Length(old));
     insert(new,s,p);
     p:=pos(old,s);
   end;
   replace:=s;
end;
*)
function replacec(s: string; cho, chn: char): string;
var
  i: integer;
begin
  for i := 1 to length(s) do
    if s[i] = cho then
      s[i] := chn;
  replacec := s;
end;

function replacenc(s: string; cho, chn: char): string;
var
  i: integer;
begin
  for i := 1 to length(s) do
    if s[i] <> cho then
      s[i] := chn;
  replacenc := s;
end;

function Spc(numofspc: integer): string;
var
  tmp: string;
begin
  if (numofspc > 0) and (numofspc < 255) then
  begin
    fillchar(tmp, succ(numofspc), ' ');
    SetLength(tmp, numofspc);
    Spc := tmp;
  end
  else
    Spc := ''
end;

function stc(s: integer; st: string): string;
var
  i: integer;
  temp: string;
begin
  if (s * Length(st)) > 255 then
    s := (255 div Length(st));
  temp := '';
  for i := 1 to s do
    temp := temp + st;
  stc := temp;
end;

function tocontrol(ch: char): char;
begin
  tocontrol := chr(ord(ch) - 64);
end;

function tonormal(ch: char): char;
begin
  tonormal := chr(ord(ch) + 64);
end;

function iscontrol(ch: char): boolean;
begin
  iscontrol := (ch <= #31);
end;

function isdigit(ch: char): boolean;
begin
  isdigit := (ch >= '0') and (ch <= '9');
end;

function IsUpper(ch: char): boolean;
begin
  isupper := isdigit(ch) or ((ch >= 'A') and (ch <= 'Z'));
end;

function lower(s: string): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    s[i] := Lowcase(s[i]);
  Lower := s;
end;

function upper(s: string): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    s[i] := Upcase(s[i]);
  Upper := s;
end;

function Xupper(s: string): string;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    s[i] := XUpcase(s[i]);
  XUpper := s;
end;

function deleteF(s: string; start, len: integer): string;
begin
  delete(s, start, len);
  deleteF := s;
end;

function Capitalize(s: string): string;
var
  i: integer;
begin
  s := Lower(s);
  s[1] := XUpcase(s[1]);
  for i := 1 to pred(Length(s)) do
    if (s[i] in [',', ' ', '_', ';', ':', '=']) then
      s[succ(i)] := XUpcase(s[succ(i)]);
  Capitalize := s;
end;

function StrCase(s: string; mode: integer): string;
begin
  case mode of
    0:
      s := Lower(s);
    1:
      s := Upper(s);
    2:
      s := Capitalize(s);
    3:
      s := XUpper(s);
  end;
  StrCase := s;
end;

function putcomma(s: string): string;
var
  i, j: integer;
  t: string;
begin
  t := '';
  j := 0;
  deleteall(' ', s);
  for i := length(s) downto 1 do
  begin
    t := s[i] + t;
    if ((j mod 3) = 2) and (i <> 1) then
      t := ',' + t;
    inc(j);
  end;
  putcomma := t;
end;

function numtostr(r: float): string;
const
  ones: array[0..9] of string[5] = ('s˝f˝r', 'bir', 'iki', '¸Á', 'dˆrt', 'be˛',
    'alt˝', 'yedi', 'sekiz', 'dokuz');
  tens: array[0..9] of string[6] = ('on', 'yirmi', 'otuz', 'k˝rk', 'elli',
    'altm˝˛', 'yetmi˛', 'seksen', 'doksan', 'y¸z');
  thous: array[0..5] of string[7] = ('', 'bin', 'milyon', 'milyar', 'trilyon', 'trilyar');
var
  str, sstr: string;
  r3: float;
  i: integer;

  function n999tostr(r: float): string;
  var
    str: string;
    digit: integer;
  begin
    str := '';
    if r < 0 then
      str := 'eksi ';
    r := abs(r);
    if r >= 100 then
    begin
      digit := round(int(r / 100));
      if digit = 1 then
        str := str + 'y¸z'
      else
        str := str + ones[digit] + 'y¸z';
      r := r - digit * 100;
    end;
    if r >= 10 then
    begin
      digit := round(int(r / 10));
      str := str + tens[digit - 1];
      r := r - digit * 10;
    end;
    if r >= 1 then
    begin
      digit := round(r);
      str := str + ones[digit];
          {r:=r-digit*10;}
    end;
    n999tostr := str;
  end;

begin
  str := '';
  i := 0;
  r := int(r) + round(frac(r));
  repeat
    r3 := r - int(r / 1000) * 1000;
    r := int((r - r3) / 1000);
    sstr := n999tostr(r3);
    if r3 <> 0 then
      str := sstr + thous[i] + str
    else
      str := sstr + str;
    inc(i);
  until r < 0.5;
  numtostr := str;
end;

function ToTurkish(s3: string): string;
const
  S1 = 'ﬁ˛–›˝‹¸÷ˆ«Á';
  S2 = 'SsGgIiUuOoCc';
var
  i, p: integer;
begin
  for i := 1 to length(s3) do
  begin
    p := pos(s3[i], s1);
    if p <> 0 then
      s3[i] := s2[p];
  end;
  ToTurkish := s3;
end;

(*
   format is <#name#,Fieldlength,Justify>
   if FieldLength<0 then all spaces are removed
   justify is the same as that of psj accepts (L,R,C)
*)

(*
function beval(var Formula:String;var ErrPos: Integer):boolean;
const
  Numbers: set of AnsiChar = ['0'..'1']; // ilker dei˛tirme
  EofLine  = ^M;
var
  Value: boolean;  { Result of formula }
  Pos: Integer;    { Current position in formula                     }
  Ch: Char;        { Current character being scanned                 }
{ Procedure NextCh returns the next character in the formula         }
{ The variable Pos contains the position ann Ch the character        }

  procedure NextCh;
  begin
    repeat
      Pos:=Pos+1;
      if Pos<=Length(Formula) then
      Ch:=Formula[Pos] else Ch:=eofline;
    until Ch<>' ';
  end  { NextCh };


  function Expression: boolean;
  var
    E: boolean;
    Opr: Char;

    function SimpleExpression: boolean;
    var
      S: boolean;
      Opr: Char;

      function Term: boolean;
      var
        T: boolean;

        function SignedFactor: boolean;

          function Factor: boolean;
          var
            F: boolean;
            Start:Integer;

          begin { Function Factor }
            if Ch in Numbers then
            begin
              Start:=Pos;
              repeat NextCh until not (Ch in Numbers);
              F:=(Formula[Start]='1');
            end else
            if Ch='(' then
            begin
              NextCh;
              F:=Expression;
              if Ch=')' then NextCh else ErrPos:=Pos;
            end else F:=false;
            Factor:=F;
          end { function Factor};

        begin { SignedFactor }
          if Ch='-' then begin
             NextCh; SignedFactor:= not Factor;
           end else SignedFactor:=Factor;
        end { SignedFactor };

      begin { Term }
        T:=SignedFactor;
        Term:=t;
      end { Term };

    begin { SimpleExpression }
      s:=term;
      while (Ch='&') or (ch='%') do
      begin
        Opr:=Ch; NextCh;
        if opr='&' then s:=term and s
          else s:=term xor s; { must be '%' }
      end;
      SimpleExpression:=s;
    end { SimpleExpression };

  begin { Expression }
    E:=SimpleExpression;
    while (Ch='+') or (ch='-') do
    begin
      Opr:=Ch; NextCh;
      if opr='+' then e:=SimpleExpression or e
        else e:=(not SimpleExpression) or e;
    end;
    Expression:=E;
  end { Expression };

begin { procedure Evaluate }
  deleteset(whole_set-[')','(','0'..'1','+','-','%','&'],formula);
  Pos:=0; NextCh;
  Value:=Expression;
  if Ch=EofLine then ErrPos:=0 else ErrPos:=Pos;
  beval:=value;
end { Evaluate };
*)

function Extract_Path(var FullPath: string): string;
begin
  Extract_Path := ExtractFilePath(ExpandFileName(fullpath));
  FullPath := ExtractFileName(ExpandFileName(fullpath));
end;

{Function Extract_ext(name:string):string;
begin
    Extract_ext:='.'+ParseToEnd(name,'.');
end;}

function Extract_name(name: string): string;
begin
  Extract_name := Parse(name, '.');
end;

function detab(line: string): string;
var
  k: integer;
begin
  repeat
    k := pos(#9, line);
    if (k <> 0) then
    begin
      delete(line, k, 1);
      insert(spc(9 - (k mod 8)), line, k);
    end;
  until k = 0;
  detab := line;
end;

procedure chipher(var si, so1, so2: string);
var
  i: integer;
begin
  randomize;
  SetLength(so1, Length(si));
  for i := 1 to length(si) do
    so1[i] := chr(random(255));
  SetLength(so2, Length(si));
  for i := 1 to length(si) do
    so2[length(si) - i + 1] := char(integer(ord(si[i]) + ord(so1[i])) mod 256);
  fillchar(si, sizeof(si), #0);
end;

function dechipher(so1, so2: string): string;
var
  si: string;
  i: integer;
begin
  SetLength(si, Length(so1));
  for i := 1 to length(si) do
    si[i] := char(integer(ord(so2[length(si) - i + 1]) - ord(so1[i]) + 256) mod 256);
  dechipher := si;
  fillchar(si, sizeof(si), #0);
end;

function reverse(s: string): string;
var
  i: integer;
  ch: char;
begin
  for i := 1 to (length(s) div 2) do
  begin
    ch := s[i];
    s[i] := s[length(s) - i + 1];
    s[length(s) - i + 1] := ch;
  end;
  reverse := s;
end;

function noblank(s: string): string;
begin
  deleteall(' ', s);
  deletealltrailing('0', s);
  noblank := s;
end;

function RandomName(l: integer): string;
var
  s: string;
  ATempfile: Longint;
begin
  ATempfile := 0;
  if IsLibrary then
  begin
    str(ATempfile:6, s);
    s := 'SM' + s;         //NML
    inc(ATempfile);
    s := replace(s, ' ', '0');
    Result := s;
  end
  else
  begin
    str(ATempfile:6, s);
    s := 'SW' + s;         //NML
    inc(ATempfile);
    s := replace(s, ' ', '0');
    Result := s;
  end;
end;

function _longkey(l1, l2: longint): string;
var
  s1, s2: string[10];
begin
  str(l1:10, s1);
  str(l2:10, s2);
  _longkey := s1 + s2;
end;

function _longrat(l1, l2: longint): string;
var
  s1, s2: string;
begin
  str(l1:10, s1);
  str(l2:10, s2);
  deleteall(' ', s1);
  deleteall(' ', s2);
  if l2 <> 0 then
    _longrat := s1 + '/' + s2
  else
    _longrat := s1;
end;

function validpname(var pname: string): boolean;
begin
  deleteall(' ', pname);
  pname := replace(pname, ',', '.');
  validpname := (pname <> '') and (pos('*', pname) = 0) and (pname[length(pname)]
    <> '.') and (pname[length(pname)] <> '/') and (count('.', pname) <= 1) and (pname
    [1] <> '/') and (pname[1] <> '.');
end;

procedure correctstringforplotting(var prompt: string);
var
  i: integer;
begin
  if pos('#', prompt) = 1 then
  begin { compressed }
    delete(prompt, 1, 1);
    for i := 1 to length(prompt) do
      if isdigit(prompt[i]) then
        prompt[i] := chr(ord(prompt[i]) + 128);
  end;
  if pos('@', prompt) = 1 then
  begin { italic }
    delete(prompt, 1, 1);
    for i := 1 to length(prompt) do
      if isdigit(prompt[i]) then
        prompt[i] := chr(ord(prompt[i]) + 144);
  end;
  if pos('^', prompt) = 1 then
  begin { kadastro , dashed text }
    delete(prompt, 1, 1);
    for i := 1 to length(prompt) do
      if isdigit(prompt[i]) then
      begin
        if prompt[i] = '0' then
          prompt[i] := chr(166)
        else if prompt[i] = '6' then
          prompt[i] := chr(153)
        else
          prompt[i] := chr(ord(prompt[i]) + 160);
      end;
  end;
end;
{$R+}

function erased(s: string): boolean;
begin
  erased := pos('*', s) <> 0;
end;

procedure addextention(var name: string; ext: string);
begin
  deleteall(' ', name);
  if (pos('.', name) = 0) and (name <> '') and (ext <> '') then
    name := name + '.' + ext;
end;

function allsame(s: string; st, en: integer): boolean;
var
  i: integer;
  f: boolean;
begin
  s := copy(s, st, en - st + 1);
  f := true;
  for i := 1 to length(s) do
    f := f and (s[1] = s[i]);
  allsame := f;
end;

function translateby(s, tchs: string): string; { used to determine order of Geometry.Points }
var
  i, p: integer;
(*
const tchs:string[16]='NPòB 0123456789ESKP';
*)
begin
  for i := 1 to length(s) do
  begin
    p := pos(s[i], tchs);
    if p <> 0 then
    begin
      if (s[i] = 'P') and (pos('.', s) = 0) then
      begin
      end
      else
        s[i] := chr(p);
    end;
  end;
  translateby := s;
end;

{ ********************* convert ********************** }

const
  h: string[16] = '0123456789ABCDEF';

function atoi(s: string): integer;
var
  p: integer;
  e: integer;
begin
  deleteall(' ', s);
  val(s, p, e);
  _A2NERROR := e <> 0;
  atoi := p;
end;

function atol(s: string): longint;
var
  p: longint;
  e: integer;
begin
  deleteall(' ', s);
  val(s, p, e);
  _A2NERROR := e <> 0;
  atol := p;
end;

function htoi(s: string): LongInt;
var
  temp: LongInt;
  i: integer;
begin
  temp := 0;
  for i := 1 to Length(s) do
    temp := 16 * temp + pred(pos(s[i], h));
  htoi := temp;
end;

function Bin(li: LongInt; nbit: byte): string;
var
  i: integer;
  s: string;
begin
  for i := 0 to Nbit - 1 do
  begin
    if odd(li) then
      s[NBit - i] := '1'
    else
      s[NBit - i] := '0';
    li := li div 2;
  end;
  SetLength(s, Nbit);
  bin := s;
end;

function hexb(b: byte): string;
var
  temp: string;
begin
  temp := '  ';
  temp[1] := Char(h[(b shr 4) + 1]); //li2016
  temp[2] := Char(h[(b and $F) + 1]); //li2016
  hexb := temp;
end;

function hexw(w: word): string;
var
  temp: string;
begin
  temp := '    ';
  temp[1] := Char(h[((w div 256) shr 4) + 1]); //li2016
  temp[2] := Char(h[((w div 256) and $F) + 1]); //li2016
  temp[3] := Char(h[((w mod 256) shr 4) + 1]); //li2016
  temp[4] := Char(h[((w mod 256) and $F) + 1]); //li2016
  hexw := temp;
end;

(* ilker silme kullan˝lm˝yor
function HexAdr(p: pointer): string;
type
  L = array[0..1] of Word;
begin
  HexAdr := Hexw(L(p)[1]) + ':' + Hexw(L(p)[0]);
end; *)

function Hexl(li: LongInt): string;
type
  L = array[0..1] of Word;
begin
  Hexl := Hexw(L(li)[1]) + ':' + Hexw(L(li)[0]);
end;

function HiWord(a: longint): word;
type
  L = array[0..1] of Word;
begin
  HiWord := L(a)[1];
end;

function LoWord(a: longint): word;
type
  L = array[0..1] of Word;
begin
  LoWord := L(a)[0];
end;

function rtos(r: float; fo1, fo2: integer): string;
begin
  repeat
    str(r:fo1:fo2, result);
    if length(result) > fo1 then str(r:fo1, result);
      if pos('E', result) <> 0 then
        inc(fo1);
  until (pos('E', result) = 0) or (fo1 > 20);
end;

function rtosb(r: float; dp: integer): string;
begin
  result := rtos(r, 20, dp);
  deleteall(' ', result);
end;

function rtosn(r: float): string;
begin
  str(r:16:0, result);
  deleteall(' ', result);
end;

function itos(i: integer; fo1: integer): string;
var
  s: string[10];
begin
  str(i:fo1,s);
  itos := s;
end;

function i2s(i: integer): string;
begin
  i2s := chr(Hi(i)) + chr(Lo(i));
end;

function li2s(i: longint): string;
type
  L = array[0..1] of Word;
begin
  li2s := chr(Hi(L(i)[1])) + chr(Lo(L(i)[1])) + chr(Hi(L(i)[0])) + chr(Lo(L(i)[0]))
end;

{ li2016 silme hiÁ bir yerde kullan˝lmam˝˛
function li2a(key:string):longint;
type L4 = array[0..3] of Ansichar; // ilker dei˛tirme
var
     t : longint;
begin
   if key='*' then key:=#0#0#0#0;
   L4(t)[3]:=Key[1];
   L4(t)[2]:=Key[2];
   L4(t)[1]:=Key[3];
   L4(t)[0]:=Key[4];
   li2a:=t;
end;}

function ltos(l: longint; fo1: integer): string;
begin
  ltos := rtos(l, fo1, 0);
end;

function ltosn(l: longint): string;
begin
  ltosn := rtosn(l);
end;

function Ratio(s: string): float;
var
  r1, r2: float;
begin
  r1 := atof(parse(s, '/'));
  if s = '' then
    s := '1';
  r2 := atof(s);
  if r2 <> 0.0 then
    Ratio := r1 / r2
  else
    Ratio := 0.0;
end;

function stodate(t: string; var D: TarihRecord): boolean;
var
  p1, p2, p3: integer;
  ok: boolean;
begin
  deleteall(' ', t);
  ok := count('/', t) = 2;
  p1 := 0;
  p2 := 0;
  p3 := 0;
  if ok then
  begin
    p1 := atoi(Parse(t, '/'));
    ok := (p1 > 0) and (p1 <= 31);
    if ok then
    begin
      p2 := atoi(Parse(t, '/'));
      ok := (p2 > 0) and (p2 <= 12);
      if ok then
      begin
        p3 := atoi(t);
        if p3 > 1900 then
          p3 := p3 - 1900;
        ok := (p3 > 0) and (p3 <= 99);
      end;
    end;
  end;
  if ok then
  begin
    D.gun := p1;
    D.ay := p2;
    D.yil := p3;
  end
  else
  begin
    D.gun := 0;
    D.ay := 0;
    D.yil := 0;
  end;
  stodate := ok;
end;

procedure dateinvalidate(var D: TarihRecord);
begin
  with D do
  begin
    if (gun < 1) or (gun > 31) then
      gun := 0;
    if (ay < 1) or (ay > 12) then
      ay := 0;
    if (yil < 1) or (yil > 99) then
      yil := 0;
  end;
end;

function MonthsBetween(var DN, DP: TarihRecord): integer;
begin
  MonthsBetween := integer(DN.yil - DP.yil) * 12 + integer(DN.ay - DP.ay);
end;

function NullDate(Date: TarihRecord): boolean;
begin
  with Date do
    NullDate := (yil = 0) and (ay = 0) and (gun = 0);
end;

function reversedate(s: string): string;
var
  t1, t2: st2;
begin
  t1 := parse(s, '/');
  t2 := parse(s, '/');
  s := s + t2 + t1;
  deleteall(' ', s);
  reversedate := s;
end;

function KMKEY(km: float; dp: integer): string;
begin
  KMKEY := rtosb(km, dp);
end;

function evaluate(Formula: string; var errpos: integer): float;
const
  EofLine = ^M;

  function isdigit(ch: char): boolean;
  begin
    isdigit := (ch >= '0') and (ch <= '9');
  end;

var
  Pos: Integer;    { Current position in formula                     }
  Ch: Char;        { Current character being scanned                 }
  Value: float;
{ Procedure NextCh returns the next character in the formula         }
{ The variable Pos contains the position ann Ch the character        }

  procedure NextCh;
  begin
    repeat
      inc(Pos);
      if Pos <= Length(Formula) then
        Ch := Formula[Pos]
      else
        Ch := eofline;
    until Ch <> ' ';
  end  { NextCh };

  function Expression: float;
  var
    E: float;
    Opr: Char;

    function SimpleExpression: float;
    var
      S: float;
      Opr: Char;

      function Term: float;
      var
        T: float;

        function SignedFactor: float;

          function Factor: float;
          type
            StandardFunction = (fabs, fsqrt, fsqr, fsin, fcos, farctan, fln,
              flog, fexp, fd2r, fr2d, fsgn, fint, ffrac, finv, fadm, frand, ffact);

            StandardFunctionList = array[StandardFunction] of string[4];
          const
            StandardFunctionNames: StandardFunctionList = ('ABS', 'SQRT', 'SQR',
              'SIN', 'COS', 'ATAN', 'LN', 'LOG', 'EXP', 'D2R', 'R2D', 'SGN',
              'INT', 'FRAC', 'INV', 'ADM', 'RAN', 'FACT');
          var
            L: Integer;       { intermediate variables }
            Found: Boolean;
            F: float;
            Sf: StandardFunction;
            Start: Integer;

            function Fact(N: Integer): float;
            var
              i: integer; { may not have enough stack space }
            begin
              if N <= 69 then
              begin
                result := 1;
                for i := 2 to N do
                  result := result * i;
              end
              else
                result := -1;
            end  { Fact };

          begin { Function Factor }
            F := 0;
            if isdigit(Ch) then
            begin
              Start := Pos;
              repeat
                NextCh
              until not isdigit(Ch);
              if Ch = '.' then
                repeat
                  NextCh
                until not isdigit(Ch);
              if Ch = 'E' then
              begin
                NextCh;
                repeat
                  NextCh
                until not isdigit(Ch);
              end;
              Val(Copy(Formula, Start, Pos - Start), F, ErrPos);
            end
            else if Ch = '(' then
            begin
              NextCh;
              F := Expression;
              if Ch = ')' then
                NextCh
              else
                ErrPos := Pos;
            end
            else
            begin
              found := false;
              for sf := fabs to ffact do
                if not found then
                begin
                  l := Length(StandardFunctionNames[sf]);
                  if copy(Formula, Pos, l) = StandardFunctionNames[sf] then
                  begin
                    Pos := Pos + l - 1;
                    NextCh;
                    F := Factor;
                    case sf of
(*
Fonksiyonlar
Genel Kullanim    : FONKSIYON(X) Seklinde olmalidir
ABS         : Mutlak Deger
SQRT        : Karekok
SQR         : Kare
SIN         : Sinus Arguman Grad olmali
COS         : Cosinus Arguman Grad olmali
ATAN        : Arc tanjant sonuc grad cinsinden

LN','LOG',
             'EXP','D2R','R2D','SGN','INT','FRAC','INV',
             'ADM','RAN','FACT');
*)
                      frand:
                        f := (random - 0.5) * f;
                      fabs:
                        f := abs(f);
                      fsqrt:
                        begin
                          try
                            f := sqrt(f);
                          except
                            f := 0;
                          end;
                        end;
                      fsqr:
                        f := sqr(f);
                      fsin:
                        f := sin(f * pi / 200);
                      fcos:
                        f := cos(f * pi / 200);
                      farctan:
                        f := arctan(f) * 200 / pi;
                      fln:
                        f := ln(f);
                      flog:
                        f := ln(f) / ln(10);
                      fexp:
                        f := exp(f);
                      fd2r:
                        f := pi / 180.0 * f;
                      fr2d:
                        f := 180.0 / pi * f;
                      finv:
                        f := 1 / f;
                      fint:
                        f := int(f);
                      ffrac:
                        f := frac(f);
                      fadm:
                        if f < 0 then
                          f := 0.0;
                      fsgn:
                        if f > 0 then
                          f := 1.0
                        else if f < 0 then
                          f := -1.0
                        else
                          f := 0.0;
                      ffact:
                        f := fact(trunc(f));
                    end;
                    Found := true;
                  end;
                end;
              if not Found then
                ErrPos := Pos;
            end;
            Factor := F;
          end { function Factor};

        begin { SignedFactor }
          if Ch = '-' then
          begin
            NextCh;
            SignedFactor := -Factor;
          end
          else
            SignedFactor := Factor;
        end { SignedFactor };

      begin { Term }
        T := SignedFactor;
        while Ch = '^' do
        begin
          NextCh;
          t := exp(ln(t) * SignedFactor);
        end;
        Term := t;
      end { Term };

    begin { SimpleExpression }
      s := term;
      while (Ch = '*') or (ch = '/') do
      begin
        Opr := Ch;
        NextCh;
        if opr = '*' then
          s := s * term
        else
          s := s / term;
      end;
      SimpleExpression := s;
    end { SimpleExpression };

  begin { Expression }
    E := SimpleExpression;
    while (Ch = '+') or (ch = '-') do
    begin
      Opr := Ch;
      NextCh;
      if opr = '+' then
        e := e + SimpleExpression
      else
        e := e - SimpleExpression;
    end;
    Expression := E;
  end { Expression };

begin { procedure Evaluate }
  if Formula = '' then
    Formula := '0';
  if Formula[1] = '.' then
    Formula := '0' + Formula;
  if Formula[1] = '+' then
    delete(Formula, 1, 1);
  Pos := 0;
  NextCh;
  Value := Expression;
  if Ch = EofLine then
    ErrPos := 0
  else
    ErrPos := Pos;
  if errpos = 0 then
    evaluate := value
  else
  begin
    evaluate := -1;
  end;
end { Evaluate };

{******************* bitlib *****************}
function bitval(bitnum: byte): longint;
var
  l: longint;
begin
  l := $0001;
  bitnum := bitnum and 31;
  bitval := l shl bitnum;
end;

function bittest(var x: longint; bitnum: bitrange): boolean;
begin
  bittest := (x and bmask[bitnum]) <> 0;
end;

procedure bitset(var x: longint; bitnum: byte);
begin
  x := x or bmask[bitnum];
end;

procedure bitsetl(var x; bitnum: word);
var
  b: array[0..0] of byte;
  bytenum: word;
begin
  bytenum := bitnum div 8;
  bitnum := bitnum mod 8;
  b[bytenum] := b[bytenum] or byte(bitval(bitnum));
end;

procedure bitclrl(var x; bitnum: word);
var
  b: array[0..0] of byte;
  bytenum: word;
begin
  bytenum := bitnum div 8;
  bitnum := bitnum mod 8;
  b[bytenum] := b[bytenum] and (not byte(bitval(bitnum)));
end;

procedure bitclr(var x: longint; bitnum: byte);
begin
  x := x and (not bitval(bitnum));
end;

procedure bitcpl(var x: longint; bitnum: byte);
begin
  x := x xor bitval(bitnum);
end;

function bitcnt(x: longint): byte;
var
  cnt: byte;
begin
  cnt := 0;
  while x <> 0 do
  begin
    inc(cnt, ord(odd(x)));
    x := x div 2;
  end;
  bitcnt := cnt;
end;

function PrepareLine(s: string): string;
begin
  s := replace(s, #9, ' ');
  s := replace(s, '  ', ' ');
  deleteallleading(' ', s);
  deletealltrailing(' ', s);
  prepareline := s;
end;

procedure ANSI_TO_Turkish(var s: string);
{WINDOWS to DOS convertion }
var
  i, q: integer;
begin
  for i := 1 to length(s) do
  begin
    for q := 1 to 12 do
      if ord(s[i]) = ANSI_set[q] then
      begin
        s[i] := chr(TURK_set[q]);
        break;
      end;
  end;
end;

function A2T(s: string): string;
begin
  Ansi_to_Turkish(s);
  A2T := s;
end;

procedure Turkish_To_ANSI(var s: string);
{DOS to WINDOWS convertion }
var
  i, q: integer;
begin
  for i := 1 to length(s) do
  begin
    for q := 1 to 12 do
      if ord(s[i]) = TURK_set[q] then
      begin
        s[i] := chr(ANSI_set[q]);
        break;
      end;
  end;
end;

function T2A(s: string): string;
begin
  Turkish_to_ansi(s);
  T2A := s;
end;

procedure ConvertANSIToTurkish(SL: TStrings);
{WINDOWS to DOS group convertion of TStrings}
var
  i: integer;
  s: string;
begin
  for i := 0 to SL.Count - 1 do
  begin
    s := SL[i];
    ANSI_TO_Turkish(s);
    SL[i] := s;
  end;
end;

procedure ConvertTurkishToANSI(SL: TStrings);
{DOS to WINDOWS group convertion of TStrings}
var
  i: integer;
  s: string;
begin
  for i := 0 to SL.Count - 1 do
  begin
    s := SL[i];
    Turkish_To_ANSI(s);
    SL[i] := s;
  end;
end;

function ShortFileName(s: string; w: integer): string;
var
  Dirs: array[0..20] of string[20];
  t, t1: string;
  i: integer;
begin
  if length(s) < w then
    result := s
  else
  begin
    t := ExtractFilePath(s);
    i := 0;
    while t <> '' do
    begin
      Dirs[i] := parse(t, '\') + '\';
      inc(i);
    end;
    if i = 1 then
    begin
      result := s;
    end
    else
    begin
      t := Dirs[0];
      t1 := '\' + ExtractFileName(s);
      if length(t + t1 + Dirs[1]) < w then
      begin
        t := t + Dirs[1];
        if length(t + t1 + Dirs[2]) < w then
        begin
          t := t + Dirs[2];
          if length(t + t1 + Dirs[3]) < w then
          begin
            t := t + Dirs[3];
            if length(t + t1 + Dirs[4]) < w then
            begin
              t := t + Dirs[4];
            end
            else
              t := t + '...' + t1;
          end
          else
            t := t + '...' + t1;
        end
        else
          t := t + '...' + t1;
      end
      else
        t := t + '...' + t1;
      result := t;
    end;
  end;
end;

function DeleteTrailingBackSlash(const Windir: string): string;
begin
  result := WinDir;
  DeleteAllTrailing('\', result);
end;

function floatToStrPoint(const v: double): string;
begin
  Result := StringReplace(FloattoSTr(v), ',', '.', [rfReplaceAll]);
end;

function ToEnglish(s3: string): string;
const
  S1 = 'ﬁ˛–›˝‹¸÷ˆ«Á';
  S2 = 'SsGgIiUuOoCc';
var
  i, p: integer;
begin
  for i := 1 to length(s3) do
  begin
    p := pos(s3[i], s1);
    if p <> 0 then
      s3[i] := s2[p];
  end;
  ToEnglish := s3;
end;

function AsFloat(const opt: string; alphacontrol: boolean = false): double;
var
  Value: string;
  i, k: integer;
  ExistsChar: boolean;
begin

  try

    Value := opt;

    ExistsChar := false;

    value := StringReplace(value, ' ', '', [rfReplaceAll]);
    value := StringReplace(value, '(', '', [rfReplaceAll]);
    value := StringReplace(value, ')', '', [rfReplaceAll]);
    value := StringReplace(value, '%', '', [rfReplaceAll]);

    if alphacontrol then
      DeleteSet(whole_set - Numerics - noktavirgulSet, value, ExistsChar);

    if ExistsChar or (Value = '') or (Value = '-') or (Value = '.') or (Value = ',') then
    begin
      Result := 0;  //<-
      exit;
    end;

    //Value :=  StringReplace(Value,SysUtils.ThousandSeparator,'',[rfReplaceAll]);
    Value := StringReplace(Value, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
    Value := StringReplace(Value, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);

    k := 0;
    for i := length(Value) downto 1 do
    begin
      if FormatSettings.DecimalSeparator = (value[i]) then
      begin
        if k = 0 then
        begin
          value[i] := ';';
          inc(k);
        end
        else
          value[i] := '*';

      end;
    end;

    Value := StringReplace(Value, '*', '', [rfReplaceAll]);
    value := StringReplace(Value, ';', FormatSettings.DecimalSeparator, [rfReplaceAll]);

    result := StrToFloat(value);
  except
    Result := 0;

  end;
end;

function getNewFileName(_fn: string; d: string = ''): string;
var
  ext, n, prefix: string;
  path: string;
  i: integer;
begin
  n := ChangeFileExt(ExtractFileName(_fn), '');

  if d <> '' then
    prefix := parse(n, d)
  else
    prefix := n;

  path := ExtractFilePath(_fn);
  ext := ExtractFileExt(_fn);
  result := path + prefix + d + '1' + ext;

  i := 1;
  while FileExists(result) do
  begin
    inc(i);
    result := path + prefix + d + inttostr(i) + ext;
  end;

end;

function getNewFileName(_path: string; var N: integer; Ext: string): string;
var
  APath: string;
begin
  APath := AddSlash(APath);
  Result := ChangeFileExt(APath + IntToStr(N), ext);

  while FileExists(Result) do
  begin
    Inc(N);
    Result := ChangeFileExt(APath + IntToStr(N), ext);
  end;
end;

function getNewDirName(_path, DirName: string): string;
var
  APath: string;
  N: integer;
begin
  N := 1;
  APath := AddSlash(_path);
  Result := APath + DirName + IntToStr(N);

  while DirectoryExists(Result) do
  begin
    Inc(N);
    Result := APath + DirName + inttostr(N);
  end;
end;

function putcommaX(s: string): string;
var
  t, t1, t2, t3: string;
begin
  s := StringReplace(s, ',', '.', [rfReplaceAll]);
  t := s;
  t1 := parse(t, '-');
  if t = '' then
  begin
    t := s;
    t1 := '';
  end
  else
    t1 := '-';

  t2 := parse(t, '.');
  t3 := t;
  t2 := PutComma(t2);
  Result := t1 + t2 + '.' + t3;
end;

function FormatCommaX(const _Format: string; const Args: array of const; Comma:string = '.'; V: double = 0): string;
begin
  try
    Result := Format(_Format, Args);
    Result := putcommaX(Result);
    if Comma = '.' then
    begin
      Result := StringReplace(Result, '.', '/', [rfReplaceAll]);
      Result := StringReplace(Result, ',', '.', [rfReplaceAll]);
      Result := StringReplace(Result, '/', ',', [rfReplaceAll]);
    end;

  except
    on e: exception do
      showmessage(FloatToStr(V));
  end;
end;

function AsFloatQuotedSt(const Value: Double): string;
begin
  Result := QuotedStr(StringReplace(FloatToStr((Value)), ',', '.', [rfReplaceAll]));
end;

function GetDateStr_YAG(Date: TDateTime): string;  // y˝l, ay, gun
var
  y, g, a, s: word;
  sy, sg, sa, ss: string;
begin

  try
      //ShowMessage('1');

    DecodeDate(Date, y, a, g);

    sa := inttostr(a);
    if Length(sa) = 1 then
      sa := '0' + sa;
    sg := inttostr(g);
    if Length(sg) = 1 then
      sg := '0' + sg;

    result := inttostr(y) + '-' + sa + '-' + sg;

  except
  end;
end;

function CreateRandomGroupName(RootGrName: String): String;  {Davulcu Ekleme}
Var
  Rdm: Integer;
begin
  Randomize;
  Rdm := RandomRange(10000, 99000);
  if RootGrName.StartsWith('#') then
    Result := RootGrName + IntToStr(Rdm)
  else
    Result := '#' + RootGrName + IntToStr(Rdm);
end;
end.


