unit Lider.CG.Com.GeoUTM;

interface

uses
  Windows,
  Math,
  Classes,
  Sysutils;

type
  TDatum3Enum = (deED50, deWGS84, deGRS80, deBessel1841, deClarke1880);

  TDatum3 = class
    a,     // Büyük Yarý Eksen
    b,     // Küçük Yarý Eksen
    alfa,  // Basýklýk
    teta: double; // Elipsoidal Çoðrafi Enlem
    constructor create(_a, _b, _alfa: double);
    function M: double;     // Meridyen eðrilik yarýçapý
    function N: Double;     // Mareidyene dik doðrultudaki eðrilik yarýçapý
    function NuKare: double;
    function t: double;
    function ekare: double; // 1.Eksentirisite
    function _ekare: double; //2.Eksentirisite

  end;

function GeoToUtm3(Enlem, Boylam: double; _datum: TDatum3Enum; var Saga, Yukari:
  double): Boolean;

function Utm3ToGeo(Saga, Yukari: double; _datum: TDatum3Enum; DOM: double; var
  Enlem, Boylam: double): Boolean;

var
  ED50_3: TDatum3;
  WGS84_3: TDatum3;
  GRS80_3: TDatum3;
  Bessel1841: TDatum3;
  Clarke1880: TDatum3;

//procedure writeDatumParameterIni;
//procedure readDatumParameterIni;
procedure resetDatumParameter;

implementation

function getAlfa(_datum: TDatum3Enum): double;
var
  t: double;
begin
  case _datum of
    deED50:
      begin
        t := sqr(ED50_3.a) / ED50_3.b;
        result := t * (1 - 3 * ED50_3._ekare / 4 + 45 * power(ED50_3._ekare, 2)
          / 64 - 175 * power(ED50_3._ekare, 3) / 256 + 11025 * power(ED50_3._ekare,
          4) / 16384);
      end;

    deWGS84:
      begin
        t := sqr(WGS84_3.a) / WGS84_3.b;
        result := t * (1 - 3 * WGS84_3._ekare / 4 + 45 * power(WGS84_3._ekare, 2)
          / 64 - 175 * power(WGS84_3._ekare, 3) / 256 + 11025 * power(WGS84_3._ekare,
          4) / 16384);

      end;

    deGRS80:
      begin
        t := sqr(GRS80_3.a) / GRS80_3.b;
        result := t * (1 - 3 * GRS80_3._ekare / 4 + 45 * power(GRS80_3._ekare, 2)
          / 64 - 175 * power(GRS80_3._ekare, 3) / 256 + 11025 * power(GRS80_3._ekare,
          4) / 16384);

      end;

    deBessel1841:
      begin
        t := sqr(Bessel1841.a) / Bessel1841.b;
        result := t * (1 - 3 * Bessel1841._ekare / 4 + 45 * power(Bessel1841._ekare,
          2) / 64 - 175 * power(Bessel1841._ekare, 3) / 256 + 11025 * power(Bessel1841._ekare,
          4) / 16384);

      end;

    deClarke1880:
      begin
        t := sqr(Clarke1880.a) / Clarke1880.b;
        result := t * (1 - 3 * Clarke1880._ekare / 4 + 45 * power(Clarke1880._ekare,
          2) / 64 - 175 * power(Clarke1880._ekare, 3) / 256 + 11025 * power(Clarke1880._ekare,
          4) / 16384);

      end;

  end;
end;

function getBeta(_datum: TDatum3Enum): double;
var
  t: double;
begin
  case _datum of
    deED50:
      begin
        t := -sqr(ED50_3.a) / (2 * ED50_3.b);
        result := t * (3 * ED50_3._ekare / 4 - 15 * power(ED50_3._ekare, 2) / 16
          + 525 * power(ED50_3._ekare, 3) / 512 - 2205 * power(ED50_3._ekare, 4) / 2048);
      end;

    deWGS84:
      begin
        t := -sqr(WGS84_3.a) / (2 * WGS84_3.b);
        result := t * (3 * WGS84_3._ekare / 4 - 15 * power(WGS84_3._ekare, 2) /
          16 + 525 * power(WGS84_3._ekare, 3) / 512 - 2205 * power(WGS84_3._ekare,
          4) / 2048);
      end;

    deGRS80:
      begin
        t := -sqr(GRS80_3.a) / (2 * GRS80_3.b);
        result := t * (3 * GRS80_3._ekare / 4 - 15 * power(GRS80_3._ekare, 2) /
          16 + 525 * power(GRS80_3._ekare, 3) / 512 - 2205 * power(GRS80_3._ekare,
          4) / 2048);
      end;

    deBessel1841:
      begin
        t := -sqr(Bessel1841.a) / (2 * Bessel1841.b);
        result := t * (3 * Bessel1841._ekare / 4 - 15 * power(Bessel1841._ekare,
          2) / 16 + 525 * power(Bessel1841._ekare, 3) / 512 - 2205 * power(Bessel1841._ekare,
          4) / 2048);
      end;

    deClarke1880:
      begin
        t := -sqr(Clarke1880.a) / (2 * Clarke1880.b);
        result := t * (3 * Clarke1880._ekare / 4 - 15 * power(Clarke1880._ekare,
          2) / 16 + 525 * power(Clarke1880._ekare, 3) / 512 - 2205 * power(Clarke1880._ekare,
          4) / 2048);
      end;

  end;
end;

function getGama(_datum: TDatum3Enum): double;
var
  t: double;
begin
  case _datum of
    deED50:
      begin
        t := sqr(ED50_3.a) / (4 * ED50_3.b);
        result := t * (15 * power(ED50_3._ekare, 2) / 64 - 105 * power(ED50_3._ekare,
          3) / 526 + 2205 * power(ED50_3._ekare, 4) / 4096);
      end;

    deWGS84:
      begin
        t := sqr(WGS84_3.a) / (4 * WGS84_3.b);
        result := t * (15 * power(WGS84_3._ekare, 2) / 64 - 105 * power(WGS84_3._ekare,
          3) / 526 + 2205 * power(WGS84_3._ekare, 4) / 4096);
      end;

    deGRS80:
      begin
        t := sqr(GRS80_3.a) / (4 * GRS80_3.b);
        result := t * (15 * power(GRS80_3._ekare, 2) / 64 - 105 * power(GRS80_3._ekare,
          3) / 526 + 2205 * power(GRS80_3._ekare, 4) / 4096);

      end;

    deBessel1841:
      begin
        t := sqr(Bessel1841.a) / (4 * Bessel1841.b);
        result := t * (15 * power(Bessel1841._ekare, 2) / 64 - 105 * power(Bessel1841._ekare,
          3) / 526 + 2205 * power(Bessel1841._ekare, 4) / 4096);
      end;

    deClarke1880:
      begin
        t := sqr(Clarke1880.a) / (4 * Clarke1880.b);
        result := t * (15 * power(Clarke1880._ekare, 2) / 64 - 105 * power(Clarke1880._ekare,
          3) / 526 + 2205 * power(Clarke1880._ekare, 4) / 4096);

      end;

  end;
end;

function getDelta(_datum: TDatum3Enum): double;
var
  t: double;
begin
  case _datum of
    deED50:
      begin
        t := sqr(ED50_3.a) / (6 * ED50_3.b);
        result := t * (35 * power(ED50_3._ekare, 3) / 512 - 215 * power(ED50_3._ekare,
          4) / 2048);
      end;

    deWGS84:
      begin
        t := sqr(WGS84_3.a) / (6 * WGS84_3.b);
        result := t * (35 * power(WGS84_3._ekare, 3) / 512 - 215 * power(WGS84_3._ekare,
          4) / 2048);
      end;

    deGRS80:
      begin
        t := sqr(GRS80_3.a) / (6 * GRS80_3.b);
        result := t * (35 * power(GRS80_3._ekare, 3) / 512 - 215 * power(GRS80_3._ekare,
          4) / 2048);
      end;

    deBessel1841:
      begin
        t := sqr(Bessel1841.a) / (6 * Bessel1841.b);
        result := t * (35 * power(Bessel1841._ekare, 3) / 512 - 215 * power(Bessel1841._ekare,
          4) / 2048);
      end;

    deClarke1880:
      begin
        t := sqr(Clarke1880.a) / (6 * Clarke1880.b);
        result := t * (35 * power(Clarke1880._ekare, 3) / 512 - 215 * power(Clarke1880._ekare,
          4) / 2048);
      end;

  end;
end;

function getB(Enlem{birim : radyan} : double; _datum: TDatum3Enum): double;
var
  _Alfa, _Beta, _Gama, _Delta: double;
begin
  _Alfa := getAlfa(_datum);
  _Beta := getBeta(_datum);
  _Gama := getGama(_datum);
  _Delta := getDelta(_datum);

  result := _Alfa * Enlem + _Beta * Sin(2 * Enlem) + _Gama * Sin(4 * Enlem) +
    _Delta * Sin(6 * Enlem);

end;

function XGaussK(Enlem{radian}, DeltaLamda: Double; _datum: TDatum3Enum): double;
var
  B: Double;
begin
  B := getB(Enlem, _datum);
  case _datum of
    deED50:
      begin
        result := B + (ED50_3.N * power(Cos(Enlem), 2) * Tan(Enlem) * sqr(DeltaLamda)
          / 2) + (ED50_3.N * power(Cos(Enlem), 4) * Tan(Enlem) * (5 - power(Tan(Enlem),
          2) + 9 * ED50_3.NuKare) * power(DeltaLamda, 4)) / 24;
      end;

    deWGS84:
      begin
        result := B + (WGS84_3.N * power(Cos(Enlem), 2) * Tan(Enlem) * sqr(DeltaLamda)
          / 2) + (WGS84_3.N * power(Cos(Enlem), 4) * Tan(Enlem) * (5 - power(Tan
          (Enlem), 2) + 9 * WGS84_3.NuKare) * power(DeltaLamda, 4)) / 24;
      end;

    deGRS80:
      begin
        result := B + (GRS80_3.N * power(Cos(Enlem), 2) * Tan(Enlem) * sqr(DeltaLamda)
          / 2) + (GRS80_3.N * power(Cos(Enlem), 4) * Tan(Enlem) * (5 - power(Tan
          (Enlem), 2) + 9 * GRS80_3.NuKare) * power(DeltaLamda, 4)) / 24;
      end;

    deBessel1841:
      begin
        result := B + (Bessel1841.N * power(Cos(Enlem), 2) * Tan(Enlem) * sqr(DeltaLamda)
          / 2) + (Bessel1841.N * power(Cos(Enlem), 4) * Tan(Enlem) * (5 - power(Tan
          (Enlem), 2) + 9 * Bessel1841.NuKare) * power(DeltaLamda, 4)) / 24;
      end;

    deClarke1880:
      begin
        result := B + (Clarke1880.N * power(Cos(Enlem), 2) * Tan(Enlem) * sqr(DeltaLamda)
          / 2) + (Clarke1880.N * power(Cos(Enlem), 4) * Tan(Enlem) * (5 - power(Tan
          (Enlem), 2) + 9 * Clarke1880.NuKare) * power(DeltaLamda, 4)) / 24;
      end;

  end;

end;

function YGaussK(Enlem{radian}, DeltaLamda: Double; _datum: TDatum3Enum): double;
begin
  case _datum of
    deED50:
      begin
        result := (ED50_3.N * Cos(Enlem) * DeltaLamda) + (ED50_3.N * power(Cos(Enlem),
          3) * (1 - power(Tan(Enlem), 2) + ED50_3.NuKare) * Power(DeltaLamda, 3))
          / 6 + (ED50_3.N * power(cos(Enlem), 5) * (5 - 18 * power(Tan(Enlem), 2)
          + power(tan(Enlem), 4)) * power(DeltaLamda, 5)) / 120;
      end;

    deWGS84:
      begin
        result := (WGS84_3.N * Cos(Enlem) * DeltaLamda) + (WGS84_3.N * power(Cos
          (Enlem), 3) * (1 - power(Tan(Enlem), 2) + WGS84_3.NuKare) * Power(DeltaLamda,
          3)) / 6 + (WGS84_3.N * power(cos(Enlem), 5) * (5 - 18 * power(Tan(Enlem),
          2) + power(tan(Enlem), 4)) * power(DeltaLamda, 5)) / 120;

      end;

    deGRS80:
      begin
        result := (GRS80_3.N * Cos(Enlem) * DeltaLamda) + (GRS80_3.N * power(Cos
          (Enlem), 3) * (1 - power(Tan(Enlem), 2) + GRS80_3.NuKare) * Power(DeltaLamda,
          3)) / 6 + (GRS80_3.N * power(cos(Enlem), 5) * (5 - 18 * power(Tan(Enlem),
          2) + power(tan(Enlem), 4)) * power(DeltaLamda, 5)) / 120;

      end;

    deBessel1841:
      begin
        result := (Bessel1841.N * Cos(Enlem) * DeltaLamda) + (Bessel1841.N *
          power(Cos(Enlem), 3) * (1 - power(Tan(Enlem), 2) + Bessel1841.NuKare)
          * Power(DeltaLamda, 3)) / 6 + (Bessel1841.N * power(cos(Enlem), 5) * (5
          - 18 * power(Tan(Enlem), 2) + power(tan(Enlem), 4)) * power(DeltaLamda,
          5)) / 120;

      end;

    deClarke1880:
      begin
        result := (Clarke1880.N * Cos(Enlem) * DeltaLamda) + (Clarke1880.N *
          power(Cos(Enlem), 3) * (1 - power(Tan(Enlem), 2) + Clarke1880.NuKare)
          * Power(DeltaLamda, 3)) / 6 + (Clarke1880.N * power(cos(Enlem), 5) * (5
          - 18 * power(Tan(Enlem), 2) + power(tan(Enlem), 4)) * power(DeltaLamda,
          5)) / 120;

      end;

  end;

end;

function getLamda0(const _L: double): double;
var
  d, n: double;
begin
  // 24,27,30,33,36,39,42,45,
  // 22.50, 25.50

  result := 3 * int((_L + 1.5) / 3);
  exit;

  if (_L >= 22.50) and (_L < 25.50) then
  begin
    result := 24;
    exit;
  end;

  if (_L >= 25.50) and (_L < 28.50) then
  begin
    result := 27;
    exit;
  end;

  if (_L >= 28.50) and (_L < 31.50) then
  begin
    result := 30;
    exit;
  end;

  if (_L >= 31.50) and (_L < 34.50) then
  begin
    result := 33;
    exit;
  end;

  if (_L >= 34.50) and (_L < 37.50) then
  begin
    result := 36;
    exit;
  end;

  if (_L >= 37.50) and (_L < 40.50) then
  begin
    result := 39;
    exit;
  end;

  if (_L >= 40.50) and (_L < 43.50) then
  begin
    result := 42;
    exit;
  end;

  if (_L >= 43.50) and (_L < 46.50) then
  begin
    result := 45;
    exit;
  end;

  if (_L >= 46.50) and (_L < 49.50) then
  begin
    result := 48;
    exit;
  end;

  result := 27;

end;

function GeoToUtm3(Enlem, Boylam: double; _datum: TDatum3Enum; var Saga, Yukari:
  double): Boolean;
var
  DeltaLamda: double;
  Lamda0: double;
  _Enlem: Double;
begin
  result := true;
  Lamda0 := getLamda0(Boylam);

  DeltaLamda := DegToRad(Boylam - Lamda0); // * 3.6;

  _Enlem := DegToRad(Enlem);

  ED50_3.teta := _Enlem;
  WGS84_3.teta := _Enlem;
  GRS80_3.teta := _Enlem;
  Bessel1841.teta := _Enlem;
  Clarke1880.teta := _Enlem;

  Yukari := XGaussK(_Enlem, DeltaLamda, _datum);
  Saga := YGaussK(_Enlem, DeltaLamda, _datum) + 500000;

end;

function Utm3ToGeo(Saga, Yukari: double; _datum: TDatum3Enum; DOM: double; var
  Enlem, Boylam: double): Boolean;
var
  saveite, ite, Xg, Yg, _E, Eo: Double;

  function _nuKare: double;
  begin
    case _datum of
      deED50:
        Result := ED50_3.NuKare;
      deWGS84:
        Result := WGS84_3.NuKare;
      deGRS80:
        Result := GRS80_3.NuKare;
      deBessel1841:
        Result := Bessel1841.NuKare;
      deClarke1880:
        Result := Clarke1880.NuKare;
    end;
  end;

  function _N: double;
  begin
    case _datum of
      deED50:
        Result := ED50_3.N;
      deWGS84:
        Result := WGS84_3.N;
      deGRS80:
        Result := GRS80_3.N;
      deBessel1841:
        Result := Bessel1841.N;
      deClarke1880:
        Result := Clarke1880.N;
    end;
  end;

  function fonksiyon1: double;
  begin
    result := getAlfa(_datum) * Eo + getBeta(_datum) * Sin(2 * Eo) + getGama(_datum)
      * sin(4 * Eo) + getDelta(_datum) * Sin(6 * Eo) - Xg;
  end;

  function fonksiyon2: double;
  begin
    result := getAlfa(_datum) + 2 * getBeta(_datum) * cos(2 * Eo) + 4 * getGama(_datum)
      * cos(4 * Eo) + 6 * getDelta(_datum) * cos(6 * Eo);
  end;

var
  dE, _24_N_4, _tan_Eo, _2_N2, _Yg2, _tan2: double;
  _Yg4: double;
  _6_nuKare: double;
  deltaLamda: double;
  itecount: integer;
begin

  result := true;

  Xg := Yukari;
  Yg := Saga - 500000;

  Eo := (Xg / getAlfa(_datum));

  ite := -1;
  saveite := 1;
  itecount := 0;
  while (ite <> saveite) do
  begin
    inc(itecount);
    if itecount = 10 then
      BREAK;
    saveite := ite;
    ite := fonksiyon1 / fonksiyon2;
    _E := Eo - ite;
    Eo := _E;
  end;

  ED50_3.teta := Eo;
  WGS84_3.teta := Eo;
  GRS80_3.teta := Eo;
  Bessel1841.teta := Eo;
  Clarke1880.teta := Eo;

  _tan2 := sqr(tan(Eo));
  _Yg4 := power(Yg, 4);
  _6_nuKare := 6 * _nuKare;
  _24_N_4 := 24 * power(_N, 4);
  _tan_Eo := tan(Eo);
  _2_N2 := 2 * sqr(_N);
  _Yg2 := sqr(Yg);

  dE := (_tan_Eo * (1 + _nuKare) * _Yg2 / _2_N2) + (_tan_Eo * (5 + 3 * _tan2 +
    _6_nuKare - _6_nuKare * _tan2) * _Yg4 / _24_N_4);

  ED50_3.teta := Eo;
  WGS84_3.teta := Eo;
  GRS80_3.teta := Eo;
  Bessel1841.teta := Eo;
  Clarke1880.teta := Eo;

  _tan2 := sqr(tan(Eo));

  deltaLamda := (yg / (_N * Cos(Eo))) - ((1 + 2 * _tan2 + _nuKare) * power(Yg, 3)
    / (6 * power(_N, 3) * Cos(Eo)));

  Enlem := Eo - dE;
  Enlem := RadToDeg(Enlem);
  Boylam := DOM + RadToDeg(deltaLamda);

end;

{ TDatum3 }

constructor TDatum3.create(_a, _b, _alfa: double);
begin
  inherited create;
  a := _a;
  b := _b;
  alfa := _alfa;
end;

function TDatum3.ekare: double;
begin
  result := (sqr(a) - sqr(b)) / sqr(a);
end;

function TDatum3._ekare: double;
begin
  result := (sqr(a) - sqr(b)) / sqr(b);
end;

function TDatum3.M: double;
begin
  Result := (a * (1 - ekare)) / (power(1 - ekare * sqr(sin(teta)), 3 / 2));
end;

function TDatum3.N: Double;
begin
//  Result :=  a  / ( sqrt (1-ekare*sqr(sin(teta)) ));
  Result := sqr(a) / (b * (sqrt(1 + NuKare)));
end;

function TDatum3.NuKare: double;
begin
  Result := _ekare * sqr(cos(teta));
end;

function TDatum3.t: double;
begin
  Result := Tan(teta);
end;

procedure resetDatumParameter;
begin
  ED50_3.free;
  WGS84_3.Free;
  GRS80_3.Free;
  Bessel1841.Free;
  Clarke1880.Free;

  ED50_3 := TDatum3.Create(6378388.0000, 6356911.9461, 1 / 297.000000000);
  WGS84_3 := TDatum3.Create(6378137.0000, 6356752.3140, 1 / 298.257223563);
  GRS80_3 := TDatum3.Create(6378137.0000, 6356752.2980, 1 / 298.257222101);

  Bessel1841 := TDatum3.Create(6377397.1550, 6356078.9632, 1 / 299.152800000);
  Clarke1880 := TDatum3.Create(3678249.1450, 6356514.9900, 1 / 293.466000000);

end;

initialization
  ED50_3 := TDatum3.Create(6378388.0000, 6356911.9461, 1 / 297.000000000);
  WGS84_3 := TDatum3.Create(6378137.0000, 6356752.3140, 1 / 298.257223563);
  GRS80_3 := TDatum3.Create(6378137.0000, 6356752.2980, 1 / 298.257222101);
  Bessel1841 := TDatum3.Create(6377397.1550, 6356078.9632, 1 / 299.152800000);
  Clarke1880 := TDatum3.Create(3678249.1450, 6356514.9900, 1 / 293.466000000);

finalization
  ED50_3.Free;
  WGS84_3.Free;
  GRS80_3.Free;
  Bessel1841.Free;
  Clarke1880.Free;

end.


