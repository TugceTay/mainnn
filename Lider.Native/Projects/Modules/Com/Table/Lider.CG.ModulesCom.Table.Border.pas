unit Lider.CG.ModulesCom.Table.Border;

interface

uses
  Lider.CG.ModulesCom.Table.BaseBorder,
  Lider.CG.ModulesCom.Table.PositionInt,
  Lider.CG.ModulesCom.Table.LineStyleInt,
  Lider.CG.ModulesCom.Table.EnumsBorder,
  Vcl.Graphics,
  System.UITypes,
  SysUtils,
  Lider.CG.ModulesCom.Table.Border.Bottom,
  Lider.CG.ModulesCom.Table.Border.Left,
  Lider.CG.ModulesCom.Table.Border.Right,
  Lider.CG.ModulesCom.Table.Border.Top,
  Lider.CG.ModulesCom.Table.DiagonalLeft,
  Lider.CG.ModulesCom.Table.DiagonalRight,
  System.RegularExpressions,
  Lider.CG.ModulesCom.Table.Continuous,
  Lider.CG.ModulesCom.Table.Dash,
  Lider.CG.ModulesCom.Table.DashDotDot,
  Lider.CG.ModulesCom.Table.Dot,
  Lider.CG.ModulesCom.Table.Doubles,
  Lider.CG.ModulesCom.Table.SlantDashDot,
  System.StrUtils,
  WinApi.Windows;

type
  Border = class(BaseBorder)

  private
    _Position: PositionInt;
    _LineStyle: LineStyleInt;

  public
    function CheckColor(ColorString: String): Boolean;

    function GetLineStyle: LineStyleInt; override;
    property LineStyle: LineStyleInt read GetLineStyle;

    function GetPosition: PositionInt; override;
    property Position: PositionInt read GetPosition;

    constructor Create; overload;
    constructor Create(Position: Positions; LineStyle: LineStyles;
                       Weight: Integer; DrvColor: TColor); overload;//Drawing.Color
    constructor Create(Position: String; LineStyle: String;
                       Weight: Integer; HexColor: String); overload;
  end;

implementation
{ Border }

function Border.CheckColor(ColorString: String): Boolean;
var
  htmlColorRegex: TRegex;
  match: TMatch;
  msg: String;
begin
    htmlColorRegex := TRegex.Create('^#?((?''R''[0-9a-f]{2})(?''G''[0-9a-f]{2})(?''B''[0-9a-f]{2}))$', [roCompiled,roIgnoreCase]);//RegexOptions.Compiled Or RegexOptions.IgnoreCase);
    try
        if String.IsNullOrEmpty(ColorString) then//Is Nothing Then
            raise EArgumentException.Create('colorString');//ArgumentNullException('colorString');

        match := htmlColorRegex.Match(ColorString);
        if Not match.Success then
        begin
            msg := 'The string \ %s \  doesn''t represent';
            msg := msg + 'a valid HTML hexadecimal color';
            msg := Format(msg, [ColorString]);

            raise EArgumentException.Create('colorString');
        end
        Else
          result := True;
    except on e: Exception do
//        'MsgBox(ex.Message)
        result := False;
    end;
end;

constructor Border.Create(Position: Positions; LineStyle: LineStyles;
  Weight: Integer; DrvColor: TColor); // Drawing.Color
var
  HexColor: String;
begin
  inherited Create;
//  if (DrvColor = nil) then
//    HexColor := '#FFFFFF'
//  else
  try
    HexColor := { red value }
                IntToHex( GetRValue( DrvColor ), 2 ) + //GetRValue
                { green value }
                IntToHex( GetGValue( DrvColor ), 2 ) + //GetGValue
                { blue value }
                IntToHex( GetBValue( DrvColor ), 2 ); //GetBValue
                //ToHtmlHexadecimal(DrvColor);
  except
    HexColor := '#FFFFFF'
  end;

  case Position of
    Positions.Bottom: _Position := Bottom.create;
    Positions.Top: _Position := Top.create;
    Positions.Left: _Position := Left.create;
    Positions.Right: _Position := Right.create;
    Positions.DiagonalLeft: _Position := DiagonalLeft.create;
    Positions.DiagonalRight: _Position := DiagonalRight.create;
  end;

  case LineStyle of
    LineStyles.Continuous:
    begin
      _LineStyle := Continuous.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    LineStyles.Dash:
    begin
      _LineStyle := Dash.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    LineStyles.DashDotDot:
    begin
      _LineStyle := DashDotDot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    LineStyles.Dot:
    begin
      _LineStyle := Dot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    LineStyles.Doubles:
    begin
      _LineStyle := Doubles.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    LineStyles.SlantDashDot:
    begin
      _LineStyle := SlantDashDot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
  end;
end;

constructor Border.Create(Position, LineStyle: String; Weight: Integer;
  HexColor: String);
begin
  inherited Create;

  if String.IsNullOrEmpty(HexColor) or (Not CheckColor(HexColor)) then //Assigned(HexColor) or
    HexColor := '#FFFFFF';

  case ansiindexstr(Position, ['Bottom', 'Top', 'Left', 'Right', 'DiagonalLeft', 'DiagonalRight']) of
    0: _Position := Bottom.create;
    1: _Position := Top.create;
    2: _Position := Left.create;
    3: _Position := Right.create;
    4: _Position := DiagonalLeft.create;
    5: _Position := DiagonalRight.create;
  end;

  case ansiindexstr(LineStyle, ['Continuous', 'Dash', 'DashDotDot', 'Dot', 'Doubles', 'SlantDashDot']) of
    0:
    begin
      _LineStyle := Continuous.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    1:
    begin
      _LineStyle := Dash.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    2:
    begin
      _LineStyle := DashDotDot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    3:
    begin
      _LineStyle := Dot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    4:
    begin
      _LineStyle := Doubles.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
    5:
    begin
      _LineStyle := SlantDashDot.create;
      _LineStyle.HexColor := HexColor;
      _LineStyle.Weight := Weight;
    end;
  end;
end;

constructor Border.Create;
begin
  inherited Create;
end;

function Border.GetLineStyle: LineStyleInt;
begin
  result := Self._LineStyle;
end;

function Border.GetPosition: PositionInt;
begin
  result := Self._Position;
end;

end.

