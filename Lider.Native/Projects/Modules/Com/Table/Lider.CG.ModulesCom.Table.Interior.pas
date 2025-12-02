unit Lider.CG.ModulesCom.Table.Interior;

interface

uses
  Lider.CG.ModulesCom.Table.BaseInterior,
  Lider.CG.ModulesCom.Table.PatternInt,
  Vcl.Graphics,
  System.UITypes,
  SysUtils,
  WinApi.Windows,
  System.StrUtils,
  Lider.CG.ModulesCom.Table.Solid,
  System.RegularExpressions;

type
  Interior = class(BaseInterior)
    private
      _Pattern: PatternInt;
    public
      function GetPattern: PatternInt; override;
      property Pattern: PatternInt read GetPattern;

      function CheckColor(ColorString: String): Boolean;

      constructor Create(DrvColor: TColor; Pattern: String; DrvPatternColor: TColor); overload;
      constructor Create(HexColor: String; Pattern: String; HexPatternColor: String); overload;
  end;

implementation
{ Interior }

function Interior.CheckColor(ColorString: String): Boolean;
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

constructor Interior.Create(DrvColor: TColor; Pattern: String;
  DrvPatternColor: TColor);
var
  HexColor, HexPatternColor: String;
begin
  inherited Create;
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
  try
    HexPatternColor := { red value }
                IntToHex( GetRValue( DrvPatternColor ), 2 ) + //GetRValue
                { green value }
                IntToHex( GetGValue( DrvPatternColor ), 2 ) + //GetGValue
                { blue value }
                IntToHex( GetBValue( DrvPatternColor ), 2 ); //GetBValue
                //ToHtmlHexadecimal(DrvColor);
  except
    HexPatternColor := '#FFFFFF'
  end;

  case ansiindexstr(Pattern, ['Solid']) of
    0:
    begin
      _Pattern := Solid.create;
      _Pattern.HexPatternColor := HexPatternColor;
    end;
  end;

  inherited HexColor := HexColor;
end;

constructor Interior.Create(HexColor, Pattern, HexPatternColor: String);
begin
  inherited Create;

  if String.IsNullOrEmpty(HexColor) or (Not CheckColor(HexColor)) then //Assigned(HexColor) or
    HexColor := '#FFFFFF';

  if String.IsNullOrEmpty(HexPatternColor) or (Not CheckColor(HexPatternColor)) then //Assigned(HexColor) or
    HexPatternColor := '#FFFFFF';

  case ansiindexstr(Pattern, ['Solid']) of
    0:
    begin
      _Pattern := Solid.create;
      _Pattern.HexPatternColor := HexPatternColor;
    end;
  end;

  inherited HexColor := HexColor;
end;

function Interior.GetPattern: PatternInt;
begin
  result := Self._Pattern;
end;

end.

