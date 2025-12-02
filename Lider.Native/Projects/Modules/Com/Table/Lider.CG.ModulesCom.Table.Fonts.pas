unit Lider.CG.ModulesCom.Table.Fonts;

interface

uses
  Lider.CG.ModulesCom.Table.FontTypeInt,
  Lider.CG.ModulesCom.Table.BaseFont,
  Lider.CG.ModulesCom.Table.EnumsFont,
  SysUtils,
  Vcl.Graphics,
  System.UITypes,
  WinApi.Windows,
  Lider.CG.ModulesCom.Table.Roman,
  Lider.CG.ModulesCom.Table.Swiss,
  Lider.CG.ModulesCom.Table.Arial,
  Lider.CG.ModulesCom.Table.ArialBlack,
  Lider.CG.ModulesCom.Table.Calibri,
  Lider.CG.ModulesCom.Table.TimesNewRoman,
  Lider.CG.ModulesCom.Table.DoubleAccounting,
  Lider.CG.ModulesCom.Table.SingleAccounting,
  Lider.CG.ModulesCom.Table.Font.Doubles,
  Lider.CG.ModulesCom.Table.Singles,
  Lider.CG.ModulesCom.Table.Subscrip,
  Lider.CG.ModulesCom.Table.Superscrip,
  System.RegularExpressions;

type
  Fonts = class(BaseFont)

  private
    _FontName: FontTypeInt;
    _Family: FontTypeInt;
    _Underline: FontTypeInt;
    _VerticalAlign: FontTypeInt;
  public
    function CheckColor(ColorString: String): Boolean;

    function GetFamily: FontTypeInt; override;
    property Family: FontTypeInt read GetFamily;

    function GetFontName: FontTypeInt; override;
    property FontName: FontTypeInt read GetFontName;

    function GetVerticalAlign: FontTypeInt; override;
    property VerticalAlign: FontTypeInt read GetVerticalAlign;

    function GetUnderline: FontTypeInt; override;
    property Underline: FontTypeInt read GetUnderline;

    constructor Create(FontName: Font; CharSet: Integer; Family: FontFamily; Size: Single;
                       DrvColor: TColor; Bold: Boolean; Italic: Boolean; StrikeThrough: Boolean;
                       VerticalAlign: FontVerticalAlign; Underline: FontUnderline); overload;
    constructor Create(FontName: Font; CharSet: Integer; Family: FontFamily; Size: Single;
                       HexColor: String; Bold: Boolean; Italic: Boolean; StrikeThrough: Boolean;
                       VerticalAlign: FontVerticalAlign; Underline: FontUnderline); overload;
  end;

implementation
{ Fonts }

function Fonts.CheckColor(ColorString: String): Boolean;
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

constructor Fonts.Create(FontName: Font; CharSet: Integer; Family: FontFamily;
  Size: Single; DrvColor: TColor; Bold, Italic, StrikeThrough: Boolean;
  VerticalAlign: FontVerticalAlign; Underline: FontUnderline);
var
  HexColor: String;
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

  case FontName of
    Font.Arial: _FontName := Arial.create;
    Font.TimesNewRoman: _FontName := TimesNewRoman.create;
    Font.ArialBlack: _FontName := ArialBlack.create;
    Font.Calibri: _FontName := Calibri.create;
    else _FontName := Arial.create;
  end;

  case Family of
    FontFamily.Roman: _Family := Roman.create;
    FontFamily.Swiss: _Family := Swiss.create;
    else _Family := Swiss.create;
  end;

  case Underline of
    FontUnderline.Singles: _Underline := Singles.create;
    FontUnderline.Doubles: _Underline := Doubles.create;
    FontUnderline.SingleAccounting: _Underline := SingleAccounting.create;
    FontUnderline.DoubleAccounting: _Underline := DoubleAccounting.create;
    else _Underline := nil;
  end;

  case VerticalAlign of
    FontVerticalAlign.Superscrip: _VerticalAlign := Superscrip.create;
    FontVerticalAlign.Subscrip: _VerticalAlign := Subscrip.create;
    else _VerticalAlign := nil;
  end;

  inherited CharSet := CharSet;
  inherited Size := Size;
  inherited HexColor := HexColor;
  inherited Bold := Bold;
  inherited Italic := Italic;
  inherited StrikeThrough := StrikeThrough;
end;

constructor Fonts.Create(FontName: Font; CharSet: Integer; Family: FontFamily;
  Size: Single; HexColor: String; Bold, Italic, StrikeThrough: Boolean;
  VerticalAlign: FontVerticalAlign; Underline: FontUnderline);
begin
  inherited Create;
  if String.IsNullOrEmpty(HexColor) or (Not CheckColor(HexColor)) then //Assigned(HexColor) or
    HexColor := '#FFFFFF';

  case FontName of
    Font.Arial: _FontName := Arial.create;
    Font.TimesNewRoman: _FontName := TimesNewRoman.create;
    Font.ArialBlack: _FontName := ArialBlack.create;
    Font.Calibri: _FontName := Calibri.create;
    else _FontName := Arial.create;
  end;

  case Family of
    FontFamily.Roman: _Family := Roman.create;
    FontFamily.Swiss: _Family := Swiss.create;
    else _Family := Swiss.create;
  end;

  case Underline of
    FontUnderline.Singles: _Underline := Singles.create;
    FontUnderline.Doubles: _Underline := Doubles.create;
    FontUnderline.SingleAccounting: _Underline := SingleAccounting.create;
    FontUnderline.DoubleAccounting: _Underline := DoubleAccounting.create;
    else _Underline := nil;
  end;

  case VerticalAlign of
    FontVerticalAlign.Superscrip: _VerticalAlign := Superscrip.create;
    FontVerticalAlign.Subscrip: _VerticalAlign := Subscrip.create;
    else _VerticalAlign := nil;
  end;

  inherited CharSet := CharSet;
  inherited Size := Size;
  inherited HexColor := HexColor;
  inherited Bold := Bold;
  inherited Italic := Italic;
  inherited StrikeThrough := StrikeThrough;
end;

function Fonts.GetFamily: FontTypeInt;
begin
  result := Self._Family;
end;

function Fonts.GetFontName: FontTypeInt;
begin
  result := Self._FontName;
end;

function Fonts.GetUnderline: FontTypeInt;
begin
  result := Self._Underline;
end;

function Fonts.GetVerticalAlign: FontTypeInt;
begin
  result := Self._VerticalAlign;
end;

end.

