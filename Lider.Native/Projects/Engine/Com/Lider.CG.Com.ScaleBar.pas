unit Lider.CG.Com.ScaleBar;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Graphics,
  SysUtils,
  IniFiles,
  Math,
  Lider.CG.Com.Math,
  Lider.CG.Com.Stringli,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.System,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GeoTypes;

type
  TlicgBarAppearance = (apBlockAlternate, apBlock, apRuler);

  TlicgResizePosition = (rpNone, rpUpperLeft, rpUpperRight, rpLowerLeft, rpLowerRight);

  TlicgScaleBar = class(TCustomControl)
  private
    FResizePosition: TlicgResizePosition;
    FMoving: Boolean;
    FOnMove: TNotifyEvent;
    FFont: TFont;
    FColor: TColor;
    FLinesPen: TPen;
    FMinorBrush: TBrush;
    FMajorBrush: TBrush;
    FAppearance: TlicgBarAppearance;
    FIntervalLengthUnits: TlicgScaleUnits;
    FIntervalLength: Double;
    FIntervalNumber: Integer;
    FBarHeight: Integer;
    FNumDecimals: Integer;
    FUnits: TlicgScaleUnits;
    FShowTrailingZeros: Boolean;
    FTransparent: Boolean;

    { temp data }
    FPaintSuspended: Boolean;
    FNeedReposition: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetAppearance(const Value: TlicgBarAppearance);
    procedure SetIntervalLength(const Value: Double);
    procedure SetIntervalLengthUnits(const Value: TlicgScaleUnits);
    procedure SetBarHeight(const Value: Integer);
    procedure SetIntervalNumber(const Value: Integer);
    procedure SetNumDecimals(const Value: Integer);
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure SetLinesPen(const Value: TPen);
    procedure SetUnits(const Value: TlicgScaleUnits);
    procedure SetMajorBrush(const Value: TBrush);
    procedure SetMinorBrush(const Value: TBrush);
    procedure SetShowTrailingZeros(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    function IsDesigning: Boolean;
    procedure LayoutChanged(Sender: TObject);
    function GetAppearance: TlicgBarAppearance;
    function GetBarHeight: Integer;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetIntervalLength: Double;
    function GetIntervalLengthUnits: TlicgScaleUnits;
    function GetIntervalNumber: Integer;
    function GetLinesPen: TPen;
    function GetMajorBrush: TBrush;
    function GetMinorBrush: TBrush;
    function GetNumDecimals: Integer;
    function GetResizePosition: TlicgResizePosition;
    function GetShowTrailingZeros: Boolean;
    function GetTransparent: Boolean;
    function GetUnits: TlicgScaleUnits;
    procedure SetResizePosition(const Value: TlicgResizePosition);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Paint; override;
  public
{$IFDEF IS_OCX}
    FOCX_Owner: IInterface;
{$ENDIF}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reposition(TryPaint: boolean = false);
    procedure SaveToInifile(const FileName: string);
    procedure LoadFromInifile(const Filename: string);
    procedure NeededDimensions(GIS: TlicgBaseGis; Grapher: TlicgBaseGrapher;
      ACanvas: TCanvas; const UnitsFactor: Double; var NeededWidth, NeededHeight:
      Integer; var RealDist: Double; var ScaleDistPixels, ATextHeight: Integer);
    procedure PaintTo(GIS: TlicgBaseGis; ACanvas: TCanvas; const ARect: TRect;
      const UnitsFactor: Double; const RealDist: Double; ScaleDistPixels,
      TotalTextHeight: Integer);
  published
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property Appearance: TlicgBarAppearance read GetAppearance write
      SetAppearance default apBlock;
    property LinesPen: TPen read GetLinesPen write SetLinesPen;
    property MinorBrush: TBrush read GetMinorBrush write SetMinorBrush;
    property MajorBrush: TBrush read GetMajorBrush write SetMajorBrush;
    property IntervalLengthUnits: TlicgScaleUnits read GetIntervalLengthUnits
      write SetIntervalLengthUnits default suCms;
    property IntervalLength: Double read GetIntervalLength write SetIntervalLength;
    property BarHeight: Integer read GetBarHeight write SetBarHeight default 16;
    property IntervalNumber: Integer read GetIntervalNumber write
      SetIntervalNumber default 3;
    property NumDecimals: Integer read GetNumDecimals write SetNumDecimals default 2;
    property ResizePosition: TlicgResizePosition read GetResizePosition write
      SetResizePosition default rpLowerRight;
    property Units: TlicgScaleUnits read GetUnits write SetUnits;
    property ShowTrailingZeros: Boolean read GetShowTrailingZeros write
      SetShowTrailingZeros default false;
    property Transparent: Boolean read GetTransparent write SetTransparent default false;
    property Visible;
    property ShowHint;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
  published
    property onDblClick;
  end;

implementation

//******************* TlicgScaleBar **********************************

{ TlicgScaleBar }

constructor TlicgScaleBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption, csOpaque, csFramed];
  TabStop := False;
  FFont := Lider.CG.Com.System.DefaultFont;
  FFont.Size := 6;
  Width := 217;
  Height := 33;
  FNumDecimals := 0;
  FResizePosition := rpLowerRight;
  Cursor := crHandPoint;

  FTransparent := True;
  FColor := clWindow;
  FLinesPen := TPen.Create;
  FLinesPen.OnChange := LayoutChanged;
  FLinesPen.Color := clRed;
  FMinorBrush := TBrush.Create;
  FMinorBrush.OnChange := LayoutChanged;
  FMinorBrush.Color := TColor($F0CAA6); // clSkyBlue
  FMajorBrush := TBrush.Create;
  FMajorBrush.OnChange := LayoutChanged;
  FMajorBrush.Color := clNavy;
  FAppearance := apBlock;
  FIntervalLengthUnits := suCms;
  FIntervalLength := 1.0; // 1 cm every separation
  FIntervalNumber := 2;
  FBarHeight := 8; // pixels
  FUnits := suMms;

end;

destructor TlicgScaleBar.Destroy;
begin
  FFont.Free;
  FLinesPen.Free;
  FMinorBrush.Free;
  FMajorBrush.Free;
  inherited Destroy;
end;

procedure TlicgScaleBar.LayoutChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TlicgScaleBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
const
  SC_DRAGMOVE: Integer = $F012;
begin
  { Might also want to check for Client alignment too. }
  if (Align = alNone) then
  begin
    FMoving := True;
    ReleaseCapture;
    SendMessage(Handle, WM_SYSCOMMAND, SC_DRAGMOVE, LPARAM(0));
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TlicgScaleBar.WMMove(var Message: TWMMove);
begin
  inherited;
  if FMoving then
  begin
    FMoving := False;
    Parent.Realign;
    if Assigned(FOnMove) then
      FOnMove(Self);
    Invalidate
  end
  else if FTransparent then
    Invalidate;
end;

{ following two methods not yet working }

procedure TlicgScaleBar.SaveToInifile(const FileName: string);
var
  Inifile: TInifile;
  MyName: string;

  procedure SaveBrush(Brush: TBrush; const Ident: string);
  begin
    Inifile.WriteInteger(MyName, Ident + '_Color', Brush.Color);
    Inifile.WriteInteger(MyName, Ident + '_Style', ord(Brush.Style));
  end;

begin
  Inifile := TIniFile.Create(Filename);
  try
    MyName := Self.Name;
    if Length(MyName) = 0 then
      MyName := Classname;
    Inifile.WriteInteger(MyName, 'Color', Color);
    Inifile.WriteInteger(MyName, 'Appearance', Ord(Appearance));
    Inifile.WriteInteger(MyName, 'LinesPen_Color', LinesPen.Color);
    Inifile.WriteInteger(MyName, 'LinesPen_Style', Ord(LinesPen.Style));
    Inifile.WriteInteger(MyName, 'LinesPen_Width', LinesPen.Width);
    SaveBrush(MinorBrush, 'MinorBrush');
    SaveBrush(MajorBrush, 'MajorBrush');
    Inifile.WriteInteger(MyName, 'IntervalLengthUnits', ord(IntervalLengthUnits));
    WriteFloatToIniFile(Inifile, MyName, 'IntervalLength', IntervalLength);
    Inifile.WriteInteger(MyName, 'BarHeight', BarHeight);
    Inifile.WriteInteger(MyName, 'IntervalNumber', IntervalNumber);
    Inifile.WriteInteger(MyName, 'NumDecimals', NumDecimals);
    Inifile.WriteInteger(MyName, 'Units', ord(Units));
    Inifile.WriteBool(MyName, 'ShowTrailingZeros', ShowTrailingZeros);
    Inifile.WriteBool(MyName, 'Transparent', Transparent);
    Inifile.WriteBool(MyName, 'Visible', Visible);
    Inifile.WriteInteger(MyName, 'Left', Left);
    Inifile.WriteInteger(MyName, 'Top', Top);

    WriteFontToIniFile(Inifile, MyName, Self.Font); { save the font }
    Inifile.UpdateFile;
  finally
    Inifile.free;
  end;
end;

procedure TlicgScaleBar.LoadFromInifile(const Filename: string);
var
  Inifile: TInifile;
  MyName: string;

  procedure LoadBrush(Brush: TBrush; const Ident: string; Default: TColor);
  begin
    Brush.Color := Inifile.ReadInteger(MyName, Ident + '_Color', Default);
    Brush.Style := TBrushStyle(Inifile.ReadInteger(MyName, Ident + '_Style', ord
      (bsSolid)));
  end;

begin
  Inifile := TInifile.create(Filename);
  try
    MyName := Self.Name;
    if Length(MyName) = 0 then
      MyName := Classname;
    Color := Inifile.ReadInteger(MyName, 'Color', clWindow);
    Appearance := TlicgBarAppearance(Inifile.ReadInteger(MyName, 'Appearance',
      Ord(apBlock)));
    LinesPen.Color := Inifile.ReadInteger(MyName, 'LinesPen_Color', clRed);
    LinesPen.Style := TPenStyle(Inifile.ReadInteger(MyName, 'LinesPen_Style',
      Ord(psSolid)));
    LinesPen.Width := Inifile.ReadInteger(MyName, 'LinesPen_Width', 1);
    LoadBrush(MinorBrush, 'MinorBrush', TColor($F0CAA6));
    LoadBrush(MajorBrush, 'MajorBrush', clNavy);
    IntervalLengthUnits := TlicgScaleUnits(Inifile.ReadInteger(MyName,
      'IntervalLengthUnits', ord(suCms)));
    IntervalLength := ReadFloatFromIniFile(Inifile, MyName, 'IntervalLength', 1.0);
    BarHeight := Inifile.ReadInteger(MyName, 'BarHeight', 8);
    IntervalNumber := Inifile.ReadInteger(MyName, 'IntervalNumber', 2);
    NumDecimals := Inifile.ReadInteger(MyName, 'NumDecimals', 2);
    units := TlicgScaleUnits(Inifile.ReadInteger(MyName, 'Units', ord(suCms)));
    ShowTrailingZeros := Inifile.ReadBool(MyName, 'ShowTrailingZeros', false);
    Transparent := Inifile.ReadBool(MyName, 'Transparent', True);
    Visible := Inifile.ReadBool(MyName, 'Visible', False); //ilker 12.09.2016
    Left := Inifile.ReadInteger(MyName, 'Left', 0);
    Top := Inifile.ReadInteger(MyName, 'Top', 0);
    Self.Font.Handle := Lider.CG.Com.System.DefaultFontHandle;
    ReadFontFromIniFile(Inifile, MyName, Self.Font);
  finally
    Inifile.free;
  end;
end;

procedure TlicgScaleBar.Reposition(TryPaint: boolean = false);
begin
  if TryPaint then
    Paint;

  case ResizePosition of
    rpUpperLeft:
      begin
        Left := 2;
        Top := 2;
      end;
    rpUpperRight:
      begin
        Left := Parent.ClientWidth - Width - 2;
        Top := 2;
      end;
    rpLowerLeft:
      begin
        Left := 2;
        Top := Parent.ClientHeight - height - 2;
      end;
    rpLowerRight:
      begin
        Left := Parent.ClientWidth - Width - 2;
        Top := Parent.ClientHeight - height - 2;
      end;
  else
    begin
    end;
  end;
end;

procedure TlicgScaleBar.NeededDimensions(GIS: TlicgBaseGis; Grapher:
  TlicgBaseGrapher; ACanvas: TCanvas; const UnitsFactor: Double; var NeededWidth,
  NeededHeight: Integer; var RealDist: Double; var ScaleDistPixels, ATextHeight: Integer);
var
  I, TextWdth: Integer;
  ScaleDist, TotalDist: Double;
  unitsStr: string;
  temp: Double;
begin

  NeededWidth := 0;
  NeededHeight := 0;

  ACanvas.Font.Assign(Self.Font);
  TextWdth := ACanvas.TextWidth(Format('%.*f', [FNumDecimals, 0.0]));

  NeededWidth := (TextWdth div 2) + 1;
  { sumale el numero de intervalos }
  //If IsDesigning Then
  ScaleDist := GetDeviceCaps(ACanvas.Handle, LOGPIXELSX);
  //Else
  //  ScaleDist := TEzBaseDrawbox(Parent).Grapher.ScreenDpiX;

  {
  if FIntervalLengthUnits = suMms then
    ScaleDist := ScaleDist / 25.4
  else if FIntervalLengthUnits = suCms then
    ScaleDist := ScaleDist / 2.54
  else
  if FIntervalLengthUnits = suMms then
    ScaleDist := ScaleDist / 0.0254
  else
  if FIntervalLengthUnits = suKms then
    ScaleDist := ScaleDist / 0.0000254;
  }

  ScaleDistPixels := Round(ScaleDist * FIntervalLength);

  Inc(NeededWidth, ScaleDistPixels * FIntervalNumber);
  { calculate the text length of last interval }

  if (Grapher = nil) or IsDesigning then
    RealDist := 10
  else if Grapher <> nil then
    RealDist := Grapher.DistToRealY(ScaleDistPixels);
  TotalDist := RealDist * FIntervalNumber;
  temp := TotalDist * UnitsFactor;
  if not FShowTrailingZeros and (temp = Int(temp)) then
    TextWdth := ACanvas.TextWidth(FloatToStr(temp))
  else
    TextWdth := ACanvas.TextWidth(Format('%.*f', [FNumDecimals, temp]));
  Inc(NeededWidth, TextWdth div 2);
   (*
  unitsStr := pj_Units[FUnits].id;

  ilker bunu bakarsýn ileride
  if not IsDesigning and (GIS <> nil) and (GIS.MapInfo.CoorsUnits = _Gdal__UnitARC_DEGREES) then
  unitsStr := pj_Units[suDeg].id;

  Inc(NeededWidth, ACanvas.TextWidth(#32 + UnitsStr) + 1);

  { now calculate the needed height }
  ATextHeight := Max(ACanvas.TextHeight('0'), ACanvas.TextHeight(UnitsStr));  *)

  TotalDist := 0;
  for I := 1 to FIntervalNumber do
  begin
    TotalDist := TotalDist + RealDist;
    //Dist:= TotalDist * (1 / pj_Units[Self.FUnits].to_meter)
    ATextHeight := Max(ACanvas.TextHeight(FloatToStr(TotalDist
      * UnitsFactor)), ATextHeight);
  end;

  NeededHeight := ATextHeight + 1 + FLinesPen.Width + FBarHeight + FLinesPen.Width + 2;

end;

procedure TlicgScaleBar.PaintTo(GIS: TlicgBaseGis; ACanvas: TCanvas; const ARect:
  TRect; const UnitsFactor: Double; const RealDist: Double; ScaleDistPixels,
  TotalTextHeight: Integer);
var
  TextStr, UnitsID: string;
  TmpR: TRect;
  TmpDist, TotalDist: Double;
  I: Integer;
  Alternate: Boolean;
  HalfR: TRect;
  AWidth, HalfHeight: Integer;
begin
  { now to paint the scale }
  // TEzBaseDrawBox(Parent)
  with ACanvas do
  begin

    Brush.Style := bsSolid;

    Brush.Color := Self.FColor;

    if not IsDesigning and (Parent is TlicgBAseDrawBox) and Self.FTransparent then
    begin
      { Is this really a transparency :-) I am copying the buffer bitmap of the
        cursor only }
      TmpR := ARect;
      TmpR.TopLeft := Parent.ScreenToClient(Self.ClientToScreen(TmpR.TopLeft));
      TmpR.BottomRight := Parent.ScreenToClient(Self.ClientToScreen(TmpR.BottomRight));
      Self.Canvas.CopyRect(Self.ClientRect, TlicgBaseDrawBox(Parent).ScreenBitmap.Canvas,
        TmpR);
    end
    else
      FillRect(ARect);

    Pen.Assign(Self.FLinesPen);

    Alternate := True;

    TotalDist := 0;

    if not FShowTrailingZeros and (TotalDist = Int(TotalDist)) then
      AWidth := TextWidth(FloatToStr(TotalDist))
    else
      AWidth := TextWidth(Format('%.*f', [FNumDecimals, TotalDist]));
    TmpR.Left := 1 + (AWidth div 2);
    TmpR.Top := TotalTextHeight + 1;
    TmpR.Right := TmpR.Left + ScaleDistPixels;
    TmpR.Bottom := TmpR.Top + FLinesPen.Width * 2 + FBarHeight;

    for I := 1 to FIntervalNumber do
    begin
      if FAppearance = apBlockAlternate then
      begin
        HalfR := TmpR;
        HalfHeight := (TmpR.Bottom - TmpR.Top) div 2;
        Halfr.Bottom := HalfR.Top + HalfHeight;
        if Alternate then
          Brush.Assign(FMajorBrush)
        else
          Brush.Assign(FMinorBrush);
        with HalfR do
          Rectangle(Left, Top, Right, Bottom);

        HalfR.Top := HalfR.Bottom - 1;
        Halfr.Bottom := HalfR.Top + HalfHeight;
        if Alternate then
          Brush.Assign(FMinorBrush)
        else
          Brush.Assign(FMajorBrush);
        with halfR do
          Rectangle(Left, Top, Right, Bottom);
      end
      else if FAppearance = apBlock then
      begin
        if Alternate then
          Brush.Assign(FMajorBrush)
        else
          Brush.Assign(FMinorBrush);
        with TmpR do
          Rectangle(Left, Top, Right, Bottom)
      end
      else
        with TmpR do
        begin
          MoveTo(Left, Top);
          LineTo(Left, Bottom);
          LineTo(Right, Bottom);
          LineTo(Right, Top);
        end;

      TmpDist := TotalDist * UnitsFactor;
      //if not FShowTrailingZeros and (TmpDist = Int(TmpDist)) then
      TextStr := FloatToStr(TmpDist);
      //else
        //TextStr := Format('%.*f', [FNumDecimals, TmpDist]);
      AWidth := TextWidth(TextStr);

      Windows.SetBkMode(Handle, Windows.TRANSPARENT);

      if not IsDesigning and (GIS <> nil) and (GIS.MapInfo.CS.IsGeographicCS) then
      begin
        TextStr := FloatToStr(degtoMeter(AsFloat(TextStr)));
      end;

      if AsFloat(TextStr) = 0 then
        TextOut(TmpR.Left - (AWidth div 2), 0, TextStr)
      else if AsFloat(TextStr) * FIntervalNumber > 999.999 then
      begin
        TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) / 1000, -FNumDecimals));
        AWidth := TextWidth(TextStr);
        TextOut(TmpR.Left - (AWidth div 2), 1, TextStr + #32)
      end
      else if (AsFloat(TextStr) * FIntervalNumber < 1000) and (AsFloat(TextStr)
        * FIntervalNumber > 1) then
      begin
        TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr), -FNumDecimals));
        AWidth := TextWidth(TextStr);
        TextOut(TmpR.Left - (AWidth div 2), 1, TextStr + #32);
      end
      else if AsFloat(TextStr) * FIntervalNumber > 0.01 then
      begin
        TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) * 100, -FNumDecimals));
        AWidth := TextWidth(TextStr);
        TextOut(TmpR.Left - (AWidth div 2), 1, TextStr + #32);
      end
      else
      begin
        TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) * 1000, -FNumDecimals));
        AWidth := TextWidth(TextStr);
        TextOut(TmpR.Left - (AWidth div 2), 1, TextStr + #32);
      end;
          //TextOut(TmpR.Left - (AWidth div 2), 0, TextStr);

      OffsetRect(TmpR, ScaleDistPixels - 1, 0);

      TotalDist := TotalDist + RealDist;

      Alternate := not Alternate;
    end;
    { TotalDist is in real map Units. Now we need to change to Units of Scale Bar }
    TmpDist := TotalDist * UnitsFactor;
    //if not FShowTrailingZeros and (TmpDist = Int(TmpDist)) then
    TextStr := FloatToStr(TmpDist);
    //else
      //TextStr := Format('%.*f', [FNumDecimals, TmpDist]);
    AWidth := TextWidth(TextStr);
    Windows.SetBkMode(Handle, Windows.TRANSPARENT);
    (* ilker silme ileride tekrar bak
    unitsID := string(pj_Units[Self.FUnits].ID);
    if not IsDesigning and (GIS <> nil) and (GIS.MapInfo.CS.IsUnknownCS) then
    begin
      unitsID := string(pj_Units[suDeg].ID);
      TextStr := FloatToStr(degtoMeter(AsFloat(TextStr)));
    end;


    //TextOut(TmpR.Left - (AWidth div 2), 1, TextStr + #32 + UnitsID)
    if AsFloat(TextStr) > 999.999 then
    begin
      TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) / 1000, -FNumDecimals));
      AWidth := TextWidth(TextStr);
      TextOut(TmpR.Left - (AWidth div 2) - 5, 1, TextStr + #32 + 'km')
    end
    else if (AsFloat(TextStr) < 1000) and (AsFloat(TextStr) > 1) then
    begin
      TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr), -FNumDecimals));
      AWidth := TextWidth(TextStr);
      TextOut(TmpR.Left - (AWidth div 2) - 5, 1, TextStr + #32 + 'm');
    end
    else if AsFloat(TextStr) > 0.01 then
    begin
      TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) * 100, -FNumDecimals));
      AWidth := TextWidth(TextStr);
      TextOut(TmpR.Left - (AWidth div 2) - 5, 1, TextStr + #32 + 'cm');
    end
    else
    begin
      TextStr := FloatToStr(RoundToDouble(AsFloat(TextStr) * 1000, -FNumDecimals));
      AWidth := TextWidth(TextStr);
      TextOut(TmpR.Left - (AWidth div 2) - 5, 1, TextStr + #32 + 'mm');
    end;
     *)
  end;
end;

procedure TlicgScaleBar.Paint;
var
  RealDist, UnitsFactor: Double;
  AWidth, AHeight: Integer;
  ScaleDistPixels, TotalTextHeight: Integer;
begin
  inherited Paint;

  if FPaintSuspended or not Assigned(Parent) or not (Parent is TlicgBaseDrawBox)
    or not Assigned(TlicgBaseDrawBox(Parent).GIS) then
  begin
    NeededDimensions(nil, nil, Self.Canvas, 1.0, AWidth, AHeight, RealDist,
      ScaleDistPixels, TotalTextHeight);
    PaintTo(nil, Self.Canvas, Self.ClientRect, 1.0, RealDist, ScaleDistPixels,
      TotalTextHeight);
    Self.ClientWidth := AWidth;
    Self.ClientHeight := AHeight;
    Exit;
  end;

  unitsFactor := 1.0;
  (* ilker ileride tekrar düþün
  if not IsDesigning and TlicgBaseDrawBox(Parent).GIS.Active and (TlicgBaseDrawBox
    (Parent).GIS.MapInfo.CoorsUnits <> _Gdal__UnitARC_DEGREES) then
    unitsFactor := pj_Units[CoorUnitToScaleUnit(TlicgBaseDrawbox(Parent).GIS.MapInfo.CoorsUnits)].to_meter
      * (1 / pj_Units[Self.FUnits].to_meter); *)

  NeededDimensions(TlicgBaseDrawBox(Parent).GIS, TlicgBaseDrawBox(Parent).Grapher,
    Self.Canvas, UnitsFactor, AWidth, AHeight, RealDist, ScaleDistPixels,
    TotalTextHeight);

  if (AWidth <= 0) or (AHeight <= 0) then
    Exit;

  if (Self.ClientWidth <> AWidth) or (Self.ClientHeight <> AHeight) then
  begin
    FPaintSuspended := True;
    try
      Self.ClientWidth := Min(Parent.Width, AWidth);
      Self.ClientHeight := Min(Parent.Height, AHeight);
    finally
      FPaintSuspended := False;
    end;
  end;

  PaintTo(TlicgBaseDrawBox(Parent).GIS, Self.Canvas, Self.ClientRect,
    UnitsFactor, RealDist, ScaleDistPixels, TotalTextHeight);

end;

procedure TlicgScaleBar.SetAppearance(const Value: TlicgBarAppearance);
begin
  if FAppearance = Value then
    Exit;
  FAppearance := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetBarHeight(const Value: Integer);
begin
  if FBarHeight = Value then
    Exit;
  FBarHeight := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetColor(const Value: TColor);
begin
  if FColor = Value then
    Exit;
  FColor := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Invalidate;
end;

procedure TlicgScaleBar.SetIntervalLength(const Value: Double);
begin
  if FIntervalLength = Value then
    Exit;
  FIntervalLength := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetIntervalLengthUnits(const Value: TlicgScaleUnits);
begin
  if FIntervalLengthUnits = Value then
    Exit;
  FIntervalLengthUnits := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetIntervalNumber(const Value: Integer);
begin
  if FIntervalNumber = Value then
    Exit;
  FIntervalNumber := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetNumDecimals(const Value: Integer);
begin
  FNumDecimals := 0;
  exit;
  if FNumDecimals = Value then
    Exit;
  FNumDecimals := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetLinesPen(const Value: TPen);
begin
  FLinesPen.Assign(Value);
  Invalidate;
end;

procedure TlicgScaleBar.SetUnits(const Value: TlicgScaleUnits);
begin
  if FUnits = Value then
    Exit;
  FUnits := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetMajorBrush(const Value: TBrush);
begin
  FMajorBrush.Assign(Value);
  Invalidate;
end;

procedure TlicgScaleBar.SetMinorBrush(const Value: TBrush);
begin
  FMinorBrush.Assign(Value);
  Invalidate;
end;

procedure TlicgScaleBar.SetShowTrailingZeros(const Value: Boolean);
begin
  if FShowTrailingZeros = Value then
    Exit;
  FShowTrailingZeros := Value;
  Invalidate;
end;

procedure TlicgScaleBar.SetTransparent(const Value: Boolean);
begin
  if FTransparent = Value then
    Exit;
  FTransparent := Value;
  RecreateWnd;
end;

function TlicgScaleBar.IsDesigning: Boolean;
begin
  Result := False;
  Exit;
  //Result:= csDesigning in ComponentState;
end;

function TlicgScaleBar.GetAppearance: TlicgBarAppearance;
begin
  Result := FAppearance
end;

function TlicgScaleBar.GetBarHeight: Integer;
begin
  Result := FBarHeight
end;

function TlicgScaleBar.GetColor: TColor;
begin
  Result := FColor
end;

function TlicgScaleBar.GetFont: TFont;
begin
  Result := FFont
end;

function TlicgScaleBar.GetIntervalLength: Double;
begin
  Result := FIntervalLength
end;

function TlicgScaleBar.GetIntervalLengthUnits: TlicgScaleUnits;
begin
  Result := FIntervalLengthUnits
end;

function TlicgScaleBar.GetIntervalNumber: Integer;
begin
  Result := FIntervalNumber
end;

function TlicgScaleBar.GetLinesPen: TPen;
begin
  Result := FLinesPen
end;

function TlicgScaleBar.GetMajorBrush: TBrush;
begin
  Result := FMajorBrush
end;

function TlicgScaleBar.GetMinorBrush: TBrush;
begin
  Result := FMinorBrush
end;

function TlicgScaleBar.GetNumDecimals: Integer;
begin
  Result := FNumDecimals
end;

function TlicgScaleBar.GetResizePosition: TlicgResizePosition;
begin
  Result := FResizePosition
end;

function TlicgScaleBar.GetShowTrailingZeros: Boolean;
begin
  Result := FShowTrailingZeros
end;

function TlicgScaleBar.GetTransparent: Boolean;
begin
  Result := FTransparent
end;

function TlicgScaleBar.GetUnits: TlicgScaleUnits;
begin
  Result := FUnits
end;

procedure TlicgScaleBar.SetResizePosition(const Value: TlicgResizePosition);
begin
  FResizePosition := Value
end;

end.


