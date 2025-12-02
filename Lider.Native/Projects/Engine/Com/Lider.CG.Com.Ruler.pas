unit Lider.CG.Com.Ruler;

interface

uses
  Windows,
  Classes,
  Graphics,
  ExtCtrls,
  Controls,
  Sysutils,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Types,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  { TlicgHorizontalRuler }
  TlicgHorizontalRuler = class(TPaintBox)
  private
    FPreviewBox: TlicgBaseDrawBox; //TlicgPreviewBox;
    FLastMousePosInRuler: TPoint;
    FRubberPenColor: TColor;
    FMarksColor: TColor;
    FBoxOnPaint: TNotifyEvent;
    FBoxOnMouseMove: TlicgMouseMoveEvent;
    FCtl3D: Boolean;
    FPaperUnits: TlicgScaleUnits;
    procedure SetPreviewBox(Value: TlicgBaseDrawBox);
    procedure DrawRulerPosition(p: TPoint; AMode: TPenMode);
    procedure SetRubberPenColor(const Value: TColor);
    procedure SetMarksColor(const Value: TColor);
    procedure MyOnPaint(Sender: TObject);
    procedure MyOnMouseMove2D(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    function GetMarksColor: TColor;
    function GetPreviewBox: TlicgBaseDrawBox;
    function GetRubberPenColor: TColor;
    function GetCtl3D: Boolean;
    procedure SetCtl3D(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PaperUnits: TlicgScaleUnits read FPaperUnits write FPaperUnits;
    property PreviewBox: TlicgBaseDrawBox read GetPreviewBox write SetPreviewBox;
  published
    property RubberPenColor: TColor read GetRubberPenColor write SetRubberPenColor;
    property MarksColor: TColor read GetMarksColor write SetMarksColor;
    property Ctl3D: Boolean read GetCtl3D write SetCtl3D default True;
  end;

  { TlicgVerticalRuler }
  TlicgVerticalRuler = class(TPaintBox)
  private
    FPreviewBox: TlicgBaseDrawBox;
    FLastMousePosInRuler: TPoint;
    FRubberPenColor: TColor;
    FMarksColor: TColor;
    FBoxOnPaint: TNotifyEvent;
    FBoxOnMouseMove: TlicgMouseMoveEvent;
    FCtl3D: Boolean;
    FPaperUnits: TlicgScaleUnits;
    procedure SetPreviewBox(Value: TlicgBaseDrawBox);
    procedure DrawRulerPosition(p: TPoint; AMode: TPenMode);
    procedure SetRubberPenColor(const Value: TColor);
    procedure SetMarksColor(const Value: TColor);
    procedure MyOnPaint(Sender: TObject);
    procedure MyOnMouseMove2D(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    function GetMarksColor: TColor;
    function GetPreviewBox: TlicgBaseDrawBox;
    function GetRubberPenColor: TColor;
    function GetCtl3D: Boolean;
    procedure SetCtl3D(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    property PreviewBox: TlicgBaseDrawBox read GetPreviewBox write SetPreviewBox;
    property PaperUnits: TlicgScaleUnits read FPaperUnits write FPaperUnits;
  published
    property RubberPenColor: TColor read GetRubberPenColor write SetRubberPenColor default clAqua;
    property MarksColor: TColor read GetMarksColor write SetMarksColor;
    property Ctl3D: Boolean read GetCtl3D write SetCtl3D default True;
  end;

implementation

{ TlicgHorizontalRuler }
function Units2Inches(const Value: Double; _PaperUnits: TlicgScaleUnits): Double;
begin
  Result := Value;
  if _PaperUnits = suMms then
    result := result / 25.4
  else if _PaperUnits = suCms then
    result := result / 2.54;
end;

function Inches2Units(const Value: Double; _PaperUnits: TlicgScaleUnits): Double;
begin
  Result := Value;
  if _PaperUnits = suMms then
    result := result * 25.4
  else if _PaperUnits = suCms then
    result := result * 2.54;
end;

procedure MaxMinSeparation(PreviewBox: TlicgBaseDrawBox; _PaperUnits:
  TlicgScaleUnits; var maxs, mins: Double);
var
  azoom: integer;
  OriginalDX, CurrentDX: Double;
begin
  with PreviewBox.Grapher do
  begin
    with OriginalParams.VisualWindow do
      OriginalDX := UpperRight.x - LowerLeft.x;
    with CurrentParams.VisualWindow do
      CurrentDX := UpperRight.x - LowerLeft.x;
    azoom := Round((OriginalDX / CurrentDX) * 100)
  end;
  if _PaperUnits = suMms then
  begin
    case azoom of
      0..24:
        begin
          mins := 10;
          maxs := 100;
        end;
      25..49:
        begin
          mins := 10;
          maxs := 50;
        end;
      50..100:
        begin
          mins := 2;
          maxs := 20;
        end;
      101..MaxInt:
        begin
          mins := 1;
          maxs := 10;
        end;
    end;
  end
  else if _PaperUnits = suCms then
  begin
    case azoom of
      0..24:
        begin
          mins := 1;
          maxs := 10;
        end;
      25..49:
        begin
          mins := 1;
          maxs := 5;
        end;
      50..100:
        begin
          mins := 0.2;
          maxs := 2;
        end;
      101..MaxInt:
        begin
          mins := 0.1;
          maxs := 1;
        end;
    end;
  end
  else if _PaperUnits = suInches then
  begin
    case azoom of
      0..24:
        begin
          mins := 1 / 2;
          maxs := 4;
        end;
      25..32:
        begin
          mins := 1 / 2;
          maxs := 2;
        end;
      33..100:
        begin
          mins := 1 / 8;
          maxs := 1;
        end;
      101..MaxInt:
        begin
          mins := 1 / 16;
          maxs := 1;
        end;
    end;
  end;
  maxs := Units2Inches(maxs, _PaperUnits);
  mins := Units2Inches(mins, _PaperUnits);
end;

constructor TlicgHorizontalRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alTop;
  FRubberPenColor := clAqua;
  FMarksColor := clOlive;
  FCtl3D := True;
  FLastMousePosInRuler.X := -1000;
end;

procedure TlicgHorizontalRuler.SetPreviewBox(Value: TlicgBaseDrawBox);
begin
{$IFDEF LEVEL5}
  //if Assigned(FPreviewBox) then
    //FPreviewBox.RemoveFreeNotification(Self);
{$ENDIF}
  if (Value <> nil) and (Value <> FPreviewBox) then
  begin
    //Value.FreeNotification(Self);
    if FPreviewBox <> nil then
      with FPreviewBox do
      begin
        OnMouseMove2D := FBoxOnMouseMove;
        OnPaint := FBoxOnPaint;
      end;
    if Value <> nil then
      with value do
      begin
        FBoxOnMouseMove := OnMouseMove2D;
        FBoxOnPaint := OnPaint;

        OnMouseMove2D := MyOnMouseMove2D;
        OnPaint := MyOnPaint;
      end;
  end;
  FPreviewBox := value;
  Invalidate;
end;

function _RealToPoint(Grapher: TlicgBaseGrapher; const P: TlicgCoor): TPoint;
begin
  with Grapher, Grapher.CurrentParams, Grapher.CurrentParams.VisualWindow do
  begin
    Result.X := Round(ViewPortExtent.LowerLeft.x + (P.X - LowerLeft.X) * Scale);
    Result.Y := Round(ViewPortExtent.UpperRight.y + (UpperRight.Y - P.Y) * Scale);
  end;
end;

procedure TlicgHorizontalRuler.MyOnMouseMove2D(Sender: TObject; Shift:
  TShiftState; X, Y: Integer; const WX, WY: Double);
var
  TmpX: Double;
  TmpPt: TPoint;
begin
  if Assigned(FBoxOnMouseMove) then
    FBoxOnMouseMove(Sender, Shift, X, Y, WX, WY);
  if FPreviewBox = nil then
    Exit;
  DrawRulerPosition(FLastMousePosInRuler, pmXor);
  TmpX := WX;
  //with FPreviewBox.GridInfo do
    //if SnapToGrid then
      //TmpX := Round(WX / GridSnap.X) * GridSnap.X + GridOffset.X;

  if FPreviewBox.GridInfo.SnapToGrid then
    TmpX := Round(WX / FPreviewBox.GridInfo.GridSnapAperture) * FPreviewBox.GridInfo.GridSnapAperture
      + FPreviewBox.GridInfo.GridOffset.X;

  TmpPt := _RealToPoint(FPreviewBox.Grapher, AsCoor(TmpX, WY));
  FLastMousePosInRuler := ScreenToClient(FPreviewBox.ClientToScreen(TmpPt));
  DrawRulerPosition(FLastMousePosInRuler, pmXor);
end;

procedure TlicgHorizontalRuler.MyOnPaint(Sender: TObject);
begin
  if Assigned(FBoxOnPaint) then
    FBoxOnPaint(Sender);
  Self.Paint;
end;

procedure TlicgHorizontalRuler.DrawRulerPosition(p: TPoint; AMode: TPenMode);
begin
  if (FPreviewBox = nil) or (p.X < 0) then
    Exit;
  with Canvas do
  begin
    Pen.Mode := AMode;
    Pen.Color := ColorToRGB(FRubberPenColor) xor ColorToRGB(Self.Color);
    Pen.Style := psDot;
    MoveTo(p.X, 0);
    LineTo(p.X, Height);
    Pen.Mode := pmCopy;
  end;
end;

procedure TlicgHorizontalRuler.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TlicgHorizontalRuler.Paint;
var
  ULColor, LRColor: TColor;
  R: TRect;
  i, W: integer;
  Text: string;
  N: Integer;
  pt: TPoint;
  TmpPt: TlicgCoor;
  XM, XLeft, MinSep, MaxSep: Double;
  EffectiveH, m, Y, DY: Cardinal;

  procedure DrawSides(const Rect: TRect);
  begin
    with Canvas, Rect do
    begin
      Pen.Color := LRColor;
      MoveTo(Left, Top);
      LineTo(Left, Bottom);

      MoveTo(Left, Top);
      LineTo(Right, Top);

      Pen.Color := ULColor;
      MoveTo(Right - 1, Top);
      LineTo(Right - 1, Bottom);

      MoveTo(Left, Bottom - 1);
      LineTo(Right, Bottom - 1);
    end;
  end;

begin
  inherited Paint;

  if (csDesigning in ComponentState) or (FPreviewBox = nil) then
    Exit;
  FLastMousePosInRuler.X := -1000;
  DrawRulerPosition(FLastMousePosInRuler, pmNotXor);

  ULColor := clBtnShadow;
  LRColor := clBtnHighlight;
  with Canvas do
  begin
    // first, paint the borders
    Font.Assign(Self.Font);
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;
    FillRect(ClientRect);
    Brush.Style := bsClear;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;
    R := ClientRect;
    if FCtl3D then
    begin
      W := 2;
      for I := 1 to W do
      begin
        DrawSides(R);
        InflateRect(R, -1, -1);
      end;
    end;
    Pen.Color := FMarksColor;

    { calc min and max separators }
    MaxMinSeparation(FPreviewBox, FPaperUnits, MaxSep, MinSep);

    if FPreviewBox.Grapher.RealToDistX(maxsep) < 10 then
      Exit;

    //PixelsSep:= FPreviewBox.Grapher.RealToDistX(MinSep);
    //if (PixelsSep < 3) or (PixelsSep >= FPreviewBox.Width) then Exit;

    { Calc the zero point }
    TmpPt := _NULL_COOR;
    pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
      TmpPt)));

    // calc the begining of ruler...
    if pt.X > 0 then
    begin
      while pt.X >= 0 do
      begin
        TmpPt.X := TmpPt.X - MaxSep;
        pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
    end
    else if pt.X < 0 then
    begin
      while pt.X <= 0 do
      begin
        TmpPt.X := TmpPt.X + MaxSep;
        pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
      if pt.X >= Self.Width then
      begin
        TmpPt.X := TmpPt.X - MaxSep;
        pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
    end;
    TmpPt.X := TmpPt.X - MaxSep;

    // ...and draw it
    XLeft := TmpPt.X;
    m := 0;
    while pt.X < Self.Width do
    begin
      TmpPt.X := XLeft + m * MaxSep;
      pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
        TmpPt)));

      Text := FloatToStr(Round(Inches2Units(TmpPt.X, FPaperUnits)));

      { Draw the mayor scale }
      MoveTo(pt.X, 2);
      LineTo(pt.X, Self.Height - 2);

      {Draw the text}
      TextOut(pt.X + 2, 2, Text);

      {draw the minor scale}
      XM := MinSep;
      n := 0;
      while XM < MaxSep do
      begin
        pt := Self.ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          AsCoor(TmpPt.X + XM, 0))));
        Inc(n);
        EffectiveH := (Self.Height - 4);
        DY := 0;
        if FPaperUnits = suMms then
        begin
          if XM = MaxSep / 2 then
            DY := EffectiveH div 2
          else
            DY := EffectiveH div 4;
        end
        else if FPaperUnits = suCms then
        begin
          if XM = MaxSep / 2 then
            DY := EffectiveH div 2
          else
            DY := EffectiveH div 4;
        end
        else
        begin
          if MinSep = 1 / 2 then
          begin
            if n * MinSep = int(n * MinSep) then
              DY := EffectiveH div 2
            else
              DY := EffectiveH div 4;
          end
          else if MinSep = 1 / 8 then
          begin
            if n = 4 then
              DY := EffectiveH div 2
            else
              DY := EffectiveH div 4
          end
          else if MinSep = 1 / 16 then
          begin
            if n = 8 then
              DY := EffectiveH div 2
            else if n mod 2 = 0 then
              DY := EffectiveH div 3
            else
              DY := EffectiveH div 5;
          end;
        end;
        Y := Self.Height - 2;
        MoveTo(pt.X, Y);
        LineTo(pt.X, Y - DY);

        XM := XM + MinSep;
      end;

      Inc(m);
    end;
  end;
  DrawRulerPosition(FLastMousePosInRuler, pmNotXor);
end;

procedure TlicgHorizontalRuler.SetRubberPenColor(const Value: TColor);
begin
  FRubberPenColor := Value;
  Invalidate;
end;

procedure TlicgHorizontalRuler.SetMarksColor(const Value: TColor);
begin
  FMarksColor := Value;
  Invalidate;
end;

function TlicgHorizontalRuler.GetMarksColor: TColor;
begin
  Result := FMarksColor;
end;

function TlicgHorizontalRuler.GetPreviewBox: TlicgBaseDrawBox;
begin
  Result := FPreviewBox;
end;

function TlicgHorizontalRuler.GetRubberPenColor: TColor;
begin
  Result := FRubberPenColor;
end;

function TlicgHorizontalRuler.GetCtl3D: Boolean;
begin
  Result := FCtl3D
end;

procedure TlicgHorizontalRuler.SetCtl3D(const Value: Boolean);
begin
  FCtl3D := Value
end;

destructor TlicgHorizontalRuler.destroy;
begin

  inherited;
end;

{ TlicgVerticalRuler }

constructor TlicgVerticalRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alLeft;
  FRubberPenColor := clAqua;
  FMarksColor := clOlive;
  FCtl3D := True;
  FLastMousePosInRuler.X := -1000;
end;

procedure TlicgVerticalRuler.SetPreviewBox(Value: TlicgBaseDrawBox);
begin
{$IFDEF LEVEL5}
  //if Assigned(FPreviewBox) then
    //FPreviewBox.RemoveFreeNotification(Self);
{$ENDIF}
  if (Value <> nil) and (Value <> FPreviewBox) then
  begin
    //Value.FreeNotification(Self);
    if FPreviewBox <> nil then
      with FPreviewBox do
      begin
        OnMouseMove2D := FBoxOnMouseMove;
        OnPaint := FBoxOnPaint;
      end;
    if Value <> nil then
      with value do
      begin
        FBoxOnMouseMove := OnMouseMove2D;
        FBoxOnPaint := OnPaint;

        OnMouseMove2D := MyOnMouseMove2D;
        OnPaint := MyOnPaint;
      end;
  end;
  FPreviewBox := value;
  Invalidate;
end;

procedure TlicgVerticalRuler.MyOnMouseMove2D(Sender: TObject; Shift: TShiftState;
  X, Y: Integer; const WX, WY: Double);
var
  TmpY: Double;
  TmpPt: TPoint;
begin
  if Assigned(FBoxOnMouseMove) then
    FBoxOnMouseMove(Sender, Shift, X, Y, WX, WY);
  if FPreviewBox = nil then
    Exit;
  DrawRulerPosition(FLastMousePosInRuler, pmXor);
  TmpY := WY;

  //with FPreviewBox.GridInfo do
    //if SnapToGrid then
      //TmpY := Round(WY / GridSnap.Y) * GridSnap.Y + GridOffset.Y;

  if FPreviewBox.GridInfo.SnapToGrid then
    TmpY := Round(WY / FPreviewBox.GridInfo.GridSnapAperture) * FPreviewBox.GridInfo.GridSnapAperture
      + FPreviewBox.GridInfo.GridOffset.Y;

  TmpPt := _RealToPoint(FPreviewBox.Grapher, AsCoor(WX, TmpY));
  FLastMousePosInRuler := ScreenToClient(FPreviewBox.ClientToScreen(TmpPt));
  DrawRulerPosition(FLastMousePosInRuler, pmXor);
end;

procedure TlicgVerticalRuler.MyOnPaint(Sender: TObject);
begin
  if Assigned(FBoxOnPaint) then
    FBoxOnPaint(Sender);
  Self.Paint;
end;

procedure TlicgVerticalRuler.DrawRulerPosition(p: TPoint; AMode: TPenMode);
begin
  if (FPreviewBox = nil) or (p.X < 0) then
    Exit;
  with Canvas do
  begin
    Pen.Mode := AMode;
    Pen.Color := ColorToRGB(FRubberPenColor) xor ColorToRGB(Self.Color);
    Pen.Style := psDot;
    MoveTo(0, p.Y);
    LineTo(Width, p.Y);
    Pen.Mode := pmCopy;
  end;
end;

procedure TlicgVerticalRuler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
{  if (Operation = opRemove) and (AComponent = FPreviewBox.DrawWinControl) then
    FPreviewBox := nil;
    }
end;

procedure TlicgVerticalRuler.Paint;
var
  ULColor, LRColor: TColor;
  R: TRect;
  i, W: integer;
  Text: string;
  N: Integer;
  pt: TPoint;
  TmpPt: TlicgCoor;
  YM, YTop, MinSep, MaxSep: Double;
  EffectiveW, m, X, DX: Cardinal;

  procedure DrawSides(Rect: TRect);
  begin
    with Canvas, Rect do
    begin
      Pen.Color := LRColor;
      MoveTo(Left, Top);
      LineTo(Left, Bottom);

      MoveTo(Left, Top);
      LineTo(Right, Top);

      Pen.Color := ULColor;
      MoveTo(Right - 1, Top);
      LineTo(Right - 1, Bottom);

      MoveTo(Left, Bottom - 1);
      LineTo(Right, Bottom - 1);
    end;
  end;

begin
  inherited Paint;
  if (csDesigning in ComponentState) or (FPreviewBox = nil) then
    Exit;
  FLastMousePosInRuler.X := -1000;
  DrawRulerPosition(FLastMousePosInRuler, pmNotXor);

  ULColor := clBtnShadow;
  LRColor := clBtnHighlight;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Self.Color;
    FillRect(ClientRect);
    Brush.Style := bsClear;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;
    R := ClientRect;
    if FCtl3D then
    begin
      W := 2;
      for I := 1 to W do
      begin
        DrawSides(R);
        InflateRect(R, -1, -1);
      end;
    end;
    Pen.Color := FMarksColor;

    {calc min and max separators}
    MaxMinSeparation(FPreviewBox, FPaperUnits, MaxSep, MinSep);

    if FPreviewBox.Grapher.RealToDistY(maxsep) < 10 then
      exit;

    {Calc the zero point}
    TmpPt := _NULL_COOR;
    pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
      TmpPt)));
    Inc(pt.Y);

    { calc the begining of ruler... }
    if pt.Y < 0 then
    begin
      while pt.Y <= 0 do
      begin
        TmpPt.Y := TmpPt.Y - MaxSep;
        pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
      if pt.Y >= Height then
      begin
        TmpPt.Y := TmpPt.Y + MaxSep;
        pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
    end
    else if pt.Y > 0 then
    begin
      while pt.Y >= 0 do
      begin
        TmpPt.Y := TmpPt.Y + MaxSep;
        pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          TmpPt)));
      end;
    end;
    TmpPt.Y := TmpPt.Y + MaxSep;

    { ...and draw it }
    YTop := TmpPt.Y;
    m := 0;
    while pt.Y < Height do
    begin
      TmpPt.Y := YTop - m * MaxSep;
      pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
        TmpPt)));

      //Text := FloatToStr(Round(abs(TmpPt.Y + DrawBox.MarginTop)));
      //Text := FloatToStr(round(Abs(TmpPt.Y - FPreviewBox.MarginTop)));
      Text := FloatToStr(round(Inches2Units(-TmpPt.Y, FPaperUnits)));

      {draw the mayor scale}
      MoveTo(2, pt.Y);
      LineTo(Width - 2, pt.Y);

      {Draw the text}
      TextOut(2, pt.Y + 1, Text);

      {draw the minor scale}
      YM := MinSep;
      n := 0;
      while YM < MaxSep do
      begin
        pt := ScreenToClient(FPreviewBox.ClientToScreen(_RealToPoint(FPreviewBox.Grapher,
          AsCoor(0, TmpPt.Y - YM))));
        Inc(n);
        EffectiveW := (Width - 4);
        DX := 0;
        if FPaperUnits = suMms then
        begin
          if YM = MaxSep / 2 then
            DX := EffectiveW div 2
          else
            DX := EffectiveW div 4;
        end
        else if FPaperUnits = suCms then
        begin
          if YM = MaxSep / 2 then
            DX := EffectiveW div 2
          else
            DX := EffectiveW div 4;
        end
        else
        begin
          if MinSep = 1 / 2 then
          begin
            if n * MinSep = int(n * MinSep) then
              DX := EffectiveW div 2
            else
              DX := EffectiveW div 4;
          end
          else if MinSep = 1 / 8 then
          begin
            if n = 4 then
              DX := EffectiveW div 2
            else
              DX := EffectiveW div 4
          end
          else if MinSep = 1 / 16 then
          begin
            if n = 8 then
              DX := EffectiveW div 2
            else if n mod 2 = 0 then
              DX := EffectiveW div 3
            else
              DX := EffectiveW div 5;
          end;
        end;
        X := Width - 2;
        MoveTo(X, pt.Y);
        LineTo(X - DX, pt.Y);

        YM := YM + MinSep;
      end;

      Inc(m);
    end;
  end;
  DrawRulerPosition(FLastMousePosInRuler, pmNotXor);
end;

procedure TlicgVerticalRuler.SetRubberPenColor(const Value: TColor);
begin
  FRubberPenColor := Value;
  Invalidate;
end;

procedure TlicgVerticalRuler.SetMarksColor(const Value: TColor);
begin
  FMarksColor := Value;
  Invalidate;
end;

function TlicgVerticalRuler.GetMarksColor: TColor;
begin
  Result := FMarksColor;
end;

function TlicgVerticalRuler.GetPreviewBox: TlicgBaseDrawBox;
begin
  Result := FPreviewBox;
end;

function TlicgVerticalRuler.GetRubberPenColor: TColor;
begin
  Result := FRubberPenColor;
end;

function TlicgVerticalRuler.GetCtl3D: Boolean;
begin
  Result := FCtl3D
end;

procedure TlicgVerticalRuler.SetCtl3D(const Value: Boolean);
begin
  FCtl3D := Value
end;

destructor TlicgVerticalRuler.Destroy;
begin
  inherited;
end;

end.


