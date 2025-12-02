unit Lider.CG.Com.Grapher;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Dialogs,
  SysUtils,
  Graphics,
  Printers,
  Controls,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.DrawToolsInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TlicgGrapher = class(TlicgBaseGrapher)
  private
    FDevice: TlicgActiveDevice;
    // To handle the previous views
    FViewsHistory: TlicgTransformParamsList;
    FInUpdate: Boolean;
    // To save/restore the canvas attribs
    FOldPenStyle: TPenStyle;
    FOldPenMode: TPenMode;
    FOldPenWidth: Integer;
    FOldPenColor: TColor;
    FOldBrushStyle: TBrushStyle;
    FOldBrushColor: TColor;
    FOnChange: TNotifyEventStdcall;
    FDrawBoxColor: TColor;
    FDrawBox: TObject;
    FAffineMatrix: TlicgMatrix;
    FUseAffine: boolean;
  protected
    procedure Push; override;
    function GetDevice: TlicgActiveDevice; override;
    function GetInUpdate: Boolean; override;
    function GetViewPortExtent: TlicgExtent; override;
    function GetViewsHistory: TlicgBaseTransformParamsList; override;
    procedure SetDevice(const Value: TlicgActiveDevice); override;
    procedure SetInUpdate(const Value: Boolean); override;
    function GetOnChange: TNotifyEventStdcall; override;
    procedure SetOnChange(value: TNotifyEventStdcall); override;
    //function GetDrawBoxColor: TColor; override;
    //procedure SetDrawBoxColor(value: TColor); override ;
    function GetDrawBox: TObject; override;
    procedure SetDrawBox(value: TObject); override;
  public
    { methods }
    constructor Create(ADevice: TlicgActiveDevice; DrawBox: TObject = nil);
    destructor Destroy; override;
    procedure Assign(Source: TlicgBaseGrapher); override;
    procedure Pop; override;
    function CanPop: Boolean; override;
    procedure Zoom(const Factor: Double); override;
    procedure SetViewPort(const ALeft, ATop, ARight, ABottom: Double); override;
    procedure Window(const Xi, Xf, Yi, Yf: Double); override;
    procedure SetWindow(const Xi, Xf, Yi, Yf: Double); override;
    procedure SetViewTo(const ViewWin: TlicgExtent); override;
    procedure ReCentre(const MX, MY: Double); override;
    {conversion}
    function PointToReal(const P: TPoint): TlicgCoor; override;
    function RealToPoint(const P: TlicgCoor): TPoint; override;
    function ExtentToReal(const ARect: TRect): TlicgExtent; override;
    function RealToExtent(const AExtent: TlicgExtent): TRect; override;
    function DistToRealX(DistX: Integer): Double; override;
    function DistToRealY(DistY: Integer): Double; override;
    function RealToDistX(const DistX: Double): Integer; override;
    function RealToDistY(const DistY: Double): Integer; override;
    function RDistToRealX(const DistX: Double): Double; override;
    function RDistToRealY(const DistY: Double): Double; override;
    function RRealToDistX(const DistX: Double): Double; override;
    function RRealToDistY(const DistY: Double): Double; override;
    function PointsToDistX(const Size: Double): Double; override;
    function PointsToDistY(const Size: Double): Double; override;
    function DistToPointsX(const Dist: Double): Double; override;
    function DistToPointsY(const Dist: Double): Double; override;
    function DpiX: Integer; override;
    function DpiY: Integer; override;
    procedure SaveCanvas(ACanvas: TCanvas); override;
    procedure RestoreCanvas(ACanvas: TCanvas); override;
    { for internal use }
    function GetSizeInPoints(const Value: Double): Double; override;
    function GetRealSize(const value: Double): Double; override;
    function ZoomToExtentScale(AZoomExtent: TlicgExtent): double; override;
    procedure SetScale(S: double); override;
    function GetAffineMatrix: TlicgMatrix; override;
    procedure SetAffineMatrix(value: TlicgMatrix); override;
    function GetUseAffineMatrix: Boolean; override;
    procedure SetUseAffineMatrix(value: Boolean); override;
    function IsGoogleGrapher: Boolean; override;
    procedure ApplyProjection(const E: IlicgEntity; var _Clip: TlicgExtent); override;
    procedure CancelProjection(const E: IlicgEntity); override;
  end;

implementation

uses
  Consts,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.Lexlib,
  Lider.CG.Com.Yacclib,
  Lider.CG.Com.Expr,
  Lider.CG.Com.Expressions,
  Lider.CG.Com.Rtree,
  Lider.CG.Com.CmdLine;

constructor TlicgGrapher.Create(ADevice:  TlicgActiveDevice; DrawBox: TObject = nil);
var
  DC: THandle;
begin
  inherited Create(ADevice, Drawbox);
  FUseAffine := false;
  FAffineMatrix := IDENTITY_MATRIX2D;

  Device := ADevice;
  DC := GetDC(0);
  ScreenDpiX := GetDeviceCaps(DC, LOGPIXELSX);
  ScreenDpiY := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
  if ADevice = adPrinter then
  begin
    if PrintersInstalled then
    begin
      try
        PrinterDpiX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
        PrinterDpiY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
      except
        PrinterDpiX := 300;
        PrinterDpiY := 300;
      end;
    end
    else
    begin
      PrinterDpiX := 300;
      PrinterDpiY := 300;
    end;
  end;
  FViewsHistory := TlicgTransformParamsList.Create;
  FDrawBox := DrawBox;
end;

destructor TlicgGrapher.Destroy;
begin
  FViewsHistory.Free;
  
  inherited Destroy;
end;

function TlicgGrapher.GetDevice: TlicgActiveDevice;
begin
  Result := FDevice
end;

function TlicgGrapher.GetInUpdate: Boolean;
begin
  Result := FInUpdate
end;

function TlicgGrapher.GetViewPortExtent: TlicgExtent;
begin
  Result := FViewPortExtent
end;

function TlicgGrapher.GetViewsHistory: TlicgBaseTransformParamsList;
begin
  Result := FViewsHistory
end;

procedure TlicgGrapher.SetDevice(const Value: TlicgActiveDevice);
begin
  FDevice := Value
end;

procedure TlicgGrapher.SetInUpdate(const Value: Boolean);
begin
  FInUpdate := Value
end;

procedure TlicgGrapher.Assign(Source: TlicgBaseGrapher);
begin
  ScreenDpiX := Source.ScreenDpiX;
  ScreenDpiY := Source.ScreenDpiY;
  PrinterDpiX := Source.PrinterDpiX;
  PrinterDpiY := Source.PrinterDpiY;
  OriginalParams := Source.OriginalParams;
  CurrentParams := Source.CurrentParams;
  FViewPortExtent := TlicgGrapher(Source).FViewPortExtent;
  FDevice := TlicgGrapher(Source).FDevice;
end;

function TlicgGrapher.DpiX: Integer;
begin
  case Device of
    adScreen:
      Result := ScreenDpiX;
    adPrinter:
      Result := PrinterDpiX;
  else
    Result := ScreenDpiX;
  end;
end;

function TlicgGrapher.DpiY: Integer;
begin
  case Device of
    adScreen:
      Result := ScreenDpiY;
    adPrinter:
      Result := PrinterDpiY;
  else
    Result := ScreenDpiY;
  end;
end;

procedure TlicgGrapher.Zoom(const Factor: Double);
var
  TmpX, TmpY: Double;
begin
  if (Factor <= 0) or (CurrentParams.Scale = 0) then
    Exit;
  if not FInUpdate then
    Push;
  with CurrentParams, CurrentParams.VisualWindow do
  begin
    MidPoint.X := (LowerLeft.X + UpperRight.X) / 2;
    MidPoint.Y := (LowerLeft.Y + UpperRight.Y) / 2;
    TmpX := (UpperRight.X - LowerLeft.X) * Scale;
    TmpY := (UpperRight.Y - LowerLeft.Y) * Scale;
    Scale := Scale / Factor;
    UpperRight.X := MidPoint.X + (TmpX / Scale) / 2;
    LowerLeft.X := MidPoint.X - (TmpX / Scale) / 2;
    UpperRight.Y := MidPoint.Y + (TmpY / Scale) / 2;
    LowerLeft.Y := MidPoint.Y - (TmpY / Scale) / 2;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TlicgGrapher.ReCentre(const MX, MY: Double);
var
  TmpX, TmpY: Double;
begin
  if not FInUpdate then
    Push; {save current view}
  with CurrentParams do
  begin
    MidPoint.X := MX;
    MidPoint.Y := MY;
    with VisualWindow do
    begin
      TmpX := (UpperRight.X - LowerLeft.X) * Scale;
      TmpY := (UpperRight.Y - LowerLeft.Y) * Scale;
      LowerLeft.X := MidPoint.X - (TmpX / Scale) / 2;
      UpperRight.X := MidPoint.X + (TmpX / Scale) / 2;
      LowerLeft.Y := MidPoint.Y - (TmpY / Scale) / 2;
      UpperRight.Y := MidPoint.Y + (TmpY / Scale) / 2;
    end;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TlicgGrapher.SetViewPort(const ALeft, ATop, ARight, ABottom: Double);
begin
  with FViewPortExtent do
  begin
    LowerLeft.X := ALeft;
    UpperRight.Y := ATop;
    UpperRight.X := ARight;
    LowerLeft.Y := ABottom;
  end;
end;

procedure TlicgGrapher.Window(const Xi, Xf, Yi, Yf: Double);
var
  TmpW, TmpH, XScale, YScale: Double;
  TmpParams: TlicgTransformParams;
begin
  TmpParams := OriginalParams;
  try
    with OriginalParams do
    begin
      TmpW := Abs(FViewPortExtent.UpperRight.x - FViewPortExtent.LowerLeft.x);
      TmpH := Abs(FViewPortExtent.LowerLeft.y - FViewPortExtent.UpperRight.y);
      if (Xf - Xi) <> 0 then
        XScale := TmpW / Abs(Xf - Xi)
      else
        XScale := 1;
      if (Yf - Yi) <> 0 then
        YScale := TmpH / Abs(Yf - Yi)
      else
        YScale := 1;

      if YScale < XScale then
        XScale := YScale;

      if (XScale = 0) or (YScale = 0) then
        Exit;
      Scale := XScale;
      MidPoint.X := (Xf + Xi) / 2;
      MidPoint.Y := (Yf + Yi) / 2;
      with VisualWindow do
      begin
        LowerLeft.X := MidPoint.X - (TmpW / 2) / Scale;
        UpperRight.X := MidPoint.X + (TmpW / 2) / Scale;
        LowerLeft.Y := MidPoint.Y - (TmpH / 2) / Scale;
        UpperRight.Y := MidPoint.Y + (TmpH / 2) / Scale;
      end;
    end;
  except
    OriginalParams := TmpParams;
    raise;
  end;
  CurrentParams := OriginalParams;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TlicgGrapher.SetWindow(const Xi, Xf, Yi, Yf: Double);
begin
  Window(Xi, Xf, Yi, Yf);
  ViewsHistory.Clear;
end;

procedure TlicgGrapher.SetViewTo(const ViewWin: TlicgExtent);
var
  f1, f2, Dx, Dy, Hx, Hy, Factor: Double;
  TmpWin: TlicgExtent;
  Cx, Cy, TmpW, TmpH: Double;
begin
  TmpWin := ReorderExtent(ViewWin);

  if EqualPoint2D(TmpWin.LowerLeft, TmpWin.UpperRight) then
  begin
    TmpWin.LowerLeft.X := TmpWin.LowerLeft.X - 1e-6;
    TmpWin.LowerLeft.Y := TmpWin.LowerLeft.Y - 1e-6;
    TmpWin.UpperRight.X := TmpWin.UpperRight.X + 1e-6;
    TmpWin.UpperRight.Y := TmpWin.UpperRight.Y + 1e-6;
  end;

  if EqualExtension(TmpWin, _NULL_EXTENT) or EqualExtension(TmpWin,
    _INVALID_EXTENT) then
    TmpWin := _DEFAULT_EXTENT;

  if EqualExtension(TmpWin, CurrentParams.VisualWindow) then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    Exit;
  end;

  if not FInUpdate then
    Push;

  Dx := abs(TmpWin.UpperRight.x - TmpWin.LowerLeft.x);
  Dy := abs(TmpWin.UpperRight.y - TmpWin.LowerLeft.y);
  with CurrentParams.VisualWindow do
  begin
    Hx := Abs(UpperRight.X - LowerLeft.X);
    Hy := Abs(UpperRight.Y - LowerLeft.Y);
    if (Hx = 0) or (Hy = 0) then
    begin
      CurrentParams.Scale := 1;
      Exit;
    end;
    f1 := Dx / Hx;
    f2 := Dy / Hy;
    if f1 > f2 then
      Factor := f1
    else
      Factor := f2;

    TmpW := (factor * Hx) / 2;
    TmpH := (factor * Hy) / 2;
    Cx := (TmpWin.LowerLeft.x + TmpWin.UpperRight.x) / 2;
    Cy := (TmpWin.LowerLeft.y + TmpWin.UpperRight.y) / 2;

    LowerLeft.X := Cx - TmpW;
    UpperRight.Y := Cy + TmpH;
    UpperRight.X := Cx + TmpW;
    LowerLeft.Y := Cy - TmpH;

    with CurrentParams do
    begin
      MidPoint.X := (LowerLeft.X + UpperRight.X) / 2;
      MidPoint.Y := (LowerLeft.Y + UpperRight.Y) / 2;
      if Factor = 0 then
        Factor := 1; //0'a bölme hatasý nedeniyle eklendi
      Scale := Scale / Factor;
    end;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TlicgGrapher.PointToReal(const P: TPoint): TlicgCoor;
begin
  if CurrentParams.Scale = 0 then
    Exit;
  with CurrentParams.VisualWindow do
  begin
    Result.X := LowerLeft.X + (P.X - FViewPortExtent.LowerLeft.x) / CurrentParams.Scale;
    Result.Y := UpperRight.Y - (P.Y - FViewPortExtent.UpperRight.y) / CurrentParams.Scale;
  end;
end;

function TlicgGrapher.RealToPoint(const P: TlicgCoor): TPoint;
begin
  if (P.X >= _MAXCOOR) or (P.Y >= _MAXCOOR) or (P.X <= _MINCOOR) or (P.Y <= _MINCOOR) then
    Exit;
  with CurrentParams.VisualWindow do
  begin
    Result.X := Round(FViewPortExtent.LowerLeft.x + (P.X - LowerLeft.X) *
      CurrentParams.Scale);
    Result.Y := Round(FViewPortExtent.UpperRight.y + (UpperRight.Y - P.Y) *
      CurrentParams.Scale);
  end;
end;

function TlicgGrapher.ExtentToReal(const ARect: TRect): TlicgExtent;
begin
  Result.LowerLeft := Self.PointToReal(ARect.TopLeft);
  Result.UpperRight := Self.PointToReal(ARect.BottomRight);
  Result := ReorderExtent(Result);
end;

function TlicgGrapher.RealToExtent(const AExtent: TlicgExtent): TRect;
var
  TmpPt1, TmpPt2: TPoint;
  Work: TlicgExtent;
begin
  Work := ReorderExtent(AExtent);
  TmpPt1 := RealToPoint(Work.LowerLeft);
  TmpPt2 := RealToPoint(Work.UpperRight);
  Result.Left := TmpPt1.X;
  Result.Top := TmpPt2.Y;
  Result.Right := TmpPt2.X;
  Result.Bottom := TmpPt1.Y;
end;

function TlicgGrapher.DistToRealX(DistX: Integer): Double;
begin
  Result := 0;
  if FViewPortExtent.UpperRight.X = FViewPortExtent.LowerLeft.X then
    Exit;
  with CurrentParams.VisualWindow do
    Result := ((UpperRight.X - LowerLeft.X) / Abs(FViewPortExtent.UpperRight.x -
      FViewPortExtent.LowerLeft.x)) * DistX;
end;

function TlicgGrapher.DistToRealY(DistY: Integer): Double;
begin
  Result := 0;
  if FViewPortExtent.UpperRight.Y = FViewPortExtent.LowerLeft.Y then
    Exit;
  with CurrentParams.VisualWindow do
    Result := ((UpperRight.Y - LowerLeft.Y) / Abs(FViewPortExtent.UpperRight.y -
      FViewPortExtent.LowerLeft.y)) * DistY;
end;

function TlicgGrapher.RealToDistX(const DistX: Double): Integer;
begin
  with CurrentParams.VisualWindow do
  begin
    if (DistX = 0) or (UpperRight.X = LowerLeft.X) or (FViewPortExtent.UpperRight.X
        = FViewPortExtent.LowerLeft.X) then
    begin
      Result := 0;
      Exit;
    end;
    Result := Round(DistX / (UpperRight.X - LowerLeft.X) * Abs(FViewPortExtent.UpperRight.x
      - FViewPortExtent.LowerLeft.x));
  end;
end;

function TlicgGrapher.RealToDistY(const DistY: Double): Integer;
begin
  with CurrentParams.VisualWindow do
  begin
    if (DistY = 0) or (UpperRight.Y = LowerLeft.Y) or (FViewPortExtent.UpperRight.y
        = FViewPortExtent.LowerLeft.y) then
    begin
      Result := 0;
      Exit;
    end;
    Result := Round((DistY / (UpperRight.Y - LowerLeft.Y)) * Abs(FViewPortExtent.UpperRight.y
      - FViewPortExtent.LowerLeft.y));
  end;
end;

function TlicgGrapher.RDistToRealX(const DistX: Double): Double;
begin
  Result := 0;
  with CurrentParams.VisualWindow do
  begin
    if FViewPortExtent.UpperRight.x = FViewPortExtent.LowerLeft.x then
      Exit;
    Result := ((UpperRight.X - LowerLeft.X) / Abs(FViewPortExtent.UpperRight.x -
      FViewPortExtent.LowerLeft.x)) * DistX;
  end;
end;

function TlicgGrapher.RDistToRealY(const DistY: Double): Double;
begin
  Result := 0;
  with CurrentParams.VisualWindow do
  begin
    if FViewPortExtent.UpperRight.y = FViewPortExtent.LowerLeft.y then
      Exit;
    Result := ((UpperRight.Y - LowerLeft.Y) / Abs(FViewPortExtent.UpperRight.y -
      FViewPortExtent.LowerLeft.y)) * DistY;
  end;
end;

function TlicgGrapher.RRealToDistX(const DistX: Double): Double;
begin
  with CurrentParams.VisualWindow do
  begin
    if (DistX = 0) or (UpperRight.X = LowerLeft.X) or (FViewPortExtent.UpperRight.x
      = FViewPortExtent.LowerLeft.x) then
    begin
      Result := 0;
      Exit;
    end;
    Result := DistX / (UpperRight.X - LowerLeft.X) * Abs(FViewPortExtent.UpperRight.x
      - FViewPortExtent.LowerLeft.x);
  end;
end;

function TlicgGrapher.RRealToDistY(const DistY: Double): Double;
begin
  with CurrentParams.VisualWindow do
  begin
    if (DistY = 0) or (UpperRight.Y = LowerLeft.Y) then
    begin
      Result := 0;
      Exit;
    end;
    Result := (DistY / (UpperRight.Y - LowerLeft.Y)) * Abs(FViewPortExtent.UpperRight.y
      - FViewPortExtent.LowerLeft.y);
  end;
end;

function TlicgGrapher.PointsToDistX(const Size: Double): Double;
var
  Dpis: Integer;
begin
  if Device = adScreen then
    Dpis := ScreenDpiX
  else
    Dpis := PrinterDpiX;
  Result := RDistToRealX(Size * Dpis / 72);
end;

function TlicgGrapher.PointsToDistY(const Size: Double): Double;
var
  Dpis: Integer;
begin
  if Device = adScreen then
    Dpis := ScreenDpiY
  else
    Dpis := PrinterDpiY;
  Result := RDistToRealY(Size * Dpis / 72);
end;

function TlicgGrapher.DistToPointsX(const Dist: Double): Double;
var
  Dpis: Integer;
begin
  if Device = adScreen then
    Dpis := ScreenDpiX
  else
    Dpis := PrinterDpiX;
  Result := RRealToDistX(Dist) * 72 / Dpis;
end;

function TlicgGrapher.DistToPointsY(const Dist: Double): Double;
var
  Dpis: Integer;
begin
  Result := 0;
  if Dpis = 0 then
    Exit;
  if Device = adScreen then
    Dpis := ScreenDpiY
  else
    Dpis := PrinterDpiY;
  Result := RRealToDistY(Dist) * 72 / Dpis;
end;

procedure TlicgGrapher.Push;
begin
  if (FViewsHistory.Count > 0) and (FViewsHistory.Count >= Licad.Settings.MaxSavedViews)
    then
    FViewsHistory.Delete(0);
  FViewsHistory.Add(CurrentParams);
end;

function TlicgGrapher.CanPop: Boolean;
begin
  Result := FViewsHistory.Count > 0;
end;

procedure TlicgGrapher.Pop;
begin
  if FViewsHistory.Count = 0 then
    Exit;
  CurrentParams := FViewsHistory.Items[FViewsHistory.Count - 1];
  FViewsHistory.Delete(FViewsHistory.Count - 1);
  FInUpdate := True;
  SetViewTo(CurrentParams.VisualWindow);
  FInUpdate := false;
end;

procedure TlicgGrapher.SaveCanvas(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    FOldPenStyle := Pen.Style;
    FOldPenMode := Pen.Mode;
    FOldPenWidth := Pen.Width;
    FOldPenColor := Pen.Color;
    FOldBrushStyle := Brush.Style;
    FOldBrushColor := Brush.Color;
  end;
end;

procedure TlicgGrapher.RestoreCanvas(ACanvas: TCanvas);
begin
  with ACanvas do
  begin
    Pen.Mode := FOldPenMode;
    Pen.Style := FOldPenStyle;
    Pen.Width := FOldPenWidth;
    Pen.Color := FOldPenColor;
    Brush.Style := FOldBrushStyle;
    Brush.Color := FOldBrushColor;
  end;
end;

function TlicgGrapher.GetSizeInPoints(const Value: Double): Double;
begin
  if Value < 0 then
    Result := Abs(Value)
  else
    Result := DistToPointsY(value);
end;

function TlicgGrapher.GetRealSize(const value: Double): Double;
begin
  if value < 0 then
    Result := PointsToDistY(Abs(Value))
  else
    Result := value;
end;

function TlicgGrapher.getOnChange: TNotifyEventStdcall;
begin
  Result := FOnChange;
end;

procedure TlicgGrapher.setOnChange(value: TNotifyEventStdcall);
begin
  FOnChange := value;
end;

function TlicgGrapher.ZoomToExtentScale(AZoomExtent: TlicgExtent): double;
var
  _CurrentParams: TlicgTransformParams;

  procedure _SetViewTo(ViewWin: TlicgExtent);
  var
    f1, f2, Dx, Dy, Hx, Hy, Factor: Double;
    TmpWin: TlicgExtent;
    Cx, Cy, TmpW, TmpH: Double;
  begin
    TmpWin := ReorderExtent(ViewWin);

    if EqualExtension(TmpWin, _NULL_EXTENT) or EqualExtension(TmpWin,
      _INVALID_EXTENT) then
      TmpWin := _DEFAULT_EXTENT;

    Dx := abs(TmpWin.UpperRight.x - TmpWin.LowerLeft.x);
    Dy := abs(TmpWin.UpperRight.y - TmpWin.LowerLeft.y);

    with _CurrentParams.VisualWindow do
    begin
      Hx := Abs(UpperRight.X - LowerLeft.X);
      Hy := Abs(UpperRight.Y - LowerLeft.Y);
      if (Hx = 0) or (Hy = 0) then
      begin
        _CurrentParams.Scale := 1;
        Exit;
      end;
      f1 := Dx / Hx;
      f2 := Dy / Hy;
      if f1 > f2 then
        Factor := f1
      else
        Factor := f2;

      TmpW := (factor * Hx) / 2;
      TmpH := (factor * Hy) / 2;
      Cx := (TmpWin.LowerLeft.x + TmpWin.UpperRight.x) / 2;
      Cy := (TmpWin.LowerLeft.y + TmpWin.UpperRight.y) / 2;

      LowerLeft.X := Cx - TmpW;
      UpperRight.Y := Cy + TmpH;
      UpperRight.X := Cx + TmpW;
      LowerLeft.Y := Cy - TmpH;

      with _CurrentParams do
      begin
        MidPoint.X := (LowerLeft.X + UpperRight.X) / 2;
        MidPoint.Y := (LowerLeft.Y + UpperRight.Y) / 2;
        if Factor = 0 then
          Factor := 1; //Sýfýra bölme hatasý nedeniyle eklendi
        Scale := Scale / Factor;
      end;
    end;
  end;

var
  TmpMarginX, TmpMarginY: double;
begin
  _CurrentParams := CurrentParams;

  with AZoomExtent do
  begin
    TmpMarginX := (UpperRight.X - LowerLeft.X) / 20;
    TmpMarginY := (UpperRight.Y - LowerLeft.Y) / 20;
    LowerLeft.X := LowerLeft.X - TmpMarginX;
    UpperRight.X := UpperRight.X + TmpMarginX;
    LowerLeft.Y := LowerLeft.Y - TmpMarginY;
    UpperRight.Y := UpperRight.Y + TmpMarginY;
    _SetViewTo(AZoomExtent);
    Result := _CurrentParams.Scale;
  end;
end;

function TlicgGrapher.GetDrawBox: TObject;
begin
  Result := FDrawBox;
end;

procedure TlicgGrapher.SetDrawBox(value: TObject);
begin
  FDrawBox := value
end;

function TlicgGrapher.GetAffineMatrix: TlicgMatrix;
begin
  Result := FAffineMatrix;
end;

function TlicgGrapher.GetUseAffineMatrix: Boolean;
begin
  Result := FUseAffine;
end;

procedure TlicgGrapher.SetAffineMatrix(value: TlicgMatrix);
begin
  FAffineMatrix := value;
end;

procedure TlicgGrapher.SetUseAffineMatrix(value: Boolean);
begin
  FUseAffine := value;
end;

function TlicgGrapher.IsGoogleGrapher: Boolean;
begin
  Result := False;
end;

procedure TlicgGrapher.ApplyProjection(const E: IlicgEntity; var _Clip: TlicgExtent);
var
  G: TlicgBaseGIS;
  inP, outP: IlicgCS;
  xi, xf, yi, yf: double;
begin
  Exit;
  if Assigned(E.Layer) then
    if Assigned(TlicgBaseLayer(E.Layer).GIS) then
    begin
      G := TlicgBaseLayer(E.Layer).GIS;
      if G.MapInfo.IsOntheFly then
      begin

        xi := CurrentParams.VisualWindow.LowerLeft.X;
        xf := CurrentParams.VisualWindow.UpperRight.X;
        yi := CurrentParams.VisualWindow.LowerLeft.y;
        yf := CurrentParams.VisualWindow.UpperRight.y;

        inP := G.MapInfo.CS;
        outP := G.MapInfo.OntheFlyCS;

        G.CSCoorToCSCoor(xi, yi, xi, yi, inP, outP, nil);
        G.CSCoorToCSCoor(xf, yf, xf, yf, inP, outP, nil);

        G.CSCoorToCSCoor(_Clip.LowerLeft.X, _Clip.LowerLeft.Y, _Clip.LowerLeft.X,
          _Clip.LowerLeft.Y, inP, outP, nil);
        G.CSCoorToCSCoor(_Clip.UpperRight.X, _Clip.UpperRight.Y, _Clip.UpperRight.X,
          _Clip.UpperRight.Y, inP, outP, nil);

        Window(xi, xf, yi, yf);

        E.ApplyProject(G, inP, outP, nil);
        E.Geometry.UpdateExtension;

      end;
       //TlicgBaseLayer(E.Layer).GIS.ProjectPoint
    end;
end;

procedure TlicgGrapher.CancelProjection(const E: IlicgEntity);
var
  G: TlicgBaseGIS;
  inP, outP: IlicgCS;
  xi, xf, yi, yf: double;
begin
  Exit;
  if Assigned(E.Layer) then
    if Assigned(TlicgBaseLayer(E.Layer).GIS) then
    begin
      G := TlicgBaseLayer(E.Layer).GIS;
      if G.MapInfo.IsOntheFly then
      begin
        xi := CurrentParams.VisualWindow.LowerLeft.X;
        xf := CurrentParams.VisualWindow.UpperRight.X;
        yi := CurrentParams.VisualWindow.LowerLeft.y;
        yf := CurrentParams.VisualWindow.UpperRight.y;

        inP := G.MapInfo.CS;
        outP := G.MapInfo.OntheFlyCS;

        G.CSCoorToCSCoor(xi, yi, xi, yi, outP, inP, nil);
        G.CSCoorToCSCoor(xf, yf, xf, yf, outP, inP, nil);

        Window(xi, xf, yi, yf);
        E.ApplyProject(G, outP, inP, nil);
        E.Geometry.UpdateExtension;
      end;
       //TlicgBaseLayer(E.Layer).GIS.ProjectPoint
    end;
end;

procedure TlicgGrapher.SetScale(S: double);
var
  dx, dy, TmpW, TmpH, XScale, YScale: Double;
  TmpParams: TlicgTransformParams;
  Xi, Xf, Yi, Yf: double;
var
  ScaleDist: Integer;
begin
  ScaleDist := Round(self.ScreenDpiX / 0.0254); // ms

  TmpW := Abs(FViewPortExtent.UpperRight.x - FViewPortExtent.LowerLeft.x);
  TmpH := Abs(FViewPortExtent.LowerLeft.y - FViewPortExtent.UpperRight.y);

  dx := ((s * 1000) / ScaleDist) * Abs(TmpW);
  dy := ((s * 1000) / ScaleDist) * Abs(TmpH);

  TmpParams := OriginalParams;
  try
    with OriginalParams do
    begin

      Xi := MidPoint.X - dx / 2;
      Xf := MidPoint.X + dx / 2;

      Yi := MidPoint.Y - dy / 2;
      Yf := MidPoint.Y + dy / 2;

      if (Xf - Xi) <> 0 then
        XScale := TmpW / Abs(Xf - Xi)
      else
        XScale := 1;

      if (Yf - Yi) <> 0 then
        YScale := TmpH / Abs(Yf - Yi)
      else
        YScale := 1;

      if YScale < XScale then
        XScale := YScale;

      if (XScale = 0) or (YScale = 0) then
        Exit;
      Scale := XScale;
      MidPoint.X := (Xf + Xi) / 2;
      MidPoint.Y := (Yf + Yi) / 2;
      with VisualWindow do
      begin
        LowerLeft.X := MidPoint.X - (TmpW / 2) / Scale;
        UpperRight.X := MidPoint.X + (TmpW / 2) / Scale;
        LowerLeft.Y := MidPoint.Y - (TmpH / 2) / Scale;
        UpperRight.Y := MidPoint.Y + (TmpH / 2) / Scale;
      end;
    end;
  except
    OriginalParams := TmpParams;
    raise;
  end;
  CurrentParams := OriginalParams;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.


