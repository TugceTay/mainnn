unit Lider.CG.Com.DistanceMeter;

interface

uses
  Graphics,
  Classes,
  Controls,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.GeoTypes;
type

TDistanceMeterAction = class(TlicgAction)
  private
    nokta1, nokta2: Tlicgcoor;
    templine : IlicgEntity;
  protected
     procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer; const WX, WY: Double);
    procedure MyPaint(Sender: TObject);
  public
    constructor CreateAction(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
    procedure UndoOperation; override;
    procedure ContinueOperation(Sender: TObject);
    procedure SuspendOperation(Sender: TObject);
  end;

var
  mesafe : double;

implementation
uses
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.System,
  Lider.CG.Com.GeoLibrary;

procedure TDistanceMeterAction.MyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
begin
   if Button = mbLeft then
  begin
    if (nokta1.X = 0) and (nokta1.Y = 0) then
    begin
      nokta1.X := WX;
      nokta1.Y := WY;
      Caption := 'Ölçülcek 2.noktayý gösteriniz.';
    end
    else
    begin
      nokta2.X := WX;
      nokta2.Y := WY;
      mesafe := _Distance(nokta1,nokta2);
      Finished := true;
    end;

  end
  else if Button = mbRight then
  begin
    mesafe := -1;
    Finished := true;
  end;
end;
procedure TDistanceMeterAction.MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
    Integer; const WX, WY: Double);
//var     Davulcu Silme
//  geciciNokta : TlicgCoor;
begin
  if not ((nokta1.X = 0) and (nokta1.Y = 0)) then
  begin
    CmdLine.ActiveDrawBox.drawEntity2DRubberBand(templine,false,false,clRed);
    templine.Geometry.Points.Clear;
    templine.Geometry.Points.Add( nokta1.X, nokta1.Y );
    templine.Geometry.Points.Add(WX, WY);
    CmdLine.ActiveDrawBox.drawEntity2DRubberBand(templine,false,false,clRed);
  end;

end;

procedure TDistanceMeterAction.MyMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
begin

end;

procedure TDistanceMeterAction.MyPaint(Sender: TObject);
begin

end;

procedure TDistanceMeterAction.SuspendOperation(Sender: TObject);
begin

end;

procedure TDistanceMeterAction.UndoOperation;
begin
  inherited;

end;
constructor TDistanceMeterAction.CreateAction(CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateAction(CmdLine);

  WaitingMouseClick := True;
  CanBeSuspended := True;
  OnMouseDown := MyMouseDown;
  OnMouseMove := MyMouseMove;
  OnMouseUp := MyMouseUp;
  OnPaint := MyPaint;
  OnSuspendOperation := SuspendOperation;
  OnContinueOperation := ContinueOperation;
  CanDoOsnap := True;
  CanDoAccuDraw := True;
  Cursor := crDrawCross;
  Caption := 'Ölçülecek 1.noktayý gösteriniz.';
  CanDoOsnap := True;
  templine := Licad.CreateEntityFactory.MakeEntity(idLine, 1, _2D);
end;

destructor TDistanceMeterAction.Destroy;
begin
  templine := nil;
  inherited;
end;

procedure TDistanceMeterAction.ContinueOperation(Sender: TObject);
begin

end;

end.
