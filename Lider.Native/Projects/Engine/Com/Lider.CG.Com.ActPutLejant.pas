unit Lider.CG.Com.ActPutLejant;

interface

uses
  Controls,
  Classes,
  SysUtils,
  DB,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TPutLejandAction = class(TlicgAction)
  private
    groupEntity: IlicgEntity;
    FDrawPoint: TlicgCoor;
    LejandLayer: string;
    procedure DrawRubberEntities;
  protected
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    procedure MyKeyPress(Sender: TObject; var Key: Char);
    procedure MyPaint(Sender: TObject);
    procedure SuspendOperation(Sender: TObject);
    procedure ContinueOperation(Sender: TObject);
  public
    constructor CreateAction(var aGroupEntity: IlicgEntity; const aLejandLayer:
      string; CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
  end;

implementation

{ TPutLejandAction }

procedure TPutLejandAction.ContinueOperation(Sender: TObject);
begin
  DrawRubberEntities;
end;

constructor TPutLejandAction.CreateAction(var aGroupEntity: IlicgEntity; const
  aLejandLayer: string; CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateAction(CmdLine);

  if not Assigned(agroupEntity) then
    exit;

  if AslicgGroupGeometry(agroupEntity.Geometry).NumEntities < 1 then
    exit;

  groupEntity := agroupEntity;
  LejandLayer := aLejandLayer;

  WaitingMouseClick := true;

  CanBeSuspended := True;

  OnMouseDown := MyMouseDown;
  OnMouseMove := MyMouseMove;
  OnSuspendOperation := SuspendOperation;
  OnContinueOperation := ContinueOperation;
  OnKeyPress := MyKeyPress;
  OnPaint := MyPaint;
  Cursor := crDrawCross;
  CanDoOsnap := True;
  Caption := 'Lejandýn yerini gösteriniz';
end;

destructor TPutLejandAction.Destroy;
begin
  groupEntity := nil;
  inherited;
end;

procedure TPutLejandAction.DrawRubberEntities;
begin
  Cmdline.ActiveDrawBox.DrawEntityRubberBand(groupEntity);
end;

procedure TPutLejandAction.MyKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Self.Finished := true;
end;

procedure TPutLejandAction.MyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
var
  _Recno: Integer;
  Layer: TlicgBaseLayer;
  p: TlicgCoor;
  Tx, Ty: Double;
  Mat: TlicgMatrix;
begin
  if Button = mbRight then
  begin
    Self.Finished := True;
    Exit;
  end;

  with cmdline.ActiveDrawBox do
  begin
    if Cmdline.IsSnapped then
      FDrawPoint := CmdLine.GetSnappedPoint
    else
      FDrawPoint := AsCoor(WX, WY);

    p.x := groupEntity.Geometry.Extent.LowerLeft.x;
    p.y := groupEntity.Geometry.Extent.UpperRight.y;
    Tx := FDrawPoint.x - p.x;
    Ty := FDrawPoint.y - p.y;

    Mat := Translate2D(tx, ty);
    groupEntity.Geometry.ResetTransform;
    groupEntity.Geometry.SetTransformMatrix(Mat);
    groupEntity.Geometry.ApplyTransform;
     //<-
{      groupEntity.Geometry.ResetTransform;
      groupEntity.Geometry.TranslateTransform(FDrawPoint.x, FDrawPoint.y);
      groupEntity.Geometry.ApplyTransform;
}
    Undo.BeginUndo(uaDelete);
    Layer := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName(LejandLayer);

    if Layer = nil then
    begin
      Layer := CmdLine.ActiveDrawBox.GIS.CreateLayer(LejandLayer, ltDesktop);
    end;
    Layer.LayerInfo.Visible := True;
    groupEntity.Geometry.UpdateExtension;
    _Recno := Layer.AddEntity(groupEntity);

    undo.AddUndo(Layer, _Recno, uaDelete);
    Undo.EndUndo;
    cmdLine.ActiveDrawBox.RepaintExtent(groupEntity.Geometry.Extent);
    Self.Finished := True;
  end;
end;

procedure TPutLejandAction.MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y:
  Integer; const WX, WY: Double);
var
  CurrPoint, p: TlicgCoor;
  Mat: TlicgMatrix;
  tx, ty: Double;
begin
  WaitingMouseClick := False;
  DrawRubberEntities;
  if Cmdline.IsSnapped then
    CurrPoint := Cmdline.GetSnappedPoint
  else
    CurrPoint := AsCoor(WX, WY);
  FDrawPoint := CurrPoint;

  p.x := groupEntity.Geometry.Extent.LowerLeft.x;
  p.y := groupEntity.Geometry.Extent.UpperRight.y;
  Tx := FDrawPoint.x - p.x;
  Ty := FDrawPoint.y - p.y;

  Mat := Translate2D(tx, ty);
  groupEntity.Geometry.ResetTransform;
  groupEntity.Geometry.SetTransformMatrix(Mat);
  groupEntity.Geometry.ApplyTransform;
   //<-

  { groupEntity.Geometry.ResetTransform;
   groupEntity.Geometry.TranslateTransform( FDrawPoint.x, FDrawPoint.y );
   groupEntity.Geometry.ApplyTransform;
  }
  DrawRubberEntities;
end;

procedure TPutLejandAction.MyPaint(Sender: TObject);
begin
  DrawRubberEntities;
end;

procedure TPutLejandAction.SuspendOperation(Sender: TObject);
begin
  DrawRubberEntities;
end;

end.


