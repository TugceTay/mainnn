unit Lider.CG.LiWork.LauncherAddingTextToTheParcel;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  Graphics,
  Forms,
  Math,
  sgConsts,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.Consts,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  System.Generics.Collections,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.Lib,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.Rtree;
//  Lider.CG.LiWork.RoleveOlcu;

type TAddingTextToTheParcelLauncher = class(TLicgLauncher)
  private
  FParselList: IlicgEntityList;
  FOldEntIDS, FNewEntIDS: TlicgEntityIDS;
    {Eventler}
   procedure LauncherFinished(Sender: TObject);
   procedure LauncherSuspendOperation(Sender: TObject);
   procedure LauncherContinueOperation(Sender: TObject);
   procedure TextEkle(FParselList: IlicgEntityList);
  public
    constructor CreateLauncher(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
end;

implementation

uses
  Lider.CG.Com.Math,
  Lider.CG.LiWork.TextForm;
{ TAddingTextToTheParcelLauncher }

constructor TAddingTextToTheParcelLauncher.CreateLauncher(
  CmdLine: TlicgBaseCmdLine);
begin
  inherited CreateLauncher(CmdLine);
  Launcher.TrackQuickSelect(SCmdLauncher);
  FOldEntIDS := CmdLine.ActiveDrawBox.NoPickFilter;
  FNewEntIDS := AllEntityIDs - [idPolygon];
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
  Launcher.CurrentAction.Caption:= 'Kapalý Alanlarý seçiniz';
  Launcher.OnFinished := LauncherFinished;
  Launcher.CurrentAction.OnContinueOperation := LauncherContinueOperation;
  Launcher.CurrentAction.OnSuspendOperation := LauncherSuspendOperation;
end;

destructor TAddingTextToTheParcelLauncher.Destroy;
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntIDS;
  FParselList := nil;
  inherited;
end;

procedure TAddingTextToTheParcelLauncher.LauncherContinueOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FNewEntIDS;
end;

procedure  TAddingTextToTheParcelLauncher.TextEkle(FParselList: IlicgEntityList);
var
  I, J, K, Rc, Rc2 : Integer;
  Ent, Ent2, TextEnt, TextEnt2, Poligon, Poligon2: IlicgEntity;
  Layer: TlicgBaseLayer;
  Kontrol, Kontrol2, Kontrol3: Boolean;
  IV: ILicgVector;
  Ext: TlicgExtent;
begin
  CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
  Ent2 := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
  TextEnt2:= Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
  if (CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1') then
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D)
  else
    TextEnt := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  TextEnt.DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
  Poligon := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _3D);
  Poligon2 := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _3D);
  TextEnt.DrawTools.FontTool.TextPos := tpCenter;                 //Text merkez noktasý ortaya alýndý
  TextEnt2.DrawTools.FontTool.TextPos := tpCenter;

  if (fmTextForm.teMetin.Text = '') or (fmTextForm.ceBoyut.Text = '') then
  begin
    CmdLine.ActiveDrawBox.Selection.Clear;
  end
  else
  begin
    TextEnt.DrawTools.FontTool.Height := fmTextForm.ceBoyut.Value * (CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000);
    TextEnt2.DrawTools.FontTool.Height := fmTextForm.ceBoyut.Value * (CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000);
    for I := 0 to FParselList.Count - 1 do
    begin
      TextEnt.Geometry.Points[0] := FParselList[I].Geometry.Centroid;
      TextEnt2.Geometry.Points[0] := FParselList[I].Geometry.Centroid;
      TextEnt.AsTextValue.Text := fmTextForm.teMetin.Text;
      TextEnt2.AsTextValue.Text := fmTextForm.teMetin.Text;
      TextEnt.Geometry.UpdateExtension;                             // Yazý nesnesinin extenti güncellenir(Tüm nesnelerde kullanýlabilir)
      TextEnt2.DrawTools.FontTool.Height := TextEnt.DrawTools.FontTool.Height;
      TextEnt2.Geometry.UpdateExtension;
      with CurrCmdLine.ActiveDrawBox do
      begin
        FOldEntIDS := NoPickFilter;
        FNewEntIDS :=  AllEntityIDs - [idText, idVectorialText] ;
        NoPickFilter := FNewEntIDS;
        IV := Licad.CreateEntityFactory.MakeVector(3, 0);
        Selection.Clear;
        Licad.SelectEntityWithGivenPolygon(FParselList[I], currcmdLine, goExtentOverlaps);
        if Selection.Count > 0 then
        begin
          for J := 0 to SeLection.Count - 1 do
          begin
            Layer := Selection[J].Layer;
            for K := 0 to Selection[J].SelList.Count - 1 do
            begin
              Rc := Selection[J].SelList[K];
              Ent := Layer.LoadEntityWithRecNo(Rc);
              Ent2.Geometry.Points[0] := Ent.Geometry.Points[0];
              Ent2.DrawTools.FontTool.Angle := Ent.DrawTools.FontTool.Angle;
              Ent2.AsTextValue.Text := Ent.AsTextValue.Text;
              Ent2.DrawTools.FontTool.TextPos := tpCenter;
              Ent2.DrawTools.FontTool.Height := Ent.DrawTools.FontTool.Height;
              Ent2.Geometry.UpdateExtension;
              Poligon.Geometry.Points.Clear;
              Poligon2.Geometry.Points.Clear;
              Poligon.Geometry.Points.Add(Ent2.Extent.LowerLeft.X, Ent2.Extent.UpperRight.Y);
              Poligon.Geometry.Points.Add(Ent2.Extent.UpperRight.X, Ent2.Extent.UpperRight.Y);
              Poligon.Geometry.Points.Add(Ent2.Extent.UpperRight.X, Ent2.Extent.LowerLeft.Y);
              Poligon.Geometry.Points.Add(Ent2.Extent.LowerLeft.X, Ent2.Extent.LowerLeft.Y);
              Poligon.Geometry.Points.Add(Ent2.Extent.LowerLeft.X, Ent2.Extent.UpperRight.Y);

              Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.UpperRight.Y);
              Poligon2.Geometry.Points.Add(TextEnt2.Extent.UpperRight.X, TextEnt2.Extent.UpperRight.Y);
              Poligon2.Geometry.Points.Add(TextEnt2.Extent.UpperRight.X, TextEnt2.Extent.LowerLeft.Y);
              Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.LowerLeft.Y);
              Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.UpperRight.Y);
              IV.Clear;
              Kontrol3 := Poligon.Geometry.inPoly(Poligon2.geometry.centroid);
              Kontrol2 := Poligon2.Geometry.inPoly(Poligon.geometry.centroid);
              Kontrol:= _IntersectionPoints(Poligon.Geometry.Points, Poligon2.Geometry.Points,
              Poligon.Geometry.Points.Count, Poligon2.Geometry.Points.Count, IV, itCrossTouch);
              if (Kontrol = True) or (Kontrol2 = True) or (Kontrol3 = True) then
              begin
                while (Kontrol = True) or (Kontrol2 = True) or (Kontrol3 = True) do
                begin
                  IV.Clear;
                  Poligon2.Geometry.Points.Clear;
                  if (Ent.DrawTools.FontTool.Angle >= (3*Pi)/9) and (Ent.DrawTools.FontTool.Angle <= (6*Pi)/9) and (fmTextForm.chkAci.checked) then
                  begin
                    Ent2.DrawTools.FontTool.Angle := Pi/2;
                    TextEnt2.DrawTools.FontTool.Angle := Pi/2;
                    TextEnt2.Geometry.Points.X[0] := TextEnt2.Geometry.Points.X[0] - TextEnt.DrawTools.FontTool.Height/4;
                  end
                  else if (Ent.DrawTools.FontTool.Angle <= -(3*Pi)/9) and (Ent.DrawTools.FontTool.Angle >= -(5*Pi)/6) and (fmTextForm.chkAci.checked) then
                  begin
                    Ent2.DrawTools.FontTool.Angle := Pi/2;
                    TextEnt2.DrawTools.FontTool.Angle := Pi/2;
                    TextEnt2.Geometry.Points.X[0] := TextEnt2.Geometry.Points.X[0] - TextEnt.DrawTools.FontTool.Height/4;
                  end
                  else
                  begin
                    Ent2.DrawTools.FontTool.Angle := 0;
                    TextEnt2.DrawTools.FontTool.Angle := 0;
                    TextEnt2.Geometry.Points.Y[0] := TextEnt2.Geometry.Points.Y[0] - TextEnt.DrawTools.FontTool.Height/4;
                  end;
                  TextEnt2.Geometry.UpdateExtension;
                  Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.UpperRight.Y);
                  Poligon2.Geometry.Points.Add(TextEnt2.Extent.UpperRight.X, TextEnt2.Extent.UpperRight.Y);
                  Poligon2.Geometry.Points.Add(TextEnt2.Extent.UpperRight.X, TextEnt2.Extent.LowerLeft.Y);
                  Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.LowerLeft.Y);
                  Poligon2.Geometry.Points.Add(TextEnt2.Extent.LowerLeft.X, TextEnt2.Extent.UpperRight.Y);
                  Kontrol3 := Poligon.Geometry.inPoly(Poligon2.geometry.centroid);
                  Kontrol2 := Poligon2.Geometry.inPoly(Poligon.geometry.centroid);
                  Kontrol:= _IntersectionPoints(Poligon.Geometry.Points, Poligon2.Geometry.Points,
                    Poligon.Geometry.Points.Count, Poligon2.Geometry.Points.Count, IV, itCrossTouch);
                end;
              end;
            end;
            {if not (fmTextForm.chkAci.checked) then
              TextEnt.DrawTools.FontTool.Angle := 0
            else}
            if fmTextForm.chkAci.checked then
              TextEnt.DrawTools.FontTool.Angle := Ent.DrawTools.FontTool.Angle;
            TextEnt.Geometry.Points[0] := TextEnt2.Geometry.Points[0];
            Rc2 := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt);
            CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc2, uaDelete);
            CalcMaxMinBounds(Ext, TextEnt.Extent);
          end;
        end
        else
        begin
          TextEnt.DrawTools.FontTool.Angle := 0;
          Rc2 := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(TextEnt);
          CurrCmdLine.ActiveDrawBox.Undo.AddUndo(CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc2, uaDelete);
          CalcMaxMinBounds(Ext, TextEnt.Extent);
        end;
      end;
//      Poligon.Geometry.Points.Clear;
//      Poligon2.Geometry.Points.Clear;
    end;

    CurrCmdLine.ActiveDrawBox.RepaintExtent(Ext);
    CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
  end;
  Poligon := nil;
  Poligon2 := nil;
  TextEnt := nil;
  TextEnt2 := nil;
end;

procedure TAddingTextToTheParcelLauncher.LauncherFinished(Sender: TObject);
var
  Rc, I, J: Integer;
  TempLayer: TLicgBaseLayer;
  Ent: ILicgEntity;
begin
  if CmdLine.ActiveDrawBox.Selection.Count > 0 then
  begin
    fmTextForm := TfmTextForm.Create(nil);
    fmTextForm.ShowModal;
    if fmTextForm.ModalResult = mrOk then
    begin
      FParselList := TlicgEntityList.Create;
      with CmdLine.ActiveDrawBox do
      for I := 0 to Selection.Count - 1 do
      begin
        for J := 0 to Selection[I].SelList.Count - 1 do
        begin
          TempLayer := TLicgBaseLayer(Selection[I].Layer);
          Rc := Selection[I].SelList[J];
          Ent := TempLayer.LoadEntityWithRecNo(Rc);
          if (Ent.EntityID in [idPolygon]) then
            FParselList.add(Selection[I].Layer.LoadEntityWithRecNo(Rc));
        end;
      end;
      Ent := nil;
      if FParselList.Count > 0 then
        TextEkle(FParselList);
    end;
    CmdLine.ActiveDrawBox.Selection.Clear;
  end;
  Self.Free;
end;

procedure TAddingTextToTheParcelLauncher.LauncherSuspendOperation(
  Sender: TObject);
begin
  CmdLine.ActiveDrawBox.NoPickFilter := FOldEntIDS;
end;

end.
