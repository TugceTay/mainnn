unit Lider.CG.Com.Utilities;

interface

uses
  Windows, Messages, Controls, Classes, Graphics, Math, Forms, SysUtils,
  Lider.CG.Com.Lists,
  Lider.CG.Com.Lib,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.Math,
  Lider.CG.Com.Base,
  Lider.CG.Com.Consts,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.ListsInt,
  Lider.CG.Com.Rtree,
  Lider.CG.Com.Types,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.DrawToolsInt;

function CreateTextEntity(const ACmdLine: TlicgBaseCmdLine; const ACoor: TlicgCoor; const AText: string; const ACharWidthFactor: Double = 1;
  const ACharSpacing: Double = 0; const AFontTool: IlicgFontTool = nil): IlicgEntity; // ilker ekleme

function VectorFill(patFileName, hatchName: string; hatchEntList, nohatchList:
  IlicgEntityList; hatchColor: tcolor; hatchAngle, hatchScale: Double;
  IsShowProgress: Boolean = false): IlicgEntityList; stdcall;

function CalculateZ(ACmdLine: TLicgBaseCmdLine; ACoor: TLicgCoor): Double;
function GetPenStyle(ACmdLine: TLicgBaseCmdLine; ALayer: TlicgBaseLayer): Integer;

//function CalculateZAbdullah(ACmdLine: TLicgBaseCmdLine; ACoor: TLicgCoor): Double;

(* ilker kullanýlmýyor ileride silersin 18.08.2023
function MultiDeleteRepetitions(LayerList: TList; DrawBox: TlicgBaseDrawBox; phandle: THandle): boolean; *)
//procedure FontStyle(const Cmdline: TlicgBaseCmdLine); stdcall;

function GetLayerVisibleDisplayNames(const ACmdLine: TlicgBaseCmdline = nil): TArray<string>; // ilker ekleme 11.09.2025
function GetLayerDisplayNames(const ACmdLine: TlicgBaseCmdline = nil): TArray<string>; // ilker ekleme 11.09.2025

implementation

function CreateTextEntity(const ACmdLine: TlicgBaseCmdLine; const ACoor: TlicgCoor; const AText: string; const ACharWidthFactor: Double = 1;
  const ACharSpacing: Double = 0; const AFontTool: IlicgFontTool = nil): IlicgEntity;
begin
  if ACmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1' then
  begin
    Result := Licad.CreateEntityFactory.MakeVectorialText(ACoor, TrimCrLf(AText), AFontTool);
  end
  else
  begin
    Result := Licad.CreateEntityFactory.MakeText(ACoor, TrimCrLf(AText), AFontTool);
  end;
  Result.DrawTools.FontTool.Name := ACmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
  Result.DrawTools.FontTool.CharWidthFactor := ACharWidthFactor;
  Result.DrawTools.FontTool.CharSpacing := ACharSpacing;
end;

procedure DeleteRepetitions(const DrawBox: TlicgBaseDrawBox; const aGis:
  TlicgBaseGis; const LayerName: string; EntityIDs: TlicgEntityIDs;
  SearchAllLayers: Boolean; isDirection: Boolean; DecimalCnt: integer; isCoor,
  isName, isStyle: boolean; phandle: THandle);
var
  I, J, L, FromIndex, ToIndex, TmpRecno: Integer;
  Layer, TestLayer: TlicgBaseLayer;
  Entity, TestEntity: IlicgEntity;
  RecCount: Integer;
  LayerList: TList;
  LayerIndex: Integer;
  records: IlicgIntegerList;
  ok: boolean;
  R: TlicgExtent;
begin
  if (aGis = nil) or (aGis.Layers.Count = 0) or (Length(LayerName) = 0) then
    Exit;
  Layer := aGis.Layers.LayerByName(LayerName);
  if Layer = nil then
    Exit;
  if SearchAllLayers then
  begin
    FromIndex := 0;
    ToIndex := aGis.Layers.Count - 1;
  end
  else
  begin
    FromIndex := Layer.Index;
    ToIndex := FromIndex;
  end;

  LayerList := TList.Create;
  records := TlicgIntegerList.Create;

  RecCount := Layer.RecordCount;

  if RecCount > 0 then
    Licad.ProgressStart(RecCount, ExtractFileName(LayerName) + ' katmanýndaki nesneler seçiliyor...');
  try
    Layer.CancelScope; // just for safety
    //n := 0;
    Layer.First;
    while not Layer.Eof do
    begin
      if Layer.RecIsDeleted then
      begin
        Layer.Next;
        Continue;
      end;
      records.Add(Layer.RecNo);
      Layer.Next;
    end;

    for I := 0 to records.Count - 1 do
    begin
      Licad.ProgressPosition(I);
      TmpRecno := records[I];

      if not DrawBox.Selection.IsSelected(Layer, TmpRecno) then
      begin
        Entity := Layer.LoadEntityWithRecNo(TmpRecno);
        try
          for L := FromIndex to ToIndex do
          begin

            TestLayer := aGis.Layers[L];

            LayerIndex := LayerList.IndexOf(TestLayer);
            if LayerIndex < 0 then
            begin
              LayerList.Add(TestLayer);
            end;

            R := Entity.Geometry.Extent;
            if aGis.MapInfo.CS.IsGeographicCS then
              InflateExtent(R, MeterToDeg(0.001), MeterToDeg(0.001))
            else
              InflateExtent(R, 0.001, 0.001);

            TestLayer.SetGraphicFilter(stOverlap, R);
            try
              TestLayer.First;
              while not TestLayer.Eof do
              begin

                if not (TestLayer.RecIsDeleted or (TestLayer.Recno = TmpRecno)
                  or DrawBox.Selection.IsSelected(TestLayer, TestLayer.Recno)) then
                begin

                  testentity := testlayer.recloadentity;
                  try

                    ok := (entity.entityid in entityids) and (entity.entityid =
                      testentity.entityid) and (isCoor or isName or isStyle);

                    if ok then
                    begin

                      if isCoor then
                        ok := entity.geometry.points.isequal(testentity.geometry.points,
                          isdirection);

                      if ok and isName then
                        ok := (entity.Name = TestEntity.Name);

                      if ok and isStyle then
                        ok :=
(
((Entity.EntityID = idPoint) and (Entity.DrawTools.PenTool.Color = TestEntity.DrawTools.PenTool.Color))
 or
((Entity.EntityID in [idPlace, idBlockInsert ]) and (Entity.DrawTools.SymbolTool.isEqual(TestEntity.DrawTools.SymbolTool)))
 or
((Entity.EntityID in TextEntityIDs) and (Entity.DrawTools.FontTool.isEqual(TestEntity.DrawTools.FontTool)))
 or
((Entity.IsPolyline) and (Entity.DrawTools.PenTool.isEqual(TestEntity.DrawTools.PenTool)))
 or ((Entity.IsClosed) and ((Entity.DrawTools.PenTool.isEqual(TestEntity.DrawTools.PenTool))
   and (Entity.DrawTools.BrushTool.isEqual(TestEntity.DrawTools.BrushTool)))));

                    end;

                    if ok then
                      drawbox.selection.add(testlayer, testlayer.Recno);

                  finally
                    testentity := nil;
                  end;
                end;

                TestLayer.Next;

              end;

            finally
              TestLayer.CancelScope;
            end;
          end;
        finally
          Entity := nil;
        end;

      end;

    end;

    {
    for I := 0 to LayerList.Count - 1 do
    begin
      TestLayer := TlicgBaseLayer(LayerList[I]);
      DelList := IBIntegerList(RecordsList[I]);
      for J := 0 to DelList.Count - 1 do
      begin
        DrawBox.Selection.Add(TestLayer, DelList[J]);
         //TestLayer.DeleteEntity(DelList[J]);
      end;
    end;
    }

  finally
    LayerList.Free;
    if RecCount > 0 then
      Licad.ProgressEnd;
    Records := nil;
  end;
end;

function CutOutLineEntityInPolygonV2(L: IlicgEntity; P: IlicgEntityList): IlicgEntityList;
var
  resultV, resultV2: IlicgVector;
  isIntersect, isMidPointInside, isInside: Boolean;
  M, iNoAreas, iiNoAreas: Integer;
  Ent: IlicgEntity;
  finPoly: IlicgEntity;
begin
  result := TlicgEntityList.Create;
  resultV2 := Licad.CreateEntityFactory.MakeVector(3, 0);
  resultV := Licad.CreateEntityFactory.MakeVector(3, 0);
  try
    //L hatch çizim nesnesidir. (nokta yada hat)
    //P taranmayacak nesneleri içeren listedir.
    //Aþaðýda çizim nesnesinin listedeki tüm nesnelerle kesiþimleri bulunuyor ve kesiþim noktalarý
    //çizim nesnesþinin noktalarý ile birlikte (resultV2.Add(L.Geometry.Points[0]), resultV2.Add(L.Geometry.Points[1]))
    //kesiþim noktalarý resultV2 vektörüne eklenir ve L[0] noktasýndan baþlamak üzere sort edilir.
    if not (L.EntityID = idPoint) then
    begin
      resultV2.Add(L.Geometry.Points[0].x, L.Geometry.Points[0].y);
      resultV2.Add(L.Geometry.Points[1].x, L.Geometry.Points[1].y);
      for iNoAreas := 0 to P.Count - 1 do
      begin
        begin
          finPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);
          begin
            Ent := P.items[iNoAreas];
            finPoly.Geometry.Points.Assign(Ent.Geometry.DrawPoints);

          end;
          isIntersect := VectIntersect(L.Geometry.Points, finPoly.Geometry.DrawPoints,
            resultV, true);
          for M := 0 to resultV.Count - 1 do
          begin
            resultV2.Add(resultV[M].x, resultV[M].y);
          end;
          finPoly := nil;
        end; // For iiNoAreas
      end; //For iNoAreas:=0
      sortIntersectPoints(L.Geometry.Points[0], resultV2);
    end; //if (L.EntityID <> idPoint) then

    //Sýralanmýþ noktalar (resultV2) ile hat oluþturulur ( resultV2[M],resultV2[M+1] ) ve
    //oluþan hattýn merkezi tüm taranmayacak nesneler ile kontrol edilerek
    //taranmayacak nesnenin merkez noktayý içerip içermediðine bakýlýr. Merkez nokta herhangi bir
    //nesnenin içine düþerse bu hatch nesne çizilmeyecektir.
    if resultV2.count > 2 then
    begin
      for M := 0 to resultV2.Count - 2 do
      begin
        isMidPointInside := False;
        for iNoAreas := 0 to P.Count - 1 do
        begin
            //For iiNoAreas:=0 to P.items[iNoAreas].SelList.count-1 do
          begin
            finPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);
            begin
              Ent := P.items[iNoAreas];
              finPoly.Geometry.Points.Assign(Ent.Geometry.DrawPoints);

            end;
            if isMidPointInsideEntity(finPoly, resultV2[M], resultV2[M + 1]) then
            begin
              isMidPointInside := True;
              break;
            end; //if hatchEnt.isMidPointInsideEntity

            finPoly := nil;
          end; // For iiNoAreas
          if isMidPointInside then
            break;
        end; //For iNoAreas:=0
        if not isMidPointInside then //hatch nesnesi taranmayacak alan dýþýnda ise çizim listesine eklenir.
        begin
          Ent := Licad.CreateEntityFactory.MakePolyline([resultV2[M], resultV2[M + 1]]);
          result.Add(Ent.Clone);
          Ent := nil;
        end;

      end; //For m

    end  // if resultV2.count>2
    else
    begin
          //Aþaðýda kesiþimi bulunmayan nesneler için taranmayacak tüm nesneler ile kontrol edilerek
          //taranmayacak nesnelerin hacth nesneyi içerip içermediðine bakýlýr. Eðer herhangi bir nesne
          //içeriyorsa hatch nesne çizilmez.
      for iNoAreas := 0 to P.Count - 1 do
      begin
        isInside := False;
        begin
          finPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);
          begin
            Ent := P.items[iNoAreas];
            finPoly.Geometry.Points.Assign(Ent.Geometry.DrawPoints);

          end;
          if L.IsInsideEntity(finPoly, True) then
          begin
            isInside := True;
            break;
          end;
          finPoly := nil;
        end;
        if isInside then
          break;
      end;
      if not isInside then
        result.Add(L.CLone);
    end; // if resultV2.count>2 ELSE
  finally
    resultV2 := nil;
    resultV := nil;
  end; //try
end; //CutOutLineEntityInPolygonV2

function DrawVectHatchVector(oCmdLine: TlicgBaseCmdLine; vsPatFileName,
  vsHatchName: string; oAreaList, oNoHatchList: IlicgEntityList; hatchColor:
  TColor; hatchAngle, hatchScale: Double; IsShowProgess: Boolean): IlicgEntityList;
var
  viHatchItem, i, j, X, M, iAreas, iiAreas, iNoAreas, iiNoAreas, RC, II: integer;
  H: TBaseHatch;
  D, W: Double;
  FPoly, L, FinPoly, Ent: IlicgEntity;
  tmpRect: TlicgExtent;
  ptC: TlicgCoor;
  TmpV3: IlicgVector;
  isAddEnt: Boolean;
  isInside, isVect: Boolean;
  eList: IlicgEntityList;
  TempFieldList: TStringList;
  TmpEnt: IlicgEntity;
  LV, PV, LineVector, PointVector: IlicgVector;
  rotMatrix: TlicgMatrix;
  si, sj: integer;
  cent1: TlicgCoor;
  cent2: TlicgCoor;
  HatchGroup: IlicgEntity;
  ProgCount: integer;
begin
  result := nil;

  if oAreaList.Count = 0 then
    Exit;

  Licad.Hatches.AddPATFile(vsPatFileName);

  viHatchItem := -1;

  for i := 0 to Licad.Hatches.Count - 1 do
  begin
    if vsHatchName = Licad.Hatches.Items[i].Name then
    begin
      viHatchItem := i;
      Break;
    end;
  end;

  if viHatchItem < 0 then
    Exit;

  H := Licad.Hatches.Items[viHatchItem];

  Result := TlicgEntityList.Create;

  try
    if IsShowProgess then
      Licad.ProgressStart(oAreaList.Count - 1, 'Tarama Nesnesi Oluþturuluyor');

    for iAreas := 0 to oAreaList.Count - 1 do
    begin
      if IsShowProgess then
        Licad.ProgressPosition(iAreas);

      FPoly := Licad.CreateEntityFactory.MakeEntity(idPolygon, 0, _2D);
      Ent := oAreaList.items[iAreas];
      FPoly.Geometry.Points.Assign(Ent.Geometry.DrawPoints);

      ptC := FPoly.Centroid;
      tmpRect := FPoly.Geometry.Extent;

      if hatchAngle > 0 then
      begin
        FPoly.Geometry.SetTransformMatrix(Scale2D(2, 2, ptC));
        FPoly.Geometry.ApplyTransform;
        FPoly.Geometry.SetTransformMatrix(Rotate2D((hatchAngle), ptC));
        FPoly.Geometry.ApplyTransform;
        MaxBound(tmpRect.UpperRight, FPoly.Geometry.Extent.UpperRight);
        MinBound(tmpRect.LowerLeft, FPoly.Geometry.Extent.LowerLeft);
        FPoly.Geometry.Points.Clear;
        FPoly := nil;

        FPoly := Licad.CreateEntityFactory.MakePolygon([tmpRect.LowerLeft,
          AsCoor(tmpRect.UpperRight.x, tmpRect.LowerLeft.y), tmpRect.UpperRight,
          AsCoor(tmpRect.LowerLeft.x, tmpRect.UpperRight.y), tmpRect.LowerLeft]);
      end;

      LineVector := Licad.CreateEntityFactory.MakeVector(3, 500000);
      PointVector := Licad.CreateEntityFactory.MakeVector(3, 500000);

      H.getLineVector(oCmdLine, FPoly, hatchScale, hatchAngle, LineVector,
        PointVector, IDENTITY_MATRIX2D, clGray, False);

      FPoly.Geometry.Points.Clear;
      Ent := oAreaList.items[iAreas];
      FPoly.Geometry.Points.Assign(Ent.Geometry.DrawPoints);

      HatchGroup := Licad.CreateEntityFactory.MakeGroupEntity;

      try

        AslicgGroupGeometry(HatchGroup.Geometry).setGroupType(gtHatch);

        if (LineVector.Count > 0) or (PointVector.Count > 0) then
        begin
          TmpEnt := Licad.CreateEntityFactory.MakePolyline([]);
          TmpEnt.DrawTools.PenTool.Color := hatchColor;
          TmpEnt.Geometry.Points.Size := LineVector.Count + (PointVector.Count * 2);
          AslicgGroupGeometry(HatchGroup.Geometry).AddEntity(TmpEnt);
        end;

        ProgCount := 0;
        try

          if hatchAngle <> 0 then
          begin
            rotMatrix := Rotate2D((hatchAngle), ptC);
            for i := 0 to LineVector.Count - 1 do
            begin
              LineVector[i] := TransformPoint2D(LineVector[i], rotMatrix);
            end;
          end;

          TmpV3 := Licad.CreateEntityFactory.MakeVector(3, 0);
          LV := Licad.CreateEntityFactory.MakeVector(3, 2);

          i := -2;
          while i <= LineVector.Count - 3 do
          begin
            inc(i, 2);
            LV[0] := LineVector[i];
            LV[1] := LineVector[i + 1];

            TmpEnt := Licad.CreateEntityFactory.MakePolyline([]);
            TmpEnt.Geometry.Points.Assign(LV);

            TmpV3.Clear;

            try

              isVect := VectIntersect(FPoly.Geometry.Points, LV, TmpV3, true);

              try

                isAddEnt := False;

                if isVect then
                begin

                  if OnPoly(FPoly, LV[0]) or FPoly.InPoly(LV[0]) then
                    TmpV3.Add(LV[0].x, LV[0].y);

                  if OnPoly(FPoly, LV[1]) or FPoly.InPoly(LV[1]) then
                    TmpV3.Add(LV[1].x, LV[1].y);

                  isAddEnt := True;

                  sortIntersectPoints(LV[0], TmpV3);

                  for M := 0 to TmpV3.Count - 2 do
                  begin
                              //Çizilecek doðrunun orta noktasý alanýn içinde mi ?
                              //Bu complex þekillerde gerekiyor.
                    isAddEnt := False;
                    if isMidPointInsideEntity(FPoly, TmpV3[M], TmpV3[M + 1]) then
                    begin
                      if (oNoHatchList = nil) or (oNoHatchList.Count = 0) then
                      begin
                        AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                          (0)).Geometry.Points.Add(TmpV3[M].x, TmpV3[M].y);
                        AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                          (0)).Geometry.Points.Add(TmpV3[M + 1].x, TmpV3[M + 1].y);
                      end
                      else
                      begin
                        L := Licad.CreateEntityFactory.MakePolyline([TmpV3[M],
                          TmpV3[M + 1]]);
                        elist := CutOutLineEntityInPolygonV2(L, oNoHatchList);
                        for II := 0 to eList.Count - 1 do
                        begin
                          for j := 0 to eList.Items[II].Geometry.Points.Count - 1 do
                            AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                              (0)).Geometry.Points.Add(eList.Items[II].Geometry.Points
                              [j].x, eList.Items[II].Geometry.Points[j].y);
                        end;
                        L := nil;
                        eList := nil;
                      end;
                    end;
                  end; //For m
                end
                else
                begin
                  isAddEnt := FPoly.InPoly(LV[0]) and FPoly.InPoly(LV[1]);
                end;
              finally

                if isAddEnt then
                begin
                  if (oNoHatchList <> nil) and (oNoHatchList.Count > 0) then
                  begin
                    if TmpEnt.IsInsideEntity(FPoly, false) then
                    begin
                      begin
                        elist := CutOutLineEntityInPolygonV2((TmpEnt), oNoHatchList);

                        try
                          for II := 0 to eList.Count - 1 do
                          begin
                            for j := 0 to eList.Items[II].Geometry.Points.Count - 1 do
                              AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                                (0)).Geometry.Points.Add(eList.Items[II].Geometry.Points
                                [j].x, eList.Items[II].Geometry.Points[j].y);
                          end;
                        finally
                          eList := nil;
                        end;
                      end;
                    end;
                  end
                  else
                  begin
                    for j := 0 to TmpEnt.Geometry.Points.Count - 1 do
                      AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                        (0)).Geometry.Points.Add(TmpEnt.Geometry.Points[j].x,
                        TmpEnt.Geometry.Points[j].y);
                  end
                end;
              end;

            finally
              TmpEnt := nil;
            end;

          end;

        finally
          TmpV3 := nil;
          LV := nil;
        end;
        PV := Licad.CreateEntityFactory.MakeVector(3, 1);
        try

          for i := 0 to PointVector.Count - 1 do
          begin

            PV[0] := PointVector[i];
            try
              isInside := FPoly.InPoly(PV[0]);

              try
                isAddEnt := False;
                if isInside then
                begin
                  isAddEnt := true;
                  if not OnPoly(FPoly, PV[0]) then
                  begin
                    if oNoHatchList <> nil then
                    begin
                      for si := 0 to oNoHatchList.Count - 1 do
                      begin
                        begin
                          TmpEnt := oNoHatchList[si];
                          try
                            if TmpEnt.InPoly(PV[0]) then
                            begin
                              isAddEnt := false;
                              break;
                            end;
                          finally
                          end;
                        end;

                        if not isAddEnt then
                          BREAK;

                      end;

                    end;

                    if isAddEnt then
                    begin
                      AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                        (0)).Geometry.Points.Add(PV[0].x, PV[0].y);
                      AslicgEntity(AslicgGroupGeometry(HatchGroup.Geometry).Entity
                        (0)).Geometry.Points.Add(PV[0].x, PV[0].y);
                    end;

                  end;

                end;
              finally
              end;
            finally
            end;
          end;
        finally
          PV := nil;
        end;
      finally
        LineVector := nil;
        PointVector := nil;
        FPoly := nil;
        if AslicgGroupGeometry(HatchGroup.Geometry).NumEntities > 0 then
        begin
          HatchGroup.Geometry.UpdateExtension;
          result.Add(HatchGroup);
        end
        else
          HatchGroup := nil;
      end;
    end;
  finally
    if IsShowProgess then
      Licad.ProgressEnd;
  end;
end;

function VectorFill(patFileName, hatchName: string; hatchEntList, nohatchList:
  IlicgEntityList; hatchColor: tcolor; hatchAngle, hatchScale: Double;
  IsShowProgress: Boolean = false): IlicgEntityList; stdcall;
begin
  result := DRawVectHatchVector(CurrCmdLine, patFileName, hatchName,
    hatchEntList, nohatchList, hatchColor, hatchAngle, hatchScale, IsShowProgress);
end;

{Davulcu Ekleme Baþlangýç}

function GetPenStyle(ACmdLine: TLicgBaseCmdLine;
  ALayer: TlicgBaseLayer): Integer;
begin
  if ACmdLine.ActiveDrawBox.GIS.MapInfo.PenTool.Style = 0 then
    Result := ALayer.LayerInfo.DefPenTool.Style
  else
    Result := ACmdLine.ActiveDrawBox.GIS.MapInfo.PenTool.Style;
end;

function CalculateZAbdullah(CmdLine: TLicgBaseCmdLine; P: TLicgCoor): Double;
var
  StackedSelList: IPickedList;
  Picked: Boolean;
  TempLayer: TLicgBaseLayer;
  TempRc, PickedPoint, I, J, PCode: Integer;
  Ent: ILicgEntity;
  PixDist, Dist, MinDist: Double;
  NearPIndex, FoundIndex: Integer;
  OldEntIDS: TlicgEntityIDs;
begin
  Result := 0;
  try
    // Önce Nokta nesnesi kontrol edilyor
    StackedSelList := TPickedList.Create;
    OldEntIDS := CmdLine.ActiveDrawBox.NoPickFilter;//Filtreler Temizleniyor
    CmdLine.ActiveDrawBox.NoPickFilter := [];
//    PixDist := CmdLine.ActiveDrawBox.Grapher.PointsToDistX(Licad.Settings.ApertureWidth) /
//      Licad.Settings.Dimension.PenWidth / 2.5;  Davulcu Deðiþtirme
    PixDist := Licad.Settings.ApertureWidth / 2;
    Picked := CmdLine.ActiveDrawBox.PickEntity(P.X, P.Y,  Licad.Settings.ApertureWidth,
     '', TempLayer, TempRc, PickedPoint, StackedSelList);
    MinDist := 99999;
    FoundIndex := -1;
    for I := 0 to StackedSelList.Count - 1 do
    begin
      TempRc := Longint(StackedSelList.PickedItems[I].Pick.RecNo);
      Ent := StackedSelList.PickedItems[I].Pick.Layer.LoadEntityWithRecNo(TempRc);
      if Ent = nil then
       Continue;
      if not (Ent.EntityID in [idPoint]) then
       Continue;
      if Lider.CG.Com.Math.IsSamePoint2D(P, Ent.Geometry.Points[0]) then  //Davulcu Ekleme
      begin
        FoundIndex := I;
        Break;
      end;
      (*PCode := Ent.Geometry.PointCode(P, PixDist , Dist, True);  //Davulcu Silme

      if PCode > -1 then
        if Dist < MinDist then
        begin
          MinDist := Dist;
          FoundIndex := I;
        end;  *)
    end;
    if FoundIndex <> -1 then
    begin
      TempRc := Longint(StackedSelList.PickedItems[FoundIndex].Pick.RecNo);
      Ent := StackedSelList.PickedItems[FoundIndex].Pick.Layer.LoadEntityWithRecNo(TempRc);
      Result := Ent.AsPoint.Points.Z[0];
    end;

    if Result <> 0 then
    begin
      CmdLine.ActiveDrawBox.NoPickFilter := OldEntIDS;
      Exit;
    end;
    MinDist := 99999;
    NearPIndex := -1;
    FoundIndex := -1;
    // Sonra Çizgi, Çoklu Doðru ve Kapalý alan nesnesi kontrol edilyor
    for I := 0 to StackedSelList.Count - 1 do
    begin
      TempRc := Longint(StackedSelList.PickedItems[I].Pick.RecNo);
      Ent := StackedSelList.PickedItems[I].Pick.Layer.LoadEntityWithRecNo(TempRc);
      if Ent = nil then
        Continue;
      if Not (Ent.EntityID in AllEntityIDs - [idPoint]) then
        Continue;
      for J := 0 to Ent.Geometry.Points.Count - 1 do
      begin
        if Lider.CG.Com.Math.IsSamePoint2D(P, Ent.Geometry.Points[J]) then  //Davulcu Ekleme
        begin
          if Ent.Geometry.Points.Z[J] <> 0 then
          begin
            FoundIndex := I;
            NearPIndex := J;
            Break;
          end;
        end;
      end;
      (*PCode := Ent.Geometry.PointCode(P, PixDist , Dist, True);  //Davulcu Silme
      if PCode > -1 then
        if Dist < MinDist then
        begin
          MinDist := Dist;
          FoundIndex := I;
          NearPIndex := PCode;
        end;*)
    end;

    if FoundIndex <> -1 then
    begin
      TempRc := Longint(StackedSelList.PickedItems[FoundIndex].Pick.RecNo);
      Ent := StackedSelList.PickedItems[FoundIndex].Pick.Layer.LoadEntityWithRecNo(TempRc);
      Result := Ent.Geometry.Points.Z[NearPIndex];
    end;

    if Result = 0 then
      Result := CmdLine.ActiveDrawBox.CalculateElevation(P.X, P.Y);
    finally
    begin
      Ent := nil;
      StackedSelList := nil;
      CmdLine.ActiveDrawBox.NoPickFilter := OldEntIDS;
//    TempLayer.Free;
    end;
  end;
end;

{Davulcu Ekleme Bitiþ}

/// <summary>
///   Verilen Koordinatda Kot varsa alýr yoksa Genel Ayarlardaki Nokta sekmesindeki,
///   Kotu Üçgen Modelden Hesapla ve Kotu Noktalardan hesaplama seçeneklerini
///   dikkate alarak hesaplar.
/// </summary>
/// <param name="ACmdLine">
///   CmdLine seçimi
/// </param>
/// <param name="ACoor">
///   Z deðeri hesaplanacak koordinat
/// </param>
function CalculateZ(ACmdLine: TLicgBaseCmdLine; ACoor: TLicgCoor): Double;
var
  AEntity: IlicgEntity;
  AEntityList: IlicgEntityList;
begin
  Result := 0;
  try
    // Ekrandaki Görünür Katmanlarda iþlem yapýalacak.
    AEntityList := ACmdLine.ActiveDrawBox.GIS.SetFilterEntityList(PointToExtent(ACoor), GetLayerVisibleDisplayNames, []);
    // Önce Nokta nesnelerini kontrol et.
    for var I := 0 to AEntityList.Count - 1 do
    begin
      AEntity := AEntityList.Items[I];
      if AEntity.EntityID = idPoint then
      begin
        if SamePoint(ACoor, AEntity.Geometry.Points.Coors2D[0]) then
        begin
          Result := AEntity.Geometry.Points.Coors3D[0].Z;
          if Result <> 0 then
            Exit;
        end;
      end;
    end;

    // Sonra Nokta dýþýndaki nesnelerini kontrol et.
    for var I := 0 to AEntityList.Count - 1 do
    begin
      AEntity := AEntityList.Items[I];
      if AEntity.EntityID = idPoint then
        Continue;

      for var J := 0 to AEntity.Geometry.Points.Count - 1 do
      begin
        if SamePoint(ACoor, AEntity.Geometry.Points.Coors2D[J]) then
        begin
          Result := AEntity.Geometry.Points.Coors3D[J].Z;
          if Result <> 0 then
            Exit;
          end;
      end;
    end;
    // Diðer Yöntemden Hesapla
    Result := ACmdLine.ActiveDrawBox.CalculateElevation(ACoor.X, ACoor.Y);
  finally
    AEntity := nil;
    AEntityList := nil;
  end;
end;

function GetLayerVisibleDisplayNames(const ACmdLine: TlicgBaseCmdline = nil): TArray<string>; // ilker ekleme 11.09.2025
var
  AList: TStrings;
  ALayers: TlicgBaseLayers;
begin
  AList := TStringList.Create;
  try
    AList.BeginUpdate;
    if ACmdLine = nil then
      ALayers := CurrCmdLine.ActiveDrawBox.GIS.Layers
    else
      ALayers := ACmdLine.ActiveDrawBox.GIS.Layers;

    if ALayers = nil then
      Exit;
    for var I := 0 to ALayers.Count - 1 do
    begin
      if ALayers[I].LayerInfo.Visible then
        AList.Add(ALayers[I].DisplayName);
    end;
    AList.EndUpdate;
    Result := AList.ToStringArray;
  finally
    AList.Free;
  end;
end;

function GetLayerDisplayNames(const ACmdLine: TlicgBaseCmdline = nil): TArray<string>; // ilker ekleme 11.09.2025
var
  AList: TStrings;
  ALayers: TlicgBaseLayers;
begin
  AList := TStringList.Create;
  try
    AList.BeginUpdate;
    if ACmdLine = nil then
      ALayers := CurrCmdLine.ActiveDrawBox.GIS.Layers
    else
      ALayers := ACmdLine.ActiveDrawBox.GIS.Layers;

    if ALayers = nil then
      Exit;
    for var I := 0 to ALayers.Count - 1 do
    begin
      AList.Add(ALayers[I].DisplayName);
    end;
    AList.EndUpdate;
    Result := AList.ToStringArray;
  finally
    AList.Free;
  end;
end;

end.


