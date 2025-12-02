unit Lider.CG.Com.UcBImpl;

interface

uses
  Windows,
  Classes,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Base,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.ModulesInt;

type
  TlicgPrepare3DFile = class
    procedure Prepare3Dfile(filename: widestring; const TriangularLayer:
      TlicgBaseLayer; DEMFileNames: TStrings = nil);
  end;

implementation

uses
  SysUtils,
  NativeXML,
  Dialogs,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

{ Tb3D_Interface }
(*
procedure TlicgPrepare3DFile.Prepare3Dfile (filename: widestring;
  const TriangularLayer: TlicgBaseLayer; const ObjectLayers : TcxListView; DEMFileNames : TStrings = nil);
var
  i,j,g: integer;
  tri  : IBEntity;
  str,
  fn   : string;
  nxml : TNativeXml;
  pntNode,
  PartNode,
  LayerNode,
  ObjectNode,
  VertNode,
  TriNode,
  surfaceTinNode,
  surfaceDemNode : TXmlNode;
  Layer          : TlicgBaseLayer;
  ColumnName     : string;
  fno : integer;
begin


  if Length(filename)<=0 then begin
    with TSaveDialog.Create(nil) do begin
      DefaultExt := 'yc3D';
      Filter := 'Licad 3D dosyalarý(*'+ '.yc3D' +')|*.yc3D';
      Title :=  '3D dosya hazýrla';
      if Execute then
         fn := FileName;
      Free;
    end;

  end
  else
    fn := filename;

  if FileExists(fn) then
     DeleteFile(fn);

  nxml := TNativeXml.CreateName('Licad_3D_File');
  nxml.EncodingString := 'windows-1254';

  surfaceTinNode := nxml.Root.NodeNew('Surface');
  surfaceTinNode.AttributeAdd('Model', 'TIN');

  // Tin Model
  if Assigned(TriangularLayer) then begin
    TriangularLayer.first;
    Licad.ProgressStart('Üçgenler yazýlýyor...', 0, TriangularLayer.RecordCount);
    while not TriangularLayer.Eof do
    begin
      if not TriangularLayer.RecIsDeleted then begin
        if TriangularLayer.RecEntityID = idPolygon then begin
          tri := TriangularLayer.LoadEntityWithRecNo(TriangularLayer.Recno);
          if Assigned(tri) and (tri.Geometry.Points.Count=4) then begin
            Str := inttostr(TriangularLayer.Recno);
            TriNode := surfaceTinNode.NodeNew('Triangle');
            TriNode.AttributeAdd('ID', TriangularLayer.Recno);
            for i:=0 to tri.Geometry.points.Count-2 do begin
              VertNode := TriNode.NodeNew('Vertex');
              Str :=  FloatToStr(tri.Geometry.points.X[i]);
              VertNode.AttributeAdd('X', Str);
              Str :=  FloatToStr(tri.Geometry.points.Y[i]);
              VertNode.AttributeAdd('Y', Str);
              Str :=  FloatToStr(tri.Geometry.points.Z[i]);
              VertNode.AttributeAdd('Z', Str);
            end;
          end;
          tri := nil;
        end;
      end;
      TriangularLayer.Next;
      Licad.ProgressPosition(TriangularLayer.Recno);
    end;
    Licad.ProgressEnd;
  end;

  // Dem Model
  if Assigned(DEMFileNames) then begin
     surfaceDemNode := nxml.Root.NodeNew('Surface');
     surfaceDemNode.AttributeAdd('Model', 'DEM');
     for i:=0 to DEMFileNames.Count-1 do begin
       surfaceDemNode.NodeNew('File').AttributeAdd('FileName', DEMFileNames.Strings[i]);
     end;
  end;

  if Assigned(ObjectLayers) then begin
    for i:=0 to ObjectLayers.Items.Count-1 do begin
      Layer := TlicgBaseLayer(ObjectLayers.Items[i].Data);
      ColumnName := '';

      if ObjectLayers.Items[i].SubItems.Count>0 then
         ColumnName := ObjectLayers.Items[i].SubItems.Strings[0];
      if Assigned(Layer) and (ColumnName<>'') then begin
        LayerNode :=  nxml.Root.NodeNew('Layer');
        LayerNode.AttributeAdd('Name', Layer.DisplayName);

        Layer.first;
        Licad.ProgressStart('Objeler yazýlýyor...', 0, Layer.RecordCount);
        while not Layer.Eof do
        begin
          if not Layer.RecIsDeleted then begin
            if Layer.RecEntityID = idPolygon then begin
               tri := Layer.RecLoadEntity;
               if Assigned(tri) then begin
                 ObjectNode := LayerNode.NodeNew('Object');
                 ObjectNode.AttributeAdd('ID', Layer.Recno);
                 ObjectNode.AttributeAdd('PartCount', tri.Geometry.Points.PartCount);
                 ObjectNode.AttributeAdd('LineColor', GetRValue(tri.DrawTools.PenTool.PenStyle.Color));
                 str := FloatToStr(tri.DrawTools.PenTool.PenStyle.Width);
                 ObjectNode.AttributeAdd('LineWidth', str);
                 ObjectNode.AttributeAdd('BackColor', GetRValue(tri.DrawTools.BrushTool.BackColor));

                 if tri.Geometry.Points.PartCount<2 then begin
                   PartNode := ObjectNode.NodeNew('Part');
                   PartNode.AttributeAdd('Type', 1);

                   fno := Layer.DBTable.FieldNo(ColumnName) ;
                   if (fno<=Layer.DBTable.FieldCount) and (fNo>1) then
                   begin
                     Layer.DBTable.RecNo := Layer.Recno;
                     PartNode.AttributeAdd('H', Layer.DBTable.FieldGet(ColumnName))
                   end
                   else
                     PartNode.AttributeAdd('H', '0');

                   for j:=0 to tri.Geometry.Points.Count-2 do begin
                     pntNode := PartNode.NodeNew('Point');
                     str := FloatToStr(tri.Geometry.Points.X[j]);
                     pntNode.AttributeAdd('X', str);
                     str := FloatToStr(tri.Geometry.Points.Y[j]);
                     pntNode.AttributeAdd('Y', str);
                     str := FloatToStr(tri.Geometry.Points.Z[j]);
                     pntNode.AttributeAdd('Z', str);
                   end;
                 end
                 end;

               end;
            end;
          end;
          Layer.Next;
          Licad.ProgressPosition(Layer.Recno);
        end;
        Licad.ProgressEnd;

      end;
    end;
  end;


  nxml.SaveToFile(ChangeFileExt(fn, '.yc3d'));
  nxml.Free;

end;

*)

procedure TlicgPrepare3DFile.Prepare3Dfile(filename: widestring; const
  TriangularLayer: TlicgBaseLayer; DEMFileNames: TStrings = nil);
var
  i, j, g: integer;
  tri: IlicgEntity;
  str, fn: string;
  _File: TextFile;
  Layer: TlicgBaseLayer;
  ColumnName: string;
  fno: integer;
begin

  if Length(filename) <= 0 then
  begin
    with TSaveDialog.Create(nil) do
    begin
      DefaultExt := 'yc3D';
      Filter := 'Licad 3D dosyalarý(*' + '.yc3D' + ')|*.yc3D';
      Title := '3D dosya hazýrla';
      if Execute then
        fn := FileName;
      Free;
    end;

  end
  else
    fn := filename;

  if FileExists(fn) then
    DeleteFile(fn);

  AssignFile(_File, ChangeFileExt(fn, '.yc3d'));
  Rewrite(_File);
  try

    // SURFACE TEXTURE;D:\raster\G23C024.TIF;501200;4443000;502300;4444000

    // Tin Model
    if Assigned(TriangularLayer) then
    begin
      TriangularLayer.first;
      Licad.ProgressStart(TriangularLayer.RecordCount, 'Üçgenler yazýlýyor...');
      try
        while not TriangularLayer.Eof do
        begin
          if not TriangularLayer.RecIsDeleted then
          begin
            if TriangularLayer.RecEntityID = idPolygon then
            begin
              tri := TriangularLayer.LoadEntityWithRecNo(TriangularLayer.Recno);
              if Assigned(tri) and (tri.Geometry.Points.Count = 4) then
              begin
                Str := 'SurfaceModel;TIN;';
                Str := str + inttostr(TriangularLayer.Recno) + ';';
                for i := 0 to tri.Geometry.points.Count - 2 do
                begin
                  Str := str + floatToStrPoint(tri.Geometry.points.X[i]) + ';';
                  Str := str + floatToStrPoint(tri.Geometry.points.Y[i]) + ';';
                  Str := str + floatToStrPoint(tri.Geometry.points.Z[i]) + ';';
                end;
                Str := str + inttostr(GetRValue(tri.DrawTools.BrushTool.BackColor)) + ';';
                Str := str + inttostr(GetGValue(tri.DrawTools.BrushTool.BackColor)) + ';';
                Str := str + inttostr(GetBValue(tri.DrawTools.BrushTool.BackColor)) + ';';

                Writeln(_file, Str);

              end;
              tri := nil;
            end;

          end;

          TriangularLayer.Next;
          Licad.ProgressPosition(TriangularLayer.Recno);
        end;
      finally
        Licad.ProgressEnd;
      end;
    end;

   (*
    // Dem Model
    if Assigned(DEMFileNames) then begin
       surfaceDemNode := nxml.Root.NodeNew('Surface');
       surfaceDemNode.AttributeAdd('Model', 'DEM');
       for i:=0 to DEMFileNames.Count-1 do begin
         surfaceDemNode.NodeNew('File').AttributeAdd('FileName', DEMFileNames.Strings[i]);
       end;
    end;

    if Assigned(ObjectLayers) then begin
      for i:=0 to ObjectLayers.Items.Count-1 do begin
        Layer := TlicgBaseLayer(ObjectLayers.Items[i].Data);
        ColumnName := '';

        if ObjectLayers.Items[i].SubItems.Count>0 then
           ColumnName := ObjectLayers.Items[i].SubItems.Strings[0];
        if Assigned(Layer) and (ColumnName<>'') then begin
          LayerNode :=  nxml.Root.NodeNew('Layer');
          LayerNode.AttributeAdd('Name', Layer.DisplayName);

          Layer.first;
          Licad.ProgressStart('Objeler yazýlýyor...', 0);
          while not Layer.Eof do
          begin
            if not Layer.RecIsDeleted then begin
              if Layer.RecEntityID = idPolygon then begin
                 tri := Layer.RecLoadEntity;
                 if Assigned(tri) then begin
                   ObjectNode := LayerNode.NodeNew('Object');
                   ObjectNode.AttributeAdd('ID', Layer.Recno);
                   ObjectNode.AttributeAdd('PartCount', tri.Geometry.Points.PartCount);

                   ObjectNode.AttributeAdd('LineColor', GetRValue(tri.DrawTools.PenTool.PenStyle.Color));
                   str := FloatToStr(tri.DrawTools.PenTool.PenStyle.Width);
                   ObjectNode.AttributeAdd('LineWidth', str);
                   ObjectNode.AttributeAdd('BackColor', GetRValue(tri.DrawTools.BrushTool.BackColor));

                   if tri.Geometry.Points.PartCount<2 then begin
                     PartNode := ObjectNode.NodeNew('Part');
                     PartNode.AttributeAdd('Type', 1);

                     fno := Layer.DBTable.FieldNo(ColumnName) ;
                     if (fno<=Layer.DBTable.FieldCount) and (fNo>1) then
                     begin
                       Layer.DBTable.RecNo := Layer.Recno;
                       PartNode.AttributeAdd('H', Layer.DBTable.FieldGet(ColumnName))
                     end
                     else
                       PartNode.AttributeAdd('H', '0');

                     for j:=0 to tri.Geometry.Points.Count-2 do begin
                       pntNode := PartNode.NodeNew('Point');
                       str := FloatToStr(tri.Geometry.Points.X[j]);
                       pntNode.AttributeAdd('X', str);
                       str := FloatToStr(tri.Geometry.Points.Y[j]);
                       pntNode.AttributeAdd('Y', str);
                       str := FloatToStr(tri.Geometry.Points.Z[j]);
                       pntNode.AttributeAdd('Z', str);
                     end;
                   end

                 end;
              end;
            end;
            Layer.Next;
            Licad.ProgressPosition(Layer.Recno);
          end;
          Licad.ProgressEnd;

        end;
      end;
    end;


    nxml.SaveToFile(ChangeFileExt(fn, '.yc3d'));
    nxml.Free;
   *)

  finally
    CloseFile(_File);
  end;

end;

end.


