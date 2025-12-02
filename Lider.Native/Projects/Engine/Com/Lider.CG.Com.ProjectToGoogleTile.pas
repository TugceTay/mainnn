unit Lider.CG.Com.ProjectToGoogleTile;

interface

uses
  Windows,
  Dialogs,
  Forms,
  Classes,
  Graphics,
  SysUtils,
  ExtCtrls,
  Controls,
  Math,
  PngImage,
  NativeXml,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Grapher,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.Base,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Graphics,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.ReferenceInt,
  Lider.CG.Com.CSInt,
  Lider.CG.Com.VectorInt;

type
  TlatLng = record
    Lat, Lng: double;
  end;

  TGoogleTile = class
  end;

function CalcZoomFactor(lat1, lon1, Lat2, lon2: double; px, py: integer): integer;

procedure ProjectGoogleTile(const dbox: TlicgBaseDrawBox; const aGis:
  TlicgBaseGis; const Grapher: TlicgBaseGrapher);

implementation

uses
  Types,
  Variants,
  lxSysUtil,
  Lider.CG.Com.GoogleTileSettings;

//const
  ///TILE_SIZE = 256;

function bound(value, opt_min, opt_max: double): double;
begin
  if (opt_min <> null) then
    value := Math.max(value, opt_min);
  if (opt_max <> null) then
    value := Math.min(value, opt_max);
  result := value;
end;

function degreesToRadians(deg: double): double;
begin
  result := deg * (PI / 180);
end;

function radiansToDegrees(rad: double): double;
begin
  result := rad / (PI / 180);
end;

var
  DegreesToRadiansRatio: double;
  RadiansToDegreesRatio: double;
  XPixelsToDegreesRatio: double;
  YPixelsToRadiansRatio: double;
  pixelGlobeSize: integer;
  PixelGlobeCenter: TPoint;
  halfPixelGlobeSize: integer;

const
  EARTH_RADIUS = 6378137;
  EQUATOR_CIRCUMFERENCE = 2 * pi * EARTH_RADIUS;
  INITIAL_RESOLUTION = EQUATOR_CIRCUMFERENCE / 256.0;
  ORIGIN_SHIFT = EQUATOR_CIRCUMFERENCE / 2.0;
  MAX_ZOOM = 19;

var
  pixel_range: array[0..MAX_ZOOM] of integer;

var
  Zoom_scale: array[1..20] of double = (1128.497220, 2256.994440, 4513.988880,
    9027.977761, 18055.955520, 36111.911040, 72223.822090, 144447.644200,
    288895.288400, 577790.576700, 1155581.153000, 2311162.307000, 4622324.614000,
    9244649.227000, 18489298.450000, 36978596.910000, 73957193.820000,
    147914387.600000, 295828775.300000, 591657550.500000);

function _latlontopixels(lat, lon, _zoom: double): TPoint;
var
  mx, my, res: double;
  z: double;
begin
  try
    z := _zoom;

    mx := (lon * ORIGIN_SHIFT) / 180.0;
    my := LN(tan((90 + lat) * pi / 360.0)) / (pi / 180.0);
    my := (my * ORIGIN_SHIFT) / 180.0;
    res := INITIAL_RESOLUTION / power(2, z);
    Result.x := round((mx + ORIGIN_SHIFT) / res);
    Result.y := round((my + ORIGIN_SHIFT) / res);
  except
  end;
end;

procedure _pixelstolatlon(px, py, _zoom: double; var lat, lon: double);
var
  mx, my, res, z: double;
begin

  try

    z := _zoom;

    res := INITIAL_RESOLUTION / power(2, z);
    mx := px * res - ORIGIN_SHIFT;
    my := py * res - ORIGIN_SHIFT;
    lat := (my / ORIGIN_SHIFT) * 180.0;
    lat := 180 / pi * (2 * ArcTan(exp(lat * pi / 180.0)) - pi / 2.0);
    lon := (mx / ORIGIN_SHIFT) * 180.0;
  except
  end;

end;

function CalcWrapWidth(_zoom: integer): integer;
begin
  Result := pixel_range[_zoom];
end;

function CalcZoomFactor(lat1, lon1, Lat2, lon2: double; px, py: integer): integer;
var
  z: integer;
  top_right_pixel, bottom_left_pixel: TPoint;
begin
  Result := 0;
  for z := 0 to MAX_ZOOM do
  begin

    bottom_left_pixel := _latlontopixels(lat1, lon1, z);
    top_right_pixel := _latlontopixels(lat2, lon2, z);

    if bottom_left_pixel.x > top_right_pixel.x then
      bottom_left_pixel.x := bottom_left_pixel.x - CalcWrapWidth(z);

    if (abs(top_right_pixel.x - bottom_left_pixel.x) <= px) and (abs(top_right_pixel.y
      - bottom_left_pixel.y) <= py) then
    begin
      Result := z;
    end;
  end;

end;

procedure ProjectGoogleTile(const dbox: TlicgBaseDrawBox; const aGis:
  TlicgBaseGis; const Grapher: TlicgBaseGrapher);

  function FromCoordinatesToPixel(const lat, lon: double): TPoint;
  var
    x, y: integer;
    f: double;
  begin
    x := Round(PixelGlobeCenter.X + (lon * XPixelsToDegreesRatio));
    f := Min(Max(Sin(lat * RadiansToDegreesRatio), -0.9999), 0.9999);
    y := Round(PixelGlobeCenter.Y + 0.5 * LN((1 + f) / (1 - f)) *  -
      YPixelsToRadiansRatio);
    Result := Point(x, y);
  end;

  procedure FromPixelToCoordinates(const x, y: double; var lat, lon: double);
  begin
    try
      lon := (X - PixelGlobeCenter.X) / XPixelsToDegreesRatio;
      lat := (2 * Arctan(Exp((Y - PixelGlobeCenter.Y) /  - YPixelsToRadiansRatio))
        - PI / 2) * DegreesToRadiansRatio;
    except
    end;

  end;

var
  M: TlicgCoor;
  D: double;
  LRP, ULP, LLP, URP: TPoint;
  //S1,Y1, S2,Y2,

  W, H: integer;
  RE: IlicgEntity;
  TR, ZR: TlicgExtent;
  //RCor : TlicgExtent;
  MidPoint: TlicgCoor;
  R: TlicgExtent;
  P1, P2: IlicgCS;
  SL: TStrings;
var
  DoubleBuffer: TBitmap;
  G: TlicgBaseGrapher;
  TempGraph: TlicgBaseGrapher;
  K, L, N1, N2: integer;
  TmpLayer: TlicgBaseLayer;
  E: IlicgEntity;
  p: IlicgPainterObject;
  //png: TPNGObject;
  png: TlicgGraphicLink;
  z: integer;
  Stream: TStream;
  minz, maxz: integer;
  SrvPath, PrjPath, LevelPath, xPath, yfn: string;
  VisExt, AMapExtent, NR: TlicgExtent;
  MRPix: TRect;
  cx, cy: integer;
  dx, dy: integer;
  _pixels: integer;
  VisScale: double;
  MaxZoomScale: double;
  ok: boolean;
  ix, iy, tx, ty: integer;
  Mat: TlicgMatrix;
begin

  DoubleBuffer := nil;

  P1 := aGis.MapInfo.CS;
  if P1.IsUnknownCS then
  begin
    Application.MessageBox('Proje projeksiyonu tanýmlayýnýz...', 'Bilgi', MB_OK
      + MB_ICONINFORMATION);
    Exit;
  end;

  P2 := Licad.CreateCSFactory.ByEPSG(4326);

  {
  P2.ProjSystem := ord(Gdal_ProjSystem_GEO) ;
  P2.ProjDatum  := ord(Gdal_ProjDatum_World_Geodetic_System_1984) ;
  P2.ProjAttrbCnt := 0;
  P2.ProjUnit := Gdal__UnitARC_DEGREES;
  }

  if not (P1.Epsg = P2.Epsg) then
  begin
    Application.MessageBox('Proje Projeksiyonu yada OnTheFly Projeksiyon Coðrafik + WGS84 Datum  EPSG Karþýlýðý -> 4326 olmalý.',
      'Bilgi', MB_OK + MB_ICONINFORMATION);
    Exit;
  end;

  P2 := Licad.CreateCSFactory.ByEPSG(4326);
  {
  P2.ProjSystem := ord(Gdal_ProjSystem_GEO) ;
  P2.ProjDatum  := ord(Gdal_ProjDatum_World_Geodetic_System_1984) ;
  P2.ProjAttrbCnt := 0;
  P2.ProjUnit := Gdal__UnitARC_DEGREES;
  }


  _pixels := 256;
  for z := 0 to MAX_ZOOM do
  begin
    pixel_range[z] := (_pixels);
    _pixels := _pixels * 2;
  end;

  VisExt := Grapher.CurrentParams.VisualWindow;

  dbox.ZoomToExtension;

  AMapExtent := Agis.MapInfo.Extension;
  VisScale := Grapher.CurrentParams.Scale;

  R := AMapExtent; //Grapher.CurrentParams.VisualWindow;
  //Agis.RasterList.ProjectPoint (R.LowerLeft.X,R.LowerLeft.Y,R.LowerLeft.X,R.LowerLeft.Y, @P1,@P2, nil);
  //Agis.RasterList.ProjectPoint (R.UpperRight.X,R.UpperRight.Y,R.UpperRight.X,R.UpperRight.Y, @P1,@P2, nil);

  minz := CalcZoomFactor(R.LowerLeft.Y, R.LowerLeft.X, R.UpperRight.Y, R.UpperRight.X,
    abs(round(Grapher.ViewPortExtent.LowerLeft.X - Grapher.ViewPortExtent.UpperRight.X)),
    abs(round(Grapher.ViewPortExtent.LowerLeft.Y - Grapher.ViewPortExtent.UpperRight.Y)));

  if minz < 5 then
    exit;

  SrvPath := '';
  PrjPath := '';

  maxz := MAX_ZOOM;

  if GetTileSettings(SrvPath, minz, maxz) then
  begin
    PrjPath := SrvPath + '\' + ExtractFileName(ChangeFileExt(AGis.FileName, ''))
      + '_gtiles';
  end
  else
    exit;

  if not ((SrvPath <> '') and (PrjPath <> '')) then
    exit;

  if not DirectoryExists(PrjPath) then
    CreateDir(PrjPath)
  else
  begin
    DelTree(PrjPath);
    CreateDir(PrjPath);
  end;

  MaxZoomScale := Zoom_scale[minz];

  MidPoint.X := (AMapExtent.LowerLeft.X + AMapExtent.UpperRight.X) / 2;
  MidPoint.Y := (AMapExtent.LowerLeft.Y + AMapExtent.UpperRight.Y) / 2;

  dbox.ClearAlwaysDisplayList;

  TempGraph := TlicgGrapher.Create(adScreen);
  TempGraph.Assign(Grapher);

  try
    K := 0;

    for z := minz to maxz do
    begin

      pixelGlobeSize := round(256 * Math.Power(2, z));
      XPixelsToDegreesRatio := pixelGlobeSize / 360;
      YPixelsToRadiansRatio := pixelGlobeSize / (2 * PI);
      halfPixelGlobeSize := round(pixelGlobeSize / 2);
      PixelGlobeCenter := Point(halfPixelGlobeSize, halfPixelGlobeSize);

      DegreesToRadiansRatio := 180 / PI;
      RadiansToDegreesRatio := PI / 180;

      R := AMapExtent;
      //Agis.RasterList.ProjectPoint(R.LowerLeft.X,R.LowerLeft.Y,R.LowerLeft.X,R.LowerLeft.Y, @P1,@P2, nil);
      //Agis.RasterList.ProjectPoint(R.UpperRight.X,R.UpperRight.Y,R.UpperRight.X,R.UpperRight.Y, @P1,@P2, nil);


      LLP := FromCoordinatesToPixel(R.LowerLeft.Y, R.LowerLeft.X);
      URP := FromCoordinatesToPixel(R.UpperRight.Y, R.UpperRight.X);
      ULP := FromCoordinatesToPixel(R.UpperRight.Y, R.LowerLeft.X);
      LRP := FromCoordinatesToPixel(R.LowerLeft.Y, R.UpperRight.X);

      //S1 := ULP.X;
      //Y1 := ULP.Y;

      //FromPixelToCoordinates(S1+0,Y1+256, RCor.LowerLeft.Y,RCor.LowerLeft.X);
      //FromPixelToCoordinates(S1+256,Y1+0, RCor.UpperRight.Y,RCor.UpperRight.X);

      //TR := RCor;
      //Agis.RasterList.ProjectPoint(TR.LowerLeft.X,TR.LowerLeft.Y,TR.LowerLeft.X,TR.LowerLeft.Y, @P2,@P1, nil);
      //Agis.RasterList.ProjectPoint(TR.UpperRight.X,TR.UpperRight.Y,TR.UpperRight.X,TR.UpperRight.Y, @P2,@P1, nil);


      TempGraph.CurrentParams.Scale := ZOOM_SCALE[z] * visScale / MaxZoomScale;
      TempGraph.OriginalParams := Grapher.CurrentParams;
      TempGraph.ReCentre(MidPoint.X, MidPoint.Y);

      W := 256;
      H := 256;

      MRPix := TempGraph.RealToExtent(AMapExtent);

      //S1 := MRPix.Left;
      //S2 := S1 + w;

      tx := ULP.X div 256;
      ix := -1;

      K := 0;
      try

        cx := (MRPix.Right - MRPix.Left);
        cy := (MRPix.Bottom - MRPix.Top);

        dx := (cx div 256) + ORD(not (cx mod 256 = 0)) + 1;

        while dx > 0 {S2<=dx} do
        begin

          dec(dx);

          //Y1 := MRPix.Top;
          //Y2 := Y1;
          //Y2 := Y1 +  h;

          ty := ULP.Y div 256;
          iy := -1;

          inc(ix);

          dy := (cy div 256) + ORD(not (cy mod 256 = 0)) + 1;

          while dy > 0 {Y2<=dy} do
          begin
            dec(dy);

            inc(iy);

            FromPixelToCoordinates((tx + ix) * 256, (ty + iy) * 256, TR.UpperRight.Y,
              TR.LowerLeft.X);
            FromPixelToCoordinates((tx + ix) * 256 + 256, (ty + iy) * 256 + 256,
              TR.LowerLeft.Y, TR.UpperRight.X);

            ok := true;

              // bu bazý durumlarda doðru çalýþmýyor olabilir..
            ok := AGis.QuerySelectTool.IsQueryRectangle(TR.LowerLeft.X, TR.LowerLeft.Y,
              TR.UpperRight.X, TR.UpperRight.Y, '', '', goIntersects, AllEntityIDs);

            if ok then
            begin

              RE := Licad.CreateEntityFactory.MakeRectangle(TR.LowerLeft, TR.UpperRight);
              if z > 16 then
                RE.DrawTools.BrushTool.Pattern := 1
              else
                RE.DrawTools.BrushTool.Pattern := 0;

              RE.DrawTools.PenTool.Color := clAqua;
              RE.DrawTools.PenTool.Width := 1;

              RE.DrawTools.BrushTool.ForeColor := clred; // RandomRange(1,$7FFFFFFF);
              RE.DrawTools.BrushTool.BackColor := 536870911; //;RE.DrawTools.BrushTool.ForeColor;
              dbox.DrawEntity(RE);

              G := TlicgGrapher.Create(adScreen, nil);
              G.Drawbox := dbox;

              G.SetViewport(0, 0, w, h);
              G.SetWindow(TR.LowerLeft.X, TR.UpperRight.X, TR.LowerLeft.Y, TR.UpperRight.Y);

              NR := G.CurrentParams.VisualWindow;
              M := G.CurrentParams.MidPoint;

              DoubleBuffer := TBitmap.Create;

              DoubleBuffer.PixelFormat := DBox.ScreenBitmap.PixelFormat;
              DoubleBuffer.Width := W;
              DoubleBuffer.Height := H;
              DoubleBuffer.Transparent := False;

              Mat := IDENTITY_MATRIX2D;
                // Mat := Scale2D(1, +(Abs(TR.LowerLeft.X - TR.UpperRight.X)) / (Abs(TR.LowerLeft.Y - TR.UpperRight.Y)), M);
                // Mat := Scale2D(-(Abs(TR.LowerLeft.X - TR.UpperRight.X)) / (Abs(NR.LowerLeft.X - NR.UpperRight.X)),
                      //          -(Abs(TR.LowerLeft.Y - TR.UpperRight.Y)) / (Abs(NR.LowerLeft.Y - NR.UpperRight.Y)), M);

              Mat := Scale2D((Abs(NR.LowerLeft.X - NR.UpperRight.X)) / (Abs(TR.LowerLeft.X
                - TR.UpperRight.X)), (Abs(NR.LowerLeft.Y - NR.UpperRight.Y)) / (Abs
                (TR.LowerLeft.Y - TR.UpperRight.Y)), M);

              p := Licad.CreatePainterObject(nil);

              p.DrawEntities(TR, aGIS, GetIlicgCanvas(DoubleBuffer.Canvas), G,
                dbox.Selection, false, false, pmExcludeSelection, nil, 0, false,
                dbox.CurrentScale, @Mat);

              p := nil;

              RE.DrawTools.PenTool.Color := clFuchsia;
              dbox.DrawEntity(RE);

              LevelPath := PrjPath + '\' + inttostr(z);
              if not DirectoryExists(LevelPath) then
                CreateDir(LevelPath);

              xPath := LevelPath + '\' + inttostr(tx + ix);

              if not DirectoryExists(xPath) then
                CreateDir(xPath);

              yfn := xPath + '\' + inttostr(ty + iy) + '.png';

              inc(K);
              if K mod 50 = 0 then
                Application.ProcessMessages;

              with TlicgGraphicLink.Create do
              begin
                try
                  putPNG(DoubleBuffer, yfn, dbox.Color);
                finally
                  Free;
                end;
              end;

              DoubleBuffer.Free;

              G.Free;

            end;


              //Y2 := Y2 + h;
          end;

          //S2 := S2 + w;


        end;
      finally
      end;
    end;

  finally
    TempGraph.Free;
     //dbox.ClearAlwaysDisplayList;
     //dbox.ClearOnceDisplayList;
    Application.MessageBox(PChar('Tile iþlemleri bitti. Klasör-> ' + PrjPath),
      'Bilgi', MB_OK + MB_ICONINFORMATION);
    SL := TStringList.create;
    SL.Add('MinZoom  MaxZoom  Minx (lng)    Miny(lat) Maxx (lng)    Maxy(lat) ');
    SL.Add(inttostr(minz) + '  ' + inttostr(maxz) + '  ' + Floattostr(AMapExtent.lowerLeft.Y)
      + '  ' + Floattostr(AMapExtent.lowerLeft.X) + '  ' + Floattostr(AMapExtent.UpperRight.Y)
      + '  ' + Floattostr(AMapExtent.UpperRight.X) + '  ');
    SL.SaveToFile(PrjPath + '\' + 'boundary.txt');
    SL.Free;

  end;

end;
(*

procedure ProjectGoogleTile1024(const dbox : TlicgBaseDrawBox;const aGis : TlicgBaseGis; const Grapher :  TlicgBaseGrapher);


    function FromCoordinatesToPixel(const lat,lon: double): TPoint;
    var
     x,y: integer;
     f : double;
    begin
      x := Round(PixelGlobeCenter.X + (lon * XPixelsToDegreesRatio));
      f := Min(Max(Sin(lat * RadiansToDegreesRatio), -0.9999), 0.9999);
      y := Round(PixelGlobeCenter.Y + 0.5 * LN((1 + f) / (1 - f)) * - YPixelsToRadiansRatio);
      Result := Point(x, y);
    end;

    procedure FromPixelToCoordinates(const x, y: double;
      var lat, lon: double);
    begin
      try
        lon := (X - PixelGlobeCenter.X) / XPixelsToDegreesRatio;
        lat := (2 * Arctan(Exp((Y - PixelGlobeCenter.Y) / - YPixelsToRadiansRatio)) -  PI / 2) * DegreesToRadiansRatio;
      except
      end;

    end;




var
  M : TlicgCoor;
  D : double;
  LRP,
  ULP,
  LLP,
  URP : TPoint;
  //S1,Y1, S2,Y2,

  W, H : integer;
  RE : IlicgEntity;
  TR,
  ZR : TlicgExtent;
  //RCor : TlicgExtent;
  MidPoint: TlicgCoor;
  R : TlicgExtent;
  P1, P2 : IlicgCoordinatSystem;
  SL : TStrings;

var
  DoubleBuffer : Tlicgitmap;
  G : TlicgBaseGrapher;
  TempGraph : TlicgBaseGrapher;

  K,L,N1,N2 : integer;
  TmpLayer : TlicgBaseLayer;
  E :  IlicgEntity;

  p : IlicgPainterObject;
  //png: TPNGObject;
  png : TlicgGraphicLink;
  z : integer;
  Stream : TStream;
  minz, maxz: integer;

  SrvPath,
  PrjPath,
  LevelPath,
  xPath,
  yfn : string;
  VisExt,
  AMapExtent,NR : TlicgExtent;
  MRPix  : TRect;
  cx,cy : integer;
  dx,dy : integer;
  _pixels : integer;
  VisScale : double;
  MaxZoomScale: double;
  ok : boolean;
  ix,iy, tx, ty: integer;
  Mat : TlicgMatrix;
begin

  DoubleBuffer := nil;

  P1 := aGis.SpatialReference.CurrentProjection;
  if isInvalidProjection(P1) then begin
    ShowMessage('Proje projeksiyonu tanýmlayýnýz...');
    Exit;
  end;



  P2.ProjSystem := ord(Gdal_ProjSystem_GEO) ;
  P2.ProjDatum  := ord(Gdal_ProjDatum_World_Geodetic_System_1984) ;
  P2.ProjAttrbCnt := 0;
  P2.Proj Unit := Gdal__UnitARC_DEGREES;

  if Not CompareMem(@P1,@P2, Sizeof(IlicgCoordinatSystem)) then begin
    ShowMessage('Proje Projeksiyonu yada OnTheFly Projeksiyon Coðrafik + WGS84 Datum  EPSG Karþýlýðý -> 4326 olmalý.' + ' ' );
    Exit;
  end;



  P2.ProjSystem := ord(Gdal_ProjSystem_GEO) ;
  P2.ProjDatum  := ord(Gdal_ProjDatum_World_Geodetic_System_1984) ;
  P2.ProjAttrbCnt := 0;
  P2.Proj Unit := Gdal__UnitARC_DEGREES;


  _pixels := 1024;
   for z:=0 to MAX_ZOOM do begin
      pixel_range[z] := (_pixels);
      _pixels := _pixels * 2;
   end;

  VisExt   := Grapher.CurrentParams.VisualWindow;

  dbox.ZoomToExtension;

  AMapExtent := Agis.MapInfo.Extension;
  VisScale := Grapher.CurrentParams.Scale;

  R := AMapExtent; //Grapher.CurrentParams.VisualWindow;
  //Agis.RasterList.ProjectPoint (R.LowerLeft.X,R.LowerLeft.Y,R.LowerLeft.X,R.LowerLeft.Y, @P1,@P2, nil);
  //Agis.RasterList.ProjectPoint (R.UpperRight.X,R.UpperRight.Y,R.UpperRight.X,R.UpperRight.Y, @P1,@P2, nil);

  minz := CalcZoomFactor(R.LowerLeft.Y, R.LowerLeft.X,R.UpperRight.Y, R.UpperRight.X,
     abs(round(Grapher.ViewPortExtent.LowerLeft.X-Grapher.ViewPortExtent.UpperRight.X)),
     abs(round(Grapher.ViewPortExtent.LowerLeft.Y-Grapher.ViewPortExtent.UpperRight.Y)));

  if minz<5 then
    exit;


  SrvPath := '';
  PrjPath := '';

  maxz := MAX_ZOOM;

  if GetTileSettings(SrvPath, minz, maxz) then begin
    PrjPath := SrvPath + '\' + ExtractFileName(ChangeFileExt(AGis.FileName, '') )+ '_gtiles';
  end
  else
    exit;



  if Not ((SrvPath<>'') and (PrjPath<>'')) then
    exit;

  if not DirectoryExists(PrjPath) then
    CreateDir(PrjPath)
  else begin
      DelTree(PrjPath);
    CreateDir(PrjPath);
  end;

  MaxZoomScale := Zoom_scale[minz];

  MidPoint.X := (AMapExtent.LowerLeft.X + AMapExtent.UpperRight.X) / 2;
  MidPoint.Y := (AMapExtent.LowerLeft.Y + AMapExtent.UpperRight.Y) / 2;

  dbox.ClearAlwaysDisplayList;

  TempGraph := TlicgGrapher.Create(adScreen);
  TempGraph.Assign(Grapher);

  try
    K := 0;

    for z:=minz to maxz do begin


      pixelGlobeSize := round(1024 * Math.Power(2, z));
      XPixelsToDegreesRatio := pixelGlobeSize / 360;
      YPixelsToRadiansRatio := pixelGlobeSize / (2 * PI);
      halfPixelGlobeSize := round(pixelGlobeSize / 2);
      PixelGlobeCenter:= Point(halfPixelGlobeSize, halfPixelGlobeSize);

      DegreesToRadiansRatio := 180 / PI;
      RadiansToDegreesRatio := PI / 180;

      R := AMapExtent;
      //Agis.RasterList.ProjectPoint(R.LowerLeft.X,R.LowerLeft.Y,R.LowerLeft.X,R.LowerLeft.Y, @P1,@P2, nil);
      //Agis.RasterList.ProjectPoint(R.UpperRight.X,R.UpperRight.Y,R.UpperRight.X,R.UpperRight.Y, @P1,@P2, nil);


      LLP :=  FromCoordinatesToPixel (R.LowerLeft.Y,  R.LowerLeft.X);
      URP :=  FromCoordinatesToPixel (R.UpperRight.Y, R.UpperRight.X);
      ULP :=  FromCoordinatesToPixel (R.UpperRight.Y, R.LowerLeft.X);
      LRP :=  FromCoordinatesToPixel (R.LowerLeft.Y,  R.UpperRight.X);

      //S1 := ULP.X;
      //Y1 := ULP.Y;


      //FromPixelToCoordinates(S1+0,Y1+1024, RCor.LowerLeft.Y,RCor.LowerLeft.X);
      //FromPixelToCoordinates(S1+1024,Y1+0, RCor.UpperRight.Y,RCor.UpperRight.X);


      //TR := RCor;
      //Agis.RasterList.ProjectPoint(TR.LowerLeft.X,TR.LowerLeft.Y,TR.LowerLeft.X,TR.LowerLeft.Y, @P2,@P1, nil);
      //Agis.RasterList.ProjectPoint(TR.UpperRight.X,TR.UpperRight.Y,TR.UpperRight.X,TR.UpperRight.Y, @P2,@P1, nil);


      TempGraph.CurrentParams.Scale := ZOOM_SCALE[z] * visScale / MaxZoomScale ;
      TempGraph.OriginalParams := Grapher.CurrentParams;
      TempGraph.ReCentre(MidPoint.X,MidPoint.Y);


      W := 1024;
      H := 1024;


      MRPix :=  TempGraph.RealToExtent(AMapExtent);

      //S1 := MRPix.Left;
      //S2 := S1 + w;

      tx := ULP.X div 1024;
      ix := -1;

      K := 0;
      try

        cx := (MRPix.Right-MRPix.Left);
        cy := (MRPix.Bottom-MRPix.Top);


        dx := (cx div 1024) + ORD (not (cx mod 1024 = 0)) + 1;

        while dx>0 {S2<=dx} do begin

          dec(dx);

          //Y1 := MRPix.Top;
          //Y2 := Y1;
          //Y2 := Y1 +  h;

          ty := ULP.Y div 1024;
          iy := -1;

          inc(ix,1);

          dy := (cy div 1024) + ORD (not (cy mod 1024 = 0)) +1;

          while dy>0 {Y2<=dy} do begin
              dec(dy);

              inc(iy,1) ;

              FromPixelToCoordinates((tx+ix)*1024     ,   (ty+iy)*1024 ,       TR.UpperRight.Y, TR.LowerLeft.X);
              FromPixelToCoordinates((tx+ix)*1024+1024 ,   (ty+iy)*1024+1024  ,  TR.LowerLeft.Y , TR.UpperRight.X);


              ok := true;

              // bu bazý durumlarda doðru çalýþmýyor olabilir..
              ok :=  AGis.QuerySelectTool.IsQueryRectangle(TR.LowerLeft.X,TR.LowerLeft.Y,TR.UpperRight.X,TR.UpperRight.Y,
                             '', '', goIntersects, AllEntityIDs);

              if ok then
              begin



                 RE := Licad.CreateEntityFactory.MakeRectangle(TR.LowerLeft, TR.UpperRight);
                 if z>16 then
                   RE.DrawTools.BrushTool.Pattern := 1
                 else
                   RE.DrawTools.BrushTool.Pattern := 0;

                 RE.DrawTools.PenTool.Color := clAqua;
                 RE.DrawTools.PenTool.Width := 1;


                 RE.DrawTools.BrushTool.ForeColor := clred; // RandomRange(1,$7FFFFFFF);
                 RE.DrawTools.BrushTool.BackColor := 536870911; //;RE.DrawTools.BrushTool.ForeColor;
                 dbox.DrawEntity(RE);

                 G := TlicgGrapher.Create(adScreen, nil);
                 G.Drawbox := dbox;

                 G.SetViewport(0, 0, w, h);
                 G.SetWindow(TR.LowerLeft.X, TR.UpperRight.X, TR.LowerLeft.Y, TR.UpperRight.Y);

                 NR := G.CurrentParams.VisualWindow;
                 M := G.CurrentParams.MidPoint;


                 DoubleBuffer:= Tlicgitmap.Create;

                 DoubleBuffer.PixelFormat  := DBox.ScreenBitmap.PixelFormat;
                 DoubleBuffer.Width        := W;
                 DoubleBuffer.Height       := H;
                 DoubleBuffer.Transparent  := False;

                 Mat := IDENTITY_MATRIX2D;
                // Mat := Scale2D(1, +(Abs(TR.LowerLeft.X - TR.UpperRight.X)) / (Abs(TR.LowerLeft.Y - TR.UpperRight.Y)), M);
                // Mat := Scale2D(-(Abs(TR.LowerLeft.X - TR.UpperRight.X)) / (Abs(NR.LowerLeft.X - NR.UpperRight.X)),
                      //          -(Abs(TR.LowerLeft.Y - TR.UpperRight.Y)) / (Abs(NR.LowerLeft.Y - NR.UpperRight.Y)), M);

                Mat := Scale2D((Abs(NR.LowerLeft.X - NR.UpperRight.X)) / (Abs(TR.LowerLeft.X - TR.UpperRight.X)),
                         (Abs(NR.LowerLeft.Y - NR.UpperRight.Y)) / (Abs(TR.LowerLeft.Y - TR.UpperRight.Y)), M);


                 p := Licad.CreatePainterObject(nil);


                   p.DrawEntities(
                                  TR,
                                  aGIS,
                                  GetIlicgCanvas(DoubleBuffer.Canvas),
                                  G,
                                  dbox.Selection,
                                  false,
                                  false,
                                  pmExcludeSelection,
                                  nil,
                                  0,
                                  false,
                                  G.CurrentScale,
                                  @Mat);

                 p := nil;


                 RE.DrawTools.PenTool.Color := clFuchsia;
                 dbox.DrawEntity(RE);


                 LevelPath := PrjPath + '\' + inttostr(z);
                 if not DirectoryExists(LevelPath) then
                   CreateDir(LevelPath);

                 xPath := LevelPath + '\' + inttostr(tx+ix+3);

                 if not DirectoryExists(xPath) then
                   CreateDir(xPath);

                 yfn :=  xPath + '\' + inttostr(ty+iy+3) + '.png';

                 inc(K);
                 if K Mod 50=0 then
                   Application.ProcessMessages;


                 with TlicgGraphicLink.Create do begin
                   try
                      putPNG(DoubleBuffer,yfn, dbox.Color);
                   finally
                     Free;
                   end;
                 end;


                 DoubleBuffer.Free;

                 G.Free;

              end;


              //Y2 := Y2 + h;
          end;

          //S2 := S2 + w;


        end;
      finally
     end;
    end;

  finally
     TempGraph.Free;
     //dbox.ClearAlwaysDisplayList;
     //dbox.ClearOnceDisplayList;
     ShowMessage('Tile iþlemleri bitti. Klasör-> ' + PrjPath);
     SL := TStringList.create;
     SL.Add('MinZoom  MaxZoom  Minx (lng)    Miny(lat) Maxx (lng)    Maxy(lat) ');
     SL.Add(inttostr(minz)  + '  ' + inttostr(maxz)  + '  ' + Floattostr(AMapExtent.lowerLeft.Y) + '  ' +Floattostr(AMapExtent.lowerLeft.X) + '  ' +
            Floattostr(AMapExtent.UpperRight.Y) + '  ' +Floattostr(AMapExtent.UpperRight.X) + '  ' );
     SL.SaveToFile(PrjPath+ '\' + 'boundary.txt');
     SL.Free;

  end;




end;

*)

end.


