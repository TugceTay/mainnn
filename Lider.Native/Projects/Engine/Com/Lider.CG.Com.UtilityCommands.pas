unit Lider.CG.Com.UtilityCommands;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Sysutils,
  Classes,
  Controls,
  Dialogs,
  Forms,
  IOUtils,
  Types,
  Zip,
  Gosterge,
  Lider.CG.Com.Lib,
  Lider.CG.Com.System,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Types,
  Lider.CG.Com.ProjectInt,
  Lider.CG.Com.ModulesConsts,
  Lider.CG.Com.ModulesTypes,
  Lider.CG.Com.ModulesInt,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TLicadProjectFile = class
  private
    FProgressForm: TfmGosterge;
    procedure OnZipProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
  published
    { published declarations }
    procedure Save(AShowProgress: Boolean = True; ACreateBackupFile: Boolean = True; AAddRecentProjects: Boolean = True);
  end;

// Proje
function NewProject(AProject: IlicgProject; AProjectFileName: string = ''; Progress: TObject = nil): boolean;
function OpenProject(AProject: IlicgProject): boolean;
function OpenLastProject(AProject: IlicgProject; AProjectFileName: string; AShowProgress: Boolean = True; AAddRecentProjects: Boolean = True): Boolean;
function SaveProject(AShowProgress: Boolean = True; ACreateBackupFile: Boolean = True; AAddRecentProjects: Boolean = True): Boolean;
function SaveAsProject(AGIS: TlicgBaseGis; var AProjectFileName: string; AAddRecentProjects: Boolean = True): Boolean;
procedure SaveSelection;
function CloseProject: boolean;
function PrintPreview(const CmdLine: TlicgBasecmdline): boolean;
procedure ProgramExit;

procedure ShowViewportByDrawBoxCount(WinCount: integer);

function OpenBlankCadProject(ph: THandle; OnMouseMove2D: TlicgMouseMoveEvent;
  OnZoomChange: TlicgZoomChangeEvent; OnSelectionChange: TNotifyEvent;
  OnHintCommadEvent: THintCommandEvent; OnShowHint: TlicgShowHintEvent;
  CreateLeftFormProc: TCreateLeftFormProc; SetFillLayersProc: TSetFillLayersProc;
  SetLeftPanelGroupProc: TSetLeftPanelGroupProc;
  WhenProgramStart: boolean = false): boolean;

// Düzenle
procedure UndoEdit;
procedure RedoEdit;
procedure CutEdit;
procedure CopyEdit;
procedure CopyAsTextEdit;
procedure CopyAsBmpEdit;
procedure PasteEdit;
procedure PasteAndRePositionEdit;
procedure PasteAsTextEdit;
procedure DeleteEdit;
procedure MoveEntity;
procedure QuickMoveEntity;
procedure MoveToActiveLayer;
procedure DeleteQuickEdit;
procedure GroupEdit;
procedure UnGroupEdit;
procedure MoveForward;
procedure MoveBackward;
procedure SendToBack;
procedure BringToFront;
procedure ReShapeEntity;
procedure ScaleEntity;
procedure ScaleXEntity;
procedure ScaleYEntity;
procedure RotateEntity;
procedure MirrorEntity;

// Görünüm
procedure RepaintView; // 150
procedure ZoomToExtension;//151
procedure ZoomIN;//152
procedure ZoomOUT;//153
procedure ZoomWIN;//154
procedure ZoomToSelection;//155
procedure ZoomToScale;//156
procedure ZoomToLayer;//157
procedure ZoomToPrevious;//158
procedure PanScreen;
procedure ScrollScreen;
//procedure AerialView (Const DrawBox : TlicgBaseDrawBox);
procedure ShowGrid;
procedure ShowGuideLines;
//procedure ChangeWScale;
procedure ClipScreenSelection; //167
procedure ClipScreenRect; //163
procedure ClipScreenPoly; //162
procedure ClipCancel; //164

//Seçim
procedure DefaultAction;
procedure SelectWin;
procedure SelectCircle;
procedure SelectPoly;
procedure SelectAll;
procedure Unselect;
procedure SelectLayer;
procedure SelectCopyToLayer;
procedure MeasureInfo;

// Draw
procedure DrawSymbolPoint;
procedure DrawLine;
procedure DrawPLine;
procedure DrawPLineRounded;
procedure DrawSketch;
procedure LINEBYGIVENPARAM;
procedure DrawSpline;
procedure DrawPolygon;
procedure DrawROUNDR;
procedure DrawRectangle;
procedure DrawBufferLine;
procedure RECTBYGIVENPARAM;
procedure DrawArc;
procedure DrawArcSE;
procedure DrawArcFCS;
procedure SetArcType(Value: Integer);
procedure DrawEllipse;
procedure DrawCircle3P;
procedure DrawCircle2P;
procedure DrawCircleCR;
procedure CIRCLEBYGIVENRADIUS;
procedure DrawDimHorz;
procedure DrawDimVert;
procedure DrawDimParall;
procedure DrawSymbolMulti;
procedure DrawTextTrueType;
procedure DrawTextVectorialStandart;
procedure DrawPERSISTBITMAP;
procedure DrawOLE;
procedure RefreshOleObjects;

implementation

uses
  IniFiles,
  cxProgressBar;

procedure SaveSelection;
var
  AProjectInt: IProjectTools;
begin
  AProjectInt := Licad.CreateProjectTools;
  try
    if Assigned(AProjectInt) then
    begin
      AProjectInt.SaveSelectedEntities(CurrCmdLine.ActiveDrawBox);
    end;
  finally
    AProjectInt := nil;
  end;
end;

function NewProject(AProject: IlicgProject; AProjectFileName: string = ''; Progress: TObject = nil): Boolean;
var
  AProjectTools: IProjectTools;
begin
  Result := False;
  AProjectTools := Licad.CreateProjectTools;
  if Assigned(AProjectTools) then
  begin
    try
      Result := AProjectTools.NewProject(AProject, AProjectFileName, Progress);
    except
      Result := false;
    end;
  end;
  AProjectTools := nil;
end;

function OpenProject(AProject: IlicgProject): boolean;
var
  AProjectTools: IProjectTools;
begin
  Result := false;
  AProjectTools := Licad.CreateProjectTools;
  if Assigned(AProjectTools) then
  begin
    Result := True;
    try
    {
      fn := '';
      fn1 := ExtractFilePath(paramstr(0))+ 'MES.EZF';
      if FileExists(fn1)  then
         fn := fn1
      else
      begin
        fn := fn1;
        fn1 := ExtractFilePath(paramstr(0))+ 'Data\Sablon.EZF';
        if FileExists(fn1)  then begin
           TFile.Copy(fn1, fn);
        end;
      end;
      }
      AProjectTools.OpenProject(AProject, '');
    except
      Result := False;
    end;
  end;
  AProjectTools := nil;
end;

function OpenLastProject(AProject: IlicgProject; AProjectFileName: string ; AShowProgress: Boolean = True; AAddRecentProjects: Boolean = True): Boolean;
var
  AProjectTools: IProjectTools;
begin
  Result := False;
  AProjectTools := Licad.CreateProjectTools;
  if Assigned(AProjectTools) then
  begin
    Result := True;
    try
      AProjectTools.OpenProject(AProject, AProjectFileName, AShowProgress, AAddRecentProjects);
    except
      Result := False;
    end;
  end;
  AProjectTools := nil;
end;

function CloseProject: Boolean;
begin
  Result := False; //IgnoreClose
  Licad.GetCmdLineList.DeleteByIndex(Licad.GetViewPortCurrIndex, Result);
end;

function SaveProject(AShowProgress: Boolean = True; ACreateBackupFile: Boolean = True; AAddRecentProjects: Boolean = True): Boolean;
var
  LicadProjectFile: TLicadProjectFile;
begin
  Result := False;
  LicadProjectFile := TLicadProjectFile.Create;
  try
    LicadProjectFile.Save(AShowProgress, ACreateBackupFile, AAddRecentProjects);
    Result := True;
  finally
    if AShowProgress then
      LicadProjectFile.FProgressForm.Free;
    LicadProjectFile.Free;
  end;
end;

function SaveAsProject(AGIS: TlicgBaseGis; var AProjectFileName: string;  AAddRecentProjects: Boolean = True): Boolean;
var
  AProjectTools: IProjectTools;
begin
  Result := False;
  AProjectTools := Licad.CreateProjectTools;
  try
    if Assigned(AProjectTools) then
    begin
      Result := AProjectTools.SaveAsProject(AGis, AProjectFileName, AAddRecentProjects);
    end;
  finally
    AProjectTools := nil;
  end;
end;

function PrintPreview(const CmdLine: TlicgBasecmdline): boolean;
var
  AProjectTools: IProjectTools;
begin
  Result := false;
  AProjectTools := Licad.CreateProjectTools;
  if Assigned(AProjectTools) then
  begin
    result := true;
    try
      AProjectTools.PrintPreview(CmdLine);
    except
      Result := false;
    end;
  end;
  AProjectTools := nil;
end;

procedure ProgramExit;
var
  AProjectTools: IProjectTools;
begin
  AProjectTools := Licad.CreateProjectTools;
  if Assigned(AProjectTools) then
  begin
    try
      AProjectTools.Exit;
    except
    end;
  end;
  AProjectTools := nil;
end;

procedure UndoEdit;
begin
  CurrCmdLine.DoCommand('UNDO', 'UNDO'); //SCmdUndo);
end;

procedure RedoEdit;
begin
  CurrCmdLine.DoCommand('REDO', 'REDO'); //SCmdRedo);
end;

procedure CutEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('CUT', 'CUT'); //SCmdCut, SCmdCut);
end;

procedure CopyEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('COPY', 'COPY'); //SCmdCopy);
end;

procedure CopyAsTextEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('COPYASTEXT', 'COPYASTEXT'); //SCmdCopy);
end;

procedure CopyAsBmpEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('COPYASBMP', 'COPYASBMP'); //SCmdCopy);
end;

procedure PasteEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('PASTE', 'PASTE'); //SCmdPaste, SCmdPaste);
end;

procedure PasteAndRePositionEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('PASTEANDREPOSITION', 'PASTEANDREPOSITION'); //SCmdPaste, SCmdPaste);
end;

procedure PasteAsTextEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('PASTEASTEXT', 'PASTEASTEXT'); //SCmdPaste, SCmdPaste);
end;

procedure MoveEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('MOVE');
end;

procedure QuickMoveEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('QMOVE');
end;

procedure MoveToActiveLayer;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('AKTIFKATMANATASI');
end;

procedure DeleteEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('DELETE', 'DELETE'); //SCmdDelete, SCmdDelete);
end;

procedure DeleteQuickEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('QDELETE');
end;

procedure ReShapeEntity;
begin
  CurrCmdLine.DoCommand('RESHAPE');
end;

procedure ScaleEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('SCALE');
end;

procedure ScaleXEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('SCALEX');
end;

procedure ScaleYEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('SCALEY');
end;

procedure RotateEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('ROTATE');
end;

procedure MirrorEntity;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('MIRROR');
end;

procedure GroupEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('GROUP');
end;

procedure UnGroupEdit;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('UNGROUP');
end;

procedure MoveForward;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('MOVEFORWARDENT');
end;

procedure MoveBackward;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('MOVEBACKWARDENT');
end;

procedure SendToBack;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('SENDBACKENT');
end;

procedure BringToFront;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('BRINGFRONTENT');
end;

procedure RepaintView;
begin
  CurrCmdLine.All_Repaint;
end;

procedure ZoomToExtension;
begin
  CurrCmdLine.ActiveDrawBox.ZoomToExtension(true);
end;

procedure ZoomIN;
begin
  CurrCmdLine.DoCommand('ZOOMIN');
end;

procedure ZoomOUT;
begin
  CurrCmdLine.DoCommand('ZOOMOUT');
end;

procedure ZoomWIN;
begin
  CurrCmdLine.DoCommand('ZOOMWIN');
end;

procedure ZoomToSelection;
begin
  CurrCmdLine.ActiveDrawBox.ZoomToSelection;
end;

procedure ZoomToScale;
begin
  // ilker eksik geliþtirilecek CurrCmdLine.ActiveDrawBox.ZoomToScale;
end;

procedure ZoomToLayer;
var
  Layers: TList;
begin
  Layers := TList.Create;
  Licad.LayerSelectDialog(CurrCmdLine, Layers, False);
  try
    if Layers.Count > 0 then
      CurrCmdLine.ActiveDrawBox.ZoomToLayer(TlicgBaselayer(Layers.items[0]), True);
  finally
    Layers.Free;
  end;
end;

procedure ZoomToPrevious;
begin
  CurrCmdLine.ActiveDrawBox.ZoomPrevious;
end;

procedure PanScreen;
begin
  CurrCmdLine.DoCommand('PAN');
end;

procedure ScrollScreen;
begin
  CurrCmdLine.DoCommand('SCROLL');
end;


{
procedure AerialView (Const DrawBox : TlicgBaseDrawBox);
begin
  fAerial.AreialParentHWND := DrawBox.handle;
  with TfmAerial.Create(DrawBox) do
    Show;
end;
}

procedure ShowGrid;
begin
  CurrCmdLine.ActiveDrawBox.GridInfo.ShowGrid := not CurrCmdLine.ActiveDrawBox.GridInfo.ShowGrid;
  CurrCmdLine.ActiveDrawBox.Repaint;
end;

procedure ShowGuideLines;
begin
  CurrCmdLine.ActiveDrawBox.GIS.ShowGuidelines := not CurrCmdLine.ActiveDrawBox.GIS.ShowGuidelines;
  CurrCmdLine.ActiveDrawBox.Refresh;
end;


{
procedure ChangeWScale;
begin
   With TfmChangeWorkScale.Create(nil) do
   begin
     try
       ChangeWorkScale(CurrCmdLine.ActiveDrawBox.GIS) ;
     Finally
       Free;
     end;
   end;
end;
}

procedure ClipScreenSelection;
var
  AVector: IlicgVector;
begin
  if CurrCmdLine.ActiveDrawBox.Selection.NumSelected > 0 then
  begin
    AVector := Licad.CreateEntityFactory.MakeVector(3, 0);
    try
      ExtentToVector(CurrCmdLine.ActiveDrawBox.Selection.GetExtension, AVector);
      CurrCmdLine.ActiveDrawBox.GIS.ClearClippedArea;
      CurrCmdLine.ActiveDrawBox.GIS.ClipPolygonalArea.Assign(AVector);
      CurrCmdLine.ActiveDrawBox.GIS.MapInfo.ClipAreaKind := cpkPolygonal;
      CurrCmdLine.ActiveDrawBox.GIS.MapInfo.IsAreaClipped := true;
    finally
      AVector := nil;
    end;
  end;
end;

procedure ClipScreenRect;
begin
  CurrCmdLine.ActiveDrawBox.GIS.ClearClippedArea;
  CurrCmdLine.DoCommand('SETCLIPAREA');
end;

procedure ClipScreenPoly;
begin
  CurrCmdLine.ActiveDrawBox.GIS.ClearClippedArea;
  CurrCmdLine.DoCommand('CLIPPOLYAREA');
end;

procedure ClipCancel;
begin
  CurrCmdLine.ActiveDrawBox.GIS.ClearClippedArea;
end;

procedure ShowViewportByDrawBoxCount(WinCount: integer);
begin
  if Licad <> nil then
  begin
    Licad.ShowViewportByDrawBoxCount(WinCount);
  end;
end;

procedure DefaultAction;
begin
  if not CurrCmdLine.isQuickSelectAction(CurrCmdLine.CurrentAction, qstDefault) then
  begin
    CurrCmdLine.Clear;
    CurrCmdLine.LastCommand := '';
    CurrCmdLine.LastActionID := '';
  end;
end;

procedure SelectWin;
begin
  CurrCmdLine.DoCommand('SELECT');
end;

procedure SelectCircle;
begin
  CurrCmdLine.DoCommand('CIRCLESEL');
end;

procedure SelectPoly;
begin
  CurrCmdLine.DoCommand('POLYGONSEL');
end;

procedure SelectAll;
begin
  CurrCmdLine.ActiveDrawBox.SelectAll;
end;

procedure Unselect;
begin
  CurrCmdLine.ActiveDrawBox.UnSelectAll;
end;

procedure SelectLayer;
var
  Layers: TList;
  Canceled: Boolean;
  i: integer;
begin
  Layers := TList.Create;
  Licad.LayerSelectDialog(CurrCmdLine, Layers, False);
  try
    if Layers.count > 0 then
    begin
      for i := 0 to Layers.count - 1 do
      begin
        CurrCmdLine.ActiveDrawBox.DoSelectLayer(TlicgBaselayer(Layers.items[i]),
          Canceled);
      end;
      CurrCmdLine.ActiveDrawBox.Regendrawing;
    end;
  finally
    Layers.free;
  end;
end;

procedure SelectCopyToLayer;
begin
 // Will Do
end;


procedure MeasureInfo;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('MEASURES');
end;

function RGB_BGR(c: string): integer;
var
  s: string;
begin
  result := strtoint('$' + c);
  s := c;
  if length(c) <> 8 then
    exit;

  s[1] := '0';
  s[2] := '0';
  s[3] := c[3];
  s[4] := c[4];
  s[5] := c[5];
  s[6] := c[6];
  s[7] := c[7];
  s[8] := c[8];

  result := strtoint('$' + s);

end;

procedure DrawSymbolPoint;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('SYMBOL');
end;

procedure DrawLine;
begin
  CurrCmdLine.DoCommand('LINE', 'LINE');
end;

procedure DrawPLine;
begin
  CurrCmdLine.DoCommand('PLINE');
end;

procedure DrawPLineRounded;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ROUNDEDPLINE');
end;

procedure DrawSketch;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('SKETCH');
end;

procedure LINEBYGIVENPARAM;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('LINEBYGIVENPARAM');
end;

procedure DrawSpline;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('SPLINE');
end;

procedure DrawPolygon;
begin
  // CurrCmdLine.clear;
  CurrCmdLine.DoCommand('POLYGON');
end;

procedure DrawROUNDR;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ROUNDR');
end;

procedure DrawRectangle;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('RECTANGLE');
end;

procedure DrawBufferLine;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('POLYGONBUFFER');
end;

procedure RECTBYGIVENPARAM;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('RECTBYGIVENPARAM');
end;

procedure DrawArc;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ARC');
end;

procedure DrawArcSE;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ARCSE');
end;

procedure DrawArcFCS;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ARCFCS');
end;

procedure SetArcType(Value: Integer);
begin
  case Value of
    0:
      Licad.Settings.ArcStyle := asOpened;
    1:
      Licad.Settings.ArcStyle := asChord;
    2:
      Licad.Settings.ArcStyle := asPie;
  end;
end;

procedure DrawEllipse;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('ELLIPSE');
end;

procedure DrawCircle3P;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('CIRCLE3P');
end;

procedure DrawCircle2P;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('CIRCLE2P');
end;

procedure DrawCircleCR;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('CIRCLECR');
end;

procedure CIRCLEBYGIVENRADIUS;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('CIRCLEBYGIVENRADIUS');
end;

procedure DrawDimHorz;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('DIMHORZ');
end;

procedure DrawDimVert;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('DIMVERT');
end;

procedure DrawDimParall;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('DIMPARALL');
end;

procedure DrawSymbolMulti;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('DRAWMULTISYMBOL');
end;

procedure DrawTextTrueType;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('TEXT');
end;

procedure DrawTextVectorialStandart;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('JUSTIFTEXT');
end;

procedure DrawPERSISTBITMAP;
begin
  CurrCmdLine.clear;
  CurrCmdLine.DoCommand('PERSISTBITMAP');
end;

procedure DrawOLE;
begin
  CurrCmdLine.Clear;
  CurrCmdLine.DoCommand('OLE');
end;

procedure RefreshOleObjects;
var
  tmpList: TStringList;
  i: Integer;
begin
  tmpList := TStringList.Create;
  for i := 0 to Licad.Settings.OleContainerList.getCount - 1 do
  begin
    tmpList.add(Licad.Settings.OleContainerList.GetFileNameWithIndex(i));
  end;
  Licad.Settings.OleContainerList.Clear;
  for i := 0 to tmpList.Count - 1 do
  begin
    Licad.Settings.OleContainerList.PickExistingOrAdd(tmpList[i]);
  end;
  tmpList.Free;
  CurrCmdLine.ActiveDrawBox.RepaintExtent(CurrCmdLine.ActiveDrawBox.Grapher.CurrentParams.VisualWindow);
end;

function OpenBlankCadProject(ph: THandle; OnMouseMove2D: TlicgMouseMoveEvent;
  OnZoomChange: TlicgZoomChangeEvent; OnSelectionChange: TNotifyEvent;
  OnHintCommadEvent: THintCommandEvent; OnShowHint: TlicgShowHintEvent;
  CreateLeftFormProc: TCreateLeftFormProc; SetFillLayersProc: TSetFillLayersProc;
  SetLeftPanelGroupProc: TSetLeftPanelGroupProc;
//      CreateActionFormEvent : TCreateActionFormEvent;
//      DestroyActionFormEvent : TDestroyActionFormEvent;
  WhenProgramStart: boolean = false): boolean;
var
  AProject: IlicgProject;
  fn: string;
begin
  Result := false;
  AProject := Licad.CreateProject;
  AProject.CreateLeftFormProc := CreateLeftFormProc;
  AProject.FillLayersProc := SetFillLayersProc;
  AProject.LeftPanelGroupProc := SetLeftPanelGroupProc;
  AProject.OnMouseMove2D := OnMouseMove2D;
  AProject.OnZoomChange := OnZoomChange;
  AProject.OnSelectionChange := OnSelectionChange;
  AProject.OnHintCommadEvent := OnHintCommadEvent;
  AProject.OnShowHint := OnShowHint;
  //AProject.CreateActionFormEvent := CreateActionFormEvent;
  //AProject.DestroyActionFormEvent := DestroyActionFormEvent;


  AProject.isFirstTemporaryProject := WhenProgramStart;
  AProject.SetBlankGIS(true);

  AProject.ParentHandle := ph;
  AProject.ReferansGis := nil;
  fn := GetTemporaryEZFFileNameInExe('Proje');

  if fn <> '' then
  begin

    Result := NewProject(AProject, fn);

    if Result then
    begin
      if CurrCmdLine <> nil then
        CurrCmdLine.ActiveDrawBox.ZoomWindow(licgExtent(-100, -100, 100, 100));
    end;

  end
  //else
    //Application.MessageBox('Yeni Proje oluþturulamadý.', 'Hata', MB_OK + MB_ICONERROR);
end;

{ TLicadProjectFile }

procedure TLicadProjectFile.Save(AShowProgress: Boolean = True; ACreateBackupFile: Boolean = True; AAddRecentProjects: Boolean = True);
var
  FZip: TZipFile;
  ZipLCG: string;
  AFileName, ALicadProjectFile: string;
  ListFiles: TStringDynArray;
  AZipComp: TZipCompression;
begin
  AFileName := Trim(ExtractFilePath(CurrCmdline.ActiveDrawBox.GIS.FileName));
  ALicadProjectFile := Trim(CurrCmdLine.ActiveDrawBox.GIS.Project.LicadProjectFile);
  if (AFileName = '') or (ALicadProjectFile = '') then
  begin
    Application.MessageBox('Proje adý boþ. Kaydedilemedi.', 'Hata', mrOk + MB_ICONERROR);
    Exit;
  end;

  // ilker baþlama bunlarý yapmazsak çalýþma esnasýnda kaydettiðimizde problem oluyor.
  CurrCmdline.ActiveDrawBox.Selection.Clear;
  CurrCmdline.ActiveDrawBox.LastSelection.Clear;
  CurrCmdline.ActiveDrawBox.Undo.Clear;
  CurrCmdline.ActiveDrawBox.Redo.Clear;
  CurrCmdline.Pop;
  CurrCmdline.Clear; // ilker bitiþ
  CurrCmdline.ActiveDrawBox.SaveGIS(True, AAddRecentProjects);
  if CurrCmdline.ActiveDrawBox.GIS.Modified then
  begin
    Application.MessageBox('Devam eden iþlemleri bitirip, tekrar kaydetmeyi deneyiniz.', 'Bilgi', mrOk + MB_ICONINFORMATION);
    Exit;
  end;
  CurrCmdline.ActiveDrawBox.GIS.Close;

  if CurrCmdline.ActiveDrawBox.GIS.Active then
  begin
    Application.MessageBox('Devam eden iþlemleri bitirip, tekrar kaydetmeyi deneyiniz.', 'Bilgi', mrOk + MB_ICONINFORMATION);
    Exit;
  end;

  if ACreateBackupFile then
  begin
    if TFile.Exists(ALicadProjectFile) then
    begin
      DeleteFile(ChangeFileExt(ALicadProjectFile, EXT_BACKUP)); // ENX FILE
      RenameFile(ALicadProjectFile, ChangeFileExt(ALicadProjectFile, EXT_BACKUP));
    end;
  end;

  //TZipFile.ZipDirectoryContents(ALicadProjectFile, AFileName, zcStored);
  FZip := TZipFile.Create;
  //FZip.Open(ALicadProjectFile, zmWrite);
  ListFiles := TDirectory.GetFiles(AFileName, '*', TSearchOption.soAllDirectories);

  if Licad.Settings.Compression then
    AZipComp := zcDeflate
  else
    AZipComp := zcStored;

  if AShowProgress then
  begin
    FProgressForm := TfmGosterge.Create(nil);
    FProgressForm.Execute(High(ListFiles), ExtractFileName(ALicadProjectFile) + ' Kaydediliyor...', False);
    FZip.ZipDirectoryContents(ALicadProjectFile, AFileName, AZipComp, OnZipProgress);
    //FZip.ZipDirectoryContents(ALicadProjectFile, AFileName, AZipComp, OnZipProgress);
  end
  else
  begin
    FZip.ZipDirectoryContents(ALicadProjectFile, AFileName, AZipComp, nil);
    //FZip.ZipDirectoryContents(ALicadProjectFile, AFileName, AZipComp, nil);
  end;
  FZip.Close;
  FZip.Free;

  CurrCmdline.ActiveDrawBox.GIS.Open;
  CurrCmdline.ActiveDrawBox.GIS.RefreshViewports;
end;


procedure TLicadProjectFile.OnZipProgress(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  FProgressForm.Gosterge;
end;

end.


