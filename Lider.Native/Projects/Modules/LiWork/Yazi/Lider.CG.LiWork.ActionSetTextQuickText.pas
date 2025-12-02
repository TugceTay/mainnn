unit Lider.CG.LiWork.ActionSetTextQuickText;

interface

uses
  Graphics,
  Classes,
  Controls,
  SysUtils,
  Forms,
  cxLookAndFeelPainters, cxCheckGroup, cxCheckComboBox,
  cxCheckBox, cxRadioGroup, cxGridCustomView, cxDropDownEdit, cxStyles, cxGridCustomTableView,
  System.Generics.Collections,
  dxLayoutContainer,
  lxStrUtil,
  Inifiles,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.CanvasInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeoLibrary,
  Lider.CG.Com.LicadInt,
  Lider.CG.LiWork.StringOperations;

Type
  Options = Record
    ParcaAdet, HarfBoslukMik: Integer;
    YaziBoyut, YaziAci, SatirAralik: Double;
    FontName: String;
    HarfBosluk, YaziBol, AltiniCiz, YaziAcisi, YaziTabaka,
     YaziBoyutu, YaziItalik, YaziFontu : boolean;
End;

type
  TSetTextQuickTextAction= class(TLicgAction)
  private
    FPathString, FCbMetin1Name, FCbMetin1IndexName: String;
    FCbMetin2Name, FCbMetin2IndexName: String;
    FCbMetin3Name, FCbMetin3IndexName: String;
    FCbMetin4Name, FCbMetin4IndexName: String;
    FCbMetin5Name, FCbMetin5IndexName: String;
    FCbMetin6Name, FCbMetin6IndexName: String;
    FCbMetin7Name, FCbMetin7IndexName: String;
    FCbMetin8Name, FCbMetin8IndexName: String;
    FCbMetin9Name, FCbMetin9IndexName: String;
    FCbMetin10Name, FCbMetin10IndexName: String;
    FCbMetin11Name, FCbMetin11IndexName: String;
    FCbMetin12Name, FCbMetin12IndexName: String;
    FCbMetin13Name, FCbMetin13IndexName: String;
    FCbMetin14Name, FCbMetin14IndexName: String;
    FCbMetin15Name, FCbMetin15IndexName: String;
    FCbMetin16Name, FCbMetin16IndexName: String;
    FIniF: TIniFile;
    FCount : Integer;
    FQuickText: String;
    FAllText: ILicgEntity;
    FListText: TList<Integer>;
    FCheckedList: TList<Integer>;
    FAllTextList: ILicgEntityList;
    MyOptions: Options;
    FCurrTextTypeIsL1: Boolean;
    function CalcAllExt: TLicgExtent;
    procedure ReadDataFromIniFile;
    procedure Init;
    function WriteData(SectionName, KeyName, Value: String): Boolean;
    function CreateTextEntity(ACmdLine: TlicgBaseCmdLine;
      AEnt: ILicgEntity; aSpacing: Double): IlicgEntity;//Davulcu Ekleme
    procedure CheckBoxArray;
    procedure SettingTextArray;
    procedure ChkOnChange(Sender: TObject);
    procedure SettingTextSize(ATextList: IlicgEntityList); Overload;
    procedure SettingTextSize(AText: ILicgEntity); Overload;
    procedure SettingTextAngle(ATextList: IlicgEntityList); Overload;
    procedure SettingTextAngle(AText: IlicgEntity); Overload;
    procedure AmountOfSpace(AText: IlicgEntity); Overload;
    procedure AmountOfSpace(ATextList: IlicgEntityList); Overload;
    function AmountOfPart(AText: IlicgEntity):IlicgEntityList;
    procedure LetterOptions(ATextList: IlicgEntityList); Overload;
    procedure LetterOptions(AText: IlicgEntity); Overload;
    procedure ItalicText(ATextList: IlicgEntityList); Overload;
    procedure ItalicText(AText: IlicgEntity); Overload;
    procedure UnderlineText(ATextList: IlicgEntityList); Overload;
    procedure UnderlineText(AText: IlicgEntity); Overload;
    procedure DrawAllEnts(Ents: ILicgEntityList; RefreshExt: TlicgExtent);
    procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer; const WX, WY: Double);
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    procedure MyKeyPress(Sender: TObject; var Key: Char);
    procedure MyButtonClick(Sender: TObject);
    procedure MyContinue(Sender: TObject);
    procedure MyCbEditValueChanged(Sender: TObject);
  public
    constructor CreateAction(CmdLine: TlicgBaseCmdLine);
    destructor Destroy; override;
  end;

implementation

uses
  Lider.CG.LiWork.HazirYaziSecenekleri,
  Lider.CG.LiWork.HazirYaziAyarlari;

{ TSetTextReadyTextAction }

constructor TSetTextQuickTextAction.CreateAction(CmdLine: TlicgBaseCmdLine);
var
  I: Integer;
begin
  inherited CreateAction(CmdLine);
  FCheckedList := Tlist<Integer>.Create;
  fmHazirYaziSecenekleri := TfmHazirYaziSecenekleri.Create(nil);
  fmHazirYaziSecenekleri.btnAyarlar.OnClick := MyButtonClick;
  fmHazirYaziAyarlari := TfmHazirYaziAyarlari.Create(nil);
  Self.Cursor := crDrawCross;
  FPathString := Extractfilepath(application.ExeName) + 'Settings\HazirYaziYeni.ini';
  FIniF := TIniFile.Create(FPathString);
  Init;
  CheckBoxArray;
  SettingTextArray;
  fmHazirYaziSecenekleri.ShowModal;
  if fmHazirYaziSecenekleri.ModalResult = mrOk then
  begin
    if BastakiveSondakiBosluklariSil(fmHazirYaziSecenekleri.teTamMetin.Text) = '' then
    begin
      Self.Finished := True;
      Exit;
    end;
    OnMouseDown := MyMouseDown;
    OnMouseMove := MyMouseMove;
    OnKeyPress := MyKeyPress;
    OnContinueOperation := MyContinue;
    CmdLine.ActiveDrawBox.SetFocus; //aktif draw boxý fokuslar.
    if fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
    begin
      if MyOptions.YaziFontu = True then
        MyOptions.FontName := fmHazirYaziAyarlari.cbYaziFont.Text
      else
        MyOptions.FontName := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;

      if MyOptions.FontName = 'L1' then
      begin
        FAllText := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
        FAllText.DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
        FCurrTextTypeIsL1 := True;
      end
      else
      begin
        FAllText := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
        FAllText.DrawTools.FontTool.Name := MyOptions.FontName;
        FCurrTextTypeIsL1 := False;
      end;

      FAllText.Geometry.Points[0] := AsCoor(0 , 0);
      FAllText.AsTextValue.Text := fmHazirYaziSecenekleri.teTamMetin.Text; //***
      FAllText.Geometry.UpdateExtension;

      if MyOptions.YaziBol = True then
      begin
        AmountOfPart(FAllText);
        if MyOptions.YaziBoyutu = True then
          SettingTextSize(FAllTextList);
        if MyOptions.YaziAcisi = True then
          SettingTextAngle(FAllTextList);
        if MyOptions.AltiniCiz = True then
          UnderlineText(FAllTextList);
        if MyOptions.YaziItalik = True then
          ItalicText(FAllTextList);
        if MyOptions.HarfBosluk = True then
          AmountOfSpace(FAllTextList);
        LetterOptions(FAllTextList);
      end
      else
      begin
        if MyOptions.YaziBoyutu = True then
          SettingTextSize(FAllText);
        if MyOptions.YaziAcisi = True then
          SettingTextAngle(FAllText);
        if MyOptions.AltiniCiz = True then
          UnderlineText(FAllText);
        if MyOptions.YaziItalik = True then
          ItalicText(FAllText);
        if MyOptions.HarfBosluk = True then
          AmountOfSpace(FAllText);
        LetterOptions(FAllText);
      end;
    end
    else
    begin
      MyOptions.FontName := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
      if MyOptions.FontName = 'L1' then
      begin
        FAllText := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
        FAllText.DrawTools.FontTool.Name := 'Courier New';
        FCurrTextTypeIsL1 := True;
      end
      else
      begin
        FAllText := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
        FAllText.DrawTools.FontTool.Name := MyOptions.FontName;
        FCurrTextTypeIsL1 := False;
      end;

      FAllText.Geometry.Points[0] := AsCoor(0 , 0);
      FAllText.AsTextValue.Text := fmHazirYaziSecenekleri.teTamMetin.Text;
      FAllText.Geometry.UpdateExtension;
    end;
  end
  else
    Self.Finished := True;
end;

function TSetTextQuickTextAction.CreateTextEntity(ACmdLine: TlicgBaseCmdLine;
  AEnt: ILicgEntity; aSpacing: Double): IlicgEntity;
begin
  if ACmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1' then
  begin
    Result := Licad.CreateEntityFactory.MakeEntity(idVectorialText, 0, _3D);
    Result.Geometry.Points[0] := AEnt.Geometry.Points[0];
  end
  else
  begin
    Result := Licad.CreateEntityFactory.MakeEntity(idText, 0, _3D);
  end;
  Result.DrawTools.Assign(AEnt.DrawTools);
  Result.AsTextValue.Text := AEnt.AsTextValue.Text;
  Result.DrawTools.FontTool.Name := ACmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
  Result.DrawTools.FontTool.CharWidthFactor := aSpacing;
end;

destructor TSetTextQuickTextAction.Destroy;
begin
  if Assigned(FIniF) then
    FreeAndNil(FIniF);
  FAllText := nil;
  FCheckedList.Clear;
  if Assigned(fmHazirYaziSecenekleri) then
    FreeAndNil(fmHazirYaziSecenekleri);
  if Assigned(fmHazirYaziAyarlari) then
    FreeAndNil(fmHazirYaziAyarlari);
  inherited;
end;

procedure TSetTextQuickTextAction.LetterOptions(ATextList: IlicgEntityList);
var
  I: Integer;
begin
  for I := 0 to ATextList.Count - 1 do
  begin
    if  fmHazirYaziAyarlari.rbIlkHarfBuyuk.Checked then
      ATextList[I].AsTextValue.Text := IlkHarflerBuyuk(ATextList[I].AsTextValue.Text)
    else if fmHazirYaziAyarlari.rbHarflerBuyuk.Checked then
      ATextList[I].AsTextValue.Text := HarfleriBuyut(ATextList[I].AsTextValue.Text)
    else if fmHazirYaziAyarlari.rbHarflerKucuk.Checked then
      ATextList[I].AsTextValue.Text := HarfleriKucult(ATextList[I].AsTextValue.Text)
    else
      ATextList[I].AsTextValue.Text := ATextList[I].AsTextValue.Text;
  end;
end;

procedure TSetTextQuickTextAction.LetterOptions(AText: IlicgEntity);
begin
  if  fmHazirYaziAyarlari.rbIlkHarfBuyuk.Checked then
    AText.AsTextValue.Text := IlkHarflerBuyuk(AText.AsTextValue.Text)
  else if fmHazirYaziAyarlari.rbHarflerBuyuk.Checked then
    AText.AsTextValue.Text := HarfleriBuyut(AText.AsTextValue.Text)
  else if fmHazirYaziAyarlari.rbHarflerKucuk.Checked then
    AText.AsTextValue.Text := HarfleriKucult(AText.AsTextValue.Text)
  else
    AText.AsTextValue.Text := AText.AsTextValue.Text;
end;

procedure TSetTextQuickTextAction.MyKeyPress(Sender: TObject; var Key: Char);
begin
   if Key = #27 then
  begin
    Self.Finished := True;
    Exit;
  end;
end;

procedure TSetTextQuickTextAction.MyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; const WX,
  WY: Double);
var
  Rc, I: Integer;
begin
  if Button = mbleft then
  begin
    if fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
    begin
      if MyOptions.YaziBol = True then
      begin
        for I := 0 to FAllTextList.Count-1  do
        begin
          CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
          Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(FAllTextList[I]);
          CurrCmdLine.ActiveDrawBox.Undo.AddUndo(
            CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
          CurrCmdLine.ActiveDrawBox.RepaintExtent(FAllTextList[I].Extent);
          CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
        end;
      end
      else
      begin
        CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
        Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(FAllText);
        CurrCmdLine.ActiveDrawBox.Undo.AddUndo(
          CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
        CurrCmdLine.ActiveDrawBox.RepaintExtent(FAllText.Extent);
        CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
      end;
    end
    else
    begin
      CurrCmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
      //FAllText.Geometry.Points[0] := asCoor(Wx, Wy);
      Rc := CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer.AddEntity(FAllText);
      CurrCmdLine.ActiveDrawBox.Undo.AddUndo(
        CurrCmdLine.ActiveDrawBox.GIS.CurrentLayer, Rc, uadelete);
      CurrCmdLine.ActiveDrawBox.Undo.EndUndo;
      CurrCmdLine.ActiveDrawBox.RepaintExtent(FAllText.Extent);
    end;
  end
  else
    Self.Finished := True;
end;

procedure TSetTextQuickTextAction.MyMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer; const WX, WY: Double);
var
  I: Integer;
  P: TLicgCoor;
begin
  FAllText.Geometry.UpdateExtension;
  if fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
  begin
    if MyOptions.YaziBol = True then
    begin
      //for I := 0 to FAllTextList.Count -1 do
      P := asCoor(Wx, Wy);
      //DrawAllEnts(FAllTextList);
      AssingingCoordinateToText(P, FAllText, FListText, FAllTextList, MyOptions.SatirAralik);
      DrawAllEnts(FAllTextList, Cmdline.ActiveDrawBox.Grapher.CurrentParams.VisualWindow); //ekranda görünen pencereyi extent alýr.
    end
    else
    begin
//      CmdLine.ActiveDrawBox.DrawEntityRubberBand(FAllText);
      CmdLine.ActiveDrawBox.RefreshExtent(Cmdline.ActiveDrawBox.Grapher.CurrentParams.VisualWindow);
      FAllText.Geometry.Points[0] := asCoor(Wx, Wy);
//      FAllText.Geometry.UpdateExtension;
      CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(FAllText, False, False);
    end;
  end
  else
  begin
//    CmdLine.ActiveDrawBox.DrawEntityRubberBand(FAllText);
    CmdLine.ActiveDrawBox.RefreshExtent(Cmdline.ActiveDrawBox.Grapher.CurrentParams.VisualWindow);
    FAllText.Geometry.Points[0] := asCoor(Wx, Wy);
//    FAllText.Geometry.UpdateExtension;
    CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(FAllText, False, False);
  end;
end;

procedure TSetTextQuickTextAction.MyMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; const WX,
  WY: Double);
begin

end;

procedure TSetTextQuickTextAction.ReadDataFromIniFile;
var
I: Integer;
begin
  if Not FileExists(FPathString) then
    Exit;

  FIniF.ReadSection(FCbMetin1Name, fmHazirYaziSecenekleri.cbMetin1.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin1.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin1.ItemIndex := FInif.ReadInteger(FCbMetin1IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin2Name, fmHazirYaziSecenekleri.cbMetin2.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin2.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin2.ItemIndex := FInif.ReadInteger(FCbMetin2IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin3Name, fmHazirYaziSecenekleri.cbMetin3.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin3.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin3.ItemIndex := FInif.ReadInteger(FCbMetin3IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin4Name, fmHazirYaziSecenekleri.cbMetin4.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin4.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin4.ItemIndex := FInif.ReadInteger(FCbMetin4IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin5Name, fmHazirYaziSecenekleri.cbMetin5.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin5.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin5.ItemIndex := FInif.ReadInteger(FCbMetin5IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin6Name, fmHazirYaziSecenekleri.cbMetin6.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin6.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin6.ItemIndex := FInif.ReadInteger(FCbMetin6IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin7Name, fmHazirYaziSecenekleri.cbMetin7.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin7.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin7.ItemIndex := FInif.ReadInteger(FCbMetin7IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin8Name, fmHazirYaziSecenekleri.cbMetin8.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin8.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin8.ItemIndex := FInif.ReadInteger(FCbMetin8IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin9Name, fmHazirYaziSecenekleri.cbMetin9.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin9.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin9.ItemIndex := FInif.ReadInteger(FCbMetin9IndexName, 'Index', -1);
  end;

  FIniF.ReadSection(FCbMetin10Name, fmHazirYaziSecenekleri.cbMetin10.Properties.Items);
  if fmHazirYaziSecenekleri.cbMetin10.Properties.Items.Count > 0 then
  begin
    fmHazirYaziSecenekleri.cbMetin10.ItemIndex := FInif.ReadInteger(FCbMetin10IndexName, 'Index', -1);
  end;
end;

procedure TSetTextQuickTextAction.DrawAllEnts(Ents: ILicgEntityList; RefreshExt: TlicgExtent);
var
 I: Integer;
begin
  CmdLine.ActiveDrawBox.RefreshExtent(RefreshExt);
  For I := 0 to Ents.Count - 1 do
    CmdLine.ActiveDrawBox.DrawEntity2DRubberBand(Ents[I]);  //** kýrmýzý yazý
   // CmdLine.ActiveDrawBox.RefreshExtent(Ents[I].Extent)
end;

procedure TSetTextQuickTextAction.ItalicText(ATextList: IlicgEntityList);
var
 I: Integer;
begin
  for I := 0 to FAllTextList.Count-1 do
  begin
    FAllTextList[I].DrawTools.FontTool.Style :=
      FAllTextList[I].DrawTools.FontTool.Style + [fsItalic];
  end;
end;

procedure TSetTextQuickTextAction.Init;
begin
  FCbMetin1Name := '' + fmHazirYaziSecenekleri.cbMetin1.Name + '';
  FCbMetin1IndexName := 'Metin1Index' ;

  FCbMetin2Name := '' + fmHazirYaziSecenekleri.cbMetin2.Name + '';
  FCbMetin2IndexName := 'Metin2Index' ;

  FCbMetin3Name := '' + fmHazirYaziSecenekleri.cbMetin3.Name + '';
  FCbMetin3IndexName := 'Metin3Index' ;

  FCbMetin4Name := '' + fmHazirYaziSecenekleri.cbMetin4.Name + '';
  FCbMetin4IndexName := 'Metin4Index' ;

  FCbMetin5Name := '' + fmHazirYaziSecenekleri.cbMetin5.Name + '';
  FCbMetin5IndexName := 'Metin5Index' ;

  FCbMetin6Name := '' + fmHazirYaziSecenekleri.cbMetin6.Name + '';
  FCbMetin6IndexName := 'Metin6Index' ;

  FCbMetin7Name := '' + fmHazirYaziSecenekleri.cbMetin7.Name + '';
  FCbMetin7IndexName := 'Metin7Index' ;

  FCbMetin8Name := '' + fmHazirYaziSecenekleri.cbMetin8.Name + '';
  FCbMetin8IndexName := 'Metin8Index' ;

  FCbMetin9Name := '' + fmHazirYaziSecenekleri.cbMetin9.Name + '';
  FCbMetin9IndexName := 'Metin9Index' ;

  FCbMetin10Name := '' + fmHazirYaziSecenekleri.cbMetin10.Name + '';
  FCbMetin10IndexName := 'Metin10Index' ;

  {FCbMetin11Name := '' + fmHazirYaziSecenekleri.cbMetin11.Name + '';
  FCbMetin11IndexName := 'Metin11Index' ;

  FCbMetin12Name := '' + fmHazirYaziSecenekleri.cbMetin12.Name + '';
  FCbMetin12IndexName := 'Metin12Index' ;

  FCbMetin13Name := '' + fmHazirYaziSecenekleri.cbMetin13.Name + '';
  FCbMetin13IndexName := 'Metin13Index' ;

  FCbMetin14Name := '' + fmHazirYaziSecenekleri.cbMetin14.Name + '';
  FCbMetin14IndexName := 'Metin14Index' ;

  FCbMetin15Name := '' + fmHazirYaziSecenekleri.cbMetin15.Name + '';
  FCbMetin15IndexName := 'Metin15Index' ;

  FCbMetin16Name := '' + fmHazirYaziSecenekleri.cbMetin16.Name + '';
  FCbMetin16IndexName := 'Metin16Index' ; }

  ReadDataFromIniFile;
  fmHazirYaziSecenekleri.cbMetin1.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin2.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin3.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin4.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin5.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin6.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin7.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin8.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin9.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin10.Properties.OnEditValueChanged := MyCbEditValueChanged;
  {fmHazirYaziSecenekleri.cbMetin11.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin12.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin13.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin14.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin15.Properties.OnEditValueChanged := MyCbEditValueChanged;
  fmHazirYaziSecenekleri.cbMetin16.Properties.OnEditValueChanged := MyCbEditValueChanged;}
end;

procedure TSetTextQuickTextAction.ItalicText(AText: IlicgEntity);
begin
  FAllText.DrawTools.FontTool.Style :=
    FAllText.DrawTools.FontTool.Style + [fsItalic];
end;

procedure TSetTextQuickTextAction.SettingTextAngle(ATextList: IlicgEntityList);
var
I: Integer;
begin
  for I := 0 to ATextList.Count-1 do
    ATextList[I].DrawTools.FontTool.Angle := MyOptions.YaziAci;
end;

procedure TSetTextQuickTextAction.SettingTextAngle(AText: IlicgEntity);
begin
  AText.DrawTools.FontTool.Angle := MyOptions.YaziAci;
end;

procedure TSetTextQuickTextAction.SettingTextArray;
begin
  MyOptions.YaziBoyut :=
    fmHazirYaziAyarlari.seYBoyut.Value * CurrCmdLine.ActiveDrawBox.GIS.ProjectScale / 1000;
  MyOptions.ParcaAdet := fmHazirYaziAyarlari.seParcaAdet.Value;
  MyOptions.YaziAci := fmHazirYaziAyarlari.seAci.Value;
  MyOptions.SatirAralik := fmHazirYaziAyarlari.seSatirAraligi.Value;
  MyOptions.HarfBoslukMik := fmHazirYaziAyarlari.seBosluk.Value;
  MyOptions.HarfBosluk :=
   TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[0]).Checked;
  MyOptions.YaziBol :=
   TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[1]).Checked;
  MyOptions.AltiniCiz :=
   TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[2]).Checked;
  MyOptions.YaziAcisi :=
  TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[3]).Checked;
  MyOptions.YaziBoyutu :=
  TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[4]).Checked;
  MyOptions.YaziItalik :=
  TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[5]).Checked;
  MyOptions.YaziFontu :=
  TdxLayoutCheckBoxItem(fmHazirYaziAyarlari.gbxYaziSecenek[6]).Checked;
end;

procedure TSetTextQuickTextAction.SettingTextSize(AText: ILicgEntity);
begin
  AText.DrawTools.FontTool.Height := MyOptions.YaziBoyut;
end;

procedure TSetTextQuickTextAction.UnderlineText(ATextList: IlicgEntityList);
var
I: Integer;
begin
  for I := 0 to FAllTextList.Count-1 do
    FAllTextList[I].DrawTools.FontTool.Style :=
      FAllTextList[I].DrawTools.FontTool.Style + [fsUnderline];
end;

procedure TSetTextQuickTextAction.UnderlineText(AText: IlicgEntity);
begin
  FAllText.DrawTools.FontTool.Style :=
    FAllText.DrawTools.FontTool.Style + [fsUnderline];
end;

function TSetTextQuickTextAction.WriteData(SectionName, KeyName,
  Value: String): Boolean;
begin
  Result := False;
  if FIniF.ValueExists(SectionName, KeyName) then
  begin
    if KeyName = 'Index' then
    begin
      FInif.DeleteKey(SectionName, KeyName);
      FIniF.WriteString(SectionName, KeyName, Value);
    end;
  end
  else
  begin
    FIniF.WriteString(SectionName, KeyName, Value);
    Result := True;
  end;
  FIniF.UpdateFile;
end;

procedure TSetTextQuickTextAction.SettingTextSize(ATextList: IlicgEntityList);
var
  I: Integer;
begin
  if MyOptions.YaziBoyut <> 0 then
  begin
    for I := 0 to ATextList.Count-1 do
      ATextList[I].DrawTools.FontTool.Height := MyOptions.YaziBoyut;
  end;
end;

function TSetTextQuickTextAction.AmountOfPart(AText: IlicgEntity):IlicgEntityList ;
var
  EntList: IlicgEntityList;
begin
  FListText := SetTextSeparate(AText.AsTextValue.Text, MyOptions.ParcaAdet);
  FAllTextList := CreateTextEntityList(FListText, AText, MyOptions.ParcaAdet);
  FAllTextList := AssingingCoordinateToText(AText.Centroid, AText, FListText, FAllTextList, MyOptions.SatirAralik);
//  FAllTextList := Result;
end;

procedure TSetTextQuickTextAction.AmountOfSpace(AText: IlicgEntity);
var
  I: Integer;
  S, TempS: String;
  Bosluk: String;
begin
  Bosluk := ' ';
  TempS := '';
  S := AText.AsTextValue.Text;
  for I := 0 to MyOptions.HarfBoslukMik-1 do
   Bosluk := ' ' + Bosluk;
  for I := 1 to Length(S) do
    TempS := TempS + S[I] + Bosluk;
  AText.AsTextValue.Text := BastakiveSondakiBosluklariSil(TempS);
end;

procedure TSetTextQuickTextAction.AmountOfSpace(ATextList: IlicgEntityList);
var
  I, J: Integer;
  S, TempS: String;
  Bosluk: String;
begin
  Bosluk := ' ';
  TempS := ' ';
  for I := 0 to MyOptions.HarfBoslukMik-1 do
    Bosluk := ' ' + Bosluk;
  for I := 0 to ATextList.Count-1 do
  begin
    S := ATextList[I].AsTextValue.Text;
    for J := 1 to Length(S) do
    begin
      TempS := TempS + S[J] + Bosluk;
      ATextList[I].AsTextValue.Text := BastakiveSondakiBosluklariSil(TempS);
    end;
  end;
end;

function TSetTextQuickTextAction.CalcAllExt: TLicgExtent;
var
  I: Integer;
begin
  Result := _NULL_EXTENT;
  if Assigned(FAllText) then
    CalcMaxMinBounds(Result, FAllText.Extent);
  if Assigned(FAllTextList) then
    for I := 0 to FAllTextList.Count - 1 do
      CalcMaxMinBounds(Result, FAllTextList[I].Extent);
  Result.LowerLeft.X := Result.LowerLeft.X - (Result.UpperRight.X - Result.LowerLeft.X);
  Result.LowerLeft.Y := Result.LowerLeft.Y - (Result.UpperRight.Y - Result.LowerLeft.Y);
  Result.UpperRight.X := Result.UpperRight.X + (Result.UpperRight.X - Result.LowerLeft.X);
  Result.UpperRight.Y := Result.UpperRight.Y + (Result.UpperRight.Y - Result.LowerLeft.Y);
end;

procedure TSetTextQuickTextAction.CheckBoxArray;
begin
  fmHazirYaziSecenekleri.chkMetin1.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin2.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin3.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin4.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin5.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin6.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin7.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin8.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin9.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin10.Properties.OnEditValueChanged := ChkOnChange;
  {fmHazirYaziSecenekleri.chkMetin11.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin12.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin13.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin14.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin15.Properties.OnEditValueChanged := ChkOnChange;
  fmHazirYaziSecenekleri.chkMetin16.Properties.OnEditValueChanged := ChkOnChange;}
end;

procedure TSetTextQuickTextAction.ChkOnChange(Sender: TObject);
var
  I, J, Index, TempIndex: Integer;
begin
  //FCount := 0;
  if TcxCheckBox(Sender).Checked then
  begin
    Inc(FCount);
    TcxCheckBox(Sender).Caption := IntToStr(FCount);
    FCheckedList.Add(TcxCheckBox(Sender).Tag);
     for I := 0 to fmHazirYaziSecenekleri.IcMain.ControlCount - 1 do
    begin
      if fmHazirYaziSecenekleri.IcMain.Controls[I] is TcxComboBox then
      begin
        if TcxCheckBox(Sender).Tag = TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[I]).Tag then
        begin
          FQuickText := FQuickText + ' ' + TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[I]).Text; //**
          fmHazirYaziSecenekleri.teTamMetin.Text :=  FQuickText;
          Break;
        end;
      end;
    end;
  end
  else
  begin
    fmHazirYaziSecenekleri.teTamMetin.Text := '';
    FQuickText := '';
    Index := StrToInt(TcxCheckBox(Sender).Caption) - 1;
    TempIndex := Index + 1;
    for I := Index + 1 to FCheckedList.Count - 1 do
    begin
      for J := 0 to fmHazirYaziSecenekleri.IcMain.ControlCount - 1 do
      begin
        if fmHazirYaziSecenekleri.IcMain.Controls[J] is TcxCheckBox then
          if TcxCheckBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Tag = FCheckedList[I] then
          begin
//          TempIndex := StrToInt(TcxCheckBox(fmHazirYaziSecenekleri.pnlOrta.Controls[J]).Caption) - 1;
            TcxCheckBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Caption := FloatToStr(TempIndex);
            Inc(TempIndex);
            Break;
          end;
      end;
    end;
    FCheckedList.Delete(Index);
    Dec(FCount);
    TcxCheckBox(Sender).Caption := '0';
    for I := 0 to FCheckedList.Count - 1 do
    begin
      for J := 0 to fmHazirYaziSecenekleri.IcMain.ControlCount - 1 do
      begin
        if fmHazirYaziSecenekleri.IcMain.Controls[J] is TcxComboBox then
        begin
          if FCheckedList[I] = TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Tag then
            begin
              FQuickText := FQuickText + ' ' + TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Text; //**
              fmHazirYaziSecenekleri.teTamMetin.Text :=  FQuickText;
            end;
        end;
      end;
    end;
  end;
end;

{ TMyEvents }

procedure TSetTextQuickTextAction.MyButtonClick(Sender: TObject);
begin
  fmHazirYaziAyarlari.ShowModal;
  if fmHazirYaziAyarlari.ModalResult = mrOk then
  begin
//  if fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
      SettingTextArray;
  end
  else
    Exit;
//  if MyOptions.HarfBosluk = cbsChecked then

end;

procedure TSetTextQuickTextAction.MyCbEditValueChanged(Sender: TObject);
var
  Text: String;
  I, J, ItemIndex: Integer;
  FindFlag: Boolean;
begin
  Text := TcxComboBox(Sender).Text;
  Text := Text.Trim;
  FindFlag := False;
  if Text.IsEmpty then
    Exit;
  for I := 0 to TcxComboBox(Sender).Properties.Items.Count - 1 do
  begin
    if TcxComboBox(Sender).Properties.Items[I].Equals(Text) then
    begin
      FindFlag := True;
      Break;
    end;
  end;
  if Not FindFlag then
  begin
    TcxComboBox(Sender).Properties.Items.Add(Text);
  end;
  ItemIndex := TcxComboBox(Sender).ItemIndex;
  TcxComboBox(Sender).Text := Text;
  if WriteData(TcxComboBox(Sender).Name, Text, FloatToStr(TcxComboBox(Sender).Properties.Items.Count - 1)) then
    if (ItemIndex <> 0) And (ItemIndex <> TcxComboBox(Sender).Properties.Items.Count - 1) then
      Inc(ItemIndex);

  if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin1) then
    WriteData(FCbMetin1IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin2) then
    WriteData(FCbMetin2IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin3) then
    WriteData(FCbMetin3IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin4) then
    WriteData(FCbMetin4IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin5) then
    WriteData(FCbMetin5IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin6) then
    WriteData(FCbMetin6IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin7) then
    WriteData(FCbMetin7IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin8) then
    WriteData(FCbMetin8IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin8) then
    WriteData(FCbMetin9IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin10) then
    WriteData(FCbMetin10IndexName, 'Index', FloatToStr(ItemIndex))
  {else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin11) then
    WriteData(FCbMetin11IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin12) then
    WriteData(FCbMetin12IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin13) then
    WriteData(FCbMetin13IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin14) then
    WriteData(FCbMetin14IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin15) then
    WriteData(FCbMetin15IndexName, 'Index', FloatToStr(ItemIndex))
  else if TcxComboBox(Sender).Equals(fmHazirYaziSecenekleri.cbMetin16) then
    WriteData(FCbMetin16IndexName, 'Index', FloatToStr(ItemIndex))};

  FQuickText := '';
  for I := 0 to FCheckedList.Count - 1 do
  begin
    for J := 0 to fmHazirYaziSecenekleri.IcMain.ControlCount - 1 do
    begin
      if fmHazirYaziSecenekleri.IcMain.Controls[J] is TcxComboBox then
      begin
        if FCheckedList[I] = TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Tag then
          begin
            FQuickText := FQuickText + ' ' + TcxComboBox(fmHazirYaziSecenekleri.IcMain.Controls[J]).Text; //**
            fmHazirYaziSecenekleri.teTamMetin.Text :=  FQuickText;
          end;
      end;
    end;
  end;
end;

procedure TSetTextQuickTextAction.MyContinue(Sender: TObject);
var
  Spacing: Double;
  I: Integer;
begin
   {Davulcu Ekleme Baþlangýç}
  if Assigned(FAllText) And Not fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
  begin
    if fmHazirYaziSecenekleri.chkAyarlarKullan.Checked then
    begin
      if MyOptions.YaziBol = True then
      begin
        if Assigned(FAllTextList) then
        begin
          if Not (FCurrTextTypeIsL1 = (CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1')) then //Eski tip ile ayný deðilse
          begin
            for I := 0 to FAllTextList.Count-1 do
            begin
              try
                Spacing := FAllTextList[I].AsVectorialText.CharSpacing;
              except on E: Exception do
                Spacing := 0;
              end;
              FAllTextList[I] := CreateTextEntity(CmdLine, FAllTextList[I], Spacing);
              FCurrTextTypeIsL1 := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1';
            end;
          end
          else
          begin
            for I := 0 to FAllTextList.Count-1 do
              FAllTextList[I].DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
          end;
        end
        else
        begin
          FAllTextList := AmountOfPart(FAllText);
        end;
      end
      else
      begin
        if Not (FCurrTextTypeIsL1 = (CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1')) then //Eski tip ile ayný deðilse
        begin
          try
            Spacing := FAllText.AsVectorialText.CharSpacing;
          except on E: Exception do
            Spacing := 0;
          end;
          FAllText := CreateTextEntity(CmdLine, FAllText, Spacing);
          FCurrTextTypeIsL1 := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1';
        end
        else
        begin
          FAllText.DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
        end;
      end;
    end
    else
    begin
      if Not (FCurrTextTypeIsL1 = (CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1')) then //Eski tip ile ayný deðilse
      begin
        try
          Spacing := FAllText.AsVectorialText.CharSpacing;
        except on E: Exception do
          Spacing := 0;
        end;
        FAllText := CreateTextEntity(CmdLine, FAllText, Spacing);
        FCurrTextTypeIsL1 := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name = 'L1';
      end
      else
      begin
        FAllText.DrawTools.FontTool.Name := CmdLine.ActiveDrawBox.GIS.MapInfo.FontTool.Name;
      end;
    end;
  end;

  {Davulcu Ekleme Bitiþ}

end;

end.

