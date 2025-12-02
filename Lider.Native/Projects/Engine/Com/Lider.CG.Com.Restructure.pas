unit Lider.CG.Com.Restructure;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  Dialogs,
  DBCtrls,
  DBGrids,
  DB,
  Menus,
  Actions,
  ActnList,
  Grids,
  cxGraphics,
  cxControls,
  cxLookAndFeels,
  cxLookAndFeelPainters,
  cxContainer,
  cxEdit,
  dxSkinsCore,
  cxButtons,
  cxNavigator,
  cxDBNavigator,
  cxLabel,
  cxGroupBox,
  Lider.CG.Com.StringLi,
  Lider.CG.Com.Base,
  Lider.CG.Com.DB,
  Lider.CG.Com.GIS, dxUIAClasses;

type
  TfmRestructDlg = class(TForm)
    DataSource1: TDataSource;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    cxGroupBox1: TcxGroupBox;
    DBNavigator1: TDBNavigator;
    DBGrid2: TDBGrid;
    Panel2: TPanel;
    LblStatus: TcxLabel;
    cxDBNavigator1: TcxDBNavigator;
    BtnAddFlds: TcxButton;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    procedure DBGrid1ColEnter(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DBGrid2ColEnter(Sender: TObject);
    procedure BtnRegenClick(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure cxDBNavigator1ButtonsButtonClick(Sender: TObject; AButtonIndex:
      Integer; var ADone: Boolean);
  private
    { Private declarations }
    FisShowMessage: boolean;
    DesignList: TList;
    FLayer: TlicgBaseLayer;
    FisCorrect: Boolean;

    bFieldsModified: boolean;
    bIndexModified: boolean;
    FullName: string;
    FDesignTable: TDataset;
    FChangingValue: Boolean;
    procedure dsDesignBeforePost(DataSet: TDataSet);
    procedure dsDesignAfterEdit(DataSet: TDataSet);
    procedure dsDesignAfterDelete(DataSet: TDataSet);
    procedure dsDesignBeforeEdit(DataSet: TDataSet);
    procedure dsDesignBeforeDelete(DataSet: TDataSet);
    //procedure FillWithDBInfo(const Filename: string; DesignList, IndexList:
      //TList);
  public
    { Public declarations }
    rest_type: integer;
    IndexList: TList;
    function Enter(Layer: TlicgBaseLayer; isSModal: boolean): Word;
  end;

procedure _ReStructureLayer(Layer: TlicgBaseLayer);

function AddDBFColumn(const ALayer: TlicgBaseLayer; AFL: TStrings; AShowProgress: Boolean = False): Boolean;

function RemoveDBFColumn(const ALayer: TlicgBaseLayer; const AFieldName: string; AShowProgress: Boolean = False): Boolean;

implementation

{$R *.DFM}

uses
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.System,
  Lider.CG.Com.AddIndex;

type
  PIndexingRec = ^TIndexingRec;

  TIndexingRec = record
    IndexName: string;
    KeyExpression: string;
    ForExpression: string;
    Unique: TlicgIndexUnique;
    SortStatus: TlicgSortStatus;
  end;

  PDesignRecord = ^TDesignRecord;

  TDesignRecord = record
    FieldName: string[10];
    FieldType: AnsiChar;
    FieldSize: Integer;
    FieldDec: Integer;
    Recno: Integer;
    IsNewField: Boolean;
    NewFieldName: string[10];
  end;

resourcestring
  SUnspecifiedFieldType = 'Kolon türü hatalý !';
  SRestructureCaption = 'Tabaka %s';
  SRestMsg1 = 'Kolon adý (En çok 10 karakter),' + CrLf +
    'Ýsim karakterle (A-Z) baþlamalý, özel karakterler kullanýlmamalýdýr';
  SRestmsg2 = 'Kolon türünü seçiniz';
  SRestMsg3 = 'Geniþlik (1 - 254).';
  SRestMsg4 = 'Ondalýk hane sayýsý';
  SRestLongWarning = 'DBF Tablonuz deðiþecek, emin misiniz';
  SNewIndexTitle = 'Indeks Adý';
  SNewIndexQuery = 'Lütfen indeks adýný giriniz';
  SDuplicateIndex = 'Bu isimde bir indeks var';
  SInvalidDBFExpression = 'Geçersiz ifade !';
  SIncorrectFieldName = 'Kolon adý hatalý !';
  SIncorrectFieldType = 'Kolon türü hatalý !' + CrLf +
    'Bunlardan biri olmalý: C=Karakter,L=Mantýksal,D=Tarih,N=Sayýsal,M=MEMO,B=BINARY,G=GRAPHIC';
  SIncorrectFieldSize = 'Kolon geniþlik deðeri hatalý !' + CrLf + '1-254 arasýnda olmalý';
  SIncorrectDecimals = 'Ondalýk basamak deðeri hatalý !' + CrLf + '0-10 arasýnda olmalý';
  SFieldNameCaption = 'Kolon adý (En çok 10 karakter),' + CrLf +
    'Ýsim karakterle (A-Z) baþlamalý, özel karakterler kullanýlmamalýdýr';
  SFieldSelectF2 = 'Kolon türünü seçiniz';
  SFieldSizeCaption = 'Kolon geniþliði (1 - 254).';
  SFieldDecCaption = 'Ondalýk hane sayýsý';
  SIndexRegenInfo = 'Indeksler oluþturuldu';
  SRestAborted = 'Yeniden yapýlandýrma sýrasýnda hata oluþtu. ' + CrLf +
    'Ýþlem tamamlanamadý' + CrLf + 'Hata: %s';

const
  Digits =['0'..'9'];
  PrimaryIdentChars =['A'..'Z', '_', #127..#255];
  IdentChars = PrimaryIdentChars + Digits;

procedure FillWithDBInfo(aLayer: TlicgBaselayer; const Filename: string;
  DesignList, IndexList: TList);
var
  I, L, D: integer;
  T: AnsiChar;
  Fullname, fname: string;
  DesignRecord: PDesignRecord;
  IndexRecord: PIndexingRec;
  ADBFTable: TlicgBaseTable;
begin
  FullName := ChangeFileExt(FileName, '');
  ADBFTable := aLayer.GIS.BaseTableClass.Create(aLayer.GIS, FullName, true, true);
  try

    for I := 1 to ADBFTable.FieldCount do
    begin
      fname := ADBFTable.Field(I);
      T := ADBFTable.FieldType(I);
      L := ADBFTable.FieldLen(I);
      D := ADBFTable.FieldDec(I);
      New(DesignRecord);
      FillChar(DesignRecord^, SizeOf(TDesignRecord), 0);
      with DesignRecord^ do
      begin
        FieldName := fname;
        FieldType := T;
        FieldSize := L;
        FieldDec := D;
      end;
      DesignList.Add(DesignRecord);
      DesignRecord^.RecNo := I;
    end;
    // Indexes
    ADBFTable.Index(FullName, '');
//    for I := 1 to ADBFTable.IndexCount do
    for I := 0 to ADBFTable.IndexCount - 1 do
    begin
      New(IndexRecord);
      with IndexRecord^ do
      begin
        IndexName := ADBFTable.IndexTagName(I);
        KeyExpression := ADBFTable.IndexExpression(I);
        ForExpression := ADBFTable.IndexFilter(I);

        if ADBFTable.IndexUnique(I) then
          Unique := Lider.CG.Com.GIS.iuUnique
        else
          Unique := Lider.CG.Com.GIS.iuDuplicates;

        if ADBFTable.IndexAscending(I) then
          SortStatus := Lider.CG.Com.GIS.ssAscending
        else
          SortStatus := Lider.CG.Com.GIS.ssDescending;
      end;
      IndexList.Add(IndexRecord);
    end;
  finally
    ADBFTable.Free;
  end;
end;

procedure ClearIndexList(IndexList: TList);
var
  I: integer;
begin
  for I := 0 to IndexList.Count - 1 do
    Dispose(PIndexingRec(IndexList[I]));
  IndexList.Clear;
end;

procedure ClearDesignList(DesignList: TList);
var
  I: integer;
begin
  for I := 0 to DesignList.Count - 1 do
    Dispose(PDesignRecord(DesignList[I]));
  DesignList.Clear;
end;

  // restructure main procedure

function DoRestructure(Layer: TlicgBaseLayer; DesignList, IndexList: TList;
  NewFieldList: TStringList; showProgress: boolean = false): Boolean;
var
  FDest, FSource: Integer;
  Recno, cnt, I, J: integer;
  Source, Destination: string;
  TmpTable, SourceTable, DestTable: TlicgBaseTable;
  S, FullName, filename, TemporaryDir: string;
  MapSource, MapDest: TStringList;
  DesignRecord: PDesignRecord;
  IndexRecord: PIndexingRec;
  usevisualbar: boolean;
  FieldList: TStringList;
  TempPath: array[0..1023] of Ansichar; // ilker deðiþtirme
  Gis: TlicgBaseGis;
  NRecs: Integer;
begin

  Result := True;

  FullName := Layer.FileName;

  GetTempPathA(1023, TempPath);
  TemporaryDir := AddSlash(TempPath);

  Gis := Layer.GIS;

  with gis.basetableclass.CreateNoOpen(Gis) do
  try
    DBDropTable(TemporaryDir + Layer.displayName);
  finally
    free;
  end;

  FieldList := TStringList.Create;
  for I := 0 to NewFieldList.Count - 1 do
  begin
    J := AnsiPos(';', NewFieldList[I]);
    if J = 0 then
      J := Length(NewFieldList[I]);
    FieldList.Add(Copy(NewFieldList[I], 1, J - 1));
  end;

  { Check for duplicated field names }
  for I := 0 to FieldList.Count - 1 do
    for J := 0 to FieldList.Count - 1 do
      if (I <> J) and (FieldList[I] = FieldList[J]) then
      begin
        if showProgress then
          ShowMessage(Format('%s daha önce tanýmlanmýþ !', [FieldList[I]]));
        FieldList.Free;
        Result := False;
        Exit;
      end;
  FieldList.Free;

  Screen.Cursor := crHourglass;

  try
    with gis.basetableclass.CreateNoOpen(Gis) do
    try
      DBCreateTable(TemporaryDir + Layer.DisplayName, NewFieldList);
    finally
      free;
    end;

    { Indexes }
    if IndexList.count > 0 then
    begin
      Filename := TemporaryDir + Layer.DisplayName;
      TmpTable := gis.BaseTableClass.Create(Gis, FileName, true, true);
      try
        for cnt := 0 to IndexList.count - 1 do
        begin
          IndexRecord := PIndexingRec(IndexList[cnt]);
          with IndexRecord^ do
          begin
            TmpTable.IndexOn(TemporaryDir + Layer.DisplayName, IndexName,
              KeyExpression, ForExpression, Unique, SortStatus);
          end;
        end;
      finally
        TmpTable.Free;
      end;
    end;
  except
    with gis.basetableclass.CreateNoOpen(Gis) do
    try
      DBDropTable(TemporaryDir + Layer.DisplayName);
    finally
      free;
    end;
    Screen.Cursor := crDefault;
    raise;
  end;

  Recno := 0;
  MapSource := TStringList.Create;
  MapDest := TStringList.Create;
  try
    with gis.basetableclass.CreateNoOpen(Gis) do
    try
      if not DBTableExists(FullName) then
        Exit;
      if not DBTableExists(TemporaryDir + Layer.DisplayName) then
        Exit;
    finally
      free;
    end;

    Layer.Close;

    SourceTable := gis.BaseTableClass.Create(Gis, AddSlash(ExtractFilePath(FullName))
      + Layer.DisplayName, True, False);
    SourceTable.SetUseDeleted(True);

    DestTable := gis.BaseTableClass.Create(Gis, AddSlash(TemporaryDir) + Layer.DisplayName,
      True, False);
    DestTable.SetUseDeleted(True);

    for I := 0 to DesignList.Count - 1 do
    begin
      DesignRecord := PDesignRecord(DesignList[I]);
      if not DesignRecord^.IsNewField then
      begin
        MapSource.Add(DesignRecord^.FieldName);
        MapDest.Add(DesignRecord^.NewFieldName);
      end;
    end;
    for I := 0 to NewFieldList.Count - 1 do
    begin
      J := AnsiPos(';', NewFieldList[I]);
      if J = 0 then
        J := Length(NewFieldList[I]);
      S := Copy(NewFieldList[I], 1, J - 1);
      if MapDest.IndexOf(s) = -1 then
      begin
        MapDest.Add(s);
        MapSource.Add(s);
      end;
    end;
    try
      try
        UseVisualBar := SourceTable.RecordCount > 0;
        if UseVisualBar and showProgress then
        begin
          Licad.ProgressStart(SourceTable.RecordCount, SRestructuring);
          Recno := 1;
        end;
        NRecs := 0;
        SourceTable.First;
        while not SourceTable.Eof do
        begin
          Inc(NRecs);
          DestTable.Append(NRecs);
          for cnt := 0 to MapDest.Count - 1 do
          begin
            FDest := DestTable.FieldNo(MapDest[cnt]);
            FSource := SourceTable.FieldNo(MapSource[cnt]);
            if not ((FSource = 0) or (FDest = 0)) then
            begin
              DestTable.Edit;
              DestTable.AssignFrom(SourceTable, FSource, FDest);
              DestTable.Post;
            end;
          end;
          SourceTable.Next;
          if UseVisualBar and showProgress then
          begin
            Inc(Recno);
            Licad.ProgressPosition(Recno);
          end;
        end;
        SourceTable.Active := false;
        DestTable.Active := false;
        if UseVisualBar and showProgress then
          Licad.ProgressEnd;



        { erase old files }

        with gis.basetableclass.CreateNoOpen(Gis) do
        try
          DBDropTable(FullName);
        finally
          free;
        end;

        Source := TemporaryDir + Layer.DisplayName;
        Destination := FullName;

        with gis.basetableclass.CreateNoOpen(Gis) do
        try
          DBRenameTable(Source, Destination);
        finally
          free;
        end;

      except
        on E: Exception do
        begin
          ShowMessage(Format(SRestAborted, [E.Message]));
          raise;
        end;
      end;
    finally
      SourceTable.Free;
      DestTable.Free;
      Layer.Open;
    end;
  finally
    MapSource.Free;
    MapDest.Free;
    Screen.Cursor := crDefault;
  end;
end;

function AddDBFColumn(const ALayer: TlicgBaseLayer; AFL: TStrings;
  AShowProgress: Boolean = False): Boolean;

  procedure DivideString(s: ansistring; var s1, s2: ansistring; ch: ansistring);
  var
    p: integer;
  begin
    p := pos(ch, s);
    if p = 0 then
      inc(p, succ(Length(s)));
    delete(s, p, length(ch));
    s1 := copy(s, 1, p - 1);
    s2 := copy(s, p, Length(s) - p + 1);
  end;

  function Parse(var s: ansistring; const ch: ansistring): ansistring;
  var
    s1, s2: ansistring;
  begin
    DivideString(s, s1, s2, ch);
    s := s2;
    Parse := s1;
  end;

var
  i, rc: integer;
  f1, f2, f3, f4, str: Ansistring; //li2016
  lyrSL, newFL: TStrings;
  DesignList, IndexList: TList;
  DesignRecord: PDesignRecord;
  IndexRecord: PIndexingRec;
begin
  result := false;

//  aLayer.DBTable.Active := true;

  DesignList := TList.Create;
  IndexList := TList.create;
  newFL := TStringList.Create;

  try

    FillWithDBInfo(aLayer, aLayer.FileName, DesignList, IndexList);

    lyrSL := TStringList.Create;
    {
    fL.Add('k1;N;12;2;');
    fL.Add('k2;N;12;2;');
    fL.Add('k3;N;12;2;');

    fL.Add('s2;C;12;0;');
    }

    try

      PopulateFieldList(aLayer.DbTable, lyrSL);

      newFL.AddStrings(lyrSL);

      for i := 0 to AFL.Count - 1 do
      begin
        str := AFL[i];
        str := StringReplace(str, ' ', '', [rfReplaceAll]);

        f1 := parse(str, ';');
        f2 := parse(str, ';');
        if (not ((lyrSL.IndexOf(f1) >= 0) or (lyrSL.IndexOf(lower(f1)) >= 0) or
          (lyrSL.IndexOf(upper(f1)) >= 0))) and (length(f2) = 1) then
        begin

          f3 := parse(str, ';');
          f4 := parse(str, ';');

          New(DesignRecord);
          FillChar(DesignRecord^, SizeOf(TDesignRecord), 0);
          with DesignRecord^ do
          begin
            FieldName := f1;
            FieldType := f2[1];
            FieldSize := strToint(f3);
            FieldDec := strToint(f4);
          end;

          rc := DesignList.Count + 1;

          DesignRecord^.RecNo := rc;

          DesignList.Add(DesignRecord);

          newFL.Add(AFL[i])

        end;

      end;
      if newFL.Count > lyrSL.Count then
        Result := DoRestructure(aLayer, DesignList, IndexList, TStringList(newFL), AShowProgress);
    finally
      lyrSL.Free;
    end;

  except
  end;

  ClearDesignList(DesignList);
  ClearIndexList(IndexList);
  DesignList.Free;
  IndexList.Free;
  newFL.Free;
  end;
 // aLayer.DBTable.Active := true;
 function RemoveDBFColumn(const ALayer: TlicgBaseLayer; const AFieldName: string; AShowProgress: Boolean = False): Boolean;
var
  lyrSL: TStrings;
  newFL: TStringList;
  DesignList, IndexList: TList;
  i, p: Integer;
  fName: string;
begin
  Result := False;
  if (ALayer = nil) or (Trim(AFieldName) = '') then
    Exit;

  DesignList := TList.Create;
  IndexList := TList.Create;
  newFL := TStringList.Create;
  lyrSL := TStringList.Create;
  try
    FillWithDBInfo(ALayer, ALayer.FileName, DesignList, IndexList);
    PopulateFieldList(ALayer.DbTable, lyrSL);

    for i := 0 to lyrSL.Count - 1 do
    begin
      fName := lyrSL[i];
      p := AnsiPos(';', fName);
      if p > 0 then
        fName := Copy(fName, 1, p - 1);
      if not SameText(fName, AFieldName) then
        newFL.Add(lyrSL[i]);
    end;

    if newFL.Count < lyrSL.Count then
      Result := DoRestructure(ALayer, DesignList, IndexList, newFL, AShowProgress);
  finally
    ClearDesignList(DesignList);
    ClearIndexList(IndexList);
    DesignList.Free;
    IndexList.Free;
    newFL.Free;
    lyrSL.Free;
  end;
end;


procedure _ReStructureLayer(Layer: TlicgBaseLayer);
begin
  with TfmRestructDlg.create(nil) do
  try
    Enter(Layer, true);
  finally
    free;
  end;
end;



// TfmRestructDlg class implementation

function TfmRestructDlg.Enter(Layer: TlicgBaseLayer; isSModal: boolean): word;
var
  I: integer;
  DesignRecord: PDesignRecord;
begin
  FisShowMessage := isSModal;
  FDesignTable := TlicgDesignTable.Create(Self);
  FDesignTable.BeforePost := dsDesignBeforePost;
  FDesignTable.AfterDelete := dsDesignAfterDelete;
  FDesignTable.BeforeEdit := dsDesignBeforeEdit;
  FDesignTable.AfterEdit := dsDesignAfterEdit;
  FDesignTable.BeforeDelete := dsDesignBeforeDelete;
  DataSource1.DataSet := FDesignTable;

  FLayer := Layer;
  FullName := FLayer.FileName;
  Caption := Format(SRestructureCaption, [FLayer.DisplayName]);

  DesignList := TList.Create;
  IndexList := TList.create;
  FillWithDBInfo(FLayer, FullName, DesignList, IndexList);

  FDesignTable.Open;

  for I := 0 to DesignList.Count - 1 do
  begin
    DesignRecord := PDesignRecord(DesignList[I]);
    with FDesignTable do
    begin
      Insert;
      FieldByName('FIELDNAME').AsString := DesignRecord^.FieldName;
      FieldByName('TYPE').AsString := DesignRecord^.FieldType;
      FieldByName('SIZE').AsInteger := DesignRecord^.FieldSize;
      FieldByName('DEC').AsInteger := DesignRecord^.FieldDec;
      FieldByName('ORIG_FIELDNO').AsInteger := DesignRecord^.Recno;
      Post;
    end;
  end;
  FDesignTable.First;

  //for I := 0 to IndexList.Count - 1 do
  //  List1.Items.Add(PIndexingRec(IndexList[I])^.IndexName);

  bIndexModified := False;
  bFieldsModified := False;

  LblStatus.Caption := SRestMsg1;

  if isSModal then
  begin
    Result := ShowModal;
    //Bilimediðim bir nedenle DBF'i olmasýna raðmen tabakanýn  UseAttachedDB özelliði false olmuþtu.
    //Bu yüzden yeniden yapýlandýr iþleminde  True atadým.
    if Result = mrOK then
    begin
      FLayer.LayerInfo.UseAttachedDB := True;
    end
  end;
end;

procedure TfmRestructDlg.DBGrid1ColEnter(Sender: TObject);
var
  Field: TField;
begin
  Field := DBGrid2.SelectedField;
  if Field = nil then
    exit;
  if Field.FieldName = 'FIELDNAME' then
    LblStatus.Caption := SRestMsg1
  else if Field.FieldName = 'TYPE' then
    LblStatus.Caption := SRestmsg2
  else if Field.FieldName = 'SIZE' then
    LblStatus.Caption := SRestMsg3
  else if Field.FieldName = 'DEC' then
    LblStatus.Caption := SRestMsg4;
end;

procedure TfmRestructDlg.FormDestroy(Sender: TObject);
begin
  ClearDesignList(DesignList);
  DesignList.free;
  ClearIndexList(IndexList);
  IndexList.Free;
  FDesignTable.Close;
end;

procedure TfmRestructDlg.Button2Click(Sender: TObject);
begin
  //Index := List1.ItemIndex;
 // if Index < 0 then
 //   exit;
 // Dispose(PIndexingRec(IndexList[Index]));
 // ;
 // IndexList.Delete(Index);
 // List1.Items.Delete(Index);
 // bIndexModified := true;
end;

procedure TfmRestructDlg.Button1Click(Sender: TObject);
var
  cnt: integer;
  IndexRecord: PIndexingRec;
  iname: string; //li2016
begin
  with TfmAddIndex.create(nil) do
  try
    FDesignTable.DisableControls;
    try
      FDesignTable.first;
      while not FDesignTable.eof do
      begin
        List2.Items.Add(FDesignTable.FieldByName('FIELDNAME').AsString);
        FDesignTable.next;
      end;
    finally
      FDesignTable.EnableControls;
    end;
    if ShowModal = mrOk then
    begin
      if AnsiPos('+', Edit2.Text) = 0 then
        iname := AnsiUpperCase(Edit2.Text);
      if (InputQuery(SNewIndexTitle, SNewIndexQuery, iName) = false) or (Length(iName)
        = 0) then
      begin
        Free;
        Exit;
      end;
      iName := AnsiUpperCase(iName);
      for cnt := 0 to IndexList.count - 1 do
      begin
        if AnsiCompareText(PIndexingRec(IndexList[cnt])^.IndexName, iName) = 0 then
        begin
          MessageToUser(SDuplicateIndex, smsgerror, MB_ICONERROR);
          Free;
          Exit;
        end;
      end;
      if Length(Edit2.Text) = 0 then
      begin
        MessageToUser(SInvalidDBFExpression, smsgerror, MB_ICONERROR);
        Free;
        Exit;
      end;
      New(IndexRecord);
      IndexRecord^.IndexName := iName;
      IndexRecord^.KeyExpression := Edit2.Text;
      IndexRecord^.ForExpression := Edit3.Text;
      if Check1.State = cbChecked then
        IndexRecord^.Unique := Lider.CG.Com.GIS.iuUnique
      else
        IndexRecord^.Unique := Lider.CG.Com.GIS.iuDuplicates;
      if Check2.State = cbChecked then
        IndexRecord^.SortStatus := Lider.CG.Com.GIS.ssDescending
      else
        IndexRecord^.SortStatus := Lider.CG.Com.GIS.ssAscending;
     // List1.Items.Add(iName);
     // IndexList.Add(IndexRecord);
     // bIndexModified := true;
    end;
  finally
    Free;
  end;
end;

procedure TfmRestructDlg.Button3Click(Sender: TObject);
begin
 // if List1.ItemIndex < 0 then
 //   Exit;
 { with TfmAddIndex.create(nil) do
  try
    FDesignTable.DisableControls;
    try
      FDesignTable.first;
      while not FDesignTable.eof do
      begin
        List2.Items.Add(FDesignTable.FieldByName('FIELDNAME').AsString);
        FDesignTable.next;
      end;
    finally
      FDesignTable.EnableControls;
    end;
    IndexRecord := PIndexingRec(IndexList[List1.ItemIndex]);
    if IndexRecord^.Unique = EzBasegis.iuUnique then
      Check1.State := cbChecked
    else
      Check1.State := cbUnChecked;
    if IndexRecord^.SortStatus = EzBasegis.ssDescending then
      Check2.State := cbChecked
    else
      Check2.State := cbUnChecked;
    Edit2.Text := IndexRecord^.KeyExpression;
    Edit3.Text := IndexRecord^.ForExpression;
    if ShowModal = mrOk then
    begin
      if Length(Edit2.Text) = 0 then
      begin
        MessageToUser(SInvalidDBFExpression, smsgerror, MB_ICONERROR);
        Free;
        Exit;
      end;
      IndexRecord^.KeyExpression := Edit2.Text;
      bIndexModified := true;
      IndexRecord^.ForExpression := Edit3.Text;
      if Check1.State = cbChecked then
        IndexRecord^.Unique := EzBasegis.iuUnique
      else
        IndexRecord^.Unique := EzBasegis.iuDuplicates;
      if Check2.State = cbChecked then
        IndexRecord^.SortStatus := EzBasegis.ssDescending
      else
        IndexRecord^.SortStatus := EzBasegis.ssAscending;
    end;
  finally
    Free;
  end;   }
end;

procedure TfmRestructDlg.dsDesignBeforePost(DataSet: TDataSet);
var
  s: ShortString;
  cnt: integer;
begin

  FisCorrect := true;

  s := AnsiUpperCase(FDesignTable.FieldByName('FIELDNAME').AsString);
  if (Length(s) < 1) or (Length(s) > 10) or not (s[1] in PrimaryIdentChars) or (AnsiPos
    (' ', s) > 0) then
  begin
    ShowMessage(SIncorrectFieldName);
    FisCorrect := false;
   // SysUtils.Abort;
  end;
  for cnt := 1 to Length(s) do
  begin
    if not (s[cnt] in IdentChars) then
    begin
      ShowMessage(SIncorrectFieldName);
      FisCorrect := false;
     // SysUtils.Abort;
    end;
  end;
  s := FDesignTable.FieldByName('TYPE').AsString;
  if Length(s) = 0 then
  begin
    ShowMessage(SUnspecifiedFieldType);
    FisCorrect := false;
    //SysUtils.Abort;
  end;
  if not (s[1] in ['C', 'L', 'D', 'N', 'M', 'B', 'G']) then
  begin
    ShowMessage(SIncorrectFieldType);
    FisCorrect := false;
    //SysUtils.Abort;
  end;
  if s[1] in ['B', 'G', 'M', 'D'] then
  begin
    FDesignTable.FieldByName('SIZE').AsString := '';
    FDesignTable.FieldByName('DEC').AsString := '';
  end;
  if s[1] in ['C', 'N'] then
  begin
    if s[1] = 'C' then
      FDesignTable.FieldByName('DEC').AsString := '';
    if not (FDesignTable.FieldByName('SIZE').AsInteger in [1..254]) then
    begin
      ShowMessage(SIncorrectFieldSize);
      FisCorrect := false;
    //  SysUtils.Abort;
    end;
    if (s[1] = 'N') and not (FDesignTable.FieldByName('DEC').AsInteger in [0..10]) then
    begin
      ShowMessage(SIncorrectDecimals);
      FisCorrect := false;
      //SysUtils.Abort;
    end;
    if (s[1] = 'N') and (FDesignTable.FieldByName('DEC').AsInteger > 0) and (FDesignTable.FieldByName
      ('DEC').AsInteger > FDesignTable.FieldByName('SIZE').AsInteger - 2) then
    begin
      ShowMessage(SIncorrectDecimals);
      FisCorrect := false;
      //SysUtils.Abort;
    end;
  end
  else
    FDesignTable.FieldByName('SIZE').AsString := '';
  bFieldsModified := true;
end;

procedure TfmRestructDlg.DBGrid2ColEnter(Sender: TObject);
var
  Field: TField;
begin
  Field := DBGrid2.SelectedField;
  if Field = nil then
    Exit;
  if Field.FieldName = 'FIELDNAME' then
    LblStatus.Caption := SFieldNameCaption
  else if Field.FieldName = 'TYPE' then
    LblStatus.Caption := SFieldSelectF2
  else if Field.FieldName = 'SIZE' then
    LblStatus.Caption := SFieldSizeCaption
  else if Field.FieldName = 'DEC' then
    LblStatus.Caption := SFieldDecCaption;
end;

procedure TfmRestructDlg.BtnRegenClick(Sender: TObject);
var
  cnt: integer;
  IndexRecord: PIndexingRec;
  TmpTable: TlicgBaseTable;
begin
  if IndexList.Count = 0 then
  begin
    with FLayer.GIS.basetableclass.CreateNoOpen(FLayer.GIS) do
    try
      DBDropIndex(FullName);
    finally
      free;
    end;
    Exit;
  end;
  Screen.Cursor := crHourglass;
  TmpTable := FLayer.GIS.BaseTableClass.Create(FLayer.GIS, FullName, true, true);
  try
    for cnt := 0 to IndexList.count - 1 do
    begin
      IndexRecord := PIndexingRec(IndexList[cnt]);
      with IndexRecord^ do
      begin
        TmpTable.IndexOn(FullName, IndexName, KeyExpression, ForExpression,
          Unique, SortStatus);
      end;
    end;
  finally
    TmpTable.Free;
  end;
  Screen.Cursor := crDefault;
  ShowMessage(SIndexRegenInfo);
end;

procedure TfmRestructDlg.dsDesignAfterDelete(DataSet: TDataSet);
begin
  bFieldsModified := true;
end;

procedure TfmRestructDlg.dsDesignBeforeEdit(DataSet: TDataSet);
begin
end;

procedure TfmRestructDlg.dsDesignAfterEdit(DataSet: TDataSet);
begin
  if (DataSet.FieldValues['FIELDNAME']) = 'UID' then
  begin
    if FisShowMessage then
      Application.MessageBox('UID kolonunu deðiþtiremezsiniz.', 'Bilgi', MB_OK +
        MB_ICONINFORMATION);
    Dataset.Cancel;
    SysUtils.Abort;
    Exit;
  end
  else if (DataSet.FieldValues['FIELDNAME']) = 'INFOID' then
  begin
    if FisShowMessage then
      Application.MessageBox('INFOID kolonunu deðiþtiremezsiniz.', 'Bilgi',
        MB_OK + MB_ICONINFORMATION);
    Dataset.Cancel;
    SysUtils.Abort;
    Exit;
  end;

  // if (FLayer.LayerInfo.IsTriangle) then // ilker üçgen modele göre ileride tekrar deðiþtir.
  begin
    if ((DataSet.FieldValues['FIELDNAME']) = 'K1') or ((DataSet.FieldValues['FIELDNAME'])
      = 'K2') or ((DataSet.FieldValues['FIELDNAME']) = 'K3') or (UpperCase((DataSet.FieldValues
      ['FIELDNAME'])) = 'VALID') then
    begin
      if FisShowMessage then
        Application.MessageBox('Üçgen tabakasýna ait zorunlu kolonlarý deðiþtiremezsiniz.',
          'Bilgi', MB_OK + MB_ICONINFORMATION);
      Dataset.Cancel;
      SysUtils.Abort;
      Exit;
    end;
  end;
end;

procedure TfmRestructDlg.dsDesignBeforeDelete(DataSet: TDataSet);
begin
  if DataSet.FieldByName('FIELDNAME').AsString = 'UID' then
  begin
    if FisShowMessage then
      Application.MessageBox('UID kolonunu silemezsiniz', 'Bilgi', MB_OK +
        MB_ICONINFORMATION);
    Dataset.Cancel;
    SysUtils.Abort;
    Exit;
  end;
  //   Exception.Create('This field cannot be edited');
  //if (FLayer.LayerInfo.IsTriangle) then  // ilker üçgen modele göre ileride tekrar deðiþtir.
  begin
    if ((DataSet.FieldValues['FIELDNAME']) = 'K1') or ((DataSet.FieldValues['FIELDNAME'])
      = 'K2') or ((DataSet.FieldValues['FIELDNAME']) = 'K3') or (UpperCase((DataSet.FieldValues
      ['FIELDNAME'])) = 'VALID') then
    begin
      if FisShowMessage then
        Application.MessageBox('Üçgen tabakasýna ait zorunlu kolonlarý silemezsiniz',
          'Bilgi', MB_OK + MB_ICONINFORMATION);
      Dataset.Cancel;
      SysUtils.Abort;
      Exit;
    end;
  end;
end;

//After User Select Field Type, Cursor will move to Next Field
//This can prevent to confuing user what he does select field.

procedure TfmRestructDlg.DataSource1DataChange(Sender: TObject; Field: TField);
var
  ALen, Adec: Integer;
  AType: Char;
begin

  if (Field <> nil) and (FDesignTable <> nil) and (FDesignTable.Active) then
    if not FChangingValue and (Field.FieldNo = 2) and (DataSource1.State = dsInsert) then
    begin
      AType := AnsiUpperCase(Field.AsString)[1];
      //Set Default Field Value;
      if AType = 'C' then
      begin
        ALen := 40;
        ADec := 0;
      end
      else if AType = 'L' then
      begin
        ALen := 1;
        ADec := 0;
      end
      else if AType = 'D' then
      begin
        ALen := 8;
        ADec := 0;
      end
      else if AType = 'N' then
      begin
        ALen := 12;
        ADec := 2;
      end
      else if AType in ['M', 'B', 'G'] then
      begin
        ALen := 10;
        ADec := 0;
      end
      else
      begin
        ALen := 10;
        ADec := 0;
      end;
      FChangingValue := TRUE;
      FDesignTable.Fields[2].AsInteger := ALen;
      FDesignTable.Fields[3].AsInteger := ADec;
      DBGrid2.SelectedField := FDesignTable.Fields[2];
      FChangingValue := FALSE;
    end;
end;

procedure TfmRestructDlg.DBNavigator1Click(Sender: TObject; Button: TNavigateBtn);
begin
  if Button = nbInsert then
  begin
    FDesignTable.Last;
    FDesignTable.Append;
    FDesignTable.Edit;
  end;
end;

procedure TfmRestructDlg.Action1Execute(Sender: TObject);
var
  Dbf: TlicgBaseTable;
  I: Integer;

  function isExistsInDesignTable(fieldName: string): Boolean;
  begin
    REsult := False;
    FDesignTable.First;
    while not FDesignTable.Eof do
    begin
      if UpperCase(FDesignTable.FieldByName('FIELDNAME').AsString) = upperCase(fieldName)
        then
      begin
        result := true;
        break;
      end;
      FDesignTable.Next;
    end;
  end;

begin
  with TOpenDialog.Create(nil) do
  begin
    DefaultEXT := 'DBF';
    Title := 'Kolonlarý DBF Dosyadan Al';
    Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist];
    Filter := 'DBase files (*.DBF)|*.DBF';
    try
      if not Execute then
        Exit;
      Dbf := currCmdline.ActiveDrawBox.GIS.BaseTableClass.Create(currCmdline.ActiveDrawBox.GIS,
        FileName, false, true);
      try

        for i := 1 to Dbf.FieldCount do
        begin
          if Dbf.Field(i) <> 'UID' then
          begin
            if isExistsInDesignTable(Dbf.Field(i)) then
            begin
              if FisShowMessage then
                Application.MessageBox(PChar(Dbf.Field(i) +
                  ' isimli kolon daha önce tanýmlanmýþ.'), 'Bilgi', MB_OK +
                  MB_ICONINFORMATION);
              exit;
            end;
          end;
        end;

        for i := 1 to Dbf.FieldCount do
        begin
          FDesignTable.Insert;
          if Dbf.Field(i) <> 'UID' then
          begin
            FDesignTable.FieldByName('FIELDNAME').AsString := Dbf.Field(i);
            FDesignTable.FieldByName('TYPE').AsString := Dbf.FieldType(i);
            FDesignTable.FieldByName('SIZE').AsInteger := Dbf.FieldLen(i);
            FDesignTable.FieldByName('DEC').AsInteger := Dbf.FieldDec(i);
            FDesignTable.Post;
          end;
        end;
      finally
        Dbf.Free;
      end;
    finally
      Free;
    end;
  end;

end;

procedure TfmRestructDlg.Action2Execute(Sender: TObject);
var
  I, bm: Integer;
  DesignRecord: PDesignRecord;
  FieldList: TStringList;
  _Field: string;
  _Type: Char;
  _Len: integer;
  _Dec: integer;
begin

  if FDesignTable.State in [dsEdit, dsInsert] then
    FDesignTable.Post;

  if not FisCorrect then
  begin
    ModalResult := mrNone;
    exit;
  end;

  if not (bIndexModified or bFieldsModified) then
  begin
    ModalResult := mrOk;
    Exit;
  end;

  if FisShowMessage then
    if Application.MessageBox(PChar(SRestLongWarning), 'Onay', MB_YESNO +
      MB_ICONQUESTION) <> mrYes then
      Exit;
//    Application.MessageBox(pchar(SRestLongWarning), pchar(SMsgConfirm),
//    MB_YESNO or MB_ICONQUESTION) <> IDYES then
//    Exit;
  Screen.Cursor := crHourglass;
  bm := FDesignTable.RecNo;
  FDesignTable.DisableControls;
  FieldList := TStringList.Create;
  try
    {Create new table}
    FDesignTable.First;
    while not FDesignTable.EOF do
    begin
      _Field := FDesignTable.FieldByName('FIELDNAME').AsString;
      _Type := AnsiUpperCase(FDesignTable.FieldByName('TYPE').AsString)[1];
      _Len := 0;
      _Dec := 0;
      case _Type of
        'C':
          begin
            _Len := FDesignTable.FieldByName('SIZE').AsInteger;
          end;
        'L':
          begin
            _Len := 1;
          end;
        'D':
          begin
            _Len := 8;
          end;
        'N':
          begin
            _Len := FDesignTable.FieldByName('SIZE').AsInteger;
            _Dec := FDesignTable.FieldByName('DEC').AsInteger;
          end;

        'M', 'B':
          begin
          end;
      end;
      FieldList.Add(Format('%s;%s;%d;%d', [_Field, _Type, _Len, _Dec]));

      FDesignTable.Next;
    end;

    for I := 0 to DesignList.Count - 1 do
    begin
      DesignRecord := PDesignRecord(DesignList[I]);
      DesignRecord^.IsNewField := True;
      FDesignTable.First;
      while not FDesignTable.EOF do
      begin
        if DesignRecord^.RecNo = FDesignTable.FieldByName('ORIG_FIELDNO').AsInteger then
        begin
          DesignRecord^.IsNewField := False;
          DesignRecord^.NewFieldName := FDesignTable.FieldByName('FIELDNAME').AsString;
          Break;
        end;

        FDesignTable.Next;
      end;
    end;
    if not DoRestructure(FLayer, DesignList, IndexList, FieldList) then
      ModalResult := mrNone;
  finally
    FDesignTable.RecNo := bm;
    FDesignTable.EnableControls;
    FieldList.Free;
    try
      if FLayer.LayerClassType = lctPostgresUserTable then
      begin
        Licad.DeleteOrAddDBFFields(FLayer);
      end;
    except
    end;
    Screen.Cursor := crDefault;

  end;
end;

procedure TfmRestructDlg.cxDBNavigator1ButtonsButtonClick(Sender: TObject;
  AButtonIndex: Integer; var ADone: Boolean);
begin
  if AButtonIndex = 7 then
  begin
    FDesignTable.Last;
    FDesignTable.Append;
    FDesignTable.Edit;
  end;
end;

end.


