unit Lider.CG.Com.ActionAddFileText;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows, SysUtils, Math, Classes, Controls, Dialogs, StrUtils, Graphics, Forms,
  cxLookAndFeelPainters,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Lib,
  Lider.CG.Com.CmdLine,
  Lider.CG.Com.System,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Consts,
  Lider.CG.Com.LicadSystem,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.GeometryInt,
  System.Diagnostics,////
  System.TimeSpan,       ////
  System.Threading,     //// Thread
  System.SyncObjs,      ////
  System.Generics.Collections,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt;

type
  TReportOutput = (roFile, roScr);
  PReport = ^TKReport;
  TKReport = Record
       colNoktaNo: String;
       colY: Double;
       colX: Double;
       colZ: Double;

  end;

  TAddFileTextAction = class(TlicgAction)
  Private
    FrP: PReport;
    FCizgiEntList, FBaslikEntList, FSatirEntList: IlicgEntityList;
    FTextGroupEnt: IlicgEntity;
    List, Newlist, ExtraList, baslik, kapama: TStringList;
    StringBuilderList: TStringBuilder;
    Katmanlar: TStringList;
    KatmanB, KatmanS, KatmanC: TlicgBaseLayer;
    YaziBoyu, SatirAraligi, ForWy, ForWx, carpan: Double;
    SatirSayisi, F8Counter, F9Counter: Integer;
    ro: TReportOutput;
    FFileName: String;
    R: TlicgExtent;
    EntGName: String;
    ATask, BTask, CTask, STask: Itask;
    procedure GenerateReport(reportOutput:TReportOutput; YaziBoyu, SatirAraligi: Double; point: TlicgCoor);
    procedure KatmanOlustur;
    procedure PrintToScreenForLine(CizgiEnt: IlicgEntity; point: TlicgCoor; charWidth,
 charHeight: Double; satirSayisi: Integer; VectFontName: string; _color: TColor; Colored: Boolean = False; SatirAraligi: Double = 0);
    procedure TranslateGroupEntP(WX, WY: Double);
    procedure DrawRubber;
    procedure SuspendOperation(Sender: TObject);
    procedure ContinueOperation(Sender: TObject);
    procedure MyPaint(Sender: TObject);
    procedure MyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      const WX, WY: Double);
    procedure MyMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; Const WX, WY: Double );
    procedure MyKeyPress( Sender: TObject; Var Key: Char );
  public
    constructor CreateAction(ACmdLine: TlicgBaseCmdLine; AFileName: string = '');
    destructor Destroy; override;
  end;

  function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; Stdcall;

implementation

uses
  Lider.CG.Com.MetinDosyasiEkleBilgileri;

{-------------------------------------------------------------------------------}
//                  TAddFileTextAction
{-------------------------------------------------------------------------------}


procedure TAddFileTextAction.ContinueOperation(Sender: TObject);
begin
  DrawRubber;
end;

constructor TAddFileTextAction.CreateAction(ACmdLine: TlicgBaseCmdLine; AFileName: string = '');
var
  I, j, y, deger, x, kapamaSayaci, _Recno: Integer;
  Ext, c, k: String;
  flag, devam, ContainsIli, ContainsIli1: Boolean;
  L: TlicgBaseLayer;
  E: IlicgEntity;
begin
  inherited CreateAction(ACmdLine);
  FFileName := AFileName;
  with TfmMetinDosyasiEkleBilgileri.Create(nil) do
  try
    ShowModal;
    if (ModalResult = mrCancel) or (ModalResult = mrClose) then
    begin
      close;
      self.Finished := True;
      exit;
    end;
    YaziBoyu := StrToFloat(ceX.Text);
    SatirAraligi := StrToFloat(ceY.Text);
    SatirSayisi := StrToInt(ceZ.Text);
    if FFileName = '' then
      if OpenDialog.Execute then
        FFileName := OpenDialog.FileName;
  finally
    Free;
  end;

  if FFileName = '' then
    Exit;
  KatmanOlustur;
  EntGName := '';
  for I := 0 to CmdLine.ActiveDrawBox.GIS.Layers.Count - 1 do
  begin
    if CmdLine.ActiveDrawBox.GIS.Layers.Items[i].GroupName = '#TBL' then
    begin
      L := CmdLine.ActiveDrawBox.GIS.Layers.Items[i];
      L.Last;
      L.StartBuffering;
      try
        //while not L.Eof do
        //begin
          E := L.LoadEntityWithRecNo(L.Recno);
          if E <> nil then
            if StrCmpLogicalW( PWideChar(WideString(E.GroupName)), PWideChar(WideString(EntGName))) = 1 then
              EntGName := E.GroupName;
          //L.Next;
        //end;
      finally
        L.EndBuffering;
      end;
    end;
  end;
  if EntGName = '' then
    EntGName := '#TAB000001'
  else
  begin
    for I := 0 to Length(EntGName) - 1 do
      if not StrISNumeric(Copy(EntGName, Length(EntGName) - i, 1)) then
        break;
    if ((I = Length(EntGName)) or (I = 0)) and not StrISNumeric(EntGName) then
      EntGName := EntGName + '1'
    else
      EntGName := Copy(EntGName, 1, Length(EntGName) - i) + copy('000000', 1, 6 - Length(inttostr(strtoint(Copy(EntGName,
        Length(EntGName) - i + 1, i)) + 1)))+ inttostr(strtoint(Copy(EntGName,
        Length(EntGName) - i + 1, i)) + 1);
  end;

  FCizgiEntList := TlicgEntityList.Create;
  FBaslikEntList := TlicgEntityList.Create;
  FSatirEntList := TlicgEntityList.Create;
  FTextGroupEnt := Licad.CreateEntityFactory.MakeEntity(idGroup, 1, _2D);
  list := TStringList.Create;
  Newlist := TStringList.Create;
  ExtraList := TStringList.Create;
  StringBuilderList := TStringBuilder.Create;
  WaitingMouseClick :=True;
  OnMouseDown := MyMouseDown;
  OnMouseMove := MyMouseMove;
  OnKeyPress := MyKeyPress;
  OnKeyDown := MyKeyDown;
  OnContinueOperation := ContinueOperation;
  OnSuspendOperation := SuspendOperation;
  OnPaint := MyPaint;
  Cursor := crDrawCross;
  CanDoOsnap := True;
  F8Counter := 0;
  F9Counter := 0;
  forWx := 0;
  ForWy := 0;
  carpan := 1;
  STask := TTask.Create (procedure ()
    var
    I: Integer;
  begin
    currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
    try
      KatmanS.StartBatchInsert;
      for I := 0 to FSatirEntList.Count - 1 do
      begin
        _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
        Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
      end;
    finally
      katmanS.FinishBatchInsert;
      currcmdline.ActiveDrawBox.Undo.EndUndo;
    end;
  end);
  BTask := TTask.Create (procedure ()
  var
    I: Integer;
  begin
    currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
    try
      KatmanB.StartBatchInsert;
      for I := 0 to FBaslikEntList.Count - 1 do
      begin
        _Recno := KatmanB.AddEntity(FBaslikEntList.Items[I]);
        Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanB, _Recno, uaDelete);
      end;
    finally
      katmanb.FinishBatchInsert;
      currcmdline.ActiveDrawBox.Undo.EndUndo;
    end;
  end);
  CTask := TTask.Create (procedure ()
  var
    I: Integer;
  begin
    currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
    try
      KatmanC.StartBatchInsert;
      for I := 0 to FCizgiEntList.Count - 1 do
      begin
        _Recno := KatmanC.AddEntity(FCizgiEntList.Items[I]);
        Currcmdline.ActiveDrawBox.Undo.AddUndo(Katmanc, _Recno, uaDelete);
      end;
    finally
      katmanC.FinishBatchInsert;
      currcmdline.ActiveDrawBox.Undo.EndUndo;
    end;
  end);
  List.LoadFromFile(FFileName);
  if AnsiContainsText(list.Text, ' İli ') then
    ContainsIli := True
  else
    ContainsIli := False;

  if (AnsiContainsText(List.Text, 'Ù')) or (AnsiContainsText(List.Text, '─┘')) then
  begin
    deger:=0;
    ext :=UpperCase(ExtractFileExt(FFileName));
    if (Ext = '.CKS') then (* .cks uzantılı ve netcad dosyaları ise *)
    begin
      if ContainsIli then (* netcad'ten acilan dosya Application raporu ise *)
      begin
        j:=0;
        while j < list.Count do
        begin
          TellWindowsWeArentFrozen;
          k := copy(List[j],1,1);
          if k=#$C then
          begin
            list.Delete(j);
            Newlist.add(#$C);
            NewList.add(#$D#$A); //'');
            inc(j);
          end
          else
          begin
            newlist.add(List[j]);
            if j<>list.count then
              inc(j);
          end;
        end;
        list.Clear;
        list.Text := Newlist.Text;
        Newlist.Clear;
        if AnsiContainsText(list.Text,'') then
        begin
          for I := 0 to list.count-1 do
          begin
            c:= copy(List[i], 1, 1);
            if c=#$C then
              continue
            else
            begin
              if Trim(List[I])='' then
              begin
                inc(deger);
                if (deger mod 4 <> 0) and (deger mod 4 <> 3) then //2 = 1
                  Newlist.add(List[I]);
              end
              else
                Newlist.add(List[I]);
            end;
          end;
        end
        else
        begin
          Newlist.LoadFromFile(FFileName);
        end;
      end
      else
      begin
        if AnsiContainsText(list.Text,'') then  (* Netcad'ten acilan dosya ise *)
        begin
          for I := 0 to list.count-1 do
          begin
            c:= copy(List[i], 1, 1);
            if c=#$C then
              continue
            else
            begin
              if Trim(List[I])='' then
              begin
                inc(deger);
                if deger mod 2= 1 then
                  Newlist.add(List[I]);
              end
              else
                Newlist.add(List[I]);
            end;
          end;
        end
        else
        begin
          Newlist.LoadFromFile(FFileName);
        end;
      end;
    end
    else
    begin
      if ContainsIli then (* netcad'ten acilan dosya Application raporu ise *)
      begin
        j:=0;
        while j < list.Count do
        begin
          TellWindowsWeArentFrozen;
          k := copy(List[j],1,1);
          if k=#$C then
          begin
            //for I := 0 to 1 do
            list.Delete(j);
            Newlist.add(#$C);
            NewList.add(#$D#$A); //'');
            inc(j);
          end
          else
          begin
            newlist.add(List[j]);
            if j<>list.count then
              inc(j);
          end;
        end;
        list.Clear;
        list.Text:=Newlist.Text;
        Newlist.Clear;
        if AnsiContainsText(list.Text,'') then  (* Netcad'ten acilan dosya ise *)
        begin
          for I := 0 to list.count-1 do
          begin
            c:= copy(List[i], 1, 1);
            if c=#$C then
              continue
            else
            begin
              if Trim(List[I])='' then
              begin
                inc(deger);
                if (deger mod 4 <> 0) and (deger mod 4 <> 3) then //2 = 1
                  Newlist.add(List[I]);
              end
              else
                Newlist.add(List[I]);
            end;
          end;
        end
        else
          newlist.LoadFromfile(FFileName);
      end
      else
        if AnsiContainsText(list.Text,'') then  (* Netcad'ten acilan dosya ise *)
        begin
          for I := 0 to list.count-1 do
          begin
            c:= copy(List[i], 1, 1);
            if c=#$C then
              continue
            else
            begin
              if Trim(List[I])='' then
              begin
                inc(deger);
                if (deger mod 2 = 1) then //2 = 1
                  Newlist.add(List[I]);
              end
              else
                Newlist.add(List[I]);
            end;
          end;
        end
        else
          newlist.LoadFromfile(FFileName);
    end;
  /////////
    StringBuilderList.Append(Newlist.Text);
    if (Ext <> '.LTF') then
    begin
//      for i:=0 to Newlist.Count-1 do
//      begin
//        k:='';
//        for j:=1 to length(NewList[i]) do   // newlist yerine list
//        begin
//          TellWindowsWeArentFrozen;
//          c:= copy(NewList[i], j, 1); // newlist yerine list
//          if c='Ú' then
//            k:= k + '┌'   //DA
//          else if c='Ä' then
//            k:= k + '─'       //C4
//          else if c='Â' then
//            k:= k + '┬'     //C2
//          else if c='¿' then
//            k:= k + '┐'    //BF
//          else if c='³' then
//            k:= k + '│'        //B3
//          else if c='Ã' then
//            k:= k + '├'      //C3
//          else if c='Å' then
//            k:= k + '┼'   //C5
//          else if c='´' then
//            k:= k + '┤'     //B4
//          else if c='À' then
//            k:= k + '└'      //
//          else if c='Á' then
//            k:= k + '┴'
//          else if c='Ù' then
//            k:= k + '┘'
//          else
//            k:= k+c;
//        end;
//        newList[i]:= k;
//      end;
      StringBuilderList.Replace('Ú', '┌');
      StringBuilderList.Replace('Ä', '─');
      StringBuilderList.Replace('Â', '┬');
      StringBuilderList.Replace('¿', '┐');
      StringBuilderList.Replace('³', '│');
      StringBuilderList.Replace('Ã', '├');
      StringBuilderList.Replace('Å', '┼');
      StringBuilderList.Replace('´', '┤');
      StringBuilderList.Replace('À', '└');
      StringBuilderList.Replace('Á', '┴');
      StringBuilderList.Replace('Ù', '┘');
    end;
    Newlist.Text := StringBuilderList.ToString;
    StringBuilderList.Free;
    if AnsiContainsText( Newlist.Text, ' İli ') then
    begin
      if SatirSayisi < 15  then
      begin
        ShowMessage('Daha büyük bir sayı giriniz!');
        exit;
      end;
    end
    else
    begin
      if SatirSayisi < 8  then
      begin
        ShowMessage('Daha büyük bir sayı giriniz!');
        exit;
      end;
    end;

    if (Newlist.Text<>'') then
    begin
      list.Free;
      list := TStringList.Create;
      list.Text:=Newlist.Text;
      newlist.Free;
      newlist := TStringList.Create;
      ExtraList.Text := List.Text;
      baslik := TStringList.Create;
      kapama := TStringList.Create;
      baslik.BeginUpdate;
      kapama.BeginUpdate;
      Newlist.BeginUpdate;
      devam := True;
      j:=0;
      if AnsiContainsText( list.Text, '├─') then
      begin
        if AnsiContainsText( List.Text, ' İli ')  then
        begin
          for I := 0 to 15 do
          begin
            if copy(List[i],1,1) = '├' then
              kapamaSayaci := i;
          end;
          while J < list.Count  do
          begin
            if (j=0) and (Trim(List[j]) = '') then
            begin
              for I := 0 to kapamaSayaci do
              begin
                baslik.Add(list[0]);
                list.Delete(0);
              end;
            end;
            if (copy(List[j],1,1) = '└') and (ExtraList[j] <> ' İli ') then
            begin
              for I := 0 to kapamaSayaci do
              begin
                if I=0 then
                  kapama.Add(list[j]);
                if j < list.count then
                  list.Delete(j);
              end;
              inc(j);
            end
            else
            begin
              newlist.add(List[j]);
              if j<>list.count then
                inc(j);
            end;
          end;
        end
        else
        begin
          for I := 0 to 15 do
          begin
            if copy(List[i],1,1) = '├' then
              kapamaSayaci := i;
          end;
          while J < list.Count do //-1
          begin
            if (j=0) and (Trim(list[j]) = '') then
            begin
              for I := 0 to kapamaSayaci do
              begin
                baslik.Add(list[0]);
                list.Delete(0);
              end;
            end;
            if copy(List[j],1,1) = '└' then
            begin
              for I := 0 to kapamaSayaci do
              begin
                if I=0 then
                  kapama.Add(list[j]);
                if j < list.count then
                  list.Delete(j);
              end;
              inc(j);
            end
            else
            begin
              newlist.add(List[j]);
              if j<>list.count then
                inc(j);
            end;
          end;
        end;
        baslik.EndUpdate;
        kapama.EndUpdate;
        Newlist.EndUpdate;
        list.Clear;

        x := 0;
        flag := True;

        if AnsiContainsText( ExtraList.Text, ' İli ') then
        begin
          while flag = True do
          begin
            inc(x);
            if (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
            begin
              list.Add(Newlist[x-1]);
              list.Add(kapama[0]);
            end
            else if (x mod (SatirSayisi-(kapamaSayaci+3)) = 1) then
            begin
              for I := 0 to baslik.Count-1 do
                list.add(baslik[I]);
              list.Add(Newlist[x-1]);
            end
            else
              list.Add(Newlist[x-1]);
            if (x = newlist.Count) and not (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
            begin
              List.Add(kapama[0]);
              flag := false;
            end
            else if (x = newlist.Count) and (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
              flag := false;
          end;
        end
        else ///////
        begin
          while flag = True do
          begin
            inc(x);
            if (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
            begin
              list.Add(Newlist[x-1]);
              list.Add(kapama[0]);
            end
            else if (x mod (SatirSayisi-(kapamaSayaci+3)) = 1) then
            begin
              for I := 0 to baslik.Count-1 do
                list.add(baslik[I]);
              list.Add(Newlist[x-1]);
            end
            else
              list.Add(Newlist[x-1]);
            if (x = newlist.Count) and not (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
            begin
              if kapama.Count>0 then
                List.Add(kapama[0]);
              flag:= false;
            end
            else if (x = newlist.Count) and (x mod (SatirSayisi-(kapamaSayaci+3)) = 0) then
              flag := false;
          end;
        end;
        Newlist.Text := List.Text;
        list.Clear;
      end
      else
      begin
        Newlist.Text := ExtraList.Text;
        list.Clear;
      end;
    end;
  end
  else
  begin
    Newlist.Text := List.Text;
  end;
  ro := roScr;
  PrintToScreenForLine(FTextGroupEnt, AsCoor(0,0), YaziBoyu * 0.82, YaziBoyu, SatirSayisi, 'Courier New', cmdline.ActiveDrawBox.GIS.CurrentLayer.layerInfo.DefFontTool.Color, False, SatirAraligi);
  Self.Caption := 'Raporun Yerini Gösteriniz (F8-Büyüt F9-Küçült)';
end;

function StrCmpLogicalW; external 'shlwapi.dll' name 'StrCmpLogicalW';

Destructor TAddFileTextAction.Destroy;
Var I:Integer;
begin
  if FTextGroupEnt <> nil then FTextGroupEnt := nil;
  if FCizgiEntList <> nil then FCizgiEntList := nil;
  if FBaslikEntList <> nil then FBaslikEntList := nil;
  if FSatirEntList <> nil then FSatirEntList := nil;
  List.Free;
  NewList.Free;
  ExtraList.Free;
//  StringBuilderList.Free;
  kapama.Free;
  baslik.Free;
  BTask.Cancel;
  CTask.Cancel;
  STask.Cancel;
  BTask.CheckCanceled;
  CTask.CheckCanceled;
  STask.CheckCanceled;
  BTask := nil;
  CTask := nil;
  STask := nil;
  Inherited Destroy;
end;

procedure TAddFileTextAction.KatmanOlustur;
var
  I: Integer;
  BaslikFlag, SatirFlag, CizgiFlag: Boolean;
begin
  BaslikFlag := False;
  SatirFlag := False;
  CizgiFlag := False;
  for i := 0 to CmdLine.ActiveDrawBox.GIS.Layers.Count - 1 do
  begin
    if CmdLine.ActiveDrawBox.GIS.Layers.IndexOfName('TBL_BASLIK') = i then
    begin
      katmanb := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_BASLIK');
      BaslikFlag := True;
    end
    else if CmdLine.ActiveDrawBox.GIS.Layers.IndexOfName('TBL_SATIR') = i then
    begin
      katmanS := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_SATIR');
      SatirFlag := True;
    end
    else if CmdLine.ActiveDrawBox.GIS.Layers.IndexOfName('TBL_CIZGI') = i then
    begin
      katmanC := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_CIZGI');
      CizgiFlag := True;
    end
    else
      continue;
  end;
  //if CmdLine.ActiveDrawBox.GIS.LayerGroups.[0]. then
  if not BaslikFlag then
  begin
    Cmdline.ActiveDrawBox.GIS.CreateLayer('TBL_BASLIK', ltMemory);
    katmanb := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_BASLIK');
    KatmanB.LayerInfo.IsMemoryLayer := True;
    KatmanB.LayerInfo.Create(CmdLine.ActiveDrawBox.GIS.CurrentLayer);
    katmanb.LayerInfo.DefFontToolTT.Color := TColor($ff0000);
    katmanb.LayerInfo.ApplyDefPen := true;
    katmanb.LayerInfo.DefBrushTool.Pattern := 0;
    katmanb.LayerInfo.ApplyDefBrush := False;
  end;
  if not SatirFlag then
  begin
    Cmdline.ActiveDrawBox.GIS.CreateLayer('TBL_SATIR', ltMemory);
    katmanS := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_SATIR');
    KatmanS.LayerInfo.IsMemoryLayer := True;
    KatmanS.LayerInfo.Create(CmdLine.ActiveDrawBox.GIS.CurrentLayer);
    katmanS.LayerInfo.DefFontToolTT.Color := TColor($0000ff);
    katmanS.LayerInfo.ApplyDefPen := true;
    katmanS.LayerInfo.DefBrushTool.Pattern := 0;
    katmanS.LayerInfo.ApplyDefBrush := False;
  end;
  if not CizgiFlag then
  begin
    Cmdline.ActiveDrawBox.GIS.CreateLayer('TBL_CIZGI', ltMemory);
    katmanC := CmdLine.ActiveDrawBox.GIS.Layers.LayerByName('TBL_CIZGI');
    KatmanC.LayerInfo.IsMemoryLayer := True;
    KatmanC.LayerInfo.Create(CmdLine.ActiveDrawBox.GIS.CurrentLayer);
    katmanc.LayerInfo.DefPenTool.Color := TColor($00ff00);
    katmanc.LayerInfo.ApplyDefPen := true;
    katmanc.LayerInfo.DefBrushTool.Pattern := 0;
    katmanc.LayerInfo.ApplyDefBrush := False;
  end;
end;
//GetPointInfo

Procedure TAddFileTextAction.MyMouseDown( Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Const WX, WY: Double );
Var
  CurrPoint: TlicgCoor;
  Key: Char;
  i, ii, j: Integer;
  _Recno: Integer;
  Stopwatch: TStopwatch;
  Elapsed: TTimeSpan;
  Seconds: Double;
begin
  if button = mbRight then
  begin
    self.Finished :=True;
    Exit;
  end;
  if (Button = mbLeft) then
  begin
    if CmdLine.IsSnapped then
      CurrPoint :=CmdLine.GetSnappedPoint
    else
      CurrPoint := CmdLine.CurrentPoint;
    //Stopwatch := TStopwatch.StartNew;
    if F8Counter > F9Counter then
    begin
      for j := 1 to F8Counter - F9Counter do
      begin
        carpan := carpan * 1.1;
      end;
    end
    else if F9Counter > F8Counter then
    begin
      for j := 1 to F9Counter - F8Counter do
      begin
        carpan := carpan * 0.9;
      end;
    end
    else
      carpan := 1;
    R.LowerLeft.X := CurrPoint.X - 1;
    R.LowerLeft.Y := CurrPoint.y - 66*(YaziBoyu * carpan + SatirAraligi * carpan);
    R.UpperRight.Y := CurrPoint.Y;
    Screen.Cursor := crHourGlass;
    GenerateReport(ro, YaziBoyu, SatirAraligi, CurrPoint);

(*    BTask := TTask.Create (procedure ()
      var
        _Recno: Integer;
      begin
        CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        try
          Katmanb.StartBatchInsert;
          TParallel.For(0, FBaslikEntList.Count-1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := Katmanb.AddEntity(FBaslikEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(Katmanb, _Recno, uaDelete);
            end);
          end);
          KatmanB.FinishBatchInsert;
     {     KatmanC.StartBatchInsert;
          TParallel.For(0, FCizgiEntList.Count-1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanC.AddEntity(FCizgiEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanC, _Recno, uaDelete);
            end);
          end);
          KatmanC.FinishBatchInsert;
          KatmanS.StartBatchInsert;
          TParallel.For(0, FSatirEntList.Count-1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end);
          end);
          KatmanS.FinishBatchInsert;}
        finally
          currcmdline.ActiveDrawBox.Undo.EndUndo;
        end;
      end);
    CTask := TTask.Create (procedure ()
      var
        _Recno: Integer;
      begin
        CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        try
          KatmanC.StartBatchInsert;
          TParallel.For(0, FCizgiEntList.Count-1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanC.AddEntity(FCizgiEntList.Items[I]);
                Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanC, _Recno, uaDelete);
            end);
          end);
        finally
          KatmanC.FinishBatchInsert;
          currcmdline.ActiveDrawBox.Undo.EndUndo;
        end;
      end);
    S1Task := TTask.Create (procedure ()
      var
        _Recno: Integer;
      begin
        CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        try
          KatmanS.StartBatchInsert;
          TParallel.For(0, FSatirEntList.Count div 3 - 1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end);
          end);
        finally
          KatmanS.FinishBatchInsert;
          currcmdline.ActiveDrawBox.Undo.EndUndo;
        end;
      end);
    S2Task := TTask.Create (procedure ()
      var
        _Recno: Integer;
      begin
        CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        try
          KatmanS.StartBatchInsert;
          TParallel.For(FSatirEntList.Count div 3, (FSatirEntList.Count div 3)*2 - 1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end);
          end);
        finally
          KatmanS.FinishBatchInsert;
          currcmdline.ActiveDrawBox.Undo.EndUndo;
        end;
      end);
    S3Task := TTask.Create (procedure ()
      var
        _Recno: Integer;
      begin
        CmdLine.ActiveDrawBox.Undo.BeginUndo(uaDelete);
        try
          KatmanS.StartBatchInsert;
          TParallel.For((FSatirEntList.Count div 3)*2, FSatirEntList.Count-1, procedure(I: Integer)
          begin
            TThread.Queue(nil,
            Procedure
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end);
          end);
        finally
          KatmanS.FinishBatchInsert;
          currcmdline.ActiveDrawBox.Undo.EndUndo;
        end;
      end);
     BTask.Start;
     CTask.Start;
     S1Task.Start;
     S2Task.start;
     S3Task.Start;
     TTask.WaitForAll([BTask, CTask, S1Task, S2Task, S3Task]);    *)


     (*      j := 3;
      SetLength(TaskForSatir, j);
        TaskForSatir[0] := TTask.Create( procedure()
        var k: Integer;
        begin
     {     currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
          try
            KatmanS.StartBatchInsert;
            for K := 0 to (FSatirEntList.Count div j) - 1 do
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[K]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end;
          finally
            katmanS.FinishBatchInsert;
            currcmdline.ActiveDrawBox.Undo.EndUndo;
          end;                   }
          for K := 0 to (FSatirEntList.Count div j) - 1 do
            AddElementsToLayer(KatmanS,FSatirEntList.Items[k]);
        end);
        TaskForSatir[1] := TTask.Create( procedure()
        var k: Integer;
        begin
          {currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
          try
            KatmanS.StartBatchInsert;
            for K := (FSatirEntList.Count div j) to (FSatirEntList.Count div j)*2 - 1 do
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[K]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end;
          finally
            katmanS.FinishBatchInsert;
            currcmdline.ActiveDrawBox.Undo.EndUndo;
          end;   }
          Tthread.Queue(nil,
          procedure
          begin
            AddElementsToLayer(KatmanS, FSatirEntList);
          end);
        end);
        TaskForSatir[2] := TTask.Create( procedure()
        var k: Integer;
        begin
         { currcmdline.ActiveDrawBox.Undo.BeginUndo(uaDelete);
          try
            KatmanS.StartBatchInsert;
            for K := (FSatirEntList.Count div j)*2 to FSatirEntList.Count-1 do
            begin
              _Recno := KatmanS.AddEntity(FSatirEntList.Items[K]);
              Currcmdline.ActiveDrawBox.Undo.AddUndo(KatmanS, _Recno, uaDelete);
            end;
          finally
            katmanS.FinishBatchInsert;
            currcmdline.ActiveDrawBox.Undo.EndUndo;
          end;           }
          Tthread.Queue(nil,
          procedure
          begin
            AddElementsToLayer(KatmanS, FSatirEntList);
          end);
        end); *)

      btask.Start;
      ctask.Start;
      stask.Start;
      TTask.WaitForAll([Stask, BTask, CTask]);

(*    CmdLine.ActiveDrawBox.Undo.BeginUndo(uadelete);
    try
      katmanc.StartBatchInsert;
      for I := 0 to FCizgiEntList.Count-1 do
      begin
        _Recno := KatmanC.AddEntity(FCizgiEntList.Items[I]);
        CmdLine.ActiveDrawBox.Undo.AddUndo(katmanc, _Recno, uaDelete);
      end;
      katmanc.FinishBatchInsert;
      KatmanB.StartBatchInsert;
      for I := 0 to FBaslikEntList.Count-1 do
      begin
        _Recno := KatmanB.AddEntity(FBaslikEntList.Items[I]);
        CmdLine.ActiveDrawBox.Undo.AddUndo(katmanB, _Recno, uaDelete);
      end;
      KatmanB.FinishBatchInsert;
      KatmanS.StartBatchInsert;
      for I := 0 to FSatirEntList.Count-1 do
      begin
        _Recno := KatmanS.AddEntity(FSatirEntList.Items[I]);
        CmdLine.ActiveDrawBox.Undo.AddUndo(katmanS, _Recno, uaDelete);
      end;
      KatmanS.FinishBatchInsert;
    finally
      CmdLine.ActiveDrawBox.Undo.EndUndo;
      Screen.Cursor := crDefault;
    end;                                           *)
//    Elapsed := Stopwatch.Elapsed;
//    Seconds := Elapsed.TotalSeconds;
//    ShowMessage(FloatToStr(Seconds));
    CmdLine.ActiveDrawBox.RepaintExtent(R);
    self.Finished := True;
    Exit;
  end;
End;  //MyMouseDown

Procedure TAddFileTextAction.MyKeyPress( Sender: TObject; Var Key: Char );
Var CurrPoint :TlicgCoor;
Begin
  If ( Key = #27 ) Then
  Begin
    Self.Finished := true;
    Exit;
  End;
End;

procedure TAddFileTextAction.GenerateReport(reportOutput:TReportOutput; YaziBoyu, SatirAraligi: Double; point: TlicgCoor );
Var i, j, deger: Integer;
    rp: TlicgEReport;
    k, c: string;
    YaziGenisligi: Double;
begin
  rp := TlicgEReport.Create;
  if NewList.Count < 2 then Exit;
  StringBuilderList := TStringBuilder.Create;
  StringBuilderList.Append(Newlist.Text);
//  for i := 0 to Newlist.Count - 1 do
//  begin
//    k := '';
//    for j := 1 to length(NewList[i]) do   // newlist yerine list
//    begin
//      TellWindowsWeArentFrozen;
//      c := copy(NewList[i], j, 1); // newlist yerine list
//      if c = '┌' then
//        k := k + 'Ú'   //DA
//      else if c = '─' then
//        k := k + 'Ä'       //C4
//      else if c = '┬' then
//        k := k + 'Â'     //C2
//      else if c = '┐' then
//        k := k + '¿'    //BF
//      else if c = '│' then
//        k := k + '³'        //B3
//      else if c = '├' then
//        k := k + 'Ã'      //C3
//      else if c = '┼' then
//        k := k + 'Å'   //C5
//      else if c = '┤' then
//        k := k + '´'     //B4
//      else if c = '└' then
//        k := k + 'À'      //
//      else if c = '┴' then
//        k := k + 'Á'
//      else if c = '┘' then
//        k := k + 'Ù'
//      else
//        k := k + c;
//    end;
//    newList[i] := k;
//  end;
  StringBuilderList.Replace('┌', 'Ú');
  StringBuilderList.Replace('─', 'Ä');
  StringBuilderList.Replace('┬', 'Â');
  StringBuilderList.Replace('┐', '¿');
  StringBuilderList.Replace('│', '³');
  StringBuilderList.Replace('├', 'Ã');
  StringBuilderList.Replace('┼', 'Å');
  StringBuilderList.Replace('┤', '´');
  StringBuilderList.Replace('└', 'À');
  StringBuilderList.Replace('┴', 'Á');
  StringBuilderList.Replace('┘', 'Ù');
  Newlist.Text := StringBuilderList.ToString;
  StringBuilderList.Free;
  With newList Do
  begin
    for i := 0 to Count - 1 do
    begin
      TellWindowsWeArentFrozen;
      rp.addRline(newlist[i]);
    end;//with rp
  End;//
  YaziGenisligi := YaziBoyu * 0.82;
  YaziGenisligi := YaziGenisligi * carpan;
  YaziBoyu := YaziBoyu * carpan;
  SatirAraligi := SatirAraligi * carpan;
  if reportOutput = roScr then
  begin
    rp.PrintToScreen(FCizgiEntList, FBaslikEntList, FSatirEntList, point, YaziGenisligi, YaziBoyu, SatirSayisi,
          'Courier New', cmdline.ActiveDrawBox.GIS.CurrentLayer.layerInfo.DefFontTool.Color,
          False, SatirAraligi, carpan, EntGName );
  end;
  R.UpperRight.X := rp.RectX;
  rp.Free ;
end;  //GenerateReport

procedure TAddFileTextAction.PrintToScreenForLine(CizgiEnt: IlicgEntity; point: TlicgCoor; charWidth,
 charHeight: Double; satirSayisi: Integer; VectFontName: string; _color: TColor; Colored: Boolean = False; SatirAraligi: Double = 0);
(* Yusuf Ekleme *)
var
  i, j, l, n, y, o, z, q, counter, projesayisi: Integer;
  p, aCoor: TlicgCoor;
  tmpEnt, tmpEntLine: IlicgEntity;
  c, s, k, m, v: String;
  TotalWidth, SatirArasiBosluk: Double;
  ATotalWidth, SatirUzunlugu, EnUzunSatir: Integer;
  SatirUzunluguDouble, EnUzunSatirDouble, CoorForTop, CoorForBottom, CoorForBegin: Double;
  flag, ContainsTextIli, AnsiContainsBakilan, ltfmi: Boolean;
  ICoor: IlicgVector;
begin
  ICoor := Licad.CreateEntityFactory.MakeVector(_3D, 2);
  ATotalWidth := 0;
  flag := True;
  projesayisi := 0;
  charWidth := charWidth * carpan;
  charHeight := charHeight * carpan;
  SatirAraligi := SatirAraligi * carpan;
  if SatirAraligi <> 0 then
    SatirArasiBosluk := charHeight + SatirAraligi
  else
    SatirArasiBosluk := charHeight+(charHeight/3);
  try
    p := AsCoor(0,0);
    n := 0;
    counter := 0;
    if Copy(Newlist[1], 1, 1) <> '┌' then
      ltfmi := True
    else
      ltfmi := false;
    for i := 1 to Newlist.Count do  //i:=0 to sList.Count-1 do
    begin
      TellWindowsWeArentFrozen;
      y := i-1;
      for z := 1 to  Length(Newlist[i-1]) do
      begin
        v := copy(Newlist[i-1],z,1);
        if flag then
        begin
          if (v = '└') then
          begin
            if AnsiContainsText(Newlist.Text, ' İli ') then
            begin
              inc(counter);
              if counter mod 2 = 0 then
              begin
                q := y;
                flag := False;
              end
              else
                projesayisi := y;
            end
            else
            begin
              q := y;
              flag := False;
            end;
          end;
        end;
      end;
    end;
    if AnsiContainsText(Newlist.Text, ' İli ') then
      ContainsTextIli := True
    else
      ContainsTextIli := False;

    if AnsiContainsText(Newlist.Text, 'Bakılan') then
      AnsiContainsBakilan := True
    else
      AnsiContainsBakilan := False;

    ACoor := AsCoor(point.x, point.y);
    TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, CmdLine.ActiveDrawBox.GIS.MapInfo.CS.Description);
    tmpEnt.DrawTools.FontTool.Height := charHeight;
    tmpEnt.DrawTools.FontTool.Angle := 0;
    tmpEnt.DrawTools.FontTool.Name := 'Courier New';
    AslicgGroupGeometry(CizgiEnt.Geometry).AddEntity(tmpEnt.clone);
    TmpEnt := nil;

    if ContainsTextIli then
    begin
      counter := 0;
      z := 0;
      TotalWidth := point.X;
      p := AsCoor(point.X, point.Y - 1*(SatirArasiBosluk));
      for i := 1 to Newlist.Count do
      begin
        flag := True;
        p := AsCoor(totalwidth, point.Y -((i-1) mod (q+1)) * (SatirArasiBosluk));
        if z < 2 then
        begin
          for j := 1 to Length(Newlist[i-1]) do
          begin
            v := Copy(Newlist[i-1], j, 1);
            if V = '┬' then
            begin
              if Z = 0 then
              begin
                CoorForTop := p.Y;
                inc(z);
              end
              else if z = 1 then
              begin
                CoorForBottom := p.Y;
                inc(z);
              end
              else
                break;
            end;
          end;
        end;

        if (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┐') or (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┤') or (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┘') then
        begin
          tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
          tmpEntLine.DrawTools.PenTool.Color := clGreen;
          tmpEntLine.GroupName := EntGName;
          ICoor.Add(TotalWidth, p.y);
          ICoor.Add(p.X + (Newlist[i-1].Length-1) * charWidth, p.Y);
          tmpEntLine.Geometry.Points.Assign(ICoor);
          AslicgGroupGeometry(CizgiEnt.Geometry).AddEntity(tmpEntLine.clone);
          ICoor.Clear;
          flag := false;
          if (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┐') then
            CoorForBegin := p.Y;
        end;
        if (Copy(Newlist[i-1],1,1) = '└') then
        begin
          for j := 1 to  Length(Newlist[i-1]) do
          begin
            c := Copy(Newlist[i-1],j,1);
            if (c = '└') or (c = '┴') or (c = '┘') then
            begin
              tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
              tmpEntLine.DrawTools.PenTool.Color := clGreen;
              tmpEntLine.GroupName := EntGName;
              ICoor.Add(p.X, p.y);
              if c = '┴' then
              begin
                if counter mod 2 = 0 then
                  ICoor.Add(p.X, CoorForTop)
                else
                  ICoor.Add(p.X, CoorForBottom);
              end
              else
                ICoor.Add(p.X, CoorForBegin);
              tmpEntLine.Geometry.Points.Assign(ICoor);
              AslicgGroupGeometry(CizgiEnt.Geometry).AddEntity(tmpEntLine.clone);
              ICoor.Clear;
              if c = '┘' then
                inc(counter);
            end;
            p.x := p.x + (charWidth);
            flag := False;
          end;
        end;
        SatirUzunluguDouble := p.x;
        if EnUzunSatirDouble < SatirUzunluguDouble then  EnUzunSatirDouble := SatirUzunluguDouble;
        if (counter mod 2 = 0) and ((Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┘')) then
        begin
          TotalWidth := EnUzunSatirDouble + 5*carpan;
          p := AsCoor(totalwidth, p.Y);
        end;
      end;
    end
    else if AnsiContainsText(Newlist.Text, '─┐') then
    begin
      if AnsiContainsText(Newlist.Text, 'Bakılan') then
        AnsiContainsBakilan := True
      else
        AnsiContainsBakilan := False;
      z := 0;
      TotalWidth := point.X;
      p:= AsCoor(point.X, point.Y - 1*(SatirArasiBosluk));
      for i := 1 to Newlist.Count do
      begin
        flag := True;
        p := AsCoor(totalwidth, point.Y -((i-1) mod (q+1)) * (SatirArasiBosluk));
        if z = 0 then
        begin
          for j := 1 to Length(Newlist[i-1]) do
          begin
            v:= Copy(Newlist[i-1], j, 1);
            if V = '┬' then
            begin
              CoorForTop := p.Y;
              inc(z);
              break;
            end;
          end;
        end;

        if (Copy(Newlist[i-1],Length(Newlist[i-1]),1)= '┐') or (Copy(Newlist[i-1],Length(Newlist[i-1]),1)= '┤') or (Copy(Newlist[i-1],Length(Newlist[i-1]),1)= '┘') then
        begin
          tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
          tmpEntLine.DrawTools.PenTool.Color := clGreen;
          tmpEntLine.GroupName := EntGName;
          ICoor.Add(TotalWidth, p.y);
          ICoor.Add(p.X + (Newlist[i-1].Length-1) * charWidth, p.Y);
          tmpEntLine.Geometry.Points.Assign(ICoor);
          AslicgGroupGeometry(CizgiEnt.Geometry).AddEntity(tmpEntLine.clone);
          ICoor.Clear;
          flag := false;
          if (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┐') then
            CoorForBegin := p.Y;
        end;
        if (Copy(Newlist[i-1],1,1) = '└') then
        begin
          for j := 1 to  Length(Newlist[i-1]) do
          begin
            c := Copy(Newlist[i-1],j,1);
            if (c = '└') or (c = '┴') or (c = '┘') then
            begin
              tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
              tmpEntLine.DrawTools.PenTool.Color := clGreen;
              tmpEntLine.GroupName := EntGName;
              ICoor.Add(p.X, p.y);
              if c = '┴' then
                ICoor.Add(p.x, CoorForTop)
              else
                ICoor.Add(p.x, CoorForBegin);
              tmpEntLine.Geometry.Points.Assign(ICoor);
              AslicgGroupGeometry(CizgiEnt.Geometry).AddEntity(tmpEntLine.clone);
              ICoor.Clear;
            end;
            p.x := p.x + (charWidth);
            flag := False;
          end;
        end;
        if (Copy(Newlist[i-1],Length(Newlist[i-1]),1) = '┘') then
        begin
          TotalWidth := p.X + 5* carpan;
          p := AsCoor(totalwidth, p.Y);  //0)
        end;
      end;
    end;
  finally
    tmpEntLine := nil;
    tmpEnt := nil;
  end;
end;

procedure TAddFileTextAction.SuspendOperation(Sender: TObject);
begin
  DrawRubber;
end;

procedure TAddFileTextAction.MyMouseMove(Sender: TObject; Shift:
  TShiftState; X, Y: Integer; const WX, WY: Double);
begin
  if (forwx = wx) and (ForWy = wy) then
    exit;
  WaitingMouseClick := False;
//  drawRubber;
  if AslicgGroupGeometry(FTextGroupEnt.Geometry).NumEntities > 0 then
  begin
    DrawRubber;
    TranslateGroupEntP(wx, wy);
    DrawRubber;
  end;
//  drawRubber;
  Forwx := wx;
  ForWy := wy;
end;

procedure TAddFileTextAction.MyPaint(Sender: TObject);
begin
  DrawRubber;
end;

procedure TAddFileTextAction.TranslateGroupEntP(WX, WY: Double);
var
  p: TlicgCoor;
  Mat: TlicgMatrix;
  tx, ty: Double;
  I, j, entnumber, t: Integer;
  Ent1, EntYedek: IlicgEntity;
begin
  p.x := FTextGroupEnt.Geometry.Extent.LowerLeft.x;
  p.y := FTextGroupEnt.Geometry.Extent.UpperRight.y;
  Tx := wx - p.x;
  Ty := wy - p.y;
  Mat := Translate2D(Tx, Ty);
  FTextGroupEnt.Geometry.ResetTransform;
  FTextGroupEnt.Geometry.SetTransformMatrix(Mat);
  FTextGroupEnt.Geometry.ApplyTransform;
end;

procedure TAddFileTextAction.DrawRubber;
var
  I: Integer;
  Line: IlicgEntity;
Begin
  if AslicgGroupGeometry(FTextGroupEnt.Geometry).NumEntities > 0 then
  begin
    Line := Licad.CreateEntityFactory.MakeEntity(idLine, 0, _2D);
    Line.DrawTools.PenTool.Style := 0;
    Line.Geometry.Points.Add( 0, 0 );
    Line.Geometry.Points.Add( FTextGroupEnt.Geometry.Extent.LowerLeft.X, FTextGroupEnt.Geometry.Extent.UpperRight.Y );
    CmdLine.ActiveDrawBox.drawEntity2DRubberBand(Line,false,false,clRed);
  end;
  Line := nil;
  if AslicgGroupGeometry(FTextGroupEnt.Geometry).NumEntities > 0 then
  begin
    cmdLine.All_DrawEntity2DRubberBand(FTextGroupEnt);
  end;
end; //DrawRubber

procedure TAddFileTextAction.MyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if AslicgGroupGeometry(FTextGroupEnt.Geometry).NumEntities = 0 then
    Exit;
  DrawRubber;
  case Key of
    VK_F8:
      begin
        Inc(F8Counter);
        FTextGroupEnt.Geometry.ResetTransform();
        FTextGroupEnt.Geometry.ScaleTransform(1.1, 1.1, FTextGroupEnt.Centroid);
        FTextGroupEnt.Geometry.ApplyTransform;
        FTextGroupEnt.Geometry.UpdateExtension;
      end;
    VK_F9:
      begin
        inc(F9Counter);
        FTextGroupEnt.Geometry.ResetTransform();
        FTextGroupEnt.Geometry.ScaleTransform(0.9, 0.9, FTextGroupEnt.Centroid);
        FTextGroupEnt.Geometry.ApplyTransform;
        FTextGroupEnt.Geometry.UpdateExtension;
      end;
    VK_ESCAPE:
      begin
        Self.Finished := True;
        exit;
      end;
  end; //Case
  DrawRubber;
end;

end.

