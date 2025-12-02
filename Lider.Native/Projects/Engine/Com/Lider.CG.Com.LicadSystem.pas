unit Lider.CG.Com.LicadSystem;

interface

uses
  Dialogs,
  SysUtils,
  StrUtils,
  Windows,
  Printers,
  Classes,
  Graphics,
  IniFiles,
  Forms,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.GeometryInt,
  Lider.CG.Com.GIS,
  Lider.CG.Com.LicadInt,
  Lider.CG.Com.Base,
  Lider.CG.Com.Lib,
  Lider.CG.Com.EntityInt;

type
  TPPreview = function(ReportMemo: TStream; TopMargin, BottomMargin: Integer):
    Integer; register;

  TlicgERColAlign = (rpLeft, rpRight); //Report column align

type
  TOffset = record   //For printer offset x and y
    X, Y: Integer;
  end;

type
  TlicgEReport = class(TObject)  //Report Lines stream object
  private
    FRLines: TMemoryStream; //Yusuf Degistirme
    FlV, FlH, FlLT, FlRT, FlLB, FlRB, FlVMR, FlVML, FlHMT, FlHMB, FlCR: Char; //Box characters (charSet is must be OEM_CHARSET)
    FRstr: string;
    FHeaderColumn, FRow: TStringList; // Yusuf Ekleme
    StringBuilder: TStringBuilder;
    FLeftMargin: Integer;
    FRectX: Double;
  public
    constructor Create;
    destructor Destroy; override;
    function ReportBuffer: TStream;
    procedure LoadRLinesToText(sLines: TStrings);
    procedure ADDRLine(s: AnsiString; ForProject: Boolean = True);
    procedure InsertRLine;
    procedure ADDstr(s: string; Size: Integer; Align: TlicgERColAlign; Line:
      Boolean = True; Comma: Boolean = False);
	  
	//Nihal Zuhal Ekleme başlama
    procedure ADDstri(s: string; Size: Integer; Align: TlicgERColAlign;
  	  Line: Boolean = True; Comma: Boolean = False);
    procedure ADDHeaderColi(s: string; Size: Integer; Align: TlicgERColAlign;
      Line: Boolean = True); 
	//Nihal Zuhal Ekleme
    procedure ADDHeaderCol(s: string; Size: Integer; Align: TlicgERColAlign;
      Line: Boolean = True);
    procedure ClearHeaderCol;
    procedure PrintHeader;
    procedure PrintFooterLine;
    procedure PrintLineAfterRecord(isDoubleLine: Boolean = False);
    procedure PrintToScreen(CizgiEnt, BaslikEnt, SatirEnt: IlicgEntityList; point: TlicgCoor; charWidth, charHeight: Double;
      satirSayisi: Integer; VectFontName: string; _color: TColor; Colored: Boolean = False;
      SatirAraligi: Double = 0; Carpan: Double = 1; EntityGroupName: String = '');
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default 0;
    Property Row: TStringList Read FRow Write FRow; // Yusuf Ekleme
    property RectX: Double read FRectX write FRectX;
  end;

{
  A4 paper : Width = 21cm.(8.26 Inch) Length = 29.7cm.(11.7 Inch)
             (1 Inch = 2.54cm)
}
const
  INCHES_PER_MILIMETER: Real = 0.04;

function StrISNumeric(tmpStr: string): Boolean;//is String numeric ?

function ReadFormValue(FormAndControlName: string): string;

procedure SaveFormValue(FormAndControlName, Value: string);

function GetDrawBoxPropertyValue(propertyName: string): string;

procedure GetPDeviceSettings(FTopmargin, FBottomMargin: Integer; var prnName:
  string; //The printer name
  var prnOrientation: TPrinterOrientation; //(poPortrait, poLandscape)
  var prnOffsetPixels: TOffset; var prnPixelsPerMMX: Real; var prnPixelsPerMMY:
  Real; var prnPageHeightMM: Integer; {page length (mm)}
  var prnMMSize: Integer; var prnPrintableRowCount: Integer);

procedure CalculatePRow(FTopmargin, FBottomMargin: Integer; var prnMMSize:
  Integer; var prnPrintableRowCount: Integer; DefaultFont: Boolean = False);
//Function LoadPackageLIB(PackageName,FuncName:String):Boolean;

function ReportPreview(ReportBuff: TStream; TopMargin, BottomMargin: Integer; _IsInfo: Boolean = false): Integer;

procedure HelpMe(FHandle: THandle; FHelpPath: string);

procedure CloseAllHelpWin;

var
  ppFunction: TFarProc;
  ppHandle: HModule;

const
  HH_DISPLAY_TOPIC = $0000;
  HH_HELP_FINDER = $0000;
  HH_DISPLAY_TOC = $0001;
  HH_DISPLAY_INDEX = $0002;
  HH_DISPLAY_SEARCH = $0003;
  HH_SET_WIN_TYPE = $0004;
  HH_GET_WIN_TYPE = $0005;
  HH_GET_WIN_HANDLE = $0006;
  HH_ENUM_INFO_TYPE = $0007;
  HH_SET_INFO_TYPE = $0008;
  HH_SYNC = $0009;
  HH_KEYWORD_LOOKUP = $000D;
  HH_DISPLAY_TEXT_POPUP = $000E;
  HH_HELP_CONTEXT = $000F;
  HH_TP_HELP_CONTEXTMENU = $0010;
  HH_TP_HELP_WM_HELP = $0011;
  HH_CLOSE_ALL = $0012;
  HH_ALINK_LOOKUP = $0013;
  HH_GET_LAST_ERROR = $0014;
  HH_ENUM_CATEGORY = $0015;
  HH_ENUM_CATEGORY_IT = $0016;
  HH_RESET_IT_FILTER = $0017;
  HH_SET_INCLUSIVE_FILTER = $0018;
  HH_SET_EXCLUSIVE_FILTER = $0019;
  HH_INITIALIZE = $001C;
  HH_UNINITIALIZE = $001D;
  HH_PRETRANSLATEMESSAGE = $00FD;
  HH_SET_GLOBAL_PROPERTY = $00FC;

function HtmlHelp(hwndCaller: HWND; pszFile: PChar; uCommand: UINT; dwData:
  DWORD): HWND; stdcall; external 'HHCTRL.OCX' name 'HtmlHelpA';

implementation

var
  FPrintLines, FProject: TStringList; // yusuf ekleme
{
 ------------------------------------------------------------------------------------------------
                            TlicgEReport Imp. begin
 ------------------------------------------------------------------------------------------------
}

constructor TlicgEReport.Create;
begin
  inherited Create;
  FHeaderColumn := TStringList.Create;
  FPrintLines := TStringList.Create; //Yusuf Ekleme
  FProject := TStringList.Create;
  FRLines := TMemoryStream.Create;
  FRow:= TStringList.Create; // Yusuf Ekleme
  FRLines.Seek(0, 0);

  FlV:='³'; // Yusuf Ekleme
  FlH := '-';
  FlLT := '+';
  FlRB := '+';
  FlRT := '+';
  FlLB := '+';
  FlVMR := '+';
  FlVML := '+';
  FlHMT := '+';
  FlHMB := '+';
  FlCR := '+';

end; //Create

destructor TlicgEReport.Destroy;
begin
  if FRLines <> Nil then
    FRLines.Free;
  FHeaderColumn.Free;
  FPrintLines.Free; // Yusuf Ekleme
  FProject.Free;
  FRow.Free; // Yusuf Ekleme
  inherited Destroy;
end; //Destroy

//Returns the FRLines

function TlicgEReport.ReportBuffer: TStream;
begin
  Result := FRLines;
end; //ReportBuffer

//Load strem to sLines

procedure TlicgEReport.LoadRLinesToText(sLines: TStrings);
begin
  sLines.Clear;
  FRLines.Seek(0, 0);
  sLines.LoadFromStream(FRLines);
end; //LoadRLinesToText

(* Zuhal Nihal Ekleme başlama*)
procedure TlicgEReport.ADDstri(s: string; Size: Integer; Align: TlicgERColAlign;
  Line: Boolean = True; Comma: Boolean = False);
var
  strFormat: string;
  LR: string;
  FirstStr, deger:Boolean;
begin
  LR := '';
  FirstStr := (FRStr = '');
  //if Size = 0 then
    //Size := Length(s);
  if StrToFloatDef(s,0)=0 then
    deger:= false
  else
    deger:=True;
  if Align = rpLeft then  LR:='-' ;
  if deger then
  begin
  	(* Yusuf Ekleme *)
    if s=' ' then
    begin
       s:= '';
       strFormat:='%'+LR+inttostr(Size)+ 's';  //inttostr(Length(s))+'s';
       FRstr:=FRstr+Format(strFormat,[s]);
    end;
    if line then
    begin
       if not FirstStr then
       begin
          if Comma then
          begin
            //s:= Trim(s);
            s:=Format('%.2n', [StrToFloat(s) * 1.0]);
            strFormat:='%'+LR+inttostr(Size)+ 's';//inttostr(Length(s))+'s';
            FRstr:=FRstr+Format(strFormat,[s]);
          end
          else
          begin
            //s:= Trim(s);
            strFormat:='%'+LR+inttostr(Size)+ 's';  //inttostr(Length(s))+'s';
            FRstr:=FRstr+Format(strFormat,[s]);
          end;
       end
       else
       begin
         //s:= Trim(s);
         strFormat:='%'+LR+inttostr(Size)+ 's';  //inttostr(Length(s))+'s';
         FRstr:=FRstr+Format(strFormat,[s]);
       end;
    end
    else
    begin
      strFormat:='%'+LR+inttostr(Size)+ 's';  //inttostr(Length(s))+'s';
      FRstr:=FRstr+Format(strFormat,[s])+FlV;
    end;
  end
  else
  begin
    //s:=Trim(s);
    strFormat:='%'+LR+inttostr(Size)+'s';//inttostr(size)+'s';
    FRstr:=FRstr+Format(strFormat,[s]);
    //FRstr:= Trim(FRstr);
  end;

  if line then
   if FirstStr then
    FRStr:=FlV+FRstr+FlV
      else
        FRStr:=FRStr+FlV;  //adds box line +Flv
end;
(* Zuhal Nihal Ekleme bitme*)
//Add string

procedure TlicgEReport.ADDstr(s: string; Size: Integer; Align: TlicgERColAlign;
  Line: Boolean = True; Comma: Boolean = False);
var
  strFormat: string;
  LR: string;
  FirstStr, deger:Boolean;
begin
  LR := '';
  FirstStr := (FRStr = '');
  //if Size = 0 then
    //Size := Length(s);
  if StrToFloatDef(s,0)=0 then
    deger:= false
  else
    deger:=True;
  if Align = rpLeft then  LR:='-' ;
  if deger then
  begin
  	(* Yusuf Ekleme *)
    if s=' ' then
    begin
       s:= '';
       strFormat:='%'+LR+inttostr(14)+ 's';  //inttostr(Length(s))+'s';
       FRstr:=FRstr+Format(strFormat,[s]);
    end;
    if line then
    begin
       if not FirstStr then
       begin
          if Comma then
          begin
            //s:= Trim(s);
            s:=Format('%.2n', [StrToFloat(s) * 1.0]);
            strFormat:='%'+LR+inttostr(14)+ 's';//inttostr(Length(s))+'s';
            FRstr:=FRstr+Format(strFormat,[s]);
          end
          else
          begin
            //s:= Trim(s);
            strFormat:='%'+LR+inttostr(14)+ 's';  //inttostr(Length(s))+'s';
            FRstr:=FRstr+Format(strFormat,[s]);
          end;
       end
       else
       begin
         //s:= Trim(s);
         strFormat:='%'+LR+inttostr(14)+ 's';  //inttostr(Length(s))+'s';
         FRstr:=FRstr+Format(strFormat,[s]);
       end;
    end
    else
    begin
      strFormat:='%'+LR+inttostr(14)+ 's';  //inttostr(Length(s))+'s';
      FRstr:=FRstr+Format(strFormat,[s])+FlV;
    end;
  end
  else
  begin
    //s:=Trim(s);
    strFormat:='%'+LR+inttostr(14)+'s';//inttostr(size)+'s';
    FRstr:=FRstr+Format(strFormat,[s]);
    //FRstr:= Trim(FRstr);
  end;

  if line then
     if FirstStr then FRStr:=FlV+FRstr+FlV else FRStr:=FRStr+FlV;  //adds box line +Flv
  (* Yusuf Ekleme Son *)
end;//ADDRLine

//Add string (column string) to Report line

procedure TlicgEReport.InsertRLine;
begin
  ADDRLine(FRstr);
  FRstr := '';
end; //ADDRLine

//Add string to Report Line

procedure TlicgEReport.ADDRLine(s: AnsiString; ForProject: Boolean = True);
var
  i:Integer;
  vLeftMarginStr, c , k: string;
begin
(* Yusuf Ekleme *)
  vLeftMarginStr := StringOfChar(' ',FLeftMargin);
    k:='';                       //////
//  for i:=1 to length(s) do
//  begin
//    c:= copy(s, i, 1);
//    if c='Ú' then
//      k:= k + #$250C   //DA
//    else if c='Ä' then
//      k:= k + #$2500       //C4
//    else if c='Â' then
//      k:= k + #$252C     //C2
//    else if c='¿' then
//      k:= k + #$2510    //BF
//    else if c='³' then
//      k:= k + #$2502        //B3
//    else if c='Ã' then
//      k:= k + #$251C      //C3
//    else if c='Å' then
//      k:= k + #$253C   //C5
//    else if c='´' then
//      k:= k + #$2524     //B4
//    else if c='À' then
//      k:= k + #$2514
//    else if c='Á' then
//      k:= k + #$2534
//    else if c='Ù' then
//      k:= k + #$2518
//    else if c='' then
//      k:= k + ''
//    else
//      k:= k+c;
//  end;
  StringBuilder := TStringBuilder.Create;
  StringBuilder.Append(s);
  StringBuilder.Replace('Ú', #$250C);
  StringBuilder.Replace('Ä', #$2500);
  StringBuilder.Replace('Â', #$252C);
  StringBuilder.Replace('¿', #$2510);
  StringBuilder.Replace('³', #$2502);
  StringBuilder.Replace('Ã', #$251C);
  StringBuilder.Replace('Å', #$253C);
  StringBuilder.Replace('´', #$2524);
  StringBuilder.Replace('À', #$2514);
  StringBuilder.Replace('Á', #$2534);
  StringBuilder.Replace('Ù', #$2518);
  StringBuilder.Replace('', '');
  k := StringBuilder.ToString;
  StringBuilder.Free;
  k:=vLeftMarginStr+k;                                 //
  if Length(k) > 0 then
    FRLines.Write(k[1], Length(k) * Sizeof(k[1]));   //Ansi
  FRLines.Size;
  if not ForProject then
    FProject.Add(k)
  else
  begin
    FProject.Add(k);
    FPrintLines.Add(k);  //.lines.add(k);
  end;
  (* Yusuf Ekleme Son *)
end;//ADDRLine

//Add header column

procedure TlicgEReport.ADDHeaderCol(s: string; Size: Integer; Align:
  TlicgERColAlign; Line: Boolean);
begin
  ADDstr(s, Size, Align, False);
  FHeaderColumn.Add(FRstr);
  FRStr := '';
end; //ADDHeaderCol

//Nihal Zuhal Ekleme başlama
procedure TlicgEReport.ADDHeaderColi(s: string; Size: Integer; Align:
  TlicgERColAlign; Line: Boolean);
begin
  ADDstri(s, Size, Align, False);
  FHeaderColumn.Add(FRstr);
  FRStr := '';
end; 
//Nihal Zuhal Ekleme bitme

//Clear cols

procedure TlicgEReport.ClearHeaderCol;
begin
  FHeaderColumn.Clear;
end; //ClearHeaderCol

//Print header

procedure TlicgEReport.PrintHeader;
var
  i: Integer;
  //hs: string; //header string
begin
  for i := 0 to FHeaderColumn.Count - 1 do //TExts
  begin
    AddStr(FHeaderColumn[i], length(FHeaderColumn[i]), rpLeft, True);
  end; //For I
  ADDRLine(FRStr);
  FRstr := '';
end;

//Print Detail Line
procedure TlicgEReport.PrintLineAfterRecord(isDoubleLine: Boolean = False);
var
  fs: string;
  I: Integer;
begin
  fs:='';//FlVMR;
  for i := 0 to FHeaderColumn.Count - 1 do
  begin
    if isDoubleLine then
      fs := fs + StringOfChar('=', length(FHeaderColumn[i]));
	  (*
    else
      fs := fs + StringOfChar(Flh, length(FHeaderColumn[i]));
    if (i < FHeaderColumn.Count - 1) then
      fs := fs + FlCR;*)
  end;
  //fs := fs + FlVML;
  //ADDRLine(fs);
end;

//Print footer line

procedure TlicgEReport.PrintFooterLine;
var
  i: Integer;
  fs: string; //footer string
begin
  fs := FlLB;
  for i := 0 to FHeaderColumn.Count - 1 do  //top line
  begin
    //fs := fs + StringOfChar(Flh, length(FHeaderColumn[i]));
    if (i < FHeaderColumn.Count - 1) then
      fs := fs + FlHMT;

  end; //For I
  fs := fs + FlRB;
  ADDRLine(fs);
end;

procedure TlicgEReport.PrintToScreen(CizgiEnt, BaslikEnt, SatirEnt: IlicgEntityList; point: TlicgCoor; charWidth,
 charHeight: Double; satirSayisi: Integer; VectFontName: string; _color: TColor; Colored: Boolean = False;
 SatirAraligi: Double = 0; Carpan: Double = 1; EntityGroupName: String = '');
(* Yusuf Ekleme *)
var
  sList, wordList, DistList: TStringList;
  i, j, l, n, t, y, o, z, q, PreJ, count, count1, counter,
  projesayisi, renkcount: Integer;
  p, aCoor: TlicgCoor;
  tmpEnt, tmpEntLine: IlicgEntity;
  c, s, k, m, margin, harf, v, temp: String;
  TotalWidth, SatirArasiBosluk: Double;
  ATotalWidth, Modi, SatirUzunlugu, EnUzunSatir: Integer;
  SatirUzunluguDouble, EnUzunSatirDouble, CoorForTop, CoorForBottom, CoorForBegin: Double;
  flag, ContainsTextIli, AnsiContainsBakilan, ltfmi: Boolean;
  ICoor: IlicgVector;
begin
  ICoor := Licad.CreateEntityFactory.MakeVector(_3D, 2);
  sList := TStringList.Create;
  wordList := TStringList.Create;
  distlist := TStringList.Create;
  ATotalWidth := 0;
  flag := True;
  prej := 1;
  projesayisi := 0;
  if SatirAraligi <> 0 then
    SatirArasiBosluk := charHeight + SatirAraligi
  else
    SatirArasiBosluk := charHeight + (charHeight/3);

   try
     Self.ReportBuffer.Position := 0;
     sList.AddStrings(FPrintLines); //LoadFromStream(self.ReportBuffer, TEncoding.Unicode);
     p := AsCoor(0,0);
     n := 0;
     counter := 0;
     if Copy(sList[1], 1, 1) <> '┌' then
       ltfmi := True
     else
       ltfmi := false;
     for i := 1 to sList.Count do  //i:=0 to sList.Count-1 do
     begin
       y := i-1;
       for z := 1 to  Length(sList[i-1]) do
       begin
         v := copy(SList[i-1], z, 1);
         if flag then
         begin
           if (v = '└') then
           begin
             if AnsiContainsText(sList.Text, ' İli ') then
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
     if AnsiContainsText(sList.Text, ' İli ') then
       ContainsTextIli := True
     else
       ContainsTextIli := False;

     if AnsiContainsText(sList.Text, 'Bakılan') then
       AnsiContainsBakilan := True
     else
       AnsiContainsBakilan := False;

     if ContainsTextIli then
     begin
       for i := 1 to sList.Count do  //i:=0 to sList.Count-1 do
       begin
         l := 0;
         count1 := 0;
         y := i - 1;
         for z := 1 to  Length(sList[i-1]) do
         begin
           v := copy(SList[i-1], z, 1);
           if v = '├' then
             n := ((i-1) mod (q+1));
         end;
         if (projesayisi <> 0) then
         begin
           if ltfmi then
           begin
             if (y mod (q+1)<>0) and (y mod (q+1)<>1) and (y mod (q+1) <> 2) and (y mod (q+1)<>3) and (y mod (q+1)<>projesayisi) and (y mod (q+1)<> projesayisi+1) and
                 {(y mod (q+1)<> projesayisi+2) and} (y mod (q+1) <> projesayisi+3) and (y mod (q+1)<> projesayisi+4) and
                 (y mod (q+1)<>n) and (y<>sList.Count-1) and (y mod (q+1) <> 2) then     // q+1 yerine 65
               wordList.add(#13#10);
           end
           else
           begin
             if (y mod (q+1)<>0) and (y mod (q+1)<>1) and (y mod (q+1)<>projesayisi) and (y mod (q+1) <> projesayisi+1) and
                 {(y mod (q+1)<> projesayisi+2) and} (y mod (q+1)<> projesayisi+3) and (y mod (q+1) <> projesayisi+4) and
                 (y mod (q+1)<>n) and (y<>sList.Count-1) and (y mod (q+1)<>2) then     // q+1 yerine 65
               wordList.add(#13#10);
           end;
         end
         else
         begin
           if ltfmi then
           begin
             if (y mod (q+1)<>0) and (y mod (q+1)<>1) and (y mod (q+1)<>2) and (y mod (q+1)<>3) and (y mod (q+1)<>4)
                  and (y mod (q+1)<>n) and (y<>sList.Count-1) then
               wordList.add(#13#10);
           end
           else
           begin
             if (y mod (q+1)<>0) and (y mod (q+1)<>1) and (y mod (q+1)<>2) and (y mod (q+1)<>3)
                  and (y mod (q+1)<>n) and (y<>sList.Count-1) then
               wordList.add(#13#10);
           end;
         end;
         k := '';
         for j := 1 to  Length(sList[i-1]) do
         begin
           m := Copy(sList[i-1],j,1);
           if m = '├' then
             o := i;

           if i mod (q+1)< projesayisi+1 then
           begin
             if m = '│' then
             begin
               inc(l);
               inc(count1);
               if (l mod 3) <> 1 then
               begin
                 if (i mod (q+1)<>0) and (i mod (q+1)<>1) {and (i mod (q+1)<>projesayisi+1)} then
                 begin
                   k := TrimLeft(k);
                   k := TrimRight(k);
                   wordList.Add(k);
                 end;
                 k := '';
               end;
               if count1 = 2 then
                 prej := j;
             end
             else
             begin
                 k := k + m;
             end;
           end
           else
           begin
             if m = '│' then
             begin
               inc(l);
               inc(count1);
               if (l mod 2) = 0 then
               begin
                 if (i mod (q+1)<>0) and (i mod (q+1)<>1) and (i mod (q+1) <> projesayisi+1) and (i mod (q+1)<>projesayisi+2) and
                    (i mod (q+1)<>projesayisi+3) {and (i mod (q+1)<>projesayisi+4)} and (i mod (q+1)<>projesayisi+5) and
                    (i mod (q+1)<>o) then
                 begin
                   k := TrimLeft(k);
                   k := TrimRight(k);
                   wordList.Add(k);
                 end;
                 inc(l);
                 k := '';
               end;
               if count1 = 2 then
                 prej := j;
               if (i = projesayisi+4) and (count1 <> 1) and (count1 <> 2) then
               begin
                 distlist.Add(IntToStr(j - prej));
                 prej := j;
               end;
             end
             else
             begin
               k := k + m;
             end;
           end;
         end;
       end;
     end
     else
     begin
       for i := 1 to sList.Count do
       begin
         l := 0;
         count1 := 0;
         y := i - 1;
         for z := 1 to  Length(sList[i - 1]) do
         begin
           v := copy(SList[i - 1], z, 1);
           if v = '├' then
             n := ((i - 1) mod (q + 1));
         end;
         if (projesayisi <> 0) then
         begin
           if (y mod (q+1) <> 0) and (y mod (q+1) <> 1) and (y mod (q+1) <> projesayisi) and (y mod (q+1) <> projesayisi + 1) and
               {(y mod (q+1)<> projesayisi+2) and} (y mod (q+1) <> projesayisi + 3) and (y mod (q+1) <> projesayisi + 4) and
               (y mod (q+1) <> n) and (y <> sList.Count-1) and (y mod (q+1) <> 2) then     // q+1 yerine 65
           begin
             wordList.add(#13#10);
           end;
         end
         else
         begin
           if (y mod (q+1)<>0) and (y mod (q+1)<>1) and (y mod (q+1)<>2) and (y mod (q+1)<>3)
                and (y mod (q+1)<>n) and (y<>sList.Count-1) then     // q+1 yerine 65
           begin
             wordList.add(#13#10);
           end;
         end;
         k := '';
         for j := 1 to  Length(sList[i - 1]) do
         begin
           m := Copy(sList[i-1], j, 1);
           if m = '├' then
             o := i;
           if m = '│' then
           begin
             inc(l);
             inc(count1);
             if (l mod 2) = 0 then
             begin
               if (i mod (q+1) <> 0) and (i mod (q+1) <> 1) and (i mod (q+1) <> 2) and (i mod (q+1) <> o) then
               begin
                 k := TrimLeft(k);
                 k := TrimRight(k);
                 wordList.Add(k);
               end;
               inc(l);
               k := '';
             end;
             if count1 = 2 then
               prej := j;
             if (i = 3) and (count1 <> 1) and (count1 <> 2) then
             begin
               distlist.Add(IntToStr(j-prej));
               prej := j;
             end;
           end
           else
           begin
             k := k + m;
           end;
         end;
       end;
     end;

     if ContainsTextIli then
     begin
       counter := 0;
       t := 0;
       renkcount := 0;
       z := 0;
       TotalWidth := point.X;
       p := AsCoor(point.X, point.Y - 1*(SatirArasiBosluk));
       for i := 1 to sList.Count do
       begin
         flag := True;
         p := AsCoor(totalwidth, point.Y -((i-1) mod (q+1)) * (SatirArasiBosluk));
         count := -1;
         if ((i-1) mod (q+1) = 1) and (ltfmi) then
         begin
           ACoor := AsCoor(p.x, p.y);
           TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, sList[i-1]);
           tmpEnt.DrawTools.FontTool.Height := charHeight;
           tmpEnt.DrawTools.FontTool.Angle := 0;
           tmpEnt.DrawTools.FontTool.Name := VectFontName;
           tmpEnt.DrawTools.FontTool.Color := clBlue;
           tmpEnt.GroupName := EntityGroupName;
           BaslikEnt.Add(TmpEnt);
         end;

         if z < 2 then
         begin
           for j := 1 to Length(sList[i-1]) do
           begin
             v := Copy(sList[i-1], j, 1);
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

         if (Copy(sList[i-1], Length(sList[i-1]), 1) = '┐') or (Copy(sList[i-1],Length(sList[i-1]),1)= '┤') or (Copy(sList[i-1],Length(sList[i-1]),1)= '┘') then
         begin
           tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
           tmpEntLine.DrawTools.PenTool.Color := clGreen;
           tmpEntLine.GroupName := EntityGroupName;
           ICoor.Add(TotalWidth, p.y);
           ICoor.Add(p.X + (slist[i-1].Length-1) * charWidth, p.Y);
           tmpEntLine.Geometry.Points.Assign(ICoor);
           CizgiEnt.Add(tmpEntLine.clone);
           ICoor.Clear;
           flag := false;
           if (Copy(sList[i-1],Length(sList[i-1]),1) = '┐') then
             CoorForBegin := p.Y;
         end;
         if (Copy(sList[i-1],1,1) = '└') then
         begin
           for j := 1 to  Length(sList[i-1]) do
           begin
             c := Copy(sList[i-1],j,1);
             if (c = '└') or (c = '┴') or (c = '┘') then
             begin
               tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
               tmpEntLine.DrawTools.PenTool.Color := clGreen;
               tmpEntLine.GroupName := EntityGroupName;
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
             (*  if (counter mod 2 = 0) then
               begin
                 if ltfmi then
                   ICoor.Add(p.X, point.Y - 2*(SatirArasiBosluk))
                 else
                   ICoor.Add(p.X, point.Y - (SatirArasiBosluk));
               end
               else
                 ICoor.Add(p.X, point.Y - (projesayisi+2) * (SatirArasiBosluk));  *)
               tmpEntLine.Geometry.Points.Assign(ICoor);
               CizgiEnt.Add(tmpEntLine.Clone);
               ICoor.Clear;
               if c = '┘' then
                 inc(counter);
             end;
             p.x := p.x + (charWidth);
             flag := False;
           end;
         end;
         if flag then
         begin
           if (Copy(sList[i-1],1,1) = '│') then
           begin
             for j := 1 to Length(sList[i-1]) do
             begin
               c := Copy(sList[i-1], j, 1);
               if c = '│' then
               begin
                 if j = 1 then
                   ACoor := AsCoor(p.x + 2*carpan, p.y)    // p.x+1
                 else
                 begin
                   if t < wordlist.Count then
                       ACoor := AsCoor(p.x + ((15*charwidth - length(wordlist[t])*charwidth)/2) - 1*carpan, p.y);
                 end;

                 if (i mod (q+1) = projesayisi+4) or (j = 1) or (i = renkcount + 3) then
                 begin
                   if t < wordlist.Count then
                     TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, wordList[t]);
                   tmpEnt.DrawTools.FontTool.Height := charHeight;
                   tmpEnt.DrawTools.FontTool.Angle := 0;
                   tmpEnt.DrawTools.FontTool.Name := VectFontName;
                   tmpEnt.DrawTools.FontTool.Color := clblue;
                   tmpEnt.GroupName := EntityGroupName;
                   BaslikEnt.Add(TmpEnt);
                 end
                 else
                 begin
                   if t < wordlist.Count then
                     TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, wordList[t]);
                   tmpEnt.DrawTools.FontTool.Height := charHeight;
                   tmpEnt.DrawTools.FontTool.Angle := 0;
                   tmpEnt.DrawTools.FontTool.Name := VectFontName;
                   tmpEnt.DrawTools.FontTool.Color := clred;
                   tmpEnt.GroupName := EntityGroupName;
                   SatirEnt.Add(tmpEnt);
                 end;
                 inc(t);
               end;
               p.x := p.x + (charWidth);
             end;
           end;
         end;
         SatirUzunluguDouble := p.x;
         if EnUzunSatirDouble < SatirUzunluguDouble then  EnUzunSatirDouble := SatirUzunluguDouble;
         if (counter mod 2 = 0) and ((Copy(sList[i-1], Length(sList[i-1]), 1) = '┘')) then
         begin
           TotalWidth := EnUzunSatirDouble + 5 * carpan;
           p := AsCoor(totalwidth, p.Y);
         end;
       end;
     end
     else if AnsiContainsText(sList.Text, '─┐') then
     begin
       if AnsiContainsText(sList.Text, 'Bakılan') then
         AnsiContainsBakilan := True
       else
         AnsiContainsBakilan := False;
       t := 0;
       renkcount := 0;
       z := 0;
       TotalWidth := point.X;
       p := AsCoor(point.X, point.Y - 1*(SatirArasiBosluk));
       for i := 1 to sList.Count do
       begin
         flag := True;
         p := AsCoor(totalwidth, point.Y -((i-1) mod (q+1)) * (SatirArasiBosluk));
         count := -1;

         if ((i-1) mod (q+1) = 1) and (ltfmi) then
         begin
           ACoor := AsCoor(p.x, p.y);
           TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, sList[i-1]);
           TmpEnt.DrawTools.FontTool.Height := charHeight;
           TmpEnt.DrawTools.FontTool.Angle := 0;
           TmpEnt.DrawTools.FontTool.Name := VectFontName;
           TmpEnt.DrawTools.FontTool.Color := clBlue;
           TmpEnt.GroupName := EntityGroupName;
           BaslikEnt.Add(TmpEnt);
         end;

         if z = 0 then
         begin
           for j := 1 to Length(sList[i-1]) do
           begin
             v := Copy(sList[i-1], j, 1);
             if V = '┬' then
             begin
               CoorForTop := p.Y;
               inc(z);
               break;
             end;
           end;
         end;

         if (Copy(sList[i-1],Length(sList[i-1]),1)= '┐') or (Copy(sList[i-1],Length(sList[i-1]),1)= '┤') or (Copy(sList[i-1],Length(sList[i-1]),1)= '┘') then
         //(pos('┐',sList[i-1]) = Length(sList[i-1])) or (pos('┤',sList[i-1]) = Length(sList[i-1])) or (pos('┘',sList[i-1]) = Length(sList[i-1])) then
         begin
           tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
           tmpEntLine.DrawTools.PenTool.Color := clGreen;
           tmpEntLine.GroupName := EntityGroupName;
           ICoor.Add(TotalWidth, p.y);
           ICoor.Add(p.X + (slist[i-1].Length-1) * charWidth, p.Y);
           tmpEntLine.Geometry.Points.Assign(ICoor);
           CizgiEnt.Add(tmpEntLine.clone);
           ICoor.Clear;
           flag := false;
           if (Copy(sList[i-1],Length(sList[i-1]),1)= '┐') then
             CoorForBegin := p.Y;
         end;
         if (Copy(sList[i-1],1,1)= '└') then
         begin
           for j := 1 to  Length(sList[i-1]) do
           begin
             c := Copy(sList[i-1],j,1);
             if (c = '└') or (c = '┴') or (c = '┘') then
             begin
               tmpEntLine := Licad.CreateEntityFactory.MakeEntity(idLine, 2, _2D);
               tmpEntLine.DrawTools.PenTool.Color := clGreen;
               tmpEntLine.GroupName := EntityGroupName;
               ICoor.Add(p.X, p.y);
               if c = '┴' then
                 ICoor.Add(p.x, CoorForTop)
               else
                 ICoor.Add(p.x, CoorForBegin);
               (*if ltfmi then
                 ICoor.Add(p.X, point.Y - 2*(SatirArasiBosluk))
               else
                 ICoor.Add(p.X, point.Y - (SatirArasiBosluk));*)
               //ICoor.Add(p.X, point.Y + (-1) * (SatirArasiBosluk));
               tmpEntLine.Geometry.Points.Assign(ICoor);
               CizgiEnt.Add(tmpEntLine.clone);
               ICoor.Clear;
             end;
             p.x := p.x + (charWidth);
             flag := False;
           end;
         end;
         if flag then
         begin
           if (Copy(sList[i-1],1,1) = '│') then
           begin
             for j := 1 to Length(sList[i-1]) do
             begin
               c := Copy(sList[i-1], j, 1);
               if c = '│' then
               begin
                 if j = 1 then
                   ACoor := AsCoor(p.x + 2*carpan, p.y)
                 else
                 begin
                   if t < wordlist.Count then
                       ACoor := AsCoor(p.x + ((15*charwidth - length(wordlist[t])*charwidth)/2) - 2*carpan , p.y);
                 end;

                 if (i mod (q+1) = 3) or (j = 1) or (i = renkcount + 3) then
                 begin
                   if t < wordlist.Count then
                     TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, wordList[t]);
                   tmpEnt.DrawTools.FontTool.Height := charHeight;
                   tmpEnt.DrawTools.FontTool.Angle := 0;
                   tmpEnt.DrawTools.FontTool.Name := VectFontName;
                   tmpEnt.DrawTools.FontTool.Color := clblue;
                   tmpEnt.GroupName := EntityGroupName;
                   BaslikEnt.Add(tmpEnt);
                 end
                 else
                 begin
                   if t < wordlist.Count then
                     TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, wordList[t]);
                   tmpEnt.DrawTools.FontTool.Height := charHeight;
                   tmpEnt.DrawTools.FontTool.Angle := 0;
                   tmpEnt.DrawTools.FontTool.Name := VectFontName;
                   tmpEnt.DrawTools.FontTool.Color := clRed;
                   tmpEnt.GroupName := EntityGroupName;
                   SatirEnt.Add(tmpEnt);
                 end;
                 inc(t);
               end;
               p.x := p.x + (charWidth);
             end;
           end;
         end;
         if (Copy(sList[i-1],Length(sList[i-1]),1)= '┘') then
         //pos('┘',sList[i-1]) = Length(sList[i-1]) then
         begin
           TotalWidth := p.X + 5*carpan;
           p := AsCoor(totalwidth, p.Y);  //0)
         end;
       end;
     end
     else
     begin
       EnUzunSatir := 0;
       renkcount := 0;
       TotalWidth := point.X;
       for i := 1 to sList.Count do
       begin
         p := AsCoor(totalwidth, point.Y -((i-1) mod (satirSayisi)) * (SatirArasiBosluk));
         ACoor := AsCoor(p.x, p.y);
         TmpEnt := Licad.CreateEntityFactory.MakeText(ACoor, sList[i-1]);
         tmpEnt.DrawTools.FontTool.Height := charHeight;
         tmpEnt.DrawTools.FontTool.Angle := 0;
         tmpEnt.DrawTools.FontTool.Name := VectFontName;
         tmpEnt.DrawTools.FontTool.Color := clred;
         tmpEnt.GroupName := EntityGroupName;
         SatirEnt.Add(TmpEnt);
         SatirUzunlugu := length(slist[i-1]);
         if EnUzunSatir < SatirUzunlugu then  EnUzunSatir := SatirUzunlugu;

         if ((i - 1) mod (satirsayisi - 1) = 0) and (i-1 <> 0) then
         begin
           TotalWidth := p.x + charwidth * EnUzunSatir + 5*carpan;
           p:= AsCoor(totalwidth, p.Y);
         end;
       end;
     end;
     Rectx := p.x + 5*carpan;
   finally
     sList.Free;
     wordList.Free;
     distlist.Free;
     tmpEntLine := nil;
     tmpEnt := nil;
   end;
(* Yusuf Ekleme Son *)
end;

{
 ------------------------------------------------------------------------------------------------
                            TlicgEReport Imp. end
 ------------------------------------------------------------------------------------------------
}

{
Function LoadPackageLIB(PackageName,FuncName:String):Boolean;
var
  _fn : string;
begin

   Result:=True;
   _fn := FuncName + #0;
   ppHandle:= LoadPackage( PackageName );
   if ppHandle=0 then
   begin
     Result:=False;
     Exit;
   end;
   ppFunction:=GetProcAddress(ppHandle, @_fn[1]);
end;//LoadPackageLIB
}



 //String numeric bir değer mi?  Gönderilen string içeriğinin
 //geçerli bir sayısal değer olup olmadığını döndürür.
function StrISNumeric(tmpStr: string): Boolean;
var
  cC, lengthStr: Integer;
begin
  tmpStr := SysUtils.Trim(tmpStr);
  lengthStr := Length(tmpStr);
  for cC := 1 to lengthStr do
  begin
    case ord(tmpStr[cC]) of
      48, 49, 50, 51, 52, 53, 54, 55, 56, 57:
        begin
          Result := true;
        end;
    else
      begin
        Result := False;
        Break;
      end;
    end; //case
  end;
end;


//Calculates printable row count for current page setup
//IMPORTANT !
//Before this procedure running, You must passed current font to the printer.canvas.font
//if not the prnPrintableRowCount and prnMMSize will not calculate as properly or
//it will calculate as Courier New Size 10 OEM_CHARSET.

procedure CalculatePRow(FTopmargin, FBottomMargin: Integer; var prnMMSize:
  Integer; var prnPrintableRowCount: Integer; DefaultFont: Boolean = False);
var
  tprnName: string;
  tprnOrientation: TPrinterOrientation; //(poPortrait, poLandscape)
  tprnOffsetPixels: TOffset;
  tprnPixelsPerMMX: Real;
  tprnPixelsPerMMY: Real;
  tprnPageHeightMM: Integer; {page length (mm)}
  defFont: TFont;
begin
  if DefaultFont then
  begin
    defFont := Tfont.Create;

    with defFont do
    begin
      Name := 'Courier New';
      Size := 9;
      Height := -12;
      CharSet := OEM_CHARSET;
      Pitch := fpFixed;
    end; //With
    Printer.Canvas.Font.Assign(defFont);

    defFont.Free;
  end; //if DefaultFont

  GetPDeviceSettings(FTopmargin, FBottomMargin, tprnName, tPrnOrientation,
    tprnOffsetPixels, tprnPixelsPerMMX, tprnPixelsPerMMY, tprnPageHeightMM,
    prnMMSize, prnPrintableRowCount);
//  showmessage(inttostr(prnMMSize)+' '+inttostr(prnPrintableRowCount)+
 //             ' '+inttostr(Printer.Canvas.Font.Height ));
end;



//Gets Printer device settings and calculates printable row count for current page setup
//IMPORTATNT !
//Before this procedure running, You must passed current font to the printer.canvas.font
//if not the prnPrintableRowCount and prnMMSize will not calculate as properly.

procedure GetPDeviceSettings(FTopmargin, FBottomMargin: Integer; var prnName:
  string; //The printer name
  var prnOrientation: TPrinterOrientation; //(poPortrait, poLandscape)
  var prnOffsetPixels: TOffset; var prnPixelsPerMMX: Real; var prnPixelsPerMMY:
  Real; var prnPageHeightMM: Integer; {page length (mm)}
  var prnMMSize: Integer; var prnPrintableRowCount: Integer);
var
  retval: integer;
  FPageWidthPixel, FPageHeightPixel, PixX, PixY, PixelSize: Integer;
begin
  PixelSize := Printer.Canvas.TextHeight('Yy');
  prnName := Printer.Printers[Printer.PrinterIndex];  {Get the name}
  FPageHeightPixel := Printer.PageHeight;                 {Page height}
  FPageWidthPixel := Printer.PageWidth;                   {Page Width}
  prnOrientation := Printer.Orientation;  //TPrinterOrientation = (poPortrait, poLandscape)
{Orientation}
    {Get the printable area offsets}
    {$IFDEF WIN32}
  prnOffsetPixels.X := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  prnOffsetPixels.Y := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
    {$ELSE}
  retval := Escape(Printer.Handle, GETPRINTINGOFFSET, 0, nil, @prnOffsetPixels);
    {$ENDIF}
    {Get Pixels per Milimeter Ratio}
  PixX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  PixY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  prnPixelsPerMMX := INCHES_PER_MILIMETER * PixX;
  prnPixelsPerMMY := INCHES_PER_MILIMETER * PixY;
  prnPageHeightMM := Round(FPageHeightPixel / prnPixelsPerMMY);
  prnMMSize := Round(PixelSize / prnPixelsPerMMY);
  prnPrintableRowCount := Round((prnPageHeightMM - (FtopMargin + FbottomMargin))
    / prnMMSize);
    {showmessage('prn name:'+prnName+#13#10+
               'FPageHeightPixel:'+inttostr(FPageHeightPixel)+#13#10+
               'prnPageHeightMM:'+inttostr(prnPageHeightMM)+#13#10+
               'prnPixelsPerMMY:'+floattostr(prnPixelsPerMMY)+#13#10+
               'pixY:'+inttostr(pixY)+#13#10+
               'Text pixel size:'+inttostr(PixelSize)+#13#10+
               'mmsize:'+inttostr(prnMMSize)+#13#10+
               'Printable Row Count:'+inttostr(prnPrintableRowCount)
               );  }

end;  //GetDeviceSettings

//print To Screen

(*procedure PrintToScreen(ReportBuff: TStream; groupEnt: IlicgEntity);
var
  sList: TStringList;
  //i: Integer;
  //textEnt: IlicgEntity;
begin
  sList := TStringList.Create;
  try
    sList.LoadFromStream(ReportBuff);
    if groupEnt = Nil then
      groupEnt := Licad.CreateEntityFactory.MakeEntity(idGroup, 0, _3D);
  finally
    sList.Free;
  end;
end;       *)


//Print Preview
function PPreview(ReportBuff: TStream; FTopMargin, FBottomMargin: Integer; _IsInfo: Boolean = false):
  Integer; register;
(* Yusuf Ekleme *)
begin
  ReportBuff.Seek(0,0) ;
  ReportBuff.Size;
  if _IsInfo then
    Licad.TextEditor.Execute(FProject.Text)
  else
    Licad.TextEditor.Execute(FPrintLines.Text);

  ReportBuff.Size := 0;//IMPORTANT ! (for  memory )
  Result := 0;
  (*
  with TfmMetinEditoru.Create(Application) do
    try
      ReportBuff.Seek(0,0) ;
      ReportBuff.Size;
      //Editor.Encoding:= TEncoding.Unicode;
      if _IsInfo then
        Editor.Text := FProject.Text    //fppreview1.lines.Text;
      else
        Editor.Text := FPrintLines.Text;
      ReportBuff.Size :=0;//IMPORTANT ! (for  memory )
      Result := showmodal;
    finally
      Free;
    end;
    result:=0;      *)
(* Yusuf Ekleme Son *)
end;

//General Report Preview call function

function ReportPreview(ReportBuff: TStream; TopMargin, BottomMargin: Integer; _IsInfo: Boolean = false): Integer;
begin

  Result := PPReview(ReportBuff, TopMargin, BottomMargin, _IsInfo);

end; // ReportPreview

procedure HelpMe(FHandle: THandle; FHelpPath: string);
var
  hn: string;
begin
  hn := FHelpPath + #0;
  HtmlHelp(FHandle, @hn[1], HH_DISPLAY_TOPIC, 0);

end;

procedure CloseAllHelpWin;
begin
  HtmlHelp(0, nil, HH_CLOSE_ALL, 0);
end;

//Form'da yer alan kontrol değerlerinin Values.INI de saklanması ve
//form açıldığında (OnCreate) önceki değerlerin yüklenmesi için ReadFormValue ve SaveFormValue
//yazılmıştır.(İlk uygulama örneği fParalelAl.pas )
function ReadFormValue(FormAndControlName: string): string;
var
  oINI: TINIFile;
begin
  oINI := TINIFile.Create(ExtractFilePath(Application.exeName) + 'Values.INI');
  Result := OINI.ReadString('Values', FormAndControlName, '');
  oINI.free;
end;

procedure SaveFormValue(FormAndControlName, Value: string);
var
  oINI: TINIFile;
begin
  oINI := TINIFile.Create(ExtractFilePath(Application.exeName) + 'Values.INI');
  oINI.WriteString('Values', FormAndControlName, Value);
  oINI.free;
end;

//DrawBoxProp.INI den istenen değerin getirilmesini sağlar.
function GetDrawBoxPropertyValue(propertyName: string): string;
var
  oINI: TINIFile;
begin
  oINI := TINIFile.Create(ExtractFilePath(Application.exeName) + 'DrawBoxProp.INI');
  Result := OINI.ReadString('DrawBoxProperties', propertyName, '');
  oINI.free;
end;

end.


