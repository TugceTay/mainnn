unit Lider.CG.Com.Graphics;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  SysUtils,
  Math,
  Lider.CG.Com.Lib,
  Lider.CG.Com.Base,
  Lider.CG.Com.GIS,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.VectorInt,
  Lider.CG.Com.Geolibrary,
  Lider.CG.Com.EntityInt,
  Lider.CG.Com.DrawToolsInt;

type
  TclNoneBitmapClass = class
    Bitmap: Graphics.TBitmap;
    BitmapInfo: PBitmapInfo;
    Bits: Pointer;
    HeaderSize, BitsSize: DWORD;
    constructor Create(Index: integer);
    destructor Destroy; override;
  end;

  TRGBTripleArray = array[WORD] of TRGBTriple;

  pRGBTripleArray = ^TRGBTripleArray;

  TRGBQuadArray = array[WORD] of TRGBQuad;

  pRGBQuadArray = ^TRGBQuadArray;

  TlicgTileGlobalInfo = record
    dc: HDC;
    TheStream: TStream;
    bmf: TBITMAPFILEHEADER;
    lpBitmapInfo: PBITMAPINFO;
    BitmapHeaderSize: integer;
    TotalBitmapWidth: integer;
    TotalBitmapHeight: integer;
    SourceIsTopDown: Boolean;
    SourceRect: TRect;
    SourceBytesPerScanLine: integer;
    SourceLastScanLine: extended;
    SourceBandHeight: extended;
    SourceFirstTileHeight: extended;
  end;

  TlicgBitmapEx = class
  private
    { internal data }
    FBlendTable: array[-255..255] of Smallint;
    { configuration }
    FPainterObject: IlicgPainterObject;
    FWasSuspended: Boolean;
    FAlphaChannel: Byte;
    FBufferBitmap: TBitmap;
    FTileGlobalInfo: TlicgTileGlobalInfo;
    function TileCurrentBand(const CurrentTileRect: TRect): Boolean;
    procedure SetAlphaChannel(Value: Byte);
    function GetPainterObject: IlicgPainterObject;
    procedure SetPainterObject(const Value: IlicgPainterObject);
    function GetWasSuspended: Boolean;
    function GetAlphaChannel: byte;
    function GetBufferBitmap: TBitmap;
    procedure SetBufferBitmap(Value: TBitmap);
  public
    { methods }
    function BitDIBFromFileInBands(const FileName: string; Stream: TStream; dc:
      HDC; DestLeft, DestTop, DestWidth, DestHeight, DestTotalHeight, SourceLeft,
      SourceTop, SourceWidth, SourceHeight, BufferSize: integer): Boolean;
    { properties }
    property PainterObject: IlicgPainterObject read GetPainterObject write SetPainterObject;
    property WasSuspended: Boolean read GetWasSuspended;
    { AlphaChannel, 0= opaque, >0 = transparent }
    property AlphaChannel: byte read GetAlphaChannel write SetAlphaChannel;
    { the bitmap agains with which will be made transparent this bitmap }
    property BufferBitmap: TBitmap read GetBufferBitmap write SetBufferBitmap;
  end;

  TlicgBILReader = class
  private
    { internal data }
    FFileName: string;
    FBlendTable: array[-255..255] of Smallint;
    { configuration }
    FPainterObject: IlicgPainterObject;
    FWasSuspended: Boolean;
    FAlphaChannel: Byte;
    FBufferBitmap: TBitmap;
    FTileGlobalInfo: TlicgTileGlobalInfo;
    function TileCurrentBand(const CurrentTileRect: TRect): Boolean;
    procedure SetAlphaChannel(Value: Byte);
    function ReadHdr: Boolean;
    function GetAlphaChannel: byte;
    function GetBufferBitmap: TBitmap;
    function GetPainterObject: IlicgPainterObject;
    function GetWasSuspended: Boolean;
    procedure SetBufferBitmap(const Value: TBitmap);
    procedure SetPainterObject(const Value: IlicgPainterObject);
  public
    BIGENDIAN: Boolean;
    SKIPBYTES: integer;
    NROWS: integer;
    NCOLS: integer;
    NBANDS: integer;
    NBITS: integer;
    BANDROWBYTES: integer;
    TOTALROWBYTES: integer;
    BANDGAPBYTES: integer;
    NODATA: integer;
    ULXMAP: double;
    ULYMAP: double;
    XDIM: double;
    YDIM: double;
    constructor Create(const Filename: string);
    { methods }
    function BILFromFileInBands(Stream: TStream; DC: HDC; DestLeft, DestTop,
      DestWidth, DestHeight, DestTotalHeight, SourceLeft, SourceTop, SourceWidth,
      SourceHeight, BufferSize: Integer): Boolean;
    { Properties }
    property PainterObject: IlicgPainterObject read GetPainterObject write SetPainterObject;
    property WasSuspended: Boolean read GetWasSuspended;
    { AlphaChannel, 0= opaque, >0 = transparent }
    property AlphaChannel: byte read GetAlphaChannel write SetAlphaChannel;
    { the bitmap agains with which will be made transparent this bitmap }
    property BufferBitmap: TBitmap read GetBufferBitmap write SetBufferBitmap;
  end;

  { useful routines }
procedure CyrusBeckLineClip(Polyline, Polygon, Result: IlicgVector);

function GetMemEx(size: DWORD): pointer;

function FreeMemEx(p: pointer): pointer;
{Function LoadDIBFromStream(TheStream: TStream;
  Var lpBitmapInfo: PBITMAPINFO;
  Var lpBits: Pointer;
  Var BitmapWidth, BitmapHeight: integer): Boolean; }

function LoadDIBFromFile(const FileName: string; var lpBitmapInfo: PBITMAPINFO;
  var lpBits: Pointer; var BitmapWidth, BitmapHeight: integer): Boolean;

function LoadDIBFromTBitmap(ABitmap: TBitmap; var lpBitmapInfo: PBITMAPINFO; var
  lpBits: Pointer; var BitmapWidth, BitmapHeight: integer): Boolean;

function GetDIBDimensions(const FileName: string; Stream: TStream; var
  BitmapWidth, BitmapHeight: integer; var IsCompressed: Boolean): Boolean;

function GetBILDimensions(const FileName: string; var BitmapWidth, BitmapHeight:
  integer): Boolean;

procedure TransparentStretchDraw(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  Bitmap: TBitmap; SrcX, SrcY, SrcW, SrcH: Integer);

procedure PrintBitmapEx(Canvas: TCanvas; const DestinationRect: TRect; ABitmap:
  TBitmap; const SourceRect: TRect);

procedure Fill8X8Bitmap(ACanvas: TCanvas; DestRect: TRect; Bitmap: TBitmap;
  ForeColor, BackColor: TColor);

procedure PrinterFill8X8Bitmap(ACanvas: TCanvas; DestRect: TRect; Bitmap:
  TBitmap; ForeColor, BackColor: TColor; Factor: Double);

procedure PolygonScreenFill8X8Bitmap(Canvas: TCanvas; Grapher: TlicgBaseGrapher;
  var Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  Bitmap: TBitmap; ForeColor, BackColor: TColor; ClipRgn: HRgn; clNoneBitmapC:
  TclNoneBitmapClass);

procedure PolygonPrinterFill8X8Bitmap(Canvas: TCanvas; Grapher: TlicgBaseGrapher;
  var Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  Bitmap: TBitmap; ForeColor, BackColor: TColor; Factor: Double;
  PlotterOptimized: Boolean; ClipRgn: HRgn);

procedure DoGradientFill(Canvas: TCanvas; Grapher: TlicgBaseGrapher; var
  Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  StartColor, EndColor: TColor; GradientDirection: TlicgFillDirection; ClipRgn: HRgn);

procedure RotateBitmap(const BitmapOriginal: TBitMap; //input bitmap (possibly converted)
  out BitMapRotated: TBitMap; //output bitmap
  const theta: Double; // rotn angle in radians counterclockwise in windows
  const oldAxis: TPOINT; // center of rotation in pixels, rel to bmp origin
  var newAxis: TPOINT); // center of rotated bitmap, relative to bmp origin

function FindContrastColor(aColor: TColor): TColor;

//function CreateText(const AText: string; APosition: TlicgLabelPos; const AAngle:
//  Double; const Pt1, Pt2: TlicgCoor; const AFont: TlicgFontStyle; TrueType:
//  Boolean; IsJustifiedVectotrel: Boolean = true): IlicgEntity;
function CreateText(const ACoor1, ACoor2: TlicgCoor; const AText: string; const AFontTool: IlicgFontTool): IlicgEntity;

procedure DoBlendFill(Canvas: TCanvas; Grapher: TlicgBaseGrapher; var Vertices:
  array of TPoint; var Parts: array of Integer; PartCount: Integer; Color:
  TColor; ClipRgn: HRgn; ATransparency: Byte = 127);

implementation

uses
  Lider.CG.Com.System,
  Lider.CG.Com.Consts,
  Lider.CG.Com.Rtree,
  Lider.CG.Com.LicadInt;

{ CreateText procedure used for on the fly labeling }

//function CreateText(const AText: string; APosition: TlicgLabelPos; const AAngle:
//  Double; const Pt1, Pt2: TlicgCoor; const AFont: TlicgFontStyle; TrueType:
//  Boolean; IsJustifiedVectotrel: Boolean = true): IlicgEntity;

function CreateText(const ACoor1, ACoor2: TlicgCoor; const AText: string; const AFontTool: IlicgFontTool): IlicgEntity;

const
  ENLARGE_FACTOR = 0.15;
var
  Angle: Double;
  DX, DY, MidY: Double;
  P, TmpPt1, TmpPt2: TlicgCoor;
  ATempFontTool: IlicgFontTool;
begin
  ATempFontTool := Licad.CreateEntityFactory.CreateFontTool;
  if AFontTool = nil then
    ATempFontTool.Assign(Licad.Settings.FontTool)
  else
    ATempFontTool.Assign(AFontTool);

  if ACoor1.X < ACoor2.X then
  begin
    TmpPt1 := ACoor1;
    TmpPt2 := ACoor2;
  end
  else
  begin
    TmpPt1 := ACoor2;
    TmpPt2 := ACoor1;
  end;
  { a veces el angulo es recibido con valor 0 }
  if Angle = 0 then
  begin
    MidY := (TmpPt1.Y + TmpPt2.Y) / 2;
    TmpPt1.Y := MidY;
    TmpPt2.Y := MidY;
  end
  else
  begin
    Angle := Angle2d(TmpPt1, TmpPt2);
  end;
  TmpPt2.X := TmpPt1.X + _Distance(ACoor1, ACoor2);

  if ATempFontTool.Name = 'L1' then
  begin
    Result := Licad.CreateEntityFactory.MakeVectorialFittedText(TmpPt1, AText, ATempFontTool);
  end
  else
  begin
    Result := Licad.CreateEntityFactory.MakeText(Tmppt1, AText, ATempFontTool);
  end;

  with Result.Geometry.Extent do
  begin
    DX := abs(UpperRight.x - LowerLeft.x);
    DY := abs(UpperRight.y - LowerLeft.y);
  end;
  { posicion as if the angle was cero }

  (* ilker ileride yeni text pos göre düzenle
  case AFontTool.TextPos of
    lpCenter:
      begin
        p.X := (TmpPt1.X + TmpPt2.X) / 2 - DX / 2;
        p.y := TmpPt1.Y + DY / 2;
      end;
    lpCenterUp:
      begin
        p.X := (TmpPt1.X + TmpPt2.X) / 2 - DX / 2;
        p.Y := TmpPt1.Y + DY * (1 + ENLARGE_FACTOR);
      end;
    lpUpperLeft:
      begin
        p.X := TmpPt1.X;
        p.Y := TmpPt1.Y + DY * (1 + ENLARGE_FACTOR);
      end;
    lpUpperRight:
      begin
        p.X := TmpPt2.X - DX;
        p.Y := TmpPt1.Y + DY * (1 + ENLARGE_FACTOR);
      end;
    lpCenterLeft:
      begin
        p.X := TmpPt1.X;
        p.Y := TmpPt1.Y + DY / 2;
      end;
    lpCenterRight:
      begin
        p.X := TmpPt2.X - DX;
        p.Y := TmpPt1.Y + DY / 2;
      end;
    lpLowerLeft:
      begin
        p.X := TmpPt1.X;
        p.Y := TmpPt1.Y - DY * ENLARGE_FACTOR;
      end;
    lpCenterDown:
      begin
        p.X := (TmpPt1.X + TmpPt2.X) / 2 - DX / 2;
        p.Y := TmpPt1.Y - DY * ENLARGE_FACTOR;
      end;
    lpLowerRight:
      begin
        p.X := TmpPt2.X - DX;
        p.Y := TmpPt1.Y - DY * ENLARGE_FACTOR;
      end;
  end;                  *)
  { now apply the angle }
  if AFontTool.Angle <> 0 then
  begin
    if ATempFontTool.Name = 'L1' then
    begin
      Result.Geometry.Points[0] := TransformPoint2d(p, Rotate2d(Angle, TmpPt1));
      Result.DrawTools.FontTool.Angle := Angle;
    end
    else
    begin
      Result.Geometry.DisableEvents := true;
      Result.Geometry.Points[0] := TransformPoint2d(p, Rotate2d(Angle, TmpPt1));
      Result.DrawTools.FontTool.Angle := Angle;
      Result.Geometry.DisableEvents := false;
    end;
  end
  else
  begin
    if ATempFontTool.Name = 'L1' then
      Result.Geometry.Points[0] := p
    else
      Result.Geometry.Points[0] := p;
  end;
end;

function FindContrastColor(aColor: TColor): TColor;
begin
  if ((aColor and $FF) * 77 + ((aColor shr 8) and $FF) * 150 + ((aColor shr 16)
    and $FF) * 29) > 127 * 256 then
    Result := clBlack
  else
    Result := clWhite;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX, MaskY:
  Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;
  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(0));
  try
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, SrcW, SrcH));
    Save := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    SelectPalette(SrcDC, SavePal, False);
    if SavePal <> 0 then
      SavePal := SelectPalette(MemDC, SavePal, True)
    else
      SavePal := SelectPalette(MemDC, SystemPalette16, True);
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if Save <> 0 then
      SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if SavePal <> 0 then
      SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;
{.$ENDIF}

procedure TransparentDraw(ACanvas: TCanvas; Bitmap: TBitmap; const Rect: TRect);
var
  MaskDC: HDC;
  Save: THandle;
begin
  Save := 0;
  MaskDC := 0;

  with Rect do
  try
    MaskDC := CreateCompatibleDC(0);
    Save := SelectObject(MaskDC, Bitmap.MaskHandle);
    TransparentStretchBlt(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
      Bitmap.Canvas.Handle, 0, 0, Right - Left, Bottom - Top, MaskDC, 0, 0);
  finally
    if Save <> 0 then
      SelectObject(MaskDC, Save);
    if MaskDC <> 0 then
      DeleteDC(MaskDC);
  end;
end;

procedure TransparentStretchDraw(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  Bitmap: TBitmap; SrcX, SrcY, SrcW, SrcH: Integer);
var
  MaskDC: HDC;
  Save: THandle;
begin
  Save := 0;
  MaskDC := 0;

  try
    MaskDC := CreateCompatibleDC(0);
    Save := SelectObject(MaskDC, Bitmap.MaskHandle);
    {Graphics.}     TransparentStretchBlt(DstDC, DstX, DstY, DstW, DstH, Bitmap.Canvas.Handle,
      SrcX, SrcY, SrcW, SrcH, MaskDC, 0, 0);
  finally
    if Save <> 0 then
      SelectObject(MaskDC, Save);
    if MaskDC <> 0 then
      DeleteDC(MaskDC);
  end;
end;


///////////////////////////////////////////////////////////////////////////////
//Rotate  a bitmap about an arbritray center point;
///////////////////////////////////////////////////////////////////////////////
//a structure to hold sine,cosine,distance (faster than angle)
type
  TSiCoDiType = record
    si, co, di: Double; {sine, cosine, distance 6/29/98}
  end;

  {	Calculate sine/cosine/distance from INTEGER Coordinates}

function SiCoDiPoint(const p1, p2: TPoint): TSiCoDiType; {out}
{	This is MUCH faster than using angle functions such as arctangent}
{11.96    Jim Hargis  original SiCoDi for rotations.
{11/22/96 modified for Zero length check, and replace SiCodi}
{6/14/98  modified  for Delphi}
{6/29/98  renamed from SiCo point}
{8/3/98	  set Zero angle for Zero length line}
{10/24/99 use hypot from math.pas}
var
  dx, dy: Integer;
begin
  dx := (p2.x - p1.x);
  dy := (p2.y - p1.y);
  with RESULT do
  begin
    di := HYPOT(dx, dy); //10/24/99 	di := Sqrt(dx * dx + dy * dy);
    if abs(di) < 1 then
    begin
      si := 0.0;
      co := 1.0
    end //Zero length line
    else
    begin
      si := dy / di;
      co := dx / di
    end;
  end;
end;

// read time stamp in CPU Cycles for Pentium

function RDTSC: Int64;
asm
        DB      0FH, 31H   //allows out-of-sequence execution, caching
end;

procedure RotateBitmap(const BitmapOriginal: TBitMap; //input bitmap (possibly converted)
  out BitMapRotated: TBitMap; //output bitmap
  const theta: Double; // rotn angle in radians counterclockwise in windows
  const oldAxis: TPOINT; // center of rotation in pixels, rel to bmp origin
  var newAxis: TPOINT); // center of rotated bitmap, relative to bmp origin
{
  (c) har*GIS L.L.C., 1999
   You are free to use this in any way, but please retain this comment block.
   Please email questions to jim@har-gis.com .
  Doc & Updates: http://www.efg2.com/Lab/ImageProcessing/RotateScanline.htm
  and http://www.efg2.com/Lab/Library/Delphi/Graphics/JimHargis_RotateBitMap.zip
}
{Notes...
  Coordinates and rotation are adjusted for 'flipped' Y axis (+Y is down)
  Bitmap origins are (0,0) in top-left.
  BitMapRotated is enlarged to contain the rotated bitmap
  BitMapOriginal may be changed from 1,2,4 bit to pf8Bit, if needed.
//	rotate about center, Oldaxis:=POINT(bmp.width div 2, bmp.height div 2);
//  rotate about origin top-left, Oldaxis:=POINT(0,0);
//  rotate about bottom-center, Oldaxis:=POINT(bmp.width div 2, bmp.height)
  NewAxis: is the new center of rotation for BitMapRotated;
}
{Usage...
  var Inbmp,Newbmp:TbitMap;
  var Center, NewCenter: TPoint;
  begin  //draw at 45 degrees rotated about center
    inbmp:=Tbitmap.Create; Newbmp:=Tbitmap.Create;
    InBMP.LoadFromFile('..\Athena.bmp'); InBMP.Transparent:=True;
    Center:=POINT(inbmp.width div 2, inbmp.height div 2);
    RotateBitMap(inBMP, 45*pi/180, NewBMP, Center, NewCenter);
    //place the bmp rotation axis at 100,100...
    Canvas.Draw(100-NewCenter.x, 100-NewCenter.y, NewBMP);
    inbmp.free; newbmp.free;
  end;
}
{Features/ improvements over original EFG RotateBitMap:
  This is generalized procedure; application independent.
  Does NOT clip corners; Enlarges Output bmp if needed.
  Output keeps same transparency and pallette as set by BitMapOriginal.
 Handles all pixel formats, format converted to least one byte per pixel.
 Axis of rotation specified by caller, but new axis will differ from oldaxis.
 Minor Delphi performance optimizations (about 8 instructions per pixel)
  Skips "null" angles which have no discernable effect.
}
{Restrictions:
 Caller responsible for create/destroy bitmaps.  This improves perfc in loops.
  Caller must provide the following:
    AngleType: the user-specified float format for real angle.
      can be single, double or extended; you won't see any difference.
    function min(const i,j:integer):integer;  // from Math.pas
    procedure sincos(const theta:real; var sine,cosine:Extended);//from Math.pas
 Uses nearest neighbor sampling, no antialiasing: poor quality;
 Not optimized for Pentium;  no MMX. (see Intel Image Processing Library)
}
{Revisions...
12/1/99 original code extracted from Earl F. Glynn
  Copyright (C) 1997-1998 Earl F. Glynn, Overland Park, KS  USA.
  All Rights Reserved.
12/10/99 new code added to make a standalone method. Original in (*comments*)
  Copyright (c) 1999-2000 har*GIS LLC, Englewood CO USA; jim@har-gis.com
    V1.0.0 release
12/15/99 add rotation axis to be specified
  add transparent color, add rotated rectangle output (non-clipped).
12/25/99  recomputed  new rotation axis.
1/6/2000  allow multibyte pixel formats. Translate 1bit and 4bit to 8bit.
10/31/00  add support for pixel formats pfDevice, pfCustom (from Rob Rossmair);
  drop err and debug message I/O;
  deleted the changed code from EFG; use "with" for efficiency;
  add check for nil angle (rotates less than 1 pizel;
  publish as general function, not a method.
11/5/00 allow variable real formats (but only need single precision);
  drop temp bitmap (BM8),  OriginalBitmap is converted if needed.
  fix BUG which ignored OldAxis, always rotated about center, set bad NewAxis
  fix BUG in false optimization for angle zero, which overwrote the input bmp)
11/12/00 fix BUG in calc of NewAxis; simplify math for center rotation.
  V1.0.5 release}
{ToDo.. use pointer arithmetic instead of type subscripting for faster pixels.
  Test pfDevice and pfCustom, test palettes. <no data>. }
var
  cosTheta: Single; {in windows}
  sinTheta: Single;
  i: INTEGER;
  iOriginal: INTEGER;
  //iRotationAxis  :  INTEGER;// Axis of rotation is normally center of image
  iPrime: INTEGER;
  //		iPrimeRotated  :  INTEGER; use width if doubled
  j: INTEGER;
  jOriginal: INTEGER;
  //jRotationAxis  :  INTEGER;
  jPrime: INTEGER;
  //		jPrimeRotated  :  INTEGER; use height if doubled
  NewWidth, NewHeight: INTEGER;
  nBytes, nBits: Integer; //no. bytes per pixelformat
  Oht, Owi, Rht, Rwi: Integer; //Original and Rotated subscripts to bottom/right
  //The variant pixel formats for subscripting       1/6/00
type // from Delphi
  TRGBTripleArray = array[0..32767] of TRGBTriple; //allow integer subscript

  pRGBTripleArray = ^TRGBTripleArray;

  TRGBQuadArray = array[0..32767] of TRGBQuad; //allow integer subscript

  pRGBQuadArray = ^TRGBQuadArray;
var //each of the following Geometry.Points to the same scanlines
  RowRotatedB: pByteArray; //1 byte
  RowRotatedW: pWordArray; //2 bytes
  RowRotatedT: pRGBtripleArray; //3 bytes
  RowRotatedQ: pRGBquadArray; //4 bytes
var //a single pixel for each format 	1/8/00
  TransparentB: Byte;
  TransparentW: Word;
  TransparentT: TRGBTriple;
  TransparentQ: TRGBQuad;
var
  DIB: TDIBSection;
  //Center:  TPOINT; //the middle of the bmp relative to bmp origin.
  SiCoPhi: TSiCoDiType; //sine,cosine, distance
  {=======================================}
  mStream: TMemoryStream;
begin

  with BitMapOriginal do
  begin

    //Decipher the appropriate pixelformat to use Delphi byte subscripting 1/6/00
    //pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit,pfCustom;
    case pixelformat of
      pfDevice:
        begin //handle only pixelbits= 1..8,16,24,32 //10/31/00
          nbits := GetDeviceCaps(Canvas.Handle, BITSPIXEL) + 1;
          nbytes := nbits div 8; //no. bytes for bits per pixel
          if (nbytes > 0) and (nbits mod 8 <> 0) then
            exit; //ignore if invalid
        end;
      pf1bit:
        nBytes := 0; // 1bit, TByteArray      //2 color pallete , re-assign byte value to 8 pixels, for entire scan line
      pf4bit:
        nBytes := 0; // 4bit, PByteArray     // 16 color pallette; build nibble for pixel pallette index; convert to 8 pixels
      pf8bit:
        nBytes := 1; // 8bit, PByteArray     // byte pallette, 253 out of 256 colors; depends on display mode, needs Truecolor ;
      pf15bit:
        nBytes := 2; // 15bit,PWordArrayType // 0rrrrr ggggg bbbbb  0+5+5+5
      pf16bit:
        nBytes := 2; // 16bit,PWordArrayType // rrrrr gggggg bbbbb  5+6+5
      pf24bit:
        nBytes := 3; // 24bit,pRGBtripleArray// bbbbbbbb gggggggg rrrrrrrr  8+8+8
      pf32bit:
        nBytes := 4; // 32bit,pRGBquadArray  // bbbbbbbb gggggggg rrrrrrrr aaaaaaaa 8+8+8+alpha
      // can assign 'Single' reals to this for generating displays/plasma!
      pfCustom:
        begin //handle only pixelbits= 1..8,16,24,32  //10/31/00
          GetObject(Handle, SizeOf(DIB), @DIB);
          nbits := DIB.dsBmih.biSizeImage;
          nbytes := nbits div 8;
          if (nbytes > 0) and (nbits mod 8 <> 0) then
            exit; //ignore if invalid
        end; // pfcustom

    else
      exit; // 10/31/00 ignore invalid formats
    end; // case

    // BitmapRotated.PixelFormat is the same as BitmapOriginal.PixelFormat;
    // IF PixelFormat is less than 8 bit, then BitMapOriginal.PixelFormat = pf8Bit,
    //  because Delphi can't index to bits, just bytes;
    // The next time BitMapOriginal is used it will already be converted.
    //(bmp storage may increase by factor of n*n, where n=8/(no. bits per pixel) )
    if nBytes = 0 then
      PixelFormat := pf8bit; //note that input bmp is changed

    //assign copies all properties, including pallette and transparency
    //fix bug where BitMapOriginal was overwritten bec. pointer was copied
//    BitmapRotated.Assign(BitMapOriginal);
    mStream := TMemoryStream.Create;
    mStream.Position := 0;
    BitMapOriginal.SaveToStream(mStream);
    mStream.Position := 0;
    BitmapRotated.LoadFromStream(mStream);
    BitmapRotated.Width := BitMapOriginal.Width;
    BitmapRotated.Height := BitmapOriginal.Height;
    mStream.Free;
//<-

    //COUNTERCLOCKWISE rotation angle in radians. 12/10/99
    sinTheta := SIN(theta);
    cosTheta := COS(theta);
    //SINCOS(theta, sinTheta, cosTheta) ; math.pas requires extended reals.

    //calculate the enclosing rectangle  12/15/00
    NewWidth := ABS(ROUND(Height * sinTheta)) + ABS(ROUND(Width * cosTheta));
    NewHeight := ABS(ROUND(Width * sinTheta)) + ABS(ROUND(Height * cosTheta));

    //diff size bitmaps have diff resolution of angle, ie r*sin(theta)<1 pixel
    //use the small angle approx: sin(theta) ~~ theta   //11/7/00
    if (ABS(theta) * MAX(width, height)) > 1 then
    begin //non-zero rotation

      //set output bitmap formats; we do not assume a fixed format or size 1/6/00
      BitmapRotated.Width := NewWidth; //resize it for rotation
      BitmapRotated.Height := NewHeight;
      //center of rotation is center of bitmap
        //iRotationAxis := width div 2;
        //jRotationAxis := height div 2;

      //local constants for loop, each was hit at least width*height times   1/8/00
      Rwi := NewWidth - 1; //right column index
      Rht := NewHeight - 1; //bottom row index
      Owi := Width - 1; //transp color column index
      Oht := Height - 1; //transp color row  index

      //Transparent pixel color used for out of range pixels 1/8/00
      //how to translate a Bitmap.TransparentColor=Canvas.Pixels[0, Height - 1];
      // from Tcolor into pixelformat..
      case nBytes of
        0, 1:
          TransparentB := PByteArray(Scanline[Oht])[0];
        2:
          TransparentW := PWordArray(Scanline[Oht])[0];
        3:
          TransparentT := pRGBtripleArray(Scanline[Oht])[0];
        4:
          TransparentQ := pRGBquadArray(Scanline[Oht])[0];
      end; //case *)

      // Step through each row of rotated image.
      for j := Rht downto 0 do //1/8/00
      begin //for j

        case nBytes of //1/6/00
          0, 1:
            RowRotatedB := BitmapRotated.Scanline[j];
          2:
            RowRotatedW := BitmapRotated.Scanline[j];
          3:
            RowRotatedT := BitmapRotated.Scanline[j];
          4:
            RowRotatedQ := BitmapRotated.Scanline[j];
        end; //case

        // offset origin by the growth factor     //12/25/99
        //	jPrime := 2*(j - (NewHeight - Height) div 2 - jRotationAxis) + 1 ;
        jPrime := 2 * j - NewHeight + 1;

        // Step through each column of rotated image
        for i := Rwi downto 0 do //1/8/00
        begin //for i

          // offset origin by the growth factor  //12/25/99
          //iPrime := 2*(i - (NewWidth - Width) div 2 - iRotationAxis) + 1;
          iPrime := 2 * i - NewWidth + 1;

          // Rotate (iPrime, jPrime) to location of desired pixel	(iPrimeRotated,jPrimeRotated)
          // Transform back to pixel Coordinates of image, including translation
          // of origin from axis of rotation to origin of image.
       //iOriginal := (ROUND(iPrime*CosTheta - jPrime*sinTheta) - 1) DIV 2 + iRotationAxis;
       //jOriginal := (ROUND(iPrime*sinTheta + jPrime*cosTheta) - 1) DIV 2 + jRotationAxis;
          iOriginal := (ROUND(iPrime * CosTheta - jPrime * sinTheta) - 1 + width) div 2;
          jOriginal := (ROUND(iPrime * sinTheta + jPrime * cosTheta) - 1 + height) div 2;

          // Make sure (iOriginal, jOriginal) is in BitmapOriginal.  If not,
          // assign background color to corner Geometry.Points.
          if (iOriginal >= 0) and (iOriginal <= Owi) and (jOriginal >= 0) and (jOriginal
            <= Oht) {//1/8/00} then
          begin //inside
            // Assign pixel from rotated space to current pixel in BitmapRotated
            //(nearest neighbor interpolation)
            case nBytes of //get pixel bytes according to pixel format   1/6/00
              0, 1:
                RowRotatedB[i] := pByteArray(scanline[joriginal])[iOriginal];
              2:
                RowRotatedW[i] := pWordArray(Scanline[jOriginal])[iOriginal];
              3:
                RowRotatedT[i] := pRGBtripleArray(Scanline[jOriginal])[iOriginal];
              4:
                RowRotatedQ[i] := pRGBquadArray(Scanline[jOriginal])[iOriginal];
            end; //case
          end //inside
          else
          begin //outside

            //12/10/99 set background corner color to transparent (lower left corner)
            //	RowRotated[i]:=tpixelformat(BitMapOriginal.TRANSPARENTCOLOR) ; wont work
            case nBytes of
              0, 1:
                RowRotatedB[i] := TransparentB;
              2:
                RowRotatedW[i] := TransparentW;
              3:
                RowRotatedT[i] := TransparentT;
              4:
                RowRotatedQ[i] := TransparentQ;
            end; //case
          end //if inside

        end //for i
      end; //for j
    end; //non-zero rotation

    //offset to the apparent center of rotation   11/12/00 12/25/99
    //rotate/translate the old bitmap origin to the new bitmap origin,FIXED 11/12/00
    sicoPhi := sicodiPoint(POINT(width div 2, height div 2), oldaxis);
    //sine/cosine/dist of axis point from center point
    with sicoPhi do
    begin
      //NewAxis := NewCenter + dist* <sin(theta+phi),cos(theta+phi)>
      NewAxis.x := newWidth div 2 + ROUND(di * (CosTheta * co - SinTheta * si));
      NewAxis.y := newHeight div 2 - ROUND(di * (SinTheta * co + CosTheta * si));
        //flip yaxis
    end;

  end; //with

end; {RotateImage}

{ Procedure for clipping a line against a polygon
  Polygon is the source polygons
  Polyline is the polyline to clip against Polygon
  Result is the resulting polylines
}

type
  Tedge = record
    xA, yA, xB, yB: Double;
    nx, ny: Double;
  end;

function ClipParam(const denom, num: Double; var tE, tL: Double): Boolean;
var
  t: Double;
begin
  Result := False;
  if denom > 0 then
  begin
    t := num / denom;
    if t > tL then
      Exit;
    tE := t;
  end
  else if denom < 0 then
  begin
    t := num / denom;
    if t < tE then
      Exit;
    tL := t;
  end
  else if num > 0 then
    Exit;
  Result := True;
end;

procedure CyrusBeckLineClip(Polyline, Polygon, Result: IlicgVector);
var
  num, denom, dx, dy, tE, tL: Double;
  e: TEdge;
  I, J, N: Integer;
  p: TlicgCoor;
  x0, y0, x1, y1: Double;
  LastPt: TlicgCoor;
  Accepted: Boolean;
begin
  if (Polygon.Rotation <> clockwise) then
    Polygon.RevertRotation(-1);

  LastPt := AsCoor(_MAXCOOR, _MAXCOOR);
  N := 0;
  for J := 0 to Polyline.Count - 2 do
  begin
    x0 := Polyline[J].x;
    y0 := Polyline[J].y;
    x1 := Polyline[J + 1].x;
    y1 := Polyline[J + 1].y;
    dx := x1 - x0;
    dy := y1 - y0;
    tE := 0.0;
    tL := 1.0;
    Accepted := True;
    for I := 0 to Polygon.Count - 2 do
    begin
      e.xA := Polygon[I].x;
      e.yA := Polygon[I].y;
      e.xB := Polygon[I + 1].x;
      e.yB := Polygon[I + 1].y;
      { compute normal }
      p := TransformPoint2d(Polygon[I + 1], Rotate2d(System.Pi / 2, Polygon[I]));
      e.nx := p.x;
      e.ny := p.y;
      // denom is -Ni.D; num is Ni.(A-P)
      denom := -e.nx * dx - e.ny * dy;
      num := e.nx * (e.xA - x0) + e.ny * (e.yA - y0);
      if not ClipParam(denom, num, tE, tL) then
      begin
        Accepted := False;
        Break;
      end;
    end;
    if not Accepted then
      Continue;

    // compute clipped end Geometry.Points
    if tL < 1 then
    begin
      x1 := x0 + tL * dx;
      y1 := y0 + tL * dy;
    end;
    if tE > 0 then
    begin
      x0 := x0 + tE * dx;
      y0 := y0 + tE * dy;
    end;
    Result.Add(x0, y0);
    Result.Add(x1, y1);
    Result.AddPart(N);
    Inc(N, 2);
  end;
  if Result.PartCount < 2 then
    Result.ClearParts;
end;

{ ******************** bitmaps sections *********************** }

{$R-}

procedure PrintBitmapEx(Canvas: TCanvas; const DestinationRect: TRect; ABitmap:
  TBitmap; const SourceRect: TRect);
var
  Info: PBitmapInfo;
  Image: Pointer;
  Tc: Integer;
  InfoSize, ImageSize: DWORD;
  SourceHeight, SourceWidth: Integer;
begin
  SourceHeight := Abs(SourceRect.Bottom - SourceRect.Top);
  SourceWidth := Abs(SourceRect.Right - SourceRect.Left);
  GetDIBSizes(ABitmap.Handle, InfoSize, ImageSize);
  Info := GetMemEx(InfoSize);
  Image := GetMemEx(ImageSize);
  try
    GetDIB(ABitmap.Handle, ABitmap.Palette, Info^, Image^);
    Tc := SourceRect.Top;
    if Info^.bmiHeader.biHeight > 0 then
      Tc := Info^.bmiHeader.biHeight - SourceHeight - SourceRect.Top;
    SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
    with DestinationRect do
      StretchDIBits(Canvas.Handle, Left, Top, (Right - Left), (Bottom - Top),
        SourceRect.Left, Tc, SourceWidth, SourceHeight, Image, Info^,
        DIB_RGB_COLORS, SRCCOPY);
  finally
    FreeMemEx(Info);
    FreeMemEx(Image);
  end;
end;

procedure Fill8X8Bitmap(ACanvas: TCanvas; DestRect: TRect; Bitmap: TBitmap;
  ForeColor, BackColor: TColor);
var
  Bits: Pointer;
  p1: Integer;
  HeaderSize, BitsSize: DWORD;
  OldHandle, ABrush: HBrush;
  compbitmap: HBitmap;
  BitmapInfo: PBitmapInfo;
begin

  GetDIBSizes(Bitmap.Handle, HeaderSize, BitsSize);
  BitmapInfo := GetMemEx(HeaderSize);
  Bits := GetMemEx(BitsSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapInfo^, Bits^);
    with BitmapInfo^.bmiHeader do
    begin
      biClrUsed := 2;
      biClrImportant := 0;
    end;
    p1 := 0;
    with BitmapInfo^.bmiColors[p1] do
    begin
      rgbRed := GetRValue(ForeColor);
      rgbGreen := GetGValue(ForeColor);
      rgbBlue := GetBValue(ForeColor);
    end;
    p1 := 1;
    with BitmapInfo^.bmiColors[p1] do
    begin
      rgbRed := GetRValue(BackColor);
      rgbGreen := GetGValue(BackColor);
      rgbBlue := GetBValue(BackColor);
    end;

    compbitmap := CreateDIBitmap(ACanvas.Handle, BitmapInfo^.bmiHeader, CBM_INIT,
      Bits, BitmapInfo^, DIB_RGB_COLORS);

    ABrush := CreatePatternBrush(compbitmap);

    DeleteObject(compbitmap);

    OldHandle := SelectObject(ACanvas.Handle, ABrush);

    with DestRect do
      PatBlt(ACanvas.Handle, left, top, (right - left), (bottom - top), PATCOPY);

    SelectObject(ACanvas.handle, OldHandle);

    DeleteObject(ABrush);

  finally
    FreeMemEx(BitmapInfo);
    FreeMemEx(Bits);
  end;
end;

procedure PrinterFill8X8Bitmap(ACanvas: TCanvas; DestRect: TRect; Bitmap:
  TBitmap; ForeColor, BackColor: TColor; Factor: Double);
var
  Header, Bits: Pointer;
  p1: Integer;
  HeaderSize, BitsSize: DWORD;
  ScaledWidth, ScaledHeight, DeltaX, DeltaY: integer;
  ScaledRect: TRect;
  WorkBmp: TBitmap;
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, BitsSize);
  Header := GetMemEx(HeaderSize);
  Bits := GetMemEx(BitsSize);

  WorkBmp := TBitmap.Create;
  try

    with WorkBmp, DestRect do
    begin
      Width := Right - Left;
      Height := Bottom - Top;
    end;

    GetDIB(Bitmap.Handle, Bitmap.Palette, Header^, Bits^);
    ScaledWidth := trunc(PBitmapInfo(Header)^.bmiHeader.biWidth * factor);
    ScaledHeight := trunc(PBitmapInfo(Header)^.bmiHeader.biHeight * factor);

    { modify the bitmap }
    if (Bitmap.WIdth = 8) and (Bitmap.Height = 8) and Bitmap.Monochrome then
    begin
      with PBitmapInfo(Header)^.bmiHeader do
      begin
        biClrUsed := 2;
        biClrImportant := 0;
      end;
      p1 := 0;
      with PBitmapInfo(Header)^.bmiColors[p1] do
      begin
        rgbRed := GetRValue(ForeColor);
        rgbGreen := GetGValue(ForeColor);
        rgbBlue := GetBValue(ForeColor);
      end;
      p1 := 1;
      with PBitmapInfo(Header)^.bmiColors[p1] do
      begin
        rgbRed := GetRValue(BackColor);
        rgbGreen := GetGValue(BackColor);
        rgbBlue := GetBValue(BackColor);
      end;
    end;

    DeltaX := 0;
    while DeltaX < WorkBmp.Width do
    begin
      DeltaY := 0;
      while DeltaY < WorkBmp.Height do
      begin
        ScaledRect := Rect(DeltaX, DeltaY, DeltaX + ScaledWidth, DeltaY + ScaledHeight);
        with ScaledRect do
          StretchDIBits(WorkBmp.Canvas.Handle, Left, Top, Right - Left, Bottom -
            Top, 0, 0, Bitmap.Width, Bitmap.Height, Bits, Windows.TBitmapInfo(Header
            ^), DIB_RGB_COLORS, SRCCOPY);
        Inc(DeltaY, ScaledHeight);
      end;
      Inc(DeltaX, ScaledWidth);
    end;
    PrintBitmapEx(ACanvas, DestRect, WorkBmp, Rect(0, 0, WorkBmp.Width, WorkBmp.Height));
  finally
    WorkBmp.free;
    FreeMemEx(Header);
    FreeMemEx(Bits);
  end;
end;

procedure _PolygonScreenFill8X8Bitmap(Canvas: TCanvas; Grapher: TlicgBaseGrapher;
  var Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  Bitmap: TBitmap; ForeColor, BackColor: TColor; ClipRgn: HRgn);
var
  Bits: Pointer;
  Index: Integer;
  HeaderSize, BitsSize: DWORD;
  OldHandle, TmpBrush: HBrush;
  CompBitmap: HBitmap;
  BitmapInfo: PBitmapInfo;
  SavedBitMap: TBitmap;
  I, N, K, Idx1, Idx2, BW, BH: Integer;
  Xmin, Ymin, Xmax, Ymax: Integer;
  PolyRgn: HRgn;
  dBoxColor: TColor;
begin
  if PartCount = 0 then
    Exit;
  if not (Bitmap.Monochrome and (Bitmap.Width = 8) and (Bitmap.Height = 8)) then
  begin
    PolygonPrinterFill8X8Bitmap(Canvas, Grapher, Vertices, Parts, PartCount,
      Bitmap, ForeColor, BackColor, 1, False, ClipRgn);
    Exit;
  end;

  GetDIBSizes(Bitmap.Handle, HeaderSize, BitsSize);
  BitmapInfo := GetMemEx(HeaderSize);
  Bits := GetMemEx(BitsSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapInfo^, Bits^);
    with BitmapInfo^.bmiHeader do
    begin
      biClrUsed := 2;
      biClrImportant := 0;
    end;
    with BitmapInfo^.bmiColors[0] do
    begin
      rgbRed := GetRValue(ForeColor);
      rgbGreen := GetGValue(ForeColor);
      rgbBlue := GetBValue(ForeColor);
    end;
    Index := 1;
    with BitmapInfo^.bmiColors[Index] do
    begin
      rgbRed := GetRValue(BackColor);
      rgbGreen := GetGValue(BackColor);
      rgbBlue := GetBValue(BackColor);
    end;

    CompBitmap := CreateDIBitmap(Canvas.Handle, BitmapInfo^.bmiHeader, CBM_INIT,
      Bits, BitmapInfo^, DIB_RGB_COLORS);

    TmpBrush := CreatePatternBrush(CompBitmap);

    DeleteObject(CompBitmap);

    { Create Transparent Bitmap }

    Xmin := 0; // to make happy the compiler
    Ymin := 0; // to make happy the compiler
    BW := 0; // to make happy the compiler
    BH := 0; // to make happy the compiler

    SavedBitMap := nil;
    if BackColor = clNone then
    begin
      Xmin := MaxInt;
      Ymin := MaxInt;
      Xmax := 0;
      Ymax := 0;
      N := 0;
      for I := 0 to PartCount - 1 do
        Inc(N, Parts[I]);
      for I := Low(vertices) to N - 1 do
      begin
        Xmin := Min(Vertices[I].X, Xmin);
        Ymin := Min(Vertices[I].Y, Ymin);
        Xmax := Max(Vertices[I].X, Xmax);
        Ymax := Max(Vertices[I].Y, Ymax);
      end;
      BW := Xmax - Xmin;
      BH := Ymax - Ymin;
      if (BW > 0) and (BH > 0) then
      begin
        SavedBitMap := TBitmap.Create;
        SavedBitMap.PixelFormat := pf24bit;
        SavedBitMap.Width := BW;
        SavedBitMap.Height := BH;
        OldHandle := SelectObject(SavedBitMap.Canvas.Handle, TmpBrush);
        Bitblt(SavedBitMap.Canvas.Handle, 0, 0, BW, BH, Bitmap.Canvas.handle, 0,
          0, PATCOPY);
        SelectObject(SavedBitMap.Canvas.handle, OldHandle);
      end
      else
        BackColor := clBlack;
    end;

    OldHandle := SelectObject(Canvas.Handle, TmpBrush);

    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Idx1 := 0;
      Idx2 := 0;
    end
    else
    begin
      Idx1 := 0;
      Idx2 := PartCount - 1;
    end;

    K := 0;

    for I := Idx1 to Idx2 do
    begin

      PolyRgn := 0; // to make happy the compiler
      if BackColor = clNone then
      begin
        if PartCount = 1 then
          PolyRgn := CreatePolygonRgn(Vertices[0], Parts[I], WINDING)
        else
        begin
          if Win32Platform = VER_PLATFORM_WIN32_NT then
            PolyRgn := CreatePolyPolygonRgn(Vertices, Parts, PartCount, Alternate)
          else
          begin
            PolyRgn := CreatePolygonRgn(Vertices[K], Parts[I], WINDING);
            Inc(K, Parts[I]);
          end;
        end;
        if PolyRgn = 0 then
          Exit;

        SelectClipRgn(Canvas.Handle, PolyRgn);
        (*
        if ( Grapher.DrawBoxColor = clBlack) then
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0, 0, SRCINVERT{SRCAND})
        else
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0, 0, SRCAND);
          *)
        if Assigned(Grapher) and (Grapher.Drawbox <> nil) and (TlicgBaseDrawBox(Grapher.Drawbox).Color
          = clBlack) then
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
            0, SRCINVERT)
        else if Assigned(Grapher) and (Grapher.Drawbox <> nil) and ((TlicgBaseDrawBox
          (Grapher.Drawbox).Color = clWindow) or (TlicgBaseDrawBox(Grapher.Drawbox).Color
          = clwhite)) then
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
            0, SRCAND)
        else if Assigned(Grapher) and (Grapher.Drawbox <> nil) then
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
            0, SRCPAINT)
        else
          BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
            0, SRCCOPY);

      end
      else
      begin
        if PartCount = 1 then
          Polygon(Canvas.Handle, Vertices, Parts[0])
        else
        begin
          if Win32Platform = VER_PLATFORM_WIN32_NT then
            PolyPolygon(Canvas.Handle, Vertices, Parts, PartCount)
          else
          begin
            Polygon(Canvas.Handle, Vertices[K], Parts[I]);
            Inc(K, Parts[I]);
          end;
        end;
      end;

      SelectObject(Canvas.handle, OldHandle);

      DeleteObject(TmpBrush);

      if BackColor = clNone then
      begin
        SelectClipRgn(Canvas.Handle, ClipRgn);
        DeleteObject(PolyRgn);
      end;
    end;
    if BackColor = clNone then
      SavedBitMap.Free;
  finally
    FreeMemEx(BitmapInfo);
    FreeMemEx(Bits);
  end;
end;

procedure PolygonScreenFill8X8Bitmap(Canvas: TCanvas; Grapher: TlicgBaseGrapher;
  var Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  Bitmap: TBitmap; ForeColor, BackColor: TColor; ClipRgn: HRgn; clNoneBitmapC:
  TclNoneBitmapClass);
var
  Index: Integer;
  OldHandle, TmpBrush: HBrush;
  CompBitmap: HBitmap;
  SavedBitMap: TBitmap;
  I, N, K, Idx1, Idx2, BW, BH: Integer;
  Xmin, Ymin, Xmax, Ymax: Integer;
  PolyRgn: HRgn;
  dBoxColor: TColor;
begin

  if clNoneBitmapC = nil then
  begin
    _PolygonScreenFill8X8Bitmap(Canvas, Grapher, Vertices, Parts, PartCount,
      Bitmap, ForeColor, BackColor, ClipRgn);
    exit;

  end;

  if PartCount = 0 then
    Exit;
  if not (Bitmap.Monochrome and (Bitmap.Width = 8) and (Bitmap.Height = 8)) then
  begin
    PolygonPrinterFill8X8Bitmap(Canvas, Grapher, Vertices, Parts, PartCount,
      Bitmap, ForeColor, BackColor, 1, False, ClipRgn);
    Exit;
  end;

  with clNoneBitmapC.BitmapInfo^.bmiColors[0] do
  begin
    rgbRed := GetRValue(ForeColor);
    rgbGreen := GetGValue(ForeColor);
    rgbBlue := GetBValue(ForeColor);
  end;

  CompBitmap := CreateDIBitmap(Canvas.Handle, clNoneBitmapC.BitmapInfo^.bmiHeader,
    CBM_INIT, clNoneBitmapC.Bits, clNoneBitmapC.BitmapInfo^, DIB_RGB_COLORS);

  TmpBrush := CreatePatternBrush(CompBitmap);

  DeleteObject(CompBitmap);



  { Create Transparent Bitmap }

  Xmin := 0; // to make happy the compiler
  Ymin := 0; // to make happy the compiler
  BW := 0; // to make happy the compiler
  BH := 0; // to make happy the compiler

  SavedBitMap := nil;
  if BackColor = clNone then
  begin
    Xmin := MaxInt;
    Ymin := MaxInt;
    Xmax := 0;
    Ymax := 0;
    N := 0;
    for I := 0 to PartCount - 1 do
      Inc(N, Parts[I]);
    for I := Low(vertices) to N - 1 do
    begin
      Xmin := Min(Vertices[I].X, Xmin);
      Ymin := Min(Vertices[I].Y, Ymin);
      Xmax := Max(Vertices[I].X, Xmax);
      Ymax := Max(Vertices[I].Y, Ymax);
    end;
    BW := Xmax - Xmin;
    BH := Ymax - Ymin;
    if (BW > 0) and (BH > 0) then
    begin
      SavedBitMap := TBitmap.Create;
      SavedBitMap.PixelFormat := pf24bit;
      SavedBitMap.Width := BW;
      SavedBitMap.Height := BH;
      OldHandle := SelectObject(SavedBitMap.Canvas.Handle, TmpBrush);
      Bitblt(SavedBitMap.Canvas.Handle, 0, 0, BW, BH, Bitmap.Canvas.handle, 0, 0,
        PATCOPY);
      SelectObject(SavedBitMap.Canvas.handle, OldHandle);
    end
    else
      BackColor := clBlack;
  end;

  OldHandle := SelectObject(Canvas.Handle, TmpBrush);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Idx1 := 0;
    Idx2 := 0;
  end
  else
  begin
    Idx1 := 0;
    Idx2 := PartCount - 1;
  end;

  K := 0;

  for I := Idx1 to Idx2 do
  begin

    PolyRgn := 0; // to make happy the compiler
    if BackColor = clNone then
    begin
      if PartCount = 1 then
        PolyRgn := CreatePolygonRgn(Vertices[0], Parts[I], WINDING)
      else
      begin
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          PolyRgn := CreatePolyPolygonRgn(Vertices, Parts, PartCount, Alternate)
        else
        begin
          PolyRgn := CreatePolygonRgn(Vertices[K], Parts[I], WINDING);
          Inc(K, Parts[I]);
        end;
      end;
      if PolyRgn = 0 then
        Exit;

      SelectClipRgn(Canvas.Handle, PolyRgn);

      if (Grapher.Drawbox <> nil) and (TlicgBaseDrawBox(Grapher.Drawbox).Color =
        clBlack) then
        BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
          0, SRCINVERT)
      else if (Grapher.Drawbox <> nil) and ((TlicgBaseDrawBox(Grapher.Drawbox).Color
        = clWindow) or (TlicgBaseDrawBox(Grapher.Drawbox).Color = clwhite)) then
        BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0, 0, SRCAND)
      else if (Grapher.Drawbox <> nil) then
        BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
          0, SRCPAINT)
      else
        BitBlt(Canvas.handle, Xmin, Ymin, BW, BH, SavedBitmap.Canvas.Handle, 0,
          0, SRCCOPY);

    end
    else
    begin
      if PartCount = 1 then
        Polygon(Canvas.Handle, Vertices, Parts[0])
      else
      begin
        if Win32Platform = VER_PLATFORM_WIN32_NT then
          PolyPolygon(Canvas.Handle, Vertices, Parts, PartCount)
        else
        begin
          Polygon(Canvas.Handle, Vertices[K], Parts[I]);
          Inc(K, Parts[I]);
        end;
      end;
    end;

    SelectObject(Canvas.handle, OldHandle);

    DeleteObject(TmpBrush);

    if BackColor = clNone then
    begin
      SelectClipRgn(Canvas.Handle, ClipRgn);
      DeleteObject(PolyRgn);
    end;
  end;
  if BackColor = clNone then
    SavedBitMap.Free;

end;

// Print polygon filled with 8x8 bitmap pattern

procedure PolygonPrinterFill8X8Bitmap(Canvas: TCanvas; Grapher: TlicgBaseGrapher;
  var Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  Bitmap: TBitmap; ForeColor, BackColor: TColor; Factor: Double;
  PlotterOptimized: Boolean; ClipRgn: HRgn);
var
  Header, Bits: Pointer;
  HeaderSize, BitsSize: DWORD;
  ScaledRect: TRect;
  DestRect: TRect;
  PolyRgn: HRgn;
  I, K, Idx1, Idx2, p1, N: Integer;
  ScaledWidth: Integer;
  ScaledHeight: Integer;
  DeltaX: Integer;
  DeltaY: integer;
  TmpWidth: Integer;
  TmpHeight: Integer;
  cnt: Integer;
  Xmin: Integer;
  Xmax: Integer;
  Ymin: Integer;
  Ymax: Integer;
  XRop: DWORD;

  { some plotters will gets full printing patterns as bitmaps }

  procedure DrawAsBitmap;
  begin
    GetDIBSizes(Bitmap.Handle, HeaderSize, BitsSize);
    Header := GetMemEx(HeaderSize);
    Bits := GetMemEx(BitsSize);

    try
      with DestRect do
      begin
        TmpWidth := Right - Left;
        TmpHeight := Bottom - Top;
      end;

      GetDIB(Bitmap.Handle, Bitmap.Palette, Header^, Bits^);
      if Trunc(Factor) <> Factor then
      begin
        ScaledWidth := Round(Factor + 0.5) * PBitmapInfo(Header)^.bmiHeader.biWidth;
        ScaledHeight := Round(Factor + 0.5) * PBitmapInfo(Header)^.bmiHeader.biHeight;
      end
      else
      begin
        ScaledWidth := Round(Factor * PBitmapInfo(Header)^.bmiHeader.biWidth);
        ScaledHeight := Round(Factor * PBitmapInfo(Header)^.bmiHeader.biHeight);
      end;

      if (Bitmap.Width = 8) and (Bitmap.Height = 8) and Bitmap.Monochrome then
      begin
        with PBitmapInfo(Header)^.bmiHeader do
        begin
          biClrUsed := 2;
          biClrImportant := 0;
        end;
        p1 := 0;
        with PBitmapInfo(Header)^.bmiColors[p1] do
        begin
          rgbRed := GetRValue(ForeColor);
          rgbGreen := GetGValue(ForeColor);
          rgbBlue := GetBValue(ForeColor);
        end;
        p1 := 1;
        with PBitmapInfo(Header)^.bmiColors[p1] do
        begin
          rgbRed := GetRValue(BackColor);
          rgbGreen := GetGValue(BackColor);
          rgbBlue := GetBValue(BackColor);
        end;
      end;

      if BackColor = clNone then
        XRop := SRCAND
      else
        XRop := SRCCOPY;

      DeltaX := 0;
      while DeltaX < TmpWidth do
      begin
        DeltaY := 0;
        while DeltaY < TmpHeight do
        begin
          ScaledRect := Rect(DeltaX, DeltaY, DeltaX + ScaledWidth, DeltaY + ScaledHeight);
          OffsetRect(ScaledRect, xmin, ymin);
          with ScaledRect do
            StretchDIBits(Canvas.Handle, Left, Top, Right - Left, Bottom - Top,
              0, 0, Bitmap.Width, Bitmap.Height, Bits, Windows.TBitmapInfo(Header
              ^), DIB_RGB_COLORS, XRop);
          Inc(DeltaY, ScaledHeight);
        end;
        Inc(DeltaX, ScaledWidth);
      end;
    finally
      FreeMemEx(Header);
      FreeMemEx(Bits);
    end;
  end;

  {This method will print 8x8 bitmap patterns as small rectangles.
   That way the plotter will not crash with so much bitmaps}
  procedure DrawAsVectors;
  var
    cntleft, cnttop, StartLeft, StartTop, ifactor, bw, bh, tmpleft, tmptop: Integer;
    WorkArray: array[0..1000, 0..1000] of Boolean;
    TmpRect: TRect;
  begin
    for cntleft := 0 to Bitmap.Width - 1 do
      for cnttop := 0 to Bitmap.Height - 1 do
        WorkArray[cntleft, cnttop] := Bitmap.Canvas.Pixels[cnttop, cntleft] = clBlack;
    ifactor := Round(Factor + 0.5);
    bw := Bitmap.Width * ifactor;
    bh := Bitmap.Height * ifactor;
    if Grapher <> nil then
      Grapher.SaveCanvas(Canvas);
    (* Paint the background color*)
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Mode := pmCopy;
    if BackColor <> clWhite then
    begin
      Canvas.Brush.Color := BackColor;
      Canvas.Pen.Color := BackColor;
      with DestRect do
        Canvas.Rectangle(Left, Top, Right, Bottom);
    end;
    (* now the forecolor *)
    Canvas.Brush.Color := ForeColor;
    Canvas.Pen.Color := ForeColor;

    StartTop := DestRect.Top;
    while StartTop <= DestRect.Bottom do
    begin
      StartLeft := DestRect.Left;
      while StartLeft <= DestRect.Right do
      begin
        cnttop := 0;
        while cnttop <= 7 do
        begin
          cntleft := 0;
          while cntleft <= 7 do
          begin
            if WorkArray[cntleft, cnttop] then
            begin
              tmpleft := StartLeft + cntleft * ifactor;
              tmptop := StartTop + cnttop * ifactor;
              TmpRect := Rect(tmpleft, tmptop, tmpleft + Pred(ifactor), tmptop +
                Pred(ifactor));
              with TmpRect do
                Canvas.Rectangle(Left, Top, Right, Bottom);
            end;
            Inc(cntleft);
          end;
          Inc(cnttop);
        end;
        Inc(StartLeft, bw);
      end;
      Inc(StartTop, bh);
    end;
    if Grapher <> nil then
      Grapher.RestoreCanvas(Canvas);
  end;

begin
  if PartCount = 0 then
    Exit;

  Xmin := MaxInt;
  Ymin := MaxInt;
  Xmax := 0;
  Ymax := 0;
  N := 0;
  for cnt := 0 to PartCount - 1 do
    Inc(N, Parts[cnt]);
  for cnt := Low(Vertices) to N - 1 do
  begin
    Xmin := Min(Vertices[cnt].X, Xmin);
    Ymin := Min(Vertices[cnt].Y, Ymin);
    Xmax := Max(Vertices[cnt].X, Xmax);
    Ymax := Max(Vertices[cnt].Y, Ymax);
  end;
  DestRect := Rect(Xmin, Ymin, Xmax, Ymax);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Idx1 := 0;
    Idx2 := 0;
  end
  else
  begin
    Idx1 := 0;
    Idx2 := PartCount - 1;
  end;

  K := 0;

  for I := Idx1 to Idx2 do
  begin
    if PartCount = 1 then
      PolyRgn := CreatePolygonRgn(Vertices[0], Parts[0], WINDING)
    else
    begin
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        PolyRgn := CreatePolyPolygonRgn(Vertices, Parts, PartCount, ALTERNATE)
      else
      begin
        PolyRgn := CreatePolygonRgn(Vertices[K], Parts[I], WINDING);
        Inc(K, Parts[I]);
      end;
    end;
    if PolyRgn = 0 then
      Exit;

    SelectClipRgn(Canvas.Handle, PolyRgn);

    try
      if (Bitmap.Monochrome) and (Bitmap.Width = 8) and (Bitmap.Height = 8) then
      begin
        if PlotterOptimized then
          { draw as vectors }
          DrawAsVectors
        else
          { draw as bitmap }
          DrawAsBitmap;
      end
      else
        DrawAsBitmap;
    finally
      SelectClipRgn(Canvas.Handle, ClipRgn);
      DeleteObject(PolyRgn);
    end;
  end;
end;

procedure DoGradientFill(Canvas: TCanvas; Grapher: TlicgBaseGrapher; var
  Vertices: array of TPoint; var Parts: array of Integer; PartCount: Integer;
  StartColor, EndColor: TColor; GradientDirection: TlicgFillDirection; ClipRgn: HRgn);
var
  N, cnt: Integer;
  Xmin: Integer;
  Xmax: Integer;
  Ymin: Integer;
  Ymax: Integer;
  DestRect: TRect;
  I, K, Idx1, Idx2: Integer;
  PolyRgn: HRgn;
  Gradient: TlicgBaseGradient;
begin
  if PartCount = 0 then
    Exit;

  Xmin := MaxInt;
  Ymin := MaxInt;
  Xmax := 0;
  Ymax := 0;
  N := 0;
  for cnt := 0 to PartCount - 1 do
    Inc(N, Parts[cnt]);
  for cnt := Low(Vertices) to N - 1 do
  begin
    Xmin := Min(Vertices[cnt].X, Xmin);
    Ymin := Min(Vertices[cnt].Y, Ymin);
    Xmax := Max(Vertices[cnt].X, Xmax);
    Ymax := Max(Vertices[cnt].Y, Ymax);
  end;
  DestRect := Rect(Xmin, Ymin, Xmax, Ymax);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Idx1 := 0;
    Idx2 := 0;
  end
  else
  begin
    Idx1 := 0;
    Idx2 := PartCount - 1;
  end;

  K := 0;

  for I := Idx1 to Idx2 do
  begin
    if PartCount = 1 then
      PolyRgn := CreatePolygonRgn(Vertices[0], Parts[0], WINDING)
    else
    begin
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        PolyRgn := CreatePolyPolygonRgn(Vertices, Parts, PartCount, ALTERNATE)
      else
      begin
        PolyRgn := CreatePolygonRgn(Vertices[K], Parts[I], WINDING);
        Inc(K, Parts[I]);
      end;
    end;
    if PolyRgn = 0 then
      Exit;

    SelectClipRgn(Canvas.Handle, PolyRgn);

    Gradient := Lider.CG.Com.LicadInt.CreateGradient;
    try
      Gradient.StartColor := EndColor;
      Gradient.EndColor := StartColor;
      Gradient.Direction := GradientDirection;
      Gradient.Draw(Canvas, DestRect);
    finally
      Gradient.Free;
      SelectClipRgn(Canvas.Handle, ClipRgn);
      DeleteObject(PolyRgn);
    end;
  end;
end;

procedure ColorBlend(const ACanvas: HDC; const ARect: TRect; const ABlendColor:
  TColor; const ABlendValue: Integer);
var
  DC: HDC;
  Brush: HBRUSH;
  Bitmap: HBITMAP;
  BlendFunction: TBlendFunction;
begin
  DC := CreateCompatibleDC(ACanvas);
  Bitmap := CreateCompatibleBitmap(ACanvas, ARect.Right - ARect.Left, ARect.Bottom
    - ARect.Top);
  Brush := CreateSolidBrush(ColorToRGB(ABlendColor));
  try
    SelectObject(DC, Bitmap);
    Windows.FillRect(DC, Rect(0, 0, ARect.Right - ARect.Left, ARect.Bottom -
      ARect.Top), Brush);
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.AlphaFormat := 0;
    BlendFunction.SourceConstantAlpha := ABlendValue;
    Windows.AlphaBlend(ACanvas, ARect.Left, ARect.Top, ARect.Right - ARect.Left,
      ARect.Bottom - ARect.Top, DC, 0, 0, ARect.Right - ARect.Left, ARect.Bottom
      - ARect.Top, BlendFunction);
  finally
    DeleteObject(Brush);
    DeleteObject(Bitmap);
    DeleteDC(DC);
  end;
end;

procedure DoBlendFill(Canvas: TCanvas; Grapher: TlicgBaseGrapher; var Vertices:
  array of TPoint; var Parts: array of Integer; PartCount: Integer; Color:
  TColor; ClipRgn: HRgn; ATransparency: Byte = 127);
var
  N, cnt: Integer;
  Xmin: Integer;
  Xmax: Integer;
  Ymin: Integer;
  Ymax: Integer;
  DestRect: TRect;
  I, K, Idx1, Idx2: Integer;
  PolyRgn: HRgn;
begin
  if PartCount = 0 then
    Exit;
  Xmin := MaxInt;
  Ymin := MaxInt;
  Xmax := 0;
  Ymax := 0;
  N := 0;
  for cnt := 0 to PartCount - 1 do
    Inc(N, Parts[cnt]);
  for cnt := Low(Vertices) to N - 1 do
  begin
    Xmin := Min(Vertices[cnt].X, Xmin);
    Ymin := Min(Vertices[cnt].Y, Ymin);
    Xmax := Max(Vertices[cnt].X, Xmax);
    Ymax := Max(Vertices[cnt].Y, Ymax);
  end;
  DestRect := Rect(Xmin, Ymin, Xmax, Ymax);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Idx1 := 0;
    Idx2 := 0;
  end
  else
  begin
    Idx1 := 0;
    Idx2 := PartCount - 1;
  end;

  K := 0;

  for I := Idx1 to Idx2 do
  begin
    if PartCount = 1 then
      PolyRgn := CreatePolygonRgn(Vertices[0], Parts[0], WINDING)
    else
    begin
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        PolyRgn := CreatePolyPolygonRgn(Vertices, Parts, PartCount, ALTERNATE)
      else
      begin
        PolyRgn := CreatePolygonRgn(Vertices[K], Parts[I], WINDING);
        Inc(K, Parts[I]);
      end;
    end;
    if PolyRgn = 0 then
      Exit;

    SelectClipRgn(Canvas.Handle, PolyRgn);
    try
      ColorBlend(Canvas.Handle, DestRect, Color, ATransparency);
    finally
      SelectClipRgn(Canvas.Handle, ClipRgn);
      DeleteObject(PolyRgn);
    end;
  end;
end;

{$R-}

function GetMemEx(size: DWORD): pointer;
begin
  try
    result := Pointer(GlobalAlloc(GPTR, size));
  except
    result := nil;
  end;
end;

function FreeMemEx(p: pointer): pointer;
begin
  try
    if p = nil then
    begin
      result := nil;
    end
    else
    begin
      result := Pointer(GlobalFree(THandle(p)));
    end;
  except
    result := nil;
  end;
end;

{ LoadDIBFromStream }

function LoadDIBFromStream(TheStream: TStream; var lpBitmapInfo: PBITMAPINFO;
  var lpBits: Pointer; var BitmapWidth: integer; var BitmapHeight: integer): Boolean;
var
  bmf: TBITMAPFILEHEADER;
  TheFileSize: integer;
  BitmapHeaderSize: integer;
  TheImageSize: integer;
begin
  lpBitmapInfo := nil;
  lpBits := nil;
  BitmapWidth := 0;
  BitmapHeight := 0;
  if TheStream = nil then
  begin
    result := FALSE;
    exit;
  end;
  try
    TheFileSize := TheStream.Size - TheStream.Position;
    TheStream.ReadBuffer(bmf, sizeof(TBITMAPFILEHEADER));
  except
    result := FALSE;
    exit;
  end;
  BitmapHeaderSize := bmf.bfOffBits - sizeof(TBITMAPFILEHEADER);
  TheImageSize := TheFileSize - integer(bmf.bfOffBits);
  if ((bmf.bfType <> $4D42) or (integer(bmf.bfOffBits) < 1) or (TheFileSize < 1)
    or (BitmapHeaderSize < 1) or (TheImageSize < 1) or (TheFileSize < (sizeof(TBITMAPFILEHEADER)
    + BitmapHeaderSize + TheImageSize))) then
  begin
    result := FALSE;
    exit;
  end;
  lpBitmapInfo := GetMemEx(BitmapHeaderSize);
  try
    TheStream.ReadBuffer(lpBitmapInfo^, BitmapHeaderSize);
  except
    try
      FreeMemEx(lpBitmapInfo);
    except
    end;
    lpBitmapInfo := nil;
    result := FALSE;
    exit;
  end;
  try
    BitmapWidth := lpBitmapInfo^.bmiHeader.biWidth;
    BitmapHeight := abs(lpBitmapInfo^.bmiHeader.biHeight);
    if lpBitmapInfo^.bmiHeader.biSizeImage <> 0 then
    begin
      TheImageSize := lpBitmapInfo^.bmiHeader.biSizeImage;
    end
    else
    begin
      TheImageSize := (((((lpBitmapInfo^.bmiHeader.biWidth * lpBitmapInfo^.bmiHeader.biBitCount)
        + 31) and not 31) shr 3) * ABS(lpBitmapInfo^.bmiHeader.biHeight));
    end;
  except
    try
      FreeMemEx(lpBitmapInfo);
    except
    end;
    lpBitmapInfo := nil;
    BitmapWidth := 0;
    BitmapHeight := 0;
    result := FALSE;
    exit;
  end;
  if (BitmapWidth < 1) or (BitmapHeight < 1) or (TheImageSize < 32) then
  begin
    try
      FreeMemEx(lpBitmapInfo);
    except
    end;
    lpBitmapInfo := nil;
    BitmapWidth := 0;
    BitmapHeight := 0;
    result := FALSE;
    exit;
  end;
  lpBits := GetMemEx(TheImageSize);
  try
    TheStream.ReadBuffer(lpBits^, TheImageSize);
  except
    try
      FreeMemEx(lpBits);
    except
    end;
    try
      FreeMemEx(lpBitmapInfo);
    except
    end;
    lpBits := nil;
    lpBitmapInfo := nil;
    result := FALSE;
    exit;
  end;
  result := True;
end;

{ LoadDIBFromFile }

function LoadDIBFromFile(const FileName: string; var lpBitmapInfo: PBITMAPINFO;
  var lpBits: Pointer; var BitmapWidth: integer; var BitmapHeight: integer): Boolean;
var
  TheFileStream: TFileStream;
begin
  lpBitmapInfo := nil;
  lpBits := nil;
  BitmapWidth := 0;
  BitmapHeight := 0;
  try
    TheFileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    result := FALSE;
    exit;
  end;
  result := LoadDIBFromStream(TheFileStream, lpBitmapInfo, lpBits, BitmapWidth,
    BitmapHeight);

  TheFileStream.Free;
end;

{ LoadDIBFromTBitmap }

function LoadDIBFromTBitmap(ABitmap: TBitmap; var lpBitmapInfo: PBITMAPINFO; var
  lpBits: Pointer; var BitmapWidth: integer; var BitmapHeight: integer): Boolean;
var
  TheStream: TMemoryStream;
begin
  lpBitmapInfo := nil;
  lpBits := nil;
  BitmapWidth := 0;
  BitmapHeight := 0;
  TheStream := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(TheStream);
    TheStream.Position := 0;
  except
    TheStream.Free;
    result := FALSE;
    exit;
  end;
  result := LoadDIBFromStream(TheStream, lpBitmapInfo, lpBits, BitmapWidth, BitmapHeight);
  TheStream.Free;
end;

{Constructor TBBitmapEx.Create;
Var
  HeaderSize: Integer;
begin
  Inherited Create;
  HeaderSize := SizeOf(TBitmapInfoHeader) + 3 * SizeOf(TRGBQuad);
  Fbmi := GetMemEx(HeaderSize);
  With Fbmi^.bmiHeader Do
  begin
    biSize := SizeOf(Fbmi^.bmiHeader);
    biWidth := 0;
    biHeight := 0;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := BI_RGB;
    biSizeImage := 0;
    biXPelsPerMeter := 1; //dont care
    biYPelsPerMeter := 1; //dont care
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  FTempBitmap := TBitmap.Create;
  FTempStream := TMemoryStream.Create;

end; }

{Destructor TBBitmapEx.Destroy;
begin
  FTempBitmap.Free;
  FTempStream.Free;
  FreeMemEx(Fbmi);
  Inherited Destroy;
end; }

procedure TlicgBitmapEx.SetAlphaChannel(Value: Byte);
var
  x: Integer;
begin
  for x := -255 to 255 do
    FBlendTable[x] := (Value * x) shr 8;
  FAlphaChannel := Value;
end;

function TlicgBitmapEx.TileCurrentBand(const CurrentTileRect: TRect): Boolean;
var
  img_start: integer;
  img_end: integer;
  img_numscans: integer;
  ScanOffsetInFile: integer;
  OldHeight: integer;
  Bits: pointer;
  TmpBitmap: TBitmap;
  x, y, bgx, bgy, bgw, bgh: Integer;
  p1_32: pRGBQuadArray;
  p1_24, p2: pRGBTripleArray;
  scanlin: Pointer;
  BackgFormat: TPixelFormat;
begin
  img_start := Round(FTileGlobalInfo.SourceLastScanLine);
  if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
  begin
    FTileGlobalInfo.SourceLastScanLine := FTileGlobalInfo.SourceLastScanLine +
      FTileGlobalInfo.SourceFirstTileHeight;
    FTileGlobalInfo.SourceFirstTileHeight := 0;
  end
  else
  begin
    FTileGlobalInfo.SourceLastScanLine := FTileGlobalInfo.SourceLastScanLine +
      FTileGlobalInfo.SourceBandHeight;
  end;
  img_end := Round(FTileGlobalInfo.SourceLastScanLine);
  if img_end > FTileGlobalInfo.TotalBitmapHeight - 1 then
  begin
    img_end := FTileGlobalInfo.TotalBitmapHeight - 1;
  end;
  img_numscans := img_end - img_start;
  if img_numscans < 1 then
  begin
    result := True;
    Exit;
  end;
  OldHeight := FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight;
  if FTileGlobalInfo.SourceIsTopDown then
  begin
    FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := -img_numscans;
  end
  else
  begin
    FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := img_numscans;
  end;
  bits := GetMemEx(Abs(FTileGlobalInfo.SourceBytesPerScanLine) * img_numscans);
  ScanOffsetInFile := FTileGlobalInfo.TotalBitmapHeight - (img_start + img_numscans);
  FTileGlobalInfo.TheStream.Seek(integer(FTileGlobalInfo.bmf.bfOffBits) + (ScanOffsetInFile
    * abs(FTileGlobalInfo.SourceBytesPerScanLine)), soFromBeginning);
  FTileGlobalInfo.TheStream.ReadBuffer(bits^, abs(FTileGlobalInfo.SourceBytesPerScanLine)
    * img_numscans);

  if (FAlphaChannel = 0) or (FBufferBitmap = nil) then
  begin
    // FBufferBitmap=nil means we are printing
    try
      { draw the bitmap }
      SetStretchBltMode(FTileGlobalInfo.dc, COLORONCOLOR);
      StretchDIBits(FTileGlobalInfo.dc, CurrentTileRect.Left, CurrentTileRect.Top,
        Abs(CurrentTileRect.Right - CurrentTileRect.Left), abs(CurrentTileRect.Bottom
        - CurrentTileRect.Top), FTileGlobalInfo.SourceRect.Left, // left
        0, // top
        FTileGlobalInfo.SourceRect.Right - FTileGlobalInfo.SourceRect.Left, // width
        img_numscans, // height
        Bits, FTileGlobalInfo.lpBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);

    finally
      FreeMemEx(bits);
      FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := OldHeight;
    end;
  end
  else if (FAlphaChannel > 0) and (FBufferBitmap <> nil) and (FBufferBitmap.PixelFormat
    in [pf24bit, pf32bit]) then
  begin
    { will do it transparent }
    TmpBitmap := TBitmap.Create;
    try
        { create a temporary bitmap }
      TmpBitmap.PixelFormat := pf24bit;
      TmpBitmap.Width := abs(CurrentTileRect.Right - CurrentTileRect.Left);
      TmpBitmap.Height := abs(CurrentTileRect.Bottom - CurrentTileRect.Top);
        { now stretch the original bitmap onto this one }
      SetStretchBltMode(TmpBitmap.Canvas.Handle, COLORONCOLOR);
      StretchDIBits(TmpBitmap.Canvas.Handle, 0, 0, abs(CurrentTileRect.Right -
        CurrentTileRect.Left), abs(CurrentTileRect.Bottom - CurrentTileRect.Top),
        FTileGlobalInfo.SourceRect.Left, // left
        0, // top
        FTileGlobalInfo.SourceRect.Right - FTileGlobalInfo.SourceRect.Left, // width
        img_numscans, // height
        Bits, FTileGlobalInfo.lpBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
        { now combine the two bitmaps: FBufferBitmap and TmpBitmap }
      BackgFormat := FBufferBitmap.PixelFormat;

      bgw := FBufferBitmap.Width;
      bgh := FBufferBitmap.Height;
      for y := 0 to TmpBitmap.Height - 1 do
      begin
          { it is assumed that FBufferBitmap.PixelFormat = pf24bit }
        bgy := y + CurrentTileRect.Top;
        if (bgy < 0) or (bgy > bgh - 1) then
          Continue;
        scanlin := FBufferBitmap.ScanLine[bgy];
        p1_24 := scanlin;
        p1_32 := scanlin;
        p2 := TmpBitmap.ScanLine[y];
        for x := 0 to TmpBitmap.Width - 1 do
        begin
          bgx := x + CurrentTileRect.Left;
          if (bgx < 0) or (bgx > bgw - 1) then
            Continue;
          case BackgFormat of
            pf24bit:
              with p1_24^[bgx] do
              begin
                rgbtBlue := FBlendTable[rgbtBlue - p2^[x].rgbtBlue] + p2^[x].rgbtBlue;
                rgbtGreen := FBlendTable[rgbtGreen - p2^[x].rgbtGreen] + p2^[x].rgbtGreen;
                rgbtRed := FBlendTable[rgbtRed - p2^[x].rgbtRed] + p2^[x].rgbtRed;
              end;
            pf32bit:
              with p1_32^[bgx] do
              begin
                rgbBlue := FBlendTable[rgbBlue - p2^[x].rgbtBlue] + p2^[x].rgbtBlue;
                rgbGreen := FBlendTable[rgbGreen - p2^[x].rgbtGreen] + p2^[x].rgbtGreen;
                rgbRed := FBlendTable[rgbRed - p2^[x].rgbtRed] + p2^[x].rgbtRed;
              end;
          end;
        end;
      end;
    finally
      TmpBitmap.free;
        //if BmpBits <> nil then FreeMemEx(BmpBits);
      FreeMemEx(bits);
      FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := OldHeight;
    end;
  end;

  result := True;
end;

  { This code will fill a bitmap by resampling an image coming from a big bitmap on disk.
    Only the area on interest will be accessed on disk and in scanlines, that is to say,
    not all the image will be read.

    FileName.- Name of the uncompressed bitamp to read
    Stream.- If <> Nil, the bitmap info will be read from here instead of FileName.
    dc.- Target device context where the bitmap section will be resampled.
    DestLeft, DestTop.- Top left of the dc where the resampling will take place
    DestWidth, DestHeight.- dimensions of the target area where the bitmap section will be resampled
    DestTotalHeight.- Usually this is equal to DestHeight, but sometimes this can be greater because
      this is used for obtaining a scale factor of the bitmap on disk against a target rectangle on the
      device context.
    SourceLeft, SourceTop.- Topleft of physical source bitmap on disk.
    SourceWidth, SourceHeight.- Dimensions of the physical bitmap section that will be extracted from disk.
    BufferSize.- The size of a memory buffer used for reading scanlines from the physical bitmap on disk.
      This value will decide how many scanlines can be read from disk at the same time, with always a
      minimum value of 2 scanlines.
  }

function TlicgBitmapEx.GetPainterObject: IlicgPainterObject;
begin
  Result := FPainterObject
end;

procedure TlicgBitmapEx.SetPainterObject(const Value: IlicgPainterObject);
begin
  FPainterObject := Value
end;

function TlicgBitmapEx.GetWasSuspended: Boolean;
begin
  Result := FWasSuspended
end;

function TlicgBitmapEx.GetAlphaChannel: byte;
begin
  Result := FAlphaChannel
end;

function TlicgBitmapEx.GetBufferBitmap: TBitmap;
begin
  Result := FBufferBitmap
end;

procedure TlicgBitmapEx.SetBufferBitmap(Value: TBitmap);
begin
  FBufferBitmap := Value
end;

function TlicgBitmapEx.BitDIBFromFileInBands(const FileName: string; Stream:
  TStream; dc: HDC; DestLeft, DestTop, DestWidth, DestHeight, DestTotalHeight,
  SourceLeft, SourceTop, SourceWidth, SourceHeight, BufferSize: integer): Boolean;
var
  TheFileSize: integer;
  TheImageSize: integer;
  TheBitmapInfo: PBITMAPINFO;
  dest_MaxScans, dsty_top, vdest_TilesDown: integer;
  dy: extended;
  dest_Residual: integer;
  vdest_TileHeight: Integer;
  CurrentTileRect: TRect;
  DestBottom: Integer;
  SourceScaleY: extended;
begin
  result := FALSE;
  FTileGlobalInfo.SourceRect := Rect(SourceLeft, SourceTop, SourceLeft +
    SourceWidth, SourceTop + SourceHeight);
  if Stream = nil then
    FTileGlobalInfo.TheStream := TFileStream.Create(FileName, fmOpenRead or
      fmShareDenyWrite)
  else
  begin
    Stream.Seek(0, 0);
    FTileGlobalInfo.TheStream := Stream;
  end;
  TheFileSize := FTileGlobalInfo.TheStream.Size;
  FTileGlobalInfo.TheStream.ReadBuffer(FTileGlobalInfo.bmf, sizeof(TBITMAPFILEHEADER));
  FTileGlobalInfo.BitmapHeaderSize := FTileGlobalInfo.bmf.bfOffBits - sizeof(TBITMAPFILEHEADER);
  TheImageSize := TheFileSize - integer(FTileGlobalInfo.bmf.bfOffBits);
  if ((FTileGlobalInfo.bmf.bfType <> $4D42) or (integer(FTileGlobalInfo.bmf.bfOffBits)
    < 1) or (TheFileSize < 1) or (FTileGlobalInfo.BitmapHeaderSize < 1) or (TheImageSize
    < 1) or (TheFileSize < (sizeof(TBITMAPFILEHEADER) + FTileGlobalInfo.BitmapHeaderSize
    + TheImageSize))) then
  begin
    if Stream = nil then
      FTileGlobalInfo.TheStream.Free;
    exit;
  end;
  FTileGlobalInfo.lpBitmapInfo := GetMemEx(FTileGlobalInfo.BitmapHeaderSize);
  try
    FTileGlobalInfo.TheStream.ReadBuffer(FTileGlobalInfo.lpBitmapInfo^,
      FTileGlobalInfo.BitmapHeaderSize);
  except
    FreeMemEx(FTileGlobalInfo.lpBitmapInfo);
    if Stream = nil then
      FTileGlobalInfo.TheStream.Free;
    exit;
  end;
  TheBitmapInfo := FTileGlobalInfo.lpBitmapInfo;
  if ((TheBitmapInfo^.bmiHeader.biCompression = BI_RLE4) or (TheBitmapInfo^.bmiHeader.biCompression
    = BI_RLE8)) then
  begin
    FreeMemEx(FTileGlobalInfo.lpBitmapInfo);
    if Stream = nil then
      FTileGlobalInfo.TheStream.Free;
    Exit;
  end;
  FTileGlobalInfo.TotalBitmapWidth := TheBitmapInfo^.bmiHeader.biWidth;
  FTileGlobalInfo.TotalBitmapHeight := abs(TheBitmapInfo^.bmiHeader.biHeight);
  FTileGlobalInfo.SourceIsTopDown := (TheBitmapInfo^.bmiHeader.biHeight < 0);
  FTileGlobalInfo.SourceBytesPerScanLine := ((((TheBitmapInfo^.bmiHeader.biWidth
    * TheBitmapInfo^.bmiHeader.biBitCount) + 31) and not 31) shr 3);

  FTileGlobalInfo.dc := dc;
  SourceScaleY := FTileGlobalInfo.TotalBitmapHeight / DestTotalHeight;
  FTileGlobalInfo.SourceLastScanLine := SourceTop;

  if BufferSize < abs(FTileGlobalInfo.SourceBytesPerScanLine) then
  begin
    BufferSize := abs(FTileGlobalInfo.SourceBytesPerScanLine);
  end;

  dest_MaxScans := round(BufferSize / abs(FTileGlobalInfo.SourceBytesPerScanLine));
  dest_MaxScans := round(dest_MaxScans * (DestTotalHeight / FTileGlobalInfo.TotalBitmapHeight));

  if dest_MaxScans < 2 then
    dest_MaxScans := 2;

  if dest_MaxScans > FTileGlobalInfo.TotalBitmapHeight then
    dest_MaxScans := FTileGlobalInfo.TotalBitmapHeight;

  { count the tiles down }
  dsty_top := 0;
  vdest_TilesDown := 0;
  while (dsty_Top + dest_MaxScans) <= DestTotalHeight do
  begin
    inc(vdest_TilesDown);
    inc(dsty_top, dest_MaxScans);
  end;

  if vdest_TilesDown = 0 then
  begin
    FTileGlobalInfo.SourceBandHeight := 0;
    FTileGlobalInfo.SourceFirstTileHeight := FTileGlobalInfo.TotalBitmapHeight;
  end
  else
  begin
    dest_Residual := DestTotalHeight mod dest_MaxScans;

    FTileGlobalInfo.SourceBandHeight := (FTileGlobalInfo.TotalBitmapHeight * (1
      - (dest_Residual / DestTotalHeight))) / vdest_TilesDown;

    if SourceTop > 0 then
    begin
      dy := 0;
      while dy < SourceTop do
        dy := dy + FTileGlobalInfo.SourceBandHeight;
      FTileGlobalInfo.SourceFirstTileHeight := (dy - SourceTop);
    end
    else
    begin
      FTileGlobalInfo.SourceFirstTileHeight := 0;
    end;
  end;

  { tile the scanlines }
  FWasSuspended := FALSE;

  CurrentTileRect.Top := DestTop;
  if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
  begin
    vdest_TileHeight := Round(FTileGlobalInfo.SourceFirstTileHeight * (1.0 /
      SourceScaleY));
    if vdest_TileHeight = 0 then
    begin
      vdest_TileHeight := dest_MaxScans;
      FTileGlobalInfo.SourceFirstTileHeight := 0;
    end;
  end
  else
    vdest_TileHeight := dest_MaxScans;
  CurrentTileRect.Bottom := DestTop + vdest_TileHeight;
  DestBottom := DestTop + DestHeight;
  if CurrentTileRect.Bottom > DestBottom then
  begin
    CurrentTileRect.Bottom := DestBottom;
    if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
      FTileGlobalInfo.SourceFirstTileHeight := FTileGlobalInfo.SourceFirstTileHeight
        * (Abs(CurrentTileRect.Bottom - CurrentTileRect.Top) / vdest_TileHeight)
    else
      FTileGlobalInfo.SourceBandHeight := FTileGlobalInfo.SourceBandHeight * (Abs
        (CurrentTileRect.Bottom - CurrentTileRect.Top) / vdest_TileHeight);
  end;
  CurrentTileRect.Left := DestLeft;
  CurrentTileRect.Right := DestLeft + DestWidth;
  while CurrentTileRect.Top < DestBottom do
  begin
    if not Windows.IsRectEmpty(CurrentTileRect) then
    begin
      if not TileCurrentBand(CurrentTileRect) then
        Break;
    end;
    CurrentTileRect.Top := CurrentTileRect.Bottom;
    CurrentTileRect.Bottom := CurrentTileRect.Top + dest_MaxScans;
    if CurrentTileRect.Bottom > DestBottom then
    begin
      CurrentTileRect.Bottom := DestBottom;
      FTileGlobalInfo.SourceBandHeight := (Abs(CurrentTileRect.Bottom -
        CurrentTileRect.Top) / dest_MaxScans) * FTileGlobalInfo.SourceBandHeight;
    end;

    if PainterObject <> nil then
    begin
      if not PainterObject.InThread then
      begin
       { PainterObject.I_sTimer, sadece PainterObject.D_rawEntities fonksiyonunda set ediliyor,
                  onun dýþýnda set edildiði bir yer bulamadýðýmdan, burada bu deðiþkenin zaten hep false olduðunu düþünüyorum - KAYATTIM.

        if PainterObject.I_sTimer and (GetTickCount > Cardinal(PainterObject.T_ickStart + PainterObject.S_ourceGis.TimerFrequency)) then begin
          PainterObject.S_ourceGis.OnGisTimer(PainterObject.S_ourceGis, FWasSuspended);
          if FWasSuspended then
            Exit;

          PainterObject.TickStart := GetTickCount;
        end;}
      end
      else if PainterObject.Terminated then
      begin
        FWasSuspended := True;
        Exit;
      end;
    end;
  end;

  if Stream = nil then
    FTileGlobalInfo.TheStream.Free;
  FreeMemEx(FTileGlobalInfo.lpBitmapInfo);
end;

function GetDIBDimensions(const FileName: string; Stream: TStream; var
  BitmapWidth, BitmapHeight: integer; var IsCompressed: Boolean): Boolean;
var
  bmf: TBITMAPFILEHEADER;
  AFileSize: integer;
  AHeaderSize: integer;
  AImageSize: integer;
  ABitmapInfo: PBITMAPINFO;
  lpBitmapInfo: pointer;
  AStream: TStream;
begin
  Result := False;
  IsCompressed := False;
  BitmapWidth := 0;
  BitmapHeight := 0;
  if Stream = nil then
    AStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite)
  else
  begin
    Stream.Seek(0, 0);
    AStream := Stream;
  end;
  try
    AFileSize := AStream.Size;
    AStream.ReadBuffer(bmf, sizeof(TBITMAPFILEHEADER));
  except
    if Stream = nil then
      AStream.Free;
    exit;
  end;
  AHeaderSize := bmf.bfOffBits - sizeof(TBITMAPFILEHEADER);
  AImageSize := AFileSize - integer(bmf.bfOffBits);
  if ((bmf.bfType <> $4D42) or (integer(bmf.bfOffBits) < 1) or (AFileSize < 1)
    or (AHeaderSize < 1) or (AImageSize < 1) or (AFileSize < (sizeof(TBITMAPFILEHEADER)
    + AHeaderSize + AImageSize))) then
  begin
    if Stream = nil then
      AStream.Free;
    Exit;
  end;
  lpBitmapInfo := GetMemEx(AHeaderSize);
  try
    AStream.ReadBuffer(lpBitmapInfo^, AHeaderSize);
  except
    FreeMemEx(lpBitmapInfo);
    if Stream = nil then
      AStream.Free;
    Exit;
  end;
  try
    ABitmapInfo := lpBitmapInfo;
    BitmapWidth := ABitmapInfo^.bmiHeader.biWidth;
    BitmapHeight := Abs(ABitmapInfo^.bmiHeader.biHeight);
    if (ABitmapInfo^.bmiHeader.biSizeImage <> 0) then
    begin
      AImageSize := ABitmapInfo^.bmiHeader.biSizeImage;
    end
    else
    begin
      AImageSize := (((((ABitmapInfo^.bmiHeader.biWidth * ABitmapInfo^.bmiHeader.biBitCount)
        + 31) and not 31) shr 3) * ABS(ABitmapInfo^.bmiHeader.biHeight));
    end;
    IsCompressed := (ABitmapInfo^.bmiHeader.biCompression = BI_RLE4) or (ABitmapInfo
      ^.bmiHeader.biCompression = BI_RLE8);
  except
    FreeMemEx(lpBitmapInfo);
    if Stream = nil then
      AStream.Free;
    BitmapWidth := 0;
    BitmapHeight := 0;
    Exit;
  end;
  FreeMemEx(lpBitmapInfo);
  if Stream = nil then
    AStream.Free;
  if (BitmapWidth < 1) or (BitmapHeight < 1) or (AImageSize < 32) then
  begin
    BitmapWidth := 0;
    BitmapHeight := 0;
    Exit;
  end;
  result := True;
end;

function GetBILDimensions(const FileName: string; var BitmapWidth, BitmapHeight:
  integer): Boolean;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;
  with TlicgBILReader.Create(FileName) do
  try
    if not ReadHdr then
      Exit;
    BitmapWidth := NCOLS;
    BitmapHeight := NROWS;
    Result := True;
  finally
    Free;
  end;
end;

{ BIL format support }

constructor TlicgBILReader.Create(const FileName: string);
begin
  inherited Create;
  FFileName := FileName;
end;

{ Reads .HDR file }

function TlicgBILReader.ReadHdr: Boolean;
var
  val1: string;
  val2: string;
  buf: string;
  TempFilename: string;
  Lines: TStrings;
  temp, loop: integer;
  SaveSeparator: Char;
begin

  Result := False;

  TempFilename := ChangeFileExt(FFileName, '.hdr');

  if not FileExists(TempFileName) then
    Exit;

  // default values
  BigEndian := False;
  self.NBANDS := 1;
  self.NBITS := 8;
  self.SKIPBYTES := 0;
  self.BANDROWBYTES := 0;
  self.TOTALROWBYTES := 0;

  SaveSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Lines := TStringList.create;
  try
    Lines.LoadFromFile(TempFilename);
    for loop := 0 to Lines.Count - 1 do
    begin

      buf := StringReplace(Lines[loop], #9, #32, [rfReplaceAll]);

      temp := AnsiPos('#', buf);
      if temp > 0 then
        buf := Trim(copy(buf, 1, temp - 1));
      if Length(buf) = 0 then
        Continue;

      temp := AnsiPos(#32, buf);
      if temp = 0 then
        continue;
      val1 := AnsiUpperCase(Trim(Copy(buf, 1, temp - 1)));
      val2 := Trim(Copy(buf, temp, length(buf)));
      if AnsiCompareText('BYTEORDER', val1) = 0 then
      begin
        // M = Motorola byte order (most significant byte first)
        if val2 = 'M' then
          BigEndian := True
        else
          BigEndian := False;
      end
      else if AnsiCompareText('LAYOUT', val1) = 0 then
      begin
        // BIL = band interleaved by line
        if val2 <> 'BIL' then
          exit; // can be BIP
      end
      else if AnsiCompareText('SKIPBYTES', val1) = 0 then
      begin
        // number of rows in the image
        SKIPBYTES := StrToInt(val2);
      end
      else if AnsiCompareText('NROWS', val1) = 0 then
      begin
        // number of rows in the image
        NROWS := StrToInt(val2);
      end
      else if AnsiCompareText('NCOLS', val1) = 0 then
      begin
        //number of columns in the image
        NCOLS := StrToInt(val2);
      end
      else if AnsiCompareText('NBANDS', val1) = 0 then
      begin
        // number of spectral bands in the image
        NBANDS := StrToInt(val2);
      end
      else if AnsiCompareText('NBITS', val1) = 0 then
      begin
        // number of bits per pixel
        NBITS := StrToInt(val2);
      end
      else if AnsiCompareText('BANDROWBYTES', val1) = 0 then
      begin
        // number of bytes per band per row
        BANDROWBYTES := StrToInt(val2);
      end
      else if AnsiCompareText('TOTALROWBYTES', val1) = 0 then
      begin
        // total number of bytes of data per row
        TOTALROWBYTES := StrToInt(val2);
      end
      else if AnsiCompareText('BANDGAPBYTES', val1) = 0 then
      begin
        // the number of bytes between bands in a BSQ format image
        BANDGAPBYTES := StrToInt(val2);
      end
      else if AnsiCompareText('NODATA', val1) = 0 then
      begin
        // value used for masking purposes
        NODATA := StrToInt(val2);
      end
      else if AnsiCompareText('ULXMAP', val1) = 0 then
      begin
        // longitude of the center of the upper-left pixel (decimal degrees)
        ULXMAP := StrToFloat(val2);
      end
      else if AnsiCompareText('ULYMAP', val1) = 0 then
      begin
        // latitude  of the center of the upper-left pixel (decimal degrees)
        ULYMAP := StrToFloat(val2);
      end
      else if AnsiCompareText('XDIM', val1) = 0 then
      begin
        // x dimension of a pixel in geographic Units (decimal degrees)
        XDIM := StrToFloat(val2);
      end
      else if AnsiCompareText('YDIM', val1) = 0 then
      begin
        // Y dimension of a pixel in geographic Units (decimal degrees)
        YDIM := StrToFloat(val2);
      end;

    end;

    if self.BANDROWBYTES = 0 then
      self.BANDROWBYTES := Round((Self.NCOLS * Self.NBITS) div 8);

    if self.TOTALROWBYTES = 0 then
      self.TOTALROWBYTES := self.NBANDS * self.BANDROWBYTES;

    Result := True;

  finally
    Lines.free;
    FormatSettings.DecimalSeparator := SaveSeparator;
  end;

end;

procedure TlicgBILReader.SetAlphaChannel(Value: Byte);
var
  x: Integer;
begin
  for x := -255 to 255 do
    FBlendTable[x] := (Value * x) shr 8;
  FAlphaChannel := Value;
end;

function TlicgBILReader.TileCurrentBand(const CurrentTileRect: TRect): Boolean;
var
  img_start: integer;
  img_end: integer;
  img_numscans: integer;
  ScanOffsetInFile: integer;
  OldHeight: integer;
  srcBits, destBits: PByte;
  TmpBitmap: TBitmap;
  x, y, bgx, bgy, bgw, bgh: Integer;
  p1_32: pRGBQuadArray;
  p1_24, p2: pRGBTripleArray;
  scanlin: Pointer;
  BackgFormat: TPixelFormat;
  DestBytesPerScanLine: integer;

  procedure ConvertToColor24;
  var
    i, j, SrcOffset, DstOffset, width2, t: Integer;
  begin
    for i := 0 to img_numscans - 1 do
    begin
      SrcOffset := i * self.TOTALROWBYTES;
      DstOffset := i * DestBytesPerScanLine;
      width2 := 2 * self.BANDROWBYTES;
      for j := 0 to self.BANDROWBYTES - 1 do
      begin
        // red byte
        t := PByte(Integer(SrcBits) + SrcOffset + j + 0)^;
        PByte(Integer(DestBits) + DstOffset + 3 * j + 2)^ := Min(t, 255);

        // green byte
        t := PByte(Integer(SrcBits) + SrcOffset + j + self.BANDROWBYTES)^;
        PByte(Integer(DestBits) + DstOffset + 3 * j + 1)^ := Min(t, 255);

        // blue byte
        t := PByte(Integer(SrcBits) + SrcOffset + j + width2)^;
        PByte(Integer(DestBits) + DstOffset + 3 * j + 0)^ := Min(t, 255);
      end;
    end;
  end;

  procedure BILToBitmap;
  var
    i, srcOffset, destOffset: integer;
  begin
    for i := 0 to img_numscans - 1 do
    begin
      srcOffset := i * self.TOTALROWBYTES;
      destOffset := i * DestBytesPerScanLine;
      CopyMemory(PByte(Integer(destBits) + destOffset), PByte(Integer(srcBits) +
        srcOffset), self.TOTALROWBYTES);
    end;
  end;

begin

  img_start := Round(FTileGlobalInfo.SourceLastScanLine);
  if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
  begin
    FTileGlobalInfo.SourceLastScanLine := FTileGlobalInfo.SourceLastScanLine +
      FTileGlobalInfo.SourceFirstTileHeight;
    FTileGlobalInfo.SourceFirstTileHeight := 0;
  end
  else
  begin
    FTileGlobalInfo.SourceLastScanLine := FTileGlobalInfo.SourceLastScanLine +
      FTileGlobalInfo.SourceBandHeight;
  end;
  img_end := Round(FTileGlobalInfo.SourceLastScanLine);
  if img_end > FTileGlobalInfo.TotalBitmapHeight - 1 then
  begin
    img_end := FTileGlobalInfo.TotalBitmapHeight - 1;
  end;
  img_numscans := img_end - img_start;
  if img_numscans < 1 then
  begin
    Result := True;
    Exit;
  end;
  OldHeight := FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight;
  if FTileGlobalInfo.SourceIsTopDown then
  begin
    FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := -img_numscans;
  end
  else
  begin
    FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := img_numscans;
  end;

  DestBytesPerScanLine := ((((self.NCOLS * FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biBitCount)
    + 31) and not 31) shr 3);
  destBits := GetMemEx(DestBytesPerScanLine * img_numscans);

  srcBits := GetMemEx(self.TOTALROWBYTES * img_numscans);

  ScanOffsetInFile := self.SKIPBYTES + img_start * self.TOTALROWBYTES;
  FTileGlobalInfo.TheStream.Seek(ScanOffsetInFile, soFromBeginning);
  FTileGlobalInfo.TheStream.ReadBuffer(srcbits^, self.TOTALROWBYTES * img_numscans);

  if Self.NBANDS >= 3 then
    { will convert to 24 bpp }
    ConvertToColor24
  else
    BILToBitmap;

  if (FAlphaChannel = 0) or (FBufferBitmap = nil) then
  begin
    // FBufferBitmap=nil means we are printing
    try
      { draw the bitmap }
      SetStretchBltMode(FTileGlobalInfo.dc, COLORONCOLOR);
      StretchDIBits(FTileGlobalInfo.dc, CurrentTileRect.Left, CurrentTileRect.Top,
        Abs(CurrentTileRect.Right - CurrentTileRect.Left), abs(CurrentTileRect.Bottom
        - CurrentTileRect.Top), FTileGlobalInfo.SourceRect.Left, // left
        0, // top
        FTileGlobalInfo.SourceRect.Right - FTileGlobalInfo.SourceRect.Left, // width
        img_numscans, // height
        destBits, FTileGlobalInfo.lpBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
    finally
      FreeMemEx(srcbits);
      FreeMemEx(destbits);
      FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := OldHeight;
    end;
  end
  else if (FAlphaChannel > 0) and (FBufferBitmap <> nil) and (FBufferBitmap.PixelFormat
    in [pf24bit, pf32bit]) then
  begin
    { will do it transparent }
      //end;
    TmpBitmap := TBitmap.Create;
    try
      { create a temporary bitmap }
      TmpBitmap.PixelFormat := pf24bit;
      TmpBitmap.Width := abs(CurrentTileRect.Right - CurrentTileRect.Left);
      TmpBitmap.Height := abs(CurrentTileRect.Bottom - CurrentTileRect.Top);
      { now stretch the original bitmap onto this one }
      SetStretchBltMode(TmpBitmap.Canvas.Handle, COLORONCOLOR);
      StretchDIBits(TmpBitmap.Canvas.Handle, 0, 0, abs(CurrentTileRect.Right -
        CurrentTileRect.Left), abs(CurrentTileRect.Bottom - CurrentTileRect.Top),
        FTileGlobalInfo.SourceRect.Left, // left
        0, // top
        FTileGlobalInfo.SourceRect.Right - FTileGlobalInfo.SourceRect.Left, // width
        img_numscans, // height
        destBits, FTileGlobalInfo.lpBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);
      { now combine the two bitmaps: FBufferBitmap and TmpBitmap }
      BackgFormat := FBufferBitmap.PixelFormat;

      bgw := FBufferBitmap.Width;
      bgh := FBufferBitmap.Height;
      for y := 0 to TmpBitmap.Height - 1 do
      begin
        { it is assumed that FBufferBitmap.PixelFormat = pf24bit }
        bgy := y + CurrentTileRect.Top;
        if (bgy < 0) or (bgy > bgh - 1) then
          Continue;

        scanlin := FBufferBitmap.ScanLine[bgy];
        p1_24 := scanlin;
        p1_32 := scanlin;
        p2 := TmpBitmap.ScanLine[y];
        for x := 0 to TmpBitmap.Width - 1 do
        begin
          bgx := x + CurrentTileRect.Left;
          if (bgx < 0) or (bgx > bgw - 1) then
            Continue;
          case BackgFormat of
            pf24bit:
              with p1_24^[bgx] do
              begin
                rgbtBlue := FBlendTable[rgbtBlue - p2^[x].rgbtBlue] + p2^[x].rgbtBlue;
                rgbtGreen := FBlendTable[rgbtGreen - p2^[x].rgbtGreen] + p2^[x].rgbtGreen;
                rgbtRed := FBlendTable[rgbtRed - p2^[x].rgbtRed] + p2^[x].rgbtRed;
              end;
            pf32bit:
              with p1_32^[bgx] do
              begin
                rgbBlue := FBlendTable[rgbBlue - p2^[x].rgbtBlue] + p2^[x].rgbtBlue;
                rgbGreen := FBlendTable[rgbGreen - p2^[x].rgbtGreen] + p2^[x].rgbtGreen;
                rgbRed := FBlendTable[rgbRed - p2^[x].rgbtRed] + p2^[x].rgbtRed;
              end;
          end;
        end;
      end;
    finally
      TmpBitmap.Free;
      //if BmpBits <> nil then FreeMemEx(BmpBits);
      FreeMemEx(srcbits);
      FreeMemEx(destbits);
      FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight := OldHeight;
    end;
  end;

  Result := True;
end;

function TlicgBILReader.BILFromFileInBands(Stream: TStream; dc: HDC; DestLeft,
  DestTop, DestWidth, DestHeight, DestTotalHeight, SourceLeft, SourceTop,
  SourceWidth, SourceHeight, BufferSize: integer): Boolean;
var
  //TheImageSize: integer;
  TheBitmapInfo: PBITMAPINFO;
  dest_MaxScans, dsty_top, vdest_TilesDown: integer;
  dy: extended;
  t, loop, dest_Residual: integer;
  vdest_TileHeight: Integer;
  CurrentTileRect: TRect;
  DestBottom: Integer;
  SourceScaleY: extended;
  pcolors: PRGBQuad;
  NumEntries: integer;
  StreamUsed: Boolean;
begin
  Result := FALSE;

  if not ReadHDR then
    Exit;

  FTileGlobalInfo.SourceRect := Rect(SourceLeft, SourceTop, SourceLeft +
    SourceWidth, SourceTop + SourceHeight);
  StreamUsed := Stream <> nil;
  if StreamUsed then
    FTileGlobalInfo.TheStream := Stream
  else
    FTileGlobalInfo.TheStream := TFileStream.Create(FFileName, fmOpenRead or
      fmShareDenyWrite);

  case self.NBITS of
    1:
      NumEntries := 2;
    4:
      NumEntries := 16;
    8:
      NumEntries := 256;
  else
    NumEntries := 0;
  end;

  if Self.NBANDS >= 3 then
    NumEntries := 0;

  FTileGlobalInfo.lpBitmapInfo := GetMemEx(sizeof(TBitmapInfoHeader) +
    NumEntries * sizeof(TRGBQuad));
  try
    with FTileGlobalInfo.lpBitmapInfo^ do
    begin
      bmiHeader.biSize := sizeof(TBitmapInfoHeader);
      bmiHeader.biWidth := self.NCOLS;
      bmiHeader.biHeight := -self.NROWS;
      bmiHeader.biPlanes := 1;
      if self.NBANDS >= 3 then
        bmiHeader.biBitCount := 24
      else
        bmiHeader.biBitCount := self.NBITS;
      for loop := 0 to NumEntries - 1 do
      begin
        pcolors := PRGBQuad(Integer(@bmiColors[0]) + loop * sizeof(TRGBQuad));
        t := loop;
        pcolors^.rgbRed := t;
        pcolors^.rgbGreen := t;
        pcolors^.rgbBlue := t;
      end;
      bmiHeader.biSizeImage := 0;
      bmiHeader.biCompression := BI_RGB;
      bmiHeader.biXPelsPerMeter := 0;
      bmiHeader.biYPelsPerMeter := 0;
      bmiHeader.biClrUsed := 0;
      bmiHeader.biClrImportant := 0;
    end;

    FTileGlobalInfo.TotalBitmapWidth := self.NCOLS;
    FTileGlobalInfo.TotalBitmapHeight := self.NROWS;
    FTileGlobalInfo.SourceIsTopDown := (FTileGlobalInfo.lpBitmapInfo^.bmiHeader.biHeight
      < 0);

    FTileGlobalInfo.dc := dc;
    SourceScaleY := FTileGlobalInfo.TotalBitmapHeight / DestTotalHeight;
    FTileGlobalInfo.SourceLastScanLine := SourceTop;

    FTileGlobalInfo.SourceBytesPerScanLine := self.TOTALROWBYTES;

    if BufferSize < abs(FTileGlobalInfo.SourceBytesPerScanLine) then
    begin
      BufferSize := abs(FTileGlobalInfo.SourceBytesPerScanLine);
    end;

    dest_MaxScans := round(BufferSize / FTileGlobalInfo.SourceBytesPerScanLine);
    dest_MaxScans := round(dest_MaxScans * (DestTotalHeight / FTileGlobalInfo.TotalBitmapHeight));

    if dest_MaxScans < 2 then
      dest_MaxScans := 2;

    if dest_MaxScans > FTileGlobalInfo.TotalBitmapHeight then
      dest_MaxScans := FTileGlobalInfo.TotalBitmapHeight;

    { count the tiles down }
    dsty_top := 0;
    vdest_TilesDown := 0;
    while (dsty_Top + dest_MaxScans) <= DestTotalHeight do
    begin
      inc(vdest_TilesDown);
      inc(dsty_top, dest_MaxScans);
    end;
    if vdest_TilesDown = 0 then
    begin
      FTileGlobalInfo.SourceBandHeight := 0;
      FTileGlobalInfo.SourceFirstTileHeight := FTileGlobalInfo.TotalBitmapHeight;
    end
    else
    begin
      dest_Residual := DestTotalHeight mod dest_MaxScans;

      FTileGlobalInfo.SourceBandHeight := (FTileGlobalInfo.TotalBitmapHeight * (1
        - (dest_Residual / DestTotalHeight))) / vdest_TilesDown;

      if SourceTop > 0 then
      begin
        dy := 0;
        while dy < SourceTop do
          dy := dy + FTileGlobalInfo.SourceBandHeight;
        FTileGlobalInfo.SourceFirstTileHeight := (dy - SourceTop);
      end
      else
      begin
        FTileGlobalInfo.SourceFirstTileHeight := 0;
      end;
    end;

    { tile the scanlines }
    FWasSuspended := FALSE;

    CurrentTileRect.Top := DestTop;
    if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
    begin
      vdest_TileHeight := Round(FTileGlobalInfo.SourceFirstTileHeight * (1 /
        SourceScaleY));
      if vdest_TileHeight = 0 then
      begin
        vdest_TileHeight := dest_MaxScans;
        FTileGlobalInfo.SourceFirstTileHeight := 0;
      end;
    end
    else
      vdest_TileHeight := dest_MaxScans;
    CurrentTileRect.Bottom := DestTop + vdest_TileHeight;
    DestBottom := DestTop + DestHeight;
    if CurrentTileRect.Bottom > DestBottom then
    begin
      CurrentTileRect.Bottom := DestBottom;
      if FTileGlobalInfo.SourceFirstTileHeight <> 0 then
        FTileGlobalInfo.SourceFirstTileHeight := FTileGlobalInfo.SourceFirstTileHeight
          * (Abs(CurrentTileRect.Bottom - CurrentTileRect.Top) / vdest_TileHeight)
      else
        FTileGlobalInfo.SourceBandHeight := FTileGlobalInfo.SourceBandHeight * (Abs
          (CurrentTileRect.Bottom - CurrentTileRect.Top) / vdest_TileHeight);
    end;
    CurrentTileRect.Left := DestLeft;
    CurrentTileRect.Right := DestLeft + DestWidth;
    while CurrentTileRect.Top < DestBottom do
    begin
      if not Windows.IsRectEmpty(CurrentTileRect) then
      begin
        if not TileCurrentBand(CurrentTileRect) then
          Break;
      end;
      CurrentTileRect.Top := CurrentTileRect.Bottom;
      CurrentTileRect.Bottom := CurrentTileRect.Top + dest_MaxScans;
      if CurrentTileRect.Bottom > DestBottom then
      begin
        CurrentTileRect.Bottom := DestBottom;
        FTileGlobalInfo.SourceBandHeight := (Abs(CurrentTileRect.Bottom -
          CurrentTileRect.Top) / dest_MaxScans) * FTileGlobalInfo.SourceBandHeight;
      end;

      if PainterObject <> nil then
      begin
        if not PainterObject.InThread then
        begin
          { PainterObject.I_sTimer, sadece PainterObject.DrawEntities fonksiyonunda set ediliyor,
                  onun dýþýnda set edildiði bir yer bulamadýðýmdan, burada bu deðiþkenin zaten hep false olduðunu düþünüyorum - KAYATTIM.

          if PainterObject.I_sTimer and (GetTickCount > Cardinal(PainterObject.T_ickStart + PainterObject.S_ourceGis.TimerFrequency)) then begin
            PainterObject.S_ourceGis.OnGisTimer(PainterObject.S_ourceGis, FWasSuspended);
            if FWasSuspended then
              Exit;

            PainterObject.TickStart := GetTickCount;
          end;}
        end
        else if PainterObject.Terminated then
        begin
          FWasSuspended := True;
          Exit;
        end;
      end;

    end;
  finally
    if not StreamUsed then
      FTileGlobalInfo.TheStream.Free;
    FreeMemEx(FTileGlobalInfo.lpBitmapInfo);
  end;
end;

function TlicgBILReader.GetAlphaChannel: byte;
begin
  Result := FAlphaChannel
end;

function TlicgBILReader.GetBufferBitmap: TBitmap;
begin
  Result := FBufferBitmap
end;

function TlicgBILReader.GetPainterObject: IlicgPainterObject;
begin
  Result := FPainterObject
end;

function TlicgBILReader.GetWasSuspended: Boolean;
begin
  Result := FWasSuspended
end;

procedure TlicgBILReader.SetBufferBitmap(const Value: TBitmap);
begin
  FBufferBitmap := Value
end;

procedure TlicgBILReader.SetPainterObject(const Value: IlicgPainterObject);
begin
  FPainterObject := Value
end;

{ TclNoneBitmapClass }

constructor TclNoneBitmapClass.Create(Index: integer);
var
  Resname: string; // li2016
  _i: integer;
begin

  inherited create;
  if index = 1 then
  begin
    Bitmap := Graphics.TBitmap.Create;
    Bitmap.Width := 8;
    Bitmap.Height := 8;
    Bitmap.PixelFormat := pf1bit;
    Bitmap.Canvas.Brush.Color := clBlack;
    Bitmap.Canvas.FillRect(Rect(0, 0, 8, 8));
  end
  else
  begin
    Resname := '#' + IntToStr(98 + Index);
    Bitmap := Graphics.TBitmap.Create;
    Bitmap.Handle := LoadBitmap(HInstance, PChar(Resname));
    ;
  end;

  GetDIBSizes(Bitmap.Handle, HeaderSize, BitsSize);
  BitmapInfo := GetMemEx(HeaderSize);
  Bits := GetMemEx(BitsSize);

  GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapInfo^, Bits^);
  with BitmapInfo^.bmiHeader do
  begin
    biClrUsed := 2;
    biClrImportant := 0;
  end;
  _i := 1;
  with BitmapInfo^.bmiColors[_i] do
  begin
    rgbRed := GetRValue(clNone);
    rgbGreen := GetGValue(clNone);
    rgbBlue := GetBValue(clNone);
  end;

end;

destructor TclNoneBitmapClass.Destroy;
begin
  FreeMemEx(BitmapInfo);
  FreeMemEx(Bits);
  Bitmap.free;
  inherited;
end;

end.


