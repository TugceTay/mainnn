//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Classes for backround map updates.
}

{$IFDEF DCC}
  unit GisBasemapHelper;
  {$HPPEMIT '#pragma link "GisBasemapHelper"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.NDK;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SyncObjs,
    System.Types,
    System.Generics.Defaults,
    System.Generics.Collections,
    System.Math,

    GisRtl,
    GisFunctions,
    GisInterfaces,
    GisLayer,
    GisTypes,
    GisTypesUI,
    GisViewer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type
  TGIS_BasemapHelperRequest = record
    Width         : Integer     ;
    Height        : Integer     ;
    Extent        : TGIS_Extent ;
    RotationPoint : TGIS_Point  ;
    RotationAngle : Double      ;
    RotationFast  : Boolean     ;
  end ;

  TGIS_BasemapHelper = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      criticalSection : TCriticalSection ;
      criticalSectionFast : TCriticalSection ;

      bBusy    : Boolean ;
      bDestroy : Boolean ;
      oBmpObj  : TObject ;// TGIS_ViewerBmp ;
      {$IFDEF DCC}
        [unsafe]
      {$ENDIF}
      oBmp     : IGIS_Viewer ;// TGIS_ViewerBmp ;

      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oWnd : IGIS_Viewer ;
      oLst : TStack<TGIS_BasemapHelperRequest> ;

      FBitmap : TGIS_Bitmap ;
      FExtent : TGIS_Extent ;
      {$IFNDEF JAVA}
        FHash : TGUID ;
      {$ENDIF}
      FRotationPoint : TGIS_Point ;
      FRotationAngle : Double ;

      iLastHash   : Int64 ;
      iLastCount  : Integer ;
      iFailTimout : Int64 ;

    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _wnd : IGIS_Viewer; const _bmp : TObject ) ;
    public
      property Bitmap : TGIS_Bitmap read FBitmap ;
      property Extent : TGIS_Extent read FExtent ;
      {$IFNDEF JAVA}
        property Hash : TGUID       read FHash ;
      {$ENDIF}
    public
      procedure Request      ( const _width          : Integer     ;
                               const _height         : Integer     ;
                               const _extent         : TGIS_Extent ;
                               const _rotation_point : TGIS_Point  ;
                               const _rotation_angle : Double
                             ) ; overload ;
      procedure Request      ( const _width          : Integer     ;
                               const _height         : Integer     ;
                               const _extent        : TGIS_Extent ;
                               const _rotation_point : TGIS_Point  ;
                               const _rotation_angle : Double      ;
                               const _rotation_fast  : Boolean
                             ) ; overload ;
      procedure Reset        ;

      procedure LockBitmap   ;
      procedure UnlockBitmap ;
      function  Active       : Boolean ;

      procedure WaitFor      ;

  end ;

const
 {#gendoc:hide}
 METADATA_BASEMAPFAILTIMEOUT
   = 'TGIS_Basemap.FailTimeout';

implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    GisInternals,
    GisLogger ;
{$ENDIF}

type
  T_threadBasemap = class( TThread )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oParent : TGIS_BasemapHelper ;
    protected

      // <summary>
      //  Execute tile loading tread.
      //  Read tile, attach data to the sublayer.
      // </summary>
      procedure Execute ; override;
  end ;


procedure T_threadBasemap.Execute ;
var
  i : Integer ;
  la : TGIS_Layer ;
  req : TGIS_BasemapHelperRequest ;
  bbase : Boolean ;
  mustinvalidate : Boolean ;
  bsuccess : Boolean ;
  subscribe : Boolean ;
begin
  try
    if GetTickCount < oParent.iFailTimout then
      exit ;

    oParent.bBusy := True ;

    oParent.oBmp.CS := oParent.oWnd.CS ;

    oParent.oBmp.CustomPPI := oParent.oWnd.PPI ;
    oParent.oBmp.RotationAngle := 0 ;

    oParent.criticalSectionFast.Enter ;
    try
      req := oParent.oLst.Pop ;
      oParent.oLst.Clear ;
    finally
      oParent.criticalSectionFast.Leave ;
    end;

    oParent.criticalSection.Enter ;
    try
      bbase := oParent.oWnd.IncrementalPaint ;
      for i:= 0 to oParent.oWnd.Items.Count -1 do begin
        la := TGIS_Layer( oParent.oWnd.Items[i] ) ;

        if bbase and la.Basemap then begin
          if la.Active then begin
            oParent.oBmp.Add( oParent.oWnd.Items[i] );
            oParent.oBmp.AttachLayer( la ) ;
            if la.IsVector then
              la.BasemapDraw := True ;
          end;
        end
        else
          break ;
      end ;
    finally
      oParent.criticalSection.Leave ;
    end;

    try
      oParent.oBmp.Color := TGIS_Color.None ;
      ( oParent.oBmp as IGIS_ViewerBmp).SetSize( req.Width, req.Height );
      oParent.oBmp.RecalcExtent ;
      if not req.RotationFast then begin
        oParent.oBmp.RotationPoint := oParent.oWnd.RotationPoint  ;
        oParent.oBmp.RotationAngle := oParent.oWnd.RotationAngle ;
      end;

      if not GisIsSameExtent(  oParent.oBmp.VisibleExtent, req.Extent ) then
        oParent.oBmp.VisibleExtent := req.Extent
      else
        (oParent.oBmp as IGIS_ViewerBmp).Draw ;

      bsuccess := True ;
      oParent.iFailTimout := 0 ;
    except
      on ex : Exception do begin
        // all fails should be silent
        oParent.iFailTimout :=
           GetTickCount +
           GisMetadataAsInteger( METADATA_BASEMAPFAILTIMEOUT, 5000 ) ;
        bsuccess := False ;
        TGIS_Logger.AsWarning( GetClassName( oParent ), ex.Message ) ;
      end;
    end;

    oParent.criticalSection.Enter ;
    try
      mustinvalidate := False ;

      if bsuccess then begin
        oParent.FExtent := oParent.oBmp.VisibleExtent ;
        {$IFNDEF JAVA}
          oParent.FHash := TGUID.NewGuid ;
        {$ENDIF}
        if not req.RotationFast then begin
          oParent.FRotationPoint := oParent.oBmp.RotationPoint ;
          oParent.FRotationAngle := oParent.oBmp.RotationAngle ;
        end;
        if ( not oParent.oBmp.IsEmpty )
           and
           ( not GisIsEmptyExtent( oParent.FExtent ) )
        then
          oParent.FBitmap.Assign( (oParent.oBmp as IGIS_ViewerBmp).GIS_Bitmap )
        else
        if oParent.FBitmap.Width > 0 then begin
          FreeObject( oParent.FBitmap );
          oParent.FBitmap := TGIS_Bitmap.Create ;
        end;

        mustinvalidate := ( oParent.FBitmap.Width > 0 )
                          or
                          ( oParent.iLastCount <> oParent.oBmp.Items.Count ) ;

        oParent.iLastCount := oParent.oBmp.Items.Count ;
      end ;

      subscribe := False ;
      for i := 0 to oParent.oBmp.Items.Count - 1 do begin
        oParent.oWnd.AttachLayer( oParent.oBmp.Items[i] ) ;

        if TGIS_Layer(oParent.oBmp.Items[i]).BasemapDraw then begin
          subscribe := True ;
          TGIS_Layer(oParent.oBmp.Items[i]).BasemapDraw := False ;
        end ;
      end ;
      oParent.oBmp.Close ;
    finally
      oParent.criticalSection.Leave ;
    end;

    if oParent.bDestroy then
      exit ;

    if subscribe then
      {$IFDEF OXYGENE}
        // synchronizing on the subscribers' side
        (oParent.oWnd as IGIS_Viewer).NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, nil ) ;
      {$ELSE}
        Synchronize(
          procedure
          begin
            (oParent.oWnd as IGIS_Viewer).NotifySubscribers( GIS_SUBSCRIBED_AFTERPAINT, nil ) ;
          end
        ) ;
      {$ENDIF}

    if oParent.bDestroy then
      exit ;

    if mustinvalidate then
      (oParent.oWnd as IGIS_ViewerParent).ControlRepaint ;

  finally
    oParent.bBusy := False ;
  end;
end;


constructor TGIS_BasemapHelper.Create(
  const _wnd : IGIS_Viewer;
  const _bmp : TObject
) ;
begin
  inherited Create ;

  oWnd := _wnd ;

  bDestroy := False ;

  oBmpObj := _bmp  ;
  {$IFDEF DCC}
    oBmp := TComponent(_bmp) as IGIS_Viewer ;
  {$ELSE}
    oBmp := _bmp as IGIS_Viewer ;
  {$ENDIF}

  oBmp.RestrictedDrag := False ;
  oBmp.Items.OwnsObjects := False ;

  FBitmap := TGIS_Bitmap.Create ;
  FExtent := GisNoWorld ;

  criticalSection := TCriticalSection.Create ;
  criticalSectionFast := TCriticalSection.Create ;
  oLst := TStack<TGIS_BasemapHelperRequest>.Create ;
end ;

procedure TGIS_BasemapHelper.doDestroy ;
begin
  bDestroy := True ;
  WaitFor ;
  FreeObject( oLst ) ;
  FreeObject( criticalSection ) ;
  FreeObject( criticalSectionFast ) ;
  oBmp := nil ;
  FreeObject( oBmpObj ) ;
  FreeObject( FBitmap ) ;

  inherited ;
end ;

procedure TGIS_BasemapHelper.Request(
  const _width          : Integer     ;
  const _height         : Integer     ;
  const _extent         : TGIS_Extent ;
  const _rotation_point : TGIS_Point  ;
  const _rotation_angle : Double
) ;
begin
  Request( _width, _height, _extent, _rotation_point, _rotation_angle, False ) ;
end;

procedure TGIS_BasemapHelper.Request(
  const _width          : Integer     ;
  const _height         : Integer     ;
  const _extent         : TGIS_Extent ;
  const _rotation_point : TGIS_Point  ;
  const _rotation_angle : Double      ;
  const _rotation_fast  : Boolean
) ;
var
  rextent    : TGIS_Extent ;
  i          : Integer ;
  la         : TGIS_Layer ;
  req        : TGIS_BasemapHelperRequest ;
  ilayercnt  : Integer ;
  bbase      : Boolean ;
  ex         : TGIS_Extent ;
  res_w      : Double ;
  res_h      : Double ;
  tilesize_w : Double ;
  tilesize_h : Double ;
  rtiles     : TRect ;
  th         : T_threadBasemap ;
  tol        : Double ;
  rot_dist   : Double ;

begin
  if GisIsEmptyExtent( _extent ) then
    exit ;

  if oWnd.IsEmpty then
    exit ;

  if not oWnd.IncrementalPaint then
    exit ;

  // ignore empty basemaps
  if ( not TGIS_Layer(oWnd.Items[0]).Basemap ) and ( iLastCount = 0 ) then
    exit ;

  if ( _width = 0 ) or ( _height = 0 ) then
    exit ;

  if oWnd.CS.EPSG > 0 then
    ex := oWnd.CS.ValidityExtent
  else
    ex := oWnd.Extent ;

  ilayercnt := 0;
  bbase := oWnd.IncrementalPaint ;
  for i:= 0 to oWnd.Items.Count -1 do begin
    la := TGIS_Layer( oWnd.Items[i] ) ;

    if bbase and la.Basemap then begin
      if la.Active then begin
        inc( ilayercnt ) ;
      end ;
    end
    else
      break ;
  end ;

  if ilayercnt = 0 then
    exit;


  rextent := _extent ;

  res_w := ( rextent.XMax - rextent.XMin ) / _width  ;
  res_h := ( rextent.YMax - rextent.YMin ) / _height ;

  tilesize_w := res_w * 256 ;
  tilesize_h := res_h * 256 ;


  if GisIsPointInsideExtent( _rotation_point, _extent ) then
    req.RotationFast := _rotation_fast
  else
    req.RotationFast := False ;

  if req.RotationFast then begin

    rot_dist :=
      GisPoint2Point( _rotation_point, GisPoint( rextent.XMin, rextent.YMin ) ) ;
    rot_dist :=
      Max( rot_dist,
           GisPoint2Point( _rotation_point, GisPoint( rextent.XMin, rextent.YMax ) )
         ) ;
    rot_dist :=
      Max( rot_dist,
           GisPoint2Point( _rotation_point, GisPoint( rextent.XMax, rextent.YMax ) )
         ) ;
    rot_dist :=
      Max( rot_dist,
           GisPoint2Point( _rotation_point, GisPoint( rextent.XMax, rextent.YMin ) )
         ) ;

    rextent.XMin := _rotation_point.X - rot_dist ;
    rextent.XMax := _rotation_point.X + rot_dist ;
    rextent.YMin := _rotation_point.Y - rot_dist ;
    rextent.YMax := _rotation_point.Y + rot_dist ;
  end;


  rtiles := Rect(
              TruncS(  ( rextent.XMin - ex.XMin - tilesize_w ) / tilesize_w ),
              TruncS( -( rextent.YMax - ex.YMax + tilesize_h ) / tilesize_h ),
              TruncS(  ( rextent.XMax - ex.XMin + tilesize_w ) / tilesize_w ),
              TruncS( -( rextent.YMin - ex.YMax - tilesize_h ) / tilesize_h )
            ) ;


  req.Extent := GisExtent(
                  ex.XMin + rtiles.Left   * tilesize_w,
                  ex.YMax - rtiles.Bottom * tilesize_h,
                  ex.XMin + rtiles.Right  * tilesize_w,
                  ex.YMax - rtiles.Top    * tilesize_h
                ) ;


  if rextent.XMin <= req.Extent.XMin then
    rtiles := Rect(rtiles.Left - 1, rtiles.Top, rtiles.Right, rtiles.Bottom ) ;
  if rextent.YMin <= req.Extent.YMin then
    rtiles :=  Rect( rtiles.Left, rtiles.Top, rtiles.Right, rtiles.Bottom + 1 ) ;
  if rextent.XMax >= req.Extent.XMax then
    rtiles := Rect( rtiles.Left, rtiles.Top, rtiles.Right + 1, rtiles.Bottom ) ;
  if rextent.YMax >= req.Extent.YMax  then
    rtiles := Rect( rtiles.Left, rtiles.Top-1, rtiles.Right, rtiles.Bottom ) ;

  req.Extent := GisExtent(
                  ex.XMin + rtiles.Left   * tilesize_w,
                  ex.YMax - rtiles.Bottom * tilesize_h,
                  ex.XMin + rtiles.Right  * tilesize_w,
                  ex.YMax - rtiles.Top    * tilesize_h
                ) ;

  if ( rextent.XMin - req.Extent.XMin ) < tilesize_w / 2 then
    rtiles := Rect( rtiles.Left - 1, rtiles.Top, rtiles.Right, rtiles.Bottom ) ;
  if ( rextent.YMin - req.Extent.YMin ) < tilesize_h / 2 then
    rtiles := Rect( rtiles.Left, rtiles.Top, rtiles.Right, rtiles.Bottom + 1  ) ;
  if ( req.Extent.XMax - rextent.XMax  ) < tilesize_w / 2 then
    rtiles := Rect( rtiles.Left, rtiles.Top, rtiles.Right + 1, rtiles.Bottom ) ;
  if ( req.Extent.YMax - rextent.YMax  ) < tilesize_h / 2 then
    rtiles := Rect( rtiles.Left, rtiles.Top -1, rtiles.Right, rtiles.Bottom ) ;

  req.Extent := GisExtent(
                  ex.XMin + rtiles.Left   * tilesize_w,
                  ex.YMax - rtiles.Bottom * tilesize_h,
                  ex.XMin + rtiles.Right  * tilesize_w,
                  ex.YMax - rtiles.Top    * tilesize_h
                ) ;

  assert( req.Extent.XMax > req.Extent.XMin ) ;
  assert( req.Extent.YMax > req.Extent.YMin ) ;

  req.Width  := ( rtiles.Right  - rtiles.Left ) * 256 ;
  assert( req.Width > 0 ) ;

  req.Height := ( rtiles.Bottom - rtiles.Top  ) * 256 ;
  assert( req.Height > 0 ) ;

  if req.RotationFast then begin
    req.RotationPoint.X := 0 ;
    req.RotationPoint.Y := 0 ;
    req.RotationAngle := 0 ;
  end
  else begin
    req.RotationPoint := _rotation_point ;
    req.RotationAngle := _rotation_angle ;
  end;

  tol := ( req.Extent.XMax - req.Extent.XMin ) / req.Width / 10 ; // 1/10 of pixels size

  if ( req.Width = FBitmap.Width )
     and
     ( req.Height = FBitmap.Height )
     and
     GisIsSameExtent( req.Extent, FExtent, tol )
     and
     GisIsSamePoint( req.RotationPoint, FRotationPoint )
     and
     ( req.RotationAngle = FRotationAngle )
     and
     ( iLastHash = oWnd.ChangeHash )
     and
     ( iLastCount = ilayercnt )
  then
    exit ;

  iLastHash := oWnd.ChangeHash ;

  criticalSectionFast.Enter ;
    oLst.Clear ;
    oLst.Push( req );
  criticalSectionFast.Leave ;

  if bBusy then exit ;

  bBusy := True ;

  th := T_threadBasemap.Create( True ) ;
  th.oParent := self ;
  th.FreeOnTerminate := True ;
  th.Start ;
end ;

procedure TGIS_BasemapHelper.Reset ;
begin
  criticalSection.Enter ;
  try
    if oLst.Count > 0 then begin
      exit ; // penning redraws will u[pdate basemap anyway
    end
    else begin
      iLastHash := 0 ;
      Request( FBitmap.Width, FBitmap.Height,
               FExtent,
               FRotationPoint, FRotationAngle,
               False
             ) ;
    end;
  finally
    criticalSection.Leave ;
  end;
end;

procedure TGIS_BasemapHelper.LockBitmap  ;
begin
  criticalSection.Enter ;
end;

procedure TGIS_BasemapHelper.UnlockBitmap  ;
begin
  criticalSection.Leave ;
end;

function TGIS_BasemapHelper.Active
  : Boolean ;
var
  i     : Integer ;
  bbase : Boolean ;
  la    : TGIS_Layer ;
begin
  Result := False ;
  criticalSection.Enter ;
  try
    if GisIsEmptyExtent( FExtent ) then
      exit ;

    bbase := oWnd.IncrementalPaint ;
    for i:= 0 to oWnd.Items.Count -1 do begin
      la := TGIS_Layer( oWnd.Items[i] ) ;

      if bbase and la.Basemap then begin
        if la.Active then begin
          Result := True ;
          break ;
        end;
      end
      else
        break ;
    end ;
  finally
    criticalSection.Leave ;
  end;
end;

procedure TGIS_BasemapHelper.WaitFor ;
begin
  while bBusy do begin
    {$IFDEF DCC}
      TThread.Sleep( 10 )
    {$ELSE}
      Sleep( 10 );
    {$ENDIF}
  end ;
end;

end.
