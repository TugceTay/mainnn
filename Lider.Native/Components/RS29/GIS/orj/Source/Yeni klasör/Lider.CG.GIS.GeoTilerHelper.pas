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
  Classes for background map updates.
}

{$IFDEF DCC}
  unit GisTilerHelper ;
  {$HPPEMIT '#pragma link "GisTilerHelper"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF DCC}
uses
  System.Math,
  System.Classes,
  System.Types,
  System.SysUtils,
  System.SyncObjs,
  System.IOUtils,
  System.Generics.Collections,

  {$IFDEF MSWINDOWS}
    Winapi.ActiveX,
  {$ENDIF}

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisFunctions,
  GisRendererAbstract,
  GisTypesUI,
  GisLayer,
  GisLayerVector;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}

type
  {#gendoc:hide}
  TGIS_ViewerBmpFactory = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    public
      function CreateViewer : TObject ; virtual; abstract ;
  end;

  {#gendoc:hide}
  TGIS_TilerState = {$IFDEF OXYGENE} public {$ENDIF} ( Repaint, UpdateAll, UpdateLabels ) ;

  {#gendoc:hide}
  TGIS_TilerRequest = record
    Mode    : Integer ;
    Scale   : Double  ;
    XPos    : Int64   ;
    YPos    : Int64   ;
    Segment : Integer ;
    Extent  : TGIS_Extent ;
    Width   : Integer ;
    Height  : Integer ;
    Primary : Boolean ;
  end;

  {#gendoc:hide}
  TGIS_TilerScope = record
    Extent : TGIS_Extent  ;
    Scale  : Double ;
    Left   : Int64  ;
    Top    : Int64  ;
    Right  : Int64  ;
    Bottom : Int64  ;
  end;

  {#gendoc:hide}
  TGIS_TilerSubtile = class( TGIS_ObjectDisposable )
    private
      criticalSection : TCriticalSection;
      bObsolate : Boolean ;
      oBitmap : TGIS_Bitmap;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;
      procedure Lock ;
      procedure Unlock ;
    public
      property Obsolate : Boolean read bObsolate;
      property Bitmap   : TGIS_Bitmap read oBitmap;
  end;

  {#gendoc:hide}
  TGIS_TilerTile = class( TGIS_ObjectDisposable )
    private
      criticalSection : TCriticalSection;
      iLock : Integer ;

    public
      Extent   : TGIS_Extent ;
      Scale    : Double ;
      XPos     : Int64 ;
      YPos     : Int64 ;
      Subtiles : array of TGIS_TilerSubtile ;
      Older    : TGIS_TilerTile ; //LRU
      Newer    : TGIS_TilerTile ; //LRU
      Count    : Integer ;

    protected
      procedure doDestroy; override ;
    public
      constructor Create ;
      procedure Lock ;
      procedure Unlock ;
  end;

  {#gendoc:hide}
  TGIS_TilerContext = class( TGIS_ObjectDisposable )
    private
      oContext : TGIS_RendererContext ;
      rExtent  : TGIS_Extent  ;
      bInUse   : Boolean ;

    protected
      procedure doDestroy ; override;

    public
      property Extent   : TGIS_Extent
                         read rExtent ;
      property Context : TGIS_RendererContext
                         read oContext ;
  end;

  {#gendoc:hide}
  TGIS_TilerHelper = class( TGIS_ObjectDisposable )
    public
      TileSize       : Integer ;
      ThreadCount    : Integer ;
      CacheSize      : Integer ;
      CurrentContext : TGIS_TilerContext ;
      CachedBmp      : TGIS_Bitmap ;
    private
      criticalSection     : TCriticalSection ;
      criticalSectionTile : TCriticalSection ;
      oViewer             : IGIS_Viewer ;
      oDictionary         : TDictionary< String, TGIS_TilerTile> ;
      oRequestList        : TList<TGIS_TilerRequest > ;
      iReparentCount      : Integer ;
      bPendingLabels      : Boolean ;
      iLockExtraPaint     : Integer ;
      oContextList        : TList<TGIS_TilerContext> ;
      iActiveCacheSize    : Integer ;

      oNewest             : TGIS_TilerTile ; //LRU
      oOldest             : TGIS_TilerTile ; //LRU

      arThreads           : array of TObject ;

      bMustClearAll       : Boolean ;
      bMustClearCache     : Boolean ;
      oTilesExtent        : TGIS_Extent ;
      dTilesBaseRes       : Double ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create         ( const _wnd      : IGIS_Viewer;
                                   const _factory  : TGIS_ViewerBmpFactory ;
                                   const _threads  : Cardinal
                                 );

    private
      function  deleteTileLRU     : Boolean ;
      procedure doClearAll        ;
      procedure doClearCache      ;
    public
      procedure ClearAll          ;

      procedure ClearCache        ;
      function  TideUp            : Boolean ;

      function  GetContext        : TGIS_TilerContext ;

      procedure ReleaseContext    ;

      function  LockTile          ( const _scale    : Double;
                                    const _xpos     : Int64 ;
                                    const _ypos     : Int64 ;
                                    const _forced   : Boolean = False
                                  ) : TGIS_TilerTile ;

      procedure GetSegment        ( const _segment  : Integer;
                                    var   _start    : Integer;
                                    var   _stop     : Integer
                                  ) ;

      function  GetScope          ( const _extent   : TGIS_Extent;
                                    const _limited  : TGIS_Extent;
                                    const _width    : Integer;
                                    const _height   : Integer
                                  ) : TGIS_TilerScope ;

      procedure Request           ( const _extent   : TGIS_Extent;
                                    const _width    : Integer;
                                    const _height   : Integer
                                  );

      procedure RequestLabels     ( const _extent   : TGIS_Extent;
                                    const _width    : Integer;
                                    const _height   : Integer
                                  );

      procedure RequestRepaint    ;

      function  Suspend           ( const _forced   : Boolean = False
                                  ) : Boolean ;
      procedure &Continue         ;
      function UpdateExtent       : Boolean ;
  end;


//##############################################################################
implementation


type
 T_TilerExecutor = class( TThread )
   public
     Parent : TGIS_TilerHelper ;
     Segment : Integer ;
   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
    oBmpObj : TObject ;// TGIS_ViewerBmp ;
      {$IFDEF DCC}
       [unsafe]
     {$ENDIF}
     oBmp    : IGIS_Viewer ;// TGIS_ViewerBmp ;

     bRunning : Boolean ;

   public
   {$IFDEF CLR}
     procedure Destroy ;
   {$ELSE}
     destructor Destroy ; override ;
   {$ENDIF}
   protected
     procedure Execute; override;
 end;

{$REGION 'T_TilerExecutor'}

{$IFDEF CLR}
procedure T_TilerExecutor.Destroy ;
{$ELSE}
destructor T_TilerExecutor.Destroy ;
{$ENDIF}
begin
  oBmp := nil ;
  FreeObject( oBmpObj ) ;
  {$IFNDEF CLR}
  inherited;
  {$ENDIF}
end;

procedure T_TilerExecutor.Execute;
var
  i : Integer ;
  req : TGIS_TilerRequest ;
  tile : TGIS_TilerTile ;
  lstart : Integer ;
  lstop : Integer ;
  la : TGIS_Layer ;
  la_extra : TGIS_LayerVector ;
  vwr : TObject ;
  ext : TGIS_Extent ;
  bmp : TGIS_Bitmap;
  istart : Integer ;
  istop : Integer ;
  {$IFDEF CLR}
  rnd : Random ;
  {$ENDIF}
  ilastcached : Integer;
  context : TGIS_TilerContext ;
  binuse : Boolean ;
begin
  bRunning := True ;
  {$IFDEF CLR}
    rnd := Random.Create ;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  {$IFDEF DCC}
    CoInitialize(nil);
  {$ENDIF}
  {$ENDIF}
  try
    while not Terminated do begin

      Parent.criticalSection.Enter ;

      if ( Parent.iLockExtraPaint > 0 ) or Parent.bPendingLabels then begin
        // nothing to do ;
        Parent.criticalSection.Leave ;
        Sleep(100);
        continue ;
      end;

      // find any matching request
      req.Segment := -1 ;

      istop := -1 ;
      for i := 0 to Parent.oRequestList.Count -1 do begin
        if not Parent.oRequestList[i].Primary then
          break ;
        istop := i ;
      end;

      if istop < 0  then
        istop := Parent.oRequestList.Count -1;

      if istop < 0 then
        istart := 0
      else
        {$IFDEF CLR}
        istart := rnd.Next( istop ) div 2 ;
        {$ELSE}
        istart := Random( istop ) div  2 ;
        {$ENDIF}

      for i := istart to istop do begin
        if Parent.oRequestList[i].Segment = Segment then begin
          req := Parent.oRequestList[i] ;
          {$IFDEF CLR}
          Parent.oRequestList.RemoveAt( i );
          {$ELSE}
          Parent.oRequestList.Delete( i );
          {$ENDIF}
          break ;
        end;
      end;
      Parent.criticalSection.Leave ;
      // unlock requets


      if req.Segment < 0 then begin
        // nothing to do ;
        Sleep(100);
        continue ;
      end;

      if req.Mode = 0 then begin
        // get a tile or create a new one
        tile := Parent.LockTile( req.Scale, req.XPos, req.YPos, True ) ;
        try
          // if segment is occupied it means is alread done -- do nothing
          if ( tile.Subtiles[ Segment ].Bitmap <> nil ) and not ( tile.Subtiles[ Segment ].Obsolate ) then begin
            continue ;
          end;

          // draw a map
          Parent.criticalSection.Enter ;
          try
        //?    T_TilerExecutor( Parent.othd0 ).oBmp.Interrupt;

            Parent.GetSegment( Segment, lstart, lstop ) ;

            if lstop < lstart then begin
              tile.Count := Min( tile.Count, Segment ) ;
              continue ;
            end;

            ext := tile.Extent ;

            oBmp.CS := Parent.oViewer.CS ;
            oBmp.CustomPPI := Parent.oViewer.PPI ;

            ilastcached := -1 ;
            for i := 0 to Parent.oViewer.Items.Count -1  do begin
              if TGIS_Layer( Parent.oViewer.Items[i] ).CachedPaint then begin
                ilastcached := i ;
              end;
            end;

            vwr := Parent.oViewer as TObject ;
            for i:=  lstart to Min( lstop, ilastcached )  do begin
              la := TGIS_Layer( Parent.oViewer.Items[i] ) ;
              if ( la.Viewer.Ref.ViewerParent as TObject ) <> vwr then
                continue ;

              oBmp.Add( Parent.oViewer.Items[i] );
              oBmp.AttachLayer( la ) ;
              la.TiledDrawMode := TGIS_LayerTiledMode.Tiles ;
            end ;

            if oBmp.Items.Count = 0 then
              continue ;

            // add layer just to quarantee consistent extent
            la_extra := TGIS_LayerVector.Create ;
            la_extra.CS := oBmp.CS ;
            la_extra.Extent := Parent.oViewer.Extent ;
            oBmp.Add( la_extra ) ;

            inc( Parent.iReparentCount ) ;
          finally
            Parent.criticalSection.Leave ;
          end;

          try
            oBmp.Color := TGIS_Color.None ;
            ( oBmp as IGIS_ViewerBmp).SetSize( Parent.TileSize, Parent.TileSize );

            oBmp.RecalcExtent ;
            oBmp.Delete( la_extra.Name ) ;
            FreeObject( la_extra ) ;

            oBmp.OverlappedExtentMargin := 32 ;

            if not GisIsSameExtent(  oBmp.VisibleExtent, ext ) then
              oBmp.VisibleExtent := ext
            else
              (oBmp as IGIS_ViewerBmp).Draw ;
          except
          end;

          Parent.criticalSection.Enter ;
          try
            if ( not oBmp.IsEmpty ) then begin
              tile.Subtiles[ req.Segment ].Lock;
              try
                try
                  FreeObject( tile.Subtiles[ req.Segment ].oBitmap ) ;
                  bmp := TGIS_Bitmap.Create ;
                  bmp.Assign( (oBmp as IGIS_ViewerBmp).GIS_Bitmap );

                  tile.Subtiles[ req.Segment ].oBitmap := bmp ;
                  tile.Subtiles[ req.Segment ].bObsolate := False ;

                except
                  FreeObject( tile.Subtiles[ req.Segment ].oBitmap ) ;
                end;
              finally
                tile.Subtiles[ req.Segment ].Unlock;
              end;
            end;

            for i := 0 to oBmp.Items.Count - 1 do begin
              la :=  TGIS_Layer( oBmp.Items[i] ) ;
              Parent.oViewer.AttachLayer( la ) ;
              la.TiledDrawMode := TGIS_LayerTiledMode.None ;
            end ;
            oBmp.Close ;
            dec( Parent.iReparentCount ) ;
          finally
            Parent.criticalSection.Leave ;
          end;
        finally
          tile.Unlock ;
        end;
      end
      else
      if req.Mode = 1 then begin
        // draw a map
        Parent.criticalSection.Enter ;
        try
          ext := req.Extent ;

          oBmp.CS := Parent.oViewer.CS ;
          oBmp.CustomPPI := Parent.oViewer.PPI ;

          vwr := Parent.oViewer as TObject ;

          binuse := False ;
          for i:=0 to Parent.oViewer.Items.Count -1  do begin
            la := TGIS_Layer( Parent.oViewer.Items[i] ) ;
            if not la.CachedPaint then
              continue ;

            if ( la.Viewer.Ref.ViewerParent as TObject ) <> vwr then begin
              Parent.oRequestList.Add( req );
              binuse := True ;
              break;
            end;
          end ;

          if binuse then
            continue ;

          for i:=0 to Parent.oViewer.Items.Count -1  do begin
            la := TGIS_Layer( Parent.oViewer.Items[i] ) ;
            if not la.CachedPaint then
              continue ;

            oBmp.Add( Parent.oViewer.Items[i] );
            oBmp.AttachLayer( la ) ;
            la.TiledDrawMode := TGIS_LayerTiledMode.Complete ;
          end ;

          if oBmp.Items.Count = 0 then
            continue ;

          // add layer just to quarantee consistent extent
          la_extra := TGIS_LayerVector.Create ;
          la_extra.CS := oBmp.CS ;
          la_extra.Extent := Parent.oViewer.Extent ;
          oBmp.Add( la_extra ) ;

          inc( Parent.iReparentCount ) ;
          Parent.bPendingLabels := True ;
        finally
          Parent.criticalSection.Leave ;
        end;

        try
          ( oBmp as IGIS_ViewerBmp).SetSize( req.Width, req.Height );
          oBmp.Color := TGIS_Color.None ;
          ( oBmp as IGIS_ViewerBmp).fset_KeepContextInternal( True )  ;

          oBmp.RecalcExtent ;
          oBmp.Delete( la_extra.Name ) ;
          FreeObject( la_extra ) ;

          if not GisIsSameExtent(  oBmp.VisibleExtent, ext ) then
            oBmp.VisibleExtent := ext
          else
            (oBmp as IGIS_ViewerBmp).Draw() ;
        except
        end;

        Parent.criticalSection.Enter ;
        try
          if ( not oBmp.IsEmpty ) then begin
            try
              bmp := TGIS_Bitmap.Create ;
              bmp.Assign( (oBmp as IGIS_ViewerBmp).GIS_Bitmap );
              FreeObject(Parent.CachedBmp);
              Parent.CachedBmp := bmp ;

              context := TGIS_TilerContext.Create ;
              context.oContext := TGIS_RendererContext(
                                    (oBmp as IGIS_ViewerBmp).fget_ContextInternal
                                  ) ;
              context.rExtent := ext ;
            except
            end;
          end;

          for i := 0 to oBmp.Items.Count - 1 do begin
            la :=  TGIS_Layer( oBmp.Items[i] ) ;
            Parent.oViewer.AttachLayer( la ) ;
            la.TiledDrawMode := TGIS_LayerTiledMode.None ;
          end ;
          oBmp.Close ;
          dec( Parent.iReparentCount ) ;

          for i:=Parent.oContextList.Count -1 downto 0 do begin
            if Parent.oContextList[i].bInUse then
              continue;

            FreeObjectNotNil( Parent.oContextList[i] ) ;
            {$IFDEF CLR}
            Parent.oContextList.RemoveAt(i)
            {$ELSE}
            Parent.oContextList.Delete(i)
            {$ENDIF}
          end;
          Parent.oContextList.Add( context );

          Parent.bPendingLabels := False ;
        finally
          Parent.criticalSection.Leave ;
        end;


      end
      else
      if req.Mode = 2 then begin
        // do nothing - just force repaint
      end;

      {$IFDEF CLR}
        (Parent.oViewer as IGIS_ViewerParent).ControlRepaint ;
      {$ELSE}
      System.Classes.TThread.Synchronize(System.Classes.TThread.CurrentThread,
        procedure()
        begin
          (Parent.oViewer as IGIS_ViewerParent).ControlRepaint ;
        end
      ) ;
      {$ENDIF}

    end;
  finally
    {$IFDEF MSWINDOWS}
    {$IFDEF DCC}
      CoUninitialize;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF CLR}
      FreeObject( rnd ) ;
    {$ENDIF}
    bRunning := False ;
  end;


end;

{$ENDREGION 'TGIS_TilerExecutor'}

{$REGION 'TGIS_TilerContext'}

procedure TGIS_TilerContext.doDestroy;
begin
  FreeObject( oContext ) ;
  inherited;
end;

{$ENDREGION 'TGIS_TilerContext'}

{$REGION 'TGIS_TilerHelper'}

constructor TGIS_TilerHelper.Create(
  const _wnd      : IGIS_Viewer;
  const _factory  : TGIS_ViewerBmpFactory ;
  const _threads  : Cardinal
) ;
var
  i : Integer ;
  bmp : TObject ;
const
  TILE_SIZE = 512;
  CACHE_SIZE = 384;
begin
  TileSize := TILE_SIZE;
  ThreadCount := _threads;

  CacheSize := ( CACHE_SIZE*1024*1024 )
               div ( TileSize * TileSize * ThreadCount * 4 );
  iActiveCacheSize := CacheSize;


  bMustClearAll := False;
  bMustClearCache := False;
  dTilesBaseRes := 0 ;
  oTilesExtent := GisNoWorld ;

  oViewer := _wnd ;

  iReparentCount := 0 ;
  iLockExtraPaint := 0 ;
  bPendingLabels := False ;

  oDictionary := TDictionary< String, TGIS_TilerTile>.Create ;
  oRequestList := TList<TGIS_TilerRequest >.Create ;
  oContextList := TList<TGIS_TilerContext >.Create ;

  oNewest := nil ;
  oOldest := nil ;

  criticalSection := TCriticalSection.Create ;
  criticalSectionTile := TCriticalSection.Create ;

  SetLength( arThreads, _threads + 1  );

  for i := 0 to high( arThreads ) do begin
    bmp := _factory.CreateViewer ;
    arThreads[i] := T_TilerExecutor.Create( True ) ;
    T_TilerExecutor(arThreads[i]).oBmpObj := bmp ;
    {$IFDEF DCC}
      T_TilerExecutor(arThreads[i]).oBmp := TComponent(bmp) as IGIS_Viewer ;
    {$ELSE}
      T_TilerExecutor(arThreads[i]).oBmp := bmp as IGIS_Viewer ;
    {$ENDIF}
    T_TilerExecutor(arThreads[i]).oBmp.MasterViewer := _wnd ;
    T_TilerExecutor(arThreads[i]).oBmp.RestrictedDrag := False ;
    T_TilerExecutor(arThreads[i]).oBmp.Items.OwnsObjects := False ;
    T_TilerExecutor(arThreads[i]).Parent :=self ;

    if i = 0 then
      T_TilerExecutor(arThreads[i]).Segment := 999 // thread for labeling
    else
      T_TilerExecutor(arThreads[i]).Segment := i-1 ;

  end;

  UpdateExtent ;

  for i := 0 to high( arThreads ) do
    {$IFDEF CLR}
    T_TilerExecutor(arThreads[i]).Start ;
    {$ELSE}
    T_TilerExecutor(arThreads[i]).Resume ;
    {$ENDIF}
end;

procedure TGIS_TilerHelper.doDestroy;
var
  i : Integer ;
begin
  for i := 0 to high( arThreads ) do
    T_TilerExecutor(arThreads[i]).Terminate ;

   for i := 0 to high( arThreads ) do begin
     while( T_TilerExecutor(arThreads[i]).bRunning ) do begin
       (oViewer as IGIS_ViewerParent).ControlProcessMessages ;
     end;
   end;

  for i := 0 to high( arThreads ) do begin
    {$IFDEF CLR}
      // it is not a destructor, the class is not disposable
      // it is a procedure that frees variables
      T_TilerExecutor(arThreads[i]).Destroy ;
    {$ENDIF}
    FreeObject( arThreads[i] ) ;
  end ;

  doClearAll ;
  FreeObject( CachedBmp ) ;
  FreeObject( oDictionary ) ;
  FreeObject( oRequestList ) ;

  for i:= oContextList.Count -1 downto 0 do begin
    FreeObjectNotNil( oContextList[i] ) ;
    {$IFDEF CLR}
    oContextList.RemoveAt(i)
    {$ELSE}
    oContextList.Delete(i)
    {$ENDIF}
  end;
  FreeObject( oContextList ) ;
  FreeObject( criticalSection ) ;
  FreeObject( criticalSectionTile ) ;

  inherited;
end;

procedure TGIS_TilerHelper.ClearAll ;
begin
  bMustClearAll := True ;
end;

procedure TGIS_TilerHelper.ClearCache ;
begin
  bMustClearCache := True ;
end;

function TGIS_TilerHelper.TideUp
  : Boolean ;
begin
  Result := bMustClearAll or bMustClearCache ;
  if bMustClearAll then begin
    Suspend( True );
    doClearAll ;
    &Continue ;
  end;
  if bMustClearCache then begin
    doClearCache ;
  end;
end;

function TGIS_TilerHelper.deleteTileLRU
  : Boolean ;
var
  key : String ;
  itm : TGIS_TilerTile ;
begin
  Result := False ;
  criticalSectionTile.Enter ;
  try
    if oDictionary.Count = 0 then
      exit ;

    itm := oOldest ;

    if not assigned( itm ) then
      exit ;

    if itm.iLock > 0 then begin
      exit ;
    end;

    oOldest := itm.Newer ;
    if assigned( oOldest ) then
      oOldest.Older := nil ;

    key := Format( '%.4e:%d:%d', [ itm.Scale, itm.XPos, itm.YPos ]) ;

    FreeObject( itm ) ;
    oDictionary.Remove( key ) ;
    Result := True ;
  finally
    criticalSectionTile.Leave ;
  end;
end;

procedure TGIS_TilerHelper.GetSegment(
  const _segment : Integer ;
  var   _start   : Integer ;
  var   _stop    : Integer
) ;
var
  i : Integer ;
  size : Integer ;
  ilastcached : Integer ;
begin
  ilastcached := -1 ;
  for i := 0 to oViewer.Items.Count -1  do begin
    if TGIS_Layer( oViewer.Items.Items[i] ).CachedPaint then
      ilastcached := i ;
  end;

  size := Max( 1, ilastcached div ThreadCount ) ;

  _start := _segment * size ;

  if _segment = ThreadCount -1 then
    _stop :=  ilastcached
  else
    _stop :=  Min( _segment * size + size -1, ilastcached ) ;
end;

procedure TGIS_TilerHelper.doClearAll ;
begin
  criticalSectionTile.Enter ;
  try
    while oDictionary.Count > 0 do
      if not deleteTileLRU then
        break;
    oNewest := nil ;
    oOldest := nil ;
  finally
    bMustClearAll := False ;
    criticalSectionTile.Leave ;
  end;
end;

procedure TGIS_TilerHelper.doClearCache ;
var
  i : Integer;
  otmp : TGIS_TilerTile ;
begin
  criticalSectionTile.Enter ;
  try
    otmp := oNewest ;
    while assigned( otmp ) do begin
      for i := low( otmp.Subtiles ) to high( otmp.Subtiles ) do begin
        otmp.Subtiles[i].bObsolate := True ;
        otmp.Count := ThreadCount ;
      end;
      otmp := otmp.Older ;
    end ;
  finally
    bMustClearCache := False ;
    criticalSectionTile.Leave ;
  end;
end;


function TGIS_TilerHelper.GetContext
 : TGIS_TilerContext ;
var
  i : Integer ;
begin
 criticalSection.Enter ;
 try
   for i := oContextList.Count -2 downto 0 do begin
     if oContextList[i].bInUse then
       continue ;

     FreeObjectNotNil( oContextList[i] ) ;
     {$IFDEF CLR}
     oContextList.RemoveAt(i) ;
     {$ELSE}
     oContextList.Delete(i) ;
     {$ENDIF}
   end;
   assert( oContextList.Count <=1 ) ;
   if oContextList.Count > 0 then begin
     CurrentContext := oContextList[oContextList.Count-1] ;
     CurrentContext.bInUse := True ;
     Result := CurrentContext ;
   end
   else begin
     Result := nil ;
   end;
 finally
   criticalSection.Leave ;
 end ;
end;

procedure TGIS_TilerHelper.ReleaseContext ;
begin
 criticalSection.Enter ;
 try
   if assigned( CurrentContext ) then
     CurrentContext.bInUse := false ;

 finally
   criticalSection.Leave ;
 end ;
end ;

function TGIS_TilerHelper.LockTile(
  const _scale  : Double ;
  const _xpos   : Int64  ;
  const _ypos   : Int64  ;
  const _forced : Boolean
) : TGIS_TilerTile;
var
  i   : Integer ;
  key : String ;
  itm : TGIS_TilerTile ;
  dsc : Double ;
begin
  criticalSectionTile.Enter ;
  try
    Result := nil ;

    key := Format( '%.4e:%d:%d', [_scale, _xpos, _ypos ]) ;

    if not oDictionary.TryGetValue( key, itm ) then begin
      if not _forced then
       exit ;

      itm := TGIS_TilerTile.Create ;
      itm.Scale := _scale ;
      itm.XPos := _xpos ;
      itm.YPos := _ypos ;
      dsc := TileSize *_scale ;
      itm.Extent.XMin := oTilesExtent.XMin + dsc * ( _xpos     ) ;
      itm.Extent.YMin := oTilesExtent.YMax - dsc * ( _ypos + 1 ) ;
      itm.Extent.XMax := oTilesExtent.XMin + dsc * ( _xpos + 1 ) ;
      itm.Extent.YMax := oTilesExtent.YMax - dsc * ( _ypos     ) ;

      itm.Newer := nil ;
      if oNewest = nil then
        oNewest := itm
      else begin
        oNewest.Newer := itm ;
        itm.Older := oNewest ;
        oNewest := itm ;
      end;
      if oOldest = nil then
        oOldest := oNewest ;

      SetLength( itm.Subtiles, ThreadCount ) ;

      for i:= low( itm.Subtiles ) to high( itm.Subtiles ) do
        itm.Subtiles[i] := TGIS_TilerSubtile.Create ;

      itm.Count := ThreadCount ;
      oDictionary.Add( key, itm ) ;

      while oDictionary.Count > iActiveCacheSize do
        if not deleteTileLRU then
          break ;
    end
    else begin
      if itm <> oNewest then begin
        itm.Newer.Older := itm.Older ;
        if assigned( itm.Older ) then
          itm.Older.Newer := itm.Newer
        else
          oOldest := itm.Newer ;

        oNewest.Newer := itm;
        itm.Newer := nil ;
        itm.Older := oNewest ;
        oNewest := itm ;

      end;
    end;


    itm.Lock;
    Result := itm ;
  finally
    criticalSectionTile.Leave ;
  end;
end;

function TGIS_TilerHelper.GetScope(
  const _extent : TGIS_Extent ;
  const _limited: TGIS_Extent ;
  const _width  : Integer     ;
  const _height : Integer
) : TGIS_TilerScope ;
var
  res_w,
  res_h      : Double ;
  tile_w,
  tile_h     : Double ;
  left,
  right,
  top,
  bottom     : Int64 ;
  left_tmp,
  top_tmp,
  right_tmp,
  bottom_tmp : Int64 ;
begin
  Result.Extent := _extent ;

  if GisIsNoWorld( _extent ) then begin
    Result.Scale  :=  1 ;
    Result.Left   :=  0 ;
    Result.Right  := -1 ;
    Result.Top    :=  0 ;
    Result.Bottom := -1 ;
    exit ;
  end ;

  res_w := (_extent.XMax - _extent.XMin ) / _width ;
  res_h := (_extent.YMax - _extent.YMin ) / _height ;

  tile_w := res_w * TileSize ;
  tile_h := res_w * TileSize ;

  left   := TruncS( ( _extent.XMin - oTilesExtent.XMin {- tile_w /2} ) / tile_w ) ;
  right  := TruncS( ( _extent.XMax - oTilesExtent.XMin {+ tile_w /2} ) / tile_w ) ;
  top    := TruncS( ( oTilesExtent.YMax - _extent.YMax {- tile_h /2} ) / tile_h ) ;
  bottom := TruncS( ( oTilesExtent.YMax - _extent.YMin {+ tile_h /2} ) / tile_h ) ;

  left_tmp   := TruncS( ( _limited.XMin - oTilesExtent.XMin {- tile_w /2} ) / tile_w ) ;
  right_tmp  := TruncS( ( _limited.XMax - oTilesExtent.XMin {+ tile_w /2} ) / tile_w ) ;
  top_tmp    := TruncS( ( oTilesExtent.YMax - _limited.YMax {- tile_h /2} ) / tile_h ) ;
  bottom_tmp := TruncS( ( oTilesExtent.YMax - _limited.YMin {+ tile_h /2} ) / tile_h ) ;

  Result.Scale  := res_w ;
  Result.Left   := Max( left, left_tmp ) ;
  Result.Right  := Min( right, right_tmp ) ;
  Result.Top    := Max( top, top_tmp ) ;
  Result.Bottom := Min( bottom, bottom_tmp ) ;

end;

procedure TGIS_TilerHelper.Request(
  const _extent : TGIS_Extent ;
  const _width  : Integer     ;
  const _height : Integer
) ;
var
  icol,
  irow   : Int64;
  seg    : Integer ;
  res_w,
  res_h  : Double ;
  tile_w,
  tile_h : Double ;
  left,
  right,
  top,
  bottom : Int64 ;
  req    : TGIS_TilerRequest ;
begin
  if GisIsNoWorld( _extent ) then
    exit ;

  // do not override last requested
//  if GisIsSameExtent( _extent , rLastExtent ) then
//    exit ;

  criticalSection.Enter ;
  try

    // clear all pending reuests
    oRequestList.Clear ;

    // calulate tile scope

    res_w := (_extent.XMax - _extent.XMin ) / _width ;
    res_h := (_extent.YMax - _extent.YMin ) / _height ;

    tile_w := res_w * TileSize ;
    tile_h := res_w * TileSize ;

    left   := TruncS( ( _extent.XMin - oTilesExtent.XMin {- tile_w /2} ) / tile_w ) ;
    right  := TruncS( ( _extent.XMax - oTilesExtent.XMin {+ tile_w /2} ) / tile_w ) ;
    top    := TruncS( ( oTilesExtent.YMax - _extent.YMax {- tile_h /2} ) / tile_h ) ;
    bottom := TruncS( ( oTilesExtent.YMax - _extent.YMin {+ tile_h /2} ) / tile_h ) ;


    req.Scale := res_w ;
    req.Mode := 0 ;


    // requested area
    for icol := left to right do begin
      req.XPos := icol ;
      for irow := top to bottom do begin
        req.YPos := irow ;
        for seg := ThreadCount -1 downto 0 do begin
          req.Segment := seg ;
          req.Primary := True ;
          oRequestList.Add( req ) ;
        end;
      end;
    end;

    // margin around
    for irow := bottom + 1 downto top do begin
      req.XPos := left -1 ;
      req.YPos := irow ;
      for seg := 0 to ThreadCount -1 do begin
        req.Segment := seg ;
        req.Primary := False ;
        oRequestList.Add( req ) ;
      end;
    end ;

    for icol := left -1 to right do begin
      req.XPos := icol ;
      req.YPos := top - 1 ;
      for seg := 0 to ThreadCount -1 do begin
        req.Segment := seg ;
        req.Primary := False ;
        oRequestList.Add( req ) ;
      end;
    end ;

    for irow := top -1 to bottom do begin
      req.XPos := right + 1;
      req.YPos := irow ;
      for seg := 0 to ThreadCount -1 do begin
        req.Segment := seg ;
        req.Primary := False ;
        oRequestList.Add( req ) ;
      end;
    end ;

    for icol := right +1 downto left do begin
      req.XPos := icol ;
      req.YPos := bottom + 1 ;
      for seg := 0 to ThreadCount -1 do begin
        req.Segment := seg ;
        req.Primary := False ;
        oRequestList.Add( req ) ;
      end;
    end ;

    iActiveCacheSize := Max( iActiveCacheSize,
                             ( right-left + 2 ) * ( bottom-top + 2 ) //+2 for side tiles
                           ) ;

  finally
    criticalSection.Leave ;
  end;
end;

procedure TGIS_TilerHelper.RequestLabels(
  const _extent : TGIS_Extent ;
  const _width  : Integer     ;
  const _height : Integer
) ;
var
  i : Integer ;
  req : TGIS_TilerRequest ;
begin
  if GisIsNoWorld( _extent ) then
    exit ;

  criticalSection.Enter ;
  try
    if bPendingLabels then
      T_TilerExecutor(arThreads[0]).oBmp.Interrupt ;

    // deleate any pending labes
    for i := oRequestList.Count -1 downto 0 do begin
      if oRequestList[i].Mode = 1 then
        {$IFDEF CLR}
        oRequestList.RemoveAt(i) ;
        {$ELSE}
        oRequestList.Delete(i) ;
        {$ENDIF}
    end;

    req.Mode := 1;
    req.Segment := 999;
    req.Extent := _extent ;
    req.Width := _width ;
    req.Height := _height ;
    oRequestList.Add( req ) ;

  finally
    criticalSection.Leave ;
  end;
end;

procedure TGIS_TilerHelper.RequestRepaint;
var
  req : TGIS_TilerRequest ;
begin
  criticalSection.Enter ;
  try
    req.Mode := 2 ;
    req.Segment := 999;
    oRequestList.Add( req ) ;
  finally
    criticalSection.Leave ;
  end;
end;

function TGIS_TilerHelper.Suspend(
  const _forced : Boolean
) : Boolean ;
begin
   while True do begin
     criticalSection.Enter ;
     try
       if iReparentCount = 0 then
         inc( iLockExtraPaint );
       Result := iLockExtraPaint > 0 ;

     finally
       criticalSection.Leave ;
     end;

     if Result then
       break ;

     if not _forced then
       break ;

     (oViewer as IGIS_ViewerParent).ControlProcessMessages ;
   end ;
end;

procedure TGIS_TilerHelper.Continue;
begin
   criticalSection.Enter ;
   try
     assert( iLockExtraPaint > 0 ) ;
     dec( iLockExtraPaint ) ;
   finally
     criticalSection.Leave ;
   end;
end;

function TGIS_TilerHelper.UpdateExtent
  : Boolean ;

  function calc_extent : TGIS_Extent ;
  var
    tmp : Double ;
  begin
    if dTilesBaseRes = 0  then
       dTilesBaseRes := ( oViewer.Extent.XMax - oViewer.Extent.XMin ) / TileSize ;

    tmp := dTilesBaseRes * TileSize;
    Result := GisExtent(
                ( TruncS( oViewer.Extent.XMin / tmp ) - 1 ) * tmp,
                ( TruncS( oViewer.Extent.YMin / tmp ) - 1 ) * tmp,
                ( TruncS( oViewer.Extent.XMax / tmp ) + 1 ) * tmp,
                ( TruncS( oViewer.Extent.YMax / tmp ) + 1 ) * tmp
              ) ;
  end;

begin
  Result := False ;

  if oViewer.CS.EPSG > 0 then begin
    if not GisIsSameExtent( oTilesExtent, oViewer.CS.ValidityExtent ) then begin
      oTilesExtent := oViewer.CS.ValidityExtent ;
      Result := True ;
    end ;
  end
  else begin
    if not GisIsEmptyExtent( oViewer.Extent ) then begin
      if not GisIsSameExtent( oTilesExtent, calc_extent ) then begin
        dTilesBaseRes := 0 ;
        oTilesExtent := calc_extent ;
        Result := True ;
      end;
    end;
  end ;
end;

{$ENDREGION 'TGIS_TilerHelper'}

{$REGION 'TGIS_TilerSubtile'}

constructor TGIS_TilerSubtile.Create ;
begin
  criticalSection := TCriticalSection.Create ;
  bObsolate := True ;
  oBitmap := nil ;
end;

procedure TGIS_TilerSubtile.doDestroy ;
begin
  FreeObject( oBitmap ) ;
  FreeObject( criticalSection ) ;

  inherited;
end;

procedure TGIS_TilerSubtile.Lock ;
begin
  criticalSection.Enter;
end;

procedure TGIS_TilerSubtile.Unlock ;
begin
  criticalSection.Leave;
end;

{$ENDREGION 'TGIS_TilerSubtile'}

{$REGION 'TGIS_TilerTile'}

constructor TGIS_TilerTile.Create;
begin
  criticalSection := TCriticalSection.Create ;
  iLock := 0 ;
end ;

procedure TGIS_TilerTile.doDestroy;
var
  i : Integer ;
begin
  for i:= 0 to high( Subtiles ) do begin
    FreeObject( Subtiles[i] ) ;
  end;
  FreeObject( criticalSection ) ;

  inherited;
end;

procedure TGIS_TilerTile.Lock;
begin
  criticalSection.Enter;
  try
    inc( iLock ) ;
  finally
    criticalSection.Leave;
  end;
end;

procedure TGIS_TilerTile.Unlock;
begin
  criticalSection.Enter;
  try
    dec( iLock ) ;
    assert( iLock >= 0 );
  finally
    criticalSection.Leave;
  end;
end;

{$ENDREGION 'TGIS_TilerTile'}

//==================================== END =====================================
end.
