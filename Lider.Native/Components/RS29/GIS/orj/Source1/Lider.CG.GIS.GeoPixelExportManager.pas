//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Pixel export manager used to serve tiled context of a viewer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoPixelExportManager ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoPixelExportManager"'}
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

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF DCC}
uses
  System.Types,
  System.SysUtils,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoFunctions ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
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

  /// <summary>
  ///   Pixel export manager.
  /// </summary>
  TGIS_PixelExportManager = {$IFDEF OXYGENE} public {$ENDIF}
                            class ( TGIS_Object )
    private
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer       : IGIS_Viewer ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oDrawer       : IGIS_ViewerBmp ;
      oLayer        : TGIS_LayerPixel ;
      iDpi          : Integer ;
      FTileSize     : Integer ;
      FQuadSize     : Integer ;
      FOnBusy       : TGIS_BusyEvent ;
      FFill         : Boolean ;
      FFillColor    : TGIS_Color ;
      FFillValue    : Single ;
      FOverlappedExtentMargin : Integer ;
      FTemporaryVisibleExtent : TGIS_Extent ;

    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_lp">
      ///    destination pixel layer to export data
      /// </param>
      constructor Create( const _lp : TGIS_LayerPixel ) ;

      /// <summary>
      ///   Export a map given by the viewer using tiles.
      /// </summary>
      /// <param name="_viewer">
      ///    viewer with a map to export
      /// </param>
      /// <param name="_drawer">
      ///    helper native bitmap viewer for drawing on during export
      /// </param>
      /// <param name="_extent">
      ///    viewer extent to export
      /// </param>
      /// <param name="_ppi">
      ///   density of output image; will be used to calculate feature size
      ///   and will be embedded into final image whenever possible
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELSIZE
      /// </exception>
      procedure ExportFrom ( const _viewer  : IGIS_Viewer ;
                             const _drawer  : IGIS_ViewerBmp ;
                             const _extent  : TGIS_Extent ;
                             const _ppi     : Integer
                            ) ; overload ;

      /// <summary>
      ///   Export a map given by the viewer using tiles.
      /// </summary>
      /// <param name="_viewer">
      ///    viewer with a map to export
      /// </param>
      /// <param name="_drawer">
      ///    helper native bitmap viewer for drawing on during export
      /// </param>
      /// <param name="_extent">
      ///    viewer extent to export
      /// </param>
      /// <param name="_width">
      ///    parameter not used
      /// </param>
      /// <param name="_height">
      ///    parameter not used
      /// </param>
      /// <param name="_ppi">
      ///   density of output image; will be used to calculate feature size
      ///   and will be embedded into final image whenever possible
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Deprecated. Use the overloaded version instead.
      ///   </note>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELSIZE
      /// </exception>
      procedure ExportFrom ( const _viewer  : IGIS_Viewer ;
                             const _drawer  : IGIS_ViewerBmp ;
                             const _extent  : TGIS_Extent ;
                             const _width   : Integer     ;
                             const _height  : Integer     ;
                             const _ppi     : Integer
                            ) ; overload ;

   published //events
     /// <event/>
     /// <summary>
     ///   Busy event handler. Will be fired regularly during
     ///   long-drawn operations. If end value will be zero, the
     ///   meaning is: long-drawn with unknown end time. Close
     ///   long-drawn operation by calling with parameters (-1,-1).
     /// </summary>
     {$IFDEF CLR}
       event    BusyEvent : TGIS_BusyEvent delegate FOnBusy ;
     {$ELSE}
       property BusyEvent : TGIS_BusyEvent read  FOnBusy
                                           write FOnBusy ;
     {$ENDIF}

   published //properties
     /// <summary>
     ///   Size of exported tile.
     /// </summary>
     property   TileSize : Integer         read  FTileSize
                                           write FTileSize ;

     /// <summary>
     ///   If True, space beyond export extent and drawer background
     ///   will be filled with FillColor. Use to enable transparency upon export.
     /// </summary>
     property   Fill : Boolean             read  FFill
                                           write FFill ;
     /// <summary>
     ///   Color of the fill. Default is TGIS_Color.None (transparent).
     /// </summary>
     property   FillColor : TGIS_Color     read  FFillColor
                                           write FFillColor ;

     /// <summary>
     ///   Value of the fill (grid layers only).
     /// </summary>
     property   FillValue : Single         read  FFillValue
                                           write FFillValue ;

     /// <summary>
     ///   Margin (in pixels) to scan around an exported tile.
     /// </summary>
     /// <remarks>
     ///   This is the number of pixels added to the edges of the tile
     ///   that increases its visible extent.
     ///   Used to help render shapes placed at tiles' boundaries where
     ///   rendered shape's elements (so also labels) must be placed at neighboring tiles.
     ///   Default value is -1, then a 2-inch margin is taken into account.
     /// </remarks>
     property   OverlappedExtentMargin : Integer
                                           read  FOverlappedExtentMargin
                                           write FOverlappedExtentMargin ;

     /// <summary>
     ///   Area for rendered labels.
     /// </summary>
     /// <remarks>
     ///   Default value is GisNoWorld, it means that only labels
     ///   that fully fit inside the whole exported extent will be rendered.
     ///   If the area exceeds the exported extent, then the label fragments
     ///   may appear on the outer edges.
     /// </remarks>
     property   TemporaryVisibleExtent : TGIS_Extent
                                           read  FTemporaryVisibleExtent
                                           write FTemporaryVisibleExtent ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.Math,

  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoViewer ;
{$ENDIF}

//==============================================================================
// TGIS_PixelExportManager
//==============================================================================

  constructor TGIS_PixelExportManager.Create(
    const _lp : TGIS_LayerPixel
  ) ;
  begin
    inherited Create;

    assert( assigned(_lp) ) ;
    oLayer := _lp ;

    FTileSize := 512 ;
    FQuadSize := 2*FTileSize ;
    FFill      := False ;
    FFillColor := TGIS_Color.None ;
    FFillValue := GIS_GRID_NOVALUE ;
    FOverlappedExtentMargin := -1 ;
    FTemporaryVisibleExtent := GisNoWorld ;
  end ;

  procedure TGIS_PixelExportManager.ExportFrom(
    const _viewer : IGIS_Viewer ;
    const _drawer : IGIS_ViewerBmp ;
    const _extent : TGIS_Extent ;
    const _ppi    : Integer
  );
  var
    old_restricted : Boolean ;
    old_extent     : TGIS_Extent ;
    old_color      : TGIS_Color ;
    xtiles, ytiles : Integer ;
    cell, row      : Integer ;
    level          : Integer ;
    scale          : Double ;
    max_level      : Integer ;
    ext            : TGIS_Extent ;
    tile_rect      : TRect ;
    ext_rect       : TRect ;
    ddx, ddy       : Double ;
    dx, dy         : Double ;
    wextent        : TGIS_Extent ;
    abrt           : Boolean ;
    cnt            : Integer ;
    pos            : Integer ;
    tss            : Boolean ;

    old_viewer     : TObject ;

    old_CustomPPI              : Integer ;
    old_RestrictedDrag         : Boolean ;
    old_TemporaryVisibleExtent : TGIS_Extent ;
    old_OverlappedExtentMargin : Integer ;
    old_Basemap                : array of Boolean ;

    i : Integer;
    ccx : Integer ;
    ccy : Integer ;

    {$IFDEF GIS_DUMPIMAGE}
    procedure writeWorldFile(
      const _ext   : TGIS_Extent ;
      const _w, _h : Integer ;
      const _path  : String
    ) ;
    var
      scale_x : Double   ;
      scale_y : Double   ;
      olist   : TGIS_StringList ;
    begin
      scale_x := ( _ext.XMax - _ext.XMin ) / _w  ;
      scale_y := ( _ext.YMax - _ext.YMin ) / _h ;

      olist := TGIS_StringList.Create ;
      try
        olist.Text := Format( '%s' + #13#10 + '%d' + #13#10 + '%d' + #13#10 +
                              '%s' + #13#10 + '%s' + #13#10 + '%s' + #13#10 ,
                              [ DotFloatToStr(   scale_x ), 0, 0,
                                DotFloatToStr( - scale_y ),
                                DotFloatToStr( _ext.XMin +0.5*scale_x ),
                                DotFloatToStr( _ext.YMax -0.5*scale_y )
                              ]
                            ) ;
        olist.SaveToFile( _path ) ;
      finally
        FreeObject( olist ) ;
      end ;
    end ;
    {$ENDIF}

    procedure fill_background ;
    var
      x, y  : Integer ;
      w, h  : Integer ;
      plock : TGIS_LayerPixelLock ;
      i, j  : Integer ;
      pos   : Integer ;
    begin
      if oLayer.IsTiled then exit ;
      y := 0 ;
      while y < oLayer.BitHeight do begin
        x := 0 ;
        while x < oLayer.BitWidth do begin
          w := Min( 512, oLayer.BitWidth  - x ) ;
          h := Min( 512, oLayer.BitHeight - y ) ;
          if ( x < tile_rect.Left ) or
             ( y < tile_rect.Top ) or
             ( x + w > tile_rect.Right ) or
             ( y + h > tile_rect.Bottom ) then begin
            // something to fill
            plock := oLayer.LockPixels( Rect( x, y, x+512, y+512 ), False ) ;
            try
              if not oLayer.IsGrid then begin
                for i := 0 to h-1 do begin
                  pos := plock.BitmapPos( x, y+i ) ;
                  for j := 0 to w-1 do
                    plock.Bitmap[pos+j] := Integer(FFillColor.ARGB) ;
                end;
              end else begin
                for i := 0 to h-1 do
                  for j := 0 to w-1 do
                    plock.Grid[plock.Bounds.Top+i][plock.Bounds.Left+j] := FFillValue ;
              end;
            finally
              oLayer.UnlockPixels( plock ) ;
            end;
          end;
          inc( x, 512 ) ;
        end;
        inc( y, 512 ) ;
      end;
    end;

    procedure set_drawing( _flag : Boolean ) ;
    begin
      if _flag then
        TGIS_Utils.GisMetadata.Values[METADATA_PIXELEXPORTDRAWLABELSETTINGS] := 'true'
      else
        TGIS_Utils.GisMetadata.Values[METADATA_PIXELEXPORTDRAWLABELSETTINGS] := 'false'
    end;

    procedure draw_map(
      const _rect : TRect
    ) ;
    var
      w, h      : Integer ;
      dw        : Integer ;
      dh        : Integer ;
      l, t      : Integer ;
      r, bw     : Integer ;
      plock     : TGIS_LayerPixelLock ;
      bmp       : TGIS_Bitmap ;
      pdata     : TGIS_Pixels ;
      pos       : Integer ;

      {$IFDEF GIS_DUMPIMAGE}
        dump_name : String ;
      {$ENDIF}
      grid_tile : TGIS_GridArray ;
      i, j      : Integer ;
      procedure set_transparent ;
      var
        ii : Integer ;
        tr : Integer ;
      begin
        tr :=  oViewer.Color.ARGB ;
        for ii := 0 to length(plock.Bitmap) -1 do
          if (plock.Bitmap[ii] and $00FFFFFF) = tr then
            plock.Bitmap[ii] := 0  ;
      end;
    begin
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := FTileSize ;
      h := FTileSize ;

      if oLayer.IsTiled then
        plock := oLayer.LockPixels( _rect, ext, level, row, cell, True )
      else
        plock := oLayer.LockPixels( _rect, True ) ;

      try
        if not oLayer.IsGridImage then begin
          if FFill then
            (oDrawer as IGIS_Viewer).Color := FillColor
          else
          begin
            if tss then
              (oDrawer as IGIS_Viewer).Color := TGIS_Color.FromARGB(oViewer.Color.ARGB and $00FFFFFF)
            else
              (oDrawer as IGIS_Viewer).Color := oViewer.Color ;

          end ;
          (oDrawer as IGIS_Viewer).Lock ;
          (oDrawer as IGIS_Viewer).VisibleExtent := ext ;
          (oDrawer as IGIS_Viewer).TemporaryScaleInternal := scale*2*(max_level-level-1) ;
          (oDrawer as IGIS_Viewer).Unlock( False ) ;
          oDrawer.TileRect := _rect ;
          oDrawer.Draw ;

          bmp := TGIS_Bitmap.Create ;
          try
            bmp.NativeBitmap := oDrawer.Bitmap ;

            {$IFDEF GIS_DUMPIMAGE}
              dump_name := Format( 'C:\TMP\tiles\%d-%d-%d', [ level, row, cell ] ) ;
              bmp.SaveToFile( dump_name + '.bmp' );
              writeWorldFile( ext, w, h, dump_name + '.bpw' ) ;
            {$ENDIF}

            bmp.LockPixels( pdata ) ;
            bw := plock.BitmapPos(0, 1) ;

            t :=  plock.Bounds.Top ;
            l :=  plock.Bounds.Left ;

            dw := Min( Min( plock.Bounds.Width + 1, w ),
                       tile_rect.Right - plock.Bounds.Left ) ;
            dh := Min( Min( plock.Bounds.Height + 1, h ),
                       tile_rect.Bottom - plock.Bounds.Top ) ;

            if (dw = bw) and (dw = w) and (dh = h) then begin
              pos := plock.BitmapPos(l, t) ;

              GisCopyPixels( pdata, 0, plock.Bitmap, pos, h*w ) ;
              if tss then
                set_transparent ;
            end
            else begin
              for r := 0 to dh - 1 do begin
                pos := plock.BitmapPos(l, t + r ) ;
                GisCopyPixels( pdata, r*bmp.Width, plock.Bitmap, pos, dw) ;
              end;
            end
          finally
            FreeObject( bmp ) ;
          end;
        end
        else begin
          try
            if (oLayer.BitWidth = length(plock.Grid[0]))
                and (oLayer.BitHeight = length(plock.Grid))  then
            begin
              grid_tile := InitializeGrid( w, h ) ;
              for i := 0 to h-1 do
                for j := 0 to w-1 do
                  grid_tile[i][j] := GIS_GRID_NOVALUE ;
              if (oDrawer as IGIS_Viewer).GetGrid( ext, grid_tile ) then ;
              for i := 0 to Min( tile_rect.Bottom - plock.Bounds.Top,
                                 Min( plock.Bounds.Height, h-1 ) ) do
                for j := 0 to Min( tile_rect.Right - plock.Bounds.Left,
                                   Min( plock.Bounds.Width, w-1 ) ) do
                  plock.Grid[plock.Bounds.Top+i][plock.Bounds.Left+j] := grid_tile[i][j] ;
            end
            else begin
              for i := 0 to length(plock.Grid)-1 do
                for j := 0 to length(plock.Grid[0])-1 do
                  plock.Grid[i][j] := GIS_GRID_NOVALUE ;
              if (oDrawer as IGIS_Viewer).GetGrid( ext, plock.Grid ) then ;
            end;
          finally
            SetLength( grid_tile, 0 ) ;
          end;
        end;
      finally
        oLayer.UnlockPixels( plock ) ;
      end ;

    end ;

  begin
    assert( assigned(_viewer) ) ;
    assert( assigned(_drawer) ) ;

    if GisIsNoWorld( _extent ) then exit ;

{    dx := Abs( oLayer.Extent.XMax - oLayer.Extent.XMin ) ;
    dy := Abs( oLayer.Extent.YMax - oLayer.Extent.YMin ) ;
{
    if ( Abs( ( dx * oLayer.BitHeight / dy ) - oLayer.BitWidth
            )  > Max( 1, dx / dy / 2 )
       ) or
       ( Abs( ( dy * oLayer.BitWidth  / dx ) - oLayer.BitHeight
            ) > Max( 1, dy / dx / 2 )
       ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELSIZE ), '', 0 ) ;
}
    oViewer := _viewer ;
    oDrawer := _drawer ;
    iDpi    := _ppi ;
    wextent := _extent ;
    abrt    := False ;

    tile_rect := oLayer.MapToRasterRect( wextent, nil ) ;
    tile_rect := Rect( Max( 0, tile_rect.Left ),
                       Max( 0, tile_rect.Top ),
                       Min( oLayer.BitWidth, tile_rect.Right ),
                       Min( oLayer.BitHeight, tile_rect.Bottom ) ) ;

    if ( tile_rect.Width = 0 ) and ( tile_rect.Height = 0 ) then
      tile_rect.Height := 512 ;

    if tile_rect.Width = 0  then
      tile_rect.Width  := RoundS( ( wextent.XMax - wextent.XMin ) /
                                  ( wextent.YMax - wextent.YMin )
                                  * tile_rect.Height
                                ) ;
    if tile_rect.Height = 0 then
      tile_rect.Height := RoundS( ( wextent.YMax - wextent.YMin ) /
                                  ( wextent.XMax - wextent.XMin )
                                  * tile_rect.Width
                                 ) ;

    old_restricted := oViewer.RestrictedDrag ;
    old_extent     := oViewer.VisibleExtent ;

    if assigned( FOnBusy ) then
      {$IFDEF OXYGENE}
        FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
      {$ELSE}
        FOnBusy( self, -1, -1, abrt ) ;
      {$ENDIF}

    old_CustomPPI              := oViewer.CustomPPI ;
    old_RestrictedDrag         := oViewer.RestrictedDrag ;
    old_TemporaryVisibleExtent := oViewer.TemporaryVisibleExtent ;
    old_OverlappedExtentMargin := oViewer.OverlappedExtentMargin ;
    old_color                  := oViewer.Color ;
    old_Basemap := nil ;
    if oViewer.Items.Count > 0 then begin
      SetLength( old_Basemap, oViewer.Items.Count ) ;
      for i := 0 to oViewer.Items.Count - 1 do
        old_Basemap[i] := TGIS_Layer( oViewer.Items[i] ).Basemap ;
    end ;
    try
      oViewer.CustomPPI := iDpi ;
      oViewer.RestrictedDrag := False ;
      oViewer.LabelsReg.Reset ;
      if GisIsNoWorld( FTemporaryVisibleExtent ) then
        oViewer.TemporaryVisibleExtent := wextent
      else
        oViewer.TemporaryVisibleExtent := FTemporaryVisibleExtent ;
      if FOverlappedExtentMargin < 0 then
        oViewer.OverlappedExtentMargin := oViewer.TwipsToPixels( 2 * 1440 )
      else
        oViewer.OverlappedExtentMargin := FOverlappedExtentMargin ;
      if oViewer.Items.Count > 0 then begin
        for i := 0 to oViewer.Items.Count - 1 do
          TGIS_Layer( oViewer.Items[i] ).Basemap := False ;
      end ;

      xtiles := tile_rect.Width div FTileSize ;
      if ( tile_rect.Width mod FTileSize ) > 0 then
        xtiles := xtiles + 1 ;

      ytiles := tile_rect.Height div FTileSize ;
      if ( tile_rect.Height mod FTileSize ) > 0 then
        ytiles := ytiles + 1 ;

      tss := False ;
      if oLayer.IsTiled then begin
        max_level := TruncS( Ln(Max(xtiles, ytiles)) / Ln(2) ) + 1 ;
        if (oLayer.NoDataColor = TGIS_Color.Maroon) and (not oLayer.IsGridImage) then
          tss := True ;
      end
      else
        max_level := 1 ;

      cnt := 0 ;
      pos := 0 ;
      for level := 0 to max_level-1 do begin
        cnt := cnt + (TruncS((tile_rect.Width/FTileSize)/Power(2,level))+1) *
                     (TruncS((tile_rect.Height/FTileSize)/Power(2,level)+1)) ;
      end ;

      oDrawer.SetSize( FTileSize, FTileSize ) ;
      old_viewer := (oDrawer as IGIS_ViewerParent).SetViewer( (oViewer as IGIS_ViewerParent).GetViewer ) ;
      oLayer.InitializeWrite ;
      try
        if FFill then
          fill_background ;

        // find scale for whole extent
        dx := (wextent.XMax-wextent.XMin)/(tile_rect.Width / FTileSize)*Power(2,max_level-1) ;
        dy := (wextent.YMax-wextent.YMin)/(tile_rect.Height / FTileSize)*Power(2,max_level-1) ;
        ddx := dx ;
        ddy := dy ;

        set_drawing( GisMetadataAsBoolean(
                       METADATA_PIXELEXPORTLABELSETTINGS,
                       False
                     )
                   ) ;
        oViewer.Lock ;
        try
          oViewer.VisibleExtent := GisExtent( wextent.XMin, wextent.YMax - ddy,
                                              wextent.XMin + ddx, wextent.YMax
                                             ) ;
          scale := oViewer.ScaleAsFloat ;

          for level := max_level -1 downto 0 do begin
            dx := (wextent.XMax-wextent.XMin)/(tile_rect.Width/FTileSize)*Power(2,level) ;
            dy := (wextent.YMax-wextent.YMin)/(tile_rect.Height/FTileSize)*Power(2,level) ;
            ddx := dx ;
            ddy := dy ;

            dy := wextent.YMax ;
            ccy := 0 ;
            row := 0 ;
            while dy > wextent.YMin do begin
              dx := wextent.XMin ;
              ccx := 0 ;
              cell := 0 ;
              while dx < wextent.XMax do begin
                // set range
                ext := GisExtent( dx, dy - ddy, dx + ddx, dy  ) ;

                if level = 0 then
                  ext_rect := oLayer.MapToRasterRect( ext, nil )
                else
                  ext_rect := Rect( cell, row, cell + FTileSize, row + FTileSize ) ;
                if ( tile_rect.Right  > ext_rect.Left ) and
                   ( tile_rect.Bottom > ext_rect.Top  ) then
                  draw_map( ext_rect ) ;

                inc(cell, FTileSize) ;
                inc(ccx) ;
                dx := wextent.XMin +ccx*ddx;

                if assigned( FOnBusy ) then
                  {$IFDEF OXYGENE}
                    FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                  {$ELSE}
                    FOnBusy( self, pos, cnt, abrt ) ;
                  {$ENDIF}

                if abrt then break ;
                inc( pos ) ;
              end ;
              inc(ccy) ;
              dy := wextent.YMax -ccy*ddy;
              inc(row, FTileSize ) ;
              if abrt then break ;
            end ;
          end ;
        finally
          oViewer.Unlock( false ) ;
        end ;
      finally
        oLayer.FinalizeWrite ;
        (oDrawer as IGIS_ViewerParent).SetViewer( old_viewer ) ;
        oViewer.ReParent( oViewer as IGIS_ViewerParent ) ;
        set_drawing( False ) ;
      end ;
    finally
      oViewer.CustomPPI              := old_CustomPPI              ;
      oViewer.RestrictedDrag         := old_RestrictedDrag         ;
      oViewer.TemporaryVisibleExtent := old_TemporaryVisibleExtent ;
      oViewer.OverlappedExtentMargin := old_OverlappedExtentMargin ;
      oViewer.TemporaryScaleInternal := 0 ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}

      if length( old_Basemap ) > 0 then begin
        for i := 0 to length( old_Basemap ) - 1 do
          TGIS_Layer( oViewer.Items[i] ).Basemap := old_Basemap[i] ;
      end ;
      oViewer.RestrictedDrag := old_restricted ;
      oViewer.VisibleExtent  := old_extent     ;
      oViewer.Color          := old_color ;
      oViewer := nil ;
      oDrawer := nil ;
    end ;
  end ;

  procedure TGIS_PixelExportManager.ExportFrom(
    const _viewer  : IGIS_Viewer ;
    const _drawer  : IGIS_ViewerBmp ;
    const _extent  : TGIS_Extent ;
    const _width   : Integer     ;
    const _height  : Integer     ;
    const _ppi     : Integer
  ) ;
  begin
    ExportFrom( _viewer, _drawer, _extent, _ppi ) ;
  end;


//==================================== END =====================================
end.
