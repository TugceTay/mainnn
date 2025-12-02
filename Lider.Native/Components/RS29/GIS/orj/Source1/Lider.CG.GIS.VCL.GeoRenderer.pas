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
  VCL renderer.
}

unit Lider.CG.GIS.VCL.GeoRenderer;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoRenderer"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  System.Types,
  VCL.Graphics,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoRendererAbstract ;

type

  {#gendoc:hide}
  /// <summary>
  ///   Brush cache for pattern brushes. For internal use only.
  /// </summary>
  TGIS_BrushCache = class ( TGIS_ObjectDisposable )
    private
      bmpHorizontal : TBitmap ;
      clrHorizontal : TGIS_Color ;
      bmpVertical   : TBitmap ;
      clrVertical   : TGIS_Color ;
      bmpFDiagonal  : TBitmap ;
      clrFDiagonal  : TGIS_Color ;
      bmpBDiagonal  : TBitmap ;
      clrBDiagonal  : TGIS_Color ;
      bmpCross      : TBitmap ;
      clrCross      : TGIS_Color ;
      bmpDiagCross  : TBitmap ;
      clrDiagCross  : TGIS_Color ;

    protected
      procedure doDestroy ; override ;

    public

      /// <summary>
      ///   Returns a bitmap with given pattern and color.
      /// </summary>
      /// <param name="_style">
      ///   given pattern
      /// </param>
      /// <param name="_color">
      ///   color of pattern
      /// </param>
      /// <returns>
      ///   Bitmap object.
      /// </returns>
      function  GetBitmap ( const _style : TGIS_BrushStyle ;
                            const _color : TGIS_Color
                          ) : VCL.Graphics.TBitmap ;
  end ;

  /// <summary>
  ///   Renderer base for VCL
  /// </summary>
  TGIS_RendererVclAbstract = class( TGIS_RendererAbstract )
    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates bitmap.
      /// </summary>
      /// <param name="_width">
      ///   given width of new bitmap
      /// </param>
      /// <param name="_height">
      ///   given width of new bitmap
      /// </param>
      /// <param name="_fill">
      ///   color fill of bitmap
      /// </param>
      /// <returns>
      ///   Bitmap object.
      /// </returns>
      function  createBitmap     ( const _width   : Integer ;
                                   const _height  : Integer ;
                                   const _fill    : TColor
                                 ) : TObject ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Creates transparent bitmap.
      /// </summary>
      /// <param name="_width">
      ///   given width of new bitmap
      /// </param>
      /// <param name="_height">
      ///   given width of new bitmap
      /// </param>
      /// <returns>
      ///   Bitmap object.
      /// </returns>
      function  createTransparentBitmap
                                 ( const _width   : Integer ;
                                   const _height  : Integer
                                 ) : TObject ; virtual;


      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Clear given bitmap.
      /// </summary>
      /// <param name="_bitmap">
      ///   given bitmap
      /// </param>
      procedure clearTransparentBitmap
                                 ( const _bitmap  : TObject
                                 ) ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Stretch draw bitmap with antialiasing and preserving transparency.
      /// </summary>
      /// <param name="_src_bmp">
      ///   bitmap to stretch
      /// </param>
      /// <param name="_dst_bmp">
      ///   bitmap to be stretched to
      /// </param>
      /// <param name="_dst_rct">
      ///   rectangle to which _bmp will be scaled
      /// </param>
      /// <param name="_transparency">
      ///   transparency factor
      /// </param>
      procedure stretchBitmap    ( const _src_bmp      : TObject ;
                                   const _dst_bmp      : TObject ;
                                   const _dst_rct      : TRect   ;
                                   const _transparency : Integer
                                 ) ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Stretch draw bitmap w/o antialiasing.
      /// </summary>
      /// <param name="_src_bmp">
      ///   bitmap to stretch
      /// </param>
      /// <param name="_dst_bmp">
      ///   bitmap to be stretched to
      /// </param>
      /// <param name="_dst_rct">
      ///   rectangle to which _bmp will be scaled
      /// </param>
      procedure stretchBitmapFast( const _src_bmp     : TObject ;
                                   const _dst_bmp     : TObject ;
                                   const _dst_rct     : TRect
                                 ) ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Copy bitmap RAW bitmap ARGB data.
      /// </summary>
      /// <param name="_src_bmp">
      ///   bitmap to copy from
      /// </param>
      /// <param name="_dst_bmp">
      ///   bitmap to copy to
      /// </param>
      procedure copyBitmap       ( const _src_bmp     : TObject ;
                                   const _dst_bmp     : TObject
                                 ) ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Blends bitmap
      /// </summary>
      /// <param name="_bmp_src">
      ///   bitmap to be blended
      /// </param>
      /// <param name="_bmp_dst">
      ///   bitmap to be blended to
      /// </param>
      /// <param name="_transparency">
      ///   transparency factor
      /// </param>
      /// <param name="_merge_alpha">
      ///   set to true to merge with background alpha channel; set to false
      ///   for faster operation
      /// </param>
      procedure blendBitmaps     ( const _bmp_src      : TObject ;
                                   const _bmp_dst      : TObject ;
                                   const _transparency : Integer ;
                                   const _merge_alpha  : Boolean
                                 ) ; virtual;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Blends bitmap. Special version for Labels w/o premuliplication.
      /// </summary>
      /// <param name="_bmp_src">
      ///   bitmap to be blended
      /// </param>
      /// <param name="_bmp_dst">
      ///   bitmap to be blended to
      /// </param>
      /// <param name="_merge_alpha">
      ///   set to true to merge with background alpha channel; set to false
      ///   for faster operation
      /// </param>
      procedure blendBitmapsLbl ( const _bmp_src      : TObject ;
                                  const _bmp_dst      : TObject ;
                                  const _merge_alpha  : Boolean
                                ) ; virtual;

    protected
      /// <summary>
      ///   Utility function to render bitmap on a current background context.
      /// </summary>
      /// <param name="_background">
      ///   background canvas
      /// </param>
      /// <param name="_handle">
      ///   handle to the context obtained by RenderBitmapBegin;
      ///   cam be nil  for non progressive display
      /// </param>
      /// <param name="_bmp">
      ///   array of bitmap pixels
      /// </param>
      /// <param name="_size">
      ///   width/height of _bmp (in pixels)
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (in pixels); scaling will apply if required
      /// </param>
      /// <param name="_format">
      ///   pixel format of _bmp array
      /// </param>
      /// <param name="_order">
      ///   line order of _bmp array
      /// </param>
      /// <param name="_on_empty">
      ///   mode to render on empty (nulled) _backround
      /// </param>
      /// <remarks>
      ///   To be used by RenderBitmap().
      /// </remarks>
      procedure renderBitmapInternal(
                                   const _handle       : TObject              ;
                                   const _background   : VCL.Graphics.TBitmap ;
                                   const _bmp          : TGIS_Pixels          ;
                                   const _size         : TPoint               ;
                                   const _dst          : TRect                ;
                                   const _format       : TGIS_BitmapFormat    ;
                                   const _order        : TGIS_BitmapLinesOrder;
                                   const _on_empty     : Boolean
                                 ) ; virtual;

  end ;

implementation

uses
  System.SysUtils,
  System.Math,
  Winapi.Windows ;

//=============================================================================
// TGIS_BrushCache
//=============================================================================

procedure TGIS_BrushCache.doDestroy ;
var
  bmp : TBitmap ;
begin
  FreeObject( bmpHorizontal ) ;
  FreeObject( bmpVertical   ) ;
  FreeObject( bmpFDiagonal  ) ;
  FreeObject( bmpBDiagonal  ) ;
  FreeObject( bmpCross      ) ;
  FreeObject( bmpDiagCross  ) ;

  inherited ;
end ;

function TGIS_BrushCache.GetBitmap(
  const _style : TGIS_BrushStyle ;
  const _color : TGIS_Color
) : VCL.Graphics.TBitmap ;
var
  dim : Integer ;
  b   : Boolean ;
  i   : Integer ;
  k   : Integer ;
  buf : PByteArray ;

  procedure init_bmp(
    var _bmp : VCL.Graphics.TBitmap ;
        _dim : Integer
  ) ;
  var
    ii  : Integer ;
    kk  : Integer ;
  begin
    b := False ;
    if not Assigned( _bmp ) then begin
      _bmp := VCL.Graphics.TBitmap.Create ;
      _bmp.PixelFormat := pf32bit ;
      _bmp.SetSize( _dim, _dim ) ;
      _bmp.AlphaFormat := TAlphaFormat.afPremultiplied ;

      for ii := 0 to _dim-1 do begin
        buf := _bmp.ScanLine[ii] ;
        for kk := 0 to _dim-1 do begin
          buf[4*kk  ] := 0 ;
          buf[4*kk+1] := 0 ;
          buf[4*kk+2] := 0 ;
          buf[4*kk+3] := 0 ;
        end ;
      end ;

      b := True ;
    end ;
  end ;

begin
  case _style of
    TGIS_BrushStyle.Horizontal :
      begin
        dim := 6 ;
        init_bmp( bmpHorizontal, dim ) ;
        if b or ( _color <> clrHorizontal ) then begin
          clrHorizontal := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpHorizontal.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( i = 3 ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end
            end;
          end;
        end ;
        Result := bmpHorizontal ;
      end ;
    TGIS_BrushStyle.Vertical   :
      begin
        dim := 6 ;
        init_bmp( bmpVertical, dim ) ;
        if b or ( _color <> clrVertical ) then begin
          clrVertical := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpVertical.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( k = 3 ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end
            end;
          end;
        end ;
        Result := bmpVertical ;
      end ;
    TGIS_BrushStyle.FDiagonal  :
      begin
        dim := 8 ;
        init_bmp( bmpFDiagonal, dim ) ;
        if b or ( _color <> clrFDiagonal ) then begin
          clrFDiagonal := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpFDiagonal.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( i = k ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end
            end;
          end;
        end ;
        Result := bmpFDiagonal ;
      end ;
    TGIS_BrushStyle.BDiagonal  :
      begin
        dim := 8 ;
        init_bmp( bmpBDiagonal, dim ) ;
        if b or ( _color <> clrBDiagonal ) then begin
          clrBDiagonal := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpBDiagonal.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( i + k = dim-1 ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end ;
            end ;
          end ;
        end ;
        Result := bmpBDiagonal ;
      end ;
    TGIS_BrushStyle.Cross      :
      begin
        dim := 6 ;
        init_bmp( bmpCross, dim ) ;
        if b or ( _color <> clrCross ) then begin
          clrCross := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpCross.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( i = 3 ) or ( k = 3 ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end ;
            end ;
          end ;
        end ;
        Result := bmpCross ;
      end ;
    TGIS_BrushStyle.DiagCross  :
      begin
        dim := 8 ;
        init_bmp( bmpDiagCross, dim ) ;
        if b or ( _color <> clrDiagCross ) then begin
          clrDiagCross := _color ;
          for i := 0 to dim-1 do begin
            buf := bmpDiagCross.ScanLine[i] ;
            for k := 0 to dim-1 do begin
              if ( i = k ) or ( i + k = dim ) then begin
                buf[4*k  ] := _color.B ;
                buf[4*k+1] := _color.G ;
                buf[4*k+2] := _color.R ;
                buf[4*k+3] := _color.A ;
              end ;
            end ;
          end ;
        end ;
        Result := bmpDiagCross ;
      end ;
    else
      begin
        Result := nil ;
        exit ;
      end ;
  end ;
end ;


//=============================================================================
// TGIS_RendererVclAbstract
//=============================================================================

function TGIS_RendererVclAbstract.createTransparentBitmap(
  const _width  : Integer ;
  const _height : Integer
) : TObject ;
var
  bmp : VCL.Graphics.TBitmap ;
begin
  bmp := VCL.Graphics.TBitmap.Create ;
  bmp.PixelFormat := pf32bit ;
  bmp.Width := _width ;
  bmp.Height := _height ;
  bmp.Modified := False ;

  clearTransparentBitmap( bmp ) ;
  Result := bmp ;
end ;

function TGIS_RendererVclAbstract.createBitmap(
  const _width  : Integer ;
  const _height : Integer ;
  const _fill   : TColor
) : TObject ;
var
  i, j : Integer ;
  pt   : pByteArray ;
  bmp  : VCL.Graphics.TBitmap ;
  r,g,b: Byte ;
begin
  bmp := VCL.Graphics.TBitmap.Create ;
  bmp.Width       := _width ;
  bmp.Height      := _height ;
  bmp.PixelFormat := pf32bit ;

  r := GetRValue( _fill ) ;
  g := GetGValue( _fill ) ;
  b := GetBValue( _fill ) ;

  for i := 0 to bmp.Height - 1 do begin
    pt := bmp.ScanLine[i] ;
    for j := 0 to bmp.Width - 1 do begin
      pt[j*4]   := b ;
      pt[j*4+1] := g ;
      pt[j*4+2] := r ;
      pt[j*4+3] := $FF ;
    end;
  end ;
  Result := bmp ;
end;

procedure TGIS_RendererVclAbstract.clearTransparentBitmap(
  const _bitmap : TObject
) ;
var
  y   : Integer ;
  bmp : VCL.Graphics.TBitmap ;
begin
  bmp := VCL.Graphics.TBitmap( _bitmap ) ;
  assert( bmp.PixelFormat = pf32bit ) ;
  for y := 0 to bmp.Height - 1 do begin
    FillChar( PByte( bmp.ScanLine[y] )^, bmp.Width * 4, $00 ) ;
  end ;
end ;

procedure TGIS_RendererVclAbstract.stretchBitmap(
  const _src_bmp      : TObject ;
  const _dst_bmp      : TObject ;
  const _dst_rct      : TRect   ;
  const _transparency : Integer
) ;
var
  src_bmp       : VCL.Graphics.TBitmap ;
  dst_bmp       : VCL.Graphics.TBitmap ;
  src_rct       : TRect ;
  dst_rct       : TRect ;

  src_bmp_w     : Integer ;
  src_bmp_h     : Integer ;
  dst_bmp_w     : Integer ;
  dst_bmp_h     : Integer ;

  dtmp          : Single ;

  ar_src_lin    : array of IntPtr ;
  dst_lin,
  src_lin       : IntPtr ;

  scale_x,
  scale_y       : Single ;

  dst_x,
  dst_y         : Integer ;
  src_x,
  src_y         : Integer ;

  ix,
  iy            : Integer ;

  dst_x_dscaled,
  dst_y_dscaled : Single ;
  dst_x_iscaled,
  dst_y_iscaled : Integer ;

  weight        : Single ;
  weight_x,
  weight_y      : array[0..1] of Single;

  w_r,
  w_g,
  w_b,
  w_a           : Single ;

  src_r,
  src_g,
  src_b,
  src_a         : Byte ;

  dst_r,
  dst_g,
  dst_b,
  dst_a         : Byte ;

  src_alpha     : Byte ;
  dst_alpha     : SmallInt ;

  transp        : Integer ;
begin
  if not assigned( _src_bmp ) then exit ;

  src_bmp := VCL.Graphics.TBitmap( _src_bmp ) ;
  dst_bmp := VCL.Graphics.TBitmap( _dst_bmp ) ;

  Assert( src_bmp.PixelFormat = pf32bit );
  Assert( dst_bmp.PixelFormat = pf32bit );

  src_bmp_w := src_bmp.Width  ;
  src_bmp_h := src_bmp.Height ;
  dst_bmp_w := dst_bmp.Width ;
  dst_bmp_h := dst_bmp.Height ;

  if ( _dst_rct.Left   >  dst_bmp_w     ) or
     ( _dst_rct.Right  <  0             ) or
     ( _dst_rct.Top    >  dst_bmp_h     ) or
     ( _dst_rct.Bottom <  0             ) or
     ( _dst_rct.Right  <= _dst_rct.Left ) or
     ( _dst_rct.Bottom <= _dst_rct.Top  )
  then
    exit ;

  src_rct := Rect( 0, 0, src_bmp_w, src_bmp_h ) ;
  dst_rct := _dst_rct ;

  transp := _transparency * 256 div 100 ;

  if transp = 0 then
    exit ;

  // calculate destination bitmap bounding
  // and alter source bitmap accordingly

  if _dst_rct.Left < 0 then begin
    dtmp := src_rct.Left
            + ( 0 - _dst_rct.Left )
            * src_rct.Width / _dst_rct.Width ;
    src_rct.Left  := Trunc( dtmp ) ;
    dst_rct.Left  := Round(
                       ( src_rct.Left - dtmp )
                       * _dst_rct.Width / src_bmp_w
                     ) ;
  end ;

  if _dst_rct.Right > dst_bmp_w then begin
    dtmp := src_rct.Right
            - ( _dst_rct.Right - dst_bmp_w )
            * src_bmp_w / _dst_rct.Width ;
    src_rct.Right := Trunc( dtmp ) ;
    dst_rct.Right := dst_bmp_w
                     - Round(
                        ( dtmp - src_rct.Right - 1 )
                        * _dst_rct.Width / src_bmp_w
                       ) ;
  end ;

  if _dst_rct.Top < 0 then begin
    dtmp := src_rct.Top
            + ( 0 - _dst_rct.Top )
            * src_bmp_h / _dst_rct.Height ;
    src_rct.Top := Trunc( dtmp ) ;
    dst_rct.Top := Round(
                    ( src_rct.Top - dtmp )
                    * _dst_rct.Height / src_bmp_h
                   ) ;
  end ;

  if _dst_rct.Bottom > dst_bmp_h then begin
    dtmp := src_rct.Bottom
            - ( _dst_rct.Bottom - dst_bmp_h )
            * src_bmp_h / _dst_rct.Height ;
    src_rct.Bottom := Round( dtmp ) ;
    dst_rct.Bottom := dst_bmp_h
                      - Round(
                          ( dtmp - src_rct.Bottom - 1 )
                          * _dst_rct.Height / src_bmp_h
                        ) ;
  end ;


  if src_rct.Width  < 2 then exit ;
  if src_rct.Height < 2 then exit ;
  if dst_rct.Width  < 2 then exit ;
  if dst_rct.Height < 2 then exit ;

  // store all scanlines; accessing scanline is not very fast
  SetLength( ar_src_lin, src_bmp_h ) ;
  for src_y := 0 to src_bmp_h - 1 do begin
    ar_src_lin[ src_y ] := IntPtr( src_bmp.ScanLine[ src_y ] ) ;
  end;

  scale_x := dst_rct.Width  / ( src_rct.Width  + 1 ) ;
  scale_y := dst_rct.Height / ( src_rct.Height + 1 ) ;

  for dst_y := dst_rct.Top to dst_rct.Bottom -1 do begin
    dst_y_dscaled := ( dst_y - dst_rct.Top ) / scale_y ;
    dst_y_iscaled := Trunc( dst_y_dscaled );
    src_y := dst_y_iscaled + src_rct.Top ;

    weight_y[1] := dst_y_dscaled - dst_y_iscaled;
    weight_y[0] := 1 - weight_y[1];

    if dst_y < 0 then
      continue ;
    if dst_y >= dst_bmp_h then
      continue ;

    dst_lin := IntPtr( dst_bmp.ScanLine[ dst_y ] ) + 4 * Max( 0, dst_rct.Left ) ;

    for dst_x := dst_rct.Left to dst_rct.Right -1 do begin
      if dst_x < 0 then
        continue ;
      if dst_x >= dst_bmp_w then
        continue ;

      dst_x_dscaled := ( dst_x - dst_rct.Left ) / scale_x;
      dst_x_iscaled := Trunc( dst_x_dscaled );
      src_x := dst_x_iscaled + src_rct.Left ;

      weight_x[1] := dst_x_dscaled - dst_x_iscaled;
      weight_x[0] := 1 - weight_x[1];

      w_r := 0.0;
      w_g := 0.0;
      w_b := 0.0;
      w_a := 0.0;

      if src_y + 1 >= src_bmp_h then
          continue ;
      if src_x + 1 >= src_bmp_w then
        continue ;

      for iy := 0 to 1 do begin
        if src_y + iy >= src_bmp_h then
          continue ;

        src_lin := ar_src_lin[ src_y + iy ] + 4 * src_x ;

        for ix := 0 to 1 do begin
          if src_x + ix >= src_bmp_w then
            continue ;

          src_lin := src_lin + 4 * ix ;

          src_b := PByte( src_lin + 0 )^ ;
          src_g := PByte( src_lin + 1 )^ ;
          src_r := PByte( src_lin + 2 )^ ;
          src_a := PByte( src_lin + 3 )^ ;

          if ( src_a > 0 ) and ( src_a <255 ) then
          begin
            src_a := src_a ;
          end;

          if ( src_a = 0 ) and
             (
               ( src_r <> 0 ) or
               ( src_g <> 0 ) or
               ( src_b <> 0 )
             )
          then
            src_a := $FF ;

          weight := weight_x[ix] * weight_y[iy];

          w_r := w_r + src_r * weight;
          w_g := w_g + src_g * weight;
          w_b := w_b + src_b * weight;
          w_a := w_a + src_a * weight ;
        end;
      end;

      if dst_x < 0 then continue ;
      if dst_x >= dst_bmp_w then continue ;

      src_r := Round( w_r ) ;
      src_g := Round( w_g ) ;
      src_b := Round( w_b ) ;
      src_a := Round( w_a ) ;

      if ( src_a = 0 ) and
         ( src_r = 0 ) and
         ( src_g = 0 ) and
         ( src_b = 0 )
      then begin
        // do nothing - transparent
      end
      else
      if PCardinal( dst_lin )^ = 0 then begin
        if src_a = 0 then
          src_a := 255 ;

        PByte(  dst_lin + 0 )^ := src_b * transp shr 8 ;
        PByte(  dst_lin + 1 )^ := src_g * transp shr 8 ;
        PByte(  dst_lin + 2 )^ := src_r * transp shr 8 ;
        PByte(  dst_lin + 3 )^ := src_a * transp shr 8 ;
      end
      else begin
        if src_a = 0 then
          src_alpha := $FF * transp shr 8
        else
          src_alpha := Round( w_a ) ;
        src_alpha := 255 ;

        dst_alpha := 256 - src_alpha ;

        dst_b := PByte(  dst_lin + 0 )^ ;
        dst_g := PByte(  dst_lin + 1 )^ ;
        dst_r := PByte(  dst_lin + 2 )^ ;
        dst_a := PByte(  dst_lin + 3 )^ ;

        try
        PByte(  dst_lin + 0 )^ := src_b * transp shr 8 + dst_b * dst_alpha shr 8 ;
        PByte(  dst_lin + 1 )^ := src_g * transp shr 8 + dst_g * dst_alpha shr 8 ;
        PByte(  dst_lin + 2 )^ := src_r * transp shr 8 + dst_r * dst_alpha shr 8 ;
        PByte(  dst_lin + 3 )^ := src_a * transp shr 8 + dst_a * dst_alpha shr 8 ;
        except
        PByte(  dst_lin + 0 )^ := src_b * transp shr 8 + dst_b * dst_alpha shr 8 ;
        PByte(  dst_lin + 1 )^ := src_g * transp shr 8 + dst_g * dst_alpha shr 8 ;
        PByte(  dst_lin + 2 )^ := src_r * transp shr 8 + dst_r * dst_alpha shr 8 ;
        PByte(  dst_lin + 3 )^ := src_a * transp shr 8 + dst_a * dst_alpha shr 8 ;
        end;
      end;

      dst_lin := dst_lin + 4 ;
    end;

  end;
end;

procedure TGIS_RendererVclAbstract.stretchBitmapFast(
  const _src_bmp : TObject ;
  const _dst_bmp : TObject ;
  const _dst_rct : TRect
) ;
var
  src_bmp       : VCL.Graphics.TBitmap ;
  dst_bmp       : VCL.Graphics.TBitmap ;
  src_rct       : TRect ;
  dst_rct       : TRect ;

  src_bmp_w     : Integer ;
  src_bmp_h     : Integer ;
  dst_bmp_w     : Integer ;
  dst_bmp_h     : Integer ;

  dtmp          : Single ;

  ar_src_lin    : array of IntPtr ;
  dst_lin,
  src_lin       : IntPtr ;

  scale_x,
  scale_y       : Single ;

  dst_x,
  dst_y         : Integer ;
  src_x,
  src_y         : Integer ;

  ix,
  iy            : Integer ;

  dst_x_dscaled,
  dst_y_dscaled : Single ;
  dst_x_iscaled,
  dst_y_iscaled : Integer ;

  cl            : Cardinal ;
  src_r,
  src_g,
  src_b,
  src_a         : Byte ;

  dst_r,
  dst_g,
  dst_b,
  dst_a         : Byte ;

  src_alpha     : Byte ;
  dst_alpha     : SmallInt ;
begin
  src_bmp := VCL.Graphics.TBitmap( _src_bmp ) ;
  dst_bmp := VCL.Graphics.TBitmap( _dst_bmp ) ;

  Assert( src_bmp.PixelFormat = pf32bit );
  Assert( dst_bmp.PixelFormat = pf32bit );

  src_bmp_w := src_bmp.Width  ;
  src_bmp_h := src_bmp.Height ;
  dst_bmp_w := dst_bmp.Width ;
  dst_bmp_h := dst_bmp.Height ;

  if ( _dst_rct.Left   >  dst_bmp_w     ) or
     ( _dst_rct.Right  <  0             ) or
     ( _dst_rct.Top    >  dst_bmp_h     ) or
     ( _dst_rct.Bottom <  0             ) or
     ( _dst_rct.Right  <= _dst_rct.Left ) or
     ( _dst_rct.Bottom <= _dst_rct.Top  )
  then
    exit ;

  src_rct := Rect( 0, 0, src_bmp_w, src_bmp_h ) ;
  dst_rct := _dst_rct ;

  // calculate destination bitmap bounding
  // and alter source bitmap accordingly

  if _dst_rct.Left < 0 then begin
    dtmp := src_rct.Left
            + ( 0 - _dst_rct.Left )
            * src_rct.Width / _dst_rct.Width ;
    src_rct.Left  := Trunc( dtmp ) ;
    dst_rct.Left  := Round(
                       ( src_rct.Left - dtmp )
                       * _dst_rct.Width / src_bmp_w
                     ) ;
  end ;

  if _dst_rct.Right > dst_bmp_w then begin
    dtmp := src_rct.Right
            - ( _dst_rct.Right - dst_bmp_w )
            * src_bmp_w / _dst_rct.Width ;
    src_rct.Right := Trunc( dtmp ) ;
    dst_rct.Right := dst_bmp_w
                     - Round(
                        ( dtmp - src_rct.Right - 1 )
                        * _dst_rct.Width / src_bmp_w
                       ) ;
  end ;

  if _dst_rct.Top < 0 then begin
    dtmp := src_rct.Top
            + ( 0 - _dst_rct.Top )
            * src_bmp_h / _dst_rct.Height ;
    src_rct.Top := Trunc( dtmp ) ;
    dst_rct.Top := Round(
                    ( src_rct.Top - dtmp )
                    * _dst_rct.Height / src_bmp_h
                   ) ;
  end ;

  if _dst_rct.Bottom > dst_bmp_h then begin
    dtmp := src_rct.Bottom
            - ( _dst_rct.Bottom - dst_bmp_h )
            * src_bmp_h / _dst_rct.Height ;
    src_rct.Bottom := Round( dtmp ) ;
    dst_rct.Bottom := dst_bmp_h
                      - Round(
                          ( dtmp - src_rct.Bottom - 1 )
                          * _dst_rct.Height / src_bmp_h
                        ) ;
  end ;


  if src_rct.Width  < 1 then exit ;
  if src_rct.Height < 1 then exit ;
  if dst_rct.Width  < 1 then exit ;
  if dst_rct.Height < 1 then exit ;

  // store all scanlines; accessing scanline is not very fast
  SetLength( ar_src_lin, src_bmp_h ) ;
  for src_y := 0 to src_bmp_h - 1 do begin
    ar_src_lin[ src_y ] := IntPtr( src_bmp.ScanLine[ src_y ] ) ;
  end;


  if ( Abs ( dst_rct.Width  - src_rct.Width  ) <= 1  )and
     ( Abs ( dst_rct.Height - src_rct.Height ) <= 1 )
  then begin
    src_y := src_rct.Top ;
    for dst_y := dst_rct.Top to dst_rct.Bottom -1 do begin
      if dst_y < 0 then
        continue ;
      if dst_y >= dst_bmp_h then
        continue ;
      if src_y >= src_bmp_h then
        continue ;

      dst_lin := IntPtr( dst_bmp.ScanLine[ dst_y ] ) + 4 * Max( 0, dst_rct.Left ) ;

      src_lin := ar_src_lin[ src_y + 0 ] + 4 * src_rct.Left ;

      for dst_x := dst_rct.Left to dst_rct.Right -1 do begin
        if dst_x < 0 then
          continue ;
        if dst_x >= dst_bmp_w then
          continue ;

        cl := PCardinal( src_lin )^ ;
        src_a := PByte( src_lin + 3 )^ ;

        if cl = 0 then begin
          // do nothing - transparent
        end
        else
        if src_a = 255 then begin
          PCardinal( dst_lin )^ := cl ;
        end
        else
        if PCardinal( dst_lin )^ = 0 then begin
          if src_a = 0 then
            PCardinal( dst_lin )^ := cl or $FF000000 // fix for pure GDI32
          else
            PCardinal( dst_lin )^ := cl ;
        end
        else begin

          src_b := PByte( src_lin + 0 )^ ;
          src_g := PByte( src_lin + 1 )^ ;
          src_r := PByte( src_lin + 2 )^ ;

          if src_a = 0 then
            src_alpha := $FF
          else
            src_alpha := src_a ;

          dst_alpha := 256 - src_alpha ;

          dst_b := PByte(  dst_lin + 0 )^ ;
          dst_g := PByte(  dst_lin + 1 )^ ;
          dst_r := PByte(  dst_lin + 2 )^ ;
          dst_a := PByte(  dst_lin + 3 )^ ;

          PByte(  dst_lin + 0 )^ := Byte(src_b * src_alpha shr 8 + dst_b * dst_alpha shr 8) ;
          PByte(  dst_lin + 1 )^ := Byte(src_g * src_alpha shr 8 + dst_g * dst_alpha shr 8) ;
          PByte(  dst_lin + 2 )^ := Byte(src_r * src_alpha shr 8 + dst_r * dst_alpha shr 8) ;
          PByte(  dst_lin + 3 )^ := Byte(src_a * src_alpha shr 8 + dst_a * dst_alpha shr 8) ;
        end;

        dst_lin := dst_lin + 4 ;
        src_lin := src_lin + 4 ;
      end;
      src_y := src_y + 1 ;
    end;
  end
  else begin
    scale_x := dst_rct.Width  / ( src_rct.Width  + 1 ) ;
    scale_y := dst_rct.Height / ( src_rct.Height + 1 ) ;

    for dst_y := dst_rct.Top to dst_rct.Bottom -1 do begin
      dst_y_dscaled := ( dst_y - dst_rct.Top ) / scale_y ;
      dst_y_iscaled := Trunc( dst_y_dscaled );
      src_y := dst_y_iscaled + src_rct.Top ;

      if dst_y < 0 then
        continue ;
      if dst_y >= dst_bmp_h then
        continue ;

      dst_lin := IntPtr( dst_bmp.ScanLine[ dst_y ] ) + 4 * Max( 0, dst_rct.Left ) ;

      for dst_x := dst_rct.Left to dst_rct.Right -1 do begin
        if dst_x < 0 then
          continue ;
        if dst_x >= dst_bmp_w then
          continue ;

        dst_x_dscaled := ( dst_x - dst_rct.Left ) / scale_x;
        dst_x_iscaled := Trunc( dst_x_dscaled );

        src_x := dst_x_iscaled + src_rct.Left ;

        if src_y + 1 >= src_bmp_h then
            continue ;
        if src_x + 1 >= src_bmp_w then
          continue ;

        src_lin := ar_src_lin[ src_y + 0 ] + 4 * src_x ;

        cl := PCardinal( src_lin )^ ;
        src_a := PByte( src_lin + 3 )^ ;

        if cl = 0 then begin
          // do nothing - transparent
        end
        else
        if src_a = 255 then begin
          PCardinal( dst_lin )^ := cl ;
        end
        else
        if PCardinal( dst_lin )^ = 0 then begin
          if src_a = 0 then
            PCardinal( dst_lin )^ := cl or $FF000000 // fix for pure GDI323
          else
            PCardinal( dst_lin )^ := cl ;
        end
        else begin
          src_b := PByte( src_lin + 0 )^ ;
          src_g := PByte( src_lin + 1 )^ ;
          src_r := PByte( src_lin + 2 )^ ;

          if src_a = 0 then
            src_alpha := $FF
          else
            src_alpha := src_a ;

          dst_alpha := 256 - src_alpha ;

          dst_b := PByte(  dst_lin + 0 )^ ;
          dst_g := PByte(  dst_lin + 1 )^ ;
          dst_r := PByte(  dst_lin + 2 )^ ;
          dst_a := PByte(  dst_lin + 3 )^ ;

          PByte(  dst_lin + 0 )^ := Byte(src_b * src_alpha shr 8 + dst_b * dst_alpha shr 8) ;
          PByte(  dst_lin + 1 )^ := Byte(src_g * src_alpha shr 8 + dst_g * dst_alpha shr 8) ;
          PByte(  dst_lin + 2 )^ := Byte(src_r * src_alpha shr 8 + dst_r * dst_alpha shr 8) ;
          PByte(  dst_lin + 3 )^ := Byte(src_a * src_alpha shr 8 + dst_a * dst_alpha shr 8) ;
        end;

        dst_lin := dst_lin + 4 ;
      end;
    end;
  end;
end;


procedure TGIS_RendererVclAbstract.copyBitmap(
  const _src_bmp : TObject ;
  const _dst_bmp : TObject
) ;
var
  src_bmp       : VCL.Graphics.TBitmap ;
  dst_bmp       : VCL.Graphics.TBitmap ;
  x, y          : Integer ;

  dst_lin,
  src_lin       : IntPtr ;

  cl            : Cardinal ;
begin
  src_bmp := VCL.Graphics.TBitmap( _src_bmp ) ;
  dst_bmp := VCL.Graphics.TBitmap( _dst_bmp ) ;

  Assert( src_bmp.PixelFormat = pf32bit );
  Assert( dst_bmp.PixelFormat = pf32bit );

  Assert( src_bmp.Width  = dst_bmp.Width  );
  Assert( src_bmp.Height = dst_bmp.Height );

  for y := 0 to src_bmp.Height -1  do begin

    src_lin := IntPtr( src_bmp.ScanLine[ y ] ) ;
    dst_lin := IntPtr( dst_bmp.ScanLine[ y ] ) ;

    for x := 0 to src_bmp.Width -1 do begin
      PCardinal( dst_lin )^ := PCardinal( src_lin )^ ;

      dst_lin := dst_lin + 4 ;
      src_lin := src_lin + 4 ;
    end;
  end
end;

procedure TGIS_RendererVclAbstract.blendBitmaps(
  const _bmp_src      : TObject ;
  const _bmp_dst      : TObject ;
  const _transparency : Integer ;
  const _merge_alpha  : Boolean
);
var
  i       : SmallInt ;
  j       : SmallInt ;
  bmp_src,
  bmp_dst : VCL.Graphics.TBitmap ;
  buf_src,
  buf_dst : PByte ;
  sa      : Byte  ;
  da      : Byte  ;
  transp  : Byte  ;
  saneg   : SmallInt ;
begin
  bmp_src := VCL.Graphics.TBitmap( _bmp_src ) ;
  bmp_dst := VCL.Graphics.TBitmap( _bmp_dst ) ;

  assert( _transparency >= 0 ) ;
  assert( _transparency <= 100 ) ;
  assert( bmp_dst.PixelFormat = pf32bit ) ;
  assert( bmp_src.PixelFormat = pf32bit ) ;

  if ( bmp_src.Width  <> bmp_dst.Width  ) then exit ;
  if ( bmp_src.Height <> bmp_dst.Height ) then exit ;


  transp := Min( 255, Max( 0, _transparency ) * 255 div 100 ) ;

  if transp = 0 then
    exit ;

  if transp = 255 then begin
    for i := 0 to bmp_src.Height - 1 do
    begin
      buf_src  := bmp_src.ScanLine[i]  ;
      buf_dst  := bmp_dst.ScanLine[i] ;

      for j := 0 to bmp_src.Width -1 do
      begin
        if PCardinal( buf_dst )^ = 0 then begin
          if ( PCardinal( buf_src )^ <> 0 ) and  // fix for pure GDI32 bitmaps
             ( PByte( buf_src+3 )^   =  0 ) then
            PCardinal( buf_dst )^ := PCardinal( buf_src )^ or $FF000000
          else
            PCardinal( buf_dst )^ := PCardinal( buf_src )^ ;
        end
        else
        if PInteger( buf_src )^ <> 0 then begin

          sa := PByte( buf_src+3 )^ ;
          da := PByte( buf_dst+3 )^ ;

          if sa = 0 then // fix for pure GDI32 bitmaps
            sa := $FF ;
          if da = 0 then // fix for pure GDI32 bitmaps
            da := $FF ;

          saneg := 256 - sa ;

          PByte( buf_dst+2 )^ := PByte( buf_src+2 )^ +
                                 PByte( buf_dst+2 )^ * saneg shr 8 ;
          PByte( buf_dst+1 )^ := PByte( buf_src+1 )^ +
                                 PByte( buf_dst+1 )^ * saneg shr 8 ;
          PByte( buf_dst   )^ := PByte( buf_src   )^ +
                                 PByte( buf_dst   )^ * saneg shr 8 ;
          if _merge_alpha then
            PByte( buf_dst+3 )^ := sa                +
                                   da                * saneg shr 8
          else
            PByte( buf_dst+3 )^ := sa ;
        end ;

        buf_src := buf_src + 4 ;
        buf_dst := buf_dst + 4 ;
      end;
    end ;
  end
  else begin
    for i := 0 to bmp_src.Height - 1 do
    begin
      buf_src  := bmp_src.ScanLine[i]  ;
      buf_dst  := bmp_dst.ScanLine[i] ;

      for j := 0 to bmp_src.Width -1 do
      begin
        if PCardinal( buf_dst )^ = 0 then begin
          if PCardinal( buf_src )^ <> 0 then begin
            PByte( buf_dst+2 )^ := PByte( buf_src+2 )^ * transp shr 8 ;
            PByte( buf_dst+1 )^ := PByte( buf_src+1 )^ * transp shr 8 ;
            PByte( buf_dst   )^ := PByte( buf_src   )^ * transp shr 8 ;
            if PByte( buf_src+3 )^ <> 0 then
              PByte( buf_dst+3 )^ := PByte( buf_src+3 )^ * transp shr 8
            else
              PByte( buf_dst+3 )^ := 255                 * transp shr 8 ;
          end;
        end
        else
        if PInteger( buf_src )^ <> 0 then begin

          sa := PByte( buf_src+3 )^ ;
          da := PByte( buf_dst+3 )^ ;

          if sa = 0 then // fix for pure GDI32 bitmaps
            sa := $FF ;
          if ( da = 0 ) and                  // fix for pure GDI32 bitmaps
             ( PCardinal( buf_dst )^ <> 0 ) then
            da := $FF ;

          saneg := 256 - ( sa * transp shr 8 ) ;

          PByte( buf_dst+2 )^ := PByte( buf_src+2 )^ * transp shr 8 +
                                 PByte( buf_dst+2 )^ * saneg  shr 8 ;
          PByte( buf_dst+1 )^ := PByte( buf_src+1 )^ * transp shr 8 +
                                 PByte( buf_dst+1 )^ * saneg  shr 8 ;
          PByte( buf_dst   )^ := PByte( buf_src   )^ * transp shr 8 +
                                 PByte( buf_dst   )^ * saneg  shr 8 ;

          if _merge_alpha then
            PByte( buf_dst+3 )^ := sa                * transp shr 8 +
                                   da                * saneg  shr 8
          else begin
            PByte( buf_dst+3 )^ := $FF ;
          end;
        end;

        buf_src := buf_src + 4 ;
        buf_dst := buf_dst + 4 ;
      end
    end ;
  end;
end ;

procedure TGIS_RendererVclAbstract.blendBitmapsLbl(
  const _bmp_src      : TObject ;
  const _bmp_dst      : TObject ;
  const _merge_alpha  : Boolean
);
var
  i       : SmallInt ;
  j       : SmallInt ;
  bmp_src,
  bmp_dst : VCL.Graphics.TBitmap ;
  buf_src,
  buf_dst : PByte ;
  sa      : Byte  ;
  da      : Byte  ;
  transp  : Byte  ;
  saneg   : SmallInt ;
begin
  bmp_src := VCL.Graphics.TBitmap( _bmp_src ) ;
  bmp_dst := VCL.Graphics.TBitmap( _bmp_dst ) ;

  assert( bmp_dst.PixelFormat = pf32bit ) ;
  assert( bmp_src.PixelFormat = pf32bit ) ;

  if ( bmp_src.Width  <> bmp_dst.Width  ) then exit ;
  if ( bmp_src.Height <> bmp_dst.Height ) then exit ;


  for i := 0 to bmp_src.Height - 1 do
  begin
    buf_src  := bmp_src.ScanLine[i]  ;
    buf_dst  := bmp_dst.ScanLine[i] ;

    for j := 0 to bmp_src.Width -1 do
    begin
      if PCardinal( buf_dst )^ = 0 then begin
        if ( PCardinal( buf_src )^ <> 0 ) and  // fix for pure GDI32 bitmaps
           ( PByte( buf_src+3 )^   =  0 ) then
          PCardinal( buf_dst )^ := PCardinal( buf_src )^ or $FF000000
        else
          PCardinal( buf_dst )^ := PCardinal( buf_src )^ ;
      end
      else
      if PInteger( buf_src )^ <> 0 then begin

        sa := PByte( buf_src+3 )^ ;
        da := PByte( buf_dst+3 )^ ;

        if sa = 0 then // fix for pure GDI32 bitmaps
          sa := $FF ;
        if da = 0 then // fix for pure GDI32 bitmaps
          da := $FF ;

        saneg := 256 - sa ;

        PByte( buf_dst+2 )^ := PByte( buf_src+2 )^ +
                               PByte( buf_dst+2 )^ * saneg shr 8 ;
        PByte( buf_dst+1 )^ := PByte( buf_src+1 )^ +
                               PByte( buf_dst+1 )^ * saneg shr 8 ;
        PByte( buf_dst   )^ := PByte( buf_src   )^ +
                               PByte( buf_dst   )^ * saneg shr 8 ;
        if _merge_alpha then
          PByte( buf_dst+3 )^ := sa                +
                                 da                * saneg shr 8
        else
          PByte( buf_dst+3 )^ := $FF ;
      end ;

      buf_src := buf_src + 4 ;
      buf_dst := buf_dst + 4 ;
    end;
  end ;
end ;

procedure TGIS_RendererVclAbstract.renderBitmapInternal(
  const _handle     : TObject              ;
  const _background : VCL.Graphics.TBitmap ;
  const _bmp        : TGIS_Pixels          ;
  const _size       : TPoint               ;
  const _dst        : TRect                ;
  const _format     : TGIS_BitmapFormat    ;
  const _order      : TGIS_BitmapLinesOrder;
  const _on_empty   : Boolean
) ;
var
  s, dy, dx : Pointer ;
  stb  : Tbytes ;
  i, k : Integer ;
  sr, sg, sb, sa : Byte ;
  dr, dg, db, da : Byte ;
  saneg : SmallInt ;
  idat, ridat : Integer ;
  bh, bw : Integer ;

  x,y       : Integer ;
  step      : Integer ;
  revcolors : Boolean ;
  revlines  : Boolean ;
begin
  assert( _background <> nil ) ;

  if      _format = TGIS_BitmapFormat.Native then
          revcolors := False
  else if _format = TGIS_Bitmap.NativeFormat then
          revcolors := False
  else    revcolors := True ;

  if      _order = TGIS_BitmapLinesOrder.Native then
          revlines := False
  else if _order = TGIS_Bitmap.NativeLineOrder then
          revlines := False
  else    revlines := True ;


  bh := _background.Height ;
  bw := _background.Width ;

  if revlines then begin
    step := -1 ;
    y    :=  _size.Y - 1 ;
  end
  else begin
    step :=  1 ;
    y    :=  0 ;
  end;

  for i := 0 to _size.Y -1 do begin
    s := Pointer( IntPtr(_bmp) +4*_size.X*y ) ;

    if (_dst.Top + i >= 0) and (_dst.Top + i < bh) then begin
      dy := _background.ScanLine[_dst.Top + i] ;

      for k :=0 to _size.X -1 do begin
        dx := Pointer( IntPtr(dy) + 4*(_dst.Left+k) ) ;
        if ((_dst.Left+k) >= 0) and ((_dst.Left+k) < bw) then begin
          sa := PByte(IntPtr(s) +3)^ ;
          if sa <> $00 then begin
            if sa = $FF then begin
              idat := PInteger(s)^ ;
              if revcolors then begin
                sr := PByte(IntPtr(s) +2)^ ;
                sg := PByte(IntPtr(s) +1)^ ;
                sb := PByte(IntPtr(s) +0)^ ;

                PInteger(dx)^ :=  Integer($FF000000 ) or
                                  (Integer(sr) shl  0) or
                                  (Integer(sg) shl  8) or
                                  (Integer(sb) shl 16) ;
              end
              else
                PInteger(dx)^ := idat ;
            end
            else
            if sa = $0 then begin
              // do nothing
            end
            else begin
              sr := PByte(IntPtr(s) +2)^ ;
              sg := PByte(IntPtr(s) +1)^ ;
              sb := PByte(IntPtr(s) +0)^ ;

              if _on_empty then begin
                dr := sr  ;
                dg := sg ;
                db := sb ;
                da := sa ;
              end
              else
              begin
                dr := PByte(IntPtr(dx) +2)^ ;
                dg := PByte(IntPtr(dx) +1)^ ;
                db := PByte(IntPtr(dx) +0)^ ;
                da := PByte(IntPtr(dx) +3)^ ;

                if ( da = 0 ) and
                   (
                     ( dr <> 0 ) and
                     ( dg <> 0 ) and
                     ( db <> 0 )
                    )
                then
                  da := $FF ;

                saneg  := 256 - sa ;

                // premultimply source
                sr := Byte( sr * sa shr 8 ) ;
                sg := Byte( sg * sa shr 8 ) ;
                sb := Byte( sb * sa shr 8 ) ;

                dr := Byte( sr +  dr * saneg shr 8 ) ;
                dg := Byte( sg +  dg * saneg shr 8 ) ;
                db := Byte( sb +  db * saneg shr 8 ) ;
                da := Byte( sa +  da * saneg shr 8 ) ;
              end;

              PByte(IntPtr(dx) +3)^ :=  da ;

              if revcolors then begin
                PByte(IntPtr(dx) +0)^ := dr ;
                PByte(IntPtr(dx) +1)^ := dg ;
                PByte(IntPtr(dx) +2)^ := db ;
              end
              else begin
                PByte(IntPtr(dx) +0)^ := db ;
                PByte(IntPtr(dx) +1)^ := dg ;
                PByte(IntPtr(dx) +2)^ := dr ;
              end;
            end;
          end;
        end;
        s := Pointer(IntPtr(s) +4) ;
      end;
    end;
    Inc(y, step) ;
  end ;
end ;


{==================================== END =====================================}
end.

