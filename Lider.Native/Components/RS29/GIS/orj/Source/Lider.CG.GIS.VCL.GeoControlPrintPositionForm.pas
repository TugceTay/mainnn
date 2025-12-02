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
  Visual control for displaying shape attributes. Rename/Delete/Add dialog.<p>
}

unit VCL.GisControlPrintPositionForm ;
{$HPPEMIT '#pragma link "VCL.GisControlPrintPositionForm"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.Classes,
  VCL.Graphics,
  VCL.StdCtrls,
  VCL.Controls,
  VCL.ExtCtrls,

  GisRtl,
  GisResource,
  GisTypes,
  GisTypesUI,
  GisPrintBuilder,
  VCL.GisModalForm,
  VCL.GisValueValidatorHelper;

type

  {#gendoc:hide}
  TGIS_AnchorCache = class ( TGIS_ObjectDisposable )
    private
      bmpLeftMargin   : TBitmap ;
      clrLeftMargin   : TGIS_Color ;
      bmpRightMargin  : TBitmap ;
      clrRightMargin  : TGIS_Color ;
      bmpLeftBorder   : TBitmap ;
      clrLeftBorder   : TGIS_Color ;
      bmpRightBorder  : TBitmap ;
      clrRightBorder  : TGIS_Color ;
      bmpTopMargin    : TBitmap ;
      clrTopMargin    : TGIS_Color ;
      bmpBottomMargin : TBitmap ;
      clrBottomMargin : TGIS_Color ;
      bmpTopBorder    : TBitmap ;
      clrTopBorder    : TGIS_Color ;
      bmpBottomBorder : TBitmap ;
      clrBottomBorder : TGIS_Color ;

    protected
      procedure doDestroy ; override ;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ; overload ;

      /// <summary>
      ///   Returns a bitmap for the given anchor type.
      /// </summary>
      /// <param name="_anchor">
      ///   given anchor type
      /// </param>
      /// <returns>
      ///   Bitmap object.
      /// </returns>
      function  GetBitmap ( const _anchor : TGIS_PrintLayoutAnchor ;
                            const _color  : TGIS_Color
                          ) : VCL.Graphics.TBitmap ;
  end ;

  {#gendoc:hide}
  TGIS_PositionPanel = class( TPanel )
    private
      FValue : String ;
      FText : String ;
      FAnchor : String ;
      FAnchorCache : TGIS_AnchorCache ;
    protected
      procedure fset_Value( _value : String ) ;
    protected
      procedure Paint ; override ;
    public
      property Value : String                 read  FValue
                                              write fset_Value ;
      property AnchorCache : TGIS_AnchorCache read  FAnchorCache
                                              write FAnchorCache ;
  end ;

  /// <summary>
  ///   Visual form for managing print position properties.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlPrintTemplateForm.
  /// </remarks>
  TGIS_ControlPrintPositionForm = class( TGIS_ModalForm )
    private
      FMaxValue    : Integer ;
      FPPI         : Integer ;
      FPrintArea   : TRect ;
      FPageWidth   : Integer ;
      FPageHeight  : Integer ;
      FAnchorCache : TGIS_AnchorCache ;
    private
      lblValue   : TLabel ;
      edtValue   : TEdit ;
      vvedtValue : IGIS_ValueValidator ;
      lblUnit    : TLabel ;
      cmbUnit    : TComboBox ;
      lblAnchor  : TLabel ;
      cmbAnchor  : TComboBox ;

    private
      oldUnit    : String ;
      oldAnchor  : String ;
    private
      procedure setValidator ;
      function  getAnchorFromValue
                                 ( _value   : String )
                                 : String ;
      function  getTextFromValue ( _value   : String )
                                 : String ;
      function  toAbsolutePosition
                                 ( _value   : Integer ;
                                   _anchor  : String
                                 ) : Integer ;
      function  fromAbsolutePosition
                                 ( _value   : Integer ;
                                   _anchor  : String
                                 ) : Integer ;

    private
      procedure cmbUnitChange    ( _sender  : TObject ) ;
      procedure cmbAnchorChange  ( _sender  : TObject ) ;
      procedure cmbUnitDrawItem  ( _control : TWinControl ;
                                   _idx     : Integer ;
                                   _rect    : TRect   ;
                                   _state   : TOwnerDrawState
                                 ) ;
      procedure cmbAnchorDrawItem( _control : TWinControl ;
                                   _idx     : Integer ;
                                   _rect    : TRect   ;
                                   _state   : TOwnerDrawState
                                 ) ;

    protected

      /// <inheritdoc/>
      procedure btnOKClick       ( _sender  : TObject
                                 ) ; override ;

      /// <inheritdoc/>
      procedure initForm         ; override;

      /// <inheritdoc/>
      procedure initControls     ; override;

    public

      /// <summary>
      ///   Show dialog for defining an element position.
      /// </summary>
      /// <returns>
      ///   modal result
      /// </returns>
      function Execute    : Integer ; overload ;

      /// <summary>
      ///   Show dialog for defining an element position.
      /// </summary>
      /// <param name="_help">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function Execute    ( const _help  : TGIS_HelpEvent
                          ) : Integer ; overload ;

    public

      /// <summary>
      ///   Return position value set by the form.
      /// </summary>
      /// <returns>
      ///   position value
      /// </returns>
      function  GetValue  : Double ;

      /// <summary>
      ///   Return unit set by the form.
      /// </summary>
      /// <returns>
      ///   position's unit
      /// </returns>
      function  GetUnit   : String ;

      /// <summary>
      ///   Return anchor set by the form.
      /// </summary>
      /// <returns>
      ///   position's anchor
      /// </returns>
      function  GetAnchor : TGIS_PrintLayoutAnchor ;

      /// <summary>
      ///   Set input values for the form.
      /// </summary>
      /// <param name="_value">
      ///   position's value
      /// </param>
      /// <param name="_unit">
      ///   position's unit
      /// </param>
      /// <param name="_anchor">
      ///   position's anchor
      /// </param>
      procedure SetValues( const _value : Double ;
                           const _unit  : String ;
                           const _anchor : TGIS_PrintLayoutAnchor
                         ) ; overload ;

    public
      /// <summary>
      ///   Maximal value.
      /// </summary>
      property MaxValue    : Integer read  FMaxValue
                                     write FMaxValue ;
      /// <summary>
      ///   Printer PPI.
      /// </summary>
      property PPI         : Integer read  FPPI
                                     write FPPI ;
      /// <summary>
      ///   Print area.
      /// </summary>
      property PrintArea   : TRect   read  FPrintArea
                                     write FPrintArea ;
      /// <summary>
      ///   Page width.
      /// </summary>
      property PageWidth   : Integer read  FPageWidth
                                     write FPageWidth ;
      /// <summary>
      ///   Page height.
      /// </summary>
      property PageHeight  : Integer read  FPageHeight
                                     write FPageHeight ;
      /// <summary>
      ///   Anchor cache.
      /// </summary>
      property AnchorCache : TGIS_AnchorCache
                                     read  FAnchorCache
                                     write FAnchorCache ;

  end;

//##############################################################################
implementation

uses
  Math,
  SysUtils,
  System.UITypes,

  GisPrintUtils,
  VCL.GisFramework,
  VCL.GisControlHelper ;

//==============================================================================
// TGIS_AnchorCache
//==============================================================================

  constructor TGIS_AnchorCache.Create ;
  begin
    inherited ;
    bmpLeftMargin   := nil ;
    bmpRightMargin  := nil ;
    bmpLeftBorder   := nil ;
    bmpRightBorder  := nil ;
    bmpTopMargin    := nil ;
    bmpBottomMargin := nil ;
    bmpTopBorder    := nil ;
    bmpBottomBorder := nil ;
  end ;

  procedure TGIS_AnchorCache.doDestroy ;
  begin
    FreeObject( bmpLeftMargin   ) ;
    FreeObject( bmpRightMargin  ) ;
    FreeObject( bmpLeftBorder   ) ;
    FreeObject( bmpRightBorder  ) ;
    FreeObject( bmpTopMargin    ) ;
    FreeObject( bmpBottomMargin ) ;
    FreeObject( bmpTopBorder    ) ;
    FreeObject( bmpBottomBorder ) ;
    inherited ;
  end;

  function TGIS_AnchorCache.GetBitmap(
    const _anchor : TGIS_PrintLayoutAnchor ;
    const _color  : TGIS_Color
  ) : VCL.Graphics.TBitmap ;
  var
    dim : Integer ;
    b   : Boolean ;
    buf : PByteArray ;

    procedure init_bmp(
      var _bmp : VCL.Graphics.TBitmap ;
          _dim : Integer
    ) ;
    var
      i : Integer ;
      k : Integer ;
    begin
      b := False ;
      if not Assigned( _bmp ) then begin
        _bmp := VCL.Graphics.TBitmap.Create ;
        _bmp.PixelFormat := pf32bit ;
        _bmp.SetSize( _dim, _dim ) ;
        _bmp.AlphaFormat := TAlphaFormat.afPremultiplied ;

        for i := 0 to _dim-1 do begin
          buf := _bmp.ScanLine[i] ;
          for k := 0 to _dim-1 do begin
            buf[4*k  ] := 0 ;
            buf[4*k+1] := 0 ;
            buf[4*k+2] := 0 ;
            buf[4*k+3] := 0 ;
          end ;
        end ;

        b := True ;
      end ;
    end ;

    procedure draw_arrow(
      _anchor : TGIS_PrintLayoutAnchor ;
      _bmp    : TBitmap
    ) ;
    var
      i, k : Integer ;
    begin
      case _anchor of
        TGIS_PrintLayoutAnchor.aLeftMargin,
        TGIS_PrintLayoutAnchor.aRightMargin,
        TGIS_PrintLayoutAnchor.aLeftBorder,
        TGIS_PrintLayoutAnchor.aRightBorder :
          begin
            for i := 0 to dim-1 do begin
              buf := _bmp.ScanLine[i] ;
              for k := 1 to dim-2 do begin
                if ( i = dim div 2 ) then begin
                  buf[4*k  ] := _color.B ;
                  buf[4*k+1] := _color.G ;
                  buf[4*k+2] := _color.R ;
                  buf[4*k+3] := _color.A ;
                end
              end;
            end;
          end;
        TGIS_PrintLayoutAnchor.aTopMargin,
        TGIS_PrintLayoutAnchor.aBottomMargin,
        TGIS_PrintLayoutAnchor.aTopBorder,
        TGIS_PrintLayoutAnchor.aBottomBorder :
          begin
            for i := 1 to dim-2 do begin
              buf := _bmp.ScanLine[i] ;
              for k := 0 to dim-1 do begin
                if ( k = dim div 2 ) then begin
                  buf[4*k  ] := _color.B ;
                  buf[4*k+1] := _color.G ;
                  buf[4*k+2] := _color.R ;
                  buf[4*k+3] := _color.A ;
                end
              end;
            end;
          end;
      end;
      case _anchor of
        TGIS_PrintLayoutAnchor.aLeftBorder,
        TGIS_PrintLayoutAnchor.aRightBorder :
          begin
            if _anchor = TGIS_PrintLayoutAnchor.aLeftBorder then k := 1
                                                            else k := dim - 2 ;
            for i := dim div 4 + 1 to dim * 3 div 4 - 1 do begin
              buf := _bmp.ScanLine[i] ;
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
            end;
          end;
        TGIS_PrintLayoutAnchor.aTopBorder,
        TGIS_PrintLayoutAnchor.aBottomBorder :
          begin
            if _anchor = TGIS_PrintLayoutAnchor.aTopBorder then i := 1
                                                           else i := dim - 2 ;
            buf := _bmp.ScanLine[i] ;
            for k := dim div 4 + 1 to dim * 3 div 4 - 1 do begin
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
            end;
          end;
      end;
      case _anchor of
        TGIS_PrintLayoutAnchor.aLeftMargin,
        TGIS_PrintLayoutAnchor.aLeftBorder :
          begin
            for k := dim - 2 downto dim - 4 do begin
              buf := _bmp.ScanLine[dim div 2 - (dim - k) + 1] ;
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
              buf := _bmp.ScanLine[dim div 2 + (dim - k) - 1] ;
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
            end ;
          end ;
        TGIS_PrintLayoutAnchor.aRightMargin,
        TGIS_PrintLayoutAnchor.aRightBorder :
          begin
            for k := 2 to 4 do begin
              buf := _bmp.ScanLine[dim div 2 - k + 1] ;
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
              buf := _bmp.ScanLine[dim div 2 + k - 1] ;
              buf[4*k  ] := _color.B ;
              buf[4*k+1] := _color.G ;
              buf[4*k+2] := _color.R ;
              buf[4*k+3] := _color.A ;
            end;
          end ;
        TGIS_PrintLayoutAnchor.aTopMargin,
        TGIS_PrintLayoutAnchor.aTopBorder :
          begin
            k := dim div 2 ;
            for i := dim - 2 downto dim - 4 do begin
              buf := _bmp.ScanLine[i] ;
              buf[4*(k-(dim-i)+1)  ] := _color.B ;
              buf[4*(k-(dim-i)+1)+1] := _color.G ;
              buf[4*(k-(dim-i)+1)+2] := _color.R ;
              buf[4*(k-(dim-i)+1)+3] := _color.A ;
              buf[4*(k+(dim-i)-1)  ] := _color.B ;
              buf[4*(k+(dim-i)-1)+1] := _color.G ;
              buf[4*(k+(dim-i)-1)+2] := _color.R ;
              buf[4*(k+(dim-i)-1)+3] := _color.A ;
            end;
          end ;
        TGIS_PrintLayoutAnchor.aBottomMargin,
        TGIS_PrintLayoutAnchor.aBottomBorder :
          begin
            k := dim div 2 ;
            for i := 2 to 4 do begin
              buf := _bmp.ScanLine[i] ;
              buf[4*(k-i+1)  ] := _color.B ;
              buf[4*(k-i+1)+1] := _color.G ;
              buf[4*(k-i+1)+2] := _color.R ;
              buf[4*(k-i+1)+3] := _color.A ;
              buf[4*(k+i-1)  ] := _color.B ;
              buf[4*(k+i-1)+1] := _color.G ;
              buf[4*(k+i-1)+2] := _color.R ;
              buf[4*(k+i-1)+3] := _color.A ;
            end;
          end ;
      end;

    end;

  begin
    dim := 13 ;
    case _anchor of
      TGIS_PrintLayoutAnchor.aLeftMargin :
        begin
          init_bmp( bmpLeftMargin, dim ) ;
          if b or ( _color <> clrLeftMargin ) then begin
            clrLeftMargin := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aLeftMargin, bmpLeftMargin ) ;
          end ;
          Result := bmpLeftMargin ;
        end ;
      TGIS_PrintLayoutAnchor.aRightMargin :
        begin
          init_bmp( bmpRightMargin, dim ) ;
          if b or ( _color <> clrRightMargin ) then begin
            clrRightMargin := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aRightMargin, bmpRightMargin ) ;
          end ;
          Result := bmpRightMargin ;
        end ;
      TGIS_PrintLayoutAnchor.aTopMargin :
        begin
          init_bmp( bmpTopMargin, dim ) ;
          if b or ( _color <> clrTopMargin ) then begin
            clrTopMargin := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aTopMargin, bmpTopMargin ) ;
          end ;
          Result := bmpTopMargin ;
        end ;
      TGIS_PrintLayoutAnchor.aBottomMargin :
        begin
          init_bmp( bmpBottomMargin, dim ) ;
          if b or ( _color <> clrBottomMargin ) then begin
            clrBottomMargin := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aBottomMargin, bmpBottomMargin ) ;
          end ;
          Result := bmpBottomMargin ;
        end ;
      TGIS_PrintLayoutAnchor.aLeftBorder :
        begin
          init_bmp( bmpLeftBorder, dim ) ;
          if b or ( _color <> clrLeftBorder ) then begin
            clrLeftBorder := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aLeftBorder, bmpLeftBorder ) ;
          end ;
          Result := bmpLeftBorder ;
        end ;
      TGIS_PrintLayoutAnchor.aRightBorder :
        begin
          init_bmp( bmpRightBorder, dim ) ;
          if b or ( _color <> clrRightBorder ) then begin
            clrRightBorder := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aRightBorder, bmpRightBorder ) ;
          end ;
          Result := bmpRightBorder ;
        end ;
      TGIS_PrintLayoutAnchor.aTopBorder :
        begin
          init_bmp( bmpTopBorder, dim ) ;
          if b or ( _color <> clrTopBorder ) then begin
            clrTopBorder := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aTopBorder, bmpTopBorder ) ;
          end ;
          Result := bmpTopBorder ;
        end ;
      TGIS_PrintLayoutAnchor.aBottomBorder :
        begin
          init_bmp( bmpBottomBorder, dim ) ;
          if b or ( _color <> clrBottomBorder ) then begin
            clrBottomBorder := _color ;
            draw_arrow( TGIS_PrintLayoutAnchor.aBottomBorder, bmpBottomBorder ) ;
          end ;
          Result := bmpBottomBorder ;
        end ;
      else
        begin
          Result := nil ;
        end ;
    end ;
  end ;


//==============================================================================
// TGIS_PositionPanel
//==============================================================================

  procedure TGIS_PositionPanel.fset_Value( _value : String ) ;
  begin
    FValue := _value ;
    FAnchor := FValue[StringFirst] ;
    FText := FValue.Substring( 2 ) ;
  end ;

  procedure TGIS_PositionPanel.Paint ;
  var
    sz  : TSize ;
    r   : TRect ;
    bmp : TBitmap ;
    tf  : TTextFormat ;
  begin
    inherited ;
    if BiDiMode = TBiDiMode.bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;
    sz := Canvas.TextExtent( FText ) ;
    r := Rect( 2, 1, Width-4, Height-2 ) ;
    Canvas.TextRect( r, FText, tf ) ;
    bmp := FAnchorCache.GetBitmap( TGIS_PrintLayoutAnchor( StrToInt(FAnchor) ),
                                   GISColor( Canvas.Font.Color ) ) ;
    if BiDiMode = bdRightToLeft then
      Canvas.Draw( r.Right - sz.cx - 4 - bmp.Width, 2, bmp )
    else
      Canvas.Draw( r.Left + sz.cx + 4, 2, bmp ) ;
  end ;

//==============================================================================
// TGIS_ControlPrintPositionForm
//==============================================================================

  procedure TGIS_ControlPrintPositionForm.initForm ;
  begin
    Self.ClientHeight := 88 ;
    Self.ClientWidth := 293 ;
    Self.Name := 'TGIS_ControlPrintTemplatePosition' ;
    Self.MaxValue := 0 ;
  end ;

  procedure TGIS_ControlPrintPositionForm.initControls ;
  var
    anchor : TAnchors ;
  begin
    if Self.BiDiMode = bdRightToLeft then
      anchor := [akRight, akTop]
    else
      anchor := [akLeft, akTop] ;

    lblValue := TLabel.Create( Self ) ;
    lblValue.Parent := Self ;
    lblValue.Anchors := anchor ;
    lblValue.Top := 8 ;
    lblValue.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblValue, 8, 85 ) ;
    lblValue.AutoSize := False ;
    lblValue.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION_VALUE ) ;
    lblValue.FocusControl := edtValue ;

    lblUnit := TLabel.Create( Self ) ;
    lblUnit.Parent := Self ;
    lblUnit.Anchors := anchor ;
    lblUnit.Top := 8 ;
    lblUnit.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblUnit, 95, 74 ) ;
    lblUnit.AutoSize := False ;
    lblUnit.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION_UNIT ) ;
    lblUnit.FocusControl := cmbUnit ;

    lblAnchor := TLabel.Create( Self ) ;
    lblAnchor.Parent := Self ;
    lblAnchor.Anchors := anchor ;
    lblAnchor.Top := 8 ;
    lblAnchor.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnchor, 171, 114 ) ;
    lblAnchor.AutoSize := False ;
    lblAnchor.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION_ANCHOR ) ;
    lblAnchor.FocusControl := cmbAnchor ;

    edtValue := TEdit.Create( Self ) ;
    edtValue.Parent := Self ;
    edtValue.Anchors := anchor ;
    edtValue.Top := 24 ;
    edtValue.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtValue, 8, 85 ) ;
    edtValue.AutoSize := False ;
    edtValue.Ctl3D := True ;
    edtValue.ParentCtl3D := False ;
    edtValue.TabOrder := 0 ;

    vvedtValue := TGIS_ValueValidatorEditHelper.Create( edtValue ) ;

    cmbUnit := TComboBox.Create( Self ) ;
    cmbUnit.Parent := Self ;
    cmbUnit.Anchors := anchor ;
    cmbUnit.Top := 24 ;
    cmbUnit.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbUnit, 95, 74 ) ;
    cmbUnit.Style := csOwnerDrawFixed ;
    cmbUnit.Ctl3D := False ;
    cmbUnit.ParentCtl3D := False ;
    cmbUnit.Items.Text := TPL_SIZE_CM +#13#10+
                          TPL_SIZE_MM +#13#10+
                          TPL_SIZE_IN +#13#10+
                          TPL_SIZE_PT +#13#10+
                          TPL_SIZE_PX ;
    cmbUnit.TabOrder := 1 ;
    cmbUnit.OnChange := cmbUnitChange ;
    cmbUnit.OnDrawItem := cmbUnitDrawItem ;

    cmbAnchor := TComboBox.Create( Self ) ;
    cmbAnchor.Parent := Self ;
    cmbAnchor.Anchors := anchor ;
    cmbAnchor.Top := 24 ;
    cmbAnchor.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbAnchor, 171, 114 ) ;
    cmbAnchor.Style := csOwnerDrawFixed ;
    cmbAnchor.Ctl3D := False ;
    cmbAnchor.ParentCtl3D := False ;
    cmbAnchor.TabOrder := 2 ;
    cmbAnchor.OnChange := cmbAnchorChange ;
    cmbAnchor.OnDrawItem := cmbAnchorDrawItem ;

    oldUnit := '' ;
    oldAnchor := '' ;

  end ;

  procedure TGIS_ControlPrintPositionForm.cmbUnitChange(
    _sender : TObject
  ) ;
  var
    px : Integer ;
    v : Double ;
    new_unit : String ;
  begin
    setValidator ;
    new_unit := cmbUnit.Items[cmbUnit.ItemIndex] ;
    if ( oldUnit <> '' ) and
       ( oldUnit <> new_unit ) then begin
      px := TGIS_PrintUtils.ToPixels( vvedtValue.Value,
                                      TGIS_PrintUtils.TextToUnit( oldUnit ),
                                      FPPI
                                    ) ;
      v := TGIS_PrintUtils.FromPixels( px,
                                       TGIS_PrintUtils.TextToUnit( new_unit ),
                                       FPPI
                                     ) ;
      vvedtValue.Value := RoundTo( v, -vvedtValue.Precision ) ;
      vvedtValue.Valid ;
    end ;
    oldUnit := new_unit ;
  end;

  procedure TGIS_ControlPrintPositionForm.cmbAnchorChange(
    _sender : TObject
  ) ;
  var
    new_anchor : String ;
    v : Double ;
    px : Integer ;
  begin
    new_anchor := getAnchorFromValue(cmbAnchor.Items[cmbAnchor.ItemIndex]) ;
    if new_anchor <> oldAnchor then begin
      v := TGIS_PrintUtils.ToTwips( vvedtValue.Value.ToString, cmbUnit.Items[cmbUnit.ItemIndex], FPPI ) ;
      px := RoundS( TGIS_PrintUtils.FromTwips( RoundS(v), TPL_SIZE_PX, FPPI ) ) ;
      px := toAbsolutePosition( px, oldAnchor ) ;
      px := fromAbsolutePosition( px, new_anchor ) ;
      v := TGIS_PrintUtils.ToTwips( IntToStr(px), TPL_SIZE_PX, FPPI ) ;
      v := TGIS_PrintUtils.FromTwips( RoundS(v), cmbUnit.Items[cmbUnit.ItemIndex], FPPI ) ;
      vvedtValue.Value := RoundTo( v, -vvedtValue.Precision ) ;
      vvedtValue.Valid ;
    end ;
    oldAnchor := new_anchor ;
  end;

  procedure TGIS_ControlPrintPositionForm.cmbUnitDrawItem(
    _control : TWinControl ;
    _idx     : Integer ;
    _rect    : TRect   ;
    _state   : TOwnerDrawState
  ) ;
  var
    tf  : TTextFormat ;
    cmb : TComboBox ;
    txt : String ;
    rct : TRect ;
  begin
    if BiDiMode = TBiDiMode.bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    cmb := TComboBox( _control ) ;
    cmb.Canvas.FillRect( _rect ) ;

    txt := cmb.Items[_idx] ;
    rct := Rect( _rect.Left+2, _rect.Top+1, _rect.Right-2, _rect.Bottom-1 ) ;
    cmb.Canvas.TextRect( rct, txt, tf ) ;
  end ;

  procedure TGIS_ControlPrintPositionForm.cmbAnchorDrawItem(
    _control : TWinControl ;
    _idx     : Integer ;
    _rect    : TRect   ;
    _state   : TOwnerDrawState
  ) ;
  var
    tf : TTextFormat ;
    txt : String ;
    rct : TRect ;
    bmp : TBitmap ;
  begin
    if BiDiMode = TBiDiMode.bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    cmbAnchor.Canvas.FillRect( _rect ) ;

    txt := cmbAnchor.Items[_idx] ;
    bmp := FAnchorCache.GetBitmap(
             TGIS_PrintLayoutAnchor( StrToInt(getAnchorFromValue(txt)) ),
             GISColor( cmbAnchor.Canvas.Font.Color )
           ) ;
    txt := getTextFromValue(txt) ;
    rct := Rect( _rect.Left + 2 + bmp.Width + 3,
                 _rect.Top + 1,
                 _rect.Right - 2 - bmp.Width - 3,
                 _rect.Bottom - 1 ) ;
    cmbAnchor.Canvas.TextRect( rct, txt, tf ) ;
    if BiDiMode = TBiDiMode.bdRightToLeft then
      cmbAnchor.Canvas.Draw( _rect.Right - 2 - bmp.Width, _rect.Top + 2, bmp )
    else
      cmbAnchor.Canvas.Draw( _rect.Left + 2, _rect.Top + 2, bmp ) ;
  end;

  procedure TGIS_ControlPrintPositionForm.btnOKClick(
    _sender : TObject
  ) ;
  begin
    if ( edtValue.Color <> clRed ) and
       vvedtValue.Valid then begin
      vvedtValue.Value := vvedtValue.Value ;
      inherited ;
    end ;
  end ;

  function TGIS_ControlPrintPositionForm.Execute
    : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( hlp ) ;
  end ;

  function TGIS_ControlPrintPositionForm.Execute(
    const _help  : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    Result  := ShowModal ;
  end ;

  function TGIS_ControlPrintPositionForm.getAnchorFromValue(
    _value : String
  ) : String ;
  begin
    Result := _value[StringFirst] ;
  end ;

  function TGIS_ControlPrintPositionForm.getTextFromValue(
    _value : String
  ) : String ;
  begin
    Result := _value.Substring(1) ;
  end ;

  function TGIS_ControlPrintPositionForm.toAbsolutePosition(
    _value  : Integer ;
    _anchor : String
  ) : Integer ;
  begin
    Result := 0 ;
    case TGIS_PrintLayoutAnchor( StrToInt(_anchor) ) of
      TGIS_PrintLayoutAnchor.aLeftMargin :
        Result := FPrintArea.Left + _value ;
      TGIS_PrintLayoutAnchor.aRightMargin :
        Result := FPrintArea.Right - _value ;
      TGIS_PrintLayoutAnchor.aLeftBorder :
        Result := _value ;
      TGIS_PrintLayoutAnchor.aRightBorder :
        Result := FPageWidth - _value ;
      TGIS_PrintLayoutAnchor.aTopMargin :
        Result := FPrintArea.Top + _value ;
      TGIS_PrintLayoutAnchor.aBottomMargin :
        Result := FPrintArea.Bottom - _value ;
      TGIS_PrintLayoutAnchor.aTopBorder :
        Result := _value ;
      TGIS_PrintLayoutAnchor.aBottomBorder :
        Result := FPageHeight - _value ;
    end;
  end ;

  function TGIS_ControlPrintPositionForm.fromAbsolutePosition(
    _value  : Integer ;
    _anchor : String
  ) : Integer ;
  begin
    Result := 0 ;
    case TGIS_PrintLayoutAnchor( StrToInt(_anchor) ) of
      TGIS_PrintLayoutAnchor.aLeftMargin :
        Result := _value - FPrintArea.Left ;
      TGIS_PrintLayoutAnchor.aRightMargin :
        Result := FPrintArea.Right - _value ;
      TGIS_PrintLayoutAnchor.aLeftBorder :
        Result := _value ;
      TGIS_PrintLayoutAnchor.aRightBorder :
        Result := FPageWidth - _value ;
      TGIS_PrintLayoutAnchor.aTopMargin :
        Result := _value - FPrintArea.Top ;
      TGIS_PrintLayoutAnchor.aBottomMargin :
        Result := FPrintArea.Bottom - _value ;
      TGIS_PrintLayoutAnchor.aTopBorder :
        Result := _value ;
      TGIS_PrintLayoutAnchor.aBottomBorder :
        Result := FPageHeight - _value ;
    end ;
  end ;

  procedure TGIS_ControlPrintPositionForm.setValidator ;
  var
    it : String ;
  begin
    vvedtValue.MinVal := -GIS_MAX_SINGLE ;
    vvedtValue.MaxVal := GIS_MAX_SINGLE ;
    it := cmbUnit.Items[cmbUnit.ItemIndex] ;
    if it = TPL_SIZE_CM then
      vvedtValue.Precision := 2
    else
    if it = TPL_SIZE_MM then
      vvedtValue.Precision := 1
    else
    if it = TPL_SIZE_IN then
      vvedtValue.Precision := 2
    else
    if it = TPL_SIZE_PT then
      vvedtValue.Precision := 2
    else
    if it = TPL_SIZE_PX then
      vvedtValue.Precision := 0 ;
  end;

  function  TGIS_ControlPrintPositionForm.GetValue
    : Double ;
  begin
    Result := vvedtValue.Value ;
  end ;

  function  TGIS_ControlPrintPositionForm.GetUnit
    : String ;
  begin
    Result := cmbUnit.Text ;
  end ;

  function  TGIS_ControlPrintPositionForm.GetAnchor
    : TGIS_PrintLayoutAnchor ;
  begin
    Result := TGIS_PrintLayoutAnchor(
                StrToInt(
                  getAnchorFromValue( cmbAnchor.Items[cmbAnchor.ItemIndex] )
                )
              ) ;
  end ;

  procedure TGIS_ControlPrintPositionForm.SetValues(
    const _value : Double ;
    const _unit  : String ;
    const _anchor : TGIS_PrintLayoutAnchor
  ) ;
  var
    i : Integer;
  begin
    // unit
    cmbUnit.ItemIndex := cmbUnit.Items.IndexOf( _unit ) ;
    setValidator ;
    oldUnit := _unit ;

    // anchor
    case _anchor of
      TGIS_PrintLayoutAnchor.aLeftMargin,
      TGIS_PrintLayoutAnchor.aLeftBorder,
      TGIS_PrintLayoutAnchor.aRightMargin,
      TGIS_PrintLayoutAnchor.aRightBorder :
        begin
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aLeftMargin ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_LM, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aRightMargin ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_RM, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aLeftBorder ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_LB, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aRightBorder ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_RB, nil ) ;
        end;
      TGIS_PrintLayoutAnchor.aTopMargin,
      TGIS_PrintLayoutAnchor.aTopBorder,
      TGIS_PrintLayoutAnchor.aBottomMargin,
      TGIS_PrintLayoutAnchor.aBottomBorder :
        begin
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aTopMargin ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_TM, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aBottomMargin ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_BM, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aTopBorder ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_TB, nil ) ;
          cmbAnchor.AddItem( IntToStr( Integer( TGIS_PrintLayoutAnchor.aBottomBorder ) ) +
                             GIS_RS_TPL_DESIGNER_POSITION_ANCHOR_BB, nil ) ;
        end;
    end;
    for i := 0 to cmbAnchor.Items.Count-1 do begin
      if getAnchorFromValue(cmbAnchor.Items[i]) = IntToStr( Integer(_anchor) ) then begin
        cmbAnchor.ItemIndex := i ;
        break ;
      end;
    end ;
    oldAnchor := getAnchorFromValue(cmbAnchor.Items[cmbAnchor.ItemIndex]) ;

    // value
    vvedtValue.Value := _value ;
  end ;

//==================================== END =====================================
end.

