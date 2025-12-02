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
  VCL implementation of TGIS_PvlGrid
}

unit VCL.GisPvlGrid;

interface

{$INCLUDE GisInclude.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,
  System.Math,
  VCL.Controls,
  VCL.Forms,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Graphics,
  VCL.GisStyleRenderer,

  Data.DB,

  GisRtl,
  GisTypes,

  VCL.GisPvl,

  PVL.GisPvl,
  PVL.GisGrid;


//##############################################################################
implementation

uses
  Vcl.Grids,
  Vcl.Themes;

const CELL_MARGIN = 3 ;

type

  T_Edit = class( TCustomEdit )
    private
      FContent : TControl ;
    public
      constructor Create(AOwner: TComponent) ;
  end;

  T_Control =   class ( TCustomControl )
    public
      property OnResize;
      procedure CreateParams(var Params: TCreateParams); override ;
  end;

  T_GridControl = class ( TCustomControl )
    private
      oMaster : TGIS_PvlGrid ;
      oBmp    : TBitmap ;
    private
      procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
      procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    public
      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy ; override ;
  end;

  T_PvlGrid = class( TGIS_PvlControlVcl, IGIS_PvlGrid )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      procedure doDestroy        ;override;
    private
      oMaster         : TGIS_PvlGrid          ;
      oEdit           : T_Edit                ;
      oRenderer       : TGIS_StyleRendererVCL ;
      sLastValidated  : String                ;
      oGrid           : T_GridControl         ;
      oHScroll        : TScrollBar            ;
      oVScroll        : TScrollBar            ;
      bEditActive     : Boolean               ;
      oDataSet        : TDataSet              ;
      rSize           : TPoint                ;
    private
      procedure doResize;
      procedure doEnter( Sender: TObject);
      procedure doExit( Sender: TObject);
      procedure doScrollChange( Sender: TObject);
      procedure doPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
      procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure doMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

      procedure doControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState) ;
      procedure doControlKeyPress(Sender: TObject; var Key: Char) ;
      procedure doValidate(Sender: TObject);
      procedure doKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState) ;

    protected // interface API
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_ClientWidth  : Integer;
      function  fget_ClientHeight : Integer;
    protected
      /// <inheritdoc/>
      function  fget_DataSet      : TObject ;
      /// <inheritdoc/>
      procedure fset_DataSet      ( const _value      : TObject
                                  );
    public /// interface API
      procedure SetFocus          ;
      function  MeasureText       ( const _txt        : String
                                  ) : TPoint ;
      procedure DrawCell          ( const _col        : Integer;
                                    const _row        : Integer;
                                    const _txt        : String ;
                                    const _canvas     : TObject
                                  ) ;
      procedure DrawAll           ( const _canvas     : TObject
                                  ) ;
      procedure InvalidateControl ;
      procedure RealignControl    ;
      procedure BeginEdit         ( const _text       : String ;
                                    const _char       : Char
                                  ) ;
      procedure EndEdit           ( const _commit     : Boolean
                                  );
      procedure First             ;
      procedure MoveBy            ( const _dist       : Integer
                                  );

  end;

{$REGION 'T_PvlGrid'}

procedure T_PvlGrid.doCreate(const _context: TGIS_PvlContext);
begin
  oMaster := TGIS_PvlGrid( oParent ) ;

  oControl := T_Control.Create( TWinControl( _context.NativeParent ) );
  T_Control( oControl ).Parent := TWinControl( _context.NativeParent );
  T_Control( oControl ).Width := 100 ;
  T_Control( oControl ).Height := 100 ;
  T_Control( oControl ).OnEnter := doEnter ;

  {$IFDEF LEVEL_RX11_VCL}
    T_Control( oControl ).Color
      := StyleServices(T_Control( oControl )).GetSystemColor( clBtnFace ) ;
  {$ELSE}
    T_Control( oControl ).Color
      := StyleServices.GetSystemColor( clBtnFace ) ;
  {$ENDIF}


  oGrid := T_GridControl.Create( TControl( oControl ) );
  oGrid.Parent := TWinControl( oControl );
  oGrid.Ctl3D := False;
  oGrid.oMaster := oMaster;
  oGrid.OnMouseUp := doMouseUp;
  oGrid.OnMouseWheel := doMouseWheel;
  oGrid.TabStop := True ;

  oRenderer := TGIS_StyleRendererVCL.Create( oGrid );

  oHScroll := TScrollBar.Create( TControl( oControl ) );
  oHScroll.Parent := TWinControl( oControl );
  oHScroll.Visible := False;
  oHScroll.Kind := sbHorizontal ;
  oHScroll.OnChange := doScrollChange;
  oHScroll.TabStop := False ;

  oVScroll := TScrollBar.Create( TControl( oControl ) );
  oVScroll.Parent := TWinControl( oControl );
  oVScroll.Visible := False;
  oVScroll.Kind := sbVertical ;
  oVScroll.OnChange := doScrollChange;
  oVScroll.TabStop := False ;

  oGrid.OnKeyDown := doControlKeyDown ;
  oGrid.OnKeyPress := doControlKeyPress;

  oEdit := T_Edit.Create( TControl( oControl ) );
  oEdit.Parent := TWinControl( oControl );
  oEdit.Ctl3D := False;
  oEdit.BorderStyle := bsNone ;

  oEdit.OnExit := doValidate;
  oEdit.OnKeyUp := doKeyUp;
  oEdit.Visible := False;
  oEdit.OnExit := doExit;
end;

procedure T_PvlGrid.doDestroy;
begin
  inherited;
  FreeObject( oRenderer ) ;
end;

procedure T_PvlGrid.doResize;
begin
  rSize.X := fget_ClientWidth;
  rSize.Y := fget_ClientHeight;
  RealignControl ;
  oMaster.ScrollTo( RoundS( oHScroll.Position ), RoundS( oVScroll.Position ) ) ;
end;

procedure T_PvlGrid.doEnter( Sender: TObject);
begin
  SetFocus ;
end;

procedure T_PvlGrid.doExit( Sender: TObject);
begin
  oMaster.EndEdit(true) ;
end;

procedure T_PvlGrid.doScrollChange( Sender: TObject);
begin
  oMaster.ScrollTo( RoundS( oHScroll.Position ), RoundS( oVScroll.Position ) ) ;
  oGrid.Repaint;
end;

function T_PvlGrid.fget_ClientWidth
  : Integer;
begin
  Result := TruncS( oGrid.Width / oContext.PPIFix );
end;

function T_PvlGrid.fget_ClientHeight
  : Integer;
begin
  Result := TruncS( oGrid.Height / oContext.PPIFix );
end;

function T_PvlGrid.fget_DataSet
  : TObject ;
begin
  Result := oDataSet ;
end;

procedure T_PvlGrid.fset_DataSet(
  const _value  : TObject
) ;
var
  i        : Integer             ;
  col      : TGIS_PVlGridColumn  ;
  row      : TGIS_PVlGridRow     ;
  old_evnt : TDataSetNotifyEvent ;
  fld_def  : TFieldDef           ;
begin
  if _value = oDataSet then
    exit ;

  if Assigned( _value ) then begin
    old_evnt := TDataSet( _value ).AfterScroll ;
    TDataSet( _value ).AfterScroll := nil;
  end
  else
    old_evnt := nil ;

  try
    oMaster.BeginUpdate ;
    oDataSet := TDataSet( _value ) ;

    oMaster.SetSize( 0, 0 );

    if not Assigned( oDataSet ) then
      exit ;

    oMaster.RowsHeader := TGIS_PVlGridHeader.First;
    oMaster.ColumnsHeader := TGIS_PVlGridHeader.Auto;

    for i := 0 to oDataSet.FieldCount -1 do begin
      col := oMaster.AddColumn ;

      fld_def := oDataSet.FieldDefs[i] ;
      col.Caption := fld_def.Name;

      col.ReadOnly := faReadonly in fld_def.Attributes ;

      case fld_def.DataType of
        ftUnknown :
          begin
            Assert( false );
          end;
        ftBoolean :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Center ;
            col.FieldType := TGIS_FieldType.Boolean ;
          end;
        ftSingle ,
        ftFloat ,
        ftExtended :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Right ;
            col.FieldType := TGIS_FieldType.Float ;
            col.FieldDecimal := fld_def.Precision ; //?
          end;
        ftAutoInc :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Right ;
            col.FieldType := TGIS_FieldType.Number ;
            col.ReadOnly := True ;
          end;
        ftCurrency :
          begin
             Assert( false );
          end;
        ftString ,
        ftWideString ,
        ftFixedWideChar ,
        ftMemo ,
        ftFmtMemo ,
        ftWideMemo,
        ftFixedChar :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Left ;
            col.FieldType := TGIS_FieldType.String ;
          end;
        ftVariant :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Left ;
            col.FieldType := TGIS_FieldType.String ;
          end;
        ftSmallint ,
        ftInteger ,
        ftWord ,
        ftLargeint ,
        ftLongWord ,
        ftShortint ,
        ftByte :
          begin
            col.Align := TGIS_PvlGridCellAlignment.Right ;
            col.FieldType := TGIS_FieldType.Number ;
          end;
        ftDate ,
        ftTime ,
        ftDateTime ,
        ftTimeStamp ,
        ftOraTimeStamp :
          begin
            Assert( false );
          end;
        ftBCD :
          begin
            Assert( false );
          end;
        ftParadoxOle :
          begin
            Assert( false );
          end;
        ftDBaseOle :
          begin
            Assert( false );
          end;
        ftADT :
          begin
            Assert( false );
          end;
        ftFMTBcd :
          begin
            Assert( false );
          end;
        ftOraInterval :
          begin
            Assert( false );
          end;
        ftTimeStampOffset :
          begin
            Assert( false );
          end;
      end;
    end;

    while not oDataSet.Eof do begin
      row := oMaster.AddRow ;
      for i := 0 to oDataSet.FieldCount -1 do begin
        row.Column[i+1] := oDataSet.Fields[i].Value ;
      end;

      oDataSet.MoveBy(1) ;
    end;

    oMaster.ScrollTop;
  finally
    if Assigned( oDataSet ) then
      oDataSet.AfterScroll := old_evnt ;
    oMaster.EndUpdate ;
  end;

  oMaster.autoSize ;
end;

procedure T_PvlGrid.SetFocus ;
begin
  oGrid.SetFocus ;
end;

function T_PvlGrid.MeasureText(
  const _txt : String
) : TPoint ;
begin
  Result.X := RoundS( ( oGrid.Canvas.TextWidth( _txt )
                        + 2 * CELL_MARGIN
                      )
                      / oContext.PPIFix
                    ) ;

  Result.Y := RoundS( ( oGrid.Canvas.TextHeight( _txt )
                        + 2 * CELL_MARGIN
                      )
                      / oContext.PPIFix
                    ) ;
end ;

procedure T_PvlGrid.DrawCell(
  const _col    : Integer ;
  const _row    : Integer ;
  const _txt    : String  ;
  const _canvas : TObject
);
var
  r      : TRect ;
  rf     : TRectF ;
  rt     : TRectF ;
  rct    : TRect ;
  text   : String ;
  cnv    : TCanvas ;
  ostl   : TCustomStyleServices ;
  odtl   : TThemedElementDetails;
  usestl : Boolean ;
  saved  : Integer ;

  bordercolor : TColor ;
  fillcolor   : TColor ;
  fontcolor   : TColor ;
  tmpcolor    : TColor ;
begin
  r := oMaster.CellRect( _col, _row );

  rf := RectF( r.Left   - 1,
               r.Top    - 1,
               r.Right  ,
               r.Bottom
             ) ;

  rt := RectF( r.Left   + CELL_MARGIN,
               r.Top    + CELL_MARGIN,
               r.Right  - CELL_MARGIN,
               r.Bottom - CELL_MARGIN
             ) ;

  cnv := TCanvas( _canvas) ;

  cnv.Font := oEdit.Font; // Ensure PPI

  text := _txt ;
  rct := rt.Round ;

  {$IFDEF LEVEL_RX11_VCL}
    ostl := StyleServices(ogrid);
  {$ELSE}
    ostl := StyleServices;
  {$ENDIF}

  usestl := False ;
  if _row = 0 then begin
    // columns header
    cnv.Rectangle( rf.Round );

    if ostl.Enabled then begin
      odtl := ostl.GetElementDetails( tgFixedCellNormal) ;
      ostl.GetElementColor(ostl.GetElementDetails(tgFixedCellNormal), ecBorderColor, bordercolor);
      usestl := True ;
    end
    else begin
      fillcolor := clBtnFace ;
      bordercolor := clGray ;
      fontcolor := clBtnText ;
    end;
  end
  else
  if _col = 0 then begin
    if ostl.Enabled then begin
      odtl := ostl.GetElementDetails( tgFixedCellNormal) ;
      ostl.GetElementColor(ostl.GetElementDetails(tgFixedCellNormal), ecBorderColor, bordercolor);
      usestl := True ;
    end
    else begin
      fillcolor := clBtnFace ;
      bordercolor := clGray ;
      fontcolor := clBtnText ;
    end;
  end
  else begin
    if //( oGrid.Focused or oEdit.Focused )and
      ( ( oMaster.ActiveCell.X = _col ) or oMaster.RowSelect )
      and
      ( oMaster.ActiveCell.Y = _row )
    then begin
      // editor

      if bEditActive and ( oMaster.ActiveCell.X = _col ) then begin

        case oMaster.Column[ _col ].Align of
          TGIS_PvlGridCellAlignment.Left:
            oEdit.Alignment := TAlignment.taLeftJustify;
          TGIS_PvlGridCellAlignment.Center:
            oEdit.Alignment := TAlignment.taCenter;
          TGIS_PvlGridCellAlignment.Right:
            oEdit.Alignment := TAlignment.taRightJustify;
        end;

        if ostl.Enabled then begin
          odtl := ostl.GetElementDetails( tgCellSelected ) ;
          ostl.GetElementColor(ostl.GetElementDetails(tgCellSelected), ecBorderColor, bordercolor);
          usestl := True ;
        end
        else begin
          fillcolor := clActiveCaption ;
          bordercolor := clGray ;
          fontcolor := clMenuText ;
        end;
      end
      else begin
        if ostl.Enabled then begin
          odtl := ostl.GetElementDetails( tgCellSelected ) ;
          ostl.GetElementColor(ostl.GetElementDetails(tgCellSelected), ecBorderColor, bordercolor);
          usestl := True ;
        end
        else begin
          fillcolor := clActiveCaption ;
          bordercolor := clGray ;
          fontcolor := clMenuText ;
        end;
      end;
    end
    else begin
      // regular cell

      fillcolor := clWindow ;
      fontcolor := clWindowText ;

      if ostl.Enabled then begin
        odtl := ostl.GetElementDetails( tgCellNormal ) ;
        if ostl.GetElementColor(odtl, ecTextColor, tmpcolor) and (tmpcolor <> clNone) then
          fontcolor := tmpcolor
        else
          fontcolor := clWindowText;
        if ostl.GetElementColor(odtl, ecFillColor, tmpcolor) and (tmpcolor <> clNone) then
          fillcolor := tmpcolor
        else
          fillcolor := clWindow;
        ostl.GetElementColor(ostl.GetElementDetails(tgCellNormal), ecBorderColor, bordercolor);
      end
      else begin
        fillcolor := clWindow ;
        bordercolor := clGray ;
        fontcolor := clWindowText ;
      end;
    end;

  end;

  if usestl then begin
    saved := SaveDC(TCanvas(_canvas).Handle);
    try
      ostl.DrawElement(cnv.Handle, odtl, rf.Round, rf.Round);
    finally
      RestoreDC(cnv.Handle, saved);
    end;
    if ostl.GetElementColor(odtl, ecTextColor, tmpcolor) and (tmpcolor <> clNone) then
      cnv.Font.Color := tmpcolor
    else
      cnv.Font.Color := clWindowText;
    cnv.Brush.Style := bsClear;
  end
  else begin
    cnv.Brush.Style := bsSolid ;
    cnv.Brush.Color := fillcolor ;
    cnv.FillRect( rf.Round );
    cnv.Font.Color := fontcolor ;
  end;

  cnv.Pen.Style := psSolid ;
  cnv.Pen.Color := bordercolor ;
  cnv.Pen.Width := 1 ;
  cnv.Rectangle( rf.Round );

  if _row = 0  then
    cnv.TextRect( rct, text, [ tfCenter ] )
  else begin
    if oMaster.Column[ _col ].FieldType = TGIS_FieldType.Boolean then begin
      oRenderer.CreateContext( cnv, False );
      oRenderer.DrawCheckBox( UpperCase( _txt ).Equals( UpperCase( 'True' ) ) , r )
    end
    else
      case oMaster.Column[ _col ].Align of
        TGIS_PvlGridCellAlignment.Left:
            cnv.TextRect( rct, text, [ tfLeft ] );
        TGIS_PvlGridCellAlignment.Center:
          cnv.TextRect( rct, text, [ tfCenter ] );
        TGIS_PvlGridCellAlignment.Right:
          cnv.TextRect( rct, text, [ tfRight ] );
      end;
  end;
end;

procedure T_PvlGrid.DrawAll(
  const _canvas: TObject
);
var
  cnv : TCanvas ;
  r   : TRect   ;
begin
  if ( rSize.X <> fget_ClientWidth  ) or
     ( rSize.Y <> fget_ClientHeight ) then
  begin
    doResize;
    exit;
  end;

  // clear screen
  cnv := TCanvas( _canvas );
  cnv.Brush.Style := bsSolid ;
  cnv.Brush.Color := clBtnFace ;

  cnv.FillRect( RectF( 0, 0,
                       fget_Width * oMaster.Context.PPIFix,
                       fget_Height * oMaster.Context.PPIFix
                     ).Round
              );

  r := oMaster.CellEditRect;

  if not ( oMaster.Column[ oMaster.ActiveCell.X ].FieldType = TGIS_FieldType.Boolean ) then
  begin
    oEdit.Visible := bEditActive;
    if oEdit.Visible then
      oEdit.SetFocus;

    oEdit.Left       := r.Left    + 1 + oGrid.Left ;
    oEdit.Top        := r.Top     + 1 + oGrid.Top  ;
    oEdit.Width      := r.Width   - 2 ;
    oEdit.Height     := r.Height  - 2 ;
  end;
end;


procedure T_PvlGrid.InvalidateControl;
begin
  oGrid.Repaint;
end;

procedure T_PvlGrid.RealignControl;
var
  iborder : Integer ;
  bchange : Boolean ;
begin
  iborder := 0 ;

  if TPanel( oControl ).BorderWidth <> 0 then
    iborder := RoundS( TPanel( oControl ).BorderWidth ) ;

  oGrid.Left := iborder ;
  oGrid.Left := iborder ;

  // adjust scrollable area

  repeat
    // repeat because vscroll can change hscroll and opisite

    bchange := False ;

    oVScroll.Visible := ( oMaster.ScrollRect.Height < oMaster.ScrollMax.Y )
                        and ( oMaster.RowsCount > 0 );

    // position scollbar and set scope
    if oVScroll.Visible then begin
      if oGrid.Width <> T_Control( oControl ).ClientWidth - oVScroll.Width - 2*iborder then begin
        oGrid.Width := T_Control( oControl ).ClientWidth - oVScroll.Width - 2*iborder;
        bchange := True ;
      end;

      oVScroll.OnChange := nil ;
      oVScroll.Position := oMaster.ScrollPos.Y ;
      oVScroll.Max :=  oMaster.ScrollMax.Y - oMaster.ScrollRect.Height ;
      oVScroll.ClientRect.Height := oMaster.ScrollRect.Height ;
      oVScroll.SmallChange := oMaster.RowsHeight ;
      oVScroll.LargeChange := oMaster.ScrollRect.Height ;
      oVScroll.Left := oGrid.Left + oGrid.Width ;
      oVScroll.Top := oGrid.Top ;
      oVScroll.Height := oGrid.Height ;
      oVScroll.OnChange := doScrollChange ;
    end
    else
      oGrid.Width := T_Control( oControl ).ClientWidth - 2*iborder;

    oHScroll.Visible := ( oMaster.ScrollRect.Width < oMaster.ScrollMax.X )
                        and ( oMaster.ColumnsCount > 0 );

    // position scollbar and set scope
    if oHScroll.Visible then begin
      if oGrid.Height <> T_Control( oControl ).ClientHeight - oHScroll.Height - 2*iborder then begin
        oGrid.Height := T_Control( oControl ).ClientHeight - oHScroll.Height - 2*iborder;
        bchange := True ;
      end;

      oHScroll.OnChange := nil ;
      oHScroll.Position  := oMaster.ScrollPos.X ;
      oHScroll.Max :=  oMaster.ScrollMax.X - oMaster.ScrollRect.Width ;
      oHScroll.ClientRect.Width := oMaster.ScrollRect.Width ;
      oHScroll.SmallChange := oMaster.RowsHeight ;
      oHScroll.LargeChange := oMaster.ScrollRect.Width ;
      oHScroll.Left := oGrid.Left ;
      oHScroll.Top := oGrid.Top + oGrid.Height;
      oHScroll.Width := oGrid.Width ;
      oHScroll.OnChange := doScrollChange ;
    end
    else
      oGrid.Height := T_Control( oControl ).ClientHeight - 2*iborder ;

  until bchange = false;

end;

procedure T_PvlGrid.BeginEdit(
  const _text : String;
  const _char : Char
);
begin
  if not ( oMaster.Column[ oMaster.ActiveCell.X ].FieldType = TGIS_FieldType.Boolean ) then
  begin
    oEdit.Text := _text ;
  //  oEdit.Caret := Length( oEdit.Text ) ;

    sLastValidated := oEdit.Text ;
  end;

  bEditActive := True ;
  InvalidateControl ;
end;

procedure T_PvlGrid.EndEdit(
  const _commit : Boolean
);
var
  val : Variant ;
begin
  if not bEditActive then
    exit;

  if _commit then begin
    try
      case oMaster.Column[oMaster.ActiveCell.X].FieldType of
        TGIS_FieldType.String:
          val := oEdit.Text;
        TGIS_FieldType.Number,
        TGIS_FieldType.Float:
          val := DotStrToFloat( oEdit.Text );
//        TGIS_FieldType.Boolean:
//          val := VarToBoolean( oEdit.Text ) ; //not an edit?
        TGIS_FieldType.&Date:
          val := VarToDateTime( oEdit.Text ) ;
      end;

      oMaster.Row[oMaster.ActiveCell.Y].Column[oMaster.ActiveCell.X] := val;

      if Assigned( oDataset ) then begin
        if oDataset.RecNo <> oMaster.ActiveCell.Y then
          oDataset.MoveBy( oDataset.RecNo - oMaster.ActiveCell.Y ) ;

        oDataSet.Edit;
        try
          oDataSet.Fields[oMaster.ActiveCell.X-1].Value := val ;
          oDataSet.UpdateRecord;
        except
          oDataSet.Cancel;
        end;
      end;

    except
      // something wrong with conversion;
    end;
  end;

  bEditActive := False ;
  if oEdit.Focused then
    oGrid.SetFocus;

  InvalidateControl ;
end;

procedure T_PvlGrid.First;
begin
  if Assigned( oDataSet ) then
    oDataSet.First;
end;

procedure T_PvlGrid.MoveBy(
  const _dist: Integer
) ;
begin
  if Assigned( oDataSet ) then
    oDataSet.MoveBy( _dist ) ;
end;

procedure T_PvlGrid.doMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cell : TPoint ;
begin
  oGrid.SetFocus;
  cell := oMaster.CellByLocation( RoundS(x),RoundS(y) );

  if ( cell.X = -1 ) and ( cell.Y = -1 ) then
    exit ;

  if oMaster.Column[ cell.X ].FieldType = TGIS_FieldType.Boolean then begin
    oMaster.Cell[ cell.X, cell.Y ] := not VarToBoolean( oMaster.Cell[ cell.X, cell.Y ] ) ;
    oMaster.ActiveCell := cell ;
    InvalidateControl ;
  end else begin
    if ( cell = oMaster.ActiveCell) then begin
      if ( cell.X > 0 ) and ( cell.Y > 0 ) then
        oMaster.BeginEdit( '', Char(0) );
    end
    else begin
      oMaster.EndEdit( True );
      oMaster.ActiveCell := cell ;
      InvalidateControl ;
    end;
  end;
end;

procedure T_PvlGrid.doMouseWheel(
  Sender: TObject;
  Shift: TShiftState;
  WheelDelta: Integer;
  MousePos: TPoint;
  var Handled: Boolean
);
begin
  oMaster.ScrollBy( 0, -WheelDelta );
end;

procedure T_PvlGrid.doPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  oMaster.DrawAll( Canvas );
end;

procedure T_PvlGrid.doControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState) ;
begin
  if ( Key = vkUp ) and ( Shift = [] ) then
    oMaster.ScrollUp
  else
  if ( Key = vkDown ) and ( Shift = [] ) then
    oMaster.ScrollDown
  else
  if ( Key = vkPrior ) and ( Shift = [] ) then
    oMaster.ScrollPageUp
  else
  if ( Key = vkNext ) and ( Shift = [] ) then
    oMaster.ScrollPageDown
  else
  if ( Key = vkLeft ) and ( Shift = [] ) then
    oMaster.ScrollLeft
  else
  if ( Key = vkRight ) and ( Shift = [] ) then
    oMaster.ScrollRight
  else
  if ( Key = vkHome ) and ( Shift = [] ) then
    oMaster.ScrollHome
  else
  if ( Key = vkEnd ) and ( Shift = [] ) then
    oMaster.ScrollEnd
  else
  if ( Key = vkHome ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollTop
  else
  if ( Key = vkEnd ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollBottom
  else
  if ( Key = vkF2 ) then
    oMaster.BeginEdit( '', Char(0) )
  else
  if ( Key = vkRight ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollNext
  else
  if ( Key = vkLeft ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollPrevious
  else
  if ( Key = vkUp ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollUp
  else
  if ( Key = vkDown ) and ( Shift = [ssCtrl] ) then
    oMaster.ScrollDown
  else
  if ( Key = vkReturn ) then
    oMaster.BeginEdit( '', Char(0) )
  ;
end;

procedure T_PvlGrid.doControlKeyPress(Sender: TObject; var Key: Char) ;
begin
  if Key >= #32 then
    oMaster.BeginEdit( '', Key )
  else
    Key := #0;
end;

procedure T_PvlGrid.doValidate(Sender: TObject);
begin
  if not ( oMaster.Column[ oMaster.ActiveCell.X ].FieldType = TGIS_FieldType.Boolean ) then
    if not oMaster.Column[ oMaster.ActiveCell.X ].Validate( oEdit.Text ) then
      oEdit.Text := sLastValidated
    else
      sLastValidated := oEdit.Text;
end;

procedure T_PvlGrid.doKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState) ;
begin
  if ( Key = vkUp ) and ( Shift = [] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollUp ;
  end
  else
  if ( Key = vkDown ) and ( Shift = [] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollDown ;
  end
  else
  if ( Key = vkPrior ) and ( Shift = [] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollPageUp ;
  end
  else
  if ( Key = vkNext ) and ( Shift = [] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollPageDown ;
  end
  else
  if ( Key = vkRight ) and ( Shift = [ssCtrl] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollNext;
    oMaster.BeginEdit( #0, #0 ) ;
  end
  else
  if ( Key = vkLeft ) and ( Shift = [ssCtrl] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollPrevious;
    oMaster.BeginEdit( #0, #0 ) ;
  end
  else
  if ( Key = vkUp ) and ( Shift = [ssCtrl] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollUp;
    oMaster.BeginEdit( #0, #0 ) ;
  end
  else
  if ( Key = vkDown ) and ( Shift = [ssCtrl] ) then begin
    oMaster.EndEdit( True ) ;
    oMaster.ScrollDown;
    oMaster.BeginEdit( #0, #0 ) ;
  end
  else
  if Key = vkReturn then begin
    oMaster.EndEdit( True ) ;
  end
  else
  if Key = vkEscape then begin
    oMaster.EndEdit( false ) ;
  end;

  InvalidateControl ;
end;

{$ENDREGION 'T_PvlGrid'}

{$REGION 'T_Edt'}

constructor T_Edit.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

  Self.Alignment := taCenter ;
end;

{$ENDREGION 'T_Edit'}

{$REGION 'T_GridControl'}

constructor T_GridControl.Create(AOwner: TComponent);
begin
  oBmp := TBitmap.Create ;
  inherited Create(Aowner);
end;

destructor T_GridControl.Destroy ;
begin
  FreeObject( oBmp ) ;
  inherited ;
end;

procedure T_GridControl.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result
                    or DLGC_WANTCHARS
                    or DLGC_WANTARROWS
                    or DLGC_WANTALLKEYS;
end;

procedure T_GridControl.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1 ;
end;

procedure T_GridControl.Paint;
begin
  oBmp.SetSize( Width, Height );

  oMaster.DrawAll( oBmp.Canvas ) ;
  Canvas.Draw( 0, 0, oBmp ) ;
end;

{$ENDREGION 'T_GridControl'}

{ T_Control }

procedure T_Control.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
  end;
end;

initialization
  RegisterPVLPlatformControl( 'Grid', T_PvlGrid );

//==================================== END =====================================
end.
                                                        4
