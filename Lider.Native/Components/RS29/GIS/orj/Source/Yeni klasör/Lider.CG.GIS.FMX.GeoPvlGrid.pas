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
  FMX implementation of TGIS_PvlGrid
}

unit FMX.GisPvlGrid;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.Variants,
  FMX.Types,
  FMX.Forms,
  FMX.Objects,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.Edit,
  Data.DB,

  GisRtl,
  GisTypes,
  GisSymbol,
  FMX.GisStyleRenderer,

  PVL.GisPvl,
  FMX.GisPvl,
  PVL.GisGrid;


//##############################################################################
implementation

const CELL_MARGIN = 3 ;

type

  T_Edit = class( TCustomEdit )
    private
      FContent : TControl ;
    public
      constructor Create(AOwner: TComponent) ;
      procedure ApplyStyle; override;
      procedure FreeStyle; override;
      function GetDefaultStyleLookupName: string; override;
  end;

  T_PvlGrid = class( TGIS_PvlControlFmx, IGIS_PvlGrid )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      procedure doDestroy        ;override;
    private
      oMaster       : TGIS_PvlGrid          ;
      oEdit         : TCustomEdit           ;
      oRenderer     : TGIS_StyleRendererFMX ;
      sLastValid    : String                ;
      oGrid         : TControl              ;
      oHScroll      : TScrollBar            ;
      oVScroll      : TScrollBar            ;
      bEditActive   : Boolean               ;
      oDataSet      : TDataSet              ;
    private
      procedure doResize( Sender: TObject);
      procedure doExit( Sender: TObject);
      procedure doScrollChange( Sender: TObject);
      procedure doPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
      procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
      procedure doMouseWheel(Sender: TObject;Shift: TShiftState;WheelDelta: Integer;var Handled: Boolean);
      procedure doControlKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState) ;
      procedure doValidate(Sender: TObject; var Text: string);
      procedure doKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState) ;

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
  oControl := TRectangle.Create( TFmxObject( _context.NativeParent ) );
  TRectangle( oControl ).Parent := TFmxObject( _context.NativeParent );
  TRectangle( oControl ).OnResize := doResize ;
  TRectangle( oControl ).Fill.Kind := TBrushKind.None ;
  TRectangle( oControl ).TabStop := False ;

  oGrid := TControl.Create( TControl( oControl ) );
  oGrid.Parent := TControl( oControl );
  oGrid.OnPaint := doPaint;
  oGrid.OnMouseUp := doMouseUp;
  oGrid.OnMouseWheel := doMouseWheel;
  oGrid.ClipChildren := True;
  oGrid.CanFocus := True;
  oGrid.CanFocus := True;
  oGrid.TabStop := True ;

  oRenderer := TGIS_StyleRendererFMX.Create( oGrid ) ;

  oHScroll := TScrollBar.Create( TControl( oControl ) );
  oHScroll.Parent := TControl( oControl );
  oHScroll.Visible := False;
  oHScroll.Orientation := TOrientation.Horizontal ;
  oHScroll.OnChange := doScrollChange;
  oHScroll.Height := 15;
  oHScroll.TabStop := False ;

  oVScroll := TScrollBar.Create( TControl( oControl ) );
  oVScroll.Parent := TControl( oControl );
  oVScroll.Visible := False;
  oVScroll.Orientation := TOrientation.Vertical ;
  oVScroll.OnChange := doScrollChange;
  oVScroll.Width := 15;
  oVScroll.TabStop := False ;

  oGrid.OnKeyDown := doControlKeyDown ;

  oEdit := T_Edit.Create( TControl( oControl ) );
  oEdit.Parent := TControl( oControl );

  oEdit.OnValidating := doValidate;
  oEdit.OnKeyUp := doKeyUp;
  oEdit.Visible := False;
  oEdit.OnExit := doExit;

  oMaster := TGIS_PvlGrid( oParent ) ;
end;

procedure T_PvlGrid.doDestroy;
begin
  inherited;
  FreeObject( oRenderer );
end;

procedure T_PvlGrid.doResize( Sender: TObject);
begin
  RealignControl ;
  oMaster.ScrollTo( RoundS( oHScroll.Value ), RoundS( oVScroll.Value ) ) ;
end;

procedure T_PvlGrid.doExit( Sender: TObject);
begin
  oMaster.EndEdit(true) ;
end;


procedure T_PvlGrid.doScrollChange( Sender: TObject);
begin
  oMaster.ScrollTo( RoundS( oHScroll.Value ), RoundS( oVScroll.Value ) ) ;
  oGrid.Repaint;
end;

function T_PvlGrid.fget_ClientWidth
  : Integer;
begin
  Result := TruncS( oGrid.Width );
end;

function T_PvlGrid.fget_ClientHeight
  : Integer;
begin
  Result := TruncS( oGrid.Height );
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
    Assert( _value is TDataSet ) ;

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
  Result.X := RoundS( oGrid.Canvas.TextWidth( _txt ) )
              + 2 * CELL_MARGIN ;
  Result.Y := RoundS( oGrid.Canvas.TextHeight( _txt ) )
              + 2 * CELL_MARGIN ;
end ;

procedure T_PvlGrid.DrawCell(
  const _col    : Integer ;
  const _row    : Integer ;
  const _txt    : String  ;
  const _canvas : TObject
);
var
  r : TRect ;
  rf : TRectF ;
  rt : TRectF ;
  rchk : TRectF ;
  cnv : TCanvas ;
begin
  r := oMaster.CellRect( _col, _row );

  rf := RectF( r.Left-0.5, r.Top-0.5, r.Right-0.5, r.Bottom-0.5) ;
  rt := RectF( r.Left+CELL_MARGIN, r.Top+CELL_MARGIN, r.Right-CELL_MARGIN, r.Bottom-CELL_MARGIN ) ;

  cnv := TCanvas( _canvas) ;

  if _row = 0 then begin
    // columns header
    cnv.Fill.Kind := TBrushKind.Solid ;
    cnv.Fill.Color := TAlphaColorRec.Silver ;
    cnv.FillRect( rf, 0, 0, [], 1);

    cnv.Stroke.Kind := TBrushKind.Solid ;
    cnv.Stroke.Color := TAlphaColorRec.Gray ;
    cnv.DrawRect( rf, 0, 0, AllCorners, 1);

    cnv.Fill.Color := TAlphaColorRec.Black ;

    cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Center );
  end
  else
  if _col = 0 then begin
    // rows header

    cnv.Fill.Kind := TBrushKind.Solid ;
    cnv.Fill.Color := TAlphaColorRec.Lightgray ;
    cnv.FillRect( rf, 0, 0, [], 1);

    cnv.Stroke.Kind := TBrushKind.Solid ;
    cnv.Stroke.Color := TAlphaColorRec.Grey ;
    cnv.DrawRect( rf, 0, 0, AllCorners, 1);

    cnv.Fill.Color := TAlphaColorRec.Black ;


    case oMaster.Column[ _col ].Align of
      TGIS_PvlGridCellAlignment.Left:
        cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Leading );
      TGIS_PvlGridCellAlignment.Center:
        cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Center );
      TGIS_PvlGridCellAlignment.Right:
        cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Trailing );
    end;
  end
  else begin
    if //( oGrid.IsFocused or oEdit.IsFocused )and
       ( ( oMaster.ActiveCell.X = _col ) or oMaster.RowSelect )
       and
       ( oMaster.ActiveCell.Y = _row )
    then begin
      // editor

      if bEditActive and ( oMaster.ActiveCell.X = _col ) then begin
        cnv.Fill.Kind := TBrushKind.Solid ;
        cnv.Fill.Color := TAlphaColorRec.White ;
        cnv.FillRect( rf, 0, 0, [], 1);

        cnv.Stroke.Kind := TBrushKind.Solid ;
        cnv.Stroke.Color := TAlphaColorRec.Steelblue ;
        cnv.Stroke.Thickness := 1 ;
        cnv.DrawRect( rf, 0, 0, AllCorners, 1);

        case oMaster.Column[ _col ].Align of
          TGIS_PvlGridCellAlignment.Left:
            oEdit.TextAlign := TTextAlign.Leading;
          TGIS_PvlGridCellAlignment.Center:
            oEdit.TextAlign := TTextAlign.Center;
          TGIS_PvlGridCellAlignment.Right:
            oEdit.TextAlign := TTextAlign.Trailing;
        end;

      end
      else begin
        cnv.Fill.Kind := TBrushKind.Solid ;
        cnv.Fill.Color := TAlphaColorRec.Lightblue;
        cnv.FillRect( rf, 0, 0, [], 1);

        cnv.Stroke.Kind := TBrushKind.Solid ;
        cnv.Stroke.Color := TAlphaColorRec.Silver ;
        cnv.Stroke.Thickness := 1 ;
        cnv.DrawRect( rf, 0, 0, AllCorners, 1);

        cnv.Fill.Color := TAlphaColorRec.Black ;


        if oMaster.Column[ _col ].FieldType = TGIS_FieldType.Boolean then begin
        oRenderer.CreateContext( cnv, False ) ;
          try
            rchk := RectF(
              RoundS( r.Right / 2 - oRenderer.GetActualCheckBoxSize / 2 ),
              r.Top + CELL_MARGIN,
              RoundS( r.Right / 2 - oRenderer.GetActualCheckBoxSize / 2 ) + oRenderer.GetActualCheckBoxSize ,
              r.Bottom - CELL_MARGIN
            );
            oRenderer.DrawCheckBox( UpperCase( _txt ).Equals( UpperCase( 'True' ) ) , ConvertRect( rchk ) )
          finally
            oRenderer.FreeContext ;
          end;
        end
        else
          case oMaster.Column[ _col ].Align of
            TGIS_PvlGridCellAlignment.Left:
              cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Leading );
            TGIS_PvlGridCellAlignment.Center:
              cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Center );
            TGIS_PvlGridCellAlignment.Right:
              cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Trailing );
          end;

      end;

    end
    else begin
      // regular cell
      cnv.Fill.Kind := TBrushKind.Solid ;
      cnv.Fill.Color := TAlphaColorRec.White ;
      cnv.FillRect( rf, 0, 0, [], 1);

      cnv.Stroke.Kind := TBrushKind.Solid ;
      cnv.Stroke.Color := TAlphaColorRec.LightGrey ;
      cnv.Stroke.Thickness := 1 ;
      cnv.DrawRect( rf, 0, 0, AllCorners, 1);

      cnv.Fill.Color := TAlphaColorRec.Black ;

      if oMaster.Column[ _col ].FieldType = TGIS_FieldType.Boolean then begin
        oRenderer.CreateContext( cnv, False ) ;
        try
          rchk := RectF(
            RoundS( r.Right / 2 - oRenderer.GetActualCheckBoxSize / 2 ),
            r.Top + CELL_MARGIN,
            RoundS( r.Right / 2 - oRenderer.GetActualCheckBoxSize / 2 ) + oRenderer.GetActualCheckBoxSize ,
            r.Bottom - CELL_MARGIN
          );
          oRenderer.DrawCheckBox( UpperCase( _txt ).Equals( UpperCase( 'True' ) ) , ConvertRect( rchk ) )
        finally
          oRenderer.FreeContext ;
        end;
      end
      else
        case oMaster.Column[ _col ].Align of
          TGIS_PvlGridCellAlignment.Left:
            cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Leading );
          TGIS_PvlGridCellAlignment.Center:
            cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Center );
          TGIS_PvlGridCellAlignment.Right:
            cnv.FillText( rt, _txt, False, 1, [], TTextAlign.Trailing );
        end;
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
  // clear screen
  cnv := TCanvas( _canvas );
  cnv.Fill.Kind := TBrushKind.Solid ;
  cnv.Fill.Color := TAlphaColorRec.White ;

  cnv.FillRect( RectF( 0, 0, fget_Width, fget_Height), 0, 0, [], 1);

  r := oMaster.CellEditRect;

  if not ( oMaster.Column[ oMaster.ActiveCell.X ].FieldType = TGIS_FieldType.Boolean ) then
  begin
    oEdit.Visible := bEditActive;
    if oEdit.Visible then
      oEdit.SetFocus;

    oEdit.Position.X := r.Left + 1 + oGrid.Position.X;
    oEdit.Position.Y := r.Top  + 1 + oGrid.Position.Y;
    oEdit.Width      := r.Width   - 2 ;
    oEdit.Height     := r.Height  - 2 ;
  end;

//  RealignControl ;
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

  if TRectangle( oControl ).Stroke.Kind <> TBrushKind.None then
    iborder := RoundS( TRectangle( oControl ).Stroke.Thickness ) ;

  oGrid.Position.X := iborder ;
  oGrid.Position.Y := iborder ;

  // adjust scrollable area

  repeat
    // repeat because shwil vscroll can change hscroll and opisite

    bchange := False ;

    oVScroll.Visible := ( oMaster.ScrollRect.Height < oMaster.ScrollMax.Y )
                        and ( oMaster.RowsCount > 0 );

    // position scollbar and set scope
    if oVScroll.Visible then begin
      if oGrid.Width <> TRectangle( oControl ).Width - oVScroll.Width - 2*iborder then begin
        oGrid.Width := TRectangle( oControl ).Width - oVScroll.Width - 2*iborder;
        bchange := True ;
      end;

      oGrid.Width := TRectangle( oControl ).Width - oVScroll.Width - 2*iborder ;
      oVScroll.OnChange := nil ;
      oVScroll.Value := oMaster.ScrollPos.Y ;
      oVScroll.Max :=  oMaster.ScrollMax.Y ;
      oVScroll.ViewportSize := oMaster.ScrollRect.Height ;
      oVScroll.SmallChange := oMaster.RowsHeight ;
      oVScroll.Position.X := oGrid.Position.X + oGrid.Width ;
      oVScroll.Position.Y := oGrid.Position.Y ;
      oVScroll.Height := oGrid.Height ;
      oVScroll.OnChange := doScrollChange ;
    end
    else
      oGrid.Width := TRectangle( oControl ).Width - 2*iborder;

    oHScroll.Visible := ( oMaster.ScrollRect.Width < oMaster.ScrollMax.X )
                        and ( oMaster.ColumnsCount > 0 );


    // position scollbar and set scope
    if oHScroll.Visible then begin
      if oGrid.Height <> TRectangle( oControl ).Height - oHScroll.Height - 2*iborder then begin
        oGrid.Height := TRectangle( oControl ).Height - oHScroll.Height - 2*iborder;
        bchange := True ;
      end;

      oHScroll.OnChange := nil ;
      oHScroll.Value := oMaster.ScrollPos.X ;
      oHScroll.Max :=  oMaster.ScrollMax.X ;
      oHScroll.ViewportSize := oMaster.ScrollRect.Width ;
      oHScroll.SmallChange := oMaster.RowsHeight ;
      oHScroll.Position.X := oGrid.Position.X ;
      oHScroll.Position.Y := oGrid.Position.Y + oGrid.Height;
      oHScroll.Width := oGrid.Width ;
      oHScroll.OnChange := doScrollChange ;
    end
    else
      oGrid.Height := TRectangle( oControl ).Height - 2*iborder ;

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
    oEdit.CaretPosition := Length( oEdit.Text ) ;

    sLastValid := oEdit.Text ;
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
//        TGIS_FieldType.Boolean: //not an edit
//          val := VarToBoolean(  ) ;
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
  if oEdit.IsFocused then
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
  Shift: TShiftState; X, Y: Single);
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
  var Handled: Boolean
);
begin
  oMaster.ScrollBy(0, -WheelDelta );
end;

procedure T_PvlGrid.doPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  oMaster.DrawAll( Canvas );
end;

procedure T_PvlGrid.doControlKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState) ;
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
  else
  if ( Ord(KeyChar) >= 32) then
    oMaster.BeginEdit( '', KeyChar );
end;

procedure T_PvlGrid.doValidate(Sender: TObject; var Text: string);
begin
  if not oMaster.Column[ oMaster.ActiveCell.X ].Validate( Text ) then
    Text := sLastValid
  else
    sLastValid := Text;
end;

procedure T_PvlGrid.doKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState) ;
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

{ T_Edit }

constructor T_Edit.Create(AOwner: TComponent);
begin
  inherited Create( AOwner ) ;

//  oRect := TRectangle.Create( AOwner ) ;


  TextSettings.HorzAlign := TTextAlign.Center ;

end;

const
  StrTextcellstyle = 'textcellstyle'; // do not localize
  StrContent = 'content'; // do not localize

procedure T_Edit.ApplyStyle;
begin
  inherited;
  if FindStyleResource<TControl>(StrContent, FContent) then
  begin
//    FOldMargins := FContent.Margins.Rect;
//    FContent.Margins.Rect := Rect( 10, 10, 100, 100 );
  end;
end;

procedure T_Edit.FreeStyle;
begin
  inherited;
  if FContent <> nil then
  begin
//    FContent.Margins.Rect := FOldMargins;
    FContent := nil;
  end;
end;

function T_Edit.GetDefaultStyleLookupName: string;
begin
  Result := StrTextcellstyle;
end;

initialization
  RegisterPVLPlatformControl( 'Grid', T_PvlGrid );

//==================================== END =====================================
end.
                                                        4
