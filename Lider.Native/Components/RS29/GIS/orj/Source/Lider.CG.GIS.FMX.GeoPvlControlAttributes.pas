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
  FMX implementation of TGIS_PvlControlAttributes
}

unit FMX.GisPvlControlAttributes ;
{$HPPEMIT '#pragma link "FMX.GisPvlControlAttributes"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,

  FMX.Types,
  FMX.Controls,
  GisCSBase,
  GisTypesUI,
  GisLayerVector,

  FMX.GisControlAttributes,
  FMX.GisFramework,
  FMX.GisPvl,

  PVL.GisPvl,
  PVL.GisControlAttributes;

implementation
type

  T_PvlControlAttributes = class ( TGIS_PvlControlFmx, IGIS_PvlControlAttributes )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );override;

      procedure doDestroy         ;override;
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      function  fget_FieldNameColumnWidth
                                    : Single ;
      procedure fset_FieldNameColumnWidth
                                    ( const _value  : Single
                                    ) ;
      function  fget_LayoutType     : TGIS_PvlAttibutesLayoutType ;
      procedure fset_LayoutType     ( const _value  : TGIS_PvlAttibutesLayoutType
                                    ) ;
      function  fget_RightToLeft    : Boolean ;
      procedure fset_RightToLeft    ( const _value  : Boolean
                                    );
      function  fget_RightToLeftFromTranslation
                                    : Boolean ;
      procedure fset_RightToLeftFromTranslation
                                    ( const _value  : Boolean
                                    );
      function  fget_ReadOnly       : Boolean ;
      procedure fset_ReadOnly       ( const _value  : Boolean
                                    ) ;
      function  fget_ShowBtnCancel  : Boolean ;
      procedure fset_ShowBtnCancel  ( const _value  : Boolean
                                    ) ;
      function  fget_ShowBtnOk      : Boolean ;
      procedure fset_ShowBtnOk      ( const _value  : Boolean
                                    ) ;
      function  fget_Units          : TGIS_CSUnits ;
      procedure fset_Units          ( const _value  : TGIS_CSUnits
                                    ) ;
      function  fget_UnitsEPSG      : Integer ;
      procedure fset_UnitsEPSG      ( const _value  : Integer
                                    ) ;
      function  fget_Font           : TGIS_Font ;
      procedure fset_Font           ( const _value  : TGIS_Font
                                    ) ;
    public

      /// <summary>
      ///   Clear attributes display. To avoid errors you should call this
      ///   before removing shapes, layer and closing viewer.
      /// </summary>
      procedure Clear               ;

      /// <summary>
      ///   Rebuild attributes display. TGIS_ControlAttributes does not
      ///   detect changing of layer structure, so after it call
      ///   Invalidate to rebuild data structure.
      /// </summary>
      procedure Invalidate          ;

      /// <summary>
      ///   Show attributes for a new shape.
      /// </summary>
      /// <param name="_shape">
      ///   new shape
      /// </param>
      procedure NewShape            ( const _shape  : TGIS_Shape
                                    ) ;

      /// <summary>
      ///   Show attributes for given _shape.
      /// </summary>
      /// <param name="_shape">
      ///   Shape object to display attribute filed for shape; nil to clear the
      ///   list
      /// </param>
      procedure ShowShape           ( const _shape  : TGIS_Shape
                                    ) ;

      /// <summary>
      ///   Show statistics for selected shapes.
      /// </summary>
      /// <param name="_layer">
      ///   layer taken into account
      /// </param>
      procedure ShowSelected        ( const _layer  : TGIS_LayerVector
                                    ) ;

      /// <summary>
      ///   Cancel changes made in the control.
      /// </summary>
      procedure CancelChanges       ;

      /// <summary>
      ///   Save changes made in the control.
      /// </summary>
      /// <returns>
      ///   True if layer updated successfully.
      /// </returns>
      function  SaveChanges         : Boolean ;

    protected // IGIS_Viewer property access routines

      /// <summary>
      ///   Copy information from the control to clipboard.
      /// </summary>
      procedure CopyToClipboard     ;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;
  end;

//==================================== END =====================================


function T_PvlControlAttributes.fget_FieldNameColumnWidth
  : Single ;
begin
  Result := TGIS_ControlAttributes( oControl ).FieldNameColumnWidth ;
end;

procedure T_PvlControlAttributes.fset_FieldNameColumnWidth(
  const _value: Single
) ;
begin
  TGIS_ControlAttributes( oControl ).FieldNameColumnWidth := _value ;
end;

function T_PvlControlAttributes.fget_LayoutType
  : TGIS_PvlAttibutesLayoutType ;
begin
  if TGIS_ControlAttributes( oControl ).LayoutType = TGIS_LayoutType.OneColumn then
    Result := TGIS_PvlAttibutesLayoutType.OneColumn
  else
    Result := TGIS_PvlAttibutesLayoutType.TwoColumns ;
end;

procedure T_PvlControlAttributes.fset_LayoutType(
  const _value: TGIS_PvlAttibutesLayoutType
) ;
begin
  if _value = TGIS_PvlAttibutesLayoutType.OneColumn then
    TGIS_ControlAttributes( oControl ).LayoutType := TGIS_LayoutType.OneColumn
  else
    TGIS_ControlAttributes( oControl ).LayoutType := TGIS_LayoutType.TwoColumns ;
end;

function T_PvlControlAttributes.fget_RightToLeft
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).BiDiMode = TBiDiMode.bdRightToLeft ;
end;

procedure T_PvlControlAttributes.fset_RightToLeft(
  const _value: Boolean
) ;
begin
  if _value then
    TGIS_ControlAttributes( oControl ).BiDiMode := TBiDiMode.bdRightToLeft
  else
    TGIS_ControlAttributes( oControl ).BiDiMode := TBiDiMode.bdLeftToRight ;
end;

function T_PvlControlAttributes.fget_RightToLeftFromTranslation
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).BiDiModeFromTranslation ;
end;

procedure T_PvlControlAttributes.fset_RightToLeftFromTranslation(
  const _value: Boolean
) ;
begin
  TGIS_ControlAttributes( oControl ).BiDiModeFromTranslation := _value ;
end;

function T_PvlControlAttributes.fget_ReadOnly
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).ReadOnly ;
end;

procedure T_PvlControlAttributes.fset_ReadOnly(
  const _value: Boolean
) ;
begin
  TGIS_ControlAttributes( oControl ).ReadOnly := _value ;
end;

function T_PvlControlAttributes.fget_ShowBtnCancel
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).ShowBtnCancel ;
end;

procedure T_PvlControlAttributes.fset_ShowBtnCancel(
  const _value: Boolean
) ;
begin
  TGIS_ControlAttributes( oControl ).ShowBtnCancel := _value ;
end;

function T_PvlControlAttributes.fget_ShowBtnOk
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).ShowBtnOk ;
end;

procedure T_PvlControlAttributes.fset_ShowBtnOk(
  const _value: Boolean
) ;
begin
  TGIS_ControlAttributes( oControl ).ShowBtnOk := _value ;
end;

function T_PvlControlAttributes.fget_Units
  : TGIS_CSUnits ;
begin
  Result := TGIS_ControlAttributes( oControl ).Units ;
end;

procedure T_PvlControlAttributes.fset_Units(
  const _value: TGIS_CSUnits
) ;
begin
  TGIS_ControlAttributes( oControl ).Units := _value ;
end;

function T_PvlControlAttributes.fget_UnitsEPSG
  : Integer ;
begin
  Result := TGIS_ControlAttributes( oControl ).UnitsEPSG ;
end;

procedure T_PvlControlAttributes.fset_UnitsEPSG(
  const _value: Integer
) ;
begin
  TGIS_ControlAttributes( oControl ).UnitsEPSG := _value ;
end;

function T_PvlControlAttributes.fget_Font
  : TGIS_Font ;
begin
  Result := FontHelper.DoCreateFromFont( TGIS_ControlAttributes( oControl ).Font ) ;
end;

procedure T_PvlControlAttributes.fset_Font(
  const _value: TGIS_Font
) ;
begin
  TGIS_ControlAttributes( oControl ).Font := FMXFont( _value ) ;
end;

procedure T_PvlControlAttributes.Clear ;
begin
  TGIS_ControlAttributes( oControl ).Clear ;
end;

procedure T_PvlControlAttributes.Invalidate ;
begin
  TGIS_ControlAttributes( oControl ).Invalidate ;
end;

procedure T_PvlControlAttributes.NewShape(
  const _shape: TGIS_Shape
) ;
begin
  TGIS_ControlAttributes( oControl ).NewShape( _shape ) ;
end;

procedure T_PvlControlAttributes.ShowShape(
  const _shape: TGIS_Shape
) ;
begin
  TGIS_ControlAttributes( oControl ).ShowShape( _shape ) ;
end;

procedure T_PvlControlAttributes.ShowSelected(
  const _layer: TGIS_LayerVector
) ;
begin
  TGIS_ControlAttributes( oControl ).ShowSelected( _layer ) ;
end;

procedure T_PvlControlAttributes.CancelChanges ;
begin
  TGIS_ControlAttributes( oControl ).CancelChanges ;
end;

function T_PvlControlAttributes.SaveChanges
  : Boolean ;
begin
  Result := TGIS_ControlAttributes( oControl ).SaveChanges ;
end;

procedure T_PvlControlAttributes.CopyToClipboard ;
begin
  TGIS_ControlAttributes( oControl ).CopyToClipboard ;
end;

procedure T_PvlControlAttributes.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  TGIS_ControlAttributes( oControl ).SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'T_PvlControlAttributes specific'}

procedure T_PvlControlAttributes.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ControlAttributes.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

procedure T_PvlControlAttributes.doDestroy;
begin
//  FreeObject( oGIS ) ;
end;

{$ENDREGION 'T_PvlControlAttributes specific'}

initialization
  RegisterPVLPlatformControl( 'Attributes', T_PvlControlAttributes );
end.

