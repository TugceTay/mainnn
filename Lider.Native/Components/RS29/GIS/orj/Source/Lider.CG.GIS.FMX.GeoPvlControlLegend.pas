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
  FMX implementation of TGIS_PvlControlLegend
}

unit FMX.GisPvlControlLegend;

interface

uses
  System.Types, System.Classes,
  System.SysUtils,
  System.UiTypes,
  FMX.Types,
  FMX.Controls,

  FMX.GisFramework,
  FMX.GisPvl,
  FMX.GisControlLegend,
  PVL.GisPvl,
  PVL.GisControlLegend,

  GisInterfaces,
  GisTypes,
  GisTypesUI,
  GisHierarchy,
  GisLayer,
  GisLegend;

implementation

type
  T_PvlControlLegend = class( TGIS_PvlControlFmx, IGIS_PvlControlLegend, IGIS_PrintableControl )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      function CreateCopy         : IGIS_PrintableControl ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure FreeCopy          ( const _control : IGIS_PrintableControl
                                  ) ;

      /// <inheritdoc from="IGIS_PrintableControl"/>
      procedure PrintBmp          ( const _bitmap  : TGIS_Bitmap
                                  ) ;


    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;
    published
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName       : String
                                          read  fget_InternalName
                                          write fset_InternalName ;


    private // IGIS_PvlControlLegend event helpers

      FLayerParamsChangeEvent       : TGIS_LayerEvent;
      FLayerActiveChangeEvent       : TGIS_LayerEvent;
      FLayerSelectEvent             : TGIS_LayerEvent;
      FGroupActiveChangeEvent       : TGIS_HierarchyGroupEvent;
      FGroupSelectEvent             : TGIS_HierarchyGroupEvent;
      FOrderChangeEvent             : TGIS_PvlEvent ;
      FOpenDialogEvent              : TGIS_LayerEvent;

      FTapSimpleEvent               : TGIS_PvlMouseEvent;
      FTapDoubleEvent               : TGIS_PvlMouseEvent;
      FTapLongEvent                 : TGIS_PvlMouseEvent;

      procedure doLayerParamsChangeEvent(
                                            _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doLayerActiveChangeEvent(
                                            _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doLayerSelectEvent  (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doGroupActiveChangeEvent(
                                            _sender : TObject ;
                                            _group  : IGIS_HierarchyGroup
                                    ) ;
      procedure doGroupSelectEvent  (       _sender : TObject ;
                                            _group  : IGIS_HierarchyGroup
                                    ) ;
      procedure doOrderChangeEvent  (       _sender : TObject
                                    ) ;
      procedure doOpenDialogEvent   (       _sender : TObject ;
                                            _layer  : TGIS_Layer
                                    ) ;
      procedure doTapDoubleEvent    (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Single        ;
                                            _y          : Single
                                    ) ;
      procedure doTapLongEvent      (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Single        ;
                                            _y          : Single
                                    ) ;
      procedure doTapSimpleEvent    (       _sender     : TObject       ;
                                            _button     : TMouseButton  ;
                                            _shift      : TShiftState   ;
                                            _x          : Single        ;
                                            _y          : Single
                                    ) ;


    protected // IGIS_PvlControlLegend property access routines

      function  fget_GIS_Viewer     : IGIS_Viewer ;
      procedure fset_GIS_Viewer     ( const _value : IGIS_Viewer
                                    ) ;
      function  fget_GIS_Layer      : TGIS_Layer ;
      procedure fset_GIS_Layer      ( const _value : TGIS_Layer
                                    ) ;
      function  fget_GIS_Layers     : TGIS_LayerAbstractList ;
      procedure fset_GIS_Layers     ( const _value : TGIS_LayerAbstractList
                                    ) ;
      function  fget_GIS_Group      : IGIS_HierarchyGroup ;
      procedure fset_GIS_Group      ( const _value : IGIS_HierarchyGroup
                                    ) ;
      function  fget_Mode           : TGIS_ControlLegendMode ;
      procedure fset_Mode           ( const _value : TGIS_ControlLegendMode
                                    ) ;
      function  fget_Options        : TGIS_ControlLegendOptions ;
      procedure fset_Options        ( const _value : TGIS_ControlLegendOptions
                                    ) ;
      function  fget_RightToLeft    : Boolean;
      function  fget_RightToLeftFromTranslation
                                    : Boolean;

      function  fget_ReverseOrder   : Boolean ;
      procedure fset_ReverseOrder   ( const _value : Boolean
                                    ) ;
      function  fget_CompactView    : Boolean ;
      procedure fset_CompactView    ( const _value : Boolean
                                    ) ;
      function  fget_DrawIconStyle  : TGIS_LegendIconStyle ;
      procedure fset_DrawIconStyle  ( const _value : TGIS_LegendIconStyle
                                    ) ;
      function  fget_DialogOptions  : TGIS_ControlLegendDialogOptions ;
      procedure fset_DialogOptions  ( const _value : TGIS_ControlLegendDialogOptions
                                    ) ;

      procedure fset_RightToLeft    ( const _value : Boolean
                                    ) ;
      procedure fset_RightToLeftFromTranslation
                                    ( const _value : Boolean
                                    ) ;
      function  fget_BackgroundColor: TGIS_Color ;
      procedure fset_BackgroundColor( const _value : TGIS_Color
                                    ) ;
      function  fget_Font           : TGIS_Font ;
      function  fget_FontColor      : TGIS_Color ;
      procedure fset_FontColor      ( const _value : TGIS_Color
                                    ) ;

      function  fget_SelectedNode   : TGIS_TreeNode ;

    private // IGIS_PvlControlLegend events access routines
      function  fget_LayerParamsChangeEvent : TGIS_LayerEvent;
      procedure fset_LayerParamsChangeEvent ( const _value : TGIS_LayerEvent
                                            );
      function  fget_LayerActiveChangeEvent : TGIS_LayerEvent;
      procedure fset_LayerActiveChangeEvent ( const _value : TGIS_LayerEvent
                                            );
      function  fget_LayerSelectEvent       : TGIS_LayerEvent;
      procedure fset_LayerSelectEvent       ( const _value : TGIS_LayerEvent
                                            );
      function  fget_GroupActiveChangeEvent : TGIS_HierarchyGroupEvent;
      procedure fset_GroupActiveChangeEvent ( const _value : TGIS_HierarchyGroupEvent
                                            );
      function  fget_GroupSelectEvent       : TGIS_HierarchyGroupEvent;
      procedure fset_GroupSelectEvent       ( const _value : TGIS_HierarchyGroupEvent
                                            );
      function  fget_OrderChangeEvent       : TGIS_PvlEvent;
      procedure fset_OrderChangeEvent       ( const _value : TGIS_PvlEvent
                                            );
      function  fget_OpenDialogEvent        : TGIS_LayerEvent ;
      procedure fset_OpenDialogEvent        ( const _value : TGIS_LayerEvent
                                            );
      function  fget_TapSimpleEvent         :  TGIS_PvlMouseEvent;
      procedure fset_TapSimpleEvent         ( const _value : TGIS_PvlMouseEvent
                                            );
      function  fget_TapLongEvent           : TGIS_PvlMouseEvent;
      procedure fset_TapLongEvent           ( const _value : TGIS_PvlMouseEvent
                                            );
      function  fget_TapDoubleEvent         :  TGIS_PvlMouseEvent;
      procedure fset_TapDoubleEvent         ( const _value : TGIS_PvlMouseEvent
                                            );
    public // public methods
      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  IsExpanded          ( const _layer   : TGIS_Layer
                                    ) : Boolean ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  IsExpanded          ( const _group   : IGIS_HierarchyGroup
                                    ) : Boolean ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _group   : IGIS_HierarchyGroup
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Expand              ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _layer   : TGIS_Layer
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure Collapse            ( const _group   : IGIS_HierarchyGroup ;
                                      const _deep    : Boolean ;
                                      const _layers  : Boolean
                                    ) ; overload ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  GetNodeAt           ( const _x       : Integer ;
                                      const _y       : Integer
                                    ) : TGIS_TreeNode ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure InvalidateItems ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      function  DrawBmp             ( const _bitmap  : TGIS_Bitmap ;
                                      const _scale   : Double  ;
                                      const _ppi     : Integer
                                    ) : TGIS_Bitmap ;

      /// <inheritdoc from="IGIS_PvlControlLegend"/>
      procedure PrintClipboard      ;
  end;

{$REGION 'Utility functions'}

function cMouseButton(
  const _value : TMouseButton
) : TGIS_PvlMouseButton ;
begin
  case( _value ) of
    TMouseButton.mbLeft   : Result := TGIS_PvlMouseButton.Left ;
    TMouseButton.mbRight  : Result := TGIS_PvlMouseButton.Right ;
    TMouseButton.mbMiddle : Result := TGIS_PvlMouseButton.Middle ;
  end;
end;

function cShiftState(
  const _value : TShiftState
) : TGIS_PvlShiftState ;
begin
  Result := [] ;

  if ssShift in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Shift] ;
  if ssCtrl in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Ctrl] ;
  if ssAlt in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Alt] ;
  if ssLeft in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Left] ;
  if ssRight in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Right] ;
  if ssMiddle in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Middle] ;
  if ssDouble in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Double] ;
  if ssTouch in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Touch] ;
  if ssPen in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Pen] ;
  if ssCommand in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Command] ;
  if ssHorizontal in _value then
    Result := Result + [TGIS_PvlShiftStateItem.Horizontal] ;
end;

{$ENDREGION 'Utility functions'}


{$REGION 'IGIS_PvlConttolLegend event helper'}

procedure T_PvlControlLegend.doLayerParamsChangeEvent(
      _sender : TObject ;
      _layer  : TGIS_Layer
);
begin
  if Assigned( FLayerParamsChangeEvent ) then
    FLayerParamsChangeEvent( _sender, _layer ) ;
end;

procedure T_PvlControlLegend.doLayerActiveChangeEvent(
       _sender : TObject ;
       _layer  : TGIS_Layer
);
begin
  if Assigned( FLayerActiveChangeEvent ) then
    FLayerActiveChangeEvent( _sender, _layer ) ;
end;

procedure T_PvlControlLegend.doLayerSelectEvent(
       _sender : TObject ;
       _layer  : TGIS_Layer
);
begin
  if Assigned( FLayerSelectEvent ) then
    FLayerSelectEvent( _sender, _layer ) ;
end;

procedure T_PvlControlLegend.doGroupActiveChangeEvent(
       _sender : TObject ;
       _group  : IGIS_HierarchyGroup
);
begin
  if Assigned( FGroupActiveChangeEvent ) then
    FGroupActiveChangeEvent( _sender, _group ) ;
end;

procedure T_PvlControlLegend.doGroupSelectEvent(
       _sender : TObject ;
       _group  : IGIS_HierarchyGroup
);
begin
  if Assigned( FGroupSelectEvent ) then
    FGroupSelectEvent( _sender, _group ) ;
end;

procedure T_PvlControlLegend.doOrderChangeEvent(
       _sender : TObject
);
begin
  if Assigned( FOrderChangeEvent ) then
    FOrderChangeEvent( _sender ) ;
end;

procedure T_PvlControlLegend.doOpenDialogEvent(
       _sender : TObject ;
       _layer  : TGIS_Layer
);
begin
  if Assigned( FOpenDialogEvent ) then
    FOpenDialogEvent( _sender, _layer ) ;
end;

procedure T_PvlControlLegend.doTapDoubleEvent(
       _sender     : TObject       ;
       _button     : TMouseButton  ;
       _shift      : TShiftState   ;
       _x          : Single        ;
       _y          : Single
);
begin
  if Assigned( FTapDoubleEvent ) then
    FTapDoubleEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

procedure T_PvlControlLegend.doTapLongEvent(
       _sender     : TObject       ;
       _button     : TMouseButton  ;
       _shift      : TShiftState   ;
       _x          : Single        ;
       _y          : Single
);
begin
  if Assigned( FTapLongEvent ) then
    FTapLongEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

procedure T_PvlControlLegend.doTapSimpleEvent(
       _sender     : TObject       ;
       _button     : TMouseButton  ;
       _shift      : TShiftState   ;
       _x          : Single        ;
       _y          : Single
);
begin
  if Assigned( FTapSimpleEvent ) then
    FTapSimpleEvent( _sender, cMouseButton(_button), cShiftState(_shift), _x, _y ) ;
end;

{$ENDREGION 'IGIS_PvlConttolLegend event helper'}

{$REGION 'IGIS_ControlLegend property access routines'}

function T_PvlControlLegend.fget_GIS_Viewer
  : IGIS_Viewer;
begin
  Result :=  TGIS_ControlLegend( oControl ).GIS_Viewer;
end;

procedure T_PvlControlLegend.fset_GIS_Viewer(
  const _value : IGIS_Viewer
);
begin
   TGIS_ControlLegend( oControl ).GIS_Viewer := _value  ;
end;

function T_PvlControlLegend.fget_GIS_Layer
  : TGIS_Layer ;
begin
  Result :=  TGIS_ControlLegend( oControl ).GIS_Layer;
end;

procedure T_PvlControlLegend.fset_GIS_Layer(
  const _value : TGIS_Layer
);
begin
   TGIS_ControlLegend( oControl ).GIS_Layer := _value ;
end;

function T_PvlControlLegend.fget_GIS_Layers
  : TGIS_LayerAbstractList ;
begin
  Result :=  TGIS_ControlLegend( oControl ).GIS_Layers;
end;

procedure T_PvlControlLegend.fset_GIS_Layers(
  const _value : TGIS_LayerAbstractList
);
begin
   TGIS_ControlLegend( oControl ).GIS_Layers := _value ;
end;

function T_PvlControlLegend.fget_GIS_Group
  : IGIS_HierarchyGroup ;
begin
  Result :=  TGIS_ControlLegend( oControl ).GIS_Group;
end;

procedure T_PvlControlLegend.fset_GIS_Group(
  const _value : IGIS_HierarchyGroup
);
begin
   TGIS_ControlLegend( oControl ).GIS_Group :=_value ;
end;

function T_PvlControlLegend.fget_Mode
  : TGIS_ControlLegendMode ;
begin
  Result :=  TGIS_ControlLegend( oControl ).Mode;
end;

procedure T_PvlControlLegend.fset_Mode(
  const _value : TGIS_ControlLegendMode
);
begin
   TGIS_ControlLegend( oControl ).Mode := _value ;
end;

function T_PvlControlLegend.fget_Options
  : TGIS_ControlLegendOptions ;
begin
  Result :=  TGIS_ControlLegend( oControl ).Options;
end;

procedure T_PvlControlLegend.fset_Options(
  const _value : TGIS_ControlLegendOptions
);
begin
   TGIS_ControlLegend( oControl ).Options := _value ;
end;

function T_PvlControlLegend.fget_ReverseOrder
  : Boolean ;
begin
  Result :=  TGIS_ControlLegend( oControl ).ReverseOrder;
end;

procedure T_PvlControlLegend.fset_ReverseOrder(
  const _value : Boolean
);
begin
   TGIS_ControlLegend( oControl ).ReverseOrder := _value ;
end;

function T_PvlControlLegend.fget_CompactView
  : Boolean ;
begin
  Result :=  TGIS_ControlLegend( oControl ).CompactView;
end;

procedure T_PvlControlLegend.fset_CompactView(
  const _value : Boolean
);
begin
   TGIS_ControlLegend( oControl ).CompactView := _value ;
end;

function T_PvlControlLegend.fget_DrawIconStyle
  : TGIS_LegendIconStyle ;
begin
  Result :=  TGIS_ControlLegend( oControl ).DrawIconStyle;
end;

procedure T_PvlControlLegend.fset_DrawIconStyle(
  const _value : TGIS_LegendIconStyle
);
begin
   TGIS_ControlLegend( oControl ).DrawIconStyle := _value ;
end;

function T_PvlControlLegend.fget_DialogOptions
  : TGIS_ControlLegendDialogOptions ;
begin
  Result :=  TGIS_ControlLegend( oControl ).DialogOptions;
end;

procedure T_PvlControlLegend.fset_DialogOptions(
  const _value : TGIS_ControlLegendDialogOptions
);
begin
   TGIS_ControlLegend( oControl ).DialogOptions := _value ;
end;

function T_PvlControlLegend.fget_RightToLeft
  : Boolean;
begin
  Result := TGIS_ControlLegend( oControl ).BiDiMode = TBiDiMode.bdRightToLeft
end;

procedure T_PvlControlLegend.fset_RightToLeft(
  const _value : Boolean
) ;
begin
  if _value then
    TGIS_ControlLegend( oControl ).BiDiMode := TBiDiMode.bdRightToLeft
  else
    TGIS_ControlLegend( oControl ).BiDiMode := TBiDiMode.bdLeftToRight ;
end;

procedure T_PvlControlLegend.fset_RightToLeftFromTranslation(
  const _value : Boolean
) ;
begin
  TGIS_ControlLegend( oControl ).BiDiModeFromTranslation := _value ;
end;

function T_PvlControlLegend.fget_RightToLeftFromTranslation
  : Boolean;
begin
  Result := TGIS_ControlLegend( oControl ).BiDiModeFromTranslation
end;

function T_PvlControlLegend.fget_BackgroundColor
  : TGIS_Color ;
begin
  Result := GisColor(  TGIS_ControlLegend( oControl ).BackgroundColor );
end;

procedure T_PvlControlLegend.fset_BackgroundColor(
  const _value : TGIS_Color
) ;
begin
   TGIS_ControlLegend( oControl ).BackgroundColor := FMXColor( _value ) ;
end;

function  T_PvlControlLegend.fget_Font
  : TGIS_Font ;
begin
  Result := TGIS_Font.Create ;
  Result.LoadFromFont( TGIS_ControlLegend( oControl ).Font ) ;
end;

function T_PvlControlLegend.fget_FontColor
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlLegend( oControl ).FontColor );
end;

procedure T_PvlControlLegend.fset_FontColor(
  const _value : TGIS_Color
);
begin
   TGIS_ControlLegend( oControl ).FontColor := FmxColor( _value ) ;
end;

function T_PvlControlLegend.fget_SelectedNode
  : TGIS_TreeNode ;
begin
  Result :=  TGIS_ControlLegend( oControl ).SelectedNode;
end;

{$ENDREGION 'IGIS_ControlLegend property access routines'}

{$REGION 'IGIS_ControlLegend events access routines'}

function T_PvlControlLegend.fget_TapSimpleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapSimpleEvent;
end;

procedure T_PvlControlLegend.fset_TapSimpleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapSimpleEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).TapSimpleEvent := doTapSimpleEvent
  else
    TGIS_ControlLegend( oControl ).TapSimpleEvent := nil ;
end;

function T_PvlControlLegend.fget_TapLongEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapLongEvent;
end;

procedure T_PvlControlLegend.fset_TapLongEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapLongEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).TapLongEvent := doTapLongEvent
  else
    TGIS_ControlLegend( oControl ).TapLongEvent := nil ;
end;

function T_PvlControlLegend.fget_TapDoubleEvent
  : TGIS_PvlMouseEvent;
begin
  Result := FTapDoubleEvent;
end;

procedure T_PvlControlLegend.fset_TapDoubleEvent(
  const _value : TGIS_PvlMouseEvent
);
begin
  FTapDoubleEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).TapDoubleEvent := doTapDoubleEvent
  else
    TGIS_ControlLegend( oControl ).TapDoubleEvent := nil ;
end;

function T_PvlControlLegend.fget_LayerParamsChangeEvent
  : TGIS_LayerEvent;
begin
  Result := FLayerParamsChangeEvent;
end;

procedure T_PvlControlLegend.fset_LayerParamsChangeEvent(
  const _value : TGIS_LayerEvent
);
begin
  FLayerParamsChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).LayerParamsChangeEvent := doLayerParamsChangeEvent
  else
    TGIS_ControlLegend( oControl ).LayerParamsChangeEvent := nil ;
end;

function T_PvlControlLegend.fget_LayerActiveChangeEvent
  : TGIS_LayerEvent;
begin
  Result := FLayerActiveChangeEvent;
end;

procedure T_PvlControlLegend.fset_LayerActiveChangeEvent(
  const _value : TGIS_LayerEvent
);
begin
  FLayerActiveChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).LayerActiveChangeEvent := doLayerActiveChangeEvent
  else
    TGIS_ControlLegend( oControl ).LayerActiveChangeEvent := nil ;
end;

function T_PvlControlLegend.fget_LayerSelectEvent
  : TGIS_LayerEvent;
begin
  Result := FLayerSelectEvent;
end;

procedure T_PvlControlLegend.fset_LayerSelectEvent(
  const _value : TGIS_LayerEvent
);
begin
  FLayerSelectEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).LayerSelectEvent := doLayerSelectEvent
  else
    TGIS_ControlLegend( oControl ).LayerSelectEvent := nil ;
end;

function T_PvlControlLegend.fget_GroupActiveChangeEvent
  : TGIS_HierarchyGroupEvent;
begin
  Result := FGroupActiveChangeEvent;
end;

procedure T_PvlControlLegend.fset_GroupActiveChangeEvent(
  const _value : TGIS_HierarchyGroupEvent
);
begin
  FGroupActiveChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).GroupActiveChangeEvent := doGroupActiveChangeEvent
  else
    TGIS_ControlLegend( oControl ).GroupActiveChangeEvent := nil ;
end;

function T_PvlControlLegend.fget_GroupSelectEvent
  : TGIS_HierarchyGroupEvent;
begin
  Result := FGroupSelectEvent;
end;

procedure T_PvlControlLegend.fset_GroupSelectEvent(
  const _value : TGIS_HierarchyGroupEvent
);
begin
  FGroupSelectEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).GroupSelectEvent := doGroupSelectEvent
  else
    TGIS_ControlLegend( oControl ).GroupSelectEvent := nil ;
end;

function T_PvlControlLegend.fget_OrderChangeEvent
  : TGIS_PvlEvent;
begin
  Result := FOrderChangeEvent;
end;

procedure T_PvlControlLegend.fset_OrderChangeEvent(
  const _value : TGIS_PvlEvent
);
begin
  FOrderChangeEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).OrderChangeEvent := doOrderChangeEvent
  else
    TGIS_ControlLegend( oControl ).OrderChangeEvent := nil ;
end;

function T_PvlControlLegend.fget_OpenDialogEvent
  : TGIS_LayerEvent ;
begin
  Result := FOpenDialogEvent;
end;

procedure T_PvlControlLegend.fset_OpenDialogEvent(
  const _value : TGIS_LayerEvent
) ;
begin
  FOpenDialogEvent := _value ;
  if Assigned( _value ) then
    TGIS_ControlLegend( oControl ).OpenDialogEvent := doOpenDialogEvent
  else
    TGIS_ControlLegend( oControl ).OpenDialogEvent := nil ;
end;

{$ENDREGION 'IGIS_ControlLegend events access routines'}

{$REGION 'IGIS_PvlControlLegend public methods'}

function T_PvlControlLegend.IsExpanded(
  const _layer   : TGIS_Layer
) : Boolean ;
begin
  Result :=  TGIS_ControlLegend( oControl ).IsExpanded( _layer );
end;

function T_PvlControlLegend.IsExpanded(
  const _group   : IGIS_HierarchyGroup
) : Boolean ;
begin
  Result :=  TGIS_ControlLegend( oControl ).IsExpanded( _group );
end;

procedure T_PvlControlLegend.Expand(
  const _layer   : TGIS_Layer
) ;
begin
   TGIS_ControlLegend( oControl ).Expand( _layer );
end;

procedure T_PvlControlLegend.Expand(
  const _group   : IGIS_HierarchyGroup
);
begin
   TGIS_ControlLegend( oControl ).Expand( _group );
end;

procedure T_PvlControlLegend.Expand(
  const _group   : IGIS_HierarchyGroup ;
  const _deep    : Boolean ;
  const _layers  : Boolean
);
begin
   TGIS_ControlLegend( oControl ).Expand( _group, _deep, _layers );
end;

procedure T_PvlControlLegend.Collapse(
  const _layer   : TGIS_Layer
);
begin
   TGIS_ControlLegend( oControl ).Collapse( _layer );
end;

procedure T_PvlControlLegend.Collapse(
  const _group   : IGIS_HierarchyGroup
);
begin
   TGIS_ControlLegend( oControl ).Collapse( _group );
end;

procedure T_PvlControlLegend.Collapse(
  const _group   : IGIS_HierarchyGroup ;
  const _deep    : Boolean ;
  const _layers  : Boolean
);
begin
   TGIS_ControlLegend( oControl ).Collapse( _group, _deep, _layers );
end;

function T_PvlControlLegend.GetNodeAt(
  const _x       : Integer ;
  const _y       : Integer
) : TGIS_TreeNode ;
begin
  Result :=  TGIS_ControlLegend( oControl ).GetNodeAt( _x, _y  );
end;

procedure T_PvlControlLegend.InvalidateItems ;
begin
   TGIS_ControlLegend( oControl ).InvalidateItems;
end;

function T_PvlControlLegend.DrawBmp(
  const _bitmap  : TGIS_Bitmap ;
  const _scale   : Double  ;
  const _ppi     : Integer
) : TGIS_Bitmap ;
begin
//?  Result :=  TGIS_ControlLegend( oControl ).DrawBmp( _bitmap, _scale, _ppi );
end;

function T_PvlControlLegend.fget_InternalName
  : string ;
begin
  Result := TGIS_ControlLegend( oControl ).InternalName ;
end;

procedure T_PvlControlLegend.fset_InternalName(
  const _value: string
) ;
begin
  TGIS_ControlLegend( oControl ).InternalName := _value ;
end;

function T_PvlControlLegend.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := TGIS_ControlLegend( oControl ).CreateCopy ;
end;

procedure T_PvlControlLegend.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  TGIS_ControlLegend( oControl ).FreeCopy( _control ) ;
end;

procedure T_PvlControlLegend.PrintBmp(
  const _bitmap  : TGIS_Bitmap
) ;
begin
  TGIS_ControlLegend( oControl ).PrintBmp( _bitmap );
end;

procedure T_PvlControlLegend.PrintClipboard ;
begin
   TGIS_ControlLegend( oControl ).PrintClipboard ;
end;

{$REGION 'IGIS_PvlControlLegend public methods'}


{$REGION 'T_PvlControlLegend specific'}

procedure T_PvlControlLegend.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ControlLegend.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

{$ENDREGION 'T_PvlViewerWnd specific'}

initialization
  RegisterPVLPlatformControl( 'Legend', T_PvlControlLegend );

end.

