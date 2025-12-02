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
  FMX implementation of TGIS_PvlControl3D
}

unit FMX.GisPvlControl3D ;
{$HPPEMIT '#pragma link "FMX.GisPvlControl3D"'}

{$INCLUDE GisInclude.inc}

interface

uses
  FMX.Controls,
  FMX.Types,
  System.Classes,

  GisTypes,
  GisRtl,

  FMX.GisViewerWnd,
  FMX.GisControl3D,
  FMX.GisPvl,

  PVL.GisPvl,
  PVL.GisViewerWnd,
  PVL.GisControl3D;

implementation
type

  T_PvlControl3D = class( TGIS_PvlControlFmx, IGIS_PvlControl3D )
    private
      oGIS                        : TGIS_PvlViewerWnd ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );override;

      procedure doDestroy         ;override;
    protected

      /// <inheritdoc/>
      procedure fset_GIS_Viewer( const _value : TGIS_PvlViewerWnd        ) ;

      /// <inheritdoc/>
      function  fget_GIS_Viewer : TGIS_PvlViewerWnd ;

      /// <inheritdoc/>
      procedure fset_AutosizeEx( const _value : Boolean               ) ;

      /// <inheritdoc/>
      function  fget_AutosizeEx : Boolean ;

      /// <inheritdoc/>
      procedure fset_Mode      ( const _value : TGIS_Viewer3DMode     ) ;

      /// <inheritdoc/>
      function  fget_Mode : TGIS_Viewer3DMode ;

      /// <inheritdoc/>
      procedure fset_Options   ( const _value : TGIS_PvlControl3DOptions ) ;

      /// <inheritdoc/>
      function  fget_Options : TGIS_PvlControl3DOptions ;

      /// <inheritdoc/>
      procedure fset_RightToLeftFromTranslation ( const _value : Boolean ) ;

      /// <inheritdoc/>
      function  fget_RightToLeftFromTranslation : Boolean ;

      /// <inheritdoc/>
      procedure fset_RightToLeft ( const _value : Boolean ) ;

      /// <inheritdoc/>
      function  fget_RightToLeft : Boolean ;

    public
      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

  end ;


procedure T_PvlControl3D.fset_GIS_Viewer(
  const _value: TGIS_PvlViewerWnd
) ;
begin
  oGIS := _value ;
  TGIS_Control3D( oControl ).GIS_Viewer := oGIS.NativeControl as TGIS_ViewerWnd  ;
end;

function T_PvlControl3D.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := oGIS ;
end;

procedure T_PvlControl3D.fset_AutosizeEx(
  const _value: Boolean
) ;
begin
  TGIS_Control3D( oControl ).AutoSize := _value ;
end;

function T_PvlControl3D.fget_AutosizeEx
  : Boolean ;
begin
  Result := TGIS_Control3D( oControl ).AutoSize ;
end;

procedure T_PvlControl3D.fset_Mode(
  const _value: TGIS_Viewer3DMode
) ;
begin
  TGIS_Control3D( oControl ).Mode := _value ;
end;

function T_PvlControl3D.fget_Mode
  : TGIS_Viewer3DMode ;
begin
  Result := TGIS_Control3D( oControl ).Mode ;
end;

procedure T_PvlControl3D.fset_Options(
  const _value: TGIS_PvlControl3DOptions
) ;
var
  options : TGIS_Control3DOptions ;
begin
  options := [] ;

  if TGIS_PvlControl3DOption.NoOptions in _value then
    options := options + [TGIS_Control3DOption.NoOptions] ;
  if TGIS_PvlControl3DOption.ShowNavigation in _value then
    options := options + [TGIS_Control3DOption.ShowNavigation] ;
  if TGIS_PvlControl3DOption.ShowCoordinates in _value then
    options := options + [TGIS_Control3DOption.ShowCoordinates] ;
  if TGIS_PvlControl3DOption.ShowReferencePoint in _value then
    options := options + [TGIS_Control3DOption.ShowReferencePoint] ;
  if TGIS_PvlControl3DOption.ShowLights in _value then
    options := options + [TGIS_Control3DOption.ShowLights] ;
  if TGIS_PvlControl3DOption.ShowFrameModes in _value then
    options := options + [TGIS_Control3DOption.ShowFrameModes] ;
  if TGIS_PvlControl3DOption.ShowScalings in _value then
    options := options + [TGIS_Control3DOption.ShowScalings] ;
  if TGIS_PvlControl3DOption.ShowFloods in _value then
    options := options + [TGIS_Control3DOption.ShowFloods] ;
  if TGIS_PvlControl3DOption.ShowWalls in _value then
    options := options + [TGIS_Control3DOption.ShowWalls] ;

  TGIS_Control3D( oControl ).Options := options ;
end;

function T_PvlControl3D.fget_Options
  : TGIS_PvlControl3DOptions ;
begin
  Result := [] ;

  if TGIS_Control3DOption.NoOptions in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.NoOptions] ;
  if TGIS_Control3DOption.ShowNavigation in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowNavigation] ;
  if TGIS_Control3DOption.ShowCoordinates in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowCoordinates] ;
  if TGIS_Control3DOption.ShowReferencePoint in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowReferencePoint] ;
  if TGIS_Control3DOption.ShowLights in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowLights] ;
  if TGIS_Control3DOption.ShowFrameModes in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowFrameModes] ;
  if TGIS_Control3DOption.ShowScalings in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowScalings] ;
  if TGIS_Control3DOption.ShowFloods in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowFloods] ;
  if TGIS_Control3DOption.ShowWalls in TGIS_Control3D( oControl ).Options then
    Result := Result + [TGIS_PvlControl3DOption.ShowWalls] ;
end;

procedure T_PvlControl3D.fset_RightToLeftFromTranslation(
  const _value: Boolean
) ;
begin
  TGIS_Control3D( oControl ).BiDiModeFromTranslation := _value ;
end;

function T_PvlControl3D.fget_RightToLeftFromTranslation
  : Boolean ;
begin
  Result := TGIS_Control3D( oControl ).BiDiModeFromTranslation ;
end;

procedure T_PvlControl3D.fset_RightToLeft(
  const _value: Boolean
) ;
begin
  if _value then
    TGIS_Control3D( oControl ).BiDiMode := TBiDiMode.bdRightToLeft
  else
    TGIS_Control3D( oControl ).BiDiMode := TBiDiMode.bdLeftToRight ;
end;

function T_PvlControl3D.fget_RightToLeft
  : Boolean ;
begin
  Result := TGIS_Control3D( oControl ).BiDiMode = TBiDiMode.bdRightToLeft ;
end;

procedure  T_PvlControl3D.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  TGIS_Control3D( oControl ).SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'T_PvlControl3D specific'}

procedure T_PvlControl3D.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_Control3D.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

procedure T_PvlControl3D.doDestroy;
begin
  FreeObject( oGIS ) ;

  inherited ;
end;

{$ENDREGION 'T_PvlControl3D specific'}

initialization
  RegisterPVLPlatformControl( 'Control3D', T_PvlControl3D );

{==================================== END =====================================}
end.


