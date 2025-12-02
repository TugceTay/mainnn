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
  FMX implementation of TGIS_PvlControlNorthArrow
}

unit FMX.GisPvlControlNorthArrow ;
{$HPPEMIT '#pragma link "FMX.GisPvlControlNorthArrow"'}

{$INCLUDE GisInclude.inc}

interface


uses
  FMX.Controls,
  FMX.Types,

  GisTypes,
  GisTypesUI,
  GisViewer,

  FMX.GisFramework,
  FMX.GisControlNorthArrow,
  FMX.GisViewerWnd,

  FMX.GisPvl,
  GisRtl,
  GisInterfaces,

  PVL.GisPvl,
  PVL.GisViewerWnd,
  PVL.GisControlNorthArrow;

implementation
type

  T_PvlControlNorthArrow = class( TGIS_PvlControlFmx, IGIS_PvlControlNorthArrow, IGIS_PrintableControl )
    private
      oGIS                        : TGIS_PvlViewerWnd ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );override;

      procedure doDestroy         ;override;

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;
    protected // properties access routines


      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      procedure fset_GIS_Viewer   ( const _value : TGIS_PvlViewerWnd
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_GIS_Viewer   : TGIS_PvlViewerWnd ;
      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      ///
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_Glow         : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      procedure fset_Color1       ( const _value : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_Color1       : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      procedure fset_Color2       ( const _value : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_Color2       : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      procedure fset_Path         ( const _value : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_Path         : String ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      procedure fset_Style        ( const _value : TGIS_ControlNorthArrowStyle
                                  ) ;

      /// <inheritdoc from="IGIS_PvlControlNorthArrow"/>
      function  fget_Style        : TGIS_ControlNorthArrowStyle ;
    public

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent   (       _sender  : TObject ;
                                          _event   : Integer ;
                                          _context : TObject
                                  ) ;

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

    published
      /// <summary>
      ///   Attached viewer.
      /// </summary>
      property GIS_Viewer      : TGIS_PvlViewerWnd
                                           read  fget_GIS_Viewer
                                           write fset_GIS_Viewer ;

      /// <summary>
      ///   Draw "glowing" shadow around scale.
      /// </summary>
      property Glow            : Boolean
                                           read    fget_Glow
                                           write   fset_Glow
                                           default True ;

      /// <summary>
      ///   First color used for drawing.
      /// </summary>
      property Color1          : TGIS_Color
                                           read    fget_Color1
                                           write   fset_Color1 ;

      /// <summary>
      ///   Second color used for drawing
      /// </summary>
      property Color2          : TGIS_Color
                                           read    fget_Color2
                                           write   fset_Color2 ;
      /// <summary>
      ///   Symbol style, used if Path is empty.
      /// </summary>
      property Style           : TGIS_ControlNorthArrowStyle
                                           read  fget_Style
                                           write fset_Style ;

      /// <summary>
      ///   Symbol path. If empty then Style property is used.
      /// </summary>
      property Path            : String
                                           read  fget_Path
                                           write fset_Path ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                                  read  fget_InternalName
                                  write fset_InternalName ;
  end ;

procedure T_PvlControlNorthArrow.fset_InternalName(
  const _value: string
) ;
begin
  TGIS_ControlNorthArrow( oControl ).InternalName := _value ;
end;

function T_PvlControlNorthArrow.fget_InternalName
  : string ;
begin
  Result := TGIS_ControlNorthArrow( oControl ).InternalName ;
end;

procedure T_PvlControlNorthArrow.fset_GIS_Viewer(
  const _value : TGIS_PvlViewerWnd
) ;
begin
  oGIS := _value ;
  TGIS_ControlNorthArrow( oControl ).GIS_Viewer :=  _value.NativeControl as TGIS_ViewerWnd  ;
end ;

function T_PvlControlNorthArrow.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := oGIS ;
end;

procedure T_PvlControlNorthArrow.fset_Glow(
  const _value : Boolean
) ;
begin
  TGIS_ControlNorthArrow( oControl ).Glow := _value ;
end ;

function T_PvlControlNorthArrow.fget_Glow
  : Boolean ;
begin
  Result := TGIS_ControlNorthArrow( oControl ).Glow ;
end;

procedure T_PvlControlNorthArrow.fset_Color1(
  const _value : TGIS_Color
) ;
begin
  TGIS_ControlNorthArrow( oControl ).Color1 := FMXColor( _value ) ;
end ;

function T_PvlControlNorthArrow.fget_Color1
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlNorthArrow( oControl ).Color1 ) ;
end;

procedure T_PvlControlNorthArrow.fset_Color2(
  const _value : TGIS_Color
) ;
begin
  TGIS_ControlNorthArrow( oControl ).Color2 := FMXColor( _value ) ;
end ;

function T_PvlControlNorthArrow.fget_Color2
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlNorthArrow( oControl ).Color2 ) ;
end;

procedure T_PvlControlNorthArrow.fset_Path(
  const _value: string
) ;
begin
  TGIS_ControlNorthArrow( oControl ).Path := _value ;
end;

function T_PvlControlNorthArrow.fget_Path
  : string ;
begin
  Result := TGIS_ControlNorthArrow( oControl ).Path ;
end;

procedure T_PvlControlNorthArrow.fset_Style(
  const _value: TGIS_ControlNorthArrowStyle
) ;
begin
  TGIS_ControlNorthArrow( oControl ).Style := _value ;
end;

function T_PvlControlNorthArrow.fget_Style
  : TGIS_ControlNorthArrowStyle ;
begin
  Result := TGIS_ControlNorthArrow( oControl ).Style ;
end;

procedure  T_PvlControlNorthArrow.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  TGIS_ControlNorthArrow( oControl ).SubscribedEvent( _sender, _event, _context ) ;
end ;

function T_PvlControlNorthArrow.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := TGIS_ControlNorthArrow( oControl ).CreateCopy ;
end;

procedure T_PvlControlNorthArrow.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  TGIS_ControlNorthArrow( oControl ).FreeCopy( _control ) ;
end;

procedure T_PvlControlNorthArrow.PrintBmp(
  const _bitmap: TGIS_Bitmap
) ;
begin
  TGIS_ControlNorthArrow( oControl ).PrintBmp( _bitmap ) ;
end;

{$REGION 'T_PvlControlNorthArrow specific'}

procedure T_PvlControlNorthArrow.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ControlNorthArrow.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

procedure T_PvlControlNorthArrow.doDestroy;
begin
  FreeObject( oGIS ) ;

  inherited ;
end;

{$ENDREGION 'T_PvlControlNorthArrow specific'}

initialization
  RegisterPVLPlatformControl( 'NorthArrow', T_PvlControlNorthArrow );

{==================================== END =====================================}
end.


