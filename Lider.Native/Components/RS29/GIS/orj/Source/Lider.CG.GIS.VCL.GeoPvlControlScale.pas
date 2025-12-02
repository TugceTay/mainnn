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
  VCL implementation of TGIS_PvlControlScale
}

unit VCL.GisPvlControlScale ;
{$HPPEMIT '#pragma link "VCL.GisPvlControlScale"'}

{$INCLUDE GisInclude.inc}

interface


uses
  VCL.Controls,

  GisTypesUI,
  GisCsBase,
  GisInterfaces,
  GisViewer,

  VCL.GisFramework,
  VCL.GisControlScale,
  VCL.GisViewerWnd,

  VCL.GisPvl,
  GisRtl,

  PVL.GisPvl,
  PVL.GisViewerWnd,
  PVL.GisControlScale;

implementation
type

  T_PvlControlScale = class( TGIS_PvlControlVcl, IGIS_PvlControlScale, IGIS_PrintableControl )
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

      procedure fset_GIS_Viewer   ( const _value : TGIS_PvlViewerWnd
                                  ) ;
      function  fget_GIS_Viewer   : TGIS_PvlViewerWnd ;
      procedure fset_Font         ( const _value : TGIS_Font
                                  ) ;
      function  fget_Font         : TGIS_Font ;
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;
      function  fget_Glow         : Boolean ;
      procedure fset_TextColor    ( const _value : TGIS_Color
                                  ) ;
      function  fget_TextColor    : TGIS_Color ;
      procedure fset_Dividers     ( const _value : Integer
                                  ) ;
      function  fget_Dividers     : Integer ;
      procedure fset_DividerColor1( const _value : TGIS_Color
                                  ) ;
      function  fget_DividerColor1: TGIS_Color ;
      procedure fset_DividerColor2( const _value : TGIS_Color
                                  ) ;
      function  fget_DividerColor2: TGIS_Color ;
      procedure fset_ShadowColor  ( const _value : TGIS_Color
                                  ) ;
      function  fget_ShadowColor  : TGIS_Color ;
      function  fget_Units        : TGIS_CSUnits ;

      procedure fset_Units        ( const _value : TGIS_CSUnits
                                  ) ;
      function  fget_UnitsEPSG    : Integer ;
      procedure fset_UnitsEPSG    ( const _value : Integer
                                  ) ;

      function  fget_PrepareEvent : TGIS_PvlControlScalePrepareEvent ;
      procedure fset_PrepareEvent ( const _value : TGIS_PvlControlScalePrepareEvent
                                  ) ;
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

    public
      /// <summary>
      ///   Units used for scale output.
      /// </summary>
      property Units : TGIS_CSUnits read  fget_Units
                                    write fset_Units ;

    published
      /// <summary>
      ///   Attached viewer.
      /// </summary>
      property GIS_Viewer : TGIS_PvlViewerWnd
                                           read  fget_GIS_Viewer
                                           write fset_GIS_Viewer ;

      /// <summary>
      ///   Font of the text.
      /// </summary>
      property Font          : TGIS_Font   read    fget_Font
                                           write   fset_Font ;

      /// <summary>
      ///   Draw "glowing" shadow around scale.
      /// </summary>
      property Glow          : Boolean     read    fget_Glow
                                           write   fset_Glow
                                           default True ;
      /// <summary>
      ///   Color of the text.
      /// </summary>
      property TextColor     : TGIS_Color  read    fget_TextColor
                                           write   fset_TextColor ;

      /// <summary>
      ///   Number of dividers.
      /// </summary>
      property Dividers : Integer read    fget_Dividers
                                  write   fset_Dividers
                                  default 5 ;

      /// <summary>
      ///   Color of the first divider.
      /// </summary>
      property DividerColor1 : TGIS_Color  read    fget_DividerColor1
                                           write   fset_DividerColor1 ;

      /// <summary>
      ///   Color of the second divider.
      /// </summary>
      property DividerColor2 : TGIS_Color  read    fget_DividerColor2
                                           write   fset_DividerColor2 ;

      /// <summary>
      ///   Color of the text shadow.
      ///   Drawn if different from TextColor.
      /// </summary>
      property ShadowColor : TGIS_Color    read    fget_ShadowColor
                                           write   fset_ShadowColor ;

      /// <summary>
      ///   Units EPSG code for scale output.
      ///   If set to 0 then automatic Matric or Imperial system will be used
      ///   based on location.
      /// </summary>
      property UnitsEPSG : Integer   read  fget_UnitsEPSG
                                     write fset_UnitsEPSG
                                     default 0 ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                                  read  fget_InternalName
                                  write fset_InternalName ;

      /// <event/>
      /// <summary>
      ///   Event fired before calculating scale.
      /// </summary>
      property PrepareEvent          : TGIS_PvlControlScalePrepareEvent
                                       read  fget_PrepareEvent
                                       write fset_PrepareEvent ;

  end ;

procedure T_PvlControlScale.fset_InternalName(
  const _value: string
) ;
begin
  TGIS_ControlScale( oControl ).InternalName := _value ;
end;

function T_PvlControlScale.fget_InternalName
  : string ;
begin
  Result := TGIS_ControlScale( oControl ).InternalName ;
end;

procedure T_PvlControlScale.fset_GIS_Viewer(
  const _value : TGIS_PvlViewerWnd
) ;
begin
  TGIS_ControlScale( oControl ).GIS_Viewer := _value.NativeControl as TGIS_ViewerWnd ;
  oGIS := _value ;
end ;

function T_PvlControlScale.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := oGIS ;
end;

procedure T_PvlControlScale.fset_Dividers(
  const _value : Integer
) ;
begin
  TGIS_ControlScale( oControl ).Dividers := _value ;
end ;

function T_PvlControlScale.fget_Dividers
  : Integer ;
begin
  Result := TGIS_ControlScale( oControl ).Dividers ;
end;

procedure T_PvlControlScale.fset_Font(
  const _value : TGIS_Font
) ;
begin
  TGIS_ControlScale( oControl ).Font := VCLFont( _value ) ;
end ;

function T_PvlControlScale.fget_Font
  : TGIS_Font ;
begin
  Result := FontHelper.DoCreateFromFont( TGIS_ControlScale( oControl ).Font ) ;
end;

procedure T_PvlControlScale.fset_Glow(
  const _value : Boolean
) ;
begin
  TGIS_ControlScale( oControl ).Glow :=  _value ;
end ;

function T_PvlControlScale.fget_Glow
  : Boolean ;
begin
  Result := TGIS_ControlScale( oControl ).Glow ;
end;

procedure T_PvlControlScale.fset_TextColor(
  const _value : TGIS_Color
) ;
begin
  TGIS_ControlScale( oControl ).Font.Color := VCLColor( _value ) ;
end ;

function T_PvlControlScale.fget_TextColor
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlScale( oControl ).Font.Color ) ;
end;

procedure T_PvlControlScale.fset_DividerColor1(
  const _value : TGIS_Color
) ;
begin
  TGIS_ControlScale( oControl ).DividerColor1 := VCLColor( _value ) ;
end ;

function T_PvlControlScale.fget_DividerColor1
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlScale( oControl ).DividerColor1 ) ;
end;

procedure T_PvlControlScale.fset_DividerColor2(
  const _value : TGIS_Color
) ;
begin
  TGIS_ControlScale( oControl ).DividerColor2 := VCLColor( _value ) ;
end ;

function T_PvlControlScale.fget_DividerColor2
  : TGIS_Color ;
begin
  Result := GISColor( TGIS_ControlScale( oControl ).DividerColor2 ) ;
end;

procedure T_PvlControlScale.fset_ShadowColor(
  const _value : TGIS_Color
) ;
begin
//?  TGIS_ControlScale( oControl ).ShadowColor := VCLColor( _value ) ;
end ;

function T_PvlControlScale.fget_ShadowColor
  : TGIS_Color ;
begin
//?  Result := GISColor( TGIS_ControlScale( oControl ).ShadowColor ) ;
end;

function T_PvlControlScale.fget_Units
  : TGIS_CSUnits ;
begin
  Result := TGIS_ControlScale( oControl ).Units ;
end ;

procedure T_PvlControlScale.fset_Units(
  const _value : TGIS_CSUnits
) ;
begin
  TGIS_ControlScale( oControl ).Units := _value ;
end ;

function T_PvlControlScale.fget_UnitsEPSG
  : Integer ;
begin
  Result := TGIS_ControlScale( oControl ).UnitsEPSG ;
end ;

procedure T_PvlControlScale.fset_UnitsEPSG(
  const _value : Integer
) ;
begin
  TGIS_ControlScale( oControl ).UnitsEPSG := _value ;
end ;

function T_PvlControlScale.fget_PrepareEvent
  : TGIS_PvlControlScalePrepareEvent ;
begin
  Result := TGIS_ControlScale( oControl ).PrepareEvent ;
end;

procedure T_PvlControlScale.fset_PrepareEvent(
  const _value: TGIS_PvlControlScalePrepareEvent
) ;
begin
  TGIS_ControlScale( oControl ).PrepareEvent := _value ;
end;

procedure  T_PvlControlScale.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  TGIS_ControlScale( oControl ).SubscribedEvent( _sender, _event, _context ) ;
end ;

function T_PvlControlScale.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := TGIS_ControlScale( oControl ).CreateCopy ;
end;

procedure T_PvlControlScale.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  TGIS_ControlScale( oControl ).FreeCopy( _control ) ;
end;

procedure T_PvlControlScale.PrintBmp(
  const _bitmap: TGIS_Bitmap
) ;
begin
  TGIS_ControlScale( oControl ).PrintBmp( _bitmap ) ;
end;

{$REGION 'T_PvlControlScale specific'}

procedure T_PvlControlScale.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ControlScale.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
end;

procedure T_PvlControlScale.doDestroy;
begin
  FreeObject( oGIS ) ;

  inherited ;
end;

{$ENDREGION 'T_PvlControlScale specific'}

initialization
  RegisterPVLPlatformControl( 'Scale', T_PvlControlScale );

{==================================== END =====================================}
end.


