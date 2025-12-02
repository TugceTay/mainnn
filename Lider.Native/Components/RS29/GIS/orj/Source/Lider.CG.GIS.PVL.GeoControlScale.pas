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
  Visual control for displaying scale.
}

{$IFDEF DCC}
  unit PVL.GisControlScale;
  {$HPPEMIT '#pragma link "PVL.GisControlScale"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

interface

uses
  {$IFDEF CLR}
    TatukGIS.NDK,
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    {$M+}
    GisCsBase,
    GisTypes,
    GisTypesUI,
    GisInterfaces,

    PVL.GisPvl,
    PVL.GisViewerWnd;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    tatukgis.jdk.*,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.rtl ;
  {$ENDIF}


type




  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired on preparing size of TGIS_ControlSize component.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   passed arguments
    /// </param>
    TGIS_PvlControlScalePrepareEvent = TGIS_ControlScalePrepareEvent ;

  {$ELSE}
    /// <summary>
    ///   Event fired on preparing size of TGIS_ControlSize component.
    /// </summary>
    /// <param name="_ptA">
    ///   starting point of distance measurement
    /// </param>
    /// <param name="_ptB">
    ///   ending of distance measurement
    /// </param>
    /// <param name="_distance">
    ///   computed distance
    /// </param>
    /// <param name="_units">
    ///   string with map units
    /// </param>
    TGIS_PvlControlScalePrepareEvent = procedure(
      const _ptA      : TGIS_Point ;
      const _ptB      : TGIS_Point ;
      var   _distance : Double ;
      var   _units    : String
    ) of object ;
  {$ENDIF}

  /// <summary>
  ///   PVL control scale intraface
  /// </summary>
  IGIS_PvlControlScale = interface( IGIS_Subscribe )
    {$IFDEF DCC}
      ['{3A789264-97A5-442A-863C-86393F8A51B8}']
    {$ENDIF}

      /// <summary>
      ///   Setter for GIS_Viewer property.
      /// </summary>
      /// <param name="_value">
      ///   New GIS_Viewer value.
      /// </param>
      procedure fset_GIS_Viewer   ( const _value : TGIS_PvlViewerWnd
                                  ) ;
      /// <summary>
      ///   Getter for GIS_Viewer property.
      /// </summary>
      /// <returns>
      ///   Current GIS_Viewer value.
      /// </returns>
      function  fget_GIS_Viewer   : TGIS_PvlViewerWnd ;

      /// <summary>
      ///   Setter for Font property.
      /// </summary>
      /// <param name="_value">
      ///   New Font value.
      /// </param>
      procedure fset_Font         ( const _value : TGIS_Font
                                  ) ;
      /// <summary>
      ///   Getter for Font property.
      /// </summary>
      /// <returns>
      ///   Current Font value.
      /// </returns>
      function  fget_Font         : TGIS_Font ;

      /// <summary>
      ///   Setter for Glow property.
      /// </summary>
      /// <param name="_value">
      ///   New Glow value.
      /// </param>
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;
      /// <summary>
      ///   Getter for Glow property.
      /// </summary>
      /// <returns>
      ///   Current Glow value.
      /// </returns>
      function  fget_Glow         : Boolean ;

      /// <summary>
      ///   Setter for TextColor property.
      /// </summary>
      /// <param name="_value">
      ///   New TextColor value.
      /// </param>
      procedure fset_TextColor    ( const _value : TGIS_Color
                                  ) ;
      /// <summary>
      ///   Getter for TextColor property.
      /// </summary>
      /// <returns>
      ///   Current TextColor value.
      /// </returns>
      function  fget_TextColor    : TGIS_Color ;

      /// <summary>
      ///   Setter for Dividers property.
      /// </summary>
      /// <param name="_value">
      ///   New Dividers value.
      /// </param>
      procedure fset_Dividers     ( const _value : Integer
                                  ) ;
      /// <summary>
      ///   Getter for Dividers property.
      /// </summary>
      /// <returns>
      ///   Current Dividers value.
      /// </returns>
      function  fget_Dividers     : Integer ;

      /// <summary>
      ///   Setter for DividerColor1 property.
      /// </summary>
      /// <param name="_value">
      ///   New DividerColor1 value.
      /// </param>
      procedure fset_DividerColor1( const _value : TGIS_Color
                                  ) ;

      /// <summary>
      ///   Getter for DividerColor1 property.
      /// </summary>
      /// <returns>
      ///   Current DividerColor1 value.
      /// </returns>
      function  fget_DividerColor1: TGIS_Color ;

      /// <summary>
      ///   Setter for DividerColor2 property.
      /// </summary>
      /// <param name="_value">
      ///   New DividerColor2 value.
      /// </param>
      procedure fset_DividerColor2( const _value : TGIS_Color
                                  ) ;
      /// <summary>
      ///   Getter for DividerColor2 property.
      /// </summary>
      /// <returns>
      ///   Current DividerColor2 value.
      /// </returns>
      function  fget_DividerColor2: TGIS_Color ;

      /// <summary>
      ///   Setter for ShadowColor property.
      /// </summary>
      /// <param name="_value">
      ///   New ShadowColor value.
      /// </param>
      procedure fset_ShadowColor  ( const _value : TGIS_Color
                                  ) ;
      /// <summary>
      ///   Getter for ShadowColor property.
      /// </summary>
      /// <returns>
      ///   Current ShadowColor value.
      /// </returns>
      function  fget_ShadowColor  : TGIS_Color ;

      /// <summary>
      ///   Getter for Units property.
      /// </summary>
      /// <returns>
      ///   Current Units value.
      /// </returns>
      function  fget_Units        : TGIS_CSUnits ;

      /// <summary>
      ///   Setter for Units property.
      /// </summary>
      /// <param name="_value">
      ///   New Units value.
      /// </param>
      procedure fset_Units        ( const _value : TGIS_CSUnits
                                  ) ;
      /// <summary>
      ///   Getter for PrepareEvent property.
      /// </summary>
      /// <returns>
      ///   Current PrepareEvent value.
      /// </returns>
      function  fget_PrepareEvent : TGIS_PvlControlScalePrepareEvent ;

      /// <summary>
      ///   Setter for PrepareEvent property.
      /// </summary>
      /// <param name="_value">
      ///   New PrepareEvent value.
      /// </param>
      procedure fset_PrepareEvent ( const _value : TGIS_PvlControlScalePrepareEvent
                                  ) ;
      /// <summary>
      ///   Getter for UnitsEPSG property.
      /// </summary>
      /// <returns>
      ///   Current UnitsEPSG value.
      /// </returns>
      function  fget_UnitsEPSG    : Integer ;

      /// <summary>
      ///   Setter for UnitsEPSG property.
      /// </summary>
      /// <param name="_value">
      ///   New UnitsEPSG value.
      /// </param>
      procedure fset_UnitsEPSG    ( const _value : Integer
                                  ) ;
  end;

  /// <summary>
  ///   PVL scale control.
  /// </summary>
  TGIS_PvlControlScale = class( TGIS_PvlControl, IGIS_PvlControlScale, IGIS_PrintableControl )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );                                             override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControlScale ;                       reintroduce;
      property PlatformControl    : IGIS_PvlControlScale
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;

    protected // IGIS_Viewer property access routines

      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_GIS_Viewer   ( const _value : TGIS_PvlViewerWnd
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_GIS_Viewer   : TGIS_PvlViewerWnd ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_Font         ( const _value : TGIS_Font
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_Font         : TGIS_Font ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_Glow         ( const _value : Boolean
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_Glow         : Boolean ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_TextColor    ( const _value : TGIS_Color
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_TextColor    : TGIS_Color ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_Dividers     ( const _value : Integer
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_Dividers     : Integer ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_DividerColor1( const _value : TGIS_Color
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_DividerColor1: TGIS_Color ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_DividerColor2( const _value : TGIS_Color
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_DividerColor2: TGIS_Color ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_ShadowColor  ( const _value : TGIS_Color
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_ShadowColor  : TGIS_Color ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_Units        : TGIS_CSUnits ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_Units        ( const _value : TGIS_CSUnits
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_UnitsEPSG    : Integer ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      procedure fset_UnitsEPSG    ( const _value : Integer
                                  ) ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
      function  fget_PrepareEvent : TGIS_PvlControlScalePrepareEvent ;
      /// <inheritdoc from="IGIS_PvlControlScale"/>
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

      /// <event/>
      /// <summary>
      ///   Event fired before calculating scale.
      /// </summary>
      property PrepareEvent : TGIS_PvlControlScalePrepareEvent
                                       read  fget_PrepareEvent
                                       write fset_PrepareEvent ;
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENSCR}
      /// <inheritdoc from="IGIS_PrintableControl"/>
      property InternalName : String
                                  read  fget_InternalName
                                  write fset_InternalName ;
//
//      {$IFDEF LEVEL_RX10_FMX}
//        /// <summary>
//        ///   Styled settings.
//        /// </summary>
//        property StyledSettings      : TGIS_ControlScaleStyledSettings
//                                       read  FStyledSettings
//                                       write FStyledSettings ;
//      {$ENDIF}
  end;



implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlControlScale
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControlScale
    {$ENDIF}
    ;
{$ENDIF}

//==============================================================================
// TGIS_PvlControlScale
//==============================================================================

function TGIS_PvlControlScale.fget_InternalName
  : string ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).InternalName ;
end;

procedure TGIS_PvlControlScale.fset_InternalName(
  const _value: string
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).InternalName := _value ;
end;

procedure TGIS_PvlControlScale.fset_GIS_Viewer(
  const _value : TGIS_PvlViewerWnd
) ;
begin
  PlatformControl.fset_GIS_Viewer( _value ) ;
end ;

function TGIS_PvlControlScale.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := PlatformControl.fget_GIS_Viewer ;
end;

procedure TGIS_PvlControlScale.fset_Dividers(
  const _value : Integer
) ;
begin
  PlatformControl.fset_Dividers( _value ) ;
end ;

function TGIS_PvlControlScale.fget_Dividers
  : Integer ;
begin
  Result := PlatformControl.fget_Dividers ;
end;

procedure TGIS_PvlControlScale.fset_Font(
  const _value : TGIS_Font
) ;
begin
  PlatformControl.fset_Font( _value ) ;
end ;

function TGIS_PvlControlScale.fget_Font
  : TGIS_Font ;
begin
  Result := PlatformControl.fget_Font ;
end;

procedure TGIS_PvlControlScale.fset_Glow(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Glow( _value ) ;
end ;

function TGIS_PvlControlScale.fget_Glow
  : Boolean ;
begin
  Result := PlatformControl.fget_Glow ;
end;

procedure TGIS_PvlControlScale.fset_TextColor(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_TextColor( _value ) ;
end ;

function TGIS_PvlControlScale.fget_TextColor
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_TextColor ;
end;

procedure TGIS_PvlControlScale.fset_DividerColor1(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_DividerColor1( _value ) ;
end ;

function TGIS_PvlControlScale.fget_DividerColor1
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_DividerColor1 ;
end;

procedure TGIS_PvlControlScale.fset_DividerColor2(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_DividerColor2( _value ) ;
end ;

function TGIS_PvlControlScale.fget_DividerColor2
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_DividerColor2 ;
end;

procedure TGIS_PvlControlScale.fset_ShadowColor(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_ShadowColor( _value ) ;
end ;

function TGIS_PvlControlScale.fget_ShadowColor
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_ShadowColor ;
end;

function TGIS_PvlControlScale.fget_Units
  : TGIS_CSUnits ;
begin
  Result := PlatformControl.fget_Units ;
end ;

procedure TGIS_PvlControlScale.fset_Units(
  const _value : TGIS_CSUnits
) ;
begin
  PlatformControl.fset_Units( _value ) ;
end ;

function TGIS_PvlControlScale.fget_UnitsEPSG
  : Integer ;
begin
  Result := PlatformControl.fget_UnitsEPSG ;
end ;

procedure TGIS_PvlControlScale.fset_UnitsEPSG(
  const _value : Integer
) ;
begin
  PlatformControl.fset_UnitsEPSG( _value ) ;
end ;

function TGIS_PvlControlScale.fget_PrepareEvent
  : TGIS_PvlControlScalePrepareEvent ;
begin
  Result := PlatformControl.fget_PrepareEvent ;
end;

procedure TGIS_PvlControlScale.fset_PrepareEvent(
  const _value: TGIS_PvlControlScalePrepareEvent
) ;
begin
  PlatformControl.fset_PrepareEvent( _value ) ;
end;

procedure  TGIS_PvlControlScale.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  PlatformControl.SubscribedEvent( _sender, _event, _context ) ;
end ;

function TGIS_PvlControlScale.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).CreateCopy ;
end;

procedure TGIS_PvlControlScale.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).FreeCopy( _control ) ;
end;

procedure TGIS_PvlControlScale.PrintBmp(
  const _bitmap: TGIS_Bitmap
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).PrintBmp( _bitmap ) ;
end;

{$REGION 'TGIS_PvlControlScale specific'}

procedure TGIS_PvlControlScale.initControl;
begin
  inherited;
end;


procedure TGIS_PvlControlScale.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'Scale');
end;

function TGIS_PvlControlScale.fget_PlatformControl
  : IGIS_PvlControlScale;
begin
  Result := oPlatform as IGIS_PvlControlScale;
end;

{$ENDREGION 'TGIS_PvlViewerWnd specific'}

end.
