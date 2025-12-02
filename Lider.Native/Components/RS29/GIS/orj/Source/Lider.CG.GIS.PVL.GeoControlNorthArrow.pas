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
  Visual control for displaying north arrow.
}

{$IFDEF DCC}
  unit PVL.GisControlNorthArrow;
  {$HPPEMIT '#pragma link "PVL.GisControlNorthArrow"'}
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

  /// <summary>
  ///   PVL control north arrow intraface
  /// </summary>
  IGIS_PvlControlNorthArrow = interface( IGIS_Subscribe )
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
      ///   Setter for Color1 property.
      /// </summary>
      /// <param name="_value">
      ///   New Color1 value.
      /// </param>
      procedure fset_Color1       ( const _value : TGIS_Color
                                  ) ;

      /// <summary>
      ///   Getter for Color1 property.
      /// </summary>
      /// <returns>
      ///   Current Color1 value.
      /// </returns>
      function  fget_Color1       : TGIS_Color ;

      /// <summary>
      ///   Setter for Color2 property.
      /// </summary>
      /// <param name="_value">
      ///   New Color2 value.
      /// </param>
      procedure fset_Color2       ( const _value : TGIS_Color
                                  ) ;
      /// <summary>
      ///   Getter for Color2 property.
      /// </summary>
      /// <returns>
      ///   Current Color2 value.
      /// </returns>
      function  fget_Color2       : TGIS_Color ;

      /// <summary>
      ///   Setter for Path property.
      /// </summary>
      /// <param name="_value">
      ///   New Path value.
      /// </param>
      procedure fset_Path         ( const _value : String
                                  ) ;
      /// <summary>
      ///   Getter for Path property.
      /// </summary>
      /// <returns>
      ///   Current Path value.
      /// </returns>
      function  fget_Path         : String ;

      /// <summary>
      ///   Setter for Style property.
      /// </summary>
      /// <param name="_value">
      ///   New Style value.
      /// </param>
      procedure fset_Style        ( const _value : TGIS_ControlNorthArrowStyle
                                  ) ;

      /// <summary>
      ///   Getter for Style property.
      /// </summary>
      /// <returns>
      ///   Current Style value.
      /// </returns>
      function  fget_Style        : TGIS_ControlNorthArrowStyle ;

  end;

  /// <summary>
  ///   PVL north arrow control.
  /// </summary>
  TGIS_PvlControlNorthArrow = class( TGIS_PvlControl, IGIS_PvlControlNorthArrow, IGIS_PrintableControl )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );                                             override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControlNorthArrow ;                  reintroduce;
      property PlatformControl    : IGIS_PvlControlNorthArrow
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    private // IGIS_PrintableControl property access routines
      function  fget_InternalName : String ;
      procedure fset_InternalName ( const _value : String
                                  ) ;
    protected

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
      property InternalName    : String
                                          read  fget_InternalName
                                          write fset_InternalName ;
  end;



implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlControlNorthArrow
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControlNorthArrow
    {$ENDIF}
    ;
{$ENDIF}

//==============================================================================
// TGIS_PvlControlNorthArrow
//==============================================================================

function TGIS_PvlControlNorthArrow.fget_InternalName
  : string ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).InternalName ;
end;

procedure TGIS_PvlControlNorthArrow.fset_InternalName(
  const _value: string
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).InternalName := _value ;
end;

procedure TGIS_PvlControlNorthArrow.fset_GIS_Viewer(
  const _value : TGIS_PvlViewerWnd
) ;
begin
  PlatformControl.fset_GIS_Viewer( _value ) ;
end ;

function TGIS_PvlControlNorthArrow.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := PlatformControl.fget_GIS_Viewer ;
end;

procedure TGIS_PvlControlNorthArrow.fset_Glow(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Glow( _value ) ;
end ;

function TGIS_PvlControlNorthArrow.fget_Glow
  : Boolean ;
begin
  Result := PlatformControl.fget_Glow ;
end;

procedure TGIS_PvlControlNorthArrow.fset_Color1(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color1( _value ) ;
end ;

function TGIS_PvlControlNorthArrow.fget_Color1
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color1 ;
end;

procedure TGIS_PvlControlNorthArrow.fset_Color2(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color2( _value ) ;
end ;

function TGIS_PvlControlNorthArrow.fget_Color2
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color2 ;
end;

procedure TGIS_PvlControlNorthArrow.fset_Path(
  const _value: string
) ;
begin
  PlatformControl.fset_Path( _value ) ;
end;

function TGIS_PvlControlNorthArrow.fget_Path
  : string ;
begin
  Result := PlatformControl.fget_Path ;
end;

procedure TGIS_PvlControlNorthArrow.fset_Style(
  const _value: TGIS_ControlNorthArrowStyle
) ;
begin
  PlatformControl.fset_Style( _value ) ;
end;

function TGIS_PvlControlNorthArrow.fget_Style
  : TGIS_ControlNorthArrowStyle ;
begin
  Result := PlatformControl.fget_Style ;
end;

procedure  TGIS_PvlControlNorthArrow.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  PlatformControl.SubscribedEvent( _sender, _event, _context ) ;
end ;

function TGIS_PvlControlNorthArrow.CreateCopy
  : IGIS_PrintableControl ;
begin
  Result := ( PlatformControl as IGIS_PrintableControl ).CreateCopy ;
end;

procedure TGIS_PvlControlNorthArrow.FreeCopy(
  const _control: IGIS_PrintableControl
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).FreeCopy( _control ) ;
end;

procedure TGIS_PvlControlNorthArrow.PrintBmp(
  const _bitmap: TGIS_Bitmap
) ;
begin
  ( PlatformControl as IGIS_PrintableControl ).PrintBmp( _bitmap ) ;
end;

{$REGION 'TGIS_PvlControlNorthArrow specific'}

procedure TGIS_PvlControlNorthArrow.initControl;
begin
  inherited;
end;


procedure TGIS_PvlControlNorthArrow.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'NorthArrow');
end;

function TGIS_PvlControlNorthArrow.fget_PlatformControl
  : IGIS_PvlControlNorthArrow;
begin
  Result := oPlatform as IGIS_PvlControlNorthArrow;
end;

{$ENDREGION 'TGIS_PvlControlNorthArrow specific'}

end.
