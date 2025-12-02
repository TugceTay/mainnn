{$IFDEF DCC}
  unit PVL.GisControl3D;
  {$HPPEMIT '#pragma link "PVL.GisControl3D"'}
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
    PVL.GisPvl,
    PVL.GisViewerWnd,
    GisTypes,
    GisInterfaces;
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
  ///  Defines which controls are to be displayed within the component.
  /// </summary>
  TGIS_PvlControl3DOption =
  (
    /// <summary>
    /// No options enabled.
    /// </summary>
    NoOptions,
    /// <summary>
    /// Navigation option visible.
    /// </summary>
    ShowNavigation,
    /// <summary>
    ///  Coordinates option visible.
    /// </summary>
    ShowCoordinates,
    /// <summary>
    ///  Reference Point option visible.
    /// </summary>
    ShowReferencePoint,
    /// <summary>
    ///  Lights option visible.
    /// </summary>
    ShowLights,
    /// <summary>
    /// Frame Modes option visible.
    /// </summary>
    ShowFrameModes,
    /// <summary>
    /// Scalings option visible.
    /// </summary>
    ShowScalings,
    /// <summary>
    ///  Floods option visible.
    /// </summary>
    ShowFloods,
    /// <summary>
    ///  Walls option visible.
    /// </summary>
    ShowWalls
  ) ;

  /// <summary>
  ///  set of TGIS_Control3DOption
  /// </summary>
  TGIS_PvlControl3DOptions = set of TGIS_PvlControl3DOption ;

  /// <summary>
  ///   PVL Control3D intraface
  /// </summary>
  IGIS_PvlControl3D = interface( IGIS_Subscribe )
    {$IFDEF DCC}
      ['{6888FB44-6C83-46F3-BAFE-CA862EB9AF0B}']
    {$ENDIF}

      ///  <summary>
      ///    Setter for GIS_Viewer property.
      ///  </summary>
      procedure fset_GIS_Viewer( const _value : TGIS_PvlViewerWnd        ) ;

      ///  <summary>
      ///    Getter for GIS_Viewer property.
      ///  </summary>
      function  fget_GIS_Viewer : TGIS_PvlViewerWnd ;

      ///  <summary>
      ///    Setter for AutosizeEx property.
      ///  </summary>
      procedure fset_AutosizeEx( const _value : Boolean               ) ;

      ///  <summary>
      ///    Getter for AutosizeEx property.
      ///  </summary>
      function  fget_AutosizeEx : Boolean ;

      ///  <summary>
      ///    Setter for Mode property.
      ///  </summary>
      procedure fset_Mode      ( const _value : TGIS_Viewer3DMode     ) ;

      ///  <summary>
      ///    Getter for Mode property.
      ///  </summary>
      function  fget_Mode : TGIS_Viewer3DMode ;

      /// <summary>
      ///   Setter for Options property.
      /// </summary>
      procedure fset_Options   ( const _value : TGIS_PvlControl3DOptions ) ;

      /// <summary>
      ///   Getter for Options property.
      /// </summary>
      function  fget_Options : TGIS_PvlControl3DOptions ;

      ///  <summary>
      ///    Setter for RightToLeftFromTranslation property.
      ///  </summary>
      procedure fset_RightToLeftFromTranslation ( const _value : Boolean ) ;

      ///  <summary>
      ///    Getter for RightToLeftFromTranslation property.
      ///  </summary>
      function  fget_RightToLeftFromTranslation : Boolean ;

      ///  <summary>
      ///    Setter for RightToLeft property.
      ///  </summary>
      procedure fset_RightToLeft ( const _value : Boolean ) ;

      ///  <summary>
      ///    Getter for RightToLeft property.
      ///  </summary>
      function  fget_RightToLeft : Boolean ;
  end;

  /// <summary>
  ///   PVL GIS Viewer control.
  /// </summary>
  TGIS_PvlControl3D = class( TGIS_PvlControl, IGIS_PvlControl3D )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControl3D ;          reintroduce;
      property PlatformControl    : IGIS_PvlControl3D
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}
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
    public

      /// <summary>
      ///   If true the size of the component is being automatically adjusted.
      /// </summary>
      property AutoSize : Boolean                 read  fget_AutosizeEx
                                                  write fset_AutosizeEx ;

      /// <summary>
      ///   Defines the purpose of mouse actions.
      /// </summary>
      property Mode : TGIS_Viewer3DMode           read  fget_Mode
                                                  write fset_Mode ;

      /// <summary>
      ///   Defines which controls are to be displayed within the component.
      /// </summary>
      property Options : TGIS_PvlControl3DOptions read fget_Options
                                                  write fset_Options ;

      /// <summary>
      ///   Defines which BiDi we are using the one from property or the one from
      ///   translation
      /// </summary>
      property BiDiModeFromTranslation : Boolean  read  fget_RightToLeftFromTranslation
                                                  write fset_RightToLeftFromTranslation ;

      /// <summary>
      ///   Attached TGIS_ViewerWnd component instance.
      /// </summary>
      property GIS_Viewer : TGIS_PvlViewerWnd     read  fget_GIS_Viewer
                                                  write fset_GIS_Viewer ;
  end;



implementation

{$IFDEF DCC}
  uses
    GisRtl
    // ensure that proper implementation files are referenced in Delphi
    {$IFDEF USE_FMX}
      ,FMX.GisPvlControl3D
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControl3D
    {$ENDIF}
    ;
{$ENDIF}

procedure TGIS_PvlControl3D.fset_GIS_Viewer(
  const _value: TGIS_PvlViewerWnd
) ;
begin
  PlatformControl.fset_GIS_Viewer( _value ) ;
end;

function TGIS_PvlControl3D.fget_GIS_Viewer
  : TGIS_PvlViewerWnd ;
begin
  Result := PlatformControl.fget_GIS_Viewer ;
end;

procedure TGIS_PvlControl3D.fset_AutosizeEx(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_AutosizeEx( _value ) ;
end;

function TGIS_PvlControl3D.fget_AutosizeEx
  : Boolean ;
begin
  Result := PlatformControl.fget_AutosizeEx ;
end;

procedure TGIS_PvlControl3D.fset_Mode(
  const _value: TGIS_Viewer3DMode
) ;
begin
  PlatformControl.fset_Mode( _value ) ;
end;

function TGIS_PvlControl3D.fget_Mode
  : TGIS_Viewer3DMode ;
begin
  Result := PlatformControl.fget_Mode ;
end;

procedure TGIS_PvlControl3D.fset_Options(
  const _value: TGIS_PvlControl3DOptions
) ;
begin
  PlatformControl.fset_Options( _value ) ;
end;

function TGIS_PvlControl3D.fget_Options
  : TGIS_PvlControl3DOptions ;
begin
  Result := PlatformControl.fget_Options ;
end;

procedure TGIS_PvlControl3D.fset_RightToLeftFromTranslation(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_RightToLeftFromTranslation( _value ) ;
end;

function TGIS_PvlControl3D.fget_RightToLeftFromTranslation
  : Boolean ;
begin
  Result := PlatformControl.fget_RightToLeftFromTranslation ;
end;

procedure TGIS_PvlControl3D.fset_RightToLeft(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_RightToLeft( _value ) ;
end;

function TGIS_PvlControl3D.fget_RightToLeft
  : Boolean ;
begin
  Result := PlatformControl.fget_RightToLeft ;
end;

procedure  TGIS_PvlControl3D.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  PlatformControl.SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'TGIS_PvlControl3D specific'}

procedure TGIS_PvlControl3D.initControl;
begin
  inherited;
end;

procedure TGIS_PvlControl3D.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'Control3D');
end;

function TGIS_PvlControl3D.fget_PlatformControl
  : IGIS_PvlControl3D;
begin
  Result := oPlatform as IGIS_PvlControl3D;
end;

{$ENDREGION 'TGIS_PvlControl3D specific'}

end.
