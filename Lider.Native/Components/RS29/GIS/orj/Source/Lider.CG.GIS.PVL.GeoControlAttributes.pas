{$IFDEF DCC}
  unit PVL.GisControlAttributes;
  {$HPPEMIT '#pragma link "PVL.GisControlAttributes"'}
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
    GisLayerVector,

    PVL.GisPvl;
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
  ///   Two layout types for attributes control.
  /// </summary>
  TGIS_PvlAttibutesLayoutType = (
    /// <summary>
    ///   Attributes names and values in one column.
    /// </summary>
    OneColumn,

    /// <summary>
    ///   Attributes names on left, attributes values on right.
    /// </summary>
    TwoColumns
  ) ;

  /// <summary>
  ///   PVL control attributes intraface
  /// </summary>
  IGIS_PvlControlAttributes = interface( IGIS_Subscribe )
    {$IFDEF DCC}
      ['{9F63FB51-480A-4BE9-B77A-5450C1BC8B72}']
    {$ENDIF}

      /// <summary>
      ///   Getter for FieldNameColumnWidth property.
      /// </summary>
      /// <returns>
      ///   Current FieldNameColumnWidth value.
      /// </returns>
      function  fget_FieldNameColumnWidth
                                    : Single ;

      /// <summary>
      ///   Setter for FieldNameColumnWidth property.
      /// </summary>
      /// <param name="_value">
      ///   New FieldNameColumnWidth value.
      /// </param>
      procedure fset_FieldNameColumnWidth
                                    ( const _value  : Single
                                    ) ;

      /// <summary>
      ///   Getter for LayoutType property.
      /// </summary>
      /// <returns>
      ///   Current LayoutType value.
      /// </returns>
      function  fget_LayoutType     : TGIS_PvlAttibutesLayoutType ;

      /// <summary>
      ///   Setter for LayoutType property.
      /// </summary>
      /// <param name="_value">
      ///   New LayoutType value.
      /// </param>
      procedure fset_LayoutType     ( const _value  : TGIS_PvlAttibutesLayoutType
                                    ) ;

      /// <summary>
      ///   Getter for RightToLeft property.
      /// </summary>
      /// <returns>
      ///   Current RightToLeft value.
      /// </returns>
      function  fget_RightToLeft    : Boolean ;

      /// <summary>
      ///   Setter for RightToLeft property.
      /// </summary>
      /// <param name="_value">
      ///   New RightToLeft value.
      /// </param>
      procedure fset_RightToLeft    ( const _value  : Boolean
                                    );

      /// <summary>
      ///   Getter for RightToLeftFromTranslation property.
      /// </summary>
      /// <returns>
      ///   Current RightToLeftFromTranslation value.
      /// </returns>
      function  fget_RightToLeftFromTranslation
                                    : Boolean ;

      /// <summary>
      ///   Setter for RightToLeftFromTranslation property.
      /// </summary>
      /// <param name="_value">
      ///   New RightToLeftFromTranslation value.
      /// </param>
      procedure fset_RightToLeftFromTranslation
                                    ( const _value  : Boolean
                                    );

      /// <summary>
      ///   Getter for ReadOnly property.
      /// </summary>
      /// <returns>
      ///   Current ReadOnly value.
      /// </returns>
      function  fget_ReadOnly       : Boolean ;

      /// <summary>
      ///   Setter for ReadOnly property.
      /// </summary>
      /// <param name="_value">
      ///   New ReadOnly value.
      /// </param>
      procedure fset_ReadOnly       ( const _value  : Boolean
                                    ) ;

      /// <summary>
      ///   Getter for ShowBtnCancel property.
      /// </summary>
      /// <returns>
      ///   Current ShowBtnCancel value.
      /// </returns>
      function  fget_ShowBtnCancel  : Boolean ;

      /// <summary>
      ///   Setter for ShowBtnCancel property.
      /// </summary>
      /// <param name="_value">
      ///   New ShowBtnCancel value.
      /// </param>
      procedure fset_ShowBtnCancel  ( const _value  : Boolean
                                    ) ;

      /// <summary>
      ///   Getter for ShowBtnOk property.
      /// </summary>
      /// <returns>
      ///   Current ShowBtnOk value.
      /// </returns>
      function  fget_ShowBtnOk      : Boolean ;

      /// <summary>
      ///   Setter for ShowBtnOk property.
      /// </summary>
      /// <param name="_value">
      ///   New ShowBtnOk value.
      /// </param>
      procedure fset_ShowBtnOk      ( const _value  : Boolean
                                    ) ;

      /// <summary>
      ///   Getter for Units property.
      /// </summary>
      /// <returns>
      ///   Current Units value.
      /// </returns>
      function  fget_Units          : TGIS_CSUnits ;

      /// <summary>
      ///   Setter for Units property.
      /// </summary>
      /// <param name="_value">
      ///   New Units value.
      /// </param>
      procedure fset_Units          ( const _value  : TGIS_CSUnits
                                    ) ;

      /// <summary>
      ///   Getter for UnitsEPSG property.
      /// </summary>
      /// <returns>
      ///   Current UnitsEPSG value.
      /// </returns>
      function  fget_UnitsEPSG      : Integer ;

      /// <summary>
      ///   Setter for UnitsEPSG property.
      /// </summary>
      /// <param name="_value">
      ///   New UnitsEPSG value.
      /// </param>
      procedure fset_UnitsEPSG      ( const _value  : Integer
                                    ) ;

      /// <summary>
      ///   Getter for Font property.
      /// </summary>
      /// <returns>
      ///   Current Font value.
      /// </returns>
      function  fget_Font           : TGIS_Font ;

      /// <summary>
      ///   Setter for Font property.
      /// </summary>
      /// <param name="_value">
      ///   New Font value.
      /// </param>
      procedure fset_Font           ( const _value  : TGIS_Font
                                    ) ;


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

  /// <summary>
  ///   PVL attributes control.
  /// </summary>
  TGIS_PvlControlAttributes = class( TGIS_PvlControl, IGIS_PvlControlAttributes )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                              override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlControlAttributes ;          reintroduce;
      property PlatformControl    : IGIS_PvlControlAttributes
                                    read  fget_PlatformControl;  {$IFDEF OXYGENE}reintroduce;{$ENDIF}

    protected

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_FieldNameColumnWidth
                                    : Single ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_FieldNameColumnWidth
                                    ( const _value  : Single
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_LayoutType     : TGIS_PvlAttibutesLayoutType ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_LayoutType     ( const _value  : TGIS_PvlAttibutesLayoutType
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_RightToLeft    : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_RightToLeft    ( const _value  : Boolean
                                    );

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_RightToLeftFromTranslation
                                    : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_RightToLeftFromTranslation
                                    ( const _value  : Boolean
                                    );

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_ReadOnly       : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_ReadOnly       ( const _value  : Boolean
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_ShowBtnCancel  : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_ShowBtnCancel  ( const _value  : Boolean
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_ShowBtnOk      : Boolean ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_ShowBtnOk      ( const _value  : Boolean
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_Units          : TGIS_CSUnits ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_Units          ( const _value  : TGIS_CSUnits
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_UnitsEPSG      : Integer ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_UnitsEPSG      ( const _value  : Integer
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      function  fget_Font           : TGIS_Font ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure fset_Font           ( const _value  : TGIS_Font
                                    ) ;
    public

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure Clear               ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure Invalidate          ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure NewShape            ( const _shape  : TGIS_Shape
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure ShowShape           ( const _shape  : TGIS_Shape
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure ShowSelected        ( const _layer  : TGIS_LayerVector
                                    ) ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
      procedure CancelChanges       ;

      /// <inheritdoc from="IGIS_PvlControlAttributes"/>
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

//    published
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
      ,FMX.GisPvlControlAttributes
    {$ENDIF}
    {$IFDEF USE_VCL}
      ,VCL.GisPvlControlAttributes
    {$ENDIF}
    ;
{$ENDIF}

function TGIS_PvlControlAttributes.fget_FieldNameColumnWidth
  : Single ;
begin
  Result := PlatformControl.fget_FieldNameColumnWidth ;
end;

procedure TGIS_PvlControlAttributes.fset_FieldNameColumnWidth(
  const _value: Single
) ;
begin
  PlatformControl.fset_FieldNameColumnWidth( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_LayoutType
  : TGIS_PvlAttibutesLayoutType ;
begin
  Result := PlatformControl.fget_LayoutType ;
end;

procedure TGIS_PvlControlAttributes.fset_LayoutType(
  const _value: TGIS_PvlAttibutesLayoutType
) ;
begin
  PlatformControl.fset_LayoutType( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_RightToLeft
  : Boolean ;
begin
  Result := PlatformControl.fget_RightToLeft ;
end;

procedure TGIS_PvlControlAttributes.fset_RightToLeft(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_RightToLeft( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_RightToLeftFromTranslation
  : Boolean ;
begin
  Result := PlatformControl.fget_RightToLeftFromTranslation ;
end;

procedure TGIS_PvlControlAttributes.fset_RightToLeftFromTranslation(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_RightToLeftFromTranslation( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_ReadOnly
  : Boolean ;
begin
  Result := PlatformControl.fget_ReadOnly ;
end;

procedure TGIS_PvlControlAttributes.fset_ReadOnly(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_ReadOnly( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_ShowBtnCancel
  : Boolean ;
begin
  Result := PlatformControl.fget_ShowBtnCancel ;
end;

procedure TGIS_PvlControlAttributes.fset_ShowBtnCancel(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_ShowBtnCancel( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_ShowBtnOk
  : Boolean ;
begin
  Result := PlatformControl.fget_ShowBtnOk ;
end;

procedure TGIS_PvlControlAttributes.fset_ShowBtnOk(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_ShowBtnOk( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_Units
  : TGIS_CSUnits ;
begin
  Result := PlatformControl.fget_Units ;
end;

procedure TGIS_PvlControlAttributes.fset_Units(
  const _value: TGIS_CSUnits
) ;
begin
  PlatformControl.fset_Units( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_UnitsEPSG
  : Integer ;
begin
  Result := PlatformControl.fget_UnitsEPSG ;
end;

procedure TGIS_PvlControlAttributes.fset_UnitsEPSG(
  const _value: Integer
) ;
begin
  PlatformControl.fset_UnitsEPSG( _value ) ;
end;

function TGIS_PvlControlAttributes.fget_Font
  : TGIS_Font ;
begin
  Result := PlatformControl.fget_Font ;
end;

procedure TGIS_PvlControlAttributes.fset_Font(
  const _value: TGIS_Font
) ;
begin
  PlatformControl.fset_Font( _value ) ;
end;

procedure TGIS_PvlControlAttributes.Clear ;
begin
  PlatformControl.Clear ;
end;

procedure TGIS_PvlControlAttributes.Invalidate ;
begin
  PlatformControl.Invalidate ;
end;

procedure TGIS_PvlControlAttributes.NewShape(
  const _shape: TGIS_Shape
) ;
begin
  PlatformControl.NewShape( _shape ) ;
end;

procedure TGIS_PvlControlAttributes.ShowShape(
  const _shape: TGIS_Shape
) ;
begin
  PlatformControl.ShowShape( _shape ) ;
end;

procedure TGIS_PvlControlAttributes.ShowSelected(
  const _layer: TGIS_LayerVector
) ;
begin
  PlatformControl.ShowSelected( _layer ) ;
end;

procedure TGIS_PvlControlAttributes.CancelChanges ;
begin
  PlatformControl.CancelChanges ;
end;

function TGIS_PvlControlAttributes.SaveChanges
  : Boolean ;
begin
  Result := PlatformControl.SaveChanges ;
end;

procedure TGIS_PvlControlAttributes.CopyToClipboard ;
begin
  PlatformControl.CopyToClipboard ;
end;

procedure TGIS_PvlControlAttributes.SubscribedEvent(
  _sender  : TObject ;
  _event   : Integer ;
  _context : TObject
) ;
begin
  PlatformControl.SubscribedEvent( _sender, _event, _context ) ;
end ;

{$REGION 'TGIS_PvlControlAttributes specific'}

procedure TGIS_PvlControlAttributes.initControl;
begin
  inherited;
end;


procedure TGIS_PvlControlAttributes.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.Platform.CreateObject(self, 'Attributes');
end;

function TGIS_PvlControlAttributes.fget_PlatformControl
  : IGIS_PvlControlAttributes;
begin
  Result := oPlatform as IGIS_PvlControlAttributes;
end;

{$ENDREGION 'TGIS_PvlControlAttributes specific'}

end.
