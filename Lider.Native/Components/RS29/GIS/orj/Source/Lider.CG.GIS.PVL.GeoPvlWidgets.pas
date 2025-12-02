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
  PVL Controls which helps us keep one code across the platforms.
}

{$IFDEF DCC}
  unit PVL.GisPvlWidgets;
  {$HPPEMIT '#pragma link "PVL.GisWidgets"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

uses
  {$IFDEF CLR}
    System.Runtime.CompilerServices,
    System.Reflection,
    TatukGIS.NDK,
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Types,
    System.Generics.Collections,

    GisBaseObject,
    GisTypesUI,
    GisClasses,
    GisRtl,
    GisCSSystems,
    GisSymbol,
    GisInterfaces,
    GisPrintManagerAbstract,
    GisViewer,
    GisResource,
    GisTypes,

    PVL.GisPvl ;
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
  ///   PVL labeled edit widget
  /// </summary>
  TGIS_PvlLabeledEdit = class ( TGIS_PvlControl, IGIS_PvlControl )
    private
      oLabel              : TGIS_PvlLabel           ;
      oEdit               : TGIS_PvlEdit            ;
      container           : TGIS_PvlPanel           ;
    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                );                                     override;
    protected
      /// <inheritdoc/>
      procedure fset_Enabled    ( const _value      : Boolean
                                );                                     override;

    public
      /// <summary>
      ///   Label property
      /// </summary>
      property &Label           : TGIS_PvlLabel
                                read  oLabel
                                write oLabel;
      /// <summary>
      ///   Label property
      /// </summary>
      property Edit             : TGIS_PvlEdit
                                read  oEdit
                                write oEdit;
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      /// <inheritdoc/>
      procedure doDestroy         ; override ;
  end;

  /// <summary>
  ///   PVL labeled combobox widget
  /// </summary>
  TGIS_PvlLabeledSelectCombo = class ( TGIS_PvlControl, IGIS_PvlControl )
    private
      oLabel              : TGIS_PvlLabel           ;
      oCombo              : TGIS_PvlComboBox        ;
      container           : TGIS_PvlPanel           ;
    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                );                                     override;
    public
      /// <summary>
      ///   Label property
      /// </summary>
      property &Label           : TGIS_PvlLabel
                                  read  oLabel
                                  write oLabel;
      /// <summary>
      ///   Label property
      /// </summary>
      property ComboBox         : TGIS_PvlComboBox
                                  read  oCombo
                                  write oCombo;
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

      /// <inheritdoc/>
      procedure fset_Enabled    ( const _value      : Boolean
                                );                                     override;
  end;

  /// <summary>
  ///   PVL labeled editable combobox widget
  /// </summary>
  TGIS_PvlLabeledEditCombo = class ( TGIS_PvlControl, IGIS_PvlControl )
    private
      oLabel              : TGIS_PvlLabel           ;
      oCombo              : TGIS_PvlComboEdit       ;
      container           : TGIS_PvlPanel           ;
    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                );                                     override;
    public
      /// <summary>
      ///   Label property
      /// </summary>
      property &Label           : TGIS_PvlLabel
                                  read  oLabel
                                  write oLabel;
      /// <summary>
      ///   Label property
      /// </summary>
      property ComboBox         : TGIS_PvlComboEdit
                                  read  oCombo
                                  write oCombo;
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

      /// <inheritdoc/>
      procedure fset_Enabled    ( const _value      : Boolean
                                );                                     override;
  end;

  /// <summary>
  ///   PVL labeled editable combobox widget
  /// </summary>
  TGIS_PvlLabeledColorCombo = class ( TGIS_PvlControl, IGIS_PvlControl )
    private
      oLabel              : TGIS_PvlLabel           ;
      oCombo              : TGIS_PvlColorComboBox   ;
      container           : TGIS_PvlPanel           ;
    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                );                                     override;
    public
      /// <summary>
      ///   Label property
      /// </summary>
      property &Label           : TGIS_PvlLabel
                                  read  oLabel
                                  write oLabel;
      /// <summary>
      ///   Label property
      /// </summary>
      property ComboBox         : TGIS_PvlColorComboBox
                                  read  oCombo
                                  write oCombo;
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      /// <inheritdoc/>
      procedure doDestroy         ; override ;

      /// <inheritdoc/>
      procedure fset_Enabled      ( const _value      : Boolean
                                  );                                     override;
  end;

  /// <summary>
  ///   PVL color ramp widget.
  /// </summary>
  TGIS_PvlColorRampWidget = class( TGIS_PvlControl, IGIS_PvlControl )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      procedure doDestroy                               ; override ;
    private
      lastColorSchemas    : TGIS_ColorSchemas           ;
      chkReverse          : TGIS_PvlCheckBox            ;
      chkDiscrete         : TGIS_PvlCheckBox            ;
      chkShowAll          : TGIS_PvlCheckBox            ;
      chkShowNames        : TGIS_PvlCheckBox            ;
      cmbRamp             : TGIS_PvlColorRampComboBox   ;
      oPanel              : TGIS_PvlPanel               ;
    private
      FTwoColumns         : Boolean                     ;
    private
      procedure doDiscreteClick         ( _sender : TObject
                                        )                                   ;
      procedure doReverseClick          ( _sender : TObject
                                        )                                   ;
      procedure doShowAllClick          ( _sender : TObject
                                        )                                   ;
      procedure doShowNamesClick        ( _sender : TObject
                                        )                                   ;
      procedure fset_ComboHeight        ( const _value : Integer
                                        )                                   ;
      procedure fset_Mode               ( const _mode : TGIS_ColorMapMode
                                        )                                   ;
      function  fget_Mode               : TGIS_ColorMapMode                 ;
      procedure fset_ReverseVisible     ( const _value : Boolean
                                        )                                   ;
      function  fget_ReverseVisible     : Boolean                           ;
      procedure fset_DiscreteVisible    ( const _value : Boolean
                                        )                                   ;
      function  fget_DiscreteVisible    : Boolean                           ;
      procedure fset_Discrete           ( const _discrete    : Boolean
                                        )                                   ;
      function  fget_Discrete           : Boolean                           ;
      procedure fset_ItemIndex          ( const _index    : Integer
                                        )                                   ;
      function  fget_ItemIndex          : Integer                           ;
      function  fget_Item               ( const _idx        : Integer
                                        ) : String;
      function  fget_ItemCount          : Integer                           ;
      procedure fset_ShowAllVisible     ( const _value : Boolean
                                        )                                   ;
      function  fget_ShowAllVisible     : Boolean                           ;
      procedure fset_ShowAll            ( const _showAll  : Boolean
                                        )                                   ;
      function  fget_ShowAll            : Boolean                           ;
      procedure fset_ShowNamesVisible   ( const _value  : Boolean
                                        )                                   ;
      function  fget_ShowNamesVisible   : Boolean                           ;
      procedure fset_ColorSchemas       ( const _type   : TGIS_ColorSchemas
                                        )                                   ;
      function  fget_ColorSchemas       : TGIS_ColorSchemas                 ;
      procedure fset_ShowNames          ( const _showNames  : Boolean
                                        )                                   ;
      function  fget_ShowNames          : Boolean                           ;
      procedure fset_Reverse            ( const _reverse    : Boolean
                                        )                                   ;
      function  fget_Reverse            : Boolean                           ;
      procedure fset_OnChange           ( const _onChange   : TGIS_PvlEvent
                                        )                                   ;
      function  fget_OnChange           : TGIS_PvlEvent                     ;

      procedure AddEvents ;
    protected
      /// <inheritdoc/>
      procedure fset_Enabled            ( const _value      : Boolean
                                        );                          override;
    public
      /// <summary>
      ///   Fill the control with data.
      /// </summary>
      procedure Fill;

      /// <summary>
      ///   Locks combobox from updates.
      /// </summary>
      procedure Lock;

      /// <summary>
      ///   Unlocks combobox from updates.
      /// </summary>
      procedure Unlock;

      /// <summary>
      ///   Color map value.
      /// </summary>
      /// <param name="_subClass">
      ///   get subclass of a ramp with specified number of colors, if available;
      ///   if 0, get default colormap;
      ///   if -1, get subclass with maximum number of colors;
      /// </param>
      /// <returns>
      ///   Array of colormap
      /// </returns>
      function  Value           ( const _subClass   : Integer = -1
                                ) : TGIS_ColorMapArray              ;

      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                );                                     override;

    public

      /// <summary>
      ///   Height of the ramp combobox, has to be set before place method.
      /// </summary>
      property ComboHeight      : Integer
                                  write fset_ComboHeight ;

      /// <summary>
      ///   If true, then checkboxes are placed in two columns, has to be set before place method.
      /// </summary>
      property TwoColumns       : Boolean
                                  read  FTwoColumns
                                  write FTwoColumns ;

      /// <summary>
      ///   ColorMap mode.
      /// </summary>
      property Mode             : TGIS_ColorMapMode
                                  read  fget_Mode
                                  write fset_Mode;
      /// <summary>
      ///   Color schema filter.
      /// </summary>
      property ColorSchemas     : TGIS_ColorSchemas
                                  read  fget_ColorSchemas
                                  write fset_ColorSchemas;

      /// <summary>
      ///   Reverse colormap.
      /// </summary>
      property &Reverse         : Boolean
                                  read  fget_Reverse
                                  write fset_Reverse;

      /// <summary>
      ///   Show captions for ramps.
      /// </summary>
      property ShowNames        : Boolean
                                  read  fget_ShowNames
                                  write fset_ShowNames;

      /// <summary>
      ///   Show all ramps.
      /// </summary>
      property ShowAll          : Boolean
                                  read  fget_ShowAll
                                  write fset_ShowAll;

      /// <summary>
      ///   Discrete ramps.
      /// </summary>
      property Discrete         : Boolean
                                  read  fget_Discrete
                                  write fset_Discrete;

      /// <summary>
      ///   Ramp count.
      /// </summary>
      property ItemCount        : Integer
                                  read  fget_ItemCount;

      /// <summary>
      ///   Ramp index.
      /// </summary>
      property ItemIndex        : Integer
                                  read  fget_ItemIndex
                                  write fset_ItemIndex;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">
      ///   ID of the item you want to get
      /// </param>
      property Items[ const _idx : Integer ]
                                : String
                                  read  fget_Item;


      /// <summary>
      ///   If true then show Reverse checkbox
      /// </summary>
      property ReverseVisible   : Boolean
                                  read  fget_ReverseVisible
                                  write fset_ReverseVisible ;

      /// <summary>
      ///   If true then show Discrete checkbox
      /// </summary>
      property DiscreteVisible  : Boolean
                                  read  fget_DiscreteVisible
                                  write fset_DiscreteVisible ;

      /// <summary>
      ///   If true then show ShowAll checkbox
      /// </summary>
      property ShowAllVisible   : Boolean
                                  read  fget_ShowAllVisible
                                  write fset_ShowAllVisible ;

      /// <summary>
      ///   If true then show ShowNames checkbox
      /// </summary>
      property ShowNamesVisible : Boolean
                                  read  fget_ShowNamesVisible
                                  write fset_ShowNamesVisible ;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;


//##############################################################################
implementation

{$REGION 'TGIS_PvlLabeledEdit'}

procedure TGIS_PvlLabeledEdit.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  container := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := container.Platform ;

  oLabel := TGIS_PvlLabel.Create( container.Context ) ;
  oEdit  := TGIS_PvlEdit.Create( container.Context ) ;
end;

procedure TGIS_PvlLabeledEdit.doDestroy;
begin
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlLabeledEdit.Place(
  const _width     : Integer;
  const _height    : Integer;
  const _xsibling  : TGIS_PvlControl;
  const _xdistance : Integer;
  const _ysibling  : TGIS_PvlControl;
  const _ydistance : Integer
);
begin
  inherited ;

  oLabel.Place( Width, 0, nil, 0, nil, 0 ) ;
  oEdit.Place( Width, 0, nil, 0, oLabel, 0 ) ;

  if _height = 0 then
    Height := oEdit.Height + RoundS( Context.PPIFix * Context.HSpace ) + oLabel.Height ;
end;

procedure TGIS_PvlLabeledEdit.fset_Enabled(
  const _value : Boolean
) ;
begin
  inherited ;

  oEdit.Enabled := _value ;
end;

{$ENDREGION 'TGIS_PvlLabeledEdit'}

{$REGION 'TGIS_PvlLabeledSelectCombo'}

procedure TGIS_PvlLabeledSelectCombo.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  container := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := container.Platform ;

  oLabel := TGIS_PvlLabel.Create( container.Context ) ;
  oCombo  := TGIS_PvlComboBox.Create( container.Context ) ;
end;

procedure TGIS_PvlLabeledSelectCombo.doDestroy;
begin
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlLabeledSelectCombo.Place(
  const _width     : Integer;
  const _height    : Integer;
  const _xsibling  : TGIS_PvlControl;
  const _xdistance : Integer;
  const _ysibling  : TGIS_PvlControl;
  const _ydistance : Integer
);
begin
  inherited ;

  oLabel.Place( Width, 0, nil, 0, nil, 0 ) ;
  oCombo.Place( Width, 0, nil, 0, oLabel, 0 ) ;

  if _height = 0 then
    Height := oCombo.Height + RoundS( Context.PPIFix * Context.HSpace ) + oLabel.Height ;
end;

procedure TGIS_PvlLabeledSelectCombo.fset_Enabled(
  const _value : Boolean
) ;
begin
  inherited ;

  oCombo.Enabled := _value ;
end;

{$ENDREGION 'TGIS_PvlLabeledSelectCombo'}

{$REGION 'TGIS_PvlLabeledEditCombo'}

procedure TGIS_PvlLabeledEditCombo.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  container := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := container.Platform ;

  oLabel := TGIS_PvlLabel.Create( container.Context ) ;
  oCombo := TGIS_PvlComboEdit.Create( container.Context ) ;
end;

procedure TGIS_PvlLabeledEditCombo.doDestroy;
begin
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlLabeledEditCombo.Place(
  const _width     : Integer;
  const _height    : Integer;
  const _xsibling  : TGIS_PvlControl;
  const _xdistance : Integer;
  const _ysibling  : TGIS_PvlControl;
  const _ydistance : Integer
);
begin
  inherited ;

  oLabel.Place( Width, 0, nil, 0, nil, 0 ) ;
  oCombo.Place( Width, 0, nil, 0, oLabel, 0 ) ;

  if _height = 0 then
    Height := oCombo.Height + RoundS( Context.PPIFix * Context.HSpace ) + oLabel.Height ;
end;

procedure TGIS_PvlLabeledEditCombo.fset_Enabled(
  const _value : Boolean
) ;
begin
  inherited ;

  oCombo.Enabled := _value ;
end;

{$ENDREGION 'TGIS_PvlLabeledEditCombo'}

{$REGION 'TGIS_PvlLabeledColorCombo'}

procedure TGIS_PvlLabeledColorCombo.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  container := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := container.Platform ;

  oLabel := TGIS_PvlLabel.Create( container.Context ) ;
  oCombo := TGIS_PvlColorComboBox.Create( container.Context ) ;
end;

procedure TGIS_PvlLabeledColorCombo.doDestroy;
begin
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlLabeledColorCombo.Place(
  const _width     : Integer;
  const _height    : Integer;
  const _xsibling  : TGIS_PvlControl;
  const _xdistance : Integer;
  const _ysibling  : TGIS_PvlControl;
  const _ydistance : Integer
);
begin
  inherited ;

  oLabel.Place( Width, 0, nil, 0, nil, 0 ) ;
  oCombo.Place( Width, 0, nil, 0, oLabel, 0 ) ;

  if _height = 0 then
    Height := oCombo.Height + RoundS( Context.PPIFix * Context.HSpace ) + oLabel.Height ;

  oCombo.Fill( false, false ) ;
end;

procedure TGIS_PvlLabeledColorCombo.fset_Enabled(
  const _value : Boolean
) ;
begin
  inherited ;

  oCombo.Enabled := _value ;
end;

{$ENDREGION 'TGIS_PvlLabeledColorCombo'}

{$REGION 'TGIS_PvlColorRampWidget'}

procedure TGIS_PvlColorRampWidget.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  oPanel    := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := oPanel.Platform ;

  cmbRamp := TGIS_PvlColorRampComboBox.Create( oPanel.Context ) ;

  chkDiscrete := TGIS_PvlCheckBox.Create( oPanel.Context ) ;
  chkDiscrete.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMP_DISCRETE ) ;
  chkDiscrete.Visible := True ;
  chkDiscrete.Checked := cmbRamp.Mode = TGIS_ColorMapMode.Discrete ;

  chkReverse := TGIS_PvlCheckBox.Create( oPanel.Context ) ;
  chkReverse.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMP_REVERSE ) ;
  chkReverse.Visible := True ;
  chkReverse.Checked := cmbRamp.Reverse ;

  chkShowAll := TGIS_PvlCheckBox.Create( oPanel.Context ) ;
  chkShowAll.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMP_SHOWALL ) ;
  chkShowAll.Visible := True ;

  {$IFDEF CLR}
    chkShowAll.Checked := cmbRamp.ColorSchemas = TGIS_ColorSchema.Diverging and
                                                 TGIS_ColorSchema.Miscellaneous and
                                                 TGIS_ColorSchema.Qualitative and
                                                 TGIS_ColorSchema.Sequential ;
  {$ENDIF}
  {$IFDEF SWING}
    chkShowAll.Checked := cmbRamp.ColorSchemas = [TGIS_ColorSchema.Diverging,
                                                  TGIS_ColorSchema.Miscellaneous,
                                                  TGIS_ColorSchema.Qualitative,
                                                  TGIS_ColorSchema.Sequential] ;
  {$ENDIF}
  {$IFDEF DCC}
    chkShowAll.Checked := cmbRamp.ColorSchemas = [TGIS_ColorSchema.Diverging,
                                                  TGIS_ColorSchema.Miscellaneous,
                                                  TGIS_ColorSchema.Qualitative,
                                                  TGIS_ColorSchema.Sequential] ;
  {$ENDIF}

  chkShowNames := TGIS_PvlCheckBox.Create( oPanel.Context ) ;
  chkShowNames.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMP_SHOWNAMES ) ;
  chkShowNames.Visible := True ;
  chkShowNames.Checked := cmbRamp.ShowNames ;
end;

procedure TGIS_PvlColorRampWidget.doDestroy;
begin
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlColorRampWidget.fset_DiscreteVisible(
  const _value : Boolean
) ;
begin
  chkDiscrete.Visible := _value ;
end;

function TGIS_PvlColorRampWidget.fget_DiscreteVisible
  : Boolean ;
begin
  Result := chkDiscrete.Visible ;
end;

procedure TGIS_PvlColorRampWidget.fset_ReverseVisible(
  const _value : Boolean
) ;
begin
  chkReverse.Visible := _value ;
end;

function TGIS_PvlColorRampWidget.fget_ReverseVisible
  : Boolean ;
begin
  Result := chkReverse.Visible ;
end;

procedure TGIS_PvlColorRampWidget.fset_ShowAllVisible(
  const _value : Boolean
) ;
begin
  chkShowAll.Visible := _value ;
end;

function TGIS_PvlColorRampWidget.fget_ShowAllVisible
  : Boolean ;
begin
  Result := chkShowAll.Visible ;
end;

procedure TGIS_PvlColorRampWidget.fset_ShowNamesVisible(
  const _value : Boolean
) ;
begin
  chkShowNames.Visible := _value ;
end;

function TGIS_PvlColorRampWidget.fget_ShowNamesVisible
  : Boolean ;
begin
  Result := chkShowNames.Visible ;
end;

procedure TGIS_PvlColorRampWidget.fset_ComboHeight(
  const _value : Integer
) ;
begin
  cmbRamp.Height := _value ;
end;

procedure TGIS_PvlColorRampWidget.fset_Mode(
  const _mode: TGIS_ColorMapMode
) ;
begin
  cmbRamp.Mode := _mode ;
  chkDiscrete.Checked := _mode = TGIS_ColorMapMode.Discrete ;

  cmbRamp.Fill ;
end;

function TGIS_PvlColorRampWidget.fget_Mode
  : TGIS_ColorMapMode;
begin
  Result := cmbRamp.Mode ;
end;

procedure TGIS_PvlColorRampWidget.fset_ColorSchemas(
  const _type: TGIS_ColorSchemas
) ;
begin
  cmbRamp.ColorSchemas := _type ;

  {$IFDEF CLR}
    chkShowAll.Checked := _type = TGIS_ColorSchema.Diverging and
                                   TGIS_ColorSchema.Miscellaneous and
                                   TGIS_ColorSchema.Qualitative and
                                   TGIS_ColorSchema.Sequential ;
  {$ENDIF}
  {$IFDEF SWING}
    chkShowAll.Checked := _type = [TGIS_ColorSchema.Diverging,
                                   TGIS_ColorSchema.Miscellaneous,
                                   TGIS_ColorSchema.Qualitative,
                                   TGIS_ColorSchema.Sequential] ;
  {$ENDIF}
  {$IFDEF DCC}
    chkShowAll.Checked := _type = [TGIS_ColorSchema.Diverging,
                                   TGIS_ColorSchema.Miscellaneous,
                                   TGIS_ColorSchema.Qualitative,
                                   TGIS_ColorSchema.Sequential] ;
  {$ENDIF}

  cmbRamp.Fill ;
end;

function TGIS_PvlColorRampWidget.fget_ColorSchemas
  : TGIS_ColorSchemas ;
begin
  Result := cmbRamp.ColorSchemas ;
end;

procedure TGIS_PvlColorRampWidget.fset_Reverse(
  const _reverse: Boolean
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;
  cmbRamp.Reverse := _reverse ;
  chkReverse.Checked := _reverse ;

  cmbRamp.Fill ;

  cmbRamp.Index := currIdx ;
end;

function TGIS_PvlColorRampWidget.fget_Reverse
  : Boolean ;
begin
  Result := cmbRamp.Reverse ;
end;

procedure TGIS_PvlColorRampWidget.fset_ShowNames(
  const _showNames : Boolean
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;

  cmbRamp.ShowNames := _showNames ;
  chkShowNames.Checked := _showNames ;

  cmbRamp.Fill ;

  cmbRamp.Index := currIdx ;
end;

function TGIS_PvlColorRampWidget.fget_ShowNames
  : Boolean ;
begin
  Result := cmbRamp.ShowNames ;
end;

procedure TGIS_PvlColorRampWidget.fset_ShowAll(
  const _showAll : Boolean
) ;
begin
  if _showAll then begin
    lastColorSchemas := cmbRamp.ColorSchemas ;
    cmbRamp.ColorSchemas := [TGIS_ColorSchema.Diverging,
                              TGIS_ColorSchema.Miscellaneous,
                              TGIS_ColorSchema.Qualitative,
                              TGIS_ColorSchema.Sequential] ;
  end
  else begin
    {$IFDEF CLR}
      if lastColorSchemas = 0 then
    {$ENDIF}
    {$IFDEF JAVA}
      if lastColorSchemas = [] then
    {$ENDIF}
    {$IFDEF DCC}
      if lastColorSchemas = [] then
    {$ENDIF}
      cmbRamp.ColorSchemas := [TGIS_ColorSchema.Diverging,
                                TGIS_ColorSchema.Miscellaneous,
                                TGIS_ColorSchema.Qualitative,
                                TGIS_ColorSchema.Sequential]
    else
      cmbRamp.ColorSchemas := lastColorSchemas ;
  end ;

  cmbRamp.Fill ;
  cmbRamp.Index := 0 ;

  chkShowAll.Checked := _showAll ;
end;

function TGIS_PvlColorRampWidget.fget_ShowAll
  : Boolean ;
begin
  Result := chkShowAll.Checked ;
end;

procedure TGIS_PvlColorRampWidget.fset_Discrete(
  const _discrete : Boolean
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;

  if _discrete then
    cmbRamp.Mode := TGIS_ColorMapMode.Discrete
  else
    cmbRamp.Mode := TGIS_ColorMapMode.Continuous ;

  cmbRamp.Fill ;

  cmbRamp.Index := currIdx ;

  chkDiscrete.Checked := _discrete ;
end;

function TGIS_PvlColorRampWidget.fget_Discrete
  : Boolean ;
begin
  Result := cmbRamp.Mode = TGIS_ColorMapMode.Discrete ;
end;

function TGIS_PvlColorRampWidget.fget_ItemCount
  : Integer ;
begin
  Result := cmbRamp.ItemCount ;
end;

procedure TGIS_PvlColorRampWidget.fset_ItemIndex(
  const _index : Integer
) ;
begin
  cmbRamp.Index := _index ;
end;

function TGIS_PvlColorRampWidget.fget_ItemIndex
  : Integer ;
begin
  Result := cmbRamp.Index ;
end;

function TGIS_PvlColorRampWidget.fget_Item(
  const _idx : Integer
) : String ;
begin
  Result := cmbRamp.Items[_idx] ;
end;

procedure TGIS_PvlColorRampWidget.fset_OnChange(
  const _onChange: TGIS_PvlEvent
) ;
begin
  cmbRamp.OnChange := _onChange ;
end;

function TGIS_PvlColorRampWidget.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := cmbRamp.OnChange ;
end;

procedure TGIS_PvlColorRampWidget.fset_Enabled(
  const _value : Boolean
) ;
begin
  inherited ;

  chkDiscrete.Enabled := _value ;
  chkReverse.Enabled := _value ;
  chkShowAll.Enabled := _value ;
  chkShowNames.Enabled := _value ;
  cmbRamp.Enabled := _value ;
end;

procedure TGIS_PvlColorRampWidget.Fill ;
begin
  cmbRamp.Fill ;
end;

procedure TGIS_PvlColorRampWidget.Lock;
begin
  cmbRamp.Lock ;
end;

procedure TGIS_PvlColorRampWidget.Unlock;
begin
  cmbRamp.Unlock ;
end;

function TGIS_PvlColorRampWidget.Value(
  const _subClass: Integer
) : TGIS_ColorMapArray ;
begin
  Result := cmbRamp.Value( _subClass ) ;
end;

procedure TGIS_PvlColorRampWidget.doDiscreteClick(
  _sender: TObject
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;

  if chkDiscrete.Checked then
    cmbRamp.Mode := TGIS_ColorMapMode.Discrete
  else
    cmbRamp.Mode := TGIS_ColorMapMode.Continuous ;

  cmbRamp.Fill ;
  cmbRamp.Index := currIdx ;
end;

procedure TGIS_PvlColorRampWidget.doReverseClick(
  _sender: TObject
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;

  cmbRamp.Reverse := chkReverse.Checked ;

  cmbRamp.Fill ;
  cmbRamp.Index := currIdx ;
end;

procedure TGIS_PvlColorRampWidget.doShowAllClick(
  _sender: TObject
) ;
begin
    if chkShowAll.Checked then begin
      lastColorSchemas := cmbRamp.ColorSchemas ;
      cmbRamp.ColorSchemas := [TGIS_ColorSchema.Diverging,
                               TGIS_ColorSchema.Miscellaneous,
                               TGIS_ColorSchema.Qualitative,
                               TGIS_ColorSchema.Sequential] ;
    end
    else begin
      {$IFDEF CLR}
        if lastColorSchemas = 0 then
      {$ENDIF}
      {$IFDEF JAVA}
        if lastColorSchemas = [] then
      {$ENDIF}
      {$IFDEF DCC}
        if lastColorSchemas = [] then
      {$ENDIF}
        cmbRamp.ColorSchemas := [TGIS_ColorSchema.Diverging,
                                 TGIS_ColorSchema.Miscellaneous,
                                 TGIS_ColorSchema.Qualitative,
                                 TGIS_ColorSchema.Sequential]
      else
        cmbRamp.ColorSchemas := lastColorSchemas ;
    end ;

    cmbRamp.Fill ;
    cmbRamp.Index := 0 ;
end;

procedure TGIS_PvlColorRampWidget.doShowNamesClick(
  _sender: TObject
) ;
var
  currIdx : Integer ;
begin
  currIdx := cmbRamp.Index ;

  cmbRamp.ShowNames := chkShowNames.Checked ;

  cmbRamp.Fill ;
  cmbRamp.Index := currIdx ;
end;

procedure TGIS_PvlColorRampWidget.Place(
  const _width: Integer;
  const _height: Integer;
  const _xsibling: TGIS_PvlControl;
  const _xdistance: Integer;
  const _ysibling: TGIS_PvlControl;
  const _ydistance: Integer
) ;
var
  iChkWidth : Integer ;
  height    : Integer ;
begin
  Self.Height := 0 ;

  inherited ;

  cmbRamp.Place( Width, 0, nil, 0, nil, 0 ) ;

  if not FTwoColumns then begin
    iChkWidth := RoundS( Width ) ;

    height := Context.HMargin + cmbRamp.Height ;

    chkDiscrete.Place( iChkWidth, 0, nil, 0, cmbRamp, Context.HSpace {$IFDEF DCC} * 2 {$ENDIF} ) ;

    if chkDiscrete.Visible then
      height := height + chkDiscrete.Height + Context.HSpace ;

    chkReverse.Place( iChkWidth, 0, nil, 0, chkDiscrete, 0 ) ;

    if chkReverse.Visible then
      height := height + chkReverse.Height + Context.HSpace ;

    chkShowAll.Place( iChkWidth, 0, nil, 0, chkReverse, 0 ) ;

    if chkShowAll.Visible then
      height := height + chkShowAll.Height + Context.HSpace ;

    chkShowNames.Place( iChkWidth, 0, nil, 0, chkShowAll, 0 ) ;

    if chkShowNames.Visible then
      height := height + chkShowNames.Height + Context.HSpace ;

  end else begin
    iChkWidth := RoundS( Width / 2 ) - Context.HMargin;

    height := Context.HMargin + cmbRamp.Height ;

    chkDiscrete.Place( iChkWidth, 0, nil, 0, cmbRamp, Context.HSpace {$IFDEF DCC} * 2 {$ENDIF} ) ;

    if chkDiscrete.Visible then
      height := height + chkDiscrete.Height + Context.HSpace ;

    chkReverse.Place( iChkWidth, 0, nil, 0, chkDiscrete, 0 ) ;

    if chkReverse.Visible then
      height := height + chkReverse.Height + Context.HSpace ;

    height := Context.HMargin + cmbRamp.Height ;

    chkShowAll.Place( iChkWidth, 0, chkDiscrete, Context.HMargin, cmbRamp, Context.HSpace {$IFDEF DCC} * 2 {$ENDIF} ) ;

    if chkShowAll.Visible then
      height := height + chkShowAll.Height + Context.HSpace ;

    chkShowNames.Place( iChkWidth, 0, chkReverse, Context.HMargin, chkShowAll, 0 ) ;

    if chkShowNames.Visible then
      height := height + chkShowNames.Height + Context.HSpace ;
  end;

  AddEvents ;

  height := height + Context.HMargin ;

  If Self.Height = 0 then
    Self.Height := height ;
end;

procedure TGIS_PvlColorRampWidget.AddEvents ;
begin
  {$IFDEF DCC}
    chkDiscrete.OnClick := doDiscreteClick ;
  {$ELSE}
    chkDiscrete.OnClick := @doDiscreteClick ;
  {$ENDIF}
  {$IFDEF DCC}
    chkReverse.OnClick := doReverseClick ;
  {$ELSE}
    chkReverse.OnClick := @doReverseClick ;
  {$ENDIF}
  {$IFDEF DCC}
    chkShowAll.OnClick := doShowAllClick ;
  {$ELSE}
    chkShowAll.OnClick := @doShowAllClick ;
  {$ENDIF}
  {$IFDEF DCC}
    chkShowNames.OnClick := doShowNamesClick ;
  {$ELSE}
    chkShowNames.OnClick := @doShowNamesClick ;
  {$ENDIF}
end;

{$ENDREGION 'TGIS_PvlColorRampWidget'}

//==================================== END =====================================
end.

