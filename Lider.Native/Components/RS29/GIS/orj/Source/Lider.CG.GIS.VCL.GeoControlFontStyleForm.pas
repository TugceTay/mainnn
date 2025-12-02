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
  Visual control for displaying font style.<p>
}

unit VCL.GisControlFontStyleForm ;
{$HPPEMIT '#pragma link "VCL.GisControlFontStyleForm"'}

{$INCLUDE GisInclude.inc}

interface

uses
  VCL.StdCtrls,
  VCL.Controls,
  VCL.ExtCtrls,
  GisTypes,
  GisTypesUI,
  VCL.GisModalForm;

type

  {#gendoc:hide}
  TGIS_FontStylePanel = class( TPanel )
    protected
      procedure Paint ; override ;
  end ;

  /// <summary>
  ///   Visual form for managing font style.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlPrintTemplateForm.
  /// </remarks>
  TGIS_ControlFontStyleForm = class( TGIS_ModalForm )
    private
      chkBold      : TCheckBox ;
      chkUnderline : TCheckBox ;
      chkItalic    : TCheckBox ;
      chkStrikeout : TCheckBox ;

    private
      function  fget_FontStyle : TGIS_FontStyles ;
      procedure fset_FontStyle ( _value : TGIS_FontStyles ) ;
    protected

      /// <inheritdoc/>
      procedure initForm         ; override;

      /// <inheritdoc/>
      procedure initControls     ; override;

    public

      /// <summary>
      ///   Show dialog for defining an element position.
      /// </summary>
      /// <returns>
      ///   modal result
      /// </returns>
      function Show       : Integer ; overload ;

      /// <summary>
      ///   Show dialog for defining an element position.
      /// </summary>
      /// <param name="_help">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function Show       ( const _help  : TGIS_HelpEvent
                          ) : Integer ; overload ;

    public
      /// <summary>
      ///   Font style set by the form.
      /// </summary>
      property FontStyle   : TGIS_FontStyles read  fget_FontStyle
                                             write fset_FontStyle ;

  end;

//##############################################################################
implementation

uses
  System.Classes,
  GisResource,
  VCL.GisControlHelper ;

//==============================================================================
// TGIS_FontStylePanel
//==============================================================================

  procedure TGIS_FontStylePanel.Paint ;
  begin
    inherited ;
  end ;

//==============================================================================
// TGIS_ControlFontStyleForm
//==============================================================================

  procedure TGIS_ControlFontStyleForm.initForm ;
  begin
    Self.ClientHeight := 93 ;
    Self.ClientWidth := 293 ;
    Self.Name := 'TGIS_ControlPrintTemplateFontStyle' ;
    Self.Caption := _rsrc( GIS_RS_TPL_DESIGNER_FONTSTYLE ) ;
  end ;

  procedure TGIS_ControlFontStyleForm.initControls ;
  var
    anchor : TAnchors ;
    t : Integer ;
    lcolumn : Integer ;
    rcolumn : Integer ;
  begin
    if Self.BiDiMode = bdRightToLeft then
      anchor := [akRight, akTop]
    else
      anchor := [akLeft, akTop] ;

    t := 12 ;
    lcolumn := 16 ;
    rcolumn := 136 ;

    chkBold := TCheckBox.Create( Self ) ;
    chkBold.Parent := Self ;
    chkBold.Anchors := anchor ;
    chkBold.Top := t ;
    PlaceControl( BiDiMode, nil, chkBold, lcolumn, chkBold.Width ) ;
    chkBold.Caption:= _rsrc( GIS_RS_LEGEND_DLGFONT_CHKBOLD ) ;

    chkUnderline := TCheckBox.Create( Self ) ;
    chkUnderline.Parent := Self ;
    chkUnderline.Anchors := anchor ;
    chkUnderline.Top := t ;
    PlaceControl( BiDiMode, nil, chkUnderline, rcolumn, chkUnderline.Width ) ;
    chkUnderline.Caption:= _rsrc( GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE ) ;

    t := t + chkBold.Height + 4 ;

    chkItalic := TCheckBox.Create( Self ) ;
    chkItalic.Parent := Self ;
    chkItalic.Anchors := anchor ;
    chkItalic.Top := t ;
    PlaceControl( BiDiMode, nil, chkItalic, lcolumn, chkItalic.Width ) ;
    chkItalic.Caption:= _rsrc( GIS_RS_LEGEND_DLGFONT_CHKITALIC ) ;

    chkStrikeout := TCheckBox.Create( Self ) ;
    chkStrikeout.Parent := Self ;
    chkStrikeout.Anchors := anchor ;
    chkStrikeout.Top := t ;
    PlaceControl( BiDiMode, nil, chkStrikeout, rcolumn, chkItalic.Width ) ;
    chkStrikeout.Caption:= _rsrc( GIS_RS_LEGEND_DLGFONT_STRIKEOUT ) ;
  end ;

  function TGIS_ControlFontStyleForm.fget_FontStyle
    : TGIS_FontStyles ;
  begin
    Result := [] ;
    if chkBold.Checked then
      Result := Result + [TGIS_FontStyle.Bold] ;
    if chkItalic.Checked then
      Result := Result + [TGIS_FontStyle.Italic] ;
    if chkUnderline.Checked then
      Result := Result + [TGIS_FontStyle.Underline] ;
    if chkStrikeout.Checked then
      Result := Result + [TGIS_FontStyle.StrikeOut] ;
  end ;

  procedure TGIS_ControlFontStyleForm.fset_FontStyle(
    _value : TGIS_FontStyles
  ) ;
  begin
    chkBold.Checked      := TGIS_FontStyle.Bold in _value ;
    chkItalic.Checked    := TGIS_FontStyle.Italic in _value ;
    chkUnderline.Checked := TGIS_FontStyle.Underline in _value ;
    chkStrikeout.Checked := TGIS_FontStyle.StrikeOut in _value ;
  end ;

  function TGIS_ControlFontStyleForm.Show
    : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Show( hlp ) ;
  end ;

  function TGIS_ControlFontStyleForm.Show(
    const _help  : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    Result  := ShowModal ;
  end ;

//==================================== END =====================================
end.

