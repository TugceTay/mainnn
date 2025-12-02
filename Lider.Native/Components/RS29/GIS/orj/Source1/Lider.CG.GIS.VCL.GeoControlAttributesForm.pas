//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Visual control for displaying shape attributes. Rename/Delete/Add dialog.<p>
}

unit Lider.CG.GIS.VCL.GeoControlAttributesForm ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlAttributesForm"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  VCL.StdCtrls,
  VCL.Controls,

  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.VCL.GeoModalForm,
  Lider.CG.GIS.VCL.GeoValueValidatorHelper ;

type

  /// <summary>
  ///   Visual form for managing Add/Remove/Delete of attribute.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlAttributes.
  /// </remarks>
  TGIS_ControlAttributesForm = class( TGIS_ModalForm )
    private
      lblName : TLabel;
      edtName : TEdit;
      lblType : TLabel;
      cmbType : TComboBox;
      lblWidth : TLabel;
      edtWidth : TEdit;
      vvedtWidth : IGIS_ValueValidator ;
      lblDecimal : TLabel;
      edtDecimal : TEdit;
      vvedtDecimal : IGIS_ValueValidator ;
      procedure frmDestroy    ( _sender  : TObject ) ;

      // Field parameter change handler.
      procedure fldChange     ( _sender  : TObject ) ;

      procedure btnOKClick    ( _sender  : TObject ) ; override;

    private

      // Altered field definition.
      FXField  : TGIS_FieldInfo ;

      // Mode: 0-add, 1-modify, 2-delete.
      oMode  : Integer ;

      // Layer object on which operations will be performed.
      oLayer : TGIS_LayerVector ;

      // Read field given by _name from given _layer. Data will be read into XField.
      procedure readField    ( const _layer : TGIS_LayerVector ;
                               const _name : String
                             ) ;

      // Prepare dialog with default values.
      procedure prepareDialog  ;

      // Prepare width and decimal parameters based on field type.
      procedure prepareWidth   ;

    protected

      /// <inheritdoc/>
      procedure initForm         ; override;

      /// <inheritdoc/>
      procedure initControls     ; override;

    public

      /// <summary>
      ///   Show dialog for creating a new field. New field definition will be
      ///   available in XField.
      /// </summary>
      /// <param name="_layer">
      ///   layer object on which field will be created
      /// </param>
      /// <param name="_help">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowAdd    ( const _layer : TGIS_LayerVector ;
                            const _help  : TGIS_HelpEvent
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for creating a new field. New field definition will be
      ///   available in XField.
      /// </summary>
      /// <param name="_layer">
      ///   layer object on which field will be created
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowAdd    ( const _layer : TGIS_LayerVector
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for creating an existing field. Renamed field definition
      ///   will be available in XField.
      /// </summary>
      /// <param name="_layer">
      ///   layer object to which field belong
      /// </param>
      /// <param name="_name">
      ///   name of field to be renamed
      /// </param>
      /// <param name="_help">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDREADONLY
      /// </exception>
      function ShowModify ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _help  : TGIS_HelpEvent
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for creating an existing field. Renamed field definition
      ///   will be available in XField.
      /// </summary>
      /// <param name="_layer">
      ///   layer object to which field belong
      /// </param>
      /// <param name="_name">
      ///   name of field to be renamed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDREADONLY
      /// </exception>
      function ShowModify ( const _layer : TGIS_LayerVector ;
                            const _name  : String
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for deleting an existing field.
      /// </summary>
      /// <param name="_layer">
      ///   layer object to which field belong
      /// </param>
      /// <param name="_name">
      ///   name of field to be deleted
      /// </param>
      /// <param name="_help">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowDelete ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _help  : TGIS_HelpEvent
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for deleting an existing field.
      /// </summary>
      /// <param name="_layer">
      ///   layer object to which field belong
      /// </param>
      /// <param name="_name">
      ///   name of field to be deleted
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowDelete ( const _layer : TGIS_LayerVector ;
                            const _name  : String
                          ) : Integer ; overload;

    public

      /// <summary>
      ///   Altered field definition.
      /// </summary>
      property XField : TGIS_FieldInfo read FXField ;

  end;


//##############################################################################
implementation

uses
  Math,
  SysUtils,
  System.Classes,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// TGIS_ControlAttributesForm
//==============================================================================

  procedure TGIS_ControlAttributesForm.initForm ;
  begin
    Self.ClientHeight := 88 ;
    Self.ClientWidth := 307 ;
    Self.Name := 'TGIS_ControlAttributesForm' ;
    Self.OnDestroy := frmDestroy ;
  end ;

  procedure TGIS_ControlAttributesForm.initControls ;
  var
    anchors   : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors  := [akRight, akTop]
    else
      anchors  := [akLeft, akTop] ;

    lblName := TLabel.Create( Self ) ;
    lblName.Parent := Self ;
    lblName.Anchors := anchors ;
    lblName.Top := 8 ;
    lblName.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblName, 8, 38 ) ;
    lblName.AutoSize := False ;
    lblName.Caption := _rsrc( GIS_RS_ATTRIBUTES_DLG_LBLNAME ) ;
    lblName.FocusControl := edtName ;

    lblType := TLabel.Create( Self ) ;
    lblType.Parent := Self ;
    lblType.Anchors := anchors ;
    lblType.Top := 8 ;
    lblType.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblType, 128, 34 ) ;
    lblType.AutoSize := False ;
    lblType.Caption := _rsrc( GIS_RS_ATTRIBUTES_DLG_LBLTYPE ) ;
    lblType.FocusControl := cmbType ;

    lblWidth := TLabel.Create( Self ) ;
    lblWidth.Parent := Self ;
    lblWidth.Anchors := anchors ;
    lblWidth.Top := 8 ;
    lblWidth.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblWidth, 200, 38 ) ;
    lblWidth.AutoSize := False ;
    lblWidth.Caption := _rsrc( GIS_RS_ATTRIBUTES_DLG_LBLWIDTH ) ;
    lblWidth.FocusControl := edtWidth ;

    lblDecimal := TLabel.Create( Self ) ;
    lblDecimal.Parent := Self ;
    lblDecimal.Anchors := anchors ;
    lblDecimal.Top := 8 ;
    lblDecimal.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblDecimal, 248, 48 ) ;
    lblDecimal.AutoSize := False ;
    lblDecimal.Caption := _rsrc( GIS_RS_ATTRIBUTES_DLG_LBLDECIMAL ) ;
    lblDecimal.FocusControl := edtDecimal ;

    edtName := TEdit.Create( Self ) ;
    edtName.Parent := Self ;
    edtName.Anchors := anchors ;
    edtName.Top := 24 ;
    edtName.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtName, 8, 121 ) ;
    edtName.AutoSize := False ;
    edtName.Ctl3D := True ;
    edtName.ParentCtl3D := False ;
    edtName.TabOrder := 0 ;
    edtName.OnChange := fldChange ;

    cmbType := TComboBox.Create( Self ) ;
    cmbType.Parent := Self ;
    cmbType.Anchors := anchors ;
    cmbType.Top := 24 ;
    cmbType.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbType, 128, 73 ) ;
    cmbType.Style := csDropDownList ;
    cmbType.Ctl3D := False ;
    cmbType.ParentCtl3D := False ;
    cmbType.Items.Text := GIS_RS_ATTRIBUTES_DLG_CMBTYPE_STRING  +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_NUMBER  +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_FLOAT   +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_BOOLEAN +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_DATE    ;
    cmbType.TabOrder := 1 ;
    cmbType.OnChange := fldChange ;

    edtWidth := TEdit.Create( Self ) ;
    edtWidth.Parent := Self ;
    edtWidth.Anchors := anchors ;
    edtWidth.Top := 24 ;
    edtWidth.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtWidth, 200, 49 ) ;
    edtWidth.AutoSize := False ;
    edtWidth.Ctl3D := True ;
    edtWidth.ParentCtl3D := False ;
    edtWidth.TabOrder := 2 ;
    edtWidth.OnChange := fldChange ;

    vvedtWidth := TGIS_ValueValidatorEditHelper.Create( edtWidth ) ;
    vvedtWidth.MinVal := 1.0 ;
    vvedtWidth.MaxVal := 19.0 ;
    vvedtWidth.Precision := 0 ;

    edtDecimal := TEdit.Create( Self ) ;
    edtDecimal.Parent := Self ;
    edtDecimal.Anchors := anchors ;
    edtDecimal.Top := 24 ;
    edtDecimal.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtDecimal, 248, 49 ) ;
    edtDecimal.AutoSize := False ;
    edtDecimal.Ctl3D := True ;
    edtDecimal.ParentCtl3D := False ;
    edtDecimal.TabOrder := 3 ;
    edtDecimal.OnChange := fldChange ;

    vvedtDecimal := TGIS_ValueValidatorEditHelper.Create( edtDecimal ) ;
    vvedtDecimal.MinVal := 1.0 ;
    vvedtDecimal.MaxVal := 18.0 ;
    vvedtDecimal.Precision := 0 ;

    FXField := TGIS_FieldInfo.Create ;

  end ;

  procedure TGIS_ControlAttributesForm.frmDestroy(
    _sender  : TObject
  ) ;
  begin
    FreeObject( FXField ) ;
  end;


  procedure TGIS_ControlAttributesForm.fldChange(
    _sender : TObject
  ) ;
  begin
    prepareWidth ;

    btnOK.Enabled := ( not IsStringEmpty( Trim( edtName.Text ) ) ) and
                     vvedtWidth.Valid and vvedtDecimal.Valid ;
  end;


  procedure TGIS_ControlAttributesForm.btnOKClick(
    _sender : TObject
  ) ;
  var
    name : String  ;
    ext  : Integer ;
    same : Boolean ;
  begin
    name := Trim( edtName.Text ) ;
    ext  := 0 ;
    same :=  CompareText( name, FXField.NewName ) = 0 ;

    FXField.NewName := name ;

    if ( not same ) or ( oMode = 0 ) then
      while True do begin
        if oLayer.FindField( FXField.NewName ) < 0 then break ;
        FXField.NewName := Format( '%s_%d', [ name, ext ] ) ;
        Inc( ext ) ;
      end ;

    FXField.FieldType  := TGIS_FieldType( cmbType.ItemIndex ) ;
    FXField.NewWidth   := RoundS( vvedtWidth.Value   ) ;
    FXField.NewDecimal := RoundS( vvedtDecimal.Value ) ;
    inherited ;
  end;


  procedure TGIS_ControlAttributesForm.readField(
    const _layer : TGIS_LayerVector ;
    const _name  : String
  ) ;
  var
    id  : Integer  ;
    fld : TGIS_FieldInfo ;
  begin
    Assert( Assigned( _layer ) ) ;

    id := _layer.FindField( _name ) ;
    Assert( id >= 0 ) ;

    if id < 0 then exit ;

    fld := _layer.FieldInfo( id ) ;

    if fld.FileFormat then begin
      raise EGIS_Exception.Create( GIS_RS_ERR_FIELDREADONLY, _name, 0 ) ;
    end ;

    XField.Deleted    := fld.Deleted     ;
    XField.Saved      := fld.Saved       ;
    XField.FieldType  := fld.FieldType   ;
    XField.Name       := fld.Name        ;
    XField.Width      := fld.Width       ;
    XField.Decimal    := fld.Decimal     ;
    XField.NewName    := fld.NewName     ;
    XField.NewWidth   := fld.NewWidth    ;
    XField.NewDecimal := fld.NewDecimal  ;
    XField.Binary     := fld.Binary      ;
    XField.ExportName := fld.ExportName  ;
    XField.Hidden     := fld.Hidden      ;
    XField.ReadOnly   := fld.ReadOnly    ;
    XField.FileFormat := fld.FileFormat  ;
  end ;


  procedure TGIS_ControlAttributesForm.prepareDialog ;
  begin
    edtName.Text       := FXField.NewName          ;
    cmbType.ItemIndex  := Ord( FXField.FieldType ) ;
    fldChange( Self )  ;
    vvedtWidth.Value   := FXField.NewWidth         ;
    vvedtDecimal.Value := FXField.NewDecimal       ;

    prepareWidth ;
  end ;


  procedure TGIS_ControlAttributesForm.prepareWidth ;
  begin
    case TGIS_FieldType( cmbType.ItemIndex ) of
      TGIS_FieldType.String  :
        begin
          lblWidth.Enabled    := edtName.Enabled ;
          edtWidth.Enabled    := edtName.Enabled ;
          lblDecimal.Enabled  := False ;
          edtDecimal.Enabled  := False ;
          vvedtWidth.MinVal   := 1     ;
          vvedtWidth.MaxVal   := 254   ;
          vvedtDecimal.MinVal := 0     ;
          vvedtDecimal.MaxVal := 0     ;
          vvedtDecimal.Value  := 0     ;
        end ;
      TGIS_FieldType.Number  :
        begin
          lblWidth.Enabled    := edtName.Enabled ;
          edtWidth.Enabled    := edtName.Enabled ;
          lblDecimal.Enabled  := edtName.Enabled ;
          edtDecimal.Enabled  := edtName.Enabled ;
          vvedtWidth.MinVal   := 1     ;
          vvedtWidth.MaxVal   := 19    ;
          if vvedtWidth.Value > 19 then
            vvedtWidth.Value  := 19    ;
          vvedtDecimal.MinVal := 0     ;
          vvedtDecimal.MaxVal := Max( 0,
                                      Min( 19, Round( vvedtWidth.Value ) ) - 2
                                    ) ;
          if vvedtDecimal.Value > vvedtDecimal.MaxVal then
            vvedtDecimal.Value := vvedtDecimal.MaxVal ;
        end ;
      TGIS_FieldType.Float   :
        begin
          lblWidth.Enabled    := False ;
          edtWidth.Enabled    := False ;
          lblDecimal.Enabled  := False ;
          edtDecimal.Enabled  := False ;
          vvedtWidth.MinVal   := 8     ;
          vvedtWidth.MaxVal   := 8     ;
          vvedtDecimal.MinVal := 0     ;
          vvedtDecimal.MaxVal := 0     ;
          vvedtWidth.Value    := 8     ;
          vvedtDecimal.Value  := 0     ;
        end ;
      TGIS_FieldType.Boolean :
        begin
          lblWidth.Enabled    := False ;
          edtWidth.Enabled    := False ;
          lblDecimal.Enabled  := False ;
          edtDecimal.Enabled  := False ;
          vvedtWidth.MinVal   := 1     ;
          vvedtWidth.MaxVal   := 1     ;
          vvedtDecimal.MinVal := 0     ;
          vvedtDecimal.MaxVal := 0     ;
          vvedtWidth.Value    := 1     ;
          vvedtDecimal.Value  := 0     ;
        end ;
      TGIS_FieldType.Date    :
        begin
          lblWidth.Enabled    := False ;
          edtWidth.Enabled    := False ;
          lblDecimal.Enabled  := False ;
          edtDecimal.Enabled  := False ;
          vvedtWidth.MinVal   := 8     ;
          vvedtWidth.MaxVal   := 8     ;
          vvedtDecimal.MinVal := 0     ;
          vvedtDecimal.MaxVal := 0     ;
          vvedtWidth.Value    := 8     ;
          vvedtDecimal.Value  := 0     ;
        end ;
    end ;
  end ;


  function TGIS_ControlAttributesForm.ShowAdd(
    const _layer : TGIS_LayerVector
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowAdd( _layer, hlp ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowAdd(
    const _layer : TGIS_LayerVector ;
    const _help  : TGIS_HelpEvent
  ) : Integer ;
  begin
     pOnHelp := _help ;
     btnHelp.Visible := Assigned( pOnHelp ) ;

    Result := mrNone ;
    oMode := 0 ;
    if not Assigned( _layer ) then exit ;

    oLayer := _layer ;

    lblName.Enabled      := True  ;
    edtName.Enabled      := True  ;
    lblType.Enabled      := True  ;
    cmbType.Enabled      := True  ;

    FXField.NewName      := GIS_RS_ATTRIBUTES_DLG_NEWFIELD ;
    FXField.FieldType    := TGIS_FieldType.String          ;
    FXField.NewWidth     := 64                             ;

    prepareDialog ;

    Caption := _rsrc(GIS_RS_ATTRIBUTES_DLG_ADD) ;
    Result  := ShowModal ;
  end;


  function TGIS_ControlAttributesForm.ShowModify(
    const _layer : TGIS_LayerVector ;
    const _name  : String
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowModify( _layer, _name, hlp ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowModify(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _help  : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    Result := mrNone ;
    oMode := 1 ;
    if not Assigned( _layer ) then exit ;

    oLayer := _layer ;

    lblName.Enabled      := True  ;
    edtName.Enabled      := True  ;
    lblType.Enabled      := False ;
    cmbType.Enabled      := False ;

    readField( _layer, _name  ) ;
    prepareDialog ;

    Caption := Format( GIS_RS_ATTRIBUTES_DLG_MODIFY, [ _name ] ) ;
    Result  := ShowModal ;
  end;


  function TGIS_ControlAttributesForm.ShowDelete(
    const _layer : TGIS_LayerVector ;
    const _name  : String
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowDelete( _layer, _name, hlp ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowDelete(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _help  : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    Result := mrNone ;
    oMode := 2 ;
    if not Assigned( _layer ) then exit ;

    oLayer := _layer ;

    lblName.Enabled      := False ;
    edtName.Enabled      := False ;
    lblType.Enabled      := False ;
    cmbType.Enabled      := False ;

    readField( _layer, _name  ) ;
    prepareDialog ;

    Caption := Format( GIS_RS_ATTRIBUTES_DLG_DELETE, [_name ] ) ;
    Result  := ShowModal ;
  end;

//==================================== END =====================================
end.

