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
  Visual control for displaying shape attributes. Rename/Delete/Add dialog.<p>
}

unit FMX.GisControlAttributesForm ;
{$HPPEMIT '#pragma link "VCL.GisControlAttributesForm"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.UITypes,
  FMX.Edit,
  FMX.ListBox,
  FMX.StdCtrls,
  FMX.Types,

  GisClasses,
  GisResource,
  GisTypes,
  GisLayerVector,
  FMX.GisModalForm,
  FMX.GisValueValidatorHelper ;

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
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowAdd    ( const _layer : TGIS_LayerVector ;
                            const _help  : TGIS_HelpEvent ;
                            const _proc  : TProc<TModalResult>
                          ) : Integer ; overload;

      /// <summary>
      ///   Show dialog for creating a new field. New field definition will be
      ///   available in XField.
      /// </summary>
      /// <param name="_layer">
      ///   layer object on which field will be created
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowAdd    ( const _layer : TGIS_LayerVector ;
                            const _proc  : TProc<TModalResult>
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
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDREADONLY
      /// </exception>
      function ShowModify ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _help  : TGIS_HelpEvent   ;
                            const _proc  : TProc<TModalResult>
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
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDREADONLY
      /// </exception>
      function ShowModify ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _proc  : TProc<TModalResult>
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
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowDelete ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _help  : TGIS_HelpEvent   ;
                            const _proc  : TProc<TModalResult>
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
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      /// <returns>
      ///   modal result
      /// </returns>
      function ShowDelete ( const _layer : TGIS_LayerVector ;
                            const _name  : String           ;
                            const _proc  : TProc<TModalResult>
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
  GisRtl,
  FMX.GisControlHelper ;

//==============================================================================
// TGIS_ControlAttributesForm
//==============================================================================

  procedure TGIS_ControlAttributesForm.initForm ;
  begin
    Self.ClientHeight := 104 ;
    Self.ClientWidth := 305 ;
    Self.Name := 'TGIS_ControlAttributesForm' ;
    Self.OnDestroy := frmDestroy ;
  end ;

  procedure TGIS_ControlAttributesForm.initControls ;
  begin

    lblName := TLabel.Create( oMainForm ) ;
    lblName.Parent := oMainForm ;
    lblName.Position.Y := 8 ;
    PlaceControl( BiDiMode, nil, lblName, 8, 38 ) ;
    lblName.Size.PlatformDefault := False ;
    lblName.Text := _rsrcna( GIS_RS_ATTRIBUTES_DLG_LBLNAME ) ;
    lblName.FixSize ;

    lblType := TLabel.Create( oMainForm ) ;
    lblType.Parent := oMainForm ;
    lblType.Position.Y := 8 ;
    PlaceControl( BiDiMode, nil, lblType, 128, 34 ) ;
    lblType.Size.PlatformDefault := False ;
    lblType.Text := _rsrcna( GIS_RS_ATTRIBUTES_DLG_LBLTYPE ) ;
    lblType.FixSize ;

    lblWidth := TLabel.Create( oMainForm ) ;
    lblWidth.Parent := oMainForm ;
    lblWidth.Position.Y := 8 ;
    lblWidth.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblWidth, 200, 38 ) ;
    lblWidth.Size.PlatformDefault := False ;
    lblWidth.Text := _rsrcna( GIS_RS_ATTRIBUTES_DLG_LBLWIDTH ) ;
    lblWidth.FixSize ;

    lblDecimal := TLabel.Create( oMainForm ) ;
    lblDecimal.Parent := oMainForm ;
    lblDecimal.Position.Y := 8 ;
    lblDecimal.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblDecimal, 248, 48 ) ;
    lblDecimal.Size.PlatformDefault := False ;
    lblDecimal.Text := _rsrcna( GIS_RS_ATTRIBUTES_DLG_LBLDECIMAL ) ;
    lblDecimal.FixSize ;

    edtName := TEdit.Create( oMainForm ) ;
    edtName.Parent := oMainForm ;
    edtName.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                          TInteractiveGesture.DoubleTap] ;
    edtName.Position.Y := 24 ;
    PlaceControl( BiDiMode, nil, edtName, 8, 121 ) ;
    edtName.Size.PlatformDefault := False ;
    edtName.FixSize ;
    edtName.TabOrder := 0 ;
    edtName.OnChange := fldChange ;

    cmbType := TComboBox.Create( oMainForm ) ;
    cmbType.Parent := oMainForm ;
    cmbType.Position.Y := 24 ;
    PlaceControl( BiDiMode, nil, cmbType, 128, 73 ) ;
    cmbType.Items.Text := GIS_RS_ATTRIBUTES_DLG_CMBTYPE_STRING  +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_NUMBER  +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_FLOAT   +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_BOOLEAN +#13#10+
                          GIS_RS_ATTRIBUTES_DLG_CMBTYPE_DATE    ;
    cmbType.TabOrder := 1 ;
    cmbType.OnChange := fldChange ;

    edtWidth := TEdit.Create( oMainForm ) ;
    edtWidth.Parent := oMainForm ;
    edtWidth.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                           TInteractiveGesture.DoubleTap] ;
    edtWidth.Position.Y := 24 ;
    PlaceControl( BiDiMode, nil, edtWidth, 200, 49 ) ;
    edtWidth.Size.PlatformDefault := False ;
    edtWidth.FixSize ;
    edtWidth.TabOrder := 2 ;
    edtWidth.OnChange := fldChange ;

    vvedtWidth := TGIS_ValueValidatorEditHelper.Create( edtWidth ) ;
    vvedtWidth.MinVal := 1.0 ;
    vvedtWidth.MaxVal := 19.0 ;
    vvedtWidth.Precision := 0 ;

    edtDecimal := TEdit.Create( oMainForm ) ;
    edtDecimal.Parent := oMainForm ;
    edtDecimal.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                             TInteractiveGesture.DoubleTap] ;
    edtDecimal.Position.Y := 24 ;
    PlaceControl( BiDiMode, nil, edtDecimal, 248, 49 ) ;
    edtDecimal.Size.PlatformDefault := False ;
    edtDecimal.FixSize ;
    edtDecimal.TabOrder := 3 ;
    edtDecimal.OnChange := fldChange ;

    vvedtDecimal := TGIS_ValueValidatorEditHelper.Create( edtDecimal ) ;
    vvedtDecimal.MinVal := 1.0 ;
    vvedtDecimal.MaxVal := 18.0 ;
    vvedtDecimal.Precision := 0 ;

    FXField := TGIS_FieldInfo.Create ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 75 ) ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;

      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
    {$ENDIF}
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
    fldChange( oMainForm )  ;
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
    const _layer : TGIS_LayerVector ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowAdd( _layer, hlp, _proc ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowAdd(
    const _layer : TGIS_LayerVector ;
    const _help  : TGIS_HelpEvent ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

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

    Caption := _rsrcna(GIS_RS_ATTRIBUTES_DLG_ADD) ;
    ShowModalEx( _proc ) ;
  end;


  function TGIS_ControlAttributesForm.ShowModify(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowModify( _layer, _name, hlp, _proc ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowModify(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _help  : TGIS_HelpEvent ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

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
    ShowModalEx( _proc ) ;
  end;


  function TGIS_ControlAttributesForm.ShowDelete(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := ShowDelete( _layer, _name, hlp, _proc ) ;
  end ;


  function TGIS_ControlAttributesForm.ShowDelete(
    const _layer : TGIS_LayerVector ;
    const _name  : String ;
    const _help  : TGIS_HelpEvent ;
    const _proc  : TProc<TModalResult>
  ) : Integer ;
  begin
    pOnHelp := _help ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

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
    ShowModalEx( _proc ) ;
  end;

//==================================== END =====================================
end.

