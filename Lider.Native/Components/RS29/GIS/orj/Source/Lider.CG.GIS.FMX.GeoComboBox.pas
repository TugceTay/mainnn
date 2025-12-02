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
  Custom combobox for better picking values on mobile devices.
}
unit FMX.GisComboBox;
{$HPPEMIT '#pragma link "FMX.GisComboBoxr"}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.UITypes,
  System.Math,
  FMX.Types,
  FMX.Pickers,
  FMX.ListBox,
  FMX.Forms,
  FMX.GisModalForm;


type

  /// <summary>
  ///   Emulation class which displays better combo box pickers on a mobile
  ///   devices as a kind of dialog boxes rather then a drop-down list.
  /// </summary>
  TGIS_ComboBox = class( TComboBox )
    {$IFDEF GIS_MOBILE_DIALOGS}
      private
        oDlg   : TGIS_ModalForm ;
        FDroppedDown : Boolean ;
      public
        /// <summary>
        ///   Standard constructor oberridden to force custom drawing.
        /// </summary>
        /// <param name="_owner">
        ///   control owner
        /// </param>
        {#ownership:_owner:ownif_empty}
        constructor Create      ( _owner: TComponent); override;

      private
        /// <summary>
        ///   Handle OnChange on event.
        /// </summary>
        /// <param name="_sender">
        ///   sender object
        /// </param>
        procedure   doOnChange  ( _sender : TObject ) ;

      public
        /// <summary>
        ///   Standard TComboBox method overridden to create custom picer.
        /// </summary>
        procedure   DropDown    ; override;

        /// <summary>
        ///   Standard TComboBox function overrideen to create custom list boc.
        /// </summary>
        /// <returns>
        ///   TLitBox object.
        /// </returns>
        function   CreateListBox : TComboListBox ; override;

        /// <summary>
        ///   Standard TComboBox property overrideen.
        /// </summary>
        property DroppedDown: Boolean read FDroppedDown;

    {$ENDIF}
    protected
      {$IFDEF LEVEL_XE8_FMX}
        /// <summary>
        ///   Standard TComboBox method overridden to make DropDown faster.
        /// </summary>
        procedure RecalculatePopupSize; override;
      {$ENDIF}
  end;


//##############################################################################
implementation

{$IFDEF GIS_MOBILE_DIALOGS}
  type
    T_ListBox = class( TListBox )
      public
        procedure ResetSelectionController ;
    end;

    // do not change this name!!!
    T_popupModalForm = class( TGIS_ModalForm )
    end ;

  procedure T_ListBox.ResetSelectionController ;
  begin
    {$IFDEF LEVEL_RX10_FMX}
      SelectionController.MouseSelectActive := False;
    {$ENDIF}
  end;

  constructor TGIS_ComboBox.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner );

    DropDownKind := TDropDownKind.Native ;

    oDlg := nil ;
  end;

  function TGIS_ComboBox.CreateListBox: TComboListBox;
  begin
    Result := TComboListBox( T_ListBox.Create(Self) );
  end;

  procedure TGIS_ComboBox.doOnChange(
    _sender : TObject
  ) ;
  begin
    if Assigned( oDlg ) and FDroppedDown then
      oDlg.ModalResult := mrOK ;
  end ;

  procedure TGIS_ComboBox.DropDown;
  var
    prnt : TFmxObject ;
  begin
    oDlg := T_popupModalForm.CreateNew( self, 0 );
    oDlg.ClientWidth := Round( Max( 200, Width ) ) ;
    oDlg.ClientHeight := Round( ItemHeight * Min( 10, Count ) + 5 ) ;
    prnt := ListBox.Parent ;
    ListBox.Parent := oDlg ;
    ListBox.Align := TAlignLayout.Client ;
    ListBox.RecalcSize ;
    T_ListBox( ListBox ).OnChange := doOnChange ;
    T_ListBox( ListBox ).ResetSelectionController;
    ListBox.ClearSelection ;

    FDroppedDown := True ;

    Assert( oDlg.ClassName = MODALFORM_POPUP ) ;

    oDlg.ShowModalEx(
      procedure ( _modal_result : TModalResult )
      begin
        OnClosePopup( self ) ;
        ListBox.Parent := prnt ;
        Repaint ;
        oDlg := nil ;
        FDroppedDown := False ;
      end
    );


  end;

{$ENDIF}

{$IFDEF LEVEL_XE8_FMX}
  procedure TGIS_ComboBox.RecalculatePopupSize;
  var
    PopupContentHeight: Single;
  begin
    if ( ItemHeight < 1 ) or ( ItemWidth > 1 ) then
      inherited
    else begin
      Popup.ApplyStyleLookup;
      if Pressed or DoubleClick then
        Popup.PreferedDisplayIndex := Screen.DisplayFromPoint(Screen.MousePos).Index
      else
        Popup.PreferedDisplayIndex := -1;

      Popup.Width := Width ;

      Popup.Height := Popup.Padding.Top +
                      Min(Count, DropDownCount) * ItemHeight +
                      ListBox.BorderHeight + Popup.Padding.Bottom;
    end;
  end;
{$ENDIF}

//==================================== END =====================================
end.

