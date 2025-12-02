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
  Legend component. Vector dialog - library symbol selector.
}


{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlSymbologyLibrary ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlSymbologyLibrary"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  {$IFDEF CLR}
    TatukGIS.ndk,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,
    System.Generics.Collections,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoLibrarySVG,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvlForms;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.jdk.*,
    tatukgis.rtl ;
  {$ENDIF}


type

  /// <summary>
  ///   Visual form for managing our symbology library.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlSymbologyLibrary = class( TGIS_PvlModalForm )
    private
      pnlMain   : TGIS_PvlPanel     ;
      lstItems  : TGIS_PvlSVGList   ;
      cmbFilter : TGIS_PvlComboEdit ;
    private
      procedure FormCreate        ( Sender: TObject ) ;
      procedure cmbFilterChange   ( Sender: TObject ) ;
      procedure lstItemsClick     ( Sender: TObject ) ;

    private // properties internal value
      /// <summary>
      ///   Selected symbol or nil.
      /// </summary>
      FSymbol : TGIS_SymbolAbstract ;

      /// <summary>
      ///   Category filter.
      /// </summary>
      FOnlyCategory : String            ;

    private
      /// <summary>
      ///   List of symbols names.
      /// </summary>
      lstNames: TStringList;

      /// <summary>
      ///   TGIS_SymbolLibrarySVG handle.
      /// </summary>
      oLib : TGIS_SymbolLibrarySVG ;

    public
      /// <inheritdoc/>
      procedure DoInitForm      ; override;

      /// <inheritdoc/>
      procedure DoInitControls  ; override;

      /// <inheritdoc/>
      procedure doPlaceButtons  ; override;

      /// <inheritdoc/>
      procedure doShowForm      ; override;

      /// <inheritdoc/>
      procedure DoRedraw        ; override;

      /// <inheritdoc/>
      procedure DoAfterCreate   ; override;

    public

      /// <summary>
      ///   Execute dialog.
      /// </summary>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function  Execute : TGIS_PvlModalResult; overload;

      /// <summary>
      ///   Execute dialog.
      /// </summary>
      /// <param name="_proc">
      ///   Action to be performed after closing modal form.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function  Execute( const _proc     : TGIS_Proc
                       ) : TGIS_PvlModalResult ; overload ;

      /// <summary>
      ///   Selected Symbol or nil.
      /// </summary>
      property Symbol : TGIS_SymbolAbstract
               read  FSymbol
               write FSymbol ;

      /// <summary>
      ///   If not empty then Libary form will present only symbols matching
      ///   categeory.
      /// </summary>
      property OnlyCategory : String
               read  FOnlyCategory
               write FOnlyCategory ;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.StrUtils,
    System.SysUtils,
    System.Math,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses;
{$ENDIF}

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlSymbologyLibrary.FormCreate(Sender: TObject);
  begin
    lstNames := TStringList.Create ;

    oLib := TGIS_SymbolLibrarySVG.Handle ;
  end;

  procedure TGIS_ControlSymbologyLibrary.lstItemsClick(Sender: TObject);
  begin
    if IsStringEmpty( lstItems.Item[ lstItems.ItemIndex ] ) then exit ;
    if lstItems.ItemIndex < 0 then exit ;

    FSymbol := SymbolList.Prepare( lstNames[ lstItems.ItemIndex ] ) ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlSymbologyLibrary.cmbFilterChange(Sender: TObject);
  var
    lst : TStringList ;
    i   : Integer ;
    symbol_name : String ;
    caption     : String ;
    filter      : String ;
    loop_filter : String ;
    search_str  : Boolean ;
  begin
    lstItems.ItemsClear ;
    lstNames.Clear ;

    if length( cmbFilter.Text ) = 0 then
      cmbFilter.ItemIndex := 1 ;

    filter := cmbFilter.Text ;
    if length( filter ) < 2 then
      exit ;

    if oLib.Categories.IndexOf( filter ) > -1 then begin
      loop_filter := filter ; // filter is category name
      search_str := False ;
    end
    else begin
      loop_filter := '*' ;   // filter is search string
      search_str := True ;
    end ;

    lstItems.BeginUpdate ;

    lst := TStringList.Create ;

    try
      lst.Assign( ( oLib.Loop( loop_filter ) ) ) ;
      for i := 0 to lst.Count - 1  do begin
        symbol_name := lst.Strings[i] ;
        caption := oLib.GetCaption( symbol_name ) ;

        if search_str and ( not LowerCase( caption ).Contains( LowerCase( filter ) ) ) then
          continue ;
        lstNames.Add( symbol_name ) ;
        lstItems.ItemsAdd( symbol_name ) ;
      end ;
    finally
      lstItems.EndUpdate ;
      FreeObject( lst ) ;
    end ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlSymbologyLibrary.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGLIB ) ;
    Self.ClientHeight := 289 ;
    Self.ClientWidth := 372 ;
  end ;

  procedure TGIS_ControlSymbologyLibrary.DoInitControls ;
  begin
    // Need to embed it in the $IFDEF since we reposition ok button and on
    // FMX OK buttons are handled differently in ModalForm.
    {$IFNDEF GIS_MOBILE_DIALOGS}
      BtnOK.Place( 0,
                   20,
                   nil,
                   -Context.HMargin,
                   nil,
                   ClientHeight - BtnCancel.Height - Context.VMargin
                 ) ;
    {$ENDIF}

    pnlMain := TGIS_PvlPanel.Create( Context ) ;
    pnlMain.Place( ClientWidth - 2 * Context.HMargin,
                   ClientHeight - 2 * Context.VMargin - 4 * Context.VSpace
                   - BtnOK.Height,
                   nil,
                   Context.HMargin,
                   nil,
                   Context.VMargin
                 ) ;
    pnlMain.Scrollable := True ;
    pnlMain.TabOrder := 1 ;

    lstItems := TGIS_PvlSVGList.Create( Context ) ;
    {$IFDEF DCC}
      lstItems.OnClick := lstItemsClick ;
    {$ELSE}
      lstItems.OnClick := @lstItemsClick ;
    {$ENDIF}

    pnlMain.AddComponent( lstItems ) ;

    BtnCancel.Visible := False ;
    BtnHelp.Visible := False ;
    BtnOK.TabOrder := 2 ;

    cmbFilter := TGIS_PvlComboEdit.Create( Context ) ;
    // Since we repositioned ok button we can relly on it in no mobile version
    // while we calculate width but we cant on mobile thats why we need $IFDEF
    // here
    {$IFDEF GIS_MOBILE_DIALOGS}
      cmbFilter.Place( ClientWidth - 2 * Context.HMargin,
                       BtnOK.Height,
                       nil,
                       Context.HMargin,
                       nil,
                       ClientHeight - BtnCancel.Height - Context.VMargin
                     ) ;
    {$ELSE}
      cmbFilter.Place( ClientWidth - BtnOK.Width - 2 * Context.HMargin
                       - Context.HSpace,
                       BtnOK.Height,
                       BtnOK,
                       -Context.HSpace,
                       BtnOK,
                       -1
                     ) ;
    {$ENDIF}
    cmbFilter.TabOrder := 0 ;
    {$IFDEF DCC}
      cmbFilter.OnChange := cmbFilterChange ;
    {$ELSE}
      cmbFilter.OnChange := @cmbFilterChange ;
    {$ENDIF}

    FormCreate( Self ) ;
  end ;

  procedure TGIS_ControlSymbologyLibrary.DoPlaceButtons;
  begin
    inherited ;
    // Need to embed it in the $IFDEF since we reposition ok button and on
    // FMX OK buttons are handled differently in ModalForm.
    {$IFNDEF GIS_MOBILE_DIALOGS}
      BtnOK.Place( 0,
                   20,
                   nil,
                   -Context.HMargin,
                   nil,
                   ClientHeight - BtnCancel.Height - Context.VMargin
                 ) ;

      if assigned( cmbFilter ) then begin
        cmbFilter.Place( ClientWidth - BtnOK.Width - 2 * Context.HMargin
                         - Context.HSpace,
                         BtnOK.Height,
                         BtnOK,
                         -Context.HSpace,
                         BtnOK,
                         -1
                       ) ;
    end;
    {$ENDIF}
  end;

  procedure TGIS_ControlSymbologyLibrary.DoShowForm ;
  var
    idx  : Integer ;
    isel : Integer ;
  begin
    if not IsStringEmpty( FOnlyCategory ) then begin
      cmbFilter.Text := FOnlyCategory ;
    end;
    oLib.OpenForBrowsing( FOnlyCategory ) ;

    for idx := 0 to oLib.Categories.Count - 1 do begin
      cmbFilter.ItemsAdd( oLib.Categories.Strings[ idx ] ) ;
    end;

    isel := Max( 0, cmbFilter.IndexOf( FOnlyCategory ) ) ;
    cmbFilter.ItemIndex := isel ;

    // Because of FMX
    cmbFilterChange( Self ) ;
  end;

  procedure TGIS_ControlSymbologyLibrary.DoRedraw ;
  begin
    lstItems.DoRedraw ;
    //cmbFilterChange( Self ) ;
  end;

  procedure TGIS_ControlSymbologyLibrary.DoAfterCreate ;
  begin
    FOnlyCategory := '' ;
  end;

  function TGIS_ControlSymbologyLibrary.Execute
   : TGIS_PvlModalResult;
  begin
    Result := Execute( nil ) ;
  end ;

  function TGIS_ControlSymbologyLibrary.Execute(
    const _proc     : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  begin
    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

  procedure TGIS_ControlSymbologyLibrary.doDestroy ;
  begin
    FreeObject( lstNames ) ;
    inherited ;
  end;

//==================================== END =====================================
end.
