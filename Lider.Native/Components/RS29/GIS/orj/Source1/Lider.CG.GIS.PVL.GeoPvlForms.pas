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
  PVL forms which helps us keep one code across the platforms.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoPvlForms;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoPvlForms"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$M+}

interface

uses
  {$IFDEF DCC}
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.PVL.GeoPvl;
  {$ENDIF}
  {$IFDEF CLR}
    tatukgis.NDK.PVL,
    tatukgis.RTL,
    TatukGIS.NDK;
  {$ENDIF}
  {$IFDEF JAVA}
    tatukgis.jdk.pvl,
    tatukgis.rtl,
    tatukgis.jdk;
  {$ENDIF}

type

  /// <summary>
  ///   Style of border withing PVL Forms.
  /// </summary>
  TGIS_PvlBorderStyle = (

    /// <summary>
    ///   Fixed border.
    /// </summary>
    Fixed,

    /// <summary>
    ///   Sizeable border.
    /// </summary>
    Sizeable,

    /// <summary>
    ///   Tollwindow border.
    /// </summary>
    ToolWindow,

    /// <summary>
    ///   Sizeable toolwindow border.
    /// </summary>
    ToolWindowSizeable
  ) ;

  /// <summary>
  ///   Style of icons withing PVL Forms.
  /// </summary>
 TGIS_PvlBorderIcon = (

    /// <summary>
    ///   System menu icon.
    /// </summary>
    SystemMenu,

    /// <summary>
    ///   Minimize icon.
    /// </summary>
    Minimize,

    /// <summary>
    ///   Maximize icon.
    /// </summary>
    Maximize
  ) ;

  /// <summary>
  ///   Set of TGIS_PvlBorderIcons
  /// </summary>
  TGIS_PvlBorderIcons = set of TGIS_PvlBorderIcon ;

  /// <summary>
  ///   Basic interface for PVL forms.
  /// </summary>
  IGIS_PvlForm = interface( IGIS_PvlBase )
    {$IFDEF DCC}
      ['{B1C08D44-0E6D-4862-9EAD-C15B975830EA}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Name property.
    /// </summary>
    /// <returns>
    ///   Name of the form
    /// </returns>
    function  fget_Name         : String;

    /// <summary>
    ///   Setter for Name property.
    /// </summary>
    /// <param name="_value">
    ///   New Name value
    /// </param>
    procedure fset_Name         ( const _value    : String
                                );

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Caption of the form
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New Caption value
    /// </param>
    procedure fset_Caption      ( const _value    : String
                                );


    /// <summary>
    ///   Setter for ClientHeight property.
    /// </summary>
    /// <returns>
    ///   Height of the client area
    /// </returns>
    function  fget_ClientHeight : Integer;

    /// <summary>
    ///   Getter for ClientHeight property.
    /// </summary>
    /// <param name="_value">
    ///   New ClientHeight value
    /// </param>
    procedure fset_ClientHeight ( const _value    : Integer
                                );

    /// <summary>
    ///   Setter for ClientWidth property.
    /// </summary>
    /// <returns>
    ///   Width of the client area
    /// </returns>
    function  fget_ClientWidth  : Integer;

    /// <summary>
    ///   Setter for ClientWidth property.
    /// </summary>
    /// <param name="_value">
    ///   New ClientWidth value
    /// </param>
    procedure fset_ClientWidth  ( const _value    : Integer
                                );

    /// <summary>
    ///   Getter for a RightToLeft property.
    /// </summary>
    /// <returns>
    ///   True if layout is RightToLeft, False if the opposite.
    /// </returns>
    function  fget_RightToLeft  : Boolean;

    /// <summary>
    ///   Getter for a PPI property.
    /// </summary>
    /// <returns>
    ///   Pixels Per Inch
    /// </returns>
    function  fget_PPI          : Integer;

    /// <summary>
    ///   Getter for a PPIFix property.
    /// </summary>
    /// <returns>
    ///   PPIFix value.
    /// </returns>
    function  fget_PPIFix       : Single;

    /// <summary>
    ///   Getter for a CanvasScale property.
    /// </summary>
    /// <returns>
    ///   Current canvas scaling
    /// </returns>
    function  fget_CanvasScale  : Single;

    /// <summary>
    ///   Getter for Context property.
    /// </summary>
    /// <returns>
    ///   Control's Context
    /// </returns>
    function  fget_Context      : TGIS_PvlContext;

    /// <summary>
    ///   Getter for BorderStyle property.
    /// </summary>
    /// <returns>
    ///   Control's BorderStyle
    /// </returns>
    function  fget_BorderStyle  : TGIS_PvlBorderStyle;

    /// <summary>
    ///   Getter for BorderIcons property.
    /// </summary>
    /// <returns>
    ///   Control's BorderIcons
    /// </returns>
    /// <remarks>
    ///   Not applicable to JAVA platform
    /// </remarks>
    function  fget_BorderIcons  : TGIS_PvlBorderIcons;

    /// <summary>
    ///   Getter for ModalResult property.
    /// </summary>
    /// <returns>
    ///   Control's ModalResult
    /// </returns>
    function  fget_ModalResult  : TGIS_PvlModalResult ;

    /// <summary>
    ///   Setter for ModalResult property.
    /// </summary>
    /// <param name="_value">
    ///   New ModalResult value
    /// </param>
    procedure fset_ModalResult  ( const _value    : TGIS_PvlModalResult
                                ) ;

    /// <summary>
    ///   Override this method to provide custom GUI updates.
    /// </summary>
    procedure doUpdateGUI;

    /// <summary>
    ///   Create standard form buttons.
    /// </summary>
    procedure doInitButtons;

    /// <summary>
    ///   Place standard form buttons.
    /// </summary>
    procedure doPlaceButtons;

    /// <summary>
    ///   Override this method to react if monitor PPI was changed.
    /// </summary>
    procedure doAfterPPIChanged;

    /// <summary>
    ///   Override this method to provide any special functionality upon
    ///   form show.
    /// </summary>
    procedure doShowForm;

    /// <summary>
    ///   Executed whenever GUI or DPI style is updated.
    /// </summary>
    procedure DoRedraw ;

    /// <summary>
    ///   Locks window from rendering.
    /// </summary>
    procedure LockWindow;

    /// <summary>
    ///   Unlocks window from rendering.
    /// </summary>
    procedure UnlockWindow;

    /// <summary>
    ///   Shows in the app.
    /// </summary>
    procedure Show              ;
                                overload;

    /// <summary>
    ///   Dispose form
    /// </summary>
    /// <remarks>
    ///   SPECIFIC FOR JAVA PLATFORM.
    ///   Doing nothing on others.
    /// </remarks>
    procedure FreeForm          ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENSCR}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   Shows modal in the app.
    /// </summary>
    /// <param name="_proc">
    ///   action to be taken upon closing modal.
    /// </param>
    /// <param name="_free">
    ///   True, if dialog should be free upon close
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  ShowModal         ( const _proc     : TGIS_Proc ;
                                  const _free     : Boolean
                                ) : TGIS_PvlModalResult ;
                                overload;

    /// <summary>
    ///   Shows modal in the app.
    /// </summary>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  ShowModal         : TGIS_PvlModalResult ;
                                overload;

    /// <summary>
    ///   Close form.
    /// </summary>
    procedure Close             ;
  end;

  /// <summary>
  ///   Interface for PVL modal forms.
  /// </summary>
  IGIS_PvlModalForm = interface( IGIS_PvlForm )
    {$IFDEF DCC}
      ['{4A4066AA-308F-43D2-9A3E-01B24AF903BE}']
    {$ENDIF}

    /// <summary>
    ///   Getter for OK button.
    /// </summary>
    /// <returns>
    ///   Button instance.
    /// </returns>
    function  fget_BtnOK        : TGIS_PvlModalButton;

    /// <summary>
    ///   Getter for Cancel button.
    /// </summary>
    /// <returns>
    ///   Button instance.
    /// </returns>
    function  fget_BtnCancel    : TGIS_PvlModalButton;

    /// <summary>
    ///   Getter for Help button.
    /// </summary>
    /// <returns>
    ///   Button instance.
    /// </returns>
    function  fget_BtnHelp      : TGIS_PvlModalButton;

    /// <summary>
    ///   Getter for TGIS_HelpEvent property.
    /// </summary>
    /// <returns>
    ///   Helper event
    /// </returns>
    function  fget_OnHelp       : TGIS_HelpEvent;

    /// <summary>
    ///   Setter for TGIS_HelpEvent property.
    /// </summary>
    /// <param name="_value">
    ///   New helper value
    /// </param>
    procedure fset_OnHelp       ( const _value    : TGIS_HelpEvent
                                );
  end;

  /// <summary>
  ///   Interface for modal wizards.
  /// </summary>
  IGIS_PvlModalWizard = interface( IGIS_PvlModalForm )
    {$IFDEF DCC}
      ['{8E463BBD-6CD9-45AC-A044-0CB4A23888D8}']
    {$ENDIF}

      /// <summary>
      ///   Getter for the btnNext property.
      /// </summary>
      /// <returns>
      ///   Instance of the next button
      /// </returns>
      function  fget_btnNext      : TGIS_PvlModalButton;

      /// <summary>
      ///   Getter for the btnPrevious property.
      /// </summary>
      /// <returns>
      ///   Instance of the Previous button
      /// </returns>
      function  fget_btnPrevious  : TGIS_PvlModalButton;

      /// <summary>
      ///   Getter for the Pages property.
      /// </summary>
      /// <returns>
      ///   Instance of the Pages
      /// </returns>
      function  fget_Pages        : TGIS_PvlPages;

  end;

  /// <summary>
  ///   Base PVL form from which we inherit in our forms.
  /// </summary>
  TGIS_PvlBaseForm = {$IFDEF OXYGENE}abstract{$ENDIF} class( TGIS_PvlBase, IGIS_PvlForm )
    private
      oContext      : TGIS_PvlContext ;
      oBorderStyle  : TGIS_PvlBorderStyle;
      oBorderIcons  : TGIS_PvlBorderIcons;
      oPlatformFrm  : IGIS_PvlForm ;
      FOnClose      : TGIS_PvlEvent ;
      FOnShow       : TGIS_PvlEvent ;
    protected
      /// <summary>
      ///   Additional create procedure.
      /// </summary>
      /// <param name="_parent">
      ///   Parent of the form.
      /// </param>
      /// <param name="_style">
      ///   Style of the border.
      /// </param>
      /// <param name="_icons">
      ///   Icons of the form.
      /// </param>
      procedure doCreate          ( const _parent : TObject;
                                          _style  : TGIS_PvlBorderStyle;
                                          _icons  : TGIS_PvlBorderIcons
                                  ) ; virtual;
    public
      /// <summary>
      ///   Constructor for the form.
      /// </summary>
      /// <param name="_parent">
      ///   Parent of the form.
      /// </param>
      /// <param name="_style">
      ///   Style of the border.
      /// </param>
      /// <param name="_icons">
      ///   Icons of the form.
      /// </param>
      constructor Create          ( const _parent : TObject;
                                          _style  : TGIS_PvlBorderStyle;
                                          _icons  : TGIS_PvlBorderIcons
                                  ) ; overload ;

      /// <summary>
      ///   Constructor for the form with default
      ///   border style and border icons.
      /// </summary>
      /// <param name="_parent">
      ///   Parent of the form.
      /// </param>
      constructor Create          ( const _parent : TObject
                                  ) ; overload; virtual;

    protected
      /// <summary>
      ///   Destructor for the form.
      /// </summary>
      procedure doDestroy         ; override;
    private
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Name         : String;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Name         ( const _value    : String
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_Caption      ( const _value    : String
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ClientHeight : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ClientHeight ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ClientWidth  : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ClientWidth  ( const _value    : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_RightToLeft  : Boolean;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_PPI          : Integer;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_PPIFix       : Single;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_CanvasScale  : Single;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_Context      : TGIS_PvlContext;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BorderStyle  : TGIS_PvlBorderStyle;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BorderIcons  : TGIS_PvlBorderIcons;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_ModalResult  : TGIS_PvlModalResult ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_ModalResult  ( const _value    : TGIS_PvlModalResult
                                  ) ;
    protected

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doUpdateGUI       ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doInitButtons     ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doPlaceButtons    ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doAfterPPIChanged ;
                                  virtual;

    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doShowForm        ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure LockWindow        ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure UnlockWindow      ;
                                  virtual;

    protected
      /// <summary>
      ///   Override this method to add any initialization to be done
      ///   before form creation.
      /// </summary>
      procedure DoBeforeCreate    ;
                                  virtual;
      /// <summary>
      ///   Override this method to add any initialization to be done
      ///   after form creation.
      /// </summary>
      procedure DoAfterCreate     ;
                                  virtual;

      /// <summary>
      ///   Override this method to provide custom caption and sizes.
      /// </summary>
      procedure DoInitForm        ;
                                  virtual;

      /// <summary>
      ///   Override this method to add controls to this form.
      /// </summary>
      procedure DoInitControls    ;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure DoRedraw          ;
                                  virtual;

    public
      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Show              ;
                                  overload;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure FreeForm          ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  ShowModal         ( const  _proc    : TGIS_Proc ;
                                    const _free     : Boolean
                                  ) : TGIS_PvlModalResult ;
                                  overload;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  ShowModal         : TGIS_PvlModalResult ;
                                  overload;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure Close             ;
    public
      /// <summary>
      ///   Nameof the form.
      /// </summary>
      property Name               : String
                                    read  fget_Name
                                    write fset_Name ;

      /// <summary>
      ///   Title caption of the form.
      /// </summary>
      property Caption            : String
                                    read  fget_Caption
                                    write fset_Caption ;

      /// <summary>
      ///   Form client height.
      /// </summary>
      property ClientHeight       : Integer
                                    read  fget_ClientHeight
                                    write fset_ClientHeight ;

      /// <summary>
      ///   Form client width.
      /// </summary>
      property ClientWidth        : Integer
                                    read  fget_ClientWidth
                                    write fset_ClientWidth ;


      /// <summary>
      ///   Form PPI.
      /// </summary>
      property PPI                : Integer
                                    read  fget_PPI ;

      /// <summary>
      ///   Form PPI fix.
      /// </summary>
      property PPIFix             : Single
                                    read  fget_PPIFix ;

      /// <summary>
      ///   Form Canvas scale,
      /// </summary>
      property CanvasScale        : Single
                                    read  fget_CanvasScale ;

      /// <summary>
      ///   Context of the form.
      /// </summary>
      property Context            : TGIS_PvlContext
                                    read  fget_Context ;

      /// <summary>
      ///   BorderStyle of the form.
      /// </summary>
      /// <remarks>
      ///   Toolbox the same as normal sizes in JAVA platform.
      /// </remarks>
      property BorderStyle        : TGIS_PvlBorderStyle
                                    read  fget_BorderStyle ;

      /// <summary>
      ///   BorderIcons of the form.
      /// </summary>
      /// <remarks>
      ///   Takes no effect on JAVA platform.
      /// </remarks>
      property BorderIcons        : TGIS_PvlBorderIcons
                                    read  fget_BorderIcons ;

      /// <summary>
      ///   Result of the modal form show operation
      /// </summary>
      property ModalResult        : TGIS_PvlModalResult
                                    read  fget_ModalResult
                                    write fset_ModalResult ;

    published
      /// <event/>
      /// <summary>
      ///   Event to bi fired upon form close.
      /// </summary>
      property OnClose            : TGIS_PvlEvent
                                    read  FOnClose
                                    write FOnClose;
      /// <event/>
      /// <summary>
      ///   Event to bi fired upon form show.
      /// </summary>
      property OnShow            : TGIS_PvlEvent
                                    read  FOnShow
                                    write FOnShow;
  end;

  /// <summary>
  ///   General use form made in PVL.
  /// </summary>
  TGIS_PvlForm = class( TGIS_PvlBaseForm )
    protected
      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      procedure doCreate          ( const _parent : TObject;
                                          _style  : TGIS_PvlBorderStyle;
                                          _icons  : TGIS_PvlBorderIcons
                                  ) ; overload ;override;

    public
      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      constructor Create          ( const _parent : TObject
                                  ) ; overload; override ;
  end ;

  /// <summary>
  ///   Modal form made in PVL.
  /// </summary>
  TGIS_PvlModalForm = class( TGIS_PvlBaseForm, IGIS_PvlModalForm )
    private
      /// <summary>
      ///   Standard OK button.
      /// </summary>
      oBtnOK: TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Cancel button.
      /// </summary>
      oBtnCancel: TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Help button.
      /// </summary>
      oBtnHelp: TGIS_PvlModalButton;

      /// <summary>
      ///   Handler for help service.
      /// </summary>
      pOnHelp : TGIS_HelpEvent ;

    protected
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnOK        : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnCancel    : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnHelp      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_OnHelp       : TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_OnHelp       ( const _value    : TGIS_HelpEvent
                                  );

    protected

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doInitButtons     ; override ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doPlaceButtons    ; override ;

      /// <summary>
      ///   Standard action on OK button. ModalResult set to mrOK.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnOKClick        ( _sender : TObject
                                  ) ; virtual;

      /// <summary>
      ///   Standard action on Cancel button. ModalResult set to mrCancel.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnCancelClick    ( _sender : TObject
                                  ) ; virtual;

      /// <summary>
      ///   Standard action on Help button. Help event is raised.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnHelpClick      ( _sender : TObject
                                  ) ; virtual;
    protected
      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      procedure doCreate          ( const _parent : TObject;
                                          _style  : TGIS_PvlBorderStyle;
                                          _icons  : TGIS_PvlBorderIcons
                                  ) ; overload ;override;

    public
      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      constructor Create          ( const _parent : TObject
                                  ) ; overload; override ;
    public

      /// <summary>
      ///   Accessor for the OK button.
      /// </summary>
      property BtnOK              : TGIS_PvlModalButton
                                    read fget_BtnOK;

      /// <summary>
      ///   Accessor for the Cancel button.
      /// </summary>
      property BtnCancel          : TGIS_PvlModalButton
                                    read fget_BtnCancel;

      /// <summary>
      ///   Accessor for the help button.
      /// </summary>
      property BtnHelp            : TGIS_PvlModalButton
                                    read fget_BtnHelp;

      /// <event/>
      /// <summary>
      ///   Accessor for the help event.
      /// </summary>
      property OnHelpEvent        : TGIS_HelpEvent
                                    read  pOnHelp
                                    write pOnHelp;
  end;

  /// <summary>
  ///   Modal wizard made in PVL.
  /// </summary>
  TGIS_PvlModalWizard = class ( TGIS_PvlBaseForm, IGIS_PvlModalWizard )
    private

      /// <summary>
      ///   Service function that returns ModalWizard interface.
      /// </summary>
      function PlatformWizard : IGIS_PvlModalWizard ;

    private

      /// <summary>
      ///   Standard OK button.
      /// </summary>
      oBtnOK                      : TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Cancel button.
      /// </summary>
      oBtnCancel                  : TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Next button.
      /// </summary>
      oBtnNext                    : TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Previous button.
      /// </summary>
      oBtnPrevious                : TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Help button.
      /// </summary>
      oBtnHelp                    : TGIS_PvlModalButton;

      /// <summary>
      ///   Standard Paging mechanism.
      /// </summary>
      oPages                      : TGIS_PvlPages ;

      /// <summary>
      ///   Handler for help service.
      /// </summary>
      pOnHelp                     : TGIS_HelpEvent ;

    protected
      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnOK        : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnCancel    : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_BtnHelp      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_btnNext      : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_btnPrevious  : TGIS_PvlModalButton;

      /// <inheritdoc from="IGIS_PvlModalWizard"/>
      function  fget_Pages        : TGIS_PvlPages;

      /// <inheritdoc from="IGIS_PvlForm"/>
      function  fget_OnHelp       : TGIS_HelpEvent;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure fset_OnHelp       ( const _value    : TGIS_HelpEvent
                                  );
    protected

      /// <summary>
      ///   Standard action on OK button. ModalResult set to mrOK.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnOKClick        ( _sender : TObject
                                  ) ; virtual;

      /// <summary>
      ///   Standard action on Cancel button. ModalResult set to mrCancel.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnCancelClick    ( _sender : TObject
                                  ) ; virtual;

      /// <summary>
      ///   Standard action on Help button. Help event is raised.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure BtnHelpClick      ( _sender : TObject
                                  ) ; virtual;

      /// <summary>
      ///   Standard action on Next button.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure btnNextClick      ( _sender : TObject
                                  ) ;

      /// <summary>
      ///   Standard action on Previous button.
      /// </summary>
      /// <remarks>
      ///   Override this method to add more action.
      /// </remarks>
      /// <param name="_sender">
      ///   Sender object.
      /// </param>
      procedure btnPreviousClick  ( _sender : TObject
                                  ) ;

    public

      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      constructor Create          ( const _parent : TObject
                                  ) ; overload; override ;
    protected

      /// <inheritdoc from="TGIS_PvlBaseForm"/>
      procedure doCreate          ( const _parent : TObject;
                                          _style  : TGIS_PvlBorderStyle;
                                          _icons  : TGIS_PvlBorderIcons
                                  ) ; overload; override;


      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doInitButtons     ; override ;

      /// <inheritdoc from="IGIS_PvlForm"/>
      procedure doPlaceButtons    ; override ;

    public

      /// <summary>
      ///   Accessor for the OK button.
      /// </summary>
      property BtnOK              : TGIS_PvlModalButton
                                    read fget_BtnOK;

      /// <summary>
      ///   Accessor for the Cancel button.
      /// </summary>
      property BtnCancel          : TGIS_PvlModalButton
                                    read fget_BtnCancel;

      /// <summary>
      ///   Accessor for the Help button.
      /// </summary>
      property BtnHelp            : TGIS_PvlModalButton
                                    read fget_BtnHelp;

      /// <summary>
      ///   Accessor for the Next button.
      /// </summary>
      property BtnNext            : TGIS_PvlModalButton
                                    read fget_btnNext ;

      /// <summary>
      ///   Accessor for the Previous button.
      /// </summary>
      property BtnPrevious        : TGIS_PvlModalButton
                                    read fget_btnPrevious ;

      /// <event/>
      /// <summary>
      ///   Accessor for the Help event.
      /// </summary>
      property OnHelpEvent        : TGIS_HelpEvent
                                    read  pOnHelp
                                    write pOnHelp;

      /// <summary>
      ///   Pages acessor.
      /// </summary>
      property Pages              : TGIS_PvlPages
                                    read  fget_Pages ;
  end;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl;
{$ENDIF}

{$REGION 'TGIS_PvlBaseForm'}

procedure TGIS_PvlBaseForm.doCreate(
  const _parent : TObject;
        _style  : TGIS_PvlBorderStyle;
        _icons  : TGIS_PvlBorderIcons
) ;
begin

end;

constructor TGIS_PvlBaseForm.Create(
  const _parent : TObject;
        _style  : TGIS_PvlBorderStyle;
        _icons  : TGIS_PvlBorderIcons
) ;
begin

end;

constructor TGIS_PvlBaseForm.Create(
  const _parent : TObject
) ;
begin

end;

procedure TGIS_PvlBaseForm.doDestroy ;
var
  o : TGIS_PvlBase ;
begin
  o := oPlatform as TGIS_PvlBase ;
  FreeObject( o ) ;
  oPlatformFrm := nil ;
  inherited ;
end;

function TGIS_PvlBaseForm.fget_Name
  : String;
begin
  Result := oPlatformFrm.fget_Name ;
end;

procedure TGIS_PvlBaseForm.fset_Name(
  const _value : String
);
begin
  oPlatformFrm.fset_Name( _value ) ;
end;

function TGIS_PvlBaseForm.fget_Caption
  : String;
begin
  Result := oPlatformFrm.fget_Caption ;
end;

procedure TGIS_PvlBaseForm.fset_Caption(
  const _value : String
);
begin
  oPlatformFrm.fset_Caption( _value ) ;
end;

function TGIS_PvlBaseForm.fget_ClientHeight
  : Integer;
begin
  Result := oPlatformFrm.fget_ClientHeight ;
end;

procedure TGIS_PvlBaseForm.fset_ClientHeight(
  const _value: Integer
);
begin
  oPlatformFrm.fset_ClientHeight( _value ) ;
end;

function TGIS_PvlBaseForm.fget_ClientWidth
  : Integer;
begin
  Result := oPlatformFrm.fget_ClientWidth ;
end;

procedure TGIS_PvlBaseForm.fset_ClientWidth(
  const _value : Integer
);
begin
  oPlatformFrm.fset_ClientWidth( _value ) ;
end;

function TGIS_PvlBaseForm.fget_RightToLeft
  : Boolean;
begin
  Result := oPlatformFrm.fget_RightToLeft ;
end;

function TGIS_PvlBaseForm.fget_PPI
  : Integer;
begin
  Result := oPlatformFrm.fget_PPI ;
end;

function TGIS_PvlBaseForm.fget_PPIFix
  : Single;
begin
  Result := oPlatformFrm.fget_PPIFix ;
end;

function TGIS_PvlBaseForm.fget_CanvasScale
  : Single;
begin
  Result := oPlatformFrm.fget_CanvasScale ;
end;

function TGIS_PvlBaseForm.fget_Context
  : TGIS_PvlContext;
begin
  if not assigned( oContext ) then
    oContext := TGIS_PvlContext.Create( self );

  Result := oContext ;
end;

function TGIS_PvlBaseForm.fget_BorderStyle
  : TGIS_PvlBorderStyle;
begin
  Result := oBorderStyle;
end;

function TGIS_PvlBaseForm.fget_BorderIcons
  : TGIS_PvlBorderIcons;
begin
  Result := oBorderIcons;
end;

function TGIS_PvlBaseForm.fget_ModalResult
  : TGIS_PvlModalResult;
begin
  Result := oPlatformFrm.fget_ModalResult;
end;

procedure TGIS_PvlBaseForm.fset_ModalResult(
  const _value: TGIS_PvlModalResult
);
begin
  oPlatformFrm.fset_ModalResult( _value ) ;
end;

procedure TGIS_PvlBaseForm.doUpdateGUI ;
begin
  oPlatformFrm.doUpdateGUI;
end;

procedure TGIS_PvlBaseForm.doInitButtons ;
begin
  oPlatformFrm.doInitButtons;
end;

procedure TGIS_PvlBaseForm.doPlaceButtons ;
begin
  oPlatformFrm.doPlaceButtons;
end;

procedure TGIS_PvlBaseForm.doAfterPPIChanged ;
begin
  oPlatformFrm.doAfterPPIChanged;
end;

procedure TGIS_PvlBaseForm.doShowForm ;
begin
  //?
end;

procedure TGIS_PvlBaseForm.LockWindow ;
begin
  oPlatformFrm.LockWindow;
end;

procedure TGIS_PvlBaseForm.UnlockWindow ;
begin
  oPlatformFrm.UnlockWindow;
end;

procedure TGIS_PvlBaseForm.DoBeforeCreate;
begin
  // for safe inheritance only
end;

procedure TGIS_PvlBaseForm.DoAfterCreate ;
begin

end;

procedure TGIS_PvlBaseForm.DoInitForm ;
begin
  // for safe inheritance only
end;

procedure TGIS_PvlBaseForm.doInitControls ;
begin
  // for safe inheritance only
end;

procedure TGIS_PvlBaseForm.DoRedraw ;
begin
  Context.Redraw ;
end;


procedure TGIS_PvlBaseForm.Show;
begin
  oPlatformFrm.Show ;
end;

function TGIS_PvlBaseForm.ShowModal(
  const _proc : TGIS_Proc;
  const _free : Boolean
): TGIS_PvlModalResult;
begin
  Result := oPlatformFrm.ShowModal( _proc, _free ) ;
end;

procedure TGIS_PvlBaseForm.FreeForm ;
begin
  oPlatformFrm.FreeForm ;
end;

function TGIS_PvlBaseForm.ShowModal
  : TGIS_PvlModalResult;
begin
  doPlaceButtons ;
  Result := oPlatformFrm.ShowModal ;
end;

procedure TGIS_PvlBaseForm.Close ;
begin
  oPlatformFrm.Close ;
end;

{$ENDREGION 'TGIS_PvlBaseForm'}

{$REGION 'TGIS_PvlForm'}

procedure TGIS_PvlForm.doCreate(
  const _parent : TObject;
        _style  : TGIS_PvlBorderStyle;
        _icons  : TGIS_PvlBorderIcons
) ;
var
  ctx : TGIS_PvlContext ;
begin
  oBorderStyle := _style ;
  oBorderIcons := _icons ;

  ctx := TGIS_PvlContext.Create(_parent) ;
  try
    oPlatform := ctx.Platform.CreateObject(self, 'Form') ;
  finally
    FreeObject( ctx ) ;
  end;

  oPlatformFrm  := oPlatform as IGIS_PvlForm;
end;

constructor TGIS_PvlForm.Create(
  const _parent : TObject
) ;
begin
  doCreate( _parent,
            TGIS_PvlBorderStyle.Sizeable,
            [ TGIS_PvlBorderIcon.Minimize,
              TGIS_PvlBorderIcon.Maximize,
              TGIS_PvlBorderIcon.SystemMenu
            ]
          ) ;

  DoInitForm ;

  doInitButtons ;
  doPlaceButtons ;
  DoInitControls ;

end;

{$ENDREGION 'TGIS_PvlForm'}

{$REGION 'TGIS_PvlModalForm'}

procedure TGIS_PvlModalForm.doCreate(
  const _parent : TObject;
        _style  : TGIS_PvlBorderStyle;
        _icons  : TGIS_PvlBorderIcons
) ;
var
  ctx : TGIS_PvlContext ;
begin
  oBorderStyle := _style ;
  oBorderIcons := _icons ;

  ctx := TGIS_PvlContext.Create( _parent ) ;
  try
    oPlatform := ctx.Platform.CreateObject(self, 'ModalForm') ;
    oPlatformFrm := oPlatform as IGIS_PvlForm;
  finally
    ctx.FreeContext ;
  end;
end;

constructor TGIS_PvlModalForm.Create(
  const _parent : TObject
) ;
begin
  DoBeforeCreate ;

  doCreate( _parent,
            TGIS_PvlBorderStyle.Fixed,
            [ TGIS_PvlBorderIcon.SystemMenu
            ]
          ) ;

  DoInitForm ;

  doInitButtons ;
  doPlaceButtons ;
  DoInitControls ;

  DoAfterCreate ;
end;

function TGIS_PvlModalForm.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := oBtnOK ;
end;

function TGIS_PvlModalForm.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := oBtnCancel ;
end;

function TGIS_PvlModalForm.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := oBtnHelp ;
end;

function TGIS_PvlModalForm.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := pOnHelp ;
end;

procedure TGIS_PvlModalForm.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  pOnHelp := _value ;
end;

procedure TGIS_PvlModalForm.doInitButtons ;
begin
  Context.Refresh ; // Make sure PPI is read properly ! Especially on 4K !
  oBtnHelp := TGIS_PvlModalButton.Create( Context ) ;
  oBtnHelp.Caption := _rsrc( GIS_RS_BTN_HELP ) ;
  {$IFNDEF OXYGENE}
    btnHelp.OnClick := BtnHelpClick ;
  {$ELSE}
    BtnHelp.OnClick := @BtnHelpClick ;
  {$ENDIF}

  pOnHelp := nil ;

  oBtnCancel := TGIS_PvlModalButton.Create( Context ) ;
  oBtnCancel.Caption := _rsrc( GIS_RS_BTN_CANCEL ) ;
  {$IFNDEF OXYGENE}
    oBtnCancel.OnClick := BtnCancelClick ;
  {$ELSE}
    oBtnCancel.OnClick := @BtnCancelClick ;
  {$ENDIF}

  oBtnOK := TGIS_PvlModalButton.Create( Context ) ;
  oBtnOK.Caption := _rsrc( GIS_RS_BTN_OK ) ;
  oBtnOK.Default := True ;
  {$IFNDEF OXYGENE}
    oBtnOK.OnClick := BtnOKClick ;
  {$ELSE}
    oBtnOK.OnClick := @BtnOKClick ;
  {$ENDIF}

  oPlatformFrm.doInitButtons ;
end;

procedure TGIS_PvlModalForm.doPlaceButtons ;
begin
  {$IFNDEF GIS_MOBILE_DIALOGS}
    oBtnHelp.Place( 75, 0,
                   nil, oContext.HMargin,
                   nil, ClientHeight - oBtnHelp.Height - oContext.VMargin
                 ) ;
    oBtnCancel.Place( 75, 0,
                     nil, -oContext.HMargin,
                     nil, ClientHeight - oBtnCancel.Height - oContext.VMargin
                   ) ;
    oBtnOK.Place( 75, 0,
                 oBtnCancel, -oContext.HSpace,
                 nil, ClientHeight - oBtnCancel.Height - oContext.VMargin
               ) ;
    oBtnOK.Caption := _rsrc( GIS_RS_BTN_OK ) ;
    oBtnCancel.Caption := _rsrc( GIS_RS_BTN_CANCEL ) ;
    oBtnHelp.Caption := _rsrc( GIS_RS_BTN_HELP ) ;
  {$ENDIF}

  oPlatformFrm.doPlaceButtons;
end;

procedure TGIS_PvlModalForm.BtnOKClick(
  _sender: TObject
) ;
begin
  if BtnOK.Visible then
    ModalResult := TGIS_PvlModalResult.OK ;

  FreeForm ;
end;

procedure TGIS_PvlModalForm.BtnCancelClick(
  _sender: TObject
) ;
begin
  if BtnCancel.Visible then
    ModalResult := TGIS_PvlModalResult.Cancel ;

  FreeForm ;
end;

procedure TGIS_PvlModalForm.BtnHelpClick(
  _sender: TObject
) ;
begin
  if BtnHelp.Visible then
    if assigned( pOnHelp ) then
      {$IFNDEF OXYGENE}
        pOnHelp( Self, Name ) ;
      {$ELSE}
        pOnHelp( Self, TGIS_HelpEventArgs.create( Name ) ) ;
      {$ENDIF}
end;


{$ENDREGION 'TGIS_PvlModalForm'}

{$REGION 'TGIS_PvlModalWizard'}

constructor TGIS_PvlModalWizard.Create(
  const _parent : TObject
) ;
begin
  doCreate( _parent,
            TGIS_PvlBorderStyle.Fixed,
            []
          ) ;

  DoInitForm ;

  doInitButtons ;
  doPlaceButtons ;
  DoInitControls ;
end;

procedure TGIS_PvlModalWizard.doCreate(
  const _parent : TObject;
  _style : TGIS_PvlBorderStyle; _icons: TGIS_PvlBorderIcons
) ;
var
  ctx : TGIS_PvlContext ;
begin
  oBorderStyle := _style ;
  oBorderIcons := _icons ;

  DoBeforeCreate ;

  ctx := TGIS_PvlContext.Create(_parent) ;
  try
    oPlatform := ctx.Platform.CreateObject(self, 'ModalWizard') ;
    oPlatformFrm := oPlatform as IGIS_PvlForm;
  finally
    ctx.FreeContext ;
  end;

  DoAfterCreate ;

end;

procedure TGIS_PvlModalWizard.doInitButtons ;
begin
  Context.Refresh ; // Make sure PPI is read properly ! Especially on 4K !
  oBtnHelp := TGIS_PvlModalButton.Create( Context ) ;
  oBtnHelp.Caption := _rsrc( GIS_RS_BTN_HELP ) ;
  {$IFNDEF OXYGENE}
    btnHelp.OnClick := BtnHelpClick ;
  {$ELSE}
    BtnHelp.OnClick := @BtnHelpClick ;
  {$ENDIF}

  pOnHelp := nil ;

  oBtnCancel := TGIS_PvlModalButton.Create( Context ) ;
  oBtnCancel.Caption := _rsrc( GIS_RS_BTN_CANCEL ) ;
  {$IFNDEF OXYGENE}
    oBtnCancel.OnClick := BtnCancelClick ;
  {$ELSE}
    oBtnCancel.OnClick := @BtnCancelClick ;
  {$ENDIF}

  oBtnOK := TGIS_PvlModalButton.Create( Context ) ;
  oBtnOK.Caption := _rsrc( GIS_RS_BTN_OK ) ;
  {$IFNDEF OXYGENE}
    oBtnOK.OnClick := BtnOKClick ;
  {$ELSE}
    oBtnOK.OnClick := @BtnOKClick ;
  {$ENDIF}
  oBtnOK.Default := True ;

  oBtnNext := TGIS_PvlModalButton.Create( Context ) ;
  oBtnNext.Caption := _rsrc( GIS_RS_BTN_NEXT ) ;
  {$IFNDEF OXYGENE}
    oBtnNext.OnClick := BtnNextClick ;
  {$ELSE}
    oBtnNext.OnClick := @btnNextClick ;
  {$ENDIF}

  oBtnPrevious := TGIS_PvlModalButton.Create( Context ) ;
  oBtnPrevious.Caption := _rsrc( GIS_RS_BTN_PREVIOUS ) ;
  oBtnPrevious.Enabled := False ;
  {$IFNDEF OXYGENE}
    oBtnPrevious.OnClick := BtnPreviousClick ;
  {$ELSE}
    oBtnPrevious.OnClick := @btnPreviousClick ;
  {$ENDIF}

  oPlatformFrm.doInitButtons ;
end;

procedure TGIS_PvlModalWizard.doPlaceButtons ;
begin
  oBtnHelp.Place( 75, 0,
                   nil, Context.HMargin,
                   nil, Self.ClientHeight - BtnHelp.Height - Context.VMargin
                 ) ;
  oBtnCancel.Place( 75, 0,
                     nil, -Context.HMargin,
                     nil, Self.ClientHeight - BtnCancel.Height - Context.VMargin
                   ) ;
  oBtnOK.Place( 75, 0,
                 BtnCancel, -Context.HSpace,
                 nil, Self.ClientHeight - BtnCancel.Height - Context.VMargin
               ) ;

  oBtnNext.Place( 75, 0,
                  oBtnCancel, -Context.HSpace,
                  nil, Self.ClientHeight - BtnCancel.Height - Context.VMargin
                ) ;
  oBtnPrevious.Place( 75, 0,
                      oBtnNext, -Context.HSpace,
                      nil, Self.ClientHeight - BtnCancel.Height - Context.VMargin
                    ) ;

  oPlatformFrm.doPlaceButtons;
end;

function TGIS_PvlModalWizard.PlatformWizard
  : IGIS_PvlModalWizard ;
begin
  Result := oPlatformFrm as IGIS_PvlModalWizard ;
end;

function TGIS_PvlModalWizard.fget_BtnOK
  : TGIS_PvlModalButton;
begin
  Result := oBtnOK ;
end;

function TGIS_PvlModalWizard.fget_BtnCancel
  : TGIS_PvlModalButton;
begin
  Result := oBtnCancel ;
end;

function TGIS_PvlModalWizard.fget_BtnHelp
  : TGIS_PvlModalButton;
begin
  Result := oBtnHelp ;
end;

function TGIS_PvlModalWizard.fget_btnNext
  : TGIS_PvlModalButton ;
begin
  Result := oBtnNext ;
end;

function TGIS_PvlModalWizard.fget_btnPrevious
  : TGIS_PvlModalButton ;
begin
  Result := oBtnPrevious ;
end;

function TGIS_PvlModalWizard.fget_Pages
  : TGIS_PvlPages ;
begin
  if not assigned( oPages ) then begin
    oPages := TGIS_PvlPages.create( Context ) ;
    oPages.Place( ClientWidth - 2 * Context.HMargin, oBtnNext.Top - Context.VMargin, nil, Context.HMargin, nil, Context.VMargin ) ;
  end ;

  Result := oPages ;
end;

function TGIS_PvlModalWizard.fget_OnHelp
  : TGIS_HelpEvent;
begin
  Result := pOnHelp ;
end;

procedure TGIS_PvlModalWizard.fset_OnHelp(
  const _value    : TGIS_HelpEvent
);
begin
  pOnHelp := _value ;
end;

procedure TGIS_PvlModalWizard.BtnOKClick(
  _sender: TObject
) ;
begin
  if BtnOK.Visible then
    ModalResult := TGIS_PvlModalResult.OK ;

  FreeForm ;
end;

procedure TGIS_PvlModalWizard.BtnCancelClick(
  _sender: TObject
) ;
begin
  if BtnCancel.Visible then
    ModalResult := TGIS_PvlModalResult.Cancel ;

  FreeForm ;
end;

procedure TGIS_PvlModalWizard.BtnNextClick(
  _sender: TObject
) ;
var
  nextEvn : TGIS_PvlEvent ;
  prevEvn : TGIS_PvlEvent ;
begin
  if PlatformWizard.fget_Pages.IsLast then
    exit ;

  nextEvn := BtnNext.OnClick ;
  prevEvn := BtnPrevious.OnClick ;

  BtnNext.OnClick := nil ;
  BtnPrevious.OnClick := nil ;

  try
    PlatformWizard.fget_Pages.Next( BtnNext ) ;
  finally
    BtnNext.OnClick := nextEvn ;
    BtnPrevious.OnClick := prevEvn ;
  end;

  BtnNext.Visible  := not PlatformWizard.fget_Pages.IsLast ;
  BtnOK.Visible    := PlatformWizard.fget_Pages.IsLast ;

  BtnPrevious.Enabled := not Pages.IsFirst ;
end;

procedure TGIS_PvlModalWizard.BtnPreviousClick(
  _sender: TObject
) ;
var
  nextEvn : TGIS_PvlEvent ;
  prevEvn : TGIS_PvlEvent ;
begin
  if PlatformWizard.fget_Pages.IsFirst then
    exit ;

  nextEvn := BtnNext.OnClick ;
  prevEvn := BtnPrevious.OnClick ;

  BtnNext.OnClick := nil ;
  BtnPrevious.OnClick := nil ;

  try
    PlatformWizard.fget_Pages.Previous( BtnPrevious ) ;
  finally
    BtnNext.OnClick := nextEvn ;
    BtnPrevious.OnClick := prevEvn ;
  end;

  BtnNext.Visible  := not PlatformWizard.fget_Pages.IsLast ;
  BtnOK.Visible    := PlatformWizard.fget_Pages.IsLast ;

  BtnPrevious.Enabled := not PlatformWizard.fget_Pages.IsFirst ;
end;

procedure TGIS_PvlModalWizard.BtnHelpClick(
  _sender: TObject
) ;
begin
  if BtnHelp.Visible then
    if assigned( pOnHelp ) then
      {$IFNDEF OXYGENE}
        pOnHelp( Self, Name ) ;
      {$ELSE}
        pOnHelp( Self, TGIS_HelpEventArgs.create( Name ) ) ;
      {$ENDIF}
end;


{$ENDREGION 'TGIS_PvlModalWizard'}

//==================================== END =====================================
end.

