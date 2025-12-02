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
  System form helper.
}

unit VCL.GisSystemForms ;
{$HPPEMIT '#pragma link "VCL.GisSystemForms"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Winapi.Messages,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.StdCtrls,
  VCL.FileCtrl,
  VCL.GisControlCsSystem,
  VCL.GisLineSymbolEditor,
  VCL.GisViewerWnd,

  PVL.GisPvl,

  GisCsSystems,
  GisControlPrintPreviewSimple,
  GisTypes,
  GisViewer,
  GisInterfaces,
  GisPrintManagerAbstract,
  GisTypesUI;

type

  /// <summary>
  ///   Platform implementation of PVL open dialog control.
  /// </summary>
  TGIS_OpenDialog = class( TGIS_PvlOpenDialog )
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    protected
      function  fget_Filter         : String ;                         override;
      procedure fset_Filter         ( const _value : String
                                    ) ;                                override;
      function  fget_FileName       : String ;                         override;
      procedure fset_FileName       ( const _value : String
                                    ) ;                                override;
    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult;                 override;

    protected
      procedure doDestroy ; override ;

  end;

  /// <summary>
  ///   Platform implementation of PVL save dialog control.
  /// </summary>
  TGIS_SaveDialog = class( TGIS_PvlSaveDialog )
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    protected
      function  fget_Filter         : String ;                         override;
      procedure fset_Filter         ( const _value : String
                                    ) ;                                override;
      function  fget_FileName       : String ;                         override;
      procedure fset_FileName       ( const _value : String
                                    ) ;                                override;
    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult;                 override;

    protected
      procedure doDestroy ; override ;

  end;

  /// <summary>
  ///   Platform implementation of PVL option dialog control.
  /// </summary>
  TGIS_OptionDialog = class( TGIS_PvlOptionDialog )
    private
      FText : String ;

    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    protected

      /// <inheritdoc/>
      function  fget_Text         : String;                            override;

      /// <inheritdoc/>
      procedure fset_Text         ( const _value      : String
                                  );                                   override;
    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult;                 override;

  end;

  /// <summary>
  ///   Platform implementation of PVL info dialog control.
  /// </summary>
  TGIS_InfoDialog = class( TGIS_PvlInfoDialog )
    private
      FText   : String ;
      FTitle  : String ;
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;

      /// <inheritdoc/>
      procedure Execute           ;                                    override;

    protected

      /// <inheritdoc/>
      function  fget_Text         : String;                            override;

      /// <inheritdoc/>
      procedure fset_Text         ( const _value      : String
                                  );                                   override;

      /// <inheritdoc/>
      function  fget_Title        : String;                            override;

      /// <inheritdoc/>
      procedure fset_Title        ( const _value      : String
                                  );                                   override;
  end;

  /// <summary>
  ///   Platform implementation of PVL warning dialog control.
  /// </summary>
  TGIS_WarningDialog = class( TGIS_PvlWarningDialog )
    private
      FText   : String ;
      FTitle  : String ;
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;

      /// <inheritdoc/>
      procedure Execute           ;                                    override;

    protected

      /// <inheritdoc/>
      function  fget_Text         : String;                            override;

      /// <inheritdoc/>
      procedure fset_Text         ( const _value      : String
                                  );                                   override;

      /// <inheritdoc/>
      function  fget_Title        : String;                            override;

      /// <inheritdoc/>
      procedure fset_Title        ( const _value      : String
                                  );                                   override;
  end;

  /// <summary>
  ///   Platform implementation of PVL error dialog control.
  /// </summary>
  TGIS_ErrorDialog = class( TGIS_PvlErrorDialog )
    private
      FText   : String ;
      FTitle  : String ;
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;

      /// <inheritdoc/>
      procedure Execute           ;                                    override;

    protected

      /// <inheritdoc/>
      function  fget_Text         : String;                            override;

      /// <inheritdoc/>
      procedure fset_Text         ( const _value      : String
                                  );                                   override;

      /// <inheritdoc/>
      function  fget_Title        : String;                            override;

      /// <inheritdoc/>
      procedure fset_Title        ( const _value      : String
                                  );                                   override;
  end;

  /// <summary>
  ///   Platform implementation of PVL select folder dialog control.
  /// </summary>
  TGIS_SelectFolderDialog = class ( TGIS_PvlSelectFolderDialog )
    public

      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    private
      sDir                      : String ;

    protected
      /// <summary>
      ///   Getter for Directory property.
      /// </summary>
      /// <returns>
      ///   Selected directory path
      /// </returns>
      function  fget_Directory  : String ;                             override;

      /// <summary>
      ///   Setter for Directory property.
      /// </summary>
      /// <param name="_value">
      ///   New value for Directory path
      /// </param>
      procedure fset_Directory  ( const _value      : String
                                );                                     override;

    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult;                 override;
    protected
      procedure doDestroy ;                                            override;
  end;

  /// <summary>
  ///   Platform implementation of PVL TGIS_LineSymbologyEditor dialog control.
  /// </summary>
  TGIS_LineSymbolEditor = class ( TGIS_PvlLineSymbolEditor )
    public
      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;

    protected

      /// <inheritdoc from="IGIS_PvlLineSymbolEditor"/>
      function  fget_Symbol       : String;                            override;

      /// <inheritdoc from="IGIS_PvlLineSymbolEditor"/>
      procedure fset_Symbol       ( const _symbol     : String
                                  );                                   override;

    public

      /// <inheritdoc from="IGIS_PvlLineSymbolEditor"/>
      function Execute            ( const _path       : String;
                                    const _proc       : TGIS_Proc
                                  ) : TGIS_PvlModalResult;    overload;override;

      /// <inheritdoc/>
      function Execute            ( const _path       : String;
                                    const _onhelp     : TGIS_HelpEvent;
                                    const _proc       : TGIS_Proc
                                  ) : TGIS_PvlModalResult;    overload;override;
    protected
      procedure doDestroy ;                                            override;
  end;

  /// <summary>
  ///   Platform implementation of PVL TGIS_ControlCSSystem dialog control.
  /// </summary>
  TGIS_ControlCSSystem = class ( TGIS_PvlControlCSSystem )
    public
      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    protected

      /// <inheritdoc/>
      function fget_CS            : TGIS_CSCoordinateSystem;           override;

    public

      /// <inheritdoc/>
      function Execute            ( const _cs         : TGIS_CSCoordinateSystem;
                                    const _proc       : TGIS_Proc
                                  ): TGIS_PvlModalResult;
                                                             overload; override;

      /// <inheritdoc/>
      function Execute            ( const _cs         : TGIS_CSCoordinateSystem;
                                    const _onhelp     : TGIS_HelpEvent;
                                    const _proc       : TGIS_Proc
                                  ): TGIS_PvlModalResult;
                                                             overload; override;

      /// <inheritdoc/>
      function Execute           ( const _cs          : TGIS_CSCoordinateSystem;
                                   const _area_epsg   : Integer ;
                                   const _area_extent : TGIS_Extent ;
                                   const _onhelp      : TGIS_HelpEvent ;
                                   const _proc        : TGIS_Proc
                                 ) : TGIS_PvlModalResult ;
                                                             overload ; virtual ;

    protected
      procedure doDestroy ;                                            override;
  end;

  /// <summary>
  ///   Platform implementation of PVL TGIS_ControlPrintPreviewSimple dialog control.
  /// </summary>
  TGIS_ControlPrintPreviewSimple = class ( TGIS_PvlControlPrintPreviewSimple )
    public
      /// <inheritdoc/>
      constructor Create            (        _context   : TGIS_PvlContext;
                                      const _self       : TGIS_PvlSystemForm
                                    ) ;                                override;
    protected
      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      function  fget_GIS_Viewer   : IGIS_Viewer;                       override;

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure fset_GIS_Viewer   ( const _GIS        : IGIS_Viewer
                                  );                                   override;
    public

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure Preview;                                      overload;override;

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure Preview           ( var   _scale      : Double
                                  );
                                                              overload;override;

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure Preview           ( const _printManager : TGIS_PrintManagerAbstract
                                  );
                                                              overload;override;

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure Preview           ( const _printManager : TGIS_PrintManagerAbstract;
                                    var   _scale      : Double
                                  );
                                                              overload;override;

      /// <inheritdoc from="IGIS_PvlControlPrintPreviewSimple"/>
      procedure Preview           ( const _printManager
                                                      : TGIS_PrintManagerAbstract;
                                    const _customPage : String;
                                    var   _scale      : Double
                                  );
                                                              overload;override;
    protected
      procedure doDestroy ;                                            override;
  end;

//##############################################################################
implementation

uses
  GisRTL,
  VCL.Dialogs,
  GisResource ;

{$REGION 'TGIS_OpenDialog'}

constructor TGIS_OpenDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := TOpenDialog.Create( TWinControl( oParent ) );
end;

function TGIS_OpenDialog.fget_Filter
  : string ;
begin
  Result := TOpenDialog( oForm ).Filter ;
end;

procedure TGIS_OpenDialog.fset_Filter(
  const _value: string
) ;
begin
  TOpenDialog( oForm ).Filter := _value ;
end;

function TGIS_OpenDialog.fget_FileName
  : string ;
begin
  Result := TOpenDialog( oForm ).FileName ;
end;

procedure TGIS_OpenDialog.fset_FileName(
  const _value: string
) ;
begin
  TOpenDialog( oForm ).FileName := _value ;
end;

function TGIS_OpenDialog.Execute
  : TGIS_PvlModalResult ;
begin
  if TOpenDialog( oForm ).Execute then
    Result := TGIS_PvlModalResult.OK
  else
    Result := TGIS_PvlModalResult.Cancel ;
end;

procedure TGIS_OpenDialog.doDestroy ;
begin
  FreeObject( oForm ) ;
  inherited ;
end;

{$ENDREGION 'TGIS_OpenDialog'}

{$REGION 'TGIS_SaveDialog'}

constructor TGIS_SaveDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := TSaveDialog.Create( TWinControl( oParent ) );
end;

function TGIS_SaveDialog.fget_Filter
  : string ;
begin
  Result := TSaveDialog( oForm ).Filter ;
end;

procedure TGIS_SaveDialog.fset_Filter(
  const _value: string
) ;
begin
  TSaveDialog( oForm ).Filter := _value ;
end;

function TGIS_SaveDialog.fget_FileName
  : string ;
begin
  Result := TSaveDialog( oForm ).FileName ;
end;

procedure TGIS_SaveDialog.fset_FileName(
  const _value: string
) ;
begin
  TSaveDialog( oForm ).FileName := _value ;
end;

function TGIS_SaveDialog.Execute
  : TGIS_PvlModalResult ;
begin
  if TSaveDialog( oForm ).Execute then
    Result := TGIS_PvlModalResult.OK
  else
    Result := TGIS_PvlModalResult.Cancel ;
end;

procedure TGIS_SaveDialog.doDestroy ;
begin
  FreeObject( oForm ) ;
  inherited ;
end;

{$ENDREGION 'TGIS_SaveDialog'}

{$REGION 'TGIS_OptionDialog}

constructor TGIS_OptionDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_OptionDialog.fset_Text(
  const _value: string
) ;
begin
  FText := _value ;
end;

function TGIS_OptionDialog.fget_Text
  : string ;
begin
  Result := FText ;
end;

function TGIS_OptionDialog.Execute
  : TGIS_PvlModalResult ;
begin
    if MessageDlg(
       Text,
       TMsgDlgType.mtConfirmation,
       [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
       0,
       TMsgDlgBtn.mbNo
     ) = mrYes then
    Result := TGIS_PvlModalResult.Yes
  else
    Result := TGIS_PvlModalResult.No ;
end;

{$ENDREGION 'TGIS_OptionDialog}

{$REGION 'TGIS_InfoDialog'}

constructor TGIS_InfoDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_InfoDialog.fset_Text(
  const _value: String
) ;
begin
  FText := _value ;
end;

function TGIS_InfoDialog.fget_Text
  : String ;
begin
  Result := FText ;
end;

procedure TGIS_InfoDialog.fset_Title(
  const _value: String
) ;
begin
  FTitle := _value ;
end;

function TGIS_InfoDialog.fget_Title
  : String ;
begin
  Result := FTitle ;
end;

procedure TGIS_InfoDialog.Execute
  ;
begin
  if IsStringEmpty( FTitle ) then
    FTitle := 'Info' ;

  Application.MessageBox( PChar( FText ), PChar( FTitle ), MB_OK or MB_ICONINFORMATION );
end;

{$ENDREGION 'TGIS_InfoDialog'}

{$REGION 'TGIS_WarningDialog'}

constructor TGIS_WarningDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_WarningDialog.fset_Text(
  const _value: String
) ;
begin
  FText := _value ;
end;

function TGIS_WarningDialog.fget_Text
  : String ;
begin
  Result := FText ;
end;

procedure TGIS_WarningDialog.fset_Title(
  const _value: String
) ;
begin
  FTitle := _value ;
end;

function TGIS_WarningDialog.fget_Title
  : String ;
begin
  Result := FTitle ;
end;

procedure TGIS_WarningDialog.Execute
 ;
begin
  if IsStringEmpty( FTitle ) then
    FTitle := 'Warning' ;

  Application.MessageBox( PChar( FText ), PChar( FTitle ), MB_OK or MB_ICONWARNING );
end;

{$ENDREGION 'TGIS_WarningDialog'}

{$REGION 'TGIS_ErrorDialog'}

constructor TGIS_ErrorDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_ErrorDialog.fset_Text(
  const _value: String
) ;
begin
  FText := _value ;
end;

function TGIS_ErrorDialog.fget_Text
  : String ;
begin
  Result := FText ;
end;

procedure TGIS_ErrorDialog.fset_Title(
  const _value: String
) ;
begin
  FTitle := _value ;
end;

function TGIS_ErrorDialog.fget_Title
  : String ;
begin
  Result := FTitle ;
end;

procedure TGIS_ErrorDialog.Execute
 ;
begin
  if IsStringEmpty( FTitle ) then
    FTitle := 'Error' ;

  Application.MessageBox( PChar( FText ), PChar( FTitle ), MB_OK or MB_ICONERROR );
end;

{$ENDREGION 'TGIS_ErrorDialog'}

{$REGION 'TGIS_SelectFolderDialog'}

constructor TGIS_SelectFolderDialog.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := TFileOpenDialog.Create( TWinControl( oParent ) ) ;
end;

function TGIS_SelectFolderDialog.fget_Directory
  : string ;
begin
  Result := sDir ;
end;

procedure TGIS_SelectFolderDialog.fset_Directory(
  const _value: string
) ;
begin
  sDir := _value ;
end;

function TGIS_SelectFolderDialog.Execute
  : TGIS_PvlModalResult ;
begin

  with TFileOpenDialog( oForm ) do begin
    Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem];
    DefaultFolder := sDir;
    FileName := sDir;
  end;

  if TFileOpenDialog( oForm ).Execute then
    Result := TGIS_PvlModalResult.OK
  else
    Result := TGIS_PvlModalResult.Cancel ;

  sDir := TFileOpenDialog( oForm ).FileName ;
end;

procedure TGIS_SelectFolderDialog.doDestroy;
begin
  FreeObject( oForm ) ;
  inherited ;
end;


{$ENDREGION 'TGIS_SelectFolderDialog'}

{$REGION 'TGIS_LineSymbolEditor'}

constructor TGIS_LineSymbolEditor.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := VCL.GisLineSymbolEditor.TGIS_LineSymbolEditor.Create( TWinControl( oParent ) );
end;

function TGIS_LineSymbolEditor.fget_Symbol
  : String;
begin
  Result := VCL.GisLineSymbolEditor.TGIS_LineSymbolEditor( oForm ).Symbol;
end;

procedure TGIS_LineSymbolEditor.fset_Symbol(
  const _symbol: string
);
begin
  VCL.GisLineSymbolEditor.TGIS_LineSymbolEditor( oForm ).Symbol := _symbol;
end;

function TGIS_LineSymbolEditor.Execute(
  const _path : String;
  const _proc : TGIS_Proc
) : TGIS_PvlModalResult;
var
  hlp : TGIS_HelpEvent;
begin
  hlp := nil;
  result := Execute( _path, hlp, _proc );
end;

function TGIS_LineSymbolEditor.Execute(
  const _path   : String;
  const _onhelp : TGIS_HelpEvent;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult;
var
  res : Integer;
begin
  res := VCL.GisLineSymbolEditor.TGIS_LineSymbolEditor( oForm ).Execute( _path, _onhelp );
  if res = mrOk then
    result := TGIS_PvlModalResult.OK
  else if res = mrCancel then
    result := TGIS_PvlModalResult.Cancel
  else
    result := TGIS_PvlModalResult.None;

  if assigned( _proc ) then
    _proc( result );
end;

procedure TGIS_LineSymbolEditor.doDestroy;
begin
  FreeObject( oForm ) ;
  inherited ;
end;

{$ENDREGION 'TGIS_LineSymbolEditor'}

{$REGION 'TGIS_ControlCSSystem'}

constructor TGIS_ControlCSSystem.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := VCL.GisControlCSSystem.TGIS_ControlCSSystem.Create( TWinControl( oParent ) );
end;

function TGIS_ControlCSSystem.fget_CS
  : TGIS_CSCoordinateSystem;
begin
  Result := VCL.GisControlCSSystem.TGIS_ControlCSSystem( oForm ).CS;
end;

function TGIS_ControlCSSystem.Execute(
  const _cs   : TGIS_CSCoordinateSystem;
  const _proc : TGIS_Proc
) : TGIS_PvlModalResult;
var
  hlp : TGIS_HelpEvent;
begin
  hlp := nil;
  result := Execute( _cs, hlp, _proc );
end;

function TGIS_ControlCSSystem.Execute(
  const _cs     : TGIS_CSCoordinateSystem;
  const _onhelp : TGIS_HelpEvent;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult;
var
  res : Integer;
begin
  res := VCL.GisControlCSSystem.TGIS_ControlCSSystem( oForm ).Execute( _cs, _onhelp );
  if res = mrOk then
    result := TGIS_PvlModalResult.OK
  else if res = mrCancel then
    result := TGIS_PvlModalResult.Cancel
  else
    result := TGIS_PvlModalResult.None;

  if assigned( _proc ) then
    _proc( result );
end;

function TGIS_ControlCSSystem.Execute(
  const _cs          : TGIS_CSCoordinateSystem;
  const _area_epsg   : Integer ;
  const _area_extent : TGIS_Extent ;
  const _onhelp      : TGIS_HelpEvent ;
  const _proc        : TGIS_Proc
) : TGIS_PvlModalResult;
var
  res : Integer;
begin
  res := VCL.GisControlCSSystem.TGIS_ControlCSSystem( oForm ).Execute(
    _cs,
    _area_epsg,
    _area_extent,
    _onhelp
  );
  if res = mrOk then
    result := TGIS_PvlModalResult.OK
  else if res = mrCancel then
    result := TGIS_PvlModalResult.Cancel
  else
    result := TGIS_PvlModalResult.None;

  if assigned( _proc ) then
    _proc( result );
end;

procedure TGIS_ControlCSSystem.doDestroy;
begin
  FreeObject( oForm ) ;
  inherited ;
end;

{$ENDREGION 'T_PvlControlCSSystem'}

{$REGION 'TGIS_ControlPrintPreviewSimple'}

constructor TGIS_ControlPrintPreviewSimple.Create(
        _context    : TGIS_PvlContext;
  const _self       : TGIS_PvlSystemForm
) ;
begin
  inherited ;

  oForm := GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple.Create( TWinControl( oParent ) );
end;


function TGIS_ControlPrintPreviewSimple.fget_GIS_Viewer: IGIS_Viewer;
begin
  Result := GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).GIS_Viewer;
end;

procedure TGIS_ControlPrintPreviewSimple.fset_GIS_Viewer(
  const _GIS: IGIS_Viewer
);
begin
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).GIS_Viewer := _GIS;
end;

procedure TGIS_ControlPrintPreviewSimple.Preview;
var
  scale : Double;
begin
  scale := 0;
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).Preview( nil, '', scale );
end;

procedure TGIS_ControlPrintPreviewSimple.Preview(
  var _scale : Double
);
begin
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).Preview( nil, '', _scale );
end;

procedure TGIS_ControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract
);
var
  scale : Double;
begin
  scale := 0;
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).Preview( _printManager, '', scale );
end;

procedure TGIS_ControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract;
  var   _scale        : Double
);
begin
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).Preview( _printManager, '', _scale );
end;

procedure TGIS_ControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract;
  const _customPage   : String;
  var   _scale        : Double
);
begin
  GisControlPrintPreviewSimple.TGIS_ControlPrintPreviewSimple( oForm ).Preview( _printManager, _customPage, _scale );
end;

procedure TGIS_ControlPrintPreviewSimple.doDestroy;
begin
  FreeObject( oForm ) ;
  inherited ;
end;

{$ENDREGION 'T_PvlControlPrintPreviewSimple'}

{==================================== END =====================================}
end.


