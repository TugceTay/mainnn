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

}

{$IFDEF DCC}
  unit GisConfig ;
  {$HPPEMIT '#pragma link "GisConfig"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

interface

{$INCLUDE GisInclude.inc}

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    {$IFDEF LEVEL_XE2_RTL}
      System.IOUtils,
    {$ENDIF}

    GisRtl,
    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisSymbol;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Encapsulation of layer configuration file.
  /// </summary>
  TGIS_Config = {$IFDEF OXYGENE} public abstract {$ENDIF}
                class( TGIS_ConfigAbstract )

    // property internal values
    private
      /// <summary>
      ///   Current section name
      /// </summary>
      FSection         : String      ;

      /// <summary>
      ///   If True (default) all paths will be save as relative to a config file
      ///   file localization.
      /// </summary>
      FUseRelativePath : Boolean     ;

      /// <summary>
      ///   If True (default) then all config keys will be saved, otherwise
      ///   only changed.
      /// </summary>
      FWriteFull       : Boolean     ;

    // property internal values
    protected

      /// <summary>
      ///   Config version number.
      /// </summary>
      FVersion         : Integer ;

      /// <summary>
      ///   True if config should be saved.
      /// </summary>
      FMustSave        : Boolean     ;

      /// <summary>
      ///   Path for config file.
      /// </summary>
      FConfigPath      : String      ;

      /// <summary>
      ///   Actual config format.
      /// </summary>
      FConfigFormat    : TGIS_ConfigFormat ;

    // other access functions
    protected

      /// <summary>
      ///   Read configuration parameter from list prepared by buildParamList.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  readParam           ( const _name     : String
                                    ) : String ; virtual; abstract;

      /// <summary>
      ///   Read custom configuration parameters.
      /// </summary>
      /// <param name="_name">
      ///   name of custom section
      /// </param>
      /// <param name="_list">
      ///   list of parameters in a  form of "name=value"
      /// </param>
      procedure readCustomParam     ( const _name     : String ;
                                      const _list     : TGIS_Strings
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Write configuration parameter to the configuration file.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure writeParam          ( const _name     : String ;
                                      const _value    : String ;
                                      const _default  : String
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Write custom configuration parameters.
      /// </summary>
      /// <param name="_name">
      ///   name of custom section
      /// </param>
      /// <param name="_list">
      ///   list of parameters in a  form of "name=value"
      /// </param>
      procedure writeCustomParam    ( const _name     : String ;
                                      const _list     : TGIS_Strings
                                    ) ; virtual; abstract;

      function  fget_PrjLayersCount : Integer ; virtual;
      function  fget_PrjLayerName   ( const _index    : Integer
                                    ) : String ; virtual;
      function  fget_PrjLayerPath   ( const _index    : Integer
                                    ) : String ; virtual;
      function  fget_PrjLayerConfig ( const _index    : Integer
                                    ) : String ; virtual;
      function  fget_FileName       : String ; virtual;
      function  fget_IsProject      : Boolean ; virtual;
      function  fget_IsShapeStyle   : Boolean ; virtual;
      function  fget_ConfigFormat   : TGIS_ConfigFormat ;

      procedure fset_Section        ( const _section : String
                                    ) ; virtual ;

      /// <summary>
      ///   Clear zones list.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      procedure clearZones          ( const _name     : String
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Read zones to list.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      /// <param name="_list">
      ///   list to be filled
      /// </param>
      procedure readZones           ( const _name     : String ;
                                      const _list     : TGIS_StringList
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Write zones from list.
      /// </summary>
      /// <param name="_name">
      ///   zone name
      /// </param>
      /// <param name="_list">
      ///   zone values list
      /// </param>
      /// <param name="_default">
      ///   zone default values list
      /// </param>
      procedure writeZones          ( const _name     : String ;
                                      const _list     : TGIS_StringList ;
                                      const _default  : TGIS_StringList
                                    ) ; virtual; abstract;

    protected
      procedure  doDestroy          ; override;
    public

      /// <summary>
      ///   Create instance.
      /// </summary>
      /// <param name="_layer">
      ///   pointer to layer object
      /// </param>
      /// <param name="_path">
      ///   full path to project configuration file; can be empty; if so then
      ///   only layer dependent files are to be used
      /// </param>
      constructor Create            ( const _layer    : TObject ;
                                      const _path     : String
                                    ) ; virtual;

      /// <summary>
      ///   Reread configuration file.
      /// </summary>
      procedure   Reread            ; virtual; abstract;

      /// <summary>
      ///   Save backup copy of configuration file.
      /// </summary>
      procedure   Save              ; virtual; abstract;

      /// <summary>
      ///   Lock configuration file. If config file has been locked by Lock() then
      ///   upon  calling Unlock() a real value of MustSave will be evaluated.
      ///   Without  Lock..Unlock value of MustSave will be set based on a any
      ///   write request  regardless if write really modified the
      ///   configuration file or not.
      /// </summary>
      procedure   Lock              ; virtual; abstract;

      /// <summary>
      ///   Unlock configuration file. If config file has been locked by Lock()
      ///   then upon  calling Unlock() a real value of MustSave will be
      ///   evaluated. Without  Lock..Unlock value of MustSave will be set
      ///   based on a any write request  regardless if write really modified
      ///   the configuration file or not.
      /// </summary>
      procedure   Unlock            ; virtual; abstract;

      /// <summary>
      ///   Select layer on which the operation will be performed. It is
      ///   important for project files. Thanks this it is possible to
      ///   recognize which entry corespondents to which layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer on which the operation will be performed; it is nil then
      ///   operation will be performed on main entry ([TatukGIS] section)
      /// </param>
      procedure SetLayer            ( const _layer    : TObject
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Select sublayer on which the operation will be performed. It is
      ///   important for project files. Thanks this it is possible to
      ///   recognize which entry corespondents to which layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer on which the operation will be performed; it is nil then
      ///   operation will be performed on main entry ([TatukGIS] section)
      /// </param>
      procedure SetSubLayer         ( const _layer    : TObject
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Add sublayer to config file.
      /// </summary>
      /// <param name="_layer">
      ///   sublayer handle
      /// </param>
      /// <param name="_layerno">
      ///   layer index
      /// </param>
      /// <param name="_sublayer">
      ///   sublayer index
      /// </param>
      procedure AddSubLayer         ( const _layer    : TObject ;
                                      const _layerno  : Integer ;
                                      const _sublayer : Integer
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Select layer on which the operation will be performed. It is
      ///   important for project files. Thanks this it is possible to
      ///   recognize which entry corespondents to which layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer on which the operation will be performed; it is nil then
      ///   operation will be performed on main entry ([TatukGIS] section)
      /// </param>
      procedure SetGroup            ( const _layer    : TObject
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Select the section on which the operation will be performed.
      /// </summary>
      /// <param name="_index">
      ///   section number; 0 means default section (first entry)
      /// </param>
      /// <param name="_force">
      ///   if True, then a section will be created if not exist
      /// </param>
      /// <returns>
      ///   True if section exists
      /// </returns>
      function  SetSection          ( const _index    : Integer ;
                                      const _force    : Boolean
                                    ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Select the subsection on which the operation will be performed.
      /// </summary>
      /// <param name="_index">
      ///   subsection number; 0 means default section (first entry)
      /// </param>
      /// <param name="_force">
      ///   if True, then a subsection will be created if not exist
      /// </param>
      /// <returns>
      ///   True if subsection exists
      /// </returns>
      function  SetSubSection       ( const _index    : Integer ;
                                      const _force    : Boolean
                                    ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Select the group section on which the operation will be performed.
      /// </summary>
      /// <param name="_index">
      ///   group section number; 0 means default section (first entry)
      /// </param>
      /// <returns>
      ///   True if group section exists
      /// </returns>
      function  SetGroupSection     ( const _index    : Integer
                                    ) : Boolean ; virtual; abstract;

      /// <summary>
      ///   Delete sections related to the selected layer.
      /// </summary>
      procedure ClearSections       ; virtual; abstract;

      /// <summary>
      ///   Delete subsections related to the selected layer.
      /// </summary>
      procedure ClearSubSections    ; virtual; abstract;

      /// <summary>
      ///   Delete group sections related to the selected group.
      /// </summary>
      procedure ClearGroups         ; virtual; abstract;

      /// <summary>
      ///   Delete elements related to the current section.
      /// </summary>
      procedure ClearActiveSection         ; virtual; abstract;

      /// <summary>
      ///     Read section values into a list.
      /// </summary>
      /// <param name="_name">
      ///   section name
      /// </param>
      /// <param name="_list">
      ///   list to fill
      /// </param>
      procedure ReadSectionValues   ( const _name     : String ;
                                      const _list     : TGIS_Strings
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadAlignment       ( const _name     : String ;
                                      const _default  : TGIS_LabelAlignment
                                    ) : TGIS_LabelAlignment ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteAlignment      ( const _name     : String ;
                                      const _value    : TGIS_LabelAlignment ;
                                      const _default  : TGIS_LabelAlignment
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadBitmap          ( const _name     : String ;
                                      const _default  : TGIS_Bitmap
                                    ) : TGIS_Bitmap ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteBitmap         ( const _name     : String ;
                                      const _value    : TGIS_Bitmap ;
                                      const _default  : TGIS_Bitmap
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadBoolean         ( const _name     : String ;
                                      const _default  : Boolean
                                    ) : Boolean ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteBoolean       ( const _name      : String ;
                                     const _value     : Boolean ;
                                     const _default   : Boolean
                                   ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadColor           ( const _name     : String ;
                                      const _default  : TGIS_Color
                                    ) : TGIS_Color ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteColor          ( const _name     : String ;
                                      const _value    : TGIS_Color ;
                                      const _default  : TGIS_Color
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadDormant         ( const _name     : String ;
                                      const _default  : TGIS_LayerDormantMode
                                    ) : TGIS_LayerDormantMode ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteDormant        ( const _name     : String ;
                                      const _value    : TGIS_LayerDormantMode ;
                                      const _default  : TGIS_LayerDormantMode
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadFontStyle       ( const _name     : String ;
                                      const _default  : TGIS_FontStyles
                                    ) : TGIS_FontStyles ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteFontStyle      ( const _name     : String ;
                                      const _value    : TGIS_FontStyles ;
                                      const _default  : TGIS_FontStyles
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadInteger         ( const _name     : String ;
                                      const _default  : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteInteger        ( const _name     : String ;
                                      const _value    : Integer ;
                                      const _default  : Integer
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadMarker          ( const _name     : String ;
                                      const _default  : TGIS_MarkerStyle
                                    ) : TGIS_MarkerStyle ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteMarker         ( const _name     : String ;
                                      const _value    : TGIS_MarkerStyle ;
                                      const _default  : TGIS_MarkerStyle
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadPattern         ( const _name     : String ;
                                      const _default  : TGIS_BrushStyle
                                    ) : TGIS_BrushStyle ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WritePattern        ( const _name     : String ;
                                      const _value    : TGIS_BrushStyle ;
                                      const _default  : TGIS_BrushStyle
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadSymbol          ( const _name     : String ;
                                      const _default  : TGIS_SymbolAbstract
                                    ) : TGIS_SymbolAbstract ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteSymbol         ( const _name     : String ;
                                      const _value    : TGIS_SymbolAbstract ;
                                      const _default  : TGIS_SymbolAbstract
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadPen             ( const _name     : String ;
                                      const _default  : TGIS_PenStyle
                                    ) : TGIS_PenStyle ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WritePen            ( const _name     : String ;
                                      const _value    : TGIS_PenStyle ;
                                      const _default  : TGIS_PenStyle
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadChart           ( const _name     : String ;
                                      const _default  : TGIS_ChartStyle
                                    ) : TGIS_ChartStyle ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteChart          ( const _name     : String ;
                                      const _value    : TGIS_ChartStyle ;
                                      const _default  : TGIS_ChartStyle
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadPosition        ( const _name     : String ;
                                      const _default  : TGIS_LabelPositions
                                    ) : TGIS_LabelPositions ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WritePosition       ( const _name     : String ;
                                      const _value    : TGIS_LabelPositions ;
                                      const _default  : TGIS_LabelPositions
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadFloat           ( const _name     : String ;
                                      const _default  : Double
                                    ) : Double ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteFloat          ( const _name     : String ;
                                      const _value    : Double ;
                                      const _default  : Double
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadString          ( const _name     : String ;
                                      const _default  : String
                                    ) : String ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadColorInterpolationMode(
                                const _name     : String ;
                                const _default  : TGIS_ColorInterpolationMode
                              ) : TGIS_ColorInterpolationMode ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteColorInterpolationMode(
                                const _name     : String ;
                                const _value    : TGIS_ColorInterpolationMode ;
                                const _default  : TGIS_ColorInterpolationMode
                              ) ;


      /// <summary>
      ///   Read custom configuration parameters.
      /// </summary>
      /// <param name="_name">
      ///   name of custom section
      /// </param>
      /// <param name="_list">
      ///   list of parameters in a form of "name=value"
      /// </param>
      procedure ReadCustomData      ( const _name     : String ;
                                      const _list     : TGIS_Strings
                                    ) ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteString         ( const _name     : String ;
                                      const _value    : String ;
                                      const _default  : String
                                    ) ;

      /// <summary>
      ///   Write parameters given by _name. File based parameters
      ///   will be saved as relative to config path.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteStyle          ( const _name     : String ;
                                      const _value    : String ;
                                      const _default  : String
                                    ) ;

      /// <summary>
      ///   Write custom configuration parameters.
      /// </summary>
      /// <param name="_name">
      ///   name of custom section
      /// </param>
      /// <param name="_list">
      ///   list of parameters in a  form of "name=value"
      /// </param>
      procedure WriteCustomData     ( const _name     : String ;
                                      const _list     : TGIS_Strings
                                    ) ;


      /// <summary>
      ///   Read parameters given by zone _name. If Parameters do not exist or
      ///   are invalid then empty list will be returned.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_list">
      ///   parameters in internal format
      /// </param>
      procedure ReadZone            ( const _name     : String ;
                                      const _list     : TGIS_StringList
                                    ) ;

      /// <summary>
      ///   Write parameters given by zone _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_list">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteZone           ( const _name     : String ;
                                      const _list     : TGIS_StringList ;
                                      const _default  : TGIS_StringList
                                    ) ;

      /// <summary>
      ///   Read color ramp parameter given by _name. If Parameter do not exist or
      ///   are invalid then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      /// <returns>
      ///   loaded color ramp
      /// </returns>
      function ReadColorRamp        ( const _name    : String ;
                                      const _default : TGIS_ColorMapArray
                                    ) : TGIS_ColorMapArray ;

      /// <summary>
      ///   Write color ramp parameter given by _name. If Parameter do not exist
      ///   or are invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_ramp">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteColorRamp      ( const _name     : String ;
                                      const _ramp     : TGIS_ColorMapArray;
                                      const _default  : TGIS_ColorMapArray
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadGround          ( const _name     : String ;
                                      const _default  : TGIS_3DGroundType
                                    ) : TGIS_3DGroundType ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteGround         ( const _name     : String ;
                                      const _value    : TGIS_3DGroundType ;
                                      const _default  : TGIS_3DGroundType
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadNormalized      ( const _name     : String ;
                                      const _default  : TGIS_3DNormalizationType
                                    ) : TGIS_3DNormalizationType ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteNormalized     ( const _name     : String ;
                                      const _value    : TGIS_3DNormalizationType ;
                                      const _default  : TGIS_3DNormalizationType
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadBasement        ( const _name     : String ;
                                      const _default  : TGIS_3DBasementType
                                    ) : TGIS_3DBasementType ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteBasement       ( const _name     : String ;
                                      const _value    : TGIS_3DBasementType ;
                                      const _default  : TGIS_3DBasementType
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  Read3DLayerType     ( const _name     : String ;
                                      const _default  : TGIS_3DLayerType
                                    ) : TGIS_3DLayerType ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure Write3DLayerType    ( const _name     : String ;
                                      const _value    : TGIS_3DLayerType ;
                                      const _default  : TGIS_3DLayerType
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadInterpretation  ( const _name     : String ;
                                      const _default  : TGIS_LayerPixelInterpretation
                                    ) : TGIS_LayerPixelInterpretation ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteInterpretation ( const _name     : String ;
                                      const _value    : TGIS_LayerPixelInterpretation ;
                                      const _default  : TGIS_LayerPixelInterpretation
                                    ) ;

      /// <summary>
      ///   Read parameters given by _name. If Parameters do not exist or are
      ///   invalid, then _default value will be used.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_default">
      ///   default value
      /// </param>
      /// <returns>
      ///   parameter value
      /// </returns>
      function  ReadOffsetPosition  ( const _name     : String ;
                                      const _default  : TGIS_OffsetPosition
                                    ) : TGIS_OffsetPosition ;

      /// <summary>
      ///   Write parameters given by _name.
      /// </summary>
      /// <param name="_name">
      ///   name of value
      /// </param>
      /// <param name="_value">
      ///   value of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter
      /// </param>
      procedure WriteOffsetPosition ( const _name     : String ;
                                      const _value    : TGIS_OffsetPosition ;
                                      const _default  : TGIS_OffsetPosition
                                    ) ;

      /// <summary>
      ///   Build project file based on current project and list of layers
      ///   attached to the Viewer.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer for which project will be constructed
      /// </param>
      procedure BuildProject        ( const _viewer   : IGIS_Viewer
                                    ) ; virtual;

      /// <summary>
      ///   Read hierarchy groups to config.
      /// </summary>
      /// <returns>
      ///   list of lines or nodes
      /// </returns>
      function ReadHierarchyGroups : TObject ; virtual;

      /// <summary>
      ///   Write hierarchy groups to config.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer for which project will be constructed
      /// </param>
      procedure WriteHierarchyGroups( const _viewer   : IGIS_Viewer
                                    ) ; virtual;

      /// <summary>
      ///   Read and parse values from list.
      /// </summary>
      /// <param name="_list">
      ///   string list
      /// </param>
      procedure SetStrings          ( _list           : TGIS_Strings
                                    ) ; virtual;

      /// <summary>
      ///   Parse and fill list with values.
      /// </summary>
      /// <param name="_list">
      ///   list to fill
      /// </param>
      procedure GetStrings          ( _list           : TGIS_Strings
                                    ) ; virtual;

      /// <summary>
      ///     Clear MustSave flag.
      /// </summary>
      procedure ClearSave ;

      /// <summary>
      ///   Computes path relative to configuration file.
      /// </summary>
      /// <param name="_path">
      ///   path to be relative
      /// </param>
      /// <returns>
      ///   relative path
      /// </returns>
      function  RelativePath        ( const _path     : String
                                    ) : String ;

      /// <summary>
      ///   Computes absolute path for path relative to configuration file.
      /// </summary>
      /// <param name="_path">
      ///   relative path
      /// </param>
      /// <returns>
      ///   absolute path
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     if resulting path is pointing to non existing path then result
      ///     will be same as _path
      ///   </note>
      /// </remarks>
      function  AbsolutePath        ( const _path     : String
                                    ) : String ;

    public
      /// <summary>
      ///   Config version number
      /// </summary>
      /// <remarks>
      ///   Version format is provided as MAJOR*1000 + MINOR so
      ///   (11002) means MAJOR=11 minor=2
      /// </remarks>
      property Version
        : Integer
        read FVersion ;

      /// <summary>
      ///   True if config file was changed.
      /// </summary>
      property MustSave
        : Boolean
        read FMustSave ;

      /// <summary>
      ///   By assigning this value you are overriding default TGIS_Config
      ///   behavior. The read/write will be performed based on provide
      ///   section.
      /// </summary>
      property Section
        : String
        read  FSection
        write fset_Section ;

      /// <summary>
      ///   If True (default) all paths will be save as relative to a config
      ///   file localization.
      /// </summary>
      property UseRelativePath
        : Boolean
        read  FUseRelativePath
        write FUseRelativePath;

      /// <summary>
      ///   If True (default) then all config keys will be saved, otherwise
      ///   only changed.
      /// </summary>
      property WriteFull
        : Boolean
        read  FWriteFull
        write FWriteFull ;

      /// <summary>
      ///   Returns the number of layers defined in project config file.
      /// </summary>
      property PrjLayersCount
        : Integer
        read fget_PrjLayersCount ;

      /// <summary>
      ///   Name of layer at a given index in a project file.
      /// </summary>
      /// <param name="_index">
      ///   index of layer
      /// </param>
      property PrjLayerName[const _index : Integer]
        : String
        read fget_PrjLayerName ;

      /// <summary>
      ///   Path of layer at a given index in a project file.
      /// </summary>
      /// <param name="_index">
      ///   index of layer
      /// </param>
      property PrjLayerPath[const _index : Integer]
        : String
        read fget_PrjLayerPath ;

      /// <summary>
      ///   Config file name (without extension) at a given index in a project file.
      /// </summary>
      /// <param name="_index">
      ///   index of layer
      /// </param>
      property PrjLayerConfig[const _index : Integer]
        : String
        read fget_PrjLayerConfig ;

      /// <summary>
      ///   Name of configuration file.
      /// </summary>
      property FileName
        : String
        read fget_FileName ;

      /// <summary>
      ///   True if config file is a project.
      /// </summary>
      property IsProject
        : Boolean
        read fget_IsProject ;

      /// <summary>
      ///   True if config is for shape style embedding.
      /// </summary>
      property IsShapeStyle
        : Boolean
        read  fget_IsShapeStyle ;

      /// <summary>
      ///   Internal format of config.
      /// </summary>
      property ConfigFormat
        : TGIS_ConfigFormat
        read fget_ConfigFormat ;
  end ;


  /// <summary>
  ///   Encapsulation of configuration file factory.
  /// </summary>
  TGIS_ConfigFactory = {$IFDEF OXYGENE} public static {$ENDIF} class
    public

      /// <summary>
      ///   Create a config instance based on a path extension.
      /// </summary>
      /// <param name="_layer">
      ///   pointer to layer object
      /// </param>
      /// <param name="_path">
      ///   full path to project configuration file; can be empty; if so then
      ///   only layer dependent files are to be used
      /// </param>
      /// <returns>
      ///   new config instance
      /// </returns>
      class function CreateConfig   ( const _layer    : TObject ;
                                      const _path     : String
                                    ) : TGIS_Config   ; static ;

      /// <summary>
      ///   Check if a config is a project.
      /// </summary>
      /// <param name="_path">
      ///   full path to project configuration file; can be empty; if so then
      ///   only layer dependent files are to be used
      /// </param>
      /// <returns>
      ///   new config instance
      /// </returns>
      class function IsProject      ( const _path     : String
                                    ) : Boolean ; static ;
  end ;

const
   {#gendoc:hide}
   METADATA_CONFIGFACTORY_ALIASES_TTKPROJECT
     = 'TGIS_ConfigFactory.Aliases.ttkproject';

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses,
    GisConfigIni,
    GisConfigXml,
    GisInternals,
    GisParams,
    GisFunctions,
    GisResource;
{$ENDIF}

//==============================================================================
// TGIS_Config
//==============================================================================

  constructor TGIS_Config.Create(
    const _layer  : TObject  ;
    const _path   : String
  ) ;
  begin
    inherited Create ;

    FVersion    := 0 ;

    FUseRelativePath := True ;
    FWriteFull       := False ;

    FConfigPath := GetPathAbsolute( '', _path ) ;
    FSection    := '' ;
    FConfigFormat := TGIS_ConfigFormat.Unknown ;
  end ;

  procedure TGIS_Config.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_Config.GetStrings(
    _list : TGIS_Strings
  ) ;
  begin

  end ;

  procedure TGIS_Config.SetStrings(
    _list : TGIS_Strings
  ) ;
  begin

  end ;

  function TGIS_Config.ReadAlignment(
    const _name    : String ;
    const _default : TGIS_LabelAlignment
  ) : TGIS_LabelAlignment ;
  begin
    Result := ParamAlignment( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteAlignment(
    const _name    : String ;
    const _value   : TGIS_LabelAlignment ;
    const _default : TGIS_LabelAlignment
  ) ;
  begin
    writeParam( _name,
                ConstructParamAlignment( _value ),
                ConstructParamAlignment( _default )
               ) ;
  end ;

  function TGIS_Config.ReadBitmap(
    const _name    : String ;
    const _default : TGIS_Bitmap
  ) : TGIS_Bitmap ;
  begin
    Result := ParamBitmap( AbsolutePath( readParam( _name ) ), _default ) ;
  end ;

  procedure TGIS_Config.WriteBitmap(
    const _name    : String      ;
    const _value   : TGIS_Bitmap ;
    const _default : TGIS_Bitmap
  ) ;
  begin
    if assigned( _value ) and not IsStringEmpty( _value.Path ) then begin
      if assigned( _default ) and not IsStringEmpty( _default.Path ) then
        writeParam( _name,
                    RelativePath(  _value.Path ),
                    RelativePath(  _default.Path )
                  )
      else
        writeParam( _name,
                    RelativePath( _value.Path ),
                    ''
                  ) ;
    end
    else if assigned( _default ) and not IsStringEmpty( _default.Path ) then
      writeParam( _name,
                  '',
                  RelativePath( _default.Path )
                )
    else
      writeParam( _name,
                  '',
                  ''
                ) ;
  end ;

  function TGIS_Config.ReadBoolean(
    const _name    : String ;
    const _default : Boolean
  ) : Boolean ;
  begin
    Result := ParamBoolean( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteBoolean(
    const _name    : String ;
    const _value   : Boolean;
    const _default : Boolean
  ) ;
  begin
    writeParam( _name,
                ConstructParamBoolean( _value ),
                ConstructParamBoolean( _default )
              ) ;
  end ;

  function TGIS_Config.ReadColor(
    const _name    : String ;
    const _default : TGIS_Color
  ) : TGIS_Color ;
  begin
    Result := ParamColor( readParam( _name ), _default ) ;
    if Result.ARGB = $FF010101 then
      Result.ARGB := $FF000000 ;
  end ;

  procedure TGIS_Config.WriteColor(
    const _name    : String ;
    const _value   : TGIS_Color ;
    const _default : TGIS_Color
  ) ;
  var
    cl1, cl2 : TGIS_Color ;
  begin
    cl1 := _value ;
    if cl1.ARGB = $FF000000 then cl1.ARGB := $FF010101 ;

    cl2 := _default ;
    if cl2.ARGB = $FF000000 then cl2.ARGB := $FF010101 ;

    writeParam( _name,
                ConstructParamColor( cl1 ),
                ConstructParamColor( cl2  )
              ) ;
  end ;

  function TGIS_Config.ReadDormant(
    const _name    : String ;
    const _default : TGIS_LayerDormantMode
  ) : TGIS_LayerDormantMode ;
  begin
    Result := ParamDormant( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteDormant(
    const _name    : String ;
    const _value   : TGIS_LayerDormantMode ;
    const _default : TGIS_LayerDormantMode
  ) ;
  begin
    writeParam( _name,
                ConstructParamDormant( _value ),
                ConstructParamDormant( _default )
              ) ;
  end ;

  function TGIS_Config.ReadFontStyle(
    const _name    : String ;
    const _default : TGIS_FontStyles
 ) : TGIS_FontStyles ;
  begin
    Result := ParamFontStyle( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteFontStyle(
    const _name    : String ;
    const _value   : TGIS_FontStyles ;
    const _default : TGIS_FontStyles
  ) ;
  begin
    writeParam( _name,
                ConstructParamFontStyle( _value ),
                ConstructParamFontStyle( _default )
              ) ;
  end ;

  function TGIS_Config.ReadInteger(
    const _name    : String ;
    const _default : Integer
  ) : Integer ;
  begin
    Result := ParamInteger( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteInteger(
    const _name    : String ;
    const _value   : Integer;
    const _default : Integer
  ) ;
  begin
    writeParam( _name,
                ConstructParamInteger( _value ),
                ConstructParamInteger( _default )
              ) ;
  end ;

  function TGIS_Config.ReadPosition(
    const _name    : String ;
    const _default : TGIS_LabelPositions
  ) : TGIS_LabelPositions ;
  begin
    Result := ParamPosition( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WritePosition(
    const _name    : String ;
    const _value   : TGIS_LabelPositions ;
    const _default : TGIS_LabelPositions
  ) ;
  begin
    writeParam( _name,
                ConstructParamPosition( _value ),
                ConstructParamPosition( _default )
              ) ;
  end ;

  function TGIS_Config.ReadPattern(
    const _name    : String ;
    const _default : TGIS_BrushStyle
  ) : TGIS_BrushStyle ;
  begin
    Result := ParamPattern( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WritePattern(
    const _name    : String ;
    const _value   : TGIS_BrushStyle ;
    const _default : TGIS_BrushStyle
  ) ;
  begin
    writeParam( _name,
                ConstructParamPattern( _value ),
                ConstructParamPattern( _default )
              ) ;
  end ;

  function TGIS_Config.ReadSymbol(
    const _name    : String ;
    const _default : TGIS_SymbolAbstract
  ) : TGIS_SymbolAbstract ;
  var
    str : String ;
  begin
    str := readParam( _name ) ;
    if not IsStringEmpty( GetFileExt( str ) ) then
      str := AbsolutePath( str ) ;
    Result := ParamSymbol( str, _default ) ;
  end ;

  procedure TGIS_Config.WriteSymbol(
    const _name    : String ;
    const _value   : TGIS_SymbolAbstract ;
    const _default : TGIS_SymbolAbstract
  ) ;
  begin
    if assigned( _value ) then begin
      if _value.IsFileBased then begin
        if assigned( _default ) then
          writeParam( _name,
                      RelativePath( _value.Name ),
                      RelativePath( _default.Name )
                    )
        else
          writeParam( _name,
                      RelativePath( _value.Name ),
                      ''
                    )
      end
      else
        if assigned( _default ) then
          writeParam( _name,
                      _value.Name,
                      _default.Name
                    )
        else
          writeParam( _name,
                      _value.Name,
                      ''
                    ) ;
    end
    else if assigned( _default ) then
      writeParam( _name,
                  '',
                  _default.Name
                )
    else
      writeParam( _name,
                  '',
                  ''
                ) ;
  end ;

  function TGIS_Config.ReadPen(
    const _name    : String ;
    const _default : TGIS_PenStyle
  ) : TGIS_PenStyle ;
  begin
    Result := ParamPen( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WritePen(
    const _name    : String ;
    const _value   : TGIS_PenStyle ;
    const _default : TGIS_PenStyle
  ) ;
  begin
    writeParam( _name,
                ConstructParamPen( _value ),
                ConstructParamPen( _default )
              ) ;
  end ;

  function TGIS_Config.ReadChart(
    const _name    : String ;
    const _default : TGIS_ChartStyle
  ) : TGIS_ChartStyle ;
  begin
    Result := ParamChart( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteChart(
    const _name    : String ;
    const _value   : TGIS_ChartStyle ;
    const _default : TGIS_ChartStyle
  ) ;
  begin
    writeParam( _name,
                ConstructParamChart( _value ),
                ConstructParamChart( _default )
              ) ;
  end ;

  function TGIS_Config.ReadMarker(
    const _name    : String ;
    const _default : TGIS_MarkerStyle
  ) : TGIS_MarkerStyle ;
  begin
    Result := ParamMarker( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteMarker(
    const _name    : String ;
    const _value   : TGIS_MarkerStyle ;
    const _default : TGIS_MarkerStyle
  ) ;
  begin
    writeParam( _name,
                ConstructParamMarker( _value ),
                ConstructParamMarker( _default )
              ) ;
  end ;

  function TGIS_Config.ReadFloat(
    const _name    : String ;
    const _default : Double
  ) : Double ;
  begin
    Result := ParamFloat( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteFloat(
    const _name    : String ;
    const _value   : Double ;
    const _default : Double
  ) ;
  begin
    writeParam( _name,
                ConstructParamFloat( _value ),
                ConstructParamFloat( _default )
              ) ;
  end ;

  function TGIS_Config.ReadString(
    const _name    : String ;
    const _default : String
  ) : String ;
  begin
    Result := readParam( _name ) ;
    if Result = GIS_PARAM_NIL then
      Result := _default ;
  end ;

  procedure TGIS_Config.WriteString(
    const _name     : String ;
    const _value    : String ;
    const _default  : String
  ) ;
  begin
    writeParam( _name, _value, _default ) ;
  end ;

  procedure TGIS_Config.ReadCustomData(
    const _name    : String ;
    const _list    : TGIS_Strings
  ) ;
  begin
    readCustomParam( _name, _list ) ;
  end ;

  procedure TGIS_Config.WriteStyle(
    const _name     : String ;
    const _value    : String ;
    const _default  : String
  ) ;
  var
    k : Integer ;
    tmp : String ;
    stype  : String ;
    svalue : String ;
  begin
    stype  := '' ;
    svalue := '' ;

    k := Pos( ':', _value ) ;
    if k < StringFirst then begin
      tmp := _value ;
    end
    else begin
      stype  := UpperCase( Copy( _value, StringFirst, k-StringFirst  ) ) ;
      svalue := Copy( _value, k+1, 4096  ) ;
      if SafeFileExists( svalue ) then
        svalue := RelativePath( svalue ) ;
      tmp := stype + ':' + svalue ;
    end;

    writeParam( _name, tmp, _default ) ;
  end ;

  procedure TGIS_Config.WriteCustomData(
    const _name     : String ;
    const _list     : TGIS_Strings
  ) ;
  begin
    writeCustomParam( _name, _list ) ;
  end ;

  procedure TGIS_Config.ReadZone(
    const _name : String ;
    const _list : TGIS_StringList
  ) ;
  begin
    assert( assigned(_list), '_list must be assigned' ) ;

    readZones( _name, _list ) ;
  end ;

  procedure TGIS_Config.WriteZone(
    const _name     : String ;
    const _list     : TGIS_StringList ;
    const _default  : TGIS_StringList
  ) ;
  begin
    assert( assigned(_list), '_list must be assigned' ) ;

    clearZones( _name ) ;
    writeZones( _name, _list, _default ) ;
  end ;

  function TGIS_Config.ReadColorInterpolationMode(
    const _name    : String ;
    const _default : TGIS_ColorInterpolationMode
  ) : TGIS_ColorInterpolationMode ;
  var
    resultStr : String;
  begin
    resultStr:= readParam( _name ) ;
    if resultStr = GIS_COLORSPACE_RGB then
      Result := TGIS_ColorInterpolationMode.RGB
    else if resultStr = GIS_COLORSPACE_HSL then
      Result := TGIS_ColorInterpolationMode.HSL
    else if resultStr = GIS_COLORSPACE_HSL360 then
      Result := TGIS_ColorInterpolationMode.HSL360
    else
      Result := _default ;
  end ;

  procedure TGIS_Config.WriteColorInterpolationMode(
    const _name     : String ;
    const _value    : TGIS_ColorInterpolationMode ;
    const _default  : TGIS_ColorInterpolationMode
  ) ;
  var
    valueStr : String ;
    defaultStr : String ;
  begin
    case _value of
      TGIS_ColorInterpolationMode.RGB    : valueStr := GIS_COLORSPACE_RGB ;
      TGIS_ColorInterpolationMode.HSL    : valueStr := GIS_COLORSPACE_HSL ;
      TGIS_ColorInterpolationMode.HSL360 : valueStr := GIS_COLORSPACE_HSL360 ;
    else
      exit;
    end;
    case _default of
      TGIS_ColorInterpolationMode.RGB    : defaultStr := GIS_COLORSPACE_RGB ;
      TGIS_ColorInterpolationMode.HSL    : defaultStr := GIS_COLORSPACE_HSL ;
      TGIS_ColorInterpolationMode.HSL360 : valueStr := GIS_COLORSPACE_HSL360 ;
    else
      defaultStr := GIS_COLORSPACE_RGB;
    end;
    writeParam( _name, valueStr, defaultStr ) ;
  end ;

  function TGIS_Config.ReadColorRamp(
    const _name    : String ;
    const _default : TGIS_ColorMapArray
  ) : TGIS_ColorMapArray ;
  var
    i_zone           : Integer ;
    zone             : String ;
    colormap         : TGIS_ColorMap ;
    tkn              : TGIS_Tokenizer ;
    zones_list       : TGIS_StringList ;
    zones_count      : Integer ;
    zones_count_real : Integer ;
  begin
    Result := _default ;
    zones_list := TStringList.Create ;
    try
      readZones( _name, zones_list ) ;

      zones_count := zones_list.Count ;
      if zones_count = 0 then
        exit ;

      zones_count_real := zones_count ;
      SetLength( Result, zones_count ) ;

      for i_zone := 0 to zones_count - 1 do begin
        zone := zones_list[i_zone] ;

        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.ExecuteEx( zone, GIS_PARAM_SEPARATOR ) ;

          if tkn.Result.Count <> 2 then begin
            dec( zones_count_real ) ;
            Continue ;
          end ;

          {$IFDEF GIS_NORECORDS}
            colormap := new TGIS_ColorMap ;
          {$ENDIF}
          colormap.Index := DotStrToFloat( tkn.Result[0] ) ;
          colormap.RGB := ParamColor( tkn.Result[1], TGIS_Color.None ) ;
        finally
          FreeObject( tkn ) ;
        end;

        Result[i_zone] := colormap ;
      end ;
    finally
      FreeObject( zones_list ) ;
    end ;

    if zones_count_real < zones_count then
      SetLength( Result, zones_count_real ) ;
  end ;

  procedure TGIS_Config.WriteColorRamp(
    const _name    : String ;
    const _ramp    : TGIS_ColorMapArray ;
    const _default : TGIS_ColorMapArray
  ) ;
  var
    zone_list : TStringList ;
    default_list : TStringList ;

    procedure color_ramp_to_list(
      const _ramp : TGIS_ColorMapArray ;
      const _list : TStringList
    ) ;
    var
      i_zone    : Integer ;
      calormap  : TGIS_ColorMap ;
      ramp_zone : String ;
    begin
      for i_zone := 0 to length( _ramp ) - 1 do begin
        calormap := _ramp[i_zone] ;

        ramp_zone := Format(
          '%s%s%s',
          [ConstructParamFloat( calormap.Index ),
          GIS_PARAM_SEPARATOR,
          ConstructParamColor( calormap.RGB )]
        ) ;

        _list.Add( ramp_zone ) ;
      end ;
    end;

  begin
    clearZones( _name ) ;

    zone_list := TStringList.Create;
    default_list := TStringList.Create;
    try
      color_ramp_to_list( _ramp, zone_list ) ;
      color_ramp_to_list( _default, default_list ) ;

      writeZones( _name, zone_list, default_list ) ;
    finally
      FreeObject( zone_list ) ;
      FreeObject( default_list ) ;
    end ;
  end;

  function TGIS_Config.ReadGround(
    const _name      : String ;
    const _default   : TGIS_3DGroundType
  ) : TGIS_3DGroundType ;
  begin
    Result := ParamGround( readParam( _name ), _default ) ;
  end;

  procedure TGIS_Config.WriteGround(
    const _name      : String ;
    const _value     : TGIS_3DGroundType ;
    const _default   : TGIS_3DGroundType
  ) ;
  begin
    writeParam( _name,
                ConstructParamGround( _value ),
                ConstructParamGround( _default )
              ) ;
  end;

  function  TGIS_Config.ReadNormalized(
    const _name     : String ;
    const _default  : TGIS_3DNormalizationType
  ) : TGIS_3DNormalizationType ;
  begin
    Result := ParamNormalized( readParam( _name ), _default ) ;
  end;

  procedure TGIS_Config.WriteNormalized(
    const _name      : String ;
    const _value     : TGIS_3DNormalizationType ;
    const _default   : TGIS_3DNormalizationType
  ) ;
  begin
    writeParam( _name,
                ConstructParamNormalized( _value ),
                ConstructParamNormalized( _default )
              ) ;
  end;

  procedure TGIS_Config.WriteHierarchyGroups(
    const _viewer : IGIS_Viewer
  ) ;
  begin

  end ;

  function TGIS_Config.ReadHierarchyGroups : TObject ;
  begin
    Result := nil ;
  end;

  function  TGIS_Config.ReadBasement(
    const _name      : String ;
    const _default   : TGIS_3DBasementType
  ) : TGIS_3DBasementType ;
  begin
    Result := ParamBasement( readParam( _name ), _default ) ;
  end;

  procedure TGIS_Config.WriteBasement(
    const _name      : String ;
    const _value     : TGIS_3DBasementType ;
    const _default   : TGIS_3DBasementType
  ) ;
  begin
    writeParam( _name,
                ConstructParamBasement( _value ),
                ConstructParamBasement( _default )
              ) ;
  end;

  function  TGIS_Config.Read3DLayerType(
    const _name      : String ;
    const _default   : TGIS_3DLayerType
  ) : TGIS_3DLayerType ;
  begin
    Result := Param3DLayerType( readParam( _name ), _default ) ;
  end;

  procedure TGIS_Config.Write3DLayerType(
    const _name      : String ;
    const _value     : TGIS_3DLayerType ;
    const _default   : TGIS_3DLayerType
  ) ;
  begin
    writeParam( _name,
                ConstructParam3DLayerType( _value ),
                ConstructParam3DLayerType( _default )
              ) ;
  end;

  function  TGIS_Config.ReadInterpretation(
    const _name      : String ;
    const _default   : TGIS_LayerPixelInterpretation
  ) : TGIS_LayerPixelInterpretation ;
  begin
    Result := ParamInterpretation( readParam( _name ), _default ) ;
  end;

  procedure TGIS_Config.WriteInterpretation(
    const _name      : String ;
    const _value     : TGIS_LayerPixelInterpretation ;
    const _default   : TGIS_LayerPixelInterpretation
  ) ;
  begin
    writeParam( _name,
                ConstructParamInterpretation( _value ),
                ConstructParamInterpretation( _default )
              ) ;
  end;

  function TGIS_Config.ReadOffsetPosition(
    const _name    : String ;
    const _default : TGIS_OffsetPosition
  ) : TGIS_OffsetPosition ;
  begin
    Result := ParamOffsetPosition( readParam( _name ), _default ) ;
  end ;

  procedure TGIS_Config.WriteOffsetPosition(
    const _name    : String ;
    const _value   : TGIS_OffsetPosition ;
    const _default : TGIS_OffsetPosition
  ) ;
  begin
    writeParam( _name,
                ConstructParamOffsetPosition( _value ),
                ConstructParamOffsetPosition( _default )
              ) ;
  end ;

  procedure TGIS_Config.ClearSave ;
  begin
    FMustSave := False ;
  end;

  function TGIS_Config.RelativePath(
    const _path : String
  ) : String ;
  begin
    if FUseRelativePath then
      Result := GetPathRelative( GetFilePath( FConfigPath ), _path )
    else
      Result := _path ;
  end ;

  function TGIS_Config.AbsolutePath(
    const _path : String
  ) : String ;
  var
    tmp : String  ;
    k   : Integer ;
  begin
    try
      if _path = GIS_PARAM_NIL then begin
        Result := _path ;
        exit ;
      end
      else
        Result := GetPathAbsolute( GetFilePath( FConfigPath ), _path ) ;

      tmp := Result ;

      k := Pos( String( '?' ), Result ) ;
      if k >= StringFirst then
        tmp := Copy( Result, StringFirst, k-StringFirst )
      else
        tmp := Result ;
      if ( not SafeFileExists( tmp ) ) and ( not DirectoryExists( tmp ) ) then
        Result := _path ;
    except
      Result := _path ;
    end ;
  end ;

  function TGIS_Config.fget_PrjLayersCount
    : Integer ;
  begin
    Result := -1 ;
  end ;

  function TGIS_Config.fget_PrjLayerName(
    const _index : Integer
  ) : String ;
  begin
    Result := ''
  end ;

  function TGIS_Config.fget_PrjLayerPath(
    const _index : Integer
  ) : String ;
  begin
    Result := '' ;
  end ;

  function TGIS_Config.fget_FileName : String ;
  begin           ;
    Result := '' ;
  end;

  function TGIS_Config.fget_IsProject : Boolean ;
  begin
    Result := False ;
  end;

  function TGIS_Config.fget_IsShapeStyle : Boolean ;
  begin
    Result := False ;
  end;

  function TGIS_Config.fget_PrjLayerConfig(
    const _index : Integer
  ) : String ;
  begin
    Result := '' ;
  end ;

  function TGIS_Config.fget_ConfigFormat : TGIS_ConfigFormat ;
  begin
    Result := FConfigFormat ;
  end ;

  procedure TGIS_Config.BuildProject(
    const _viewer : IGIS_Viewer
  ) ;
  begin

  end ;

  procedure TGIS_Config.fset_Section(
    const _section : String
  ) ;
  begin
    FSection := _section ;
  end;


//==============================================================================
// TGIS_ConfigFactory
//==============================================================================

  class function TGIS_ConfigFactory.CreateConfig(
    const _layer  : TObject ;
    const _path   : String
  ) : TGIS_Config ;
  var
    ext   : String ;
    path  : String ;
    alias : String ;
  begin
    if IsStringEmpty( _path ) then begin
      path := '' ;
      ext  := '' ;
    end
    else begin
      path := ExpandFileNameEx( _path );
      ext  := GetFileExt( path ) ;
    end ;

    if      CompareText( ext, GIS_INI_EXT  ) = 0 then
      Result := TGIS_ConfigIni.Create( _layer, path )
    else if CompareText( ext, GIS_TTKSTYLE_EXT ) = 0 then
      Result := TGIS_ConfigXml.Create( _layer, path )
    else if CompareText( ext, GIS_TTKGP_EXT  ) = 0 then
      Result := TGIS_ConfigProjectIni.Create( _layer, path )
    else if CompareText( ext, GIS_TTKPROJECT_EXT ) = 0 then
      Result := TGIS_ConfigProjectXml.Create( _layer, path )
    else if CompareText( ext, GIS_TTKLS_EXT ) = 0 then
      Result := TGIS_ConfigIni.Create( _layer, path )
    else if CompareText( ext, GIS_TTKPS_EXT ) = 0 then
      Result := TGIS_ConfigIni.Create( _layer, path )
    else if CompareText( ext, GIS_TTKWP_EXT ) = 0 then
      Result := TGIS_ConfigIni.Create( _layer, path )
    else if CompareText( ext, GIS_TTKLAYER_EXT ) = 0 then
      Result := TGIS_ConfigXml.Create( _layer, path )
    else begin
      alias := GisMetadataAsString( METADATA_CONFIGFACTORY_ALIASES_TTKPROJECT, '' ) ;
      if not IsStringEmpty( alias ) and ( CompareText( ext, alias ) = 0 ) then
        Result := TGIS_ConfigProjectXml.Create( _layer, path )
      else
        Result := TGIS_ConfigIni.Create( _layer, path ) ;
    end ;
  end ;

  class function TGIS_ConfigFactory.IsProject(
    const _path : String
  ) : Boolean ;
  var
    ext   : String ;
    alias : String ;
  begin
    ext := GetFileExt( _path ) ;

    Result := ( CompareText( ext, GIS_TTKGP_EXT      ) = 0 ) or
              ( CompareText( ext, GIS_TTKPROJECT_EXT ) = 0  ) ;

    if not Result then begin
      alias := GisMetadataAsString( METADATA_CONFIGFACTORY_ALIASES_TTKPROJECT, '' ) ;
      if not IsStringEmpty( alias ) and ( CompareText( ext, alias ) = 0 ) then
        Result := True ;
    end ;
  end ;

//==================================== END =====================================



end.

