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
  Print Preview Factory class.
}

unit GisPrintPreviewHelperAbstract ;
{$HPPEMIT '#pragma link "GisPrintPreviewHelperAbstract"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.UITypes,
  GisInterfaces,
  GisTypes,
  GisPrintManagerAbstract ;

type

  /// <summary>
  ///   Abstract class that is a handle to a real framework dependent form.
  /// </summary>
  TGIS_PrintPreviewFactory = class
    public

      /// <summary>
      ///   Check if any printer exists.
      /// </summary>
      /// <returns>
      ///   True, if any printer exists.
      /// </returns>
      function  IsPrinter : Boolean ; virtual; abstract;

      /// <summary>
      ///   Execute print preview.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_customPage">
      ///   custom page format
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <param name="_caption">
      ///   preview caption
      /// </param>
      /// <param name="_left">
      ///   x-coordinate of the rectangle to print
      /// </param>
      /// <param name="_top">
      ///   y-coordinate of the rectangle to print
      /// </param>
      /// <param name="_width">
      ///   width of the rectangle to print
      /// </param>
      /// <param name="_height">
      ///   height of the rectangle to print
      /// </param>
      /// <param name="_state">
      ///   not used
      /// </param>
      /// <param name="_help">
      ///   help event
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when _viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview   ( const _viewer       : IGIS_Viewer ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            const _customPage   : String  ;
                            var   _scale        : Double  ;
                            const _caption      : String  ;
                            const _left         : Integer ;
                            const _top          : Integer ;
                            const _width        : Integer ;
                            const _height       : Integer ;
                            const _state        : TWindowState ;
                            const _help         : TGIS_HelpEvent
                          ) ; virtual; abstract;
  end ;

var
  /// <summary>
  ///   Reference to a real framework dependent form.
  /// </summary>
  PrintPreviewHelper : TGIS_PrintPreviewFactory ;

//##############################################################################
implementation

{==================================== END =====================================}
end.


