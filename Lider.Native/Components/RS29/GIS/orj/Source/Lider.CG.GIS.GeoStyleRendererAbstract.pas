//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476fv
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Utilities for constructing legend.
}

{$IFDEF DCC}
  unit GisStyleRendererAbstract ;
  {$HPPEMIT '#pragma link "GisStyleRendererAbstract"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.UITypes,
    System.Classes,
    System.Generics.Collections,

    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisRtl,
    GisRendererAbstract,
    GisLayer,
    GisHierarchy,
    GisParams,
    GisLayerVector ;
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

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   FOR INTERNAL USE ONLY. An abstract legend renderer.
  /// </summary>
  TGIS_StyleRendererAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class ( TGIS_BaseObjectDisposable )

    public

      /// <summary>
      ///   Gets current brush color.
      /// </summary>
      /// <returns>
      ///   color according to current visual style and selected flag
      /// </returns>
      function  GetBrushColor      : TGIS_Color ; virtual ;

      /// <summary>
      ///   Sets background color.
      /// </summary>
      /// <param name="_selected">
      ///   true, if it is a background color for selected nodes
      ///   false, if it is a background color for unselected nodes
      /// </param>
      procedure SetBrushColor      ( const _selected : Boolean
                                   ) ; virtual ;

      /// <summary>
      ///   Draws a checkbox.
      /// </summary>
      /// <param name="_checked">
      ///   true if the checkbox is checked
      /// </param>
      /// <param name="_rect">
      ///   rectangle to draw the checkbox in
      /// </param>
      procedure DrawCheckBox       ( const _checked  : Boolean ;
                                     const _rect     : TRect
                                   ) ; virtual ;

      /// <summary>
      ///   Draws an expand/collapse marker.
      /// </summary>
      /// <param name="_expanded">
      ///   true if the marker is expanded
      /// </param>
      /// <param name="_rect">
      ///   rectangle to draw the marker in
      /// </param>
      procedure DrawExpandCollapseMarker
                                   ( const _expanded : Boolean ;
                                     const _rect     : TRect
                                   ) ; virtual ;

      /// <summary>
      ///   Draws a rectangle.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle to draw the rectangle in
      /// </param>
      procedure DrawRectangle      ( const _rect     : TRect
                                   ) ; overload ; virtual ;

      /// <summary>
      ///   Draws a rectangle.
      /// </summary>
      /// <param name="_left">
      ///   left position of the rectangle
      /// </param>
      /// <param name="_top">
      ///   top position of the rectangle
      /// </param>
      /// <param name="_width">
      ///   width of the rectangle
      /// </param>
      /// <param name="_height">
      ///   height of the rectangle
      /// </param>
      /// <param name="_brush">
      ///   brush to fill the rectangle
      /// </param>
      /// <param name="_pen">
      ///   pen to draw the rectangle
      /// </param>
      procedure DrawRectangle      ( const _left     : Integer ;
                                     const _top      : Integer ;
                                     const _width    : Integer ;
                                     const _height   : Integer ;
                                     const _brush    : TGIS_Color ;
                                     const _pen      : TGIS_Color
                                   ) ; overload ; virtual ;

      /// <summary>
      ///   Gets text extent.
      /// </summary>
      /// <param name="_selected">
      ///   true if the text is placed at selected node
      /// </param>
      /// <param name="_text">
      ///   text to examine
      /// </param>
      /// <returns>
      ///   text extent
      /// </returns>
      function  GetTextExtent      ( const _selected : Boolean ;
                                     const _text     : String
                                   ) : TPoint ; virtual ;

      /// <summary>
      ///   Draws text.
      /// </summary>
      /// <param name="_selected">
      ///   true if the text is placed at selected node
      /// </param>
      /// <param name="_text">
      ///   text to examine
      /// </param>
      /// <param name="_rect">
      ///   rectangle to place the text in
      /// </param>
      procedure DrawText           ( const _selected : Boolean ;
                                     const _text     : String  ;
                                     const _rect     : TRect
                                   ) ; virtual ;

      /// <summary>
      ///   Draws image.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to draw
      /// </param>
      /// <param name="_left">
      ///   left position of the bitmap
      /// </param>
      /// <param name="_top">
      ///   top position of the bitmap
      /// </param>
      /// <param name="_transparent">
      ///   true if the bitmap is transparent
      /// </param>
      procedure DrawImage          ( const _bitmap   : TGIS_Bitmap ;
                                     const _left     : Integer ;
                                     const _top      : Integer ;
                                     const _transparent
                                                     : Boolean
                                   ) ; virtual ;

      /// <summary>
      ///   Prepares a temporary context.
      /// </summary>
      /// <param name="_width">
      ///   width of the context
      /// </param>
      /// <param name="_height">
      ///   height of the context
      /// </param>
      /// <param name="_ppi">
      ///   desired resolution
      /// </param>
      procedure CreateTemporaryContext
                                   ( const _width    : Integer ;
                                     const _height   : Integer ;
                                     const _ppi      : Integer
                                   ) ; virtual ;

      /// <summary>
      ///   Renders the temporary context.
      /// </summary>
      /// <param name="_left">
      ///   left position of the context
      /// </param>
      /// <param name="_top">
      ///   top position of the context
      /// </param>
      procedure RenderTemporaryContext
                                   ( const _left     : Integer ;
                                     const _top      : Integer
                                   ) ; virtual ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,

    GisFunctions,
    GisClasses ;
{$ENDIF}

//==============================================================================
// TGIS_StyleRendererAbstract
//==============================================================================

  function TGIS_StyleRendererAbstract.GetBrushColor
    : TGIS_Color ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.SetBrushColor(
    const _selected : Boolean
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawCheckBox(
    const _checked : Boolean ;
    const _rect    : TRect
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawExpandCollapseMarker(
    const _expanded : Boolean ;
    const _rect     : TRect
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawRectangle(
    const _rect : TRect
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawRectangle(
    const _left   : Integer ;
    const _top    : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _brush  : TGIS_Color ;
    const _pen    : TGIS_Color
  ) ;
  begin

  end ;

  function TGIS_StyleRendererAbstract.GetTextExtent(
    const _selected : Boolean ;
    const _text     : String
  ) : TPoint ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawText(
    const _selected : Boolean ;
    const _text     : String  ;
    const _rect     : TRect
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.DrawImage(
    const _bitmap      : TGIS_Bitmap ;
    const _left        : Integer ;
    const _top         : Integer ;
    const _transparent : Boolean
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.CreateTemporaryContext(
    const _width  : Integer ;
    const _height : Integer ;
    const _ppi    : Integer
  ) ;
  begin

  end ;

  procedure TGIS_StyleRendererAbstract.RenderTemporaryContext(
    const _left : Integer ;
    const _top  : Integer
  ) ;
  begin

  end ;

//==================================== END =====================================
end.
