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
  Number of declarations which makes FireMonkey more compatible with VCL.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoTypesUI ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoTypesUI"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.Math,
    System.SysUtils,

    Lider.CG.GIS.GeoRtl ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.IO,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF COCOA}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL,
    Foundation ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Defines color interpolation method.
  /// </summary>
  TGIS_ColorInterpolationMode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Alpha, red, green and blue components interpolation.
    /// </summary>
    RGB,

    /// <summary>
    ///   Alpha, hue, saturation and lightness components interpolation.
    /// </summary>
    HSL,

    /// <summary>
    ///   No interpolation, discrete colors are used.
    /// </summary>
    None,

    /// <summary>
    ///   Alpha, hue, saturation and lightness components interpolation.
    ///   This method forcess the hue fo the color to go through 360.
    /// </summary>
    HSL360
  ) ;

  /// <summary>
  ///   Defines colormap mode.
  /// </summary>
  TGIS_ColorMapMode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Continuous colormap.
    /// </summary>
    Continuous,

    /// <summary>
    ///   Discrete colormap.
    /// </summary>
    Discrete
  ) ;

  /// <summary>
  ///   Platform independent color declaration.
  /// </summary>
  TGIS_Color = {$IFDEF OXYGENE} public {$ENDIF}
               {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    private
      class function fget_None        : TGIS_Color ; static ;
      class function fget_Aqua        : TGIS_Color ; static ;
      class function fget_Black       : TGIS_Color ; static ;
      class function fget_Blue        : TGIS_Color ; static ;
      class function fget_DimGray     : TGIS_Color ; static ;
      class function fget_Fuchsia     : TGIS_Color ; static ;
      class function fget_Gray        : TGIS_Color ; static ;
      class function fget_Green       : TGIS_Color ; static ;
      class function fget_LightGray   : TGIS_Color ; static ;
      class function fget_Lime        : TGIS_Color ; static ;
      class function fget_Maroon      : TGIS_Color ; static ;
      class function fget_Navy        : TGIS_Color ; static ;
      class function fget_Olive       : TGIS_Color ; static ;
      class function fget_Purple      : TGIS_Color ; static ;
      class function fget_Red         : TGIS_Color ; static ;
      class function fget_Silver      : TGIS_Color ; static ;
      class function fget_Teal        : TGIS_Color ; static ;
      class function fget_White       : TGIS_Color ; static ;
      class function fget_Yellow      : TGIS_Color ; static ;
      class function fget_Crazy       : TGIS_Color ; static ;
      class function fget_RenderColor : TGIS_Color ; static ;

    public

      function fget_A : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
      function fget_R : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
      function fget_G : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
      function fget_B : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
      function fget_H : Single ;
      function fget_S : Single ;
      function fget_L : Single ;

    public

      /// <summary>
      ///   Internal representation as little endian ARGB integer.
      /// </summary>
      ARGB : Cardinal ;

      {$IFNDEF GENDOC}
        class operator Equal   ( _value1, _value2 : TGIS_Color ) : Boolean ;
        class operator NotEqual( _value1, _value2 : TGIS_Color ) : Boolean ;
      {$ENDIF}

      /// <summary>
      ///   Extracts Alpha part of color ARGB definition.
      /// </summary>
      property A : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF}
                                                 read  fget_A ;

      /// <summary>
      ///   Extracts Red part of ARGB color definition.
      /// </summary>
      property R : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF}
                                                 read  fget_R ;

      /// <summary>
      ///   Extracts Green part of ARGB color definition.
      /// </summary>
      property G : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF}
                                                 read  fget_G ;

      /// <summary>
      ///   Extracts Blue part of ARGB color definition.
      /// </summary>
      property B : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF}
                                                 read  fget_B ;

      /// <summary>
      ///   Extracts Hue part of HSL color definition.
      /// </summary>
      property H : Single
                   read  fget_H ;

      /// <summary>
      ///   Extracts Saturation part of HSL color definition.
      /// </summary>
      property S : Single
                   read  fget_S ;

      /// <summary>
      ///   Extracts Luminescence part of HSL color definition.
      /// </summary>
      property L : Single
                   read  fget_L ;

      /// <summary>
      ///   <para>
      ///     Constructs a standard color.
      ///   </para>
      ///   <para>
      ///     Use this color as "not defined" color (it constitute transparent
      ///     color).
      ///   </para>
      /// </summary>
      class property None      : TGIS_Color
                           read fget_None ;

      /// <summary>
      ///   Constructs a standard color - $ff00ffff.
      /// </summary>
      class property Aqua      : TGIS_Color
                           read fget_Aqua ;

      /// <summary>
      ///   Constructs a standard color - $ff000000.
      /// </summary>
      class property Black     : TGIS_Color
                           read fget_Black ;

      /// <summary>
      ///   Constructs a standard color - $ff0000ff.
      /// </summary>
      class property Blue      : TGIS_Color
                           read fget_Blue ;

      /// <summary>
      ///   Constructs a standard color - $ff696969.
      /// </summary>
      class property DimGray   : TGIS_Color
                           read fget_DimGray ;

      /// <summary>
      ///   Constructs a standard color - $ffff00ff.
      /// </summary>
      class property Fuchsia   : TGIS_Color
                           read fget_Fuchsia ;

      /// <summary>
      ///   Constructs a standard color - $ff808080.
      /// </summary>
      class property Gray      : TGIS_Color
                           read fget_Gray ;

      /// <summary>
      ///   Constructs a standard color - $ff008000.
      /// </summary>
      class property Green     : TGIS_Color
                           read fget_Green ;

      /// <summary>
      ///   Constructs a standard color - $ffd3d3d3.
      /// </summary>
      class property LightGray : TGIS_Color
                           read fget_LightGray ;

      /// <summary>
      ///   Constructs a standard color - $ff00ff00.
      /// </summary>
      class property Lime      : TGIS_Color
                           read fget_Lime ;

      /// <summary>
      ///   Constructs a standard color - $ff800000.
      /// </summary>
      class property Maroon    : TGIS_Color
                           read fget_Maroon ;

      /// <summary>
      ///   Constructs a standard color - $ff000080.
      /// </summary>
      class property Navy      : TGIS_Color
                           read fget_Navy ;


      /// <summary>
      ///   Constructs a standard color - $ff808000.
      /// </summary>
      class property Olive     : TGIS_Color
                           read fget_Olive ;


      /// <summary>
      ///   Constructs a standard color - $ff800080.
      /// </summary>
      class property Purple    : TGIS_Color
                           read fget_Purple ;

      /// <summary>
      ///   Constructs a standard color - $ffff0000.
      /// </summary>
      class property Red       : TGIS_Color
                           read fget_Red ;

      /// <summary>
      ///   Constructs a standard color - $ffc0c0c0.
      /// </summary>
      class property Silver    : TGIS_Color
                           read fget_Silver ;

      /// <summary>
      ///   Constructs a standard color - $ff008080.
      /// </summary>
      class property Teal      : TGIS_Color
                           read fget_Teal ;

      /// <summary>
      ///   Constructs a standard color - $ffffffff.
      /// </summary>
      class property White     : TGIS_Color
                           read fget_White ;

      /// <summary>
      ///   Constructs a standard color - $ffffff00.
      /// </summary>
      class property Yellow    : TGIS_Color
                           read fget_Yellow ;


      /// <summary>
      ///   Constructs a standard color - $00010816.
      /// </summary>
      class property Crazy     : TGIS_Color
                           read fget_Crazy ;

      /// <summary>
      ///   <para>
      ///     Constructs a standard color - $00ff9933.
      ///   </para>
      ///   <para>
      ///     Use this color to mark feature that render must be used to
      ///     color it.
      ///   </para>
      /// </summary>
      class property RenderColor : TGIS_Color
                           read fget_RenderColor ;

      /// <summary>
      ///   Creates a color using the RGB color model.
      /// </summary>
      /// <param name="_r">
      ///   red; expected value: 0..255
      /// </param>
      /// <param name="_g">
      ///   green; expected value: 0..255
      /// </param>
      /// <param name="_b">
      ///   blue; expected value: 0..255
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromRGB   ( const _r     : Byte ;
                                 const _g     : Byte ;
                                 const _b     : Byte
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the RGB color model.
      /// </summary>
      /// <param name="_value">
      ///   color value as 4 byte little endian integer; bytes from most
      ///   significant are: ignored, red, green, nlue
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromRGB   ( const _value : Cardinal
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the BGR color model.
      /// </summary>
      /// <param name="_value">
      ///   color value as 4 byte little endian integer; bytes from most
      ///   significant are: Ignored, Blue, Green, Red
      /// </param>
      /// <returns>
      ///   Created color
      /// </returns>
      class function FromBGR   ( const _value : Cardinal
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the BGR color model.
      /// </summary>
      /// <param name="_b">
      ///   blue; expected value: 0..255
      /// </param>
      /// <param name="_g">
      ///   green; expected value: 0..255
      /// </param>
      /// <param name="_r">
      ///   red; expected value: 0..255
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromBGR   ( const _b     : Byte ;
                                 const _g     : Byte ;
                                 const _r     : Byte
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color using the BGR color model.
      /// </summary>
      /// <param name="_value">
      ///   color value as 4 byte little endian integer; bytes from most
      ///   significant are: Alpha, Blue, Green, Blue
      /// </param>
      /// <returns>
      ///   Created color
      /// </returns>
      class function FromABGR  ( const _value : Cardinal
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


      /// <summary>
      ///   Converts a color to an integer in the RGB form.
      /// </summary>
      /// <returns>
      ///   Color value as 4 byte little endian integer; bytes from most
      ///   significant are: 0, red, green, blue.
      /// </returns>
      function ToRGB : Cardinal ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Converts a color to an integer in the ABGR form.
      /// </summary>
      /// <returns>
      ///   Color value as 4 byte little endian integer; bytes from most
      ///   significant are: alpha, red, green, blue.
      /// </returns>
      function ToARGB : Cardinal ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}


      /// <summary>
      ///   Converts a color to an integer in the BGR form.
      /// </summary>
      /// <returns>
      ///   Color value as 4 byte little endian integer; bytes from most
      ///   significant are: 0, Blue, Green, Red.
      /// </returns>
      function ToBGR : Cardinal ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Converts a color to an integer in the ABGR form.
      /// </summary>
      /// <returns>
      ///   Color value as 4 byte little endian integer; bytes from most
      ///   significant are: Alpha, Blue, Green, Blue.
      /// </returns>
      function ToABGR : Cardinal ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the hue (H), saturation (S) and lightness (L) components
      ///   of the current color.
      /// </summary>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_s">
      ///   saturation, varies from 0 to 1
      /// </param>
      /// <param name="_l">
      ///   lightness, varies from 0 to 1
      /// </param>
      procedure ToHSL ( var _h : Double ;
                        var _s : Double ;
                        var _l : Double
                      ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the alpha (A), hue (H), saturation (S) and lightness (L)
      ///   components of the current color.
      /// </summary>
      /// <param name="_a">
      ///   alpha, varies from 0 to 1
      /// </param>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_s">
      ///   saturation, varies from 0 to 1
      /// </param>
      /// <param name="_l">
      ///   lightness, varies from 0 to 1
      /// </param>
      procedure ToAHSL( var _a : Double ;
                        var _h : Double ;
                        var _s : Double ;
                        var _l : Double
                      ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the hue (H), saturation (S) and value (V) components
      ///   of the current color.
      /// </summary>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_s">
      ///   saturation, varies from 0 to 1
      /// </param>
      /// <param name="_v">
      ///   value/brightness, varies from 0 to 1
      /// </param>
      procedure ToHSV ( var _h : Double ;
                        var _s : Double ;
                        var _v : Double
                      ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the alpha (A), hue (H), saturation (S) and value (V)
      ///   components of the current color.
      /// </summary>
      /// <param name="_a">
      ///   alpha, varies from 0 to 1
      /// </param>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_s">
      ///   saturation, varies from 0 to 1
      /// </param>
      /// <param name="_v">
      ///   value (brightness), varies from 0 to 1
      /// </param>
      procedure ToAHSV( var _a : Double ;
                        var _h : Double ;
                        var _s : Double ;
                        var _v : Double
                      ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color using the RGB color model.
      /// </summary>
      /// <param name="_a">
      ///   alpha; expected value: 0..255
      /// </param>
      /// <param name="_r">
      ///   red; expected value: 0..255
      /// </param>
      /// <param name="_g">
      ///   green; expected value: 0..255
      /// </param>
      /// <param name="_b">
      ///   blue; expected value: 0..255
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromARGB  ( const _a     : Byte ;
                                 const _r     : Byte ;
                                 const _g     : Byte ;
                                 const _b     : Byte
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color using RGB color model.
      /// </summary>
      /// <param name="_value">
      ///   color value as 4 byte little endian integer; bytes from most
      ///   significant are: alpha, red, green, blue
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromARGB  ( const _value : Cardinal
                               ) : TGIS_Color ; overload; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the HSL color model.
      /// </summary>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_s">
      ///   saturation; expected value: 0..1
      /// </param>
      /// <param name="_l">
      ///   luminance; expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromHSL   ( const _h     : Single ;
                                 const _s     : Single ;
                                 const _l     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color using the HSL color model.
      /// </summary>
      /// <param name="_a">
      ///   alpha; expected value: 0..1
      /// </param>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_s">
      ///   saturation; expected value: 0..1
      /// </param>
      /// <param name="_l">
      ///   luminance; expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromAHSL  ( const _a     : Single ;
                                 const _h     : Single ;
                                 const _s     : Single ;
                                 const _l     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the HSV color model.
      /// </summary>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_s">
      ///   saturation; expected value: 0..1
      /// </param>
      /// <param name="_v">
      ///   value (brightness); expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromHSV   ( const _h     : Single ;
                                 const _s     : Single ;
                                 const _v     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color using the HSV color model.
      /// </summary>
      /// <param name="_a">
      ///   alpha; expected value: 0..1
      /// </param>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_s">
      ///   saturation; expected value: 0..1
      /// </param>
      /// <param name="_v">
      ///   value (brightness); expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromAHSV  ( const _a     : Single ;
                                 const _h     : Single ;
                                 const _s     : Single ;
                                 const _v     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the CMYK color model.
      /// </summary>
      /// <param name="_c">
      ///   cyan; expected value: 0..1
      /// </param>
      /// <param name="_m">
      ///   magenta; expected value: 0..1
      /// </param>
      /// <param name="_y">
      ///   yellow; expected value: 0..1
      /// </param>
      /// <param name="_k">
      ///   key (black); expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromCMYK  ( const _c     : Single ;
                                 const _m     : Single ;
                                 const _y     : Single ;
                                 const _k     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a transparent color with alpha using the CMYK color model.
      /// </summary>
      /// <param name="_a">
      ///   alpha; expected value: 0..1
      /// </param>
      /// <param name="_c">
      ///   cyan; expected value: 0..1
      /// </param>
      /// <param name="_m">
      ///   magenta; expected value: 0..1
      /// </param>
      /// <param name="_y">
      ///   yellow; expected value: 0..1
      /// </param>
      /// <param name="_k">
      ///   key (black); expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromACMYK ( const _a     : Single ;
                                 const _c     : Single ;
                                 const _m     : Single ;
                                 const _y     : Single ;
                                 const _k     : Single
                               ) : TGIS_Color ; static ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the cyan (C), magenta (M), yellow (Y), and black (K)
      ///   components of the current color.
      /// </summary>
      /// <param name="_c">
      ///   cyan; expected value: 0..1
      /// </param>
      /// <param name="_m">
      ///   magenta; expected value: 0..1
      /// </param>
      /// <param name="_y">
      ///   yellow; expected value: 0..1
      /// </param>
      /// <param name="_k">
      ///   key (black); expected value: 0..1
      /// </param>
      procedure ToCMYK         ( var _c : Double ;
                                 var _m : Double ;
                                 var _y : Double ;
                                 var _k : Double
                               ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Retrieves the alpha (A), cyan (C), magenta (M), yellow (Y),
      ///   and black (K) components of the current color.
      /// </summary>
      /// <param name="_a">
      ///   alpha, varies from 0 to 1
      /// </param>
      /// <param name="_c">
      ///   cyan; expected value: 0..1
      /// </param>
      /// <param name="_m">
      ///   magenta; expected value: 0..1
      /// </param>
      /// <param name="_y">
      ///   yellow; expected value: 0..1
      /// </param>
      /// <param name="_k">
      ///   key (black); expected value: 0..1
      /// </param>
      procedure ToACMYK        ( var _a : Double ;
                                 var _c : Double ;
                                 var _m : Double ;
                                 var _y : Double ;
                                 var _k : Double
                               ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Converts the color to a hexadecimal string.
      /// </summary>
      /// <param name="_addAlpha">
      ///   if true, the result string will contain an alpha value (this is
      ///   default); otherwise only RGB values will be returned
      /// </param>
      /// <param name="_addHash">
      ///   if True, the result string will contain the hash (#) symbol at the
      ///   beginning; False is default value
      /// </param>
      /// <returns>
      ///   Hexadecimal color representation as a string.
      /// </returns>
      function ToString        ( const _addAlpha : Boolean = True ;
                                 const _addHash  : Boolean = False
                               ) : String ;
                               {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Creates a color using the hex string.
      /// </summary>
      /// <param name="_value">
      ///   a CSS string representation of a color; see remarks:
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_WRONGVALUE
      /// </exception>
      /// <remarks>
      ///   Valid CSS color strings are listed below.
      ///   <list type="table">
      ///     <item>
      ///       <term>hex color with hash symbol</term>
      ///       <description>#090, #009900, #090a, #009900aa,</description>
      ///     </item>
      ///     <item>
      ///       <term>hex color without hash symbol</term>
      ///       <description>090, 009900, 090a, 009900aa,</description>
      ///     </item>
      ///     <item>
      ///       <term>rgb color</term>
      ///       <description>
      ///         rgb(34, 12, 64, 0.6), rgba(34, 12, 64, 0.6),
      ///         rgb(34 12 64 / 0.6), rgba(34 12 64 / 0.3)
      ///         rgb(34.0 12 64 / 60%), rgba(34.6 12 64 / 30%)
      ///       </description>
      ///     </item>
      ///     <item>
      ///       <term>hsl color</term>
      ///       <description>
      ///         hsl(30, 100%, 50%, 0.6), hsla(30, 100%, 50%, 0.6),
      ///         hsl(30 100% 50% / 0.6), hsla(30 100% 50% / 0.6),
      ///         hsl(30.0 100% 50% / 60%), hsla(30.2 100% 50% / 60%)
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      class function FromString( const _value : String
                               ) : TGIS_Color ; static ;

      /// <summary>
      ///   Tries to create a color using the hex string.
      /// </summary>
      /// <param name="_value">
      ///   a CSS string representation of a color; see remarks:
      /// </param>
      /// <param name="_color">
      ///   a variable to assign a created color
      /// </param>
      /// <returns>
      ///   True if _hexString represents a valid color; otherwise False
      /// </returns>
      class function TryFromString( const _value : String ;
                                    var   _color : TGIS_Color
                                  ) : Boolean ; static ;
                                  {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  end ;

  /// <summary>
  ///   Platform independent storage for pixels as 32bit array (for ARGB support)
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_Pixels = {$IFDEF OXYGENE} public {$ENDIF} Array of Int32 ;
  {$ELSE}
    {#typehint:array:Cardinal}
    TGIS_Pixels = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : {$IFDEF GIS_PDK}
                                                   Cardinal
                                                 {$ELSE}
                                                   Int32
                                                 {$ENDIF}
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of TGIS_Color.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_ColorArray = array of TGIS_Color ;
  {$ELSE}
    {#typehint:array:TGIS_Color}
    TGIS_ColorArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_Color
          read  dummy
          write dummy;
    end ;
  {$ENDIF}


  /// <summary>
  ///   Platform independent pixel format specification
  /// </summary>
  TGIS_PixelFormat = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   1 bit per pixel.
    /// </summary>
    Bit1,
    /// <summary>
    ///   4 bit per pixel.
    /// </summary>
    Bit4,
    /// <summary>
    ///   8 bit per pixel.
    /// </summary>
    Bit8,
    /// <summary>
    ///   24 bit per pixel in RGB.
    /// </summary>
    RGB,
    /// <summary>
    ///   32 bit per pixels in ARGB.
    /// </summary>
    ARGB,
    /// <summary>
    ///   Custom for special use.
    /// </summary>
    Custom
  ) ;

  /// <summary>
  ///   Platform independent pixel storage format definition.
  /// </summary>
  TGIS_PixelSubFormat = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Raw bytes.
      /// </summary>
      None,

      /// <summary>
      ///   BMP image format.
      /// </summary>
      BMP,

      /// <summary>
      ///   JPEG image format.
      /// </summary>
      JPEG,

      /// <summary>
      ///   PNG image format.
      /// </summary>
      PNG,

      /// <summary>
      ///   JPEG 2000 image format.
      /// </summary>
      JP2,

      /// <summary>
      ///   GRID data.
      /// </summary>
      GRID
  ) ;


  {$IFDEF OXYGENE}
    {$IFDEF CLR}
      /// <summary>
      ///   Platform independent font style definition.
      /// </summary>
      TGIS_FontStyle = public flags (
      /// <summary>
      ///   Bold font
      /// </summary>
      Bold,
      /// <summary>
      ///   Italic font
      /// </summary>
      Italic,
      /// <summary>
      ///   Underline font
      /// </summary>
      Underline,
      /// <summary>
      ///   Strikeout font
      /// </summary>
      StrikeOut
    ) ;
  {$ELSE}
      /// <summary>
      ///   Platform independent font style definition.
      /// </summary>
      TGIS_FontStyle = public (
      /// <summary>
      ///   Bold font
      /// </summary>
      Bold,
      /// <summary>
      ///   Italic font
      /// </summary>
      Italic,
      /// <summary>
      ///   Underline font
      /// </summary>
      Underline,
      /// <summary>
      ///   Strikeout font
      /// </summary>
      StrikeOut
    ) of Integer ;
  {$ENDIF}
  {$ELSE}
  /// <summary>
  ///   Platform independent font style definition.
  /// </summary>
  TGIS_FontStyle = (
    /// <summary>
    ///   Bold font
    /// </summary>
    Bold,
    /// <summary>
    ///   Italic font
    /// </summary>
    Italic,
    /// <summary>
    ///   Underline font
    /// </summary>
    Underline,
    /// <summary>
    ///   Strikeout font
    /// </summary>
    StrikeOut
  )  ;
  {$ENDIF}

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      fsBold       = TGIS_FontStyle.Bold      ;
      fsItalic     = TGIS_FontStyle.Italic    ;
      fsUnderline  = TGIS_FontStyle.Underline ;
      fsStrikeOut  = TGIS_FontStyle.StrikeOut ;
  {$ENDIF}

type

  /// <summary>
  ///   Platform independent set of font style definition.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
    TGIS_FontStyles = public set of TGIS_FontStyle ;
    {$ELSE}
    TGIS_FontStyles = public TGIS_FontStyle ;
    {$ENDIF}
  {$ELSE}
    TGIS_FontStyles = set of TGIS_FontStyle ;
  {$ENDIF}

  /// <summary>
  ///   Platform independent font class.
  /// </summary>
  TGIS_Font = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Font name
      /// </summary>
      Name : String ;

      /// <summary>
      ///  Font size expressed in points.
      /// </summary>
      Size : Integer ;

      /// <summary>
      ///    Font styles ( bold, italic etc.)
      /// </summary>
      Style     : TGIS_FontStyles ;

      /// <summary>
      ///    Font color
      /// </summary>
      Color     : TGIS_Color ;

    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Assign font definition from other instance.
      /// </summary>
      /// <param name="_source">
      ///   source font definition
      /// </param>
      procedure Assign( const _source : TGIS_Font ) ;

      /// <summary>
      ///   Load definition from platform specific object
      /// </summary>
      /// <param name="_font">
      ///   platform specific object (like TFont for VCL)
      /// </param>
      procedure LoadFromFont( const _font : TObject ) ;
   end ;

type
  /// <summary>
  ///   Platform independent brush style definition.
  /// </summary>
  TGIS_BrushStyle = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Brush is solid.
      /// </summary>
      Solid      ,

      /// <summary>
      ///   Brush is invisible.
      /// </summary>
      Clear      ,

      /// <summary>
      ///   Brush is horizontal.
      /// </summary>
      Horizontal ,

      /// <summary>
      ///   Brush is vertical.
      /// </summary>
      Vertical   ,

      /// <summary>
      ///   Brush is forward diagonal.
      /// </summary>
      FDiagonal  ,

      /// <summary>
      ///   Brush is backward diagonal.
      /// </summary>
      BDiagonal  ,

      /// <summary>
      ///   Brush is cross.
      /// </summary>
      Cross      ,

      /// <summary>
      ///   Brush is diagonal cross.
      /// </summary>
      DiagCross
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisBsSolid      = TGIS_BrushStyle.Solid      ;
      gisBsClear      = TGIS_BrushStyle.Clear      ;
      gisBsHorizontal = TGIS_BrushStyle.Horizontal ;
      gisBsVertical   = TGIS_BrushStyle.Vertical   ;
      gisBsFDiagonal  = TGIS_BrushStyle.FDiagonal  ;
      gisBsBDiagonal  = TGIS_BrushStyle.BDiagonal  ;
      gisBsCross      = TGIS_BrushStyle.Cross      ;
      gisBsDiagCross  = TGIS_BrushStyle.DiagCross  ;
  {$ENDIF}

type
  /// <summary>
  ///   Platform independent pen style definition.
  /// </summary>
  TGIS_PenStyle = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Solid line.
      /// </summary>
      Solid       ,

      /// <summary>
      ///   Pen is dashed.
      /// </summary>
      Dash        ,

      /// <summary>
      ///   Pen is doted.
      /// </summary>
      Dot         ,

      /// <summary>
      ///   Pen has alternating dashes and dots.
      /// </summary>
      DashDot     ,

      /// <summary>
      ///   Pen has alternating dashes and double dots.
      /// </summary>
      DashDotDot  ,

      /// <summary>
      ///   Pen is invisible.
      /// </summary>
      Clear
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisPsSolid       = TGIS_PenStyle.Solid       ;
      gisPsDash        = TGIS_PenStyle.Dash        ;
      gisPsDot         = TGIS_PenStyle.Dot         ;
      gisPsDashDot     = TGIS_PenStyle.DashDot     ;
      gisPsDashDotDot  = TGIS_PenStyle.DashDotDot  ;
      gisPsClear       = TGIS_PenStyle.Clear       ;
  {$ENDIF}

type
  /// <summary>
  ///   Platform independent pen cap style.
  /// </summary>
  TGIS_LineCap = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   End caps are flat.
      /// </summary>
      Flat      = 0 ,

      /// <summary>
      ///   End caps are square.
      /// </summary>
      Square    = 1 ,

      /// <summary>
      ///   End caps are round.
      /// </summary>
      Round     = 2
  ) ;

  /// <summary>
  ///   Platform independent pen join style.
  /// </summary>
  TGIS_LineJoin = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Joins are beveled.
      /// </summary>
      Bevel     = 0 ,

      /// <summary>
      ///   Joins are mittered within a limit; if it exceeds this limit,
      ///   the join is beveled.
      /// </summary>
      Miter     = 1 ,

      /// <summary>
      ///   Joins are round.
      /// </summary>
      Round     = 2
  ) ;


type
  /// <summary>
  ///   Platform independent pen class.
  /// </summary>
  TGIS_Pen = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Pen width.
      /// </summary>
      Width : Integer ;

      /// <summary>
      ///   Pen style.
      /// </summary>
      Style : TGIS_PenStyle ;

      /// <summary>
      ///   Pen color
      /// </summary>
      Color : TGIS_Color ;

      /// <summary>
      ///   End cap style.
      /// </summary>
      LineCap : TGIS_LineCap ;

      /// <summary>
      ///   Join style.
      /// </summary>
      LineJoin : TGIS_LineJoin ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Assign pen definition from other instance.
      /// </summary>
      /// <param name="_source">
      ///   source pen definition
      /// </param>
      procedure Assign( const _source : TGIS_Pen ) ;
  end;

  /// <summary>
  ///   Platform independent brush class.
  /// </summary>
  TGIS_Brush = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Brush style.
      /// </summary>
      Style : TGIS_BrushStyle ;

      /// <summary>
      ///   Brush color.
      /// </summary>
      Color : TGIS_Color ;

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Assign brush from other instance.
      /// </summary>
      /// <param name="_source">
      ///   source brush definition
      /// </param>
      procedure Assign( const _source : TGIS_Brush ) ;
  end;

type
  /// <summary>
  ///   TGIS_Bitmap supported formats
  /// </summary>
  TGIS_BitmapFormat = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   System native format.
    /// </summary>
    Native,
    /// <summary>
    ///   ARGB order.
    /// </summary>
    ARGB,
    /// <summary>
    ///   ABGR order.
    /// </summary>
    ABGR
  ) ;

  /// <summary>
  ///   TGIS_Bitmap lines order
  /// </summary>
  TGIS_BitmapLinesOrder = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   System native order.
    /// </summary>
    Native,
    /// <summary>
    ///   top-bottom order.
    /// </summary>
    Down,
    /// <summary>
    ///   bottom-top order.
    /// </summary>
    Up
  ) ;

  /// <summary>
  ///   Bitmap scaling filtering type
  /// </summary>
  TGIS_ScalingFilter = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Linear (Triangle) resampling.
    /// </summary>
    Linear,
    /// <summary>
    ///   Lanczos3 resampling.
    /// </summary>
    Lanczos3
  ) ;

  TGIS_Bitmap = class ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class( TGIS_ObjectDisposable )
    public
      /// <summary>
      ///  Master TGIS_Bitmap object.
      /// </summary>
      Master         : TGIS_Bitmap ;
    private
      FPremultiplied : Boolean ;
    protected
      function  fget_Width        : Integer ; virtual; abstract;
      function  fget_Height       : Integer ; virtual; abstract;
      function  fget_NativeBitmap : TObject ; virtual; abstract;
      procedure fset_NativeBitmap ( const _value : TObject
                                  ) ; virtual; abstract;

    public
      /// <summary>
      ///   Create bitmap from platform specific object.
      /// </summary>
      /// <param name="_bmp">
      ///   platform specific object (like TBitmap for VCL)
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      class function FromBitmap   ( const _bmp    : TObject
                                  ) : TGIS_BitmapAbstract ; virtual; abstract;

      /// <summary>
      ///   Create bitmap from file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEBADFORMAT or native exception
      /// </exception>
      class function FromFile     ( const _path   : String
                                  ) : TGIS_BitmapAbstract ; virtual; abstract;

      /// <summary>
      ///   Create bitmap from stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEBADFORMAT or native exception
      /// </exception>
      class function FromStream   ( const _stream : TObject
                                  ) : TGIS_BitmapAbstract ; virtual; abstract;
    public
      /// <summary>
      ///   Save bitmap to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure   ToFile          ( const _path   : String
                                  ) ; virtual; abstract;

      /// <summary>
      ///   Save bitmap to stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      procedure   ToStream        ( const _stream : TObject
                                  ) ; overload; virtual; abstract;

      /// <summary>
      ///   Save bitmap to stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      /// <param name="_format">
      ///   output pixel format type
      /// </param>
      /// <param name="_subformat">
      ///   output pixel subformat type
      /// </param>
      /// <param name="_compression">
      ///   compression level
      /// </param>
      procedure   ToStream        ( const _stream       : TObject ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; overload; virtual; abstract;

      /// <summary>
      ///   Make bitmap transparent
      /// </summary>
      procedure   MakeTransparent ; virtual; abstract;

      /// <summary>
      ///   Prepare bitmap for direct pixel access operation.
      /// </summary>
      /// <param name="_pixels">
      ///   pixel buffer (allocated and maintained by LockPixels ..
      ///   UnlockPixels
      /// </param>
      /// <param name="_writable">
      ///   True if pixels will be modified
      /// </param>
      /// <param name="_format">
      ///   desired format of locked pixels; on some platforms some system
      ///   level operation required different pixel format then default
      /// </param>
      /// <param name="_order">
      ///   desired line order of locked pixels; on some platforms some system
      ///   level operation required different order then default
      /// </param>
      /// <remarks>
      ///   Must be paired with UnlockPixels.
      /// </remarks>
      procedure   LockPixels      ( var   _pixels   : TGIS_Pixels ;
                                    const _writable : Boolean     ;
                                    const _format   : TGIS_BitmapFormat ;
                                    const _order    : TGIS_BitmapLinesOrder
                                  ) ; virtual; abstract;

      /// <summary>
      ///   Finalize pixel access operation.
      /// </summary>
      /// <remarks>
      ///   Must be paired with LockPixels.
      /// </remarks>
      procedure   UnlockPixels    ; virtual; abstract;


      /// <summary>
      ///   Draw a shape on a bitmap. Used for internal purposes of
      ///   TGIS_Shape.PrepareContourInternal.
      /// </summary>
      /// <param name="_shape">
      ///   TGIS_Shape object
      /// </param>
      /// <param name="_outline">
      ///   if _true then polygonal shape will be drawn with black outline,
      ///   otherwise only internal will be drawn.
      /// </param>
      /// <param name="_scale">
      ///   pixels/unit scale of a shape
      /// </param>
      /// <param name="_offset">
      ///   margin between left-up corner of the bitmap and left-up corner of
      ///   shape extent (expressed in pixels)
      /// </param>
      /// <remarks>
      ///   Internals will be drawn as TGIS_Color.DimGray; borders will be
      ///   drawn with TGIS_Color.Black.
      ///   Default PPI for platfom is used (96 for Windows)
      /// </remarks>
      procedure   DrawShape       ( const _shape      : TObject ;
                                    const _outline    : Boolean ;
                                    var   _scale      : Double  ;
                                    var   _offset     : TPoint
                                  ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a shape on a bitmap (for internal
      ///   TGIS_Shape.PrepareContourInternal purposes).
      /// </summary>
      /// <param name="_shape">
      ///   shape to be drawn
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_outline">
      ///   if True then outline of polygonal object will be drawn; otherwise
      ///   only interiors will be drawn
      /// </param>
      /// <param name="_scale">
      ///   shape size in pixels divided by the shape size in map units
      /// </param>
      /// <param name="_offset">
      ///   offset of shape within bitmap caused by map placement, margins etc.
      /// </param>
      /// <remarks>
      ///   Interiors are drawn with TGIS_Color.LightGray. Borders and lines
      ///   are drawn TGIS_Color.DimGray.
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure   DrawShape         ( const _shape    : TObject ;
                                      const _ppi      : Integer ;
                                      const _outline  : Boolean ;
                                      var   _scale    : Double  ;
                                      var   _offset   : TPoint
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a shape on a bitmap (for internal
      ///   TGIS_Shape.PrepareContourInternal purposes).
      /// </summary>
      /// <param name="_shape">
      ///   shape to be drawn
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_outline">
      ///   if True then outline of polygonal object will be drawn; otherwise
      ///   only interiors will be drawn
      /// </param>
      /// <param name="_areacolor">
      ///   color for interiors
      /// </param>
      /// <param name="_linecolor">
      ///   color for lines and outlines
      /// </param>
      /// <param name="_scale">
      ///   pixels/unit scale of a shape
      /// </param>
      /// <param name="_offset">
      ///   offset of shape within bitmap caused by map placement, margins etc.
      /// </param>
      /// <remarks>
      ///   Interiors are drawn with TGIS_Color.LightGray. Borders and lines
      ///   are drawn TGIS_Color.DimGray.
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure   DrawShape         ( const _shape    : TObject ;
                                      const _ppi      : Integer ;
                                      const _outline  : Boolean ;
                                      const _areacolor: TGIS_Color ;
                                      const _linecolor: TGIS_Color ;
                                      var   _scale    : Double  ;
                                      var   _offset   : TPoint
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      /// <remarks>
      ///   Default PPI for platfom is used (96 for Windows)
      /// </remarks>
      procedure   DrawSymbol        ( const _name     : String
                                    ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      ///</param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      procedure   DrawSymbol        ( const _name     :  String ;
                                      const _ppi      :  Integer
                                    ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      ///</param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_areacolor">
      ///   color for interiors
      /// </param>
      /// <param name="_linecolor">
      ///   color for lines and outlines
      /// </param>
      procedure   DrawSymbol        ( const _name     : String ;
                                      const _ppi      : Integer ;
                                      const _areacolor: TGIS_Color ;
                                      const _linecolor: TGIS_Color
                                    ) ; overload ; virtual; abstract;
      /// <inheritdoc/>
      procedure   DrawGlyph         ( const _symbo    : TObject ;
                                      const _ppi      : Integer ;
                                      const _color    : TGIS_Color ;
                                      const _enabled  : Boolean
                                    ) ; overload ; virtual; abstract;

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Create a bitmap viewer
      /// </summary>
      /// <returns>
      ///   viewer interface
      /// </returns>
      function CreateViewer : IInterface ; virtual; abstract;
    public
      /// <summary>
      ///   Platform specific bitmap object (like TBitmap for VCL).
      /// </summary>
      property NativeBitmap : TObject
                              read  fget_NativeBitmap
                              write fset_NativeBitmap ;

      /// <summary>
      ///   Bitmap width in pixels.
      /// </summary>
      property Width        : Integer
                              read  fget_Width ;
      /// <summary>
      ///   Bitmap height in pixels.
      /// </summary>
      property Height       : Integer
                              read  fget_Height ;

      /// <summary>
      ///   True if bitmap should be treated as premultipied. Important only
      ///   on some platforms.
      /// </summary>
      property Premultiplied  : Boolean
                                read  FPremultiplied
                                write FPremultiplied ;
  end;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Platform independent factory for platform dependent TGIS_Bitmap object.
  /// </summary>
  TGIS_BitmapFactory = {$IFDEF OXYGENE} public abstract {$ENDIF}class
    public

      /// <summary>
      ///   Create bitmap with a given size.
      /// </summary>
      /// <param name="_parent">
      ///   master TGIS_Bitmap object
      /// </param>
      /// <param name="_width">
      ///   width of bitmap in pixels
      /// </param>
      /// <param name="_height">
      ///   height of bitmap in pixels
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function DoCreate          ( const _parent : TGIS_Bitmap ;
                                   const _width  : Integer ;
                                   const _height : Integer
                                 ) : TGIS_BitmapAbstract ;
                                 overload; virtual; abstract;

      /// <summary>
      ///   Create bitmap from platform specific bitmap object.
      /// </summary>
      /// <param name="_parent">
      ///   master TGIS_Bitmap object
      /// </param>
      /// <param name="_bmp">
      ///   platform specific bitmap (like TBitmap for VCL)
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function DoCreateFromBitmap( const _parent : TGIS_Bitmap ;
                                   const _bmp    : TObject
                                 ) : TGIS_BitmapAbstract ;
                                 virtual; abstract;

      /// <summary>
      ///   Create bitmap from file.
      /// </summary>
      /// <param name="_parent">
      ///   master TGIS_Bitmap object
      /// </param>
      /// <param name="_path">
      ///   path to file
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function DoCreateFromFile  ( const _parent : TGIS_Bitmap ;
                                   const _path   : String
                                 ) : TGIS_BitmapAbstract ;
                                 virtual; abstract;

      /// <summary>
      ///   Create bitmap from platform stream.
      /// </summary>
      /// <param name="_parent">
      ///   master TGIS_Bitmap object
      /// </param>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function DoCreateFromStream( const _parent : TGIS_Bitmap ;
                                   const _stream : TObject
                                 ) : TGIS_BitmapAbstract ;
                                 virtual; abstract;

      /// <summary>
      ///   Create bitmap from resource.
      /// </summary>
      /// <param name="_parent">
      ///   master TGIS_Bitmap object
      /// </param>
      /// <param name="_ref">
      ///   module handle; to load DK standard resource use 0;
      ///   see remarks for TGIS_Bitmap.LoadFromResourceName for more
      /// </param>
      /// <param name="_name">
      ///   name of the resource
      /// </param>
      /// <returns>
      ///   created bitmap
      /// </returns>
      function DoCreateFromResource(
                                   const _parent : TGIS_Bitmap ;
                                   const _ref    : IntPtr ;
                                   const _name   : String
                                 ) : TGIS_BitmapAbstract ;
                                 virtual; abstract;

      /// <summary>
      ///   Provides platform specific bitmap format.
      /// </summary>
      /// <returns>
      ///   bitmap format
      /// </returns>
      function NativeFormat      : TGIS_BitmapFormat ;
                                 virtual; abstract;

      /// <summary>
      ///   Provides platform specific bitmap's line order.
      /// </summary>
      /// <returns>
      ///   bitmap line order
      /// </returns>
      function NativeLineOrder   : TGIS_BitmapLinesOrder ;
                                 virtual; abstract;
  end;


  /// <summary>
  ///   Platform independent Bitmap (same object for all platforms).
  /// </summary>
  TGIS_Bitmap = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FPath         : String ;
      FBitmap       : TGIS_BitmapAbstract ;
      FTransparent  : Boolean ;
      FPrecomputed  : Boolean ;
    private
      function  fget_IsEmpty       : Boolean ;
      function  fget_NativeBitmap  : TObject ;
      procedure fset_NativeBitmap  ( const _value : TObject
                                   ) ;
      function  fget_Premultiplied : Boolean ;
      procedure fset_Premultiplied ( const _value : Boolean
                                   ) ;
      function  fget_Width         : Integer ;
      function  fget_Height        : Integer ;
    private
      class function lanczos3(_dval : Single) : Single ;
      class function linear  (_dval : Single) : Single ;
    protected
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ; {$IFNDEF MANAGED} overload; {$ENDIF}

      /// <summary>
      ///   Create an instance with a given size.
      /// </summary>
      /// <param name="_width">
      ///   width of bitmap in pixels
      /// </param>
      /// <param name="_height">
      ///   height of bitmap in pixels
      /// </param>
      constructor Create            ( const _width  : Integer ;
                                      const _height : Integer
                                    ) ; overload;

      /// <summary>
      ///   Create an instance with a given size.
      /// </summary>
      /// <param name="_width">
      ///   width of bitmap in pixels
      /// </param>
      /// <param name="_height">
      ///   height of bitmap in pixels
      /// </param>
      /// <param name="_premult">
      ///   create a bitmap premutiplied; required and supported only on
      ///   selected platforms
      /// </param>
      constructor Create            ( const _width   : Integer ;
                                      const _height  : Integer ;
                                      const _premult : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Create an instance from exiting bitmap.
      /// </summary>
      /// <param name="_bmp">
      ///   source bitmap
      /// </param>
      constructor Create            ( const _bmp : TGIS_Bitmap
                                    ) ; overload;
    public
      /// <summary>
      ///   Assign bitmap from other instance.
      /// </summary>
      /// <param name="_source">
      ///   source bitmap
      /// </param>
      procedure Assign              (       _source : TGIS_Bitmap
                                    ) ;

      /// <summary>
      ///   Load bitmap from file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure LoadFromFile        ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Load bitmap from platform stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      procedure LoadFromStream      ( {$IFDEF GENXDK}
                                        const _stream : TStream
                                      {$ELSE}
                                        const _stream : TObject
                                      {$ENDIF}
                                    ) ;

      /// <summary>
      ///   Load bitmap from DK standard resource.
      /// </summary>
      /// <param name="_name">
      ///   resource name
      /// </param>
      procedure LoadFromResourceName( const _name     : String
                                    ) ; overload ;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Load bitmap from resource from specific module.
      /// </summary>
      /// <param name="_ref">
      ///   module handle; to load DK standard resource use 0;
      ///   see remarks for more
      /// </param>
      /// <param name="_name">
      ///   resource name
      /// </param>
      procedure LoadFromResourceName( const _ref      : IntPtr ;
                                      const _name     : String
                                    ) ; overload ;

      /// <summary>
      ///   Load bitmap from platform specific bitmap object.
      /// </summary>
      /// <param name="_bitmap">
      ///   platform specific bitmap (like TBitmap for VCL)
      /// </param>
      /// <param name="_path">
      ///   path to be embedded into bitmap (to be treated as a unique name)
      /// </param>
      procedure LoadFromBitmap      ( {$IFDEF GENXDK}
                                        const _bitmap : TBitmap ;
                                      {$ELSE}
                                        const _bitmap : TObject ;
                                      {$ENDIF}
                                      const _path   : String
                                    ) ;

      /// <summary>
      ///   Save bitmap to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure SaveToFile          ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Save bitmap to stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      procedure SaveToStream        ( const _stream : TObject
                                    ) ; overload;

      /// <summary>
      ///   Save bitmap to stream.
      /// </summary>
      /// <param name="_stream">
      ///   platform specific stream (like TStream for VCL)
      /// </param>
      /// <param name="_format">
      ///  pixel format
      /// </param>
      /// <param name="_subformat">
      ///  pixel subformat
      /// </param>
      /// <param name="_compression">
      ///  compression level
      /// </param>
      procedure SaveToStream        ( const _stream       : TObject ;
                                      const _format       : TGIS_PixelFormat ;
                                      const _subformat    : TGIS_PixelSubFormat ;
                                      const _compression  : Integer
                                    ) ; overload;
      /// <summary>
      ///   Make bitmap transparent by replacing all pixels with same color
      ///   as left-up corner with TGIS_Color.None color (full ARGB transparency)
      /// </summary>
      procedure MakeTransparent     ;

      /// <summary>
      ///   Add semitransparent "glowing" shadow to the bitmap.
      /// </summary>
      /// <param name="_color">
      ///   color of the glow; is semitransparent color provide, then glow will
      ///   start from semitransparency.
      /// </param>
      /// <param name="_size">
      ///   pixels with of the glow
      /// </param>
      procedure MakeGlowing          ( const _color        : TGIS_Color ;
                                       const _size         : Integer
                                     ) ;

      /// <summary>
      ///   Return raw pixels
      /// </summary>
      /// <param name="_pixels">
      ///   array of pixels to be returned
      /// </param>
      /// <param name="_writable">
      ///   if true then upon Unlock changes in locked pixel array will be
      ///   populated back to bitmap
      /// </param>
      /// <param name="_format">
      ///   format of locked pixel array; ARGB or BGRA
      /// </param>
      /// <param name="_order">
      ///   rows order: bottom-up or top-bottom
      /// </param>
      procedure LockPixels          ( var   _pixels   : TGIS_Pixels ;
                                      const _writable : Boolean     ;
                                      const _format   : TGIS_BitmapFormat ;
                                      const _order    : TGIS_BitmapLinesOrder
                                    ) ; overload;

      /// <summary>
      ///   Return read only raw pixels in native system format and native
      ///   system line order,
      /// </summary>
      /// <param name="_pixels">
      ///   array of pixels to be returned
      /// </param>
      procedure LockPixels          ( var   _pixels   : TGIS_Pixels
                                    ) ; overload;

      /// <summary>
      ///   Return raw pixels in native system format and native
      ///   system line order.
      /// </summary>
      /// <param name="_pixels">
      ///   array of pixels to be returned
      /// </param>
      /// <param name="_writable">
      ///   if true then upon Unlock changes in locked pixel array will be
      ///   populated back to bitmap
      /// </param>
      procedure LockPixels          ( var   _pixels   : TGIS_Pixels ;
                                      const _writable : Boolean
                                    ) ; overload;

      /// <summary>
      ///   Unlock pixels obtained by Lock() method. Post back any changes if
      ///   Lock was writable.
      /// </summary>
      procedure UnlockPixels        ;

      /// <summary>
      ///   Draw a shape on a bitmap (for internal
      ///   TGIS_Shape.PrepareContourInternal purposes).
      /// </summary>
      /// <param name="_shape">
      ///   shape to be drawn
      /// </param>
      /// <param name="_outline">
      ///   if True then outline of polygonal object will be drawn; otherwise
      ///   only interiors will be drawn
      /// </param>
      /// <param name="_scale">
      ///   shape size in pixels divided by the shape size in map units
      /// </param>
      /// <param name="_offset">
      ///   offset of shape within bitmap caused by map placement, margins etc.
      /// </param>
      /// <remarks>
      ///   Interiors are drawn with TGIS_Color.LightGray. Borders and lines
      ///   are drawn TGIS_Color.DimGray.
      ///   Default PPI for platfom is used (96 for Windows).
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure DrawShape           ( const _shape    :  TObject ;
                                      const _outline  :  Boolean ;
                                      var   _scale    :  Double  ;
                                      var   _offset   :  TPoint
                                    ) ; overload;

      /// <summary>
      ///   Draw a shape on a bitmap (for internal
      ///   TGIS_Shape.PrepareContourInternal purposes).
      /// </summary>
      /// <param name="_shape">
      ///   shape to be drawn
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_outline">
      ///   if True then outline of polygonal object will be drawn; otherwise
      ///   only interiors will be drawn
      /// </param>
      /// <param name="_scale">
      ///   shape size in pixels divided by the shape size in map units
      /// </param>
      /// <param name="_offset">
      ///   offset of shape within bitmap caused by map placement, margins etc.
      /// </param>
      /// <remarks>
      ///   Interiors are drawn with TGIS_Color.LightGray. Borders and lines
      ///   are drawn TGIS_Color.DimGray.
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure DrawShape           ( const _shape    :  TObject ;
                                      const _ppi      :  Integer ;
                                      const _outline  :  Boolean ;
                                      var   _scale    :  Double  ;
                                      var   _offset   :  TPoint
                                    ) ; overload;

      /// <summary>
      ///   Draw a shape on a bitmap (for internal
      ///   TGIS_Shape.PrepareContourInternal purposes).
      /// </summary>
      /// <param name="_shape">
      ///   shape to be drawn
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_outline">
      ///   if True then outline of polygonal object will be drawn; otherwise
      ///   only interiors will be drawn
      /// </param>
      /// <param name="_areacolor">
      ///   color for interiors
      /// </param>
      /// <param name="_linecolor">
      ///   color for lines and outlines
      /// </param>
      /// <param name="_scale">
      ///   pixels/unit scale of a shape
      /// </param>
      /// <param name="_offset">
      ///   offset of shape within bitmap caused by map placement, margins etc.
      /// </param>
      /// <remarks>
      ///   Interiors are drawn with TGIS_Color.LightGray. Borders and lines
      ///   are drawn TGIS_Color.DimGray.
      ///   <note type="note">
      ///     This method is for internal use only.
      ///   </note>
      /// </remarks>
      procedure DrawShape           ( const _shape    :  TObject ;
                                      const _ppi      :  Integer ;
                                      const _outline  :  Boolean ;
                                      const _areacolor:  TGIS_Color ;
                                      const _linecolor:  TGIS_Color ;
                                      var   _scale    :  Double  ;
                                      var   _offset   :  TPoint
                                    ) ; overload;

      /// <summary>
      ///   Draw rectangle part of a bitmap on a part of a bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_bmprect">
      ///  defines part of _bmp to be drawn
      /// </param>
      /// <param name="_drawrect">
      ///   part in this rectangle of original bitmap will be overwrite
      /// </param>
      procedure DrawBitmap         ( const _bmp      :  TGIS_Bitmap ;
                                     const _bmprect  :  TRect ;
                                     const _drawrect :  TRect
                                    ) ; overload;

      /// <summary>
      ///   Draw rectangle part of a bitmap on a part of a bitmap
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_bmppix">
      ///   pixels buffer from lock to be drawn
      /// </param>
      /// <param name="_bmprect">
      ///  defines part of _bmp to be drawn
      /// </param>
      /// <param name="_drawrect">
      ///   part in this rectangle of original bitmap will be overwrite
      /// </param>
      procedure DrawBitmap         ( const _bmp      :  TGIS_Bitmap ;
                                     const _bmppix   :  TGIS_Pixels ;
                                     const _bmprect  :  TRect ;
                                     const _drawrect :  TRect
                                    ) ; overload;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      /// <remarks>
      ///   Default PPI for platfom is used (96 for Windows).
      /// </remarks>
      procedure DrawSymbol          ( const _name     :  String
                                    ) ; overload ;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      /// <param name="_ppi">
      ///   pixels per inch
      /// </param>
      procedure DrawSymbol          ( const _name     :  String ;
                                      const _ppi      :  Integer
                                    ) ; overload ;

      /// <summary>
      ///   Draws symbol
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      ///</param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_areacolor">
      ///   color for interiors
      /// </param>
      /// <param name="_linecolor">
      ///   color for lines and outlines
      /// </param>
      procedure DrawSymbol          ( const _name     :  String ;
                                      const _ppi      :  Integer ;
                                      const _areacolor:  TGIS_Color ;
                                      const _linecolor:  TGIS_Color
                                    ) ; overload ;

      /// <summary>
      ///   Draws symbolk optimized to be used by a buttn glyph
      /// </summary>
      /// <param name="_symbol">
      ///   symbol to be drawn
      ///</param>
      /// <param name="_ppi">
      ///   pixels per inch; us3ed to calculate proper line sizes etc.
      ///</param>
      /// <param name="_color">
      ///   symbol color
      /// </param>
      /// <param name="_enabled">
      ///   if false draw glyph as disabled
      /// </param>
      procedure DrawGlyph           ( const _symbol   :  TObject ;
                                      const _ppi      :  Integer ;
                                      const _color    :  TGIS_Color;
                                      const _enabled  :  Boolean
                                    ) ; overload ;
      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Create a bitmap viewer
      /// </summary>
      /// <returns>
      ///   viewer interface
      /// </returns>
      function CreateViewer : IInterface ;

      /// <summary>
      ///   Native format of bitmaps (ARGB or ABGR)  for current platform
      /// </summary>
      /// <returns>
      ///   format value
      /// </returns>
      class function NativeFormat    : TGIS_BitmapFormat ;

      /// <summary>
      ///   Native line order of bitmaps (up-down or down-up) for current
      ///   platform.
      /// </summary>
      /// <returns>
      ///   line order value
      /// </returns>
      class function NativeLineOrder : TGIS_BitmapLinesOrder ;

      /// <summary>
      ///   Scales bitmap using filtering
      /// </summary>
      /// <param name="_srcBmp">
      ///   source bitmap
      /// </param>
      /// <param name="_srcWidth">
      ///   with of bitmap in _srcBmp
      /// </param>
      /// <param name="_srcHeight">
      ///   height of bitmap in _srcBmp
      /// </param>
      /// <param name="_dstBmp">
      ///   destination bitmap
      /// </param>
      /// <param name="_dstWidth">
      ///   with of output bitmap in _dstBmp
      /// </param>
      /// <param name="_dstHeight">
      ///   height of output bitmap in _dstBmp
      /// </param>
      /// <param name="_filtering">
      ///   scaling resampling (filtering) method
      /// </param>
      class procedure ScaleBitmap  ( const _srcBmp    : TGIS_Pixels ;
                                     const _srcWidth  : Integer ;
                                     const _srcHeight : Integer ;
                                     const _dstBmp    : TGIS_Pixels ;
                                     const _dstWidth  : Integer ;
                                     const _dstHeight : Integer ;
                                     const _filtering : TGIS_ScalingFilter
                                   ) ;

      /// <summary>
      ///   Test if bitmap is emptyt or nil
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to test
      /// </param>
      /// <returns>
      ///   True if bitmap is not allocated or is empty.
      /// </returns>
      class function IsNilOrEmpty  ( const _bmp : TGIS_Bitmap
                                   ) : Boolean ;
                                  {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      {$IFDEF GIS_PDK}
        /// <summary>
        ///   Export png as ready as arry of bytes in PNG format.
        /// </summary>
        /// <returns>
        ///   Arrya of bytes.
        /// </returns>
        function AsPng : TBytes ;
      {$ENDIF}
    public

      /// <summary>
      ///   True if bitmap is empty (has no NativeBitmap attached.
      /// </summary>
      property IsEmpty      : Boolean
                              read  fget_IsEmpty ;

      /// <summary>
      ///   Path to bitmap (exits always for bitmap created form files).
      /// </summary>
      property Path         : String
                              read  FPath ;

      /// <summary>
      ///   Platform specific bitmap object (like TBitmap for VCL).
      /// </summary>
      property NativeBitmap : {$ifndef GENXDK}
                                TObject
                              {$else}
                                TBitmap
                              {$endif}
                              read  fget_NativeBitmap
                              write fset_NativeBitmap ;

      /// <summary>
      ///   True if bitmap is transparent.
      /// </summary>
      property Transparent  : Boolean
                              read  FTransparent
                              write FTransparent ;

      /// <summary>
      ///   True if bitmap should be treated as premultipied. Important only
      ///   on some platforms.
      /// </summary>
      property Premultiplied: Boolean
                              read  fget_Premultiplied
                              write fset_Premultiplied ;

      /// <summary>
      ///   Bitmap width in pixels.
      /// </summary>
      property Width        : Integer
                              read  fget_Width ;

      /// <summary>
      ///   Bitmap height in pixels.
      /// </summary>
      property Height       : Integer
                              read  fget_Height ;
  end ;


  /// <summary>
  ///   Platform independent factory for platform dependent TGIS_Font object.
  /// </summary>
  TGIS_FontFactory = {$IFDEF OXYGENE} public abstract {$ENDIF}class
    public
      /// <summary>
      ///   Create font with a given specification.
      /// </summary>
      /// <param name="_name">
      ///   name of the font
      /// </param>
      /// <param name="_size">
      ///   size of the font in points
      /// </param>
      /// <param name="_style">
      ///   style of the font
      /// </param>
      /// <param name="_color">
      ///   color of the font
      /// </param>
      /// <returns>
      ///   created font
      /// </returns>
      function DoCreate          ( const _name  : String ;
                                   const _size  : Integer ;
                                   const _style : TGIS_FontStyles ;
                                   const _color : TGIS_Color
                                 ) : TGIS_Font ; virtual; abstract;

      /// <summary>
      ///   Create font from platform specific font object.
      /// </summary>
      /// <param name="_font">
      ///   platform specific font (like TFont for VCL)
      /// </param>
      /// <returns>
      ///   created font
      /// </returns>
      function DoCreateFromFont( const _font    : TObject
                                 ) : TGIS_Font ; virtual; abstract;
  end ;

  /// <summary>
  ///  Array of color map for ramps.
  /// </summary>
  TGIS_ColorMap = {$IFDEF OXYGENE} public {$ENDIF} record
    public
      /// <summary>
      ///   Range value related with RGB color, normalized to 0-100.
      /// </summary>
      &Index : Double ;

      /// <summary>
      ///   Color assigned to given range.
      /// </summary>
      RGB   : TGIS_Color ;
  end ;

  /// <summary>
  ///  Array of color map for ramps.
  /// </summary>
  TGIS_ColorMapEx = {$IFDEF OXYGENE} public {$ENDIF} record
    public
      /// <summary>
      ///   Class value related with ramp.
      /// </summary>
      &Index : Integer ;

      /// <summary>
      ///   Colors assigned to given class.
      /// </summary>
      Colors : TGIS_ColorArray ;
  end ;

  /// <summary>
  ///   Array of TGIS_ColorMap.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_ColorMapArray = {$IFDEF OXYGENE} public {$ENDIF} array of TGIS_ColorMap ;
  {$ELSE}
    {#typehint:array:TGIS_ColorMap}
    TGIS_ColorMapArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_ColorMap
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Array of TGIS_ColorMap.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_ColorMapExArray = {$IFDEF OXYGENE} public {$ENDIF} array of TGIS_ColorMapEx ;
  {$ELSE}
    {#typehint:array:TGIS_ColorMapEx}
    TGIS_ColorMapExArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
      // XDK stub definition
      public
        /// <summary>
        ///   Set array size.
        /// </summary>
        /// <param name="_size">
        ///   array size
        /// </param>
        procedure SetLength( const _size : Integer ) ;

        /// <summary>
        ///   Array size.
        /// </summary>
        property Length : Integer
          read  dummy ;

        /// <summary>
        ///   Array element value.
        /// </summary>
        /// <param name="_idx">
        ///   index of the element
        /// </param>
        property Value[ const _idx : Integer ] : TGIS_ColorMapEx
          read  dummy
          write dummy;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Color schema types.
  /// </summary>
  TGIS_ColorSchema = {$IFDEF OXYGENE} public {$IFDEF CLR} flags {$ENDIF} {$ENDIF} (
    /// <summary>
    /// Diverging type put equal emphasis on mid-range critical values and
    /// extremes at both ends of the data range.
    /// </summary>
    Diverging,

    /// <summary>
    /// Qualitative type is suited to representing nominal or categorical data.
    /// </summary>
    Qualitative,

    /// <summary>
    /// Sequential type is suited to ordered data that progress from low to high.
    /// </summary>
    Sequential,

    /// <summary>
    /// Other color ramps not matching the main categories.
    /// </summary>
    Miscellaneous
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   Platform independent set of font style definition.
  /// </summary>
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
    TGIS_ColorSchemas = public set of TGIS_ColorSchema ;
    {$ELSE}
    TGIS_ColorSchemas = public TGIS_ColorSchema ;
    {$ENDIF}
  {$ELSE}
    TGIS_ColorSchemas = set of TGIS_ColorSchema ;
  {$ENDIF}

  /// <summary>
  ///   Structure of a gradient color ramp.
  /// </summary>
  TGIS_GradientMap = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      /// <summary>
      ///   Unique name.
      /// </summary>
      FName    : String ;

      /// <summary>
      ///   Assigned colors array (range-color).
      /// </summary>
      FMap     : TGIS_ColorMapArray ;

      /// <summary>
      ///   Assigned colors array (class-colors).
      /// </summary>
      FSubMap  : TGIS_ColorMapExArray ;

      /// <summary>
      ///   Map type.
      /// </summary>
      FMapType : TGIS_ColorSchema ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function rampHasEqualIntervals( const _colorMapArr : TGIS_ColorMapArray
                                    ) : Boolean ;

      function getBaseColorMap      ( const _subClass : Integer
                                    ) : TGIS_ColorMapArray ; virtual ;

    public
      /// <summary>
      ///   Creates a new color ramp.
      /// </summary>
      constructor Create   ; overload ;

      /// <summary>
      ///   Creates a new color ramp.
      /// </summary>
      /// <param name="_name">
      ///   name of the map
      /// </param>
      /// <param name="_map">
      ///   array of ramp map (index-color)
      /// </param>
      /// <param name="_submap">
      ///   array of ramp map (class-colors)
      /// </param>
      /// <param name="_mapType">
      ///   type of color map
      /// </param>
      constructor Create      ( const _name     : String ;
                                const _map      : TGIS_ColorMapArray ;
                                const _submap   : TGIS_ColorMapExArray ;
                                const _mapType  : TGIS_ColorSchema
                              ) ; overload ;

      /// <summary>
      ///   Prepares a colormap as requested.
      /// </summary>
      /// <param name="_mode">
      ///   colormap mode (continuous by default or discrete)
      /// </param>
      /// <param name="_subClass">
      ///   if available, gets subclass of a ramp with specified, or the
      ///   possible nearest to specified, number of colors;
      ///   if 0, gets default colormap (this is the default parameter);
      /// </param>
      /// <param name="_reverse">
      ///   if True, reverses colors from colormap (False is the default parameter);
      /// </param>
      /// <returns>
      ///   Array of a colormap.
      /// </returns>
      function RealizeColorMap( const _mode     : TGIS_ColorMapMode
                                                = TGIS_ColorMapMode.Continuous ;
                                const _subClass : Integer = 0 ;
                                const _reverse  : Boolean = False
                              ) : TGIS_ColorMapArray ;
    public
      /// <summary>
      ///   Unique name.
      /// </summary>
      property Name    : String read FName ;

      /// <summary>
      ///   Assigned color array (range-color).
      /// </summary>
      property Map     : TGIS_ColorMapArray read FMap ;

      /// <summary>
      ///   Assigned color array (class-colors).
      /// </summary>
      property SubMap  : TGIS_ColorMapExArray read FSubMap ;

      /// <summary>
      ///   Map type.
      /// </summary>
      property MapType : TGIS_ColorSchema read FMapType ;
  end ;

  /// <summary>
  ///   Structure of a gradient color ramp.
  /// </summary>
  TGIS_GradientMapUnique = {$IFDEF OXYGENE} public {$ENDIF}
                                            class ( TGIS_GradientMap )
    private const
      // Least Common Multiple - LCM (1375, 3600)/1375/10 ~= 14 (next start)
      DEFAULT_CLASS_COUNT = 13 ;

    private
      FHMin : Double ;
      FHMax : Double ;
      FSMin : Double ;
      FSMax : Double ;
      FLMin : Double ;
      FLMax : Double ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function getBaseColorMap ( const _subClass : Integer
                               ) : TGIS_ColorMapArray ; override ;

    public
      /// <summary>
      ///   Creates a new color ramp.
      /// </summary>
      constructor Create ; overload ;

      /// <summary>
      ///   Creates a new dynamic color ramp in HSL color space.
      /// </summary>
      /// <param name="_name">
      ///   name of the map
      /// </param>
      /// <param name="_mapType">
      ///   type of color map
      /// </param>
      /// <param name="_hMin">
      ///   minimum hue, varies from 0 to 1
      /// </param>
      /// <param name="_hMax">
      ///   maximum hue, varies from 0 to 1
      /// </param>
      /// <param name="_sMin">
      ///   minimum saturation, varies from 0 to 1
      /// </param>
      /// <param name="_sMax">
      ///   maximum saturation, varies from 0 to 1
      /// </param>
      /// <param name="_lMin">
      ///   minimum lightness, varies from 0 to 1
      /// </param>
      /// <param name="_lMax">
      ///   maximum lightness, varies from 0 to 1
      /// </param>
      constructor Create ( const _name     : String ;
                           const _mapType  : TGIS_ColorSchema ;
                           const _hMin     : Double = 0 ;
                           const _hMax     : Double = 1 ;
                           const _sMin     : Double = 0 ;
                           const _sMax     : Double = 1 ;
                           const _lMin     : Double = 0 ;
                           const _lMax     : Double = 1
                         ) ; overload ;

    public
      /// <summary>
      ///   Minimum hue, varies from 0 to 1.
      /// </summary>
      property HMin : Double read FHMin ;

      /// <summary>
      ///   Maximum hue, varies from 0 to 1.
      /// </summary>
      property HMax : Double read FHMax ;

      /// <summary>
      ///   Minimum saturation, varies from 0 to 1.
      /// </summary>
      property SMin : Double read FSMin ;

      /// <summary>
      ///   Maximum saturation, varies from 0 to 1.
      /// </summary>
      property SMax : Double read FSMax ;

      /// <summary>
      ///   Minimum lightness, varies from 0 to 1.
      /// </summary>
      property LMin : Double read FLMin ;

      /// <summary>
      ///   Maximum lightness, varies from 0 to 1.
      /// </summary>
      property LMax : Double read FLMax ;
  end ;

  /// <summary>
  ///   Style of legend icons.
  /// </summary>
  TGIS_LegendIconStyle = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Default style.
    /// </summary>
    Default,
    /// <summary>
    ///   Area is drawn as a rectangle instead of triangle.
    /// </summary>
    Rectangular
  ) ;

const
  /// <summary>
  ///   Minimum movement threshold.
  /// </summary>
  GIS_GESTURE_MOVEMENT_THRESHOLD  = 20 ;

  /// <summary>
  ///   Maximum time interval to discover taps as double taps.
  /// </summary>
  GIS_GESTURE_TAPDOUBLE_THRESHOLD = 500 ;

  /// <summary>
  ///   Time interval of persistent tap to be interpreted as long tap.
  /// </summary>
  GIS_GESTURE_TAPLONG_THRESHOLD   = 700 ;

type
  /// <summary>
  ///   Platform independent gesture state.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    This method is used only for internal use on layer reading.
  ///    </note>
  /// </remarks>
  TGIS_GestureState = {$IFDEF OXYGENE} public {$ENDIF} record
    public
    /// <summary>
    ///   Keyboard special key status.
    /// </summary>

    Shift      : Boolean ;

    /// <summary>
    ///   Keyboard special key status.
    /// </summary>
    Alt        : Boolean ;

    /// <summary>
    ///   Keyboard special key status.
    /// </summary>
    Ctrl       : Boolean ;

    /// <summary>
    ///   Mouse button status.
    /// </summary>
    Left       : Boolean ;

    /// <summary>
    ///   Mouse button status.
    /// </summary>
    Right      : Boolean ;

    /// <summary>
    ///   Mouse button status.
    /// </summary>
    Middle     : Boolean ;

    /// <summary>
    ///   Finger touch source.
    /// </summary>
    Touch     : Boolean ;

    /// <summary>
    ///   Pen touch source.
    /// </summary>
    Pen       : Boolean ;

    /// <summary>
    ///   Position upon taping.
    /// </summary>
    DownX      : Double  ;

    /// <summary>
    ///   Position upon taping.
    /// </summary>
    DownY      : Double  ;

    /// <summary>
    ///   Tracking tap duration.
    /// </summary>
    DownTime   : Int64 ;

    /// <summary>
    ///   Tracking number of taps.
    /// </summary>
    DownCount  : Cardinal ;

    /// <summary>
    ///   Movement tracking displacement.
    /// </summary>
    MoveDelta  : Double  ;

    /// <summary>
    ///   Gesture state tracking.
    /// </summary>
    GestureCnt : Integer ;
  end ;

var

  /// <summary>
  ///   Global object for device dependent bitmap construction.
  /// </summary>
  BitmapHelper : TGIS_BitmapFactory ;

  /// <summary>
  ///   Global object for device dependent font construction.
  /// </summary>
  FontHelper   : TGIS_FontFactory ;

  /// <summary>
  ///   Global scale factor to scale GUI elements.
  /// </summary>
  GUIScale : Single = 1;

implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

type
  T_Contributor = record
    weight : Integer ;
    pixel  : Integer ;
  end ;

  T_Contributors = array of T_Contributor ;

  T_ContributorEntry = record
    num          : Integer ;
    contributors : T_Contributors ;
  end ;
  T_ContributorList = array of T_ContributorEntry;

  T_FilterFunction = function ( _val : Single) : Single of object ;

{$REGION 'TGIS_Color'}

  {$IFNDEF GENDOC}
  class operator TGIS_Color.Equal(
    _value1  : TGIS_Color ;
    _value2  : TGIS_Color
  ) : Boolean ;
  begin
    {$IFDEF GIS_NORECORDS}
      if assigned(_value1) and assigned(_value2) then
        Result := _value1.ARGB = _value2.ARGB
      else
        Result := False ;
    {$ELSE}
      Result := _value1.ARGB = _value2.ARGB
    {$ENDIF}
  end;

  class operator TGIS_Color.NotEqual(
    _value1  : TGIS_Color ;
    _value2  : TGIS_Color
  ) : Boolean ;
  begin
    {$IFDEF GIS_NORECORDS}
      if assigned(_value1) and assigned(_value2) then
        Result := _value1.ARGB <> _value2.ARGB
      else
        Result := True ;
    {$ELSE}
      Result := _value1.ARGB <> _value2.ARGB
    {$ENDIF}
  end;
  {$ENDIF}

  function TGIS_Color.fget_A
    : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
  begin
    Result := Byte(Integer( ARGB and $ff000000 ) shr 24) and $ff ;
  end;

  function TGIS_Color.fget_R
    : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
  begin
    Result := Byte(Integer( ARGB and $00ff0000 ) shr 16) and $ff ;
  end;

  function TGIS_Color.fget_G
    : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
  begin
    Result := Byte(Integer( ARGB and $0000ff00 ) shr 8) and $ff ;
  end;

  function TGIS_Color.fget_B
    : {$IFDEF JAVA} Integer {$ELSE} Byte {$ENDIF} ;
  begin
    Result := Byte( ARGB and $000000ff ) and $ff ;
  end;

  function TGIS_Color.fget_H
    : Single ;
  var
    rh,rs,rl : Double ;
  begin
    ToHSL( rh, rs, rl ) ;
    Result := rh ;
  end;

  function TGIS_Color.fget_S
    : Single ;
  var
    rh,rs,rl : Double ;
  begin
    ToHSL( rh, rs, rl ) ;
    Result := rs ;
  end;


  function TGIS_Color.fget_L
    : Single ;
  var
    rh,rs,rl : Double ;
  begin
    ToHSL( rh, rs, rl ) ;
    Result := rl ;
  end;


  class function TGIS_Color.FromRGB(
    const _r : Byte ;
    const _g : Byte ;
    const _b : Byte
  ) : TGIS_Color ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    {$IFDEF JAVA}
      Result.ARGB := Cardinal( Integer($ff000000) +
                               Integer(Integer($ff and _r) shl 16) +
                               Integer(Integer($ff and _g) shl 8) +
                               Integer($ff and _b)
                              ) ;
    {$ELSE}
      Result.ARGB := $ff000000 or ( _r shl 16 + _g shl 8 + _b ) ;
    {$ENDIF}
  end;

  class function TGIS_Color.FromRGB(
    const _value : Cardinal
  ) : TGIS_Color ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    Result.ARGB := $ff000000 or _value ;
  end;

  class function TGIS_Color.FromBGR(
    const _value : Cardinal
  ) : TGIS_Color ;
  var
    br, bg, bb : Byte ;
  begin
    bb := _value shr 16 and $000000ff ;
    bg := _value shr  8 and $000000ff ;
    br := _value        and $000000ff ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    {$IFDEF JAVA}
      Result.ARGB := Cardinal( $ff000000 +
                               Integer(Integer($ff and br) shl 16) +
                               Integer(Integer($ff and bg) shl 8) +
                               Integer($ff and bb)
                              ) ;
    {$ELSE}
      Result.ARGB := $ff000000 + br shl 16 + bg shl 8 + bb ;
    {$ENDIF}
  end;

  class function TGIS_Color.FromBGR(
    const _b : Byte ;
    const _g : Byte ;
    const _r : Byte
  ) : TGIS_Color ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    {$IFDEF JAVA}
      Result.ARGB := Cardinal( Integer($ff000000) +
                               Integer(Integer($ff and _r) shl 16) +
                               Integer(Integer($ff and _g) shl 8) +
                               Integer($ff and _b)
                              ) ;
    {$ELSE}
      Result.ARGB := $ff000000 or ( _r shl 16 + _g shl 8 + _b ) ;
    {$ENDIF}
  end;

  class function TGIS_Color.FromABGR(
    const _value : Cardinal
  ) : TGIS_Color ;
  var
    ba, br, bg, bb : Byte ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    ba := _value shr 24 and $000000ff ;
    bb := _value shr 16 and $000000ff ;
    bg := _value shr  8 and $000000ff ;
    br := _value        and $000000ff ;

    {$IFDEF JAVA}
      Result.ARGB := Cardinal( Integer(Integer($ff and ba) shl 24) +
                               Integer(Integer($ff and br) shl 16) +
                               Integer(Integer($ff and bg) shl 8) +
                               Integer($ff and bb)
                             ) ;
    {$ELSE}
      Result.ARGB := ba shl 24 + br shl 16 + bg shl 8 + bb ;
    {$ENDIF}
  end;

  class function TGIS_Color.FromARGB(
    const _a : Byte ;
    const _r : Byte ;
    const _g : Byte ;
    const _b : Byte
  ) : TGIS_Color ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    {$IFDEF JAVA}
      Result.ARGB := Cardinal( Integer(Integer($ff and _a) shl 24) +
                               Integer(Integer($ff and _r) shl 16) +
                               Integer(Integer($ff and _g) shl 8) +
                               Integer($ff and _b)
                              ) ;
    {$ELSE}
      Result.ARGB := _a shl 24 + _r shl 16 + _g shl 8 + _b ;
    {$ENDIF}
  end;

  class function TGIS_Color.FromARGB(
    const _value  : Cardinal
  ) : TGIS_Color ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Color ;
    {$ENDIF}

    Result.ARGB := _value ;
  end;

  class function TGIS_Color.FromHSL(
    const _h : Single ;
    const _s : Single ;
    const _l : Single
  ) : TGIS_Color ;
  begin
    Result := FromAHSL( 1.0, _h, _s, _l  ) ;
  end ;

  // utility function for FromAHSL which help in better inlining
  // especially on no Delphi platform
  function hue_to_color(
    _hue     : Double ;
    _m1, _m2 : Double
  ) : Byte ; {$IFDEF DCC} inline ; {$ENDIF}
  var
    dv : Double ;
  begin
    if      _hue < 0 then _hue := _hue + 1
    else if _hue > 1 then _hue := _hue - 1 ;

    if      6 * _hue < 1 then dv := _m1 + ( _m2 - _m1 ) * _hue * 6
    else if 2 * _hue < 1 then dv := _m2
    else if 3 * _hue < 2 then dv := _m1 + ( _m2 - _m1 ) * ( 2.0/3 - _hue ) * 6
    else                      dv := _m1 ;
    Result := RoundS( 255 * Max(0,Min(1,dv)) ) ;
  end ;

  class function TGIS_Color.FromAHSL(
    const _a : Single ;
    const _h : Single ;
    const _s : Single ;
    const _l : Single
  ) : TGIS_Color ;
  var
    m1 : Double ;
    m2 : Double ;

  var
    ba : Byte ;
    br : Byte ;
    bg : Byte ;
    bb : Byte;
  begin
    ba := RoundS( 255 * Max(0,Min(1,_a)) ) ;

    if _s = 0 then begin
      br := RoundS( 255 * Max(0,Min(1,_l)) ) ;
      bg := br ;
      bb := br ;
    end
    else begin
      if _l <= 0.5 then m2 := _l * ( 1 + _s )
                   else m2 := _l + _s - _l * _s ;
      m1 := 2 * _l - m2 ;

      br := hue_to_color( _h + 1.0/3, m1, m2 ) ;
      bg := hue_to_color( _h        , m1, m2 ) ;
      bb := hue_to_color( _h - 1.0/3, m1, m2 ) ;
    end ;

    Result := FromARGB( ba, br, bg, bb ) ;
  end ;

  class function TGIS_Color.FromHSV(
    const _h : Single ;
    const _s : Single ;
    const _v : Single
  ) : TGIS_Color ;
  begin
    Result := FromAHSV( 1.0, _h, _s, _v  ) ;
  end ;

  class function TGIS_Color.FromAHSV(
    const _a : Single ;
    const _h : Single ;
    const _s : Single ;
    const _v : Single
  ) : TGIS_Color ;
  var
    a, r, g, b : Byte ;
    f, p, q, t : Single ;
    i : Integer ;
  begin
    a := RoundS( 255 * Max( 0, Min( 1, _a ) ) ) ;
    r := 0 ;
    g := 0 ;
    b := 0 ;

    if _v = 0 then begin
      r := 0 ;
      g := 0 ;
      b := 0 ;
    end
    else begin
      f := _h * 60.0 ;
      i := FloorS( f ) ;
      f := f - i ;
      p := _v*( 1.0 - _s ) ;
      q := _v*( 1.0 - _s*f ) ;
      t := _v*( 1.0 - _s*( 1.0 - f ) );
      case i of
        0 : begin
              r := RoundS( 255*_v ) ;
              g := RoundS( 255*t  ) ;
              b := RoundS( 255*p  ) ;
            end ;
        1 : begin
              r := RoundS( 255*q  ) ;
              g := RoundS( 255*_v ) ;
              b := RoundS( 255*p  ) ;
            end ;
        2 : begin
              r := RoundS( 255*p  ) ;
              g := RoundS( 255*_v ) ;
              b := RoundS( 255*t  ) ;
            end ;
        3 : begin
              r := RoundS( 255*p  ) ;
              g := RoundS( 255*q  ) ;
              b := RoundS( 255*_v ) ;
            end ;
        4 : begin
              r := RoundS( 255*t  ) ;
              g := RoundS( 255*p  ) ;
              b := RoundS( 255*_v ) ;
            end ;
        5 : begin
              r := RoundS( 255*_v ) ;
              g := RoundS( 255*p  ) ;
              b := RoundS( 255*q  ) ;
            end ;
      end ;
    end ;

    Result := FromARGB( a, r, g, b ) ;
  end ;

  function TGIS_Color.ToRGB
    : Cardinal ;
  begin
    Result := Cardinal( Integer( Integer(R) shl 16 ) +
                        Integer( Integer(G) shl  8 ) +
                        B
                      ) ;
  end;

  function TGIS_Color.ToARGB
    : Cardinal ;
  begin
    Result := Cardinal( Integer( Integer(A) shl 24 ) +
                        Integer( Integer(R) shl 16 ) +
                        Integer( Integer(G) shl  8 ) +
                        B
                      ) ;
  end;

  function TGIS_Color.ToBGR
    : Cardinal ;
  begin
    Result := Cardinal( Integer( Integer(B) shl 16 ) +
                        Integer( Integer(G) shl  8 ) +
                        R
                      ) ;
  end;

  function TGIS_Color.ToABGR
    : Cardinal ;
  begin
    Result := Cardinal( Integer( Integer(A) shl 24 ) +
                        Integer( Integer(B) shl 16 ) +
                        Integer( Integer(G) shl  8 ) +
                        R
                      ) ;
  end;

  procedure TGIS_Color.ToHSL(
    var _h : Double ;
    var _s : Double ;
    var _l : Double
  ) ;
  var
    rp   : Double ;
    gp   : Double ;
    bp   : Double ;
    cmin : Double ;
    cmax : Double ;
    dlt  : Double ;
    dh    : Double ;
    ds    : Double ;
    dl    : Double ;
  begin
    rp := ( 1.0*self.R )/255 ;
    gp := ( 1.0*G )/255 ;
    bp := ( 1.0*B )/255 ;

    cmin := Min( rp, Min( gp, bp ) ) ;
    cmax := Max( rp, Max( gp, bp ) ) ;

    dlt := cmax - cmin ;

    dl := ( cmax + cmin ) / 2 ;
    dh := 0 ;
    if dlt = 0 then begin
      dh := 0 ;
      ds := 0 ;
    end
    else begin
      ds := dlt/( 1 - Abs( 2*dl - 1 ) ) ;
      if cmax = rp then begin
        dh := 60.0*( ( ( gp - bp )/dlt ) ) ;
        if dh < 0 then
          dh := 360.0 + dh ;
      end
      else
      if cmax = gp then
        dh := 60.0*( ( ( bp - rp )/dlt ) + 2.0 )
      else
      if cmax = bp then
        dh := 60.0*( ( ( rp - gp )/dlt ) + 4.0 ) ;
    end ;

    _h := dh/360.0 ;
    _s := ds ;
    _l := dl ;
  end ;

  procedure TGIS_Color.ToAHSL(
    var _a : Double ;
    var _h : Double ;
    var _s : Double ;
    var _l : Double
  ) ;
  begin
    ToHSL( _h, _s, _l ) ;

    _a := A / 255 ;
  end ;

  procedure TGIS_Color.ToHSV(
    var _h : Double ;
    var _s : Double ;
    var _v : Double
  ) ;
  var
    rp   : Double ;
    gp   : Double ;
    bp   : Double ;
    cmin : Double ;
    cmax : Double ;
    dlt  : Double ;
    dh    : Double ;
    ds    : Double ;
    v    : Double ;
  begin
    rp := ( 1.0*R )/255 ;
    gp := ( 1.0*G )/255 ;
    bp := ( 1.0*B )/255 ;

    cmin := Min( rp, Min( gp, bp ) ) ;
    cmax := Max( rp, Max( gp, bp ) ) ;

    dlt := cmax - cmin ;

    v := cmax ;
    dh := 0 ;
    if dlt = 0 then begin
      ds := 0 ;
      dh := 0 ;
    end
    else begin
      ds := dlt/v ;
      if cmax = rp then begin
        dh := 60.0*( ( ( gp - bp )/dlt ) ) ;
        if dh < 0 then
          dh := 360.0 + dh ;
      end
      else
      if cmax = gp then
        dh := 60.0*( ( ( bp - rp )/dlt ) + 2.0 )
      else
      if cmax = bp then
        dh := 60.0*( ( ( rp - gp )/dlt ) + 4.0 ) ;
    end ;

    _h := dh/360.0 ;
    _s := ds ;
    _v := v ;
  end ;

  procedure TGIS_Color.ToAHSV(
    var _a : Double ;
    var _h : Double ;
    var _s : Double ;
    var _v : Double
  ) ;
  begin
    ToHSV( _h, _s, _v ) ;

    _a := A / 255 ;
  end ;

  class function TGIS_Color.FromCMYK(
    const _c : Single ;
    const _m : Single ;
    const _y : Single ;
    const _k : Single
  ): TGIS_Color;
  begin
    Result := TGIS_Color.FromACMYK( 1, _c, _m, _y, _k ) ;
  end;

  class function TGIS_Color.FromACMYK(
    const _a : Single ;
    const _c : Single ;
    const _m : Single ;
    const _y : Single ;
    const _k : Single
  ): TGIS_Color;
  var
    a  : Single ;
    c  : Single ;
    m  : Single ;
    y  : Single ;
    k  : Single ;
    ba : Byte ;
    br : Byte ;
    bg : Byte ;
    bb : Byte ;
  begin
    a := Max( 0, Min( 1, _a ) ) ;
    c := Max( 0, Min( 1, _c ) ) ;
    m := Max( 0, Min( 1, _m ) ) ;
    y := Max( 0, Min( 1, _y ) ) ;
    k := Max( 0, Min( 1, _k ) ) ;

    ba := RoundS( 255 * a ) ;
    br := RoundS( 255 * (1-c) * (1-k) ) ;
    bg := RoundS( 255 * (1-m) * (1-k) ) ;
    bb := RoundS( 255 * (1-y) * (1-k) ) ;

    Result := TGIS_Color.FromARGB(ba, br, bg, bb ) ;
  end;

  procedure TGIS_Color.ToCMYK(
    var _c : Double ;
    var _m : Double ;
    var _y : Double ;
    var _k : Double
  ) ;
  var
    dr    : Double ;
    dg    : Double ;
    db    : Double ;
    k_factor : Double ;
  begin
    dr := R / 255.0 ;
    dg := G / 255.0 ;
    db := B / 255.0 ;

    _c := 0 ;
    _m := 0 ;
    _y := 0 ;
    _k := 1 - Max( Max( dr, dg ), db ) ;

    if _k < 1 then begin
      k_factor := 1 - _k ;
      _c := ( 1 - dr - _k ) / k_factor ;
      _m := ( 1 - dg  - _k ) / k_factor ;
      _y := ( 1 - db - _k ) / k_factor ;
    end ;
  end;

  procedure TGIS_Color.ToACMYK(
    var _a : Double ;
    var _c : Double ;
    var _m : Double ;
    var _y : Double ;
    var _k : Double
  ) ;
  begin
    _a := A / 255.0 ;
    ToCMYK( _c, _m, _y, _k ) ;
  end;

  function TGIS_Color.ToString(
    const _addAlpha : Boolean ;
    const _addHash  : Boolean
  ) : String ;
  begin
    if _addAlpha then
      Result := Format( '%.8x', [ARGB] )
    else
      Result := Format( '%.6x', [ToRGB] ) ;

    if _addHash then
      Result := '#' + Result ;
  end;

  function HexToGisColor(
    const _value : String ;
    var   _color : TGIS_Color
  ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  var
    hex_str : String ;
    sub_str : String ;
    str_len : Integer ;
    a, r, g, b : Byte ;
  begin
    Result := False ;

    hex_str := _value ;
    if hex_str.StartsWith( '#' ) then
      hex_str:= hex_str.Replace( '#', '' ) ;

    str_len := length( hex_str ) ;
    a := 255 ;
    r := 0 ;
    g := 0 ;
    b := 0 ;

    try
      if str_len = 3 then begin  // #fff
        sub_str := Copy( hex_str, StringFirst, 1 ) ;
        r := StrToInt( '$' + sub_str + sub_str ) ;
        sub_str := Copy( hex_str, StringFirst+1, 1 ) ;
        g := StrToInt( '$' + sub_str + sub_str ) ;
        sub_str := Copy( hex_str, StringFirst+2, 1 ) ;
        b := StrToInt( '$' + sub_str + sub_str ) ;
      end
      else if str_len = 6 then begin  // #ffffff
        sub_str := '$' + Copy( hex_str, StringFirst,2 ) ;
        r := StrToInt( sub_str ) ;
        sub_str := '$' + Copy( hex_str, StringFirst+2, 2 ) ;
        g := StrToInt( sub_str ) ;
        sub_str := '$' + Copy( hex_str, StringFirst+4, 2 ) ;
        b := StrToInt( sub_str ) ;
      end
      else if str_len = 8 then begin  // #ffffffff
        sub_str := '$' + Copy( hex_str, StringFirst, 2 ) ;
        a := StrToInt( sub_str ) ;
        sub_str := '$' + Copy( hex_str, StringFirst+2, 2 ) ;
        r := StrToInt( sub_str ) ;
        sub_str := '$' + Copy( hex_str, StringFirst+4, 2 ) ;
        g := StrToInt( sub_str ) ;
        sub_str := '$' + Copy( hex_str, StringFirst+6, 2 ) ;
        b := StrToInt( sub_str ) ;
      end
      else
        exit ;
    except
      exit ;
    end ;

    _color := TGIS_Color.FromARGB( a, r, g, b ) ;
    Result := True ;
  end;

  function ExtractComponents(
    const _value      : String ;
    const _components : TStringList
  ) : Boolean ;
  var
    i            : Integer ;
    a            : String ;
    start_pos    : Integer ;
    end_pos      : Integer ;
    color_args   : String ;
    colors       : TArray<String> ;
    colors_rgb_a : TArray<String> ;
  begin
    Result := False ;

    start_pos := Pos( '(', _value ) + 1 ;
    end_pos := Pos( ')', _value ) ;
    if ( end_pos < start_pos ) or ( end_pos * start_pos = 0 ) then
      exit ;

    color_args := Copy( _value, start_pos, end_pos - start_pos ) ;
    a := '';

    // rgb(34, 12, 64, 0.6);
    // rgb(255, 0, 0);
    // rgb(100%, 0%, 0%);
    // rgba(34, 12, 64, 0.6);
    // rgba(255, 0, 0, 0.5);
    // hsl(30, 100%, 50%, 0.6);
    // hsl(0, 100%, 50%);
    // hsla(30, 100%, 50%, 0.6);
    // hsla(0, 100%, 50%, 0.5);
    if color_args.Contains(',') then begin
      {$IFDEF JAVA OR ISLAND}
        colors := color_args.Split( ',' ).ToArray ;
      {$ELSE}
        colors := color_args.Split( [','] ) ;
      {$ENDIF}
    end
    else begin

      // rgb(34 12 64 / 0.6);
      // rgb(34.0 12 64 / 60%);
      // rgba(34 12 64 / 0.3);
      // rgba(34.6 12 64 / 30%);
      // hsl(30 100% 50% / 0.6);
      // hsl(30.0 100% 50% / 60%);
      // hsla(30 100% 50% / 0.6);
      // hsla(30.2 100% 50% / 60%);
      if color_args.Contains('/') then begin
        {$IFDEF JAVA OR ISLAND}
          colors_rgb_a := color_args.Split( '/').ToArray ;
        {$ELSE}
          colors_rgb_a := color_args.Split( ['/'] ) ;
        {$ENDIF}
        color_args := Trim( colors_rgb_a[0] ) ;
        a := Trim( colors_rgb_a[1] ) ;
      end ;

      // rgb(255 0 0);
      {$IFDEF JAVA OR ISLAND}
        colors := color_args.Split( ' ' ).ToArray ;
      {$ELSE}
        colors := color_args.Split( [' '] ) ;
      {$ENDIF}
    end ;

    // just collect color components
    for i := low( colors ) to high( colors ) do begin
      _components.Add( colors[i] )
    end ;

    // minimum 3 components are required
    if _components.Count < 3 then
      exit ;

    // add alpha if found
    if not IsStringEmpty( a ) then
      _components.Add( a ) ;

    Result := True ;
  end;

  type
    T_CssColorType = (
      RGB,
      RGBA,
      HSL,
      HSLA
    ) ;

  // always return floating value [0..1]
  function ConvertColorValue(
    const _value                : String ;
    const _normalization_factor : Double = 1.0
  ) : Double ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  var
    val : String ;
  begin
    if _value.Contains('%') then begin
      val := _value.Replace( '%', '') ;
      Result := DotStrToFloat( val ) / 100 ;
    end
    else begin
      Result := DotStrToFloat( _value ) / _normalization_factor ;
    end ;
  end;

  function CssColorToGisColor(
    const _value : String ;
    const _type  : T_CssColorType ;
    var   _color : TGIS_Color
  ) : Boolean ;
  var
    colors_count : Integer ;
    ab, r, g, b :  Byte ;
    ad, h, s, l : Double ;
    color_components : TStringList ;
  begin
    Result := False ;
    color_components := TStringList.Create ;
    try
      if not ExtractComponents( _value, color_components ) then
        exit ;

      colors_count := color_components.Count ;
      try
        case _type of
          T_CssColorType.RGB : begin
            if colors_count < 3 then
              exit ;

            r := RoundS( 255 * ConvertColorValue( color_components[0], 255) ) ;
            g := RoundS( 255 * ConvertColorValue( color_components[1], 255 ) ) ;
            b := RoundS( 255 * ConvertColorValue( color_components[2], 255 ) ) ;

            ab := 255 ;
            if colors_count = 4 then
              ab := RoundS( 255 * ConvertColorValue( color_components[3] ) ) ;

            _color := TGIS_Color.FromARGB( ab, r, g, b ) ;
          end;

          T_CssColorType.RGBA : begin
            if colors_count < 4 then
              exit ;

            r := RoundS( 255 * ConvertColorValue( color_components[0], 255 ) ) ;
            g := RoundS( 255 * ConvertColorValue( color_components[1], 255 ) ) ;
            b := RoundS( 255 * ConvertColorValue( color_components[2], 255 ) ) ;
            ab := RoundS( 255 * ConvertColorValue( color_components[3] ) ) ;

            _color := TGIS_Color.FromARGB( ab, r, g, b ) ;
          end;

          T_CssColorType.HSL : begin
            if colors_count < 3 then
              exit ;

            h := ConvertColorValue( color_components[0] , 360 ) ;
            s := ConvertColorValue( color_components[1] ) ;
            l := ConvertColorValue( color_components[2] ) ;

            ad := 1.0 ;
            if colors_count = 4 then
              ad := ConvertColorValue( color_components[3] ) ;

            _color := TGIS_Color.FromAHSL( ad, h, s, l ) ;
          end;

          T_CssColorType.HSLA : begin
            if colors_count < 4 then
              exit ;

            h := ConvertColorValue( color_components[0], 360 ) ;
            s := ConvertColorValue( color_components[1] ) ;
            l := ConvertColorValue( color_components[2] ) ;
            ad := ConvertColorValue( color_components[3] ) ;

            _color := TGIS_Color.FromAHSL( ad, h, s, l ) ;
          end ;
        end ;
      except
        exit ;
      end ;
    finally
      FreeObject( color_components ) ;
    end ;

    Result := True ;
  end;

  class function TGIS_Color.FromString(
    const _value : String
  ) : TGIS_Color;
  const
    CSS_FUNC_RGBA = 'rgba' ;
    CSS_FUNC_RGB  = 'rgb' ;
    CSS_FUNC_HSLA = 'hsla' ;
    CSS_FUNC_HSL  = 'hsl' ;
    procedure raise_exception( const value : String ) ;
    begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGVALUE ), value, 0 ) ;
    end ;
  begin
    if _value.StartsWith( CSS_FUNC_RGBA ) and CssColorToGisColor( _value, T_CssColorType.RGBA, Result ) then
      exit
    else if _value.StartsWith( CSS_FUNC_RGB ) and CssColorToGisColor( _value, T_CssColorType.RGB, Result ) then
      exit
    else if _value.StartsWith( CSS_FUNC_HSLA ) and CssColorToGisColor( _value, T_CssColorType.HSLA, Result ) then
      exit
    else if _value.StartsWith( CSS_FUNC_HSL ) and CssColorToGisColor( _value, T_CssColorType.HSL, Result ) then
      exit
    else if HexToGisColor( _value, Result ) then
      exit
    else
      raise_exception( _value ) ;
  end;

  class function TGIS_Color.TryFromString(
    const _value : String;
    var   _color     : TGIS_Color
  ) : Boolean ;
  begin
    Result := True ;
    try
      _color := FromString( _value ) ;
    except on E : EGIS_Exception do
      Result := False ;
    end ;
  end;

  class function TGIS_Color.fget_None
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $00000000 ) ;
  end;

  class function TGIS_Color.fget_Aqua
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff00ffff ) ;
  end;

  class function TGIS_Color.fget_Black
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff000000 ) ;
  end;

  class function TGIS_Color.fget_Blue
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff0000ff ) ;
  end;

  class function TGIS_Color.fget_DimGray
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff696969 ) ;
  end;

  class function TGIS_Color.fget_Fuchsia
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffff00ff ) ;
  end;

  class function TGIS_Color.fget_Green
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff008000 ) ;
  end;

  class function TGIS_Color.fget_Gray
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff808080 ) ;
  end;

  class function TGIS_Color.fget_LightGray
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffd3d3d3 ) ;
  end;

  class function TGIS_Color.fget_Lime
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff00ff00 ) ;
  end;

  class function TGIS_Color.fget_Maroon
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff800000 ) ;
  end;

  class function TGIS_Color.fget_Navy
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff000080 ) ;
  end;

  class function TGIS_Color.fget_Olive
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff808000 ) ;
  end;

  class function TGIS_Color.fget_Purple
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff800080 ) ;
  end;

  class function TGIS_Color.fget_Red
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffff0000 ) ;
  end;

  class function TGIS_Color.fget_Silver
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffc0c0c0 ) ;
  end;

  class function TGIS_Color.fget_Teal
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ff008080 ) ;
  end;

  class function TGIS_Color.fget_White
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffffffff ) ;
  end;

  class function TGIS_Color.fget_Yellow
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $ffffff00 ) ;
  end;

  class function TGIS_Color.fget_Crazy
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $00010816 ) ;
  end;

  class function TGIS_Color.fget_RenderColor
    : TGIS_Color ;
  begin
    Result := TGIS_Color.FromARGB( $00ff9933 ) ;
  end;
{$ENDREGION}

{$REGION 'TGIS_Font'}

  constructor TGIS_Font.Create ;
  begin
    inherited ;

    self.Name  := 'Arial'  ;
    self.Size  := 8       ;
    self.Style := GisGetEmptyFontStyle ;
    self.Color := TGIS_Color.Black ;
  end;

  procedure TGIS_Font.Assign(
    const _source : TGIS_Font
  ) ;
  begin
    assert( assigned( _source ) ) ;

    self.Name  := _source.Name  ;
    self.Size  := _source.Size  ;
    self.Style := _source.Style ;
    self.Color := _source.Color ;
  end;

  procedure TGIS_Font.LoadFromFont(
    const _font : TObject
  ) ;
  var
    fnt : TGIS_Font ;
  begin
    fnt := FontHelper.DoCreateFromFont( _font ) ;
    try
      Assign( fnt ) ;
    finally
      FreeObject( fnt ) ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_Pen'}
  constructor TGIS_Pen.Create ;
  begin
    inherited ;

    Width    := 1 ;
    Style    := TGIS_PenStyle.Solid ;
    Color    := TGIS_Color.Black ;
    LineCap  := TGIS_LineCap.Round ;
    LineJoin := TGIS_LineJoin.Round ;
  end;

  procedure TGIS_Pen.Assign( const _source : TGIS_Pen ) ;
  begin
    assert( assigned( _source ) ) ;

    self.Width    := _source.Width    ;
    self.Style    := _source.Style    ;
    self.LineCap  := _source.LineCap  ;
    self.Color    := _source.Color    ;
    self.LineJoin := _source.LineJoin ;
  end;
{$ENDREGION}

{$REGION 'TGIS_Brush'}
  constructor TGIS_Brush.Create ;
  begin
    inherited ;

    Style := TGIS_BrushStyle.Solid ;
    Color := TGIS_Color.Black ;
  end;

  procedure TGIS_Brush.Assign( const _source : TGIS_Brush ) ;
  begin
    assert( assigned( _source ) ) ;

    self.Style := _source.Style ;
    self.Color := _source.Color ;
  end;
{$ENDREGION}

{$REGION 'TGIS_Bitmap'}
constructor TGIS_Bitmap.Create ;
begin
  inherited Create ;

  FPath   := ''  ;
  FBitmap := nil ;
end ;


constructor TGIS_Bitmap.Create(
  const _width  : Integer ;
  const _height : Integer
) ;
begin
  inherited Create ;

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
      if not assigned (BitmapHelper) then
        BitmapHelper := TGIS_BitmapFactoryJava.create ;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF CLR}
    if not assigned( BitmapHelper ) then
      BitmapHelper := TGIS_BitmapFactoryCLR.Create ;
  {$ENDIF}

  FBitmap := BitmapHelper.DoCreate( self, _width, _height ) ;
end;

constructor TGIS_Bitmap.Create(
  const _width   : Integer ;
  const _height  : Integer ;
  const _premult : Boolean
) ;
begin
  inherited Create ;

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
      if not assigned (BitmapHelper) then
        BitmapHelper := TGIS_BitmapFactoryJava.create ;
    {$ENDIF}
  {$ENDIF}
  FBitmap := BitmapHelper.DoCreate( self, _width, _height ) ;
  FBitmap.Premultiplied := _premult ;
end;

constructor TGIS_Bitmap.Create(
  const _bmp : TGIS_Bitmap
) ;
begin
  inherited Create ;

  FPath := '' ;
  Assign( _bmp );
end ;

procedure TGIS_Bitmap.doDestroy ;
begin
  FreeObject( FBitmap ) ;

  inherited ;
end;

function TGIS_Bitmap.fget_IsEmpty
  : Boolean ;
begin
  Result := not assigned( FBitmap ) ;
end ;

function TGIS_Bitmap.fget_NativeBitmap
  : TObject ;
begin
  if not assigned( FBitmap ) then begin
    Result := nil ;
    exit ;
  end ;

  Result := FBitmap.NativeBitmap ;
end;

procedure TGIS_Bitmap.fset_NativeBitmap(
  const _value : TObject
) ;
begin
  FreeObject( FBitmap ) ;

  if assigned( _value ) then begin
     FBitmap := BitmapHelper.DoCreateFromBitmap( self, _value ) ;
  end ;

  FPath := '' ;
end ;

function TGIS_Bitmap.fget_Premultiplied
  : Boolean ;
begin
  if not assigned( FBitmap ) then begin
    Result := False ;
    exit ;
  end ;

  Result := FBitmap.Premultiplied ;
end;

procedure TGIS_Bitmap.fset_Premultiplied(
  const _value : Boolean
) ;
begin
  if assigned( FBitmap ) then
    FBitmap.Premultiplied := _value ;
end ;

function TGIS_Bitmap.fget_Width : Integer ;
begin
  if not assigned( FBitmap ) then begin
    Result := 0 ;
    exit ;
  end ;

  Result := FBitmap.Width ;
end ;


function TGIS_Bitmap.fget_Height : Integer ;
begin
  if not assigned( FBitmap ) then begin
    Result := 0 ;
    exit ;
  end ;

  Result := FBitmap.Height ;
end ;


procedure TGIS_Bitmap.Assign(
  _source : TGIS_Bitmap
) ;
begin
  FreeObject( FBitmap ) ;
  FPath := '' ;

  if assigned( _source ) then begin
    if not TGIS_Bitmap.IsNilOrEmpty( _source ) then begin
      FBitmap := BitmapHelper.DoCreateFromBitmap( self, _source.NativeBitmap ) ;
      FPath := _source.Path ;
    end
  end
end ;


procedure TGIS_Bitmap.LoadFromFile(
  const _path : String
) ;
begin
  FreeObject( FBitmap ) ;

  if not FileExists( _path ) then
    exit ;

  FBitmap := BitmapHelper.DoCreateFromFile( self, _path ) ;
  FPath := _path ;
end ;


procedure TGIS_Bitmap.LoadFromStream(
  const _stream : TObject
) ;
begin
  FreeObject( FBitmap ) ;

  FBitmap := BitmapHelper.DoCreateFromStream( self, _stream ) ;
  FPath := '' ;
end ;


procedure TGIS_Bitmap.LoadFromResourceName(
  const _name : String
) ;
begin
  FBitmap := BitmapHelper.DoCreateFromResource( self, 0, _name ) ;
end ;

procedure TGIS_Bitmap.LoadFromResourceName(
  const _ref  : IntPtr ;
  const _name : String
) ;
begin
  FBitmap := BitmapHelper.DoCreateFromResource( self,_ref, _name ) ;
end ;

procedure TGIS_Bitmap.LoadFromBitmap(
  const _bitmap : TObject ;
  const _path   : String
) ;
begin
  FreeObject( FBitmap ) ;

  FBitmap := BitmapHelper.DoCreateFromBitmap( self,_bitmap ) ;

  FPath := _path ;
end;


procedure TGIS_Bitmap.SaveToFile(
  const _path   : String
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.ToFile( _path );
end;

procedure TGIS_Bitmap.SaveToStream(
  const _stream : TObject
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.ToStream( _stream );
end;

procedure TGIS_Bitmap.SaveToStream(
  const _stream       : TObject ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.ToStream( _stream, _format, _subformat, _compression );
end;

procedure TGIS_Bitmap.MakeTransparent ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.MakeTransparent ;
end ;

procedure TGIS_Bitmap.MakeGlowing(
  const _color : TGIS_Color ;
  const _size  : Integer
) ;
var
  bmp_glow  : TGIS_Bitmap ;
  px_src    : TGIS_Pixels ;
  px_glow   : TGIS_Pixels ;
  x, y, xy  : Integer     ;
  c1, c2,c3 : TGIS_Color  ;
  a, a1, a2 : Integer     ;
  step      : Integer     ;
  da        : Integer     ;

  function val(
    const _val  : Integer;
    const _step : Integer
  ) : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  var
    d : Integer ;
  begin
    d := _val - _step ;
    if (d < 0) then
      d := 0;

    Result := d;
  end ;
begin
  assert( assigned( FBitmap ) ) ;
  if _size = 0 then exit ;

  step := _color.A div _size ;

  bmp_glow := TGIS_Bitmap.Create( Width, Height )  ;
  try
    self.LockPixels(
      px_src, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native
    ) ;
    bmp_glow.LockPixels(
      px_glow, True, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native
    ) ;
    try
      // upd-down
      for x := 0 to Width-1 do begin
        for y := 0 to Height-2 do begin
          xy := y*Width + x ;

          if px_src[ xy ] <> 0 then
            a1 := _color.A
          else
            a1 := 0 ;

          c2 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy ] ) ) ;
          a2 := val(c2.A, step);

          a := Max(a1, a2);
          if a = 0 then
            continue ;

            px_glow[xy+Width] := Integer(
                                   TGIS_Color.FromARGB(
                                     a,
                                     _color.R,
                                     _color.G,
                                     _color.B
                                   ).ARGB
                                 )
        end;
      end ;

      // down-up
      for x := 0 to Width-1 do begin
        for y :=Height-1 downto 1  do begin
          xy := y*Width + x ;

          if px_src[ xy ] <> 0 then
            a1 := _color.A
          else
            a1 := 0 ;

          c2 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy ] ) ) ;
          a2 := val(c2.A, step);

          a := Max(a1, a2);
          if a = 0 then
            continue ;

          c3 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy - Width ] ) ) ;
          if c3.A > a then
            continue ;

            px_glow[ xy - Width ] := Integer(
                                       TGIS_Color.FromARGB(
                                         a,
                                         _color.R,
                                         _color.G,
                                         _color.B
                                       ).ARGB
                                     ) ;
        end ;
      end;

      //right left
      for x := 0 to Width-2 do begin
        for y := 0 to Height-1   do begin
          xy := y*Width + x ;

          if px_src[ xy ] <> 0 then
            a1 := _color.A
          else
            a1 := 0 ;

          c2 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy ] ) ) ;
          a2 := val(c2.A, step);

          a := Max(a1, a2);
          if a = 0 then
            continue ;

          c3 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy + 1 ] ) ) ;
          if c3.A > a then
            continue ;

            px_glow[ xy + 1 ] := Integer(
                                   TGIS_Color.FromARGB(
                                     a,
                                     _color.R,
                                     _color.G,
                                     _color.B
                                   ).ARGB
                                 ) ;
        end;

      end;

      // left right
      for x := Width-1 downto 1 do begin
        for y := 0 to Height-1   do begin
          xy := y*Width + x ;

          if px_src[ xy ] <> 0 then
            a1 := _color.A
          else
            a1 := 0 ;

          c2 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy ] ) ) ;
          a2 := val(c2.A, step);

          a := Max(a1, a2);
          if a = 0 then
            continue ;

          c3 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy - 1 ] ) ) ;
          if c3.A > a then
            continue ;

            px_glow[ xy - 1 ] := Integer(
                                   TGIS_Color.FromARGB(
                                     a,
                                     _color.R,
                                     _color.G,
                                     _color.B
                                   ).ARGB
                                 ) ;

        end;

      end;

      // merge with original using precomputed bitmap calculations
      for x := 0 to Width-1 do begin
        for y :=0 to Height-1 do begin
          xy := y*Width +x ;
          c1 := TGIS_Color.FromARGB( Cardinal( px_src [ xy ] ) );
          c2 := TGIS_Color.FromARGB( Cardinal( px_glow[ xy ] ) );

          da := 255 - c1.A ;

          px_src[ xy ] := Integer(
                            TGIS_Color.FromARGB(
                              Byte( c1.A + c2.A * da div 255 ),
                              Byte( ( c1.R * c1.A div 255 + c2.R * da div 255) ),
                              Byte( ( c1.G * c1.A div 255 + c2.G * da div 255) ),
                              Byte( ( c1.B * c1.A div 255 + c2.B * da div 255) )
                            ).ARGB
                          ) ;

        end;
      end;
   finally
      self.UnlockPixels ;
      bmp_glow.UnlockPixels;
    end;
  finally
    FreeObject( bmp_glow );
  end;
end;



procedure TGIS_Bitmap.LockPixels(
  var   _pixels   : TGIS_Pixels ;
  const _writable : Boolean     ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.LockPixels( _pixels, _writable, _format, _order ) ;
end;

procedure TGIS_Bitmap.LockPixels(
  var   _pixels   : TGIS_Pixels
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.LockPixels( _pixels, False,
                      TGIS_BitmapFormat.Native,
                      TGIS_BitmapLinesOrder.Native
                    ) ;
end;

procedure TGIS_Bitmap.LockPixels(
  var   _pixels   : TGIS_Pixels ;
  const _writable : Boolean
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.LockPixels( _pixels, _writable,
                      TGIS_BitmapFormat.Native,
                      TGIS_BitmapLinesOrder.Native
                    ) ;
end;

procedure TGIS_Bitmap.UnlockPixels ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.UnlockPixels ;
end;

procedure TGIS_Bitmap.DrawShape(
  const _shape    :  TObject ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawShape( _shape, _outline, _scale, _offset )  ;
end;

procedure TGIS_Bitmap.DrawShape(
  const _shape    :  TObject ;
  const _ppi      :  Integer ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawShape( _shape, _ppi, _outline, _scale, _offset )  ;
end;

procedure TGIS_Bitmap.DrawShape(
  const _shape    :  TObject    ;
  const _ppi      :  Integer    ;
  const _outline  :  Boolean    ;
  const _areacolor:  TGIS_Color ;
  const _linecolor:  TGIS_Color ;
  var   _scale    :  Double     ;
  var   _offset   :  TPoint
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawShape( _shape, _ppi, _outline,
                    _areacolor, _linecolor,
                    _scale, _offset
                  )  ;
end;

procedure TGIS_Bitmap.DrawBitmap(
  const _bmp      :  TGIS_Bitmap ;
  const _bmprect  :  TRect ;
  const _drawrect :  TRect
) ;
begin
  DrawBitmap( _bmp, nil, _bmprect, _drawrect ) ;
end;

procedure TGIS_Bitmap.DrawBitmap(
  const _bmp      :  TGIS_Bitmap ;
  const _bmppix   :  TGIS_Pixels ;
  const _bmprect  :  TRect ;
  const _drawrect :  TRect
) ;
var
  iw, ih, it, il : Integer ;
  ow, oh, ot, ol : Integer ;
  i : Integer ;
  ipix, opix : TGIS_Pixels ;
  ispix, ospix : TGIS_Pixels ;
begin
  if not assigned( _bmp ) then
    exit ;
  if _bmprect.Top >= _bmp.Height then
    exit
  else
  if _bmprect.Top < 0 then
    it := 0
  else
    it := _bmprect.Top ;

  if _bmprect.Bottom < 0 then
    exit
  else
  if _bmprect.Bottom >  _bmp.Height then
    ih := _bmp.Height - it
  else
    ih := _bmprect.Bottom -it ;


  if _bmprect.Left >= _bmp.Width then
    exit
  else
  if _bmprect.Left < 0 then
    il := 0
  else
    il := _bmprect.Left ;

  if _bmprect.Right < 0 then
    exit
  else
  if _bmprect.Right >  _bmp.Width then
    iw := _bmp.Width - il
  else
    iw := _bmprect.Right -il ;


//draw area
  if _drawrect.Top >= Height then
    exit
  else
  if _drawrect.Top < 0 then
    ot := 0
  else
    ot := _drawrect.Top ;

  if _drawrect.Bottom < 0 then
    exit
  else
  if _drawrect.Bottom >  Height then
    oh := Height - ot
  else
    oh := _drawrect.Bottom -ot;


  if _drawrect.Left >= Width then
    exit
  else
  if _drawrect.Left < 0 then
    ol := 0
  else
    ol := _drawrect.Left ;

  if _drawrect.Right < 0 then
    exit
  else
  if _drawrect.Right >  Width then
    ow := Width - ol
  else
    ow := _drawrect.Right -ol;

  if _bmppix <> nil then
    ipix := _bmppix
  else begin
    SetLength(ipix, _bmp.Height*_bmp.Width) ;
    _bmp.LockPixels(ipix) ;
  end ;

  SetLength(opix, Height*Width) ;
  LockPixels(opix, True) ;
  if (iw <> ow) or (ih <> oh) then begin
  // scaling needed
    SetLength(ispix, iw*ih) ;
    SetLength(ospix, ow*oh) ;
    if iw = _bmp.Width then
      GisCopyPixels ( ipix, it*iw, ispix, 0, iw*ih)
    else begin
      for i := 0 to ih -1 do
        GisCopyPixels ( ipix,il + (it +i)*_bmp.Width, ispix, i*iw, iw) ;
    end;

    ScaleBitmap(ispix, iw, ih, ospix, ow, oh, TGIS_ScalingFilter.Lanczos3);
    ipix := nil ;
    ispix := nil ;

    ipix := ospix ;
    iw := ow ;
    il := 0 ;
    it := 0 ;
  end
  else begin
    iw := _bmp.Width ;
  end;

  if (ow = Width) and (ow = iw) then // solid input and draw area
      GisCopyPixels ( ipix, it*iw, opix, ot*ow, ow*oh)
  else begin
      for i := 0 to oh -1 do
        GisCopyPixels ( ipix,il + (it +i)*iw, opix,ol + (i +ot)*Width, ow) ;
  end;

  if _bmppix = nil then
    _bmp.UnlockPixels ;
  UnlockPixels ;
  opix := nil ;
  ipix := nil ;
end;

procedure TGIS_Bitmap.DrawSymbol(
  const _name     :  String
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawSymbol( _name )  ;
end;

procedure TGIS_Bitmap.DrawSymbol(
  const _name     :  String ;
  const _ppi      :  Integer
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawSymbol( _name, _ppi )  ;
end;

procedure TGIS_Bitmap.DrawSymbol(
  const _name     :  String     ;
  const _ppi      :  Integer    ;
  const _areacolor:  TGIS_Color ;
  const _linecolor:  TGIS_Color
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawSymbol( _name, _ppi, _areacolor, _linecolor )  ;
end;

procedure TGIS_Bitmap.DrawGlyph(
  const _symbol   :  TObject    ;
  const _ppi      :  Integer    ;
  const _color    :  TGIS_Color ;
  const _enabled  :  Boolean
) ;
begin
  assert( assigned( FBitmap ) ) ;
  FBitmap.DrawGlyph( _symbol, _ppi, _color, _enabled )  ;
end;

{$IFDEF GIS_PDK}
  function TGIS_Bitmap.AsPng : TBytes ;
  var
    strm : TMemoryStream ;
  begin
    strm := TMemoryStream.Create ;
    try
      SaveToStream( strm, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.PNG, 100 ) ;

      SetLength( Result, strm.Size ) ;

      Move( strm.Memory^, Result[0], strm.Size ) ;
    finally
      strm.Free ;
    end;
  end;
{$ENDIF}

function TGIS_Bitmap.CreateViewer : IInterface ;
begin
  Result := FBitmap.CreateViewer ;
end;

class function TGIS_Bitmap.NativeFormat
  : TGIS_BitmapFormat ;
begin
  Result := BitmapHelper.NativeFormat ;
end;

class function TGIS_Bitmap.NativeLineOrder
  : TGIS_BitmapLinesOrder ;
begin
  Result := BitmapHelper.NativeLineOrder ;
end;

class function TGIS_Bitmap.lanczos3(
  _dval : Single
) : Single ;

  function onSin(_dv: Single): Single ;
  begin
    if _dv <> 0 then begin
      _dv := _dv * Pi;
      Result := Sin(_dv) / _dv;
    end
    else
      Result := 1;
  end ;

begin
  if _dval < 0 then
    _dval := -_dval;

  if _dval < 3 then
    Result := onSin(_dval) * onSin(_dval / 3)
  else
    Result := 0;
end ;

class function TGIS_Bitmap.linear(
  _dval : Single
) : Single ;
begin
  if _dval < 0 then
    _dval := -_dval;

  if (_dval < 1.0) then
    Result := 1.0 - _dval
  else
    Result := 0.0;
end;

class procedure TGIS_Bitmap.ScaleBitmap(
  const _srcBmp     : TGIS_Pixels ;
  const _srcWidth   : Integer ;
  const _srcHeight  : Integer ;
  const _dstBmp     : TGIS_Pixels ;
  const _dstWidth   : Integer ;
  const _dstHeight  : Integer ;
  const _filtering  : TGIS_ScalingFilter
) ;
var
  scalex1,
  scaley1         : Single;
  scalex1_256,
  scaley1_256     : Single;
  i, j,
  k, n            : Integer;
  center          : Single;
  weight          : Integer;
  left,
  right           : Integer;
  wbmp            : TGIS_Pixels;
  srcBmp          : TGIS_Pixels ;
  dstBmp          : TGIS_Pixels ;
  contributorList : T_ContributorList;
  psourceLine_idx,
  pdestLine_idx   : Integer;
  pdestpixel_idx  : Integer;
  trpl            : Integer ;
  srcHeight,
  srcWidth,
  dstHeight,
  dstWidth    : Integer;
  currLineR   : array of Integer;
  currLineG   : array of Integer;
  currLineB   : array of Integer;
  currLineA   : array of Integer;

  sw          : Single ;
  prun_idx    : Integer ;

  cbrlst_len  : Integer ;
  cbr_nbr     : Integer ;
  cbr_fact_x  : Single ;
  cbr_fact_y  : Single ;
  src_dst     : Integer ;
  ff          : T_FilterFunction ;

    function trunc2byte(_iv : Integer) : Byte ;
    begin
      if _iv > 255 then
        Result := 255
      else
      if _iv < 0 then
        Result := 0
      else
        Result := Byte(_iv) ;
    end ;


    function apply_contribs(
      const _num          : Integer;
      const _contributors : T_Contributors
    ) : Integer ;
    var
      h           : Integer;
      red, green,
      blue, alpha : Integer;
      total,
      weight      : Integer;
      aweight     : Integer;
      pixel       : Cardinal;
      pcontr_idx  : Integer ;
    begin
      red   := 0;
      green := 0;
      blue  := 0;
      alpha := 0;
      total := 0;
      pcontr_idx := 0;

      for h := 0 to _num - 1 do begin
        weight := _contributors[pcontr_idx].weight;
        inc(total, weight);
        pixel := _contributors[pcontr_idx].pixel;

        if weight < 0 then
          aweight := weight * src_dst
        else
          aweight := weight ;

        inc(red, currLineR[pixel]   * weight);
        inc(green, currLineG[pixel] * weight);
        inc(blue, currLineB[pixel]  * weight);
        inc(alpha, currLineA[pixel] * aweight);

        inc(pcontr_idx) ;
      end ;

      if total = 0 then begin
        Result := Integer($FF and trunc2byte(red   shr 8)) shl 16 ;
        Result := Result or (Integer($FF and trunc2byte(green shr 8)) shl 8) ;
        Result := Result or (Integer($FF and trunc2byte(blue  shr 8))) ;
        Result := Result or ($FF and trunc2byte(alpha   shr 8)) shl 24 ;
      end
      else begin
        Result := Integer($FF and trunc2byte(red   div total)) shl 16 ;
        Result := Result or (Integer($FF and trunc2byte(green div total)) shl 8) ;
        Result := Result or (Integer($FF and trunc2byte(blue div total))) ;
        Result := Result or (Integer($FF and trunc2byte(alpha div total)) shl 24) ;
      end ;
    end ;

begin
  if (not assigned(_srcBmp)) or (not assigned(_dstBmp)) then exit ;

  if (_srcWidth = 0) or (_dstWidth = 0) then exit ;

  if _srcWidth > length(_srcBmp) then
    exit ;

  if _dstWidth > length(_dstBmp) then
    exit ;

  if  _dstWidth <= 6 then
    sw := 1
  else
    sw := 3 ;

  srcHeight := _srcHeight;
  srcWidth  := _srcWidth;
  dstHeight := _dstHeight;
  dstWidth  := _dstWidth;

  if (srcHeight = 0) or (srcWidth = 0) or
     (dstHeight = 0) or (dstWidth = 0) then Exit;

  if srcHeight > srcWidth then begin
    SetLength(currLineR, srcHeight);
    SetLength(currLineG, srcHeight);
    SetLength(currLineB, srcHeight);
    SetLength(currLineA, srcHeight);
  end
  else begin
    SetLength(currLineR, srcWidth);
    SetLength(currLineG, srcWidth);
    SetLength(currLineB, srcWidth);
    SetLength(currLineA, srcWidth);
  end ;

  scalex1 :=  dstWidth / srcWidth ;
  scaley1 :=  dstHeight / srcHeight ;

  if scaley1 < 0.1 then
    src_dst := -1
  else
    src_dst := 1 ;

  if (scalex1 = 0) or (scaley1 = 0) then
    exit ;

  cbrlst_len := dstWidth ;
  if dstWidth < dstHeight then
    cbrlst_len := dstHeight ;

  if srcHeight > srcWidth then
    SetLength( wbmp, cbrlst_len*srcHeight)
  else
    SetLength( wbmp, cbrlst_len*srcWidth) ;

  SetLength(contributorList, cbrlst_len);
  {$IFDEF GIS_NORECORDS}
    for i := 0 to cbrlst_len - 1 do
      contributorList[i] := new T_ContributorEntry ;
  {$ENDIF}

  if scalex1 < 1.0 then
    cbr_fact_x := sw / scalex1
  else
    cbr_fact_x := sw ;

  if scaley1 < 1.0 then
    cbr_fact_y := sw / scaley1
  else
    cbr_fact_y := sw ;

  if cbr_fact_x > cbr_fact_y then
    cbr_nbr  := TruncS(2 * cbr_fact_x + 1)
  else
    cbr_nbr  := TruncS(2 * cbr_fact_y + 1) ;

  scalex1_256 := scalex1 * 256 ;
  if _filtering = TGIS_ScalingFilter.Lanczos3 then
    {$IFDEF OXYGENE}
      ff := @lanczos3
    {$ELSE}
      ff := lanczos3
    {$ENDIF}
  else
    {$IFDEF OXYGENE}
      ff := @linear ;
    {$ELSE}
      ff := linear ;
    {$ENDIF}

  for i := 0 to dstWidth - 1 do begin
    contributorList[i].num := 0;
    SetLength(contributorList[i].contributors, cbr_nbr +3);
    {$IFDEF GIS_NORECORDS}
      for k := 0 to cbr_nbr +3-1 do
        contributorList[i].contributors[k] := new T_Contributor ;
    {$ENDIF}
    center := (i -scalex1*0.5)/ scalex1;
    left  := FloorS(center - cbr_fact_x);
    right := CeilS(center + cbr_fact_x);

    for j := left to right do begin
      if scalex1 < 1.0 then
        weight := RoundS(  ff( (center - j) * scalex1 ) * scalex1_256 )
      else
        weight := RoundS(ff(center - j) * 256);

      if weight <> 0 then begin
        if j < 0 then
          n := -j
        else
        if j >= srcWidth then
          n := srcWidth - j + srcWidth - 1
        else
          n := j;
        k := contributorList[i].num;
        inc(contributorList[i].num);

        contributorList[i].contributors[k].pixel := n;
        contributorList[i].contributors[k].weight := weight;
      end ;
    end ;
  end ;

  srcBmp := _srcBmp ;
  scaley1_256 := scaley1 * 256 ;
  try
    for k := 0 to srcHeight - 1 do begin
      psourceLine_idx := k*srcWidth;
      prun_idx := psourceLine_idx ;
      for i := 0 to srcWidth - 1 do begin
        currLineA[i] := ((srcBmp[prun_idx]) shr 24) and $FF ;
        currLineR[i] := ((srcBmp[prun_idx]) shr 16) and $FF ;
        currLineG[i] := ((srcBmp[prun_idx]) shr 08) and $FF ;
        currLineB[i] := ((srcBmp[prun_idx]) shr 00) and $FF ;
        inc(prun_idx);
      end ;

      pdestpixel_idx := cbrlst_len*k;
      for i := 0 to dstWidth - 1 do
        with contributorList[i] do begin
          wbmp[pdestpixel_idx] := apply_contribs(num, contributorList[i].contributors);
          inc(pdestpixel_idx) ;
        end ;
    end ;

    for i := 0 to dstHeight - 1 do begin
      contributorList[i].num := 0;
      SetLength(contributorList[i].contributors, cbr_nbr +3);
      {$IFDEF GIS_NORECORDS}
        for k := 0 to cbr_nbr +3-1 do
          contributorList[i].contributors[k] := new T_Contributor ;
      {$ENDIF}
      center := (i -scaley1*0.5)/ scaley1;
      left  := FloorS(center - cbr_fact_y);
      right := CeilS(center + cbr_fact_y);

      for j := left to right do begin
        if scaley1 < 1.0 then
          weight := RoundS(ff((center - j) * scaley1) * scaley1_256)
        else
          weight := RoundS(ff(center - j) * 256);

        if weight <> 0 then begin
          if j < 0 then
            n := -j
          else
          if j >= srcHeight then
            n := srcHeight - j + srcHeight - 1
          else
            n := j;
          k := contributorList[i].num;
          inc(contributorList[i].num);
          contributorList[i].contributors[k].pixel := n;
          contributorList[i].contributors[k].weight := weight;
        end ;
      end ;
    end ;

    dstBmp := _dstBmp ;
    psourceLine_idx := 0 ;

    for k := 0 to dstWidth - 1 do begin
      for i := 0 to srcHeight - 1 do begin
        psourceLine_idx := i * cbrlst_len;
        currLineA[i] := (wbmp[psourceLine_idx +k] shr 24) and $FF ;
        currLineR[i] := (wbmp[psourceLine_idx +k] shr 16) and $FF ;
        currLineG[i] := (wbmp[psourceLine_idx +k] shr 08) and $FF  ;
        currLineB[i] := (wbmp[psourceLine_idx +k] shr 00) and $FF   ;
      end ;

      for i := 0 to dstHeight - 1 do begin
        trpl := apply_contribs(contributorList[i].num, contributorList[i].contributors);
        pdestLine_idx := dstWidth * i ;
        dstBmp[pdestLine_idx +k] :=  trpl ;
      end ;
    end ;

    for i := cbrlst_len - 1 downto 0 do
      SetLength(contributorList[i].contributors, 0);
    SetLength(contributorList, 0);

  finally
    wbmp      := nil ;
    currLineA := nil ;
    currLineB := nil ;
    currLineG := nil ;
    currLineR := nil ;
  end ;
end ;

class function TGIS_Bitmap.IsNilOrEmpty(
  const _bmp : TGIS_Bitmap
) : Boolean ;
begin
  Result := not assigned( _bmp ) ;
  if not Result then
     Result := _bmp.IsEmpty ;
end;

{$ENDREGION}

{$REGION 'TGIS_GradientMap'}
constructor TGIS_GradientMap.Create(
  const _name     : String ;
  const _map      : TGIS_ColorMapArray ;
  const _submap   : TGIS_ColorMapExArray ;
  const _mapType  : TGIS_ColorSchema
) ;
begin
  inherited Create ;

  FName    := _name ;
  FMap     := _map ;
  FSubMap  := _submap ;
  FMapType := _mapType ;
end ;

constructor TGIS_GradientMap.Create ;
begin
  inherited Create ;

  FName    := '' ;
  FMap     := nil ;
  FSubMap  := nil ;
  FMapType := TGIS_ColorSchema.Sequential ;
end ;

function TGIS_GradientMap.rampHasEqualIntervals(
  const _colorMapArr : TGIS_ColorMapArray
) : Boolean ;
var
  i           : Integer ;
  delta_first : Double ;
  delta_next  : Double ;
begin
  Result := True ;

  if length( _colorMapArr ) <= 2 then
    exit ;

  delta_first := _colorMapArr[1].Index - _colorMapArr[0].Index ;

  for i := 1 to high( _colorMapArr )-1 do begin
    delta_next := _colorMapArr[i+1].Index - _colorMapArr[i].Index ;
    if not GisIsSameValue( delta_first, delta_next, 1 ) then begin
      Result := False ;
      exit ;
    end ;
  end ;
end;

function TGIS_GradientMap.getBaseColorMap(
  const _subClass : Integer
) : TGIS_ColorMapArray ;
var
  i, j      : Integer ;
  sub_len   : Integer ;
  sub_count : Integer ;
  sub_id    : Integer ;
begin
  Result := nil ;

  sub_id    := -1 ;
  sub_count := -1 ;

  // try use subclass
  if ( _subClass <> 0 ) and ( length( Map ) <> _subClass ) then begin
    for i := 0 to high( SubMap ) do begin
      sub_len := length( SubMap[i].Colors ) ;

      // find the specified subclass or the nearest
      if ( _subClass > 0 ) then begin
        if ( sub_len = _subClass ) then begin
          sub_id := i ;
          break ;
        end
        else if ( sub_len >= sub_count ) then begin
          sub_count := sub_len ;
          sub_id := i ;
          if ( sub_count > _subClass ) then
            break ;
        end ;
      end ;
    end ;

    if sub_id >= 0 then begin
      sub_len := length( SubMap[sub_id].Colors ) ;
      SetLength( Result, sub_len ) ;
      for j := 0 to sub_len - 1 do begin
        {$IFDEF GIS_NORECORDS}
          Result[j] := new TGIS_ColorMap ;
        {$ENDIF}
        Result[j].Index := j / (sub_len-1) * 100 ;
        Result[j].RGB   := SubMap[sub_id].Colors[j] ;
      end ;
    end ;
  end ;

  // use default representation
  if Result = nil then  begin
    {$IFDEF OXYGENE}
      Result := new array of TGIS_ColorMap( length( Map ) ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      for i := 0 to result.length - 1 do begin
        result[i] := new tatukgis.jdk.TGIS_ColorMap ;
        result[i].Index := Map[i].Index ;
        result[i].RGB := Map[i].RGB ;
      end;
    {$ENDIF}
    {$IFDEF CLR}
      &Array.Copy( Map, 0, Result, 0, length( Map ) ) ;
    {$ENDIF}
    {$IFDEF DCC}
      Result := copy( Map, 0, length( Map ) )
    {$ENDIF}
  end ;
end;

function TGIS_GradientMap.RealizeColorMap(
  const _mode     : TGIS_ColorMapMode ;
  const _subClass : Integer ;
  const _reverse  : Boolean
) : TGIS_ColorMapArray ;
var
  i                : Integer ;
  tmp              : TGIS_Color ;
  ramp_steps       : Integer ;
  curr, prev, next : Double ;
  left, right      : Double ;
  factor           : Double ;
  res              : TGIS_ColorMapArray ;
begin
  Result := getBaseColorMap( _subClass ) ;

  if _reverse then begin
    for i := low( Result ) to high( Result ) div 2 do begin
      tmp := Result[i].RGB ;
      Result[i].RGB := Result[high( Result ) - i].RGB ;
      Result[high( Result ) - i].RGB := tmp ;
    end ;
  end ;

  // prepare discrete representation of color ramp
  if _mode = TGIS_ColorMapMode.Discrete then begin

    ramp_steps := length( Result ) ;
    SetLength( res, 2 * ramp_steps ) ;

    // factor is used only in proportional split
    factor := ( ramp_steps - 1.0 ) / ramp_steps ;

    for i := 0 to ramp_steps - 1 do begin
      // proportional split
      if rampHasEqualIntervals( Result ) then begin
        left := factor * Result[i].Index ;
        if i = ramp_steps-1 then
          right := Result[i].Index
        else
          right := factor * Result[i+1].Index ;
      end
      // simple half split
      else begin
        curr := Result[i].Index ;

        if i = ramp_steps-1 then
          next := curr
        else
          next := Result[i+1].Index ;

        if i = 0 then
          prev := curr
        else
          prev := Result[i-1].Index ;

        left :=  ( prev + curr ) / 2 ;
        right := ( curr + next ) / 2 ;
      end ;

      {$IFDEF GIS_NORECORDS}
        res[2*i] := new TGIS_ColorMap ;
        res[2*i+1] := new TGIS_ColorMap ;
      {$ENDIF}

      res[2*i].Index := left ;
      res[2*i].RGB := Result[i].RGB ;
      res[2*i+1].Index := right ;
      res[2*i+1].RGB := Result[i].RGB ;
    end ;

    // copy discrete representation to Result
    SetLength( Result, 2 * ramp_steps ) ;
    for i := 0 to high(res) do begin
      {$IFDEF GIS_NORECORDS}
        Result[i] := new TGIS_ColorMap ;
      {$ENDIF}
      Result[i].Index := res[i].Index ;
      Result[i].RGB := res[i].RGB ;
    end ;
  end ;
end;
{$ENDREGION}

{$REGION 'TGIS_GradientMapUnique'}
constructor TGIS_GradientMapUnique.Create;
begin
  inherited ;

  FHMin := 0 ;
  FHMax := 1 ;
  FHMin := 0 ;
  FHMin := 1 ;
  FHMin := 0 ;
  FHMin := 1 ;
end;

constructor TGIS_GradientMapUnique.Create(
  const _name     : String ;
  const _mapType  : TGIS_ColorSchema ;
  const _hMin     : Double ;
  const _hMax     : Double ;
  const _sMin     : Double ;
  const _sMax     : Double ;
  const _lMin     : Double ;
  const _lMax     : Double
) ;
begin
  inherited Create( _name, nil, nil, _mapType ) ;

  FHMin := _hMin ;
  FHMax := _hMax ;
  FSMin := _sMin ;
  FSMax := _sMax ;
  FLMin := _lMin ;
  FLMax := _lMax ;

  // gradient map need to have at least 1 color
  // to determine font color in the TGIS_ColorRampComboBox
  FMap := getBaseColorMap( 1 ) ;
end;

function TGIS_GradientMapUnique.getBaseColorMap(
  const _subClass : Integer
) : TGIS_ColorMapArray ;
var
  i, n_class           : Integer ;
  h, s, l, h_offset    : Double ;
  h_min_deg, h_max_deg : Double ;
  GOLDEN_ANGLE         : Double ;
  delta                : Double ;
  color                : TGIS_Color ;
begin
  Result := nil ;

  if ( _subClass = 0 ) or ( _subClass = -1 ) then
    n_class := DEFAULT_CLASS_COUNT
  else if ( _subClass < -1 ) then
    exit
  else
    n_class := _subClass ;

  SetLength( Result, n_class ) ;

  if ( n_class = 1 ) then
    delta := 0
  else
    delta := 100 / ( n_class - 1 ) ;

  //? randomize, randseed

  // working in degrees 0..360
  h_offset := GetRandom * 360 ;
  h_min_deg := HMin * 360 ;
  h_max_deg := HMax * 360 ;

  // golden angle ~137.5
  GOLDEN_ANGLE := 180 * ( 3 - Sqrt(5) ) ;

  for i := 0 to n_class-1 do begin
    h := h_offset + ( i * GOLDEN_ANGLE ) ;
    // do not exceed 360 degrees
    h := h - 360 * FloorS( h/360 ) ;

    // hue also may be limited
    h := h_min_deg + ( h_max_deg - h_min_deg ) * h/360 ;
    s := GetRandom * ( SMax - SMin ) + SMin ;
    l := GetRandom * ( LMax - LMin ) + LMin ;

    color := TGIS_Color.FromHSL( h/360, s, l) ;

    {$IFDEF GIS_NORECORDS}
      Result[i] := new TGIS_ColorMap ;
    {$ENDIF}
    Result[i].Index := i * delta ;
    Result[i].RGB := color ;
  end ;
end;
{$ENDREGION}

//==================================== END =====================================

end.
