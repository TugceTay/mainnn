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
  Number of declarations which makes FireMonkey more compatible with VCL.
}

{$IFDEF DCC}
  unit GisTypesUI ;
  {$HPPEMIT '#pragma link "GisTypesUI"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.Math,
    System.SysUtils,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisRtl ;
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
    ///   This method forcess the hue for the color to go through 360.
    /// </summary>
    HSL360,

    /// <summary>
    ///   Alpha, Hue, Chroma, Luminance components interpolation.
    /// </summary>
    HCL
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
      ///   Retrieves the alpha (A), hue (H), chroma (C) and luminance (L)
      ///   components of the current color.
      /// </summary>
      /// <param name="_a">
      ///   alpha, varies from 0 to 1
      /// </param>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_c">
      ///   chroma, varies from 0 to 1.5
      /// </param>
      /// <param name="_l">
      ///   luminance, varies from 0 to 1
      /// </param>
      procedure ToAHCL         ( var _a : Double ;
                                 var _h : Double ;
                                 var _c : Double ;
                                 var _l : Double
                               ) ;

      /// <summary>
      ///   Retrieves the alpha (A), hue (H), chroma (C) and luminance (L)
      ///   components of the current color.
      /// </summary>
      /// <param name="_h">
      ///   hue, varies from 0 to 1
      /// </param>
      /// <param name="_c">
      ///   chroma, varies from 0 to 1.5
      /// </param>
      /// <param name="_l">
      ///   luminance, varies from 0 to 1
      /// </param>
      procedure ToHCL          ( var _h : Double ;
                                 var _c : Double ;
                                 var _l : Double
                               ) ;

      /// <summary>
      ///   Creates a color using the HCL color model.
      /// </summary>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_c">
      ///   chroma; expected value: 0..1.5
      /// </param>
      /// <param name="_l">
      ///   luminance; expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromHCL   ( const _h     : Double ;
                                 const _c     : Double ;
                                 const _l     : Double
                               ) : TGIS_Color ; static ;

      /// <summary>
      ///   Creates a transparent color using the HCL color model.
      /// </summary>
      /// <param name="_a">
      ///   alpha; expected value: 0..1
      /// </param>
      /// <param name="_h">
      ///   hue; expected value: 0..1
      /// </param>
      /// <param name="_c">
      ///   chroma; expected value: 0..1.5
      /// </param>
      /// <param name="_l">
      ///   luminance; expected value: 0..1
      /// </param>
      /// <returns>
      ///   Created color.
      /// </returns>
      class function FromAHCL  ( const _a     : Double ;
                                 const _h     : Double ;
                                 const _c     : Double ;
                                 const _l     : Double
                               ) : TGIS_Color ; static ;

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
  ///   Array of Single.
  /// </summary>
  {$IFNDEF GEN_ARRAY_WRAPPER}
    TGIS_DashArray = {$IFDEF OXYGENE} public {$ENDIF} Array of Single;
  {$ELSE}
    {#typehint:array:Single}
    TGIS_DashArray = {$IFNDEF GENPDK} class {$ELSE} record {$ENDIF}
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
        property Value[ const _idx : Integer ] : Single
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
      ///   End cap style.
      /// </summary>
      LineDash : TGIS_DashArray ;

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

  {#gendoc:hide:GENSCR}
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   TGIS_Bitmap underlaying native bitmap type
  /// </summary>
  TGIS_BitmapType = {$IFDEF OXYGENE} public {$ENDIF} (
    {$IFDEF JAVA}
      /// <summary>
      ///   Java bitmap.
      /// </summary>
      Java
    {$ENDIF}
    {$IFDEF CLR}
      /// <summary>
      ///   CLR bitmap.
      /// </summary>
      CLR,
      /// <summary>
      ///   Skia Sharp bitmap.
      /// </summary>
      Skia,
      /// <summary>
      ///   D2D bitmap.
      /// </summary>
      D2D,
      /// <summary>
      ///   WPF bitmap.
      /// </summary>
      WPF
    {$ENDIF}
    {$IFDEF DCC}
      /// <summary>
      ///   VCL bitmap.
      /// </summary>
      VCL,
      /// <summary>
      ///   FMX bitmap.
      /// </summary>
      FMX,
      /// <summary>
      ///   Skia image.
      /// </summary>
      Skia,
      /// <summary>
      ///   D2D bitmap.
      /// </summary>
      D2D
    {$ENDIF}
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
      function  fget_PPI          : Integer ; virtual; abstract;
      procedure fset_PPI          ( const _value : Integer
                                  ) ; virtual; abstract;
      function  fget_Data         : TObject ; virtual; abstract;
      procedure fset_Data         ( const _value : TObject
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
      ///   Return platform specific bitmap object (like TBitmap for VCL).
      /// </summary>
      /// <param name="_target">
      ///   optional platform specific object which modifies returned data
      /// </param>
      /// <returns>
      ///   bitmap object
      /// </returns>
      {$IFDEF CLR}
      function  GetData           ( const _target : TObject = nil
                                  ) : TObject ; virtual ;
      {$ELSE}
      function  GetData           ( const _target : IInterface = nil
                                  ) : TObject ; virtual ;
      {$ENDIF}

      /// <summary>
      ///   Save bitmap to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure   ToFile          ( const _path   : String
                                  ) ; overload; virtual; abstract;

      /// <summary>
      ///   Save bitmap to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
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
      procedure   ToFile          ( const _path         : String ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; overload; virtual; abstract;

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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Clear the bitmap with a color
      /// </summary>
      /// <param name="_color">
      ///   the color to clear with
      /// </param>
      procedure Clear             ( const _color       : TGIS_Color
                                  ) ; virtual; abstract;

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
      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Platform specific bitmap object (like TBitmap for VCL).
      /// </summary>
      property Data         : TObject
                              read  fget_Data
                              write fset_Data ;

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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Bitmap resolution.
      /// </summary>
      property PPI          : Integer
                              read  fget_PPI
                              write fset_PPI ;

      /// <summary>
      ///   True if bitmap should be treated as premultipied. Important only
      ///   on some platforms.
      /// </summary>
      property Premultiplied  : Boolean
                                read  FPremultiplied
                                write FPremultiplied ;
  end;

  {gendoc:hide:GENXDK}
  /// <summary>
  ///   Platform independent factory for platform dependent TGIS_Bitmap object.
  /// </summary>
  TGIS_BitmapFactory = {$IFDEF OXYGENE} public abstract {$ENDIF} class
    public

      {#gendoc:hide:GENXDK}
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

      {#gendoc:hide:GENXDK}
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

      {#gendoc:hide:GENXDK}
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

      {#gendoc:hide:GENXDK}
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

      {#gendoc:hide:GENXDK}
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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Provides platform specific bitmap's type.
      /// </summary>
      /// <returns>
      ///   bitmap type
      /// </returns>
      function BitmapType        : TGIS_BitmapType ;
                                 virtual; abstract;
  end;


  /// <summary>
  ///   Basic bitmap factory class.
  /// </summary>
  TGIS_BitmapFactoryClass = {$IFDEF OXYGENE} public {$ENDIF}
                            class of TGIS_BitmapFactory ;


  /// <summary>
  ///   Platform independent Bitmap (same object for all platforms).
  /// </summary>
  TGIS_Bitmap = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FPath         : String ;
      FBitmapFactory: TGIS_BitmapFactory ;
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
      function  fget_PPI           : Integer ;
      procedure fset_PPI           ( const _value : Integer
                                   ) ;

    private
      procedure checkFBitmap;
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
      ///   Create an instance with a given size with a given bitmap factory.
      /// </summary>
      /// <param name="_factory">
      ///   bitmap data factory; if null then default one will be used
      /// </param>
      constructor Create            ( const _factory : TGIS_BitmapFactory
                                    ) ; overload;

      /// <summary>
      ///   Create an instance with a given size with a given bitmap factory.
      /// </summary>
      /// <param name="_width">
      ///   width of bitmap in pixels
      /// </param>
      /// <param name="_height">
      ///   height of bitmap in pixels
      /// </param>
      /// <param name="_factory">
      ///   bitmap data factory; if null then default one will be used
      /// </param>
      constructor Create            ( const _width   : Integer ;
                                      const _height  : Integer ;
                                      const _factory : TGIS_BitmapFactory
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
                                    ) ; overload;

     {#gendoc:hide:GENSCR}
     {#gendoc:hide:GENXDK}
     {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Save bitmap to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
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
      procedure SaveToFile          ( const _path         : String ;
                                      const _format       : TGIS_PixelFormat ;
                                      const _subformat    : TGIS_PixelSubFormat ;
                                      const _compression  : Integer
                                    ) ; overload;

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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Clear the bitmap with a color
      /// </summary>
      /// <param name="_color">
      ///   the color to clear with
      /// </param>
      procedure Clear                ( const _color        : TGIS_Color
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

      {#gendoc:hide}
      /// <summary>
      ///   Return the bitmap data in the specified type.
      /// </summary>
      /// <param name="_factory">
      ///   factory determines data type
      /// </param>
      /// <returns>
      ///   returned data
      /// </returns>
      {$IFDEF CLR}
      function  GetData             ( const _factory : TGIS_BitmapFactory;
                                      const _target  : TObject = nil
                                    ) : TObject ;
      {$ELSE}
      function  GetData             ( const _factory : TGIS_BitmapFactory;
                                      const _target  : IInterface = nil
                                    ) : TObject ;
      {$ENDIF}

      {#gendoc:hide}
      /// <summary>
      ///   Save the bitmap data.
      /// </summary>
      /// <param name="_factory">
      ///   factory determines data type
      /// </param>
      /// <param name="_data">
      ///   data to save
      /// </param>
      procedure SetData             ( const _factory  : TGIS_BitmapFactory ;
                                      const _data     : TObject
                                    ) ;

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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Provides platform specific bitmap's type.
      /// </summary>
      /// <returns>
      ///   bitmap type
      /// </returns>
      function BitmapType            : TGIS_BitmapType ;

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
      ///   Bitmap factory used to produce the bitmap data.
      /// </summary>
      property BitmapFactory : TGIS_BitmapFactory
                              read  FBitmapFactory ;

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

      {#gendoc:hide:GENSCR}
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Resolution of the bitmap.
      /// </summary>
      property PPI          : Integer
                              read  fget_PPI
                              write fset_PPI ;
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
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      /// <summary>
      ///   Current color index of the color ramp.
      /// </summary>
      FColorIndex : Integer ;

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
      ///   Generates the next color from a default color ramp.
      /// </summary>
      /// <returns>
      ///   color
      /// </returns>
      /// <remarks>
      ///   If the method reaches the last color in the color ramp,
      ///   it returns TGIS_Color.None.
      ///   Use Reset method to start over.
      /// </remarks>
      function NextColor : TGIS_Color ; virtual ;

      /// <summary>
      ///   Resets the internal color index.
      /// </summary>
      /// <remarks>
      ///   Next method will return the first color from a default color ramp.
      /// </remarks>
      procedure Reset;

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
    // Golden Angle: 180 * ( 3 - Sqrt(5) )
    GOLDEN_ANGLE = 137.5077640500378546463487 ;  // OEIS: A096627

  private
    FAMin       : Double ;
    FAMax       : Double ;
    FBMin       : Double ;
    FBMax       : Double ;
    FCMin       : Double ;
    FCMax       : Double ;
    FHOffset    : Double ;
    FColorSpace : TGIS_ColorInterpolationMode ;

  private
    function fget_HMin : Double ;
    function fget_HMax : Double ;

    function fget_SMin : Double ;
    function fget_SMax : Double ;

    function fget_LMin : Double ;
    function fget_LMax : Double ;

    function fget_CMin : Double ;
    function fget_CMax : Double ;

    function fget_RMin : Byte ;
    function fget_RMax : Byte ;

    function fget_GMin : Byte ;
    function fget_GMax : Byte ;

    function fget_BMin : Byte ;
    function fget_BMax : Byte ;

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

    /// <summary>
    ///   Creates a new dynamic color ramp in specified color space.
    /// </summary>
    /// <param name="_name">
    ///   name of the map
    /// </param>
    /// <param name="_mapType">
    ///   type of color map
    /// </param>
    /// <param name="_aMin">
    ///   minimum value of the first color component
    /// </param>
    /// <param name="_aMax">
    ///   maximum value of the first color component
    /// </param>
    /// <param name="_bMin">
    ///   minimum value of the second color component
    /// </param>
    /// <param name="_bMax">
    ///   maximum value of the second color component
    /// </param>
    /// <param name="_cMin">
    ///   minimum value of the third color component
    /// </param>
    /// <param name="_cMax">
    ///   maximum value of the third color component
    /// </param>
    /// <param name="_colorSpace">
    ///   color space
    /// </param>
    /// <remarks>
    ///   The color components order depends on the specified _colorSpace,
    ///   e.g. for HSV, the first is Hue, the second is Saturation, and the third is Lightness.
    /// </remarks>
    constructor Create ( const _name       : String ;
                         const _mapType    : TGIS_ColorSchema ;
                         const _aMin       : Double ;
                         const _aMax       : Double ;
                         const _bMin       : Double ;
                         const _bMax       : Double ;
                         const _cMin       : Double ;
                         const _cMax       : Double ;
                         const _colorSpace : TGIS_ColorInterpolationMode
                       ) ; overload ;

  public
    /// <inheritdoc"/>
    function NextColor : TGIS_Color ; override ;

    /// <summary>
    ///   Creates a color based on the specified hue value.
    /// </summary>
    /// <param name="_hue">
    ///   hue; expected value: 0..1
    /// </param>
    /// <returns>
    ///   resulting color
    /// </returns>
    function CreateColorFromHue( const _hue : Double ) : TGIS_Color ;

  public
    /// <summary>
    ///   Minimum hue, varies from 0 to 1.
    /// </summary>
    property HMin : Double read fget_HMin ;

    /// <summary>
    ///   Maximum hue, varies from 0 to 1.
    /// </summary>
    property HMax : Double read fget_HMax ;

    /// <summary>
    ///   Minimum saturation, varies from 0 to 1.
    /// </summary>
    property SMin : Double read fget_SMin ;

    /// <summary>
    ///   Maximum saturation, varies from 0 to 1.
    /// </summary>
    property SMax : Double read fget_SMax ;

    /// <summary>
    ///   Minimum lightness, varies from 0 to 1 in HSL color space, or
    ///   Minimum Luminance, varies from 0 to 100 in HCL color space.
    /// </summary>
    property LMin : Double read fget_LMin ;

    /// <summary>
    ///   Maximum lightness, varies from 0 to 1.
    /// </summary>
    property LMax : Double read fget_LMax ;

    /// <summary>
    ///   Minimum chroma, varies from 0 to 1.
    /// </summary>
    property CMin : Double read fget_CMin ;

    /// <summary>
    ///   Maximum chroma, varies from 0 to 1.
    /// </summary>
    property CMax : Double read fget_CMax ;

    /// <summary>
    ///   Minimum lightness, varies from 0 to 1.
    /// </summary>
    property RMin : Byte read fget_RMin ;

    /// <summary>
    ///   Maximum lightness, varies from 0 to 1.
    /// </summary>
    property RMax : Byte read fget_RMax ;

    /// <summary>
    ///   Minimum lightness, varies from 0 to 1.
    /// </summary>
    property GMin : Byte read fget_GMin ;

    /// <summary>
    ///   Maximum lightness, varies from 0 to 1.
    /// </summary>
    property GMax : Byte read fget_GMax ;

    /// <summary>
    ///   Minimum lightness, varies from 0 to 1.
    /// </summary>
    property BMin : Byte read fget_BMin ;

    //? fix docs
    /// <summary>
    ///   Maximum lightness, varies from 0 to 1.
    /// </summary>
    property BMax : Byte read fget_BMax ;
  end ;

  /// <summary>
  ///   Represents a collection of predefined color ramp names.
  ///   This class provides a list of color ramp names to be used with
  ///   TGIS_Utils.GisColorRampList.ByName method.
  /// </summary>
  TGIS_ColorRampNames = {$IFDEF OXYGENE} public {$ENDIF}
                        {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    private const
      CR_ACCENT                 = 'Accent';
      CR_ARCTIC                 = 'Arctic';
      CR_ASPECT                 = 'Aspect';
      CR_AUTUMN                 = 'Autumn';
      CR_BLACKWHITE             = 'BlackWhite';
      CR_BATHYMETRY1            = 'Bathymetry1';
      CR_BATHYMETRY2            = 'Bathymetry2';
      CR_BATHYMETRYWARM         = 'BathymetryWarm';
      CR_BLUEGREEN              = 'BlueGreen';
      CR_BLUEPURPLE             = 'BluePurple';
      CR_BLUERED                = 'BlueRed';
      CR_BLUES1                 = 'Blues1';
      CR_BLUES2                 = 'Blues2';
      CR_BLUESTEEL              = 'BlueSteel';
      CR_BROWNBLUE              = 'BrownBlue';
      CR_BROWNGREEN             = 'BrownGreen';
      CR_BROWNYELLOW            = 'BrownYellow';
      CR_CARNIVAL               = 'Carnival';
      CR_CBF_BRIGHT             = 'CBF-Bright';
      CR_CBF_DARK               = 'CBF-Dark';
      CR_CBF_DEUTERANOPIA8      = 'CBF-Deuteranopia8';
      CR_CBF_DEUTERANOPIA12     = 'CBF-Deuteranopia12';
      CR_CBF_DEUTERANOPIA15     = 'CBF-Deuteranopia15';
      CR_CBF_HIGHCONTRAST       = 'CBF-HighContrast';
      CR_CBF_IBM                = 'CBF-IBM';
      CR_CBF_INCANDESCENT       = 'CBF-Incandescent';
      CR_CBF_IRIDESCENT         = 'CBF-Iridescent';
      CR_CBF_LANDCOVER          = 'CBF-LandCover';
      CR_CBF_LIGHT              = 'CBF-Light';
      CR_CBF_MEDIUMCONTRAST     = 'CBF-MediumContrast';
      CR_CBF_MUTED              = 'CBF-Muted';
      CR_CBF_PALE               = 'CBF-Pale';
      CR_CBF_RAINBOWDISCRETE14  = 'CBF-RainbowDiscrete14';
      CR_CBF_RAINBOWDISCRETE29  = 'CBF-RainbowDiscrete29';
      CR_CBF_RAINBOWSMOOTH      = 'CBF-RainbowSmooth';
      CR_CBF_SUNSET             = 'CBF-Sunset';
      CR_CBF_VIBRANT            = 'CBF-Vibrant';
      CR_CBF_OKABEITO           = 'CBF-OkabeIto';
      CR_CHROMADEPTH            = 'ChromaDepth';
      CR_CIVIDIS                = 'Cividis';
      CR_COLD                   = 'Cold';
      CR_COPPER                 = 'Copper';
      CR_COOLWARM               = 'CoolWarm';
      CR_COTTONCANDY            = 'CottonCandy';
      CR_CUBEHELIX              = 'Cubehelix';
      CR_CURRENTDIR             = 'CurrentDir';
      CR_CURRENTVELOCITY        = 'CurrentVelocity';
      CR_CURVATURE              = 'Curvature';
      CR_DARK                   = 'Dark';
      CR_DEMPRINT               = 'DEMPrint';
      CR_DEMSCREEN              = 'DEMScreen';
      CR_DESERT                 = 'Desert';
      CR_DESERTIFICATION        = 'Desertification';
      CR_DIRT                   = 'Dirt';
      CR_DIREACTIVE             = 'DireActive';
      CR_DRYWET                 = 'DryWet';
      CR_EARTH                  = 'Earth';
      CR_ELEVATION1             = 'Elevation1';
      CR_ELEVATION2             = 'Elevation2';
      CR_ELEVATION3             = 'Elevation3';
      CR_ELEVATIONDARK          = 'ElevationDark';
      CR_EQUALMIXED             = 'EqualMixed';
      CR_EXPLORATION            = 'Exploration';
      CR_FLAG                   = 'Flag';
      CR_FORECAST               = 'Forecast';
      CR_FOREST                 = 'Forest';
      CR_GEOLOGY                = 'Geology';
      CR_GLOBALWARMING          = 'GlobalWarming';
      CR_GLOBE                  = 'Globe';
      CR_GRAVITY1               = 'Gravity1';
      CR_GRAVITY2               = 'Gravity2';
      CR_GREENBLUE              = 'GreenBlue';
      CR_GREENS1                = 'Greens1';
      CR_GREENS2                = 'Greens2';
      CR_GREYS                  = 'Greys';
      CR_GNUPLOT                = 'Gnuplot';
      CR_HEAT                   = 'Heat';
      CR_HIGHPOINTS1            = 'HighPoints1';
      CR_HIGHPOINTS2            = 'HighPoints2';
      CR_HOT                    = 'Hot';
      CR_HUMIDITY               = 'Humidity';
      CR_ICE1                   = 'Ice1';
      CR_ICE2                   = 'Ice2';
      CR_ICEFIRE                = 'Icefire';
      CR_INFERNO                = 'Inferno';
      CR_JET                    = 'Jet';
      CR_LAND1                  = 'Land1';
      CR_LAND2                  = 'Land2';
      CR_LANDEUROPE             = 'LandEurope';
      CR_LANDAMERICA            = 'LandAmerica';
      CR_LANDARID               = 'LandArid';
      CR_LANDSEA                = 'LandSea';
      CR_MAGMA                  = 'Magma';
      CR_MAKO                   = 'Mako';
      CR_OCEAN1                 = 'Ocean1';
      CR_OCEAN2                 = 'Ocean2';
      CR_ORANGEPURPLE           = 'OrangePurple';
      CR_ORANGERED              = 'OrangeRed';
      CR_ORANGES                = 'Oranges';
      CR_PASTEL1                = 'Pastel1';
      CR_PASTEL2                = 'Pastel2';
      CR_PASTEL3                = 'Pastel3';
      CR_PASTEL4                = 'Pastel4';
      CR_PAIRED                 = 'Paired';
      CR_PINK                   = 'Pink';
      CR_PLASMA                 = 'Plasma';
      CR_PINKGREEN              = 'PinkGreen';
      CR_PRISM                  = 'Prism';
      CR_PURPLEBLUE             = 'PurpleBlue';
      CR_PURPLEBLUEGREEN        = 'PurpleBlueGreen';
      CR_PURPLEGREEN1           = 'PurpleGreen1';
      CR_PURPLEGREEN2           = 'PurpleGreen2';
      CR_PURPLERED              = 'PurpleRed';
      CR_PURPLES                = 'Purples';
      CR_RAINBOW1               = 'Rainbow1';
      CR_RAINBOW2               = 'Rainbow2';
      CR_RAINBOW3               = 'Rainbow3';
      CR_RAINBOW4               = 'Rainbow4';
      CR_RAINBOW5               = 'Rainbow5';
      CR_RAINBOWLIGHT           = 'RainbowLight';
      CR_RAINBOWPASTEL          = 'RainbowPastel';
      CR_REDBLUE                = 'RedBlue';
      CR_REDGRAY                = 'RedGray';
      CR_REDPURPLE              = 'RedPurple';
      CR_REDYELLOWBLUE          = 'RedYellowBlue';
      CR_REDYELLOWGREEN         = 'RedYellowGreen';
      CR_REDHOT                 = 'RedHot';
      CR_REDS                   = 'Reds';
      CR_ROCKET                 = 'Rocket';
      CR_SEA                    = 'Sea';
      CR_SEALAND                = 'Sealand';
      CR_SEISMIC                = 'Seismic';
      CR_SET1                   = 'Set1';
      CR_SET2                   = 'Set2';
      CR_SET3                   = 'Set3';
      CR_SOIL                   = 'Soil';
      CR_SPECTRAL               = 'Spectral';
      CR_SPICE                  = 'Spice';
      CR_SPRING                 = 'Spring';
      CR_STERN                  = 'Stern';
      CR_SUMMER                 = 'Summer';
      CR_SUNSET                 = 'Sunset';
      CR_TAB10                  = 'Tab10';
      CR_TAB20                  = 'Tab20';
      CR_TAB20B                 = 'Tab20b';
      CR_TAB20C                 = 'Tab20c';
      CR_TEMPERATURE            = 'Temperature';
      CR_TERRAIN                = 'Terrain';
      CR_TOPO1                  = 'Topo1';
      CR_TOPO2                  = 'Topo2';
      CR_TURBO                  = 'Turbo';
      CR_TWILIGHT               = 'Twilight';
      CR_TWILIGHTSHIFTED        = 'TwilightShifted';
      CR_WINTER                 = 'Winter';
      CR_WISTIA                 = 'Wistia';
      CR_VIRIDIS                = 'Viridis';
      CR_YELLOWGREEN            = 'YellowGreen';
      CR_YELLOWGREENBLUE        = 'YellowGreenBlue';
      CR_YELLOWHIGH             = 'YellowHigh';
      CR_YELLOWJACKET           = 'YellowJacket';
      CR_YELLOWORANGEBROWN      = 'YellowOrangeBrown';
      CR_YELLOWORANGERED        = 'YellowOrangeRed';
      CR_UNIQUE                 = 'Unique';
      CR_UNIQUEBRIGHT           = 'UniqueBright';
      CR_UNIQUEDARK             = 'UniqueDark';
      CR_UNIQUEDEEP             = 'UniqueDeep';
      CR_UNIQUEDEEPDARK         = 'UniqueDeepDark';
      CR_UNIQUELIGHT            = 'UniqueLight';
      CR_UNIQUEPASTEL           = 'UniquePastel';

    private
      class function fget_Accent                   : String; static;
      class function fget_Arctic                   : String; static;
      class function fget_Aspect                   : String; static;
      class function fget_Autumn                   : String; static;
      class function fget_BlackWhite               : String; static;
      class function fget_Bathymetry1              : String; static;
      class function fget_Bathymetry2              : String; static;
      class function fget_BathymetryWarm           : String; static;
      class function fget_BlueGreen                : String; static;
      class function fget_BluePurple               : String; static;
      class function fget_BlueRed                  : String; static;
      class function fget_Blues1                   : String; static;
      class function fget_Blues2                   : String; static;
      class function fget_BlueSteel                : String; static;
      class function fget_BrownBlue                : String; static;
      class function fget_BrownGreen               : String; static;
      class function fget_BrownYellow              : String; static;
      class function fget_Carnival                 : String; static;
      class function fget_CBF_Bright               : String; static;
      class function fget_CBF_Dark                 : String; static;
      class function fget_CBF_Deuteranopia8        : String; static;
      class function fget_CBF_Deuteranopia12       : String; static;
      class function fget_CBF_Deuteranopia15       : String; static;
      class function fget_CBF_HighContrast         : String; static;
      class function fget_CBF_IBM                  : String; static;
      class function fget_CBF_Incandescent         : String; static;
      class function fget_CBF_Iridescent           : String; static;
      class function fget_CBF_LandCover            : String; static;
      class function fget_CBF_Light                : String; static;
      class function fget_CBF_MediumContrast       : String; static;
      class function fget_CBF_Muted                : String; static;
      class function fget_CBF_Pale                 : String; static;
      class function fget_CBF_RainbowDiscrete14    : String; static;
      class function fget_CBF_RainbowDiscrete29    : String; static;
      class function fget_CBF_RainbowSmooth        : String; static;
      class function fget_CBF_Sunset               : String; static;
      class function fget_CBF_Vibrant              : String; static;
      class function fget_CBF_OkabeIto             : String; static;
      class function fget_ChromaDepth              : String; static;
      class function fget_Cividis                  : String; static;
      class function fget_Cold                     : String; static;
      class function fget_Copper                   : String; static;
      class function fget_CoolWarm                 : String; static;
      class function fget_CottonCandy              : String; static;
      class function fget_Cubehelix                : String; static;
      class function fget_CurrentDir               : String; static;
      class function fget_CurrentVelocity          : String; static;
      class function fget_Curvature                : String; static;
      class function fget_Dark                     : String; static;
      class function fget_DEMPrint                 : String; static;
      class function fget_DEMScreen                : String; static;
      class function fget_Desert                   : String; static;
      class function fget_Desertification          : String; static;
      class function fget_Dirt                     : String; static;
      class function fget_DireActive               : String; static;
      class function fget_DryWet                   : String; static;
      class function fget_Earth                    : String; static;
      class function fget_Elevation1               : String; static;
      class function fget_Elevation2               : String; static;
      class function fget_Elevation3               : String; static;
      class function fget_ElevationDark            : String; static;
      class function fget_EqualMixed               : String; static;
      class function fget_Exploration              : String; static;
      class function fget_Flag                     : String; static;
      class function fget_Forecast                 : String; static;
      class function fget_Forest                   : String; static;
      class function fget_Geology                  : String; static;
      class function fget_GlobalWarming            : String; static;
      class function fget_Globe                    : String; static;
      class function fget_Gravity1                 : String; static;
      class function fget_Gravity2                 : String; static;
      class function fget_GreenBlue                : String; static;
      class function fget_Greens1                  : String; static;
      class function fget_Greens2                  : String; static;
      class function fget_Greys                    : String; static;
      class function fget_Gnuplot                  : String; static;
      class function fget_Heat                     : String; static;
      class function fget_HighPoints1              : String; static;
      class function fget_HighPoints2              : String; static;
      class function fget_Hot                      : String; static;
      class function fget_Humidity                 : String; static;
      class function fget_Ice1                     : String; static;
      class function fget_Ice2                     : String; static;
      class function fget_Icefire                  : String; static;
      class function fget_Inferno                  : String; static;
      class function fget_Jet                      : String; static;
      class function fget_Land1                    : String; static;
      class function fget_Land2                    : String; static;
      class function fget_LandEurope               : String; static;
      class function fget_LandAmerica              : String; static;
      class function fget_LandArid                 : String; static;
      class function fget_LandSea                  : String; static;
      class function fget_Magma                    : String; static;
      class function fget_Mako                     : String; static;
      class function fget_Ocean1                   : String; static;
      class function fget_Ocean2                   : String; static;
      class function fget_OrangePurple             : String; static;
      class function fget_OrangeRed                : String; static;
      class function fget_Oranges                  : String; static;
      class function fget_Pastel1                  : String; static;
      class function fget_Pastel2                  : String; static;
      class function fget_Pastel3                  : String; static;
      class function fget_Pastel4                  : String; static;
      class function fget_Paired                   : String; static;
      class function fget_Pink                     : String; static;
      class function fget_Plasma                   : String; static;
      class function fget_PinkGreen                : String; static;
      class function fget_Prism                    : String; static;
      class function fget_PurpleBlue               : String; static;
      class function fget_PurpleBlueGreen          : String; static;
      class function fget_PurpleGreen1             : String; static;
      class function fget_PurpleGreen2             : String; static;
      class function fget_PurpleRed                : String; static;
      class function fget_Purples                  : String; static;
      class function fget_Rainbow1                 : String; static;
      class function fget_Rainbow2                 : String; static;
      class function fget_Rainbow3                 : String; static;
      class function fget_Rainbow4                 : String; static;
      class function fget_Rainbow5                 : String; static;
      class function fget_RainbowLight             : String; static;
      class function fget_RainbowPastel            : String; static;
      class function fget_RedBlue                  : String; static;
      class function fget_RedGray                  : String; static;
      class function fget_RedPurple                : String; static;
      class function fget_RedYellowBlue            : String; static;
      class function fget_RedYellowGreen           : String; static;
      class function fget_RedHot                   : String; static;
      class function fget_Reds                     : String; static;
      class function fget_Rocket                   : String; static;
      class function fget_Sea                      : String; static;
      class function fget_Sealand                  : String; static;
      class function fget_Seismic                  : String; static;
      class function fget_Set1                     : String; static;
      class function fget_Set2                     : String; static;
      class function fget_Set3                     : String; static;
      class function fget_Soil                     : String; static;
      class function fget_Spectral                 : String; static;
      class function fget_Spice                    : String; static;
      class function fget_Spring                   : String; static;
      class function fget_Stern                    : String; static;
      class function fget_Summer                   : String; static;
      class function fget_Sunset                   : String; static;
      class function fget_Tab10                    : String; static;
      class function fget_Tab20                    : String; static;
      class function fget_Tab20b                   : String; static;
      class function fget_Tab20c                   : String; static;
      class function fget_Temperature              : String; static;
      class function fget_Terrain                  : String; static;
      class function fget_Topo1                    : String; static;
      class function fget_Topo2                    : String; static;
      class function fget_Turbo                    : String; static;
      class function fget_Twilight                 : String; static;
      class function fget_TwilightShifted          : String; static;
      class function fget_Winter                   : String; static;
      class function fget_Wistia                   : String; static;
      class function fget_Viridis                  : String; static;
      class function fget_YellowGreen              : String; static;
      class function fget_YellowGreenBlue          : String; static;
      class function fget_YellowHigh               : String; static;
      class function fget_YellowJacket             : String; static;
      class function fget_YellowOrangeBrown        : String; static;
      class function fget_YellowOrangeRed          : String; static;
      class function fget_Unique                   : String; static;
      class function fget_UniqueBright             : String; static;
      class function fget_UniqueDark               : String; static;
      class function fget_UniqueDeep               : String; static;
      class function fget_UniqueDeepDark           : String; static;
      class function fget_UniqueLight              : String; static;
      class function fget_UniquePastel             : String; static;

    public
      /// <summary>
      ///    'Accent' color ramp.
      /// </summary>
      class property Accent: String read fget_Accent;

      /// <summary>
      ///    'Arctic' color ramp.
      /// </summary>
      class property Arctic: String read fget_Arctic;

      /// <summary>
      ///    'Aspect' color ramp.
      /// </summary>
      class property Aspect: String read fget_Aspect;

      /// <summary>
      ///    'Autumn' color ramp.
      /// </summary>
      class property Autumn: String read fget_Autumn;

      /// <summary>
      ///    'BlackWhite' color ramp.
      /// </summary>
      class property BlackWhite: String read fget_BlackWhite;

      /// <summary>
      ///    'Bathymetry1' color ramp.
      /// </summary>
      class property Bathymetry1: String read fget_Bathymetry1;

      /// <summary>
      ///    'Bathymetry2' color ramp.
      /// </summary>
      class property Bathymetry2: String read fget_Bathymetry2;

      /// <summary>
      ///    'BathymetryWarm' color ramp.
      /// </summary>
      class property BathymetryWarm: String read fget_BathymetryWarm;

      /// <summary>
      ///    'BlueGreen' color ramp.
      /// </summary>
      class property BlueGreen: String read fget_BlueGreen;

      /// <summary>
      ///    'BluePurple' color ramp.
      /// </summary>
      class property BluePurple: String read fget_BluePurple;

      /// <summary>
      ///    'BlueRed' color ramp.
      /// </summary>
      class property BlueRed: String read fget_BlueRed;

      /// <summary>
      ///    'Blues1' color ramp.
      /// </summary>
      class property Blues1: String read fget_Blues1;

      /// <summary>
      ///    'Blues2' color ramp.
      /// </summary>
      class property Blues2: String read fget_Blues2;

      /// <summary>
      ///    'BlueSteel' color ramp.
      /// </summary>
      class property BlueSteel: String read fget_BlueSteel;

      /// <summary>
      ///    'BrownBlue' color ramp.
      /// </summary>
      class property BrownBlue: String read fget_BrownBlue;

      /// <summary>
      ///    'BrownGreen' color ramp.
      /// </summary>
      class property BrownGreen: String read fget_BrownGreen;

      /// <summary>
      ///    'BrownYellow' color ramp.
      /// </summary>
      class property BrownYellow: String read fget_BrownYellow;

      /// <summary>
      ///    'Carnival' color ramp.
      /// </summary>
      class property Carnival: String read fget_Carnival;

      /// <summary>
      ///    'CBF-Bright' color ramp.
      /// </summary>
      class property CBF_Bright: String read fget_CBF_Bright;

      /// <summary>
      ///    'CBF-Dark' color ramp.
      /// </summary>
      class property CBF_Dark: String read fget_CBF_Dark;

      /// <summary>
      ///    'CBF-Deuteranopia8' color ramp.
      /// </summary>
      class property CBF_Deuteranopia8: String read fget_CBF_Deuteranopia8;

      /// <summary>
      ///    'CBF-Deuteranopia12' color ramp.
      /// </summary>
      class property CBF_Deuteranopia12: String read fget_CBF_Deuteranopia12;

      /// <summary>
      ///    'CBF-Deuteranopia15' color ramp.
      /// </summary>
      class property CBF_Deuteranopia15: String read fget_CBF_Deuteranopia15;

      /// <summary>
      ///    'CBF-HighContrast' color ramp.
      /// </summary>
      class property CBF_HighContrast: String read fget_CBF_HighContrast;

      /// <summary>
      ///    'CBF-IBM' color ramp.
      /// </summary>
      class property CBF_IBM: String read fget_CBF_IBM;

      /// <summary>
      ///    'CBF-Incandescent' color ramp.
      /// </summary>
      class property CBF_Incandescent: String read fget_CBF_Incandescent;

      /// <summary>
      ///    'CBF-Iridescent' color ramp.
      /// </summary>
      class property CBF_Iridescent: String read fget_CBF_Iridescent;

      /// <summary>
      ///    'CBF-LandCover' color ramp.
      /// </summary>
      class property CBF_LandCover: String read fget_CBF_LandCover;

      /// <summary>
      ///    'CBF-Light' color ramp.
      /// </summary>
      class property CBF_Light: String read fget_CBF_Light;

      /// <summary>
      ///    'CBF-MediumContrast' color ramp.
      /// </summary>
      class property CBF_MediumContrast: String read fget_CBF_MediumContrast;

      /// <summary>
      ///    'CBF-Muted' color ramp.
      /// </summary>
      class property CBF_Muted: String read fget_CBF_Muted;

      /// <summary>
      ///    'CBF-Pale' color ramp.
      /// </summary>
      class property CBF_Pale: String read fget_CBF_Pale;

      /// <summary>
      ///    'CBF-RainbowDiscrete14' color ramp.
      /// </summary>
      class property CBF_RainbowDiscrete14: String read fget_CBF_RainbowDiscrete14;

      /// <summary>
      ///    'CBF-RainbowDiscrete29' color ramp.
      /// </summary>
      class property CBF_RainbowDiscrete29: String read fget_CBF_RainbowDiscrete29;

      /// <summary>
      ///    'CBF-RainbowSmooth' color ramp.
      /// </summary>
      class property CBF_RainbowSmooth: String read fget_CBF_RainbowSmooth;

      /// <summary>
      ///    'CBF-Sunset' color ramp.
      /// </summary>
      class property CBF_Sunset: String read fget_CBF_Sunset;

      /// <summary>
      ///    'CBF-Vibrant' color ramp.
      /// </summary>
      class property CBF_Vibrant: String read fget_CBF_Vibrant;

      /// <summary>
      ///    'CBF-OkabeIto' color ramp.
      /// </summary>
      class property CBF_OkabeIto: String read fget_CBF_OkabeIto;

      /// <summary>
      ///    'ChromaDepth' color ramp.
      /// </summary>
      class property ChromaDepth: String read fget_ChromaDepth;

      /// <summary>
      ///    'Cividis' color ramp.
      /// </summary>
      class property Cividis: String read fget_Cividis;

      /// <summary>
      ///    'Cold' color ramp.
      /// </summary>
      class property Cold: String read fget_Cold;

      /// <summary>
      ///    'Copper' color ramp.
      /// </summary>
      class property Copper: String read fget_Copper;

      /// <summary>
      ///    'CoolWarm' color ramp.
      /// </summary>
      class property CoolWarm: String read fget_CoolWarm;

      /// <summary>
      ///    'CottonCandy' color ramp.
      /// </summary>
      class property CottonCandy: String read fget_CottonCandy;

      /// <summary>
      ///    'Cubehelix' color ramp.
      /// </summary>
      class property Cubehelix: String read fget_Cubehelix;

      /// <summary>
      ///    'CurrentDir' color ramp.
      /// </summary>
      class property CurrentDir: String read fget_CurrentDir;

      /// <summary>
      ///    'CurrentVelocity' color ramp.
      /// </summary>
      class property CurrentVelocity: String read fget_CurrentVelocity;

      /// <summary>
      ///    'Curvature' color ramp.
      /// </summary>
      class property Curvature: String read fget_Curvature;

      /// <summary>
      ///    'Dark' color ramp.
      /// </summary>
      class property Dark: String read fget_Dark;

      /// <summary>
      ///    'DEMPrint' color ramp.
      /// </summary>
      class property DEMPrint: String read fget_DEMPrint;

      /// <summary>
      ///    'DEMScreen' color ramp.
      /// </summary>
      class property DEMScreen: String read fget_DEMScreen;

      /// <summary>
      ///    'Desert' color ramp.
      /// </summary>
      class property Desert: String read fget_Desert;

      /// <summary>
      ///    'Desertification' color ramp.
      /// </summary>
      class property Desertification: String read fget_Desertification;

      /// <summary>
      ///    'Dirt' color ramp.
      /// </summary>
      class property Dirt: String read fget_Dirt;

      /// <summary>
      ///    'DireActive' color ramp.
      /// </summary>
      class property DireActive: String read fget_DireActive;

      /// <summary>
      ///    'DryWet' color ramp.
      /// </summary>
      class property DryWet: String read fget_DryWet;

      /// <summary>
      ///    'Earth' color ramp.
      /// </summary>
      class property Earth: String read fget_Earth;

      /// <summary>
      ///    'Elevation1' color ramp.
      /// </summary>
      class property Elevation1: String read fget_Elevation1;

      /// <summary>
      ///    'Elevation2' color ramp.
      /// </summary>
      class property Elevation2: String read fget_Elevation2;

      /// <summary>
      ///    'Elevation3' color ramp.
      /// </summary>
      class property Elevation3: String read fget_Elevation3;

      /// <summary>
      ///    'ElevationDark' color ramp.
      /// </summary>
      class property ElevationDark: String read fget_ElevationDark;

      /// <summary>
      ///    'EqualMixed' color ramp.
      /// </summary>
      class property EqualMixed: String read fget_EqualMixed;

      /// <summary>
      ///    'Exploration' color ramp.
      /// </summary>
      class property Exploration: String read fget_Exploration;

      /// <summary>
      ///    'Flag' color ramp.
      /// </summary>
      class property Flag: String read fget_Flag;

      /// <summary>
      ///    'Forecast' color ramp.
      /// </summary>
      class property Forecast: String read fget_Forecast;

      /// <summary>
      ///    'Forest' color ramp.
      /// </summary>
      class property Forest: String read fget_Forest;

      /// <summary>
      ///    'Geology' color ramp.
      /// </summary>
      class property Geology: String read fget_Geology;

      /// <summary>
      ///    'GlobalWarming' color ramp.
      /// </summary>
      class property GlobalWarming: String read fget_GlobalWarming;

      /// <summary>
      ///    'Globe' color ramp.
      /// </summary>
      class property Globe: String read fget_Globe;

      /// <summary>
      ///    'Gravity1' color ramp.
      /// </summary>
      class property Gravity1: String read fget_Gravity1;

      /// <summary>
      ///    'Gravity2' color ramp.
      /// </summary>
      class property Gravity2: String read fget_Gravity2;

      /// <summary>
      ///    'GreenBlue' color ramp.
      /// </summary>
      class property GreenBlue: String read fget_GreenBlue;

      /// <summary>
      ///    'Greens1' color ramp.
      /// </summary>
      class property Greens1: String read fget_Greens1;

      /// <summary>
      ///    'Greens2' color ramp.
      /// </summary>
      class property Greens2: String read fget_Greens2;

      /// <summary>
      ///    'Greys' color ramp.
      /// </summary>
      class property Greys: String read fget_Greys;

      /// <summary>
      ///    'Gnuplot' color ramp.
      /// </summary>
      class property Gnuplot: String read fget_Gnuplot;

      /// <summary>
      ///    'Heat' color ramp.
      /// </summary>
      class property Heat: String read fget_Heat;

      /// <summary>
      ///    'HighPoints1' color ramp.
      /// </summary>
      class property HighPoints1: String read fget_HighPoints1;

      /// <summary>
      ///    'HighPoints2' color ramp.
      /// </summary>
      class property HighPoints2: String read fget_HighPoints2;

      /// <summary>
      ///    'Hot' color ramp.
      /// </summary>
      class property Hot: String read fget_Hot;

      /// <summary>
      ///    'Humidity' color ramp.
      /// </summary>
      class property Humidity: String read fget_Humidity;

      /// <summary>
      ///    'Ice1' color ramp.
      /// </summary>
      class property Ice1: String read fget_Ice1;

      /// <summary>
      ///    'Ice2' color ramp.
      /// </summary>
      class property Ice2: String read fget_Ice2;

      /// <summary>
      ///    'Icefire' color ramp.
      /// </summary>
      class property Icefire: String read fget_Icefire;

      /// <summary>
      ///    'Inferno' color ramp.
      /// </summary>
      class property Inferno: String read fget_Inferno;

      /// <summary>
      ///    'Jet' color ramp.
      /// </summary>
      class property Jet: String read fget_Jet;

      /// <summary>
      ///    'Land1' color ramp.
      /// </summary>
      class property Land1: String read fget_Land1;

      /// <summary>
      ///    'Land2' color ramp.
      /// </summary>
      class property Land2: String read fget_Land2;

      /// <summary>
      ///    'LandEurope' color ramp.
      /// </summary>
      class property LandEurope: String read fget_LandEurope;

      /// <summary>
      ///    'LandAmerica' color ramp.
      /// </summary>
      class property LandAmerica: String read fget_LandAmerica;

      /// <summary>
      ///    'LandArid' color ramp.
      /// </summary>
      class property LandArid: String read fget_LandArid;

      /// <summary>
      ///    'LandSea' color ramp.
      /// </summary>
      class property LandSea: String read fget_LandSea;

      /// <summary>
      ///    'Magma' color ramp.
      /// </summary>
      class property Magma: String read fget_Magma;

      /// <summary>
      ///    'Mako' color ramp.
      /// </summary>
      class property Mako: String read fget_Mako;

      /// <summary>
      ///    'Ocean1' color ramp.
      /// </summary>
      class property Ocean1: String read fget_Ocean1;

      /// <summary>
      ///    'Ocean2' color ramp.
      /// </summary>
      class property Ocean2: String read fget_Ocean2;

      /// <summary>
      ///    'OrangePurple' color ramp.
      /// </summary>
      class property OrangePurple: String read fget_OrangePurple;

      /// <summary>
      ///    'OrangeRed' color ramp.
      /// </summary>
      class property OrangeRed: String read fget_OrangeRed;

      /// <summary>
      ///    'Oranges' color ramp.
      /// </summary>
      class property Oranges: String read fget_Oranges;

      /// <summary>
      ///    'Pastel1' color ramp.
      /// </summary>
      class property Pastel1: String read fget_Pastel1;

      /// <summary>
      ///    'Pastel2' color ramp.
      /// </summary>
      class property Pastel2: String read fget_Pastel2;

      /// <summary>
      ///    'Pastel3' color ramp.
      /// </summary>
      class property Pastel3: String read fget_Pastel3;

      /// <summary>
      ///    'Pastel4' color ramp.
      /// </summary>
      class property Pastel4: String read fget_Pastel4;

      /// <summary>
      ///    'Paired' color ramp.
      /// </summary>
      class property Paired: String read fget_Paired;

      /// <summary>
      ///    'Pink' color ramp.
      /// </summary>
      class property Pink: String read fget_Pink;

      /// <summary>
      ///    'Plasma' color ramp.
      /// </summary>
      class property Plasma: String read fget_Plasma;

      /// <summary>
      ///    'PinkGreen' color ramp.
      /// </summary>
      class property PinkGreen: String read fget_PinkGreen;

      /// <summary>
      ///    'Prism' color ramp.
      /// </summary>
      class property Prism: String read fget_Prism;

      /// <summary>
      ///    'PurpleBlue' color ramp.
      /// </summary>
      class property PurpleBlue: String read fget_PurpleBlue;

      /// <summary>
      ///    'PurpleBlueGreen' color ramp.
      /// </summary>
      class property PurpleBlueGreen: String read fget_PurpleBlueGreen;

      /// <summary>
      ///    'PurpleGreen1' color ramp.
      /// </summary>
      class property PurpleGreen1: String read fget_PurpleGreen1;

      /// <summary>
      ///    'PurpleGreen2' color ramp.
      /// </summary>
      class property PurpleGreen2: String read fget_PurpleGreen2;

      /// <summary>
      ///    'PurpleRed' color ramp.
      /// </summary>
      class property PurpleRed: String read fget_PurpleRed;

      /// <summary>
      ///    'Purples' color ramp.
      /// </summary>
      class property Purples: String read fget_Purples;

      /// <summary>
      ///    'Rainbow1' color ramp.
      /// </summary>
      class property Rainbow1: String read fget_Rainbow1;

      /// <summary>
      ///    'Rainbow2' color ramp.
      /// </summary>
      class property Rainbow2: String read fget_Rainbow2;

      /// <summary>
      ///    'Rainbow3' color ramp.
      /// </summary>
      class property Rainbow3: String read fget_Rainbow3;

      /// <summary>
      ///    'Rainbow4' color ramp.
      /// </summary>
      class property Rainbow4: String read fget_Rainbow4;

      /// <summary>
      ///    'Rainbow5' color ramp.
      /// </summary>
      class property Rainbow5: String read fget_Rainbow5;

      /// <summary>
      ///    'RainbowLight' color ramp.
      /// </summary>
      class property RainbowLight: String read fget_RainbowLight;

      /// <summary>
      ///    'RainbowPastel' color ramp.
      /// </summary>
      class property RainbowPastel: String read fget_RainbowPastel;

      /// <summary>
      ///    'RedBlue' color ramp.
      /// </summary>
      class property RedBlue: String read fget_RedBlue;

      /// <summary>
      ///    'RedGray' color ramp.
      /// </summary>
      class property RedGray: String read fget_RedGray;

      /// <summary>
      ///    'RedPurple' color ramp.
      /// </summary>
      class property RedPurple: String read fget_RedPurple;

      /// <summary>
      ///    'RedYellowBlue' color ramp.
      /// </summary>
      class property RedYellowBlue: String read fget_RedYellowBlue;

      /// <summary>
      ///    'RedYellowGreen' color ramp.
      /// </summary>
      class property RedYellowGreen: String read fget_RedYellowGreen;

      /// <summary>
      ///    'RedHot' color ramp.
      /// </summary>
      class property RedHot: String read fget_RedHot;

      /// <summary>
      ///    'Reds' color ramp.
      /// </summary>
      class property Reds: String read fget_Reds;

      /// <summary>
      ///    'Rocket' color ramp.
      /// </summary>
      class property Rocket: String read fget_Rocket;

      /// <summary>
      ///    'Sea' color ramp.
      /// </summary>
      class property Sea: String read fget_Sea;

      /// <summary>
      ///    'Sealand' color ramp.
      /// </summary>
      class property Sealand: String read fget_Sealand;

      /// <summary>
      ///    'Seismic' color ramp.
      /// </summary>
      class property Seismic: String read fget_Seismic;

      /// <summary>
      ///    'Set1' color ramp.
      /// </summary>
      class property Set1: String read fget_Set1;

      /// <summary>
      ///    'Set2' color ramp.
      /// </summary>
      class property Set2: String read fget_Set2;

      /// <summary>
      ///    'Set3' color ramp.
      /// </summary>
      class property Set3: String read fget_Set3;

      /// <summary>
      ///    'Soil' color ramp.
      /// </summary>
      class property Soil: String read fget_Soil;

      /// <summary>
      ///    'Spectral' color ramp.
      /// </summary>
      class property Spectral: String read fget_Spectral;

      /// <summary>
      ///    'Spice' color ramp.
      /// </summary>
      class property Spice: String read fget_Spice;

      /// <summary>
      ///    'Spring' color ramp.
      /// </summary>
      class property Spring: String read fget_Spring;

      /// <summary>
      ///    'Stern' color ramp.
      /// </summary>
      class property Stern: String read fget_Stern;

      /// <summary>
      ///    'Summer' color ramp.
      /// </summary>
      class property Summer: String read fget_Summer;

      /// <summary>
      ///    'Sunset' color ramp.
      /// </summary>
      class property Sunset: String read fget_Sunset;

      /// <summary>
      ///    'Tab10' color ramp.
      /// </summary>
      class property Tab10: String read fget_Tab10;

      /// <summary>
      ///    'Tab20' color ramp.
      /// </summary>
      class property Tab20: String read fget_Tab20;

      /// <summary>
      ///    'Tab20b' color ramp.
      /// </summary>
      class property Tab20b: String read fget_Tab20b;

      /// <summary>
      ///    'Tab20c' color ramp.
      /// </summary>
      class property Tab20c: String read fget_Tab20c;

      /// <summary>
      ///    'Temperature' color ramp.
      /// </summary>
      class property Temperature: String read fget_Temperature;

      /// <summary>
      ///    'Terrain' color ramp.
      /// </summary>
      class property Terrain: String read fget_Terrain;

      /// <summary>
      ///    'Topo1' color ramp.
      /// </summary>
      class property Topo1: String read fget_Topo1;

      /// <summary>
      ///    'Topo2' color ramp.
      /// </summary>
      class property Topo2: String read fget_Topo2;

      /// <summary>
      ///    'Turbo' color ramp.
      /// </summary>
      class property Turbo: String read fget_Turbo;

      /// <summary>
      ///    'Twilight' color ramp.
      /// </summary>
      class property Twilight: String read fget_Twilight;

      /// <summary>
      ///    'TwilightShifted' color ramp.
      /// </summary>
      class property TwilightShifted: String read fget_TwilightShifted;

      /// <summary>
      ///    'Winter' color ramp.
      /// </summary>
      class property Winter: String read fget_Winter;

      /// <summary>
      ///    'Wistia' color ramp.
      /// </summary>
      class property Wistia: String read fget_Wistia;

      /// <summary>
      ///    'Viridis' color ramp.
      /// </summary>
      class property Viridis: String read fget_Viridis;

      /// <summary>
      ///    'YellowGreen' color ramp.
      /// </summary>
      class property YellowGreen: String read fget_YellowGreen;

      /// <summary>
      ///    'YellowGreenBlue' color ramp.
      /// </summary>
      class property YellowGreenBlue: String read fget_YellowGreenBlue;

      /// <summary>
      ///    'YellowHigh' color ramp.
      /// </summary>
      class property YellowHigh: String read fget_YellowHigh;

      /// <summary>
      ///    'YellowJacket' color ramp.
      /// </summary>
      class property YellowJacket: String read fget_YellowJacket;

      /// <summary>
      ///    'YellowOrangeBrown' color ramp.
      /// </summary>
      class property YellowOrangeBrown: String read fget_YellowOrangeBrown;

      /// <summary>
      ///    'YellowOrangeRed' color ramp.
      /// </summary>
      class property YellowOrangeRed: String read fget_YellowOrangeRed;

      /// <summary>
      ///    'Unique' color ramp.
      /// </summary>
      class property Unique: String read fget_Unique;

      /// <summary>
      ///    'UniqueBright' color ramp.
      /// </summary>
      class property UniqueBright: String read fget_UniqueBright;

      /// <summary>
      ///    'UniqueDark' color ramp.
      /// </summary>
      class property UniqueDark: String read fget_UniqueDark;

      /// <summary>
      ///    'UniqueDeep' color ramp.
      /// </summary>
      class property UniqueDeep: String read fget_UniqueDeep;

      /// <summary>
      ///    'UniqueDeepDark' color ramp.
      /// </summary>
      class property UniqueDeepDark: String read fget_UniqueDeepDark;

      /// <summary>
      ///    'UniqueLight' color ramp.
      /// </summary>
      class property UniqueLight: String read fget_UniqueLight;

      /// <summary>
      ///    'UniquePastel' color ramp.
      /// </summary>
      class property UniquePastel: String read fget_UniquePastel;
  end;

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
  NativeBitmapFactory : TGIS_BitmapFactory ;

  /// <summary>
  ///   Global object for list of available bitmap factories.
  /// </summary>
  BitmapFactoryList : TDictionary< String, TGIS_BitmapFactoryClass > ;

  /// <summary>
  ///   Global object for device dependent font construction.
  /// </summary>
  FontHelper   : TGIS_FontFactory ;

  /// <summary>
  ///   Global scale factor to scale GUI elements.
  /// </summary>
  GUIScale : Single = 1;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Register a bitmap factory.
  /// </summary>
  /// <param name="_name">
  ///   unique name of the factory; name is case insensitive
  /// </param>
  /// <param name="_class">
  ///   class implementing bitmap factory
  /// </param>
  procedure RegisterBitmapFactory(
    const _name  : String ;
    const _class : TGIS_BitmapFactoryClass
  ) ;

implementation

{$IFDEF DCC}
  uses
    GisClasses,
    GisUtils,
    GisFunctions,
    GisInternals,
    GisResource,
    GisRendererAbstract;
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

{$REGION 'T_HCLHelper'}
type
  T_RGB = record
    R, G, B : Double ;
  end;

  T_LAB = record
    L, A, B, Opacity : Double ;
  end;

  T_HCL = record
    H, C, L, Opacity : Double ;
  end;

  T_HCLHelper = class
    const
      K = 18 ;
      Xn = 0.96422 ; // D50  illuminant; D65 standard is the possible alternative
      Yn = 1 ;
      Zn = 0.82521 ;
      t0 = 4 / 29 ;
      t1 = 6 / 29 ;
      t2 = 3 * t1 * t1 ;
      t3 = t1 * t1 * t1 ;

    class function XYZToLab ( const _t : Double ) : Double ; static ;
    class function LabToXYZ ( const _t : Double ) : Double ; static ;
    class function LRGBToRGB( const _x : Double ) : Double ; static ;
    class function RGBToLRGB( const _x : Double ) : Double ; static ;
    class function RGBToLab ( const _rgb : T_RGB ) : T_LAB ; static ;
    class function GrayLab  ( const _l, _opacity : Double ) : T_LAB ; static ;
    class function Lab      ( const _l, _a, _b, _opacity : Double ) : T_LAB ; static ;
    class function HclToLab ( const _hcl : T_HCL ) : T_LAB ; static ;
    class function LabToRgb ( const _lab : T_LAB ) : T_RGB ; static ;
    class function LabToHcl ( const _lab : T_LAB ) : T_HCL ; static ;
  end;

class function T_HCLHelper.XYZToLab( const _t : Double ) : Double ;
begin
  if _t > t3 then
    Result := Power( _t, 1/3 )
  else
    Result := _t / t2 + t0;
end;

class function T_HCLHelper.LabToXYZ( const _t : Double ) : Double ;
begin
  if _t > t1 then
    Result := Power( _t, 3 )
  else
    Result := t2 * ( _t - t0 ) ;
end;

class function T_HCLHelper.LRGBToRGB( const _x : Double ) : Double ;
begin
  if _x <= 0.0031308 then
    Result := 12.92 * _x
  else
    Result := 1.055 * Power( _x, 1/2.4) - 0.055 ;

  Result := 255 * Result ;
end;

class function T_HCLHelper.RGBToLRGB( const _x : Double ) : Double ;
var
  x : Double ;
begin
  x := _x / 255 ;

  if x <= 0.04045 then
    Result := x / 12.92
  else
    Result := Power( ( x + 0.055) / 1.055, 2.4 ) ;
end;

class function T_HCLHelper.RGBToLab( const _rgb : T_RGB ) : T_LAB ;
var
  r, g, b, y       : Double ;
  labX, labY, labZ : Double ;
begin
  r := RGBToLRGB( _rgb.R ) ;
  g := RGBToLRGB( _rgb.G ) ;
  b := RGBToLRGB( _rgb.B ) ;

  y := XYZToLab( ( 0.2225045*r + 0.7168786*g + 0.0606169*b ) / Yn ) ;

  if SameValue( r, g ) and SameValue( g, b ) then
  begin
    labX := y ;
    labY := y ;
    labZ := y ;
  end
  else
  begin
    labX := XYZToLab( ( 0.4360747*r + 0.3850649*g + 0.1430804*b) / Xn ) ;
    labZ := XYZToLab( ( 0.0139322*r + 0.0971045*g + 0.7141733*b) / Zn ) ;
  end;

  Result.L := 116 * y - 16;
  Result.A := 500 * ( labX - y ) ;
  Result.B := 200 * ( y - labZ ) ;
  Result.Opacity := 1;
end;

class function T_HCLHelper.GrayLab( const _l, _opacity : Double ) : T_LAB ;
begin
  Result.L := _l ;
  Result.A := 0 ;
  Result.B := 0 ;
  Result.Opacity := _opacity ;
end;

class function T_HCLHelper.Lab( const _l, _a, _b, _opacity : Double ) : T_LAB ;
begin
  Result.L := _l ;
  Result.A := _a ;
  Result.B := _b ;
  Result.Opacity := _opacity ;
end;

class function T_HCLHelper.HclToLab( const _hcl : T_HCL ) : T_LAB ;
var
  h : Double ;
begin
  if IsNan( _hcl.H ) then
    Result := GrayLab( _hcl.L, _hcl.Opacity )
  else
  begin
    h := DegToRad( _hcl.H ) ;
    Result := Lab( _hcl.L, Cos(h) * _hcl.C, Sin(h) * _hcl.C, _hcl.Opacity ) ;
  end;
end;

class function T_HCLHelper.LabToRgb( const _lab : T_LAB ) : T_RGB ;
var
  x, y, z : Double ;
begin
  y := ( _lab.L + 16 ) / 116 ;
  x := y + _lab.A / 500 ;
  z := y - _lab.B / 200 ;

  x := Xn * LabToXYZ( x ) ;
  y := Yn * LabToXYZ( y ) ;
  z := Zn * LabToXYZ( z ) ;

  Result.R := LRGBToRGB( 3.1338561*x - 1.6168667*y - 0.4906146*z ) ;
  Result.G := LRGBToRGB( -0.9787684*x + 1.9161415*y + 0.0334540*z ) ;
  Result.B := LRGBToRGB( 0.0719453*x - 0.2289914*y + 1.4052427*z ) ;
end;

class function T_HCLHelper.LabToHcl( const _lab : T_LAB ) : T_HCL ;
begin
  Result.H := RadToDeg( ArcTan2( _lab.B, _lab.A ) ) ;
  if Result.H < 0 then
    Result.H := Result.H + 360 ;

  Result.C := Sqrt( Sqr( _lab.A ) + Sqr( _lab.B ) ) ;
  Result.L := _lab.L ;
  Result.Opacity := _lab.Opacity ;
end;
{$ENDREGION}

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

  class function TGIS_Color.FromHCL(
    const _h : Double ;
    const _c : Double ;
    const _l : Double
  ) : TGIS_Color ;
  begin
    Result := TGIS_Color.FromAHCL(1, _h, _c, _l) ;
  end;

  class function TGIS_Color.FromAHCL(
    const _a : Double ;
    const _h : Double ;
    const _c : Double ;
    const _l : Double
  ) : TGIS_Color ;
  var
    hcl : T_HCL ;
    lab : T_LAB ;
    rgb : T_RGB ;
  begin
    hcl.H := _h * 360 ;
    hcl.C := _c * 100 ;
    hcl.L := _l * 100 ;

    lab := T_HCLHelper.HclToLab( hcl ) ;
    rgb := T_HCLHelper.LabToRgb( lab ) ;

    Result := TGIS_Color.FromARGB(
      RoundS( _a * 255 ),
      Max( 0, Min( 255, RoundS( rgb.R ))),
      Max( 0, Min( 255, RoundS( rgb.G ))),
      Max( 0, Min( 255, RoundS( rgb.B )))
    ) ;
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

  procedure TGIS_Color.ToAHCL(var _a, _h, _c, _l: Double);
  var
    rgb : T_RGB ;
    lab : T_LAB ;
    hcl : T_HCL ;
  begin
    _a := A / 255 ;

    rgb.R := R ;
    rgb.G := G ;
    rgb.B := B ;

    lab := T_HCLHelper.RGBToLab( rgb ) ;
    hcl := T_HCLHelper.LabToHcl( lab ) ;

    _h := hcl.H / 360 ;
    _c := hcl.C / 100 ;
    _l := hcl.L / 100 ;
  end;

  procedure TGIS_Color.ToHCL(
    var _h : Double ;
    var _c : Double ;
    var _l : Double
  );
  var
    alpha : Double ;
  begin
    ToAHCL( alpha, _h, _c, _l ) ;
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

{$REGION 'TGIS_ColorRampNames'}
  class function TGIS_ColorRampNames.fget_Accent
    : String;
  begin
    Result := CR_ACCENT ;
  end;

  class function TGIS_ColorRampNames.fget_Arctic
    : String;
  begin
    Result := CR_ARCTIC ;
  end;

  class function TGIS_ColorRampNames.fget_Aspect
    : String;
  begin
    Result := CR_ASPECT ;
  end;

  class function TGIS_ColorRampNames.fget_Autumn
    : String;
  begin
    Result := CR_AUTUMN ;
  end;

  class function TGIS_ColorRampNames.fget_BlackWhite
    : String;
  begin
    Result := CR_BLACKWHITE ;
  end;

  class function TGIS_ColorRampNames.fget_Bathymetry1
    : String;
  begin
    Result := CR_BATHYMETRY1 ;
  end;

  class function TGIS_ColorRampNames.fget_Bathymetry2
    : String;
  begin
    Result := CR_BATHYMETRY2 ;
  end;

  class function TGIS_ColorRampNames.fget_BathymetryWarm
    : String;
  begin
    Result := CR_BATHYMETRYWARM ;
  end;

  class function TGIS_ColorRampNames.fget_BlueGreen
    : String;
  begin
    Result := CR_BLUEGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_BluePurple
    : String;
  begin
    Result := CR_BLUEPURPLE ;
  end;

  class function TGIS_ColorRampNames.fget_BlueRed
    : String;
  begin
    Result := CR_BLUERED ;
  end;

  class function TGIS_ColorRampNames.fget_Blues1
    : String;
  begin
    Result := CR_BLUES1 ;
  end;

  class function TGIS_ColorRampNames.fget_Blues2
    : String;
  begin
    Result := CR_BLUES2 ;
  end;

  class function TGIS_ColorRampNames.fget_BlueSteel
    : String;
  begin
    Result := CR_BLUESTEEL ;
  end;

  class function TGIS_ColorRampNames.fget_BrownBlue
    : String;
  begin
    Result := CR_BROWNBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_BrownGreen
    : String;
  begin
    Result := CR_BROWNGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_BrownYellow
    : String;
  begin
    Result := CR_BROWNYELLOW ;
  end;

  class function TGIS_ColorRampNames.fget_Carnival
    : String;
  begin
    Result := CR_CARNIVAL ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Bright
    : String;
  begin
    Result := CR_CBF_BRIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Dark
    : String;
  begin
    Result := CR_CBF_DARK ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Deuteranopia8
    : String;
  begin
    Result := CR_CBF_DEUTERANOPIA8 ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Deuteranopia12
    : String;
  begin
    Result := CR_CBF_DEUTERANOPIA12 ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Deuteranopia15
    : String;
  begin
    Result := CR_CBF_DEUTERANOPIA15 ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_HighContrast
    : String;
  begin
    Result := CR_CBF_HIGHCONTRAST ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_IBM
    : String;
  begin
    Result := CR_CBF_IBM ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Incandescent
    : String;
  begin
    Result := CR_CBF_INCANDESCENT ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Iridescent
    : String;
  begin
    Result := CR_CBF_IRIDESCENT ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_LandCover
    : String;
  begin
    Result := CR_CBF_LANDCOVER ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Light
    : String;
  begin
    Result := CR_CBF_LIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_MediumContrast
    : String;
  begin
    Result := CR_CBF_MEDIUMCONTRAST ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Muted
    : String;
  begin
    Result := CR_CBF_MUTED ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Pale
    : String;
  begin
    Result := CR_CBF_PALE ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_RainbowDiscrete14
    : String;
  begin
    Result := CR_CBF_RAINBOWDISCRETE14 ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_RainbowDiscrete29
    : String;
  begin
    Result := CR_CBF_RAINBOWDISCRETE29 ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_RainbowSmooth
    : String;
  begin
    Result := CR_CBF_RAINBOWSMOOTH ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Sunset
    : String;
  begin
    Result := CR_CBF_SUNSET ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_Vibrant
    : String;
  begin
    Result := CR_CBF_VIBRANT ;
  end;

  class function TGIS_ColorRampNames.fget_CBF_OkabeIto
    : String;
  begin
    Result := CR_CBF_OKABEITO ;
  end;

  class function TGIS_ColorRampNames.fget_ChromaDepth
    : String;
  begin
    Result := CR_CHROMADEPTH ;
  end;

  class function TGIS_ColorRampNames.fget_Cividis
    : String;
  begin
    Result := CR_CIVIDIS ;
  end;

  class function TGIS_ColorRampNames.fget_Cold
    : String;
  begin
    Result := CR_COLD ;
  end;

  class function TGIS_ColorRampNames.fget_Copper
    : String;
  begin
    Result := CR_COPPER ;
  end;

  class function TGIS_ColorRampNames.fget_CoolWarm
    : String;
  begin
    Result := CR_COOLWARM ;
  end;

  class function TGIS_ColorRampNames.fget_CottonCandy
    : String;
  begin
    Result := CR_COTTONCANDY ;
  end;

  class function TGIS_ColorRampNames.fget_Cubehelix
    : String;
  begin
    Result := CR_CUBEHELIX ;
  end;

  class function TGIS_ColorRampNames.fget_CurrentDir
    : String;
  begin
    Result := CR_CURRENTDIR ;
  end;

  class function TGIS_ColorRampNames.fget_CurrentVelocity
    : String;
  begin
    Result := CR_CURRENTVELOCITY ;
  end;

  class function TGIS_ColorRampNames.fget_Curvature
    : String;
  begin
    Result := CR_CURVATURE ;
  end;

  class function TGIS_ColorRampNames.fget_Dark
    : String;
  begin
    Result := CR_DARK ;
  end;

  class function TGIS_ColorRampNames.fget_DEMPrint
    : String;
  begin
    Result := CR_DEMPRINT ;
  end;

  class function TGIS_ColorRampNames.fget_DEMScreen
    : String;
  begin
    Result := CR_DEMSCREEN ;
  end;

  class function TGIS_ColorRampNames.fget_Desert
    : String;
  begin
    Result := CR_DESERT ;
  end;

  class function TGIS_ColorRampNames.fget_Desertification
    : String;
  begin
    Result := CR_DESERTIFICATION ;
  end;

  class function TGIS_ColorRampNames.fget_Dirt
    : String;
  begin
    Result := CR_DIRT ;
  end;

  class function TGIS_ColorRampNames.fget_DireActive
    : String;
  begin
    Result := CR_DIREACTIVE ;
  end;

  class function TGIS_ColorRampNames.fget_DryWet
    : String;
  begin
    Result := CR_DRYWET ;
  end;

  class function TGIS_ColorRampNames.fget_Earth
    : String;
  begin
    Result := CR_EARTH ;
  end;

  class function TGIS_ColorRampNames.fget_Elevation1
    : String;
  begin
    Result := CR_ELEVATION1 ;
  end;

  class function TGIS_ColorRampNames.fget_Elevation2
    : String;
  begin
    Result := CR_ELEVATION2 ;
  end;

  class function TGIS_ColorRampNames.fget_Elevation3
    : String;
  begin
    Result := CR_ELEVATION3 ;
  end;

  class function TGIS_ColorRampNames.fget_ElevationDark
    : String;
  begin
    Result := CR_ELEVATIONDARK ;
  end;

  class function TGIS_ColorRampNames.fget_EqualMixed
    : String;
  begin
    Result := CR_EQUALMIXED ;
  end;

  class function TGIS_ColorRampNames.fget_Exploration
    : String;
  begin
    Result := CR_EXPLORATION ;
  end;

  class function TGIS_ColorRampNames.fget_Flag
    : String;
  begin
    Result := CR_FLAG ;
  end;

  class function TGIS_ColorRampNames.fget_Forecast
    : String;
  begin
    Result := CR_FORECAST ;
  end;

  class function TGIS_ColorRampNames.fget_Forest
    : String;
  begin
    Result := CR_FOREST ;
  end;

  class function TGIS_ColorRampNames.fget_Geology
    : String;
  begin
    Result := CR_GEOLOGY ;
  end;

  class function TGIS_ColorRampNames.fget_GlobalWarming
    : String;
  begin
    Result := CR_GLOBALWARMING ;
  end;

  class function TGIS_ColorRampNames.fget_Globe
    : String;
  begin
    Result := CR_GLOBE ;
  end;

  class function TGIS_ColorRampNames.fget_Gravity1
    : String;
  begin
    Result := CR_GRAVITY1 ;
  end;

  class function TGIS_ColorRampNames.fget_Gravity2
    : String;
  begin
    Result := CR_GRAVITY2 ;
  end;

  class function TGIS_ColorRampNames.fget_GreenBlue
    : String;
  begin
    Result := CR_GREENBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_Greens1
    : String;
  begin
    Result := CR_GREENS1 ;
  end;

  class function TGIS_ColorRampNames.fget_Greens2
    : String;
  begin
    Result := CR_GREENS2 ;
  end;

  class function TGIS_ColorRampNames.fget_Greys
    : String;
  begin
    Result := CR_GREYS ;
  end;

  class function TGIS_ColorRampNames.fget_Gnuplot
    : String;
  begin
    Result := CR_GNUPLOT ;
  end;

  class function TGIS_ColorRampNames.fget_Heat
    : String;
  begin
    Result := CR_HEAT ;
  end;

  class function TGIS_ColorRampNames.fget_HighPoints1
    : String;
  begin
    Result := CR_HIGHPOINTS1 ;
  end;

  class function TGIS_ColorRampNames.fget_HighPoints2
    : String;
  begin
    Result := CR_HIGHPOINTS2 ;
  end;

  class function TGIS_ColorRampNames.fget_Hot
    : String;
  begin
    Result := CR_HOT ;
  end;

  class function TGIS_ColorRampNames.fget_Humidity
    : String;
  begin
    Result := CR_HUMIDITY ;
  end;

  class function TGIS_ColorRampNames.fget_Ice1
    : String;
  begin
    Result := CR_ICE1 ;
  end;

  class function TGIS_ColorRampNames.fget_Ice2
    : String;
  begin
    Result := CR_ICE2 ;
  end;

  class function TGIS_ColorRampNames.fget_Icefire
    : String;
  begin
    Result := CR_ICEFIRE ;
  end;

  class function TGIS_ColorRampNames.fget_Inferno
    : String;
  begin
    Result := CR_INFERNO ;
  end;

  class function TGIS_ColorRampNames.fget_Jet
    : String;
  begin
    Result := CR_JET ;
  end;

  class function TGIS_ColorRampNames.fget_Land1
    : String;
  begin
    Result := CR_LAND1 ;
  end;

  class function TGIS_ColorRampNames.fget_Land2
    : String;
  begin
    Result := CR_LAND2 ;
  end;

  class function TGIS_ColorRampNames.fget_LandEurope
    : String;
  begin
    Result := CR_LANDEUROPE ;
  end;

  class function TGIS_ColorRampNames.fget_LandAmerica
    : String;
  begin
    Result := CR_LANDAMERICA ;
  end;

  class function TGIS_ColorRampNames.fget_LandArid
    : String;
  begin
    Result := CR_LANDARID ;
  end;

  class function TGIS_ColorRampNames.fget_LandSea
    : String;
  begin
    Result := CR_LANDSEA ;
  end;

  class function TGIS_ColorRampNames.fget_Magma
    : String;
  begin
    Result := CR_MAGMA ;
  end;

  class function TGIS_ColorRampNames.fget_Mako
    : String;
  begin
    Result := CR_MAKO ;
  end;

  class function TGIS_ColorRampNames.fget_Ocean1
    : String;
  begin
    Result := CR_OCEAN1 ;
  end;

  class function TGIS_ColorRampNames.fget_Ocean2
    : String;
  begin
    Result := CR_OCEAN2 ;
  end;

  class function TGIS_ColorRampNames.fget_OrangePurple
    : String;
  begin
    Result := CR_ORANGEPURPLE ;
  end;

  class function TGIS_ColorRampNames.fget_OrangeRed
    : String;
  begin
    Result := CR_ORANGERED ;
  end;

  class function TGIS_ColorRampNames.fget_Oranges
    : String;
  begin
    Result := CR_ORANGES ;
  end;

  class function TGIS_ColorRampNames.fget_Pastel1
    : String;
  begin
    Result := CR_PASTEL1 ;
  end;

  class function TGIS_ColorRampNames.fget_Pastel2
    : String;
  begin
    Result := CR_PASTEL2 ;
  end;

  class function TGIS_ColorRampNames.fget_Pastel3
    : String;
  begin
    Result := CR_PASTEL3 ;
  end;

  class function TGIS_ColorRampNames.fget_Pastel4
    : String;
  begin
    Result := CR_PASTEL4 ;
  end;

  class function TGIS_ColorRampNames.fget_Paired
    : String;
  begin
    Result := CR_PAIRED ;
  end;

  class function TGIS_ColorRampNames.fget_Pink
    : String;
  begin
    Result := CR_PINK ;
  end;

  class function TGIS_ColorRampNames.fget_Plasma
    : String;
  begin
    Result := CR_PLASMA ;
  end;

  class function TGIS_ColorRampNames.fget_PinkGreen
    : String;
  begin
    Result := CR_PINKGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_Prism
    : String;
  begin
    Result := CR_PRISM ;
  end;

  class function TGIS_ColorRampNames.fget_PurpleBlue
    : String;
  begin
    Result := CR_PURPLEBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_PurpleBlueGreen
    : String;
  begin
    Result := CR_PURPLEBLUEGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_PurpleGreen1
    : String;
  begin
    Result := CR_PURPLEGREEN1 ;
  end;

  class function TGIS_ColorRampNames.fget_PurpleGreen2
    : String;
  begin
    Result := CR_PURPLEGREEN2 ;
  end;

  class function TGIS_ColorRampNames.fget_PurpleRed
    : String;
  begin
    Result := CR_PURPLERED ;
  end;

  class function TGIS_ColorRampNames.fget_Purples
    : String;
  begin
    Result := CR_PURPLES ;
  end;

  class function TGIS_ColorRampNames.fget_Rainbow1
    : String;
  begin
    Result := CR_RAINBOW1 ;
  end;

  class function TGIS_ColorRampNames.fget_Rainbow2
    : String;
  begin
    Result := CR_RAINBOW2 ;
  end;

  class function TGIS_ColorRampNames.fget_Rainbow3
    : String;
  begin
    Result := CR_RAINBOW3 ;
  end;

  class function TGIS_ColorRampNames.fget_Rainbow4
    : String;
  begin
    Result := CR_RAINBOW4 ;
  end;

  class function TGIS_ColorRampNames.fget_Rainbow5
    : String;
  begin
    Result := CR_RAINBOW5 ;
  end;

  class function TGIS_ColorRampNames.fget_RainbowLight
    : String;
  begin
    Result := CR_RAINBOWLIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_RainbowPastel
    : String;
  begin
    Result := CR_RAINBOWPASTEL ;
  end;

  class function TGIS_ColorRampNames.fget_RedBlue
    : String;
  begin
    Result := CR_REDBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_RedGray
    : String;
  begin
    Result := CR_REDGRAY ;
  end;

  class function TGIS_ColorRampNames.fget_RedPurple
    : String;
  begin
    Result := CR_REDPURPLE ;
  end;

  class function TGIS_ColorRampNames.fget_RedYellowBlue
    : String;
  begin
    Result := CR_REDYELLOWBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_RedYellowGreen
    : String;
  begin
    Result := CR_REDYELLOWGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_RedHot
    : String;
  begin
    Result := CR_REDHOT ;
  end;

  class function TGIS_ColorRampNames.fget_Reds
    : String;
  begin
    Result := CR_REDS ;
  end;

  class function TGIS_ColorRampNames.fget_Rocket
    : String;
  begin
    Result := CR_ROCKET ;
  end;

  class function TGIS_ColorRampNames.fget_Sea
    : String;
  begin
    Result := CR_SEA ;
  end;

  class function TGIS_ColorRampNames.fget_Sealand
    : String;
  begin
    Result := CR_SEALAND ;
  end;

  class function TGIS_ColorRampNames.fget_Seismic
    : String;
  begin
    Result := CR_SEISMIC ;
  end;

  class function TGIS_ColorRampNames.fget_Set1
    : String;
  begin
    Result := CR_SET1 ;
  end;

  class function TGIS_ColorRampNames.fget_Set2
    : String;
  begin
    Result := CR_SET2 ;
  end;

  class function TGIS_ColorRampNames.fget_Set3
    : String;
  begin
    Result := CR_SET3 ;
  end;

  class function TGIS_ColorRampNames.fget_Soil
    : String;
  begin
    Result := CR_SOIL ;
  end;

  class function TGIS_ColorRampNames.fget_Spectral
    : String;
  begin
    Result := CR_SPECTRAL ;
  end;

  class function TGIS_ColorRampNames.fget_Spice
    : String;
  begin
    Result := CR_SPICE ;
  end;

  class function TGIS_ColorRampNames.fget_Spring
    : String;
  begin
    Result := CR_SPRING ;
  end;

  class function TGIS_ColorRampNames.fget_Stern
    : String;
  begin
    Result := CR_STERN ;
  end;

  class function TGIS_ColorRampNames.fget_Summer
    : String;
  begin
    Result := CR_SUMMER ;
  end;

  class function TGIS_ColorRampNames.fget_Sunset
    : String;
  begin
    Result := CR_SUNSET ;
  end;

  class function TGIS_ColorRampNames.fget_Tab10
    : String;
  begin
    Result := CR_TAB10 ;
  end;

  class function TGIS_ColorRampNames.fget_Tab20
    : String;
  begin
    Result := CR_TAB20 ;
  end;

  class function TGIS_ColorRampNames.fget_Tab20b
    : String;
  begin
    Result := CR_TAB20B ;
  end;

  class function TGIS_ColorRampNames.fget_Tab20c
    : String;
  begin
    Result := CR_TAB20C ;
  end;

  class function TGIS_ColorRampNames.fget_Temperature
    : String;
  begin
    Result := CR_TEMPERATURE ;
  end;

  class function TGIS_ColorRampNames.fget_Terrain
    : String;
  begin
    Result := CR_TERRAIN ;
  end;

  class function TGIS_ColorRampNames.fget_Topo1
    : String;
  begin
    Result := CR_TOPO1 ;
  end;

  class function TGIS_ColorRampNames.fget_Topo2
    : String;
  begin
    Result := CR_TOPO2 ;
  end;

  class function TGIS_ColorRampNames.fget_Turbo
    : String;
  begin
    Result := CR_TURBO ;
  end;

  class function TGIS_ColorRampNames.fget_Twilight
    : String;
  begin
    Result := CR_TWILIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_TwilightShifted
    : String;
  begin
    Result := CR_TWILIGHTSHIFTED ;
  end;

  class function TGIS_ColorRampNames.fget_Winter
    : String;
  begin
    Result := CR_WINTER ;
  end;

  class function TGIS_ColorRampNames.fget_Wistia
    : String;
  begin
    Result := CR_WISTIA ;
  end;

  class function TGIS_ColorRampNames.fget_Viridis
    : String;
  begin
    Result := CR_VIRIDIS ;
  end;

  class function TGIS_ColorRampNames.fget_YellowGreen
    : String;
  begin
    Result := CR_YELLOWGREEN ;
  end;

  class function TGIS_ColorRampNames.fget_YellowGreenBlue
    : String;
  begin
    Result := CR_YELLOWGREENBLUE ;
  end;

  class function TGIS_ColorRampNames.fget_YellowHigh
    : String;
  begin
    Result := CR_YELLOWHIGH ;
  end;

  class function TGIS_ColorRampNames.fget_YellowJacket
    : String;
  begin
    Result := CR_YELLOWJACKET ;
  end;

  class function TGIS_ColorRampNames.fget_YellowOrangeBrown
    : String;
  begin
    Result := CR_YELLOWORANGEBROWN ;
  end;

  class function TGIS_ColorRampNames.fget_YellowOrangeRed
    : String;
  begin
    Result := CR_YELLOWORANGERED ;
  end;

  class function TGIS_ColorRampNames.fget_Unique
    : String;
  begin
    Result := CR_UNIQUE ;
  end;

  class function TGIS_ColorRampNames.fget_UniqueBright
    : String;
  begin
    Result := CR_UNIQUEBRIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_UniqueDark
    : String;
  begin
    Result := CR_UNIQUEDARK ;
  end;

  class function TGIS_ColorRampNames.fget_UniqueDeep
    : String;
  begin
    Result := CR_UNIQUEDEEP ;
  end;

  class function TGIS_ColorRampNames.fget_UniqueDeepDark
    : String;
  begin
    Result := CR_UNIQUEDEEPDARK ;
  end;

  class function TGIS_ColorRampNames.fget_UniqueLight
    : String;
  begin
    Result := CR_UNIQUELIGHT ;
  end;

  class function TGIS_ColorRampNames.fget_UniquePastel
    : String;
  begin
    Result := CR_UNIQUEPASTEL ;
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
    LineDash := [] ;
  end;

  procedure TGIS_Pen.Assign( const _source : TGIS_Pen ) ;
  begin
    assert( assigned( _source ) ) ;

    self.Width    := _source.Width    ;
    self.Style    := _source.Style    ;
    self.LineCap  := _source.LineCap  ;
    self.Color    := _source.Color    ;
    self.LineJoin := _source.LineJoin ;
    self.LineDash := _source.LineDash ;
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

{$REGION 'TGIS_BitmapAbstract'}
{$IFDEF CLR}
  function TGIS_BitmapAbstract.GetData(
    const _target : TObject
  ) : TObject ;
{$ELSE}
  function TGIS_BitmapAbstract.GetData(
    const _target : IInterface
  ) : TObject ;
{$ENDIF}
begin
  Result := Data ;
end;
{$ENDREGION}

{$REGION 'TGIS_Bitmap'}
constructor TGIS_Bitmap.Create ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;
  FBitmapFactory := NativeBitmapFactory ;

  FPath   := ''  ;
  FBitmap := nil ;
end ;

constructor TGIS_Bitmap.Create(
  const _factory : TGIs_BitmapFactory
) ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;

  if not assigned( _factory ) then
    FBitmapFactory := NativeBitmapFactory
  else
    FBitmapFactory := _factory;

  FPath   := ''  ;
  FBitmap := nil ;
end;

constructor TGIS_Bitmap.Create(
  const _width  : Integer ;
  const _height : Integer
) ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;
  FBitmapFactory := NativeBitmapFactory ;

  FPath   := ''  ;
  FBitmap := FBitmapFactory.DoCreate( self, _width, _height ) ;
end;

constructor TGIS_Bitmap.Create(
  const _width   : Integer ;
  const _height  : Integer ;
  const _factory : TGIs_BitmapFactory
) ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;

  if not assigned( _factory ) then
    FBitmapFactory := NativeBitmapFactory
  else
    FBitmapFactory := _factory;

  FPath   := ''  ;
  FBitmap := FBitmapFactory.DoCreate( self, _width, _height ) ;
end;

constructor TGIS_Bitmap.Create(
  const _width   : Integer ;
  const _height  : Integer ;
  const _premult : Boolean
) ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;
  FBitmapFactory := NativeBitmapFactory ;

  FPath := '' ;
  FBitmap := FBitmapFactory.DoCreate( self, _width, _height ) ;
  FBitmap.Premultiplied := _premult ;
end;

constructor TGIS_Bitmap.Create(
  const _bmp : TGIS_Bitmap
) ;
begin
  inherited Create ;

  if not assigned (NativeBitmapFactory) then
    NativeBitmapFactory := TGIS_Utils.GisBitmapFactoryUniversal ;

  FPath := '' ;
  Assign( _bmp ) ;
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
  if not Result then
    Result := not assigned( FBitmap.Data ) ;
end ;

function TGIS_Bitmap.fget_NativeBitmap
  : TObject ;
begin
  Result := GetData( NativeBitmapFactory );
end;

procedure TGIS_Bitmap.fset_NativeBitmap(
  const _value : TObject
) ;
begin
  SetData( NativeBitmapFactory, _value ) ;
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

function TGIS_Bitmap.fget_PPI : Integer ;
begin
  if not assigned( FBitmap ) then begin
    result := 96 ;
    exit ;
  end ;

  Result := FBitmap.PPI ;
end ;

procedure TGIS_Bitmap.fset_PPI(
  const _value : Integer
) ;
begin
  if assigned( FBitmap ) then
    FBitmap.PPI := _value ;
end ;

procedure TGIS_Bitmap.Assign(
  _source : TGIS_Bitmap
) ;
begin
  FreeObject( FBitmap ) ;
  FPath := '' ;

  if assigned( _source ) then begin
    FBitmapFactory := _source.BitmapFactory ;
    if not TGIS_Bitmap.IsNilOrEmpty( _source ) then begin
      FBitmap := FBitmapFactory.DoCreateFromBitmap(
                   self, _source.GetData( FBitmapFactory )
                 ) ;
      FPath := _source.Path ;
      Premultiplied := _source.Premultiplied;
    end
  end
  else
    FBitmapFactory := NativeBitmapFactory ;
end ;


procedure TGIS_Bitmap.LoadFromFile(
  const _path : String
) ;
begin
  FreeObject( FBitmap ) ;

  if not FileExists( _path ) then
    exit ;

  FBitmap := FBitmapFactory.DoCreateFromFile( self, _path ) ;
  FPath := _path ;
end ;


procedure TGIS_Bitmap.LoadFromStream(
  const _stream : TObject
) ;
begin
  FreeObject( FBitmap ) ;

  FBitmap := FBitmapFactory.DoCreateFromStream( self, _stream ) ;
  FPath := '' ;
end ;


procedure TGIS_Bitmap.LoadFromResourceName(
  const _name : String
) ;
begin
  FBitmap := FBitmapFactory.DoCreateFromResource( self, 0, _name ) ;
end ;

procedure TGIS_Bitmap.LoadFromResourceName(
  const _ref  : IntPtr ;
  const _name : String
) ;
begin
  FBitmap := FBitmapFactory.DoCreateFromResource( self,_ref, _name ) ;
end ;

procedure TGIS_Bitmap.LoadFromBitmap(
  const _bitmap : TObject ;
  const _path   : String
) ;
begin
  FreeObject( FBitmap ) ;

  FBitmap := FBitmapFactory.DoCreateFromBitmap( self,_bitmap ) ;

  FPath := _path ;
end;


procedure TGIS_Bitmap.SaveToFile(
  const _path   : String
) ;
begin
  checkFBitmap;

  FBitmap.ToFile( _path );
end;

procedure TGIS_Bitmap.SaveToFile(
  const _path         : String ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
begin
  checkFBitmap;

  FBitmap.ToFile( _path, _format, _subformat, _compression );
end;

procedure TGIS_Bitmap.SaveToStream(
  const _stream : TObject
) ;
begin
  checkFBitmap;

  FBitmap.ToStream( _stream );
end;

procedure TGIS_Bitmap.SaveToStream(
  const _stream       : TObject ;
  const _format       : TGIS_PixelFormat ;
  const _subformat    : TGIS_PixelSubFormat ;
  const _compression  : Integer
) ;
begin
  checkFBitmap;

  FBitmap.ToStream( _stream, _format, _subformat, _compression );
end;

procedure TGIS_Bitmap.MakeTransparent ;
begin
  checkFBitmap;

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
  checkFBitmap;

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


procedure TGIS_Bitmap.Clear(
  const _color : TGIS_Color
) ;
begin
  if assigned( FBitmap ) then
    FBitmap.Clear( _color ) ;
end ;


procedure TGIS_Bitmap.LockPixels(
  var   _pixels   : TGIS_Pixels ;
  const _writable : Boolean     ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) ;
begin
  checkFBitmap;

  FBitmap.LockPixels( _pixels, _writable, _format, _order ) ;
end;

procedure TGIS_Bitmap.LockPixels(
  var   _pixels   : TGIS_Pixels
) ;
begin
  checkFBitmap;

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
  checkFBitmap;

  FBitmap.LockPixels( _pixels, _writable,
                      TGIS_BitmapFormat.Native,
                      TGIS_BitmapLinesOrder.Native
                    ) ;
end;

procedure TGIS_Bitmap.UnlockPixels ;
begin
  checkFBitmap;

  FBitmap.UnlockPixels ;
end;

{$IFDEF CLR}
  function TGIS_Bitmap.GetData(
    const _factory : TGIS_BitmapFactory;
    const _target  : TObject
  ) : TObject ;
{$ELSE}
  function TGIS_Bitmap.GetData(
    const _factory : TGIS_BitmapFactory;
    const _target  : IInterface
  ) : TObject ;
{$ENDIF}
var
  bmp : TGIS_BitmapAbstract ;
  src : TGIS_Pixels ;
  dst : TGIS_Pixels ;
begin
  if not assigned( FBitmap ) then begin
    Result := nil ;
    exit ;
  end ;

  if ( _factory = FBitmapFactory )  then begin
    Result := FBitmap.GetData( _target );
    exit ;
  end ;

  bmp := _factory.DoCreate( Self, FBitmap.Width, FBitmap.Height );
  try
    FBitmap.LockPixels( src, false, _factory.NativeFormat, _factory.NativeLineOrder );
    bmp.LockPixels( dst, true, _factory.NativeFormat, _factory.NativeLineOrder );
    {$IFDEF OXYGENE}
      GisCopyPixels( src, 0, dst, 0, src.Length ) ;
    {$ELSE}
      GisCopyPixels( src, 0, dst, 0, length( src ) ) ;
    {$ENDIF}
  finally
    FBitmap.UnlockPixels() ;
    bmp.UnlockPixels() ;

    FreeObject( FBitmap ) ;
    FBitmap := bmp ;
    FBitmapFactory := _factory ;
  end;
  Result := FBitmap.GetData( _target ) ;
end ;

procedure TGIS_Bitmap.SetData(
  const _factory : TGIS_BitmapFactory ;
  const _data    : TObject
) ;
begin
  FreeObject( FBitmap ) ;

  if _factory <> FBitmapFactory then
    FBitmapFactory := _factory ;

  FPath := '' ;
  if assigned( _data ) then
    FBitmap := FBitmapFactory.DoCreateFromBitmap( self, _data ) ;
end ;

procedure TGIS_Bitmap.DrawShape(
  const _shape    :  TObject ;
  const _outline  :  Boolean ;
  var   _scale    :  Double  ;
  var   _offset   :  TPoint
) ;
begin
  checkFBitmap;

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
  checkFBitmap;

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
  checkFBitmap;

  FBitmap.DrawShape( _shape, _ppi, _outline,
                     _areacolor, _linecolor,
                     _scale, _offset
                   ) ;
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
  checkFBitmap;

  FBitmap.DrawSymbol( _name )  ;
end;

procedure TGIS_Bitmap.DrawSymbol(
  const _name     :  String ;
  const _ppi      :  Integer
) ;
begin
  checkFBitmap;

  FBitmap.DrawSymbol( _name, _ppi )  ;
end;

procedure TGIS_Bitmap.DrawSymbol(
  const _name     :  String     ;
  const _ppi      :  Integer    ;
  const _areacolor:  TGIS_Color ;
  const _linecolor:  TGIS_Color
) ;
begin
  checkFBitmap;

  FBitmap.DrawSymbol( _name, _ppi, _areacolor, _linecolor )  ;
end;

procedure TGIS_Bitmap.DrawGlyph(
  const _symbol   :  TObject    ;
  const _ppi      :  Integer    ;
  const _color    :  TGIS_Color ;
  const _enabled  :  Boolean
) ;
begin
  checkFBitmap;

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
  Result := NativeBitmapFactory.NativeFormat ;
end;

class function TGIS_Bitmap.NativeLineOrder
  : TGIS_BitmapLinesOrder ;
begin
  Result := NativeBitmapFactory.NativeLineOrder ;
end;

function TGIS_Bitmap.BitmapType
  : TGIS_BitmapType ;
begin
  Result := FBitmapFactory.BitmapType
end ;

procedure TGIS_Bitmap.checkFBitmap;
begin
  if not assigned( FBitmap ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BITMAPDOESNOTEXISTS ), '',  0 ) ;
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
  Create ;

  FName    := _name ;
  FMap     := _map ;
  FSubMap  := _submap ;
  FMapType := _mapType ;
end ;

constructor TGIS_GradientMap.Create ;
begin
  FName    := '' ;
  FMap     := nil ;
  FSubMap  := nil ;
  FMapType := TGIS_ColorSchema.Sequential ;
  FColorIndex := 0 ;
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

function TGIS_GradientMap.NextColor : TGIS_Color ;
begin
  if FColorIndex >= length(FMap) then begin
    Result := TGIS_Color.None ;
    exit;
  end;

  Result := FMap[FColorIndex].RGB;

  inc(FColorIndex);
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

procedure TGIS_GradientMap.Reset;
begin
  FColorIndex := 0;
end;

{$ENDREGION}

{$REGION 'TGIS_GradientMapUnique'}
constructor TGIS_GradientMapUnique.Create;
begin
  inherited ;

  FHOffset := 0 ;
  FColorSpace := TGIS_ColorInterpolationMode.None ;

  FAMin   := 0 ;
  FAMax   := 1 ;
  FBMin   := 0 ;
  FBMin   := 1 ;
  FCMin   := 0 ;
  FCMin   := 1 ;
end;

constructor TGIS_GradientMapUnique.Create(
  const _name       : String ;
  const _mapType    : TGIS_ColorSchema ;
  const _aMin       : Double ;
  const _aMax       : Double ;
  const _bMin       : Double ;
  const _bMax       : Double ;
  const _cMin       : Double ;
  const _cMax       : Double ;
  const _colorSpace : TGIS_ColorInterpolationMode
) ;
begin
  inherited Create( _name, nil, nil, _mapType ) ;

  FColorSpace := _colorSpace ;

  // internally we use degrees 0..360
  FHOffset := GetRandom * 360 ;

  FAMin := _aMin ;
  FAMax := _aMax ;
  FBMin := _bMin ;
  FBMax := _bMax ;
  FCMin := _cMin ;
  FCMax := _cMax ;

  // gradient map need to have at least 1 color
  // to determine font color in the TGIS_ColorRampComboBox
  FMap := getBaseColorMap( 1 ) ;
end;

// Red from RGB
function TGIS_GradientMapUnique.fget_RMax: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FAMax ) ;
end;

function TGIS_GradientMapUnique.fget_RMin: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FAMin ) ;
end;

// Green from RGB
function TGIS_GradientMapUnique.fget_GMax: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FBMax ) ;
end;

function TGIS_GradientMapUnique.fget_GMin: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FBMin ) ;
end;

// Blue from RGB
function TGIS_GradientMapUnique.fget_BMax: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FCMax ) ;
end;

function TGIS_GradientMapUnique.fget_BMin: Byte;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.RGB then
    Result := RoundS( FCMin ) ;
end;

// Hue from HSL or HCL
function TGIS_GradientMapUnique.fget_HMax: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HCL )
  then
    Result := FAMax ;
end;

function TGIS_GradientMapUnique.fget_HMin: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HCL )
  then
    Result := FAMin ;
end;

// Saturation from HSL
function TGIS_GradientMapUnique.fget_SMax: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 )
  then
    Result := FBMax ;
end;

function TGIS_GradientMapUnique.fget_SMin: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 )
  then
    Result := FBMin ;
end;

// Chroma from HCL
function TGIS_GradientMapUnique.fget_CMax: Double;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.HCL then
    Result := FBMax ;
end;

function TGIS_GradientMapUnique.fget_CMin: Double;
begin
  Result := 0 ;
  if FColorSpace = TGIS_ColorInterpolationMode.HCL then
    Result := FBMin ;
end;

// Lightness from HSL or Luminence from HCL
function TGIS_GradientMapUnique.fget_LMax: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HCL )
  then
    Result := FCMax ;
end;

function TGIS_GradientMapUnique.fget_LMin: Double;
begin
  Result := 0 ;
  if ( FColorSpace = TGIS_ColorInterpolationMode.HSL ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HSL360 ) or
     ( FColorSpace = TGIS_ColorInterpolationMode.HCL )
  then
    Result := FCMin ;
end;

constructor TGIS_GradientMapUnique.Create(
  const _name    : String ;
  const _mapType : TGIS_ColorSchema ;
  const _hMin    : Double ;
  const _hMax    : Double ;
  const _sMin    : Double ;
  const _sMax    : Double ;
  const _lMin    : Double ;
  const _lMax    : Double
) ;
begin
  Create( _name, _mapType, _hMin, _hMax, _sMin, _sMax, _lMin, _lMax, TGIS_ColorInterpolationMode.HSL )
end;

function TGIS_GradientMapUnique.NextColor : TGIS_Color;
var
  h : Double ;
begin
  // we use internal FCurrentId index for generating significantly different subsequent colors
  h := FHOffset + ( FColorIndex * GOLDEN_ANGLE ) ;

  Result := CreateColorFromHue( h/360 ) ;

  inc( FColorIndex ) ;
end;

function TGIS_GradientMapUnique.getBaseColorMap(
  const _subClass : Integer
) : TGIS_ColorMapArray ;
var
  i, n_class           : Integer ;
  delta                : Double ;
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

  for i := 0 to n_class-1 do begin
    {$IFDEF GIS_NORECORDS}
      Result[i] := new TGIS_ColorMap ;
    {$ENDIF}
    Result[i].Index := i * delta ;
    Result[i].RGB := NextColor ;
  end ;
end;

function TGIS_GradientMapUnique.CreateColorFromHue( const _hue : Double ) : TGIS_Color ;
var
  h, s, l, c : Double ;
  h_min_deg, h_max_deg : Double ;
begin
  // DK Color API uses range 0..1, but internally we use degrees 0..360
  h := _hue * 360 ;

  // do not exceed 360 degrees
  h := h - 360 * FloorS( h/360 ) ;

  h_min_deg := HMin ;
  h_max_deg := HMax ;
  if (FColorSpace = TGIS_ColorInterpolationMode.HSL) or (FColorSpace = TGIS_ColorInterpolationMode.HSL) then begin
    h_min_deg := h_min_deg * 360 ;
    h_max_deg := h_max_deg * 360 ;
  end;

  // hue also may be limited
  h := h_min_deg + ( h_max_deg - h_min_deg ) * h/360 ;
  l := GetRandom * ( LMax - LMin ) + LMin ;

  case FColorSpace of
    TGIS_ColorInterpolationMode.RGB: ;
    TGIS_ColorInterpolationMode.HSL,
    TGIS_ColorInterpolationMode.HSL360: begin
      s := GetRandom * ( SMax - SMin ) + SMin ;
      Result := TGIS_Color.FromHSL( h/360, s, l ) ;
    end;
    TGIS_ColorInterpolationMode.HCL: begin
      c := GetRandom * ( CMax - CMin ) + CMin ;
      Result := TGIS_Color.FromHCL( h, c, l) ;
    end;
  end;
end ;

{$ENDREGION}

  procedure RegisterBitmapFactory(
    const _name  : String ;
    const _class : TGIS_BitmapFactoryClass
  ) ;
  begin
    if not assigned( BitmapFactoryList ) then
      BitmapFactoryList := TDictionary< String, TGIS_BitmapFactoryClass >.Create(
                {$IFDEF OXYGENE}
                  {$IFDEF JAVA}
                    java.lang.String.CASE_INSENSITIVE_ORDER
                  {$ENDIF}
                  {$IFDEF CLR}
                    StringComparer.OrdinalIgnoreCase
                  {$ENDIF}
                {$ELSE}
                  TIStringComparer.Ordinal
                {$ENDIF}
              ) ;
    BitmapFactoryList.Add( _name, _class ) ;
  end ;

//==================================== END =====================================

end.

