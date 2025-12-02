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
  GDI library; Basic HTMl Label renderer
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoHtmlLabel ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoHtmlLabel"'}
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

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,
    System.SysUtils,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoRendererAbstract,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI ;
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
  ///   Formatted HTML label.
  /// </summary>
  TGIS_HtmlLabel = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oBoundingBox : TRect         ;
      oRenderer    : TGIS_RendererAbstract ;
      oLines       : TGIS_ObjectList ;
      eAlign       : TGIS_LabelAlignment ;
      oAttributes  : TStringList   ;
      oPrevFont    : TGIS_Font     ;
      oPrevColor   : TGIS_Color    ;
      oElements    : TGIS_ObjectList ;
      iWidth       : Integer       ;
      iHeight      : Integer       ;
      cWidth       : Integer       ;
      cHeight      : Integer       ;
      iPos         : Integer       ;
      bError       : Boolean       ; // for doTokenXXX error handling
      sLabel       : String        ;
      bUpperCase   : Boolean       ;
      bLowerCase   : Boolean       ;
      bNbsp        : Boolean       ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure    doDestroy   ; override;
    public

      /// <summary>
      ///   Create an instance of an object.
      /// </summary>
      /// <param name="_renderer">
      ///   canvas object on which label should be drawn
      /// </param>
      /// <param name="_text">
      ///   HTML text of the label
      /// </param>
      /// <param name="_align">
      ///   label alignment
      /// </param>
      /// <param name="_width">
      ///   maximum label width
      /// </param>
      /// <param name="_height">
      ///   maximum label height
      /// </param>
      constructor  Create      ( const _renderer     : TGIS_RendererAbstract ;
                                 const _text         : String   ;
                                 const _align        : TGIS_LabelAlignment ;
                                 const _width        : Integer  ;
                                 const _height       : Integer
                               ) ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Parse next token from the HTML text.
      /// </summary>
      function     doTokenHtml  : Boolean ;

      /// <summary>
      ///   Parse next token from the non HTML text.
      /// </summary>
      function     doTokenPlain : Boolean ;

      /// <summary>
      ///   Interpret special character like &amp;nbsp; &amp;gt; &amp;lt
      /// </summary>
      /// <param name="_value">
      ///   special character name
      /// </param>
      function     doSpecChar   ( const _value        : String
                                ) : String ;

      /// <summary>
      ///   Interpret single marker.
      /// </summary>
      /// <param name="_value">
      ///   marker name
      /// </param>
      function     doMarker     ( const _value        : String
                                ) : String ;

      /// <summary>
      ///   Add a single text (single word) into the list of all text fragments.
      /// </summary>
      procedure    addText      ( const _txt          : String
                                ) ;

      /// <summary>
      ///   Perform HTML label parsing. Tokenize HTML and interpret markers.
      /// </summary>
      procedure    doParse      ;

      /// <summary>
      ///   Perform line formatting. Split text based on end of line markers or
      ///   split on space to create text flow.
      /// </summary>
      procedure    doFormat     ;

      /// <summary>
      ///   Measure maximum label width and height.
      /// </summary>
      procedure    doMeasure    ;

      /// <summary>
      ///   Perform label drawing.
      /// </summary>
      /// <param name="_rect">
      ///   bounding rectangle of the label
      /// </param>
      /// <param name="_force_color">
      ///   if true then all label text will be drawn with _forced_color; used
      ///   for shadowing of not true-type font
      /// </param>
      /// <param name="_forced_color">
      ///   color used by _force_color state
      /// </param>
      procedure    doDraw       ( const _rect         : TRect    ;
                                  const _force_color  : Boolean  ;
                                  const _forced_color : TGIS_Color
                                ) ;

    public
      /// <summary>
      ///   Perform label drawing.
      /// </summary>
      /// <param name="_rect">
      ///   bounding rectangle of the label
      /// </param>
      /// <param name="_angle">
      ///   rotation angle in radians
      /// </param>
      /// <param name="_origin">
      ///   origin point
      /// </param>
      /// <param name="_shadow_width">
      ///   shadow width
      /// </param>
      /// <param name="_shadow_color">
      ///   shadow color
      /// </param>
      procedure    Draw         ( const _rect         : TRect    ;
                                  const _angle        : Double   ;
                                  const _origin       : TPoint   ;
                                  const _shadow_width : Integer  ;
                                  const _shadow_color : TGIS_Color
                                ) ;

    public

      /// <summary>
      ///   Box within which text could be placed.
      ///   Will never be bigger then then box with width and hight provided in
      ///   a constructor.
      /// </summary>
      property BoundingBox : TRect read oBoundingBox ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,

    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}

type

  { Single HTML text (word).
    Contains text and font object which will be used for formatting.
  }
  T_htmlText = class( TGIS_ObjectDisposable )
    oRenderer    : TGIS_RendererAbstract  ;
    oFont        : TGIS_Font ;
    sText        : String    ;
    iSize        : TPoint    ;
    iEmSpace     : Integer   ;
    tmBreakChar  : Char      ;
    tmHeight     : Integer   ;
    tmAscent     : Integer   ;
    bHorzLine    : Boolean   ;
    bNewLine     : Boolean   ;
    bSpace       : Boolean   ;
    iBiDi        : Integer   ;

    protected
      procedure doDestroy ; override;
    public
      constructor Create  ( const _renderer : TGIS_RendererAbstract ;
                            const _txt      : String
                          )  ;
  end ;

  { Single HTML line.
    Contains a set of T_htmlText objects which will form a single line.
  }
  T_htmlLine = class( TGIS_ObjectDisposable )
    iWidth        : Integer ;
    iWidthNoTrail : Integer ;
    iEmSpaceBeg   : Integer ;
    iEmSpaceEnd   : Integer ;
    iHeight       : Integer ;
    iAscent       : Integer ;
    iYOffset      : Integer ;
    oElements     : TGIS_ObjectList ;
    bCanBreak     : Boolean ;
    iLastLR       : Integer ;

    protected
      procedure   doDestroy ; override;
    public
      constructor Create  ;
    private
      function getIsEmpty  : Boolean ;
    public
      procedure AddText( const _element : T_htmlText
                       ) ;
      procedure Draw   ( const _x            : Integer ;
                         const _y            : Integer ;
                         const _xoff         : Integer ;
                         const _width        : Integer
                       ) ; overload;
      procedure Draw   ( const _x            : Integer ;
                         const _y            : Integer ;
                         const _xoff         : Integer ;
                         const _width        : Integer ;
                         const _forced_color : TGIS_Color
                       ) ; overload;
    public
      property  IsEmpty    : Boolean read  getIsEmpty ;
      property  Width      : Integer read  iWidth     ;
      property  CanBreak   : Boolean read  bCanBreak  ;
  end ;

const
  HTML_HR        = #01 ;
  HTML_CR        = #13 ;
  HTML_NBSP      = #02 ;
  BIDI_WEAK      = 0 ;
  BIDI_LR_STRONG = 1 ;
  BIDI_RL_STRONG = 2 ;


const Unicode_BasicLatin                           = 1;
const Unicode_Latin_1Supplement                    = 2;
const Unicode_LatinExtended_A                      = 3;
const Unicode_LatinExtended_B                      = 4;
const Unicode_IPAExtensions                        = 5;
const Unicode_SpacingModifierLetters               = 6;
const Unicode_CombiningDiacriticalMarks            = 7;
const Unicode_Greek                                = 8;
const Unicode_GreekandCoptic                       = Unicode_Greek;
const Unicode_Cyrillic                             = 9;
const Unicode_CyrillicSupplement                   = 10;
const Unicode_Armenian                             = 11;
const Unicode_Hebrew                               = 12;
const Unicode_Arabic                               = 13;
const Unicode_Syriac                               = 14;
const Unicode_Thaana                               = 15;
const Unicode_Devanagari                           = 16;
const Unicode_Bengali                              = 17;
const Unicode_Gurmukhi                             = 18;
const Unicode_Gujarati                             = 19;
const Unicode_Oriya                                = 20;
const Unicode_Tamil                                = 21;
const Unicode_Telugu                               = 22;
const Unicode_Kannada                              = 23;
const Unicode_Malayalam                            = 24;
const Unicode_Sinhala                              = 25;
const Unicode_Thai                                 = 26;
const Unicode_Lao                                  = 27;
const Unicode_Tibetan                              = 28;
const Unicode_Myanmar                              = 29;
const Unicode_Georgian                             = 30;
const Unicode_HangulJamo                           = 31;
const Unicode_Ethiopic                             = 32;
const Unicode_Cherokee                             = 33;
const Unicode_UnifiedCanadianAboriginalSyllabics   = 34;
const Unicode_Ogham                                = 35;
const Unicode_Runic                                = 36;
const Unicode_Tagalog                              = 37;
const Unicode_Hanunoo                              = 38;
const Unicode_Buhid                                = 39;
const Unicode_Tagbanwa                             = 40;
const Unicode_Khmer                                = 41;
const Unicode_Mongolian                            = 42;
const Unicode_Limbu                                = 43;
const Unicode_TaiLe                                = 44;
const Unicode_KhmerSymbols                         = 45;
const Unicode_PhoneticExtensions                   = 46;
const Unicode_LatinExtendedAdditional              = 47;
const Unicode_GreekExtended                        = 48;
const Unicode_GeneralPunctuation                   = 49;
const Unicode_SuperscriptsandSubscripts            = 50;
const Unicode_CurrencySymbols                      = 51;
const Unicode_CombiningDiacriticalMarksforSymbols  = 52;
const Unicode_CombiningMarksforSymbols             = Unicode_CombiningDiacriticalMarksforSymbols;
const Unicode_LetterlikeSymbols                    = 53;
const Unicode_NumberForms                          = 54;
const Unicode_Arrows                               = 55;
const Unicode_MathematicalOperators                = 56;
const Unicode_MiscellaneousTechnical               = 57;
const Unicode_ControlPictures                      = 58;
const Unicode_OpticalCharacterRecognition          = 59;
const Unicode_EnclosedAlphanumerics                = 60;
const Unicode_BoxDrawing                           = 61;
const Unicode_BlockElements                        = 62;
const Unicode_GeometricShapes                      = 63;
const Unicode_MiscellaneousSymbols                 = 64;
const Unicode_Dingbats                             = 65;
const Unicode_MiscellaneousMathematicalSymbols_A   = 66;
const Unicode_SupplementalArrows_A                 = 67;
const Unicode_BraillePatterns                      = 68;
const Unicode_SupplementalArrows_B                 = 69;
const Unicode_MiscellaneousMathematicalSymbols_B   = 70;
const Unicode_SupplementalMathematicalOperators    = 71;
const Unicode_MiscellaneousSymbolsandArrows        = 72;
const Unicode_CJKRadicalsSupplement                = 73;
const Unicode_KangxiRadicals                       = 74;
const Unicode_IdeographicDescriptionCharacters     = 75;
const Unicode_CJKSymbolsandPunctuation             = 78;
const Unicode_Hiragana                             = 77;
const Unicode_Katakana                             = 78;
const Unicode_Bopomofo                             = 79;
const Unicode_HangulCompatibilityJamo              = 80;
const Unicode_Kanbun                               = 81;
const Unicode_BopomofoExtended                     = 82;
const Unicode_KatakanaPhoneticExtensions           = 83;
const Unicode_EnclosedCJKLettersandMonths          = 84;
const Unicode_CJKCompatibility                     = 85;
const Unicode_CJKUnifiedIdeographsExtensionA       = 86;
const Unicode_YijingHexagramSymbols                = 87;
const Unicode_CJKUnifiedIdeographs                 = 88;
const Unicode_YiSyllables                          = 89;
const Unicode_YiRadicals                           = 90;
const Unicode_HangulSyllables                      = 91;
const Unicode_HighSurrogates                       = 92;
const Unicode_HighPrivateUseSurrogates             = 93;
const Unicode_LowSurrogates                        = 94;
const Unicode_PrivateUse                           = 95 ;
const Unicode_PrivateUseArea                       = Unicode_PrivateUse;
const Unicode_CJKCompatibilityIdeographs           = 96;
const Unicode_AlphabeticPresentationForms          = 97;
const Unicode_ArabicPresentationForms_A            = 98;
const Unicode_VariationSelectors                   = 99;
const Unicode_CombiningHalfMarks                   = 100;
const Unicode_CJKCompatibilityForms                = 101;
const Unicode_SmallFormVariants                    = 102;
const Unicode_ArabicPresentationForms_B            = 103;
const Unicode_HalfwidthandFullwidthForms           = 104;
const Unicode_Specials                             = 105;

function getUnicodeBlock(
  const _c : Char
)  : Integer ;
var
  c : Integer ;
begin
  c := ord( _c ) ;
  if ( c >= $0000 ) and ( c <= $007F ) then
    Result := Unicode_BasicLatin
  else
  if ( c >= $0080 ) and ( c <= $00FF ) then
    Result := Unicode_Latin_1Supplement
  else
  if ( c >= $0100 ) and ( c <= $017F ) then
    Result := Unicode_LatinExtended_A
  else
  if ( c >= $0180 ) and ( c <= $024F ) then
    Result := Unicode_LatinExtended_B
  else
  if ( c >= $0250 ) and ( c <= $02AF ) then
    Result := Unicode_IPAExtensions
  else
  if ( c >= $02B0 ) and ( c <= $02FF ) then
    Result := Unicode_SpacingModifierLetters
  else
  if ( c >= $0300 ) and ( c <= $036F ) then
    Result := Unicode_CombiningDiacriticalMarks
  else
  if ( c >= $0370 ) and ( c <= $03FF ) then
    Result := Unicode_Greek
  else
  if ( c >= $0400 ) and ( c <= $04FF ) then
    Result := Unicode_Cyrillic
  else
  if ( c >= $0500 ) and ( c <= $052F ) then
    Result := Unicode_CyrillicSupplement
  else
  if ( c >= $0530 ) and ( c <= $058F ) then
    Result := Unicode_Armenian
  else
  if ( c >= $0590 ) and ( c <= $05FF ) then
    Result := Unicode_Hebrew
  else
  if ( c >= $0600 ) and ( c <= $06FF ) then
    Result := Unicode_Arabic
  else
  if ( c >= $0700 ) and ( c <= $074F ) then
    Result := Unicode_Syriac
  else
  if ( c >= $0780 ) and ( c <= $07BF ) then
    Result := Unicode_Thaana
  else
  if ( c >= $0900 ) and ( c <= $097F ) then
    Result := Unicode_Devanagari
  else
  if ( c >= $0980 ) and ( c <= $09FF ) then
    Result := Unicode_Bengali
  else
  if ( c >= $0A00 ) and ( c <= $0A7F ) then
    Result := Unicode_Gurmukhi
  else
  if ( c >= $0A80 ) and ( c <= $0AFF ) then
    Result := Unicode_Gujarati
  else
  if ( c >= $0B00 ) and ( c <= $0B7F ) then
    Result := Unicode_Oriya
  else
  if ( c >= $0B80 ) and ( c <= $0BFF ) then
    Result := Unicode_Tamil
  else
  if ( c >= $0C00 ) and ( c <= $0C7F ) then
    Result := Unicode_Telugu
  else
  if ( c >= $0C80 ) and ( c <= $0CFF ) then
    Result := Unicode_Kannada
  else
  if ( c >= $0D00 ) and ( c <= $0D7F ) then
    Result := Unicode_Malayalam
  else
  if ( c >= $0D80 ) and ( c <= $0DFF ) then
    Result := Unicode_Sinhala
  else
  if ( c >= $0E00 ) and ( c <= $0E7F ) then
    Result := Unicode_Thai
  else
  if ( c >= $0E80 ) and ( c <= $0EFF ) then
    Result := Unicode_Lao
  else
  if ( c >= $0F00 ) and ( c <= $0FFF ) then
    Result := Unicode_Tibetan
  else
  if ( c >= $1000 ) and ( c <= $109F ) then
    Result := Unicode_Myanmar
  else
  if ( c >= $10A0 ) and ( c <= $10FF ) then
    Result := Unicode_Georgian
  else
  if ( c >= $1100 ) and ( c <= $11FF ) then
    Result := Unicode_HangulJamo
  else
  if ( c >= $1200 ) and ( c <= $137F ) then
    Result := Unicode_Ethiopic
  else
  if ( c >= $13A0 ) and ( c <= $13FF ) then
    Result := Unicode_Cherokee
  else
  if ( c >= $1400 ) and ( c <= $167F ) then
    Result := Unicode_UnifiedCanadianAboriginalSyllabics
  else
  if ( c >= $1680 ) and ( c <= $169F ) then
    Result := Unicode_Ogham
  else
  if ( c >= $16A0 ) and ( c <= $16FF ) then
    Result := Unicode_Runic
  else
  if ( c >= $1700 ) and ( c <= $171F ) then
    Result := Unicode_Tagalog
  else
  if ( c >= $1720 ) and ( c <= $173F ) then
    Result := Unicode_Hanunoo
  else
  if ( c >= $1740 ) and ( c <= $175F ) then
    Result := Unicode_Buhid
  else
  if ( c >= $1760 ) and ( c <= $177F ) then
    Result := Unicode_Tagbanwa
  else
  if ( c >= $1780 ) and ( c <= $17FF ) then
    Result := Unicode_Khmer
  else
  if ( c >= $1800 ) and ( c <= $18AF ) then
    Result := Unicode_Mongolian
  else
  if ( c >= $1900 ) and ( c <= $194F ) then
    Result := Unicode_Limbu
  else
  if ( c >= $1950 ) and ( c <= $197F ) then
    Result := Unicode_TaiLe
  else
  if ( c >= $19E0 ) and ( c <= $19FF ) then
    Result := Unicode_KhmerSymbols
  else
  if ( c >= $1D00 ) and ( c <= $1D7F ) then
    Result := Unicode_PhoneticExtensions
  else
  if ( c >= $1E00 ) and ( c <= $1EFF ) then
    Result := Unicode_LatinExtendedAdditional
  else
  if ( c >= $1F00 ) and ( c <= $1FFF ) then
    Result := Unicode_GreekExtended
  else
  if ( c >= $2000 ) and ( c <= $206F ) then
    Result := Unicode_GeneralPunctuation
  else
  if ( c >= $2070 ) and ( c <= $209F ) then
    Result := Unicode_SuperscriptsandSubscripts
  else
  if ( c >= $20A0 ) and ( c <= $20CF ) then
    Result := Unicode_CurrencySymbols
  else
  if ( c >= $20D0 ) and ( c <= $20FF ) then
    Result := Unicode_CombiningDiacriticalMarksforSymbols
  else
  if ( c >= $2100 ) and ( c <= $214F ) then
    Result := Unicode_LetterlikeSymbols
  else
  if ( c >= $2150 ) and ( c <= $218F ) then
    Result := Unicode_NumberForms
  else
  if ( c >= $2190 ) and ( c <= $21FF ) then
    Result := Unicode_Arrows
  else
  if ( c >= $2200 ) and ( c <= $22FF ) then
    Result := Unicode_MathematicalOperators
  else
  if ( c >= $2300 ) and ( c <= $23FF ) then
    Result := Unicode_MiscellaneousTechnical
  else
  if ( c >= $2400 ) and ( c <= $243F ) then
    Result := Unicode_ControlPictures
  else
  if ( c >= $2440 ) and ( c <= $245F ) then
    Result := Unicode_OpticalCharacterRecognition
  else
  if ( c >= $2460 ) and ( c <= $24FF ) then
    Result := Unicode_EnclosedAlphanumerics
  else
  if ( c >= $2500 ) and ( c <= $257F ) then
    Result := Unicode_BoxDrawing
  else
  if ( c >= $2580 ) and ( c <= $259F ) then
    Result := Unicode_BlockElements
  else
  if ( c >= $25A0 ) and ( c <= $25FF ) then
    Result := Unicode_GeometricShapes
  else
  if ( c >= $2600 ) and ( c <= $26FF ) then
    Result := Unicode_MiscellaneousSymbols
  else
  if ( c >= $2700 ) and ( c <= $27BF ) then
    Result := Unicode_Dingbats
  else
  if ( c >= $27C0 ) and ( c <= $27EF ) then
    Result := Unicode_MiscellaneousMathematicalSymbols_A
  else
  if ( c >= $27F0 ) and ( c <= $27FF ) then
    Result := Unicode_SupplementalArrows_A
  else
  if ( c >= $2800 ) and ( c <= $28FF ) then
    Result := Unicode_BraillePatterns
  else
  if ( c >= $2900 ) and ( c <= $297F ) then
    Result := Unicode_SupplementalArrows_B
  else
  if ( c >= $2980 ) and ( c <= $29FF ) then
    Result := Unicode_MiscellaneousMathematicalSymbols_B
  else
  if ( c >= $2A00 ) and ( c <= $2AFF ) then
    Result := Unicode_SupplementalMathematicalOperators
  else
  if ( c >= $2B00 ) and ( c <= $2BFF ) then
    Result := Unicode_MiscellaneousSymbolsandArrows
  else
  if ( c >= $2E80 ) and ( c <= $2EFF ) then
    Result := Unicode_CJKRadicalsSupplement
  else
  if ( c >= $2F00 ) and ( c <= $2FDF ) then
    Result := Unicode_KangxiRadicals
  else
  if ( c >= $2FF0 ) and ( c <= $2FFF ) then
    Result := Unicode_IdeographicDescriptionCharacters
  else
  if ( c >= $3000 ) and ( c <= $303F ) then
    Result := Unicode_CJKSymbolsandPunctuation
  else
  if ( c >= $3040 ) and ( c <= $309F ) then
    Result := Unicode_Hiragana
  else
  if ( c >= $30A0 ) and ( c <= $30FF ) then
    Result := Unicode_Katakana
  else
  if ( c >= $3100 ) and ( c <= $312F ) then
    Result := Unicode_Bopomofo
  else
  if ( c >= $3130 ) and ( c <= $318F ) then
    Result := Unicode_HangulCompatibilityJamo
  else
  if ( c >= $3190 ) and ( c <= $319F ) then
    Result := Unicode_Kanbun
  else
  if ( c >= $31A0 ) and ( c <= $31BF ) then
    Result := Unicode_BopomofoExtended
  else
  if ( c >= $31F0 ) and ( c <= $31FF ) then
    Result := Unicode_KatakanaPhoneticExtensions
  else
  if ( c >= $3200 ) and ( c <= $32FF ) then
    Result := Unicode_EnclosedCJKLettersandMonths
  else
  if ( c >= $3300 ) and ( c <= $33FF ) then
    Result := Unicode_CJKCompatibility
  else
  if ( c >= $3400 ) and ( c <= $4DBF ) then
    Result := Unicode_CJKUnifiedIdeographsExtensionA
  else
  if ( c >= $4DC0 ) and ( c <= $4DFF ) then
    Result := Unicode_YijingHexagramSymbols
  else
  if ( c >= $4E00 ) and ( c <= $9FFF ) then
    Result := Unicode_CJKUnifiedIdeographs
  else
  if ( c >= $A000 ) and ( c <= $A48F ) then
    Result := Unicode_YiSyllables
  else
  if ( c >= $A490 ) and ( c <= $A4CF ) then
    Result := Unicode_YiRadicals
  else
  if ( c >= $AC00 ) and ( c <= $D7AF ) then
    Result := Unicode_HangulSyllables
  else
  if ( c >= $D800 ) and ( c <= $DB7F ) then
    Result := Unicode_HighSurrogates
  else
  if ( c >= $DB80 ) and ( c <= $DBFF ) then
    Result := Unicode_HighPrivateUseSurrogates
  else
  if ( c >= $DC00 ) and ( c <= $DFFF ) then
    Result := Unicode_LowSurrogates
  else
  if ( c >= $E000 ) and ( c <= $F8FF ) then
    Result := Unicode_PrivateUse or Unicode_PrivateUseArea
  else
  if ( c >= $F900 ) and ( c <= $FAFF ) then
    Result := Unicode_CJKCompatibilityIdeographs
  else
  if ( c >= $FB00 ) and ( c <= $FB4F ) then
    Result := Unicode_AlphabeticPresentationForms
  else
  if ( c >= $FB50 ) and ( c <= $FDFF ) then
    Result := Unicode_ArabicPresentationForms_A
  else
  if ( c >= $FE00 ) and ( c <= $FE0F ) then
    Result := Unicode_VariationSelectors
  else
  if ( c >= $FE20 ) and ( c <= $FE2F ) then
    Result := Unicode_CombiningHalfMarks
  else
  if ( c >= $FE30 ) and ( c <= $FE4F ) then
    Result := Unicode_CJKCompatibilityForms
  else
  if ( c >= $FE50 ) and ( c <= $FE6F ) then
    Result := Unicode_SmallFormVariants
  else
  if ( c >= $FE70 ) and ( c <= $FEFF ) then
    Result := Unicode_ArabicPresentationForms_B
  else
  if ( c >= $FF00 ) and ( c <= $FFEF ) then
    Result := Unicode_HalfwidthandFullwidthForms
  else
  if ( c >= $FFF0 ) and ( c <= $FFFF ) then
    Result := Unicode_Specials
end;

function testBiDiChar(
  const _char : Char
) : Integer ;
var
  c : Integer ;
begin
  Result := BIDI_LR_STRONG ;

  c := ord( _char ) ;

  // WEAK & NEUTRAL
  if (c <=$000041)                            then Result := BIDI_WEAK

  // RL characters
  else if (c >= $005BE) and (c <= $10B7F) then
  begin
    if (c =  $0200F)                          then Result := BIDI_RL_STRONG
    else if c <= $0085E then begin
      if      (c =  $005BE)                   then Result := BIDI_RL_STRONG
      else if (c =  $005C0)                   then Result := BIDI_RL_STRONG
      else if (c =  $005C3)                   then Result := BIDI_RL_STRONG
      else if (c =  $005C6)                   then Result := BIDI_RL_STRONG
      else if (c >= $005D0) and (c <= $005EA) then Result := BIDI_RL_STRONG
      else if (c >= $005F0) and (c <= $005F4) then Result := BIDI_RL_STRONG
      else if (c =  $00608)                   then Result := BIDI_RL_STRONG
      else if (c =  $0060B)                   then Result := BIDI_RL_STRONG
      else if (c =  $0060D)                   then Result := BIDI_RL_STRONG
      else if (c =  $0061B)                   then Result := BIDI_RL_STRONG
      else if (c >= $0061E) and (c <= $0064A) then Result := BIDI_RL_STRONG
      else if (c >= $0066D) and (c <= $0066F) then Result := BIDI_RL_STRONG
      else if (c >= $00671) and (c <= $006D5) then Result := BIDI_RL_STRONG
      else if (c >= $006E5) and (c <= $006E6) then Result := BIDI_RL_STRONG
      else if (c >= $006EE) and (c <= $006EF) then Result := BIDI_RL_STRONG
      else if (c >= $006FA) and (c <= $0070D) then Result := BIDI_RL_STRONG
      else if (c =  $00710)                   then Result := BIDI_RL_STRONG
      else if (c >= $00712) and (c <= $0072F) then Result := BIDI_RL_STRONG
      else if (c >= $0074D) and (c <= $007A5) then Result := BIDI_RL_STRONG
      else if (c =  $007B1)                   then Result := BIDI_RL_STRONG
      else if (c >= $007C0) and (c <= $007EA) then Result := BIDI_RL_STRONG
      else if (c >= $007F4) and (c <= $007F5) then Result := BIDI_RL_STRONG
      else if (c =  $007FA)                   then Result := BIDI_RL_STRONG
      else if (c >= $00800) and (c <= $00815) then Result := BIDI_RL_STRONG
      else if (c =  $0081A)                   then Result := BIDI_RL_STRONG
      else if (c =  $00824)                   then Result := BIDI_RL_STRONG
      else if (c =  $00828)                   then Result := BIDI_RL_STRONG
      else if (c >= $00830) and (c <= $0083E) then Result := BIDI_RL_STRONG
      else if (c >= $00840) and (c <= $00858) then Result := BIDI_RL_STRONG
      else if (c =  $0085E)                   then Result := BIDI_RL_STRONG
    end
    else if (c >= $0FB1D) then begin
      if      (c =  $0FB1D)                   then Result := BIDI_RL_STRONG
      else if (c >= $0FB1F) and (c <= $0FB28) then Result := BIDI_RL_STRONG
      else if (c >= $0FB2A) and (c <= $0FB36) then Result := BIDI_RL_STRONG
      else if (c >= $0FB38) and (c <= $0FB3C) then Result := BIDI_RL_STRONG
      else if (c =  $0FB3E)                   then Result := BIDI_RL_STRONG
      else if (c >= $0FB40) and (c <= $0FB41) then Result := BIDI_RL_STRONG
      else if (c >= $0FB43) and (c <= $0FB44) then Result := BIDI_RL_STRONG
      else if (c >= $0FB46) and (c <= $0FBC1) then Result := BIDI_RL_STRONG
      else if (c >= $0FBD3) and (c <= $0FD3D) then Result := BIDI_RL_STRONG
      else if (c >= $0FD50) and (c <= $0FD8F) then Result := BIDI_RL_STRONG
      else if (c >= $0FD92) and (c <= $0FDC7) then Result := BIDI_RL_STRONG
      else if (c >= $0FDF0) and (c <= $0FDFC) then Result := BIDI_RL_STRONG
      else if (c >= $0FE70) and (c <= $0FE74) then Result := BIDI_RL_STRONG
      else if (c >= $0FE76) and (c <= $0FEFC) then Result := BIDI_RL_STRONG
      else if (c >= $10800) and (c <= $10805) then Result := BIDI_RL_STRONG
      else if (c =  $10808)                   then Result := BIDI_RL_STRONG
      else if (c >= $1080A) and (c <= $10835) then Result := BIDI_RL_STRONG
      else if (c >= $10837) and (c <= $10838) then Result := BIDI_RL_STRONG
      else if (c =  $1083C)                   then Result := BIDI_RL_STRONG
      else if (c >= $1083F) and (c <= $10855) then Result := BIDI_RL_STRONG
      else if (c >= $10857) and (c <= $1085F) then Result := BIDI_RL_STRONG
      else if (c >= $10900) and (c <= $1091B) then Result := BIDI_RL_STRONG
      else if (c >= $10920) and (c <= $10939) then Result := BIDI_RL_STRONG
      else if (c =  $1093F)                   then Result := BIDI_RL_STRONG
      else if (c =  $10A00)                   then Result := BIDI_RL_STRONG
      else if (c >= $10A10) and (c <= $10A13) then Result := BIDI_RL_STRONG
      else if (c >= $10A15) and (c <= $10A17) then Result := BIDI_RL_STRONG
      else if (c >= $10A19) and (c <= $10A33) then Result := BIDI_RL_STRONG
      else if (c >= $10A40) and (c <= $10A47) then Result := BIDI_RL_STRONG
      else if (c >= $10A50) and (c <= $10A58) then Result := BIDI_RL_STRONG
      else if (c >= $10A60) and (c <= $10A7F) then Result := BIDI_RL_STRONG
      else if (c >= $10B00) and (c <= $10B35) then Result := BIDI_RL_STRONG
      else if (c >= $10B40) and (c <= $10B55) then Result := BIDI_RL_STRONG
      else if (c >= $10B58) and (c <= $10B72) then Result := BIDI_RL_STRONG
      else if (c >= $10B78) and (c <= $10B7F) then Result := BIDI_RL_STRONG
    end ;
  end;
end;


//==============================================================================
// T_htmlText
//==============================================================================

  constructor T_htmlText.Create(
    const _renderer : TGIS_RendererAbstract ;
    const _txt      : String
  ) ;
  begin
    inherited Create ;

    iBiDi := BIDI_WEAK ;

    oRenderer := _renderer ;

    oFont := TGIS_Font.Create ;
    oFont.Assign( oRenderer.CanvasFont ) ;

    tmBreakChar := ' ' ;

    tmAscent := 0 ;

    {$IFDEF GIS_NORECORDS}
      iSize := new TPoint(0,0);
    {$ENDIF}

    if _txt = HTML_CR then begin
      sText    := '' ;
      iSize.X  := 0  ;
      {$IFDEF CLR}
        tmHeight := Convert.ToInt32( oRenderer.CanvasTextExtent( 'A' ).Y ) ;
      {$ELSE}
        tmHeight := oRenderer.CanvasTextExtent( 'A' ).Y ;
      {$ENDIF}
      iSize.Y  := tmHeight ;
      bNewLine := True ;
      exit ;
    end
    else if _txt = HTML_HR then begin
      sText     := '' ;
      iSize.X   := 0  ;
      iSize.Y   := 3  ;
      tmAscent  := 0  ;
      tmHeight  := 3  ;
      bHorzLine := True ;
      exit ;
    end
    else if _txt = HTML_NBSP then begin
      sText := ' '
    end
    else
      sText := _txt ;

    bHorzLine := False ;
    bNewLine  := False ;
    bSpace    := False ;

    iSize := oRenderer.CanvasTextExtent( sText ) ;
    {$IFDEF CLR}
      tmHeight := Convert.ToInt32( iSize.Y ) ;
    {$ELSE}
      tmHeight := iSize.Y ;
    {$ENDIF}

    if _txt = ' ' then begin  // but not HTML_NBSP
      bSpace := True ;
    end
    else
      bSpace := False ;

    iEmSpace := oRenderer.CanvasTextEm( 1 ) ;

    if length( sText ) > 0 then
      iBiDi := testBiDiChar( sText[StringFirst] ) ;
  end;

  procedure T_htmlText.doDestroy ;
  begin
    FreeObject( oFont ) ;
    inherited ;
  end;

//==============================================================================
// T_htmlLine
//==============================================================================

  constructor T_htmlLine.Create ;
  begin
    inherited Create ;
    oElements    := TGIS_ObjectList.Create( False ) ;
    iYOffset     := 0 ;
    bCanBreak    := False ;
    iLastLR      := 0 ;
  end;

  procedure T_htmlLine.doDestroy ;
  begin
    FreeObject( oElements ) ;
    inherited ;
  end;

  function T_htmlLine.getIsEmpty
    : Boolean ;
  begin
    Result := oElements.Count = 0 ;
  end;

  procedure T_htmlLine.AddText(
    const _element : T_htmlText
  ) ;
  begin
    if _element.sText <> ' ' then begin
      {$IFDEF CLR}
        iWidthNoTrail := iWidth +  Convert.ToInt32( _element.iSize.X ) ;
      {$ELSE}
        iWidthNoTrail := iWidth + _element.iSize.X ;
      {$ENDIF}
    end;

    {$IFDEF CLR}
      iWidth := iWidth + Convert.ToInt32( _element.iSize.X ) ;
    {$ELSE}
      iWidth := iWidth + _element.iSize.X ;
    {$ENDIF}

    bCanBreak    := _element.bSpace ;

    if _element.iBiDi = BIDI_LR_STRONG
    then begin
      oElements.Add( _element ) ;
      iLastLR := oElements.Count - 1 ;
    end
    else
    if  _element.iBiDi = BIDI_WEAK
    then begin
      if ( oElements.Count = 0 )
      then begin
        oElements.Add( _element ) ;
        iLastLR := oElements.Count - 1 ;
      end
      else
      if ( T_htmlText( oElements.Last ).iBiDi = BIDI_LR_STRONG )
         or
         ( T_htmlText( oElements.Last ).iBiDi = BIDI_WEAK )
      then begin
        oElements.Add( _element ) ;
        iLastLR := oElements.Count ;
      end
      else begin
        assert( iLastLR >= 0 );
        oElements.Insert( iLastLR, _element ) ;
      end
    end
    else
    if  _element.iBiDi = BIDI_RL_STRONG
    then begin
      if ( oElements.Count = 0 )
      then begin
        oElements.Add( _element ) ;
      end
      else
      if ( T_htmlText( oElements.Last ).iBiDi = BIDI_LR_STRONG )
         or
         ( T_htmlText( oElements.Last ).iBiDi = BIDI_WEAK )
      then begin
        oElements.Add( _element )
      end
      else begin
        assert( iLastLR >= 0 );
        oElements.Insert( iLastLR, _element ) ;
      end;
    end
    else begin
      assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
    end;

    iAscent := Max( iAscent, _element.tmAscent ) ;
    iHeight := Max( iHeight, _element.tmHeight ) ;

    if oElements.Count = 0 then
      iEmSpaceBeg := _element.iEmSpace ;
    iEmSpaceEnd := _element.iEmSpace ;
  end;

  procedure T_htmlLine.Draw(
    const _x     : Integer ;
    const _y     : Integer ;
    const _xoff  : Integer ;
    const _width : Integer
  ) ;
  var
    i       : Integer    ;
    elm     : T_htmlText ;
    xoff    : Integer    ;
    old_pen : TGIS_Pen   ;
  begin
    xoff := _xoff ;

    for i:=0 to oElements.Count - 1 do begin
      elm := T_htmlText( oElements[i] ) ;
      if elm.bHorzLine then begin
        old_pen := TGIS_Pen.Create ;
        old_pen.Assign( elm.oRenderer.CanvasPen ) ;
        try
          elm.oRenderer.CanvasPen.Color := elm.oFont.Color ;
          elm.oRenderer.CanvasPen.Width := Max( elm.oRenderer.PPI div 96, 1 ) ;
          elm.oRenderer.CanvasPen.Style := TGIS_PenStyle.Solid ;

          elm.oRenderer.CanvasDrawLine( _x,          _y + iYOffset + 1,
                                        _x + _width, _y + iYOffset + 1
                                      ) ;
        finally
          elm.oRenderer.CanvasPen.Assign( old_pen ) ;
          FreeObject( old_pen ) ;
        end ;
      end
      else begin
        elm.oRenderer.CanvasFont.Assign( elm.oFont );
        //RectEx
        elm.oRenderer.CanvasDrawText( Rect( _x + xoff,
                                            _y + iYOffset + iAscent - elm.tmAscent,
                                            _x + xoff + iWidth,
                                            _y + iYOffset + iHeight
                                          ),
                                      elm.sText
                                    ) ;
        {$IFDEF CLR}
          xoff := xoff +
                  Convert.ToInt32( elm.iSize.X ) ;
        {$ELSE}
          xoff := xoff + elm.iSize.X ;
        {$ENDIF}
      end ;
    end ;
  end;

  procedure T_htmlLine.Draw(
    const _x            : Integer ;
    const _y            : Integer ;
    const _xoff         : Integer ;
    const _width        : Integer ;
    const _forced_color : TGIS_Color
  ) ;
  var
    i       : Integer    ;
    elm     : T_htmlText ;
    xoff    : Integer    ;
    old_pen : TGIS_Pen   ;
  begin
    xoff := _xoff ;

    for i:=0 to oElements.Count - 1 do begin
      elm := T_htmlText( oElements[i] ) ;
      if elm.bHorzLine then begin
        old_pen := TGIS_Pen.Create ;
        try
          elm.oRenderer.CanvasPen.Color := _forced_color ;
          elm.oRenderer.CanvasPen.Width := Max( elm.oRenderer.PPI div 96, 1 ) ;
          elm.oRenderer.CanvasPen.Style := TGIS_PenStyle.Solid ;

          elm.oRenderer.CanvasDrawLine( _x,          _y + iYOffset + 1,
                                        _x + _width, _y + iYOffset + 1
                                      ) ;
        finally
          elm.oRenderer.CanvasPen.Assign( old_pen );
          FreeObject( old_pen ) ;
        end;
      end
      else begin
        elm.oRenderer.CanvasFont.Assign( elm.oFont );
        elm.oRenderer.CanvasFont.Color := _forced_color ;
        //RectEx
        elm.oRenderer.CanvasDrawText( Rect( _x + xoff,
                                            _y + iYOffset  + iAscent - elm.tmAscent,
                                            _x + xoff + iWidth,
                                            _y + iYOffset + iHeight
                                          ),
                                       elm.sText
                                    ) ;
      end ;
      {$IFDEF CLR}
        xoff := xoff +
                Convert.ToInt32( elm.iSize.X ) ;
      {$ELSE}
        xoff := xoff + elm.iSize.X ;
      {$ENDIF}
    end ;
  end;

//==============================================================================
// TGIS_HtmlLabel
//==============================================================================

  constructor TGIS_HtmlLabel.Create(
    const _renderer : TGIS_RendererAbstract ;
    const _text     : String   ;
    const _align    : TGIS_LabelAlignment ;
    const _width    : Integer  ;
    const _height   : Integer
  ) ;
  begin
    inherited Create ;

    oRenderer := _renderer ;
    sLabel    := _text     ;
    eAlign    := _align    ;
    iWidth    := _width    ;
    iHeight   := _height   ;

    bUpperCase := False ;
    bLowerCase := False ;
    bNbsp      := False ;

    oLines      := TGIS_ObjectList.Create( True ) ;
    oAttributes := TStringList.Create ;
    oPrevFont   := TGIS_Font.Create   ;
    oElements   := TGIS_ObjectList.Create( True ) ;

    doParse   ;
    doFormat  ;
    doMeasure ;
  end;

  procedure TGIS_HtmlLabel.doDestroy ;
  begin
    FreeObject( oLines ) ;
    FreeObject( oAttributes ) ;
    FreeObject( oPrevFont ) ;
    FreeObject( oElements ) ;
    inherited ;
  end;

  function TGIS_HtmlLabel.doTokenHtml
    : Boolean ;
  var
    c          : Char    ;
    l          : Integer ;
    state      : Integer ;
    attr_name  : TStringBuilder ;
    attr_value : TStringBuilder ;
    text       : TStringBuilder ;
    stmp       : String  ;
    last_block : Integer ;
    curr_block : Integer ;

    procedure clear_text( var _o : TStringBuilder ) ;
    begin
      FreeObject( _o ) ;
      _o := TStringBuilder.Create ;
    end;

    procedure prepare_text( var _o : TStringBuilder ) ;
    begin
      _o := TStringBuilder.Create ;
    end;

  const
    ST_START                =  1 ;
    ST_SPACES               =  2 ;
    ST_SPACES_END           =  3 ;
    ST_TEXT                 =  4 ;
    ST_TEXT_LEADING_SPACES  =  5 ;
    ST_TEXT_END             =  6 ;
    ST_SPECCHAR             =  7 ;
    ST_SPECCHAR_END         =  8 ;
    ST_MARKER               =  9 ;
    ST_MARKER_END           = 10 ;
    ST_ATTR_NAME            = 11 ;
    ST_ATTR_VALUE           = 12 ;
    ST_ATTR_VALUE_QUOTED    = 13 ;
    ST_ATTR_VALUE_NOTQUOTED = 14 ;
    ST_HTML_ERROR           = 15 ;

    // set attribute value
    procedure set_attribute ;
    begin
      oAttributes.Values[ UpperCase( attr_name.ToString ) ] := attr_value.ToString ;
      clear_text( attr_name  ) ;
      clear_text( attr_value ) ;
    end;

    // fetch next HTLp character
    procedure next_char ;
    begin
      inc( iPos ) ;
      if iPos < StringFirst + l then
        c := sLabel[ iPos ]
      else
        c := #0 ;
    end;
  begin
    bError := False ;
    Result := False ;

    state := ST_START ;
    l := length( sLabel ) ;

    oAttributes.Clear ;

    if iPos >= StringFirst + l then
      exit ;

    prepare_text( text       ) ;
    prepare_text( attr_value ) ;
    prepare_text( attr_name  ) ;

    c := sLabel[ iPos ] ;

    while iPos < StringFirst + l do begin
      case state of
        ST_START :
          begin // recognize first char
            last_block := 0 ;
            if c = '<' then begin
              state := ST_MARKER ;
              next_char ;
            end
            else if c = '&' then begin
              state := ST_SPECCHAR ;
              next_char ;
            end
            else if c = HTML_CR then begin
              addText( HTML_CR ) ;
              next_char ;
            end
            else if c = #32 then begin
              if bNbsp then
                addText( HTML_NBSP )
              else
                addText( #32 ) ;
              next_char ;
            end
            else if c < #32 then begin
              // treat all other special chars as breaking spaces
              if bNbsp then
                addText( HTML_NBSP )
              else
                addText( #32 ) ;
              next_char ;
            end
            else begin
              state := ST_TEXT ;
            end ;
          end ;
        ST_SPACES :
          begin
            if c = HTML_CR then begin
              addText( HTML_CR ) ;
              next_char ;
            end
            else if c > #32 then
              state := ST_SPACES_END
            else begin
              addText( #32 ) ;
              next_char ;
            end ;
          end;
        ST_SPACES_END :
          begin
            if bNbsp then
              addText( HTML_NBSP )
            else
              addText( #32 ) ;
            clear_text( text ) ;
            break ;
          end;
        ST_TEXT :
          begin // collect until < found
            if c = '<' then begin
              state := ST_TEXT_END ;
            end
            else if c = '>' then begin
              state := ST_HTML_ERROR ;
            end
            else if c = '&' then begin
              state := ST_TEXT_END ;
            end
            else if c = HTML_CR then begin
              state := ST_TEXT_END ;
            end
            else if c = #32 then
              state := ST_TEXT_END
            else if c < #32 then begin
              // all other special chars as breaking spaces
              state := ST_TEXT_END ;
            end
            else begin
              {$IFDEF CLR}
                if c <> ' ' then begin
                  curr_block := getUnicodeBlock(c);
                  if last_block = 0 then
                    last_block := curr_block
                  else
                    if curr_block <> last_block then begin
                      state := ST_TEXT_END ;
                      continue ;
                    end ;
                end ;
              {$ENDIF}


              if bUpperCase then
                text.Append( UpCase( c ) )
              else if bLowerCase then
                {$IFDEF CLR}
                  text.Append( LowCase( c ) )
                {$ELSE}
                  text.Append( LowerCase( c ) )
                {$ENDIF}
              else
                text.Append( c  ) ;

              next_char ;
            end;
          end ;
        ST_TEXT_END :
          begin // collected whole text
            addText( text.ToString ) ;
            clear_text( text ) ;
            break ;
          end ;
        ST_SPECCHAR :
          begin // waiting for ';'
            if c = ';' then begin
              state := ST_SPECCHAR_END ;
            end
            else if c = '<' then begin
              stmp := text.ToString ;
              clear_text( text ) ;
              text.Append( '&' ) ;
              text.Append( stmp ) ;
              state := ST_TEXT_END ;
            end
            else begin
              text.Append( c ) ;
              next_char ;
            end ;
          end ;
        ST_SPECCHAR_END :
          begin // collected whole special character '&something;'
            next_char ;
            stmp := doSpecChar( text.ToString ) ;
            if not IsStringEmpty( stmp ) then
              addText( stmp ) ;
            clear_text( text ) ;
            break ;
          end ;
        ST_MARKER :
          begin // waiting for '>'
            if c = '>' then begin
              state := ST_MARKER_END ;
            end
            else if c = '<' then begin
              state := ST_HTML_ERROR ;
            end
            else if c <= ' ' then begin
              state := ST_ATTR_NAME ;
              next_char ;
            end
            else begin
              text.Append( c ) ;
              next_char ;
            end ;
          end ;
        ST_MARKER_END :
          begin // collected whole control '<something>'
            next_char ;
            stmp := doMarker( text.ToString ) ;
            if not IsStringEmpty( stmp ) then
              addText( stmp ) ;
            clear_text( text ) ;
            break ;
          end ;
        ST_ATTR_NAME :
          begin // parse attributes
            if c <= ' ' then begin // ignore all spaces etc
              next_char ;
            end
            else if c = '>' then
              state := ST_MARKER_END
            else
              if c = '=' then begin
                state := ST_ATTR_VALUE ;
                next_char ;
              end
              else begin
                attr_name.Append( c ) ;
                next_char ;
              end;
          end ;
        ST_ATTR_VALUE :
          begin // parse attribute value
            if c = '"' then begin
              state := ST_ATTR_VALUE_QUOTED ;
              next_char ;
            end
            else
              state := ST_ATTR_VALUE_NOTQUOTED ;
          end ;
        ST_ATTR_VALUE_QUOTED :
          begin // parse attribute value
            if c = '"' then begin
              set_attribute ;
              next_char ;
              state := ST_ATTR_NAME ;
            end
            else if c = '>' then begin
              set_attribute ;
              state := ST_MARKER_END ;
            end
            else begin
              attr_value.Append( c ) ;
              next_char ;
            end;
          end ;
        ST_ATTR_VALUE_NOTQUOTED :
          begin // parse attribute value
            if c <= ' ' then begin
              set_attribute ;
              state := ST_ATTR_NAME ;
            end
            else if c = '>' then begin
              set_attribute ;
              state := ST_MARKER_END ;
            end
            else begin
              attr_value.Append( c ) ;
              next_char ;
            end;
          end ;
        ST_HTML_ERROR :
          begin // parse attribute value
            bError := True  ;
            Result := False ;
            break ;
          end ;
        else
          begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
            bError := True  ;
            Result := False ;
            break ;
          end ;
      end ;

      Result := True ;
    end ;

    if text.Length > 0 then begin
      if state = ST_SPECCHAR then
        addText( '&' ) ;

      addText( text.ToString ) ;
    end ;

    FreeObject( attr_name  ) ;
    FreeObject( attr_value ) ;
    FreeObject( text       ) ;
  end ;

  function TGIS_HtmlLabel.doTokenPlain
    : Boolean ;
  var
    c          : Char    ;
    l          : Integer ;
    state      : Integer ;
    text       : TStringBuilder ;

    procedure clear_text( var _o : TStringBuilder ) ;
    begin
      FreeObject( _o ) ;
      _o := TStringBuilder.Create ;
    end;

    procedure prepare_text( var _o : TStringBuilder ) ;
    begin
      _o := TStringBuilder.Create ;
    end;

  const
    ST_START                =  1 ;
    ST_SPACES               =  2 ;
    ST_SPACES_END           =  3 ;
    ST_TEXT                 =  4 ;
    ST_TEXT_TRAILING_SPACES =  5 ;
    ST_TEXT_END             =  6 ;

    // fetch next HTLp character
    procedure next_char ;
    begin
      inc( iPos ) ;
      if iPos < StringFirst + l then
        c := sLabel[ iPos ]
      else
        c := #0 ;
    end;
  begin
    bError := False ;
    Result := False ;

    state := ST_START ;
    l := length( sLabel ) ;

    oAttributes.Clear ;

    if iPos >= StringFirst + l then
      exit ;

    prepare_text( text       ) ;

    c := sLabel[ iPos ] ;
    while iPos < StringFirst + l do begin
      case state of
        ST_START :
          begin // recognize first char
            if c = HTML_CR then begin
              addText( HTML_CR ) ;
              next_char ;
            end
            else if c = #32 then
              state := ST_SPACES
            else if c < #32 then begin
              // treat all other special chars as breaking spaces
              text.Append( #32 ) ;
              next_char ;
              state := ST_TEXT_END ;
            end
            else begin
              state := ST_TEXT ;
            end ;
          end ;
        ST_SPACES :
          begin
            if c = HTML_CR then begin
              addText( HTML_CR ) ;
              next_char ;
            end
            else if c > #32 then
              state := ST_SPACES_END
            else begin
              addText( #32 ) ;
              next_char ;
            end ;
          end;
        ST_SPACES_END :
          begin
            addText( ' ' ) ;
            clear_text( text ) ;
            break ;
          end;
        ST_TEXT :
          begin // collect until < found
            if c = HTML_CR then begin
              state := ST_TEXT_END ;
            end
            else if c = #32 then
              state := ST_TEXT_END
            else if c < #32 then begin
              // treat all other special chars as breaking spaces
              text.Append( #32 ) ;
              next_char ;
              state := ST_TEXT_END ;
            end
            else begin
              text.Append( c  ) ;
              next_char ;
            end;
          end ;
        ST_TEXT_TRAILING_SPACES :
          begin // collect until not space begin
            if c > #32 then
              state := ST_TEXT_END
            else begin
              text.Append( ' ' ) ;
              next_char ;
            end ;
          end ;
        ST_TEXT_END :
          begin // collected whole text
            addText( text.ToString ) ;
            clear_text( text ) ;
            break ;
          end ;
        else
          begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
            bError := True  ;
            Result := False ;
            break ;
          end ;
      end ;

      Result := True ;
    end ;

    if text.Length > 0 then begin
      addText( text.ToString ) ;
    end ;

    FreeObject( text       ) ;
  end;

  function TGIS_HtmlLabel.doSpecChar(
    const _value : String
  ) : String ;
  var
    tk : String ;
  begin
    Result := '' ;

    tk := UpperCase( _value ) ;

    if      tk = 'AMP'     then Result := '&'
    else if tk = 'NBSP'    then Result := HTML_NBSP
    else if tk = 'GT'      then Result := '>'
    else if tk = 'LT'      then Result := '<' ;
  end ;

  function TGIS_HtmlLabel.doMarker(
    const _value : String
  ) : String ;
  var
    tk : String ;

    procedure set_font ;
    var
      s : String ;

      function parse_color( const _color : String ) : TGIS_Color ;
      var
        r, g, b : Byte ;
      begin
        try
          r := StrToInt( '$' + Copy( _color, StringFirst + 1, 2 ) ) ;
        except
          r := 0 ;
        end;

        try
          g := StrToInt( '$' + Copy( _color, StringFirst + 3, 2 ) ) ;
        except
          g := 0 ;
        end;

        try
          b := StrToInt( '$' + Copy( _color, StringFirst + 5, 2 ) ) ;
        except
          b := 0 ;
        end;

        Result := TGIS_Color.FromRGB( r, g, b ) ;

      end;
    begin
      oPrevFont.Assign( oRenderer.CanvasFont ) ;
      s := oAttributes.Values[ 'NAME' ] ;
      if not IsStringEmpty( s ) then begin
        try
          oRenderer.CanvasFont.Name := s ;
        except
          //
        end;
      end ;

      s := oAttributes.Values[ 'SIZE' ] ;
      if not IsStringEmpty( s ) then begin
        try
          oRenderer.CanvasFont.Size := StrToInt( s )  ;
        except
          //
        end;
      end ;

      s := oAttributes.Values[ 'SCALE' ] ;
      if not IsStringEmpty( s ) then begin
        try
          oRenderer.CanvasFont.Size := oRenderer.CanvasFont.Size * StrToInt( s ) div 100 ;
        except
          //
        end;
      end;

      s := UpperCase( oAttributes.Values[ 'COLOR' ] ) ;

      if not IsStringEmpty( s ) then begin
        if      s = 'BLACK'   then oRenderer.CanvasFont.Color := TGIS_Color.Black
        else if s = 'BLUE'    then oRenderer.CanvasFont.Color := TGIS_Color.Blue
        else if s = 'FUCHSIA' then oRenderer.CanvasFont.Color := TGIS_Color.Fuchsia
        else if s = 'GRAY'    then oRenderer.CanvasFont.Color := TGIS_Color.Gray
        else if s = 'GREEN'   then oRenderer.CanvasFont.Color := TGIS_Color.Green
        else if s = 'LIME'    then oRenderer.CanvasFont.Color := TGIS_Color.Lime
        else if s = 'MAROON'  then oRenderer.CanvasFont.Color := TGIS_Color.Maroon
        else if s = 'NAVY'    then oRenderer.CanvasFont.Color := TGIS_Color.Navy
        else if s = 'OLIVE'   then oRenderer.CanvasFont.Color := TGIS_Color.Olive
        else if s = 'PURPLE'  then oRenderer.CanvasFont.Color := TGIS_Color.Purple
        else if s = 'RED'     then oRenderer.CanvasFont.Color := TGIS_Color.Red
        else if s = 'SILVER'  then oRenderer.CanvasFont.Color := TGIS_Color.Silver
        else if s = 'TEAL'    then oRenderer.CanvasFont.Color := TGIS_Color.Teal
        else if s = 'WHITE'   then oRenderer.CanvasFont.Color := TGIS_Color.White
        else if s = 'YELLOW'  then oRenderer.CanvasFont.Color := TGIS_Color.Yellow
        else                       oRenderer.CanvasFont.Color := parse_color( s ) ;
      end;
   end;

    procedure set_clr ;
    var
      s       : String ;
      r, g, b : Byte   ;

    begin
      oPrevColor := oRenderer.CanvasFont.Color ;

      r := 0 ;
      g := 0 ;
      b := 0 ;

      s := oAttributes.Values[ 'RED' ] ;
      if not IsStringEmpty( s ) then begin
        try
          r := StrToInt( s ) ;
        except
          //
        end;
      end ;

      s := oAttributes.Values[ 'GREEN' ] ;
      if not IsStringEmpty( s ) then begin
        try
          g := StrToInt( s ) ;
        except
          //
        end;
      end ;

      s := oAttributes.Values[ 'BLUE' ] ;
      if not IsStringEmpty( s ) then begin
        try
          b := StrToInt( s ) ;
        except
          //
        end;
      end ;

      oRenderer.CanvasFont.Color := TGIS_Color.FromRGB( r, g, b ) ;
   end;

  begin
    Result := '' ;

    tk := UpperCase( _value ) ;

    if      tk = 'B'       then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )
    else if tk = '/B'      then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )
    else if tk = 'I'       then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Italic )
    else if tk = '/I'      then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Italic )
    else if tk = 'U'       then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Underline )
    else if tk = '/U'      then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Underline )
    else if tk = 'HR'      then Result := HTML_HR
    else if tk = 'BR'      then Result := HTML_CR
    else if tk = '/BR'     then Result := HTML_CR
    else if tk = 'P'       then Result := HTML_CR
    else if tk = '/P'      then Result := HTML_CR

    // less common phrases
    else if tk = 'FONT'    then set_font
    else if tk = '/FONT'   then oRenderer.CanvasFont.Assign( oPrevFont )
    else if tk = 'STRONG'  then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )
    else if tk = '/STRONG' then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )

    // ESRI markers
    else if tk = 'FNT'     then set_font
    else if tk = '/FNT'    then oRenderer.CanvasFont.Assign( oPrevFont )
    else if tk = 'CLR'     then set_clr
    else if tk = '/CLR'    then oRenderer.CanvasFont.Color := oPrevColor
    else if tk = 'UCP'     then bUpperCase := True
    else if tk = '/UCP'    then bUpperCase := False
    else if tk = 'SCP'     then bLowerCase := True
    else if tk = '/SCP'    then bLowerCase := False
    else if tk = 'BOL'     then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )
    else if tk = '/BOL'    then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Bold )
    else if tk = 'ITA'     then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Italic )
    else if tk = '/ITA'    then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Italic )
    else if tk = 'UND'     then
      oRenderer.CanvasFont.Style := GisAddFontStyle   ( oRenderer.CanvasFont.Style, TGIS_FontStyle.Underline )
    else if tk = '/UND'    then
      oRenderer.CanvasFont.Style := GisRemoveFontStyle( oRenderer.CanvasFont.Style, TGIS_FontStyle.Underline )

    // forced non-breaking-space mode
    else if tk = 'NBSP'    then bNbsp := True
    else if tk = '/NBSP'   then bNbsp := False ;
  end ;

  procedure TGIS_HtmlLabel.addText(
    const _txt : String
  ) ;
  var
    i : Integer ;
    elm : T_htmlText ;
  begin

    for i := 0 to oElements.Count -1 do
      elm := T_htmlText( oElements[i] ) ;

    if _txt = #32 then begin
      // eliminate all spaces before
      for i := oElements.Count - 1 downto 0 do begin
        if T_htmlText( oElements[i] ).bSpace then
          oElements.Delete( i )
        else
          break ;
      end;
      // do not add after new line
      i := oElements.Count - 1;
      if i >= 0 then begin
        if not T_htmlText( oElements[i] ).bNewLine then
         oElements.Add( T_htmlText.Create( oRenderer, ' ' ) ) ;
      end ;
    end
    else if _txt = HTML_CR then begin
      // eliminate all spaces after
      for i := oElements.Count - 1 downto 0 do begin
        if T_htmlText( oElements[i] ).bSpace then
          oElements.Delete( i )
        else
          break ;
      end;
      oElements.Add( T_htmlText.Create( oRenderer, HTML_CR ) ) ;
    end
    else
      oElements.Add( T_htmlText.Create( oRenderer, _txt    ) ) ;
  end;

  procedure TGIS_HtmlLabel.doParse ;
  var
    old_font : TGIS_Font ;
  begin
    if not assigned( oRenderer ) then exit ;
    old_font := TGIS_Font.Create ;
    old_font.Assign( oRenderer.CanvasFont ) ;
    try
      iPos := StringFirst ;
      while doTokenHtml do ;

      if bError then begin
        oAttributes.Clear ;
        oElements.Clear ;
        iPos := StringFirst ;
        while doTokenPlain do ;
      end;
    finally
      oRenderer.CanvasFont.Assign( old_font ) ;
      FreeObject( old_font ) ;
    end;
  end;

  procedure TGIS_HtmlLabel.doFormat ;
  var
    i     : Integer    ;
    elm   : T_htmlText ;
    state : Integer    ;
    lin   : T_htmlLine ;
  const
    ST_START      = 0 ;
    ST_GETELEMENT = 1 ;
    ST_CANFIT     = 2 ;
    ST_NOFIT      = 3 ;
    ST_NEWLINE    = 4 ;
    ST_HORZLINE   = 5 ;

    function nextElement() : T_htmlText ;
    begin
      if i < oElements.Count then begin
        Result := T_htmlText( oElements[i] ) ;
        inc( i ) ;
      end
      else
        Result := nil ;
    end;

    procedure init_line ;
    var
      h : Integer ;
    begin
      if assigned( lin ) then begin
        h := lin.iYOffset + lin.iHeight ;
      end
      else
        h := 0 ;
      lin := T_htmlLine.Create ;
      lin.iYOffset := h ;
      oLines.Add( lin ) ;
    end;
  begin
    lin := nil ;

    init_line ;

    state := ST_START ;
    elm := nil ;

    i := 0 ;
    while True do begin
      case state of
        ST_START :
          begin
            state := ST_GETELEMENT ;
          end;
        ST_GETELEMENT :
          begin
            elm := nextElement() ;
            if elm = nil  then  begin
              break ;
            end
            else if elm.bNewLine then begin
              state := ST_NEWLINE
            end
            else if elm.bHorzLine then begin
              state := ST_HORZLINE
            end
              else if ( lin.Width + elm.iSize.X <= iWidth ) or
                      ( not lin.CanBreak )
              then
                state := ST_CANFIT
            else
              state := ST_NOFIT
          end;
        ST_CANFIT :
          begin
            // truncate text if wider then line
            if lin.iWidth + elm.iSize.X  > iWidth then
              oRenderer.CanvasFont.Assign( elm.oFont ) ;
            while lin.Width + elm.iSize.X  > iWidth do begin
              SetLengthStr( elm.sText, length( elm.sText ) - 1 ) ;
              if IsStringEmpty( elm.sText ) then
                elm.iSize.X := 0
              else
                elm.iSize := oRenderer.CanvasTextExtent( elm.sText ) ;
            end;

            lin.AddText( elm ) ;
            state := ST_GETELEMENT ;
          end;
        ST_NOFIT  :
          begin
            if not lin.IsEmpty then
              init_line ;

            // truncate elm if wider then line
            if lin.iWidth + elm.iSize.X  > iWidth then
              oRenderer.CanvasFont.Assign( elm.oFont ) ;
            while lin.iWidth + elm.iSize.X  > iWidth do begin
              SetLengthStr( elm.sText, length( elm.sText ) - 1 ) ;
              elm.iSize := oRenderer.CanvasTextExtent( elm.sText ) ;
            end;

            lin.AddText( elm ) ;
            state := ST_GETELEMENT ;
          end;
        ST_NEWLINE  :
          begin
            init_line ;
            state := ST_GETELEMENT ;
          end;
        ST_HORZLINE :
          begin
            init_line ;
            lin.AddText( elm ) ;
            init_line ;
            state := ST_GETELEMENT ;
          end;
        else
          begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end ;
      end;
    end;

    // eliminate lines outside the label size
    for i := oLines.Count -1 downto 0 do begin
      lin := T_htmlLine( oLines[i] ) ;
      if lin.iHeight + lin.iYOffset > iHeight then
        oLines.Delete( i )
      else
        break ;
    end ;
  end ;

  procedure TGIS_HtmlLabel.doMeasure ;
  var
    i        : Integer ;
    cnt      : Integer ;
    r_left   : Integer ;
    r_top    : Integer ;
    r_right  : Integer ;
    r_bottom : Integer ;
    lin      : T_htmlLine ;
  begin
    cWidth  := 0 ;
    cHeight := 0 ;

    case eAlign of
      TGIS_LabelAlignment.LeftJustify :
             cnt := oLines.Count ;
      TGIS_LabelAlignment.Center :
             cnt := oLines.Count ;
      TGIS_LabelAlignment.RightJustify :
             cnt := oLines.Count ;
      else   // gisLabelAlignmentSingle
             cnt := Min( 1, oLines.Count ) ;
    end ;

    for i:=0 to cnt - 1 do begin
      lin := T_htmlLine( oLines[i] ) ;
      cWidth := Max( cWidth,
                     lin.iWidthNoTrail + lin.iEmSpaceBeg
                   ) ;
      cHeight := cHeight + lin.iHeight ;
    end;

    r_left := 0 ;
    r_top  := 0 ;
    if cWidth > 0 then
      r_right := cWidth - 1
    else
      r_right := 0 ;
    if cHeight > 0 then
      r_bottom := cHeight -1
    else
      r_bottom := 0 ;

    //RectEx
    oBoundingBox := Rect( r_left, r_top, r_right, r_bottom ) ;
  end;

  procedure TGIS_HtmlLabel.doDraw(
    const _rect         : TRect   ;
    const _force_color  : Boolean ;
    const _forced_color : TGIS_Color
  ) ;
  var
    i        : Integer ;
    w        : Integer ;
    x, y     : Integer ;
    xoff     : Integer ;
    old_font : TGIS_Font ;

    procedure draw_line ;
    begin
      if _force_color then
        T_htmlLine( oLines[i] ).Draw( x , y, xoff, w, _forced_color )
      else
        T_htmlLine( oLines[i] ).Draw( x , y, xoff, w );
    end;
  begin
     w := _rect.Right - _rect.Left + 1 ;

    {$IFDEF CLR}
      x := Convert.ToInt32( _rect.Left ) ;
      y := Convert.ToInt32( _rect.Top  ) ;
    {$ELSE}
      x := _rect.Left ;
      y := _rect.Top  ;
    {$ENDIF}

    old_font := TGIS_Font.Create ;
    old_font.Assign( oRenderer.CanvasFont ) ;
    try
      for i:=0 to oLines.Count - 1 do begin
        case eAlign of
          TGIS_LabelAlignment.LeftJustify :
               begin
                 xoff := 0 ;
                 draw_line ;
               end;
          TGIS_LabelAlignment.Center :
               begin
                 xoff := RoundS( ( w - T_htmlLine( oLines[i] ).iWidthNoTrail ) / 2 );
                 draw_line ;
               end;
          TGIS_LabelAlignment.RightJustify :
               begin
                 xoff := w - T_htmlLine( oLines[i] ).iWidthNoTrail ;
                 draw_line ;
               end;
          else   // gisLabelAlignmentSingle
               begin
                 xoff := 0 ;
                 draw_line ;
                 break ;
               end;
        end;
      end;
    finally
      oRenderer.CanvasFont.Assign( old_font ) ;
      FreeObject( old_font ) ;
    end ;
  end;

  procedure TGIS_HtmlLabel.Draw(
    const _rect         : TRect   ;
    const _angle        : Double  ;
    const _origin       : TPoint  ;
    const _shadow_width : Integer ;
    const _shadow_color : TGIS_Color
  ) ;
  var
    rct     : TRect   ;
    i, j    : Integer ;
    rct_tmp : TRect   ;
  begin
    try
      rct := _rect ;
      if _angle <> 0 then
        {$IFDEF CLR}
          oRenderer.CanvasSetTransformation(
            _angle,
            Convert.ToInt32( _origin.X ),
            Convert.ToInt32( _origin.Y )
          ) ;
        {$ELSE}
          oRenderer.CanvasSetTransformation(
            _angle,
            _origin.X,
            _origin.Y
          ) ;
        {$ENDIF}
      try
        if _shadow_width > 0 then begin
          for i := -_shadow_width to _shadow_width do begin
            for j := -_shadow_width to _shadow_width do begin
              rct_tmp := Rect( rct.Left  + i, rct.Top    + j,
                               rct.Right + i, rct.Bottom + j ) ;
              doDraw( rct_tmp, True, _shadow_color );
            end ;
          end ;
        end ;
        doDraw( rct, False, TGIS_Color.Black ) ;
      finally
        if _angle <> 0 then
          oRenderer.CanvasClearTransformation ;
      end;
    finally
    end ;
  end;

//==================================== END =====================================
end.


