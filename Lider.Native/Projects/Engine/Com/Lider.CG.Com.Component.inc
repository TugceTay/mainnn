(* ilker ekleme baþlama *)
{$IFDEF DEBUG}
  {$DEFINE DEVELOPER_MODE}  // Release Modu için . ile pasif yap. Developor modu için açýk olsun.
{$ENDIF}

{.$DEFINE LISANSDATE_MODE}  // Normal Modu için . ile pasif yap.

{$WARNINGS OFF}
{$WARN SYMBOL_PLATFORM OFF}

(* ilker ekleme bitiþ *)

{$DEFINE USE_INTRAWEB}

{.$DEFINE USE_GRAPHICEX} { enable if you want to use GraphicEx support }

{$DEFINE GIS_CONTROLS}          { Please dont' move ! - this switch prevents compiling only for Lider GIS product }
{$DEFINE CAD_CONTROLS}          { Please dont' move ! - this switch prevents compiling only for Lider CAD product }

{ language used - only in some forms is used this. See also Lider.CG.Consts.Pas }
{$DEFINE LANG_TRK}     // { Türkçe }
{.$DEFINE LANG_ENG}     // { enable for English }

{$DEFINE JPEG_SUPPORT}    { enable if you want to use .JPG files}
{$DEFINE GIFGraphic}
