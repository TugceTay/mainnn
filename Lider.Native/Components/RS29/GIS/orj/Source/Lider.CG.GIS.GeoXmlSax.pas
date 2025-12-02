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
  Simple API for XML (SAX) implementation. Mimics MSXML to some extent.
}

{$IFDEF DCC}
  unit GisXmlSax ;
  {$HPPEMIT '#pragma link "GisXmlSax"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.RTL.XML ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS.RTL.XML ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    System,
    System.IO,
    System.Collections.Generic,
    System.Text,
    TatukGIS.RTL,
    TatukGIS.NDK ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Generics.Collections,

    GisRtl,
    GisStreams ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    java.io,
    java.text,
    tatukgis.rtl,
    tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS ;
{$ENDIF}

type

  {$IFDEF DCC}
    {#GENDOC:HIDE}
    TXMLOutputStream  = TStream ;
    {#GENDOC:HIDE}
    TXMLStreamWriter  = TStreamWriter ;
    {#GENDOC:HIDE}
    TXMLStringBuilder = TStringBuilder ;
  {$ENDIF}
  {$IFDEF CLR}
    {#GENDOC:HIDE}
    TXMLOutputStream  = public Stream ;
    {#GENDOC:HIDE}
    TXMLStreamWriter  = public StreamWriter ;
    {#GENDOC:HIDE}
    TXMLStringBuilder = public StringBuilder ;
  {$ENDIF}
  {$IFDEF JAVA}
    {#GENDOC:HIDE}
    TXMLOutputStream  = public OutputStream ;
    {#GENDOC:HIDE}
    TXMLStreamWriter  = public OutputStreamWriter ;
    {#GENDOC:HIDE}
    TXMLStringBuilder = public StringBuilder ;
  {$ENDIF}
  {$IFDEF ISLAND}
    {#GENDOC:HIDE}
    TXMLOutputStream  = public TStream ;
    {#GENDOC:HIDE}
    TXMLStreamWriter  = public StreamWriter ;
    {#GENDOC:HIDE}
    TXMLStringBuilder = public StringBuilder ;
  {$ENDIF}

  /// <summary>
  ///   Abstract class mimicking the ISAXAttributes interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXAttributes_ = {$IFDEF OXYGENE} public abstract {$ENDIF}
                    class
    public
      {#GENDOC:HIDE}
      function  GetIndexFromName  ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : Integer ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetIndexFromQName ( const _qname  : String
                                  ) : Integer ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetLocalName      ( const _index  : Integer
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetQName          ( const _index  : Integer
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetType           ( const _index  : Integer
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetTypeFromName   ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetTypeFromQName  ( const _qname  : String
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetURI            ( const _index  : Integer
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetValue          ( const _index  : Integer
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetValueFromName  ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : String ; virtual; abstract;
      {#GENDOC:HIDE}
      function  GetValueFromQName ( const _qname  : String
                                  ) : String ; virtual; abstract;
  end ;

  /// <summary>
  ///   Abstract class mimicking the ISAXAttributes interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXAttributes = {$IFDEF OXYGENE} public abstract {$ENDIF}
                   class ( ISAXAttributes_ )
    public
      {#GENDOC:HIDE}
      procedure GetLength         (   out _len    : Integer
                                  ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure GetName           ( const _index  : Integer ;
                                      out _uri    : String  ;
                                      out _luri   : Integer ;
                                      out _lname  : String  ;
                                      out _llname : Integer ;
                                      out _qname  : String  ;
                                      out _lqname : Integer
                                  ) ; virtual; abstract;
  end ;

  /// <summary>
  ///   Abstract class mimicking the IVBSAXAttributes interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  IVBSAXAttributes = {$IFDEF OXYGENE} public abstract {$ENDIF}
                     class ( ISAXAttributes )
    protected
      function  fget_Length : Integer ; virtual; abstract;
    public
      {#GENDOC:HIDE}
      property  Length : Integer
                         read  fget_Length ;
  end ;

  /// <summary>
  ///   Abstract class mimicking the IMXAttributes interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  IMXAttributes = {$IFDEF OXYGENE} public abstract {$ENDIF}
                  class ( IVBSAXAttributes )
    public
      {#GENDOC:HIDE}
      procedure AddAttribute ( const _uri    : String ;
                               const _lname  : String ;
                               const _qname  : String ;
                               const _type   : String ;
                               const _value  : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure Clear        ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetAttribute ( const _index  : Integer ;
                               const _uri    : String ;
                               const _lname  : String ;
                               const _qname  : String ;
                               const _type   : String ;
                               const _value  : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure RemoveAttribute
                             ( const _index  : Integer
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetLocalName ( const _index  : Integer ;
                               const _lname  : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetQName     ( const _index  : Integer ;
                               const _qname  : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetType      ( const _index  : Integer ;
                               const _type   : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetURI       ( const _index  : Integer ;
                               const _uri    : String
                             ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SetValue     ( const _index  : Integer ;
                               const _value  : String
                             ) ; virtual; abstract;
  end ;

  {#GENDOC:HIDE}
  SAXAttributes = {$IFDEF OXYGENE} public {$ENDIF} IMXAttributes ;

  /// <summary>
  ///   Abstract class mimicking the ISAXLocator interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXLocator_ = {$IFDEF OXYGENE} public abstract {$ENDIF}
                 class
    protected
      function  fget_ColumnNumber : Integer ; virtual; abstract;
      function  fget_LineNumber   : Integer ; virtual; abstract;
      function  fget_PublicId     : String  ; virtual; abstract;
      function  fget_SystemId     : String  ; virtual; abstract;
    public
      {#GENDOC:HIDE}
      procedure GetColumnNumber   (   out _val : Integer
                                  ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure GetLineNumber     (   out _val : Integer
                                  ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure GetPublicId       (   out _val : String
                                  ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure GetSystemId       (   out _val : String
                                  ) ; virtual; abstract;
    public
      {#GENDOC:HIDE}
      property  ColumnNumber : Integer
                               read  fget_ColumnNumber ;
      {#GENDOC:HIDE}
      property  LineNumber   : Integer
                               read  fget_LineNumber ;
      {#GENDOC:HIDE}
      property  PublicId     : String
                               read  fget_PublicId ;
      {#GENDOC:HIDE}
      property  SystemId     : String
                               read  fget_SystemId ;
  end ;

  {#GENDOC:HIDE}
  ISAXLocator = {$IFDEF OXYGENE} public {$ENDIF} ISAXLocator_ ;

  {#GENDOC:HIDE}
  IVBSAXLocator = {$IFDEF OXYGENE} public {$ENDIF} ISAXLocator_ ;

  /// <summary>
  ///   Abstract class mimicking the ISAXContentHandler interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXContentHandler_ = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class
    public
      {#GENDOC:HIDE}
      procedure StartDocument  ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure EndDocument    ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure StartElement   ( const _uri     : String ;
                                 const _lname   : String ;
                                 const _qname   : String ;
                                 const _attribs : IVBSAXAttributes
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure EndElement     ( const _uri     : String ;
                                 const _lname   : String ;
                                 const _qname   : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure Characters     ( const _chars   : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure IgnorableWhitespace
                               ( const _chars   : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure StartPrefixMapping
                               ( const _prefix  : String ;
                                 const _uri     : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure EndPrefixMapping
                               ( const _prefix  : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure ProcessingInstruction
                               ( const _target  : String ;
                                 const _data    : String
                               ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure SkippedEntity
                               ( const _name    : String
                               ) ; virtual; abstract;
  end ;

  /// <summary>
  ///   Abstract class mimicking the ISAXContentHandler interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXContentHandler = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class ( ISAXContentHandler_ )
    public
      {#GENDOC:HIDE}
      procedure PutDocumentLocator
                               ( const _locator : IVBSAXLocator
                               ) ; virtual; abstract;
  end ;

  /// <summary>
  ///   Abstract class mimicking the IVBSAXContentHandler interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  IVBSAXContentHandler = {$IFDEF OXYGENE} public abstract {$ENDIF}
                         class ( ISAXContentHandler )
    protected
      function  fget_Locator : IVBSAXLocator ; virtual; abstract;
    public
      {#GENDOC:HIDE}
      property  DocumentLocator : IVBSAXLocator
                                  read  fget_Locator ;
  end ;

  /// <summary>
  ///   Abstract class mimicking the ISAXErrorHandler interface.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  ISAXErrorHandler_ = {$IFDEF OXYGENE} public abstract {$ENDIF}
                      class ( IVBSAXContentHandler )
    public
      {#GENDOC:HIDE}
      procedure Error            ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure FatalError       ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; virtual; abstract;
      {#GENDOC:HIDE}
      procedure IgnorableWarning ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; virtual; abstract;
  end ;

  {#GENDOC:HIDE}
  ISAXErrorHandler = {$IFDEF OXYGENE} public {$ENDIF} ISAXErrorHandler_ ;

  {#GENDOC:HIDE}
  IVBSAXErrorHandler = {$IFDEF OXYGENE} public {$ENDIF} ISAXErrorHandler_ ;

  {#GENDOC:HIDE}
  TGIS_SAXAttribute = class
    public
      URI   : String ;
      LName : String ;
      QName : String ;
      AType : String ;
      Value : String ;
  end ;

  /// <summary>
  ///   Implementation of element attributes for SAX (Simple API for XML).
  /// </summary>
  TGIS_SAXAttributes = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( IMXAttributes )
    private
      {$IFDEF DCC}
        oAttributes : TObjectList<TGIS_SAXAttribute> ;
      {$ENDIF}
      {$IFDEF CLR}
        oAttributes : List<TGIS_SAXAttribute> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        oAttributes : List<TGIS_SAXAttribute> ;
      {$ENDIF}
      {$IFDEF JAVA}
        oAttributes : ArrayList<TGIS_SAXAttribute> ;
      {$ENDIF}
    protected
      function  fget_Length : Integer ; override;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Gets the index of an attribute by its local name.
      /// </summary>
      /// <param name="_uri">
      ///   URI of the attribute
      /// </param>
      /// <param name="_lname">
      ///   local name of the attribute
      /// </param>
      /// <returns>
      ///   index of the attribute
      /// </returns>
      function  GetIndexFromName  ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : Integer ; override;
      /// <summary>
      ///   Gets the index of an attribute by its qualified name.
      /// </summary>
      /// <param name="_qname">
      ///   qualified name of the attribute
      /// </param>
      /// <returns>
      ///   index of the attribute
      /// </returns>
      function  GetIndexFromQName ( const _qname  : String
                                  ) : Integer ; override;
      /// <summary>
      ///   Gets the local name of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <returns>
      ///   local name of the attribute
      /// </returns>
      function  GetLocalName      ( const _index  : Integer
                                  ) : String ; override;
      /// <summary>
      ///   Gets the qualified name of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <returns>
      ///   qualified name of the attribute
      /// </returns>
      function  GetQName          ( const _index  : Integer
                                  ) : String ; override;
      /// <summary>
      ///   Gets the type of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <returns>
      ///   type of the attribute
      /// </returns>
      function  GetType           ( const _index  : Integer
                                  ) : String ; override;
      /// <summary>
      ///   Gets the type of an attribute by its local name.
      /// </summary>
      /// <param name="_uri">
      ///   URI of the attribute
      /// </param>
      /// <param name="_lname">
      ///   local name of the attribute
      /// </param>
      /// <returns>
      ///   type of the attribute
      /// </returns>
      function  GetTypeFromName   ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : String ; override;
      /// <summary>
      ///   Gets the type of an attribute by its qualified name.
      /// </summary>
      /// <param name="_qname">
      ///   qualified name of the attribute
      /// </param>
      /// <returns>
      ///   type of the attribute
      /// </returns>
      function  GetTypeFromQName  ( const _qname  : String
                                  ) : String ; override;
      /// <summary>
      ///   Gets the URI of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <returns>
      ///   URI of the attribute
      /// </returns>
      function  GetURI            ( const _index  : Integer
                                  ) : String ; override;
      /// <summary>
      ///   Gets the value of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <returns>
      ///   value of the attribute
      /// </returns>
      function  GetValue          ( const _index  : Integer
                                  ) : String ; override;
      /// <summary>
      ///   Gets the value of an attribute by its local name.
      /// </summary>
      /// <param name="_uri">
      ///   URI of the attribute
      /// </param>
      /// <param name="_lname">
      ///   local name of the attribute
      /// </param>
      /// <returns>
      ///   value of the attribute
      /// </returns>
      function  GetValueFromName  ( const _uri    : String  ;
                                    const _lname  : String
                                  ) : String ; override;
      /// <summary>
      ///   Gets the value of an attribute by its qualified name.
      /// </summary>
      /// <param name="_qname">
      ///   qualified name of the attribute
      /// </param>
      /// <returns>
      ///   value of the attribute
      /// </returns>
      function  GetValueFromQName ( const _qname  : String
                                  ) : String ; override;
    public
      /// <summary>
      ///   Gets the number of attributes.
      /// </summary>
      /// <param name="_len">
      ///   the number of attributes
      /// </param>
      procedure GetLength         (   out _len    : Integer
                                  ) ; override;
      /// <summary>
      ///   Gets the full identification of an attribute by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_uri">
      ///   URI of the attribute
      /// </param>
      /// <param name="_luri">
      ///   length of the URI string
      /// </param>
      /// <param name="_lname">
      ///   local name of the attribute
      /// </param>
      /// <param name="_llname">
      ///   length of the local name string
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the attribute
      /// </param>
      /// <param name="_lqname">
      ///   length of the qualified name string
      /// </param>
      procedure GetName           ( const _index  : Integer ;
                                      out _uri    : String  ;
                                      out _luri   : Integer ;
                                      out _lname  : String  ;
                                      out _llname : Integer ;
                                      out _qname  : String  ;
                                      out _lqname : Integer
                                  ) ; override;
    public
      /// <summary>
      ///   Adds a new attribute.
      /// </summary>
      /// <param name="_uri">
      ///   URI of the new attribute
      /// </param>
      /// <param name="_lname">
      ///   local name of the new attribute
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the new attribute
      /// </param>
      /// <param name="_type">
      ///   type of the new attribute
      /// </param>
      /// <param name="_value">
      ///   value of the new attribute
      /// </param>
      procedure AddAttribute      ( const _uri    : String ;
                                    const _lname  : String ;
                                    const _qname  : String ;
                                    const _type   : String ;
                                    const _value  : String
                                  ) ; override;
      /// <summary>
      ///   Clears the list of attributes.
      /// </summary>
      procedure Clear             ; override;
      /// <summary>
      ///   Sets the properties of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_uri">
      ///   new URI of the attribute
      /// </param>
      /// <param name="_lname">
      ///   new local name of the attribute
      /// </param>
      /// <param name="_qname">
      ///   new qualified name of the attribute
      /// </param>
      /// <param name="_type">
      ///   new type of the attribute
      /// </param>
      /// <param name="_value">
      ///   new value of the attribute
      /// </param>
      procedure SetAttribute      ( const _index  : Integer ;
                                    const _uri    : String ;
                                    const _lname  : String ;
                                    const _qname  : String ;
                                    const _type   : String ;
                                    const _value  : String
                                  ) ; override;
      /// <summary>
      ///   Deletes the attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      procedure RemoveAttribute
                                  ( const _index  : Integer
                                  ) ; override;
      /// <summary>
      ///   Sets the local name of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_lname">
      ///   new local name of the attribute
      /// </param>
      procedure SetLocalName      ( const _index  : Integer ;
                                    const _lname  : String
                                  ) ; override;
      /// <summary>
      ///   Sets the qualified name of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_qname">
      ///   new qualified name of the attribute
      /// </param>
      procedure SetQName          ( const _index  : Integer ;
                                    const _qname  : String
                                  ) ; override;
      /// <summary>
      ///   Sets the type of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_type">
      ///   new type of the attribute
      /// </param>
      procedure SetType           ( const _index  : Integer ;
                                    const _type   : String
                                  ) ; override;
      /// <summary>
      ///   Sets the URI of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_uri">
      ///   new URI of the attribute
      /// </param>
      procedure SetURI            ( const _index  : Integer ;
                                    const _uri    : String
                                  ) ; override;
      /// <summary>
      ///   Sets the value of an attribute at a specific index.
      /// </summary>
      /// <param name="_index">
      ///   index of the attribute
      /// </param>
      /// <param name="_value">
      ///   new value of the attribute
      /// </param>
      procedure SetValue          ( const _index  : Integer ;
                                    const _value  : String
                                  ) ; override;
  end ;

  /// <summary>
  ///   Functional extension of the SAX (Simple API for XML) element attributes
  ///   implementation.
  /// </summary>
  CoSAXAttributes = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_SAXAttributes )
    public
      /// <inheritdoc/>
      constructor Create ;
    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Creates a new instance of CoSAXAttributes.
      /// </summary>
      /// <returns>
      ///   new instance of CoSAXAttributes
      /// </returns>
      class function _Create       : IMXAttributes ;
      /// <summary>
      ///   Creates a new instance of CoSAXAttributes.
      /// </summary>
      /// <param name="_machinename">
      ///   unused; for interface compatibility only
      /// </param>
      /// <returns>
      ///   new instance of CoSAXAttributes
      /// </returns>
      class function _CreateRemote ( const _machinename : String
                                   ) : IMXAttributes ;
  end ;

  /// <summary>
  ///   SAX (Simple API for XML) reader implementation.
  /// </summary>
  TGIS_SAXContentHandler = {$IFDEF OXYGENE} public {$ENDIF}
      class ( IVBSAXErrorHandler )
    private
      FLocator : IVBSAXLocator ;
    private
      oStream     : TObject ;
      oReader     : TGIS_TextStreamReader ;
      iCodePage   : Integer ;
    {$IFDEF OXYGENE} unit {$ENDIF}
      iLine       : Integer ;
      iColumn     : Integer ;
    protected
      /// <summary>
      ///   Stores path to the XML file.
      /// </summary>
      sPath       : String ;
      /// <summary>
      ///   Allows to break the parsing procedure.
      /// </summary>
      bBreak      : Boolean ;
      /// <summary>
      ///   Allows to suppress encoding recognition.
      /// </summary>
      bEncoding   : Boolean ;
      /// <summary>
      ///   Read a line only instead of a buffer.
      /// </summary>
      bReadLineOnly   : Boolean ;
      /// <summary>
      ///   if True, whitespace chars are preserved, otherwise trimmed.
      /// </summary>
      bPreserveWhiteSpace : Boolean ;
    protected
      function  fget_Locator : IVBSAXLocator ; override;
    private
      function  isWhiteSpace   ( const _char : Char
                               ) : Boolean ;
                               {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure loadFromSource ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Initializes loading of an XML document from file.
      /// </summary>
      /// <param name="_path">
      ///   path to the XML file
      /// </param>
      procedure LoadFromFile     ( const _path   : String
                                 ) ;
      /// <summary>
      ///   Initializes loading of an XML document from stream.
      /// </summary>
      /// <param name="_stream">
      ///   stream which contains XML data
      /// </param>
      procedure LoadFromStream   ( const _stream : TStream
                                 ) ; overload;
    public // utilities
      /// <summary>
      ///   Removes white characters from a string.
      /// </summary>
      /// <param name="_str">
      ///   string to be cleared
      /// </param>
      /// <returns>
      ///   processed string
      /// </returns>
      function  RemoveWhitesFast ( const _str   : String
                                 ) : String ;
      /// <summary>
      ///   Replaces special characters with escape strings.
      /// </summary>
      /// <param name="_str">
      ///   string to be parsed
      /// </param>
      /// <returns>
      ///   processed string
      /// </returns>
      function  EscapeText       ( const _str   : String
                                 ) : String ;
    public // inherited from ISAXContentHandler/IVBSAXContentHandler
      /// <summary>
      ///   Indicates the beginning of an XML document.
      /// </summary>
      procedure StartDocument    ; override;
      /// <summary>
      ///   Indicates the end of an XML document.
      /// </summary>
      procedure EndDocument      ; override;
      /// <summary>
      ///   Indicates an occurrence of the beginning of an element (start tag).
      /// </summary>
      /// <param name="_uri">
      ///   URI of the element
      /// </param>
      /// <param name="_lname">
      ///   local name of the element
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the element
      /// </param>
      /// <param name="_attribs">
      ///   attributes of the element
      /// </param>
      procedure StartElement     ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String ;
                                   const _attribs : IVBSAXAttributes
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of the end of an element (end tag).
      /// </summary>
      /// <param name="_uri">
      ///   URI of the element
      /// </param>
      /// <param name="_lname">
      ///   local name of the element
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the element
      /// </param>
      procedure EndElement       ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of a character or a character data (CDATA)
      ///   block.
      /// </summary>
      /// <param name="_chars">
      ///   the character string
      /// </param>
      procedure Characters       ( const _chars   : String
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of a ignorable whitespace character block;
      ///   unused.
      /// </summary>
      /// <param name="_chars">
      ///   the character block as string
      /// </param>
      procedure IgnorableWhitespace
                                 ( const _chars   : String
                                 ) ; override;
      /// <summary>
      ///   Indicates the beginning of a scope of a prefix-URI namespace
      ///   mapping.
      /// </summary>
      /// <param name="_prefix">
      ///   the prefix
      /// </param>
      /// <param name="_uri">
      ///   the URI the prefix maps to
      /// </param>
      procedure StartPrefixMapping
                                 ( const _prefix  : String ;
                                   const _uri     : String
                                 ) ; override;
      /// <summary>
      ///   Indicates the end of a scope of a prefix-URI namespace mapping.
      /// </summary>
      /// <param name="_prefix">
      ///   the prefix
      /// </param>
      procedure EndPrefixMapping ( const _prefix  : String
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of a processing instruction.
      /// </summary>
      /// <param name="_target">
      ///   the target of the processing instruction
      /// </param>
      /// <param name="_data">
      ///   the processing instruction data
      /// </param>
      procedure ProcessingInstruction
                                 ( const _target  : String ;
                                   const _data    : String
                                 ) ; override;
      /// <summary>
      ///   Indicates a skipped entity.
      /// </summary>
      /// <param name="_name">
      ///   the name of the skipped entity
      /// </param>
      procedure SkippedEntity
                                 ( const _name    : String
                                 ) ; override;
    public // inherited from ISAXContentHandler
      /// <summary>
      ///   Attaches a SAX locator to this SAX content handler instance.
      /// </summary>
      /// <param name="_locator">
      ///   the SAX locator to be attached
      /// </param>
      procedure PutDocumentLocator
                                 ( const _locator : IVBSAXLocator
                                 ) ; override;
    public // inherited from IVBSAXErrorHandler
      /// <summary>
      ///   Indicates an occurrence of a non-critical error in the XML
      ///   structure; document parsing is being continued.
      /// </summary>
      /// <param name="_locator">
      ///   the SAX locator
      /// </param>
      /// <param name="_message">
      ///   the error message
      /// </param>
      /// <param name="_code">
      ///   the error code
      /// </param>
      procedure Error            ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of a critical error in the XML structure;
      ///   document parsing cannot be continued and is immediately terminated.
      /// </summary>
      /// <param name="_locator">
      ///   the SAX locator
      /// </param>
      /// <param name="_message">
      ///   the error message
      /// </param>
      /// <param name="_code">
      ///   the error code
      /// </param>
      procedure FatalError       ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
      /// <summary>
      ///   Indicates an occurrence of a minor inconsistency in the XML
      ///   structure.
      /// </summary>
      /// <param name="_locator">
      ///   the SAX locator
      /// </param>
      /// <param name="_message">
      ///   the error message
      /// </param>
      /// <param name="_code">
      ///   the error code
      /// </param>
      procedure IgnorableWarning ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
    public // additional "events" for DOM implementation
      /// <summary>
      ///   Additional event not found in MSXML SAX.
      ///   Indicates the XML declaration.
      /// </summary>
      /// <param name="_ver">
      ///   XML version
      /// </param>
      /// <param name="_enc">
      ///   a string which identifies the document encoding
      /// </param>
      /// <param name="_stndl">
      ///   is the document standalone - yes or no
      /// </param>
      procedure _XMLDecl         ( const _ver   : String ;
                                   const _enc   : String ;
                                   const _stndl : String
                                 ) ; virtual;
      /// <summary>
      ///   Additional event not found in MSXML SAX.
      ///   Indicates the document type declaration (DTD, DOCTYPE).
      /// </summary>
      /// <param name="_data">
      ///   the content of the document type declaration
      /// </param>
      procedure _DocType         ( const _data  : String
                                 ) ; virtual;
      /// <summary>
      ///   Indicates an occurrence of a character block.
      /// </summary>
      /// <param name="_chars">
      ///   the character block as string
      /// </param>
      procedure _Content         ( const _chars : String
                                 ) ; virtual;
      /// <summary>
      ///   Indicates an occurrence of a comment block.
      /// </summary>
      /// <param name="_chars">
      ///   the content of the comment block
      /// </param>
      procedure _Comment         ( const _chars : String
                                 ) ; virtual;
      /// <summary>
      ///   Indicates an occurrence of a character data (CDATA) block.
      /// </summary>
      /// <param name="_chars">
      ///   the content of the character data block
      /// </param>
      procedure _CDATA           ( const _chars : String
                                 ) ; virtual;
  end ;

  /// <summary>
  ///   SAX (Simple API for XML) writer implementation.
  /// </summary>
  TGIS_SAXWriter = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      oWriter    : TXMLStreamWriter ;
      oBuilder   : TXMLStringBuilder ;
      iIndent    : Integer ;
      sQName     : String ;
      bEmpty     : Boolean ;
      bTextNode  : Boolean ;
      bInCDATA   : Boolean ;
      oEnc       : TEncoding ;
    private
      FIndent    : Boolean ;
      FIndentStr : String ;
      FCompact   : Boolean ;
    private
      procedure closeElement  ( const _empty : Boolean
                              ) ;
      procedure closeNotEmpty ;
      procedure incIndent     ;
      procedure decIndent     ;
      procedure appendIndent  ;
      procedure appendCRLF    ;
      procedure appendChars   ( const _chars : String  ;
                                const _bspc  : Boolean
                              ) ;
      function  putCharEntRef ( const _str   : String
                              ) : String ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_strm">
      ///   the stream to write to
      /// </param>
      constructor Create ( const _strm : TXMLOutputStream
                         ) ;
    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Begins the XML document.
      /// </summary>
      procedure StartDocument    ;
      /// <summary>
      ///   Closes the XML document.
      /// </summary>
      procedure EndDocument      ;
      /// <summary>
      ///   Begins an element (adds a start tag).
      /// </summary>
      /// <param name="_uri">
      ///   URI of the element
      /// </param>
      /// <param name="_lname">
      ///   local name of the element
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the element
      /// </param>
      /// <param name="_attribs">
      ///   attributes of the element
      /// </param>
      procedure StartElement     ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String ;
                                   const _attribs : IVBSAXAttributes
                                 ) ;
      /// <summary>
      ///   Closes an element (adds an end tag).
      /// </summary>
      /// <param name="_uri">
      ///   URI of the element
      /// </param>
      /// <param name="_lname">
      ///   local name of the element
      /// </param>
      /// <param name="_qname">
      ///   qualified name of the element
      /// </param>
      procedure EndElement       ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String
                                 ) ;
      /// <summary>
      ///   Adds a character block.
      /// </summary>
      /// <param name="_chars">
      ///   the character block as string
      /// </param>
      procedure Characters       ( const _chars   : String
                                 ) ;
      /// <summary>
      ///   Adds a processing instruction.
      /// </summary>
      /// <param name="_target">
      ///   the target of the processing instruction
      /// </param>
      /// <param name="_data">
      ///   the processing instruction data
      /// </param>
      procedure ProcessingInstruction
                                 ( const _target  : String ;
                                   const _data    : String
                                 ) ;
    public // for convenience
      /// <summary>
      ///   Begins a character data (CDATA) block. A CDATA block
      ///   can be then filled with the Characters method.
      /// </summary>
      procedure StartCDATA       ;
      /// <summary>
      ///   Closes a character data (CDATA) block.
      /// </summary>
      procedure EndCDATA         ;
    public
      /// <summary>
      ///   Indicates if the content of the XML document should be indented.
      /// </summary>
      property Indent       : Boolean read FIndent    write FIndent    ;
      /// <summary>
      ///   The character string used as indent.
      /// </summary>
      property IndentString : String  read FIndentStr write FIndentStr ;
    public // additional "events" for DOM implementation
      /// <summary>
      ///   Additional event not found in MSXML SAX.
      ///   Adds the XML declaration.
      /// </summary>
      /// <param name="_ver">
      ///   XML version
      /// </param>
      /// <param name="_enc">
      ///   a string which identifies the document encoding
      /// </param>
      /// <param name="_stndl">
      ///   is the document standalone - yes or no
      /// </param>
      procedure _XMLDecl         ( const _ver   : String ;
                                   const _enc   : String ;
                                   const _stndl : String
                                 ) ;
      /// <summary>
      ///   Additional event not found in MSXML SAX.
      ///   Adds the document type declaration (DTD, DOCTYPE).
      /// </summary>
      /// <param name="_data">
      ///   the content of the document type declaration
      /// </param>
      procedure _DocType         ( const _data  : String
                                 ) ;
      /// <summary>
      ///   Adds a character block.
      /// </summary>
      /// <param name="_chars">
      ///   the character block as string
      /// </param>
      procedure _Content         ( const _chars : String
                                 ) ;
      /// <summary>
      ///   Adds a comment block.
      /// </summary>
      /// <param name="_chars">
      ///   the content of the comment block
      /// </param>
      procedure _Comment         ( const _chars : String
                                 ) ;
      /// <summary>
      ///   Adds a character data (CDATA) block.
      /// </summary>
      /// <param name="_chars">
      ///   the content of the character data block
      /// </param>
      procedure _CDATA           ( const _chars : String
                                 ) ;
    public
      /// <summary>
      ///   Indicates if the XML document should be saved in compact form
      ///   (no line breaks between elements and no indenting).
      /// </summary>
      property SaveCompact       : Boolean
                                   read  FCompact
                                   write FCompact ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses ;
{$ENDIF}

const
  SAX_MSG_ERROR           : String  =
      'Error parsing XML (line %d, column %d) with the message: ' ;
  SAX_ERR_GENERIC         : String  =
      'Unexpected character.' ;
  SAX_ERR_CODE_GENERIC    : Integer = $F100 ;
  SAX_ERR_XML             : String  =
      'XML declaration must appear before the root element.' ;
  SAX_ERR_CODE_XML        : Integer = $F101 ;
  SAX_ERR_DOCTYPE         : String  =
      'Document type declaration must appear before the root element.' ;
  SAX_ERR_CODE_DOCTYPE    : Integer = $F102 ;
  SAX_ERR_ELEMENTEND      : String  =
      'End tag </%s> does not match the last start tag <%s>.' ;
  SAX_ERR_CODE_ELEMENTEND : Integer = $F103 ;
  SAX_ERR_ATTIRBUTEVALUE  : String =
      'Attribute value is not placed between quotes.' ;
  SAX_ERR_CODE_ATTIRBUTEVALUE
                          : Integer = $F104 ;
  SAX_ERR_CER             : String  =
      'Wrong character entity reference.' ;
  SAX_ERR_CODE_CER        : Integer = $F105 ;

type

  T_saxList = class
    private
      capacity : Integer ;
    public
      {$IFDEF DCC}
        Strings : array of String ;
      {$ENDIF}
      {$IFDEF CLR}
        Strings : List<String> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        Strings : List<String> ;
      {$ENDIF}
      {$IFDEF JAVA}
        Strings : ArrayList<String> ;
      {$ENDIF}
    public
      constructor Create ( const _capacity : Integer
                         ) ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure Add      ( const _str : String
                         ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure Clear    ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  ToString : String ; {$IFDEF OXYGENE} reintroduce ; {$ENDIF}
    public
      Count : Integer ;
  end ;

  T_saxState = (
    Outside,
    Inside,
    ProcInstName,
    ProcInst,
    ProcInstEnd,
    ElementName,
    Element,
    ElementEnd,
    ElementEmptyEnd,
    Exclamation,
    Comment1,
    Comment2,
    CommentEnd1,
    CommentEnd2,
    CDATA1,
    CDATA2,
    CDATA3,
    CDATA4,
    CDATA5,
    CDATA6,
    CDATA7,
    CDATAEnd1,
    CDATAEnd2,
    DOCTYPE1,
    DOCTYPE2,
    DOCTYPE3,
    DOCTYPE4,
    DOCTYPE5,
    DOCTYPE6,
    DOCTYPE7,
    DOCTYPE8,
    CharEntRef,
    Quote1,
    Quote2
  ) ;

  T_saxElementState = (
    Whitespace,
    RequireWhitespace,
    EPrefix,
    EName,
    APrefix,
    AName,
    Axmlns1,
    Axmlns2,
    Axmlns3,
    Axmlns4,
    Axmlns5,
    AValue,
    AValue0,
    AValue1,
    AValue2,
    ACharEntRef,
    DTD
  ) ;

  T_saxNamespaceDef = class
    public
      Element : String ;
      Prefix  : String ;
      URI     : String ;
  end ;

  T_saxNamespaceMap = array of T_saxNamespaceDef ;

  T_saxStack = class
    {$IFDEF DCC} private {$ENDIF}
    {$IFDEF OXYGENE} unit {$ENDIF}
      oSAX       : TGIS_SAXContentHandler ;
      {$IFDEF DCC}
        oStack   : TList<String> ;
      {$ENDIF}
      {$IFDEF CLR}
        oStack   : List<String> ;
      {$ENDIF}
      {$IFDEF JAVA}
        oStack   : ArrayList<String> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        oStack   : TList<String> ;
      {$ENDIF}
      {$IFDEF DCC}
        oNSStack : TObjectList<T_saxNamespaceDef> ;
      {$ENDIF}
      {$IFDEF CLR}
        oNSStack : List<T_saxNamespaceDef> ;
      {$ENDIF}
      {$IFDEF JAVA}
        oNSStack : ArrayList<T_saxNamespaceDef> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        oNSStack : TList<T_saxNamespaceDef> ;
      {$ENDIF}
    private
      function  fget_LastIn : String ;
                            {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  fget_Count  : Integer ;
                            {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
    public
      constructor Create ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure Push    ( const _str  : String
                        ) ; overload; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      {$IFDEF DCC}
        procedure Push  ( const _str  : String ;
                          const _nss  : T_saxNamespaceMap
                        ) ; overload; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      {$ENDIF}
      {$IFDEF CLR}
        procedure Push  ( const _str  : String ;
                          const _nss  : List<T_saxNamespaceDef>
                        ) ; overload; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      {$ENDIF}
      {$IFDEF ISLAND}
        procedure Push  ( const _str  : String ;
                          const _nss  : List<T_saxNamespaceDef>
                        ) ; overload ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      {$ENDIF}
      {$IFDEF JAVA}
        procedure Push  ( const _str  : String ;
                          const _nss  : ArrayList<T_saxNamespaceDef>
                        ) ; overload; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      {$ENDIF}
      procedure Pop     ;  {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure ForcePop
                        ( const _str  : String
                        ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function ResolvePrefix
                        ( const _prfx : String
                        ) : String ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure Clear   ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
    public
      property LastIn   : String
                          read  fget_LastIn ;
      property Count    : Integer
                          read  fget_Count ;
  end ;

  T_saxLocator = class ( IVBSAXLocator )
    {$IFDEF DCC} private {$ENDIF}
    {$IFDEF OXYGENE} unit {$ENDIF}
      oSAX : TGIS_SAXContentHandler ;
    protected
      function  fget_ColumnNumber : Integer ; override;
      function  fget_LineNumber   : Integer ; override;
      function  fget_PublicId     : String  ; override;
      function  fget_SystemId     : String  ; override;
    public
      constructor Create ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure GetColumnNumber   (   out _val : Integer
                                  ) ; override;
      procedure GetLineNumber     (   out _val : Integer
                                  ) ; override;
      procedure GetPublicId       (   out _val : String
                                  ) ; override;
      procedure GetSystemId       (   out _val : String
                                  ) ; override;
  end ;

  T_saxDeclReader = class ( TGIS_SAXContentHandler )
    public
      constructor Create ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure StartElement     ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String ;
                                   const _attribs : IVBSAXAttributes
                                 ) ; override;
      procedure _XMLDecl         ( const _ver   : String ;
                                   const _enc   : String ;
                                   const _stndl : String
                                 ) ; override;
      procedure Error            ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
      procedure FatalError       ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
    public
      Encoding : Integer ;
  end ;

  {$IFDEF DCC}
    T_UTF8NoBOMEncoding = class( TUTF8Encoding )
    public
      function GetPreamble : TBytes ; override ;
    end ;

    function T_UTF8NoBOMEncoding.GetPreamble : TBytes ;
    begin
      SetLength( Result, 0 ) ;
    end ;
  {$ENDIF}

  procedure saxSBClear(
    const _sb : TXMLStringBuilder
  ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  begin
    {$IFDEF DCC}
      _sb.Length := 0 ;
    {$ENDIF}
    {$IFDEF CLR}
      _sb.Length := 0 ;
    {$ENDIF}
    {$IFDEF JAVA}
      _sb.setLength( 0 ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      _sb.Length := 0 ;
    {$ENDIF}
  end ;

  function saxLength(
    const _str : String
  ) : Integer ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  begin
    Result := length( _str ) ;
  end ;

//==============================================================================
// T_saxList
//==============================================================================

  constructor T_saxList.Create(
    const _capacity : Integer
  ) ;
  begin
    inherited Create ;

    capacity := _capacity ;
    {$IFDEF DCC}
      SetLength( Strings, capacity ) ;
    {$ENDIF}
    {$IFDEF CLR}
      Strings := new List<String> ;
      Strings.Capacity := _capacity ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Strings := new List<String> ;
    {$ENDIF}
    {$IFDEF JAVA}
      Strings := new ArrayList<String> ;
    {$ENDIF}
  end ;

  {$IFDEF DCC}
    destructor T_saxList.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}

  procedure T_saxList.Add(
    const _str : String
  ) ;
  begin
    {$IFDEF DCC}
      if Count = capacity then begin
        Inc( capacity, 10 ) ;
        SetLength( Strings, capacity ) ;
      end ;

      Strings[Count] := _str ;
    {$ENDIF}
    {$IFDEF CLR}
      Strings.Add( _str ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Strings.Add( _str ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      Strings.Add( _str ) ;
    {$ENDIF}

    inc( Count ) ;
  end ;

  procedure T_saxList.Clear ;
  begin
    {$IFDEF CLR}
      Strings.Clear ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Strings.Clear ;
    {$ENDIF}
    {$IFDEF JAVA}
      Strings.Clear ;
    {$ENDIF}
    Count := 0 ;
  end ;

  function T_saxList.ToString : String ;
  var
    sb  : TXMLStringBuilder ;
    res : String ;
    i   : Integer ;
  begin
    sb := TXMLStringBuilder.Create ;
    try
      for i := 0 to Count - 1 do
        sb.Append( Strings[i] ) ;
      res := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;

//==============================================================================
// T_saxStack
//==============================================================================

  constructor T_saxStack.Create ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      oStack := TList<String>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oStack := List<String>.Create ;
    {$ENDIF}
    {$IFDEF JAVA}
      oStack := ArrayList<String>.Create ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oStack := TList<String>.Create ;
    {$ENDIF}

    {$IFDEF DCC}
      oNSStack := TObjectList<T_saxNamespaceDef>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oNSStack := List<T_saxNamespaceDef>.Create ;
    {$ENDIF}
    {$IFDEF JAVA}
      oNSStack := ArrayList<T_saxNamespaceDef>.Create ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oNSStack := TList<T_saxNamespaceDef>.Create ;
    {$ENDIF}
  end ;

  {$IFDEF DCC}
    destructor T_saxStack.Destroy ;
    begin
      FreeObject( oNSStack ) ;
      FreeObject( oStack   ) ;

      inherited ;
    end ;
  {$ENDIF}

  function T_saxStack.fget_LastIn : String ;
  begin
    if oStack.Count > 0 then
      Result := oStack[oStack.Count-1]
    else
      Result := '' ;
  end ;

  function T_saxStack.fget_Count : Integer ;
  begin
    Result := oStack.Count ;
  end ;

  procedure T_saxStack.Push(
    const _str : String
  ) ;
  begin
    oStack.Add( _str ) ;
  end ;

{$IFDEF DCC}
  procedure T_saxStack.Push(
    const _str  : String ;
    const _nss  : T_saxNamespaceMap
  ) ;
  var
    i  : Integer ;
  begin
    Push( _str ) ;

    for i := 0 to Length( _nss ) - 1 do
      oNSStack.Add( _nss[i] ) ;
  end ;
{$ENDIF}

{$IFDEF CLR}
  procedure T_saxStack.Push(
    const _str  : String ;
    const _nss  : List<T_saxNamespaceDef>
  ) ;
  var
    i  : Integer ;
  begin
    Push( _str ) ;

    for i := 0 to _nss.Count - 1 do
      oNSStack.Add( _nss[i] ) ;
  end ;
{$ENDIF}

{$IFDEF ISLAND}
  procedure T_saxStack.Push(
    const _str  : String ;
    const _nss  : List<T_saxNamespaceDef>
  ) ;
  var
    i  : Integer ;
  begin
    Push( _str ) ;

    for i := 0 to _nss.Count - 1 do
      oNSStack.Add( _nss[i] ) ;
  end ;
{$ENDIF}

{$IFDEF JAVA}
  procedure T_saxStack.Push(
    const _str  : String ;
    const _nss  : ArrayList<T_saxNamespaceDef>
  ) ;
  var
    i  : Integer ;
  begin
    Push( _str ) ;

    for i := 0 to _nss.size - 1 do
      oNSStack.Add( _nss[i] ) ;
  end ;
{$ENDIF}

  procedure T_saxStack.Pop ;
  var
    str : String ;
    i   : Integer ;
  begin
    if oStack.Count = 0 then
      exit ;

    for i := oNSStack.Count - 1 downto 0 do begin
      if CompareText( oNSStack[i].Element, LastIn ) = 0 then begin
        if length( oNSStack[i].Prefix ) <> 0 then begin
          str := oNSStack[i].Prefix ;
          oSAX.EndPrefixMapping( str ) ;
        end ;
        {$IFDEF DCC}
          oNSStack.Delete( i ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oNSStack.RemoveAt( i ) ;
        {$ENDIF}
        {$IFDEF ISLAND}
          oNSStack.RemoveAt( i ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          oNSStack.Remove( i ) ;
        {$ENDIF}
      end
      else
        break ;
    end ;

    {$IFDEF DCC}
      oStack.Delete( oStack.Count - 1 ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oStack.RemoveAt( oStack.Count - 1 ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oStack.RemoveAt( oStack.Count - 1 ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      oStack.Remove( oStack.Count - 1 ) ;
    {$ENDIF}
  end ;

  procedure T_saxStack.ForcePop(
    const _str : String
  ) ;
  var
    i : Integer ;
  begin
    for i := oStack.Count - 1 downto 0 do begin
      if CompareText( _str, oStack[i] ) <> 0 then
        Pop ;
    end ;
  end ;

  function T_saxStack.ResolvePrefix(
    const _prfx : String
  ) : String ;
  var
    i : Integer ;
  begin
    if oNSStack.Count = 0 then
      exit ;

    for i := oNSStack.Count - 1 downto 0 do begin
      if CompareText( oNSStack[i].Prefix, _prfx ) = 0 then begin
        Result := oNSStack[i].URI ;
        break ;
      end ;
    end ;
  end ;

  procedure T_saxStack.Clear ;
  begin
    oNSStack.Clear ;
    oStack.Clear ;
  end ;

//==============================================================================
// T_saxLocator
//==============================================================================

  constructor T_saxLocator.Create ;
  begin
    inherited Create ;

  end ;

  {$IFDEF DCC}
    destructor T_saxLocator.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}

  function T_saxLocator.fget_ColumnNumber : Integer ;
  begin
    if assigned( oSAX ) then
      Result := oSAX.iColumn
    else
      Result := -1 ;
  end ;

  function T_saxLocator.fget_LineNumber : Integer ;
  begin
    if assigned( oSAX ) then
      Result := oSAX.iLine
    else
      Result := -1 ;
  end ;

  function T_saxLocator.fget_PublicId : String  ;
  begin
    Result := '' ; // not implemented
  end ;

  function T_saxLocator.fget_SystemId : String  ;
  begin
    Result := '' ; // not implemented
  end ;

  procedure T_saxLocator.GetColumnNumber(
      out _val : Integer
  ) ;
  begin
    if assigned( oSAX ) then
      _val := oSAX.iColumn
    else
      _val := -1 ;
  end ;

  procedure T_saxLocator.GetLineNumber(
      out _val : Integer
  ) ;
  begin
    if assigned( oSAX ) then
      _val := oSAX.iLine
    else
      _val := -1 ;
  end ;

  procedure T_saxLocator.GetPublicId(
      out _val : String
  ) ;
  begin
    _val := '' ; // not implemented
  end ;

  procedure T_saxLocator.GetSystemId(
      out _val : String
  ) ;
  begin
    _val := '' ; // not implemented
  end ;

//==============================================================================
// T_saxAttributes
//==============================================================================

  constructor TGIS_SAXAttributes.Create ;
  begin
    inherited ;

    {$IFDEF DCC}
      oAttributes := TObjectList<TGIS_SAXAttribute>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oAttributes := List<TGIS_SAXAttribute>.Create ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oAttributes := List<TGIS_SAXAttribute>.Create ;
    {$ENDIF}
    {$IFDEF JAVA}
      oAttributes := ArrayList<TGIS_SAXAttribute>.Create ;
    {$ENDIF}
  end ;

  {$IFDEF DCC}
    destructor TGIS_SAXAttributes.Destroy ;
    begin
      FreeObject( oAttributes ) ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_SAXAttributes.fget_Length : Integer ;
  begin
    Result := oAttributes.Count ;
  end ;

  function TGIS_SAXAttributes.GetIndexFromName(
    const _uri    : String  ;
    const _lname  : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].URI   = _uri   ) and
         ( oAttributes[i].LName = _lname ) then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_SAXAttributes.GetIndexFromQName(
    const _qname  : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oAttributes.Count - 1 do begin
      if oAttributes[i].LName = _qname then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_SAXAttributes.GetLocalName(
    const _index  : Integer
  ) : String ;
  begin
    if ( _index >= 0 ) and ( _index < oAttributes.Count ) then
      Result := oAttributes[_index].LName ;
  end ;

  function TGIS_SAXAttributes.GetQName(
    const _index  : Integer
  ) : String ;
  begin
    if ( _index >= 0 ) and ( _index < oAttributes.Count ) then
      Result := oAttributes[_index].QName ;
  end ;

  function TGIS_SAXAttributes.GetType(
    const _index  : Integer
  ) : String ;
  begin
    Result := 'CDATA' ;
  end ;

  function TGIS_SAXAttributes.GetTypeFromName(
    const _uri    : String  ;
    const _lname  : String
  ) : String ;
  begin
    Result := 'CDATA' ;
  end ;

  function TGIS_SAXAttributes.GetTypeFromQName(
    const _qname  : String
  ) : String ;
  begin
    Result := 'CDATA' ;
  end ;

  function TGIS_SAXAttributes.GetURI(
    const _index  : Integer
  ) : String ;
  begin
    if ( _index >= 0 ) and ( _index < oAttributes.Count ) then
      Result := oAttributes[_index].URI ;
  end ;

  function TGIS_SAXAttributes.GetValue(
    const _index  : Integer
  ) : String ;
  begin
    if ( _index >= 0 ) and ( _index < oAttributes.Count ) then
      Result := oAttributes[_index].Value ;
  end ;

  function TGIS_SAXAttributes.GetValueFromName(
    const _uri    : String  ;
    const _lname  : String
  ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].URI   = _uri   ) and
         ( oAttributes[i].LName = _lname ) then begin
        Result := oAttributes[i].Value ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_SAXAttributes.GetValueFromQName(
    const _qname  : String
  ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;

    for i := 0 to oAttributes.Count - 1 do begin
      if oAttributes[i].LName = _qname then begin
        Result := oAttributes[i].Value ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_SAXAttributes.GetLength(
    out _len    : Integer
  ) ;
  begin
    _len := oAttributes.Count ;
  end ;

  procedure TGIS_SAXAttributes.GetName(
    const _index  : Integer ;
      out _uri    : String  ;
      out _luri   : Integer ;
      out _lname  : String  ;
      out _llname : Integer ;
      out _qname  : String  ;
      out _lqname : Integer
  ) ;
  begin
    if ( _index >= 0 ) and ( _index < oAttributes.Count ) then begin
      _uri    := oAttributes[_index].URI ;
      _luri   := saxLength( oAttributes[_index].URI ) ;
      _lname  := oAttributes[_index].LName ;
      _llname := saxLength( oAttributes[_index].LName ) ;
      _qname  := oAttributes[_index].QName ;
      _lqname := saxLength( oAttributes[_index].QName ) ;
    end
    else begin
      _uri    := '' ;
      _luri   := 0 ;
      _lname  := '' ;
      _llname := 0 ;
      _qname  := '' ;
      _lqname := 0 ;
    end ;
  end ;

  procedure TGIS_SAXAttributes.AddAttribute(
    const _uri    : String ;
    const _lname  : String ;
    const _qname  : String ;
    const _type   : String ;
    const _value  : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    att := TGIS_SAXAttribute.Create ;

    att.URI   := _uri   ;
    att.LName := _lname ;
    att.QName := _qname ;
    att.AType := _type  ;
    att.Value := _value ;

    oAttributes.Add( att ) ;
  end ;

  procedure TGIS_SAXAttributes.Clear ;
  begin
    oAttributes.Clear ;
  end ;

  procedure TGIS_SAXAttributes.SetAttribute(
    const _index  : Integer ;
    const _uri    : String ;
    const _lname  : String ;
    const _qname  : String ;
    const _type   : String ;
    const _value  : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.URI   := _uri ;
    att.LName := _lname ;
    att.QName := _qname ;
    att.AType := _type ;
    att.Value := _value ;
  end ;

  procedure TGIS_SAXAttributes.RemoveAttribute(
    const _index  : Integer
  ) ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    {$IFDEF DCC}
      oAttributes.Delete( _index ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oAttributes.RemoveAt( _index ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oAttributes.RemoveAt( _index ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      oAttributes.Remove( _index ) ;
    {$ENDIF}
  end ;

  procedure TGIS_SAXAttributes.SetLocalName(
    const _index  : Integer ;
    const _lname  : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.LName := _lname ;
  end ;

  procedure TGIS_SAXAttributes.SetQName(
    const _index  : Integer ;
    const _qname  : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.QName := _qname ;
  end ;

  procedure TGIS_SAXAttributes.SetType(
    const _index  : Integer ;
    const _type   : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.AType := _type ;
  end ;

  procedure TGIS_SAXAttributes.SetURI(
    const _index  : Integer ;
    const _uri    : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.URI   := _uri ;
  end ;

  procedure TGIS_SAXAttributes.SetValue(
    const _index  : Integer ;
    const _value  : String
  ) ;
  var
    att : TGIS_SAXAttribute ;
  begin
    if ( _index < 0 ) and ( _index >= oAttributes.Count )  then
      exit ;

    att := oAttributes[_index] ;
    att.Value := _value ;
  end ;


//==============================================================================
// T_saxDeclReader
//==============================================================================

  constructor T_saxDeclReader.Create ;
  begin
    inherited ;

    bEncoding := False ;
    bReadLineOnly := True ;

    Encoding := 65001 ;
  end ;

  {$IFDEF DCC}
    destructor T_saxDeclReader.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}

  procedure T_saxDeclReader._XMLDecl(
    const _ver   : String ;
    const _enc   : String ;
    const _stndl : String
  ) ;
  begin
    if CompareText( _enc, 'UTF-8' ) = 0 then
      Encoding := 65001
    else
    if CompareText( _enc, 'UTF-16' ) = 0 then
      Encoding := 1200
    else
    if CompareText( _enc, 'ISO-10646-UCS-2' ) = 0 then
      Encoding := 1200
    else
    if CompareText( _enc, 'ISO-10646-UCS-4' ) = 0 then
      Encoding := 12000
    else
    if ( CompareText( _enc, 'ISO-8859-1'   ) = 0 ) or
       ( CompareText( _enc, 'Windows-1252' ) = 0 ) then
      Encoding := 28591
    else
    if CompareText( _enc, 'ISO-8859-2'   ) = 0 then
      Encoding := 28592
    else
    if CompareText( _enc, 'ISO-8859-3' ) = 0 then
      Encoding := 28593
    else
    if CompareText( _enc, 'ISO-8859-4' ) = 0 then
      Encoding := 28594
    else
    if CompareText( _enc, 'ISO-8859-5' ) = 0 then
      Encoding := 28595
    else
    if CompareText( _enc, 'ISO-8859-6' ) = 0 then
      Encoding := 28596
    else
    if CompareText( _enc, 'ISO-8859-7' ) = 0 then
      Encoding := 28597
    else
    if CompareText( _enc, 'ISO-8859-8' ) = 0 then
      Encoding := 28598
    else
    if CompareText( _enc, 'ISO-8859-9' ) = 0 then
      Encoding := 28599
    else
    if CompareText( _enc, 'ISO-2022-JP' ) = 0 then
      Encoding := 50220
    else
    if CompareText( _enc, 'SHIFT_JIS' ) = 0 then
      Encoding := 932
    else
    if CompareText( _enc, 'EUC-JP' ) = 0 then
      Encoding := 20932 ;

    bBreak := True ;
  end ;

  procedure T_saxDeclReader.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  begin
    bBreak := True ;
  end ;

  procedure T_saxDeclReader.Error(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    bBreak := True ;
  end ;

  procedure T_saxDeclReader.FatalError(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    bBreak := True ;
  end ;

//==============================================================================
// CoSAXAttributes
//==============================================================================

  constructor CoSAXAttributes.Create ;
  begin
    inherited ;

  end ;

  {$IFDEF DCC}
    destructor CoSAXAttributes.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}

  class function CoSAXAttributes._Create : IMXAttributes ;
  begin
    Result := CoSAXAttributes.Create ;
  end ;

  class function CoSAXAttributes._CreateRemote(
    const _machinename : String
  ) : IMXAttributes ;
  begin
    Result := CoSAXAttributes.Create ;
  end ;

//==============================================================================
// TGIS_SAXContentHandler
//==============================================================================

  constructor TGIS_SAXContentHandler.Create ;
  begin
    inherited ;

    FLocator := T_saxLocator.Create ;
    T_saxLocator( FLocator ).oSAX := Self ;

    iColumn := 0 ;
    iLine := 0 ;

    sPath := '' ;
    bBreak := False ;
    bEncoding := True ;
    bReadLineOnly := False ;
    bPreserveWhiteSpace := False ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_SAXContentHandler.Destroy ;
    begin
      FreeObject( FLocator ) ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_SAXContentHandler.isWhiteSpace(
    const _char : Char
  ) : Boolean ;
  begin
    Result := False ;

    if ord( _char ) > 32 then
      exit ;

    case _char of
      #9 ,
      #10,
      #13,
      #32 : Result := True ;
    end ;
  end ;

  procedure TGIS_SAXContentHandler.LoadFromFile(
    const _path : String
  ) ;
  var
    strm : TGIS_FileStream ;
  begin
    strm := TGIS_FileStream.Create( _path,
                                    fmOpenRead or
                                    fmShareDenyWrite
                                  ) ;
    try
      sPath := _path ;
      LoadFromStream( strm ) ;
    finally
      {$IFDEF DCC}
        FreeObject( strm ) ;
      {$ENDIF}
      {$IFDEF CLR}
        disposeAndNil( strm ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        strm.Close ;
      {$ENDIF}
    end;
  end ;

  procedure TGIS_SAXContentHandler.LoadFromStream(
    const _stream : TStream
  ) ;
  var
    decl : T_saxDeclReader ;
    pos  : Int64 ;
  begin
    pos := _stream.Position ;

    if bEncoding then begin
      decl := T_saxDeclReader.Create ;
      try
        decl.LoadFromStream( _stream ) ;
        iCodePage := decl.Encoding ;
      finally
        bReadLineOnly := False ;
        {$IFDEF DCC}
          FreeObject( decl ) ;
        {$ENDIF}
        {$IFDEF CLR}
          disposeAndNil( decl ) ;
        {$ENDIF}
      end ;
    end
    else
      iCodePage := 65001 ;

    _stream.Position := pos ;

    oReader := TGIS_TextStreamReader.Create( _stream ) ;
    try
      if oReader.CodePage = 0 then
        oReader.CodePage := iCodePage ;
      try
        loadFromSource ;
      except
        on e : Exception do begin
          raise EGIS_Exception.Create(
                  Format( SAX_MSG_ERROR,
                          [ FLocator.LineNumber, FLocator.ColumnNumber ]
                        ) +
                  e.Message,
                  sPath,
                  FLocator.LineNumber
                ) ;
        end ;
      end ;
    finally
      {$IFDEF DCC}
        FreeObject( oReader ) ;
      {$ENDIF}
      {$IFDEF CLR}
        disposeAndNil( oReader ) ;
      {$ENDIF}
    end;
  end;


  procedure TGIS_SAXContentHandler.loadFromSource ;
  var
    buf    : String ;
    bufend : Integer ;
    bufpos : Integer ;

    sb     : TXMLStringBuilder ;
    sbcer  : TXMLStringBuilder ;
    stack  : T_saxStack ;
    atts   : TGIS_SAXAttributes ;
    c      : Char ;
    s      : T_saxState ;
    sl     : T_saxState ;
    scer   : T_saxState ;

    suri   : String ;
    slnm   : String ;
    sqnm   : String ;
    svlu   : String ;

    blb    : Boolean ;

    eadd   : Boolean ;
    es     : T_saxElementState ;
    escer  : T_saxElementState ;
    elst   : T_saxList ;
    esb    : TXMLStringBuilder ;
    esbq   : TXMLStringBuilder ;
    epref  : String ;

    tmp    : String ;
    cer    : Integer ;

    procedure throw_error( const _s : String ; const _c : HResult ) ;
    begin
      Error( FLocator, _s, _c ) ;
    end ;

    procedure throw_fatalerror( const _s : String ; const _c : HResult ) ;
    begin
      FatalError( FLocator, _s, _c ) ;
    end ;

    procedure throw_ignorablewarning( const _s : String ; const _c : HResult ) ;
    begin
      IgnorableWarning( FLocator, _s, _c ) ;
    end ;

    procedure e_restore ;
    var
      tmp : String ;
    begin
      if esb.Length = 0 then begin
        elst.Add( epref ) ;
        elst.Add( '' ) ;
        elst.Add( epref ) ;
      end
      else begin
        tmp := esb.ToString ;
        esb.Length := 0 ;

        esbq.Append( epref ) ;
        esbq.Append( ':' ) ;
        esbq.Append( tmp ) ;
        elst.Add( esbq.ToString ) ;
        esbq.Length := 0 ;

        elst.Add( epref ) ;
        elst.Add( tmp ) ;
      end ;
    end ;

    procedure e_close ;
    begin
      case es of
        T_saxElementState.EPrefix :
          begin
            epref := esb.ToString ;
            esb.Length := 0 ;
            e_restore ;
          end ;
        T_saxElementState.EName :
          begin
            e_restore ;
          end ;
        T_saxElementState.APrefix :
          begin
            epref := esb.ToString ;
            esb.Length := 0 ;
            e_restore ;
            elst.Add( '' ) ;
          end ;
        T_saxElementState.AName :
          begin
            e_restore ;
            elst.Add( '' ) ;
          end ;
        T_saxElementState.Axmlns1,
        T_saxElementState.Axmlns2,
        T_saxElementState.Axmlns3,
        T_saxElementState.Axmlns4 :
          begin
            epref := esb.ToString ;
            esb.Length := 0 ;
            e_restore ;
            elst.Add( '' ) ;
          end ;
        T_saxElementState.AValue :
          begin
            elst.Add( esb.ToString ) ;
            esb.Length := 0 ;
          end ;
        T_saxElementState.AValue0 :
          begin
            elst.Add( esb.ToString ) ;
            esb.Length := 0 ;
          end ;
        T_saxElementState.AValue1 : // "
          begin
            elst.Add(
              Copy( esb.ToString, StringFirst + 1, esb.Length - 1 )
            ) ;
            //error
          end ;
        T_saxElementState.AValue2 : // '
          begin
            elst.Add(
              Copy( esb.ToString, StringFirst + 1, esb.Length - 1 )
            ) ;
            //error
          end ;
      end ;
    end ;

    procedure e_clear ;
    begin
      elst.Clear ;

      esb.Length := 0 ;
      esbq.Length := 0 ;

      es := T_saxElementState.EPrefix ;
    end ;

    procedure proc_outside ;
    begin
      s := T_saxState.Inside ;

      if stack.Count = 0 then begin
        saxSBClear( sb ) ;
        exit ;
      end ;

      tmp := Trim( sb.ToString ) ;

      if length( tmp ) > 0 then begin
        if bPreserveWhiteSpace then begin
          tmp := sb.ToString ;
          Characters( tmp ) ;
          _Content( tmp ) ;
        end
        else begin
          Characters( tmp ) ;
          _Content( tmp ) ;
        end;
      end ;

      saxSBClear( sb ) ;
    end ;

    procedure proc_procinst ;
    var
      ver : String ;
      enc : String ;
      stn : Boolean ;
      ii  : Integer ;
    begin
      s := T_saxState.Outside ;

      e_close ;

      slnm := elst.Strings[0] ;

      if CompareText( slnm, 'xml' ) = 0 then begin
        if stack.Count > 0 then
          throw_error( SAX_ERR_XML, SAX_ERR_CODE_XML ) ;

        ver := '1.0' ;
        enc := 'UTF-8' ;
        stn := True ;

        ii := 3 ;
        while ii < elst.Count - 1 do begin

          slnm := elst.Strings[ii] ;
          if CompareText( slnm, 'version' ) = 0 then
            ver := elst.Strings[ii+3]
          else
          if CompareText( slnm, 'encoding' ) = 0 then
            enc := elst.Strings[ii+3]
          else
          if CompareText( slnm, 'standalone' ) = 0 then begin
            if CompareText( elst.Strings[ii+3], 'yes' ) = 0 then
              stn := True
            else
              stn := False ;
          end ;

          inc( ii, 4 ) ;
        end ;

        if stn then
          _XMLDecl( ver, enc, 'yes' )
        else
          _XMLDecl( ver, enc, 'no'  ) ;
      end
      else begin
        tmp := elst.ToString ;
        svlu := Copy( tmp, StringFirst + 4, length( tmp ) - 4 ) ;
        ProcessingInstruction( slnm, svlu ) ;
      end ;

      e_clear ;
    end ;

    procedure proc_element ;
    var
      {$IFDEF DCC}
        nss : T_saxNamespaceMap ;
      {$ENDIF}
      {$IFDEF CLR}
        nss : List<T_saxNamespaceDef> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        nss : List<T_saxNamespaceDef> ;
      {$ENDIF}
      {$IFDEF JAVA}
        nss : ArrayList<T_saxNamespaceDef> ;
      {$ENDIF}
      ns : T_saxNamespaceDef ;
      ii : Integer ;
      kk : Integer ;
    begin
      s := T_saxState.Outside ;

      e_close ;

      atts := TGIS_SAXAttributes.Create ;
      try

        {$IFDEF CLR}
          nss := new List<T_saxNamespaceDef> ;
        {$ENDIF}
        {$IFDEF ISLAND}
          nss := new List<T_saxNamespaceDef> ;
        {$ENDIF}
        {$IFDEF JAVA}
          nss := new ArrayList<T_saxNamespaceDef> ;
        {$ENDIF}

        ii := 3 ;
        while ii < elst.Count - 1 do begin
          if CompareText( elst.Strings[ii+1], 'xmlns' ) = 0 then begin
            kk := length( nss ) ;
            {$IFDEF DCC}
              SetLength( nss, kk + 1 ) ;
            {$ENDIF}
            ns := T_saxNamespaceDef.Create ;
            if length( elst.Strings[ii+1] ) = 0 then begin
              ns.Element := elst.Strings[0] ;
              ns.Prefix  := '' ;
              ns.URI     := elst.Strings[ii+3] ;
              {$IFDEF DCC}
                nss[kk] := ns ;
              {$ENDIF}
              {$IFDEF OXYGENE}
                nss.Add( ns ) ;
              {$ENDIF}
            end
            else begin
              ns.Element := elst.Strings[0] ;
              ns.Prefix  := elst.Strings[ii+2] ;
              ns.URI     := elst.Strings[ii+3] ;
              {$IFDEF DCC}
                nss[kk] := ns ;
              {$ENDIF}
              {$IFDEF OXYGENE}
                nss.Add( ns ) ;
              {$ENDIF}
            end ;
          end ;

          suri := stack.ResolvePrefix( elst.Strings[ii+1] ) ;
          slnm := elst.Strings[ii+2] ;
          sqnm := elst.Strings[ii] ;
          svlu := elst.Strings[ii+3] ;

          atts.AddAttribute( suri, slnm, sqnm, 'CDATA', svlu ) ;

          inc( ii, 4 ) ;
        end ;

        suri := stack.ResolvePrefix( elst.Strings[1] ) ;
        slnm := elst.Strings[2] ;
        sqnm := elst.Strings[0] ;

        StartElement( suri, slnm, sqnm, atts ) ;

        if length( nss ) = 0 then
          stack.Push( elst.Strings[0] )
        else begin
          stack.Push( elst.Strings[0], nss ) ;
          for ii := 0 to length( nss ) - 1 do
            if nss[ii].Prefix <> '' then begin
              ns := nss[ii] ;
              StartPrefixMapping( ns.Prefix, ns.URI ) ;
            end ;
        end ;

      finally
        FreeObject( atts ) ;
      end ;

      e_clear ;
    end ;

    procedure proc_elementend ;
    begin
      s := T_saxState.Outside ;

      e_close ;

      if CompareText( elst.Strings[0], stack.LastIn ) = 0 then begin

        stack.Pop ;

        suri := stack.ResolvePrefix( elst.Strings[1] ) ;
        slnm := elst.Strings[2] ;
        sqnm := elst.Strings[0] ;

        EndElement( suri, slnm, sqnm ) ;

      end
      else begin
        throw_error( Format( SAX_ERR_ELEMENTEND,
                       [elst.Strings[0], stack.LastIn]
                     ),
                     SAX_ERR_CODE_ELEMENTEND
                   ) ;
        stack.ForcePop( elst.Strings[0] ) ;
      end ;

      e_clear ;
    end ;

    procedure proc_elementempty ;
    var
      ii : Integer ;
    begin
      s := T_saxState.Outside ;

      e_close ;

      atts := TGIS_SAXAttributes.Create ;
      try

        ii := 3 ;
        while ii < elst.Count - 1 do begin
          suri := stack.ResolvePrefix( elst.Strings[ii+1] ) ;
          slnm := elst.Strings[ii+2] ;
          sqnm := elst.Strings[ii] ;
          svlu := elst.Strings[ii+3] ;

          atts.AddAttribute( suri, slnm, sqnm, 'CDATA', svlu ) ;

          inc( ii, 4 ) ;
        end ;

        suri := stack.ResolvePrefix( elst.Strings[1] ) ;
        slnm := elst.Strings[2] ;
        sqnm := elst.Strings[0] ;

        StartElement( suri, slnm, sqnm, atts ) ;
        EndElement( suri, slnm, sqnm ) ;

      finally
        FreeObject( atts ) ;
      end ;

      e_clear ;
    end ;

    procedure proc_comment ;
    begin
      s := T_saxState.Outside ;

      tmp := sb.ToString ;

      svlu := Trim( Copy( tmp, StringFirst, length( tmp ) - 2 ) ) ;
      if length( svlu ) > 0 then
        _Comment( svlu ) ;

      saxSBClear( sb ) ;
    end ;

    procedure proc_cdata ;
    begin
      s := T_saxState.Outside ;

      tmp := sb.ToString ;

      svlu := Trim( Copy( tmp, StringFirst, length( tmp ) - 2 ) ) ;
      if length( svlu ) > 0 then begin
        Characters( svlu ) ;
        _CDATA( svlu ) ;
      end ;

      saxSBClear( sb ) ;
    end ;

    procedure proc_doctype ;
//    var
//      dtdRoot   : String ;
//      dtdKind   : String ;
//      dtdFPI    : String ;
//      dtdURI    : String ;
//      dtdSubset : String ;
    begin
      s := T_saxState.Outside ;

      if stack.Count > 0 then
        throw_error( SAX_ERR_DOCTYPE, SAX_ERR_CODE_DOCTYPE ) ;

      tmp := Trim( sb.ToString ) ;
      _DocType( tmp ) ;
      saxSBClear( sb ) ;

      exit ;
      //? left here if we ever need to parse DOCTYPE

//      e_close ;
//
//      if elst.Count > 0 then
//        dtdRoot := elst.Strings[0] ;
//
//      if elst.Count > 3 then begin
//        slnm := elst.Strings[3] ;
//
//        if length( slnm ) = 0 then
//          dtdSubset := elst.Strings[6]
//        else
//        if CompareText( slnm, 'public' ) = 0 then
//          dtdKind := 'public'
//        else
//        if CompareText( slnm, 'system' ) = 0 then
//          dtdKind := 'system'
//        else
//          dtdFPI := elst.Strings[3] ;
//      end ;
//
//      if elst.Count > 7 then begin
//        slnm := elst.Strings[7] ;
//
//        if length( slnm ) = 0 then
//          dtdSubset := elst.Strings[10]
//        else begin
//          if CompareText( dtdKind, 'public' ) = 0 then
//            dtdFPI := elst.Strings[7]
//          else
//            dtdURI := elst.Strings[7] ;
//        end ;
//      end ;
//
//      if elst.Count > 11 then begin
//        slnm := elst.Strings[11] ;
//
//        if length( slnm ) = 0 then
//          dtdSubset := elst.Strings[14]
//        else begin
//          if CompareText( dtdKind, 'public' ) = 0 then
//            dtdURI := elst.Strings[11] ;
//        end ;
//      end ;
//
//      if elst.Count > 15 then begin
//        slnm := elst.Strings[15] ;
//
//        if length( slnm ) = 0 then
//          dtdSubset := elst.Strings[18] ;
//      end ;
//
//      e_clear ;
    end ;

    function proc_cer : String ;
    var
      rr : String ;
      tt : String ;
    begin
      tmp := sbcer.ToString ;
      sbcer.Length := 0 ;

      if CompareText( tmp, 'quot' ) = 0 then
        rr := '"'
      else
      if CompareText( tmp, 'amp' ) = 0 then
        rr := '&'
      else
      if CompareText( tmp, 'apos' ) = 0 then
        rr := ''''
      else
      if CompareText( tmp, 'lt' ) = 0 then
        rr := '<'
      else
      if CompareText( tmp, 'gt' ) = 0 then
        rr := '>'
      else begin

        if tmp[StringFirst] = '#' then begin

          tt := Copy( tmp, StringFirst + 1, length( tmp ) - 1 ) ;

          if TryStrToInt( tt, cer ) then
            rr := Char( cer )
          else begin
            throw_error( SAX_ERR_CER, SAX_ERR_CODE_CER ) ;
            rr := '$' + tmp + ';' ;
          end ;

        end
        else
          rr := '$' + tmp + ';' ;
      end ;

      Result := rr ;
    end ;

  begin
    bBreak := False ;

    {$IFDEF DCC}
      sb := TXMLStringBuilder.Create ;
      sb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF CLR}
      sb := TXMLStringBuilder.Create ;
      sb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF ISLAND}
      sb := TXMLStringBuilder.Create ;
      sb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF JAVA}
      sb := TXMLStringBuilder.Create( 255 ) ;
    {$ENDIF}

    {$IFDEF DCC}
      sbcer := TXMLStringBuilder.Create ;
      sbcer.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF CLR}
      sbcer := TXMLStringBuilder.Create ;
      sbcer.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF ISLAND}
      sbcer := TXMLStringBuilder.Create ;
      sbcer.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF JAVA}
      sbcer := TXMLStringBuilder.Create( 255 ) ;
    {$ENDIF}

    stack := T_saxStack.Create ;

    elst := T_saxList.Create( 50 ) ;

    {$IFDEF DCC}
      esb := TXMLStringBuilder.Create ;
      esb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF CLR}
      esb := TXMLStringBuilder.Create ;
      esb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF ISLAND}
      esb := TXMLStringBuilder.Create ;
      esb.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF JAVA}
      esb := TXMLStringBuilder.Create( 255 ) ;
    {$ENDIF}

    {$IFDEF DCC}
      esbq := TXMLStringBuilder.Create ;
      esbq.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF CLR}
      esbq := TXMLStringBuilder.Create ;
      esbq.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF ISLAND}
      esbq := TXMLStringBuilder.Create ;
      esbq.Capacity := 255 ;
    {$ENDIF}
    {$IFDEF JAVA}
      esbq := TXMLStringBuilder.Create( 255 ) ;
    {$ENDIF}

    try

      stack.oSAX := Self ;

      StartDocument ;

      c := #0 ;
      s := T_saxState.Outside ;
      sl := T_saxState.Outside ;
      scer := T_saxState.Outside ;

      eadd := False ;
      es := T_saxElementState.EPrefix  ;

      iColumn := 1 ;
      iLine   := 1 ;
      blb     := False ;

      buf := '' ;
      bufend := 0 ;
      bufpos := 1 ;
      while True do begin

        if bufpos > bufend then begin
          if oReader.Eof then
            break ;
          if bReadLineOnly then
            buf := oReader.ReadXMLHeader
          else
            buf := oReader.ReadBuffer ;
          bufend := StringLast( buf ) ;
          if bufend < StringFirst then
            break ;
          bufpos := StringFirst ;
        end ;

        c := buf[bufpos] ;
        inc( bufpos ) ;

        case c of
          #10 :
            begin
              if not blb then begin
                iColumn := 1 ;
                inc( iLine ) ;
              end ;
              blb := False ;
            end ;
          #13 :
            begin
              iColumn := 1 ;
              inc( iLine ) ;
              blb := True ;
            end ;
          else
            begin
              inc( iColumn ) ;
              blb := False ;
            end ;
        end ;

        case s of
          T_saxState.Outside :
            begin
              case c of
                '<' : proc_outside ;
                '&' :
                  begin
                    scer := T_saxState.Outside ;
                    s := T_saxState.CharEntRef ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
          T_saxState.CharEntRef :
            begin
              case c of
                ';' :
                  begin
                    tmp := proc_cer ;
                    sb.Append( tmp ) ;
                    s := scer ;
                  end ;
                else
                  sbcer.Append( c ) ;
              end ;
            end ;
          T_saxState.Inside :
            begin
              case c of
                '/' : s := T_saxState.ElementEnd ;
                '?' :
                  begin
                    s := T_saxState.ProcInstName ;
                    sl := T_saxState.ProcInst ;
                  end ;
                '!' : s := T_saxState.Exclamation ;
                else
                  begin
                    if isWhiteSpace( c ) then begin
                      throw_fatalerror( SAX_ERR_GENERIC,
                        SAX_ERR_CODE_GENERIC ) ;
                      exit ;
                    end
                    else begin
                      s := T_saxState.ElementName ;
                      sl := T_saxState.Element ;
                      eadd := True ;
                    end ;
                  end ;
              end ;
            end ;
          T_saxState.ProcInstName :
            begin
              case c of
                '?' : s := T_saxState.ProcInstEnd ;
                else
                  begin
                    s := T_saxState.ProcInst ;
                    eadd := True ;
                  end ;
              end ;
            end ;
          T_saxState.ProcInst :
            begin
              case c of
                '?' : s := T_saxState.ProcInstEnd ;
                '"' :
                  begin
                    sl := s ;
                    s := T_saxState.Quote1 ;
                    eadd := True ;
                  end ;
                '''' :
                  begin
                    sl := s ;
                    s := T_saxState.Quote2 ;
                    eadd := True ;
                  end ;
                else
                  eadd := True ;
              end ;
            end ;
          T_saxState.ProcInstEnd :
            begin
              case c of
                '>' : proc_procinst ;
                else
                  begin
                    throw_fatalerror( SAX_ERR_GENERIC,
                      SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.ElementName :
            begin
              case c of
                '/' : s := T_saxState.ElementEmptyEnd ;
                '>' : proc_element ;
                else
                  begin
                    s := T_saxState.Element ;
                    eadd := True ;
                  end ;
              end ;
            end ;
          T_saxState.Element :
            begin
              case c of
                '/' : s := T_saxState.ElementEmptyEnd ;
                '>' : proc_element ;
                '"' :
                  begin
                    sl := s ;
                    s := T_saxState.Quote1 ;
                    eadd := True ;
                  end ;
                '''' :
                  begin
                    sl := s ;
                    s := T_saxState.Quote2 ;
                    eadd := True ;
                  end ;
                else
                  eadd := True ;
              end ;
            end ;
          T_saxState.ElementEnd :
            begin
              case c of
                '>' : proc_elementend ;
                else
                  eadd := True ;
              end ;
            end ;
          T_saxState.ElementEmptyEnd :
            begin
              case c of
                '>' : proc_elementempty ;
                else
                  begin
                    throw_fatalerror( SAX_ERR_GENERIC,
                      SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.Quote1 :
            begin
              case c of
                '"' : s := sl ;
              end ;
              eadd := True ;
            end ;
          T_saxState.Quote2 :
            begin
              case c of
                '''' : s := sl ;
              end ;
              eadd := True ;
            end ;
          T_saxState.Exclamation :
            begin
              case c of
                '-' : s := T_saxState.Comment1 ;
                '[' : s := T_saxState.CDATA1 ;
                'D' : s := T_saxState.DOCTYPE1 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.Comment1 :
            begin
              case c of
                '-' : s := T_saxState.Comment2 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.Comment2 :
            begin
              case c of
                '-' :
                  begin
                    s := T_saxState.CommentEnd1 ;
                    sb.Append( c ) ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
          T_saxState.CommentEnd1 :
            begin
              case c of
                '-' :
                  begin
                    s := T_saxState.CommentEnd2 ;
                    sb.Append( c ) ;
                  end ;
                else
                  begin
                    s := T_saxState.Comment2 ;
                    sb.Append( c ) ;
                  end ;
              end ;
            end ;
          T_saxState.CommentEnd2 :
            begin
              case c of
                '>' : proc_comment ;
                else
                  begin
                    s := T_saxState.Comment2 ;
                    sb.Append( c ) ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA1 :
            begin
              case c of
                'C' : s := T_saxState.CDATA2 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA2 :
            begin
              case c of
                'D' : s := T_saxState.CDATA3 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA3 :
            begin
              case c of
                'A' : s := T_saxState.CDATA4 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA4 :
            begin
              case c of
                'T' : s := T_saxState.CDATA5 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA5 :
            begin
              case c of
                'A' : s := T_saxState.CDATA6 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA6          :
            begin
              case c of
                '[' : s := T_saxState.CDATA7 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.CDATA7 :
            begin
              case c of
                ']' :
                  begin
                    s := T_saxState.CDATAEnd1 ;
                    sb.Append( c ) ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
          T_saxState.CDATAEnd1 :
            begin
              case c of
                ']' :
                  begin
                    s := T_saxState.CDATAEnd2 ;
                    sb.Append( c ) ;
                  end ;
                else
                  begin
                    s := T_saxState.CDATA7 ;
                    sb.Append( c ) ;
                  end ;
              end ;
            end ;
          T_saxState.CDATAEnd2 :
            begin
              case c of
                '>' : proc_cdata ;
                ']' : sb.Append( c ) ;
                else
                  begin
                    s := T_saxState.CDATA7 ;
                    sb.Append( c ) ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE1 :
            begin
              case c of
                'O' : s := T_saxState.DOCTYPE2 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE2 :
            begin
              case c of
                'C' : s := T_saxState.DOCTYPE3 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE3 :
            begin
              case c of
                'T' : s := T_saxState.DOCTYPE4 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE4 :
            begin
              case c of
                'Y' : s := T_saxState.DOCTYPE5 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE5 :
            begin
              case c of
                'P' : s := T_saxState.DOCTYPE6 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE6 :
            begin
              case c of
                'E' : s := T_saxState.DOCTYPE7 ;
                else
                  begin
                    throw_fatalerror(
                      SAX_ERR_GENERIC, SAX_ERR_CODE_GENERIC ) ;
                    exit ;
                  end ;
              end ;
            end ;
          T_saxState.DOCTYPE7 :
            begin
              case c of
                '[' :
                  begin
                    sb.Append( c ) ;
                    s := T_saxState.DOCTYPE8 ;
                  end ;
                '>' : proc_doctype ;
              // left here if we ever need to parse DOCTYPE
              //  '"' :
              //    begin
              //      sl := s ;
              //      s := T_saxState.Quote1 ;
              //      sb.Append( c ) ;
              //    end ;
              //  '''' :
              //    begin
              //      sl := s ;
              //      s := T_saxState.Quote2 ;
              //      sb.Append( c ) ;
              //    end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
          T_saxState.DOCTYPE8 :
            begin
              case c of
                ']' :
                  begin
                    sb.Append( c ) ;
                    s := T_saxState.DOCTYPE7 ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
        end ;

        if eadd then begin

          case es of
            T_saxElementState.Whitespace :
              begin
                if not isWhiteSpace( c ) then begin
                  case c of
                    '[' : es := T_saxElementState.DTD ;
                    else
                      begin
                        es := T_saxElementState.APrefix ;
                        esb.Append( c ) ;
                      end ;
                  end ;
                end ;
              end ;
            T_saxElementState.EPrefix :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                end
                else begin
                  case c of
                    ':' :
                      begin
                        es := T_saxElementState.EName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.EName :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  e_restore ;
                end
                else
                  esb.Append( c ) ;
              end ;
            T_saxElementState.APrefix :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    'x',
                    'X' :
                      begin
                        esb.Append( c ) ;
                        es := T_saxElementState.Axmlns1 ;
                      end ;
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                        e_restore ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.Axmlns1 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    'm',
                    'M' :
                      begin
                        esb.Append( c ) ;
                        es := T_saxElementState.Axmlns2 ;
                      end ;
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                        e_restore ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.Axmlns2 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    'l',
                    'L' :
                      begin
                        esb.Append( c ) ;
                        es := T_saxElementState.Axmlns3 ;
                      end ;
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                        e_restore ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.Axmlns3 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    'n',
                    'N' :
                      begin
                        esb.Append( c ) ;
                        es := T_saxElementState.Axmlns4 ;
                      end ;
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                        e_restore ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.Axmlns4 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  epref := esb.ToString ;
                  esb.Length := 0 ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    's',
                    'S' :
                      begin
                        esb.Append( c ) ;
                        es := T_saxElementState.Axmlns4 ;
                      end ;
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                        e_restore ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.Axmlns5 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                end
                else begin
                  case c of
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        elst.Add( esb.ToString ) ;
                        esb.Length := 0 ;
                        elst.Add( elst.Strings[elst.Count-1] ) ;
                        elst.Add( '' ) ;
                      end ;
                    ':' :
                      begin
                        es := T_saxElementState.AName ;
                        epref := esb.ToString ;
                        esb.Length := 0 ;
                      end ;
                    else
                      begin
                        es := T_saxElementState.APrefix ;
                        esb.Append( c ) ;
                      end ;
                  end ;
                end ;
              end ;
            T_saxElementState.AName :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  e_restore ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    '=' :
                      begin
                        es := T_saxElementState.AValue ;
                        e_restore ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.AValue :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  elst.Add( '' ) ;
                end
                else begin
                  case c of
                    '"'  :
                      es := T_saxElementState.AValue1 ;
                    '''' :
                      es := T_saxElementState.AValue2 ;
                    else
                      begin
                        es := T_saxElementState.AValue0 ;
                        esb.Append( c ) ;
                      end ;
                  end ;
                end ;
              end ;
            T_saxElementState.AValue0 :
              begin
                if isWhiteSpace( c ) then begin
                  es := T_saxElementState.Whitespace ;
                  elst.Add( esb.ToString ) ;
                  esb.Length := 0 ;
                end
                else begin
                  case c of
                    '&' :
                      begin
                        escer := es ;
                        es := T_saxElementState.ACharEntRef ;
                      end ;
                    else
                      esb.Append( c ) ;
                  end ;
                end ;
              end ;
            T_saxElementState.AValue1 : // "
              begin
                case c of
                  '"' :
                    begin
                      es := T_saxElementState.RequireWhitespace ;
                      elst.Add( esb.ToString ) ;
                      esb.Length := 0 ;
                    end ;
                  '&' :
                    begin
                      escer := es ;
                      es := T_saxElementState.ACharEntRef ;
                    end ;
                  else
                    esb.Append( c ) ;
                end ;
              end ;
            T_saxElementState.AValue2 : // '
              begin
                case c of
                  '''' :
                    begin
                      es := T_saxElementState.RequireWhitespace ;
                      elst.Add( esb.ToString ) ;
                      esb.Length := 0 ;
                    end ;
                  '&' :
                    begin
                      escer := es ;
                      es := T_saxElementState.ACharEntRef ;
                    end ;
                  else
                    esb.Append( c ) ;
                end ;
              end ;
            T_saxElementState.RequireWhitespace :
              begin
                if isWhiteSpace( c ) then
                  es := T_saxElementState.Whitespace
                else
                  //error
                  esb.Append( c ) ;
              end ;
            T_saxElementState.ACharEntRef :
              begin
                case c of
                  ';' :
                    begin
                      tmp := proc_cer ;
                      esb.Append( tmp ) ;
                      es := escer ;
                    end ;
                  else
                    sbcer.Append( c ) ;
                end ;
              end ;
            T_saxElementState.DTD :
              begin
                case c of
                  ']' :
                    begin
                      es := T_saxElementState.RequireWhitespace ;
                      elst.Add( '' ) ;
                      elst.Add( '' ) ;
                      elst.Add( '' ) ;
                      elst.Add( esb.ToString ) ;
                      esb.Length := 0 ;
                    end ;
                  else
                    esb.Append( c ) ;
                end ;
              end ;
          end ;

          eadd := False ;

        end ;

        if bBreak then
          exit ;

      end ;

      EndDocument ;

    finally
      FreeObject( sb    ) ;
      FreeObject( sbcer ) ;
      FreeObject( stack ) ;

      FreeObject( elst   ) ;
      FreeObject( esb    ) ;
      FreeObject( esbq   ) ;
    end ;
  end ;

  function TGIS_SAXContentHandler.RemoveWhitesFast(
    const _str : String
  ) : String ;
  var
    i, j : Integer ;
    {$IFDEF OXYGENE}
      chars : array of Char ;
    {$ENDIF}
    c : Char ;
  begin
    {$IFDEF OXYGENE}
      chars := new array of Char( length( _str ) ) ;
      j := 0 ;
      for i := StringFirst to StringLast( _str ) do begin
        c := _str[i] ;
        if ord( c ) >= 32 then begin
          chars[ j ] := c ;
          inc( j ) ;
        end
        else begin
          // in case to use tabs instead of spaces in coordinates
          if ( ord( c ) = 9 ) or ( ord( c ) = $A ) then begin
            chars[ j ] := ' ' ;
            inc( j ) ;
          end ;
        end ;
      end ;
      {$IFDEF ISLAND}
      Result := String.FromCharArray( chars ) ;
      {$ELSE}
      Result := new String( chars ) ;
      {$ENDIF}
    {$ELSE}
      SetLength( Result, Length( _str ) ) ;
      j := 0 ;
      for i := StringFirst to StringLast( _str ) do begin
        c := _str[i] ;
        if ( Ord( c ) >= 32 ) then begin
          inc( j ) ;
          Result[j] := c ;
        end
        else begin
          // in case to use tabs instead of spaces in coordinates
          if ( Ord( c ) = 9 ) or ( Ord( c ) = $A ) then begin
            inc( j ) ;
            Result[j] := ' ' ;
          end ;
        end ;
      end ;
      SetLength( Result, j ) ;
    {$ENDIF}
  end ;

  function TGIS_SAXContentHandler.EscapeText(
    const _str : String
  ) : String;
  var
    isrc : Integer ;
    idst : Integer ;
    sb   : TXMLStringBuilder ;
  const
    ESC_AMP  = '&amp;'  ;
    ESC_LT   = '&lt;'   ;
    ESC_GT   = '&gt;'   ;
    ESC_APOS = '&apos;' ;
    ESC_QUOT = '&quot;' ;

    procedure _concat( const _txt : String ) ;
    var
      isrc1 : Integer ;
    begin
      {$IFDEF JAVA}
        sb.setLength( sb.Length + Length( _txt ) - 1 ) ;
      {$ELSE}
        sb.Length := sb.Length + length( _txt ) - 1 ;
      {$ENDIF}
      for isrc1 := StringFirst to StringLast( _txt ) do begin
        sb[idst] := _txt[isrc1] ;
        inc( idst ) ;
      end;
    end;
  begin
    sb := TXMLStringBuilder.Create ;
    try
      {$IFDEF JAVA}
        sb.setLength( Length( _str ) ) ;
      {$ELSE}
        sb.Length := length( _str ) ;
      {$ENDIF}

      idst := 0 ;
      for isrc := StringFirst to StringLast( _str ) do begin
        case _str[ isrc ] of
          '&'  : _concat( ESC_AMP  ) ;
          '<'  : _concat( ESC_LT   ) ;
          '>'  : _concat( ESC_GT   ) ;
          '''' : _concat( ESC_APOS ) ;
          '"'  : _concat( ESC_QUOT ) ;
          else
            begin
              sb[idst] := _str[isrc] ;
              inc( idst ) ;
            end ;
        end ;
      end ;

    finally
      Result := sb.ToString ;
      FreeObject( sb ) ;
    end ;
  end ;

  function TGIS_SAXContentHandler.fget_Locator : IVBSAXLocator ;
  begin
    Result := FLocator ;
  end ;

  procedure TGIS_SAXContentHandler.StartDocument ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.EndDocument ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.Characters(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.IgnorableWhitespace(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.StartPrefixMapping(
    const _prefix  : String ;
    const _uri     : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.EndPrefixMapping(
    const _prefix  : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.ProcessingInstruction(
    const _target  : String ;
    const _data    : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.SkippedEntity(
    const _name    : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.PutDocumentLocator(
    const _locator : IVBSAXLocator
  ) ;
  begin
    // DO NOTHING - no need to replace the locator
  end ;

  procedure TGIS_SAXContentHandler.Error(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.FatalError(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    raise EGIS_Exception.Create( _message, sPath, FLocator.LineNumber ) ;

    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler.IgnorableWarning(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler._XMLDecl(
    const _ver   : String ;
    const _enc   : String ;
    const _stndl : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler._DocType(
    const _data  : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler._Content(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler._Comment(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

  procedure TGIS_SAXContentHandler._CDATA(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - to be handled in a descendant class
  end ;

//==============================================================================
// TGIS_SAXWriter
//==============================================================================

  constructor TGIS_SAXWriter.Create(
    const _strm : TXMLOutputStream
  ) ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      _strm.Seek( 0, TSeekOrigin.soBeginning ) ;
    {$ENDIF}
    {$IFDEF CLR}
      _strm.Seek( 0, SeekOrigin.Begin ) ;
    {$ENDIF}

    {$IFDEF JAVA}
      oWriter := TXMLStreamWriter.Create( _strm, 'UTF-8' ) ;
    {$ENDIF}
    {$IFDEF DCC}
      oEnc    := T_UTF8NoBOMEncoding.Create ;
      oWriter := TXMLStreamWriter.Create( _strm, oEnc ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oWriter := TXMLStreamWriter.Create( _strm, new UTF8Encoding(false) ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oWriter := TXMLStreamWriter.Create( _strm, TEncoding.UTF8 ) ;
    {$ENDIF}

    oBuilder   := TXMLStringBuilder.Create ;
    iIndent    := 0 ;
    FIndent    := True ;
    FIndentStr := '  ' ;

    bEmpty   := False ;
    bInCDATA := False ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_SAXWriter.Destroy ;
    begin
      FreeObject( oBuilder ) ;
      FreeObject( oWriter  ) ;
      FreeObject( oEnc     ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_SAXWriter.closeElement(
    const _empty : Boolean
  ) ;
  begin
    if _empty then begin
      oBuilder.Append( '/>' ) ;
      appendCRLF ;
      oWriter.Write( oBuilder.ToString ) ;
      saxSBClear( oBuilder ) ;
    end
    else begin
      oBuilder.Append( '>' ) ;
      appendCRLF ;
      oWriter.Write( oBuilder.ToString ) ;
      saxSBClear( oBuilder ) ;
    end ;
  end ;

  procedure TGIS_SAXWriter.closeNotEmpty ;
  begin
    if bEmpty then begin
      closeElement( False ) ;
      bEmpty := False ;
    end ;
  end ;

  procedure TGIS_SAXWriter.incIndent ;
  begin
    if not bInCDATA then
      inc( iIndent ) ;
  end ;

  procedure TGIS_SAXWriter.decIndent ;
  begin
    if not bInCDATA then
      dec( iIndent ) ;
  end ;

  procedure TGIS_SAXWriter.appendIndent ;
  var
    i : Integer ;
  begin
    if bInCDATA then
      exit ;

    if SaveCompact then
      exit ;

    for i := 1 to iIndent do
      oBuilder.Append( IndentString ) ;
  end ;

  procedure TGIS_SAXWriter.appendCRLF ;
  begin
    if bInCDATA then
      exit ;

    if SaveCompact then
      exit ;

    oBuilder.Append( #13#10 ) ;
  end ;

  procedure TGIS_SAXWriter.appendChars(
    const _chars : String  ;
    const _bspc  : Boolean
  ) ;
  var
    i : Integer ;
    k : Integer ;
    l : Integer ;

    procedure catch_up ;
    begin
      oBuilder.Append( Copy( _chars, k, l - 1 ) ) ;
      k := i+1 ;
      l := 0 ;
    end ;

    procedure test_spec_char ;
    begin
      case _chars[i] of
        '<'  :
          begin
            catch_up ;
            oBuilder.Append( '&lt;' ) ;
          end ;
        '&'  :
          begin
            catch_up ;
            oBuilder.Append( '&amp;' ) ;
          end ;
        '>'  :
          begin
            catch_up ;
            oBuilder.Append( '&gt;' ) ;
          end ;
        '"'  :
          begin
            catch_up ;
            oBuilder.Append( '&quot;' ) ;
          end ;
        '''' :
          begin
            catch_up ;
            oBuilder.Append( '&apos;' ) ;
          end ;
      end ;
    end ;

  begin
    if _bspc then begin
      k := StringFirst ;
      l := 0 ;
      for i := StringFirst to StringLast( _chars ) do begin
        inc( l ) ;
        test_spec_char ;
      end ;
      oBuilder.Append( Copy( _chars, k, l ) ) ;
    end
    else
      oBuilder.Append( _chars ) ;
  end ;

  function TGIS_SAXWriter.putCharEntRef(
    const _str   : String
  ) : String ;
  var
    sb : TXMLStringBuilder ;
    i  : Integer ;
    c  : Char ;
  begin
    sb := TXMLStringBuilder.Create ;
    try
      for i := StringFirst to StringLast( _str ) do begin
        c := _str[i] ;
        case c of
          '<' : sb.Append( '&lt;'   ) ;
          '&' : sb.Append( '&amp;'  ) ;
          '>' : sb.Append( '&gt;'   ) ;
          '"' : sb.Append( '&quot;' ) ;
         '''' : sb.Append( '&apos;' ) ;
         else   sb.Append( c        ) ;
        end ;
      end ;

      Result := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  procedure TGIS_SAXWriter.StartDocument ;
  begin
    // DO NOTHING
  end ;

  procedure TGIS_SAXWriter.EndDocument ;
  begin
    // DO NOTHING
    oWriter.Flush ;
    {$IFDEF JAVA}
       oWriter.close ;
    {$ENDIF}
  end ;

  procedure TGIS_SAXWriter.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i : Integer ;
  begin
    closeNotEmpty ;

    bEmpty := True ;
    bTextNode := False ;
    sQName := _qname ;

    appendIndent ;

    oBuilder.Append( '<' ) ;
    oBuilder.Append( _qname ) ;
    if assigned( _attribs ) then begin
      for i := 0 to _attribs.Length - 1 do begin
        oBuilder.Append( ' ' ) ;
        oBuilder.Append( _attribs.GetQName( i ) ) ;
        oBuilder.Append( '="' ) ;
        oBuilder.Append( putCharEntRef( _attribs.GetValue( i ) ) ) ;
        oBuilder.Append( '"' ) ;
      end ;
    end ;

    oWriter.Write( oBuilder.ToString ) ;

    incIndent ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  begin
    decIndent ;

    if bEmpty and ( CompareText( sQName, _qname ) = 0 ) then begin
      closeElement( True ) ;
      bEmpty := False ;
      exit ;
    end ;

    if not bTextNode then
      appendIndent ;

    bEmpty := False ;
    bTextNode := False ;

    oBuilder.Append( '</' ) ;
    oBuilder.Append( _qname ) ;
    oBuilder.Append( '>' ) ;
    appendCRLF ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter.Characters(
    const _chars   : String
  ) ;
  begin
    bTextNode := True ;

    if bEmpty then begin
      oBuilder.Append( '>' ) ;
      bEmpty := False ;
    end ;

    appendChars( _chars, not bInCDATA ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter.ProcessingInstruction(
    const _target  : String ;
    const _data    : String
  ) ;
  begin
    closeNotEmpty ;

    appendIndent ;

    oBuilder.Append( '<?' ) ;
    oBuilder.Append( _target ) ;
    oBuilder.Append( ' ' ) ;
    oBuilder.Append( _data ) ;
    oBuilder.Append( '?>' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter.StartCDATA ;
  begin
    closeNotEmpty ;

    appendIndent ;

    oBuilder.Append( '<![CDATA[ ' ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;

    bInCDATA := True ;
  end ;

  procedure TGIS_SAXWriter.EndCDATA ;
  begin
    oBuilder.Append( ' ]]>' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;

    bInCDATA := False ;
  end ;

  procedure TGIS_SAXWriter._XMLDecl(
    const _ver   : String ;
    const _enc   : String ;
    const _stndl : String
  ) ;
  begin
    oBuilder.Append( '<?xml ' ) ;

    oBuilder.Append( 'version="' ) ;
    if length( _ver ) > 0 then
      oBuilder.Append( _ver )
    else
      oBuilder.Append( '1.0' ) ;

    oBuilder.Append( '" encoding="' ) ;
    if length( _enc ) > 0 then
      oBuilder.Append( _enc )
    else
      oBuilder.Append( 'UTF-8' ) ;

    if length( _stndl ) > 0 then begin
      oBuilder.Append( '" standalone="' ) ;
      oBuilder.Append( _stndl ) ;
    end ;

    oBuilder.Append( '"?>' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter._DocType(
    const _data  : String
  ) ;
  begin
    oBuilder.Append( '<!DOCTYPE ' ) ;
    appendChars( _data, False ) ;
    oBuilder.Append( '>' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter._Content(
    const _chars   : String
  ) ;
  begin
    Characters( _chars ) ;
  end ;

  procedure TGIS_SAXWriter._Comment(
    const _chars   : String
  ) ;
  begin
    closeNotEmpty ;

    appendIndent ;

    oBuilder.Append( '<!-- ' ) ;
    appendChars( _chars, False ) ;
    oBuilder.Append( ' -->' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

  procedure TGIS_SAXWriter._CDATA(
    const _chars   : String
  ) ;
  begin
    closeNotEmpty ;

    appendIndent ;

    oBuilder.Append( '<![CDATA[ ' ) ;
    oBuilder.Append( _chars ) ;
    oBuilder.Append( ' ]]>' ) ;
    oBuilder.Append( #13#10 ) ;

    oWriter.Write( oBuilder.ToString ) ;

    saxSBClear( oBuilder ) ;
  end ;

//=================================== END ======================================
end.

