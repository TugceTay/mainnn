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

{$IFDEF DCC}
  unit GisXmlDoc ;
  {$HPPEMIT '#pragma link "GisXmlDoc"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Generics.Collections,
    System.Variants,

    GisRtl,
    GisXmlSax ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    java.io,
    java.text,
    java.nio.charset,
    tatukgis.rtl,
    tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Enum for node type
  /// </summary>
  TNodeType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Reserved node type.
    /// </summary>
    ntReserved,
    /// <summary>
    ///   Element node type.
    /// </summary>
    ntElement,
    /// <summary>
    ///   Attribute node type.
    /// </summary>
    ntAttribute,
    /// <summary>
    ///   Text node type.
    /// </summary>
    ntText,
    /// <summary>
    ///   CData node type.
    /// </summary>
    ntCData,
    /// <summary>
    ///   EntityRef node type.
    /// </summary>
    ntEntityRef,
    /// <summary>
    ///   Entity node type.
    /// </summary>
    ntEntity,
    /// <summary>
    ///   ProcessingInstr node type.
    /// </summary>
    ntProcessingInstr,
    /// <summary>
    ///   Comment node type.
    /// </summary>
    ntComment,
    /// <summary>
    ///   Document node type.
    /// </summary>
    ntDocument,
    /// <summary>
    ///   DocType node type.
    /// </summary>
    ntDocType,
    /// <summary>
    ///   DocFragment node type.
    /// </summary>
    ntDocFragment,
    /// <summary>
    ///   Notation node type.
    /// </summary>
    ntNotation
  ) ;

  IXMLNode = {$IFDEF OXYGENE} public {$ENDIF} class ;
  IXMLNodeList = {$IFDEF OXYGENE} public {$ENDIF} class ;
  IXMLNodeCollection = {$IFDEF OXYGENE} public {$ENDIF} class ;
  IXMLDocument = {$IFDEF OXYGENE} public {$ENDIF} class ;

  /// <summary>
  ///   XML Error
  /// </summary>
  EXMLDocError = {$IFDEF OXYGENE} public {$ENDIF} class( Exception )
  end ;

  /// <summary>
  ///   XML Node
  /// </summary>
  IXMLNode = {$IFDEF OXYGENE} public abstract {$ENDIF}
             class
    protected
      function  fget_Attribute      ( const _name  : String
                                    ) : OleVariant ; virtual; abstract;
      procedure fset_Attribute      ( const _name  : String ;
                                      const _value : OleVariant
                                    ) ; virtual; abstract;
      function  fget_AttributeNodes : IXMLNodeList ; virtual; abstract;
      function  fget_ChildNodes     : IXMLNodeList ; virtual; abstract;
      function  fget_ChildValue     ( const _iname : OleVariant
                                    ) : OleVariant ; virtual; abstract;
      procedure fset_ChildValue     ( const _iname : OleVariant ;
                                      const _value : OleVariant
                                    ) ; virtual; abstract;
      function  fget_Collection     : IXMLNodeCollection ; virtual; abstract;
      function  fget_HasChildNodes  : Boolean ; virtual; abstract;
      function  fget_IsTextElement  : Boolean ; virtual; abstract;
      function  fget_LocalName      : String ; virtual; abstract;
      function  fget_NamespaceURI   : String ; virtual; abstract;
      function  fget_NodeName       : String ; virtual; abstract;
      function  fget_NodeType       : TNodeType ; virtual; abstract;
      function  fget_NodeValue      : OleVariant ; virtual; abstract;
      procedure fset_NodeValue      ( const _value : OleVariant
                                    ) ; virtual; abstract;
      function  fget_OwnerDocument  : IXMLDocument ; virtual; abstract;
      function  fget_ParentNode     : IXMLNode ; virtual; abstract;
      function  fget_Prefix         : String ; virtual; abstract;
      function  fget_ReadOnly       : Boolean ; virtual; abstract;
      procedure fset_ReadOnly       ( const _value : Boolean
                                    ) ; virtual; abstract;
      function  fget_Text           : String ; virtual; abstract;
      procedure fset_Text           ( const _value : String
                                    ) ; virtual; abstract;
      function  fget_XML            : String ; virtual; abstract;
    public
      /// <summary>
      ///   Adds a new child node to the node.
      /// </summary>
      /// <param name="_name">
      ///   name for created node
      /// </param>
      /// <param name="_index">
      ///   points to the position of the child node
      /// </param>
      /// <returns>
      ///   returns the interface for the newly created child node
      /// </returns>
      function  AddChild            ( const _name  : String ;
                                            _index : Integer = -1
                                    ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Adds a new child node to the node.
      /// </summary>
      /// <param name="_name">
      ///   name for created node
      /// </param>
      /// <param name="_uri">
      ///   URI for new node's definition
      /// </param>
      /// <param name="_prfx">
      ///   true for generating prefix for URI
      /// </param>
      /// <param name="_index">
      ///   points to the position of the child node
      /// </param>
      /// <returns>
      ///   returns the interface for the newly created child node
      /// </returns>
      function  AddChild            ( const _name  : String ;
                                      const _uri   : String ;
                                            _prfx  : Boolean = False ;
                                            _index : Integer = -1
                                    ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Creates a copy of this node.
      /// </summary>
      /// <param name="_deep">
      ///   true for copying also children of node
      /// </param>
      /// <returns>
      ///  returns interface of copied node
      /// </returns>
      function  CloneNode           (       _deep  : Boolean
                                    ) : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Adds an attribute to the node that declares a specified namespace URI.
      /// </summary>
      /// <param name="_prfx">
      ///   URI prefix
      /// </param>
      /// <param name="_uri">
      ///   corresponding URI
      /// </param>
      procedure DeclareNamespace    ( const _prfx  : String ;
                                      const _uri   : String
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Returns the namespace URI.
      /// </summary>
      /// <param name="_prfx">
      ///   URI prefix or a tag name
      /// </param>
      /// <returns>
      ///   namespace URI
      /// </returns>
      function  FindNamespaceURI    ( const _prfx  : String
                                    ) : String ; virtual; abstract;
      /// <summary>
      ///   Returns the attribute node.
      /// </summary>
      /// <param name="_uri">
      ///   namespace URI
      /// </param>
      /// <returns>
      ///   attribute node
      /// </returns>
      function  FindNamespaceDecl   ( const _uri   : String
                                    ) : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Returns the value of a specified attribute.
      /// </summary>
      /// <param name="_name">
      ///   name of the attribute whose value you want to get
      /// </param>
      /// <param name="_uri">
      ///   URI in which attribute is located
      /// </param>
      /// <returns>
      ///   value of the attribute
      /// </returns>
      function  GetAttributeNS      ( const _name  : String ;
                                      const _uri   : String
                                    ) : OleVariant ; virtual; abstract;
      /// <summary>
      ///   Giving us information whether the node has attribute with given name.
      /// </summary>
      /// <param name="_name">
      ///   name of the attribute
      /// </param>
      /// <returns>
      ///   true if found
      /// </returns>
      function  HasAttribute        ( const _name  : String
                                    ) : Boolean ; overload; virtual; abstract;
      /// <summary>
      ///   Giving us information whether the node has attribute with given name.
      /// </summary>
      /// <param name="_name">
      ///   name of the attribute
      /// </param>
      /// <param name="_uri">
      ///   URI of the attribute
      /// </param>
      /// <returns>
      ///   true if found
      /// </returns>
      function  HasAttribute        ( const _name  : String ;
                                      const _uri   : String
                                    ) : Boolean ;overload; virtual; abstract;
      /// <summary>
      ///   Returns the next child of this node's parent.
      /// </summary>
      /// <returns>
      ///   next child of this node's parent
      /// </returns>
      function  NextSibling         : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Converts the subtree underneath this node to the structure it would have if it were just loaded from a document.
      /// </summary>
      procedure Normalize           ; virtual; abstract;
      /// <summary>
      ///   Returns the previous child of this node's parent
      /// </summary>
      /// <returns>
      ///   previous child of this node's parent
      /// </returns>
      function  PreviousSibling     : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Forces all child nodes and attribute nodes to be read again .
      /// </summary>
      procedure Resync              ; virtual; abstract;
      /// <summary>
      ///   Sets the value of one of this node's attributes when you must explicitly specify the attribute's URI.
      /// </summary>
      /// <param name="_name">
      ///   name of the attribute
      /// </param>
      /// <param name="_uri">
      ///   URI for the attribute
      /// </param>
      /// <param name="_value">
      ///   value assigned to the attribute
      /// </param>
      procedure SetAttributeNS      ( const _name  : String ;
                                      const _uri   : String ;
                                      const _value : OleVariant
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Transforms the subtree rooted at this node.
      /// </summary>
      /// <param name="_node">
      ///   node in an XML document that represents an XSL transformation
      /// </param>
      /// <param name="_out">
      ///   result of the transformation, returns a string of XML
      /// </param>
      procedure TransformNode       ( const _node  : IXMLNode ;
                                        var _out   : String
                                    ) ; overload; virtual; abstract;
      /// <summary>
      ///   Transforms the subtree rooted at this node.
      /// </summary>
      /// <param name="_node">
      ///   node in an XML document that represents an XSL transformation
      /// </param>
      /// <param name="_out">
      ///   result of the transformation, replaces the content of that document with the results of the transformation
      /// </param>
      procedure TransformNode       ( const _node  : IXMLNode ;
                                      const _out   : IXMLDocument
                                    ) ; overload; virtual; abstract;
    public
      /// <summary>
      ///   Represents the values of this node's attributes.
      /// </summary>
      /// <param name="_name">name of the attribute</param>
      /// <value>
      ///   attribute
      /// </value>
      property  Attributes [const _name : String]
                               : OleVariant
                                 read  fget_Attribute
                                 write fset_Attribute ;
      /// <summary>
      ///   Interfaces for nodes represent attributes of this node
      /// </summary>
      /// <value>
      ///   attribute nodes
      /// </value>
      property  AttributeNodes : IXMLNodeList
                                 read  fget_AttributeNodes ;
      /// <summary>
      ///   Child nodes of this node.
      /// </summary>
      /// <value>
      ///   child nodes
      /// </value>
      property  ChildNodes     : IXMLNodeList
                                 read  fget_ChildNodes ;
      /// <summary>
      ///   Gives us values of this node's child nodes.
      /// </summary>
      /// <param name="_iname">
      /// desired child node</param>
      /// <value>
      ///   child value
      /// </value>
      property  ChildValues [const _iname : OleVariant]
                               : OleVariant
                                 read  fget_ChildValue
                                 write fset_ChildValue ;
                                 default ;
      /// <summary>
      ///   Collection in which node appears.
      /// </summary>
      /// <value>
      ///   collection
      /// </value>
      property  Collection     : IXMLNodeCollection
                                 read  fget_Collection ;
      /// <summary>
      ///   Document in which node appears
      /// </summary>
      /// <value>
      ///   owner document
      /// </value>
      property  OwnerDocument  : IXMLDocument
                                 read  fget_OwnerDocument ;
      /// <summary>
      ///   Gives us information whether this node has any child nodes.
      /// </summary>
      /// <value>
      ///   has child nodes
      /// </value>
      property  HasChildNodes  : Boolean
                                 read  fget_HasChildNodes ;
      /// <summary>
      ///   Gives us information whether the node has any single text value.
      /// </summary>
      /// <value>
      ///   is text element
      /// </value>
      property  IsTextElement  : Boolean
                                 read  fget_IsTextElement ;
      /// <summary>
      ///   Gives us information about the name of the node without URI prefix.
      /// </summary>
      /// <value>
      ///   local name
      /// </value>
      property  LocalName      : String
                                 read  fget_LocalName ;
      /// <summary>
      ///   Gives us URI for the namespace used.
      /// </summary>
      /// <value>
      ///   namespace uri
      /// </value>
      property  NamespaceURI   : String
                                 read  fget_NamespaceURI ;
      /// <summary>
      ///   Gives us name of the node
      /// </summary>
      /// <value>
      ///   node name
      /// </value>
      property  NodeName       : String
                                 read  fget_NodeName ;
      /// <summary>
      ///   Gives us type of node.
      /// </summary>
      /// <value>
      ///   node type
      /// </value>
      property  NodeType       : TNodeType
                                 read  fget_NodeType ;
      /// <summary>
      ///   Gives us value of the node.
      /// </summary>
      /// <value>
      ///   node value
      /// </value>
      property  NodeValue      : OleVariant
                                 read  fget_NodeValue
                                 write fset_NodeValue ;
      /// <summary>
      ///   Gives us parent node of the node.
      /// </summary>
      /// <value>
      ///   parent node
      /// </value>
      property  ParentNode     : IXMLNode
                                 read  fget_ParentNode ;
      /// <summary>
      ///   Gives us URI prefix of the node's name.
      /// </summary>
      /// <value>
      ///   prefix
      /// </value>
      property  Prefix         : String
                                 read  fget_Prefix ;
      /// <summary>
      ///   Gives us information whether the node can be modified.
      /// </summary>
      /// <value>
      ///   read only
      /// </value>
      property  &ReadOnly      : Boolean
                                 read  fget_ReadOnly
                                 write fset_ReadOnly ;
      /// <summary>
      ///   Text of the node
      /// </summary>
      /// <value>
      ///   text
      /// </value>
      property  Text           : String
                                 read  fget_Text
                                 write fset_Text ;
      /// <summary>
      ///   Indicates the XML that corresponds to the subtree rooted at this node.
      /// </summary>
      /// <value>
      ///   xml
      /// </value>
      property  XML            : String
                                 read  fget_XML ;
  end ;
  /// <summary>
  ///   Set of nodes in an XML document.
  /// </summary>
  IXMLNodeList = {$IFDEF OXYGENE} public abstract {$ENDIF}
                 class
    protected
      function  fget_Count       : Integer ; virtual; abstract;
      function  fget_Node        ( const _iname : OleVariant
                                 ) : IXMLNode ; virtual; abstract;
      function  fget_UpdateCount : Integer ; virtual; abstract;
    public
      /// <summary>
      ///   Add a new node to the end of the list.
      /// </summary>
      /// <param name="_node">
      ///   Node to be added to the list
      /// </param>
      /// <returns>index of the added node</returns>
      function  Add              ( const _node  : IXMLNode
                                 ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Increments the value of UpdateCount.
      /// </summary>
      procedure BeginUpdate      ; virtual; abstract;
      /// <summary>
      ///   Remove all nodes from the list.
      /// </summary>
      procedure Clear            ; virtual; abstract;
      /// <summary>
      ///   Removes node by its index.
      /// </summary>
      /// <param name="_index">
      ///   index of the node that should be remove
      /// </param>
      /// <returns>
      ///   index of removed node
      /// </returns>
      function  Delete           ( const _index : Integer
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Removes node by its name.
      /// </summary>
      /// <param name="_name">
      ///   name of the node that should be removed
      /// </param>
      /// <returns>
      ///   index of removed node
      /// </returns>
      function  Delete           ( const _name  : String
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Removes node by its name and uri.
      /// </summary>
      /// <param name="_name">
      ///   name of the node that should be removed
      /// </param>
      /// <param name="_uri">
      ///   URI to be used when interpreting _name
      /// </param>
      /// <returns>
      ///   index of removed node
      /// </returns>
      function  Delete           ( const _name  : String ;
                                   const _uri   : String
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Decrements the value of UpdateCount.
      /// </summary>
      procedure EndUpdate        ; virtual; abstract;
      /// <summary>
      ///   Returns the first node in the list.
      /// </summary>
      /// <returns>
      ///   first node in the list
      /// </returns>
      function  First            : IXMLNode  ; virtual; abstract;
      /// <summary>
      ///   Returns specified node from the list based on its name.
      /// </summary>
      /// <param name="_name">
      ///   name to be used
      /// </param>
      /// <returns>
      ///   interface of the node
      /// </returns>
      function  FindNode         (       _name  : String
                                 ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Returns specified node from the list based on its name and URI.
      /// </summary>
      /// <param name="_name">
      ///   name to be used
      /// </param>
      /// <param name="_uri">
      ///   URI that qualifies name of the node
      /// </param>
      /// <returns>
      ///   interface of the node
      /// </returns>
      function  FindNode         (       _name  : String ;
                                         _uri   : String
                                 ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Returns specified node from the list based on its guid
      /// </summary>
      /// <param name="_guid">
      ///   interface type
      /// </param>
      /// <returns>
      ///   interface of the node
      /// </returns>
      function  FindNode         (       _guid  : TGUID
                                 ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Returns a node that appears a specified amount before or after another node.
      /// </summary>
      /// <param name="_node">
      ///   node in the list to be used as reference point
      /// </param>
      /// <param name="_delta">
      ///   indicates where the desired node appears
      /// </param>
      /// <returns>
      ///   node that appears at the position offset by delta
      /// </returns>
      function  FindSibling      ( const _node  : IXMLNode ;
                                         _delta : Integer
                                 ) : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Returns a specified node in the list.
      /// </summary>
      /// <param name="_index">
      ///   index of the node to get
      /// </param>
      /// <returns>
      ///   node in the list
      /// </returns>
      function  Get              (       _index : Integer
                                 ) : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Returns the index of the node.
      /// </summary>
      /// <param name="_node">
      ///   interface of the node to locate
      /// </param>
      /// <returns>
      ///   index of the specified node
      /// </returns>
      function  IndexOf          ( const _node  : IXMLNode
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Returns the index of the node.
      /// </summary>
      /// <param name="_name">
      ///   LocalName property of the node to locate
      /// </param>
      /// <returns>
      ///   index of the specified node
      /// </returns>
      function  IndexOf          ( const _name  : String
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Returns the index of the node.
      /// </summary>
      /// <param name="_name">
      ///   LocalName property of the node to locate
      /// </param>
      /// <param name="_uri">
      ///   URI to be used when interpreting _name
      /// </param>
      /// <returns>
      ///   index of the specified node
      /// </returns>
      function  IndexOf          ( const _name  : String ;
                                   const _uri   : String
                                 ) : Integer ; overload; virtual; abstract;
      /// <summary>
      ///   Inserts a new node into a specified position.
      /// </summary>
      /// <param name="_index">
      ///   specifies where to insert node
      /// </param>
      /// <param name="_node">
      ///   node to add to the list
      /// </param>
      procedure Insert           (       _index : Integer ;
                                   const _node  : IXMLNode
                                 ) ; virtual; abstract;
      /// <summary>
      ///   Returns the last node in the list.
      /// </summary>
      /// <returns>
      ///   last node in the list
      /// </returns>
      function  Last             : IXMLNode ; virtual; abstract;
      /// <summary>
      ///   Removes node from list.
      /// </summary>
      /// <param name="_node">
      ///   node to be removed
      /// </param>
      /// <returns>
      ///   index of the node before it was removed
      /// </returns>
      function  Remove           ( const _node  : IXMLNode
                                 ) : Integer ; virtual; abstract;
      /// <summary>
      ///   Replaces a specified node in the list with another node.
      /// </summary>
      /// <param name="_onode">
      ///   node to be replaced
      /// </param>
      /// <param name="_nnode">
      ///   node to replace the old one
      /// </param>
      /// <returns>
      ///   old node
      /// </returns>
      function  ReplaceNode      ( const _onode : IXMLNode ;
                                   const _nnode : IXMLNode
                                 ) : IXMLNode ; virtual; abstract;
    public
      /// <summary>
      ///   Number of nodes in the list.
      /// </summary>
      /// <value>
      ///   count
      /// </value>
      property  Count       : Integer
                              read  fget_Count ;
      /// <summary>
      ///   Provide access to the nodes in the list.
      /// </summary>
      /// <param name="_iname">
      ///   identifies the desired node
      /// </param>
      /// <value>
      ///   node
      /// </value>
      property  Nodes [const _iname : OleVariant]
                            : IXMLNode
                              read  fget_Node ;
                              default ;
      /// <summary>
      ///   Number of times an application has called BeginUpdate
      ///    without a matching call to EndUpdate.
      /// </summary>
      /// <value>
      ///   update count
      /// </value>
      property  UpdateCount : Integer
                              read  fget_UpdateCount ;
  end ;
  /// <summary>
  ///   Interface for a set of child nodes of the same type.
  /// </summary>
  IXMLNodeCollection = {$IFDEF OXYGENE} public abstract {$ENDIF}
                        class( IXMLNode )
    protected
      function  fget_Count : Integer ; virtual; abstract;
      function  fget_Node  (       _index : Integer
                           ) : IXMLNode ; virtual; abstract;
    public
      /// <inheritdoc from="IXMLNode"/>
      procedure Clear      ; virtual; abstract;
      /// <inheritdoc from="IXMLNode"/>
      procedure Delete     (       _index : Integer
                           ) ; virtual; abstract;
      /// <inheritdoc from="IXMLNode"/>
      function  Remove     ( const _node  : IXMLNode
                           ) : Integer ; virtual; abstract;
    public
      /// <inheritdoc from="IXMLNode"/>
      property  Count : Integer
                        read  fget_Count ;
      /// <inheritdoc from="IXMLNode"/>
      property  Nodes [_index : Integer]
                      : IXMLNode
                        read  fget_Node ;
                        default ;
  end ;

  /// <summary>
  ///   indicate types of information that an XML document automatically supplies.
  /// </summary>
  TXMLDocOption = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///  Create node if read node does not exist.
    /// </summary>
    doNodeAutoCreate,
    /// <summary>
    ///   Automatically indent child nodes from their parents.
    /// </summary>
    doNodeAutoIndent,
    /// <summary>
    ///   Gives null when reading the value of the attribute that does not exist.
    /// </summary>
    doAttrNull,
    /// <summary>
    ///   Automatically gives URI prefix to newly created nodes.
    /// </summary>
    doAutoPrefix,
    /// <summary>
    ///   If AddChild method must create a new URI for the node, it also adds a URI attribute to declare that new URI.
    /// </summary>
    doNamespaceDecl,
    /// <summary>
    ///   Automatically save on close of XML document.
    /// </summary>
    doAutoSave
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   Set of TXMLOption values.
  /// </summary>
  TXMLDocOptions = {$IFDEF OXYGENE} public {$ENDIF} set of TXMLDocOption ;

  /// <summary>
  ///   Provides information on how DOM parser should parse an XML document.
  /// </summary>
  TParseOption = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   External definitions are resolved at parse time.
    /// </summary>
    poResolveExternals,
    /// <summary>
    ///   Parser validates XML document against its schema as well as verifying that it is well-formatted.
    /// </summary>
    poValidateOnParse,
    /// <summary>
    ///   Not stripping off white spaces in the text of XML document.
    /// </summary>
    poPreserveWhiteSpace,
    /// <summary>
    ///   Make the DOM parser to work asynchronously.
    /// </summary>
    poAsyncLoad
  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   Set of TParseOption values.
  /// </summary>
  TParseOptions = {$IFDEF OXYGENE} public {$ENDIF} set of TParseOption ;

  /// <summary>
  /// Indicates the character set used to encode an XML document.
  /// </summary>
  TXMLEncodingType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Unknown character set.
    /// </summary>
    xetUnknown,
    /// <summary>
    ///   UCS-4, big-endian (1234).
    /// </summary>
    xetUCS_4BE,
    /// <summary>
    ///   UCS-4, little-endian (4321).
    /// </summary>
    xetUCS_4LE,
    /// <summary>
    ///   UCS-4, unusual octet order (2134).
    /// </summary>
    xetUCS_4Order2134,
    /// <summary>
    ///   UCS-4, unusual octet order (3412).
    /// </summary>
    xetUCS_4Order3412,
    /// <summary>
    ///   UTF-16, big-endian.
    /// </summary>
    xetUTF_16BE,
    /// <summary>
    ///   UTF-16, little-endian.
    /// </summary>
    xetUTF_16LE,
    /// <summary>
    ///   UTF-8.
    /// </summary>
    xetUTF_8,
    /// <summary>
    ///   Any UCS-4.
    /// </summary>
    xetUCS_4Like,
    /// <summary>
    ///   Any UTF-16, big-endian.
    /// </summary>
    xetUTF_16BELike,
    /// <summary>
    ///   Any UTF-16, little-endian.
    /// </summary>
    xetUTF_16LELike,
    /// <summary>
    ///   UTF-8, ISO 646, ASCII, or other 7- or 8-bit encoding, including multi-byte character sets.
    /// </summary>
    xetUTF_8Like,
    /// <summary>
    ///   EBCDIC
    /// </summary>
    xetEBCDICLike
  ) ;

  /// <summary>
  ///   Represents an XML document
  /// </summary>
  IXMLDocument = {$IFDEF OXYGENE} public abstract {$ENDIF}
                 class
    protected
      function  fget_Active          : Boolean ; virtual; abstract;
      procedure fset_Active          ( const _value : Boolean
                                     ) ; virtual; abstract;
      function  fget_ChildNodes      : IXMLNodeList ; virtual; abstract;
      function  fget_DocumentElement : IXMLNode ; virtual; abstract;
      procedure fset_DocumentElement ( const _value : IXMLNode
                                     ) ; virtual; abstract;
      function  fget_DocumentNode    : IXMLNode ; virtual; abstract;
      function  fget_Encoding        : String ; virtual; abstract;
      procedure fset_Encoding        ( const _value : String
                                     ) ; virtual; abstract;
      function  fget_FileName        : String ; virtual; abstract;
      procedure fset_FileName        ( const _value : String
                                     ) ; virtual; abstract;
      function  fget_Modified        : Boolean ; virtual; abstract;
      function  fget_NodeIndentStr   : String ; virtual; abstract;
      procedure fset_NodeIndentStr   ( const _value : String
                                     ) ; virtual; abstract;
      function  fget_Options         : TXMLDocOptions ; virtual; abstract;
      procedure fset_Options         ( const _value : TXMLDocOptions
                                     ) ; virtual; abstract;
      function  fget_ParseOptions    : TParseOptions ; virtual; abstract;
      procedure fset_ParseOptions    ( const _value : TParseOptions
                                     ) ; virtual; abstract;
      function  fget_SchemaRef       : String ; virtual; abstract;
      function  fget_StandAlone      : String ; virtual; abstract;
      procedure fset_StandAlone      ( const _value : String
                                     ) ; virtual; abstract;
      function  fget_Version         : String ; virtual; abstract;
      procedure fset_Version         ( const _value : String
                                     ) ; virtual; abstract;
      function  fget_XML             : TStrings ; virtual; abstract;
      procedure fset_XML             ( const _value : TStrings
                                     ) ; virtual; abstract;
    public
      /// <summary>
      ///   Creates and then adds a node to the child nodes of document
      /// </summary>
      /// <param name="_name">
      ///   name for new element node
      /// </param>
      /// <returns>
      ///   interface for the new node
      /// </returns>
      function  AddChild             ( const _name  : String
                                     ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Creates and then adds a node to the child nodes of document
      /// </summary>
      /// <param name="_name">
      ///   name for new element node
      /// </param>
      /// <param name="_uri">
      ///   identifies the URI, if any, that includes the new node's definition
      /// </param>
      /// <returns>
      ///   interface for the new node
      /// </returns>
      function  AddChild             ( const _name  : String ;
                                       const _uri   : String
                                     ) : IXMLNode ; overload; virtual; abstract;
      /// <summary>
      ///   Creates a new element node that is associated with this document.
      /// </summary>
      /// <param name="_name">
      ///   name of created node
      /// </param>
      /// <param name="_uri">
      ///   URI which includes the new node definition
      /// </param>
      /// <returns>
      ///   interface for the new node
      /// </returns>
      function  CreateElement        ( const _name  : String ;
                                       const _uri   : String
                                     ) : IXMLNode ; virtual; abstract;
      {$IFDEF JAVA}
        /// <summary>
        ///   Creates a new node associated with document.
        /// </summary>
        /// <param name="_name">
        ///   name of the node
        /// </param>
        /// <returns>
        ///   interface for the new node
        /// </returns>
        function  CreateNode         ( const _name  : String
                                     ) : IXMLNode ; overload; virtual; abstract;
        /// <summary>
        ///   Creates a new node associated with document.
        /// </summary>
        /// <param name="_name">
        ///   name of the node
        /// </param>
        /// <param name="_type">
        ///   type of node
        /// </param>
        /// <returns>
        ///   interface for the new node
        /// </returns>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType
                                     ) : IXMLNode ; overload; virtual; abstract;
        /// <summary>
        ///   Creates a new node associated with document.
        /// </summary>
        /// <param name="_name">
        ///   name of the node
        /// </param>
        /// <param name="_data">
        ///   additional data
        /// </param>
        /// <returns>
        ///   interface for the new node
        /// </returns>
        function  CreateNode         ( const _name  : String ;
                                       const _data  : String
                                     ) : IXMLNode ; overload; virtual; abstract;
        /// <summary>
        ///   Creates a new node associated with document.
        /// </summary>
        /// <param name="_name">
        ///   name of the node
        /// </param>
        /// <param name="_type">
        ///   type of node
        /// </param>
        /// <param name="_data">
        ///   additional data
        /// </param>
        /// <returns>
        ///   interface for the new node
        /// </returns>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType ;
                                       const _data  : String
                                     ) : IXMLNode ; overload; virtual; abstract;
      {$ELSE}
        /// <summary>
        ///   Creates a new node associated with document.
        /// </summary>
        /// <param name="_name">
        ///   name of the node
        /// </param>
        /// <param name="_type">
        ///   type of node
        /// </param>
        /// <param name="_data">
        ///   additional data
        /// </param>
        /// <returns>
        ///   interface for the new node
        /// </returns>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType = TNodeType.ntElement ;
                                       const _data  : String = ''
                                     ) : IXMLNode ; virtual; abstract;
      {$ENDIF}
      /// <summary>
      ///   Generates unique URI prefix.
      /// </summary>
      /// <param name="_node">
      ///   interface for the node which will use generated prefix
      /// </param>
      /// <returns>
      ///   unique string to represent a namespace URI
      /// </returns>
      function  GeneratePrefix       ( const _node  : IXMLNode
                                     ) : String ; virtual; abstract;
      /// <summary>
      ///   Gives us information whether document is empty.
      /// </summary>
      /// <returns>
      ///   true if empty
      /// </returns>
      function  IsEmptyDoc           : Boolean ; virtual; abstract;
      /// <summary>
      ///   Loads an XML document from file.
      /// </summary>
      /// <param name="_path">
      ///   path to the file
      /// </param>
      procedure LoadFromFile         ( const _path  : String
                                     ) ; virtual; abstract;
      {$IFDEF JAVA}
        /// <summary>
        ///   Loads an XML document from a stream and activates it.
        /// </summary>
        /// <param name="_strm">
        ///   stream object
        /// </param>
        procedure LoadFromStream     ( const _strm  : TStream
                                     ) ; overload; virtual; abstract;
        /// <summary>
        ///   Loads an XML document from a stream and activates it.
        /// </summary>
        /// <param name="_strm">
        ///   stream object
        /// </param>
        /// <param name="_enct">
        ///   character set used in stream
        /// </param>
        procedure LoadFromStream     ( const _strm  : TStream ;
                                             _enct  : TXMLEncodingType
                                     ) ; overload; virtual; abstract;
      {$ELSE}
        /// <summary>
        ///   Loads an XML document from a stream and activates it.
        /// </summary>
        /// <param name="_strm">
        ///   stream object
        /// </param>
        /// <param name="_enct">
        ///   character set used in stream
        /// </param>
        procedure LoadFromStream     ( const _strm  : TStream ;
                                             _enct  : TXMLEncodingType = TXMLEncodingType.xetUnknown
                                     ) ; virtual; abstract;
      {$ENDIF}
      /// <summary>
      ///   Loads a string representation of an XML document and activates it.
      /// </summary>
      /// <param name="_xml">
      ///   string representation of XML document
      /// </param>
      procedure LoadFromXML          ( const _xml   : String
                                     ) ; virtual; abstract;
      /// <summary>
      ///   Updates parsed XML document.
      /// </summary>
      procedure Refresh              ; virtual; abstract;
      /// <summary>
      ///   Forces all child nodes and attribute nodes to be reread.
      /// </summary>
      procedure Resync               ; virtual; abstract;
      /// <summary>
      ///   Saves the XML document to the file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      procedure SaveToFile           ( const _path  : String
                                     ) ; virtual; abstract;
      /// <summary>
      ///   Saves the XML document to the stream.
      /// </summary>
      /// <param name="_strm">
      ///   output stream
      /// </param>
      procedure SaveToStream         ( const _strm  : TXMLOutputStream
                                     ) ; virtual; abstract;
      /// <summary>
      ///   Saves the XML document to a string-type variable.
      /// </summary>
      /// <param name="_xml">
      ///   variable to which we want to save XML document
      /// </param>
      procedure SaveToXML            (   var _xml   : String
                                     ) ; virtual; abstract;
    public
      /// <summary>
      ///   Gives us information whether the XML document has been parsed.
      /// </summary>
      /// <value>
      ///   active
      /// </value>
      property  Active          : Boolean
                                  read  fget_Active
                                  write fset_Active ;
      /// <summary>
      ///   Lists all of the child nodes of the document.
      /// </summary>
      /// <value>
      ///   child nodes
      /// </value>
      property  ChildNodes      : IXMLNodeList
                                  read  fget_ChildNodes ;
      /// <summary>
      ///   Provides access to the root node of the XML document.
      /// </summary>
      /// <value>
      ///   document element
      /// </value>
      property  DocumentElement : IXMLNode
                                  read  fget_DocumentElement
                                  write fset_DocumentElement ;
      /// <summary>
      ///   Indicates the character set that the XML document uses to encode characters.
      /// </summary>
      /// <value>
      ///   encoding
      /// </value>
      property  Encoding        : String
                                  read  fget_Encoding
                                  write fset_Encoding ;
      /// <summary>
      ///   Indicates the XML document file.
      /// </summary>
      /// <value>
      ///   file name
      /// </value>
      property  FileName        : String
                                  read  fget_FileName
                                  write fset_FileName ;
      /// <summary>
      ///   Indicates whether XML document has been modified.
      /// </summary>
      /// <value>
      ///   modified
      /// </value>
      property  Modified        : Boolean
                                  read  fget_Modified ;
      /// <summary>
      ///   Provides access to the document node.
      /// </summary>
      /// <value>
      ///   node
      /// </value>
      property  Node            : IXMLNode
                                  read  fget_DocumentNode ;
      /// <summary>
      ///   Indicates the string that is inserted before nested nodes in the formatted XML text.
      /// </summary>
      /// <value>
      ///   node indent string
      /// </value>
      property  NodeIndentStr   : String
                                  read  fget_NodeIndentStr
                                  write fset_NodeIndentStr ;
      /// <summary>
      ///   Influences various aspects of the document's behavior.
      /// </summary>
      /// <value>
      ///   options
      /// </value>
      property  Options       : TXMLDocOptions
                                read  fget_Options
                                write fset_Options ;
      /// <summary>
      ///   Specifies implementation-specific options that are followed when parsing the XML document.
      /// </summary>
      /// <value>
      ///   parse options
      /// </value>
      property  ParseOptions  : TParseOptions
                                read  fget_ParseOptions
                                write fset_ParseOptions ;

      /// <summary>
      ///   Returns the name of the schema associated with this XML document.
      /// </summary>
      /// <value>
      ///   schema reference
      /// </value>
      property  SchemaRef       : String
                                  read  fget_SchemaRef ;
      /// <summary>
      ///   Indicates whether the XML document includes any external declarations.
      /// </summary>
      /// <value>
      ///   stand alone
      /// </value>
      property  StandAlone      : String
                                  read  fget_StandAlone
                                  write fset_StandAlone ;
      /// <summary>
      ///   Indicates XML version that the document uses.
      /// </summary>
      /// <value>
      ///   version
      /// </value>
      property  Version         : String
                                  read  fget_Version
                                  write fset_Version ;
      /// <summary>
      ///   Specifies the content of the XML document associated with this IXMLDocument interface.
      /// </summary>
      /// <value>
      ///   xml
      /// </value>
      property  XML             : TStrings
                                  read  fget_XML
                                  write fset_XML ;
  end ;

type

  TGIS_XMLNode     = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_XMLNodeList = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_XMLDocument = {$IFDEF OXYGENE} public {$ENDIF} class ;
  /// <summary>
  ///   Implementation of XMLNode interface
  /// </summary>
  TGIS_XMLNode = {$IFDEF OXYGENE} public {$ENDIF}
                 class ( IXMLNode )
    private
      oNodes      : TGIS_XMLNodeList ;
      oPrefixMap  : TObject ;
      oAttributes : TGIS_XMLNodeList ;
    private
      FDocument : TGIS_XMLDocument ;
      FParent   : TGIS_XMLNode     ;
      FURI      : String ;
      FLName    : String ;
      FQname    : String ;
      FPrefix   : String ;
      FType     : TNodeType ;
      FValue    : String ;
      FReadOnly : Boolean ;
    protected
      function  fget_Attribute      ( const _name  : String
                                    ) : OleVariant ; override;
      procedure fset_Attribute      ( const _name  : String ;
                                      const _value : OleVariant
                                    ) ;  override;
      function  fget_AttributeNodes : IXMLNodeList ; override;
      function  fget_ChildNodes     : IXMLNodeList ; override;
      function  fget_ChildValue     ( const _iname : OleVariant
                                    ) : OleVariant ; override;
      procedure fset_ChildValue     ( const _iname : OleVariant ;
                                      const _value : OleVariant
                                    ) ; override;
      function  fget_Collection     : IXMLNodeCollection ; override;
      function  fget_HasChildNodes  : Boolean ; override;
      function  fget_IsTextElement  : Boolean ; override;
      function  fget_LocalName      : String ; override;
      function  fget_NamespaceURI   : String ; override;
      function  fget_NodeName       : String ; override;
      function  fget_NodeType       : TNodeType ; override;
      function  fget_NodeValue      : OleVariant ; override;
      procedure fset_NodeValue      ( const _value : OleVariant
                                    ) ; override;
      function  fget_OwnerDocument  : IXMLDocument ; override;
      function  fget_ParentNode     : IXMLNode ; override;
      function  fget_Prefix         : String ; override;
      function  fget_ReadOnly       : Boolean ; override;
      procedure fset_ReadOnly       ( const _value : Boolean
                                    ) ; override;
      function  fget_Text           : String ; override;
      procedure fset_Text           ( const _value : String
                                    ) ; override;
      function  fget_XML            : String ; override;
    private
      procedure genLNameAndPrefix   ;
      function  indentChars         ( const _chars : String  ;
                                      const _bspc  : Boolean ;
                                      const _lvl   : Integer
                                    ) : String ;
    public
      /// <summary>
      ///   Constructor for TGIS_XMLNode.
      /// </summary>
      /// <param name="_doc">
      ///   represents the XML document in which the new node occurs
      /// </param>
      /// <param name="_prnt">
      ///   interface for the DOM implementation of the node
      /// </param>
      /// <param name="_uri">
      ///   URI which includes the new node
      /// </param>
      /// <param name="_lname">
      ///    local name of the attribute
      /// </param>
      /// <param name="_qname">
      ///    qualified name of the attribute
      /// </param>
      /// <param name="_type">
      ///   type of node
      /// </param>
      /// <param name="_value">
      ///   text value for node
      /// </param>
      constructor Create ( const _doc   : TGIS_XMLDocument ;
                           const _prnt  : TGIS_XMLNode ;
                           const _uri   : String ;
                           const _lname : String ;
                           const _qname : String ;
                           const _type  : TNodeType ;
                           const _value : String
                         ) ;

    {$IFDEF DCC}
      public
        ///<inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    {$IFDEF DCC} private {$ENDIF}
    {$IFDEF OXYGENE} unit {$ENDIF}
      function  addChild            ( const _uri   : String ;
                                      const _lname : String ;
                                      const _qname : String ;
                                      const _type  : TNodeType ;
                                      const _value : String
                                    ) : TGIS_XMLNode ; overload;
      procedure addAttribute        ( const _uri   : String ;
                                      const _lname : String ;
                                      const _qname : String ;
                                      const _type  : String ;
                                      const _value : String
                                    ) ;
      procedure setParentNode       ( const _node  : TGIS_XMLNode
                                    ) ;
      function  indentXML           ( const _lvl   : Integer
                                    ) : String ;
    public
      ///<inheritdoc from="IXMLNode"/>
      function  AddChild            ( const _name  : String ;
                                            _index : Integer = -1
                                    ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLNode"/>
      function  AddChild            ( const _name  : String ;
                                      const _uri   : String ;
                                            _prfx  : Boolean = False ;
                                            _index : Integer = -1
                                    ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLNode"/>
      function  CloneNode           (       _deep  : Boolean
                                    ) : IXMLNode ; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure DeclareNamespace    ( const _prfx  : String ;
                                      const _uri   : String
                                    ) ; override;
      ///<inheritdoc from="IXMLNode"/>
      function  FindNamespaceURI    ( const _prfx  : String
                                    ) : String ; override;
      ///<inheritdoc from="IXMLNode"/>
      function  FindNamespaceDecl   ( const _uri   : String
                                    ) : IXMLNode ; override;
      ///<inheritdoc from="IXMLNode"/>
      function  GetAttributeNS      ( const _name  : String ;
                                      const _uri   : String
                                    ) : OleVariant ; override;
      ///<inheritdoc from="IXMLNode"/>
      function  HasAttribute        ( const _name  : String
                                    ) : Boolean ; overload; override;
      ///<inheritdoc from="IXMLNode"/>
      function  HasAttribute        ( const _name  : String ;
                                      const _uri   : String
                                    ) : Boolean ; overload; override;
      ///<inheritdoc from="IXMLNode"/>
      function  NextSibling         : IXMLNode ; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure Normalize           ; override;
      ///<inheritdoc from="IXMLNode"/>
      function  PreviousSibling     : IXMLNode ; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure Resync              ; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure SetAttributeNS      ( const _name  : String ;
                                      const _uri   : String ;
                                      const _value : OleVariant
                                    ) ; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure TransformNode       ( const _node  : IXMLNode ;
                                        var _out   : String
                                    ) ; overload; override;
      ///<inheritdoc from="IXMLNode"/>
      procedure TransformNode       ( const _node  : IXMLNode ;
                                      const _out   : IXMLDocument
                                    ) ; overload; override;
  end ;
  /// <summary>
  /// Implementation of IXMLNodeList interface.
  /// </summary>
  TGIS_XMLNodeList = {$IFDEF OXYGENE} public {$ENDIF}
                     class ( IXMLNodeList )
    private
      oOwner : TGIS_XMLNode ;
      oNodes : TObjectList<TGIS_XMLNode> ;
    private
      FUpdateCount : Integer ;
    public
      /// <summary>
      ///   Constructor for TGIS_XMLNodeList.
      /// </summary>
      /// <param name="_owner">
      ///   node that creates TGIS_XMLNodeList
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create ( const _owner : TGIS_XMLNode
                         ) ;
    {$IFDEF DCC}
      public
        ///<inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    protected
      function  fget_Count       : Integer ; override;
      function  fget_Node        ( const _iname : OleVariant
                                 ) : IXMLNode ; override;
      function  fget_UpdateCount : Integer ; override;
    public
      ///<inheritdoc from="IXMLNodeList"/>
      function  Add              ( const _node  : IXMLNode
                                 ) : Integer ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      procedure BeginUpdate      ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      procedure Clear            ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  Delete           ( const _index : Integer
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  Delete           ( const _name  : String
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  Delete           ( const _name  : String ;
                                   const _uri   : String
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      procedure EndUpdate        ; override;
      ///<inheritdoc from="IXMLDocument"/>
      function  First            : IXMLNode  ; override;
      ///<inheritdoc from="IXMLDocument"/>
      function  FindNode         (       _name  : String
                                 ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  FindNode         (       _name  : String ;
                                         _uri   : String
                                 ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  FindNode         (       _guid  : TGUID
                                 ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  FindSibling      ( const _node  : IXMLNode ;
                                         _delta : Integer
                                 ) : IXMLNode ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  Get              (       _index : Integer
                                 ) : IXMLNode ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  IndexOf          ( const _node  : IXMLNode
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  IndexOf          ( const _name  : String
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  IndexOf          ( const _name  : String ;
                                   const _uri   : String
                                 ) : Integer ; overload; override;
      ///<inheritdoc from="IXMLNodeList"/>
      procedure Insert           (       _index : Integer ;
                                   const _node  : IXMLNode
                                 ) ; override;
      ///<inheritdoc from="IXMLDocument"/>
      function  Last             : IXMLNode ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  Remove           ( const _node  : IXMLNode
                                 ) : Integer ; override;
      ///<inheritdoc from="IXMLNodeList"/>
      function  ReplaceNode      ( const _onode : IXMLNode ;
                                   const _nnode : IXMLNode
                                 ) : IXMLNode ; override;
  end ;
  /// <summary>
  /// Implementation of IXMLDocument interface.
  /// </summary>
  TGIS_XMLDocument = {$IFDEF OXYGENE} public {$ENDIF}
                     class ( IXMLDocument )
    private
      iPrefixGen  : Integer ;
    {$IFDEF DCC} private {$ENDIF}
    {$IFDEF OXYGENE} unit {$ENDIF}
      FNode       : TGIS_XMLNode ;
    private
      FActive     : Boolean ;
      FModified   : Boolean ;
      FVersion    : String ;
      FEncoding   : String ;
      FStandalone : String ;
      FFileName   : String ;
      FIndentStr  : String ;
      FXML        : TStrings ;
      FCompact    : Boolean ;
      FParseOptions : TParseOptions ;
    public
      /// <summary>
      /// Constructor for TGIS_XMLDocument.
      /// </summary>
      constructor Create ;
    {$IFDEF DCC}
      public
        ///<inheritdoc/>
        destructor Destroy ; override;
    {$ENDIF}
    {$IFDEF DCC} private {$ENDIF}
    {$IFDEF OXYGENE} unit {$ENDIF}
      procedure markModified         ;
    private
      procedure clear                ;
      procedure markFresh            ;
      procedure loadFromStrings      ;
    public
      ///<inheritdoc from="IXMLDocument"/>
      procedure Reset                ;
    protected
      function  fget_Active          : Boolean ; override;
      procedure fset_Active          ( const _value : Boolean
                                     ) ; override;
      function  fget_ChildNodes      : IXMLNodeList ; override;
      function  fget_DocumentElement : IXMLNode ; override;
      procedure fset_DocumentElement ( const _node  : IXMLNode
                                     ) ; override;
      function  fget_DocumentNode    : IXMLNode ; override;
      function  fget_Encoding        : String ; override;
      procedure fset_Encoding        ( const _value : String
                                     ) ; override;
      function  fget_FileName        : String ; override;
      procedure fset_FileName        ( const _path  : String
                                     ) ; override;
      function  fget_Modified        : Boolean ; override;
      function  fget_NodeIndentStr   : String ; override;
      procedure fset_NodeIndentStr   ( const _value : String
                                     ) ; override;
      function  fget_Options       : TXMLDocOptions ; override;
      procedure fset_Options       ( const _value : TXMLDocOptions
                                    ) ; override;
      function  fget_ParseOptions  : TParseOptions ; override;
      procedure fset_ParseOptions  ( const _value : TParseOptions
                                    ) ; override;
      function  fget_SchemaRef       : String ; override;
      function  fget_StandAlone      : String ; override;
      procedure fset_StandAlone      ( const _value : String
                                     ) ; override;
      function  fget_Version         : String ; override;
      procedure fset_Version         ( const _value : String
                                     ) ; override;
      function  fget_XML             : TStrings ; override;
      procedure fset_XML             ( const _xml   : TStrings
                                     ) ; override;
    public
      ///<inheritdoc from="IXMLDocument"/>
      function  AddChild             ( const _name  : String
                                     ) : IXMLNode ; overload; override;
      ///<inheritdoc from="IXMLDocument"/>
      function  AddChild             ( const _name  : String ;
                                       const _uri   : String
                                     ) : IXMLNode ; overload; override;
        ///<inheritdoc from="IXMLDocument"/>
      function  CreateElement        ( const _name  : String ;
                                       const _uri   : String
                                     ) : IXMLNode ; override;
      {$IFDEF JAVA}
        ///<inheritdoc from="IXMLDocument"/>
        function  CreateNode         ( const _name  : String
                                     ) : IXMLNode ; overload; override;
        ///<inheritdoc from="IXMLDocument"/>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType
                                     ) : IXMLNode ; overload; override;
        ///<inheritdoc from="IXMLDocument"/>
        function  CreateNode         ( const _name  : String ;
                                       const _data  : String
                                     ) : IXMLNode ; overload; override;
        ///<inheritdoc from="IXMLDocument"/>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType ;
                                       const _data  : String
                                     ) : IXMLNode ; overload; override;
      {$ELSE}
        ///<inheritdoc from="IXMLDocument"/>
        function  CreateNode         ( const _name  : String ;
                                             _type  : TNodeType = TNodeType.ntElement ;
                                       const _data  : String = ''
                                     ) : IXMLNode ; override;
      {$ENDIF}
      ///<inheritdoc from="IXMLDocument"/>
      function  GeneratePrefix       ( const _node  : IXMLNode
                                     ) : String ; override;
      ///<inheritdoc from="IXMLDocument"/>
      function  IsEmptyDoc           : Boolean ; override;
      ///<inheritdoc from="IXMLDocument"/>
      procedure LoadFromFile         ( const _path  : String
                                     ) ; override;
      ///<inheritdoc from="IXMLDocument"/>
      {$IFDEF JAVA}
        procedure LoadFromStream     ( const _strm  : TStream
                                     ) ; overload; override;
        ///<inheritdoc from="IXMLDocument"/>
        procedure LoadFromStream     ( const _strm  : TStream ;
                                             _enct  : TXMLEncodingType
                                     ) ; overload; override;
      {$ELSE}
      ///<inheritdoc from="IXMLDocument"/>
        procedure LoadFromStream     ( const _strm  : TStream ;
                                             _enct  : TXMLEncodingType = TXMLEncodingType.xetUnknown
                                     ) ; override;
      ///<inheritdoc from="IXMLDocument"/>
      {$ENDIF}
      ///<inheritdoc from="IXMLDocument"/>
      procedure LoadFromXML          ( const _xml   : String
                                     ) ; override;
      ///<inheritdoc from="IXMLDocument"/>
      procedure Refresh              ; override;
      ///<inheritdoc from="IXMLDocument"/>
      procedure Resync               ; override;
      ///<inheritdoc from="IXMLDocument"/>
      procedure SaveToFile           ( const _path  : String
                                     ) ; override;
      ///<inheritdoc from="IXMLDocument"/>
      procedure SaveToStream         ( const _strm  : TXMLOutputStream
                                     ) ; overload; override;
      {$IFDEF JAVA}
        {#GENDOC:HIDE}
        procedure SaveToStream       ( const _strm  : TStream
                                     ) ; overload;
      {$ENDIF}
      ///<inheritdoc from="IXMLDocument"/>
      procedure SaveToXML            (   var _xml   : String
                                     ) ; override;
    public
      ///<inheritdoc from="IXMLDocument"/>
      property SaveCompact      : Boolean
                                  read  FCompact
                                  write FCompact ;
  end ;

//##############################################################################
implementation

type

  T_xmlNamespaceDef = record
    Prefix : String ;
    URI    : String ;
  end ;

  T_xmlPrefixMap = class
    private
      oList : TList<T_xmlNamespaceDef> ;
    public
      constructor Create ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure Add     ( const _prfx  : String ;
                          const _uri   : String
                        ) ;
      function  FindURI ( const _prfx  : String
                        ) : String ;
      function  HasURI  ( const _uri   : String
                        ) : Boolean ;
  end ;

  T_domContentHandler = class( TGIS_SAXContentHandler )
    private
      oDoc  : TGIS_XMLDocument ;
      oNode : TGIS_XMLNode ;
    public
      constructor Create ( const _doc : TGIS_XMLDocument
                         ) ;
    {$IFDEF DCC}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure StartDocument    ; override;
      procedure EndDocument      ; override;
      procedure StartElement     ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String ;
                                   const _attribs : IVBSAXAttributes
                                 ) ; override;
      procedure EndElement       ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String
                                 ) ; override;
      procedure Characters       ( const _chars   : String
                                 ) ; override;
      procedure StartPrefixMapping
                                 ( const _prefix  : String ;
                                   const _uri     : String
                                 ) ; override;
      procedure EndPrefixMapping ( const _prefix  : String
                                 ) ; override;
      procedure ProcessingInstruction
                                 ( const _target  : String ;
                                   const _data    : String
                                 ) ; override;
    public
      procedure FatalError       ( const _locator : IVBSAXLocator ;
                                   const _message : String        ;
                                   const _code    : HResult
                                 ) ; override;
    public
      procedure _XMLDecl         ( const _ver   : String ;
                                   const _enc   : String ;
                                   const _stndl : String
                                 ) ; override;
      procedure _DocType         ( const _data  : String
                                 ) ; override;
      procedure _Content         ( const _chars : String
                                 ) ; override;
      procedure _Comment         ( const _chars : String
                                 ) ; override;
      procedure _CDATA           ( const _chars : String
                                 ) ; override;
  end ;

  function xml_colon_separate(
    const _str   : String ;
      out _left  : String ;
      out _right : String
  ) : Boolean ;
  var
    last : Integer ;
    i    : Integer ;
  begin
    Result := False ;

    last := StringLast( _str ) ;
    for i := StringFirst to last do begin
      if _str[i] = ':' then begin
        _left  := Copy( _str, StringFirst, i - StringFirst ) ;
        _right := Copy( _str, i + 1, last - i ) ;
        Result := True ;
        break ;
      end ;
    end ;

    if not Result then begin
      _left  := ''   ;
      _right := _str ;
    end ;
  end ;

  procedure xmlSBClear(
    const _sb : TXMLStringBuilder
  ) ;
  begin
    {$IFDEF DCC}
      _sb.Clear ;
    {$ENDIF}
    {$IFDEF CLR}
      _sb.Length := 0 ;
    {$ENDIF}
    {$IFDEF JAVA}
      _sb.setLength( 0 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// T_xmlPrefixMap
//==============================================================================

  constructor T_xmlPrefixMap.Create ;
  begin
    inherited ;

    oList := TList<T_xmlNamespaceDef>.Create ;
  end ;

  {$IFDEF DCC}
    destructor T_xmlPrefixMap.Destroy ;
    begin
      oList.Free ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_xmlPrefixMap.Add(
    const _prfx  : String ;
    const _uri   : String
  ) ;
  var
    nd : T_xmlNamespaceDef ;
  begin
    nd.Prefix := _prfx ;
    nd.URI    := _uri  ;

    oList.Add( nd ) ;
  end ;

  function T_xmlPrefixMap.FindURI(
    const _prfx  : String
  ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;

    for i := 0 to oList.Count - 1 do begin
      if _prfx = oList[i].Prefix then begin
        Result := oList[i].URI ;
        break ;
      end ;
    end ;
  end ;

  function T_xmlPrefixMap.HasURI(
    const _uri   : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;

    for i := 0 to oList.Count - 1 do begin
      if _uri = oList[i].URI then begin
        Result := True ;
        break ;
      end ;
    end ;
  end ;

//==============================================================================
// T_domContentHandler
//==============================================================================

  constructor T_domContentHandler.Create(
    const _doc : TGIS_XMLDocument
  ) ;
  begin
    inherited Create ;

    oDoc := _doc ;
    if TParseOption.poPreserveWhiteSpace in oDoc.ParseOptions then
      bPreserveWhiteSpace := True
    else
      bPreserveWhiteSpace := False ;
  end ;

  {$IFDEF DCC}
    destructor T_domContentHandler.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}

  procedure T_domContentHandler.StartDocument ;
  begin
    oDoc.Reset ;
    oNode := oDoc.FNode ;
  end ;

  procedure T_domContentHandler.EndDocument ;
  begin
    // DO NOTHING
  end ;

  procedure T_domContentHandler.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i : Integer ;
  begin
    oNode := oNode.addChild(
               _uri, _lname, _qname, TNodeType.ntElement, ''
             ) ;

    for i := 0 to _attribs.Length - 1 do begin
      oNode.addAttribute(
        _attribs.GetURI( i ),
        _attribs.GetLocalName( i ),
        _attribs.GetQName( i ),
        _attribs.GetType( i ),
        _attribs.GetValue( i )
      ) ;
    end ;
  end ;

  procedure T_domContentHandler.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  begin
    oNode := TGIS_XMLNode( oNode.ParentNode ) ;
  end ;

  procedure T_domContentHandler.Characters(
    const _chars   : String
  ) ;
  begin
    // DO NOTHING - too little information, use additional events instead
  end ;

  procedure T_domContentHandler.StartPrefixMapping(
    const _prefix  : String ;
    const _uri     : String
  ) ;
  begin
    oNode.DeclareNamespace( _prefix, _uri ) ;
  end ;

  procedure T_domContentHandler.EndPrefixMapping(
    const _prefix  : String
  ) ;
  begin
    // DO NOTHING
  end ;

  procedure T_domContentHandler.ProcessingInstruction(
    const _target  : String ;
    const _data    : String
  ) ;
  begin
    oNode.addChild(
      '', _target, _target, TNodeType.ntProcessingInstr, _data
    ) ;
  end ;

  procedure T_domContentHandler.FatalError(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    oDoc.Reset ;
    raise EXMLDocError.Create( _message ) ;
  end ;

  procedure T_domContentHandler._XMLDecl(
    const _ver   : String ;
    const _enc   : String ;
    const _stndl : String
  ) ;
  begin
    oDoc.Version    := _ver   ;
    oDoc.Encoding   := _enc   ;
    oDoc.StandAlone := _stndl ;
  end ;

  procedure T_domContentHandler._DocType(
    const _data  : String
  ) ;
  begin
    oNode.addChild(
      '', '#doctype', '#doctype', TNodeType.ntDocType, _data
    ) ;
  end ;

  procedure T_domContentHandler._Content(
    const _chars : String
  ) ;
  begin
    oNode.addChild(
      '', '#text', '#text', TNodeType.ntText, _chars
    )
  end ;

  procedure T_domContentHandler._Comment(
    const _chars : String
  ) ;
  begin
    oNode.addChild(
      '', '#comment', '#comment', TNodeType.ntComment, _chars
    ) ;
  end ;

  procedure T_domContentHandler._CDATA(
    const _chars : String
  ) ;
  begin
    oNode.addChild(
      '', '#cdata-section', '#cdata-section', TNodeType.ntCData, _chars
    ) ;
  end ;

//==============================================================================
// TGIS_XMLNode
//==============================================================================

  constructor TGIS_XMLNode.Create(
    const _doc   : TGIS_XMLDocument ;
    const _prnt  : TGIS_XMLNode ;
    const _uri   : String ;
    const _lname : String ;
    const _qname : String ;
    const _type  : TNodeType ;
    const _value : String
  ) ;
  begin
    inherited Create ;

    FDocument := _doc ;
    FParent   := _prnt ;
    FURI      := _uri ;
    FLName    := _lname ;
    FQname    := _qname ;
    FType     := _type ;
    FValue    := _value ;
    FReadOnly := False ;

    genLNameAndPrefix ;

    oNodes := TGIS_XMLNodeList.Create( Self ) ;
    oAttributes := TGIS_XMLNodeList.Create( Self ) ;
    oPrefixMap := T_xmlPrefixMap.Create ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_XMLNode.Destroy ;
    begin
      oNodes.Free ;
      oAttributes.Free ;
      oPrefixMap.Free ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_XMLNode.fget_Attribute(
    const _name  : String
  ) : OleVariant ;
  var
    i : Integer ;
  begin
    Result := Unassigned ;

    if NodeType <> TNodeType.ntElement then
      exit ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].NodeName  = _name ) or
         ( oAttributes[i].LocalName = _name ) then begin
        Result := oAttributes[i].NodeValue ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_XMLNode.fset_Attribute(
    const _name  : String ;
    const _value : OleVariant
  ) ;
  var
    att : IXMLNode ;
    i   : Integer ;
  begin
    if NodeType <> TNodeType.ntElement then
      exit ;

    att := nil ;
    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].NodeName  = _name ) or
         ( oAttributes[i].LocalName = _name ) then begin
        att := oAttributes[i] ;
        break ;
      end ;
    end ;

    if not assigned( att ) then begin
      att := TGIS_XMLNode.Create(
               FDocument,
               Self,
               FURI,
               _name,
               _name,
               TNodeType.ntAttribute,
               VarToString( _value )
             ) ;
      oAttributes.Add( att ) ;
    end ;

    att.NodeValue := _value ;

    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.fget_AttributeNodes : IXMLNodeList ;
  begin
    Result := oAttributes ;
  end ;

  function TGIS_XMLNode.fget_ChildNodes : IXMLNodeList ;
  begin
    Result := oNodes ;
  end ;

  function TGIS_XMLNode.fget_ChildValue(
    const _iname : OleVariant
  ) : OleVariant ;
  var
    node : IXMLNode ;
    name : String ;
    i    : Integer ;
  begin
    if not IsVariantString( _iname ) then begin
      i := VarToInt32( _iname ) ;
      Result := oAttributes[i].NodeValue ;
      exit ;
    end ;

    name := VarToString( _iname ) ;

    node := nil ;
    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].NodeName  = name ) or
         ( oAttributes[i].LocalName = name ) then begin
        node := oAttributes[i] ;
        break ;
      end ;
    end ;

    if not assigned( node ) then begin
      for i := 0 to oAttributes.Count - 1 do begin
        if ( oAttributes[i].NodeName  = name ) or
           ( oAttributes[i].LocalName = name ) then begin
          node := oAttributes[i] ;
          break ;
        end ;
      end ;
    end ;

    if assigned( node ) then
      Result := node.NodeValue ;
  end ;

  procedure TGIS_XMLNode.fset_ChildValue(
    const _iname : OleVariant ;
    const _value : OleVariant
  ) ;
  var
    node : IXMLNode ;
    name : String ;
    i    : Integer ;
  begin
    if not IsVariantString( _iname ) then begin
      i := VarToInt32( _iname ) ;
      oAttributes[i].NodeValue := _value ;
      exit ;
    end ;

    name := VarToString( _iname ) ;

    node := nil ;
    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].NodeName  = name ) or
         ( oAttributes[i].LocalName = name ) then begin
        node := oAttributes[i] ;
        break ;
      end ;
    end ;

    if not assigned( node ) then begin
      for i := 0 to oAttributes.Count - 1 do begin
        if ( oAttributes[i].NodeName  = name ) or
           ( oAttributes[i].LocalName = name ) then begin
          node := oAttributes[i] ;
          break ;
        end ;
      end ;
    end ;

    if assigned( node ) then
      node.NodeValue := _value ;
  end ;

  function TGIS_XMLNode.fget_Collection : IXMLNodeCollection ;
  begin
    // NOT IMPLEMENTED

    Result := nil ;
  end ;

  function TGIS_XMLNode.fget_HasChildNodes : Boolean ;
  begin
    if oNodes.Count > 0 then
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_XMLNode.fget_IsTextElement : Boolean ;
  begin
    if ( oNodes.Count = 1 ) and
       ( ( oNodes[0].NodeType = TNodeType.ntText  ) or
       ( oNodes[0].NodeType = TNodeType.ntCData ) ) then
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_XMLNode.fget_LocalName : String ;
  begin
    Result := FLName ;
  end ;

  function TGIS_XMLNode.fget_NamespaceURI : String ;
  begin
    Result := FURI ;
  end ;

  function TGIS_XMLNode.fget_NodeName : String ;
  begin
    Result := FQname ;
  end ;

  function TGIS_XMLNode.fget_NodeType : TNodeType ;
  begin
    Result := FType ;
  end ;

  function TGIS_XMLNode.fget_NodeValue : OleVariant ;
  begin
    case NodeType of
      TNodeType.ntElement         :
        begin
          if IsTextElement then
            Result := oNodes[0].NodeValue
          else
            Result := '' ;
        end ;
      TNodeType.ntAttribute      ,
      TNodeType.ntText           ,
      TNodeType.ntCData          ,
      TNodeType.ntProcessingInstr,
      TNodeType.ntComment        ,
      TNodeType.ntDocType        : Result := FValue ;
      else
        Result := '' ;
    end ;
  end ;

  procedure TGIS_XMLNode.fset_NodeValue(
    const _value : OleVariant
  ) ;
  begin
    case NodeType of
      TNodeType.ntElement        : Text := VarToString( _value ) ;
      TNodeType.ntAttribute      ,
      TNodeType.ntText           ,
      TNodeType.ntCData          ,
      TNodeType.ntProcessingInstr,
      TNodeType.ntComment        ,
      TNodeType.ntDocType        : FValue := VarToString( _value ) ;
    end ;

    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.fget_OwnerDocument : IXMLDocument ;
  begin
    Result := FDocument ;
  end ;

  function TGIS_XMLNode.fget_ParentNode : IXMLNode ;
  begin
    Result := FParent ;
  end ;

  function TGIS_XMLNode.fget_Prefix : String ;
  begin
    Result := FPrefix ;
  end ;

  function TGIS_XMLNode.fget_ReadOnly : Boolean ;
  begin
    Result := FReadOnly ;
  end ;

  procedure TGIS_XMLNode.fset_ReadOnly(
    const _value : Boolean
  ) ;
  begin
    FReadOnly := _value ;
  end ;

  function TGIS_XMLNode.fget_Text : String ;
  begin
    if IsTextElement then
      Result := oNodes[0].Text
    else
    if ( NodeType = TNodeType.ntText      ) or
       ( NodeType = TNodeType.ntAttribute ) or
       ( NodeType = TNodeType.ntCData ) then
      Result := FValue
    else
      Result := '' ;
  end ;

  procedure TGIS_XMLNode.fset_Text(
    const _value : String
  ) ;
  var
    node : TGIS_XMLNode ;
  begin
    if ( NodeType = TNodeType.ntElement ) and
       ( oNodes.Count = 0               ) then begin
      node := TGIS_XMLNode.Create(
        FDocument, Self, '', '#text', '#text', TNodeType.ntText, _value
      ) ;
      oNodes.Add( node ) ;
      FDocument.markModified ;
    end
    else
    if IsTextElement then begin
      oNodes[0].Text := _value ;
      FDocument.markModified ;
    end
    else
    if NodeType = TNodeType.ntText then begin
      FValue := _value ;
      FDocument.markModified ;
    end ;
  end ;

  function TGIS_XMLNode.fget_XML : String ;
  begin
    Result := indentXML( 0 ) ;
  end ;

  function TGIS_XMLNode.AddChild(
    const _name  : String ;
          _index : Integer = -1
  ) : IXMLNode ;
  var
    node  : TGIS_XMLNode ;
    left  : String ;
    right : String ;
    uri   : String ;
  begin
    Result := nil ;

    if _index < -1 then
      exit ;

    xml_colon_separate( _name, left, right ) ;
    uri := FindNamespaceURI( left ) ;

    node := TGIS_XMLNode.Create(
              FDocument, Self, uri, right, _name, TNodeType.ntElement, ''
            ) ;
    if _index = -1 then begin
      oNodes.Add( node ) ;
      Result := oNodes.Nodes[oNodes.Count-1] ;
    end
    else begin
      oNodes.Insert( _index, node ) ;
      Result := oNodes.Nodes[_index] ;
    end ;

    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.AddChild(
    const _name  : String ;
    const _uri   : String ;
          _prfx  : Boolean = False ;
          _index : Integer = -1
  ) : IXMLNode ;
  var
    node  : TGIS_XMLNode ;
    name  : String ;
    left  : String ;
    right : String ;
  begin
    Result := nil ;

    if _index < -1 then
      exit ;

    xml_colon_separate( _name, left, right ) ;

    if _prfx then begin
      left := FDocument.GeneratePrefix( Self ) ;
      DeclareNamespace( left, _uri ) ;
      name := left + ':' + right ;
    end
    else
      name := _name ;

    node := TGIS_XMLNode.Create(
              FDocument, Self, _uri, right, name, TNodeType.ntElement, ''
            ) ;
    if _index = -1 then begin
      oNodes.Add( node ) ;
      Result := oNodes.Nodes[oNodes.Count-1] ;
    end
    else begin
      oNodes.Insert( _index, node ) ;
      Result := oNodes.Nodes[_index] ;
    end ;

    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.CloneNode(
    _deep  : Boolean
  ) : IXMLNode ;
  begin
    // NOT IMPLEMENTED
    Result := nil ;
  end ;

  procedure TGIS_XMLNode.DeclareNamespace(
    const _prfx  : String ;
    const _uri   : String
  ) ;
  var
    node : TGIS_XMLNode ;
    name : String ;
  begin
    T_xmlPrefixMap( oPrefixMap ).Add( _prfx, _uri ) ;

    name := 'xmlns:' + _prfx ;
    node := TGIS_XMLNode( oAttributes.FindNode( name ) ) ;
    if assigned( node ) then
      exit ;

    addAttribute( NamespaceURI, name, name, 'CDATA', _uri ) ;

    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.FindNamespaceURI(
    const _prfx  : String
  ) : String ;
  begin
    Result := T_xmlPrefixMap( oPrefixMap ).FindURI( _prfx ) ;

    if length( Result ) = 0 then begin
      if assigned( ParentNode ) then
        Result := ParentNode.FindNamespaceURI( _prfx ) ;
    end ;
  end ;

  function TGIS_XMLNode.FindNamespaceDecl(
    const _uri   : String
  ) : IXMLNode ;
  begin
    Result := nil ;
    if T_xmlPrefixMap( oPrefixMap ).HasURI( _uri ) then
      Result := Self
    else
      if assigned( ParentNode ) then
        Result := ParentNode.FindNamespaceDecl( _uri ) ;
  end ;

  function TGIS_XMLNode.GetAttributeNS(
    const _name  : String ;
    const _uri   : String
  ) : OleVariant ;
  var
    i : Integer ;
  begin
    Result := '' ;

    if NodeType <> TNodeType.ntElement then
      exit ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].LocalName    = _name ) and
         ( oAttributes[i].NamespaceURI = _uri  ) then begin
        Result := oAttributes[i].NodeValue ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_XMLNode.HasAttribute(
    const _name  : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].NodeName  = _name ) or
         ( oAttributes[i].LocalName = _name ) then begin
        Result := True ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_XMLNode.HasAttribute(
    const _name  : String ;
    const _uri   : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].LocalName    = _name ) and
         ( oAttributes[i].NamespaceURI = _uri  ) then begin
        Result := True ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_XMLNode.NextSibling : IXMLNode ;
  var
    i : Integer ;
  begin
    i := oNodes.IndexOf( Self ) ;

    if i < oNodes.Count - 2 then
      Result := oNodes[i+1]
    else
      Result := nil ;
  end ;

  procedure TGIS_XMLNode.Normalize ;
  begin
    // NOT IMPLEMENTED
  end ;

  function TGIS_XMLNode.PreviousSibling : IXMLNode ;
  var
    i : Integer ;
  begin
    i := oNodes.IndexOf( Self ) ;

    if i > 0 then
      Result := oNodes[i-1]
    else
      Result := nil ;
  end ;

  procedure TGIS_XMLNode.Resync ;
  begin
    // NOT IMPLEMENTED
  end ;

  procedure TGIS_XMLNode.SetAttributeNS(
    const _name  : String ;
    const _uri   : String ;
    const _value : OleVariant
  ) ;
  var
    i : Integer ;
  begin
    if NodeType <> TNodeType.ntElement then
      exit ;

    for i := 0 to oAttributes.Count - 1 do begin
      if ( oAttributes[i].LocalName    = _name ) and
         ( oAttributes[i].NamespaceURI = _uri  ) then begin
        oAttributes[i].NodeValue := _value ;
        break ;
      end ;
    end ;

    FDocument.markModified ;
  end ;

  procedure TGIS_XMLNode.TransformNode(
    const _node  : IXMLNode ;
      var _out   : String
  ) ;
  begin
    // NOT IMPLEMENTED
  end ;

  procedure TGIS_XMLNode.TransformNode(
    const _node  : IXMLNode ;
    const _out   : IXMLDocument
  ) ;
  begin
    // NOT IMPLEMENTED
  end ;

  procedure TGIS_XMLNode.genLNameAndPrefix ;
  var
    b  : Boolean ;
    i  : Integer ;
  begin
    b := False ;
    for i := StringFirst to StringLast( FQname ) do begin
      if FQname[i] = ':' then begin
        b := True ;
        break ;
      end ;
    end ;

    if b then begin
      FPrefix := Copy(
                   FQname,
                   StringFirst,
                   i - StringFirst
                 ) ;
      FLName  := Copy(
                   FQname,
                   i + 1,
                   length( FQname ) - length( FPrefix ) - 1
                 ) ;
    end
    else begin
      FPrefix := '' ;
      FLName  := FQname ;
    end ;
  end ;

  function TGIS_XMLNode.indentChars(
    const _chars : String  ;
    const _bspc  : Boolean ;
    const _lvl   : Integer
  ) : String ;
  var
    sb : TStringBuilder ;
    i  : Integer ;
    k  : Integer ;
    l  : Integer ;

    procedure catch_up ;
    begin
      sb.Append( Copy( _chars, k, l ) ) ;
      k := i+1 ;
      l := 0 ;
    end ;

    function test_spec_char : Boolean ;
    begin
      case _chars[i] of
        '<'  :
          begin
            catch_up ;
            sb.Append( '&lt;' ) ;
            Result := True ;
          end ;
        '&'  :
          begin
            catch_up ;
            sb.Append( '&amp;' ) ;
            Result := True ;
          end ;
        '>'  :
          begin
            catch_up ;
            sb.Append( '&gt;' ) ;
            Result := True ;
          end ;
        '"'  :
          begin
            catch_up ;
            sb.Append( '&quot;' ) ;
            Result := True ;
          end ;
        '''' :
          begin
            catch_up ;
            sb.Append( '&apos;' ) ;
            Result := True ;
          end
          else
            Result := False ;
      end ;
    end ;

  begin
    if _bspc then begin
      Result := '' ;
      sb := TStringBuilder.Create ;
      try
        k := StringFirst ;
        l := 0 ;
        for i := StringFirst to StringLast( _chars ) do begin
          if not test_spec_char then
            inc( l ) ;
        end ;
        sb.Append( Copy( _chars, k, l ) ) ;
        Result := sb.ToString ;
      finally
        {$IFDEF DCC}
          sb.Free ;
        {$ENDIF}
      end ;
    end
    else
      Result := _chars ;
  end ;

  function TGIS_XMLNode.addChild(
    const _uri   : String ;
    const _lname : String ;
    const _qname : String ;
    const _type  : TNodeType ;
    const _value : String
  ) : TGIS_XMLNode ;
  var
    node : TGIS_XMLNode ;
  begin
    node := TGIS_XMLNode.Create(
              FDocument, Self, _uri, _lname, _qname, _type, _value
            ) ;
    oNodes.Add( node ) ;

    Result := TGIS_XMLNode( oNodes.Nodes[oNodes.Count-1] ) ;

    FDocument.markModified ;
  end ;

  procedure TGIS_XMLNode.addAttribute(
    const _uri    : String ;
    const _lname  : String ;
    const _qname  : String ;
    const _type   : String ;
    const _value  : String
  ) ;
  var
    att : TGIS_XMLNode ;
  begin
    att := TGIS_XMLNode.Create(
             FDocument,
             Self,
             _uri,
             _lname,
             _qname,
             TNodeType.ntAttribute,
             _value
           ) ;

    oAttributes.Add( att ) ;

    FDocument.markModified ;
  end ;

  procedure TGIS_XMLNode.setParentNode(
    const _node  : TGIS_XMLNode
  ) ;
  begin
    FParent := _node ;
    FDocument.markModified ;
  end ;

  function TGIS_XMLNode.indentXML(
    const _lvl   : Integer
  ) : String ;
  var
    sb   : TStringBuilder ;
    node : IXMLNode ;
    ind  : String ;
    i    : Integer ;
  begin
    Result := '' ;

    sb := TStringBuilder.Create ;
    try
      for i := 1 to _lvl do
        sb.Append( FDocument.NodeIndentStr ) ;
      ind := sb.ToString ;

      xmlSBClear( sb ) ;

      case NodeType of
        TNodeType.ntElement :
          begin
            sb.Append( ind ) ;
            sb.Append( '<' ) ;
            sb.Append( NodeName ) ;

            for i := 0 to AttributeNodes.Count - 1 do begin
              node := AttributeNodes[i] ;
              sb.Append( ' ' ) ;
              sb.Append( node.NodeName ) ;
              sb.Append( '="' ) ;
              sb.Append( String( node.NodeValue ) ) ;
              sb.Append( '"' ) ;
            end ;

            if ChildNodes.Count = 0 then begin
              sb.Append( '/>' ) ;
              sb.Append( #13#10 ) ;
            end
            else begin
              sb.Append( '>' ) ;
              if not IsTextElement then
                sb.Append( #13#10 ) ;
            end ;

            if ChildNodes.Count > 0 then begin

              for i := 0 to ChildNodes.Count - 1 do
                sb.Append(
                  TGIS_XMLNode( ChildNodes[i] ).indentXML( _lvl + 1 )
                ) ;

              if not IsTextElement then
                sb.Append( ind ) ;
              sb.Append( '</' ) ;
              sb.Append( NodeName ) ;
              sb.Append( '>' ) ;
              sb.Append( #13#10 ) ;
            end ;
          end ;
        TNodeType.ntText :
          begin
            sb.Append( indentChars( Text, True, _lvl ) ) ;
          end ;
        TNodeType.ntCData :
          begin
            sb.Append( ind ) ;
            sb.Append( '<![CDATA[ ' ) ;
            sb.Append( VarToString( NodeValue ) ) ;
            sb.Append( ' ]]>' ) ;
            sb.Append( #13#10 ) ;
          end ;
        TNodeType.ntProcessingInstr :
          begin
            sb.Append( ind ) ;
            sb.Append( '<?' ) ;
            sb.Append( NodeName ) ;
            sb.Append( ' ' ) ;
            sb.Append( VarToString( NodeValue ) ) ;
            sb.Append( '?>' ) ;
            sb.Append( #13#10 ) ;
          end ;
        TNodeType.ntComment :
          begin
            sb.Append( ind ) ;
            sb.Append( '<!-- ' ) ;
            sb.Append( indentChars( VarToString( NodeValue ), False, _lvl ) ) ;
            sb.Append( ' -->' ) ;
            sb.Append( #13#10 ) ;
          end ;
        TNodeType.ntDocument :
          begin
            sb.Append( '<?xml ' ) ;
            sb.Append( 'version="' ) ;
            sb.Append( FDocument.Version ) ;
            sb.Append( '" encoding="' ) ;
            sb.Append( FDocument.Encoding ) ;
            sb.Append( '" standalone="' ) ;
            sb.Append( FDocument.StandAlone ) ;
            sb.Append( '"?>' ) ;
            sb.Append( #13#10 ) ;

            for i := 0 to ChildNodes.Count - 1 do
              sb.Append(
                TGIS_XMLNode( ChildNodes[i] ).indentXML( 0 )
              ) ;
          end ;
        TNodeType.ntDocType :
          begin
            sb.Append( ind ) ;
            sb.Append( '<!DOCTYPE ' ) ;
            sb.Append( indentChars( VarToString( NodeValue ), False, _lvl ) ) ;
            sb.Append( '>' ) ;
            sb.Append( #13#10 ) ;
          end ;
      end ;

      Result := sb.ToString ;
    finally
      {$IFDEF DCC}
        sb.Free ;
      {$ENDIF}
    end ;
  end ;

//==============================================================================
// TGIS_XMLNodeList
//==============================================================================

  constructor TGIS_XMLNodeList.Create(
    const _owner : TGIS_XMLNode
  ) ;
  begin
    inherited Create ;

    oOwner := _owner ;

    FUpdateCount := 0 ;

    oNodes := TObjectList<TGIS_XMLNode>.Create ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_XMLNodeList.Destroy ;
    begin
      oNodes.Free ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_XMLNodeList.fget_Count : Integer ;
  begin
    Result := oNodes.Count ;
  end ;

  function TGIS_XMLNodeList.fget_Node(
    const _iname : OleVariant
  ) : IXMLNode ;
  var
    name : String ;
    idx  : Integer ;
  begin
    if IsVariantString( _iname ) then begin

      name := VarToString( _iname ) ;
      Result := FindNode( name ) ;

      if not assigned( Result ) then
        Result := oOwner.addChild(
          oOwner.NamespaceURI, name, name, TNodeType.ntElement, ''
        ) ;

      exit ;
    end ;

    idx := VarToInt32( _iname ) ;

    if ( idx >= 0 ) and ( idx < oNodes.Count ) then
      Result := oNodes[idx]
    else
      Result := nil ;
  end ;

  function TGIS_XMLNodeList.fget_UpdateCount : Integer ;
  begin
    Result := FUpdateCount ;
  end ;

  function TGIS_XMLNodeList.Add(
    const _node  : IXMLNode
  ) : Integer ;
  {$IFDEF DCC}
    var
      i : Integer ;
  {$ENDIF}
  begin
    {$IFDEF DCC}
      i := oNodes.Add( TGIS_XMLNode( _node ) ) ;
      oNodes[i].setParentNode( oOwner ) ;
      Result := i ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      TGIS_XMLNode( _node ).setParentNode( oOwner ) ;
      oNodes.Add( TGIS_XMLNode( _node ) ) ;
      Result := Count - 1 ;
    {$ENDIF}
  end ;

  procedure TGIS_XMLNodeList.BeginUpdate ;
  begin
    inc( FUpdateCount ) ;
  end ;

  procedure TGIS_XMLNodeList.Clear ;
  begin
    oNodes.Clear ;
  end ;

  function TGIS_XMLNodeList.Delete(
    const _index : Integer
  ) : Integer ;
  begin
    if ( _index >= 0 ) and ( _index < oNodes.Count ) then begin
      oNodes.Delete( _index ) ;
      Result := _index ;
    end
    else
      Result := -1 ;
  end ;

  function TGIS_XMLNodeList.Delete(
    const _name  : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].NodeName  = _name ) or
         ( oNodes[i].LocalName = _name ) then begin
        oNodes.Delete( i ) ;
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_XMLNodeList.Delete(
    const _name  : String ;
    const _uri   : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].LocalName    = _name ) and
         ( oNodes[i].NamespaceURI = _uri  ) then begin
        oNodes.Delete( i ) ;
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_XMLNodeList.EndUpdate ;
  begin
    if FUpdateCount > 0 then
      dec( FUpdateCount ) ;
  end ;

  function TGIS_XMLNodeList.First : IXMLNode ;
  begin
    if oNodes.Count > 0 then
      Result := oNodes[0]
    else
      Result := nil ;
  end ;

  function TGIS_XMLNodeList.FindNode(
    _name  : String
  ) : IXMLNode ;
  var
    node : IXMLNode ;
    i    : Integer ;
  begin
    node := nil ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].NodeName  = _name ) or
         ( oNodes[i].LocalName = _name ) then begin
        node := oNodes[i] ;
        break ;
      end ;
    end ;

    Result := node ;
  end ;

  function TGIS_XMLNodeList.FindNode(
    _name  : String ;
    _uri   : String
  ) : IXMLNode ;
  var
    node : IXMLNode ;
    i    : Integer ;
  begin
    node := nil ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].LocalName    = _name ) and
         ( oNodes[i].NamespaceURI = _uri  ) then begin
        node := oNodes[i] ;
        break ;
      end ;
    end ;

    if not assigned( node ) then begin
      for i := 0 to oNodes.Count - 1 do begin
        node := oNodes[i].ChildNodes.FindNode( _name, _uri ) ;
        if assigned( node ) then
          break ;
      end ;
    end ;

    Result := node ;
  end ;

  function TGIS_XMLNodeList.FindNode(
    _guid  : TGUID
  ) : IXMLNode ;
  begin
    // NOT IMPLEMENTED

    Result := nil ;
  end ;

  function TGIS_XMLNodeList.FindSibling(
    const _node  : IXMLNode ;
          _delta : Integer
  ) : IXMLNode ;
  var
    idx : Integer ;
  begin
    idx := IndexOf( _node ) ;

    idx := idx + _delta ;

    if ( idx >= 0 ) and ( idx < oNodes.Count ) then
      Result := oNodes[idx]
    else
      Result := nil ;
  end ;

  function TGIS_XMLNodeList.Get(
    _index : Integer
  ) : IXMLNode ;
  begin
    if ( _index >= 0 ) and ( _index < oNodes.Count ) then
      Result := oNodes[_index]
    else
      Result := nil ;
  end ;

  function TGIS_XMLNodeList.IndexOf(
    const _node  : IXMLNode
  ) : Integer ;
  begin
    Result := IndexOf( _node.LocalName ) ;
  end ;

  function TGIS_XMLNodeList.IndexOf(
    const _name  : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].NodeName  = _name ) or
         ( oNodes[i].LocalName = _name ) then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_XMLNodeList.IndexOf(
    const _name  : String ;
    const _uri   : String
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;

    for i := 0 to oNodes.Count - 1 do begin
      if ( oNodes[i].LocalName    = _name ) and
         ( oNodes[i].NamespaceURI = _uri  ) then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_XMLNodeList.Insert(
          _index : Integer ;
    const _node  : IXMLNode
  ) ;
  begin
    if ( _index >= 0 ) and ( _index < oNodes.Count ) then
      oNodes.Insert( _index, TGIS_XMLNode( _node ) ) ;
  end ;

  function TGIS_XMLNodeList.Last : IXMLNode ;
  begin
    if oNodes.Count > 0 then
      Result := oNodes[oNodes.Count-1]
    else
      Result := nil ;
  end ;

  function TGIS_XMLNodeList.Remove(
    const _node  : IXMLNode
  ) : Integer ;
  begin
    {$IFDEF DCC}
      Result := oNodes.Remove( TGIS_XMLNode( _node ) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result := IndexOf( _node ) ;
      oNodes.Remove( TGIS_XMLNode( _node ) ) ;
    {$ENDIF}
  end ;

  function TGIS_XMLNodeList.ReplaceNode(
    const _onode : IXMLNode ;
    const _nnode : IXMLNode
  ) : IXMLNode ;
  var
    idx : Integer ;
  begin
    idx := oNodes.Remove( TGIS_XMLNode( _onode ) ) ;

    if idx >= 0 then
      oNodes.Insert( idx, TGIS_XMLNode( _nnode ) )
    else
      idx := oNodes.Add( TGIS_XMLNode( _nnode ) ) ;

    Result := oNodes[idx] ;
  end ;

//==============================================================================
// TGIS_XMLDocument
//==============================================================================

  constructor TGIS_XMLDocument.Create ;
  begin
    inherited ;

    iPrefixGen  := 1 ;

    FActive     := True ;
    FModified   := False ;
    FVersion    := '1.0' ;
    FEncoding   := 'UTF-8' ;
    FStandalone := 'yes' ;
    FFileName   := '' ;
    FIndentStr  := '  ' ;
    FCompact    := False ;
    FParseOptions := [] ;

    FNode := TGIS_XMLNode.Create(
               Self, nil, '', '#document', '#document', TNodeType.ntDocument, ''
             ) ;

    markFresh ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_XMLDocument.Destroy ;
    begin
      if Assigned( FNode ) then
        FNode.Free ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_XMLDocument.fget_Active : Boolean ;
  begin
    Result := FActive ;
  end ;

  procedure TGIS_XMLDocument.fset_Active(
    const _value : Boolean
  ) ;
  begin
    if _value then begin
      if length( FFileName ) > 0 then
        LoadFromFile( FFileName )
      else
      if assigned( FXML ) then
        loadFromStrings
      else
        Reset ;
    end ;

    FActive := _value ;
  end ;

  function TGIS_XMLDocument.fget_ChildNodes : IXMLNodeList ;
  begin
    Result := FNode.ChildNodes ;
  end ;

  function TGIS_XMLDocument.fget_DocumentElement : IXMLNode ;
  var
    i : Integer ;
  begin
    Result := nil ;
    for i := 0 to FNode.ChildNodes.Count - 1 do begin
      if FNode.ChildNodes[i].NodeType = TNodeType.ntElement then begin
        Result := FNode.ChildNodes[i] ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_XMLDocument.fset_DocumentElement(
    const _node : IXMLNode
  ) ;
  begin
    FNode.ChildNodes.ReplaceNode( DocumentElement, _node ) ;
    markModified ;
  end ;

  function TGIS_XMLDocument.fget_DocumentNode : IXMLNode ;
  begin
    Result := FNode ;
  end ;

  function TGIS_XMLDocument.fget_Encoding : String ;
  begin
    Result := FEncoding ;
  end ;

  procedure TGIS_XMLDocument.fset_Encoding(
    const _value : String
  ) ;
  begin
    FEncoding := _value ;
  end ;

  function TGIS_XMLDocument.fget_FileName : String ;
  begin
    Result := FFileName ;
  end ;

  procedure TGIS_XMLDocument.fset_FileName(
    const _path  : String
  ) ;
  begin
    FFileName := _path ;
    FActive := False ;
  end ;

  function TGIS_XMLDocument.fget_Modified : Boolean ;
  begin
    Result := FModified ;
  end ;

  function TGIS_XMLDocument.fget_NodeIndentStr : String ;
  begin
    Result := FIndentStr ;
  end ;

  procedure TGIS_XMLDocument.fset_NodeIndentStr(
    const _value : String
  ) ;
  begin
    FIndentStr := _value ;
  end ;

  function TGIS_XMLDocument.fget_Options : TXMLDocOptions ;
  begin
    // NOT IMPLEMENTED
  end ;

  procedure TGIS_XMLDocument.fset_Options(
    const _value : TXMLDocOptions
  ) ;
  begin
    // NOT IMPLEMENTED
  end ;

  function TGIS_XMLDocument.fget_ParseOptions : TParseOptions ;
  begin
    Result := FParseOptions ;
  end ;

  procedure TGIS_XMLDocument.fset_ParseOptions(
    const _value : TParseOptions
  ) ;
  begin
    FParseOptions := _value ;
  end ;

  function TGIS_XMLDocument.fget_SchemaRef : String ;
  begin
    // NOT IMPLEMENTED

    Result := '' ;
  end ;

  function TGIS_XMLDocument.fget_StandAlone : String ;
  begin
    Result := FStandalone ;
  end ;

  procedure TGIS_XMLDocument.fset_StandAlone(
    const _value : String
  ) ;
  begin
    FStandalone := _value ;
  end ;

  function TGIS_XMLDocument.fget_Version : String ;
  begin
    Result := FVersion ;
  end ;

  procedure TGIS_XMLDocument.fset_Version(
    const _value : String
  ) ;
  begin
    FVersion := _value ;
  end ;

  function TGIS_XMLDocument.fget_XML : TStrings ;
  begin
    if not FActive then begin
      if assigned( FXML ) then
        Result := FXML
      else
        Result := nil ;
      exit ;
    end ;

    {$IFDEF DCC}
      if Assigned( FXML ) then
        FXML.Free ;
    {$ENDIF}

    FXML := TStringList.Create ;
    FXML.Text := FNode.XML ;
    Result := FXML ;
  end ;

  procedure TGIS_XMLDocument.fset_XML(
    const _xml : TStrings
  ) ;
  begin
    FXML := _xml ;
    FActive := False ;
  end ;

  function TGIS_XMLDocument.AddChild(
    const _name : String
  ) : IXMLNode ;
  var
    n : TGIS_XMLNode ;
    i : Integer ;
  begin
    n := TGIS_XMLNode.Create(
           Self, FNode, '', _name, _name, TNodeType.ntElement, ''
         ) ;
    i := FNode.ChildNodes.Add( n ) ;
    Result := FNode.ChildNodes[i] ;

    markModified ;
  end ;

  function TGIS_XMLDocument.AddChild(
    const _name  : String ;
    const _uri   : String
  ) : IXMLNode ;
  var
    left  : String ;
    right : String ;
    n     : TGIS_XMLNode ;
    i     : Integer ;
  begin
    xml_colon_separate( _name, left, right ) ;

    n := TGIS_XMLNode.Create(
           Self, FNode, _uri, right, _name, TNodeType.ntElement, ''
         ) ;
    i := FNode.ChildNodes.Add( n ) ;
    Result := FNode.ChildNodes[i] ;

    markModified ;
  end ;

  function TGIS_XMLDocument.CreateElement(
    const _name  : String ;
    const _uri   : String
  ) : IXMLNode ;
  var
    n     : TGIS_XMLNode ;
    left  : String ;
    right : String ;
  begin
    xml_colon_separate( _name, left, right ) ;

    n := TGIS_XMLNode.Create(
           Self, nil, _uri, right, _name, TNodeType.ntElement, ''
         ) ;
    Result := n ;
  end ;

  {$IFDEF JAVA}
    function TGIS_XMLDocument.CreateNode(
      const _name  : String
    ) : IXMLNode ;
    begin
      Result := CreateNode( _name, TNodeType.ntElement, '' ) ;
    end ;

    function TGIS_XMLDocument.CreateNode(
      const _name  : String ;
            _type  : TNodeType
    ) : IXMLNode ;
    begin
      Result := CreateNode( _name, _type, '' ) ;
    end ;

    function TGIS_XMLDocument.CreateNode(
      const _name  : String ;
      const _data  : String
    ) : IXMLNode ;
    begin
      Result := CreateNode( _name, TNodeType.ntElement, _data ) ;
    end ;
  {$ENDIF}

  {$IFDEF JAVA}
    function TGIS_XMLDocument.CreateNode(
      const _name  : String ;
            _type  : TNodeType ;
      const _data  : String
    ) : IXMLNode ;
  {$ELSE}
    function TGIS_XMLDocument.CreateNode(
      const _name  : String ;
            _type  : TNodeType = TNodeType.ntElement ;
      const _data  : String = ''
    ) : IXMLNode ;
  {$ENDIF}
  var
    n     : TGIS_XMLNode ;
    left  : String ;
    right : String ;
  begin
    n := nil ;

    case _type of
      TNodeType.ntElement,
      TNodeType.ntAttribute :
        begin
          xml_colon_separate( _name, left, right ) ;

          n := TGIS_XMLNode.Create(
            Self, nil, _data, right, _name, _type, ''
          ) ;
        end ;
      TNodeType.ntText :
        n := TGIS_XMLNode.Create(
          Self, nil, '', '#text', '#text', _type, _name
        ) ;
      TNodeType.ntCData :
        n := TGIS_XMLNode.Create(
          Self, nil, '', '#cdata-section', '#cdata-section', _type, _name
        ) ;
      TNodeType.ntComment :
        n := TGIS_XMLNode.Create(
          Self, nil, '', '#comment', '#comment', _type, _name
        ) ;
      TNodeType.ntProcessingInstr :
        n := TGIS_XMLNode.Create(
          Self, nil, '', _name, _name, _type, _data
        ) ;
    end ;

    Result := n ;
  end ;

  function TGIS_XMLDocument.GeneratePrefix(
    const _node  : IXMLNode
  ) : String ;
  var
    prfx : String ;
  begin
    while True do begin
      prfx := 'ns' + IntToStr( iPrefixGen ) ;
      inc( iPrefixGen ) ;
      if length( FNode.FindNamespaceURI( prfx ) ) = 0 then
        break ;
    end ;
  end ;

  function TGIS_XMLDocument.IsEmptyDoc : Boolean ;
  begin
    if ( not assigned( DocumentElement )      ) or
       ( DocumentElement.ChildNodes.Count > 0 ) then
      Result := False
    else
      Result := True ;
  end ;

  procedure TGIS_XMLDocument.LoadFromFile(
    const _path  : String
  ) ;
  var
    sax : T_domContentHandler ;
  begin
     sax := T_domContentHandler.Create( Self ) ;
     try
       sax.LoadFromFile( _path ) ;
       markFresh ;
       FFileName := _path ;
     finally
       {$IFDEF DCC}
         sax.Free ;
       {$ENDIF}
     end ;
  end ;

  {$IFDEF JAVA}
    procedure TGIS_XMLDocument.LoadFromStream(
      const _strm : TStream
    ) ;
    begin
      LoadFromStream( _strm, TXMLEncodingType.xetUnknown ) ;
    end ;
  {$ENDIF}

  {$IFDEF JAVA}
    procedure TGIS_XMLDocument.LoadFromStream(
      const _strm : TStream ;
            _enct : TXMLEncodingType
    ) ;
  {$ELSE}
    procedure TGIS_XMLDocument.LoadFromStream(
      const _strm : TStream ;
            _enct : TXMLEncodingType = TXMLEncodingType.xetUnknown
    ) ;
  {$ENDIF}
  var
    sax : T_domContentHandler ;
  begin
     sax := T_domContentHandler.Create( Self ) ;
     try
       sax.LoadFromStream( _strm ) ;
       markFresh ;
     finally
       {$IFDEF DCC}
         sax.Free ;
       {$ENDIF}
     end ;

     FFileName := '' ;
  end ;

  procedure TGIS_XMLDocument.LoadFromXML(
    const _xml : String
  ) ;
  var
    sax    : T_domContentHandler ;
    {$IFDEF JAVA}
      strm : TGIS_MemoryStream ;
    {$ELSE}
      strm : TMemoryStream ;
      swrt : TXMLStreamWriter ;
    {$ENDIF}
  begin
    {$IFDEF JAVA}
      strm := TGIS_MemoryStream.Create ;
      sax := T_domContentHandler.Create( Self ) ;
      try
        strm.Write( _xml.ToByteArray( Charset.forName( 'UTF-8' ) ) ) ;
        strm.Seek( 0, TSeekOrigin.soBeginning ) ;
        sax.LoadFromStream( strm ) ;
        markFresh ;
      finally
        strm.Close ;
      end ;
    {$ELSE}
      strm := TMemoryStream.Create ;
      try
        swrt := TXMLStreamWriter.Create( strm ) ;
        try
          swrt.Write( _xml ) ;
        finally
          {$IFDEF CLR}
            swrt.Flush ;
          {$ENDIF}
          {$IFDEF DCC}
            swrt.Free ;
          {$ENDIF}
        end ;

        {$IFDEF DCC}
          strm.Seek( 0, TSeekOrigin.soBeginning ) ;
        {$ENDIF}
        {$IFDEF CLR}
          strm.Seek( 0, SeekOrigin.Begin ) ;
        {$ENDIF}

        sax := T_domContentHandler.Create( Self ) ;
        try
          sax.LoadFromStream( strm ) ;
          markFresh ;
        finally
          {$IFDEF DCC}
            sax.Free ;
          {$ENDIF}
        end ;
      finally
        {$IFDEF DCC}
          strm.Free ;
        {$ENDIF}
      end ;
    {$ENDIF}

    FFileName := '' ;
  end ;

  procedure TGIS_XMLDocument.Refresh ;
  begin
    if length( FFileName ) = 0 then
      exit ;

    LoadFromFile( FFileName ) ;
  end ;

  procedure TGIS_XMLDocument.Resync ;
  begin
    // NOT IMPLEMENTED
  end ;

  procedure TGIS_XMLDocument.SaveToFile(
    const _path  : String
  ) ;
  var
    {$IFDEF JAVA}
      strm : FileOutputStream ;
    {$ELSE}
      strm : TFileStream ;
    {$ENDIF}
  begin
    {$IFDEF DCC}
      strm := TFileStream.Create( _path, fmCreate or fmOpenWrite ) ;
      try
        SaveToStream( strm ) ;
      finally
        strm.Free ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      strm := new TFileStream( _path, FileMode.&Create, FileAccess.Write ) ;
      try
        SaveToStream( strm ) ;
      finally
        strm.Close;
        disposeAndNil( strm ) ;
      end ;
    {$ENDIF}
    {$IFDEF JAVA}
      strm := new FileOutputStream( _path ) ;
      try
        SaveToStream( strm ) ;
      finally
        strm.close ;
      end ;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

  {$IFDEF JAVA}
  procedure TGIS_XMLDocument.SaveToStream(
    const _strm  : TStream
  ) ;
  var
    strm : ByteArrayOutputStream ;
  begin
    strm := new ByteArrayOutputStream() ;
    try
      SaveToStream( strm ) ;

      _strm.Write( strm.toByteArray ) ;
    finally
      strm.close ;
    end ;
  end ;
  {$ENDIF}

  procedure TGIS_XMLDocument.SaveToStream(
    const _strm  : TXMLOutputStream
  ) ;
  var
    sax  : TGIS_SAXWriter ;

    procedure proc_node( const _n : IXMLNode ) ;
    var
      atts : TGIS_SAXAttributes ;
      n    : IXMLNode ;
      name : String  ;
      val  : String ;
      ii   : Integer ;
    begin
      case _n.NodeType of
        TNodeType.ntElement :
          begin
            atts := TGIS_SAXAttributes.Create ;
            try
              for ii := 0 to _n.AttributeNodes.Count - 1 do begin
                n := _n.AttributeNodes[ii] ;
                atts.AddAttribute(
                  n.NamespaceURI, n.LocalName, n.NodeName,
                  'CDATA', String( n.NodeValue )
                ) ;
              end ;

              sax.StartElement(
                _n.NamespaceURI, _n.LocalName, _n.NodeName, atts
              ) ;
            finally
              FreeObject( atts ) ;
            end ;

            for ii := 0 to _n.ChildNodes.Count - 1 do
              proc_node( _n.ChildNodes[ii] ) ;

            sax.EndElement( _n.NamespaceURI, _n.LocalName, _n.NodeName ) ;
          end ;
        TNodeType.ntText :
          begin
            val := _n.Text ;
            sax._Content( val ) ;
          end ;
        TNodeType.ntCData :
          begin
            val := VarToString( _n.NodeValue ) ;
            sax._CDATA( val ) ;
          end ;
        TNodeType.ntProcessingInstr :
          begin
            name := _n.NodeName ;
            val  := VarToString( _n.NodeValue ) ;
            sax.ProcessingInstruction( name, val ) ;
          end ;
        TNodeType.ntComment :
          begin
            val := VarToString( _n.NodeValue ) ;
            sax._Comment( val ) ;
          end ;
        TNodeType.ntDocument :
          begin
            sax._XMLDecl( Version, Encoding, StandAlone ) ;

            for ii := 0 to _n.ChildNodes.Count - 1 do
              proc_node( _n.ChildNodes[ii] ) ;
          end ;
        TNodeType.ntDocType :
          begin
            val := VarToString( _n.NodeValue ) ;
            sax._DocType( val ) ;
          end ;
      end ;
    end ;

  begin
    sax := TGIS_SAXWriter.Create( _strm ) ;
    try
      sax.SaveCompact := SaveCompact ;
      sax.IndentString := NodeIndentStr ;
      proc_node( FNode ) ;
      sax.EndDocument;
    finally
      {$IFDEF DCC}
        sax.Free ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_XMLDocument.SaveToXML(
    var _xml : String
  ) ;
  var
    strm : TMemoryStream ;
    lst  : TStringList ;
  begin
    strm := TMemoryStream.Create ;
    try
      SaveToStream( strm ) ;
      strm.Position := 0 ;
      lst := TStringList.Create ;
      try
        lst.LoadFromStream( strm ) ;
        _xml := lst.Text ;
      finally
        FreeObject( lst ) ;
      end;
    finally
      FreeObject( strm ) ;
    end ;
  end ;

  procedure TGIS_XMLDocument.clear ;
  begin
    {$IFDEF DCC}
      if Assigned( FNode ) then
        FNode.Free ;
    {$ENDIF}

    FNode := nil ;
  end ;

  procedure TGIS_XMLDocument.markFresh ;
  begin
    FActive := True ;
    FModified := False ;
  end ;

  procedure TGIS_XMLDocument.markModified ;
  begin
    FModified := True ;
  end ;

  procedure TGIS_XMLDocument.loadFromStrings ;
  var
    {$IFDEF JAVA}
      strm : TGIS_MemoryStream ;
      eol  : TBytes ;
      i    : Integer ;
    {$ELSE}
      strm : TMemoryStream ;
    {$ENDIF}
  begin
    {$IFDEF JAVA}
      eol := new Byte[2] ;
      eol[0] := 13 ;
      eol[1] := 10 ;

      strm := TGIS_MemoryStream.Create ;
      try
        for i := 0 to FXML.Count - 1 do begin
          strm.Write(
            FXML.Strings[i].ToByteArray( Charset.forName( 'UTF-8' ) )
          ) ;
          if i < FXML.Count - 1 then
            strm.Write( eol ) ;
        end ;
        strm.Seek( 0, TSeekOrigin.soBeginning ) ;
        LoadFromStream( strm ) ;
      finally
        strm.Close ;
      end ;
    {$ELSE}
      strm := TMemoryStream.Create ;
      try
        FXML.SaveToStream( strm ) ;
        LoadFromStream( strm ) ;
      finally
        {$IFDEF DCC}
          strm.Free ;
        {$ENDIF}
      end ;
    {$ENDIF}

    FFileName := '' ;
  end ;

  procedure TGIS_XMLDocument.Reset ;
  begin
    if not IsEmptyDoc then
      clear ;

    FNode := TGIS_XMLNode.Create(
               Self, nil, '', '#document', '#document', TNodeType.ntDocument, ''
             ) ;

    markFresh ;
  end ;

//=================================== END ======================================
end.

