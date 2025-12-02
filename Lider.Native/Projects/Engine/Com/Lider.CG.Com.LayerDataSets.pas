unit Lider.CG.Com.LayerDataSets;

{$I Lider.CG.Com.Component.inc}

interface

uses
  Classes,
  SysUtils, {XMLDoc, XMLIntf,}
  NativeXml,
  Variants,
  Dialogs,
  Lider.CG.Com.GIS,
  Lider.CG.Com.DBConnection;

type
  TLayerDatasets = class(TObject)
  private
    FFileName: WideString;
//    FXMLDoc           : IXMLDocument;
    FXMLDoc: TNativeXml;
    FLayer: TlicgBaseLayer;
    FConnectionMan: TDBConnections;
//    FConnection       : TADOConnection;
    //function GetConnectionCount:Integer;
    procedure OpenDataSetFile(FileName: WideString);
  public
    constructor Create(FileName: WideString);
    destructor Destroy; override;
    procedure PopulateLayersDataSets(List: TStrings; Filter: WideString = '');
//    procedure PopulateLayersDataSetFields(layerName,datasetName:String;List:TStrings);
    procedure GetDatasetSql(dataSetName: WideString; var sql: TStrings; var
      ConnectionString: WideString);
    procedure GetRelFields(datasetName: WideString; var dbFields, layerFields:
      TStringList);
    property Layer: TlicgBaseLayer read FLayer write FLayer;
    property ConnectionManager: TDBConnections read FConnectionMan;
  end;

implementation

uses
  ComObj;

{ TLayerDatasets }

constructor TLayerDatasets.Create(FileName: WideString);
var
  dbConnfileName: WideString;
begin
  inherited Create;
  FFileName := FileName;

  if FFileName <> '' then
    OpenDatasetFile(FFileName);

  dbConnfileName := ExtractFilePath(Self.FFileName);
  dbConnfileName := dbConnfileName + 'DBConnections.xml';
  FConnectionMan := TDBConnections.Create(dbConnfileName);



  //FConnection := TADOConnection.Create(nil);

  //FConnection.LoginPrompt := false;
end;

destructor TLayerDatasets.Destroy;
begin
//  if FXMLDoc <> Nil then FXMLDoc := Nil;
  if FXMLDoc <> Nil then
    FXMLDoc.Free;

  //if FConnection <> Nil then FConnection.Free;

  if FConnectionMan <> Nil then
    FConnectionMan.Free;

  inherited;
end;



{function TLayerDatasets.GetConnectionCount: Integer;
Var
  node  : IXMLNode;
begin
  Result := 0;
  Result:=FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes.Count;
  FConnectionCount := Result;
end;
}

procedure TLayerDatasets.OpenDataSetFile(FileName: WideString);
begin
  if not FileExists(FileName) then
    Exit;
  FXMLDoc := TNativeXml.Create(nil); // TXMLDocument.Create(nil); li2016

//  FXMLDoc.Active := False;
//  FXMLDoc.XML.Text := '';
//  FXMLDoc.Active := True;
  FXMLDoc.LoadFromFile(FileName);

end;

procedure TLayerDatasets.PopulateLayersDataSets(List: TStrings; Filter: WideString = '');
var
  i, cpos: Integer;
//  node,cNode  : IXMLNode;
  node, cNode: TXMLNode;
  filterChar: string;
  isFilter: Boolean;
begin
  if Filter <> '' then
  begin
    cpos := StrToInt(Copy(Filter, 1, Pos(',', Filter) - 1));
    filterChar := Copy(Filter, Pos(',', Filter) + 1, 1);
  end;
  isFilter := True;
  if (Layer <> Nil) and (FXMLDoc <> Nil) then
  begin
    if List = Nil then
      List := TStringList.Create
    else
      List.Clear;
//    cNode := FXMLDoc.ChildNodes.FindNode('Datasets');

    cNode := FXMLDoc.Root.FindNode('Datasets'); //li2016
//    cNode := cNode.ChildNodes.FindNode(Trim(FLayer.DisplayName));
    cNode := cNode.FindNode(Trim(FLayer.DisplayName));

    if cNode = Nil then
      exit;


//    for i:=0 to cNode.ChildNodes.Count-1 do begin
    for i := 0 to cNode.NodeCount - 1 do
    begin
      //node := cNode.ChildNodes[i];
      node := cNode.Nodes[i];
      if Filter <> '' then
      begin

//        isFilter := (Copy(VarToStr(Node.AttributeNodes[0].NodeValue),cpos,1) = filterChar);
        isFilter := (Copy(VarToStr(Node.AttributeValue[0]), cpos, 1) = filterChar);
      end;
      if isFilter then
      begin
        //List.Add(Trim(node.NodeName)) ;
        List.Add(Trim(node.Name));
      end;
    end; //for i
  end;

end;

procedure TLayerDatasets.GetRelFields(datasetName: WideString; var dbFields,
  layerFields: TStringList);
var
  i, j: Integer;
  cNode, node: TXmlNode; //IXMLNode;
  f: string;
begin
  if Layer <> Nil then
  begin

    if dbFields = Nil then
      dbFields := TStringList.Create
    else
      dbFields.Clear;

    if layerFields = Nil then
      layerFields := TStringList.Create
    else
      layerFields.Clear;

//    cNode := FXMLDoc.ChildNodes.FindNode('Datasets');

    cNode := FXMLDoc.Root.FindNode('Datasets'); //li2016
//    cNode := cNode.ChildNodes.FindNode(Trim(FLayer.DisplayName));
    cNode := cNode.FindNode(Trim(FLayer.DisplayName));

    if cNode = Nil then
      exit;

//    for i:=0 to cNode.ChildNodes.Count-1 do begin
    for i := 0 to cNode.NodeCount - 1 do
    begin
//      node := cNode.ChildNodes[i];
      node := cNode.Nodes[i];
//      if datasetName = node.NodeName then begin
      if datasetName = node.Name then
      begin
        //j = 0 Options item
//        for j:=1 to node.AttributeNodes.Count-1 do begin
        for j := 1 to node.AttributeCount - 1 do
        begin
//           dbFields.Add ( node.AttributeNodes[j].NodeName ) ;
          dbFields.Add(node.AttributeName[j]);
//           f :=  VarToStr( node.AttributeNodes[j].NodeValue ) ;
          f := VarToStr(node.AttributeValue[j]);
          f := Copy(f, pos('.', f) + 1, length(f));
          layerFields.Add(f);
        end;
      end;
    end; //for i
  end;
end;

procedure TLayerDatasets.GetDatasetSql(dataSetName: WideString; var sql:
  TStrings; var ConnectionString: WideString);
var
  connectionName: string;
  dataset: string;
begin
  connectionName := Copy(dataSetName, 1, pos('.', dataSetName) - 1);
  dataset := Copy(dataSetName, pos('.', dataSetName) + 1, length(dataSetName));

  if FConnectionMan <> Nil then
  begin
    connectionString := FConnectionMan.GetConnectionString(connectionName);

  end; //if FConnectionMan<>Nil
  if connectionString <> '' then
  begin
    FConnectionMan.GetSQLString(connectionName, dataset, SQL);
  end; // if connectionString <> ''

end;


{
procedure TLayerDatasets.PopulateLayersDataSetFields(layerName,datasetName:String;List: TStrings);
Var
  i           : Integer;
  node,
  dSetNode,
  cNode  : IXMLNode;

  dataSource  : Tdatasource;

begin
  if Layer <> Nil then begin
    if List = Nil then
      List := TStringList.Create
    Else
      List.Clear;
    cNode := FXMLDoc.ChildNodes.FindNode('Datasets');
    cNode := cNode.ChildNodes.FindNode(layerName);
    if cNode = Nil then exit;
    dSetNode := cNode.ChildNodes.FindNode(datasetName);
    if dSetNode = Nil then exit;

    for i:=0 to dSetNode.ChildNodes.Count-1 do begin
      node := dSetNode.ChildNodes[i];
      List.Add(node.NodeName);
    end; //for i
  end;
end;
  }

end.


