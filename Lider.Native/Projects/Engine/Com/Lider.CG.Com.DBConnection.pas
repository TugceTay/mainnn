unit Lider.CG.Com.DBConnection;

interface

uses
  Classes,
  SysUtils, {XMLDoc, XMLIntf,}
  NativeXml,
  Variants,
  Dialogs;

type
  TDBConnections = class(TObject)
  private
    FFileName: string;
    FConnectionCount: Integer;
//    FXMLDoc           : IXMLDocument;
    FXMLDoc: TNativeXml;
    FCurrentConName: string;
    function GetConnectionCount: Integer;
    procedure OpenConnectionFile(FileName: string);
  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    function GetConnectionString(ConnectionName: string): string;
    procedure PopulateConnections(List: TStrings);
    procedure PopulateTables(connectionName: string; List: TStrings);
    procedure PopulateColumns(connectionName: string; TableName: string; List: TStrings);
    procedure GetSQLString(connectionName, datasetName: string; sql: TStrings);
    function GetTableCount(connectionName: string): Integer;
    function GetColumnCount(connectionName, TableName: string): Integer;
    function GetTableDisplayName(connectionName: string; TableName: string): string;
    function GetTableName(connectionName: string; TableDisplayName: string): string;
    function GetColumnDisplayName(connectionName: string; TableName: string;
      columnName: string): string;
    function GetColumnName(connectionName: string; TableName: string;
      columnDisplayName: string): string;
    property ConnectionCount: Integer read GetConnectionCount;
    property CurrentConName: string read FCurrentConName write FCurrentConName;
  end;

implementation

{ TDBConnections }

constructor TDBConnections.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  if FFileName <> '' then
    OpenConnectionFile(FFileName);
  FCurrentConName := '';
end;

destructor TDBConnections.Destroy;
begin
//  if FXMLDoc<>Nil then FXMLDoc := Nil;
  if FXMLDoc <> Nil then
    FXMLDoc.Free;
  inherited;
end;

function TDBConnections.GetColumnCount(connectionName, TableName: string): Integer;
begin
  if connectionName = '' then
    connectionName := FCurrentConName;
  if (connectionName = '') or (TableName = '') then
    exit;
end;

function TDBConnections.GetColumnDisplayName(connectionName, TableName,
  columnName: string): string;
var
//  cNode,node  : IXMLNode;
  cNode, node, fNode: TXMLNode;
  i: Integer;
begin
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;
  Result := '';
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode <> nil then
    begin
//      node := cNode.ChildNodes.FindNode('Tablolar');
      node := cNode.FindNode('Tablolar');
      if node <> nil then
      begin
        //node := node.ChildNodes.FindNode(TableName);
        node := node.FindNode(TableName);
        if node <> nil then
        begin
//          for i:=1 to node.AttributeNodes.Count-1 do begin
          for i := 1 to node.AttributeCount - 1 do
          begin
//            if node.AttributeNodes[i].NodeName = columnName then begin
            if node.AttributeName[i] = columnName then
            begin
//              Result := node.AttributeNodes[i].NodeValue ;
              Result := node.AttributeValue[i];
              Break;
            end;
          end;  //for i
        end
        else
        begin
          cNode := FindNode(connectionName);
          if cNode <> nil then
          begin
//            node := cNode.ChildNodes.FindNode('Sorgular');
            node := cNode.FindNode('Sorgular');
            if node <> nil then
            begin
//              node := node.ChildNodes.FindNode(TableName);
              node := node.FindNode(TableName);
              if node <> nil then
              begin
//                for i:=1 to node.AttributeNodes.Count-1 do begin
                for i := 1 to node.AttributeCount - 1 do
                begin
//                  if node.AttributeNodes[i].NodeName = columnName then begin
                  if node.AttributeName[i] = columnName then
                  begin
//                    Result := node.AttributeNodes[i].NodeValue ;
                    Result := node.AttributeValue[i];
                    Break;
                  end;
                end;  //for i
              end;
            end;
          end;
        end;
      end;
    end;
  end; //With FXMLDoc
end;

function TDBConnections.GetColumnName(connectionName, TableName,
  columnDisplayName: string): string;
var
//  cNode,node  : IXMLNode;
  cNode, node, fNode: TXMLNode;
  i: Integer;
begin
  if connectionName = '' then
    connectionName := FCurrentConName;
  if (connectionName = '') or (TableName = '') or (columnDisplayName = '') then
    exit;
  Result := '';
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode <> nil then
    begin
//      node := cNode.ChildNodes.FindNode('Tablolar');
      node := cNode.FindNode('Tablolar');
      if node <> nil then
      begin
//        node := node.ChildNodes.FindNode(TableName);
        node := node.FindNode(TableName);
        if node <> nil then
        begin
          result := node.AttributeName[node.AttributeIndexByname(columnDisplayName)];

         { for i:=1 to node.AttributeCount-1 do begin
//            if node.AttributeNodes[i].NodeValue = columnDisplayName then begin
            if node.AttributeValue[i]= columnDisplayName then begin
//              Result := node.AttributeNodes[i].NodeName ;
              Result := node.AttributeName[i];

              Break;
            end;
          end;  //for i   }
        end
        else
        begin
          cNode := FindNode(connectionName);
          if cNode <> nil then
          begin
//            node := cNode.ChildNodes.FindNode('Sorgular');
            node := cNode.FindNode('Sorgular');
            if node <> nil then
            begin
//              node := node.ChildNodes.FindNode(TableName);
              node := node.FindNode(TableName);
              if node <> nil then
              begin
                result := node.AttributeName[node.AttributeIndexByname(columnDisplayName)];
//                for i:=1 to node.AttributeNodes.Count-1 do begin
                {for i:=1 to node.AttributeCount-1 do begin
//                  if node.AttributeNodes[i].NodeValue = columnDisplayName then begin
                  if node.AttributeValue[i]= columnDisplayName then begin
//                    Result := node.AttributeNodes[i].NodeName ;
                    Result := node.AttributeName[i];
                    Break;
                  end;
                end;  //for i}
              end;
            end;
          end;
        end;
      end;
    end;
  end; //With FXMLDoc

end;

function TDBConnections.GetConnectionCount: Integer;
var
//  node  : IXMLNode;
  node: TXMLNode;
begin
  Result := 0;
//  Result:=FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes.Count;
  Result := FXMLDoc.Root.FindNode('Connections').NodeCount; //li2016
  FConnectionCount := Result;
end;

function TDBConnections.GetConnectionString(ConnectionName: string): string;
var
  i: Integer;
//  node  : IXMLNode;
  node: TXMLNode;
begin
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;
  Result := '';
  if FXMLDoc <> Nil then
  begin
    with FXMLDoc.Root do
    begin //li2016
  //    for i:=0 to FindNode('Connections').ChildNodes.Count-1 do begin
      for i := 0 to FindNode('Connections').NodeCount - 1 do
      begin
  //      node := FindNode('Connections').ChildNodes[i];
        node := FindNode('Connections').Nodes[i];
  //      if Trim(node.NodeName) = Trim(ConnectionName) then begin
        if Trim(node.Name) = Trim(ConnectionName) then
        begin
  //        result := node.Attributes['ConnectionString'];
          result := node.AttributeValueByNameWide['ConnectionString']; //li2016
          break;
        end;
      end; //for i
    end; //With FXMLDoc
  end;
end;

function TDBConnections.GetTableCount(connectionName: string): Integer;
var
//  cNode,node  : IXMLNode;
  cNode, fNode, node: TXMLNode;
begin
  Result := 0;
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;

//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  fNode := FXMLDoc.Root.FindNode('Connections'); // li2016
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    node := cNode.ChildNodes[0];
    node := cNode.Nodes[0];
//    Result := node.ChildNodes.Count;
    Result := node.NodeCount;
  end; //With FXMLDoc

end;

function TDBConnections.GetTableDisplayName(connectionName, TableName: string): string;
var
//  cNode,node  : IXMLNode;
  cNode, fNode, node: TXMLNode;
begin
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    node := cNode.ChildNodes[0].ChildNodes.FindNode(TableName);
    node := cNode.Nodes[0].FindNode(TableName);
//    REsult := node.AttributeNodes[0].NodeValue ;
    REsult := node.AttributeValue[0];
  end; //With FXMLDoc
end;

function TDBConnections.GetTableName(connectionName, TableDisplayName: string): string;
var
//  cNode,node  : IXMLNode;
  cNode, fNode, node: TXMLNode;
  i: Integer;
begin
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes  do begin
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    for i:=0 to cNode.ChildNodes[0].ChildNodes.Count-1 do begin
    for i := 0 to cNode.Nodes[0].NodeCount - 1 do
    begin
//      node := cNode.ChildNodes[0].ChildNodes[i];
      node := cNode.Nodes[0].Nodes[i];
//      if node.AttributeNodes[0].NodeValue = TableDisplayName then begin
      if node.AttributeValue[0] = TableDisplayName then
      begin
//        REsult := node.NodeName ;
        REsult := node.Name;
        Break;
      end;
    end; //for i
  end; //With FXMLDoc
end;

procedure TDBConnections.GetSQLString(connectionName, datasetName: string; sql: TStrings);
var
  cNode, datasetsNode,
//  node        : IXMLNode;
node, fNode: TXMLNode;
  i, c: Integer;
begin
  sql.Clear;
  if ConnectionName = '' then
    ConnectionName := FCurrentConName;
  if ConnectionName = '' then
    exit;
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes  do begin
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    datasetsNode := cNode.ChildNodes.FindNode('Tablolar');
    datasetsNode := cNode.FindNode('Tablolar');
    if datasetsNode = nil then
      exit;

//    for i:=0 to datasetsNode.ChildNodes.Count-1 do begin
    for i := 0 to datasetsNode.NodeCount - 1 do
    begin
//      node := datasetsNode.ChildNodes[i];
      node := datasetsNode.Nodes[i];
//      if node.NodeName = datasetName then begin
      if node.Name = datasetName then
      begin
        sql.Add('Select');
//        for c:=1 to node.AttributeNodes.Count-1 do begin
        for c := 1 to node.AttributeCount - 1 do
        begin
          if c > 1 then
            sql.add(',');
//           sql.Add(node.AttributeNodes[c].nodeName);
          sql.Add(node.AttributeName[c]);
        end;
//        sql.Add('from '+node.NodeName );
        sql.Add('from ' + node.Name);
        Break;
      end; //if node.NodeName = datasetName
    end; //for i

    if sql.Count = 0 then
    begin
//      datasetsNode := cNode.ChildNodes.FindNode('Sorgular');
      datasetsNode := cNode.FindNode('Sorgular');
      if datasetsNode = nil then
        exit;
//      for i:=0 to datasetsNode.ChildNodes.Count-1 do begin
      for i := 0 to datasetsNode.NodeCount - 1 do
      begin
//        node := datasetsNode.ChildNodes[i];
        node := datasetsNode.Nodes[i];
//        if node.NodeName = datasetName then begin
        if node.Name = datasetName then
        begin
//          sql.LoadFromFile( changeFileExt(node.AttributeNodes[0].NodeValue,'.sql') );
          sql.LoadFromFile(ExtractFilepath(Self.FFileName) + 'Sorgular\' +
            datasetName + '.sql');
          Break;
        end;
      end; //for i
    end; //if sql.Count = 0 then begin

  end; //With FXMLDoc
end;

procedure TDBConnections.OpenConnectionFile(FileName: string);
begin
  if not FileExists(FileName) then
    Exit;
//  FXMLDoc :=  TXMLDocument.Create(nil);
  FXMLDoc := TNativeXml.Create(nil); //li2016
{  FXMLDoc.Active := False;
  FXMLDoc.XML.Text := '';
  FXMLDoc.Active := True;
  }
  FXMLDoc.LoadFromFile(FileName);
end;

procedure TDBConnections.PopulateColumns(connectionName: string; TableName:
  string; List: TStrings);
var
  i: Integer;
//  cNode,node  : IXMLNode;
  cNode, node, fNode: TXMLNode;
begin
  if List = Nil then
    List := TStringList.Create
  else
    List.Clear;
  fNode := FXMLDoc.Root.FindNode('Connections'); //li2016
//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  with fNode do
  begin
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    node := cNode.ChildNodes.FindNode('Tablolar').ChildNodes.FindNode(TableName);
    node := cNode.FindNode('Tablolar').FindNode(TableName);
    if node <> Nil then
    begin
//      for i:=1 to node.AttributeNodes.Count-1 do begin
      for i := 1 to node.AttributeCount - 1 do
      begin
//        List.Add(VarToStr(node.AttributeNodes[i].NodeValue));
        List.Add(VarToStr(node.AttributeValue[i]));
      end;
    end;
    if List.Count = 0 then
    begin
//      node := cNode.ChildNodes.FindNode('Sorgular').ChildNodes.FindNode(TableName);
      node := cNode.FindNode('Sorgular').FindNode(TableName);
      if node <> Nil then
      begin
//        for i:=1 to node.AttributeNodes.Count-1 do begin
        for i := 1 to node.AttributeCount - 1 do
        begin
//          List.Add(VarToStr(node.AttributeNodes[i].NodeValue));
          List.Add(VarToStr(node.AttributeValue[i]));
        end;
      end;
    end;
  end; //With FXMLDoc
end;

procedure TDBConnections.PopulateConnections(List: TStrings);
var
  i: Integer;
  node, cNode: TXMLNode;
begin
  if List = Nil then
    List := TStringList.Create
  else
    List.Clear;
  cNode := FXMLDoc.Root.FindNode('Connections'); //li2016
  if cNode = Nil then
    exit;
  for i := 0 to cNode.NodeCount - 1 do
  begin
//    node := cNode.ChildNodes[i];
    node := cNode.Nodes[i];
//    List.Add(Trim(node.NodeName)) ;
    List.Add(Trim(node.Name));
  end; //for i

end;

procedure TDBConnections.PopulateTables(connectionName: string; List: TStrings);
var
  i, c: Integer;
//  cNode,node  : IXMLNode;
  cNode, node: TXMLNode;
  tableName: string;
begin
  if List = Nil then
    List := TStringList.Create
  else
    List.Clear;

//  With FXMLDoc.ChildNodes.FindNode('Connections').ChildNodes do begin
  with FXMLDoc.Root.FindNode('Connections') do
  begin //li2016
    cNode := FindNode(connectionName);
    if cNode = nil then
      exit;
//    node := cNode.ChildNodes.FindNode('Tablolar');
    node := cNode.FindNode('Tablolar');
    if Node <> Nil then
    begin
//      for c:=0 to node.ChildNodes.Count-1 do begin //tablolar
      for c := 0 to node.NodeCount - 1 do
      begin //tablolar
        //AttributeNodes[0] = TableDisplayName
//        if VarToStr(node.ChildNodes[c].AttributeNodes[0].NodeValue) = '' then
        if VarToStr(node.Nodes[c].AttributeValue[0]) = '' then
//          tableName := VarToStr(node.ChildNodes[c].NodeName)
          tableName := VarToStr(node.Nodes[c].Name)
        else
//          tableName := VarToStr(node.ChildNodes[c].AttributeNodes[0].NodeValue);
          tableName := VarToStr(node.Nodes[c].AttributeValue[0]);
        List.Add(tableName);
      end;  //For c
    end;

  end; //With FXMLDoc

end;

end.


