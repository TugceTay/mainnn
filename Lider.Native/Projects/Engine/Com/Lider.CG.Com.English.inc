  { (c) 2003 EzGis - EzGis main constants }

  { Command line commands }
  SCommand = 'Command: ';

  SFeature= 'Feature';
  SRange= 'Range';
  SFrequency= 'Frequency';
  SSymbolCaption = 'ELEMENT %d';
  SScaleFmt = 'Scale = %.3f %%';
  STranslateFmt = 'DX = %.3f, DY = %.3f, < %.3f';
  SRotateFmt = '< %.3f';
  //SNewEntityInfo = '< %.*n, DX = %.*n, DY = %.*n, Area = %.*n, Dist = %.*n';
  SNewEntityInfo = 'Angle = %.*n, Area = %.*n, Dist = %.*n';
  SIsInvalidFloatValue = 'Invalid floating point value %s';
  SIsInvalidIntegerValue = 'Invalid integer value %s';
  SIsInvalidBoolValue = 'Invalid boolean value %s';
  SLauncher= 'Launcher';
  SDragDropClick= 'Click desired entity that you wish to drag';
  STextPivotPt= 'Define pivot point';
  STextRefPtFirst= 'Define location of text';
  SIndivClickToEdit= 'Click desired entity on the map to show/edit information';
  SEntityClickCaption= 'Click an entity on the map';
  SSymbolTextCaption= 'Symbol text';
  SSymbolText= 'Text :';
  SUnconnectedDrawBox= 'The Component is not connected to a TEzBaseDrawBox';
  SMsgConfirm= 'Confirm';
  SMsgError= 'Error';
  SMsgWarning= 'Warning';
  SMsgInfo= 'Information';
  SWrongBuffer= 'Cannot create a buffer from this polyline';
  SCannotCreateBuffer= 'Cannot create buffer for this kind of entity';
  SCannotContainParams= 'Field %s of datatabase cannot have parameters !';
  SWrongLayername= 'Wrong layer name %s, or layer is not visible or editable';
  SWrongParameters= 'Parameter(s) invalid for function %s';
  SExprParserError= ' %s at line : %d, Column: %d, token: %s';
  SBadExtension= 'Corner Coors are wrong !' + CrLf +
                 'First corner must be at lower left';
  SRestNotExclusive= 'Cannot open file in exclusive mode !';
  SRestructuring= 'Restructuring table';
  SDuplicateLayer= 'Layer name already exists or' + CrLf + 'there is a file with same name on disk.';
  SRecnoInvalid= 'Recno out of range';
  SRebuildTree= 'Rebuilding r-tree %s...';
  SDBFNotFound= 'Base datafile of this layer cannot be found.' + CrLf +
                'The layer is kept without an attached DBF file';
  SUpdateExtension= 'Updating layer %s';
  SThereAreNoDeleted= 'There are no deleted entities. Do you wish to' + CrLf +
                     'continue to do an evaluation of this file ?';
  SPackingLayer= 'Packing layer...';
  SWrongRecordsRecover= 'Cannot recover %d damaged records';
  SExprFail= 'Expression is not of boolean type !';
  SCannotDeleteLayer= 'Layer cannot be deleted.' + CrLf +
                      'Probably is being used in a network';
  SInternalListError= 'Internal list error';
  SLayerFilesNotFound= 'Cannot found the formerly files of this layer %s !' + CrLf +
                       'The layer is deleted from the map';
  SPrinterIsBusy= 'Printer is busy';
  SGISListError= 'Wrong Index in DrawBoxList';
  SGISUnassigned= 'Unassigned';

  SEzImplMissed= 'You must include unit ezimpl.pas in your project';
  SNoGISDefined= 'No basic component linked';
  SSelectinLayer= 'Selecting layer ';
  SEntityNotClosed= 'Entity is not closed. Cannot clip';
  SMultiPartNotSupported= 'Multi-part entities not supported in this release';
  SLayerNotFound= 'Layer name not found';
  SCannotGroup= 'Can group entities from same layer only';

  SUndoDelete= 'Undo erase';
  SUndoInsert= 'Undo insert';
  SCannotUndo= 'Cannot undo';
  SUndoTransform= 'Undo transform';
  SCannotRedo= 'Cannot Redo';
  SCanRedo= 'Redo';

  SFlatScrollBarNotAllowed='Flat ScrollBar cannot be used ' +CrLf+
                           'the version of COMCTL32.dll in this system is lower than 4.71';

  SEntityOutOfBound= 'Entity number out of range !';
  SInvalidSymbolFile= 'Wrong file format for symbols';
  SSymbolOutOfBound= 'Element number out of range !';

  SInvalidSymbolVersion= 'File version is wrong !';
  SWrongImageFormat= 'This file format is not supported';

  SEntListIndexError= 'List index out of bounds';
  SEntListCountError= 'List count error';
  SEntListCapacityError= 'Capacity error';
  SVectorOutOfBound= 'Vector out of bound';


  SDefineFromPoint   = 'From point : ';
  SDefineToPoint     = 'To point : ';
  SFirstPoint        = 'First point : ';
  SSecondPoint       = 'Second point : ';
  SThirdPoint        = 'Third point : ';
  SNextPoint         = 'Next point : ';
  SNextPointPoly     = 'Specify next point or [Arc] : ';
  SPolyArcEndPoint   = 'Specify end point of the arc or [Second, Center, Direction, Line] : ';
  SPolyArcSecondPoint= 'Specify second point of arc : ';
  SPolyArcCenterPoint= 'Specify center point of arc';
  SPolyArcDirection  ='Specify the tangent direction for the start point of the arc : ';
  SPolyArcEndPointAfter= 'Specify end point of the arc : ';

  SClipPolygon= 'Clipping entity, '; // 'Clip Polygon, ';
  SCircleCenter= 'Center :';
  SCircleRadius= 'Radius :';
  SArcSweepAngle= 'Define sweep angle:';
  SCircleEndPoint= 'Identify First Arc End Point';


  SRealTimeZoom= 'Press Pick button and drag vertically to zoom';

  SDefineFirstZoomPoint= 'First corner';
  SDefineSecondZoomPoint= 'other corner';

  SNotEnoughData= 'Need more points to build entity !';

  SStartSingleSelect= 'Click an entity to select. SHIFT for single select';

  SDefineBasePoint= 'Base point :';
  SDefineEndPoint= 'Last point :';

  SReferenceFirstPoint= 'Reference length, first point :';
  SReferenceSecondPoint= 'Referenge length, second point :';
  STransformLastPoint= 'Last point :';
  SRotTransformLastPoint= 'Last point or angle :';

  SDefineSecondSelection= 'Other corner of selection';
  SWarnNoSelection= 'No entities selected !';
  SVertexSingleEntityError= 'Only one entity must be selected for the operation !';
  SInsertVertexPoint= 'Click in the new point for the selected vertex';
  SVertexEntityBadError= 'You can add vertexes to Polylines and Polygons only !';
  SChangeGridOrigin= 'Please click the new Grid Origin :';
  SShowHintCaption= 'Move mouse over an entity, then click for show hint. Click again for stacked entities. <Esc> finish';
  SVectorialFirstPt= 'Define area. First corner:';
  SVectorialSecondPt= 'Define area. Other corner:';


  SSelectRefEntity= 'Please select the reference entity';
  SBreakSelectFirstPoint= 'Select first point of entity';
  SBreakSelectSecndPoint= 'Select second point';
  SBreakWrongSelection= 'You must pick on same entity';
  SBreakWrongEntity= 'Cannot break this entity';
  SBreakNotClicked= 'No entities found';
  STrimNoCuttingEdge= 'No cutting edge selected yet';
  SExtendNoExtendEdge= 'No extend edge selected yet';
  STrimSelectEnt = 'Select entity to trim';
  SExtendSelectEnt= 'Select entity to extend';
  STrimWrongEntity= 'Cannot trim this entity';
  SExtendWrongEntity= 'Cannot extend this entity';
  SExtendWrongSegment= 'Cannot extend this segment';
  STrimOnlyOneCuttingEdge= 'Supported only one cutting edge';
  SExtendOnlyOneCuttingEdge= 'Supported only one extending edge';
  SFilletWrongEntity= 'Cannot fillet this entity';
  SFilletNotContiguous= 'No contiguous segments picked';
  STrimCuttingEdge= 'Cannot trim the cutting edge';
  SFirstExtensionLine= 'First extension line origin';
  SSecondExtensionLine= 'Second extension line origin';
  SDefineTextLocation= 'Text Location';
  SClickSegmentError= 'You must click on a line segment';

  SBlobErrNotEditing= 'Dataset not in edit or insert mode';
  SCannotOpenDesignMode= 'Cannot open on design mode';
  SLayerNotAssigned= 'Layer property is not assigned or is not visible';
  SLayerEmpty= 'Layer name is not defined';
  SExpressionEmpty= 'Expression or field name cannot be empty';
  SWrongFieldnameCol= 'Wrong field name column %d';
  SGetRecordInvalid= 'Error reading record';
  SInsertNotAllowed= 'Insertion is not allowed';
  SWrongReadOnly= 'Cannot change readonly property when dataset is active';

  SLatLonMismatch= 'Destination file is in different Coordinate system' + CrLf +
                   'Action canceled';
  SWrongProjParams= 'Parameters for the projection are wrong';
  SWrongProjector= 'Projector property not defined';
  SLayerAlreadyExists= 'There is a layer with same name.'+CrLf+
                       'Do you want to replace existing layer ?';
  SCannotCreateNewLayer= 'New layer cannot be created !';
  SWrongEzGIS= 'GIS property unassigned';
  //SProjEditSource= 'Select Source File Coordinate System';
  //SProjEditDestin= 'Select Destination File Coordinate System';
  SProjEditCS= 'Select File Coordinate System';


  SDXFReading= 'Reading AutoCAD file...';
  SDXFPolylineBad = 'Polyline contained more than %d vertices %d';
  SDXFInvalidFloat= 'Invalid Floating point conversion';
  SDXFInvalidInteger= 'Invalid Integer conversion';
  SDXFFileReadingNow= ' DXF File reading now..';
  SDXFUndefBlock = 'Undefined Block reference error';
  SDXFWrongBlockInfo= 'This drawing have wrong block information,'+crlf+
                      'but continue reading with ignoreing ...';
  SDXFLWPolylineBad= 'LwPolyline contained more than %d  vertices %d';
  SDXFPOINTBad= 'Error reading POINT entity ';
  SDXFINSERTBad= 'Error reading INSERT entity';
  SDXFTEXTBad= 'Error reading TEXT entity';
  SDXFLINEBad= 'Error reading LINE entity';
  SDXFPOLYLINEBad2= 'Error reading POLYLINE entity';
  SDXFLWPOLYLINEBad2= 'Error reading LWPOLYLINE entity';
  SDXF3DFACEBad= 'Error reading 3DFACE entity';
  SDXFSOLIDBad= 'Error reading SOLID entity';
  SDXFCIRCLEBad= 'Error reading CIRCLE entity';
  SDXFARCBad= 'Error reading ARC entity';
  SDXFATTDEFBad= 'Error reading ATTDEF entity';
  SDXFATTRIBBad= 'Error reading ATTRIB entity';
  SDXFThereIsNoHeader= 'No Header or invalid Header section in DXF file';
  SDXFThereIsNoLayers= 'No Layers or Tables section invalid in DXF file';
  SDXFThereIsNoBlocks= 'No Blocks or Blocks section invalid DXF file';
  SDXFThereIsNoEntities= 'There are no Entities or Entities section is invalid in DXF file';
  SDXFThereIsNoMaxMin= 'File does not contain Max/Min extents. Calculating...';
  SDXFErrorInLine= 'An error ocurred in line or near to ';
  SDXFDupLayer= 'Tried to create a layer with duplicated name';
  SDXFNoName= 'Tried to add a layer with no name';
  SDXFCannotSave= 'Maybe destination path is on CDrom or'+CrLf+
                  'not enough disk space,Can''t save. Warning!';


  STextIsNotVisible= 'Text is invisible !' + CrLf +
                     'The inserted text will be invisible';
  SInsertText= 'Click the text insertion point';
  SEntityToReshape= 'Select entity to reshape. Hold down SHIFT to accumulate, CTRL to select in frame';
  SReshapeToPoint= 'Move to point (Hold down ALT key to change rotate origin):';
  SVertexToReshape= 'Select vertex to modify';
  SVertexToDelete= 'Select node to delete';
  SClickInterestPoint= 'Please click the point';
  SEntityToDrag= 'Select an entity to drag & drop';
  SDragFromCtrlPt= 'Drag from control point';


  SDefineCircleCenter= 'Click the center of the circle selection';
  SDefineCircle= 'Move mouse to define the circle selection';

 //@UBC: 17.07.2006 Ölçü bilgilerinde semt açýsý vb. eklentiler
 // SAreaAndPerimeter= 'Area = %.*n %s' + CrLf +
 //                    'Length = %.*n %s' + CrLf +
 //                    'Angle = %.2n' + CrLf +
 //                    'Points = %.2n';

  SAreaAndPerimeter= 'Alan = %.*n %s' + CrLf +
                     'Uzunluk = %.*n %s' + CrLf +
                     'Toplam Uzunluk = %.*n %s' + CrLf +
                     'Açý = %.2n' + CrLf +
                     'Semt Açýsý (Derece) = %.2n' + CrLf +
                     'Semt Açýsý (Grad)   = %.2n' + CrLf +
                     'Semt Açýsý (Radyan) = %10.6f' + CrLf +
                     'DY = %10.6f' + CrLf +
                     'DX = %10.6f' + CrLf +
                     'Eðim = %10.6f' + CrLf +
                     'Noktalar = %.2n';

  SZoomUnZoom= 'Click a point to zoomin/zoomout';
  SScrolling= 'Press Pick button and move the mouse to pan';
  SInsertXGuideLine= 'X Coor for guideline :';
  SInsertYGuideLine= 'Y Coor for guideline :';
  SSelectGline= 'Select the guideline to move';
  SMoveGLinetoNew= 'Move the guideline to its new position';


  SUnrecognizedCommand= 'Unknown Command !';
  SShpFileNotFound= 'File %s was not found !' + CrLf +
                    'Cannot import the file';
  SThereAreNoLayers= 'You need at least one layer !';
  SDLLLoadError = 'Error loading DLL library !';
  SGridTooDense= 'Grid too dense to display';
  Sscrcanceled= 'Script canceled';
  SEmptyTable= '<Empty Table>';
  SPageid='Page %d';
  SROwCol='Row %d, Col %d';
  SWrongFontSize= 'Font size too small !';
  SLoadingCAD= 'Loading Layer %s';
  SSavingCAD= 'Saving Layer %s';
  SWrongCADFile= 'This file is not a EzCAD file !';
  SFileAlreadyExists= 'The file already exists !';
  STextInfo= 'Height: %.2n Points; %.2n units';
  SErrWrongField= 'Wrong field definition';

  SThematicNotUpdated = 'Thematic map is out of date !';
  SMBToomanyfields= 'Too many fields selected for thematic creation';
  SCalculatingRanges= 'Calculating...';
  SExpresionNull= 'Expression is empty';
  SWrongFieldName= 'Field name does not exists';

  SCircleInfo= 'Radius = %.*n; Area = %.*n; Perimeter = %.*n';
  SBufferRefInfo= 'Radius = %.*n';
  SOneSelection= 'Selected on Layer %s, Recno %d';
  SMultiSelection= '%d selected entities';
  SUnknownFormat= 'Unknown file format';
  SNoNodeClicked= 'Node was not clicked';

  { networkd messages }
  SNetSameNodes= 'Cannot link to same node';
  SNetExpressionNotNumeric= 'Exprssion is not of numeric type';
  SNetFNODENotFound = 'Field F_NODE not found !';
  SNetTNODENotFound = 'Field T_NODE not found !';
  //SNetWrongFIleVersion = 'File version mismatch with software version';
  //SNetWrongFilename = 'The defined file does not exists';
  //SNetCannotCreateDir = 'Cannot create directory for network files';
  //SNetExpressionNotNumeric = 'The expression is not of numeric type';
  SNetCreatingNetwork = 'Creating Network...';
  SNetNodesOverlap = 'There are nodes that overlaps.'#13#10'Do you want to see the exact location ?';
  //SNetLayernameMismatch = 'Layer name mismatch with the linked layer';
  //SNetNodeToMove = 'Pick the node to move';
  //SNetMoveNodeTonew = 'Move node to new position';
  //SNetGraphicPickN = 'Picked Point %d';
  //SNetTotalCost = 'Total Route Cost: %n';
  //SNetNumberOfStops = 'Number of stops: %d';
  //SNetLoadStops = 'Load stops from layer';
  //SNetAddStopsCaption = 'Click a point where you wish to add a stop';
  //SNetInvalidLayer = 'The layer does not have attached a DBF file';
  SNetDirections1 = 'Starting from Picked Point %d';
  SNetDirections2 = '  Travel on %s for %.*n %s';
  SNetContinueStraight = 'Continue Straight onto %s';
  SNetDirections3 = 'Turn %s onto %s';
  SNetDirections4 = 'Total cost of traveling is %.*n %s';
  SNetDirections5 = '=====================';
  SNetRight = 'Right';
  SNetLeft = 'Left';
  SNetCleaning = 'Cleaning up layer...';
  //SNetWrongLayer = 'Cannot clean-up for this layer.'#13#10'Currently in use.';
  //SNetBestRouteNotShown = '"Find Best Route" form is not opened';
  //SNetCleanCaption = 'Clean up a layer';
  //SNetOpenCaption = 'Open network for layer';
  //SNetConfirmDeletion = 'Okay to delete selected links?';
  SNetUnits= 'Units';
  SDGNWrongHeader= 'File `%s'' does not have expected DGN header';
  SNoneColorTextInColorPicker= 'Transparent';
  SCustomTextInColorPicker= '&More Colors...';
  SLayerIndexOutOfRange= 'Layer index out of range';
  sDrawBoxCollectionNotSame= 'All member of collection must have same TEzBaseGIS';

  { dic/1/2002 }
  SFilterEnabled= 'A filter is active';
  SBFFFolderName= 'C:\';
  SBFFSTatusText= 'Plase select folder';
  SBFFCaption= 'Select Folder';
  { layer list box }
  SLLBCurrent= 'Current';
  SLLBVisible= 'Visible';
  SLLBSelectable= 'Selectable';
  SLLBLayerName= 'Name';
  SLLBGroupName= 'Group Name';

  SNoImagesFound= 'No images/files found on %s';
  SSelectImgTitle= 'Select Image/Element';
  SBandedImagesFilter= 'Windows Bitmaps (*.BMP)|*.BMP' +
                       '|ESRI Band Interleaved by Line (*.BIL)|*.BIL' +
                       '|TIFF files (*.TIF)|*.TIF';
  SGeorefsFilter = 'Georeferenced Images (*.GI2)|*.GI2';
  SBitmapFilter = 'Windows Bitmap (*.BMP)|*.BMP';
  SJPGFilter= 'Jpeg (*.JPG) |*.JPG';
  SMetafileFilter = 'Windows Metafile (*.WMF)|*.WMF|Windows Enhanced Metafile (*.EMF)|*.EMF';
  SICOFilter = 'Windows ICON files (*.ICO)|*.ICO';
  SGIFFilter= 'Compuserve Gif (*.GIF)|*.GIF';
  STIFFilter= 'TIFF files (*.TIF)|*.TIF';
  STargaFilter= 'True Vision images (*.TGA)|*.TGA';
  SPCXFilter= 'PCX files (*.PCX)|*.PCX';
  SPCDFilter= 'Kodak Photo CD images (*.PCD)|*.PCD';
  SPSDFilter= 'PhotoShop images (*.PSD)|*.PSD;*.PDD';
  SPSPFilter= 'PaintShopPro images (*.PSP)|*.PSP';
  SPNGFilter= 'Portable Network Graphics images (*.PNG)|*.PNG';

  SEDBFilter= 'EzGIS blocks (*.EDB)|*.EDB';
  SOLEFilter= 'All files (*.*)|*.*';
  SChangeDirNotAllowed= 'You must select the file on the common subdir only !';
  SInspProperty= 'Property';
  SInspValue= 'Value';
  SMenuAllToCurve= 'All to curve';
  SMenuAllToLinear= 'All to linear';
  SMenuTypeOfTangent= 'Type of tangent';
  SMenuTangent= 'Tangent';
  SMenuBroken= 'Broken';
  SMenuAddNode= 'Insert vertex';
  SMenuDeleteNode= 'Delete vertex';
  SMenuEditNode= 'Edit vertex...';
  SMenuMoveCenterOfRot= 'Move Center of Rotation';

  SDefineNumSides= 'Enter number of sides :';
  SDefineCenterOfPoly= 'Specify center of polygon :';
  SInsCircumScribed= 'Enter an option (I)nscribed in circle/(C)ircumscribed about circle :';
  SDefineRadius= 'Define radius of circle :';

  SMeasureInfo= 'Measure Info';
  SHintInfo= 'Show Hints';
  SEmptyHint= '(Undefined)';
  SGeneralCategory= 'General';
  SWrongCategory= 'Category does not exists';
  SWordListError= 'Word List error';
  SWrongCanvasRegion= 'Push: Wrong canvas in stacked regions';

  // paper sizes
  SpsPrinter= 'Printer';
  SpsLetter= 'Letter';
  SpsLegal= 'Legal';
  SpsLedger= 'Ledger';
  SpsStatement= 'Statement';
  SpsExecutive= 'Executive';
  SpsA3= 'A3';
  SpsA4= 'A4';
  SpsA5= 'A5';
  SpsB3= 'B3';
  SpsB4= 'B4';
  SpsB5= 'B5';
  SpsFolio= 'Folio';
  SpsQuarto= 'Quarto';
  Sps10x14= '10x14';
  Sps11x17= '11x17';
  SpsCsize= 'C size';
  SpsUSStdFanfold= 'US Std Fanfold';
  SpsGermanStdFanfold= 'Germand Std Fanfold';
  SpsGermanLegalFanfold= 'German Legal Fanfold';
  Sps6x8= '6x8';
  SpsFoolscap= 'Foolscap';
  SpsLetterPlus= 'Letter Plus';
  SpsA4Plus= 'A4 Plus';

  // EzTCPBaseGIS resource strings
  SFlatBarNotAllowed= 'Flat scroll bar not allowed';

