unit Lider.CG.Com.Consts;

{$I Lider.CG.Com.Component.inc}

interface

const
  CRLF = #$D#$A;
  LICGEDIT = 'E'#0'd'#0'i'#0't'#0#0#0;

resourcestring

{$IFDEF LANG_ENG}
{$I Lider.CG.Com.English.inc}
{$ENDIF}

{$IFDEF LANG_TRK}
{$I Lider.CG.Com.Turkish.inc}
{$ENDIF}

  // not nationalized section

  { for generating script }
  sDataInfo = 'Sözel Veriler';
  sFieldValue = '%s : %s ';
  sPointPenInfo = 'Kalem %d,$,%d';
  sPenInfo = 'Kalem %d,$%.8x,%f,%s';
  sBrushInfo = 'Tarama %d,$%.8x,$%.8x,%s,%d,%f,%d';
  sSymbolInfo = 'Sembol %d,%f,%f';
  sTTFontInfo = 'Font "%s",%s,%s,%s,%s,$%.8x,%d';
  sVectorFontInfo = 'Vektör Font "%s"';
  sNoneInfo = 'Boþ';
  sPointInfo = 'Nokta %s';
  SPlaceInfo = 'Sembol %s,"%s"';
  sArcsInfo = 'Yay at %s';
  sBeziersInfo = 'Beziers at %s';
  sOneBezierInfo = '(%d,(%f,%f),(%f,%f),%d)';
  sPolylineInfo = 'Çoklu Doðru %s';
  sPolygonInfo = 'Kapalý Alan %s';
  sRectangleInfo = 'Dikdörtgen %s,%f,%f';
  sArcInfo = 'Yay %s,%d';
  sEllipseInfo = 'Daire %s,%f';
  sPicRefInfo = 'Resim (Referanslý) %s,"%s",%d,%f';
  sOleEntityInfo = 'Ole Nesne %s, "%s"';
  sPersistBmpInfo = 'Resim (Proje içinde) %s,"%s"';
  sBandsBmpInfo = 'BandsBitmap %s,"%s",%d';
  sBandsTiffInfo = 'BandsTiff %s,"%s",%d';
  sCustomPicInfo = 'CustPict %s';
  sSplineInfo = 'Eðri %s';
  sTTTextInfo = 'True Type Yazý (%f,%f),"%s",%f,%f';
  sMTTTextInfo = 'True Type Yazý (M) (%f,%f),"%s",%d,%d,%f,%f';
  sTTTextInfoFX = 'True Type Yazý  (%f,%f),"%s",%f,%f,%d,(%f,%f),$%.8x,%s,%f,%f,%s,%d';
  sTableInfo = 'Tablo %s,%d,%d,%s,%s,%s,%d,$%.8x,%f,%f,$%.8x';
  sColumnInfo = 'Kolon {';
  sTableColumnInfo = '%f,%s,$%.8x,%d,%d,%f';
  sColumnTitleInfo = '"%s",%d,$%.8x,%s,%f';
  sRowDataInfo = '"%s"';
  sTitleCaption = 'Baþlýk ';
  sGroupName = 'Grup {';
  sJustifTextInfo = 'Yaslanmýþ (Justified) Yazý (%f,%f),(%f,%f),"%s",%f,%f,$%.8x,%d,%d';
  sFittedTextInfo = 'Hizalanmýþ (Fitted) Yazý (%f,%f),"%s",%f,%f,%f,$%.8x';
  sBlockInfo = 'Blok %s,"%s",%f,%f,%f,"%s"';
//  sSplineTextInfo = 'Eðri (Yazýlý) %s,"%s",%s';
  sDimHorizInfo = 'Yatay ölçü (%f,%f),(%f,%f),%f';
  sDimVertInfo = 'Dikey Ölçü (%f,%f),(%f,%f),%f';
  sDimParallelInfo = 'Paralel Ölçü (%f,%f),(%f,%f),%f';
  sPreviewInfo = 'Ön izleme %s,%d,%f,%f,%s,%f,%f,%f,%f';
  sERMapperInfo = 'ERMapper %s,"%s",%d';
  sDashPatt = 'Ýmaj ';
  SDefaultDateFormat = 'dd.mm.yyyy';
  SEntityField = '.ENT';
  SRecNoField = '.RECNO';
  SSelection = 'SELECTION';
  SUTMZone = 'UTM Zone ';
  SCartesianFormat = '###,###,###,##0.000000';
  SDegreesFormat = '###0.00000000000';
  SCmdReshape = 'RESHAPE';
  SCmdLine = 'LINE';
  SCmdPolyline = 'PLINE';
  SCmdPolygon = 'POLYGON';
  SCmdPolygonFigure = 'FIGURE';
  SCmdSketch = 'SKETCH';
  sCmdArc = 'ARC';
  SCmdEllipse = 'ELLIPSE';
  SCmdSpline = 'SPLINE';
  SCmdFitPoints = 'FITPOINTS';
  SCmdBezier = 'BEZIER';
  SCmdRectangle = 'RECTANGLE';
  SCmdSymbol = 'SYMBOL';
  SCmdTextSymbol = 'TEXTSYMBOL';
  SCmdText = 'TEXT';
  SCmdHorzGLine = 'HGLINE';
  SCmdVertGLine = 'VGLINE';
  SCmdMoveGLine = 'MOVEGLINE';
  SCmdHints = 'HINTS';
  SCmdPictRef = 'PICTUREREF';
  SCmdCustomPicture = 'CUSTOMPICTURE';
  SCmdGeorefBitmap = 'GEOREFBITMAP';
  SCmdBandsBitmap = 'BANDSBITMAP';
  SCmdPersistBitmap = 'PERSISTBITMAP';
  SCmdUndo = 'UNDO';
  SCmdRedo = 'REDO';
  SCmdPan = 'PAN';
  SCmdSelect = 'SELECT';
  SCmdSelPLine = 'SELPLINE';
  SCmdMeasures = 'MEASURES';
  SCmdOffset = 'OFFSET';
  SCmdCut = 'CUT';
  SCmdCopy = 'COPY';
  SCmdPaste = 'PASTE';
  SCmdPasteFromClipboardToLocation = 'PASTETOLOCATION';
  SCmdCopyAsText = 'COPYASTEXT';
  SCmdCopyAsBmp = 'COPYASBMP';
  SCmdPasteAsText = 'PASTEASTEXT';
  SCmdInsertVertex = 'INSERTVERTEX';
  SCmdGridOrigin = 'GRIDORIGIN';
  SCmdEditDB = 'EDITDB';
  SCmdPolygonSelect = 'POLYGONSEL';
  SCmdCircleSelect = 'CIRCLESEL';
  SCmdZoomIn = 'ZOOMIN';
  SCmdZoomOut = 'ZOOMOUT';
  SCmdZoomWindow = 'ZOOMWIN';
  SCmdHandScroll = 'SCROLL';
  SCmdSetClipPolyArea = 'CLIPPOLYAREA';
  SCmdClipPolyline = 'CLIPPLINE';
  SCmdDeleteVertex = 'DELVERTEX';
  SCmdSetClipArea = 'SETCLIPAREA';
  SCmdBreak = 'BREAK';
  SCmdTrim = 'TRIM';
  SCmdExtend = 'EXTEND';
  SCmdFillet = 'FILLET';
  SCmdAddMarker = 'MARKER';
  SCmdPolygonBuffer = 'POLYGONBUFFER';
  SCmdCalloutText = 'CALLOUTTEXT';
  SCmdBulletLeader = 'BULLETLEADER';
  SCmdBannerText = 'BANNER';
  SCmdTTCalloutText = 'TTCALLOUTTEXT';
  SCmdTTBulletLeader = 'TTBULLETLEADER';
  SCmdTTBannerText = 'TTBANNER';
  SCmdZoomAll = 'ZOOMALL';
  SCmdJustifText = 'JUSTIFTEXT';
  SCmdFittedText = 'FITTEDTEXT';
  SCmdPoint = 'POINT';
  SCmdPointArray = 'POINTARRAY'; //ilker ekleme
  SCmdDimHoriz = 'DIMHORZ';
  SCmdDimVert = 'DIMVERT';
  SCmdDimParall = 'DIMPARALL';
  SCmdCircle2P = 'CIRCLE2P';
  SCmdCircle3P = 'CIRCLE3P';
  SCmdCircleCR = 'CIRCLECR';
  SCmdRichText = 'RICHTEXT';
  SCmdInsert = 'INSERT';
  SCmdOLE = 'OLE';
  SCmdCustomClick = 'CUSTOMCLICK';
  SCmdDropSelection = 'DROP';
  SCmdDragDrop = 'DRAG&DROP';
  SCmdTracking = 'TRACKING';
  SCmdArcSE = 'ARCSE';
  SCmdArcFCS = 'ARCFCS';
  //SCmdNode = 'NODE';
  //SCmdNodeLink = 'NODELINK';
  SCmdBufferOffset = 'BUFFEROFFSET';
  SCmdRoundR = 'ROUNDR';
  SCmdRtf = 'RICHTEXT';
  SCmdExternal = 'EXTERNALCOMMAND';
  SCmdLauncher = 'LAUNCHER';

  SCmdChangeLayer = 'CHANGELAYER';
  SCmdOpenAllLayers = 'OPENALLLAYERS';
  SCmdCloseAllLayers = 'CLOSEALLLAYERS';
  SCmdOpenCloseAllLayers = 'OPENCLOSEALLLAYERS';
  SCmdSelectOpenLayerOtherClose = 'SELECTOPENLAYER';
  SCmdSelectCloseLayer = 'SELECTCLOSELAYER';
  SCmdSelectCurrentLayer = 'SELECTCURRENTLAYER';
  SCmdSelectSelectableLayer = 'SELECTSELECTABLELAYER';
  SCmdSelectLayerSettings = 'SELECTLAYERSETTINGS';

  SCmdDelete = 'DELETE';
  SCmdQuickDelete = 'QDELETE';
  SCmdMove = 'MOVE';
  SCmdQuickMove = 'QMOVE';

  SCmdCreateArray = 'CREATEARRAY';
  SCmdGroup = 'GROUP';
  SCmdUnGroup = 'UNGROUP';
  SCmdMoveForward = 'MOVEFORWARDENT';
  SCmdMoveBackward = 'MOVEBACKWARDENT';
  SCmdSendBack = 'SENDBACKENT';
  SCmdBringFront = 'BRINGFRONTENT';
  SCmdRoundedPline = 'ROUNDEDPLINE';
  ScmdHatlariBirlestir = 'HATLARIBIRLESTIR';
  SCmdParallelAl = 'PARALELAL';
  SCmdMultiSymbol = 'DRAWMULTISYMBOL';
  SCmdMultiBlock = 'DRAWMULTIBLOCK';
  SCmdRoleveAciYaz = 'ROLEVEACIYAZ';
  SCmdRoleveCEpheYaz = 'ROLEVECEPHEYAZ';
  SCmdKuturBagla = 'KUTURBAGLA';
  SCmdDikDus = 'ROLEVEDIKDUS';
  SCmdRolevePoligonHatti = 'ROLEVEPOLIGONHATTI';
  SCmdAlinmanOlculendir = 'ALINMANOLCULENDIR';
  SCmdRoleveAlanYaz = 'ROLEVEALANYAZ';
  SCmdKoordinatYaz = 'KOORDINATYAZ';
  SCmdRoleveNoktaAdiYaz = 'ROLEVENOKTAADIYAZ';
  SCmdAraMesafeYaz = 'ARAMESAFEYAZ';
  SCmdPligonHatYonu = 'POLIGONHATYONU';
  SCmdAralikAcisi = 'ARALIKACISI';
  SCmdRevertDir = 'REVERTDIRECTION';
  SCmdAreaDiv = 'AREADIV';
  SCmdTransformTextEnt = 'TRANSFORMTEXTENT';
  SCmdRegenDrawing = 'REGENDRAWING'; // Refresh Drawbox - Regendrawing Çizimi refesh eder.
  sCmdPolygonToPolyLine = 'ALANCDOGRU';
  sCmdPolylineToPolygon = 'CDOGRUALAN';
  SCmdAreaHatch = 'ALANTARAMA';
  SCmdQuickSelect = 'QUICKSELECT';
  SCmdZoomToSelection = 'ZOOMTOSELECTION'; // Drawbox.zoomtoselection iþlemini yerine getirir
  SCmdZoomToLayer = 'ZOOMTOLAYER'; // Zoom to layer Tabaka ölçülerinde zoom iþlemini yerine getirir
  SCmdZoomPrevious = 'ZOOMPREVIOUS'; // ZoomPrevious metodunu çalýþtýrýr. Önceki görüntü ölçeðine dönüþ
  SCmdSelectAll = 'SELECTALL'; // DrawBox.selectAll metodunu çaðýrýr. tüm nesneleri seçer
  SCmdUnselectAll = 'UNSELECTALL'; // DrawBox.UnselectAll metodunu çaðýrýr. Seçili olan tüm nesneleri unselect eder.
  SCmdSelectLayer = 'SELECTLAYER'; // Belirtilen Tabaka içindeki tüm nesneleri seçer DrawBox.DoSelectLayer(Layer,Canceled) çaðýrýr.
  SCmdGroupSel = 'GROUPSEL'; // DrawBox.GroupSelection metodunu çaðýrýr. Seçili nesneleri Gruplar
  SCmdUnGroupSel = 'UNGROUPSEL'; // DrawBox.UnGroupSelection metodunu çaðýrýr. Gruplamayý kaldýrýr
  SCmdBringToFront = 'BRINGTOFRONT'; // DrawBox.BringToFront metodunu çaðýrýr.Nesneyi öne alýr.
  SCmdSendToBack = 'SENDTOBACK'; // DrawBox.SendToBack metodunu çaðýrýr.Nesneyi arkaya alýr.
  SCmdAreaToPolyLine = 'AREATOPOLYLINE'; // DrawBox.SelectionToPolyline metodunu çaðýrýr.Kapalý Alaný Çoklu hatta çevirir.
  SCmdPolyLineToArea = 'POLYLINETOAREA'; // DrawBox.SelectionToPolygon metodunu çaðýrýr. Çoklu hattý kapalý alana çevirir.
  SCmdUndoUndo = 'UNDOUNDO'; // DrawBox.SelectionToPolygon metodunu çaðýrýr. Çoklu hattý kapalý alana çevirir.
  SCmdRoadBal = 'ROADBAL'; // TRoadBalanceAction (Yol dengeleme) Hat dengeleme
  SCmdRoadBalPoint = 'ROADBALP'; // TRoadBalanceAction (Yol dengeleme) Nokta dengeleme
  SCmdOBreak = 'OBREAK'; // BREAK ile ayný iþleve sahiptir ->Action ismine command name verildiðinden Break keyword ile çakýþmamasý için
  SCmdOTrim = 'OTRIM'; // TRIM ile ayný iþleve sahiptir ->Action ismine command name verildiðinden Trimn function ile çakýþmamasý için
  SCmdOMove = 'OMOVE'; // MOVE command ile Move Procedure karýþmamasý için deðiþtirildi. Action name'lerine Command isimleri verildiðinden isim - procedure çakýþmasýný ortadan kaldýrýr.
  SCmdGenAutoPoint = 'GENARATEPOINT';
  SCmdCircleWithRadius = 'CIRCLEWITHRADIUS';
  SCmdLineByGivenPARAM = 'LINEBYGIVENPARAM';
  SCmdRectByGivenPARAM = 'RECTBYGIVENPARAM';
  SCmdFarMove = 'FARMOVE';
  SCmdKoseNoktaYaz = 'OTOMATIKKOSENOYAZ';
  SCmdPrint = 'PRINT';
  SCmdKarelaj = 'KARELAJ';
  ScmdParallel = 'PARALLEL';
  SCmdNesneKir = 'NESNEKIR';
  SCmdYolCiz = 'YOLCIZ';
  SCmdYolYuvarla = 'YOLYUVARLA';
  SCmdKoseDuzelt = 'KOSEDUZELT';
  sCmdRefujKapat = 'REFUJKAPAT';
  sCmdKavsak = 'KAVSAK';
  SCmdYerlesimSembol = 'YERLESIMSEMBOL';
  SCmdYolSembol = 'YOLSEMBOL';
  SCmdTaksKaks = 'TAKSKAKSSEMBOL';
  SCmdEmsalSembol = 'EMSALSEMBOL';
  SCmdAdaParselOlustur = 'ADAPARSELOLUSTUR';
  SCmdParselNoYaz = 'PARSELNOYAZ';
  SCmdAlanAyarla = 'ALANAYARLA';
  SCmdAlanKes = 'ALANKES';
  SCmdAlanCikar = 'ALANCIKAR';
  SCmdParselasyon = 'PARSELASYON';
  SCmdAlanBirlestir = 'ALANBIRLESTIR';
  SCmdBinaOlustur = 'BINAOLUSTUR';
  SCmdBinaParalel = 'BINAPARALEL';
  SCmdNoktaTabakalandir = 'NOKTATABAKALANDIR';
  SCmdAdadaNoktaAdlandir = 'ADADANOKTAADLANDIR';
  SCmdKomsuBul = 'KOMSUBUL';
  SCmdKurpYerlestir = 'KURPYERLESTIR';
  SCmdYatayKenarKotFarki = 'YATAYKENARKOTFARKI';
  SCmdEgikKenarDuseyAci = 'EGIKKENARDUSEYACI';
  SCmdYatayKenarDuseyAci = 'YATAYKENARDUSEYACI';
  SCmdTakeometrikHesap = 'TAKEOMETRIKHESAP';
  SCmdPoligonHesabi = 'POLIGONHESAP';
  SCmdYanNokta = 'YANNOKTA';
  SCmdKesisim4Nokta = 'KESISIM4NOKTA';
  SCmd2KenardanKesisim = 'KESISIM2KENAR';
  SCmdKutupsalRapor = 'KUTUPSALRAPOR';
  SCmdPrizmatikRapor = 'PRIZMATIKRAPOR';
  SCmdKoordineOzetRaporu = 'KOORDINEOZETRAPORU';
  SCmdCLoseArea = 'CLOSEAREA';
  SCmdYzmVectorialText = 'VECTORTEXT';
  SCmdExplode = 'EXPLODE';

  // //Dönüþüm Araçlarý ilker
  SCmdTwoPointsTransformation = 'TWOPOINTSTRANS';
  SCmdMirror = 'MIRROR';
  SCmdRotate = 'ROTATE';
  SCmdScale = 'SCALE';
  SCmdScaleHorizontal = 'SCALEHORIZONTAL';
  SCmdScaleVertical = 'SCALEVERTICAL';

  SCmdBinaTara           = 'BINATARA';
  SCmdMoveToActiveLayer = 'AKTIFKATMANATASI';
  SCmdSetActiveFont = 'AKTIFFONATA';
  SCmdSetActivePen = 'AKTIFKALEMATA';
  SCmdSetActiveBrush = 'AKTIFDOLGUATA';
  SCmdQuickLineUnion = 'QUICKLINETOLINEUNION';
  SCmdSelEntPropSetLayers = 'SECILENOZELLIKLERIKATMANAATA';
  SCmdRasterTransformFrom2Point = 'RASTERTRANSFROM2POINT';

  SCmdGeoInfo = 'GEOINFO';
  SCmdAreaInfo = 'AREAINFO'; //ilker ekleme
  SCmdXYZInfo = 'XYZINFO'; //ilker ekleme
  SCmdDikAyakBoyInfo = 'DIKAYAKBOYINFO';
  SCmdRasInfo = 'RASTERINFO';

const
  Reserved_Balastro = 0;
  Reserved_PointName = 1;
  Reserved_PolygonName = 2;
  Reserved_Print = 3;
  Reserved_Brush = 4;
  Reserved_PenNo = 5;
  Reserved_LayerTransparent = 6; // ilker transparent Boolean
  Reserved_LayerBrush = 7;
  Reserved_ShowArrow = 8;
  Reserved_LayerTransparency = 9; // ilker transparency value deðeri için kullanýlýyor.
  Reserved_ZDisplay = 10;
  Reserved_ShowDBInfoOnCreateEnt = 11;
  Reserved_NetworkLayer = 12;
  Reserved_Labeling = 13;
  Reserved_NodeVisible = 14;
  Reserved_ApplyBufferForPolyline = 15;
  Reserved_LineWidthDirection = 16;
  Reserved_PointKod = 17;
  Reserved_Thickness = 18;
  Reserved_GuideIsoline = 19;

implementation

end.


