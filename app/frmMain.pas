unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase, orca_scene2d;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

    FConsole: TD2HudMemo;
    FLogStr: string;

    procedure AddConsoleLog(log: string);
    procedure innerAddLog;
    procedure onAlertAccept(AAccept: Boolean; AText: string);
    procedure onBoxItemClicked(Sender: TObject);
    procedure onDownloadCallback(Sender: TObject; ASucc: Boolean;
      AFileName: string);
    procedure onDownloadProgressCallback(Sender: TObject; AProgress: Integer;
      ACount: Integer);
    procedure onHttpGetCallback(Sender: TObject; ACode: Integer;
      AResult: string; AError: string);
    procedure onHttpPostCallback(Sender: TObject; ACode: Integer;
      AResult: string; AError: string);
    procedure onHttpUploadFileCallback(Sender: TObject; ACode: Integer;
      AResult: string; AError: string);
    function onListCell(Sender: TD2VisualObject; AItem: String): TD2ListBoxItem;
    procedure onSampleThreadCallback(Sender: TObject; AIdx: Integer);
  public
    procedure sampleThreadQueue();
    procedure sampleThreadHttpGet();
    procedure sampleThreadHttpPost();
    procedure sampleThreadHttpUploadFile();
    procedure sampleThreadDownload();
    procedure sampleAlert();
    procedure sampleToast();
    procedure sampleFileIOTF();
    procedure sampleFileIOTS();
    procedure sampleFileIOFS();
    procedure sampleFileIOST();
    procedure sampleFileIOSF();
    procedure sampleFileIOFT();
    procedure sampleImage();
    procedure sampleJSON();
    procedure sampleConfig();
    procedure sampleRegExp();
  end;



var
  FormMain: TFormMain;

implementation

uses
  orcautils, adaptedlistview, alertutils, jsonparser, fpjson, jsonscanner, regexutils, toaster, jsonutils, fileutils, threadqueue, threads, configutils, frmImage;

const
  HTTPROOT = 'http://10.211.55.24:12345/phproot';
  HTTPURL = 'http://10.211.55.24:12345/phproot/sample.php';

type

  TSampleQueueThreadCallback = procedure(Sender: TObject; AIdx: Integer) of object;

  { TSampleQueueThread }

  TSampleQueueThread = class(TThread)
  private
    FIdx: Integer;
    FCallback: TSampleQueueThreadCallback;
    procedure onThreadTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AIdx: Integer; ACallback: TSampleQueueThreadCallback);
  end;

  { TSampleSub }

  TSampleSub = class(TPersistent)
  private
    Fsub1: string;
    Fsub2: Integer;
  public
    function ToString: ansistring; override;
  published
    property sub1: string read Fsub1 write Fsub1;
    property sub2: Integer read Fsub2 write Fsub2;
  end;

  { TSampleObj }

  TSampleObj = class(TPersistent)
  private
    Farr: TList;
    Farr__type__: TClass;
    Fi1: Integer;
    Fs1: string;
    Fsa: TStringList;
    Fsub: TSampleSub;
  public
    constructor Create;
    destructor Destroy; override;
    function ToString: ansistring; override;
  published
    property s1: string read Fs1 write Fs1;
    property i1: Integer read Fi1 write Fi1;
    property sub: TSampleSub read Fsub write Fsub;
    property arr: TList read Farr write Farr;
    property arr__type__: TClass read Farr__type__ write Farr__type__;
    property sa: TStringList read Fsa write Fsa;
  end;

type
  TAdaListBox = specialize THudAdaptedListBox<String>;

  { TAdaItem }

  TAdaItem = class(specialize TAdaptedListItem<String>)
  private
    FlblTxt: TD2Text;
  protected
    procedure SetItem(AValue: String); override;
    procedure layout(); override;
  end;

{$R *.frm}

{ Common }
var
  queue: TThreadQueue;

function ListToString(AList: TStringList): string;
var
  s: string;
  ret: string = '';
begin
  for s in AList do begin
    ret += s + ',';
  end;
  ret := ret.TrimRight([',']);
  Exit(ret);
end;

{ TSampleQueueThread }

procedure TSampleQueueThread.onThreadTerminated(Sender: TObject);
begin
  FCallback(Self, FIdx);
end;

procedure TSampleQueueThread.Execute;
begin
  Sleep(500);
end;

constructor TSampleQueueThread.Create(AIdx: Integer;
  ACallback: TSampleQueueThreadCallback);
begin
  inherited Create(True);
  FreeOnTerminate:= True;
  FIdx:=  AIdx;
  FCallback:= ACallback;
  OnTerminate:=@onThreadTerminated;
end;

{ TAdaItem }

procedure TAdaItem.SetItem(AValue: String);
begin
  inherited SetItem(AValue);
  FlblTxt.Text:= AValue;
end;

procedure TAdaItem.layout();
begin
  FlblTxt := TD2Text(createD2VO(Self, TD2Text, 0, 0, 0, 0, vaClient, 8, 0, 0, 0));
  FlblTxt.setup('');
  FlblTxt.HitTest:= False;
end;

{ TSampleSub }

function TSampleSub.ToString: ansistring;
begin
  Exit(Format('sub1 => %s, sub2 => %d', [Fsub1, Fsub2]));
end;

{ TSampleObj }

constructor TSampleObj.Create;
begin
  Farr__type__:= TSampleSub;
  Fsub := TSampleSub.Create;
  sa := TStringList.Create;
  Farr := TList.Create;
end;

destructor TSampleObj.Destroy;
var
  i: Integer;
begin
  Fsub.Free;
  sa.Free;
  for i := Farr.Count - 1 downto 0 do TObject(Farr[i]).Free;
  Farr.Free;
  inherited Destroy;
end;

function TSampleObj.ToString: ansistring;
var
  i: Integer;
  s: string;
begin
  s := Format('s1 => %s, i1 => %d, sa => [%s], sub => [%s], arr => [', [Fs1, Fi1, ListToString(Fsa), Fsub.ToString]);
  for i:= 0 to Farr.Count - 1 do begin
    s += Format('[obj => %s], ', [TSampleSub(Farr[i]).ToString]);
  end;
  s += ']';
  Exit(s);

end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  box: TAdaListBox;
begin
  inherited;
  Window.Text:= 'MyCT';
  Width:= 500;
  Height:= 500;

  queue := TThreadQueue.Create;

  box := TAdaListBox.Create(Root, @onListCell);
  box.Align:= vaClient;
  Root.AddObject(box);

  box.List.Add('Thread Queue');
  box.List.Add('Thread Http (Get)');
  box.List.Add('Thread Http (Post)');
  box.List.Add('Thread Http (Upload File)');
  box.List.Add('Thread Download');
  box.List.Add('Alert');
  box.List.Add('Toast');
  box.List.Add('FileIO (Text -> File)');
  box.List.Add('FileIO (Text -> Stream)');
  box.List.Add('FileIO (File -> Stream)');
  box.List.Add('FileIO (Stream -> Text)');
  box.List.Add('FileIO (Stream -> File)');
  box.List.Add('FileIO (File -> Text)');
  box.List.Add('Image (...)');
  box.List.Add('JSON');
  box.List.Add('Config');
  box.List.Add('Regular Expression');
  box.reloadData();

  box.OnClick:=@onBoxItemClicked;

  FConsole:= TD2HudMemo(createD2VO(Root, TD2HudMemo, 0, 0, 0, 100, vaBottom));
  FConsole.ReadOnly:= True;
  FConsole.UseSmallScrollBars:= True;
  FConsole.WordWrap:= True;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  queue.Free;
end;

procedure TFormMain.AddConsoleLog(log: string);
begin
  FLogStr:= log;
  TThread.Synchronize(nil, @innerAddLog);
end;

procedure TFormMain.innerAddLog;
begin
  FConsole.Lines.Add(WideString(FLogStr));
end;

function TFormMain.onListCell(Sender: TD2VisualObject; AItem: String
  ): TD2ListBoxItem;
begin
  Exit(TAdaItem.Create(Sender, 40, AItem));
end;

procedure TFormMain.onSampleThreadCallback(Sender: TObject; AIdx: Integer);
begin
  AddConsoleLog(Format('Thread Callback => %d', [AIdx]));
end;

procedure TFormMain.sampleThreadQueue();
var
  i: Integer;
  t: TSampleQueueThread;
begin
  // thread queue
  for i:= 1 to 9 do begin
    t := TSampleQueueThread.Create(i, @onSampleThreadCallback);
    queue.AddThread(t);
  end;
end;

procedure TFormMain.sampleThreadHttpGet();
begin
  // get
  with THttpThread.Create(HTTPURL + '?m=get&gp=rarnuget', hmGet) do begin
    Callback:=@onHttpGetCallback;
    Start;
  end;
end;

procedure TFormMain.sampleThreadHttpPost();
begin
  // post
  with THttpThread.Create(HTTPURL, hmPost) do begin
    PostParam.Add('m', 'post');
    PostParam.Add('pp', 'rarnupost');
    Callback:=@onHttpPostCallback;
    Start;
  end;
end;

procedure TFormMain.sampleThreadHttpUploadFile();
var
  p: string;
begin
  p := ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'upload.txt';
  // upload file
  textToFile('sample upload', p);
  with THttpThread.Create(HTTPURL, hmPost) do begin
    PostParam.Add('m', 'file');
    PostParam.Add('fp', 'rarnufile');
    FileParam.Add('file', p);
    Callback:=@onHttpUploadFileCallback;
    Start;
  end;
end;

procedure TFormMain.sampleThreadDownload();
var
  p: string;
begin
  p := ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'server.txt';
  // download
  with TDownloadThread.Create(HTTPROOT + '/sample.txt', p) do begin
    Callback:=@onDownloadCallback;
    ProgressCallback:=@onDownloadProgressCallback;
    Start;
  end;
end;

procedure TFormMain.sampleAlert();
begin
  alert('666', '88888', 'OK');
  alert('777', 'aaaaaaaa', 'Cancel', 'OK');
  alert('888', 'vskljnvbjgnet', 'Cancel', 'Try!', 'OK');
  alert('999', 'dejkbdoewufb', 'sample', 'this is a sample alert.', 'Cancel', 'OK', @onAlertAccept);
end;

procedure TFormMain.sampleToast();
begin
  TToast.show(Root, 'Sample Toast');
end;

procedure TFormMain.sampleFileIOTF();
var
  txt: string = 'sample text';
  ret: Boolean;
begin
  ret := textToFile(txt, ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'sample.txt');
  AddConsoleLog(Format('TextToFile => %s', [BoolToStr(ret, True)]));
end;

procedure TFormMain.sampleFileIOTS();
var
  txt: string = 'sample text';
  s: TStream;
begin
  s := textToStream(txt);
  AddConsoleLog(Format('TextToStream => %s', [s.ToString]));
  s.Free;
end;

procedure TFormMain.sampleFileIOFS();
var
  s: TStream;
begin
  s := fileToStream(ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'sample.txt');
  AddConsoleLog(Format('FileToStream => %s', [s.ToString]));
  s.Free;
end;

procedure TFormMain.sampleFileIOST();
var
  s: TStringStream;
  t: String;
begin
  s := TStringStream.Create('sample text');
  t := streamToText(s);
  AddConsoleLog(Format('StreamToText => %s', [t]));
  s.Free;
end;

procedure TFormMain.sampleFileIOSF();
var
  s: TStringStream;
  ret: Boolean;
begin
  s := TStringStream.Create('sample text');
  ret := streamToFile(s, ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'sample.txt');
  AddConsoleLog(Format('StreamToFile => %s', [BoolToStr(ret, True)]));
  s.Free;
end;

procedure TFormMain.sampleFileIOFT();
var
  t: string;
begin
  t := fileToText(ExtractFilePath(ParamStr(0)) + 'Document' + DirectorySeparator + 'sample.txt');
  AddConsoleLog(Format('FileToText => %s', [t]));
end;

procedure TFormMain.sampleImage();
begin
  // image
  with TFormImage.Create(nil) do begin
    ShowModal;
    Free;
  end;
end;

procedure TFormMain.sampleJSON();
var
  parser: TJSONParser;
  json: TJSONObject;
  jarr: TJSONArray;
  o: TSampleObj;
  jsonstr: string;
  lst: TList;
  slst: TStringList;
  i: Integer;
begin

  //
  jsonstr:= '{"s1":"vs1", "i1":1, "sa":["aaa","bbb","ccc"], "sub":{"sub1":"sub1", "sub2":2}, "arr":[{"sub1":"sub3", "sub2":4}, {"sub1":"sub5", "sub2":6}]}';
  o := TSampleObj.Create;
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  json.mapping(o);
  json.Free;
  parser.Free;
  AddConsoleLog(o.ToString);
  o.Free;

  //
  jsonstr:= '[{"sub1":"sub3", "sub2":4}, {"sub1":"sub5", "sub2":6}]';
  parser := TJSONParser.Create(jsonstr, []);
  jarr := TJSONArray(parser.Parse);
  lst := TList.Create;
  jarr.mapping(lst, TSampleSub);
  jarr.Free;
  parser.Free;
  for i:= 0 to lst.Count - 1 do begin
    AddConsoleLog(TSampleSub(lst[i]).ToString);
  end;
  for i:= lst.Count - 1 downto 0 do TSampleSub(lst[i]).Free;
  lst.Free;

  //
  jsonstr:= '["aaa","bbb","ccc"]';
  parser := TJSONParser.Create(jsonstr, []);
  jarr := TJSONArray(parser.Parse);
  slst := TStringList.Create;
  jarr.mapping(slst);
  jarr.Free;
  parser.Free;
  for i:= 0 to slst.Count - 1 do begin
    AddConsoleLog(Format('item %d => %s', [i, slst[i]]));
  end;
  slst.Free;
end;

procedure TFormMain.sampleConfig();
var
  key: string = 'samplekey';
  cfg: string;
begin
  // config
  writeStringConfig(key, 'sample config');
  cfg := readStringConfig(key, '');
  AddConsoleLog(Format('Config => %s', [cfg]));
end;

procedure TFormMain.sampleRegExp();
var
  b1, b2: Boolean;
  e1, e2: Boolean;
begin
  // regexp
  b1 := isStringReg('abcdefg_123456', 5);
  b2 := isStringReg('abc我们_123', 5);
  AddConsoleLog(Format('b1 => %s, b2 => %s', [BoolToStr(b1, True), BoolToStr(b2, True)]));
  e1 := isEmail('rarnu1985@gmail.com');
  e2 := isEmail('abcdefg.com');
  AddConsoleLog(Format('e1 => %s, e2 => %s', [BoolToStr(e1, True), BoolToStr(e2, True)]));
end;

procedure TFormMain.onBoxItemClicked(Sender: TObject);
var
  box: TAdaListBox;
  idx: Integer;
begin
  box := TAdaListBox(Sender);
  idx := box.ItemIndex;
  if (idx = -1) then Exit;
  case idx of
  0: sampleThreadQueue();
  1: sampleThreadHttpGet();
  2: sampleThreadHttpPost();
  3: sampleThreadHttpUploadFile();
  4: sampleThreadDownload();
  5: sampleAlert();
  6: sampleToast();
  7: sampleFileIOTF();
  8: sampleFileIOTS();
  9: sampleFileIOFS();
  10: sampleFileIOST();
  11: sampleFileIOSF();
  12: sampleFileIOFT();
  13: sampleImage();
  14: sampleJSON();
  15: sampleConfig();
  16: sampleRegExp();
  end;
end;

procedure TFormMain.onDownloadCallback(Sender: TObject; ASucc: Boolean;
  AFileName: string);
begin
  AddConsoleLog(Format('Downloaded => [%s] %s', [BoolToStr(ASucc, True), AFileName]));
end;

procedure TFormMain.onDownloadProgressCallback(Sender: TObject;
  AProgress: Integer; ACount: Integer);
begin
  AddConsoleLog(Format('Downloading => %d/%d', [AProgress, ACount]));
end;

procedure TFormMain.onHttpGetCallback(Sender: TObject; ACode: Integer;
  AResult: string; AError: string);
begin
  AddConsoleLog(Format('code => %d, result => %s, error => %s', [ACode, AResult, AError]));
end;

procedure TFormMain.onHttpPostCallback(Sender: TObject; ACode: Integer;
  AResult: string; AError: string);
begin
  AddConsoleLog(Format('code => %d, result => %s, error => %s', [ACode, AResult, AError]));
end;

procedure TFormMain.onHttpUploadFileCallback(Sender: TObject; ACode: Integer;
  AResult: string; AError: string);
begin
  AddConsoleLog(Format('code => %d, result => %s, error => %s', [ACode, AResult, AError]));
end;

procedure TFormMain.onAlertAccept(AAccept: Boolean; AText: string);
begin
  AddConsoleLog(Format('Accept => %s, Text => %s', [BoolToStr(AAccept, True), AText]));
end;

end.

