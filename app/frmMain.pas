unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase, orca_scene2d,
  jsonparser, fpjson, jsonscanner;

type

  { TFormMain }

  TFormMain = class(TFormBase)
    procedure FormCreate(Sender: TObject);
  private
    FBtn: TD2HudButton;
    procedure onAlertAccept(AAccept: Boolean; AText: string);
    procedure onBtnInfoClicked(Sender: TObject);
    procedure onHttpCallback(Sender: TObject; ACode: Integer; AResult: string;
      AError: string);
    function onListCell(Sender: TD2VisualObject; AItem: String): TD2ListBoxItem;
  public

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

var
  FormMain: TFormMain;

implementation

uses
  orcautils, adaptedlistview, imageutils;

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

{ TAdaItem }

procedure TAdaItem.SetItem(AValue: String);
begin
  inherited SetItem(AValue);
  FlblTxt.Text:= AValue;
end;

procedure TAdaItem.layout();
begin
  FlblTxt := TD2Text(createD2VO(Self, TD2Text, 0, 0, 0, 0, vaClient));
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
  s := Format('s1 => %s, i1 => %d, sa => [%s], sub => [%s], arr => [', [Fs1, Fi1, Fsa.Text, Fsub.ToString]);
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
  FBtn := TD2HudButton(createD2VO(Root, TD2HudButton, 8, 8, 60, 40, vaTop));
  FBtn.OnClick:=@onBtnInfoClicked;

  // with THttpThread.Create('https://www.hujiang.com') do begin
  //   Callback := @onHttpCallback;
  //   Start;
  // end;

  box := TAdaListBox.Create(Root, @onListCell);
  box.Align:= vaClient;
  Root.AddObject(box);

  box.List.Add('aaaa');
  box.List.Add('bbb');
  box.List.Add('ccc');
  box.reloadData();

end;

procedure TFormMain.onHttpCallback(Sender: TObject; ACode: Integer;
  AResult: string; AError: string);
begin
  WriteLn(Format('code: %d, text: %s, error: %s', [ACode, AResult, AError]));
end;

function TFormMain.onListCell(Sender: TD2VisualObject; AItem: String
  ): TD2ListBoxItem;
begin
  Exit(TAdaItem.Create(Sender, 40, AItem));
end;

procedure TFormMain.onBtnInfoClicked(Sender: TObject);
var
  (*
  parser: TJSONParser;
  json: TJSONObject;
  jarr: TJSONArray;
  o: TSampleObj;
  jsonstr: string;
  lst: TList;
  slst: TStringList;
  i: Integer;
  *)
  img: TPortableNetworkGraphic;
  i1: TPortableNetworkGraphic;
begin

  (*
  //
  jsonstr:= '{"s1":"vs1", "i1":1, "sa":["aaa","bbb","ccc"], "sub":{"sub1":"sub1", "sub2":2}, "arr":[{"sub1":"sub3", "sub2":4}, {"sub1":"sub5", "sub2":6}]}';
  o := TSampleObj.Create;
  parser := TJSONParser.Create(jsonstr, []);
  json := TJSONObject(parser.Parse);
  json.mapping(o);
  json.Free;
  parser.Free;
  WriteLn(o.ToString);
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
    WriteLn(TSampleSub(lst[i]).ToString);
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
    WriteLn(slst[i]);
  end;
  slst.Free;
  *)

  // alert('666', '88888', 'OK');
  // alert('777', 'aaaaaaaa', 'Cancel', 'OK');
  // alert('888', 'vskljnvbjgnet', 'Cancel', 'Try!', 'OK');
  // alert('999', 'dejkbdoewufb', 'sample', 'this is a sample alert.', 'Cancel', 'OK', @onAlertAccept);
  img := TPortableNetworkGraphic.Create;
  img.LoadFromFile('a.png');
  i1 := img.scale(0.5);
  i1.SaveToFile('a4.png');
  i1.Free;
  img.Free;
end;

procedure TFormMain.onAlertAccept(AAccept: Boolean; AText: string);
begin
  WriteLn(AText);
end;


end.

