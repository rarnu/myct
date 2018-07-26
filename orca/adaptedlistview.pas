(* usage sample

==== interface ====

type
  TAdaListBox = specialize TAdaptedListBox<String>;

  { TAdaItem }

  TAdaItem = class(specialize TAdaptedListItem<String>)
  private
    FlblTxt: TD2Text;
  protected
    procedure SetItem(AValue: String); override;
    procedure layout(); override;
  end;

==== implementation ====

...
box := TAdaListBox.Create(Root, @onListCell);
box.Align:= vaClient;
Root.AddObject(box);
box.List.Add('aaaa');
box.List.Add('bbb');
box.List.Add('ccc');
box.reloadData();
...

function TFormMain.onListCell(Sender: TD2VisualObject; AItem: String): TD2ListBoxItem;
begin
  Exit(TAdaItem.Create(Sender, 40, AItem));
end;

*)


unit adaptedlistview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, orca_scene2d, fgl;

type
  { TAdaptedListItem }

  generic TAdaptedListItem<T> = class(TD2ListBoxItem)
  private
    FItem: T;
  protected
    procedure SetItem(AValue: T); virtual;
    procedure layout(); virtual; abstract;
  public
    constructor Create(AOwner: TComponent; AHeight: Single; AItem: T); reintroduce;
  public
    property Item: T read FItem write SetItem;
  end;

  { TAdaptedListBox }

  generic TAdaptedListBox<T> = class(TD2ListBox)
  public type
    TInnerList = specialize TFPGList<T>;
    TOnCellBuild = function(Sender: TD2VisualObject; AItem: T): TD2ListBoxItem of object;
  private
    FList: TInnerList;
    FOnCell: TOnCellBuild;
    procedure SetList(AValue: TInnerList);
    procedure CleanList();
  public
    constructor Create(AOwner: TD2VisualObject; AOnCell: TOnCellBuild); reintroduce;
    destructor Destroy; override;
    procedure reloadData();
  public
    property List: TInnerList read FList write SetList;
  end;

  { THudAdaptedListBox }

  generic THudAdaptedListBox<T> = class(TD2HudListBox)
  public type
    TInnerList = specialize TFPGList<T>;
    TOnCellBuild = function(Sender: TD2VisualObject; AItem: T): TD2ListBoxItem of object;
  private
    FList: TInnerList;
    FOnCell: TOnCellBuild;
    procedure SetList(AValue: TInnerList);
    procedure CleanList();
  public
    constructor Create(AOwner: TD2VisualObject; AOnCell: TOnCellBuild); reintroduce;
    destructor Destroy; override;
    procedure reloadData();
  public
    property List: TInnerList read FList write SetList;
  end;

implementation

{ THudAdaptedListBox }

procedure THudAdaptedListBox.SetList(AValue: TInnerList);
begin
  CleanList();
  FList.AddList(AValue);
  reloadData();
end;

procedure THudAdaptedListBox.CleanList();
var
  i: Integer;
begin
  Clear;
  if (FList <> nil) then begin
    for i:= FList.Count - 1 downto 0 do begin
      try
        TObject(FList[i]).Free;
      except
      end;
    end;
  end;
end;

constructor THudAdaptedListBox.Create(AOwner: TD2VisualObject;
  AOnCell: TOnCellBuild);
begin
  inherited Create(AOwner);
  Align:= vaClient;
  AOwner.AddObject(Self);
  FOnCell:= AOnCell;
  UseSmallScrollBars:= True;
  FList:= TInnerList.Create;
end;

destructor THudAdaptedListBox.Destroy;
begin
  CleanList();
  FList.Free;
  inherited Destroy;
end;

procedure THudAdaptedListBox.reloadData();
var
  i: Integer;
  item: TD2ListBoxItem;
begin
  Clear;
  for i := 0 to FList.Count - 1 do begin
    item := FOnCell(Self, FList[i]);
    Self.AddObject(item);
  end;
end;

{ TAdaptedListBox }

procedure TAdaptedListBox.SetList(AValue: TInnerList);
begin
  // refresh list
  CleanList();
  FList.AddList(AValue);
  reloadData();
end;

procedure TAdaptedListBox.CleanList();
var
  i: Integer;
begin
  Clear;
  if (FList <> nil) then begin
    for i:= FList.Count - 1 downto 0 do begin
      try
        TObject(FList[i]).Free;
      except
      end;
    end;
  end;
end;

constructor TAdaptedListBox.Create(AOwner: TD2VisualObject;
  AOnCell: TOnCellBuild);
begin
  inherited Create(AOwner);
  Align:= vaClient;
  AOwner.AddObject(Self);
  FOnCell:= AOnCell;
  UseSmallScrollBars:= True;
  FList:= TInnerList.Create;
end;

destructor TAdaptedListBox.Destroy;
begin
  CleanList();
  FList.Free;
  inherited Destroy;
end;

procedure TAdaptedListBox.reloadData();
var
  i: Integer;
  item: TD2ListBoxItem;
begin
  Clear;
  for i := 0 to FList.Count - 1 do begin
    item := FOnCell(Self, FList[i]);
    Self.AddObject(item);
  end;
end;

{ TAdaptedListItem }

procedure TAdaptedListItem.SetItem(AValue: T);
begin
  FItem:= AValue;
end;

constructor TAdaptedListItem.Create(AOwner: TComponent; AHeight: Single;
  AItem: T);
begin
  inherited Create(AOwner);
  Height:= AHeight;
  layout();
  SetItem(AItem);
end;

end.

