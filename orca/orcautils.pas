unit orcautils;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, orca_scene2d;

type
  TD2VisualObjectClass = class of TD2VisualObject;

  { TD2TextHelper }

  TD2TextHelper = type Helper for TD2Text
  public
    procedure setup(txt: string;
      color: string = vcWhite;
      hAlign: TD2TextAlign = d2TextAlignNear;
      vAlign: TD2TextAlign = d2TextAlignCenter;
      wrap: Boolean = False);
  end;

  { TD2VisualObjectHelper }

  TD2VisualObjectHelper = type Helper for TD2VisualObject
  public
    function getTextHeight(txt: string; w: Single): Single;
  end;

function createD2O(AOwner: TD2Object; AClass: TD2ObjectClass): TD2Object;
function createD2VO(AOwner: TD2Object; AClass:TD2VisualObjectClass;
  x, y, w, h: Single;
  AAlign: TD2Align = vaNone;
  pLeft: Single = 0; pTop: Single = 0; pRight: Single = 0; pBottom: Single = 0): TD2VisualObject;

implementation

function createD2O(AOwner: TD2Object; AClass: TD2ObjectClass): TD2Object;
var
  o: TD2Object;
begin
  o := AClass.Create(AOwner);
  AOwner.AddObject(o);
  Exit(o);
end;

function createD2VO(AOwner: TD2Object; AClass: TD2VisualObjectClass; x, y, w,
  h: Single; AAlign: TD2Align; pLeft: Single; pTop: Single; pRight: Single;
  pBottom: Single): TD2VisualObject;
var
  o: TD2VisualObject;
begin
  o := AClass.Create(AOwner);
  o.Position.X:= x;
  o.Position.Y:= y;
  o.Width:= w;
  o.Height:= h;
  o.Align:= AAlign;
  o.Padding.Left:= pLeft;
  o.Padding.Right:= pRight;
  o.Padding.Top:= pTop;
  o.Padding.Bottom:= pBottom;
  AOwner.AddObject(o);
  Exit(o);
end;

{ TD2VisualObjectHelper }

function TD2VisualObjectHelper.getTextHeight(txt: string; w: Single): Single;
var
  r: TD2Rect;
begin
  if (txt = '') then Exit(0);
  r := d2Rect(0, 0, w, Integer.MaxValue);
  Self.Canvas.MeasureText(r, r, WideString(txt), True, d2TextAlignNear, d2TextAlignNear);
  Exit(r.Bottom);
end;

{ TD2TextHelper }

procedure TD2TextHelper.setup(txt: string; color: string; hAlign: TD2TextAlign;
  vAlign: TD2TextAlign; wrap: Boolean);
begin
  Self.Text:= txt;
  Self.Fill.Color:= color;
  Self.HorzTextAlign:= hAlign;
  Self.VertTextAlign:= vAlign;
  Self.WordWrap:= wrap;
end;

end.

