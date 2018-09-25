unit frmImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, frmBase, orca_scene2d;

type

  { TFormImage }

  TFormImage = class(TFormBase)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImgOrigin: TPortableNetworkGraphic;
    FImage: TD2Image;
    FBtnFlipHorz: TD2HudCornerButton;
    FBtnFlipVert: TD2HudCornerButton;
    FBtnRotate: TD2HudCornerButton;
    FBtnBlur: TD2HudCornerButton;
    FBtnScale: TD2HudCornerButton;
    FBtnOrigin: TD2HudCornerButton;
    procedure onBtnClicked(Sender: TObject);
    procedure pngToStream(p: TPortableNetworkGraphic);
  public

  end;

var
  FormImage: TFormImage;

implementation

uses
  orcautils, imageutils;

{$R *.frm}

{ TFormImage }

procedure TFormImage.FormCreate(Sender: TObject);
begin
  inherited;
  Window.Text:= 'Image';
  Width:= 385;
  Height:= 315;
  FImage := TD2Image(createD2VO(Root, TD2Image, 8, 8, 240, 240));
  FImgOrigin := TPortableNetworkGraphic.Create;
  FImgOrigin.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'sample.png');
  pngToStream(FImgOrigin);

  FBtnFlipHorz:= TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 8, 80, 32));
  FBtnFlipHorz.Text:= 'Flip (Horz)';
  FBtnFlipHorz.Corners:= [d2CornerTopLeft, d2CornerTopRight];
  FBtnFlipVert := TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 40, 80, 32));
  FBtnFlipVert.Text:= 'Flip (Vert)';
  FBtnFlipVert.Corners:= [];
  FBtnRotate:= TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 72, 80, 32));
  FBtnRotate.Text:= 'Rotate';
  FBtnRotate.Corners:= [];
  FBtnBlur:= TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 104, 80, 32));
  FBtnBlur.Text:= 'Blur';
  FBtnBlur.Corners:= [];
  FBtnScale:= TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 136, 80, 32));
  FBtnScale.Text:= 'Scale';
  FBtnScale.Corners:= [];
  FBtnOrigin := TD2HudCornerButton(createD2VO(Root, TD2HudCornerButton, 260, 168, 80, 32));
  FBtnOrigin.Text:= 'Origin';
  FBtnOrigin.Corners:= [d2CornerBottomLeft, d2CornerBottomRight];

  FBtnFlipHorz.OnClick:=@onBtnClicked;
  FBtnFlipVert.OnClick:= @onBtnClicked;
  FBtnRotate.OnClick:= @onBtnClicked;
  FBtnBlur.OnClick:= @onBtnClicked;
  FBtnScale.OnClick:= @onBtnClicked;
  FBtnOrigin.OnClick:= @onBtnClicked;
end;

procedure TFormImage.FormDestroy(Sender: TObject);
begin
  FImgOrigin.Free;
end;

procedure TFormImage.pngToStream(p: TPortableNetworkGraphic);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  p.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  FImage.Bitmap.LoadFromStream(ms);
  ms.Free;
end;

procedure TFormImage.onBtnClicked(Sender: TObject);
var
  p: TPortableNetworkGraphic;
begin
  if (Sender = FBtnFlipHorz) then begin
    p := FImgOrigin.flip(TFlipMode.fmHorz);
  end else if (Sender = FBtnFlipVert) then begin
    p := FImgOrigin.flip(TFlipMode.fmVert);
  end else if (Sender = FBtnRotate) then begin
    p := FImgOrigin.rotate(90);
  end else if (Sender = FBtnBlur) then begin
    p := FImgOrigin.blur(5);
  end else if (Sender = FBtnScale) then begin
    p := FImgOrigin.scale(0.5);
  end else if (Sender = FBtnOrigin) then begin
    pngToStream(FImgOrigin);
    Exit;
  end;
  pngToStream(p);
  p.Free;
end;

end.

