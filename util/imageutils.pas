unit imageutils;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Graphics, orca_scene2d;

type
  TFlipMode = (fmHorz, fmVert);

  { TPortableNetworkGraphicHelper }

  TPortableNetworkGraphicHelper = type Helper for TPortableNetworkGraphic
  public
    function flip(mode: TFlipMode): TPortableNetworkGraphic;
    function rotate(angle: Single): TPortableNetworkGraphic;
    function blur(level: Integer): TPortableNetworkGraphic;
    function scale(per: Single): TPortableNetworkGraphic;
  end;

  { TBitmapHelper }

  TBitmapHelper = type Helper for TBitmap
  public
    function flip(mode: TFlipMode): TBitmap;
    function rotate(angle: Single): TBitmap;
    function blur(level: Integer): TBitmap;
    function scale(per: Single): TBitmap;
  end;

  { TJPEGImageHelper }

  TJPEGImageHelper = type Helper for TJPEGImage
  public
    function flip(mode: TFlipMode): TJPEGImage;
    function rotate(angle: Single): TJPEGImage;
    function blur(level: Integer): TJPEGImage;
    function scale(per: Single): TJPEGImage;
  end;

  { TTiffImageHelper }

  TTiffImageHelper = type Helper for TTiffImage
  public
    function flip(mode: TFlipMode): TTiffImage;
    function rotate(angle: Single): TTiffImage;
    function blur(level: Integer): TTiffImage;
    function scale(per: Single): TTiffImage;
  end;

implementation

procedure innerFlip(c1, c2: TFPImageBitmap; mode: TFlipMode);
var
  img: TD2Bitmap;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  c1.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  img := TD2Bitmap.CreateFromStream(ms);
  case mode of
  fmVert:img.FlipVertical;
  fmHorz:img.FlipHorizontal;
  end;
  ms.Clear;
  img.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  c1.LoadFromStream(ms);
  ms.Free;
  img.Free;
end;

procedure innerRotate(c1, c2: TFPImageBitmap; angle: Single);
var
  img: TD2Bitmap;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  c1.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  img := TD2Bitmap.CreateFromStream(ms);
  img.Rotate(angle);
  ms.Clear;
  img.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  c2.LoadFromStream(ms);
  ms.Free;
  img.Free;
end;

procedure innerBlur(c1, c2: TFPImageBitmap; level: Integer);
var
  ms: TMemoryStream;
  img: TD2Bitmap;
begin
  ms := TMemoryStream.Create;
  c1.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  img := TD2Bitmap.CreateFromStream(ms);
  Blur(img.Canvas, img, level);
  ms.Clear;
  img.SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  c2.LoadFromStream(ms);
  img.Free;
  ms.Free;
end;

procedure innerScale(c1, c2: TFPImageBitmap; scale: Single);
var
  w, h: Integer;
  dest: TRect;
begin
  w := Trunc(c1.Width * scale);
  h := Trunc(c1.Height * scale);
  c2.Width:= w;
  c2.Height:= h;
  dest := Rect(0, 0, c2.Width, c2.Height);
  c2.Canvas.StretchDraw(dest, c1);
end;

{ TTiffImageHelper }

function TTiffImageHelper.flip(mode: TFlipMode): TTiffImage;
var
  dest: TTiffImage;
begin
  dest := TTiffImage.Create;
  innerFlip(self, dest, mode);
  Exit(dest);
end;

function TTiffImageHelper.rotate(angle: Single): TTiffImage;
var
  dest: TTiffImage;
begin
  dest := TTiffImage.Create;
  innerRotate(self, dest, angle);
  Exit(dest);
end;

function TTiffImageHelper.blur(level: Integer): TTiffImage;
var
  dest: TTiffImage;
begin
  dest := TTiffImage.Create;
  innerBlur(self, dest, level);
  Exit(dest);
end;

function TTiffImageHelper.scale(per: Single): TTiffImage;
var
  dest: TTiffImage;
begin
  dest := TTiffImage.Create;
  innerScale(self, dest, per);
  Exit(dest);
end;

{ TJPEGImageHelper }

function TJPEGImageHelper.flip(mode: TFlipMode): TJPEGImage;
var
  dest: TJPEGImage;
begin
  dest := TJPEGImage.Create;
  innerFlip(self, dest, mode);
  Exit(dest);
end;

function TJPEGImageHelper.rotate(angle: Single): TJPEGImage;
var
  dest: TJPEGImage;
begin
  dest := TJPEGImage.Create;
  innerRotate(self, dest, angle);
  Exit(dest);
end;

function TJPEGImageHelper.blur(level: Integer): TJPEGImage;
var
  dest: TJPEGImage;
begin
  dest := TJPEGImage.Create;
  innerBlur(self, dest, level);
  Exit(dest);
end;

function TJPEGImageHelper.scale(per: Single): TJPEGImage;
var
  dest: TJPEGImage;
begin
  dest := TJPEGImage.Create;
  innerScale(self, dest, per);
  Exit(dest);
end;

{ TBitmapHelper }

function TBitmapHelper.flip(mode: TFlipMode): TBitmap;
var
  dest: TBitmap;
begin
  dest := TBitmap.Create;
  innerFlip(self, dest, mode);
  Exit(dest);
end;

function TBitmapHelper.rotate(angle: Single): TBitmap;
var
  dest: TBitmap;
begin
  dest := TBitmap.Create;
  innerRotate(Self, dest, angle);
  Exit(dest);
end;

function TBitmapHelper.blur(level: Integer): TBitmap;
var
  dest: TBitmap;
begin
  dest := TBitmap.Create;
  innerBlur(self, dest, level);
  Exit(dest);
end;

function TBitmapHelper.scale(per: Single): TBitmap;
var
  dest: TBitmap;
begin
  dest := TBitmap.Create;
  innerScale(self, dest, per);
  Exit(dest);
end;

{ TGraphicHelper }


function TPortableNetworkGraphicHelper.flip(mode: TFlipMode
  ): TPortableNetworkGraphic;
var
  dest: TPortableNetworkGraphic;
begin
  dest := TPortableNetworkGraphic.Create;
  innerFlip(Self, dest, mode);
  Exit(dest);
end;

function TPortableNetworkGraphicHelper.rotate(angle: Single
  ): TPortableNetworkGraphic;
var
  dest: TPortableNetworkGraphic;
begin
  dest := TPortableNetworkGraphic.Create;
  innerRotate(Self, dest, angle);
  Exit(dest);
end;

function TPortableNetworkGraphicHelper.blur(level: Integer
  ): TPortableNetworkGraphic;
var
  dest: TPortableNetworkGraphic;
begin
  dest := TPortableNetworkGraphic.Create;
  innerBlur(Self, dest, level);
  Exit(dest);
end;

function TPortableNetworkGraphicHelper.scale(per: Single
  ): TPortableNetworkGraphic;
var
  dest: TPortableNetworkGraphic;
begin
  dest := TPortableNetworkGraphic.Create;
  innerScale(Self, dest, per);
  Exit(dest);
end;

end.

