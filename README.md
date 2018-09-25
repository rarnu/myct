## myct

Function Utils for CodeTyphon (Native)


#### Using in CodeTyphon Project

```
add dependence of myct and sub folders.
```

#### Try this

```
type
  TAdaListBox = specialize THudAdaptedListBox<String>;
  TAdaItem = class(specialize TAdaptedListItem<String>)
  private
    FlblTxt: TD2Text;
  protected
    procedure SetItem(AValue: String); override;
    procedure layout(); override;
  end;
  
... ...

jsonstr:= '{"s1":"vs1", "i1":1, "sa":["aaa","bbb","ccc"], "sub":{"sub1":"sub1", "sub2":2}, "arr":[{"sub1":"sub3", "sub2":4}, {"sub1":"sub5", "sub2":6}]}';
o := TSampleObj.Create;
parser := TJSONParser.Create(jsonstr, []);
json := TJSONObject(parser.Parse);
json.mapping(o);
WriteLn(o.ToString);

... ...

img := TPortableNetworkGraphic.Create;
img.LoadFromFile('a.png');
img1 := img.scale(0.5);

... ...

with THttpThread.Create(HTTPURL, hmPost) do begin
  PostParam.Add('name', 'rarnu');
  FileParam.Add('file', 'sample.png');
  Callback := @onHttpUploadFileCallback;
  Start;
end;
```


#### Screenshot

![](https://raw.githubusercontent.com/rarnu/myct/master/screenshot/demo1.gif)

