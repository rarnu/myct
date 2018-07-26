unit jsonutils;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, jsonparser, fpjson, jsonscanner, typinfo;

type

  { TJSONObjectHelper }

  TJSONObjectHelper = type Helper for TJSONObject
  public
    procedure mapping(o: TObject);
  end;

  { TJSONArrayHelper }

  TJSONArrayHelper = type Helper for TJSONArray
  public
    procedure mapping(o: TList; clz: TClass);
    procedure mapping(o: TStringList);
  end;

procedure mappingObject(o: TObject; j: TJSONObject);
procedure mappingObjectArray(o: TList; clz: TClass; j: TJSONArray);
procedure mappingArray(o: TStringList; j: TJSONArray);

implementation

// procedure mappingObject(o: TObject; j: TJSONObject);

procedure mappingArray(o: TStringList; j: TJSONArray);
var
  i: Integer;
begin
  for i:= 0 to j.Count - 1 do o.Add(j.Strings[i]);
end;

procedure mappingObjectArray(o: TList; clz: TClass; j: TJSONArray);
var
  i: Integer;
  oo: TObject;
begin
  for i := 0 to j.Count - 1 do begin
    oo := clz.Create;
    mappingObject(oo, j.Objects[i]);
    o.Add(oo);
  end;
end;

procedure mappingObject(o: TObject; j: TJSONObject);
var
  d: PTypeData;
  pCount: Integer;
  pList: PPropList;
  i: Integer;
  info: PPropInfo;
  n: string;
  tobj: TObject;
  cpname: string;
  cpinfo: PPropInfo;
  cpclz: TClass;
begin
  d := GetTypeData(o.ClassInfo);
  pCount := d^.PropCount;
  GetPropList(o.ClassInfo, pList);
  for i:= 0 to pCount - 1 do begin
    info := pList^[i];
    n := info^.Name;
    case info^.PropType^.Kind of
    tkInteger, tkInt64, tkQWord: if (j.Find(n) <> nil) then SetInt64Prop(o, info, j.Int64s[n]);
    tkFloat: if (j.Find(n) <> nil) then SetFloatProp(o, info, j.Floats[n]);
    tkSString, tkLString, tkAString, tkWString, tkUString: if (j.Find(n) <> nil) then SetStrProp(o, info, j.Strings[info^.Name]);
    tkClass:
      begin
        if (j.Find(n) <> nil) then begin
          if (info^.PropType^.Name = 'TStringList') then begin
            tobj := GetObjectProp(o, info);
            if (tobj <> nil) then begin
              mappingArray(TStringList(tobj), j.Arrays[n]);
            end;
          end else if (info^.PropType^.Name = 'TList') then begin
            tobj := GetObjectProp(o, info);
            if (tobj <> nil) then begin
              cpname:= Format('%s__type__', [n]);
              cpinfo := FindPropInfo(o, cpname);
              cpclz:= TClass(GetObjectProp(o, cpinfo));
              mappingObjectArray(TList(tobj), cpclz, j.Arrays[n]);
            end;
          end else begin
            tobj := GetObjectProp(o, info);
            if (tobj <> nil) then mappingObject(tobj, j.Objects[n]);
          end;
        end;
      end;
    end;
  end;
  Freemem(pList);
end;

{ TJSONArrayHelper }

procedure TJSONArrayHelper.mapping(o: TList; clz: TClass);
begin
  // mapping array
  mappingObjectArray(o, clz, Self);
end;

procedure TJSONArrayHelper.mapping(o: TStringList);
begin
  mappingArray(o, Self);
end;

{ TJSONObjectHelper }

procedure TJSONObjectHelper.mapping(o: TObject);
begin
  mappingObject(o, Self);
end;

end.

