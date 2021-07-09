unit uObj;

interface

uses
  uVectorGeometry;

type
  TObjModel = class
  private
    FVerts: TArray<TVector3f>;
    FFaces: TArray<TArray<Integer>>;

    procedure AddVert(_AVert: TVector3f);
    procedure ParseVert(const _ALine: String);
    procedure ParseFace(const _ALine: String);
    procedure AddItem(var _AArray: TArray<Integer>; _AItem: Integer);
    procedure AddFace(_AFace: TArray<Integer>);
  public
    constructor Create(const _AFileName: String);
    function VertCount: integer;
    function FaceCount: integer;
    function Vert(i: Integer): TVector3f;
    function Face(idx: Integer): TArray<Integer>;
  end;

implementation

uses
  System.Classes, SysUtils, StrUtils, System.Types;

{ TObjModel }

procedure TObjModel.AddFace(_AFace: TArray<Integer>);
begin
  SetLength(FFaces, Length(FFaces) + 1);
  FFaces[High(FFaces)] := _AFace;
end;

procedure TObjModel.AddItem(var _AArray: TArray<Integer>; _AItem: Integer);
begin
  SetLength(_AArray, Length(_AArray) + 1);
  _AArray[High(_AArray)] := _AItem;
end;

procedure TObjModel.AddVert(_AVert: TVector3f);
begin
  SetLength(FVerts, Length(FVerts) + 1);
  FVerts[High(FVerts)] := _AVert;
end;

constructor TObjModel.Create(const _AFileName: String);
var
  AFile: TStringList;
  ALine: String;
begin
  AFile := TStringList.Create;
  try
    AFile.LoadFromFile(_AFileName);

    for ALine in AFile do
    begin
      if Pos('v ', ALine) = 1 then
      begin
        ParseVert(ALine);
      end
      else if Pos('f ', ALine) = 1 then
      begin
        ParseFace(ALine);
      end;
    end;
  finally
    AFile.Free;
  end;
end;

function TObjModel.Face(idx: Integer): TArray<Integer>;
begin
  result := FFaces[idx];
end;

function TObjModel.FaceCount: integer;
begin
  result := Length(FFaces);
end;

procedure TObjModel.ParseFace(const _ALine: String);
var
  AFace: TArray<Integer>;
  i: integer;

  faceVertices: TStringDynArray;
  faceVertice: TStringDynArray;
begin
  Setlength(AFace, 0);

  faceVertices := SplitString(_ALine, ' ');

  for i:=1 to Length(faceVertices) - 1 do
  begin
    faceVertice := SplitString(faceVertices[i], '/');
    AddItem(AFace, StrToInt(faceVertice[0]) - 1); //obj index start in 1
  end;

  AddFace(AFace);
end;

procedure TObjModel.ParseVert(const _ALine: String);
var
  ALineValues: TStringDynArray;
  AVert: TVector3f;
  AX, AY, AZ: Single;
  AFormatSettings: TFormatSettings;
begin
  ALineValues := SplitString(_ALine, ' ');

  AFormatSettings := FormatSettings;
  AFormatSettings.DecimalSeparator := '.';

  AX := StrToFloat(ALineValues[1], AFormatSettings);
  AY := StrToFloat(ALineValues[2], AFormatSettings);
  AZ := StrToFloat(ALineValues[3], AFormatSettings);

  AVert := TVector3f.Create(AX, AY, AZ);

  AddVert(AVert);
end;

function TObjModel.Vert(i: Integer): TVector3f;
begin
  result := FVerts[i];
end;

function TObjModel.VertCount: integer;
begin
  result := Length(FVerts);
end;

end.
