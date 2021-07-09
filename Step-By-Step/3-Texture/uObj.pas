unit uObj;

interface

uses
  uVectorGeometry;

type
  TObjModel = class
  private
    FVerts: TArray<TVector3f>;
    FVertsTexture: TArray<TVector3f>;
    FFaces: TArray<TArray<TFaceVertex>>;

    procedure AddVert(_AVert: TVector3f);
    procedure AddVertTexture(_AVert: TVector3f);
    procedure ParseVert(const _ALine: String);
    procedure ParseVertTexture(const _ALine: String);
    procedure ParseFace(const _ALine: String);
    procedure AddItem(var _AArray: TArray<TFaceVertex>; _AItem: TFaceVertex);
    procedure AddFace(_AFace: TArray<TFaceVertex>);
  public
    constructor Create(const _AFileName: String);
    function VertCount: integer;
    function FaceCount: integer;
    function Vert(i: Integer): TVector3f;
    function VertTexture(i: Integer): TVector3f;
    function Face(idx: Integer): TArray<TFaceVertex>;
  end;

implementation

uses
  System.Classes, SysUtils, StrUtils, System.Types;

{ TObjModel }

procedure TObjModel.AddFace(_AFace: TArray<TFaceVertex>);
begin
  SetLength(FFaces, Length(FFaces) + 1);
  FFaces[High(FFaces)] := _AFace;
end;

procedure TObjModel.AddItem(var _AArray: TArray<TFaceVertex>; _AItem: TFaceVertex);
begin
  SetLength(_AArray, Length(_AArray) + 1);
  _AArray[High(_AArray)] := _AItem;
end;

procedure TObjModel.AddVert(_AVert: TVector3f);
begin
  SetLength(FVerts, Length(FVerts) + 1);
  FVerts[High(FVerts)] := _AVert;
end;

procedure TObjModel.AddVertTexture(_AVert: TVector3f);
begin
  SetLength(FVertsTexture, Length(FVertsTexture) + 1);
  FVertsTexture[High(FVertsTexture)] := _AVert;
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
      else if Pos('vt ', ALine) = 1 then
      begin
        ParseVertTexture(ALine);
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

function TObjModel.Face(idx: Integer): TArray<TFaceVertex>;
begin
  result := FFaces[idx];
end;

function TObjModel.FaceCount: integer;
begin
  result := Length(FFaces);
end;

procedure TObjModel.ParseFace(const _ALine: String);
var
  AFace: TArray<TFaceVertex>;
  i: integer;

  faceVertices: TStringDynArray;
  faceVertice: TStringDynArray;

  pos, uv, normal: integer;
begin
  Setlength(AFace, 0);

  faceVertices := SplitString(_ALine, ' ');

  for i:=1 to Length(faceVertices) - 1 do
  begin
    faceVertice := SplitString(faceVertices[i], '/');

    pos := StrToInt(faceVertice[0]) - 1;
    uv := StrToInt(faceVertice[1]) - 1;
    normal := StrToInt(faceVertice[2]) - 1;

    AddItem(AFace, TFaceVertex.Create(pos, uv, normal));
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

procedure TObjModel.ParseVertTexture(const _ALine: String);
var
  ALineValues: TStringDynArray;
  AVert: TVector3f;
  AU, AV, AW: Single;
  AFormatSettings: TFormatSettings;
begin
  ALineValues := SplitString(_ALine, ' ');

  AFormatSettings := FormatSettings;
  AFormatSettings.DecimalSeparator := '.';

  AU := StrToFloat(ALineValues[2], AFormatSettings);
  AV := StrToFloat(ALineValues[3], AFormatSettings);
  AW := StrToFloat(ALineValues[4], AFormatSettings);

  AVert := TVector3f.Create(AU, AV, AW);

  AddVertTexture(AVert);
end;

function TObjModel.Vert(i: Integer): TVector3f;
begin
  result := FVerts[i];
end;

function TObjModel.VertCount: integer;
begin
  result := Length(FVerts);
end;

function TObjModel.VertTexture(i: Integer): TVector3f;
begin
  result := FVertsTexture[i];
end;

end.
