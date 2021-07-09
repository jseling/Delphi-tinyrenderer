unit uObj;

interface

uses
  uVectorGeometry;

type
  TObjModel = class
  private
    FVerts: TArray<TVector3f>;
    FFaces: TArray<TArray<Integer>>;
  public
    constructor Create(const _AFileName: String);
    function VertCount: integer;
    function FaceCount: integer;
    function Vert(i: Integer): TVector3f;
    function Face(idx: Integer): TArray<Integer>;
  end;

implementation

uses
  System.Classes;

{ TObjModel }

constructor TObjModel.Create(const _AFileName: String);
var
  AFile: TStringList;
begin
  AFile := TStringList.Create;
  try
    AFile.LoadFromFile(_AFileName);
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

function TObjModel.Vert(i: Integer): TVector3f;
begin
  result := FVerts[i];
end;

function TObjModel.VertCount: integer;
begin
  result := Length(FVerts);
end;

end.
