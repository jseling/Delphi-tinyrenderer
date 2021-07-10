unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  uVectorGeometry, uViewer;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    procedure FlipVertical (_ABmp: TBitmap);

    function world2screen(v: TVector3f; _AWidth, _AHeight: Integer): TVector3f;
    function Gray(_AIntensity: Single): TVector3f;

    procedure Render(_AViewer: TViewer);

    procedure Triangle(_APts: TArray<TVector3f>; _AUVs: TArray<TVector3f>; _ABitmap: TBitmap; _AViewer: TViewer; light: Single);
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math, uObj;

{$R *.dfm}

procedure TForm1.FlipVertical (_ABmp: TBitmap);
type
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;
var i, y : integer;
    RowSrc, RowDest : pRGBTripleArray;
    tmpBitmap : TBitmap;
begin
  tmpBitmap := TBitmap.Create;
  try
    tmpBitmap.Assign(_ABmp);
    for y := 0 to _ABmp.Height -1 do begin
       RowSrc := _ABmp.ScanLine[y];
       RowDest := tmpBitmap.ScanLine[_ABmp.Height -1 -y];
       for i := 0 to _ABmp.Width -1 do
           RowDest[i] := RowSrc[i];
    end;
    _ABmp.Assign(tmpBitmap);
  finally
    FreeAndNil(tmpBitmap);
  end;
end;

function TForm1.world2screen(v: TVector3f; _AWidth, _AHeight: Integer): TVector3f;
begin
    Result := TVector3f.Create((v.x + 1) * _AWidth / 2 + 0.5,
                               (v.y + 1) * _AHeight / 2 + 0.5,
                                v.z);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AViewer: TViewer;
  ABitmap: TBitmap;
begin
  AViewer := TViewer.Create(1800, 1800);
  try
    Render(AViewer);

    ABitmap := AViewer.ToBitmap;
    try
      Image.Picture.Assign(ABitmap);
      ABitmap.SaveToFile('c:\tmp\raster.bmp');
    finally
      ABitmap.Free;
    end;
  finally
    AViewer.Free;
  end;
end;

function TForm1.Gray(_AIntensity: Single): TVector3f;
begin
  result := TVector3f.Create(_AIntensity, _AIntensity, _AIntensity);
end;

procedure TForm1.Render(_AViewer: TViewer);
var
  AModel: TObjModel;
  i, j: integer;
  AFace: TArray<TFaceVertex>;
  v: TVector3f;
  t: TVector3f;
  n: TVector3f;
  world_coords : TArray<TVector3f>;
  screen_coords: TArray<TVector3f>;
  texture_coords: TArray<TVector3f>;
  light_dir: TVector3f;
  intensity: Single;
  ATexture: TBitmap;
begin
  SetLength(screen_coords, 3);
  SetLength(world_coords, 3);
  SetLength(texture_coords, 3);

  light_dir := TVector3f.Create(0,0,-1);

  AModel := TObjModel.Create('..\..\Resources\african_head.obj');
  ATexture := TBitmap.Create;
  try
    ATexture.LoadFromFile('..\..\Resources\african_head_diffuse.bmp');
    FlipVertical(ATexture);
    for i:=0 to AModel.FaceCount -1 do
    begin
        AFace := AModel.Face(i);
        for j := 0 to 2 do
        begin
            v := AModel.Vert(AFace[j].IDPosition);
            screen_coords[j] := world2screen(v, _AViewer.Width, _AViewer.Height);

            world_coords[j] := v;
            texture_coords[j] := AModel.VertTexture(AFace[j].IDUV);
        end;

        n := world_coords[2].Subtract(world_coords[0]).CrossProduct(world_coords[1].Subtract(world_coords[0]));
        n := n.Normalize();
        intensity := n.DotProduct(light_dir);

        if (intensity>0) then
          triangle(screen_coords, texture_coords, ATexture, _AViewer, intensity);
    end;
  finally
    AModel.Free;
    ATexture.Free;
  end;
end;

procedure TForm1.Triangle(_APts: TArray<TVector3f>; _AUVs: TArray<TVector3f>; _ABitmap: TBitmap; _AViewer: TViewer; light: Single);
var
  bboxmin: TVector2f;
  bboxmax: TVector2f;
  clamp: TVector2i;
  i, j: integer;
  P: TVector3f;
  bc_screen: TVector3f;
  bc_texture: TVector3f;
  color: TColor;
begin
  bboxmin := TVector2f.Create(_AViewer.Width - 1, _AViewer.Height - 1);
  bboxmax := TVector2f.Create(0, 0);
  clamp := TVector2i.Create(_AViewer.Width - 1, _AViewer.Height - 1);

  for i := 0 to 2 do
  begin
    bboxmin.X := max(0,       min(bboxmin.X, _APts[i].X));
    bboxmax.X := min(clamp.X, max(bboxmax.X, _APts[i].X));

    bboxmin.Y := max(0,       min(bboxmin.Y, _APts[i].Y));
    bboxmax.Y := min(clamp.Y, max(bboxmax.Y, _APts[i].Y));
  end;

  for i := Trunc(bboxmin.X) to Trunc(bboxmax.X) do
    for j := Trunc(bboxmin.Y) to Trunc(bboxmax.Y) do
    begin
      P := TVector3f.Create(i, j, 0);
      bc_screen := barycentric(_APts, P);

      if (bc_screen.X < 0) or (bc_screen.Y < 0) or (bc_screen.Z < 0) then
        continue;

      P.z := 0;
      P.z := P.z + _APts[0].z * bc_screen.x;
      P.z := P.z + _APts[1].z * bc_screen.y;
      P.z := P.z + _APts[2].z * bc_screen.z;

      if (_AViewer.GetZValue(Trunc(P.x), Trunc(P.y)) < P.z) then
      begin
        _AViewer.SetZValue(Trunc(P.x), Trunc(P.y), P.z);

        bc_texture := TVector3f.Create(0, 0, 0);

        bc_texture.x := 0;
        bc_texture.x := bc_texture.x + _AUVs[0].x * bc_screen.x;
        bc_texture.x := bc_texture.x + _AUVs[1].x * bc_screen.y;
        bc_texture.x := bc_texture.x + _AUVs[2].x * bc_screen.z;

        bc_texture.y := 0;
        bc_texture.y := bc_texture.y + _AUVs[0].y * bc_screen.x;
        bc_texture.y := bc_texture.y + _AUVs[1].y * bc_screen.y;
        bc_texture.y := bc_texture.y + _AUVs[2].y * bc_screen.z;

        color := _ABitmap.Canvas.Pixels[trunc(bc_texture.X * _ABitmap.Width), trunc(bc_texture.Y * _ABitmap.Height)];

        _AViewer.SetPixel(Trunc(P.X), Trunc(P.Y), getColor(color).Scale(light));
      end;
    end;
end;

end.
