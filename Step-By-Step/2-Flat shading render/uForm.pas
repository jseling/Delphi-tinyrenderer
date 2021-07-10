unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  uVectorGeometry;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    function RandomColor: TColor;

    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);

    procedure FlipVertical (_ABmp: TBitmap);

    procedure Triangle(_APts: TArray<TVector2i>; _ACanvas: TCanvas; _AWidth, _AHeight: Integer; _AColor: TColor);
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

procedure TForm1.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Height := 800;
    ABitmap.Width := 800;
    ABitmap.PixelFormat := pf24Bit;
    ABitmap.Canvas.Brush.Color := clBlack;
    ABitmap.Canvas.FillRect(TRect.Create(0,0,ABitmap.Width,ABitmap.Height));

    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height);

    FlipVertical(ABitmap);

    Image.Picture.Assign(ABitmap);
    ABitmap.SaveToFile('c:\tmp\raster.bmp');
  finally
    ABitmap.Free;
  end;
end;

function TForm1.RandomColor: TColor;
begin
  Randomize;
  Result := RGB(Random(255), Random(255), Random(255));
end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);
var
  AModel: TObjModel;
  i, j: integer;
  AFace: TArray<Integer>;
  world_coords : TVector3f;
  screen_coords: TArray<TVector2i>;
begin
  SetLength(screen_coords, 3);

  AModel := TObjModel.Create('..\..\Resources\african_head.obj');
  try
    for i:=0 to AModel.FaceCount -1 do
    begin
        AFace := AModel.Face(i);
        for j := 0 to 2 do
        begin
            world_coords := AModel.Vert(AFace[j]);
            screen_coords[j] := TVector2i.Create(Trunc((world_coords.x + 1) * _AWidth / 2),
                                                 Trunc((world_coords.y + 1) * _AHeight / 2));
        end;
        triangle(screen_coords, _ACanvas, _AWidth, _AHeight, RandomColor);
    end;
  finally
    AModel.Free;
  end;
end;

procedure TForm1.Triangle(_APts: TArray<TVector2i>; _ACanvas: TCanvas; _AWidth,
  _AHeight: Integer; _AColor: TColor);
var
  bboxmin: TVector2i;
  bboxmax: TVector2i;
  clamp: TVector2i;
  i, j: integer;
  P: TVector2i;
  bc_screen: TVector3f;
begin
  bboxmin := TVector2i.Create(_AWidth - 1, _AHeight - 1);
  bboxmax := TVector2i.Create(0, 0);
  clamp := TVector2i.Create(_AWidth - 1, _AHeight - 1);

  for i := 0 to 2 do
  begin
    bboxmin.X := max(0,       min(bboxmin.X, _APts[i].X));
    bboxmax.X := min(clamp.X, max(bboxmax.X, _APts[i].X));

    bboxmin.Y := max(0,       min(bboxmin.Y, _APts[i].Y));
    bboxmax.Y := min(clamp.Y, max(bboxmax.Y, _APts[i].Y));
  end;

  for i := bboxmin.X to bboxmax.X do
    for j := bboxmin.Y to bboxmax.Y do
    begin
      P := TVector2i.Create(i, j);
      bc_screen := barycentric(_APts, P);

      if (bc_screen.X < 0) or (bc_screen.Y < 0) or (bc_screen.Z < 0) then
        continue;

      _ACanvas.Pixels[P.X, P.Y] := _AColor;
    end;
end;

end.
