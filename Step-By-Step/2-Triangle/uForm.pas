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
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);


    procedure Swap(var v1: integer; var v2: integer); overload;
    procedure Swap(var v1: TVector2i; var v2: TVector2i); overload;
    procedure FlipVertical (_ABmp: TBitmap);

    procedure Line(t0, t1: TVector2i; _ACanvas: TCanvas; _AColor: TColor);
    procedure Triangle(t0, t1, t2: TVector2i; _ACanvas: TCanvas; _AColor: TColor);
    procedure Triangle2(t0, t1, t2: TVector2i; _ACanvas: TCanvas; _AColor: TColor);
    procedure Triangle3(t0, t1, t2: TVector2i; _ACanvas: TCanvas; _AColor: TColor);

    procedure Triangle5(_APts: TArray<TVector2i>; _ACanvas: TCanvas; _AWidth, _AHeight: Integer; _AColor: TColor);
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math;

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
    ABitmap.Height := 200;
    ABitmap.Width := 200;
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

procedure TForm1.Line(t0, t1: TVector2i; _ACanvas: TCanvas; _AColor: TColor);
var
  steep: boolean;
  dx, dy: integer;
  derror2, error2: integer;
  x, y: integer;
begin
  steep := false;
  if (abs(t0.X-t1.X) < abs(t0.Y-t1.Y)) then
  begin
    swap(t0.X, t0.Y);
    swap(t1.X, t1.Y);
    steep := true;
  end;

  if (t0.X>t1.X) then
  begin
    swap(t0.X, t1.X);
    swap(t0.Y, t1.Y);
  end;

  dx := t1.X-t0.X;
  dy := t1.Y-t0.Y;
  derror2 := abs(dy)*2;
  error2 := 0;
  y := t0.Y;
  for x := t0.X to t1.X do
  begin
    if (steep) then
      _ACanvas.Pixels[y, x] := _AColor
    else
      _ACanvas.Pixels[x, y] := _AColor;

    error2 := error2 + derror2;
    if (error2 > dx) then
    begin
      if t1.Y>t0.Y then
        y := y + 1
      else
        y := y - 1;

      error2 := error2 - dx * 2;
    end;
  end;
end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);
var
  t: TArray<TVector2i>;
//  t0: TArray<TVector2i>;
//  t1: TArray<TVector2i>;
//  t2: TArray<TVector2i>;
begin
//  t0 := [TVector2i.Create(10, 70),
//         TVector2i.Create(50, 160),
//         TVector2i.Create(70, 80)];
//
//  t1 := [TVector2i.Create(180, 50),
//         TVector2i.Create(150, 1),
//         TVector2i.Create(70, 180)];
//
//  t2 := [TVector2i.Create(180, 150),
//         TVector2i.Create(120, 160),
//         TVector2i.Create(130, 180)];
//
//  triangle3(t0[0], t0[1], t0[2], _ACanvas, clred);
//  triangle3(t1[0], t1[1], t1[2], _ACanvas, clwhite);
//  triangle3(t2[0], t2[1], t2[2], _ACanvas, cllime);

  t := [TVector2i.Create(10, 10),
        TVector2i.Create(100, 30),
        TVector2i.Create(190, 160)];

  triangle5(t, _ACanvas, _AWidth, _AHeight, clred);
end;

procedure TForm1.Swap(var v1, v2: TVector2i);
var
  tmp: TVector2i;
begin
  tmp := v1;
  v1 := v2;
  v2 := tmp;
end;

procedure TForm1.Swap(var v1: integer; var v2: integer);
var
  tmp: integer;
begin
  tmp := v1;
  v1 := v2;
  v2 := tmp;
end;

procedure TForm1.Triangle(t0, t1, t2: TVector2i; _ACanvas: TCanvas;
  _AColor: TColor);
begin
  line(t0, t1, _ACanvas, _AColor);
  line(t1, t2, _ACanvas, _AColor);
  line(t2, t0, _ACanvas, _AColor);
end;

procedure TForm1.Triangle2(t0, t1, t2: TVector2i; _ACanvas: TCanvas;
  _AColor: TColor);
var
  total_height: Integer;
  segment_height: integer;
  alpha: single;
  beta: single;
  y: integer;
  A, B: TVector2i;
begin
// sort the vertices, t0, t1, t2 lower−to−upper (bubblesort yay!)
    if (t0.y>t1.y) then swap(t0, t1);
    if (t0.y>t2.y) then swap(t0, t2);
    if (t1.y>t2.y) then swap(t1, t2);

    total_height := t2.y-t0.y;

    for y := t0.y to t1.y do
    begin
        segment_height := t1.y-t0.y+1;
        alpha := (y-t0.y)/total_height;
        beta  := (y-t0.y)/segment_height; // be careful with divisions by zero
        A := t0.Add(t2.Subtract(t0).Scale(alpha));
        B := t0.Add(t1.Subtract(t0).Scale(beta));

        _ACanvas.Pixels[A.x, y] := clRed;
        _ACanvas.Pixels[B.x, y] := clLime;
    end;
end;

procedure TForm1.Triangle3(t0, t1, t2: TVector2i; _ACanvas: TCanvas;
  _AColor: TColor);
var
  total_height: Integer;
  segment_height: integer;
  alpha: single;
  beta: single;
  y, j: integer;
  A, B: TVector2i;
begin
// sort the vertices, t0, t1, t2 lower−to−upper (bubblesort yay!)
    if (t0.y>t1.y) then swap(t0, t1);
    if (t0.y>t2.y) then swap(t0, t2);
    if (t1.y>t2.y) then swap(t1, t2);

    total_height := t2.y-t0.y;

    for y := t0.y to t1.y do
    begin
        segment_height := t1.y-t0.y+1;
        alpha := (y-t0.y)/total_height;
        beta  := (y-t0.y)/segment_height; // be careful with divisions by zero
        A := t0.Add(t2.Subtract(t0).Scale(alpha));
        B := t0.Add(t1.Subtract(t0).Scale(beta));

        if A.X > B.X then swap(A, B);

        for j := A.X to B.X do
        begin
          _ACanvas.Pixels[j, y] := _AColor;
        end;
    end;

    for y := t1.y to t2.y do
    begin
        segment_height := t2.y-t1.y+1;
        alpha := (y-t0.y)/total_height;
        beta  := (y-t1.y)/segment_height; // be careful with divisions by zero
        A := t0.Add(t2.Subtract(t0).Scale(alpha));
        B := t1.Add(t2.Subtract(t1).Scale(beta));

        if A.X > B.X then swap(A, B);

        for j := A.X to B.X do
        begin
          _ACanvas.Pixels[j, y] := _AColor;
        end;
    end;
end;

procedure TForm1.Triangle5(_APts: TArray<TVector2i>; _ACanvas: TCanvas; _AWidth,
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
