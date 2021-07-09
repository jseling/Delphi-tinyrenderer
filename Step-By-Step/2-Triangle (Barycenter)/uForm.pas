unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  uVectorGeometry, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    lblDuracao: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    lblPos: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    procedure Desenha(_x, _y: integer);
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer; _x, _y: integer);

    procedure FlipVertical (_ABmp: TBitmap);

    procedure Triangle5(_APts: TArray<TVector2i>; _ACanvas: TCanvas; _AWidth, _AHeight: Integer; _AColor: TColor);
  end;

var
  Form1: TForm1;

implementation

uses
  System.Math, dateutils;

{$R *.dfm}

procedure TForm1.Desenha(_x, _y: integer);
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

    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height, _x, _y);

    //FlipVertical(ABitmap);

    ABitmap.Canvas.Pixels[_x, _y] := clWhite;

    Image.Picture.Assign(ABitmap);
    ABitmap.SaveToFile('c:\tmp\raster.bmp');
  finally
    ABitmap.Free;
  end;
end;

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
begin
  Desenha(0,0);
end;


procedure TForm1.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ini, fim: TDateTime;
begin
  ini:= now;
  Desenha(x ,y);
  fim := now;

  lblDuracao.Caption := 'tempo: ' + inttostr(millisecondsbetween(ini, fim)) + ' ms';



end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer; _x, _y: integer);
var
  t, tuv: TArray<TVector2i>;
  bc: TVector3f;
  proj: TVector3f;
begin
  lblPos.Caption := 'Pos: (10, 10) (100, 30) (190, 160)' + #13#10+
                    'Proj: (10, 150) (100, 10) (190, 150)';
  tuv := [TVector2i.Create(10, 150),
        TVector2i.Create(100, 10),
        TVector2i.Create(190, 150)];


  triangle5(tuv, _ACanvas, _AWidth, _AHeight, clred);


  t := [TVector2i.Create(10, 10),
        TVector2i.Create(100, 30),
        TVector2i.Create(190, 160)];
  triangle5(t, _ACanvas, _AWidth, _AHeight, clBlue);

  bc := barycentric(t, TVector2i.Create(_x, _y));

  lblX.Caption := 'X: ' + floattostr(bc.X);
  lblY.Caption := 'Y: ' + floattostr(bc.Y);
  lblZ.Caption := 'Z: ' + floattostr(bc.Z);

  proj := TVector3f.Create(0, 0, 0);

  proj.x := 0;
  proj.x := proj.x + tuv[0].x * bc.x;
  proj.x := proj.x + tuv[1].x * bc.y;
  proj.x := proj.x + tuv[2].x * bc.z;

  proj.y := 0;
  proj.y := proj.y + tuv[0].y * bc.x;
  proj.y := proj.y + tuv[1].y * bc.y;
  proj.y := proj.y + tuv[2].y * bc.z;

  _ACanvas.Pixels[trunc(proj.x), trunc(proj.y)] := clYellow;

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
