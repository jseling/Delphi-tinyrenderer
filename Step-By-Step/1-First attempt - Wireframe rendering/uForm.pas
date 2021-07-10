unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);


    procedure Line(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);

    procedure Swap(var v1: integer; var v2: integer);
    procedure FlipVertical (_ABmp: TBitmap);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uObj, uVectorGeometry;

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

procedure TForm1.Line(x0, y0, x1, y1: Integer; _ACanvas: TCanvas;
  _AColor: TColor);
var
  steep: boolean;
  dx, dy: integer;
  derror2, error2: integer;
  x, y: integer;
begin
  steep := false;
  if (abs(x0-x1) < abs(y0-y1)) then
  begin
    swap(x0, y0);
    swap(x1, y1);
    steep := true;
  end;

  if (x0>x1) then
  begin
    swap(x0, x1);
    swap(y0, y1);
  end;

  dx := x1-x0;
  dy := y1-y0;
  derror2 := abs(dy)*2;
  error2 := 0;
  y := y0;
  for x := x0 to x1 do
  begin
    if (steep) then
      _ACanvas.Pixels[y, x] := _AColor
    else
      _ACanvas.Pixels[x, y] := _AColor;

    error2 := error2 + derror2;
    if (error2 > dx) then
    begin
      if y1>y0 then
        y := y + 1
      else
        y := y - 1;

      error2 := error2 - dx * 2;
    end;
  end;
end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);
var
  AModel: TObjModel;
  i, j: integer;
  AFace: TArray<Integer>;
  v0, v1: TVector3f;
  x0, x1, y0, y1: Integer;
begin
  AModel := TObjModel.Create('..\..\Resources\african_head.obj');
  try
    for i:=0 to AModel.FaceCount -1 do
    begin
        AFace := AModel.Face(i);
        for j:=0 to 2 do
        begin
            v0 := amodel.vert(aface[j]);
            v1 := amodel.vert(aface[(j+1) mod 3]);

            x0 := trunc((v0.x + 1) * _AWidth / 2);
            y0 := trunc((v0.y + 1) * _AHeight / 2);
            x1 := trunc((v1.x + 1) * _AWidth / 2);
            y1 := trunc((v1.y + 1) * _AHeight / 2);

            line(x0, y0, x1, y1, _ACanvas, clWhite);
        end;
    end;

  finally
    AModel.Free;
  end;
end;

procedure TForm1.Swap(var v1: integer; var v2: integer);
var
  tmp: integer;
begin
  tmp := v1;
  v1 := v2;
  v2 := tmp;
end;

end.
