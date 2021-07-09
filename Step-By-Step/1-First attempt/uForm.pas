unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    function Lerp(v0, v1, t: Single): Single;
    function InvLerp(v0, v1, vt: Single): Single;
  private
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);

    procedure Line_Attemp1(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);
    procedure Line_Attemp2(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);
    procedure Line_Attemp3(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);
    procedure Line_Attemp4(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);
    procedure Line_Attemp5(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);

    procedure Swap(var v1: integer; var v2: integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Height := 100;
    ABitmap.Width := 100;
    ABitmap.Canvas.Brush.Color := clBlack;
    ABitmap.Canvas.FillRect(TRect.Create(0,0,ABitmap.Width,ABitmap.Height));

    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height);

    Image.Picture.Assign(ABitmap);
    ABitmap.SaveToFile('c:\tmp\raster.bmp');
  finally
    ABitmap.Free;
  end;
end;

function TForm1.InvLerp(v0, v1, vt: Single): Single;
begin
  if (v1 - v0) = 0 then
  begin
    result := 1;
    exit;
  end;

  Result := (vt - v0) / (v1 - v0);
end;

function TForm1.Lerp(v0, v1, t: Single): Single;
begin
  //Result := v0 + t * (v1 - v0); //lerp não preciso, não garante v = v1 quando t = 1
  Result := v0 * (1 - t) + v1 * t; //lerp preciso, garante v = v1 quando t = 1
end;

procedure TForm1.Line_Attemp1(x0, y0, x1, y1: Integer; _ACanvas: TCanvas; _AColor: TColor);
var
  t: single;
  x, y: integer;
begin
  t := 0;
  while (t<1) do
  begin
    x := trunc(Lerp(x0, x1, t));
    y := trunc(Lerp(y0, y1, t));
    _ACanvas.Pixels[x, y] := _AColor;
    t := t + 0.01;
  end;
end;

procedure TForm1.Line_Attemp2(x0, y0, x1, y1: Integer; _ACanvas: TCanvas;
  _AColor: TColor);
var
  x: Integer;
  y: integer;
  t: single;
begin
  for x := x0 to x1 do
  begin
    t := InvLerp(x0, x1, x);
    y := trunc(Lerp(y0, y1, t));
    _ACanvas.Pixels[x, y] := _AColor;
  end;
end;

procedure TForm1.Line_Attemp3(x0, y0, x1, y1: Integer; _ACanvas: TCanvas;
  _AColor: TColor);
var
  steep: boolean;
  x: Integer;
  y: integer;
  t: single;
begin
  steep := False;
  if (Abs(x0-x1) < abs(y0-y1)) then // if the line is steep, we transpose the image
  begin
    swap(x0, y0);
    swap(x1, y1);
    steep := true;
  end;

  if (x0>x1) then // make it left−to−right
  begin
    swap(x0, x1);
    swap(y0, y1);
  end;

  for x := x0 to x1 do
  begin
    t := InvLerp(x0, x1, x);
    y := trunc(Lerp(y0, y1, t));

    if (steep) then
      _ACanvas.Pixels[y, x] := _AColor// if transposed, de−transpose
    else
      _ACanvas.Pixels[x, y] := _AColor;

  end;
end;

procedure TForm1.Line_Attemp4(x0, y0, x1, y1: Integer; _ACanvas: TCanvas;
  _AColor: TColor);
var
  steep: boolean;
  dx, dy: integer;
  error, derror: single;
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

  derror := Abs(dy / dx);
  error := 0;

  y := y0;

  for x := x0 to x1 do
  begin
    if (steep) then
      _ACanvas.Pixels[y, x] := _AColor
    else
      _ACanvas.Pixels[x, y] := _AColor;

    error := error + derror;

    if (error > 0.5) then
    begin
      if y1>y0 then
        y := y + 1
      else
        y := y - 1;

      error := error - 1;
    end;
  end;
end;

procedure TForm1.Line_Attemp5(x0, y0, x1, y1: Integer; _ACanvas: TCanvas;
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
begin
  Line_Attemp4(13, 20, 80,40, _ACanvas, clWhite);
  Line_Attemp4(20, 13, 40, 80, _ACanvas, clRed);
  Line_Attemp4(80, 40, 13, 20, _ACanvas, clRed);
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
